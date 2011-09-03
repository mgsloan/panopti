{-# LANGUAGE FlexibleInstances, TemplateHaskell, TupleSections, TypeOperators,
ScopedTypeVariables, TypeFamilies #-}

import Simple

import Control.Applicative (liftA2, pure, (<*>))
import Control.Arrow ((&&&), (***), first, second)
import Control.Concurrent.MVar
import Control.Monad (liftM, zipWithM, zipWithM_, foldM, foldM_, join, when)
import qualified Control.Monad.State as ST
import Control.Monad.Trans (liftIO)
import Data.Colour.SRGB (channelRed, channelGreen, channelBlue, toSRGB24)
import qualified Data.Colour.Names as Color
import Data.Curve hiding ((<.>))
import Data.Curve.Util (mapT)
import Data.Data
import Data.Either
import Data.Function (on)
import Data.Generics.Aliases
import Data.Hashable
import Data.IntervalMap.FingerTree as IM
import Data.Label
import Data.List (groupBy, sortBy, sort, nub)
import qualified Data.Map as M
import Data.Maybe
import Data.Ord (comparing)
import Debug.Trace
import Graphics.ToyFramework
import qualified Graphics.Rendering.Cairo as C
import Language.Haskell.Exts.Annotated
import Language.Haskell.Meta.Parse (parseResultToEither)
import qualified Language.Haskell.Interpreter as I
import System.FilePath ((</>), (<.>))
import System.IO (writeFile)
import System.IO.Unsafe (unsafePerformIO)
import System.Directory (createDirectoryIfMissing, doesFileExist)

data State = State
  { _code :: String
  , _cursor :: Int
  , _parsed :: (ParseResult (Decl SrcSpanInfo))
  , _chan :: TaskChan
  , _mouseCursor :: (Double, Double)
  }

$(mkLabels [''State])

modM :: Monad m => (b :-> a) -> (a -> a) -> b -> m b
modM l f = return . modify l f

setM :: Monad m => (b :-> a) -> a -> b -> m b
setM l x = return . set l x

lensed :: (f :-> a) -> (f :-> a') -> (a -> a') -> f -> f
lensed l l' f s = set l' (f $ get l s) s

updateParse :: State -> State
updateParse = lensed code parsed parseDecl

sourceDir = "source"

main = createDirectoryIfMissing False sourceDir
--    >> print "hi!"
    >> startGHCiServer [sourceDir] fail print
    >>= \chan -> runToy $ Toy
    { initialState = updateParse $ 
        State "fibs = 0 : 1 : zipWith (+) fibs (tail fibs)" 0 undefined chan (0, 220)
    , mouse   = const $ setM mouseCursor
    , key     = handleKey
    , display = handleDisplay
    , tick    = const return
    }

handleKey :: Bool -> Either [Char] Char -> State -> IO State
handleKey True (Right k) (State xs ix p c m) =
  return . updateParse $ State (pre ++ (k : post)) (ix + 1) p c m
 where 
  (pre, post) = splitAt ix xs

handleKey True (Left k) s@(State xs ix _ _ _) = liftM updateParse $ (case k of
  "Left"  -> modM cursor (max 0 . subtract 1)
  "Right" -> modM cursor (min endPos . (+1))
  "Home"  -> setM cursor 0
  "End"   -> setM cursor endPos
  "BackSpace" -> modM cursor (max 0 . subtract 1)
               . set code (delIx (ix - 1))
  "Delete" -> setM code (delIx ix)
  "Escape" -> const $ error "User escape"
  _ -> return) s
 where
  endPos = length xs
  delIx i | (pre, (_:post)) <- splitAt i xs = pre ++ post
          | otherwise = xs

handleKey _ _ s = return s

handleDisplay :: IPnt -> IRect -> State -> C.Render State
handleDisplay _ (tl, br) s@(State txt ix p c (_, ypos)) = do
  let textPos = (50.5, 200.5)
      height = (fromIntegral . snd $ br ^-^ tl) * 0.5
      astPos = textPos ^+^ (0.0, ypos - height)
  C.setFontSize 20

  move textPos
  C.showText txt

  -- Draw the mouse cursor.
  C.setLineWidth 1
  draw . offset (textPos ^+^ (-1, 0)) . rside 1 . expandR 2 
       =<< textRect txt 0 ix 
  C.stroke
  
  case p of
    ParseOk decl -> drawApps (textPos ^-^ (0, 20)) txt c
                             (concatApps $ getApps decl)
--                    drawSpans astPos txt (getSpans decl)
    f@(ParseFailed _ _) -> C.showText (show f)
  C.stroke

  return s

drawLabeledLine :: String -> DLine -> C.Render ()
drawLabeledLine txt lin = do
  draw lin
  relText 0.5 (lin `at` 0.5 ^-^ (0, 7)) txt

reverseLinear :: Linear Double -> Linear Double
reverseLinear = (`compose` (Linear 1 0 :: Linear Double))

spanLine :: String -> Ivl -> C.Render DLine
spanLine txt (f, t) = liftM (mapT reverseLinear . rside 2 . expandR 2) 
                    $ textRect txt (f - 1) (t - 1)

drawSpans :: DPoint  -> String -> [(Ivl, String)] -> C.Render ()
drawSpans pos txt =
      -- Draw each labeled line, with each subsequent line 15 pixels lower.
  (>>= zipWithM_ (\d (l, n) -> drawLabeledLine n
                             $ offset (pos ^+^ (0, 15) ^* fromIntegral d) l)
                 [0..])

      -- Turn each span into an appropriately sized line segment.
  . mapM (\(s, n) -> liftM (, n) $ spanLine txt s)

      -- Prefer last of all identically-spanned tokens.  Pretty arbitrary.
  . map last . groupBy ((==) `on` (\(x,_)->x))

srcSpan :: SrcSpanInfo -> Ivl
srcSpan = (srcSpanStartColumn &&& srcSpanEndColumn) .  srcInfoSpan

getSpan :: (Data a) => a -> Maybe Ivl
getSpan = listToMaybe . catMaybes
        . gmapQ (const Nothing `extQ` (Just . srcSpan))

getSpans :: (Data a) => a -> [(Ivl, String)]
getSpans x = maybeToList (fmap (, show $ toConstr x) $ getSpan x)
          ++ concat (gmapQ getSpans x)

getSpan' :: (Data a) => a -> Ivl
getSpan' = fromJust . getSpan

debug x = trace (show x) x

type Ivl = (Int, Int) 
type Apps = [(Ivl, [Ivl])]

getApps :: forall a. (Data a) => a -> Apps
getApps ast = ((const []) `extQ` processExp) ast ++ recurse ast
 where 
  --processExp (InfixApp l (QVarOp ()) = (getSpan l, [getSpan r])
  processExp :: Exp SrcSpanInfo -> Apps
  processExp (InfixApp _ l o r) = case o of
    (QVarOp _ (UnQual _ (Symbol _ "."))) -> [(getSpan' l, [getSpan' r])]
    (QVarOp _ (UnQual _ (Symbol _ "$"))) -> [(getSpan' l, [getSpan' r])]
    _                                    -> [(getSpan' o, [getSpan' l, getSpan' r])]
  processExp (App _ l r)        = [(getSpan' l, [getSpan' r])]
  processExp _ = []
  recurse :: a -> Apps
  recurse = concat . gmapQ getApps

-- Takes a list of apps, and merges by left-hand-span, and therefore app lhs.
concatApps = map (second $ reverse . concat)
           . combineFstOn (\((x, _), _) -> x)
           . reverse


{-
addTypes :: String
         -> [(Int, Int)]
         -> IO [((Int, Int), Maybe String)]
addTypes code = mapM (\x -> liftM (x,) $ getType x)
 where  -}-- takes an IO function and returns a cached version

memoIO f = do
  v <- newMVar M.empty
  let f' x = do
        m <- readMVar v
        case M.lookup x m of
          Nothing -> do r <- f x
                        m <- takeMVar v
                        putMVar v (M.insert x r m)
                        return r
          Just r  -> return r
  return f'

{-memoCatch f = unsafePerformIO $ memoIO $ \x -> 
--  (liftM Just $ f x) --`Simple.catchError_fixed` (const $ return Nothing)

getType' = memoCatch I.typeOf

getType :: (Int, Int) -> String -> IO (Maybe String)
getType (f, t) = getType' . 
-}

-- Sorts and groups based on some derived, comparable property
groupSortOn f = groupBy ((==) `on` f) . sortBy (comparing f)

-- Eliminate redundant fst, concatenate together snds.
combineFstOn f = map (\xs -> (fst $ head xs, map snd xs)) . groupSortOn f

substr (f, t) = take (t - f) . drop (f - 1)

eitherToMaybe = either (const Nothing) Just

-- | Conditionally run an action, using a @Maybe a@ to decide.
whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (return ()) f mg

exprName (f, t) = "__" ++ show f ++ "_" ++ show t

subExpr :: String -> Ivl -> String
subExpr full r = "(" ++ substr r full ++ ")"

writeExprs :: String -> [Ivl] -> IO String
writeExprs full exprs = do
    exists <- doesFileExist filepath
    when (not exists) $ writeFile filepath code2
    return filename
  where filename = "M" ++ show (abs $ hash code)
        filepath = sourceDir </> filename <.> "hs"
        code = full ++ "\n"
            ++ concatMap (\r -> exprName r ++ " = "
                             ++ subExpr full r ++ "\n") exprs
        code2 = "module " ++ filename ++ " where\n" ++ code

{-
    mapm_ (\(s, xs) -> lifta2 
      (const $ (c.liftio . print :: [(linear double, linear double)] -> c.render ()))
      (spanline txt s) $ mapm (spanline txt) xs)
 -}

type TypeMap = M.Map Ivl (Type SrcSpanInfo)
type ColorMap = M.Map (Type SrcSpanInfo) (Int, Int, Int)
type HeightMap = IM.IntervalMap Int Double

isRight (Right _) = True
isRight _ = False

ivlHull (f1, t1) (f2, t2) = (min f1 f2, max t1 t2)

floatFst (f, s) = pure (,s) <*> f
floatSnd (f, s) = pure (f,) <*> s
floatBoth (f, s)  = liftA2 (,) (pure f) (pure s)

type STT = ST.State (String, M.Map String Char)

cannonicalType2 :: Type SrcSpanInfo -> Type SrcSpanInfo
cannonicalType2 t = ST.evalState (rec t) (['a'..], M.empty)
 where
  doVar :: String -> STT String
  doVar other = do
    (xs, m) <- ST.get
    case M.lookup other m of
      Just x -> return [x]
      Nothing -> ST.put (tail xs, M.insert other (head xs) m)
              >> return [head xs]

  removeVar :: String -> STT ()
  removeVar other = ST.modify (second $ M.delete other)

  doName (Ident l n)  = doVar n >>= \n' -> return (n, Ident l n')
  doName (Symbol l n) = doVar n >>= \n' -> return (n, Symbol l n')

  doBind (KindedVar l n k) = do
    (o, n') <- doName n
    return (o, KindedVar l n' k)

  doType :: Type SrcSpanInfo -> STT (Type SrcSpanInfo)
  doType (TyForall l bnds ctx t) = do
    bs <- maybe (return Nothing) (liftM Just . mapM doBind) bnds
    cx <- maybe (return Nothing) (liftM Just . rec) ctx
    t' <- rec t
    whenJust bs $ mapM_ (removeVar . fst)
    return $ TyForall l (liftM (map snd) bs) cx t'
  doType t = gmapM rec t

  preserveQ :: QName SrcSpanInfo -> STT (QName SrcSpanInfo)
  preserveQ = return

  rec :: (Data a, Typeable a) => a -> STT a
  rec = gmapM rec
   `extM` (\n -> doName n >>= return . snd)
   `extM` preserveQ
   `extM` doType

{-
cannonicalType :: Type l -> Type l
cannonicalType t = ST.evalState (rec t) (['a'..], M.empty)
 where
  doVar other = do
    (xs, m) <- ST.get
    case M.lookup other m of
      Just x -> return [x]
      Nothing -> ST.put (tail xs, M.insert other (head xs) m)
              >> return [head xs]

  removeVar other = ST.modify (second $ M.delete other)

  doName (Ident l n)  = doVar n >>= \n' -> return (n, Ident l n')
  doName (Symbol l n) = doVar n >>= \n' -> return (n, Symbol l n')

  doBind (KindedVar l n k) = do
    (o, n') <- doName n
    return (o, KindedVar l n' k)

--  rec :: Type l -> ST.State (String, M.Map String Char) (Type l)
  rec (TyForall l bnds ctx t) = do
    bs <- maybe (return Nothing) (liftM Just . mapM doBind) bnds
    cx <- maybe (return Nothing) (liftM Just . recCx) ctx
    t' <- rec t
    whenJust bs $ mapM_ (removeVar . fst)
    return $ TyForall l (liftM (map snd) bs) cx t'

  rec (TyFun l a b) = do
    a' <- rec a
    b' <- rec b
    return $ TyFun l a' b'

  rec (TyApp l a b) = do
    a' <- rec a
    b' <- rec b
    return $ TyApp l a' b'

  rec (TyVar l n) = doName n >>= return . TyVar l . snd

  rec (TyTuple l b ts) = mapM rec ts >>= return . TyTuple l b
  rec (TyList l t)   = rec t >>= return . TyList l
  rec (TyParen l t)  = rec t >>= return . TyParen l
  rec (TyKind l t k) = rec t >>= return . (flip (TyKind l) $ k)

  rec x = return x
  --TODO: make sure?
  --rec (TyCon l n) = 
  --rec (TyInfix l t n t') = 

  recCx (CxSingle l a) = recA a       >>= return . CxSingle l
  recCx (CxTuple l as) = mapM recA as >>= return . CxTuple l
  recCx (CxParen l c)  = recCx c      >>= return . CxParen l
  recCx x = return x -- CxEmpty

  recA (ClassA l n ts)  = mapM rec ts >>= return . ClassA l n
  recA (InfixA l a n b) = do
    a' <- rec a
    b' <- rec b
    return $ InfixA l a' n b'
  recA (IParam l p t) = rec t >>= return . IParam l p
  recA (EqualP l a b) = do
    a' <- rec a
    b' <- rec b
    return $ EqualP l a' b'
-}

getTypeMap :: String -> TaskChan -> Apps -> IO TypeMap
getTypeMap txt c apps = do
  rec <- liftM isRight . getType . recText $ words txt !! 0
  fs <- mapM (getExpr rec . fst) apps
  ps <- mapM (mapM (getExpr rec) . snd) apps
  return . M.fromList . map (second cannonicalType2) . catMaybes $ fs ++ concat ps
 where
  getExpr :: Bool -> Ivl -> IO (Maybe (Ivl, Type SrcSpanInfo))
  getExpr rec ivl
    = liftM (\t -> eitherToMaybe t >>=
        liftM (ivl,) . eitherToMaybe . parseResultToEither . parseType)
    . getType . (if rec then recText else id) $ subExpr txt ivl
  getType = interpret c "MyMain" . I.typeOf
  recText x = "let {" ++ txt ++ "} in " ++ x

getHeight :: (Double, Double) -> Ivl -> IM.IntervalMap Int Double -> Double
getHeight (_, y) (f, t) = minimum . (y:) . map snd . intersections (IM.Interval f t)

drawApps :: (Double, Double) -> String -> TaskChan -> Apps -> C.Render ()
drawApps pos txt c apps = do
    tm <- C.liftIO $ getTypeMap txt c apps
    let tc = M.fromList . (`zip` colours) . nub . sort
           . catMaybes . map (`M.lookup` tm) $ concatMap snd apps
    drawLegend tc
--    liftIO $ print tc
    liftIO $ print tm
--    liftIO $ print . map (first (`substr` txt)) $ M.toList tm
--    liftIO . print $ map (first (`substr` txt)) apps
    C.setLineWidth 1
    foldM_ (drawFunc tm tc) IM.empty $ reverse apps
 where
  drawLegend :: ColorMap -> C.Render ()
  drawLegend = zipWithM_ (\y (t, c) -> do
      setColor c
      relText 0 (fst pos, y) (prettyPrint t))
    [220, 240..] . M.toList
  drawFunc :: TypeMap -> ColorMap -> HeightMap -> (Ivl, [Ivl]) -> C.Render HeightMap
  drawFunc tm tc hm (func, params) = do
    fspan <- spanLine txt func
    foldM (drawParam fspan) hm 
        . catMaybes
        . map (\r -> liftM (func, r,) $ M.lookup r tm)
        $ reverse params
   where
    drawParam :: DLine -> HeightMap -> (Ivl, Ivl, Type SrcSpanInfo) -> C.Render HeightMap
    drawParam fs hm (fivl, pivl, t) = do
      ps <- spanLine txt pivl
      let height = getHeight pos (snd fivl, snd pivl) hm
          fs' = offset (fst pos, height) fs
          ps' = offset (fst pos, height) ps
          f = fs' `at` 1 ^-^ (5, 0)
          pl = ps' `at` 0 ^+^ (4, 0)
          pm = ps' `at` 0.5
          pr = ps' `at` 1 ^+^ (-4, 0)
--        l = magnitude (f ^-^ p)
{-        bez = bezierFromPoints
              [ f ^+^ (1, 0)
              , f ^+^ (1, -l * 0.5)
              , p ^+^ ((if p < f then l else -l) * 0.5, 0)
              , p] -}
      C.liftIO $ print (fivl, pivl, height)
      maybe (setColor ((0,0,0) :: DColor)) setColor (M.lookup t tc)
      move $ f ^+^ (3,0)
      C.arc (fst f) (snd f) 3 0 (2 * pi)
      C.fill
      if ' ' `elem` substr pivl txt then do
        draw $ bezierFromPoints [f, pr]
        draw $ bezierFromPoints [f, pl]
        draw $ bezierFromPoints [pl, pl ^+^ (0, 4)]
        draw $ bezierFromPoints [pr, pr ^+^ (0, 4)]
       else do
        draw $ bezierFromPoints [f, pm]
      C.stroke
      return $ IM.insert (uncurry IM.Interval $ pivl) (height - 10) hm
--      relText (0.5, 0.5) ((bez `at` 1) ^-^ (0, 10)) t

    {- draw . moveR (0, 10 * d) . expandR (2.0, 1.0) $
       singletonR (fs `at` (d * step) ^+^ pos)



     -}

colours :: [(Int, Int, Int)]
colours = cycle . map (\c -> (fromIntegral $ channelRed c, 
                              fromIntegral $ channelGreen c,
                              fromIntegral $ channelBlue c))
                $ map toSRGB24
  [ Color.lightgreen
  , Color.steelblue
  , Color.yellowgreen
  , Color.peru
  , Color.gold
  , Color.cyan
  , Color.lightsalmon
  ]
