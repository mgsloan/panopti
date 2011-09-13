{-# LANGUAGE FlexibleInstances, TemplateHaskell, TupleSections, TypeOperators,
ScopedTypeVariables, TypeFamilies #-}

import Simple

import Control.Applicative (liftA2, pure, (<*>))
import Control.Arrow ((&&&), (***), first, second)
import Control.Concurrent.MVar
import Control.Monad (liftM, zipWithM, zipWithM_, foldM, foldM_, join, when)
import qualified Control.Monad.State as ST
import Control.Monad.Trans (liftIO)
import Data.Char (isSpace)
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
import Data.IORef
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
import qualified System.Glib.MainLoop as G
import System.IO (writeFile)
import System.IO.Unsafe (unsafePerformIO)
import System.Directory (createDirectoryIfMissing, doesFileExist)

{- Things to work on

 * Parse identifier errors
 * Parse type errors
 * Application structure manipulation
 * User input of parameter-example table
 * Depiction of concrete value flow
 * Handle full source files
 * Live update of running haskell program
 * Detection of untenable type classes
 * Contextual Hoogle / paste buffers

 -}

type Ivl = (Int, Int) 
type Apps = [(Ivl, [Ivl])]
type TypeMap = M.Map Ivl (Type SrcSpanInfo)
type ColorMap = M.Map String (Int, Int, Int)

data State = State
  { _code :: String
  , _cursor :: Int
  , _parsed :: Maybe (String, Apps, TypeMap, ColorMap)
  , _chan :: TaskChan
  , _mouseCursor :: (Double, Double)
  , _timeout :: G.HandlerId
  , _selfRef :: IORef (KeyTable, State)
  }

$(mkLabels [''State])

modM :: Monad m => (b :-> a) -> (a -> a) -> b -> m b
modM l f = return . modify l f

setM :: Monad m => (b :-> a) -> a -> b -> m b
setM l x = return . set l x

lensed :: (f :-> a) -> (f :-> a') -> (a -> a') -> f -> f
lensed l l' f s = set l' (f $ get l s) s

modifyIORefM :: IORef a -> (a -> IO a) -> IO ()
modifyIORefM r f = readIORef r >>= f >>= writeIORef r

firstM f = (\(x, y) -> f x >>= \x' -> return (x', y))
secondM f = (\(x, y) -> f y >>= \y' -> return (x, y'))

updateParse :: TaskChan -> State -> IO State
updateParse c s = case parseDecl txt of
  ParseOk decl -> do
    let apps = concatApps $ getApps decl
    tm <- getTypeMap txt c apps
    let tc = M.fromList . (`zip` colours) . nub . sort . map (trim .  prettyPrint)
           . catMaybes . map (`M.lookup` tm) $ concatMap snd apps
    setM parsed (Just (txt, apps, tm, tc)) s
  _ -> setM parsed Nothing s
 where
  txt = get code s 

sourceDir = "source"

main = do
  createDirectoryIfMissing False sourceDir
  chan <- startGHCiServer [sourceDir] fail print
  (stateRef, loop) <- runToyState $ Toy
    { initialState =
        State "fibs = 0 : 1 : zipWith (+) fibs (tail fibs)" 0 Nothing chan (0, 220) 0 undefined
    , mouse   = const $ setM mouseCursor
    , key     = handleKey
    , display = handleDisplay
    , tick    = const return
    }
  modifyIORefM stateRef (secondM (setTimeout . set selfRef stateRef))
  loop

updateTime = 200

setTimeout s = do
  case get timeout s of
    0 -> return ()
    x -> G.timeoutRemove x
  time <- G.timeoutAdd (handler $ get selfRef s) updateTime
  return $ set timeout time s
 where
  handler ref = do
    (km, st) <- readIORef ref
    st' <- updateParse (get chan s) st
    writeIORef ref (km, st')
    return False

handleKey :: Bool -> Either [Char] Char -> State -> IO State
handleKey True (Right k) (State xs ix p c m t s) =
  setTimeout $ State (pre ++ (k : post)) (ix + 1) p c m t s
 where 
  (pre, post) = splitAt ix xs

handleKey True (Left k) s@(State xs ix _ c _ _ _) = (case k of
  "Left"  -> modM cursor (max 0 . subtract 1)
  "Right" -> modM cursor (min endPos . (+1))
  "Home"  -> setM cursor 0
  "End"   -> setM cursor endPos
  "BackSpace" -> modM cursor (max 0 . subtract 1)
               . set code (delIx (ix - 1))
  "Delete" -> setM code (delIx ix)
  "Escape" -> const $ error "User escape"
  _ -> return) s >>= setTimeout
 where
  endPos = length xs
  delIx i | (pre, (_:post)) <- splitAt i xs = pre ++ post
          | otherwise = xs

handleKey _ _ s = return s

handleDisplay :: IPnt -> IRect -> State -> C.Render State
handleDisplay _ (tl, br) s@(State txt ix p c (_, ypos) _ _) = do
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
  
  whenJust p (\t -> drawApps (textPos ^-^ (0, 20)) c t)
  return s

reverseLinear :: Linear Double -> Linear Double
reverseLinear = (`compose` (Linear 1 0 :: Linear Double))

spanLine :: String -> Ivl -> C.Render DLine
spanLine txt (f, t) = liftM (mapT reverseLinear . rside 2 . expandR 2) 
                    $ textRect txt (f - 1) (t - 1)

colSpan :: SrcSpan -> Ivl
colSpan = (srcSpanStartColumn &&& srcSpanEndColumn)

getSpan :: (Data a) => a -> Maybe SrcSpan
getSpan = listToMaybe . catMaybes
        . gmapQ (const Nothing `extQ` (Just . srcInfoSpan))

getSpan' :: (Data a) => a -> Ivl
getSpan' = colSpan . fromJust . getSpan

debug x = trace (show x) x

getApps :: forall a. (Data a) => a -> Apps
getApps ast = ((const []) `extQ` processExp) ast ++ recurse ast
 where 
  processExp :: Exp SrcSpanInfo -> Apps
  processExp (InfixApp _ l o r) = case o of
    (QVarOp _ (UnQual _ (Symbol _ "."))) -> [(getSpan' l, [getSpan' r])]
    (QVarOp _ (UnQual _ (Symbol _ "$"))) -> [(getSpan' l, [getSpan' r])]
    _                                    -> [(getSpan' o, [getSpan' l, getSpan' r])]
  processExp (LeftSection _ l o)  = [(getSpan' o, [getSpan' l])]
  processExp (RightSection _ o r) = [(getSpan' o, [getSpan' r])] --TODO: uhhh
  processExp (App _ l r)          = [(getSpan' l, [getSpan' r])]
  processExp _ = []
  recurse :: a -> Apps
  recurse = concat . gmapQ getApps

-- Takes a list of apps, and merges by left-hand-span, and therefore app lhs.
concatApps = map (second $ reverse . concat)
           . combineFstOn (\((x, _), _) -> x)
           . reverse

-- takes an IO function and returns a cached version
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

-- Sorts and groups based on some derived, comparable property
groupSortOn f = groupBy ((==) `on` f) . sortBy (comparing f)

-- Eliminate redundant fst, concatenate together snds.
combineFstOn f = map (\xs -> (fst $ head xs, map snd xs)) . groupSortOn f

substr (f, t) = take (t - f) . drop (f - 1)

subst (f, t) xs ys = (take (f - 1) ys) ++ xs ++ drop (t - 1) ys

eitherToMaybe = either (const Nothing) Just

isRight (Right _) = True
isRight _ = False

trim = f . f
 where f = reverse . dropWhile isSpace

ivlHull (f1, t1) (f2, t2) = (min f1 f2, max t1 t2)

type HeightMap = IM.IntervalMap Int Double

getTypeMap :: String -> TaskChan -> Apps -> IO TypeMap
getTypeMap txt c apps = do
  rec <- liftM isRight . getType . recText1 $ words txt !! 0
  fs <- mapM (getExpr rec . fst) apps
  ps <- mapM (mapM (getExpr rec) . snd) apps
  return . M.fromList . map (second cannonicalType) . catMaybes $ fs ++ concat ps
 where
  getExpr :: Bool -> Ivl -> IO (Maybe (Ivl, Type SrcSpanInfo))
  getExpr rec ivl
    = liftM (\t -> eitherToMaybe t >>=
        liftM (ivl,) . eitherToMaybe . parseResultToEither . parseType)
    . getType . (if rec then recText2 ivl else id) $ "(" ++ substr ivl txt ++ ")"
  getType = interpret c "MyMain" . I.typeOf
  recText1 x = "let {" ++ txt ++ "} in " ++ x
  recText2 ivl x = "let {" ++ subst ivl "__e" txt ++ "; __e = " ++ x ++ "} in __e"

getHeight :: (Double, Double) -> Ivl -> IM.IntervalMap Int Double -> Double
getHeight (_, y) (f, t) = minimum . (y:) . map snd . intersections (IM.Interval f t)

drawApps :: (Double, Double) -> TaskChan 
         -> (String, Apps, TypeMap, ColorMap)  -> C.Render ()
drawApps pos c (txt, apps, tm, tc) = do
    drawLegend tc
    foldM_ (drawFunc tm tc) IM.empty $ reverse apps
 where
  drawLegend :: ColorMap -> C.Render ()
  drawLegend = zipWithM_ (\y (t, c) -> do
      setColor c
      relText 0 (fst pos, y) t)
    [220, 240..] . M.toList
  drawFunc :: TypeMap -> ColorMap -> HeightMap -> (Ivl, [Ivl]) -> C.Render HeightMap
  drawFunc tm tc hm (func, params) = do
    fspan <- spanLine txt func
    foldM (\h -> drawFunc tm tc h . (, [])) hm 
          $ filter (not . (`elem` (map fst apps))) params
    let pts = liftM (map cannonicalType . drop (length params + 1) . splitFunc)
            $ M.lookup func tm
    foldM (drawParam fspan) hm 
        . (++ map (\t -> (func, Nothing, Just t)) (maybe [] id pts))
        . map (\r -> (func, Just r,) $ M.lookup r tm)
        $ reverse params
   where
    drawParam :: DLine -> HeightMap -> (Ivl, Maybe Ivl, Maybe (Type SrcSpanInfo)) 
              -> C.Render HeightMap
    drawParam fs hm (fivl, pivl', t) = do
      maybe (setColor ((0,0,0) :: DColor)) setColor 
            (t >>= (`M.lookup` tc) . trim . prettyPrint)
      move $ f ^+^ (3,0)
      C.arc (fst f) (snd f) 3 0 (2 * pi)

      case pivl' of
        Just pivl -> do
          C.fill
          C.setLineWidth 1
          ps <- spanLine txt pivl
          let ps' = offset (fst pos, height) ps
              pl = ps' `at` 0 ^+^ (4, 0)
              pm = ps' `at` 0.5
              pr = ps' `at` 1 ^+^ (-4, 0)
          if ' ' `elem` substr pivl txt then do
            draw $ bezierFromPoints [f, pr]
            draw $ bezierFromPoints [f, pl]
            draw $ bezierFromPoints [pl, pl ^+^ (0, 4)]
            draw $ bezierFromPoints [pr, pr ^+^ (0, 4)]
          else do
            draw $ bezierFromPoints [f, pm]
          C.stroke
        Nothing -> do
          C.setLineWidth 2
          C.stroke

      return $ IM.insert (uncurry IM.Interval $ ivl) (height - 10) hm
     where
      height = getHeight pos (snd fivl, maybe (snd fivl) snd pivl') hm
      ivl = maybe (snd fivl, snd fivl) (ivlHull fivl) pivl'
      fs' = offset (fst pos, height) fs
      f = fs' `at` 1 ^-^ (5, 0)
      
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

-- | Conditionally run an action, using a @Maybe a@ to decide.
whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (return ()) f mg

-- | Conditionally run an action, and yield result.
maybeM :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
maybeM f = maybe (return Nothing) (liftM Just . f)

type STT = ST.State (String, M.Map String Char)

cannonicalType :: Type SrcSpanInfo -> Type SrcSpanInfo
cannonicalType t = ST.evalState (rec t) (['a'..], M.empty)
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
    bs <- maybeM (mapM doBind) bnds
    cx <- maybeM rec ctx
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

splitFunc ty = case ty of
    (TyForall l bnds (Just ctx) t) -> rec t . map (\a -> (vars a, a)) $ fromContext ctx
    t -> rec t []
 where
  rec (TyFun l a b) as = toForall a as : rec b as
  rec t as = [toForall t as]

  toForall t [] = t
  toForall t as = TyForall l Nothing 
    (Just . toContext l . map snd $ filter (overlaps $ vars t) as) t
   where l = noInfoSpan . fromJust $ getSpan t

  overlaps xs (ys, _) = any (`elem` ys) xs

  vars :: (Data a) => a -> [String]
  vars = (concat . gmapQ vars) `extQ` getVar

  getVar :: Name SrcSpanInfo -> [String]
  getVar (Ident _ n) = [n]
  getVar (Symbol _ n) = [n]

  fromContext (CxSingle l a) = [a]
  fromContext (CxTuple l as) = as
  fromContext (CxParen l c) = fromContext c
  fromContext _ = []

  toContext l [] = CxEmpty l
  toContext l [a] = CxSingle l a
  toContext l as = CxTuple l as
