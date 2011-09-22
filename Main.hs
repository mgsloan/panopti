{-# LANGUAGE DoAndIfThenElse, ScopedTypeVariables, TemplateHaskell, TupleSections #-}

import Simple
import Utils

import Prelude hiding ((.))

import Control.Arrow ((***), (&&&), first, second)
import Control.Category ((.))
import Control.Monad (liftM, zipWithM_, foldM, foldM_, msum)
import Data.Char (isSpace, toLower)
import Data.Colour.SRGB (channelRed, channelGreen, channelBlue, toSRGB24, sRGB24read)
import qualified Data.Colour.Names as Color
import Data.Curve hiding ((<.>))
import Data.Curve.Util (mapT, foldT)
import Data.Data
import Data.Generics.Aliases
import Data.IntervalMap.FingerTree as IM
import Data.IORef
import Data.Label
import Data.List (sort, findIndex, groupBy, maximumBy, sortBy, find)
import Data.Maybe
import qualified Data.Map as M
import Data.Ord (comparing)
import Debug.Trace
import Graphics.ToyFramework
import qualified Graphics.Rendering.Cairo as C
import Language.Haskell.Exts (prettyPrint, Type(..), ParseResult(..), SrcLoc(..), Context)
import qualified Language.Haskell.Exts.Annotated as A
import qualified Language.Haskell.Interpreter as I
import qualified System.Glib.MainLoop as G
import System.Directory (createDirectoryIfMissing)

{- Things to work on

 * Parse type errors
 * Application structure manipulation
 * User input of parameter-example table
 * Depiction of concrete value flow
 * Handle full source files
 * Live update of running haskell program
 * Detection of untenable type classes
 * Contextual Hoogle / paste buffers
 * Illustration of which types have changed with edits
 * Mode for type information relevant to selection
 * Visualization of parallel possibilities for expression tree
   - Depiction of seminal style autofix possibilities
   - Depiction of unmerged collaborative edits
 * Application of visual pretty printing / pretty editing

 -}

type Apps = [(Ivl, Ivl, [Ivl])]
type ColorMap = M.Map String (Int, Int, Int)
type GapMap = IM.IntervalMap Int Int
type TypeMap = M.Map Ivl Type
type Errors = [(Ivl, String)]

data Results = Results
  { _source :: String 
  , _partial_source :: String
  , _decl :: A.Decl A.SrcSpanInfo
  , _appIvls :: Apps
  , _typeMap :: TypeMap
  , _colorMap :: ColorMap
  , _errors :: Errors
  }

data UserMode = Normal | Selection Bool

data State = State
  { _code :: String
  , _cursor :: (Int, Int)
  , _user :: UserMode
  , _parsed :: Maybe Results
  , _chan :: TaskChan
  , _mouseCursor :: (Double, Double)
  , _timeout :: G.HandlerId
  , _selfRef :: IORef (KeyTable, State)
  , _pastes :: [(String, Maybe String)]
  }

$(mkLabels [''Results, ''State])

-- TODO: static import considerations
sourceDir = "source"

main = do
  createDirectoryIfMissing False sourceDir
  chan <- startGHCiServer [sourceDir] fail print
  (stateRef, loop) <- runToyState $ Toy
    { initialState =
        State "fibs = 0 : 1 : zipWith (+) fibs (tail fibs)"
              0 Normal Nothing chan (0, 220) 0 undefined []
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
    st' <- update st
    writeIORef ref (km, st')
    return False

keyHeld key = liftM (maybe False (\(a,_,_)->a) . (M.lookup key) . fst)
            . readIORef . get selfRef

eitherHeld key s = do
  l <- keyHeld (key ++ "_L") s
  r <- keyHeld (key ++ "_R") s
  return (l || r)

addPaste :: String -> State -> IO State
addPaste x s = getType (get chan s) x
           >>= \t -> return $ modify pastes ((x, rightToMaybe t):) s

getSelection :: State -> String
getSelection s = substr (get cursor s) (get code s)

--parentOfSpan :: (Data a) => Ivl -> a -> Maybe (ExpS, Int)

handleKey :: Bool -> Either [Char] Char -> State -> IO State
handleKey True (Left "Escape") _ = error "User escape"

handleKey True (Right k) s@(State xs ix Normal _ _ _ _ _ _) = do
  ctrl <- eitherHeld "Control" s
  case (ctrl, k) of
    (True, ' ') -> return $ selectAST ix s
    _ -> setTimeout
       . set code (pre ++ (k : post))
       $ set cursor (ix ^+^ 1) s
 where
  (pre, post) = splitAt (fst ix) xs

handleKey True (Left k) s@(State xs _ Normal _ _ _ _ _ _) = do
  ctrl <- eitherHeld "Control" s
  let ix = get curs s
      lr = ((if ctrl then wordIx (-2) ix xs
                     else max 0 $ ix - 1), ix)
      rr = (ix, (if ctrl then wordIx 1 ix xs
                         else min endPos $ ix + 1))
  (case k of
    "Left"  -> setM curs (fst lr)
    "Right" -> setM curs (snd rr)
    "Home"  -> setM curs 0
    "End"   -> setM curs endPos
    "BackSpace" -> setM curs (fst lr)
                 . modify code (subst lr "")
    "Delete" -> modM code (subst rr "")
    _ -> return) s >>= setTimeout
 where
  curs = lens (fst . get cursor) (\a -> set cursor (a, a))
  endPos = length xs

handleKey True (Right k) s@(State xs ix (Selection _) _ _ _ _ _ _) = do
  shift <- eitherHeld "Shift" s
  case toLower k of
    'y' -> update =<< addPaste (getSelection s) s
    'd' -> update =<< addPaste (getSelection s)
      ( set cursor (fst ix, fst ix)
      . set user Normal
      $ modify code (subst ix "") s)
    k -> maybe (return s) (if shift then update else return)
      (case k of
        'k' -> liftM (\((ivl, _), _) -> set cursor ivl s) parent
        'j' -> maySetCursor $ childSpan' (ix, 0)
        'h' -> parent >>= (\(p, _) -> (if shift then moveIx else mutateIx)
                                      (wrapped (fst p) . (subtract 1)) p)
        'l' -> parent >>= (\(p, _) -> (if shift then moveIx else mutateIx)
                                      (wrapped (fst p) . (+1)) p)
        _ -> Nothing)
 where
  wrapped c = (`mod` (maybe 1 id $ arity c))
  --TODO: use for parameter introduction
--  arity x = subtract 1 . length . splitFunc . fromJust
--          . M.lookup x $ getRes typeMap s
  arity x = liftM (length . thd3) $ find ((==x).snd3) (getRes appIvls s)
  maySetCursor = liftM (\ivl -> set cursor ivl s)
  childSpan' x = childSpan x (getRes appIvls s)
  mutateIx f p = maySetCursor . childSpan' $ second f p
  moveIx f p = do
    from <- childSpan' p
    to <- childSpan' $ second f p
    let (f', _, s') = swapIxs from to s
    return $ set cursor f' s'
  swapIxs a b
    | b < a = (\(x, y, xs) -> (y, x, xs)) . swapIxs b a 
    | otherwise = let b' = mapT ((+ (ivlWidth b - ivlWidth a))) b in
                ( (id &&& (+ivlWidth a)) $ fst b'
                , (id &&& (+ivlWidth b)) $ fst a
                , )
                . modify code (\xs -> subst b' (substr a xs)
                                    $ subst a  (substr b xs) xs)
  parent = do
    cur@(_, ivl) <- atSpan' ix s
    par@(ivl', _) <- parentSpan ivl (getRes appIvls s)
    return (par, cur)

handleKey _ _ s = return s

getRes f = get (f . maybeLens . parsed)

atSpan' :: Ivl -> State -> Maybe (ExpS, Ivl)
atSpan' ivl = liftM (id &&& getSpan') . atSpan ivl . getRes decl

selectAST :: Ivl -> State -> State
selectAST ivl st = maybe st ((`selectIt`st) . snd) $ atSpan' ivl st
 where
  selectIt c = set user (Selection True) . set cursor c

--  curs' e = liftM fst . parentSpan (getSpan' e) $ get (appIvls . maybeLens . parsed) st

childSpan :: (Ivl, Int) -> Apps -> Maybe Ivl
childSpan (ivl, ix) as = (`atMay` ix) . reverse . thd3 =<<
  ( listToMaybe . sortBy (comparing (ivlWidth . snd3))
  $ filter ((`icontains` ivl) . snd3) as)

parentSpan :: Ivl -> Apps -> Maybe (Ivl, Int)
parentSpan ivl = fmap snd . listToMaybe . sortBy (comparing fst) . catMaybes . map helper
 where
  helper (_, x, xs) = findIndex (`icontains` ivl) xs'
                  >>= \ix -> return (ivlWidth $ xs' !! ix, (x, ix))
   where
    xs' = reverse xs


handleDisplay :: IPnt -> IRect -> State -> C.Render State
handleDisplay _ (tl, br) s@(State txt ix user p c (_, ypos) _ _ _) = do
  let textPos = (50.5, 200.5)
      height = (fromIntegral . snd $ br ^-^ tl) * 0.5
      astPos = textPos ^+^ (0.0, ypos - height)

  C.setLineWidth 1
  C.setFontSize 20
  C.selectFontFace "monospace" C.FontSlantNormal C.FontWeightNormal

  C.paint
  setColor $ readColor "242424"
  C.fill

  setColor $ readColor "656565"
  (C.FontExtents a d _ w _) <- C.fontExtents
  let f = fromIntegral $ fst ix
      t = fromIntegral $ snd ix
  draw . moveR (textPos ^-^ (0, 12)) . expandR (2,2)
       $ boundPoints [(w * f, a), (w * t, -d)]
  C.fill
  
  whenJust p (\t -> drawTypes (textPos ^-^ (0, 25)) c t)

  setColor $ readColor "f6f3e8"
  move textPos
  C.showText txt
  C.fill

  move (50.5, 400.5)
  mapM (\txt -> C.showText (fst txt) >> C.relMoveTo 0 20) (get pastes s)

  return s

reverseLinear :: Linear Double -> Linear Double
reverseLinear = (`compose` (Linear 1 0 :: Linear Double))

spanLine :: String -> Ivl -> C.Render DLine
spanLine txt (f, t) = liftM (mapT reverseLinear . rside 2 . expandR 2) 
                    $ textRect txt f t

type HeightMap = IM.IntervalMap Int Double

getHeight :: DPoint -> Ivl -> IM.IntervalMap Int Double -> Double
getHeight (_, y) (f, t) = minimum . (y:) . map snd . intersections (IM.Interval f t)

-- TODO: consider selecting the primitive types based on repetition counts /
-- some fitness factor / etc.

drawLTR :: (a -> C.Render DRect) -> [a] -> C.Render DRect
drawLTR f = liftM (foldl1 Data.Curve.union)
          . mapM (\x -> do
              r <- f x
              move $ rside 1 r `at` 0.5
              return r)

surround :: String -> String -> DRect -> C.Render DRect
surround pre post r = do
  from <- liftM (((rside 3 r `at` 0.5) ^-^) . second (/(-2))) $ textSize pre  
  move from
  C.showText pre
  psz <- textSize post
  let to = (rside 1 r `at` 0.5) ^+^ (second (/2) psz)
  move $ to ^-^ (fst psz, 0)
  C.showText post
  return $ boundPoints [from, to] `Data.Curve.union` r

drawTypes :: DPoint -> TaskChan -> Results -> C.Render ()
drawTypes pos c (Results txt ftxt _ apps tm tc es) = do
  let prims = onub . sort . map (prettyPrint . cannonicalType)
            . concatMap splitType $ M.elems tm
      cmap = M.fromList $ zip prims colours
  mapM_ (drawType cmap) (M.toList tm)
  C.setFontSize 20
 where
  drawType :: M.Map String (Int, Int, Int) -> (Ivl, Type) -> C.Render DRect
  drawType m (ivl, t) = do
    C.setFontSize 20
    sp <- spanLine txt ivl
    C.setFontSize 10
    move $ (pos ^+^ sp `at` 0.5)
    rec [] t
   where
    rec :: Context -> Type -> C.Render DRect
    rec ctx t@(TyFun _ _) = do
      rect <- drawLTR (rec ctx) $ splitFunc t
      let line = toBezier $ offset (0, -4) $ rside 0 rect
      draw line
      drawArrow 4 1 0.8 line
      C.stroke
      return rect
    rec ctx (TyTuple _ xs) = do
      rect <- drawLTR (rec ctx) xs
      surround "(" ")" rect
    rec ctx (TyList t) = do
      rect <- rec ctx t
      surround "[" "]" rect
    rec ctx t@(TyVar _) = prim ctx t
    rec ctx t@(TyCon _) = prim ctx t
    rec ctx (TyForall _ ctx' t) = rec (ctx' ++ ctx) t
    rec ctx t = fallback t
    prim ctx t =
      case M.lookup (prettyPrint $ addCtx ctx t) m of
        Just c -> do
          pnt <- liftM (^+^ (6, 0)) $ C.getCurrentPoint
          setColor c
          move pnt
          C.arc (fst pnt) (snd pnt) 3 0 (2 * pi)
          bnds <- pathBounds
          C.fill
          return bnds
        Nothing -> fallback t
    fallback t = do
      let txt = prettyPrint t
      sz <- textSize txt
      fr <- C.getCurrentPoint
      relMove (0, snd sz / 2)
      return $ boundPoints [fr, fr ^+^ (fst sz, 0)]

drawApps :: DPoint -> TaskChan -> Results -> C.Render ()
drawApps pos c (Results txt ftxt _ apps tm tc es) = do
  mapM_ drawError es
  drawGaps
  drawLegend tc
  foldM_ (drawFunc tm tc) IM.empty $ reverse apps
 where
  drawLegend :: ColorMap -> C.Render ()
  drawLegend = zipWithM_ (\y (t, c) -> do
      setColor c
      relText 0 (fst pos, y) t)
    [220, 240..] . M.toList
  drawGaps :: C.Render ()
  drawGaps = mapM_ drawGap . map (mapT fst . (head &&& last))
           . pairGroupBy adjacent . filter ((==gapChar) . snd) $ zip [0..] ftxt
   where
    adjacent (i, _) (i', _) = i + 1 == i'
    drawGap (f, t) = do
      rect <- textRect txt f (t + 1)
      setColor ((0.5, 0.2, 0.2) :: DColor)
      draw (moveR (pos ^+^ (0, 24)) rect)
      C.fill
  drawError :: (Ivl, String) -> C.Render ()
  drawError ((f, t), err) = do
    rect <- textRect txt f t
    setColor ((1.0, 0.2, 0.2) :: DColor)
    let (x, y) = (middle $ moveR pos rect)
    C.moveTo x (y + x)
    C.setFontSize 10
    C.showText err
    C.setFontSize 20
    C.lineTo x y
    C.stroke
  drawFunc :: TypeMap -> ColorMap -> HeightMap -> (Ivl, Ivl, [Ivl]) -> C.Render HeightMap
  drawFunc tm tc hm (func, _, params) = do
    fspan <- spanLine txt func
    foldM (\h -> drawFunc tm tc h . (, undefined, [])) hm 
          $ filter (not . (`elem` (map fst3 apps))) params
    let pts = liftM (debug . map cannonicalType . drop (length params + 1) . splitFunc .  debug)
            $ M.lookup func tm
    foldM (drawParam fspan) hm 
        . (++ map (\t -> (func, Nothing, Just t)) (maybe [] id pts))
        . map (\r -> (func, Just r,) $ M.lookup r tm)
        $ reverse params
   where
    drawParam :: DLine -> HeightMap -> (Ivl, Maybe Ivl, Maybe Type) 
              -> C.Render HeightMap
    drawParam fs hm (fivl, pivl', t) = do
      maybe (setColor ((1,1,1) :: DColor)) setColor 
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
      

update :: State -> IO State
update s = (parsed' $ partialParse A.parseDecl txt) >>= flip (setM parsed) s
 where
  txt = get code s 
  parsed' (Just (decl, txt', errs)) = do
    let apps = concatApps $ getApps decl
    tm <- getTypeMap txt' (get chan s) apps
--    mapM (print . (`atSpan` decl)) $ M.keys tm
    let tc = getColorMap tm apps
    return (Just $ Results txt txt' decl apps tm tc errs)
  parsed' Nothing = return Nothing


ivlWidth :: (Int, Int) -> Int
ivlWidth = foldT subtract

partialParse :: (String -> ParseResult a) -> String -> Maybe (a, String, Errors)
partialParse f txt = rec Nothing txt
 where
  rec prior txt = case f txt of
    ParseOk decl -> Just (decl, txt, [])
    ParseFailed l err -> trace err $ if (Just l == prior) then Nothing else
      case (lexify txt) of
        ParseOk xs -> case findIndex ((`spanContains` l) . A.loc) xs of
          Just ix -> msum 
                   [ process l err (fr, to)
                   | fr <- map (subtract 1 . A.srcSpanStartColumn . A.loc . (xs !!))
                               [ix, ix - 1 .. max 0 (ix - 9)]
                   , to <- map (subtract 1 . A.srcSpanEndColumn   . A.loc . (xs !!))
                               [ix .. min (ix + 2) (length xs - 1)]]
          Nothing -> Nothing
        ParseFailed l err -> trace err Nothing
  process l e i = liftM (\(a, s, es)->(a, s, err:es))
                . rec (Just l) $ subst i (replicate (ivlWidth i) gapChar) txt
    where err = ((srcColumn l, srcColumn l), e)
      
gapChar = '\x180E'

imFromList :: (Ord a) => [(IM.Interval a, b)] -> IM.IntervalMap a b
imFromList = foldl (\m p -> (uncurry IM.insert) p m) IM.empty


getApps :: forall a. (Data a) => a -> Apps
getApps ast = ((const []) `extQ` processExp) ast ++ recurse ast
 where
  processExp :: ExpS -> Apps
  processExp (A.InfixApp s l o r) = case o of
    (A.QVarOp _ (A.UnQual _ (A.Symbol _ "."))) -> [(getSpan' l, cs s, [getSpan' r])]
    (A.QVarOp _ (A.UnQual _ (A.Symbol _ "$"))) -> [(getSpan' l, cs s, [getSpan' r])]
    _                                          -> [(getSpan' o, cs s, [getSpan' l, getSpan' r])]
  processExp (A.LeftSection s l o)  = [(getSpan' o, cs s, [getSpan' l])]
  processExp (A.RightSection s o r) = [(getSpan' o, cs s, [getSpan' r])] --TODO: uhhh
  processExp (A.App s l r)          = [(getSpan' l, cs s, [getSpan' r])]
  processExp _ = []
  recurse :: a -> Apps
  recurse = concat . gmapQ getApps
  cs = colSpan . A.srcInfoSpan

-- Takes a list of apps, and merges by left-hand-span, and therefore app lhs.
concatApps :: Apps -> Apps
concatApps = map process . groupSortOn (\((x, _), _, _) -> x) . reverse
  where
   process xs = (h, l, reverse . concat $ map thd3 xs)
    where
     ((h,_,_),(_,l,_)) = (head &&& last) $ sortBy (comparing (ivlWidth . snd3)) xs

preferOk = rightToMaybe . parseResultToEither

getType c = interpret c "MyMain" . I.typeOf . deMongol

-- Actually unecessary
deMongol = map (\c -> if c == gapChar then ' ' else c)

getTypeMap :: String -> TaskChan -> Apps -> IO TypeMap
getTypeMap txt c apps = do
  rec <- liftM isRight . getType c . recText1 $ words txt !! 0
  fs <- mapM (getExpr rec . fst3) apps
  ps <- mapM (mapM (getExpr rec) . thd3) apps
  return . M.fromList . map (second cannonicalType) . catMaybes $ fs ++ concat ps
 where
  getExpr :: Bool -> Ivl -> IO (Maybe (Ivl, Type))
  getExpr rec ivl
    = liftM (\t -> rightToMaybe t >>= liftM (ivl,) . preferOk . parseType)
    . getType c . (if rec then recText2 ivl else id) $ "(" ++ substr ivl txt ++ ")"
  recText1 x = "let {" ++ txt ++ "} in " ++ x
  recText2 ivl x = "let {" ++ subst ivl "__e" txt ++ "; __e = " ++ x ++ "} in __e"


getColorMap :: TypeMap -> Apps -> ColorMap
getColorMap tm = M.fromList . (`zip` colours)
               . onub . sort . map (trim . prettyPrint)
               . catMaybes . map (`M.lookup` tm) . concatMap thd3

colours :: [(Int, Int, Int)]
colours = cycle $ map readColor
  [ "8f8f8f"
  , "e5786d"
  , "95e454"
  , "8ac6f2"
  , "e5786d"
  , "e7f6da"
  ]

readColor = toIColor . toSRGB24 . sRGB24read

--toIColor :: RGB Double -> (Int, Int, Int)
toIColor c =
  (fromIntegral $ channelRed   c, 
   fromIntegral $ channelGreen c,
   fromIntegral $ channelBlue  c)
