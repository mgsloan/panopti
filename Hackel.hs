{-# LANGUAGE FlexibleInstances, TemplateHaskell, TupleSections, TypeOperators,
  ScopedTypeVariables, TypeFamilies, ParallelListComp #-}

import Simple
import Utils

import Prelude hiding ((.))

import Control.Arrow ((&&&), second)
import Control.Category ((.))
import Control.Monad (liftM, zipWithM_, foldM, foldM_, msum)
import Data.Char (isSpace)
import Data.Colour.SRGB (channelRed, channelGreen, channelBlue, toSRGB24, sRGB24read)
import qualified Data.Colour.Names as Color
import Data.Curve hiding ((<.>))
import Data.Curve.Util (mapT, foldT)
import Data.Data
import Data.Generics.Aliases
import Data.IntervalMap.FingerTree as IM
import Data.IORef
import Data.Label
import Data.List (sort, findIndex, groupBy, sortBy)
import Data.Maybe
import qualified Data.Map as M
import Data.Ord (comparing)
import Debug.Trace
import Graphics.ToyFramework
import qualified Graphics.Rendering.Cairo as C
import Language.Haskell.Exts.Annotated
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

 -}

type Apps = [(Ivl, [Ivl])]
type ColorMap = M.Map String (Int, Int, Int)
type GapMap = IM.IntervalMap Int Int
type TypeMap = M.Map Ivl (Type SrcSpanInfo)
type Errors = [(Ivl, String)]


data Results = Results
  { _source :: String 
  , _partial_source :: String
  , _decl :: Decl SrcSpanInfo
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
              0 Normal Nothing chan (0, 220) 0 undefined
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
    st' <- update (get chan s) st
    writeIORef ref (km, st')
    return False

keyHeld key = liftM (maybe False (\(a,_,_)->a) . (M.lookup key) . fst)
            . readIORef . get selfRef

controlHeld s = do
  l <- keyHeld "Control_L" s
  r <- keyHeld "Control_R" s
  return (l || r)

allIvls :: Apps -> [Ivl]
allIvls xs = onub . sort $ map fst xs ++ concatMap snd xs

selectAST :: Ivl -> State -> IO State
selectAST ivl st = 
  case atSpan ivl (get decl r) of
    (Just e) -> setM user (Selection True) $ set cursor ivl st
    Nothing -> return st
 where
  r = get (maybeLens . parsed) st

parentSpan :: Ivl -> Apps -> Maybe (Ivl, Int)
parentSpan ivl = listToMaybe . catMaybes 
               . map (secondM $ findIndex (`icontains` ivl))

--parentOfSpan :: (Data a) => Ivl -> a -> Maybe (ExpS, Int)

handleKey :: Bool -> Either [Char] Char -> State -> IO State
handleKey True (Left "Escape") _ = error "User escape"

handleKey True (Right k) s@(State xs ix Normal _ _ _ _ _f) = do
  ctrl <- controlHeld s
  case (ctrl, k) of
    (True, ' ') -> selectAST ix s
    _ -> setTimeout
       . set code (pre ++ (k : post))
       $ set cursor (ix ^+^ 1) s
 where
  (pre, post) = splitAt (fst ix) xs

handleKey True (Left k) s@(State xs _ Normal _ _ _ _ _) = do
  ctrl <- controlHeld s
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

handleKey True (Right k) s@(State xs _ (Selection _) _ _ _ _ _) =
  setM user Normal s
--  case k of
--    'h' ->
--    'l' ->

handleKey _ _ s = return s

handleDisplay :: IPnt -> IRect -> State -> C.Render State
handleDisplay _ (tl, br) s@(State txt ix user p c (_, ypos) _ _) = do
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
  
  whenJust p (\t -> drawApps (textPos ^-^ (0, 20)) c t)

  setColor $ readColor "f6f3e8"
  move textPos
  C.showText txt
  C.fill

  return s

reverseLinear :: Linear Double -> Linear Double
reverseLinear = (`compose` (Linear 1 0 :: Linear Double))

spanLine :: String -> Ivl -> C.Render DLine
spanLine txt (f, t) = liftM (mapT reverseLinear . rside 2 . expandR 2) 
                    $ textRect txt f t

type HeightMap = IM.IntervalMap Int Double

getHeight :: (Double, Double) -> Ivl -> IM.IntervalMap Int Double -> Double
getHeight (_, y) (f, t) = minimum . (y:) . map snd . intersections (IM.Interval f t)

drawApps :: (Double, Double) -> TaskChan -> Results  -> C.Render ()
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
      

update :: TaskChan -> State -> IO State
update c s = (parsed' $ partialParse parseDecl txt) >>= flip (setM parsed) s
 where
  txt = get code s 
  parsed' (Just (decl, txt', errs)) = do
    let apps = concatApps $ getApps decl
    tm <- getTypeMap txt' c apps
--    mapM (print . (`atSpan` decl)) $ M.keys tm
    let tc = getColorMap tm apps
    return (Just $ Results txt txt' decl apps tm tc errs)
  parsed' Nothing = return Nothing


ivlWidth :: (Int, Int) -> Int
ivlWidth = foldT subtract

partialParse :: (String -> ParseResult a) -> String -> Maybe (a, String, Errors)
partialParse f txt = rec Nothing txt
 where
  rec prior txt = case f (debug txt) of
    ParseOk decl -> Just (decl, txt, [])
    ParseFailed l err -> trace err $ if (Just l == prior) then Nothing else
      case (lexify txt) of
        ParseOk xs -> case findIndex ((`spanContains` l) . loc) xs of
          Just ix -> msum 
                   [ process l err (fr, to)
                   | fr <- map (subtract 1 . srcSpanStartColumn . loc . (xs !!))
                               [ix, ix - 1 .. max 0 (ix - 9)]
                   , to <- map (subtract 1 . srcSpanEndColumn   . loc . (xs !!))
                               [ix .. min (ix + 2) (length xs - 1)]]
          Nothing -> Nothing
        ParseFailed l err -> trace err Nothing
  process l e i = liftM (\(a, s, es)->(a, s, err:es))
                . rec (Just l) $ subst i (replicate (ivlWidth i) gapChar) txt
    where err = ((srcColumn l, srcColumn l), e)
      
gapChar = '\x180E'

-- Actually unecessary
deMongol = map (\c -> if c == gapChar then ' ' else c)

imFromList :: (Ord a) => [(IM.Interval a, b)] -> IM.IntervalMap a b
imFromList = foldl (\m p -> (uncurry IM.insert) p m) IM.empty


getApps :: forall a. (Data a) => a -> Apps
getApps ast = ((const []) `extQ` processExp) ast ++ recurse ast
 where
  processExp :: ExpS -> Apps
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

preferOk = rightToMaybe . parseResultToEither

getTypeMap :: String -> TaskChan -> Apps -> IO TypeMap
getTypeMap txt c apps = do
  rec <- liftM isRight . getType . recText1 $ words txt !! 0
  fs <- mapM (getExpr rec . fst) apps
  ps <- mapM (mapM (getExpr rec) . snd) apps
  return . M.fromList . map (second cannonicalType) . catMaybes $ fs ++ concat ps
 where
  getExpr :: Bool -> Ivl -> IO (Maybe (Ivl, Type SrcSpanInfo))
  getExpr rec ivl
    = liftM (\t -> rightToMaybe t >>= liftM (ivl,) . preferOk . parseType)
    . getType . (if rec then recText2 ivl else id) $ "(" ++ substr ivl txt ++ ")"
  getType = interpret c "MyMain" . I.typeOf . deMongol
  recText1 x = "let {" ++ txt ++ "} in " ++ x
  recText2 ivl x = "let {" ++ subst ivl "__e" txt ++ "; __e = " ++ x ++ "} in __e"


getColorMap :: TypeMap -> Apps -> ColorMap
getColorMap tm = M.fromList . (`zip` colours)
               . onub . sort . map (trim . prettyPrint)
               . catMaybes . map (`M.lookup` tm) . concatMap snd

colours :: [(Int, Int, Int)]
colours = cycle $ map readColor
  [ "99968b"
  , "8f8f8f"
  , "e5786d"
  , "95e454"
  , "cae682"
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
