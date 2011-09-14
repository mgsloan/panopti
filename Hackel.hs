{-# LANGUAGE FlexibleInstances, TemplateHaskell, TupleSections, TypeOperators,
ScopedTypeVariables, TypeFamilies, ParallelListComp #-}

import Simple
import Utils

import Control.Arrow ((&&&), second)
import Control.Monad (liftM, zipWithM_, foldM, foldM_, msum)
import Data.Char (isSpace)
import Data.Colour.SRGB (channelRed, channelGreen, channelBlue, toSRGB24)
import qualified Data.Colour.Names as Color
import Data.Curve hiding ((<.>))
import Data.Curve.Util (mapT, foldT)
import Data.Data
import Data.Generics.Aliases
import Data.IntervalMap.FingerTree as IM
import Data.IORef
import Data.Label
import Data.List (sort, findIndex)
import Data.Maybe
import qualified Data.Map as M
import Debug.Trace
import Graphics.ToyFramework
import qualified Graphics.Rendering.Cairo as C
import Language.Haskell.Exts.Annotated
import qualified Language.Haskell.Interpreter as I
import qualified System.Glib.MainLoop as G
import System.Directory (createDirectoryIfMissing)

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

type Apps = [(Ivl, [Ivl])]
type TypeMap = M.Map Ivl (Type SrcSpanInfo)
type ColorMap = M.Map String (Int, Int, Int)
type GapMap = IM.IntervalMap Int Int

data Results = Results
  { _source :: String
  , _appIvls :: Apps
  , _typeMap :: TypeMap
  , _colorMap :: ColorMap
  , _gaps :: [Ivl]
  , _gapMap :: GapMap 
  }

data State = State
  { _code :: String
  , _cursor :: Int
  , _parsed :: Maybe Results
  , _chan :: TaskChan
  , _mouseCursor :: (Double, Double)
  , _timeout :: G.HandlerId
  , _selfRef :: IORef (KeyTable, State)
  }

$(mkLabels [''State])

updateParse :: TaskChan -> State -> IO State
updateParse c s = (parsed' $ partialParse parseDecl txt) >>= flip (setM parsed) s
 where
  txt = get code s 
  parsed' (Just (decl, gaps)) = do
    let apps = concatApps $ getApps decl
    tm <- getTypeMap txt c apps
    let tc = M.fromList . (`zip` colours) . onub . sort . map (trim .  prettyPrint)
           . catMaybes . map (`M.lookup` tm) $ concatMap snd apps
        gaps' = mergeGaps txt gaps
    print gaps
    print gaps'
    return (Just $ Results txt apps tm tc gaps' (imGaps gaps'))
  parsed' Nothing = return Nothing

preferOk = eitherToMaybe . parseResultToEither

imFromList :: (Ord a) => [(IM.Interval a, b)] -> IM.IntervalMap a b
imFromList = foldl (\m p -> (uncurry IM.insert) p m) IM.empty

imGaps :: [Ivl] -> IM.IntervalMap Int Int
imGaps ivls = imFromList
  $ zip (zipWith (\(_,f) (t,_) -> IM.Interval f t) ivls' $ tail ivls')
        (map (foldT subtract) ivls')
 where
  ivls' = (0, 0) : ivls

mergeGaps :: String -> [Ivl] -> [Ivl]
mergeGaps txt = foldl merger [] . sort
 where
  merger [] x = [x]
  merger ((f, t):xs) (f', t') = if all isSpace (substr (t, f') txt)
                                then (f, t'):xs
                                else (f', t'):(f, t):xs

partialParse :: (String -> ParseResult a) -> String -> Maybe (a, [Ivl])
partialParse f txt = case f (debug txt) of
  ParseOk decl -> Just (decl, [])
  ParseFailed l err ->
    case (lexify txt) of
      ParseOk xs -> case findIndex ((`spanContains` l) . loc) xs of
        Just ix -> attemptPartials xs ix
        Nothing -> Nothing
      ParseFailed l err -> trace err Nothing
 where
  attemptPartials xs ix =
    msum $ map (\i -> liftM (second (i:)) . partialParse f $ subst i "" txt) ivls
   where
    ivls = [ (fr, to) 
           | fr <- map (srcSpanStartColumn . loc . (xs !!)) [ix, ix - 1..0]
           , to <- map (srcSpanEndColumn   . loc . (xs !!)) [ix..length xs - 1]]
    
-- TODO: static import considerations
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

handleKey True (Left k) s@(State xs ix _ c _ _ ref) = do
  ctrl <- liftM (maybe False (\(a,_,_)->a) . (M.lookup "Control_L") . fst)
        $ readIORef ref
  (case k of
    "Left"  -> modM cursor (if ctrl then flip (wordIx (-2)) xs
                                    else max 0 . subtract 1)
    "Right" -> modM cursor (if ctrl then flip (wordIx 1) xs
                                    else min endPos . (+1))
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
    = liftM (\t -> eitherToMaybe t >>= liftM (ivl,) . preferOk . parseType)
    . getType . (if rec then recText2 ivl else id) $ "(" ++ substr ivl txt ++ ")"
  getType = interpret c "MyMain" . I.typeOf
  recText1 x = "let {" ++ txt ++ "} in " ++ x
  recText2 ivl x = "let {" ++ subst ivl "__e" txt ++ "; __e = " ++ x ++ "} in __e"

getHeight :: (Double, Double) -> Ivl -> IM.IntervalMap Int Double -> Double
getHeight (_, y) (f, t) = minimum . (y:) . map snd . intersections (IM.Interval f t)

drawApps :: (Double, Double) -> TaskChan -> Results  -> C.Render ()
drawApps pos c (Results txt apps tm tc gs gm) = do
  drawLegend tc
  mapM_ drawGap gs
  foldM_ (drawFunc tm tc) IM.empty $ reverse apps
 where
  drawLegend :: ColorMap -> C.Render ()
  drawLegend = zipWithM_ (\y (t, c) -> do
      setColor c
      relText 0 (fst pos, y) t)
    [220, 240..] . M.toList
  drawGap :: Ivl -> C.Render ()
  drawGap (f, t) = do
    grect <- textRect txt (f - 1) t
    setColor ((0.5, 0.5, 0.5) :: DColor)
    draw (moveR pos grect)
    C.fill
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
