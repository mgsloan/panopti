{-# LANGUAGE DoAndIfThenElse, ScopedTypeVariables, TemplateHaskell,
TupleSections, ParallelListComp #-}

import Brace
import Simple
import Utils
import ErrorParser

import Prelude hiding ((.))

import Control.Arrow ((***), (&&&), first, second)
import Control.Category ((.))
import Control.Monad (liftM, zipWithM_, foldM, foldM_, when, msum)
import Data.Char (toLower)
import Data.Colour.SRGB (channelRed, channelGreen, channelBlue, toSRGB24, sRGB24read)
import Data.Curve hiding ((<.>))
import Data.Curve.Util (mapT, foldT, zipT)
import qualified Data.Curve.Interval as Ivl
import Data.Data hiding (typeOf)
import Data.Generics.Schemes (listify)
import Data.IntervalMap.FingerTree as IM
import Data.IORef
import Data.Label
import Data.List (sort, findIndex, sortBy, find, (\\), partition, isPrefixOf, group)
import Data.Maybe
import qualified Data.Map as M
import Data.Ord (comparing)
import qualified Data.Set as S
import Debug.Trace
import Graphics.ToyFramework
import qualified Graphics.Rendering.Cairo as C
import Language.Haskell.Exts (prettyPrint, Type(..), ParseResult(..), SrcLoc(..), Context)
import qualified Language.Haskell.Exts.Annotated as A
import qualified Language.Haskell.Exts as E
import qualified Language.Haskell.Interpreter as I
import Numeric.Rounding (Precision)
import System.Directory (createDirectoryIfMissing)
import System.FilePath
import qualified System.Glib.MainLoop as G
import System.IO.Unsafe (unsafeInterleaveIO)

{- Things to work on

 * Make application structure manipulation system do the following:
   - Insert / remove parens when necessary / appropriate
   - Manipulate pointfree expressions / $ style
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

-- (function span, full expression's span, [argument spans])
type Apps = [(Ivl, Ivl, [Ivl])]
type ColorMap = M.Map String (Int, Int, Int)
type TypeMap = M.Map Ivl Type
type Error = (Ivl, String)
type Edit = (Ivl, String)

data Resolution
  = DataRes { resType :: Type }
  | TypeRes { resType, targType :: Type }
  | InstRes { resType :: Type }
  deriving (Eq, Ord)

data Results = Results
  { _source :: String 
  , _partial_source :: String
  , _errors :: [Error]
  , _subsets :: [[([Resolution], [(Ivl, Type)])]]
  }

data UserMode = Normal | Selection Bool

data State = State
  { _code :: String
  , _cursor :: (Int, Int)
  , _user :: UserMode
  , _parsed :: Maybe Results
  , _chan :: TaskChan
  , _mousePos :: (Double, Double)
  , _timeout :: G.HandlerId
  , _selfRef :: IORef (KeyTable, State)
  , _clips :: [(String, Maybe String)]
  , _autoEdits :: [([Edit], Results, String)]
  }

$(mkLabels [''Results, ''State])

sourceDir :: String
sourceDir = "source"

-- TODO: configureable static imports
main :: IO ()
main = do
  createDirectoryIfMissing False sourceDir
  chan <- startGHCiServer [sourceDir] print print
  (stateRef, loop) <- runToyState $ Toy
    { initialState =
        State "map (+1) . map (*2) $ (filter isSpace $ tail \"hello world\")"
              0 Normal Nothing chan (0, 220) 0 undefined [] []
    , mouse   = const $ setM mousePos
    , key     = handleKey'
    , display = handleDisplay
    , tick    = const return
    }
  modifyIORefM stateRef (secondM (setTimeout . set selfRef stateRef))
  loop

updateTime :: Int
updateTime = 200

setTimeout :: State -> IO State
setTimeout s = do
  case get timeout s of
    0 -> return ()
    x -> G.timeoutRemove x
  time <- G.timeoutAdd (handler $ get selfRef s) updateTime
  setM timeout time s
 where
  handler ref = do
    (km, st) <- readIORef ref
    st' <- update st
    writeIORef ref (km, st')
    return False

keyHeld :: String -> State -> IO Bool
keyHeld key = liftM (maybe False (\(a,_,_)->a) . (M.lookup key) . fst)
            . readIORef . get selfRef

eitherHeld :: [Char] -> State -> IO Bool
eitherHeld key s = do
  l <- keyHeld (key ++ "_L") s
  r <- keyHeld (key ++ "_R") s
  return (l || r)

addClip :: String -> State -> IO State
addClip x s = getType (get chan s) x
          >>= \t -> return $ modify clips ((x, rightToMaybe t):) s

getSelection :: State -> String
getSelection s = substr (get cursor s) (get code s)

applyEdit :: Edit -> String -> String
applyEdit = uncurry subst

applyEdits :: [Edit] -> String -> String
applyEdits = flip $ foldr applyEdit

editCode :: [Edit] -> State -> State
editCode es s = modify code (applyEdits $ debug es)
              $ set cursor curs' s
 where
  curs = get cursor s
  curs' = mapT (offset+) curs
  offset = sum . map (\(ivl, xs) -> length xs - ivlWidth ivl) 
         $ debug $ takeWhile ((<= curs) . fst) $ debug es

handleKey' :: Bool -> Either [Char] Char -> State -> IO State
handleKey' True e s = do
  ctrl <- eitherHeld "Control" s
  shift <- eitherHeld "Shift" s
  h (ctrl, shift, get cursor s, get user s) e s
 where

  h :: (Bool, Bool, Ivl, UserMode) -> Either [Char] Char -> State -> IO State
  -- Escape = Quit
  h _ (Left "Escape") _ = error "User escape"

  -- Ctrl p = save svg screenshot to out.svg
  h (True, _, _, _) (Right 'p') st =
    C.withSVGSurface "out.svg" 640 480 
      (`C.renderWith` handleDisplay undefined ((0,0),(640, 480)) st)

  -- regular key, in insert mode = insert char representation
  h (_, _, ix, Normal) (Right k) s =
      setTimeout
    $ editCode [(ix, [k])] s

  -- cursor manipulation keys
  h (ctrl, _, _, Normal) (Left k) s =
    (case k of
       "Left"  -> setM curs (fst lr)
       "Right" -> setM curs (snd rr)
       "Home"  -> setM curs 0
       "End"   -> setM curs endPos
       "BackSpace" -> return . editCode [(lr, "")]
       "Delete"    -> return . editCode [(rr, "")]
       _ -> return) s >>= setTimeout
   where
    xs = get code s
    ix = get curs s
    curs = lens (fst . get cursor) (\a -> set cursor (a, a))
    endPos = length xs
    -- Interval of the text one word/character to the left.
    lr = ((if ctrl then wordIx (-2) ix xs
                   else max 0 $ ix - 1), ix)
    -- Interval of the text one word/character to the right.
    rr = (ix, (if ctrl then wordIx 1 ix xs
                       else min endPos $ ix + 1))

  -- y, in selection mode = Yank
  h (_, _, ix, Selection _) (Right 'y') s =
    update =<< addClip (getSelection s) s

  -- d, in selection mode = Cut
  h (_, _, ix, Selection _) (Right 'd') s =
    update =<< addClip (getSelection s)
      ( set cursor (fst ix, fst ix)
      . set user Normal
      $ editCode [(ix, "")] s ) 

  h _ _ s = return s

handleKey' _ _ s = return s

handleDisplay :: IPnt -> IRect -> State -> C.Render State
handleDisplay _ _ s@(State txt ix user p c mouse _ _ _ _) = do
  let textPos = (50.5, 200.5)

  C.setLineWidth 1
  C.setFontSize 20
  C.selectFontFace "monospace" C.FontSlantNormal C.FontWeightNormal

  -- Draw background
  C.paint
  setColor $ readColor "242424"
  C.fill

  -- Draw selection.
  setColor $ readColor "656565"
  (C.FontExtents a d _ w _) <- C.fontExtents
  let f = fromIntegral $ fst ix
      t = fromIntegral $ snd ix
  if f == t 
    then draw . moveR (textPos ^-^ (0, 12.5)) . expandR (2,2)
              $ boundPoints [(w * f, a), (w * t, -d)]
    else draw . moveR (textPos ^-^ (0, 12.5)) . expandR (2,2)
              $ boundPoints [(w * f, a), (w * t, -d)]
  C.fill
  
  let pos = textPos ^+^ (0, 20)
  whenJust p (\r -> drawTypes shapes pos mouse r >> drawGaps pos r)

  -- Draw text
  setColor $ readColor "f6f3e8"
  move textPos
  C.showText txt
  C.fill

  -- Draw clips
  move (50.5, 400.5)
  mapM (\txt -> C.showText (fst txt) >> C.relMoveTo 0 20) (get clips s)

  return s

drawGaps :: DPoint -> Results -> C.Render ()
drawGaps p r = mapM_ drawGap . map (mapT fst . (head &&& last))
             . pairGroupBy adjacent . filter ((==gapChar) . snd) . zip [0..]
             $ get partial_source r
 where
  adjacent (i, _) (i', _) = i + 1 == i'
  drawGap (f, t) = do
    rect <- textRect (get source r) f (t + 1)
    setColor ((0.5, 0.2, 0.2) :: DColor)
    draw (moveR (p ^-^ (0, 24)) rect)
    C.fill

reverseLinear :: Linear Double -> Linear Double
reverseLinear = (`compose` (Linear 1 0 :: Linear Double))

spanLine :: String -> Ivl -> C.Render DLine
spanLine txt (f, t) 
  = liftM (mapT reverseLinear . rside 2 . expandR 2) 
  $ textRect txt f t

type HeightMap = IM.IntervalMap Int Double

getHeight :: DPoint -> Ivl -> IM.IntervalMap Int Double -> Double
getHeight (_, y) (f, t)
  = minimum . (y:) . map snd . intersections (IM.Interval f t)


surround :: String -> String -> DRect -> C.Render DRect
surround pre post r = do
  initial <- C.getCurrentPoint
  from <- liftM (((rside 3 r `at` 0.5) ^-^) . second (/(-2))) $ textSize pre  
  move from
  C.showText pre
  psz <- textSize post
  let to = (rside 1 r `at` 0.5) ^+^ (second (/2) psz)
  move to
  C.showText post
  return $ Data.Curve.union (boundPoints [from, (fst to, snd from)]) r

allIvls :: Apps -> [(Ivl, Ivl)]
allIvls = onubSortBy snd
        . concatMap (\(a, b, xs) -> (a, b) : map (\x -> (x, x)) xs)

shapes :: [C.Render DRect]
shapes = [ scolor >> dshape 
         | scolor <- cycle $ map (setColor . readColor)
                   [ "#8f8f8f"
                   , "#e5786d"
                   , "#95e454"
                   , "#8ac6f2"
                   , "#e7f6da"
                   , "#3f3ff2"
                   , "#05786d"
                   , "#90f494"
                   , "#8fc6a2"
                   ]
         | dshape <- cycle
                   [ drawShape (\p -> C.arc (fst p) (snd p) 3 0 (2 * pi))
                   , drawPoly 3 q
                   , drawPoly 3 (q * 3)
                   , drawPoly 4 (q / 2)
                   , drawPoly 4 0
                   , drawPoly 5 q
                   ]
         ]
 where
  q = pi / 2
  drawShape s = do
    p <- liftM (^+^ (3, 0)) C.getCurrentPoint
    s p
    C.fill
    return . expandR (1, 0)
           $ boundPoints [p ^+^ (3, 0), p ^+^ (0, 3), p ^-^ (0, 3), p ^-^ (3, 0)]
  drawPoly :: Int -> Double -> C.Render DRect
  drawPoly c r = drawShape (\pnt -> do
    let cnt = fromIntegral c
        (x:xs) = map (\i -> (pnt ^+^ fromPolar (3, r + i / cnt * 2 * pi))) [0..cnt-1]
    move x
    mapM_ (uncurry C.lineTo) xs)

type HeightMap2 = IM.IntervalMap Double Double

getTop, getBottom :: Double -> Ivl.Interval Double -> IM.IntervalMap Double Double -> Double
getTop    y ivl = minimum . (y:) . map snd . intersections (cvtIvl ivl)
getBottom y ivl = maximum . (y:) . map snd . intersections (cvtIvl ivl)

cvtIvl :: (Precision a) => Ivl.Interval a -> IM.Interval a
cvtIvl ivl = IM.Interval (Ivl.inf ivl) (Ivl.sup ivl) 

type PosMap = M.Map String [DPoint]
type HLine = (Ivl.Interval Double, Double)
type IvlMap = M.Map String [HLine]

csort :: (a -> Ivl) -> [a] -> [a]
--csort f = sortBy $ comparing (\x -> length . filter ((`ivlContains` f x) . f))
csort f = sortBy $ comparing (\x -> ivlWidth $ f x)

{- TODO:
  need to do the drawing of types in a way that transparently
 -}

drawTypes :: [C.Render DRect] -> DPoint -> DPoint -> Results -> C.Render ()
drawTypes shs pos m res = do
   let subs = get subsets res
   when (not . null $ subs) $
     zipWithM_ drawOption [(fst pos, y) | y <- ys] ((:[]) . last $ subs)
   C.setFontSize 20
 where
  ys = [snd pos, snd pos + 150..]
  txt = get source res
 
  drawOption :: DPoint -> [([Resolution], [(Ivl, Type)])] -> C.Render ()
  drawOption pos subs = do
     let ordered = csort fst 
                 $ map (\(_, xs) -> (,xs) . foldl1 ivlHull $ map fst xs)
                 -- temporary stopgap
                 $ filter (not . null . snd) subs
              
     foldM drawChunk IM.empty ordered
     return ()
   where
-- TODO: consider selecting the primitive types based on repetition counts /
-- some fitness factor / etc.
    prims = M.empty --M.fromList
          -- $ zip (concatMap (map (trim . prettyPrint) . fst) subs) shs

    spanLine' ivl = do
      C.setFontSize 20
      sp <- liftM (offset (pos ^+^ (2, 0))) $ spanLine txt ivl
      C.setFontSize 8
      return sp

    drawChunk hm (ivl, xs) = do
      (hm', ivlmap) <- foldM drawType (hm, M.empty) $ csort fst xs

      -- Draw red divider
      sp <- liftM (offset (-5, 0)) $ spanLine' ivl
      let (x1, y1) = sp `at` 0
          (x2, y2) = sp `at` 1
          y = getBottom y2 (Ivl.makeIvl x1 x2) hm'
          lineb = (Linear x1 x2, Linear y y)
          linel = (Linear x1 x1, Linear y (y - 15))
          liner = (Linear x2 x2, Linear y (y - 15))
      C.setLineWidth 2
      setColor ((1.0, 0.0, 0.0) :: DColor)
      draw lineb
      draw linel
      draw liner
      C.stroke
      C.setLineWidth 1
      return hm'

    drawMetaUnions im 
      = liftM (M.fromList . (preserve++))
      $ mapM (\(txt, hls) -> do
        move (-10, -10)
        case M.lookup txt prims of
          Just prim -> prim >> return ()
          Nothing -> setColor ((1, 1, 1) :: DColor)
        hl <- drawBridges hls 
        C.stroke
        return (txt, [hl])) depict
     where
      (depict, preserve) = partition ((>1) . length . snd) $ M.toList im

    drawBridges :: [HLine] -> C.Render HLine
    drawBridges [x] = return x
    drawBridges xs = do
       x0 <- drawUnion x1 x2
       drawBridges (x0 : xs')
     where
      order = comparing (\(a, b) -> (b, Ivl.inf a))
      (x1:x2:xs') = sortBy order xs

      drawUnion :: HLine -> HLine -> C.Render HLine
      drawUnion (x1, y1) (x2, y2)
        = let inter = Ivl.intersection x1 x2 in do
            if Ivl.null inter 
              then if x1 < x2 
                   then manhatten (Ivl.sup x1, y1) (Ivl.inf x2, y2)
                   else manhatten (Ivl.inf x1, y1) (Ivl.sup x2, y2)
              else manhatten (Ivl.midpoint inter, y1) (Ivl.midpoint inter, y2)
            return $ case compare y1 y2 of
                       LT -> (x1, y1)
                       EQ -> (Ivl.hull x1 x2, y1)
                       GT -> (x2, y2)

      manhatten :: DPoint -> DPoint -> C.Render ()
      manhatten (x1, y1) (x2, y2)
        | y1 >= y2 = do 
           draw (Linear x1 x2, Linear y1 y1)
           draw (Linear x2 x2, Linear y1 y2)
        {-
           let x3 = (x1 + x2) / 2
           draw (Linear x1 x3, Linear y1 y1)
           draw (Linear x3 x3, Linear y1 y2)
           draw (Linear x3 x2, Linear y2 y2)
           return (x1 Ivl.... x3, y1)
         -}
        | otherwise = manhatten (x2, y2) (x1, y1)

    drawUnions :: Double -> PosMap -> C.Render IvlMap
    drawUnions y
      = foldM drawUnion M.empty
      . zip [y+3, y+13..]
      . sortBy (comparing $ (length &&& (negate . fst . last)) . snd)
      . filter (not . null . snd)
      . M.toList
     where
      drawUnion im (y, (txt, pnts)) = do
        let xivl = Ivl.fromList $ map fst pnts
        move (-10, -10)
        whenJust (M.lookup txt prims) (>> return ())
        draw (xivl, y)
        mapM_ (\(px, py) -> draw (px, Ivl.makeIvl y py)) pnts
        C.stroke
        return $ M.insertWith (++) txt [(xivl, y)] im
      
    drawType :: (HeightMap2, IvlMap) -> (Ivl, Type)
             -> C.Render (HeightMap2, IvlMap)
    drawType (hm, im) (ivl, t) = do
      C.setFontSize 20
      sp <- spanLine' ivl
      let (x1, y1) = sp `at` 0
          (x2, y2) = sp `at` 1.0
          y = getBottom y2 (Ivl.makeIvl x1 x2) hm
          omitBrace = True -- not $ ' ' `elem` substr ivl txt
      if omitBrace
        then move (x1, y)
        else do
          let (x3, y3) = sp `at` 0.5
          move (x3, y)
          setColor ((1.0, 1.0, 1.0) :: DColor)
          when (' ' `elem` substr ivl txt)
            $ drawBrace ((x1 - x3) / 2 + 15) ((x2 - x3) / 2 - 15)
          C.fill
          move (x3, y + 5)
      (r, pm) <- rec [] t
      im' <- drawUnions (Ivl.inf $ snd r) pm
      let xivl = Ivl.makeIvl x1 x2
          (within, outside) = (fmap fst &&& fmap snd)
            $ fmap (partition (Ivl.contains (Ivl.expand 10 xivl) . fst)) im
      im'' <- drawMetaUnions $ M.unionWith (++) within im'
      let y' = (+(if omitBrace then 10 else 20)) 
             . maximum . map snd . concat $ M.elems im'
      return (IM.insert (cvtIvl $ fst r) y' hm, M.unionWith (++) outside im'')
     where
      drec ctx t = do
        pnt <- C.getCurrentPoint
        C.liftIO $ print $ prettyPrint t ++ show pnt ++ "; "
        res <- rec ctx t
        C.liftIO $ print (fst res)
        return res
      rec :: Context -> Type -> C.Render (DRect, PosMap)
      rec ctx t@(TyFun _ _) = do
        (rect, pm) <- drawLTR (drec ctx) $ splitFunc t
        let line = toBezier $ offset (2, -10) $ rside 0 rect
        setColor ((1.0, 1.0, 1.0) :: DColor)
        draw line
        drawArrow 2 1 0.9 line
        C.stroke
        return . (,pm) $ expandR (5, 5) rect
      rec ctx (TyTuple _ xs) = do
        (rect, pm) <- drawLTR (drec ctx) xs
        setColor ((1.0, 1.0, 1.0) :: DColor)
        liftM (,pm) $ surround "(" ")" rect
      rec ctx (TyList t) = do
        (rect, pm) <- drec ctx t
        setColor ((1.0, 1.0, 1.0) :: DColor)
        liftM (,pm) $ surround "[" "]" rect
      rec ctx t@(TyVar _) = prim ctx t
      rec ctx t@(TyCon _) = prim ctx t
      rec ctx (TyForall _ ctx' t) = drec (ctx' ++ ctx) t
      rec ctx (TyParen t) = drec ctx t
      rec ctx t = fallback t
      prim ctx t =
        let str = trim . prettyPrint $ addCtx ctx t in
          case M.lookup str prims of
            Just sh -> do
              fr <- C.getCurrentPoint
              liftM (, M.singleton str [fr ^+^ (3,0)]) sh
            Nothing -> --trace ("fallback of prim " ++ str) 
                       fallback t
      fallback t = do
        let typ = trim $ prettyPrint t
        setColor ((1.0, 1.0, 1.0) :: DColor)
        sz <- textSize typ
        fr <- C.getCurrentPoint
        relMove (0, snd sz / 2)
        C.showText typ
        return . (, M.singleton typ [fr]) 
               . expandR (2, 0) $ boundPoints [fr, fr ^+^ (fst sz, 0)]

    drawLTR :: (a -> C.Render (DRect, PosMap)) -> [a] -> C.Render (DRect, PosMap)
    drawLTR f = liftM ((foldl1 Data.Curve.union *** foldl1 (M.unionWith (++))) .  unzip)
--              . liftM (debugWith $ show . length)
              . mapM (\x -> do
                  res@(r,_) <- liftM (first $ expandR (3, 0)) $ f x
                  move $ rside 1 r `at` 0.5
                  return res)

update :: State -> IO State
update s = (parsed' $ partialParse A.parseExp txt) >>= flip (setM parsed) s
 where
  txt = get code s
  parsed' (Just (expr, txt', errs)) = do
    let w = whereify expr
    subs <- (liftM (map tail)
          . mapM (scanM (\(_,_,_,ns) -> unifyTypes (get chan s) ns)
                        (undefined, [], [], manyNames)))
        =<< getSubsets w (get chan s)
    mapM_ (return . watchTypeds "unified " . map (\(_,_,c,_) -> c)) subs
    printWatches
    return (Just $ Results txt txt' errs (processSubsets subs))
  parsed' Nothing = return Nothing
  processSubsets = map (map (\(_,a,b,_) -> (a, map (first getSpan') b)))

ivlWidth :: Ivl -> Int
ivlWidth = foldT subtract

-- While I do not think this is a particularly good way of auto-fixing syntax
-- errors, it does get us from zero type information to some type information!
-- The question is, do we prefer to avoid giving any potentially misleading
-- information? I think the answer is to design the visualization to be
-- relatively transparent about the knowledge of the program.

partialParse :: (Show a, Eq a)
             => (String -> ParseResult a) -> String -> Maybe (a, String, [Error])
partialParse f t0 = rec 10 Nothing t0
 where
  rec 0 _ _ = Nothing
  rec limit prior txt =
    case f txt of
      ParseOk decl -> Just (decl, txt, [])
      err -> if Just err == prior then Nothing else handleError err
   where
    handleError err@(ParseFailed l e)
      = liftM (third3 ((ivl, e):)) . rec (limit - 1) (Just err) $ applyEdits errorEdits txt
     where
      errorEdits
       | isPrefixOf "Parse error: EOF"             e = [(eivl, balanceParens lexed)]
       | isPrefixOf "Improperly terminated string" e = [(eivl, "\"")]
       | isPrefixOf "Unterminated nested comment"  e = [(eivl, "-}")]
       | isPrefixOf "Parse error:"                 e = [(tivl, "")]
       | isPrefixOf "Parse error in expression: "  e = [(tivl, "")]
       | otherwise = trace e []
      end = length txt
      eivl = (end, end)
      ivl = (srcColumn l, srcColumn l)
      lexed = fromJust . preferOk $ lexify txt
      tivl = maybe ivl id . find ((`ivlContains` ivl)) $ map (colSpan . A.loc) lexed

imFromList :: (Ord a) => [(IM.Interval a, b)] -> IM.IntervalMap a b
imFromList = foldl (\m p -> (uncurry IM.insert) p m) IM.empty

gapChar :: Char
gapChar = '\x180E'

getType :: TaskChan -> String -> IO (IError String)
getType c t = interpret c "MyMain" . Simple.typeOf $ deMongol t

-- TODO: configureable static imports
interpretWith :: TaskChan -> String
              -> I.Interpreter a -> IO (IError a)
interpretWith c s f = do
  writeFile (sourceDir </> "L" <.> "hs")
    $ "{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}\n"
   ++ "module L where\nimport MyMain\n" ++ s ++ "\n"

  interpret c "L" f

-- Actually unecessary
deMongol :: [Char] -> [Char]
deMongol = map (\c -> if c == gapChar then ' ' else c)

subsetTypes :: TypedDecls -> TypeMap
subsetTypes = M.fromList . map (first getSpan')

-- Type for a map of declarations (such as yielded by twhered / dwhered), 
-- paired with an expression which references the root of the expression.
type RootedMap = (ExpS, DeclMap)
type TypedDecls = [(DeclS, Type)]

-- This is the seminal approach
getTopSubset :: RootedMap -> TaskChan -> IO [TypedDecls]
getTopSubset (top, dm) chan = do 
   types <- getTypes $ M.elems dm
   case types of
     Left err -> watch' ("top " ++ err) err $ recTop
     Right xs -> return [xs]
 where
  recTop :: IO [TypedDecls]
  recTop = maybe (return []) derefChildren
         $ M.lookup (head . getVars $ top) dm

  -- Removes declarations that the passed declaration references, and
  -- recursively removes their children.  In other words, this deletes an
  -- entire expression subtree in the twhered / dwhered expression.
  derefChildren :: DeclS -> IO [TypedDecls]
  derefChildren = liftM concat
                . mapM (derefNode . (id &&& (`M.lookup` dm)))
                . declChildVars

  derefNode :: (String, Maybe DeclS) -> IO [TypedDecls]
  derefNode (_, Nothing) = return []
  derefNode (n, Just d) = do
    ts <- getTypes . map snd . undefDecls [n] $ M.toList dm
    case ts of
      Right xs -> do
        results <- derefChildren d
        return (if null results then [xs] else results)
      Left err -> watch' "Cause" err $ return []

  getTypes :: [DeclS] -> IO (Either String TypedDecls)
  getTypes decls = do
    let ppr = A.prettyPrint $ twhered decls
    typ <- getType chan ppr
    return $ case typ of
      Left err -> Left (show err)
      Right typeStr -> Right $
        case watchWith (concatMap (concatMap E.prettyPrint) . maybeToList) "tupT"
           $ processType $ parseType typeStr of
          Just xs -> zip decls xs
          _ -> []

  -- Apply the context to each subcomponent, enumerate contents of tuple type.
  processType :: ParseResult Type -> Maybe [Type]
  processType (ParseOk (TyTuple _ xs)) = Just xs
  processType (ParseOk (TyForall bnds ctx t)) = liftM (map $ addCtx ctx)
                                              $ processType (ParseOk t)
  processType (ParseOk t) = Just [t]
  processType _ = Nothing

-- | Gets all possible breakdowns into validly-typeable subsets
getSubsets :: (ExpS, [DeclS]) -> TaskChan -> IO [[TypedDecls]]
getSubsets p@(top, ds) chan = recurse (top, declMap ds)
 where
  recurse :: RootedMap -> IO [[TypedDecls]]
  recurse arg@(r, dm) | M.null dm = return []
                      | otherwise = do
    --putStrLn $ unlines . map prettyPrint $ M.elems dm
    results <- getTopSubset arg chan
    if null results
      then return []
      else mapM (\xs -> liftM (xs:) . recurse'
                      . foldr M.delete dm 
                      $ map (get funName . fst) xs
                ) $ watchTypeds (prettyPrint r) results

  recurse' :: DeclMap -> IO [TypedDecls]
  recurse' dm
    = liftM (concat . concat)
    . mapM (recurse . (,dm) . mkPlain . fst)
    . filter ((== 0) . length . (`declChildren` dm) . snd)
    $ M.toList dm

-- Should be a better way to do this....
annType = mustOk . A.parse . prettyPrint


{- unifyTypes

In order to extract the unified constraints between these polymorphic variables,
replace all of the variables and literals with explicitly typed undefineds.  We
give these undefineds monomorphic types, with all the variables replaced with 
references to dummy, constructorless data types. Unification of these types 
happens in the compound portions of the expression, which are left unchanged.

The errors that result from type checking this transformed definition tells us
the typing context necessary to make it compile.  For example, an exception of 
the form "Couldn't match expected type `A' with actual type `B'." means that 
the corresponding parametric variables have an equality constraint.  Next, we 
need to resolve the error so that further information can be gleaned.  This can
either be done by introducing a type synonym, or rewriting the type signatures, 
cannonicalizing either A or B.

Typeclass errors inform of the constraints between the different type variables.
Rather than generating instance definitions directly, $(mkDummyInstance ''A)
template-haskell invocations are created.  mkDummyInstance gets the information 
about the type class, and implements it by setting all of the methods to 
'undefined'.  I figure that having TH generate ASTs is faster than serializing a
definition, and having it re-parsed.
-}
isTypeRes (TypeRes _ _) = True
isTypeRes _ = False

instance Show Resolution where
  show (TypeRes t b) = "type " ++ A.prettyPrint (deQual t) ++ " = " ++ A.prettyPrint b
  show (DataRes t)   = "data " ++ A.prettyPrint (deQual t)
  show (InstRes t)   = "$(mkDummyInstance \"" ++ A.prettyPrint (deQual t) ++ "\")"

watchTypeds :: String -> [TypedDecls] -> [TypedDecls]
watchTypeds n = zipWith (\i -> watchTyped (n ++ show i)) [0..]

watchTyped :: String -> TypedDecls -> TypedDecls
watchTyped n xs = watch' n 
  (unlines $ map (\(a, b) -> padLeft padding a ++ " :: " ++ b) ys)
  xs
 where
  ys = map (\(a, b) -> (prettyPrint a, prettyPrint b)) xs
  padding = maximum $ map (length . fst) ys

  {-
cons = map head . group . sort
         $ concatMap (listify (\x -> case x of (TyCon _) -> True; _ -> False)) res

    -- All of the parametric variables used in the result.
    datas = [a | DataRes a <- debug' "Resolutions" rs]
  -}

unifyTypes :: TaskChan -> [String] -> TypedDecls
           -> IO (Either I.InterpreterError String, [Resolution], TypedDecls, [String])
unifyTypes chan names tdecls  = do
  (typ, rs, ds) <- rec (map DataRes typesUsed, tdecls)
  return (typ, rs, rewrite tm' ds, namesRemaining)
 where
  (vars, comps) = partition (isVarOrLit . get funExpr . fst) tdecls

  isVarOrLit :: ExpS -> Bool
  isVarOrLit (A.Var _ _) = True
  isVarOrLit (A.IPVar _ _) = True
  isVarOrLit (A.Lit _ _) = True
  isVarOrLit _ = False

  -- All of the free polymorphic variables in the declarations
  freeVars = onubSortBy id $ concatMap (freeTVars . snd) tdecls

  -- Get the names we'll need for dummy datatypes.
  (namesUsed, namesRemaining) = splitAt (length freeVars) names

  -- Create types out of the names.
  typesUsed = [ E.TyCon . E.Qual (E.ModuleName "L") . E.Ident $ "T" ++ n | n <- namesUsed ]

  -- Forward and reverse mappings of the types.
  tm  = M.fromList [(t, n) | t <- freeVars | n <- typesUsed]
  tm' = M.fromList [(prettyPrint n, E.TyVar $ E.Ident t) | t <- freeVars | n <- typesUsed]

  rewrite m = map (second $ specializeTypes m)

  -- Declarations with variables and literals are replaced with "undefined"
  -- with explicit types.
  rewritten = comps ++ map mkSig (rewrite tm vars)
   where
    mkSig (d, t) = (set funExpr (A.ExpTypeSig sp (mkPlain "undefined") $ annType t) d, t)

  -- Get the code for a function definition, from the rewritten declarations.
  rec (rs, ds)  = do
    let fname = "foo"
        txt = (A.prettyPrint . mkFun fname . twhered $ map fst ds)
            ++ "\n" ++ (unlines . map show $ onubSortBy id rs)
    typ <- interpretWith chan (debugWith ("unify-state:" ++) txt) (typeOf fname)
    case typ of
      Left (I.WontCompile xs) -> handleErrors typ xs
      _ -> return (typ, rs, ds)
   where
    -- Pick a type to use as the cannonical name for a synonym group
    pickCannonical rs xs 
        = case partition isConL xs of
            -- Prefer constructors if there is one.
            (xs, [h]) -> (h, xs)
            (xs, []) -> fromJust $ extractFirst ((`elem`rs) . DataRes) xs

    isConL (TyCon (E.Qual (E.ModuleName "L") _)) = True
    isConL _ = False

    -- Create a resolution for each kind of error, and rewrite based on type
    -- equality constraint. 
    handleErrors typ xs = do
      let (ts, rs') = partition isTypeRes $ concatMap resolve xs
          -- Build a map from each of these to the cannonical type.
          synMap = M.fromList
                 . concatMap (\(x, xs) -> map ((,x) . prettyPrint) xs)
          -- Pick the cannonical type for each group.
                 . map (pickCannonical rs . onubSortBy id . concatMap (\(a, b) -> [a, b]))
          -- Find all related type-synonyms
                 $ transitivePartition (\(a, b) (a', b') -> b == b' || a == b' || b == a')
                 [ (a, b) | TypeRes a b <- ts ]
      if null rs'
        then return (typ, rs, ds)
        else rec (rs' ++ rs, rewrite synMap ds)

    resolve (I.GhcError (_, _, c, _)) = case parseGHCError c of
      TypeError (EqualityError a b _ _) _ ->
        let a' = mustOk $ parseType a
            b' = mustOk $ parseType b in [TypeRes a' b']
      TypeError (InstanceError a _) _ -> [InstRes (mustOk $ parseType a)]
      _ -> []

  deQualL (TyCon (E.Qual (E.ModuleName "L") x)) = Right (TyCon $ E.UnQual x)
  -- Temporary hack
  deQualL (TyCon (E.Qual _ x)) = Left (TyCon $ E.UnQual x)
  deQualL t = Left t

readColor :: String -> (Int, Int, Int)
readColor = toIColor . toSRGB24 . sRGB24read

--toIColor :: RGB Double -> (Int, Int, Int)
toIColor c =
  (fromIntegral $ channelRed   c, 
   fromIntegral $ channelGreen c,
   fromIntegral $ channelBlue  c)

{-
getApps :: ExpS -> Apps
getApps = uncurry (++) . processExp
 where
  processExp :: ExpS -> (Apps, Apps)
  processExp (A.InfixApp s l o r) = case o of
    (A.QVarOp _ (A.UnQual _ (A.Symbol _ "."))) -> doApp s l [r]
    (A.QVarOp _ (A.UnQual _ (A.Symbol _ "$"))) -> doApp s l [r]
    _                                          -> doApp s (convertOp o) [l, r]
  processExp (A.LeftSection s l o)  = doApp s (convertOp o) [l]
  processExp (A.RightSection s o r) = doApp s (convertOp o) [r] --TODO: uhhh
  processExp (A.App s l r)          = doApp s l [r]
  processExp e = foldr1 (zipT (++))
               $ gmapQ (const ([], []) `extQ`
                 (first (map $ second3 $ const (getSpan' e)) . processExp)) e
  doApp :: A.SrcSpanInfo -> ExpS -> [ExpS] -> (Apps, Apps)
  doApp s x xs = ( [(getSpan' x, colSpan $ A.srcInfoSpan s, map getSpan' xs)]
                 , concatMap getApps (x:xs) )
  convertOp :: A.QOp A.SrcSpanInfo -> ExpS
  convertOp (A.QVarOp s' qn) = A.Var s' qn
  convertOp (A.QConOp s' qn) = A.Con s' qn

-- Takes a list of apps, and merges by left-hand-span, and therefore app lhs.
concatApps :: Apps -> Apps
concatApps = map process . groupSortOn (\((x, _), _, _) -> x) . reverse
  where
   process xs = (h, l, reverse . concat $ map thd3 xs)
    where
     ((h,_,_),(_,l,_)) = (head &&& last) $ sortBy (comparing (ivlWidth . snd3)) xs
-}


{-
handleKey (True, _, ix, Normal) (Right ' ') s = return . setSel $ atSpan' ix s
 where
  setSel = maybe s (\(_, ivl) -> set user (Selection True) $ set cursor ivl s)
handleKey (_, shift, ix, Selection _) (Right k) s =
  maybe (return s) (if shift then update else return)
  (case toLower k of
    'k' -> liftM (\(_, ivl, _) -> set cursor ivl s) parent
    'j' -> maySetCursor $ childSpan' (ix, 0)
    'h' -> parent >>= (\p ->
           swapOrSwitch shift (wrapped (snd3 p) . (subtract 1)) $ rmfst3 p)
    'l' -> parent >>= (\p ->
           swapOrSwitch shift (wrapped (snd3 p) . (+1)) $ rmfst3 p)
    'o' -> (if shift then (swapHead =<<) else maySetCursor)
           . liftM fst3 . findApp ix $ getRes appIvls s
    _ -> Nothing)
 where
  parent = parentSpan ix (getRes appIvls s)

  wrapped c = (`mod` (maybe 1 id $ arity c))
  arity x = liftM (length . thd3) $ find ((==x).snd3) (getRes appIvls s)

  swapHead :: Ivl -> Maybe State
  swapHead ivl = liftM (thd3 . swapIvls ivl . snd) $ atSpan' ivl s

  swapOrSwitch True = swapIx
  swapOrSwitch False = switchIx

  maySetCursor = liftM (\ivl -> set cursor ivl s)

  childSpan' x = childSpan x (getRes appIvls s)

  switchIx f p = maySetCursor . childSpan' $ second f p

  swapIx f p = do
    from <- childSpan' p
    to <- childSpan' $ second f p
    let (f', _, s') = swapIvls from to
    setM cursor f' s'

  swapIvls a b
    | b < a = (\(x, y, xs) -> (y, x, xs)) $ swapIvls b a
    | otherwise = ( ((+(ivlWidth b - ivlWidth a)) &&& (+(ivlWidth b))) $ fst b
                  , (id &&& (+ivlWidth b)) $ fst a
                  , )
                  $ editCode [(a, substr b txt), (b, substr a txt)] s

  txt = get code s

  --TODO: use for parameter introduction
--  arity x = subtract 1 . length . splitFunc . fromJust
--          . M.lookup x $ getRes typeMap s


getRes f = get (f . maybeLens . parsed)

atSpan' :: Ivl -> State -> Maybe (ExpS, Ivl)
atSpan' ivl = liftM (id &&& getSpan') . atSpan ivl . getRes decl

selectAST :: Ivl -> State -> State
selectAST ivl st = maybe st ((`selectIt`st) . snd) $ atSpan' ivl st
 where
  selectIt c = set user (Selection True) . set cursor c

--  curs' e = liftM fst . parentSpan (getSpan' e) $ get (appIvls . maybeLens . parsed) st

findApp :: Ivl -> Apps -> Maybe (Ivl, Ivl, [Ivl])
findApp ivl = listToMaybe . sortBy (comparing (ivlWidth . snd3))
            . filter ((`ivlContains` ivl) . snd3)

childSpan :: (Ivl, Int) -> Apps -> Maybe Ivl
childSpan (ivl, ix) as = (`atMay` ix) . reverse . thd3 =<< findApp ivl as

parentSpan :: Ivl -> Apps -> Maybe (Ivl, Ivl, Int)
parentSpan ivl = fmap snd . listToMaybe . sortBy (comparing fst)
               . catMaybes . map helper
 where
  helper (a, b, xs) = findIndex (`ivlContains` ivl) xs'
                  >>= \ix -> return (ivlWidth $ xs' !! ix, (a, b, ix))
   where
    xs' = reverse xs
-}
