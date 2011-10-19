{-# LANGUAGE DoAndIfThenElse, ScopedTypeVariables, TemplateHaskell,
TupleSections, ParallelListComp #-}

import Brace
import Simple
import Utils
import ErrorParser

import Prelude hiding ((.))

import Control.Arrow ((***), (&&&), first, second)
import Control.Category ((.))
import Control.Monad (liftM, zipWithM_, foldM, foldM_, when)
import Data.Char (toLower)
import Data.Colour.SRGB (channelRed, channelGreen, channelBlue, toSRGB24, sRGB24read)
import Data.Curve hiding ((<.>))
import Data.Curve.Util (mapT, foldT, zipT)
import Data.Curve.Interval ((...))
import qualified Data.Curve.Interval as Curve
import Data.Data hiding (typeOf)
import Data.Generics.Aliases
import Data.IntervalMap.FingerTree as IM
import Data.IORef
import Data.Label
import Data.List (sort, findIndex, sortBy, find, (\\), partition)
import Data.Maybe
import qualified Data.Map as M
import Data.Ord (comparing)
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

 * Fix partial parse
 * Give type explanations
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

data Results = Results
  { _source :: String 
  , _partial_source :: String
  , _errors :: [Error]
  , _subsets :: [([Type], [(Ivl, Type)])]
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

-- TODO: static import considerations
sourceDir :: String
sourceDir = "source"

main :: IO ()
main = do
  createDirectoryIfMissing False sourceDir
  chan <- startGHCiServer [sourceDir] print print
  (stateRef, loop) <- runToyState $ Toy
    { initialState =
  --      State "fibs = 0 : 1 : zipWith (+) fibs (tail fibs)"
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

--parentOfSpan :: (Data a) => Ivl -> a -> Maybe (ExpS, Int)

handleKey' :: Bool -> Either [Char] Char -> State -> IO State
handleKey' True e s = do
  ctrl <- eitherHeld "Control" s
  shift <- eitherHeld "Shift" s
  h (ctrl, shift, get cursor s, get user s) e s
 where

  h :: (Bool, Bool, Ivl, UserMode) -> Either [Char] Char -> State -> IO State
  h _ (Left "Escape") _ = error "User escape"

  h (True, _, _, _) (Right 'p') st =
    C.withSVGSurface "out.svg" 640 480 
      (`C.renderWith` handleDisplay undefined ((0,0),(640, 480)) st)

  h (_, _, ix, Normal) (Right k) s =
      setTimeout
    $ editCode [(ix, [k])] s

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
    lr = ((if ctrl then wordIx (-2) ix xs
                   else max 0 $ ix - 1), ix)
    rr = (ix, (if ctrl then wordIx 1 ix xs
                       else min endPos $ ix + 1))

  h (_, _, ix, Selection _) (Right 'y') s =
    update =<< addClip (getSelection s) s

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


  -- Draw cursor
  setColor $ readColor "656565"
  (C.FontExtents a d _ w _) <- C.fontExtents
  let f = fromIntegral $ fst ix
      t = fromIntegral $ snd ix
  draw . moveR (textPos ^-^ (0, 12)) . expandR (2,2)
       $ boundPoints [(w * f, a), (w * t, -d)]
  C.fill
  
  let pos = textPos ^+^ (0, 25)
  whenJust p (\r -> drawTypes shapes pos mouse r >> drawGaps pos r)
--  whenJust p (\r -> drawTypes shapes pos c r >> drawGaps pos r)
--  whenJust p (\r -> drawApps pos c r >> drawGaps pos r)

  -- Draw text
  setColor $ readColor "f6f3e8"
  move textPos
  C.showText txt
  C.fill

  move (textPos ^+^ (100,100))
  head shapes
  move (textPos ^+^ (100,100))
--  drawBrace (negate 100) 100
  C.fill

  move (50.5, 400.5)
  mapM (\txt -> C.showText (fst txt) >> C.relMoveTo 0 20) (get clips s)

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

surround :: String -> String -> DRect -> C.Render DRect
surround pre post r = do
  from <- liftM (((rside 3 r `at` 0.5) ^-^) . second (/(-2))) $ textSize pre  
  move from
  C.showText pre
  psz <- textSize post
  let to = (rside 1 r `at` 0.5) ^+^ (second (/2) psz)
  move $ to ^-^ (fst psz, 0)
  C.showText post
  return . expandR (2, 0) $ boundPoints [from, to] `Data.Curve.union` r

-- TODO: consider writing Type information into application tree?

allIvls :: Apps -> [(Ivl, Ivl)]
allIvls = onubSortBy snd
        . concatMap (\(a, b, xs) -> (a, b) : map (\x -> (x, x)) xs)

shapes :: [C.Render DRect]
shapes = zipWith (\c -> (setColor c >>)) colours
  [ drawShape (\p -> C.arc (fst p) (snd p) 3 0 (2 * pi))
  , drawPoly 3 q
  , drawPoly 3 (q * 3)
  , drawPoly 4 (q / 2)
  , drawPoly 4 0
  , drawPoly 5 q
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

drawGaps :: DPoint -> Results -> C.Render ()
drawGaps p r = mapM_ drawGap . map (mapT fst . (head &&& last))
             . pairGroupBy adjacent . filter ((==gapChar) . snd) . zip [0..]
             $ get partial_source r
 where
  adjacent (i, _) (i', _) = i + 1 == i'
  drawGap (f, t) = do
    rect <- textRect (get source r) f (t + 1)
    setColor ((0.5, 0.2, 0.2) :: DColor)
    draw (moveR (p ^+^ (0, 24)) rect)
    C.fill

type HeightMap2 = IM.IntervalMap Double Double

getTop, getBottom :: Double -> Curve.Interval Double -> IM.IntervalMap Double Double -> Double
getTop    y ivl = minimum . (y:) . map snd . intersections (cvtIvl ivl)
getBottom y ivl = maximum . (y:) . map snd . intersections (cvtIvl ivl)

cvtIvl :: (Precision a) => Curve.Interval a -> IM.Interval a
cvtIvl ivl = IM.Interval (Curve.inf ivl) (Curve.sup ivl) 

type PosMap = M.Map String [DPoint]

{-
csort :: (a -> Ivl) -> [a] -> [a]
csort f xs = sortBy $ comparing (\x -> length . filter ((`icontains` f x) . f) xs)
-}

drawTypes :: [C.Render DRect] -> DPoint -> DPoint -> Results -> C.Render ()
drawTypes shs pos m res = do
--  C.liftIO . print $ M.keys prims
  (hm', pm') <- foldM (\(hm, pm) -> liftM (second $ M.unionWith (++) pm)
                                  . drawSubset hm)
                      (IM.empty, M.empty)
              . reverse $ map snd subs
--  C.liftIO $ print pm'
  foldM_ (flip $ uncurry drawUnion) hm' 
    $ map (second (map (first (+3)))) $ M.toList pm'
  C.setFontSize 20 
  where
  subs = get subsets res
  txt = get source res
  prims = M.fromList $ zip (concatMap (map (trim . prettyPrint) . fst) subs) shs

{-
  ordered = concat . zipWith (\ix -> map (\(i,t) -> (i,t,ix)) . csort fst) [0..]
          . map snd . csort fst
          $ map (map (\xs -> (,xs) . foldl1 ivlHull $ map fst xs)) subs
 -}

  drawUnion :: String -> [DPoint] -> HeightMap2 -> C.Render HeightMap2
  drawUnion txt pnts hm = do
    let sameYs = groupSortOn snd pnts
    let xivl = Curve.fromList $ map fst pnts
        y = getBottom (snd pos) xivl hm
    move (-10, -10)
    whenJust (M.lookup txt prims) (>> return ())
    draw (xivl, y)
    mapM_ (\(px, py) -> draw (px, y ... py)) pnts
    C.stroke
    return $ IM.insert (cvtIvl xivl) (y + 10) hm

  drawSubset hm xs = 
    foldM (\(hm, pm) t -> do
      (r, pm') <- drawType (hm, pm) t
      return ( IM.insert (cvtIvl $ fst r) ((+20) $ Curve.inf $ snd r) hm
             , M.unionWith (++) pm' pm))
     (hm, M.empty) xs

  drawType :: (HeightMap2, PosMap) -> (Ivl, Type) 
           -> C.Render (DRect, PosMap)
  drawType (hm, pm) (ivl, t) = do
    C.setFontSize 20
    sp <- spanLine txt ivl
    let sp' = offset (pos ^+^ (5, 0)) sp
    C.setFontSize 8
    if ' ' `elem` substr ivl txt
      then do 
        let (x1, y1) = sp' `at` 0
            (x2, y2) = sp' `at` 0.5
            (x3, y3) = sp' `at` 1.0
            y' = getBottom y2 (x1 ... x3) hm
        move (x2, y')
        setColor ((1.0, 1.0, 1.0) :: DColor)
        when (' ' `elem` substr ivl txt)
          $ drawBrace ((x1 - x2) / 2 + 15) ((x3 - x2) / 2 - 15)
        C.fill
        move (x2, y' + 5)
      else do
        let (x, y) = sp' `at` 0
            -- TODO: width estimates ?
            y' = getBottom y (x ... x + 10) hm
        move (x, y)

    rec [] t
   where
    rec :: Context -> Type -> C.Render (DRect, PosMap)
    rec ctx t@(TyFun _ _) = do
      (rect, pm) <- drawLTR (rec ctx) $ splitFunc t
      let line = toBezier $ offset (2, -2) $ rside 0 rect
      setColor ((1.0, 1.0, 1.0) :: DColor)
      draw line
      drawArrow 2 1 0.9 line
      C.stroke
      return . (,pm) $ expandR (5, 0) rect
    rec ctx (TyTuple _ xs) = do
      (rect, pm) <- drawLTR (rec ctx) xs
      setColor ((1.0, 1.0, 1.0) :: DColor)
      liftM (,pm) $ surround "(" ")" rect
    rec ctx (TyList t) = do
      (rect, pm) <- rec ctx t
      setColor ((1.0, 1.0, 1.0) :: DColor)
      liftM (,pm) $ surround "[" "]" rect
    rec ctx t@(TyVar _) = prim ctx t
    rec ctx t@(TyCon _) = prim ctx t
    rec ctx (TyForall _ ctx' t) = rec (ctx' ++ ctx) t
    rec ctx t = fallback t
    prim ctx t =
      let str = trim . prettyPrint $ addCtx ctx t in
        case M.lookup str prims of
          Just sh -> do
            fr <- C.getCurrentPoint
            liftM (, M.singleton str [fr]) sh
          Nothing -> fallback t
    fallback t = do
      let typ = trim $ prettyPrint t
--      C.liftIO $ print typ
      setColor ((1.0, 1.0, 1.0) :: DColor)
      sz <- textSize typ
      fr <- C.getCurrentPoint
      relMove (0, snd sz / 2)
      C.showText typ
      return . (, M.singleton typ [fr]) 
             . expandR (2, 0) $ boundPoints [fr, fr ^+^ (fst sz, 0)]

  drawLTR :: (a -> C.Render (DRect, PosMap)) -> [a] -> C.Render (DRect, PosMap)
  drawLTR f = liftM ((foldl1 Data.Curve.union *** foldl1 (M.unionWith (++))) .  unzip)
            . mapM (\x -> do
                res@(r,_) <- liftM (first $ expandR (1, 0)) $ f x
                move $ rside 1 r `at` 0.5
                return res)

update :: State -> IO State
update s = (parsed' $ partialParse A.parseExp txt) >>= flip (setM parsed) s
 where
  txt = get code s 
  parsed' (Just (expr, txt', errs)) = do
--    let apps = concatApps $ collate getApps expr
--    tm <- getTypeMap txt' (get chan s) apps
--    mapM (print . (`atSpan` decl)) $ M.keys tm
--    print "start"
{-
    let tm = if not $ null tsub
             then subsetTypes $ head tsub
             else M.empty
    let tc = getColorMap tm apps
 -}
    let w = whereify expr
    subs <- liftM tail . scanM (\(_,_,_,ns) -> unifyTypes (get chan s) ns) 
                               (undefined, [], [], manyNames)
        =<< getSubsets w (get chan s)
    let subs' = map (\(_,b,c,_) -> (b,) $ map (first getSpan') c) subs
  --  printEm subs'
    return (Just $ Results txt txt' errs subs')
  parsed' Nothing = return Nothing
  {-
  printEm = do
    mapM (\subs -> putStrLn "==" 
                >> mapM (\(a, b) -> putStrLn $ prettyPrint a ++ "\n" ++ prettyPrint b) subs)
                -}

ivlWidth :: (Int, Int) -> Int
ivlWidth = foldT subtract

partialParse :: (String -> ParseResult a) -> String -> Maybe (a, String, [Error])
partialParse f txt = rec Nothing txt
 where
  rec prior txt = case f txt of
    ParseOk decl -> Just (decl, txt, [])
    ParseFailed l err -> trace err $ if (Just l == prior) then Nothing else
      case lexify txt of
        ParseOk xs -> case findIndex ((`spanContains` l) . A.loc) xs of
          Just ix -> let tb = mapT (subtract 1)
                            . (A.srcSpanStartColumn *** A.srcSpanEndColumn) 
                            . mapT (A.loc . (xs!!))
                     in process l err $ tb (ix, ix)
                  {-
                   msum 
                   [ process l err (fr, to)
                   | fr <- map (subtract 1 . A.srcSpanStartColumn . A.loc . (xs !!))
                               [ix, ix - 1 .. max 0 (ix - 9)]
                   , to <- map (subtract 1 . A.srcSpanEndColumn   . A.loc . (xs !!))
                               [ix .. min (ix + 2) (length xs - 1)]]
                   -}
          Nothing -> process l err (length xs, length xs)
        ParseFailed l err -> trace err Nothing
  process l e i = liftM (\(a, s, es)->(a, s, err:es))
                . rec (Just l) $ subst i (replicate (ivlWidth i) gapChar) txt
    where err = ((srcColumn l, srcColumn l), e)
      
imFromList :: (Ord a) => [(IM.Interval a, b)] -> IM.IntervalMap a b
imFromList = foldl (\m p -> (uncurry IM.insert) p m) IM.empty

gapChar :: Char
gapChar = '\x180E'

getType :: TaskChan -> String -> IO (Either I.InterpreterError String)
getType c t = interpret c "MyMain" . Simple.typeOf $ deMongol t

interpretWith :: TaskChan -> String
              -> I.Interpreter a -> IO (Either I.InterpreterError a)
interpretWith c s f = do
  writeFile (sourceDir </> "L" <.> "hs")
    $ "{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}\n"
   ++ "module L where\nimport MyMain\n" ++ s ++ "\n"

  interpret c "L" f

-- Actually unecessary
deMongol :: [Char] -> [Char]
deMongol = map (\c -> if c == gapChar then ' ' else c)

-- TODO: this needs to pass in declaration parameters

getTypeMap :: String -> TaskChan -> Apps -> IO TypeMap
getTypeMap txt c apps = do
  res <- getType c . recText1 $ words txt !! 0
  let rec = isRight res
  --print rec
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

subsetTypes :: [(DeclS, Type)] -> TypeMap
subsetTypes = M.fromList . map (first getSpan')

-- This is the seminal approach
getTopSubset :: (ExpS, DeclMap) -> TaskChan -> IO [[(DeclS, Type)]]
getTopSubset (top, dm) chan 
  = maybe (return []) (\d -> do
      results <- derefChildren d
      case results of
        [] -> liftM maybeToList . getTypes $ M.elems dm
        xs -> return xs)
  $ M.lookup (head . getVars $ top) dm
 where
  getTypes :: [DeclS] -> IO (Maybe [(DeclS, Type)])
  getTypes decls = do
    let ppr = A.prettyPrint $ twhered decls
    typ <- getType chan ppr
    case typ of
      Left err -> return Nothing
      Right typeStr ->
        case processType $ parseType typeStr of
          Just xs -> return . Just $ zip decls xs
          _ -> return $ Just []

  derefChildren = liftM concat
                . mapM (derefNode . (id &&& (`M.lookup` dm)))
                . declChildVars

  derefNode :: (String, Maybe DeclS) -> IO [[(DeclS, Type)]]
  derefNode (_, Nothing) = return []
  derefNode (n, Just d) = do
    ts <- getTypes . map snd . undefDecls [n] $ M.toList dm
    case ts of
      Just xs -> do
        results <- derefChildren d
        return (if null results then [xs] else results)
      Nothing -> return []

  processType (ParseOk (TyTuple _ xs)) = Just xs
  processType (ParseOk (TyForall bnds ctx t)) = liftM (map $ addCtx ctx)
                                              $ processType (ParseOk t)
  processType (ParseOk t) = Just [t]
  processType _ = Nothing

getSubsets :: (ExpS, [DeclS]) -> TaskChan -> IO [[(DeclS, Type)]]
getSubsets p@(top, ds) chan = recurse (top, declMap ds)
 where
  recurse arg@(_, dm) = do
    --putStrLn $ unlines . map prettyPrint $ M.elems dm
    results <- getTopSubset arg chan
    if null results
      then return []
      else do
        let xs = head results
            dm' = foldr M.delete dm $ map (get funName . fst) xs
        liftM ((xs:) . concat) 
          . mapM (recurse . (,dm') . mkPlain . fst)
          . filter ((== 0) . length . (`declChildren` dm') . snd)
          $ M.toList dm'

-- Should be a better way to do this....
annType = mustOk . A.parse . prettyPrint

data Resolution
  = DataRes { resType :: Type }
  | TypeRes { resType, targType :: Type }
  | InstRes { resType :: Type }
  deriving (Eq, Ord)

instance Show Resolution where
  show (TypeRes t b) = "type " ++ A.prettyPrint (deQual t) ++ " = " ++ A.prettyPrint b
  show (DataRes t)   = "data " ++ A.prettyPrint (deQual t)
  show (InstRes t)   = "$(mkDummyInstance \"" ++ A.prettyPrint (deQual t) ++ "\")"

printTyped :: [(DeclS, Type)] -> IO ()
printTyped xs = mapM_ (\(a, b) -> putStrLn $ padLeft padding a ++ " :: " ++ b) ys
 where
  ys = map (\(a, b) -> (prettyPrint a, prettyPrint b)) xs
  padding = maximum $ map (length . fst) ys

unifyTypes :: TaskChan -> [String] -> [(DeclS, Type)]
           -> IO (Either I.InterpreterError String, [Type], [(DeclS, Type)], [String])
unifyTypes chan names xs = do
  (typ, rs) <- rec $ map DataRes typesUsed
  print rs
  let syns = map ( --(deQual *** map deQual) .
                   pickRoot rs . onubSortBy id . concatMap (\(a, b) -> [a, b]))
           $ fullPartition (\(a, b) (a', b') -> b == b' || a == b' || b == a')
             [(a, b) | TypeRes a b <- rs]
      synM = M.fromList $ concatMap (\(x, xs) -> map ((,x) . prettyPrint) xs) syns
      datas = [a | DataRes a <- rs]
  print synM
  let res = map (second $ specializeTypes synM) rewritten
  printTyped res
  return (typ, datas ++ map fst syns, res, namesRemaining)
 where
  freeVars = onubSortBy id $ concatMap (freeTVars . snd) xs
  (namesUsed, namesRemaining) = splitAt (length freeVars) names
  typesUsed = map (E.TyCon . E.Qual (E.ModuleName "L") . E.Ident . ("T" ++)) namesUsed

  tm = M.fromList [(t, n) | t <- freeVars | n <- typesUsed]

  pickRoot rs xs = case partition isConL xs of
                     (xs, [h]) -> (h, xs)
                     (xs, []) -> fromJust 
                               $ extractFirst ((`elem`rs) . DataRes) xs

  rec rs = do
    typ <- interpretWith chan (base ++ mkDecls rs) (typeOf fname)
    case typ of
      Left (I.WontCompile xs) -> let rs' = concatMap resolve xs in
        if null rs' then return (typ, rs)
          else rec $ rs' ++ (rs \\ [DataRes t | (TypeRes t _) <- rs'])
      _ -> return (typ, rs)

  resolve (I.GhcError (_, _, c, _)) = case parseGHCError c of
    TypeError (EqualityError a b _ _) _ ->
      let a' = mustOk $ parseType a
          b' = mustOk $ parseType b in
      case (isConL a', isConL b') of
        (True,  True ) -> if a < b then [TypeRes a' b'] else [TypeRes b' a']
        (False, True ) -> [TypeRes b' a']
        (True,  False) -> [TypeRes a' b']
        _ -> []
    TypeError (InstanceError a _) _ -> [InstRes (mustOk $ parseType a)]
    _ -> []

  isConL (TyCon (E.Qual (E.ModuleName "L") _)) = True
  isConL _ = False

  deQualL (TyCon (E.Qual (E.ModuleName "L") x)) = Right (TyCon $ E.UnQual x)
  -- Temporary hack
  deQualL (TyCon (E.Qual _ x)) = Left (TyCon $ E.UnQual x)
  deQualL t = Left t

  mkDecls = unlines . map show . onubSortBy id 

  fname = "foo"
  base = (++ "\n")
       . A.prettyPrint . mkFun fname . twhered 
       $ map (modify funName (++ "'") . fst) xs
      ++ map (mkSig . second annType) rewritten
  mkSig (d, t) = set funExpr (A.ExpTypeSig sp (mkPlain "undefined") t) d
  rewritten = map (second $ specializeTypes tm) xs

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
  , "e7f6da"
  ]

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
            . filter ((`icontains` ivl) . snd3)

childSpan :: (Ivl, Int) -> Apps -> Maybe Ivl
childSpan (ivl, ix) as = (`atMay` ix) . reverse . thd3 =<< findApp ivl as

parentSpan :: Ivl -> Apps -> Maybe (Ivl, Ivl, Int)
parentSpan ivl = fmap snd . listToMaybe . sortBy (comparing fst)
               . catMaybes . map helper
 where
  helper (a, b, xs) = findIndex (`icontains` ivl) xs'
                  >>= \ix -> return (ivlWidth $ xs' !! ix, (a, b, ix))
   where
    xs' = reverse xs
-}
