{-# LANGUAGE DoAndIfThenElse, ScopedTypeVariables, TemplateHaskell, TupleSections #-}

import Simple
import Utils
-- import ErrorParser

import Prelude hiding ((.))

import Control.Arrow ((***), (&&&), first, second)
import Control.Category ((.))
import Control.Monad (liftM, zipWithM_, foldM, foldM_)
import Data.Char (toLower)
import Data.Colour.SRGB (channelRed, channelGreen, channelBlue, toSRGB24, sRGB24read)
import Data.Curve hiding ((<.>))
import Data.Curve.Util (mapT, foldT, zipT)
import Data.Data hiding (typeOf)
import Data.Generics.Aliases
import Data.IntervalMap.FingerTree as IM
import Data.IORef
import Data.Label
import Data.List (sort, findIndex, sortBy, find)
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

 * Fix partial parse
 * Parse type errors
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
  , _decl :: A.Decl A.SrcSpanInfo
  , _appIvls :: Apps
  , _typeMap :: TypeMap
  , _colorMap :: ColorMap
  , _errors :: [Error]
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
        State "fibs = 0 : 1 : zipWith (+) fibs (tail fibs)"
              0 Normal Nothing chan (0, 220) 0 undefined [] []
    , mouse   = const $ setM mouseCursor
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
  offset = sum . map (\(ivl, xs) -> length xs - ivlWidth ivl) $ debug $ takeWhile ((<= curs) . fst) $ debug es

--parentOfSpan :: (Data a) => Ivl -> a -> Maybe (ExpS, Int)

handleKey' :: Bool -> Either [Char] Char -> State -> IO State
handleKey' True e s = do
  ctrl <- eitherHeld "Control" s
  shift <- eitherHeld "Shift" s
  handleKey (ctrl, shift, get cursor s, get user s) e s
handleKey' _ _ s = return s

handleKey :: (Bool, Bool, Ivl, UserMode) -> Either [Char] Char -> State -> IO State
handleKey _ (Left "Escape") _ = error "User escape"

handleKey (True, _, ix, Normal) (Right ' ') s = return . setSel $ atSpan' ix s
 where
  setSel = maybe s (\(_, ivl) -> set user (Selection True) $ set cursor ivl s)

handleKey (_, _, ix, Normal) (Right k) s =
    setTimeout
  $ editCode [(ix, [k])] s

handleKey (ctrl, _, _, Normal) (Left k) s =
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

handleKey (_, _, ix, Selection _) (Right 'y') s =
  update =<< addClip (getSelection s) s

handleKey (_, _, ix, Selection _) (Right 'd') s =
  update =<< addClip (getSelection s)
    ( set cursor (fst ix, fst ix)
    . set user Normal
    $ editCode [(ix, "")] s ) 

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

handleKey _ _ s = return s

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


handleDisplay :: IPnt -> IRect -> State -> C.Render State
handleDisplay _ (tl, br) s@(State txt ix user p c (_, ypos) _ _ _ _) = do
  let textPos = (50.5, 200.5)
      height = (fromIntegral . snd $ br ^-^ tl) * 0.5
      astPos = textPos ^+^ (0.0, ypos - height)

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
  
  let pos = textPos ^-^ (0, 25)
--  whenJust p (\t -> drawTypes shapes pos c t >> drawGaps pos t)
  whenJust p (\t -> drawApps pos c t >> drawGaps pos t)

  -- Draw text
  setColor $ readColor "f6f3e8"
  move textPos
  C.showText txt
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
    return $ boundPoints [p ^+^ (3, 0), p ^+^ (0, 3), p ^-^ (0, 3), p ^-^ (3, 0)]
  drawPoly :: Int -> Double -> C.Render DRect
  drawPoly c r = drawShape (\pnt -> do
    let cnt = fromIntegral c
        (x:xs) = map (\i -> (pnt ^+^ fromPolar (3, r + i / cnt * 2 * pi))) [0..cnt-1]
    move x
    mapM_ (uncurry C.lineTo) xs)

drawTypes :: [C.Render DRect] -> DPoint -> TaskChan -> Results -> C.Render ()
drawTypes shs pos c (Results txt ftxt _ apps tm tc es) = do
  let prims = onub . sort . map (trim . prettyPrint . cannonicalType)
            . concatMap splitType $ M.elems tm
      cmap = M.fromList $ zip prims shs
  mapM_ (drawType cmap) (M.toList tm) {- ( catMaybes
                        . map (\(i1, i2) -> liftM (i1,) $ M.lookup i2 tm)
                        $ allIvls apps ) -}
  C.setFontSize 20
 where
  drawType :: M.Map String (C.Render DRect) -> (Ivl, Type) -> C.Render DRect
  drawType m (ivl, t) = do
    C.setFontSize 20
    sp <- spanLine txt ivl
    C.setFontSize 8
    move $ pos ^+^ (5, 0) ^+^ sp `at` 0
    rec [] t
   where
    rec :: Context -> Type -> C.Render DRect
    rec ctx t@(TyFun _ _) = do
      rect <- drawLTR (rec ctx) $ splitFunc t
      let line = toBezier $ offset (0, -4) $ rside 0 rect
      setColor ((1.0, 1.0, 1.0) :: DColor)
      draw line
      drawArrow 4 1 0.8 line
      C.stroke
      return rect
    rec ctx (TyTuple _ xs) = do
      rect <- drawLTR (rec ctx) xs
      setColor ((1.0, 1.0, 1.0) :: DColor)
      surround "(" ")" rect
    rec ctx (TyList t) = do
      rect <- rec ctx t
      setColor ((1.0, 1.0, 1.0) :: DColor)
      surround "[" "]" rect
    rec ctx t@(TyVar _) = prim ctx t
    rec ctx t@(TyCon _) = prim ctx t
    rec ctx (TyForall _ ctx' t) = rec (ctx' ++ ctx) t
    rec ctx t = fallback t
    prim ctx t =
      case M.lookup (trim . prettyPrint $ addCtx ctx t) m of
        Just s -> s
        Nothing -> fallback t
    fallback t = do
      let typ = trim $ prettyPrint t
      setColor ((1.0, 1.0, 1.0) :: DColor)
      sz <- textSize typ
      fr <- C.getCurrentPoint
      relMove (0, snd sz / 2)
      C.showText typ
      return $ boundPoints [fr, fr ^+^ (fst sz, 0)]

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

drawApps :: DPoint -> TaskChan -> Results -> C.Render ()
drawApps pos c (Results txt ftxt _ apps tm tc es) = do
  mapM_ drawError es
  drawLegend tc
  foldM_ (drawFunc tm tc) IM.empty $ reverse apps
 where
  drawLegend :: ColorMap -> C.Render ()
  drawLegend = zipWithM_ (\y (t, c) -> do
      setColor c
      relText 0 (fst pos, y) t)
    [220, 240..] . M.toList
  drawError :: (Ivl, String) -> C.Render ()
  drawError ((f, t), err) = do
    rect <- textRect txt f t
    setColor ((1.0, 0.2, 0.2) :: DColor)
    let (x, y) = (middle $ moveR pos rect)
    C.moveTo x (y + x)
    C.setFontSize 8
    C.showText err
    C.setFontSize 20
    C.lineTo x y
    C.stroke
  drawFunc :: TypeMap -> ColorMap -> HeightMap -> (Ivl, Ivl, [Ivl]) -> C.Render HeightMap
  drawFunc tm tc hm (func, _, params) = do
    fspan <- spanLine txt func
    foldM (\h -> drawFunc tm tc h . (, undefined, [])) hm 
          $ filter (not . (`elem` (map fst3 apps))) params
    let pts = liftM (map cannonicalType . drop (length params + 1) . splitFunc)
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

collate :: (Data a, Typeable b) => (b -> [c]) -> a -> [c]
collate f = concat . gmapQ (collate f `extQ` f)

update :: State -> IO State
update s = (parsed' $ partialParse A.parseDecl txt) >>= flip (setM parsed) s
 where
  txt = get code s 
  parsed' (Just (decl, txt', errs)) = do
    let apps = concatApps $ collate getApps decl
    tm <- getTypeMap txt' (get chan s) apps
--    mapM (print . (`atSpan` decl)) $ M.keys tm
    let tc = getColorMap tm apps
    return (Just $ Results txt txt' decl apps tm tc errs)
  parsed' Nothing = return Nothing


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

{-
getType c t = liftM (mapEither errParse id) . interpret c "MyMain" . I.typeOf $ deMongol t
 where
  errParse (I.WontCompile errs) = map (parseGHCError . debug . I.errMsg) errs
  errParse e = [UnknownError (show e)]
-}

{-
getType _ t = I.runInterpreter $ do
  I.set [I.searchPath I.:= ["source"]]
  I.loadModules ["MyMain"]
  I.setTopLevelModules ["MyMain"]
  I.typeOf t
-}

gapChar :: Char
gapChar = '\x180E'

getType :: TaskChan -> String -> IO (Either I.InterpreterError String)
getType c t = interpret c "MyMain" . Simple.typeOf $ deMongol t

preferOk :: ParseResult a -> Maybe a
preferOk = rightToMaybe . parseResultToEither

-- Actually unecessary
deMongol :: [Char] -> [Char]
deMongol = map (\c -> if c == gapChar then ' ' else c)

-- TODO: this needs to pass in declaration parameters

getTypeMap :: String -> TaskChan -> Apps -> IO TypeMap
getTypeMap txt c apps = do
  res <- getType c . recText1 $ words txt !! 0
  let rec = isRight res
  print rec
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
  , "e7f6da"
  ]

readColor :: String -> (Int, Int, Int)
readColor = toIColor . toSRGB24 . sRGB24read

--toIColor :: RGB Double -> (Int, Int, Int)
toIColor c =
  (fromIntegral $ channelRed   c, 
   fromIntegral $ channelGreen c,
   fromIntegral $ channelBlue  c)
