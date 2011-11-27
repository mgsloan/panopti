module Input where

import State
import Utils

import Control.Monad (liftM, msum)
import Data.IORef
import Data.Label
import qualified Data.Map as M

handleKey' :: (State -> IO State)
           -> Bool -> Either [Char] Char -> State -> IO State
handleKey' setTimeout True e s = do
  ctrl <- eitherHeld "Control" s
  shift <- eitherHeld "Shift" s
  h (ctrl, shift, get cursor s, get user s) e s
 where

  h :: (Bool, Bool, Ivl, UserMode) -> Either [Char] Char -> State -> IO State
  -- Escape = Quit
  h _ (Left "Escape") _ = error "User escape"

  -- Ctrl p = save svg screenshot to out.svg
  {-
  h (True, _, _, _) (Right 'p') st =
    C.withSVGSurface "out.svg" 640 480 
      (\surf -> `C.renderWith` handleDisplay undefined ((0,0),(640, 480)) st)
  -}

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
  {-
  h (_, _, ix, Selection _) (Right 'y') s =
    setTimeout =<< addClip (getSelection s) s

  -- d, in selection mode = Cut
  h (_, _, ix, Selection _) (Right 'd') s =
    setTimeout =<< addClip (getSelection s)
      ( set cursor (fst ix, fst ix)
      . set user Normal
      $ editCode [(ix, "")] s ) 
  -}

  h _ _ s = return s

handleKey' _ _ _ s = return s

keyHeld :: String -> State -> IO Bool
keyHeld key = liftM (maybe False (\(a,_,_)->a) . (M.lookup key) . fst)
            . readIORef . get selfRef

eitherHeld :: [Char] -> State -> IO Bool
eitherHeld key s = do
  l <- keyHeld (key ++ "_L") s
  r <- keyHeld (key ++ "_R") s
  return (l || r)

{-
addClip :: String -> State -> IO State
addClip x s = getType (get chan s) x
          >>= \t -> return $ modify clips ((x, rightToMaybe t):) s
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
