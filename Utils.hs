{-# LANGUAGE FlexibleInstances, TemplateHaskell, TupleSections, TypeOperators,
ScopedTypeVariables, TypeFamilies, ParallelListComp, NoMonomorphismRestriction #-}

module Utils where

-- import Control.Applicative (liftA2, pure, (<*>))

import Control.Arrow ((&&&), second)
import Control.Concurrent.MVar
import Control.Monad (liftM)
import qualified Control.Monad.State as ST
import Data.Char (isSpace)
import Data.Data
import Data.Function (on)
import Data.IORef
import Data.Label
import Data.List (groupBy, sortBy, findIndex)
import Data.Maybe
import qualified Data.Map as M
import Data.Ord (comparing)
import Data.Generics.Aliases
import Debug.Trace
import Language.Haskell.Exts.Annotated
import Language.Haskell.Exts.Lexer
import Language.Haskell.Exts.ParseMonad
import Language.Haskell.Exts.SrcLoc

-- Actual utils.
-------------------------------------------------------------------------------

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

onub = onubBy (==)
onubBy f = map head . groupBy f

debug x = trace (show x) x

-- | Conditionally run an action, using a @Maybe a@ to decide.
whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (return ()) f mg

-- | Conditionally run an action, and yield result.
maybeM :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
maybeM f = maybe (return Nothing) (liftM Just . f)

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

wordIx :: Int -> Int -> String -> Int
wordIx cnt ix txt = getIx (cnt + maybe loffs id (findIndex (>ix) offs))
 where 
  offs = onubBy (\x y -> x + 1 == y) . map fst . filter (isSpace . snd) . zip [0..]
       $ txt ++ " "
  loffs = length offs
  getIx i | i < 0 = 0
          | i >= loffs = last offs
          | otherwise = offs !! i

ivlHull (f1, t1) (f2, t2) = (min f1 f2, max t1 t2)


-- Haskell-src-exts utils.
-------------------------------------------------------------------------------

type Ivl = (Int, Int) 

colSpan :: SrcSpan -> Ivl
colSpan = (srcSpanStartColumn &&& srcSpanEndColumn)

getSpan :: (Data a) => a -> Maybe SrcSpan
getSpan = listToMaybe . catMaybes
        . gmapQ (const Nothing `extQ` (Just . srcInfoSpan))

getSpan' :: (Data a) => a -> Ivl
getSpan' = colSpan . fromJust . getSpan

spanContains :: SrcSpan -> SrcLoc -> Bool 
spanContains (SrcSpan f sl sc el ec) (SrcLoc f' l c) =
  f == f' && (if sl == l then sc <= c else sl < l)
          && (if el == l then ec >= c else el > l)

lexify = runParser lexRec 
 where
  lexRec = runL (Lex lexer) (\x -> case unLoc x of
      EOF -> return []
      _ -> liftM (x:) lexRec)

-- Taken from Language.Haskell.Meta.Parse
parseResultToEither :: ParseResult a -> Either String a
parseResultToEither (ParseOk a) = Right a
parseResultToEither (ParseFailed loc e)
  = let line = srcLine loc - 1
    in Left (unlines [show line,show loc,e])

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
