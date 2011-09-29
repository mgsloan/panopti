{-# LANGUAGE FlexibleInstances, TemplateHaskell, TupleSections, TypeOperators,
TypeFamilies, ParallelListComp, NoMonomorphismRestriction, ScopedTypeVariables,
QuasiQuotes, RankNTypes #-}

module Utils where

-- import Control.Applicative (liftA2, pure, (<*>))

import Control.Arrow ((***), (&&&), second)
import Control.Concurrent.MVar
import Control.Monad (liftM, msum)
import qualified Control.Monad.State as ST
import Data.Char (isSpace)
import Data.Data hiding (typeOf)
import Data.Function (on)
import Data.Generics.Text (gshow)
import Data.Generics.Schemes (listify)
import Data.IORef
import Data.Label
import Data.List (sort, groupBy, sortBy, findIndex, (\\))
import Data.Maybe
import qualified Data.Map as M
import Data.Ord (comparing)
import Data.Generics.Aliases
import Debug.Trace
import Language.Haskell.Exts hiding (parseType)
import qualified Language.Haskell.Exts.Annotated as A
import Language.Haskell.Exts.Lexer
import Language.Haskell.Exts.ParseMonad
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.TH.Quote
import qualified Text.Regex.PCRE.Rex as PCRE
-- import qualified Text.Regex.PCRE.Light as PCRE

-- Actual utils.
-------------------------------------------------------------------------------

rex = PCRE.makeQuasiMultiline $ PCRE.rexConf False True "id" [] []

modM :: Monad m => (b :-> a) -> (a -> a) -> b -> m b
modM l f = return . modify l f

setM :: Monad m => (b :-> a) -> a -> b -> m b
setM l x = return . set l x

lensed :: (f :-> a) -> (f :-> a') -> (a -> a') -> f -> f
lensed l l' f s = set l' (f $ get l s) s

maybeLens :: Maybe a :-> a
maybeLens = lens (fromJust) (\v _ -> Just v)

modifyIORefM :: IORef a -> (a -> IO a) -> IO ()
modifyIORefM r f = readIORef r >>= f >>= writeIORef r

firstM f = (\(x, y) -> f x >>= \x' -> return (x', y))
secondM f = (\(x, y) -> f y >>= \y' -> return (x, y'))

liftFst (x, y) = y >>= return . (x,)
liftSnd (x, y) = x >>= return . (,y)

fst3 (x, _, _) = x
snd3 (_, x, _) = x
thd3 (_, _, x) = x

rmfst3 (_, a, b) = (a, b)
rmsnd3 (a, _, b) = (a, b)
rmthd3 (a, b, _) = (a, b)

first3  f (a, b, c) = (f a, b, c)
second3 f (a, b, c) = (a, f b, c)
third3  f (a, b, c) = (a, b, f c)

uncurry3 f (a, b, c) = f a b c

atMay [] _ = Nothing
atMay (x:_) 0 = Just x
atMay (_:xs) n = atMay xs (n-1)

pairGroupBy f [] = []
pairGroupBy f [x] = [[x]]
pairGroupBy f (x:y:xs) 
  = if f x y then (x:y'):ys' else [x]:y':ys'
 where
  (y':ys') = pairGroupBy f (y:xs)

onub = onubBy (==)
onubBy f = map head . pairGroupBy f

onubSortBy f = onubBy ((==) `on` f) . sortBy (comparing f)

debug x = trace (show x) x
gdebug x = trace (gshow x) x
gdebug' pre x = trace (pre ++ gshow x) x

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

substr (f, t) = take (t - f) . drop f

subst (f, t) xs ys = (take f ys) ++ xs ++ drop t ys

rightToMaybe = either (const Nothing) Just
leftToMaybe  = either Just (const Nothing)

mapEither f g = either (Left . f) (Right . g)

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
colSpan = (subtract 1 *** subtract 1) . (srcSpanStartColumn &&& srcSpanEndColumn)

getSpan :: (Data a) => a -> Maybe SrcSpan
getSpan = listToMaybe . catMaybes
        . gmapQ (const Nothing `extQ` (Just . srcInfoSpan))

getSpan' :: (Data a) => a -> Ivl
getSpan' = colSpan . fromJust . getSpan

spanContains :: SrcSpan -> SrcLoc -> Bool 
spanContains (SrcSpan f sl sc el ec) (SrcLoc f' l c) =
  f == f' && (if sl == l then sc <= c else sl < l)
          && (if el == l then ec >= c else el > l)
type STT = ST.State (String, M.Map String Char)

cannonicalType :: Type -> Type
cannonicalType t = ST.evalState (rec t) (['a'..'z'], M.empty)
 where
  rec :: (Data a, Typeable a) => a -> STT a
  rec = gmapM rec
   `extM` (\n -> doName False n >>= return . snd)
   `extM` (return :: QName -> STT QName)
   `extM` doType

  doName b (Ident n)  = doVar b n >>= \n' -> return (n, Ident n')
  doName b (Symbol n) = doVar b n >>= \n' -> return (n, Symbol n')

  -- Returns the new name associated with the given variable, or creates one.
  -- The bool parameter determines if prior the prior rewrite for this variable
  -- is overwritten
  doVar :: Bool -> String -> STT String
  doVar overwrite other = do
    (xs, m) <- ST.get
    case if overwrite then Nothing else M.lookup other m of
      Just x -> return [x]
      Nothing -> ST.put (tail xs, M.insert other (head xs) m)
              >> return [head xs]

  doBind (UnkindedVar n) = do
    (o, n') <- doName True n
    return (o, UnkindedVar n')
  
  doBind (KindedVar n k) = do
    (o, n') <- doName True n
    return (o, KindedVar n' k)

  removeVar :: M.Map String Char -> String -> STT ()
  removeVar old other = ST.modify (second $ \m ->
    maybe (M.delete other m) (\v -> M.insert other v old) (M.lookup other old))

  doType :: Type -> STT Type
  doType (TyForall bnds ctx t) = do
    (_, old) <- ST.get
    bs <- maybeM (mapM doBind) bnds
    cx <- rec ctx
    t' <- rec t
    whenJust bs $ mapM_ (removeVar old . fst)
    return $ TyForall (liftM (map snd) bs) cx t'
  doType t = gmapM rec t

splitFunc :: Type -> [Type]
splitFunc ty = case ty of
    (TyForall bnds ctx t) -> rec t ctx
    t -> rec t []
 where
  rec t ctx = case t of
    (TyFun a b) -> addCtx ctx a : rec b ctx
    t -> [addCtx ctx t]

-- Precondition for semantically correct splitting: type is cannonicalized
splitType :: Type -> [Type]
splitType (TyForall bnds ctx t) = map (addCtx ctx) $ splitType t
splitType t@(TyVar _) = [t] 
splitType t@(TyCon _) = [t] 
splitType t@(TyInfix _ n _) = [TyCon n]
splitType t = concat $ gmapQ (const [] `extQ` splitType) t

listAll :: forall b c. (Data b) => (forall a. (Data a) => a -> [c]) -> b -> [c]
listAll f = concat . gmapQ (\n -> f n ++ listAll f n)

getVar :: Name -> [String]
getVar (Ident n) = [n]
getVar (Symbol n) = [n]

getVars :: Data a => a -> [String]
getVars = listAll (const [] `extQ` getVar)

-- Free polymorphic variables in the type.
freeVars :: Type -> [String]
freeVars = filterForall 
 where
  rec :: (Data a) => a -> [String]
  rec = (concat . gmapQ (rec `extQ` filterForall)) `extQ` getVar
  filterForall :: Type -> [String]
  filterForall (TyForall bnds _ t) = rec t \\ getVars bnds
  filterForall t = rec t

-- TODO: transitive closure on polymorphic var usage..
addCtx [] t = t
addCtx ctx t = uncurry3 TyForall
             . second3 (++ (map snd . filter (overlaps $ freeVars t)
                                    $ map (\a -> (getVars a, a)) ctx))
             $ case t of
               f@(TyForall b c i) -> (b, c, i)
               t -> (Nothing, [], t)
 where
  overlaps xs (ys, _) = any (`elem` ys) xs
  
getExp :: (Data a) => a -> Maybe ExpS
getExp = (const Nothing) `extQ` Just

icontains :: Ivl -> Ivl -> Bool
icontains (f, t) (f', t') = f <= f' && t' <= t

atSpan :: (Data a) => Ivl -> a -> Maybe ExpS
atSpan s = child `extQ` (\x -> ifContains (whenExp x) x)
 where
  ifContains :: (Data a) => Maybe b -> a -> Maybe b
  ifContains b x = do
    sp <- getSpan x
    if colSpan sp `icontains` s then b else Nothing
  whenExp :: ExpS -> Maybe ExpS
  whenExp x = maybe (Just x) Just $ child x
  child :: (Data x) => x -> Maybe ExpS
  child x = ifContains (msum $ gmapQ (atSpan s) x) x

type ExpS = A.Exp SrcSpanInfo

{-
toExpList :: ExpS -> [ExpS]
toExpList e = e : catMaybes (gmapQ ((const Nothing) `extQ` Just) e)

fromExpList :: [ExpS] -> ExpS
fromExpList (e:ps) = ST.evalState (gmapM ((return . id) `extM` setParam) e) ps
 where
  setParam :: ExpS -> ST.State [ExpS] ExpS
  setParam _ = do
    (x:xs) <- ST.get
    ST.put xs
    return x

mutateExpList :: ([ExpS] -> [ExpS]) -> ExpS -> ExpS
mutateExpList f = fromExpList . f . toExpList
-}

lexify = runParserWithMode parseMode lexRec 
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

parseType :: String -> ParseResult Type
parseType = parseTypeWithMode parseMode

parseMode :: ParseMode
parseMode = ParseMode "" extensions False False (Just baseFixities)
 where
  extensions = 
    [ OverlappingInstances	 
    , UndecidableInstances	 
    , IncoherentInstances	 
    , RecursiveDo	 
    , ParallelListComp	 
    , MultiParamTypeClasses	 
    , NoMonomorphismRestriction	 
    , FunctionalDependencies	 
    , ExplicitForall	 
    , Rank2Types	 
    , RankNTypes	 
    , PolymorphicComponents	 
    , ExistentialQuantification	 
    , ScopedTypeVariables	 
    , ImplicitParams	 
    , FlexibleContexts	 
    , FlexibleInstances	 
    , EmptyDataDecls	 
    , CPP	 
    , KindSignatures	 
    , BangPatterns	 
    , TypeSynonymInstances	 
    , TemplateHaskell	 
    , ForeignFunctionInterface	 
    , Arrows	 
    , Generics	 
    , NoImplicitPrelude	 
    , NamedFieldPuns	 
    , PatternGuards	 
    , GeneralizedNewtypeDeriving	 
    , ExtensibleRecords	 
    , RestrictedTypeSynonyms	 
    , HereDocuments	 
    , MagicHash	 
    , TypeFamilies	 
    , StandaloneDeriving	 
    , UnicodeSyntax	 
    , PatternSignatures	 
    , UnliftedFFITypes	 
    , LiberalTypeSynonyms	 
    , TypeOperators	 
    , RecordWildCards	 
    , RecordPuns	 
    , DisambiguateRecordFields	 
    , OverloadedStrings	 
    , GADTs	 
    , MonoPatBinds	 
    , NoMonoPatBinds	 
    , RelaxedPolyRec	 
    , ExtendedDefaultRules	 
    , UnboxedTuples	 
    , DeriveDataTypeable	 
    , ConstrainedClassMethods	 
    , PackageImports	 
    , ImpredicativeTypes	 
    , NewQualifiedOperators	 
    , PostfixOperators	 
    , QuasiQuotes	 
    , TransformListComp	 
    , ViewPatterns	 
    , XmlSyntax	 
    , RegularPatterns	 
    , TupleSections
    ]
