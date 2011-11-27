{-# LANGUAGE FlexibleInstances, TemplateHaskell, TupleSections, TypeOperators,
TypeFamilies, ParallelListComp, NoMonomorphismRestriction, ScopedTypeVariables,
QuasiQuotes, RankNTypes, ViewPatterns #-}

module Utils where

-- import Control.Applicative (liftA2, pure, (<*>))

import Control.Arrow ((***), (&&&), first, second)
import Control.Category ((.))
import Control.Concurrent.MVar
import Control.Monad (liftM, msum)
import qualified Control.Monad.State as ST
import Data.Char (isSpace)
import Data.Curve.Util (foldT)
import Data.Data hiding (typeOf)
import Data.Function (on)
import Data.Generics.Text (gshow)
import Data.Generics.Schemes (listify)
import Data.IORef
import Data.Label
import Data.List (sort, groupBy, sortBy, findIndex, (\\), isPrefixOf, partition)
import Data.Maybe
import qualified Data.Map as M
import Data.Ord (comparing)
import Data.Generics.Aliases
import Debug.Trace
import Language.Haskell.Exts hiding (parseType)
import qualified Language.Haskell.Exts.Annotated as A
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.TH.Quote
import Prelude hiding ((.))
import qualified Text.Regex.PCRE.Rex as PCRE
-- import qualified Text.Regex.PCRE.Light as PCRE

-- Actual utils.
-------------------------------------------------------------------------------

rex :: QuasiQuoter
rex = PCRE.makeQuasiMultiline $ PCRE.rexConf False False "id" [] []

modM :: Monad m => (b :-> a) -> (a -> a) -> b -> m b
modM l f = return . modify l f

setM :: Monad m => (b :-> a) -> a -> b -> m b
setM l x = return . set l x

lensed :: (f :-> a) -> (f :-> a') -> (a -> a') -> f -> f
lensed l l' f s = set l' (f $ get l s) s

maybeLens :: Maybe a :-> a
maybeLens = lens (fromJust) (\v _ -> Just v)

headLens :: [a] :-> a
headLens = lens head setHead
  where
   setHead x [] = [x]
   setHead x (_:xs) = x : xs

modifyIORefM :: IORef a -> (a -> IO a) -> IO ()
modifyIORefM r f = readIORef r >>= f >>= writeIORef r

firstM :: Monad m => (a -> m c) -> (a, b) -> m (c, b)
firstM f = (\(x, y) -> f x >>= \x' -> return (x', y))

secondM :: Monad m => (b -> m c) -> (a, b) -> m (a, c)
secondM f = (\(x, y) -> f y >>= \y' -> return (x, y'))

liftFst :: Monad m => (t, m a) -> m (t, a)
liftFst (x, y) = y >>= return . (x,)

liftSnd :: Monad m => (m a, t) -> m (a, t)
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

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

atMay :: Num n => [a] -> n -> Maybe a
atMay [] _ = Nothing
atMay (x:_) 0 = Just x
atMay (_:xs) n = atMay xs (n-1)

pairGroupBy :: (t -> t -> Bool) -> [t] -> [[t]]
pairGroupBy f [] = []
pairGroupBy f [x] = [[x]]
pairGroupBy f (x:y:xs) 
  = if f x y then (x:y'):ys' else [x]:y':ys'
 where
  (y':ys') = pairGroupBy f (y:xs)

onubBy :: (b -> b -> Bool) -> [b] -> [b]
onubBy f = map head . pairGroupBy f

onub :: Eq b => [b] -> [b]
onub = onubBy (==)

onubSortBy :: Ord a => (b -> a) -> [b] -> [b]
onubSortBy f = onubBy ((==) `on` f) . sortBy (comparing f)

-- from Emil Axelsson's 'syntactic' package
fullPartition :: (a -> a -> Bool) -> [a] -> [[a]]
fullPartition eq []     = []
fullPartition eq (a:as) = (a:as1) : fullPartition eq as2
  where
    (as1,as2) = partition (eq a) as

-- Not super efficient, but ohwell.
transitivePartition :: (a -> a -> Bool) -> [a] -> [[a]]
transitivePartition f [] = []
transitivePartition f (x:xs) = xs1' : transitivePartition f xs2'
 where
  (xs1', xs2') = helper [x] xs
  helper [] xs = ([], xs)
  helper ys xs = first (xs1++) $ helper xs1 xs2
   where
    (xs1, xs2) = partition (\x -> any (`f` x) ys) xs

extractFirst :: (a -> Bool) -> [a] -> Maybe (a, [a])
extractFirst fn [] = Nothing
extractFirst fn (a:as) =
   if fn a then Just (a,as) else extractFirst fn as >>= return . second (a:)

deleteFirst :: (a -> Bool) -> [a] -> [a]
deleteFirst fn = snd . fromJust . extractFirst fn

dropLast :: [a] -> [a] 
dropLast [x] = []
dropLast (x:xs) = x : dropLast xs

debug :: Show a => a -> a
debug x = trace (show x) x

debug' :: Show a => String -> a -> a
debug' pre x = trace (pre ++ show x) x

debugWith :: (a -> String) -> a -> a
debugWith f x = trace (f x) x

gdebug :: Data a => a -> a
gdebug x = trace (gshow x) x

gdebug' :: Data a => String -> a -> a
gdebug' pre x = trace (pre ++ gshow x) x

-- | Conditionally run an action, using a @Maybe a@ to decide.
whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (return ()) f mg

-- | Conditionally run an action, and yield result.
maybeM :: Monad m => Maybe a -> (a -> m b) -> m (Maybe b)
maybeM x f = maybe (return Nothing) (liftM Just . f) x

-- takes an IO function and returns a cached version
memoIO :: Ord k => (k -> IO b) -> IO (k -> IO b)
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
groupSortOn :: Ord a1 => (a -> a1) -> [a] -> [[a]]
groupSortOn f = groupBy ((==) `on` f) . sortBy (comparing f)

-- Eliminate redundant fst, concatenate together snds.
combineFstOn :: Ord c => ((a, b) -> c) -> [(a, b)] -> [(a, [b])]
combineFstOn f = map (\xs -> (fst $ head xs, map snd xs)) . groupSortOn f

substr :: (Int, Int) -> [a] -> [a]
substr (f, t) = take (t - f) . drop f

subst :: (Int, Int) -> [a] -> [a] -> [a]
subst (f, t) xs ys = (take f ys) ++ xs ++ drop t ys

type Edit = (Ivl, String)

applyEdits :: [Edit] -> String -> String
applyEdits = flip $ foldr (uncurry subst)

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

leftToMaybe :: Either a b -> Maybe a
leftToMaybe  = either Just (const Nothing)

mapEither :: (a -> a') -> (b -> b') -> Either a b -> Either a' b'
mapEither f g = either (Left . f) (Right . g)

isRight :: Either t t1 -> Bool
isRight (Right _) = True
isRight _ = False

trim :: String -> String
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

manyNames :: [String]
manyNames = ["__" ++ filter (not . isSpace) [a, b, c] | c <- az, b <- az, a <- tail az ]
 where
  az = ' ' : ['a'..'z']

-- From Yi code.  Defns seem backwards..
padLeft, padRight :: Int -> String -> String
padLeft n [] = replicate n ' '
padLeft n (x:xs) = x : padLeft (n-1) xs
padRight n = reverse . padLeft n . reverse

-- From Thomas Eding's monadlist
scanM :: (Monad m, ST.MonadPlus p) => (a -> b -> m a) -> a -> [b] -> m (p a)
scanM _ z [] =  return $ return z
scanM f z (x:xs) = do
  z' <- f z x
  liftM (ST.mplus $ return z) $ scanM f z' xs

ivlHull :: (Ord t, Ord t1) => (t, t1) -> (t, t1) -> (t, t1)
ivlHull (f1, t1) (f2, t2) = (min f1 f2, max t1 t2)


-- Haskell-src-exts utils.
-------------------------------------------------------------------------------

type Ivl = (Int, Int) 

ivlWidth :: (Int, Int) -> Int
ivlWidth = foldT subtract

colSpan :: SrcSpan -> Ivl
colSpan = (subtract 1 *** subtract 1) . (srcSpanStartColumn &&& srcSpanEndColumn)

getSpan :: (Data a) => a -> Maybe SrcSpan
getSpan = listToMaybe . catMaybes
        . gmapQ (const Nothing `extQ` (Just . srcInfoSpan))

getSpan' :: (Data a) => a -> Ivl
getSpan' = colSpan . fromJust . getSpan

spanContains :: SrcSpan -> SrcLoc -> Bool 
spanContains (SrcSpan f sl sc el ec) (SrcLoc f' l c)
  =  (if sl == l then sc <= c else sl < l)
  && (if el == l then ec >= c else el > l)

preferOk :: ParseResult a -> Maybe a
preferOk = rightToMaybe . parseResultToEither

mustOk :: ParseResult a -> a
mustOk = fromJust . preferOk

sp :: SrcSpanInfo
sp = SrcSpanInfo (SrcSpan "" 0 0 0 0) [] --error "srcspan"

twhered :: [DeclS] -> ExpS
twhered funcs
  = A.Let sp (A.BDecls sp funcs)
  $ A.Tuple sp (map (mkPlain . get funName) funcs)

dwhered :: (ExpS, [DeclS]) -> ExpS
dwhered (e, funcs) = A.Let sp (A.BDecls sp funcs) e

mutate :: ST.MonadState s m => (s -> s) -> m s
mutate f = do
  x <- ST.get
  ST.put (f x)
  return x

-- TODO: consider writing Type information into application tree?

unPlain :: A.Exp t -> String
unPlain (A.Var _ (A.UnQual _ (A.Ident _ n))) = n

mkPlain :: String -> ExpS
mkPlain n = (A.Var sp (A.UnQual sp (A.Ident sp n)))

mkFun :: String -> ExpS -> DeclS
mkFun n e = A.FunBind (A.ann e)
  [ A.Match sp (A.Ident sp n) [] (A.UnGuardedRhs sp e) Nothing ]

matchName :: MatchS :-> String
matchName = lens getName setName
 where
  getName (A.Match _ (A.Ident _ n) _ _ _) = n
  setName n (A.Match b (A.Ident c _) d e f) = A.Match b (A.Ident c n) d e f

matchExpr :: MatchS :-> ExpS
matchExpr = lens getExpr setExpr
 where
  getExpr (A.Match _ _ _ (A.UnGuardedRhs _ e) _ ) = e
  setExpr f (A.Match a c d (A.UnGuardedRhs b _) e) =
            (A.Match a c d (A.UnGuardedRhs a f) e)

funMatches :: DeclS :-> [MatchS]
funMatches = lens getMatches setMatches
 where
  getMatches (A.FunBind _ ms) = ms
  setMatches ms (A.FunBind a _) = A.FunBind a ms

funName :: DeclS :-> String
funName = lens getName setName . funMatches
 where
  getName ((A.Match _ (A.Ident _ n) _ _ _):_) = n
  setName n = map (\(A.Match a (A.Ident b _) c d e) ->
                     A.Match a (A.Ident b n) c d e)

funExpr :: DeclS :-> ExpS
funExpr = matchExpr . headLens . funMatches

letBinds :: ExpS :-> [DeclS]
letBinds = lens getBinds setBinds
 where
  getBinds (A.Let _ (A.BDecls _ xs) _) = xs
  setBinds xs (A.Let a (A.BDecls b _) c) = A.Let a (A.BDecls b xs) c

funcAsExp :: DeclS -> ExpS
funcAsExp d = A.Let sp (A.BDecls sp [d]) (mkPlain $ get funName d)

{-
The theory can be described as: The typing environment which makes a
monomorphically-punned equivalent compile gives all of the information about the
polymorphic expression.

here's the procedure:

 1) Set the whole top-level declaration to 'undefined', in order to get the
 type, in the case that an explicit type is not specified.

 2) Whereify, and use recursive application of the seminal removal scheme

 3) Rename all of the created definitions, and replace the old names with
 explicitly typed dummy-undefineds, which assign a unique constructor to each
 potentially unique polymorphic variable.

 3) Use the type errors to incrementally guide creating a dummy typing
 environment.  The construction of this environment determines the causation of
 types within the subexpression being analyzed.
-}

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
    bs <- maybeM bnds $ mapM doBind
    cx <- rec ctx
    t' <- rec t
    whenJust bs $ mapM_ (removeVar old . fst)
    return $ TyForall (liftM (map snd) bs) cx t'
  doType t = gmapM rec t

-- NOTE: Throws away top foralls
-- TODO: shadow nested foralls
specializeTypes :: M.Map String Type -> Type -> Type
specializeTypes m (TyForall _ _ t) = specializeTypes m t
specializeTypes m t = rec t
 where
  rec :: (Data a) => a -> a
  rec = gmapT rec `extT` doName
  doName ((`M.lookup` m). prettyPrint -> Just t') = t'
  doName t = gmapT rec t

preserveContext :: Monad m => (Type -> m Type) -> Type -> m Type
preserveContext f (TyForall bnds ctx t) = f t >>= return . TyForall bnds ctx
preserveContext f t = f t


splitTApp :: Type -> [Type]
splitTApp = reverse . helper
 where
  helper (TyApp a b) = b : helper a
  helper t = [t]

buildTApp :: [Type] -> Type
buildTApp = helper . reverse
 where
  helper [t] = t
  helper (b : a) = TyApp (helper a) b

splitEApp :: ExpS -> [ExpS]
splitEApp = reverse . helper
 where
  helper (A.App _ a b) = b : helper a
  helper t = [t]

buildEApp :: [ExpS] -> ExpS
buildEApp = helper . reverse
 where
  helper [t] = t
  helper (b : a) = A.App sp (buildEApp a) b

splitTFunc :: Type -> [Type]
splitTFunc (TyFun a b) = a : splitTFunc b
splitTFunc t = [t]

buildTFunc :: [Type] -> Type
buildTFunc [t] = t
buildTFunc (a : b) = TyFun a (buildTFunc b)


getVar :: Name -> [String]
getVar (Ident n) = [n]
getVar (Symbol n) = [n]

getVarA :: NameS -> [String]
getVarA (A.Ident _ n) = [n]
getVarA (A.Symbol _ n) = [n]

getVars :: Data a => a -> [String]
getVars = listAll (const [] `extQ` getVarA `extQ` getVar)

listAll :: forall b c. (Data b) => (forall a. (Data a) => a -> [c]) -> b -> [c]
listAll f = concat . gmapQ (\n -> f n ++ listAll f n)

deQual :: Data a => a -> a
deQual = gmapT (deQual `extT` doA `extT` doE)
 where
  doA :: A.QName SrcSpanInfo-> A.QName SrcSpanInfo
  doA (A.Qual s a b) = A.UnQual s b
  doA x = x
  doE (Qual a b) = UnQual b
  doE x = x

reQual :: Data a => String -> a -> a
reQual n = gmapT (reQual n `extT` doE)
 where
  doE (UnQual a) = Qual (ModuleName n) a
  doE x = x

-- Free variables in the expression.
freeEVars :: ExpS -> [String]
freeEVars (A.Lambda _ vs e) = freeEVars e \\ getVars vs
freeEVars e = rec e
 where
  rec :: (Data a) => a -> [String]
  rec = (concat . gmapQ (rec `extQ` freeEVars)) `extQ` getVarA

-- Free polymorphic variables in the type.
freeTVars :: Type -> [String]
freeTVars (TyForall bnds _ t) = freeTVars t \\ getVars bnds
freeTVars t = rec t
 where
  rec :: (Data a) => a -> [String]
  rec = (concat . gmapQ (rec `extQ` freeTVars)) `extQ` getVar

-- Get 'primitives', things replaceable by dot and line
{-
uniquePrims :: [Type] -> [Type]
uniquePrims = map head . group . sort . concatMap getPrims
isPrim :: Type -> Bool
isPrim t@(TyVar _) = True
isPrim t@(TyCon _) = True
isPrim _ = False
getPrims :: Type -> [Type]
getPrims = listify isPrim
-}

-- TODO: transitive closure on polymorphic var usage..
addCtx :: [Asst] -> Type -> Type
addCtx [] t = t
addCtx ctx t = uncurry3 TyForall
             . second3 (++ (map snd . filter (overlaps $ freeTVars t)
                                    $ map (\a -> (getVars a, a)) ctx))
             $ case t of
               f@(TyForall b c i) -> (b, c, i)
               t -> (Nothing, [], t)
 where
  overlaps xs (ys, _) = any (`elem` ys) xs
  
getExp :: (Data a) => a -> Maybe ExpS
getExp = (const Nothing) `extQ` Just

ivlContains :: Ivl -> Ivl -> Bool
ivlContains (f, t) (f', t') = f <= f' && t' <= t

atSpan :: (Data a) => Ivl -> a -> Maybe ExpS
atSpan s = child `extQ` (\x -> ifContains (whenExp x) x)
 where
  ifContains :: (Data a) => Maybe b -> a -> Maybe b
  ifContains b x = do
    sp <- getSpan x
    if colSpan sp `ivlContains` s then b else Nothing
  whenExp :: ExpS -> Maybe ExpS
  whenExp x = maybe (Just x) Just $ child x
  child :: (Data x) => x -> Maybe ExpS
  child x = ifContains (msum $ gmapQ (atSpan s) x) x

type ExpS   = A.Exp   A.SrcSpanInfo
type DeclS  = A.Decl  A.SrcSpanInfo
type NameS  = A.Name  A.SrcSpanInfo 
type PatS   = A.Pat   A.SrcSpanInfo
type MatchS = A.Match A.SrcSpanInfo

instance (Eq a) => Eq (ParseResult a) where
  (ParseOk x)       == (ParseOk y)       = x == y
  (ParseFailed a x) == (ParseFailed b y) = x == y && a == b
  _ == _ = False
  
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
