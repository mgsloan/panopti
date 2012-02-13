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
import Data.Data hiding (typeOf)
import Data.Foldable (concat)
import Data.Function (on)
import Data.Generics.Aliases
import Data.Generics.Text (gshow)
import Data.Generics.Schemes (listify)
import Data.IORef
import Data.Label
import Data.List (sort, groupBy, sortBy, findIndex, (\\), isPrefixOf, partition, group)
import Data.Maybe
import qualified Data.Map as M
import Data.Ord (comparing)
import Debug.Trace
import Language.Haskell.Exts.Annotated
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.TH.Quote
import Prelude hiding ((.), concat)
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

atMay :: (Eq n, Num n) => [a] -> n -> Maybe a
atMay [] _ = Nothing
atMay (x:_) 0 = Just x
atMay (_:xs) n = atMay xs (n-1)


-- Next two from the "safe" package
liftMay :: (a -> b) -> (a -> Bool) -> (a -> Maybe b)
liftMay func test val = if test val then Nothing else Just $ func val

lastMay :: [a] -> Maybe a
lastMay = liftMay last null

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
transitivePartition f (x:xs) = (if null xs1' then id else (xs1':))
                             $ transitivePartition f xs2'
 where
  (xs1', xs2') = helper [x] xs
  helper [] xs = ([], xs)
  helper ys xs = first (xs1++) $ helper xs1 xs2
   where
    (xs1, xs2) = partition (\x -> any (`f` x) ys) xs

-- | Removes the first element matching the predicate, and yields the element.
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

pdebug' pre x = trace (pre ++ prettyPrint x) x

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

foldT :: (a -> b -> c) -> (a, b) -> c
foldT f (x, y) = f x y

ivlWidth :: (Int, Int) -> Int
ivlWidth = foldT subtract

ivlContains :: Ivl -> Ivl -> Bool
ivlContains (f, t) (f', t') = f <= f' && t' <= t

csort :: (a -> Ivl) -> [a] -> [a]
csort f = sortBy $ comparing (\x -> ivlWidth $ f x)
--csort f = sortBy $ comparing (\x -> length . filter ((`ivlContains` f x) . f))

colSpan :: SrcSpan -> Ivl
colSpan = (subtract 1 *** subtract 1) . (srcSpanStartColumn &&& srcSpanEndColumn)

getSpan :: (Data a) => a -> Maybe SrcSpan
getSpan = listToMaybe . catMaybes
        . gmapQ (const Nothing `extQ` (Just . srcInfoSpan))

getSpan' :: (Data a) => a -> Ivl
getSpan' = colSpan . fromJust . getSpan

annSpan :: (Annotated a) => a SrcSpanInfo -> Ivl
annSpan = colSpan . srcInfoSpan . ann

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
  = Let sp (BDecls sp funcs)
  $ Tuple sp (map (mkPlain . get funName) funcs)

dwhered :: (ExpS, [DeclS]) -> ExpS
dwhered (e, funcs) = Let sp (BDecls sp funcs) e

mutate :: ST.MonadState s m => (s -> s) -> m s
mutate f = do
  x <- ST.get
  ST.put (f x)
  return x

-- TODO: consider writing Type information into application tree?

unPlain :: Exp t -> String
unPlain (Var _ (UnQual _ (Ident _ n))) = n

mkPlain :: String -> ExpS
mkPlain n = (Var sp (UnQual sp (Ident sp n)))

mkFun :: String -> ExpS -> DeclS
mkFun n e = FunBind (ann e)
  [ Match sp (Ident sp n) [] (UnGuardedRhs sp e) Nothing ]

matchName :: MatchS :-> String
matchName = lens getName setName
 where
  getName (Match _ (Ident _ n) _ _ _) = n
  setName n (Match b (Ident c _) d e f) = Match b (Ident c n) d e f

matchExpr :: MatchS :-> ExpS
matchExpr = lens getExpr setExpr
 where
  getExpr (Match _ _ _ (UnGuardedRhs _ e) _ ) = e
  setExpr f (Match a c d (UnGuardedRhs b _) e) =
            (Match a c d (UnGuardedRhs a f) e)

funMatches :: DeclS :-> [MatchS]
funMatches = lens getMatches setMatches
 where
  getMatches (FunBind _ ms) = ms
  setMatches ms (FunBind a _) = FunBind a ms

funName :: DeclS :-> String
funName = lens getName setName . funMatches
 where
  getName ((Match _ (Ident _ n) _ _ _):_) = n
  setName n = map (\(Match a (Ident b _) c d e) ->
                     Match a (Ident b n) c d e)

funExpr :: DeclS :-> ExpS
funExpr = matchExpr . headLens . funMatches

letBinds :: ExpS :-> [DeclS]
letBinds = lens getBinds setBinds
 where
  getBinds (Let _ (BDecls _ xs) _) = xs
  setBinds xs (Let a (BDecls b _) c) = Let a (BDecls b xs) c

contextList :: Maybe ContextS :-> [AsstS]
contextList = lens getList setList
 where
   getList Nothing = []
   getList (Just (CxEmpty _)) = []
   getList (Just (CxSingle _ a)) = [a]
   getList (Just (CxTuple _ as)) = as
   getList (Just (CxParen _ a)) = getList $ Just a
   setList []  c = Just $ CxEmpty  (maybe sp ann c)
   setList [a] c = Just $ CxSingle (maybe sp ann c) a
   setList as  c = Just $ CxTuple  (maybe sp ann c) as

funcAsExp :: DeclS -> ExpS
funcAsExp d = Let sp (BDecls sp [d]) (mkPlain $ get funName d)

type STT = ST.State (String, M.Map String Char)

cannonicalType :: TypeS -> TypeS
cannonicalType t = ST.evalState (rec t) (['a'..'z'], M.empty)
 where
  rec :: (Data a, Typeable a) => a -> STT a
  rec = gmapM rec
   `extM` handleName
   `extM` (return :: QNameS -> STT QNameS)
   `extM` doType

  handleName :: NameS -> STT NameS
  handleName n = doName False n >>= return . snd

  doName b (Ident s n)  = doVar b n >>= \n' -> return (n, Ident s n')
  doName b (Symbol s n) = doVar b n >>= \n' -> return (n, Symbol s n')

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

  doBind (UnkindedVar s n) = do
    (o, n') <- doName True n
    return (o, UnkindedVar s n')
  
  doBind (KindedVar s n k) = do
    (o, n') <- doName True n
    return (o, KindedVar s n' k)

  removeVar :: M.Map String Char -> String -> STT ()
  removeVar old other = ST.modify (second $ \m ->
    maybe (M.delete other m) (\v -> M.insert other v old) (M.lookup other old))

  doType :: TypeS -> STT TypeS
  doType (TyForall s bnds ctx t) = do
    (_, old) <- ST.get
    bs <- maybeM bnds $ mapM doBind
    cx <- rec ctx
    t' <- rec t
    whenJust bs $ mapM_ (removeVar old . fst)
    return $ TyForall s (liftM (map snd) bs) cx t'
  doType t = gmapM rec t

-- NOTE: Throws away top foralls
-- TODO: shadow nested foralls
specializeTypes :: M.Map String TypeS -> TypeS -> TypeS
specializeTypes m (TyForall s _ _ t) = specializeTypes m t
specializeTypes m t = rec t
 where
  rec :: (Data a) => a -> a
  rec = gmapT rec `extT` doName
  doName ((`M.lookup` m). prettyPrint -> Just t') = t'
  doName t = gmapT rec t

preserveContext :: Monad m => (TypeS -> m TypeS) -> TypeS -> m TypeS
preserveContext f (TyForall s bnds ctx t) = f t >>= return . TyForall s bnds ctx
preserveContext f t = f t

splitTApp :: TypeS -> [TypeS]
splitTApp = reverse . helper
 where
  helper (TyApp _ a b) = b : helper a
  helper t = [t]

buildTApp :: [TypeS] -> TypeS
buildTApp = helper . reverse
 where
  helper [t] = t
  helper (b : a) = TyApp sp (helper a) b

splitEApp :: ExpS -> [ExpS]
splitEApp = reverse . helper
 where
  helper (App _ a b) = b : helper a
  helper t = [t]

buildEApp :: [ExpS] -> ExpS
buildEApp = helper . reverse
 where
  helper [t] = t
  helper (b : a) = App sp (helper a) b

splitTFunc :: TypeS -> [TypeS]
splitTFunc (TyFun _ a b) = a : splitTFunc b
splitTFunc t = [t]

buildTFunc :: [TypeS] -> TypeS
buildTFunc [t] = t
buildTFunc (a : b) = TyFun sp a (buildTFunc b)

getVar :: NameS -> [String]
getVar (Ident _ n) = []
getVar (Symbol _ n) = [n] -- [n]

getVars :: Data a => a -> [String]
getVars = listAll (const [] `extQ` getVar)

listAll :: forall b c. (Data b) => (forall a. (Data a) => a -> [c]) -> b -> [c]
listAll f = concat . gmapQ (\n -> f n ++ listAll f n)

deQual :: Data a => a -> a
deQual = gmapT (deQual `extT` doA)
 where
  doA :: QNameS -> QNameS
  doA (Qual s a b) = UnQual s b
  doA x = x

reQual :: Data a => String -> a -> a
reQual n = gmapT (reQual n `extT` doE)
 where
  doE (UnQual s a) = Qual s (ModuleName sp n) a
  doE x = x

-- Free variables in the expression.
freeEVars :: ExpS -> [String]
freeEVars (Lambda _ vs e) = freeEVars e \\ getVars vs
freeEVars e = rec e
 where
  rec :: (Data a) => a -> [String]
  rec = (concat . gmapQ (rec `extQ` freeEVars)) `extQ` getVar

-- Free polymorphic variables in the type.
freeTVars :: TypeS -> [String]
freeTVars (TyForall _ bnds _ t) = freeTVars t \\ getVars bnds
freeTVars t = rec t
 where
  rec :: (Data a) => a -> [String]
  rec = (concat . gmapQ (rec `extQ` freeTVars)) `extQ` getVar

-- All of the named things - variables and constructors.
uniquePrims :: [TypeS] -> [TypeS]
uniquePrims = map head . group . sort . concatMap getPrims

getPrims :: TypeS -> [TypeS]
getPrims = listify isPrim

isPrim :: TypeS -> Bool
isPrim t@(TyVar _ _) = True
isPrim t@(TyCon _ _) = True
isPrim _ = False

type BindS = [TyVarBind SrcSpanInfo]

-- TODO: transitive closure on polymorphic var usage..
setCtx :: BindS -> [AsstS] -> TypeS -> TypeS
setCtx [] [] t = t
setCtx bnds ctx t 
  = case t of
      (TyForall _ _ _ t') -> setCtx bnds ctx t'
      _ -> TyForall l (Just usedBnds) (Just $ CxTuple l usedAssts) t
 where
  l = ann t
  overlaps xs ys = any (`elem` ys) xs
  usedAssts = [ c | c <- ctx,  overlaps (getVars c) (freeTVars t) ]
  usedBnds  = [ b | b <- bnds, overlaps (getVars b) (freeTVars t) ]
 
getExp :: (Data a) => a -> Maybe ExpS
getExp = (const Nothing) `extQ` Just

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

-- Convenient aliases for annotated AST types
type ExpS     = Exp     SrcSpanInfo
type DeclS    = Decl    SrcSpanInfo
type NameS    = Name    SrcSpanInfo 
type PatS     = Pat     SrcSpanInfo
type MatchS   = Match   SrcSpanInfo
type TypeS    = Type    SrcSpanInfo
type ContextS = Context SrcSpanInfo
type AsstS    = Asst    SrcSpanInfo
type QNameS   = QName   SrcSpanInfo

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

parseIt = parseWithMode parseMode

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
