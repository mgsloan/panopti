{-# LANGUAGE FlexibleInstances, TemplateHaskell, TupleSections, TypeOperators,
TypeFamilies, ParallelListComp, NoMonomorphismRestriction, ScopedTypeVariables,
QuasiQuotes, RankNTypes, ViewPatterns #-}

module Utils where

-- import Control.Applicative (liftA2, pure, (<*>))

import Control.Arrow ((***), (&&&), first, second)
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
import Data.List (sort, groupBy, sortBy, findIndex, (\\), isPrefixOf, partition)
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

rex :: QuasiQuoter
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

onub :: Eq b => [b] -> [b]
onub = onubBy (==)

onubBy :: (b -> b -> Bool) -> [b] -> [b]
onubBy f = map head . pairGroupBy f

onubSortBy :: Ord a => (b -> a) -> [b] -> [b]
onubSortBy f = onubBy ((==) `on` f) . sortBy (comparing f)

debug :: Show a => a -> a
debug x = trace (show x) x

debug' :: Show a => String -> a -> a
debug' pre x = trace (pre ++ show x) x

gdebug :: Data a => a -> a
gdebug x = trace (gshow x) x

gdebug' :: Data a => String -> a -> a
gdebug' pre x = trace (pre ++ gshow x) x

-- | Conditionally run an action, using a @Maybe a@ to decide.
whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (return ()) f mg

-- | Conditionally run an action, and yield result.
maybeM :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
maybeM f = maybe (return Nothing) (liftM Just . f)

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

ivlHull :: (Ord t, Ord t1) => (t, t1) -> (t, t1) -> (t, t1)
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


preferOk :: ParseResult a -> Maybe a
preferOk = rightToMaybe . parseResultToEither

manyNames :: [String]
manyNames = ["__" ++ filter (not . isSpace) [a, b, c] | c <- az, b <- az, a <- tail az ]
 where
  az = ' ' : ['a'..'z']

decompApp :: ExpS -> [ExpS]
decompApp = reverse . helper
 where
  helper (A.App _ l r) = r : decompApp l
  helper x = [x]

sp :: SrcSpanInfo
sp = SrcSpanInfo (SrcSpan "" 0 0 0 0) [] --error "srcspan"

buildApp :: [ExpS] -> ExpS
buildApp = helper . reverse
 where
  helper [x] = x
  helper (x:xs) = A.App sp (buildApp xs) x

twhered :: String -> (ExpS, [DeclS]) -> ExpS
twhered n (_, funcs) 
  = A.Let sp (A.BDecls sp funcs)
  $ A.Tuple sp (map (mkPlain . funName) funcs)

dwhered :: String -> (ExpS, [DeclS]) -> ExpS
dwhered n (e, funcs) = A.Let sp (A.BDecls sp funcs) e

mutate :: ST.MonadState s m => (s -> s) -> m s
mutate f = do
  x <- ST.get
  ST.put (f x)
  return x

type WST = ST.State ([String], [DeclS])

whereify :: ExpS -> (ExpS, [DeclS])
whereify top = second snd $ ST.runState (rec top) (manyNames, [])
 where
  gs = (`SrcSpanInfo`[]) . fromJust . getSpan
  rec :: ExpS -> WST ExpS
  rec l@(A.Lambda _ ps e) = do
    (ns, acc) <- mutate (second $ const []) 
    v <- rec e
    (_, bs) <- ST.get
    addDecl (gs l) v ps bs acc
  rec e@(A.App _ _ _) = addDecl' (gs e) 
                      =<< (liftM buildApp . mapM rec $ decompApp e)
  rec v@(A.Var _ _) = return v
  rec e = addDecl' (gs e) =<< grec e

  grec :: forall a . Data a => a -> WST a
  grec = gmapM (grec `extM` rec)

  addDecl' srcsp e = do
    (_, acc) <- ST.get
    addDecl srcsp e [] [] acc
  addDecl :: SrcSpanInfo -> ExpS -> [PatS] -> [DeclS] -> [DeclS] -> WST ExpS
  addDecl srcsp v ps bs acc = do
    (n:ns, _) <- ST.get
    ST.put . (ns,) . (:acc) $
      (A.FunBind srcsp [ A.Match sp (A.Ident sp n) ps (A.UnGuardedRhs sp
                         $ if null bs 
                           then v 
                           else A.Let sp (A.BDecls sp bs) v) Nothing ])
    return $ mkPlain n

unPlain :: A.Exp t -> String
unPlain (A.Var _ (A.UnQual _ (A.Ident _ n))) = n

mkPlain :: String -> ExpS
mkPlain n = (A.Var sp (A.UnQual sp (A.Ident sp n)))

funName :: DeclS -> String
funName (A.FunBind _ ((A.Match _ (A.Ident _ n) _ _ _):_)) = n


funExpr :: DeclS :-> ExpS
funExpr = lens getExpr setExpr
 where
  getExpr (A.FunBind _ ((A.Match _ _ _ (A.UnGuardedRhs _ e) _):_)) = e
  setExpr e (A.FunBind a ((A.Match b c d (A.UnGuardedRhs z _) f):g)) =
            (A.FunBind a ((A.Match b c d (A.UnGuardedRhs z e) f):g))

type DeclMap = M.Map String DeclS

declMap :: [DeclS] -> DeclMap
declMap = M.fromList . map (funName &&& id)

declChildVars :: DeclS -> [String]
declChildVars = filter (isPrefixOf "__") . freeEVars . get funExpr

declChildren :: DeclS -> DeclMap -> [DeclS]
declChildren d m = catMaybes . map (`M.lookup` m) $ declChildVars d

-- TODO: More efficient impl?
declParents :: DeclS -> DeclMap -> [DeclS]
declParents d = filter (elem (funName d). declChildVars) . M.elems

substExpr :: forall a. Data a
          => [(ExpS, ExpS)] -> a -> a
substExpr subs = gmapT (substExpr subs) `extT` doExp
 where
  getMatches e = map snd $ filter ((e A.=~=) . fst) subs
  doExp (getMatches -> (x:_)) = x
  doExp x = gmapT (substExpr subs) x

-- Substitute the given

-- Recursively substitute the given expression subtrees with undefined
undefDecls :: [String] -> [(String, DeclS)] -> [(String, DeclS)]
undefDecls [] ys = ys
undefDecls xs ys = undefDecls (concatMap (declChildVars . snd) removed)
                 . substExpr (map ((, mkPlain "undefined") . mkPlain) xs)
                 -- . trace (show $ (map (funName . snd) derefed, map (funName .  snd) remaining))
                 $ remaining
 where
  (removed, remaining) = partition ((`elem` xs) . fst) ys
  

funcAsExp :: DeclS -> ExpS
funcAsExp d = A.Let sp (A.BDecls sp [d]) (mkPlain $ funName d)

{-
data DeclTree = DeclTree
  { dtVar :: A.Exp A.SrcSpanInfo
  , dtDecl :: DeclS
  , dtSpan :: SrcSpan
  , dtChildren :: [DeclTree]
  }

treeWhered :: (ExpS, [(DeclS, SrcSpan)]) -> Maybe DeclTree
treeWhered (v, xs) = mkTree (unPlain v)
 where
  m = declMap xs
  mkTree n = do
    (decl, span) <- M.lookup n m
    return . DeclTree (mkPlain n) decl span
           $ catMaybes . map mkTree . filter (startsWith "__") $ freeEVars decl)

wheredTree :: DeclTree -> (ExpS, [(DeclS, SrcSpan)]
wheredTree (DeclTree v decl span xs) =
  (v, (decl, span) : concatMap (snd .  wheredTree) xs)
-}


--getWDeps  =

{- 
data DeclTree = DeclTree 
  { dtVar :: A.Exp A.SrcSpanInfo
  , dtDecl :: Maybe DeclS
  , dtChildren :: [DeclTree]
  }

whereify :: ExpS -> DeclTree
whereify top = ST.evalState (rec top) manyNames
 where
  rec :: ExpS -> ST.State [String] DeclTree
  rec (A.Lambda _ ps e) = do
    dt <- rec e
    decl ps (catMaybes . map dtDecl $ dtChildren dt) [] $ dtVar dt
  rec e@(A.App _ _ _) = do
    dts <- mapM rec $ decompApp e
    decl [] [] [] $ buildApp $ map dtVar dts
  rec v@(A.Var _ _) = return (DeclTree v Nothing [])
  rec e = decl [] [] [] =<< grec e

  grec :: forall a . Data a => a -> ST.State [String] a
  grec = gmapM (grec `extM` rec)

  decl :: [PatS] -> [DeclS] -> [DeclTree] -> ExpS -> ST.State [String] DeclTree
  decl ps bs ds v = do
    (n:ns) <- ST.get
    ST.put ns
    return $ DeclTree (mkPlain n)),
      Just $ A.FunBind sp [A.Match sp (A.Ident sp n) ps $ A.UnGuardedRhs sp 
        (case bs of
          [] ->
          _ -> A.Let sp (A.BDecls sp bs) v)]) ds
-}

--findSubset :: 

{- TODO: the denester will have to deal with all kinds of scopinmg issues.
  The main problem is extracting functions which reference variables which get
  shadowed before the invocation.

deNest :: (DeclS -> Bool) -> DeclS -> DeclS
deNest f = 
-}

{-

Ok, here's the procedure:

 1) Set the whole top-level declaration to 'undefined', in order to get the
 type, in the case that an explicit type is not specified.

 2) Whereify, and speculatively set subtrees to undefined.

 3) Duplicate definitions which are referenced in multiple locations, and create
 artificial unions:
   " __a = x; __b = x; __c = if undefined then __a else __b"

 4) Use iterative application of the seminal top-down-removal scheme

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

getVarA :: NameS -> [String]
getVarA (A.Ident _ n) = [n]
getVarA (A.Symbol _ n) = [n]

getVars :: Data a => a -> [String]
getVars = listAll (const [] `extQ` getVarA `extQ` getVar)

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

type ExpS = A.Exp A.SrcSpanInfo
type DeclS = A.Decl A.SrcSpanInfo
type NameS = A.Name A.SrcSpanInfo 
type PatS = A.Pat A.SrcSpanInfo

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

lexify :: String -> ParseResult [Loc Token]
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
