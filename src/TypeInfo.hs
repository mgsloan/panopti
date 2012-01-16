{-# LANGUAGE ParallelListComp, ViewPatterns #-}

module TypeInfo where

import ErrorParser
import PartialParse
import Simple
import State
import Utils

import Control.Arrow ((***), (&&&), first, second)
import Control.Monad (liftM, join)
import qualified Control.Monad.State as ST
import Data.Data hiding (typeOf)
import Data.Foldable (concat)
import Data.Generics.Aliases
import Data.Label
import Data.List (partition, isPrefixOf)
import qualified Data.Map as M
import Data.Maybe
import Graphics.ToyFramework
import Language.Haskell.Exts.Annotated
import qualified Language.Haskell.Interpreter as I
import Prelude hiding (concat)
import System.FilePath

{- We need to be able to achieve the following tasks with the static
representation of the code:

  * Create a pretty-printed representation of the parseable portions.

  This is achieved by including the expression subtrees in the results. One
issue is that resolved types have a lot of redundancy.  We want to store this
redundancy in order to enable simpler projections of the type information in
the result-drawing stage, as we will want to restrict / summarize the
information in a user customizable fashion.

  * Have some way of inter-relating the types within the drawing.

  This is done in a (hopefully) reasonably user reconfigureable /
understandeable / extensible way by labelling the types with tags that will be
preserved as labels in the diagram drawing stage.  These labels will initially
be stored in a fashion that gives the application structure of the expression.

 * Make it possible to flexibly extend this system, based on a relatively
compact, understandeable kernel (XMonad style)

  The user will then be able to overload two functions, which determine how to
draw each component of the expression, as well as each component of the type-
signatures.  This will allow for layering extensions and configurations to
display code and types in the way that they wish.  An interesting possibility
for an extension in this system would be to encode pretty synonyms / more
mnemonic representations in an extra module in cabal packages.

  * Show the modifications performed on the code to make it compile
(immediately below the code).

  * Eventually: Store a cached version of the rendering.  This way, the fresh
results can be diffed with the existing visualization.  Better yet, this
makes it even more likely for smoothly animated transitions to happen.

  * Long-way out feature: Check when editing a subset changes its calculated
type, and only then recalculate subsets.
-}

update s 
  = liftM (\r -> modify results (`ST.mplus` join r) s)
          (maybeM (partialParse parseExp $ get code s) processParsed)
 where
  processParsed (expr, txt', errs) = do
    subs <- getSubsets (whereify expr) (get chan s)
  -- TODO: make this a heuristic choice / only compute the preferred subset
    let pickedSubset = head subs

  -- Unify subsets, ensuring that disjoint subsets have different type names
    unified <- scanM (\(_,_,_,ns) -> unifyTypes (get chan s) ns)
                     (undefined, [], [], manyNames) pickedSubset

  -- Associate regions of the source code with types.
    return . Just $ Results (get code s) txt' expr errs
           $ map (\(_,a,b,_) -> (a, M.fromList $ map (first getSpan') b)) unified

-- A sequence of chunks make up a code diagram.  Each code chunk enumerates
-- every expression tree + type known at that juncture
data Chunk = CodeChunk [(ExpS, TypeS, Maybe ChunkLabel)]
           | OmitChunk String

type ChunkLabel = (Int, Int)

data TypeDiagramSpec = TypeDiagramSpec
  { _chunks :: M.Map ChunkLabel Chunk
  , _chunkSequence :: [ChunkLabel]
  , _resolutions :: [TypeResolution]
  }

processParsed :: (ExpS, String, [ParseResolution]) -> TaskChan -> IO TypeDiagramSpec
processParsed (expr, txt', res) = do
    subs <- getSubsets (whereify expr) (get chan s)
  -- TODO: make this a heuristic choice / only compute the preferred subset
    let pickedSubset = head subs

  -- Unify subsets, ensuring that disjoint subsets have different type names
    unified <- scanM (\(_,_,_,ns) -> unifyTypes (get chan s) ns)
                     (undefined, [], [], manyNames) pickedSubset

  -- Associate regions of the source code with types.
    return . Just $ Results (get code s) txt' expr errs
           $ map (\(_,a,b,_) -> (a, M.fromList $ map (first getSpan') b)) unified

-- Map of declarations to names
type DeclMap = M.Map String DeclS

-- A list of declarations, with their accompanying type
type TypedDecls = [(DeclS, TypeS)]

-- Type used for internal state in whereification
type WST = ST.State ([String], [DeclS])

-- | "Whereify" the expression, giving names to all of the subtrees we're
-- intererested in.  The locations of these subtrees are replaced with a
-- reference to it.  Lambdas and case expressions turn into pattern matching,
-- with the subtrees of their contents appended in a where clause.  Giving
-- names to all of the parts of the expression allows us to build a tuple, as
-- the head expression, which references all of them, allowing us to get types
-- for each component in just one query to GHC.
whereify :: ExpS -> (ExpS, DeclMap)
whereify top = second (declMap . snd) $ ST.runState (rec top) (manyNames, [])
 where
  gs = (`SrcSpanInfo`[]) . fromJust . getSpan
  rec :: ExpS -> WST ExpS
  -- Lambdas create a declaration with an appended where clause
  rec l@(Lambda _ ps e) = do
    (ns, acc) <- mutate (second $ const []) 
    v <- rec e
    (_, bs) <- ST.get
    addDecl (gs l) v ps bs acc
  -- Applying a function to  
  rec e@(App _ _ _) = addDecl' (gs e) 
                      =<< (liftM buildEApp . mapM rec $ splitEApp e)
  -- Variables get their  own declaration
  rec v@(Var _ _) = addDecl' (gs v) v
  rec v@(Paren _ e) = rec e
  rec e = addDecl' (gs e) =<< grec e

  -- Generic recursion over 
  grec :: forall a . Data a => a -> WST a
  grec = gmapM (grec `extM` rec)

  -- Adds a whereless / patternless declaration
  addDecl' srcsp e = do
    (_, acc) <- ST.get
    addDecl srcsp e [] [] acc
   
  addDecl :: SrcSpanInfo -> ExpS -> [PatS] -> [DeclS] -> [DeclS] -> WST ExpS
  addDecl srcsp v ps bs acc = do
    (n:ns, _) <- ST.get
    ST.put . (ns,) . (:acc) $
      (FunBind srcsp [ Match sp (Ident sp n) ps (UnGuardedRhs sp
                         $ if null bs 
                           then v 
                           else Let sp (BDecls sp bs) v) Nothing ])
    return $ mkPlain n

declMap :: [DeclS] -> DeclMap
declMap = M.fromList . map (get funName &&& id)

declChildVars :: DeclS -> [String]
declChildVars = filter (isPrefixOf "__") . freeEVars . get funExpr

declChildren :: DeclS -> DeclMap -> [DeclS]
declChildren d m = catMaybes . map (`M.lookup` m) $ declChildVars d

-- TODO: More efficient impl?
declParents :: DeclS -> DeclMap -> [DeclS]
declParents d = filter (elem (get funName d). declChildVars) . M.elems

-- Substitute each found instance of the (first, ) with the ( , second)
substExpr :: forall a. Data a
          => [(ExpS, ExpS)] -> a -> a
substExpr subs = gmapT (substExpr subs) `extT` doExp
 where
  getMatches e = map snd $ filter ((e =~=) . fst) subs
  doExp (getMatches -> (x:_)) = x
  doExp x = gmapT (substExpr subs) x

-- Recursively substitute the given expression subtrees with undefined
undefDecls :: [String] -> [(String, DeclS)] -> [(String, DeclS)]
undefDecls [] ys = ys
undefDecls xs ys = undefDecls (concatMap (declChildVars . snd) removed)
                 . substExpr (map ((, mkPlain "undefined") . mkPlain) xs)
                 $ remaining
 where
  (removed, remaining) = partition ((`elem` xs) . fst) ys
 
-- | Gets all possible breakdowns into validly-typeable subsets.
--TODO: use unsafeInterleaveIO to export this same interface, but lazily
--compute the ambiguous possibilities
getSubsets 
  :: (String, DeclMap) -- ^ Variable referencing the top of the decl-tree.
  -> TaskChan          -- ^ Task chan for driving Haskell interpreter.
  -> IO [[TypedDecls]] -- ^ List of potential partitionings.
getSubsets p chan = recurse p
 where
  -- Finds a typeable subset rooted at the given name.
  recurse :: (String, DeclMap) -> IO [[TypedDecls]]
  recurse arg@(r, dm) | M.null dm = return []
                      | otherwise = do
    --putStrLn $ unlines . map prettyPrint $ M.elems dm
    results <- getTopSubset arg chan
    if null results
      then return []
      else mapM (\xs -> liftM (xs:) 
                      . recurse'
                      . foldr M.delete dm 
                      $ map (get funName . fst) xs
                ) $ watchTypeds r results

  -- This implementation is a little bit backwards.  These mutually recursive
  -- functions figure out information that's apparent in the recursion of 
  -- getTopSubset - which tree was removed.

  recurse' :: DeclMap -> IO [TypedDecls]
  recurse' dm
    = liftM (concat . concat)
    . mapM (recurse . (,dm) . mkPlain . fst)
    . filter ((== 0) . length . (`declChildren` dm) . snd)
    $ M.toList dm

-- Type slicing by driving the interpreter (Seminal approach)
getTopSubset :: (ExpS, DeclMap) -> TaskChan -> IO [TypedDecls]
getTopSubset (top, dm) chan = do 
   types <- getTypes $ M.elems dm
   case types of
     Left err -> watch' ("top " ++ err) err
               . maybe (return []) derefChildren
               $ M.lookup top dm
     Right xs -> return [xs]
 where
  -- Removes declarations that the passed declaration references, and
  -- recursively removes their children.  In other words, this deletes an
  -- entire expression subtree in the twhered expression.
  derefChildren :: DeclS -> IO [TypedDecls]
  derefChildren = liftM concat
                . mapM (derefNode . (id &&& (`M.lookup` dm)))
                . declChildVars

  -- TODO: determine whether returning the empty list makes sense
  derefNode :: (String, Maybe DeclS) -> IO [TypedDecls]
  derefNode (_, Nothing) = return []
  derefNode (n, Just d) = do
    ts <- getTypes . map snd . undefDecls [n] $ M.toList dm
    case ts of
      Right xs -> do
        results <- derefChildren d
        return (if null results then [xs] else results)
      Left err -> watch' "Cause" err $ return []

  -- Attempts to get the types of a set of declarations
  getTypes :: [DeclS] -> IO (Either String TypedDecls)
  getTypes decls = do
    let ppr = prettyPrint $ twhered decls
    typ <- getType chan ppr
    return $ case typ of
      Left err -> Left (show err)
      Right typeStr -> Right $
        case watchWith (concatMap (concatMap prettyPrint) . maybeToList) "tupT"
           $ processType $ parseType typeStr of
          Just xs -> zip decls xs
          _ -> []

  -- Apply the context to each subcomponent, enumerate contents of tuple type
  processType :: ParseResult TypeS -> Maybe [TypeS]
  processType (ParseOk (TyTuple _ _ xs)) = Just xs
  processType (ParseOk (TyForall _ bnds ctx t)) 
    = liftM (map (setCtx (concat bnds) (get contextList ctx)))
    $ processType (ParseOk t)
  processType (ParseOk (TyForall _ bnds _ t))
    = processType (ParseOk t)
  
  processType (ParseOk t) = Just [t]
  processType _ = Nothing

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

instance Show TypeResolution where
  show (TypeRes t b) = "type " ++ prettyPrint (deQual t) ++ " = " ++ prettyPrint b
  show (DataRes t)   = "data " ++ prettyPrint (deQual t)
  show (InstRes t)   = "$(mkDummyInstance \"" ++ prettyPrint (deQual t) ++ "\")"

unifyTypes :: TaskChan -> [String] -> TypedDecls
           -> IO (Either I.InterpreterError String, [TypeResolution], TypedDecls, [String])
unifyTypes chan names tdecls  = do
  (typ, rs, ds) <- rec (map DataRes typesUsed, tdecls)
  return (typ, rs, rewrite tm' ds, namesRemaining)
 where
  (vars, comps) = partition (isVarOrLit . get funExpr . fst) tdecls

  isVarOrLit :: ExpS -> Bool
  isVarOrLit (Var _ _) = True
  isVarOrLit (IPVar _ _) = True
  isVarOrLit (Lit _ _) = True
  isVarOrLit _ = False

  -- All of the free polymorphic variables in the declarations
  freeVars = onubSortBy id $ concatMap (freeTVars . snd) tdecls

  -- Get the names we'll need for dummy datatypes.
  (namesUsed, namesRemaining) = splitAt (length freeVars) names

  -- Create types out of the names.
  typesUsed = [ TyCon sp . Qual sp (ModuleName sp "L") . Ident sp $ "T" ++ n 
              | n <- namesUsed ]

  -- Forward and reverse mappings of the types.
  tm  = M.fromList [ (t, n) 
                   | t <- freeVars | n <- typesUsed]
  tm' = M.fromList [ (prettyPrint n, TyVar sp $ Ident sp t)
                   | t <- freeVars | n <- typesUsed]

  rewrite m = map (second $ specializeTypes m)

  -- Declarations with variables and literals are replaced with "undefined"
  -- with explicit types.
  rewritten = comps ++ map mkSig (rewrite tm vars)
   where
    mkSig (d, t) = (set funExpr (ExpTypeSig sp (mkPlain "undefined") 
                 . mustOk . parse $ prettyPrint t) d, t)

  -- Get the code for a function definition, from the rewritten declarations.
  rec (rs, ds)  = do
    let fname = "foo"
        txt = (prettyPrint . mkFun fname . twhered $ map fst ds)
            ++ "\n" ++ (unlines . map show $ onubSortBy id rs)
    typ <- interpretWith chan txt $ typeOf fname
    case typ of
      Left (I.WontCompile xs) -> handleErrors typ xs
      _ -> return (typ, rs, ds)
   where
    -- Pick a type to use as the cannonical name for a synonym group.
    pickCannonical rs xs = case cons of
       -- Prefer a constructor if there is one.
       [h] -> (h, syns)
       [] -> fromJust $ extractFirst ((`elem`rs) . DataRes) syns
      where 
       (syns, cons) = partition isConL
                    . onubSortBy id
                    $ concatMap (\(a, b) -> [a, b]) xs

    isConL (TyCon _ (Qual _ (ModuleName _ "L") _)) = True
    isConL _ = False

    -- Create a resolution for each kind of error, and rewrite based on type
    -- equality constraint. 
    handleErrors typ xs = do
      let (ts, rs') = partition isTypeRes $ concatMap resolve xs
          -- Build a map from each of these to the cannonical type
          synMap = M.fromList
          -- Write the map for rewriting types to the cannonical
                 . concatMap (\(x, xs) -> map ((,x) . prettyPrint) xs)
          -- Pick a cannonical type to rewrite the others to
                 . map (pickCannonical rs)
          -- Find all related type-synonyms
                 $ unionEqual [ (a, b) | TypeRes a b <- ts ]
      if null rs'
        then return (typ, rs, ds)
        else rec (rs' ++ rs, rewrite synMap ds)

    unionEqual = transitivePartition 
      (\(a, b) (a', b') -> b == b' || a == b' || b == a')

    resolve (I.GhcError (_, _, c, _)) = case parseGHCError c of
      TypeError (EqualityError a b _ _) _ ->
        let a' = mustOk $ parseType a
            b' = mustOk $ parseType b in [TypeRes a' b']
      TypeError (InstanceError a _) _ -> [InstRes (mustOk $ parseType a)]
      _ -> []

  deQualL (TyCon s (Qual s' (ModuleName _ "L") x)) = Right (TyCon s $ UnQual s' x)
  -- Temporary hack
  deQualL (TyCon s (Qual s' _ x)) = Left (TyCon s $ UnQual s' x)
  deQualL t = Left t


-- Debugging Utilities
watchTypeds :: String -> [TypedDecls] -> [TypedDecls]
watchTypeds n = zipWith (\i -> watchTyped (n ++ show i)) [0..]

watchTyped :: String -> TypedDecls -> TypedDecls
watchTyped n xs = watch' n 
  (unlines $ map (\(a, b) -> padLeft padding a ++ " :: " ++ b) ys)
  xs
 where
  ys = map (\(a, b) -> (prettyPrint a, prettyPrint b)) xs
  padding = maximum $ map (length . fst) ys

getType :: TaskChan -> String -> IO (IError String)
getType c t = interpret c "MyMain" $ Simple.typeOf t

-- TODO: configureable static imports
interpretWith :: TaskChan -> String
              -> I.Interpreter a -> IO (IError a)
interpretWith c s f = do
  writeFile (sourceDir </> "L" <.> "hs")
    $ "{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}\n"
   ++ "module L where\nimport MyMain\n" ++ s ++ "\n"

  interpret c "L" f
