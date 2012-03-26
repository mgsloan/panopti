{-# LANGUAGE ParallelListComp, ViewPatterns #-}

module TypeInfo where

import ActiveHs.Simple
import ErrorParser
import PartialParse
import State
import Utils

import Prelude hiding (concat)

import Control.Applicative((<$>))
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
import Debug.Trace
import Language.Haskell.Exts.Annotated
import qualified Language.Haskell.Interpreter as I
import System.FilePath


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
  recurse' dm = liftM (concat . concat) $ sequence
              [ recurse (n, dm)
              | (n, d) <- M.toList dm
              , length (declChildren d dm) == 0
              ]

-- Type slicing by driving the interpreter (Seminal approach)
getTopSubset :: (String, DeclMap) -> TaskChan -> IO [TypedDecls]
getTopSubset (top, dm) chan = do 
  let decls = M.elems dm
  types <- getTypes decls
  case types of
    Left err -> watch' "top " err
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

  -- Attempts to get the types of a set of potentially inter-related declarations
  getTypes :: [DeclS] -> IO (Either String TypedDecls)
  getTypes decls = do
    let ppr = prettyPrint $ twhered decls
    typ <- getType chan (watch' "toi " ppr ppr)
    return $ case typ of
      Left err -> Left (show err)
      Right typeStr -> Right $
        case --watchWith (concatMap (concatMap prettyPrint) . maybeToList) "tupT" . 
             processType $ parseIt typeStr of
          Just xs -> zip decls xs
          _ -> []

  -- Apply the context to each subcomponent, enumerate contents of tuple type.
  processType :: ParseResult TypeS -> Maybe [TypeS]
  processType (ParseOk (TyTuple _ _ xs)) = Just xs
  processType (ParseOk (TyForall _ bnds ctx t)) 
    = liftM (map (setCtx (concat bnds) (get contextList ctx)))
    $ processType (ParseOk t)
  processType (ParseOk (TyForall _ bnds _ t))
    = processType (ParseOk t)
  
  processType (ParseOk t) = Just [t]
  processType _ = Nothing