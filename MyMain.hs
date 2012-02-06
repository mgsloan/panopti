module MyMain (module Prelude, module Data.Char, module Control.Arrow, dummyInstance, mkDummyInstance, mkDummyInstances) where

import Control.Arrow
import Control.Monad
import Language.Haskell.TH
import Language.Haskell.Meta.Parse
import Debug.Trace
import Data.Char

import Prelude

default ()

debug x = trace (show x) x

-- TODO: since we do things this way, we don't support implicit parameters +
-- typeclasses
dummyInstance :: String -> Q Dec
dummyInstance xs = do
  let (Right parsed) = parseType xs
      (cxt, (cls, t)) = extractCxt parsed
  (ClassI (ClassD _ _ _ _ xs) _) <- reify cls
  return $ InstanceD cxt (AppT (ConT cls) t)
    [FunD mn [Clause [] (NormalB $ SigE (VarE $ mkName "undefined") mt) []] | (SigD mn mt) <- xs]
 where
  extractCxt (ForallT [] cxt t) = (cxt, extractClass t)
  extractCxt t = ([], extractClass t)
  extractClass (AppT (ConT cls) t) = (cls, t)

mkDummyInstance :: String -> Q [Dec]
mkDummyInstance = liftM (:[]) . dummyInstance

mkDummyInstances :: [String] -> Q [Dec]
mkDummyInstances = mapM dummyInstance

{-
whereify :: String -> Name -> Q [Dec]
whereify n' n = do
  d <- reify n
  case d of
    (FunD n xs) -> [ FunD (mkName n')
                   , FunD (mkName $ n' ++ "_names") 
 where
  :: Clause -> [String, Exp]
-}
