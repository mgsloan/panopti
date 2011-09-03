{-# LANGUAGE FlexibleInstances #-}
module Specialize 
    ( specialize
    ) where

import Data.List
import Data.Function

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Extension

-----------------------------

specialize :: String -> Either String (String, String)
specialize a
    = case parseTypeWithMode (defaultParseMode {extensions = [FlexibleContexts]}) a of
        ParseFailed loc s -> Left $ show s
        ParseOk t -> let

                (t',t'') = convert (split t)

            in Right (prettyPrint t', prettyPrint t'')

split :: Type -> ([(String, [String])], Type)
split (TyForall Nothing l t) 
    = ( map (\x -> (fst (head x), map snd x)) $ groupBy ((==) `on` fst) $ sort
          [(v,s) | ClassA (UnQual (Ident s)) [TyVar (Ident v)]<-l]
      , t
      )
split t 
    = ([], t)

convert :: ([(String, [String])], Type) -> (Type, Type)
convert (m, t) = (app True mm t, app False mm t)  where mm = map resolve m

app :: Bool -> [(String, [[Char]])] -> Type -> Type
app b m t = f t where
    f (TyFun a b) = TyFun (f a) (f b)
    f (TyTuple bo l) = TyTuple bo $ map f l
    f (TyList t) = TyList (f t)
    f (TyParen t) = TyParen (f t)
    f (TyApp x t) = TyApp (f x) (f t)
    f (TyVar (Ident s)) = mkV $ head $ [y | (v,x)<-m, v==s, y<-ff  x] ++ ff allT
    f t = t

    ff = if b then id else reverse

mkV :: String -> Type
mkV v = TyVar $ Ident v

resolve :: (String, [String]) -> (String, [String])
resolve (v, l) = (v, foldl1 intersect $ map res l)

allT :: [String]
allT = ["Double","Integer","Char","Bool","()"]

res :: String -> [String]
res x | x `elem` ["RealFrac", "Real", "RealFloat", "Floating", "Fractional"] = ["Double"]
res x | x `elem` ["Num"] = ["Double","Integer"]
res x | x `elem` ["Integral"] = ["Integer"]
res x | x `elem` ["Monad"] = ["[]","Maybe"]
res x | x `elem` ["Ord","Show","Eq","Enum"] = allT
res x = []  -- !!!


