-- | Qualify given names in a Haskell expression
module Qualify
    ( qualify
    ) where

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Syntax

import Data.List ((\\))
import Data.Generics
import Control.Monad.Reader

-------------------------------------

type R = Reader [String]

qualify 
    :: String   -- ^ qualifier to add
    -> [String] -- ^ names to qualifiy
    -> String   -- ^ Haskell expression
    -> Either String String     -- ^ either the modified expression or an error
qualify q ns x = case parseExpWithMode defaultParseMode x of
    ParseOk y -> Right $ prettyPrint $ runReader (trExp y) ns
    e         -> Left $ show e
 where 
    trQName :: QName -> R QName
    trQName y@(UnQual x) = do
        b <- asks (printName x `elem`)
        return $ if b then (Qual (ModuleName q) x) else y
    trQName y = return y

    trExp :: Exp -> R Exp
    trExp (Lambda loc pats e) = do
        pats' <- tr pats
        e' <- local (\\ vars pats) $ trExp e
        return $ Lambda loc pats' e'
    trExp (Let b e) = do
        (b', e') <- local (\\ vars b) $ tr (b, e)
        return $ Let b' e'
    trExp x = gmapM tr x

{-
Alt:
Alt SrcLoc Pat GuardedAlts Binds
-}

    tr :: Data x => x -> R x
    tr = everywhereM (mkM trQName) `extM` trExp

    vars :: Data a => a -> [String]
    vars = map printName . everything (++) (mkQ [] patVars_)

    patVars_ :: Pat -> [Name]
    patVars_ (PVar x) = [x]
    patVars_ (PAsPat x _) = [x]
    patVars_ (PNPlusK x _) = [x]
    patVars_ _ = []

    printName (Ident x) = x
    printName (Symbol x) = x

{- !!!
PatTypeSig SrcLoc Pat Type	
PViewPat Exp Pat
-}

