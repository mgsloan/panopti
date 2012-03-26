{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

-- Thanks Luite!!! from http://hpaste.org/65785

{-
   1. install first: ghc-paths
   2. make a file Test1.hs in this dir with what you want to parse/transform
   3 .run with: ghci -package ghc
-}

module Panopti.GHC where

import Data.Generics.Schemes
import Data.Generics.Aliases

import qualified GHC
import qualified DynFlags              as GHC
import qualified Outputable            as GHC
import qualified MonadUtils            as GHC
import qualified NameSet               as GHC
import qualified HsLit                 as GHC

import PprTyThing
import DynFlags
import GHC
import Outputable
import SrcLoc
import qualified OccName(occNameString)
import Bag(Bag,bagToList)
import Var(Var)
import FastString(FastString)
import NameSet(NameSet,nameSetToList)
import Data.List (intersperse)

import GHC.Paths ( libdir )
import Data.Data


targetMod = "MyMain"
 
main = example

example =
   GHC.runGhc (Just libdir) $ do
        dflags <- GHC.getSessionDynFlags
        let dflags' = foldl GHC.xopt_set dflags
                            [GHC.Opt_Cpp, GHC.Opt_ImplicitPrelude, GHC.Opt_MagicHash]
        GHC.setSessionDynFlags dflags'
        target <- GHC.guessTarget (targetMod ++ ".hs") Nothing
        GHC.setTargets [target]
        GHC.load GHC.LoadAllTargets
        modSum <- GHC.getModSummary $ GHC.mkModuleName targetMod
        p <- GHC.parseModule modSum
        t <- GHC.typecheckModule p
        let p' = processParsedMod shortenLists p
            t' = tm_typechecked_source t
        GHC.liftIO (putStrLn . showParsedModule $ p)
        GHC.liftIO (putStrLn . showParsedModule $ p')
        GHC.liftIO (putStrLn $ showData TypeChecker 0 t')

showParsedModule p = showData Parser  0 (GHC.pm_parsed_source  p)

processParsedMod f pm = pm { GHC.pm_parsed_source = ps' }
  where
   ps  = GHC.pm_parsed_source pm
   ps' :: GHC.ParsedSource
--   ps' = SYB.everythingStaged SYB.Parser (SYB.mkT f') -- does not work yet
   ps' = everywhereStaged Parser (mkT f) ps

newtype ID x = ID { unID :: x }

everywhereStaged :: Stage -> (forall a. Data a => a -> a) -> (forall a. Data a => a -> a)
everywhereStaged s f = f . gmapT' (everywhereStaged s f)
  where
    nameSet    = const (s `elem` [Parser, TypeChecker]) :: GHC.NameSet    -> Bool
    postTcType = const (s < TypeChecker)                :: GHC.PostTcType -> Bool
    fixity     = const (s < Renamer)                    :: GHC.Fixity     -> Bool
    gmapT' :: (Data a) => (forall b. Data b => b -> b) -> a -> a
    gmapT' f x0
      | (const False `extQ` postTcType `extQ` fixity `extQ` nameSet) x0   = x0
      | otherwise  = unID (gfoldl k ID x0)
                       where
                          k :: Data d => ID (d -> b) -> d -> ID b
                          k (ID c) x = ID (c (f x))


shortenLists :: GHC.HsExpr GHC.RdrName -> GHC.HsExpr GHC.RdrName
shortenLists (GHC.ExplicitList t exprs) = GHC.ExplicitList t []
shortenLists x                          = x

-------- from ghc-syb-utils below, fixed to avoid extra traps

data Stage = Parser | Renamer | TypeChecker deriving (Eq,Ord,Show)

type BLB a = Bag (Located (HsBind a))

showData :: Data a => Stage -> Int -> a -> String
showData stage n =
  generic `ext1Q` list `extQ` string `extQ` fastString `extQ` srcSpan 
          `extQ` name `extQ` occName `extQ` moduleName `extQ` var `extQ` dataCon
          `extQ` bagName `extQ` bagRdrName `extQ` bagVar `extQ` nameSet
          `extQ` postTcType `extQ` fixity
          `extQ` syntaxExprExpr `extQ` syntaxExprStmt `extQ` syntaxExprOlit
  where generic :: Data a => a -> String
        generic t = indent n ++ "(" ++ showConstr (toConstr t)
                 ++ space (concat (intersperse " " (gmapQ (showData stage (n+1)) t))) ++ ")"
        space "" = ""
        space s  = ' ':s
        indent n = "\n" ++ replicate n ' '
        string     = show :: String -> String
        list l     = indent n ++ "["
                              ++ concat (intersperse "," (map (showData stage (n+1)) l)) ++ "]"

        br n = (("{" ++ n) ++) . (++ "}")
        brshow n = br n . show
        brppr  n = br n . showSDoc . ppr 
        brblb  n = br n . list . bagToList

        occName    = br    "OccName: " . OccName.occNameString
        fastString = brshow "FastString: " :: FastString  -> String

        name       = brppr "Name: "        :: Name        -> String
        moduleName = brppr "ModuleName: "  :: ModuleName  -> String
        srcSpan    = brppr ""              :: SrcSpan     -> String
        var        = brppr "Var: "         :: Var         -> String
        dataCon    = brppr "DataCon: "     :: DataCon     -> String

        bagRdrName = brblb "BLB RdrName: " :: BLB RdrName -> String
        bagName    = brblb "BLB Name: "    :: BLB Name    -> String
        bagVar     = brblb "BLB Name: "    :: BLB Var     -> String

        nameSet | stage `elem` [Parser,TypeChecker] 
                = const ("{!NameSet placeholder here!}") :: NameSet -> String
                | otherwise
                = br "NameSet: " . list . nameSetToList

        postTcType | stage<TypeChecker = const "{!type placeholder here?!}" :: PostTcType -> String
                   | otherwise     = showSDoc . ppr :: Type -> String

        fixity | stage<Renamer = const "{!fixity placeholder here?!}" :: GHC.Fixity -> String
               | otherwise     = brppr "Fixity: " :: GHC.Fixity -> String

        syntaxExprExpr :: HsExpr RdrName -> String
        syntaxExprExpr (NegApp e1 e2)           | stage < Renamer = "todo: NegApp"
        syntaxExprExpr (HsIf me1 e2 e3 e4)      | stage < Renamer = "todo: HsIf"
        syntaxExprExpr x = generic x

        syntaxExprStmt :: StmtLR RdrName RdrName -> String
        syntaxExprStmt (LastStmt e1 e2)         | stage < Renamer = "todo: LastStmt"
        syntaxExprStmt (BindStmt pat e1 e2 e3)  | stage < Renamer = "todo: BindStmt"
        syntaxExprStmt (ExprStmt e1 e2 e3 ptt)  | stage < Renamer = "todo: ExprStmt"
        syntaxExprStmt (ParStmt stmts e1 e2 e3) | stage < Renamer = "todo: ParStmt"
        syntaxExprStmt (TransStmt {})           | stage < Renamer = "todo: TransStmt"
        syntaxExprStmt (RecStmt {})             | stage < Renamer = "todo: RecStmt"
        syntaxExprStmt x = generic x

        syntaxExprOlit :: HsOverLit RdrName -> String
        syntaxExprOlit (OverLit v r w t)        | stage < Renamer = "todo: OverLit"
        syntaxExprOlit x = generic x

everythingStaged :: Stage -> (r -> r -> r) -> r -> GenericQ r -> GenericQ r
everythingStaged stage k z f x 
  | (const False `extQ` postTcType `extQ` fixity `extQ` nameSet) x = z
  | otherwise = foldl k (f x) (gmapQ (everythingStaged stage k z f) x)
  where nameSet    = const (stage `elem` [Parser,TypeChecker]) :: NameSet -> Bool
        postTcType = const (stage<TypeChecker)                 :: PostTcType -> Bool
        fixity     = const (stage<Renamer)                     :: GHC.Fixity -> Bool
