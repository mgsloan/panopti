{-# LANGUAGE FlexibleContexts, TypeFamilies, ViewPatterns #-}

module Diagrams where

import State
import TypeDiagrams

import Control.Arrow ((***), (&&&), first, second)
import Data.Curve.Util (mapT, zipT)
import Data.Data hiding (typeOf)
import Data.Default (def)
import Data.Generics.Aliases
import Data.IORef
import Data.Label
import Data.List (intersperse, sort)
import Data.Maybe (listToMaybe)
import Data.Supply
import Diagrams.Prelude hiding ((===), (|||))
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.TwoD.Text(Text(..))
import Utils
import Language.Haskell.Exts.Annotated
import qualified Data.Map as M

import System.IO.Unsafe

ivlsGaps :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
ivlsGaps (l, u) xs = map (    snd    ***    fst)
                   $ zip ((0, l) : xs) ((u, 0) : tail xs)

type DrawResult = (CDiagram, [(Int, Int)])

-- stateDiagram :: State -> IO CDiagram 
stateDiagram :: State -> IO CDiagram
stateDiagram = return . drawResults . get results
 where
--  drawResults :: Maybe Results -> ISupply -> CDiagram
  drawResults Nothing
    = mempty
  drawResults (Just (Results txt txt' expr errs subs))
    = drawExpr expr
   where
    --TODO: IntervalMap
    mergedIvls = csort fst . M.toList . foldl1 M.union $ map snd subs
--    mergedIxs = zipWith (\i -> map (,i) . M.keys) [0..] $ snd subs

    drawType = typeDiagram "yo" shapes (const $ const Nothing)

    shapes = coloredShapes . map prettyPrint . uniquePrims $ map snd mergedIvls

    getContainers ivl = filter ((`ivlContains` (debug ivl)) . fst) mergedIvls

    drawContainer f ivl
      = maybe mempty (drawType . snd) . listToMaybe . f $ getContainers ivl

    -- Draw full type for a given interval 
    drawFType = drawContainer id

    -- Naive way of drawing result types
    drawRType ivl = drawContainer (filter ((fst ivl ==) . fst . fst) . reverse) ivl

    drawCode :: ExpS -> CDiagram
    drawCode e = text' (prettyPrint e)

    drawExpr :: ExpS -> CDiagram
    drawExpr v@(Var _ _)   = drawCode v === drawFType (getSpan' v)
    drawExpr v@(IPVar _ _) = drawCode v === drawFType (getSpan' v)
    drawExpr v@(Con _ _)   = drawCode v === drawFType (getSpan' v)
    drawExpr v@(Lit _ _)   = drawCode v === drawFType (getSpan' v)

    drawExpr e = hsep $ gmapQ (const mempty `extQ` (drawExpr . convertOp) `extQ` drawExpr) e
   
    convertOp :: QOp SrcSpanInfo -> ExpS
    convertOp (QVarOp s' qn) = Var s' qn
    convertOp (QConOp s' qn) = Con s' qn

{-
    drawType :: ExpS -> CDiagram
    drawType e = drawExpr e
                  ===
                 drawFType (getSpan' e)

    merge = hsep *** (++)

    drawExpr :: ExpS -> DrawResults
    drawExpr (InfixApp s l o r)
      = (hsep [ld, drawCode o, rd], [(head lns, last rns)])
     where
      (ld, lns) = drawType l
      (rd, rns) = drawType r

    drawExpr (LeftSection s l o)
      = (hsep [text' "(", drawExpr l, drawExpr o, text' ")"])
     where
      (ld, lns) = drawType l

    drawExpr (RightSection s o r)
      = 
-}

{-
    withType :: ExpS -> Type -> CDiagram
    withType = undefined

    resultType = 

    drawExpr (InfixApp s l o r) = case o of
      (QVarOp _ (UnQual _ (Symbol _ "."))) 
        -> hsep [l, withType o $ resultType r, drawChildren r]
      (QVarOp _ (UnQual _ (Symbol _ "$"))) -> doApp o [l, r]
      _                                          -> doApp o [l, (convertOp o), r]
    drawExpr (LeftSection s l o)  = doApp o [l, convertOp o]
    drawExpr (RightSection s o r) = doApp o [convertOp o, r]
    drawExpr (App s l r)          = doApp l (l, splitEApp r)
    drawExpr e = ([eIvl], hsep $ zipWith process gaps children)
     where
      process gap (_, d)
        | fst gap == snd gap = d
        | otherwise = text' (substr gap txt) ||| d
      eIvl = getSpan' e
      gaps = ivlsGaps eIvl . sort $ map fst children
      children = gmapQ grec e
      grec :: Data d => d -> AnnDiagram Cairo R2 Any
      grec = const () `extQ` drawExpr

--    doApp :: SrcSpanInfo -> ExpS -> [ExpS] -> CDiagram
    doApp s es = ( ivls
                 , ( eDiagram
                      ===
                     drawRType (colSpan (srcInfoSpan s))
                   )
                 ) ||| 
     where
      (ivls, eDiagram) = drawExpr e
      args = hsep (map drawExpr es)
-}
