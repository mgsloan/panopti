{-# LANGUAGE FlexibleContexts, TypeFamilies, ViewPatterns #-}

module ExprDiagrams where

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
stateDiagram = return . alignT . expandRectBounds (1, 1) 
             . drawResults . get results

drawResults Nothing
  = mempty
drawResults (Just (Results txt txt' expr errs subs))
  = drawExpr expr
 where
  --TODO: IntervalMap
  mergedIvls = csort fst . M.toList . foldl1 M.union $ map snd subs
--    mergedIxs = zipWith (\i -> map (,i) . M.keys) [0..] $ snd subs

  drawType = fontSize 1 $ typeDiagram "yo" shapes (const $ const $ const Nothing)

  shapes = coloredShapes . map prettyPrint . uniquePrims $ map snd mergedIvls

  getContainers ivl = filter ((`ivlContains` ivl) . fst) mergedIvls

  drawFirst = maybe mempty (drawType . snd) . listToMaybe

  -- Draw full type for a given interval 
  drawFType :: (Data a) => a -> CDiagram
  drawFType = drawFirst . getContainers . getSpan'

  -- Naive way of drawing result types
  drawRType :: (Data a) => a -> CDiagram
  drawRType e = drawFirst . filter ((fst ivl ==) . fst . fst) . reverse
              $ getContainers ivl
   where ivl = getSpan' e

  codeText = stext 4

  drawCode :: Pretty a => a -> CDiagram
  drawCode e = codeText $ prettyPrint e

  enclose pre post d = hsep [pre, d, post]
  encloseP = enclose (codeText "(") (codeText ")")

  drawExpr :: ExpS -> CDiagram
  drawExpr v@(Var _ _)   = drawCode v === drawFType v
  drawExpr v@(IPVar _ _) = drawCode v === drawFType v
  drawExpr v@(Con _ _)   = drawCode v === drawFType v
  drawExpr v@(Lit _ _)   = drawCode v === drawFType v
  drawExpr v@(Paren _ e) = encloseP $ drawExpr e
--  drawExpr v@(InfixApp _ l o r) =
--    hsep [drawExpr l, drawExpr $ convertOp o, drawExpr r]
  drawExpr v@(App _ l r) = hsep [drawExpr l === lt, drawExpr r === rt]
   where
    lt = drawFType l
    rt = drawFType r
  drawExpr v@(LeftSection _ e o) =
    barBottom (encloseP $ drawExpr e ||| drawCode o)
      ===
    drawFType v
  drawExpr v@(RightSection _ o e) = 
    barBottom (encloseP $ drawCode o ||| drawExpr e)
      ===
    drawFType v
  drawExpr v@(Lambda _ bnds e) =
    hsep $ [codeText "λ"] ++ map drawCode bnds ++ [larrow, drawCode e]
  drawExpr e = hsep $ gmapQ gdraw e

  gdraw :: (Data a) => a -> CDiagram
  gdraw = const mempty `extQ` (drawExpr . convertOp) `extQ` drawExpr
 
  convertOp :: QOp SrcSpanInfo -> ExpS
  convertOp (QVarOp s' qn) = Var s' qn
  convertOp (QConOp s' qn) = Con s' qn

  larrow = lined $ arrow (3, 0) (1, 1)

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
