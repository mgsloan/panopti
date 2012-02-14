{-# LANGUAGE FlexibleContexts, TypeFamilies, ViewPatterns #-}

module ExprDiagrams where

import Annotations
import State
import TypeDiagrams
import Utils

import Control.Applicative ((<$>))
import Control.Arrow ((***), (&&&), first, second)
import Control.Monad (msum)
import Data.Data hiding (typeOf)
import Data.Default (def)
import Data.Dynamic (fromDynamic)
import Data.Generics.Aliases
import Data.IORef
import Data.Label
import Data.List (find, findIndices)
import Data.Maybe (isJust, fromMaybe)
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Backend.Cairo.Text (StyleParam, textLineBounded)
import Diagrams.Prelude hiding ((===), (|||), fromDynamic)
import Diagrams.TwoD.Text(Text(..))
import Graphics.UI.Gtk.Toy.Prelude hiding (debug, debug', fromDynamic, ivlContains)
import Language.Haskell.Exts.Annotated
import qualified Data.Map as M

import System.IO.Unsafe

monostyle :: StyleParam
monostyle = font "monospace" . fontSize 18

monotext :: String -> CairoDiagram
monotext = textLineBounded monostyle 

coloredShapes :: [String] -> M.Map String CairoDiagram
coloredShapes names
  = M.fromList 
  . (++ [("Int", scale 0.2 $ monotext "Z")])
  . zip names
  $ zipWith (\i c -> fc c $ polygon $ def {polyType = PolyRegular i 1.0}) [4..]
    [aqua, crimson, brown, fuchsia, khaki, indigo, papayawhip, thistle]

drawCode :: MarkedText (Versioned Ann) -> CairoDiagram
drawCode mt
  = vcat
  . map (draw . substrText True mtSorted . second (+1))
  . ivlsFromSlices (textLength mt)
  . findIndices (=='\n')
  $ get mText mt
 where
  mtSorted = modify mMarks sortMarks mt

  marks = get mMarks mtSorted

  subsetIx = debug $ case find (isCursor . snd) $ get mMarks mt of
    Just (civl, _) 
     -> fromMaybe 0
      $ lastMay [s | (mivl, Version _ (SubsetA (s, _) _)) <- marks, mivl `ivlContains` civl]
    Nothing -> 0

  drawText = draw . (`MarkedText` [])

  drawType = scale 5 . typeDiagram "" shapes (const $ const $ const Nothing)

  shapes = coloredShapes . map prettyPrint $ uniquePrims allTypes

  allTypes = [x | (_, Version _ (TypeA (s, _) x)) <- marks, s == subsetIx]

  draw (MarkedText txt [])
    = monotext $ filter (not . (`elem` "\r\n")) txt

  draw (MarkedText txt (((fm, tm), m):xs))
    =   draw (substrText False mt (-1, fm))
    ||| handleMark (get versionValue m) (substrText True mt (fm, tm))
    ||| draw (substrText False mt (tm, length txt + 1))
   where
    mt = MarkedText txt xs

    handleMark CursorA -- = drawMark CursorMark txt . draw
      = \mt -> case get mText mt of
          "" -> lineWidth 1 . lineColor black
              . moveOriginBy (-1.5, 2)
              . setBounds mempty
              . stroke . pathFromTrail
              $ Trail [Linear (0, 18)] False
          _ -> draw mt
             # fc white
             # highlight black          
    handleMark (TypeA (s, _) ty) 
      | s == subsetIx = (\x -> x === drawType ty) . draw
      | otherwise = draw
    handleMark (SubsetA (s, _) tyc) 
      | s == subsetIx = (\x -> x === drawText (show tyc)) . draw
      | otherwise = draw
    handleMark (AstA d)
      = \t -> let t' = removeAstMarks t 
               in maybe (draw t') id $ msum [ drawExpr t' <$> fromDynamic d ]
    handleMark _ = draw
    -- TODO: find position of ->
    drawExpr t l@(Lambda _ pats expr)
      =   drawText "λ"
      ||| draw (substrText True t bndsIvl)
      ||| alignB (lined (arrow (20, 0) (5, 5)))
      ||| strutX 10
      ||| draw (substrText True t exprIvl)
     where
      offset = fm - fst (annSpan l)
      bndsIvl = first (subtract 1) . mapT (+offset) . foldl1 ivlUnion $ map annSpan pats
      exprIvl = first (subtract 1) . mapT (+offset) $ annSpan expr
--    drawExpr t e@()
    drawExpr t _ = draw t
  
    removeAstMarks (MarkedText txt ms)
      = MarkedText txt $ filter (not . isTopExpr . second (get versionValue)) ms
     where
      isTopExpr (i, AstA d) 
        = i == (0, length txt - 1) -- && isJust (fromDynamic d :: Maybe ExpS)
      isTopExpr _ = False


{-
ivlsGaps :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
ivlsGaps (l, u) xs = map (    snd    ***    fst)
                   $ zip ((0, l) : xs) ((u, 0) : tail xs)

type DrawResult = (CairoDiagram, [(Int, Int)])

-- stateDiagram :: State -> IO CairoDiagram 
stateDiagram :: State -> IO CairoDiagram
stateDiagram = return . alignT . expandRectBounds (1, 1) 
             . drawResults . get results

drawResults :: Maybe Results -> CairoDiagram
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
  drawFType :: (Data a) => a -> CairoDiagram
  drawFType = drawFirst . getContainers . getSpan'

  -- Naive way of drawing result types
  drawRType :: (Data a) => a -> CairoDiagram
  drawRType e = drawFirst . filter ((fst ivl ==) . fst . fst) . reverse
              $ getContainers ivl
   where ivl = getSpan' e

  codeText = stext 4

  drawCode :: Pretty a => a -> CairoDiagram
  drawCode e = codeText $ prettyPrint e

  enclose pre post d = hsep [pre, d, post]
  encloseP = enclose (codeText "(") (codeText ")")

  drawExpr :: ExpS -> CairoDiagram
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

  gdraw :: (Data a) => a -> CairoDiagram
  gdraw = const mempty `extQ` (drawExpr . convertOp) `extQ` drawExpr
 
  convertOp :: QOp SrcSpanInfo -> ExpS
  convertOp (QVarOp s' qn) = Var s' qn
  convertOp (QConOp s' qn) = Con s' qn

  larrow = lined $ arrow (3, 0) (1, 1)
-}

{-
  drawType :: ExpS -> CairoDiagram
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
  withType :: ExpS -> Type -> CairoDiagram
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

--    doApp :: SrcSpanInfo -> ExpS -> [ExpS] -> CairoDiagram
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
