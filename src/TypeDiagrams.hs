{-# LANGUAGE FlexibleContexts, TypeFamilies, ViewPatterns, TupleSections #-}

module TypeDiagrams
  ( drawCode
  , typeDiagram
  , lined, arrow ) where

import Annotations
import State
import Utils

import Control.Arrow ((***), (&&&), first, second)
import Control.Monad
import Control.Newtype
import Data.Data hiding (typeOf)
import Data.Default
import Data.Dynamic (fromDynamic)
import Data.Foldable (concat)
import Data.IORef
import Data.Label
import Data.List (intersperse, sort, find, findIndices)
import Data.List.Split (splitWhen, splitOn)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid (Monoid)
import Data.Supply
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Backend.Cairo.Text (StyleParam, textLineBounded)
import Graphics.Rendering.Diagrams.Names (NameMap(..), Name(..), AName(..))
import Graphics.UI.Gtk.Toy.Diagrams
import Graphics.UI.Gtk.Toy.Prelude hiding 
  ((===), (|||), trim, debug, debug', fromDynamic, ivlContains, Ivl)
import Prelude hiding (concat)
import Language.Haskell.Exts.Annotated hiding (Name)
import qualified Data.Map as M

import System.IO.Unsafe

type ISupply = IORef (Supply Int)

startSupply :: IO ISupply
startSupply = newIORef =<< newDupableNumSupply 

popSupply r = unsafePerformIO $ do
  val <- readIORef r
  let (val', _) = split2 val
  writeIORef r val'
  return (supplyValue val)

coloredShapes :: [String] -> M.Map String CairoDiagram
coloredShapes names
  = M.fromList 
  . (++ [("Int", scale 0.2 $ monoText "Z")])
  . zip names
  $ zipWith (\i c -> fc c $ polygon $ def {polyType = PolyRegular i 1.0}) [4..]
    [aqua, crimson, brown, fuchsia, khaki, indigo, papayawhip, thistle]

main :: IO ()
main = do
  --sup <- startSupply
  let tys = map (second parseT)
          [ ("map", "forall a. (Arrow a) => a b c -> a d e -> a (b, d) (c, e)")
          , ("(+1)", "a -> a")
          , ("[1..]", "[a]") ]
      squig _ xs = Just $ (lined (squiggle 1) # alignT ||| vsep xs # alignT)
      usr = matchContext (parseT "Arrow a => a") (gtDraw squig)
--      usr r a t = r a t
  let dia = typeDiagram (0 :: Int) (coloredShapes $ map (:[]) ['a'..]) usr (snd $ tys !! 0)
      bg = fc white $ rect (20 + width dia) (20 + height dia)
  defaultMain ((centerXY dia) `atop` (centerXY bg))
 where
  parseT = mustOk . parseIt

monoText :: String -> CairoDiagram
monoText = textLineBounded monoStyle 

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

  subsetIx = case filter (isCursor . snd) marks of
    (ivl, _) : _ -> fromMaybe 0 $ do
      (i, m) <- smallestEnclosing isSubsetA ivl mt
      (s, _) <- getSubset m
      return s
    [] -> 0

  drawText = draw . (`MarkedText` [])

  drawSubstr mt = draw . substrText False mt

  drawType = scale 5 . typeDiagram "" shapes (const $ const $ const Nothing)

  drawIvlType ivl t =
    case smallestEnclosing isTypeA ivl t of
      Just (_, Version _ (TypeA (s, _) typ))
        | s == subsetIx -> drawType typ
      Nothing -> drawText "?"

  shapes = coloredShapes . map prettyPrint $ uniquePrims allTypes

  allTypes = [x | (_, Version _ (TypeA (s, _) x)) <- marks, s == subsetIx]

  draw (MarkedText txt [])
    = monoText $ filter (not . (`elem` "\r\n")) txt

  draw (MarkedText txt (((fm, tm), m):xs))
    =   drawSubstr mt (-1, fm)
    ||| handleMark (get versionValue m) (substrText True mt (fm, tm))
    ||| drawSubstr mt (tm, length txt + 1)
   where
    mt = MarkedText txt xs


    includeGaps mt = ivlsFromSlices (textLength mt)
                   . concatMap ivlSlices

    handleMark CursorA -- = drawMark CursorMark txt . draw
      = \mt' -> case get mText mt' of
          "" -> lineWidth 1 . lineColor black
              . moveOriginBy (-1.5, 2)
              . setEnvelope mempty
              $ drawSeg (0, 18)
          _ -> draw mt'
             # fc white
             # highlight black

{-
    handleMark (AppA ivls)
      = \mt' -> let rec = map (drawSubstr mt') $ includeGaps mt' ivls
                    w = width rec
                in hsep rec === alignL (hrule w) === texts "App"
 -}

{-
    handleMark (TypeA (s, _) ty)
      | s == subsetIx = (\x -> x === drawType ty) . draw
      | otherwise = draw
 -}
    
    {-
    handleMark (SubsetA (s, _) tyc)
      | s == subsetIx = (\x -> x === drawText (show tyc)) . draw
      | otherwise = draw
    -}

    handleMark (AstA d)
      = \t -> let t' = removeAstMarks t 
               in maybe (draw t') id
                $ msum [ drawExpr t' <$> fromDynamic d
                       , drawOp   t' <$> fromDynamic d ]

    handleMark _ = draw

    removeAstMarks (MarkedText txt ms)
      = MarkedText txt $ filter (not . isTopExpr . second (get versionValue)) ms
     where
      isTopExpr (i, AstA d)
        = i == (0, length txt - 1) -- && isJust (fromDynamic d :: Maybe ExpS)
      isTopExpr _ = False

    -- TODO: show (via highlight) which parts are part of the code

    -- TODO: find position of ->
    drawExpr t (InfixApp _ l o r) = hcat
      [ drawSubstr t soIvl
      , drawSubstr t oIvl
      , drawSubstr t oeIvl
      ]
     where
      [soIvl, oIvl, oeIvl] = includeGaps mt [annSpan o]

    drawExpr t a@(App _ l r) = drawApp t a
--      [ drawSubstr t ]

    drawExpr t (Lambda _ pats expr) = hcat
      [ drawText "λ"
      , drawSubstr t bs
      , strutX 10
      , alignB . lined $ arrow (20, 0) (5, 5)
      , strutX 10
      , drawSubstr t e
      , drawSubstr t pe
      ]
     where
      [pbs, bs, m, e, pe]
        = includeGaps t [foldl1 ivlUnion $ map annSpan pats, annSpan expr]
    
    drawExpr t (EnumFrom       _ f)
      = drawEnum t (annSpan f) Nothing
    drawExpr t (EnumFromTo     _ f to)
      = drawEnum t (annSpan f) . Just $ annSpan to
    drawExpr t (EnumFromThen   _ f th)
      = drawEnum t (ivlUnion (annSpan f) $ annSpan th) Nothing
    drawExpr t (EnumFromThenTo _ f th to)
      = drawEnum t (ivlUnion (annSpan f) $ annSpan th) . Just $ annSpan to

    drawExpr t _ = draw t

    drawApp :: MarkedText (Versioned Ann) -> ExpS -> CairoDiagram
    drawApp t a@(App _ _ _)
      = dia <> appBar
     where
      appBar :: CairoDiagram
      appBar = foldl1 (<>) . zipWith lineBetween ps $ tail ps

      lineBetween :: Point R2 -> Point R2 -> CairoDiagram
      lineBetween p1 p2 = scale (-1) . moveOriginTo p1 $ drawSeg (p2 .-. p1)

      ps = map (location . head) . catMaybes $ map (\n -> lookupN n (names dia)) xs

      dia = hcat . zipWith tDia xs . ((strutY 5 ===):) $ repeat id

      tDia ivl f = drawSubstr t ivl === (named ivl . f $ drawIvlType ivl t)

      xs = map annSpan $ splitEApp a

      {-
        lPos :: [Point R2]
        lPos ivl = [ location $ head p 
               | n <- M.assocs . unpack $ names dia
               , let (Name (AName (cast -> Just nivl):_), p) = n
               , nivl == ivl
               ]

       dia = ( drawSubstr t lIvl
              === 
              (--if isApp l then mempty else 
                named (rt,lIvl) $ drawIvlType lIvl t) )

    drawApp t l r = dia <> case tPos of
      (tr:tl:_) -> 
                   -- moveOriginTo tl $ drawSeg (tr .-. tl)
                   {- moveOriginTo (scaleX (-1) tr) (circle 5)
                   <> moveOriginTo (scaleX (-1) tl) (circle 10)
                   -}
      _ -> mempty
     where
      lIvl = annSpan l
      rIvl = annSpan r

        ||| ( drawSubstr t rIvl 
              === 
              (named (rt,rIvl) . (strutY 2 ===) $ drawIvlType rIvl t) )
      
      rt = "result type"

      ns :: [(Name, Point R2)]
      ns = map (second $ location . head) . M.assocs . unpack $ names dia

      tPos :: [Point R2]
      tPos = [p | (Name (AName (cast -> Just ("result type", ivl :: Ivl)):_), p) <- ns]
      -}

--    drawExpr t (NegApp _ l) = hcat

    ellipsis = translateY 2.5 (c1 ||| c1 ||| c1)
     where c1 = circle 0.1 ||| strutX 3

    drawEnum t (_, fr) to = hcat
      [ drawSubstr t (0, fr)
      , strutX 2
      , ellipsis
      , maybe mempty (drawSubstr t . (second $ const $ textLength t)) to
      ]

-- TODO: special cases for %

    fancy = "@$%?%"

    drawOp t op = case op of
      QVarOp _ o -> helper o
      QConOp _ o -> helper o
     where
      helper (Qual _ mn n) = drawText (prettyPrint $ Qual sp mn $ Symbol sp "")
                          ||| drawOp' (prettyPrint n)
      helper v = drawOp' . init . tail $ prettyPrint v

    drawOp' :: String -> CairoDiagram
    drawOp' = hcat . map (translateY 5.5 . helper)
     where
      seg = translateY 0.5 $ hrule 10
      slash = rotate (Deg 60.0) (hrule 12)
      equals = seg === strutY 3 === seg
      diamond l r xs = undefined
      helper '-'  = seg
      helper '+'  = seg <> rotate (Deg 90.0) seg
      helper '='  = equals
      helper '#'  = equals <> rotate (Deg 90.0) equals
      helper '/'  = slash
      helper '\\' = scaleX (-1) slash
      --TODO: better enclosure.
      {-
      helper '<'  =
        case map reverse . splitOn ">" $ reverse xs of
          [post, pre] -> diamond True True pre ||| drawOp post
          _           -> diamond True False xs
      helper (p:'>':xs) = diamond 
      -}
      helper '|' = vrule 16
      
      helper '.' = circle 4
      helper x = centerXY $ drawText [x]

{-
  | Let l (Binds l) (Exp l)
  | If l (Exp l) (Exp l) (Exp l)
  | Case l (Exp l) [Alt l]
  | Do l [Stmt l]
  | MDo l [Stmt l]
  | Tuple l [Exp l]
  | TupleSection l [Maybe (Exp l)]
  | List l [Exp l]
  | Paren l (Exp l)
  | LeftSection l (Exp l) (QOp l)
  | RightSection l (QOp l) (Exp l)
  | RecConstr l (QName l) [FieldUpdate l]
  | RecUpdate l (Exp l) [FieldUpdate l]
  | ListComp l (Exp l) [QualStmt l]
  | ParComp l (Exp l) [[QualStmt l]]
  | ExpTypeSig l (Exp l) (Type l)
  | VarQuote l (QName l)
  | TypQuote l (QName l)
  | BracketExp l (Bracket l)
  | SpliceExp l (Splice l)
  | QuasiQuote l String String
  | XTag l (XName l) [XAttr l] (Maybe (Exp l)) [Exp l]
  | XETag l (XName l) [XAttr l] (Maybe (Exp l))
  | XPcdata l String
  | XExpTag l (Exp l)
  | XChildTag l [Exp l]
  | CorePragma l String (Exp l)
  | SCCPragma l String (Exp l)
  | GenPragma l String (Int, Int) (Int, Int) (Exp l)
  | Proc l (Pat l) (Exp l)
  | LeftArrApp l (Exp l) (Exp l)
  | RightArrApp l (Exp l) (Exp l)
  | LeftArrHighApp l (Exp l) (Exp l)
  | RightArrHighApp l (Exp l) (Exp l)
-}

type TypeDiagram = [AsstS] -> TypeS -> CairoDiagram

type UserDiagram = TypeDiagram -> [AsstS] -> [TypeS] -> Maybe CairoDiagram

-- Utility to convert a function from a Type and its arguments into a
-- UserDiagram.
gtDraw :: (TypeS -> [CairoDiagram] -> Maybe CairoDiagram) -> UserDiagram
gtDraw f g as (t:ts) = f t $ map (g as) ts

-- Applies to primitives that posess a similar context as the given type
matchContext :: TypeS -> UserDiagram -> UserDiagram
matchContext (TyForall _ _ c t) usr rec c' ts@(t':_)
  | process t (get contextList c) == process t' c' = usr rec c' ts
  | otherwise = Nothing
 where
  process t = sort . map (\(ClassA _ x _) -> prettyPrint x) . filter pred
   where
    pred a@(ClassA _ _ xs) = elem (prettyPrint t) $ map prettyPrint xs
    pred _ = False

matchContext _ _ _ _ _ = Nothing

-- Diagrams Utilities

cw  (x, y) = (negate y, x)
ccw (x, y) = (y, -x)

--cwd  = rotate (Deg (-90))
--ccwd = rotate (Deg 90)
ltrail = Trail . map Linear

scaleXY (x, y) = scaleX x . scaleY y

hsep, vsep
  :: ( HasOrigin a, Enveloped a, Monoid a, Semigroup a, Juxtaposable a
     , V a ~ (Double, Double) )
  => [a] -> a
hsep = hcat' (def {sep = 1})
vsep = vcat' (def {sep = 1})

hsep', vsep' 
  :: ( HasOrigin a, Enveloped a, Monoid a, Semigroup a, Juxtaposable a
     , V a ~ (Double, Double) )
  => Double -> [a] -> a
hsep' s = hcat' (def {sep = s})
vsep' s = vcat' (def {sep = s})

drawSeg v = stroke . pathFromTrail
          $ Trail [Linear v] False

stroked :: Path R2 -> CairoDiagram
stroked = stroke' (def { vertexNames = [] :: [[Int]] })

infixl 6 ===
infixl 6 |||

(|||), (===)
  :: ( Monoid a, Juxtaposable a, HasOrigin a, Enveloped a, Semigroup a
     , V a ~ (Double, Double))
  => a -> a -> a

x === y = vsep [x, y]
x ||| y = hsep [x, y]

setRectBounds :: R2 -> Diagram b R2 -> Diagram b R2
setRectBounds r2 = setEnvelope bnds
 where
  bnds = getEnvelope $ centerXY $ Path
    [ (P zeroV, ltrail [ r2 ] False) ]

expandRectBounds :: R2 -> Diagram b R2 -> Diagram b R2
expandRectBounds (x, y) d = setRectBounds (width d + x, height d + y) d

texts :: String -> CairoDiagram
texts = centerY . textLineBounded (fontSize 14)

texts' :: String -> CairoDiagram
texts' = centerY . scale 0.1 . textLineBounded (fontSize 14)

arrow :: ( Floating (Scalar t)
         , Num t
         , InnerSpace t
         , AdditiveGroup (Scalar t) )
      => (t, t)               -- ^ Vector used for direction and length
      -> (Scalar t, Scalar t) -- ^ Width and height of the arrow head
      -> Path (t, t)          -- ^ Path which yields an arrow diagram when stroked
arrow v (w, h) = Path
  [ (P zeroV, ltrail [ v ] False)
  , (P (v ^+^ hp ^+^ hn), ltrail [ negateV (hn ^+^ hp), hn ^-^ hp ] False)
  ]
 where
  nv = negateV $ normalized v
  hn = nv ^* h
  hp = cw nv ^* w

bracket :: (Fractional a) => (a, a) -> Path (a, a)
bracket (w, h) = Path
  [ (P (w, -h / 2), ltrail [ (-w, 0), (0, h), (w, 0) ] False) ]

bannana :: R2 -> Double -> Double -> Path R2
bannana (x, y) t1 t2 = centerY $ Path $ [(P (0, -y), Trail segs True)]
 where
  segs = arcTrail (-40) 40 (x, y)
      ++ [Linear (-t2, 0)]
      ++ arcTrail (-40) 40 (x - t1, -y)

firstTrail :: Path R2 -> [Segment R2]
firstTrail (Path ((_, Trail t _):_)) = t

arcTrail :: Double -> Double -> R2 -> [Segment R2]
arcTrail ld hd v = firstTrail $ scaleXY v $ arc (Deg ld) (Deg hd)

squiggle :: Double -> Path R2
squiggle h = scaleY (-1)
           . (mappend (arrow (0, 2) (1, 1))) 
           . Path . (:[]) . (P zeroV,) . (`Trail` False)
           $ arcTrail (90) (-90) v
          ++ arcTrail (-270) (-90) v
          ++ arcTrail (90) (-90) v
          ++ arcTrail (-270) (-90) v
 where
  v = (h / 4, h / 3)

cross :: R2 -> Path R2
cross v = Path [ (P v,      Trail [Linear $ v ^* (-2)] False)
               , (P $ cw v, Trail [Linear $ ccw (v ^* 2)]     False)
               ]

lined :: Path R2 -> CairoDiagram
lined = lineWidth 1 . stroked

{-
barTop d    = (lined . hrule $ width d) 
               ===
              centerX d
barBottom d = centerX d 
               ===
              (lined . hrule $ width d)
 -}

typeDiagram :: IsName t
  => t
  -> M.Map String CairoDiagram
  -> UserDiagram
  -> TypeS
  -> CairoDiagram
typeDiagram pre dm usr = (=== strutY 2) . rec [] 
 where
  prim ident s
    = case M.lookup ident dm of
        Just d  -> named (ident, s) d
        Nothing -> named (ident, s) $ texts' ident

  primT ty = prim (trim $ prettyPrint ty) (getSpan' ty)

  nameEnds t d = hcat
    [ named (pre, getSpan' t, False) mempty
    , d
    , named (pre, getSpan' t, True) mempty
    ]

  rec :: TypeDiagram
  rec ctx ty = nameEnds ty $ draw ty
   where
    recc = rec ctx

    draw t@(TyCon _ _) = primT t
    draw t@(TyVar _ _) = primT t

    draw t@(TyFun _ _ _)
      = hsep' (-0.5) [ alignT . farrow $ height parts, alignT parts ]
     where
      parts = vsep . map recc $ splitTFunc t
      farrow h = lined $ arrow (0, negate $ h + 3.0) (1.5, 1.5)

    draw t@(TyApp _ _ _) = case usr rec ctx ts of 
      Just d -> d
      _ -> let parts = hsep $ map recc ts
            in moveOriginBy (0, -1.3)
             $ centerX parts
     where
      ts = splitTApp t

    draw (TyForall _ Nothing ctx' t) = -- rec ctx t --TODO 
      rec (ctx ++ get contextList ctx') t
    draw (TyForall _ bnds ctx' t)
      = ( texts' "A" # scaleY (-1)
--           ||| -- ===
--          vsep (zipWith prim (getVars bnds))
           ||| text "." # scaleXY (0.2, 0.2)
        ) ||| rec ctx t

    draw (TyList _ x)  = bracketDia $ draw x
    draw (TyParen _ x) = draw x -- hsep [ lparen,   draw x, rparen ]
    draw (TyTuple _ _ xs) = hsep
      $ [lparen] ++ intersperse tcross (map recc xs) ++ [rparen]
    
    -- TODO: better intervals
    draw (TyInfix s l o r) = recc (TyApp s (TyApp s (TyCon s o) l) r)
    draw (TyKind _ t _)    = recc t

    tcross   = lined $ cross (0.5, 0.5)
    bracketDia d = hsep [ centerY . lined $ bracket (1, h), d, centerY . lined $ bracket (-1, h) ]
     where h = height d
    lparen   = scaleX (-1) rparen
    rparen   = lineWidth 0 . fc black . stroked $ bannana (2, 2) 0.1 0.3
