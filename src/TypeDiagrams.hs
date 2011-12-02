{-# LANGUAGE FlexibleContexts, TypeFamilies, ViewPatterns #-}

module TypeDiagrams where

import Control.Arrow ((&&&), second)
import Data.Default
import Data.Foldable (concat)
import Data.IORef
import Data.Label
import Data.List (intersperse, sort)
import Data.Monoid (Monoid)
import Data.Supply
import Diagrams.Prelude hiding ((===), (|||))
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.TwoD.Text(Text(..))
import Prelude hiding (concat)
import Utils
import Language.Haskell.Exts.Annotated
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

coloredShapes :: [String] -> M.Map String CDiagram
coloredShapes names
  = M.fromList 
  . zip names
  $ zipWith (\i c -> fc c $ polygon $ def {polyType = PolyRegular i 1.0}) [4..]
    [aqua, crimson, brown, fuchsia, khaki, indigo, papayawhip, thistle]

main :: IO ()
main = do
  --sup <- startSupply
  let tys = map (second parseT)
          [ ("map", "a")
          , ("(+1)", "a -> a")
          , ("[1..]", "[a]") ]
--      usr = matchContext (parseT "Monad m => m") (text' "M")
      usr = const $ const Nothing
  let dia = fontSize 2 . showLabels -- . fontSize 10
          $ typeDiagram (0 :: Int) (coloredShapes $ map (:[]) ['a'..]) usr (snd $ tys !! 0)
      bg = fc white $ rect (20 + width dia) (20 + height dia)
  defaultMain ((centerXY dia) `atop` (centerXY bg))
 where
  parseT = mustOk . parseType

type CDiagram = AnnDiagram Cairo R2 Any

type UserDiagram = ContextS -> TypeS -> Maybe CDiagram

-- Applies to primitives that posess a similar context as the given type
{-
matchContext :: TypeS -> CDiagram -> UserDiagram
matchContext (TyForall _ _ c (TyVar _ n)) d c' (TyVar _ n') 
  | process n c == process n' c' = Just d
  | otherwise = Nothing
 where
  process m = sort . map (\(ClassA _ x _) -> x) . filter pred
   where
    pred a@(ClassA _ xs) 
      = elem (prettyPrint $ TyVar sp m) $ map prettyPrint xs
    pred _ = False

matchContext _ _ _ _ = Nothing
-}

-- Diagrams Utilities

cw  (x, y) = (negate y, x)
ccw (x, y) = (y, -x)

--cwd  = rotate (Deg (-90))
--ccwd = rotate (Deg 90)
ltrail = Trail . map Linear

scaleXY (x, y) = scaleX x . scaleY y

hsep, vsep
  :: (HasOrigin a, Boundable a, Monoid a, V a ~ (Double, Double))
  => [a] -> a
hsep = hcat' (def {sep = 1})
vsep = vcat' (def {sep = 1})

hsep', vsep' 
  :: (HasOrigin a, Boundable a, Monoid a, V a ~ (Double, Double))
  => Double -> [a] -> a
hsep' s = hcat' (def {sep = s})
vsep' s = vcat' (def {sep = s})

stroked :: Path R2 -> CDiagram
stroked = stroke' (def { vertexNames = [] :: [[Int]] })

infixl 6 ===
infixl 6 |||

x === y = vsep [x, y]
x ||| y = hsep [x, y]

text' :: (Renderable (Path R2) b, Renderable Text b) 
      => String -> Diagram b R2
text' xs = text xs # setBounds bnds # fillColor forestgreen
 where
  bnds = getBounds $ centerXY $ Path
    [ (P zeroV, ltrail [ (1 * (fromIntegral $ length xs), 2) ] False) ]

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
  segs = arcTrail (x, y)
      ++ [Linear (-t2, 0)]
      ++ arcTrail (x - t1, -y)
  arcTrail v = t
   where
    (Path ((_, Trail t _):_)) = scaleXY v $ arc (Deg (-40)) (Deg 40)

cross :: R2 -> Path R2
cross v = Path [ (P v,      Trail [Linear $ v ^* (-2)] False)
               , (P $ cw v, Trail [Linear $ ccw (v ^* 2)]     False)
               ]

bar d = lineWidth 0.3 (stroked . hrule $ width d) === centerX d

-- Boyd optimization

typeDiagram :: IsName t
  => t
  -> M.Map String (AnnDiagram Cairo R2 Any)
  -> UserDiagram
  -> TypeS
  -> CDiagram
typeDiagram pre dm usr = rec []
 where
  prim ident s
    = case M.lookup ident dm of
        Just d  -> named (ident, s) d
        Nothing -> named (ident, s) $ text' ident

  primT ty = prim (trim $ prettyPrint ty) (getSpan' ty)

  nameEnds t d = hcat
    [ named (pre, getSpan' t, False) mempty
    , d
    , named (pre, getSpan' t, True) mempty
    ]

  rec ctx t@(usr undefined -> Just d) = nameEnds t d
  rec ctx t = nameEnds t $ draw t
   where
    recc = rec ctx

    draw t@(TyCon _ _) = primT t
    draw t@(TyVar _ _) = primT t

    draw t@(TyFun _ _ _)
      = hsep' (-0.5) [ alignT . farrow $ height parts, alignT parts ]
     where
      parts = vsep . map recc $ splitTFunc t
      farrow h = lines $ arrow (0, negate $ h + 3.0) (1.5, 1.5)

{-    = vsep' (-0.5) [ alignL . farrow $ width parts, alignL parts ]
     where
      parts = hsep . map (rec ctx) $ splitTFunc t
      farrow w = lines $ arrow (w + 3.0, 0) (1.5, 1.5)
 -}

    draw t@(TyApp _ _ _) = moveOriginBy (0, -1.3) . vsep' (-0.7) $
                         [topper (width parts), centerX parts]
     where
      parts = hsep $ map recc $ splitTApp t

    draw (TyForall _ Nothing ctx' t) = rec ctx t --TODO rec (ctx ++ get contextList (concat ctx')) t
    draw (TyForall _ bnds ctx' t)
      = ( text "A" # scaleXY (0.2, -0.2)
--           ||| -- ===
--          vsep (zipWith prim (getVars bnds))
           ||| text "." # scaleXY (0.2, 0.2)
        ) ||| rec ctx t

    draw (TyList _ x)  = hsep [ lbracket, draw x, rbracket ]
    draw (TyParen _ x) = draw x -- hsep [ lparen,   draw x, rparen ]
    draw (TyTuple _ _ xs) = hsep
      $ [lparen] ++ intersperse tcross (map recc xs) ++ [rparen]
    
    -- TODO: better intervals
    draw (TyInfix s l o r) = recc (TyApp s (TyApp s (TyCon s o) l) r)
    draw (TyKind _ t _)    = recc t

    lines = lineWidth 0.3 . stroked
    topper w = lines $ rotate (Deg (-90))
             $ bracket (1, w + 1)
    tcross   = lines $ cross (0.5, 0.5)
    lbracket = lines $ bracket (1, 2.2)
    rbracket = scaleX (-1) lbracket
    lparen   = scaleX (-1) rparen
    rparen   = lineWidth 0 . fc black . stroked $ bannana (2, 2) 0.1 0.3
