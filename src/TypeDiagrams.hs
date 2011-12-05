{-# LANGUAGE FlexibleContexts, TypeFamilies, ViewPatterns, TupleSections #-}

module TypeDiagrams where

import Control.Monad
import Control.Arrow ((&&&), second)
import Data.Default
import Data.Foldable (concat)
import Data.IORef
import Data.Label
import Data.List (intersperse, sort)
import Data.Maybe (catMaybes)
import Data.Monoid (Monoid)
import Data.Supply
import Debug.Trace
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
          [ ("map", "forall a. (Arrow a) => a b c -> a d e -> a (b, d) (c, e)")
          , ("(+1)", "a -> a")
          , ("[1..]", "[a]") ]
      squig _ xs = Just $ (lined (squiggle 1) # alignT ||| vsep xs # alignT)
      usr = matchContext (parseT "Arrow a => a") (gtDraw squig)
--      usr r a t = r a t
  let dia = fontSize 2 -- . showLabels -- . fontSize 10
          $ typeDiagram (0 :: Int) (coloredShapes $ map (:[]) ['a'..]) usr (snd $ tys !! 0)
      bg = fc white $ rect (20 + width dia) (20 + height dia)
  defaultMain ((centerXY dia) `atop` (centerXY bg))
 where
  parseT = mustOk . parseIt

type CDiagram = AnnDiagram Cairo R2 Any

type TypeDiagram = [AsstS] -> TypeS -> CDiagram

type UserDiagram = TypeDiagram -> [AsstS] -> [TypeS] -> Maybe CDiagram

-- Utility to convert a function from a Type and its arguments into a
-- UserDiagram.
gtDraw :: (TypeS -> [CDiagram] -> Maybe CDiagram) -> UserDiagram
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
  :: (HasOrigin a, Boundable a, Monoid a, V a ~ (Double, Double))
  => [a] -> a
hsep = hcat' (def {sep = 0.5})
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
  segs = arcTrail (-40) 40 (x, y)
      ++ [Linear (-t2, 0)]
      ++ arcTrail (-40) 40 (x - t1, -y)

firstTrail (Path ((_, Trail t _):_)) = t

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

lined = lineWidth 0.3 . stroked
bar d = (lined . hrule $ width d) === centerX d

typeDiagram :: IsName t
  => t
  -> M.Map String (AnnDiagram Cairo R2 Any)
  -> UserDiagram
  -> TypeS
  -> CDiagram
typeDiagram pre dm usr = rec [] . debug' "ty "
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

  rec :: TypeDiagram
  rec ctx t = nameEnds t $ draw t
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
            in moveOriginBy (0, -1.3) . vsep' (-0.7)
             $ [topper (width parts), centerX parts]
     where
      ts = splitTApp t

    draw (TyForall _ Nothing ctx' t) = -- rec ctx t --TODO 
      rec (ctx ++ get contextList ctx') t
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

    topper w = lined $ rotate (Deg (-90))
             $ bracket (1, w + 1)
    tcross   = lined $ cross (0.5, 0.5)
    lbracket = lined $ bracket (1, 2.2)
    rbracket = scaleX (-1) lbracket
    lparen   = scaleX (-1) rparen
    rparen   = lineWidth 0 . fc black . stroked $ bannana (2, 2) 0.1 0.3
