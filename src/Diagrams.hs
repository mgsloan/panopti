{-# LANGUAGE FlexibleContexts, TypeFamilies, ViewPatterns #-}

module Diagrams where

import Control.Arrow ((&&&), second)
import Data.Default
import Data.IORef
import Data.List (intersperse, sort)
import Data.Supply
import Diagrams.Prelude hiding ((===), (|||))
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.TwoD.Text(Text(..))
import Utils
import Language.Haskell.Exts
  (prettyPrint, Type(..), Name(..), Context, Asst(..), QName(..))
import qualified Data.Map as M

import System.IO.Unsafe

supplyStart :: IO (IORef (Supply Int))
supplyStart = newIORef =<< newDupableNumSupply 

supplyPop r = unsafePerformIO $ do
  val <- readIORef r
  let (val', _) = split2 val
  writeIORef r val'
  return (supplyValue val)

coloredShapes :: M.Map String CDiagram
coloredShapes 
  = M.fromList 
  . zip (map (:[]) ['a'..])
  $ zipWith (\i c -> fc c $ polygon $ def {polyType = PolyRegular i 1.0}) [4..]
    [aqua, crimson, brown, fuchsia, khaki, indigo, papayawhip, thistle]

{-
main :: IO ()
main = do
  sup <- supplyStart
  let tys = map (second parseT)
          [ ("map", "[(a b c, b)]")
          , ("(+1)", "a -> a")
          , ("[1..]", "[a]") ]
--      usr = matchContext (parseT "Monad m => m") (text' "M")
      usr = const $ const Nothing
  let dia = fontSize 2 . fontSize 10
          $ codeDiagram sup coloredShapes usr tys
      bg = fc white $ rect (20 + width dia) (20 + height dia)
  defaultMain ((centerXY dia) `atop` (centerXY bg))
 where
  parseT = mustOk . parseType
-}

type CDiagram = AnnDiagram Cairo R2 Any

type UserDiagram = Context -> Type -> Maybe CDiagram

-- Applies to primitives that posess a similar context as the given type
matchContext :: Type -> CDiagram -> UserDiagram
matchContext (TyForall _ c (TyVar n)) d c' (TyVar n') 
  | process n c == process n' c' = Just d
  | otherwise = Nothing
 where
  process m = debug . sort . map (\(ClassA x _) -> x) . filter pred
   where
    pred a@(ClassA _ xs) = elem (TyVar m) xs
    pred _ = False

matchContext _ _ _ _ = Nothing

-- Diagrams Utilities

cw  (x, y) = (negate y, x)
ccw (x, y) = (y, -x)

ltrail = Trail . map Linear

scaleXY (x, y) = scaleX x . scaleY y

hsep = hcat' (def {sep = 1})
vsep = vcat' (def {sep = 0.1})
vsep' s = vcat' (def {sep = s})
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
    [ (P zeroV, ltrail [ (7 * (fromIntegral $ length xs), 2) ] False) ]

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

paren :: R2 -> Double -> Double -> Path R2
paren (x, y) t1 t2 = centerY $ Path $ [(P (0, -y), Trail segs True)]
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

--stateDiagram :: State -> CDiagram
--stateDiagram


codeDiagram :: IsName t
  => IORef (Supply t)
  -> M.Map String CDiagram
  -> UserDiagram
  -> [(String, Type)]
  -> CDiagram

codeDiagram ns dm usr = hsep . map (\(txt, typ) -> vsep' 5
  [ centerX $ text' txt
  , centerX $ typeDiagram ns dm usr typ ])

typeDiagram :: IsName t
  => IORef (Supply t)
  -> M.Map String (AnnDiagram Cairo R2 Any)
  -> UserDiagram
  -> Type
  -> CDiagram

typeDiagram ns dm usr = rec []
 where
  prim ident 
    = case M.lookup ident dm of
        Just d  -> named name d
        Nothing -> named name $ text' ident
   where
    name = (ident, supplyPop ns)

  rec ctx (usr ctx -> Just d) = d
  rec ctx t = draw t
   where
    draw (TyCon x) = prim . trim $ prettyPrint x
    draw (TyVar x) = prim . trim $ prettyPrint x

    draw t@(TyFun _ _)
      = vsep' (-0.5) [ alignL . farrow $ width parts, alignL parts ]
     where
      parts = hsep . map (rec ctx) $ splitTFunc t

    draw t@(TyApp _ _) = moveOriginBy (0, -1.3) . vsep' (-0.7) $
                         [topper (width parts), centerX parts]
     where
      parts = hsep $ map (rec ctx) $ splitTApp t

    draw (TyForall Nothing ctx' t) = rec (ctx ++ ctx') t
    draw (TyForall bnds ctx' t)
      = (scaleY (-1) $ text "A")
         ===
        (vsep . map prim $ getVars bnds)
      ||| rec (ctx ++ ctx') t

    draw (TyList x)  = hsep [ lbracket, draw x, rbracket ]
    draw (TyParen x) = hsep [ lparen,   draw x, rparen ]
    draw (TyTuple _ xs) = hsep
      $ [lparen] ++ intersperse tcross (map (rec ctx) xs) ++ [rparen]
    
    draw (TyInfix l o r) = rec ctx (TyApp (TyApp (TyCon o) l) r)
    draw (TyKind t _)    = rec ctx t

    lines = lineWidth 0.3 . stroked
    farrow w = lines $ arrow (w + 3.0, 0) (1.5, 1.5)
    topper w = lines $ rotate (Deg (-90))
             $ bracket (1, w + 1)
    tcross   = lines $ cross (0.5, 0.5)
    lbracket = lines $ bracket (1, 2.2)
    rbracket = scaleX (-1) lbracket
    lparen   = scaleX (-1) rparen
    rparen   = lineWidth 0 . fc black . stroked $ paren (2, 2) 0.1 0.3
