{-# LANGUAGE StandaloneDeriving, TupleSections, TypeFamilies #-}
module Annotations where

import State
import TypeInfo
import Utils

import Control.Applicative ((<$>))
import Control.Arrow ((***), (&&&), second)
import Data.Data
import Data.Dynamic (toDyn)
import Data.Generics.Aliases
import Data.Label
import qualified Data.Map as M
import Data.Maybe (listToMaybe, catMaybes, maybeToList)
import Graphics.UI.Gtk.Toy.Text hiding (Ivl, debug)
import Language.Haskell.Exts.Annotated
import Language.Haskell.Exts.Annotated.Syntax

deriving instance Show Ann

instance Eq Ann where
  CursorA     == CursorA   = True
  ParseResA a == ParseResA b = a == b
  AppA      a == AppA      b = a == b
  TypeA   a b == TypeA   c d = a == c && b == d
  ErrorA    a == ErrorA    b = a == b
  _ == _ = False

instance Mark Ann where
  type DrawState Ann = StyleState
  {-
  initialDrawState _ = StyleState monoStyle
  drawStateStyle _ (StyleState s) = s
  drawMark f CursorA = drawMark f CursorMark
  drawMark f _ = f
  -}
  initialDrawState = undefined
  drawStateStyle = undefined
  drawMark = undefined
--  drawMark (ParseResA x) = 
--  drawMark (ParseResA p) = 
  mergeMark CursorA CursorA = Just CursorA
  mergeMark x y = if x == y then Just x else Nothing
  mergeMark _ _ = Nothing
  splitMark _ CursorA = (Just CursorA, Just CursorA)
  splitMark _ x@(SubsetA _ _) = (Just x, Just x)
  splitMark _ _ = (Nothing, Nothing)

instance CanBeCursor Ann where
  isCursor CursorA = True
  isCursor _       = False
  mkCursor         = CursorA

instance Mark a => Mark (Versioned a) where
  type DrawState (Versioned a) = DrawState a
  initialDrawState = undefined
  drawStateStyle = undefined
  drawMark = undefined
  {-
  initialDrawState _ = initialDrawState (emptyText :: MarkedText a)
  drawStateStyle _ = drawStateStyle (emptyText :: MarkedText a)
                   . get versionValue
  drawMark x s m = drawMark (get versionValue x) s m
  -}
  mergeMark (Version na va) (Version nb vb)
    | na == nb = Version na <$> mergeMark va vb
    | otherwise = Nothing
  
instance CanBeCursor a => CanBeCursor (Versioned a) where
  isCursor = isCursor . get versionValue
  --TODO: uhh
  mkCursor = Version 0 mkCursor

astAnns :: Data a => a -> [(Ivl, Ann)]
astAnns x = (++ concat (gmapQ astAnns x)) . maybeToList $ do
  ivl@(f, _) <- get colSpan <$> getSpan x
  return (ivl, AstA . toDyn $ mutateSpans' (\(fr, to) -> (fr + 1 - f, to - f + 1)) x)

annIvl :: Annotated a => a SrcSpanInfo -> Ivl
annIvl = get colSpan . srcInfoSpan . ann

{-
appAnns :: DeclMap -> [(Ivl, Ann)]
appAnns dm = [ (ivl, AppA ivls)
             | d <- M.elems dm
             , let ivl@(f, _) = annIvl d
             , let ivls = map (mapT (subtract f) . annIvl)
                        . splitEApp' . debug $ get funExpr d
             , length (debug ivls) > 1   --TODO: inefficient
             ]
-}

typeAnns :: SubsetId -> TypedDecls -> [(Ivl, Ann)]
typeAnns sub = map (annIvl *** TypeA sub)

subsetsAnns :: [[ ([TypeResolution], TypedDecls) ]] -> [(Ivl, Ann)]
subsetsAnns = concat . concat . zipWith (\i -> zipWith (anns i) [0..]) [0..]
 where
  anns i j (tr, d)
    | null tas = []
    | otherwise = (foldl1 ivlUnion $ map fst tas, SubsetA (i, j) tr) : tas
   where
    tas = typeAnns (i, j) d

isCursorA   (Version _  CursorA     ) = True; isCursorA   _ = False
isAstA      (Version _ (AstA      _)) = True; isAstA      _ = False
isAppA      (Version _ (AppA      _)) = True; isAppA      _ = False
isTypeA     (Version _ (TypeA   _ _)) = True; isTypeA     _ = False
isErrorA    (Version _ (ErrorA    _)) = True; isErrorA    _ = False
isSubsetA   (Version _ (SubsetA _ _)) = True; isSubsetA   _ = False
isParseResA (Version _ (ParseResA _)) = True; isParseResA _ = False

getSubset (Version _ (SubsetA s _)) = Just s
getSubset (Version _ (TypeA   s _)) = Just s
getSubset _ = Nothing
