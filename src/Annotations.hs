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
import Graphics.UI.Gtk.Toy.Text hiding (Ivl)
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
astAnns x = maybe children (:children) 
          $ (, AstA $ toDyn x) . colSpan <$> getSpan x
 where
  children :: [(Ivl, Ann)]
  children = concat $ gmapQ astAnns x

annIvl :: Annotated a => a SrcSpanInfo -> Ivl
annIvl = colSpan . srcInfoSpan . ann

appAnns :: DeclMap -> [(Ivl, Ann)]
appAnns = map ((annIvl &&& AppA) . snd) . M.toList

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

isParseResA (ParseResA _) = True
isParseResA _ = False