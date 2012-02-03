{-# LANGUAGE TupleSections #-}
module Annotations where

import Control.Applicative ((<$>))
import Control.Arrow ((&&&))
import Data.Data
import Data.Dynamic
import Data.Generics.Aliases
import Data.Maybe (listToMaybe, catMaybes, maybeToList)
import Graphics.UI.Gtk.Toy.Text
import Language.Haskell.Exts.Annotated
import Language.Haskell.Exts.Annotated.Syntax

type ExpS     = Exp     SrcSpanInfo
type DeclS    = Decl    SrcSpanInfo
type NameS    = Name    SrcSpanInfo 
type PatS     = Pat     SrcSpanInfo
type MatchS   = Match   SrcSpanInfo
type TypeS    = Type    SrcSpanInfo
type ContextS = Context SrcSpanInfo
type AsstS    = Asst    SrcSpanInfo
type QNameS   = QName   SrcSpanInfo

data Versioned a = Version Int a

type Cause = String

data Ann
  = AstA Dynamic
  | TypA TypeS
  | OmitA Cause
  | CursorA CursorMark

srcSpan :: SrcSpanInfo -> Ivl
srcSpan = (srcSpanStartColumn &&& srcSpanEndColumn) .  srcInfoSpan

getSpan :: Data a => a -> Maybe Ivl
getSpan = listToMaybe . catMaybes . gmapQ (const Nothing `extQ` (Just . srcSpan))

astAnns :: Data a => a -> [(Ivl, Ann)]
astAnns x = (++children) . maybeToList $ (, AstA $ toDyn x) <$> getSpan x
 where
  children = concat $ gmapQ astAnns x

