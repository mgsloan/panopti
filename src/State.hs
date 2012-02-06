{-# LANGUAGE TemplateHaskell #-}

module State where

import ActiveHs.Simple
import Utils

import Control.Arrow (second)
import Data.Dynamic
import Data.Label
import Graphics.UI.Gtk.Toy.Prelude (CairoDiagram, CursorMark, MarkedText(..))

data State = State
  { _chan :: TaskChan
  , _code :: MarkedText (Versioned Ann)
  , _result :: CairoDiagram
  }

data Versioned a 
  = Version
  { _versionValue :: a
  , _versionNumber :: VersionNumber
  }
 deriving (Eq, Show)

type VersionNumber = Int

data Ann
  = CursorA
  | AstA Dynamic
  | AppA DeclS
  | TypeA SubsetId TypeS
  | ErrorA Cause
  | SubsetA SubsetId [TypeResolution]
  | ParseResA ParseRes

type Cause = String

type SubsetId = (Int, Int)

data ParseRes = ParseRes Cause String
 deriving (Eq, Show)
 
data TypeResolution
  = DataRes { resType :: TypeS }
  | TypeRes { resType, targType :: TypeS }
  | InstRes { resType :: TypeS }
  deriving (Eq, Ord)

$(mkLabels [''State, ''Versioned])

debugMarks pre = debugWith (\(MarkedText t ms) -> pre ++ (show $ map (second $ annType . get versionValue) ms))

annType CursorA = "CursorA"
annType (AstA d) = "AstA-" ++ (tyConName . typeRepTyCon $ dynTypeRep d)
annType (AppA _) = "AppA"
annType (TypeA _ _) = "TypeA"
annType (ErrorA _) = "ErrorA"
annType (SubsetA _ _) = "SubsetA"
annType (ParseResA _) = "ParseResA"
