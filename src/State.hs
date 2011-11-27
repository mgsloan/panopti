{-# LANGUAGE TemplateHaskell #-}

module State where

import Utils

import Data.Curve.Util (mapT)
import Data.IORef
import Data.Label
import Data.Map (Map)
import Graphics.ToyFramework
import Language.Haskell.Exts (Type(..))
import Simple
import System.Glib.MainLoop (HandlerId, timeoutRemove, timeoutAdd)

-- (function span, full expression's span, [argument spans])
type Apps = [(Ivl, Ivl, [Ivl])]
type ColorMap = Map String (Int, Int, Int)
type TypeMap = Map Ivl Type
type Error = (Ivl, String)

data Resolution
  = DataRes { resType :: Type }
  | TypeRes { resType, targType :: Type }
  | InstRes { resType :: Type }
  deriving (Eq, Ord)

data Results = Results
  { _source :: String 
  , _partial_source :: String
  , _errors :: [Error]
  , _subsets :: [([Resolution], Map Ivl Type)]
  }

data UserMode = Normal | Selection Bool

data State = State
  { _code :: String
  , _cursor :: (Int, Int)
  , _user :: UserMode
  , _parsed :: Maybe Results
  , _chan :: TaskChan
  , _mousePos :: (Double, Double)
  , _timeout :: HandlerId
  , _selfRef :: IORef (KeyTable, State)
  , _clips :: [(String, Maybe String)]
  , _autoEdits :: [([Edit], Results, String)]
  }

$(mkLabels [''Results, ''State])

sourceDir :: String
sourceDir = "source"

getSelection :: State -> String
getSelection s = substr (get cursor s) (get code s)

editCode :: [Edit] -> State -> State
editCode es s = modify code (applyEdits $ debug es)
              $ set cursor curs' s
 where
  curs = get cursor s
  curs' = mapT (offset+) curs
  offset = sum . map (\(ivl, xs) -> length xs - ivlWidth ivl) 
         $ takeWhile ((<= curs) . fst) $ debug es
