{-# LANGUAGE DoAndIfThenElse, ScopedTypeVariables, TemplateHaskell,
TupleSections, ParallelListComp #-}

import Brace
import Diagrams
import ErrorParser
import Input
import PartialParse
import Simple
import State
import TypeInfo
import Utils

import Prelude hiding ((.))

import Control.Category ((.))
import Data.IORef
import Data.Label
import Diagrams.Backend.Cairo
import Diagrams.TwoD
import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Diagrams
import Graphics.ToyFramework
import qualified Language.Haskell.Exts.Annotated as A
import System.Directory (createDirectoryIfMissing)
import qualified System.Glib.MainLoop as G

-- TODO: configureable static imports
main :: IO ()
main = do
  createDirectoryIfMissing False sourceDir
  chan <- startGHCiServer [sourceDir] print print
  (stateRef, loop) <- runToyState $ Toy
    { initialState =
        State "map (+1) . map (*2) $ (filter isSpace $ tail \"hello world\")"
              (0, 0) Normal Nothing chan (0, 220) 0 undefined [] []
    , mouse   = const $ setM mousePos
    , key     = handleKey' setTimeout
    , display = handleDisplay
    , tick    = const return
    }
  modifyIORefM stateRef (secondM (setTimeout . set selfRef stateRef))
  loop

handleDisplay :: DrawWindow -> IPnt -> IRect -> State -> C.Render State
handleDisplay dw _ _ s@(State txt ix user p c mouse _ _ _ _) = do
  let textPos = (50.5, 200.5)

  C.liftIO $ fst
           . renderDia Cairo (CairoOptions "" (GTK dw Nothing False))
           . scale 10
           =<< stateDiagram s

  return s

updateTime :: Int
updateTime = 200

setTimeout :: State -> IO State
setTimeout s = do
  case get timeout s of
    0 -> return ()
    x -> G.timeoutRemove x
  time <- G.timeoutAdd (handler $ get selfRef s) updateTime
  setM timeout time s
 where
  handler ref = do
    (km, st) <- readIORef ref
    st' <- update st
    writeIORef ref (km, st')
    return False
