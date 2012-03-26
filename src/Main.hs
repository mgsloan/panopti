{-# LANGUAGE TemplateHaskell
           , StandaloneDeriving #-}

import Annotations
import TypeDiagrams
import TypeInfo     (whereify, getSubsets, subsetTypes)
import PartialParse (partialParse)
import State
import Utils (parseIt, setM, modifyIORefM)

import ActiveHs.Simple
import qualified Language.Haskell.Interpreter as Ghci

import Prelude hiding ((.))
import Control.Category ((.))

import Control.Arrow (second)
import Data.Data
import Data.IORef
import Data.Label
import qualified Graphics.UI.Gtk.General.General as G
import Graphics.UI.Gtk.Toy.Prelude

main :: IO ()
main = do
  ch <- startGHCiServer ["."] print print
  let state = State ch (addText cursorText $ plainText "map (+1) [1..10]./") mempty 0
  runToy =<< newIORef state

modifyIORefM' f sr = do
  s <- readIORef sr
  r <- f s
  writeIORef sr r
  return sr

instance Interactive (IORef State) where
  keyboard k _ s
    | fst k == True = setTimeout =<< modifyIORefM' (return . modify code (textKeyHandler k)) s
    | otherwise = return s

instance GtkInteractive (IORef State) where
  display dw i = modifyIORefM' $ displayDiagram toDiagram dw i

instance Diagrammable State Cairo R2 where
  toDiagram (State _ c r _)
    = scaleY (-1) . (strutX 50 |||) . (strutY 58 |||)
    $ (alignT $ textString "> " ||| drawCode c)
        ===
      alignL r

textString :: String -> CairoDiagram
textString = drawText (StyleState monoStyle)
           . (plainText :: String -> MarkedText CursorMark)

-- Needed in order to be able to provide a witness for CairoDiagram.
deriving instance Typeable Any

-- TODO: set response diagram

update s = do
  case e of
    Nothing -> return $ modify code (addMarks prms . remPrms) s
    Just e' -> do
      let apps = whereify e'
          tchan = get chan s

      subs <- getSubsets apps tchan

      types <- mapM (subsetTypes tchan) subs

      let anns = prms ++ map (second $ Version 0)
               ( astAnns e'
              -- ++ appAnns (snd apps)
              ++ subsetsAnns types
               )

      val <- interpret (get chan s) "MyMain"
           $ Ghci.interpret (get (mText . code) s) (mempty :: CairoDiagram)

      return . set result (either (textString . show) id val)
             $ modify code (addMarks anns . filterMarks (isCursor . snd)) s
 where
  (e, prs) = partialParse parseIt $ get (mText . code) s
  prms = map (second $ Version 0 . ParseResA) prs
  remPrms = filterMarks $ not . isParseResA . snd

timeoutMs = 200

setTimeout :: IORef State -> IO (IORef State)
setTimeout sr = do
  s <- readIORef sr
  case get timer s of
    0 -> return ()
    x -> G.timeoutRemove x
  time <- G.timeoutAdd (handler sr) timeoutMs
  modifyIORefM' (setM timer time) sr
 where
  handler ref = do
    st <- readIORef ref
    st' <- update st
    writeIORef ref st'
    return False
