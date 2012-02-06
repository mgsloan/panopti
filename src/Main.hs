{-# LANGUAGE TemplateHaskell
           , StandaloneDeriving #-}

import Annotations
import ExprDiagrams
import TypeInfo     (whereify, getSubsets, subsetTypes)
import PartialParse (partialParse)
import State
import Utils (parseIt)

import ActiveHs.Simple
import qualified Language.Haskell.Interpreter as Ghci

import Prelude hiding ((.))
import Control.Category ((.))

import Control.Arrow (second)
import Data.Data
import Data.Label
import Graphics.UI.Gtk.Toy.Prelude

main :: IO ()
main = do
  ch <- startGHCiServer ["."] print print
  runToy $ State ch cursorText mempty

instance Interactive State where
  keyboard k _ s | fst k == True = update $ modify code (textKeyHandler k) s
                 | otherwise = return s

instance GtkInteractive State where
  display = displayDiagram toDiagram

instance Diagrammable State Cairo R2 where
  toDiagram (State _ c r)
    = scaleY (-1) . (strutX 50 |||) . (strutY 58 |||)
    $ (alignT $ plainText "> " ||| drawCode c)
        ===
      alignL r

plainText :: String -> CairoDiagram
plainText = monoText . (`MarkedText` ([] :: [(Ivl, CursorMark)]))

-- Needed in order to be able to provide a witness for CairoDiagram.
deriving instance Typeable Any

{-
update s = do
  val <- interpret (get chan s) "MyPrelude"
       $ Ghci.interpret (get (mText . code) s) (mempty :: CairoDiagram)
  return $ set response (either (plainText . show) id val) s
-}

-- TODO: set response diagram

update s = do
  let (e, prs) = partialParse parseIt $ get (mText . code) s
      prms = map (second $ (`Version` 0) . ParseResA) prs
      remPrms = filterMarks $ not . isParseResA . get versionValue . snd

  f <- case e of
    Nothing -> return $ addMarks prms . remPrms
    Just e' -> do
      let apps = whereify e'
          tchan = get chan s

      subs <- getSubsets apps tchan

      types <- mapM (subsetTypes tchan) subs
    
      let anns = prms ++ map (second (`Version` 0))
               ( astAnns e'
              ++ appAnns (snd apps)
              ++ subsetsAnns types
               )
       
      return $ addMarks anns . filterMarks (isCursor . snd)

  return $ modify code (debugMarks "cur" . f) s

{-
setTimeout :: State -> IO State
-setTimeout s = do
-  case get timeout s of
-    0 -> return ()
-    x -> G.timeoutRemove x
-  time <- G.timeoutAdd (handler $ get selfRef s) updateTime
-  setM timeout time s
- where
-  handler ref = do
-    (km, st) <- readIORef ref
-    st' <- update st
-    writeIORef ref (km, st')
-    return False
-}