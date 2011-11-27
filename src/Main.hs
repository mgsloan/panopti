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

import Control.Arrow ((***), (&&&), first, second)
import Control.Category ((.))
import Control.Monad (liftM, join)
import Data.Curve hiding ((<.>))
import Data.Curve.Util (mapT)
import Data.IORef
import Data.Label
import qualified Graphics.Rendering.Cairo as C
import Graphics.ToyFramework
import System.Directory (createDirectoryIfMissing)
import qualified System.Glib.MainLoop as G

import Diagrams.Backend.Cairo
import Diagrams.TwoD
import Graphics.Rendering.Diagrams
import qualified Language.Haskell.Exts.Annotated as A

{- Things to work on

 * Make application structure manipulation system do the following:
   - Insert / remove parens when necessary / appropriate
   - Manipulate pointfree expressions / $ style
 * User input of parameter-example table
 * Depiction of concrete value flow
 * Handle full source files
 * Live update of running haskell program
 * Detection of untenable type classes
 * Contextual Hoogle / paste buffers
 * Illustration of which types have changed with edits
 * Mode for type information relevant to selection
 * Visualization of parallel possibilities for expression tree
   - Depiction of seminal style autofix possibilities
   - Depiction of unmerged collaborative edits
 * Application of visual pretty printing / pretty editing
 -}


{-
The theory can be described as: The typing environment which makes a
monomorphically-punned equivalent compile gives all of the information about the
polymorphic expression.

here's the procedure:

 1) Set the whole top-level declaration to 'undefined', in order to get the
 type, in the case that an explicit type is not specified.

 2) Whereify, and use recursive application of the seminal removal scheme

 3) Rename all of the created definitions, and replace the old names with
 explicitly typed dummy-undefineds, which assign a unique constructor to each
 potentially unique polymorphic variable.

 3) Use the type errors to incrementally guide creating a dummy typing
 environment.  The construction of this environment determines the causation of
 types within the subexpression being analyzed.
-}


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

  sup <- C.liftIO supplyStart
  C.liftIO . fst 
      . renderDia Cairo (CairoOptions "" (GTK dw Nothing False)) 
      . scale 10
      . typeDiagram sup coloredShapes (const $ const Nothing) 
      . mustOk 
      $ parseType "(a, b)"

{-
  C.liftIO . fst 
      . renderDia Cairo (CairoOptions "" (GTK dw Nothing False)) 
      . scale 10
      $ stateDiagram s
 -}

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

{-
getApps :: ExpS -> Apps
getApps = uncurry (++) . processExp
 where
  processExp :: ExpS -> (Apps, Apps)
  processExp (A.InfixApp s l o r) = case o of
    (A.QVarOp _ (A.UnQual _ (A.Symbol _ "."))) -> doApp s l [r]
    (A.QVarOp _ (A.UnQual _ (A.Symbol _ "$"))) -> doApp s l [r]
    _                                          -> doApp s (convertOp o) [l, r]
  processExp (A.LeftSection s l o)  = doApp s (convertOp o) [l]
  processExp (A.RightSection s o r) = doApp s (convertOp o) [r] --TODO: uhhh
  processExp (A.App s l r)          = doApp s l [r]
  processExp e = foldr1 (zipT (++))
               $ gmapQ (const ([], []) `extQ`
                 (first (map $ second3 $ const (getSpan' e)) . processExp)) e
  doApp :: A.SrcSpanInfo -> ExpS -> [ExpS] -> (Apps, Apps)
  doApp s x xs = ( [(getSpan' x, colSpan $ A.srcInfoSpan s, map getSpan' xs)]
                 , concatMap getApps (x:xs) )
  convertOp :: A.QOp A.SrcSpanInfo -> ExpS
  convertOp (A.QVarOp s' qn) = A.Var s' qn
  convertOp (A.QConOp s' qn) = A.Con s' qn

-- Takes a list of apps, and merges by left-hand-span, and therefore app lhs.
concatApps :: Apps -> Apps
concatApps = map process . groupSortOn (\((x, _), _, _) -> x) . reverse
  where
   process xs = (h, l, reverse . concat $ map thd3 xs)
    where
     ((h,_,_),(_,l,_)) = (head &&& last) $ sortBy (comparing (ivlWidth . snd3)) xs
-}
