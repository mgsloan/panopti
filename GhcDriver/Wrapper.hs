-- Based on code from Hint, written by Daniel Goren
--                and GhcMod, written by Kazu Yamamoto

module GhcDriver.Wrapper where

import qualified GhcDriver.Compat as Compat
import GhcDriver.Conversions

-- GHC imports
import CoreMonad
import ErrUtils
import FastString
import GHC
import GHC.Paths (libdir)
import Outputable (text)
import SrcLoc

import Control.Applicative
import Control.Monad       (liftM)
import Data.IORef
import Data.Maybe (fromJust)
import Language.Haskell.Syntax ( HsQualType )

data Options = Options
  { search_paths :: [String] }

type ErrorInfo = (Severity, SrcSpan, Message)
type LogReader = IO [ErrorInfo]

initGhc :: Options -> Ghc LogReader
initGhc opts = do
    dflags <- getSessionDynFlags
    loggerAcc <- liftIO (newIORef []) :: Ghc (IORef [ErrorInfo])
    setSessionDynFlags $ dflags
      { log_action = appendLog loggerAcc
      , ghcLink = LinkInMemory
      , hscTarget = HscInterpreted
      , importPaths = search_paths opts
      }
    return (reverse <$> liftIO (readIORef loggerAcc))
  where
    appendLog ref sev src _ msg = modifyIORef ref ((sev, src, msg):)

--  consider commandline opts
--    let opts = map noLoc cmdOpts
--    (dflags',_,_) <- parseDynamicFlags dflags opts

execGhc :: Options -> Ghc a -> IO a
execGhc _ = runGhc (Just libdir)

setGhcModules :: Options -> [String] -> Ghc Bool
setGhcModules _ xs = mapM (`guessTarget` Nothing) xs >>= setTargets
                  >> load LoadAllTargets >>= return . succeeded
{-
setGhcModules xs = do
  reset
  loadModules xs
  setTopLevelModules xs
-}

makeSimpleError :: String -> ErrorInfo
makeSimpleError e =
  ( SevFatal
  , span
  , errMsgShortDoc $ mkPlainErrMsg span (text $ "GHCi server died:" ++ e))
 where
  span = mkGeneralSrcSpan (fsLit "")

typeOf :: String -> Ghc String
typeOf xs = do
  --failOnParseError parseExpr xs
  ty <- Compat.exprType xs
  showGhcType $ fromJust ty
