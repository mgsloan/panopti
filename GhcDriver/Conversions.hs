-- Based on Hint, written by Daniel Goren

module GhcDriver.Conversions( showGhcType, showGhcKind, showModuleName ) where

import qualified GhcDriver.GHC as GHC
import qualified GhcDriver.Compat as Compat

-- --------- Types / Kinds -----------------------
showGhcType t = do
  -- Unqualify necessary types
  -- (i.e., do not expose internals)
  unqual <- GHC.getPrintUnqual
  return $ GHC.showSDocForUser unqual (Compat.pprType t)

showGhcKind k = GHC.showSDoc $ Compat.pprKind k

showModuleName = GHC.moduleNameString . GHC.moduleName

{-
isSucceeded :: GHC.SuccessFlag -> Bool
isSucceeded GHC.Succeeded = True
isSucceeded GHC.Failed    = False
-}
