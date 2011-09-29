{-# LANGUAGE CPP, RankNTypes, ExistentialQuantification, ScopedTypeVariables,
    PatternGuards, FlexibleContexts #-}

-- Based on "Simple" from ActiveHs, written by Péter Diviánszky

module GhcDriver.Server
    ( Task (..), TaskChan
    , startGHCiServer
    , restartGHCiServer
    , interpret
    ) where

import GhcDriver.Wrapper
-- import GhcDriver.Context

-- GHC
import CoreMonad
import ErrUtils
import GHC


--import GhcMod.Types (Options(..))

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar, tryPutMVar)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception (SomeException, catch)
import Control.Monad (when, forever)
import Control.Monad.Error (MonadError, catchError)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (isPrefixOf)
import Data.Maybe (isJust)
import Exception (ghandle)
import Prelude hiding (catch)

data Task 
    = forall a. Task FilePath String (MVar (Maybe a, [ErrorInfo])) (Ghc (Maybe a))

newtype TaskChan 
    = TaskChan (Chan (Maybe Task))

type ErrorRef
    = IORef (ErrorInfo -> IO ())

startGHCiServer :: Options
                -> (String -> IO ()) -> (String -> IO ()) -> IO TaskChan
startGHCiServer options logError logMsg = do
  ch <- newChan
  ref <- newIORef (const $ return ())

  _ <- forkIO $ forever $ do
         logMsg "start interpreter"
         (execGhc options $ do 
           logger <- initGhc options
           handleTask ref ch logger Nothing)
          `catch` \(e :: SomeException) -> liftIO $
            -- If we don't do this, then the MVar will remain stuck in 'take'
            do doPut <- readIORef ref
               doPut $ makeSimpleError (show e)
  return $ TaskChan ch
 where
  handleTask :: ErrorRef -> Chan (Maybe Task) -> LogReader -> Maybe FilePath 
             -> Ghc ()
  handleTask ref ch rdr oldFn = do
      task <- liftIO $ readChan ch
      case task of
          Just task@(Task _ _ repVar _) -> do
            liftIO $ writeIORef ref $ \x -> tryPutMVar repVar (Nothing, [x]) >> return ()
            handleIt task
          Nothing ->
            liftIO $ logError "interpreter stopped intentionally"
   where
    handleIt (Task fn mod repVar m) = do
      (cont, res) <- ghandle handleErr (setup >> m >>= yield)
      liftIO $ putMVar repVar res
      when cont $ handleTask ref ch rdr
                -- Only omit reloading if operation actually worked.
                $ if isJust . fst $ res
                  then Just fn
                  else Nothing
     where
      handleErr er = yieldResult Nothing [makeSimpleError (showGhcException er "")]
      setup = if oldFn /= Just fn
                then setGhcModules options [fn]
                else return True
      yield x = do
        errs <- liftIO $ rdr
        yieldResult x errs
      yieldResult :: Maybe a -> [ErrorInfo] -> Ghc (Bool, (Maybe a, [ErrorInfo]))
      yieldResult x errs = return (True, (x, errs))

mif m t e = m >>= \ok -> if ok then t else e

restartGHCiServer :: TaskChan -> IO ()
restartGHCiServer (TaskChan ch) = writeChan ch Nothing

interpret :: TaskChan -> FilePath -> Ghc (Maybe a) -> IO (Maybe a, [ErrorInfo])
interpret (TaskChan ch) fn m = do
    rep <- newEmptyMVar
    writeChan ch $ Just $ Task fn "" rep m
    takeMVar rep

{-
-- Modified from Kazu Yamamoto's GhcMod: Info.hs
setModuleContext :: Options -> FilePath -> ModuleString -> Ghc a -> IO (Maybe a)
setModuleContext opt fileName modstr action = withGHC valid
  where
    valid = do
        (file,_) <- initializeGHC opt fileName ["-w"] False
        setTargetFile file
        load LoadAllTargets
        mif setContextFromTarget (liftM Just action) invalid
    invalid = do
        initializeGHC opt fileName ["-w"] False
        setTargetBuffer
        load LoadAllTargets
        mif setContextFromTarget (liftM Just action) (return Nothing)
    setTargetBuffer = do
        modgraph <- depanal [mkModuleName modstr] True
        let imports = concatMap (map (showSDoc . ppr . unLoc)) $
                      map ms_imps modgraph ++ map ms_srcimps modgraph
            moddef = "module " ++ sanitize modstr ++ " where"
            header = moddef : imports
#if __GLASGOW_HASKELL__ >= 702
            importsBuf = stringToStringBuffer . unlines $ header
#else
        importsBuf <- liftIO . stringToStringBuffer . unlines $ header
#endif
        clkTime <- liftIO getClockTime
        setTargets [Target (TargetModule $ mkModuleName modstr) True (Just (importsBuf, clkTime))]
    mif m t e = m >>= \ok -> if ok then t else e
    sanitize = fromMaybe "SomeModule" . listToMaybe . words
-}
