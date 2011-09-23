{-# LANGUAGE RankNTypes, ExistentialQuantification, ScopedTypeVariables, PatternGuards, FlexibleContexts #-}

module Simple
    ( Task (..), TaskChan
    , startGHCiServer
    , restartGHCiServer
    , interpret
    , catchError_fixed
    ) where

import Language.Haskell.Interpreter hiding (interpret)

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar, tryPutMVar)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception (SomeException, catch)
import Control.Monad (when, forever)
import Control.Monad.Error (MonadError, catchError)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (isPrefixOf)
import Prelude hiding (catch)

-------------------------

data Task 
    = forall a. Task FilePath (MVar (Either InterpreterError a)) (Interpreter a)

newtype TaskChan 
    = TC (Chan (Maybe Task))

---------------

startGHCiServer :: [String] -> (String -> IO ()) -> (String -> IO ()) -> IO TaskChan
startGHCiServer paths{-searchpaths-} logError logMsg = do
    ch <- newChan 
    ref <- newIORef (const $ return ())

    _ <- forkIO $ forever $ do
        logMsg "start interpreter"
        e <- runInterpreter (handleTask ref ch Nothing)
              `catch` \(e :: SomeException) -> do
                let err = UnknownError ("GHCi server died:" ++ show e)
                doPut <- readIORef ref
                doPut err
                return $ Left err
        case e of
            Left  e  -> logError $ "stop interpreter: " ++ show e
            Right _  -> return ()

    return $ TC ch

  where
    handleTask :: IORef (InterpreterError -> IO ())
               -> Chan (Maybe Task) -> Maybe FilePath -> Interpreter ()
    handleTask ref ch oldFn = do
        task <- lift $ readChan ch
        case task of
            Just task@(Task fn repVar m) -> do
              lift $ writeIORef ref (\x -> tryPutMVar repVar (Left x) >> return ())
              handleTask_ ref ch oldFn task
            Nothing ->
              liftIO $ logError "interpreter stopped intentionally"

    handleTask_ ref ch oldFn (Task fn repVar m) = do
        (cont, res) <- do  
            when (oldFn /= Just fn) $ do
                reset
                set [searchPath := paths]
                loadModules [fn]
                setTopLevelModules [fn]

            x <- m
            return (True, Right x)

          `catchError_fixed` \er ->
            return (not $ fatal er, Left er)

        lift $ putMVar repVar res
        when cont $ handleTask ref ch $ case res of
            Right _ -> Just fn
            Left  _ -> Nothing


restartGHCiServer :: TaskChan -> IO ()
restartGHCiServer (TC ch) = writeChan ch Nothing

interpret :: TaskChan -> FilePath -> Interpreter a -> IO (Either InterpreterError a)
interpret (TC ch) fn m = do
    rep <- newEmptyMVar
    writeChan ch $ Just $ Task fn rep m
    takeMVar rep




fatal :: InterpreterError -> Bool
fatal (WontCompile _) = False
fatal (NotAllowed _)  = False
fatal _ = True

catchError_fixed 
    :: MonadError InterpreterError m 
    => m a -> (InterpreterError -> m a) -> m a
m `catchError_fixed` f = m `catchError` (f . fixError)
  where
    fixError (UnknownError s) 
        | Just x <- dropPrefix "GHC returned a result but said: [GhcError {errMsg =" s
        = WontCompile [GhcError {errMsg = case reads x of ((y,_):_) -> y; _ -> s}]
    fixError x = x

dropPrefix :: Eq a => [a] -> [a] -> Maybe [a]
dropPrefix s m
    | s `isPrefixOf` m = Just $ drop (length s) m
    | otherwise = Nothing



