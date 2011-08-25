{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, ViewPatterns, PatternGuards, NamedFieldPuns #-}

module Special
    ( SpecialTask (..), exerciseServer'
    ) where

import Smart
import QuickCheck
import Result
import Lang
import Html
import Qualify (qualify)

import ActiveHs.Base (WrapData2)

import Language.Haskell.Interpreter hiding (eval)
import Data.Digest.Pure.MD5 (MD5Digest)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.ByteString.UTF8 (fromString)

import Control.DeepSeq
import Control.Concurrent.MVar
import Control.Exception
import System.FilePath ((</>),takeFileName)
import System.Directory (getTemporaryDirectory)

import Control.Concurrent (threadDelay, forkIO, killThread)
import Prelude hiding (catch)


---------------------------------------------------------------

timeout :: forall b. Int -> IO b -> IO b -> IO b
timeout delay error action = do
    v <- newEmptyMVar 
    t1 <- forkIO $ threadDelay delay >> error >>= putMVar v
    t2 <- forkIO $ action >>= putMVar v
    x <- takeMVar v
    killThread t1
    killThread t2
    return x

----------------------------

data SpecialTask
    = Eval
    | Compare String String
    | Compare2 T.Text [String] String
    | Check T.Text [String] [([String],String)] String String

exerciseServer' 
    :: String
    -> TaskChan
    -> Bool
    -> FilePath
    -> T.Text
    -> Language
    -> MD5Digest
    -> SpecialTask
    -> IO [Result]

exerciseServer' qualifier ch verbose fn sol lang m5 task = do

    let error = do
            logMsgWithImportance 5 (logger ch) $ fromString $ "Server error:" ++ show m5
            return [Error True "Server error."]

        action =
                 do res <- eval task
                    res `deepseq` return res
           `catch` \(e :: SomeException) ->             -- ???
                  return [Error True $ show e]

    timeout (10*1000000) error action

  where

    eval Eval
        = interp verbose m5 lang ch fn (T.unpack sol) $ \a -> return $ return []

    eval (Compare hiddenname goodsol)
        = interp verbose m5 lang ch fn (T.unpack sol) $ \a ->
            do  x <- interpret (wrap2 a hiddenname) (as :: WrapData2)
                return $ compareMistGen lang (show m5) x $ goodsol

    eval (Compare2 env funnames s) = do
        fn' <- tmpSaveHs (show m5) $ env `T.append` sol
        case qualify qualifier funnames s of
            Left err -> return [Error True err]
            Right s2 -> interp verbose m5 lang ch fn' s $ \a ->
                fmap (compareClearGen lang (show m5)) $ interpret (wrap2 a s2) (as :: WrapData2)

    eval (Check env funnames is i j) = do
        fn' <- tmpSaveHs (show m5) $ env `T.append` sol
        ss <- quickCheck qualifier m5 lang ch fn' (T.unpack sol) funnames is
        let ht = head $ [x | ModifyCommandLine x <- ss] ++ [""]
        return [ShowInterpreter lang 59 (getTwo "eval2" (takeFileName fn) j i j) j 'E' ht ss]

tmpSaveHs :: String -> T.Text -> IO FilePath
tmpSaveHs x s = do
    tmpdir <- getTemporaryDirectory
    let tmp = tmpdir </> "GHCiServer_" ++ x ++ ".hs"
    T.writeFile tmp s
    return tmp




