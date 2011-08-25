{-# LANGUAGE OverloadedStrings, ViewPatterns, NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, ViewPatterns, PatternGuards, NamedFieldPuns #-}

module Main where

import Smart
import Cache
import Converter
import Args
import Special
import Lang
import Result
import Html

import Snap.Types
import Snap.Http.Server (httpServe)
import Snap.Http.Server.Config
import Snap.Util.FileServe (getSafePath, serveDirectoryWith, simpleDirectoryConfig)

import Data.Digest.Pure.MD5 (md5)
import System.FastLogger

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString)
import qualified Data.ByteString.Lazy.UTF8 as Lazy

import System.FilePath ((</>), takeExtension, dropExtension)
import System.Directory (doesFileExist)
import Language.Haskell.Interpreter (liftIO)
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Applicative ((<|>))
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Maybe (listToMaybe)
import Prelude hiding (catch)

---------------------------------------------------------------

main :: IO ()
main = getArgs >>= mainWithArgs

mainWithArgs :: Args -> IO ()
mainWithArgs args@(Args {port, static, logdir, hoogledb, fileservedir, gendir, mainpage, restartpath, sourcedir}) = do 

    ch <- startGHCiServer [sourcedir] (logdir </> "interpreter") hoogledb
    cache <- newCache 10

    httpServe

        ( setPort port
        . setAccessLog (if null logdir then Nothing else Just (logdir </> "access.log"))
        . setErrorLog  (if null logdir then Nothing else Just (logdir </> "error.log"))
        $ emptyConfig
        )

        (   method GET
                (   serveDirectoryWith simpleDirectoryConfig fileservedir
                <|> serveHtml ch
                <|> ifTop (redirect $ fromString mainpage)
                <|> path (fromString restartpath) (liftIO $ restart ch >> clearCache cache)
                )
        <|> method POST (exerciseServer (cache, ch) args)
        <|> notFound
        )
  where
    serveHtml ch = do
        p <- getSafePath
        when (not static && takeExtension p `elem` [".xml"]) $ liftIO $
            convert ch args $ dropExtension p
        serveDirectoryWith simpleDirectoryConfig gendir

    notFound :: Snap ()
    notFound = do
        writeText "<html xmlns=\"http://www.w3.org/1999/xhtml\"><body>Page not found.</body></html>"
        getResponse >>= finishWith . setResponseCode 404






---------------------------------------------------------------

logNormalMsg :: TaskChan -> String -> IO ()
logNormalMsg ch x = do
    v <- timestampedLogEntry $ fromString x
    logMsg (logger ch) v 

getParam' :: ByteString -> Snap (Maybe T.Text)
getParam' = fmap (fmap $ decodeUtf8With lenientDecode) . getParam

type TaskChan' = (Cache (Int, T.Text), TaskChan)

exerciseServer :: TaskChan' -> Args -> Snap ()
exerciseServer (cache, ch) args@(Args {magicname, lang, exercisedir, verboseinterpreter}) = do
    params <- fmap show getParams
    when (length params > 3000) $ do
        writeText "<html xmlns=\"http://www.w3.org/1999/xhtml\"><body>Too long request.</body></html>"
        getResponse >>= finishWith . setResponseCode 400

    let md5Id = md5 $ Lazy.fromString params   -- could be more efficient
    liftIO $ logNormalMsg ch $ " eval " ++ show md5Id ++ " " ++ params
    j <- liftIO $ lookupCache cache md5Id
    (>>= writeText) $ case j of
      Left (delay, res) -> liftIO $ do
        logNormalMsg ch $ "   ch " ++ show md5Id
        threadDelay delay
        return res
      Right cacheAction -> do
        time <- liftIO $ getCurrentTime
        res <- fmap renderResults $ do
            Just [ss, fn_, x, y, T.unpack -> lang']  <- fmap sequence $ mapM getParam' ["c","f","x","y","lang"]

            let fn = exercisedir </> T.unpack fn_
            True <- liftIO $ doesFileExist fn
            Just task <- liftIO $ fmap (eval_ ss y . T.splitOn (T.pack delim)) $ T.readFile fn
            liftIO $ exerciseServer' ('X':magicname) ch verboseinterpreter fn x lang md5Id task

         <|> return [Error True $ translate lang "Inconsistency between server and client."]

        liftIO $ do
            time' <- getCurrentTime
            let delay = round $ 1000000 * (realToFrac $ diffUTCTime time' time :: Double) :: Int
            logNormalMsg ch $ "  end " ++ show md5Id ++ " " ++ show delay ++ " ms  " ++ T.unpack res
            cacheAction (delay, res)
            return res

  where
    eval_ "eval"  _ [_]
        = Just Eval
    eval_ "eval"  _ [_, goodsol]
        = Just $ Compare magicname $ T.unpack $ T.drop (length magicname + 4) $ goodsol
    eval_ comm
      (T.unpack -> s) 
      [env, hidden, re -> Just (is :: [([String],String)]), T.unpack -> j, T.unpack -> i, re -> Just funnames] 
        = Just $ case comm of 
            "eval2" -> Compare2 env funnames s
            "check" -> Check env funnames is i j
    eval_ _ _ _
        = Nothing

maybeRead :: Read a => String -> Maybe a
maybeRead s = listToMaybe [a | (a,"") <- reads s] 

re :: Read b => T.Text -> Maybe b
re = maybeRead . T.unpack


