{-# LANGUAGE DeriveDataTypeable, NamedFieldPuns #-}
module Args where

import Paths_activehs

import System.Directory (createDirectoryIfMissing)
import System.Console.CmdArgs.Implicit
import System.FilePath

------------------

data Args
    = Args
        { sourcedir     :: String
        , gendir        :: String
        , exercisedir   :: String
        , templatedir   :: String
        , fileservedir  :: String
        , logdir        :: String

        , hoogledb      :: String

        , mainpage      :: String
        , restartpath   :: String

        , port          :: Int
        , lang          :: String
        , static        :: Bool
        , verbose       :: Bool
        , verboseinterpreter :: Bool
        , recompilecmd  :: String
        , magicname     :: String
        }
        deriving (Show, Data, Typeable)

myargs :: Args
myargs = Args
        { sourcedir     = "."     &= typDir         &= help "Directory of lhs files to serve. Default is '.'"
        , gendir        = "html"  &= typDir         &= help "Directory to put generated content to serve. Default is 'html'"
        , exercisedir   = "exercise" &= typDir      &= help "Directory to put generated exercises to serve. Default is 'exercise'"
        , templatedir   = ""      &= typDir         &= help "Directory of html template files for pandoc. Default points to the distribution's directory."
        , fileservedir  = ""      &= typDir         &= help "Files in this directory will be served as they are (for css and javascript files). Default points to the distribution's directory."
        , logdir        = "log"   &= typDir         &= help "Directory to put log files in. Default is 'log'."

        , hoogledb      = ""      &= typFile        &= help "Hoogle database file"

        , mainpage      = "Index.xml" &= typ "PATH" &= help "Main web page path"
        , restartpath   = ""      &= typ "PATH"     &= help "Opening this path in browser restarts the ghci server."

        , lang          = "en"    &= typ "LANGUAGE" &= help "Default language. It is 'en' by default."
        , port          = 8000    &= typ "PORT"     &= help "Port to listen"
        , static        = False                     &= help "Do not regenerate pages."
        , verbose       = False                     &= help "Verbose activehs output"
        , verboseinterpreter = False                &= help "Verbose interpreter output in the browser"
        , recompilecmd  = "ghc -O" &= typ "COMMAND" &= help "Command to run before page generation. Default is 'ghc -O'."
        , magicname    = "a9xYf"  &= typ "VARNAME"  &= help "Magic variable name."
        }  &= summary "activehs 0.2, (C) 2010-2011 Péter Diviánszky"
           &= program "activehs"

completeArgs :: Args -> IO Args
completeArgs args
--    | gendir args == "" = completeArgs (args {gendir = root args </> "html"})
--    | exercisedir args == "" = completeArgs (args {exercisedir = root args </> "exercise"})
    | templatedir args == "" = do
        dir <- getDataDir
        completeArgs (args {templatedir = dir </> "template"})
    | fileservedir args == "" = do
        dir <- getDataDir
        completeArgs (args {fileservedir = dir </> "copy"})
completeArgs args = return args


createDirs :: Args -> IO ()
createDirs (Args {sourcedir, gendir, exercisedir, logdir}) 
    = mapM_ f [sourcedir, gendir, exercisedir, logdir]
 where
    f "" = return ()
    f path = createDirectoryIfMissing True path


getArgs :: IO Args
getArgs = do
    args <- cmdArgs myargs >>= completeArgs
    createDirs args
    return args



