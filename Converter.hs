{-# LANGUAGE PatternGuards, ViewPatterns, NamedFieldPuns #-}

module Converter
    ( convert
    ) where

import Parse

import Smart (TaskChan, restart, mkId, interp)
import Result (hasError)
import Html
import Lang
import Args

import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Syntax

import Text.XHtml.Strict hiding (lang)
import Text.Pandoc

import System.Process (readProcessWithExitCode)
import System.Cmd
import System.FilePath
import System.Exit
import System.Directory (getTemporaryDirectory, getModificationTime, doesFileExist, getTemporaryDirectory, createDirectoryIfMissing)
import System.Time (diffClockTimes, noTimeDiff) 

import Control.Monad
import Data.List

----------------------------------

convert :: TaskChan -> Args -> String -> IO ()
convert ghci args@(Args {magicname, sourcedir, gendir, recompilecmd, verbose}) what = do
    whenOutOfDate () output input $ do
        whenOutOfDate () object input $ do
            when verbose $ putStrLn $ object ++ " is out of date, regenerating"
--            x <- system $ recompilecmd ++ " " ++ input
            let (ghc:args) = words recompilecmd -- !!!
            (x, out, err) <- readProcessWithExitCode ghc (args ++ [input]) ""
            if x == ExitSuccess 
                then do
                    restart ghci
                    return ()
                else fail $ unlines [unwords [recompilecmd, input], show x, out, err]
        when verbose $ putStrLn $ output ++ " is out of date, regenerating"
        mainParse False input  >>= extract verbose ghci args what
    whenOutOfDate () output input2 $ do
        when verbose $ putStrLn $ output ++ " is out of date, regenerating"
        mainParse True input2 >>= extract verbose ghci args what
--        return True
 where
    input  = sourcedir  </> what <.> "lhs"
    input2 = sourcedir  </> what <.> "lagda"
    output = gendir     </> what <.> "xml"
    object = sourcedir  </> what <.> "o"


extract :: Bool -> TaskChan -> Args -> Language -> Doc -> IO ()
extract verbose ghci (Args {lang, templatedir, sourcedir, exercisedir, gendir, magicname}) what (Doc meta modu ss) = do

    writeEx (what <.> "hs") [showEnv $ importsHiding []]
    ss' <- zipWithM processBlock [1..] $ preprocessForSlides ss
    ht <- readFile' $ templatedir </> lang' ++ ".template"

    writeFile' (gendir </> what <.> "xml") $ flip writeHtmlString (Pandoc meta $ concat ss')
      $ defaultWriterOptions
        { writerStandalone      = True
        , writerTableOfContents = True
        , writerSectionDivs     = True
        , writerTemplate        = ht
        }

 where
    lang' = case span (/= '_') . reverse $ what of
        (l, "")                -> lang
        (l, _) | length l > 2  -> lang
        (x, _)                 -> reverse x

    writeEx f l =
        writeFile' (exercisedir </> f) $ intercalate delim l

    writeFile' f s = do
        when verbose $ putStrLn $ f ++ " is written."
        createDirectoryIfMissing True (dropFileName f)
        writeFile f s

    readFile' f = do
        when verbose $ putStrLn $ f ++ " is to read..."
        readFile f

    system' s = do
        when verbose $ putStrLn $ "executing " ++ s
        system s

    importsHiding funnames
        = prettyPrint $ 
            Module loc (ModuleName "Main") directives Nothing Nothing
              ([mkImport modname funnames, mkImport_ ('X':magicname) modname] ++ imps) []
     where
        (Module loc (ModuleName modname) directives _ _ imps _) = modu

    mkCodeBlock l =
        [ CodeBlock ("", ["haskell"], []) $ intercalate "\n" l | not $ null l ]

----------------------------

    processBlock :: Int -> BBlock -> IO [Block]

    processBlock _ (Exercise visihidden _ _ funnames is)
        | null funnames || null is
        = return $ mkCodeBlock $ visihidden

    processBlock _ (Exercise _ visi hidden funnames is) = do
        let i = show $ mkId $ unlines $ map printName funnames
            j = "_j" ++ i
            fn = what ++ "_" ++ i <.> "hs"
            (static_, inForm, rows) = if null hidden
                then ([], visi, length visi) 
                else (visi, [], 2 + length hidden)

        writeEx fn  [ showEnv $ importsHiding funnames ++ "\n" ++ unlines static_
                    , unlines $ hidden, show $ map parseQuickCheck is, j, i
                    , show $ map printName funnames ]
        return
            $  mkCodeBlock static_
            ++ showBlockSimple lang' fn i rows (intercalate "\n" inForm)

    processBlock ii (OneLineExercise 'H' erroneous exp) 
        = return []
    processBlock ii (OneLineExercise p erroneous exp) = do
        let m5 = mkId $ show ii ++ exp
            i = show m5
            fn = what ++ (if p == 'R' then "_" ++ i else "") <.> "hs"
            act = getOne "eval" fn i i

        when (p == 'R') $ writeEx fn [showEnv $ importsHiding [], "\n" ++ magicname ++ " = " ++ exp]
        (b, exp') <- if p == 'F' && all (==' ') exp 
                    then return (True, [])
                    else do
                        when verbose $ putStrLn $ "interpreting  " ++ exp
                        r <- interp False m5 lang' ghci (exercisedir </> fn) exp $ \a -> return $ return []

                        return $ (not $ hasError r, take 1 r)

        when (erroneous /= b) 
            $ error $ translate lang' "Erroneous evaluation"  ++ ": " ++ exp ++ " ; " ++ showHtmlFragment (renderResults_ exp')

        return [rawHtml $ showHtmlFragment $ showInterpreter lang' 60 act i p exp exp']

    processBlock _ (Text (CodeBlock ("",[t],[]) l)) 
        | t `elem` ["dot","neato","twopi","circo","fdp","dfdp","latex"] = do
            tmpdir <- getTemporaryDirectory
            let i = show $ mkId $ t ++ l
                fn = what ++ i
                imgname = takeFileName fn <.> "png"
                outfile = gendir </> fn <.> "png"
                tmpfile = tmpdir </> takeFileName fn <.> if t=="latex" then "tex" else t

            writeFile' tmpfile $ unlines $ case t of
                "latex" -> 
                    [ "\\documentclass{article}"
                    , "\\usepackage{ucs}"
                    , "\\usepackage[utf8x]{inputenc}"
                    , "\\usepackage{amsmath}"
                    , "\\pagestyle{empty}"
                    -- , "\\newcommand{\\cfrac}[2]{\\displaystyle\\frac{#1}{#2}}"
                    , "\\begin{document}"
                    , "$$", l, "$$"
                    , "\\end{document}" ]
                _ ->
                    ["digraph G {", l, "}"]

            createDirectoryIfMissing True (dropFileName outfile)

            x <- system' $ unwords $ case t of
                "latex" ->  [ "(", "cd", dropFileName tmpfile, "&&"
                            , "latex -halt-on-error", takeFileName tmpfile, "2>&1 >/dev/null", ")"
                            , "&&", "(", "dvipng -D 150 -T tight", "-o", outfile
                            , replaceExtension tmpfile "dvi", "2>&1 >/dev/null",")"]
                _       ->  [ t, "-Tpng", "-o", outfile, tmpfile, "2>&1 >/dev/null" ]

            if x == ExitSuccess 
                then return [Para [Image [Str imgname] (imgname, "")]]
                else fail $ "processDot " ++ tmpfile ++ "; " ++ show x

    processBlock _ (Text l)
        = return [l]


---------------------------------

preprocessForSlides :: [BBlock] -> [BBlock]
preprocessForSlides x = case span (not . isLim) x of
    (a, []) -> a
    (a, b) -> a ++ case span (not . isHeader) b of
        (c, d) -> [Text $ rawHtml "<div class=\"handout\">"] ++ c 
               ++ [Text $ rawHtml "</div>"] ++ preprocessForSlides d
 where
    isLim (Text HorizontalRule) = True
    isLim _ = False

    isHeader (Text (Header _ _)) = True
    isHeader _ = False

------------------------------------

rawHtml :: String -> Block
rawHtml x = RawBlock "html" x

showBlockSimple :: Language -> String -> String -> Int -> String -> [Block]

showBlockSimple lang fn i rows_ cont = (:[]) $ rawHtml $ showHtmlFragment $ indent $
  [ form
    ! [ theclass $ if null cont then "interpreter" else "resetinterpreter"
      , action $ getOne "check" fn i i
      ]
    <<[ textarea 
        ! [ cols "80"
          , rows $ show rows_
          , identifier $ "tarea" ++ i
          ]
        << cont
      , br
      , input ! [thetype "submit", value $ translate lang "Check"]
      ]
  , thediv ! [theclass "answer", identifier $ "res" ++ i] << ""
  ]

-----------------

showEnv :: String -> String
showEnv prelude
    =  "{-# LINE 1 \"testenv\" #-}\n"
    ++ prelude
    ++ "\n{-# LINE 1 \"input\" #-}\n"

mkImport :: String -> [Name] -> ImportDecl
mkImport m d 
    = ImportDecl
        { importLoc = undefined
        , importModule = ModuleName m
        , importQualified = False
        , importSrc = False
        , importPkg = Nothing
        , importAs = Nothing
        , importSpecs = Just (True, map IVar d)
        }

mkImport_ :: String -> String -> ImportDecl
mkImport_ magic m 
    = (mkImport m []) { importQualified = True, importAs = Just $ ModuleName magic }

------------------

whenOutOfDate :: b -> FilePath -> FilePath -> IO b -> IO b
whenOutOfDate def x src m = do
    a <- modTime x
    b <- modTime src
    case (a, b) of
        (Nothing, Just _) -> m
        (Just t1, Just t2) | diffClockTimes t2 t1 > noTimeDiff -> m
        _   -> return def
 where
    modTime f = do
        a <- doesFileExist f
        if a then fmap Just $ getModificationTime f else return Nothing


--------------------



