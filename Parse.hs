{-# LANGUAGE RelaxedPolyRec, PatternGuards, ViewPatterns #-}

module Parse 
    ( Doc (..)
    , BBlock (..)
    , Prompt
    , mainParse
    , getCommand
    , printName
    , parseQuickCheck
    ) where

import Text.Pandoc

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax

{- Agda support (unfinished)
import qualified Agda.Syntax.Common as Agda
import qualified Agda.Syntax.Concrete as Agda
import qualified Agda.Syntax.Parser as Agda
-}

import Data.List.Split (splitOn)
import Data.List (tails, partition, groupBy)
import Data.Function (on)
import Data.Char (isAlpha, isSpace, toUpper, isUpper)
import Control.Monad (zipWithM)



--------------------------------- data structures

data Doc
    = Doc
        Meta{-title, author, date-}
        Module{-module directives, module name, imports-}
        [BBlock]
        deriving (Show)

data BBlock
    = Text Block{-pandoc block-}
    | OneLineExercise
        Prompt
        Bool{-intentional error-}
        String
    | Exercise
        [String]{-lines-}
        [String]{-visible lines-}
        [String]{-hidden lines-}
        [Name]{-defined names-}
        [String]{-test expressions-}
        deriving (Show)

type Prompt = Char  -- see the separate documentation

-----------------------------------

mainParse :: Bool -> FilePath -> IO Doc
mainParse agda s = do
    c <- readFile s
    case readMarkdown pState . unlines . concatMap preprocess . lines $ c of
        Pandoc meta (CodeBlock ("",["sourceCode","literate","haskell"],[]) h: blocks) -> do
            header <- liftError . parseModule' $ h
            fmap (Doc meta header) $ collectTests agda $ map (interpreter . Text) blocks
        Pandoc meta blocks -> do
            header <- liftError . parseModule' $ "module Unknown where"
            fmap (Doc meta header) $ collectTests agda $ map (interpreter . Text) blocks
 where

    parseModule' = parseModuleWithMode (defaultParseMode {fixities = []})

    preprocess (c:'>':' ':l) | c `elem` commandList
        = ["~~~~~ {." ++ [c] ++ "}", dropWhile (==' ') l, "~~~~~", ""]
    preprocess ('|':l) 
        = []
    preprocess l
        = [l]

    pState = defaultParserState 
        { stateSmart = True
        , stateStandalone = True
        , stateLiterateHaskell = True 
        }

    liftError :: (Monad m, Show a) => ParseResult a -> m a
    liftError (ParseOk m) = return m
    liftError x = fail $ "parseHeader: " ++ show x

    interpreter :: BBlock -> BBlock
    interpreter (Text (CodeBlock ("",[[x]],[]) e)) | x `elem` commandList 
        = OneLineExercise (toUpper x) (isUpper x) e
    interpreter a = a


commandList, testCommandList :: String
commandList = "AaRr" ++ testCommandList
testCommandList = "EeFfH"


------------------------------

collectTests :: Bool -> [BBlock] -> IO [BBlock]
collectTests agda l = zipWithM f l $ tail $ tails l where

    f (Text (CodeBlock ("",["sourceCode","literate","haskell"],[]) h)) l = do
        let
            isExercise = True -- not $ null $ concatMap fst exps

        (visible, hidden, funnames) <- processLines agda isExercise h
        let
            exps = [snd $ getCommand e | (OneLineExercise _ _ e) <- takeWhile p l]

            p (OneLineExercise x _ e) = x `elem` testCommandList && fst (getCommand e) == ""
            p _ = False

        return $ Exercise (lines h) visible hidden funnames exps

    f x _ = return x

processLines :: Bool -> Bool -> String -> IO ([String], [String], [Name])
--processLines True = processAgdaLines
processLines _ = processHaskellLines

{- Agda support (unfinished)
processAgdaLines :: Bool -> String -> IO ([String], [String], [Name])
processAgdaLines isExercise l_ = do
    let
        l = parts l_

    x <- fmap (zip l) $ mapM (Agda.parse Agda.moduleParser . ("module X where\n"++) . unlines) l
    let
        names = map toName $ concatMap (getFName . snd . snd) x

--        getFName [Agda.Module _ _ [Agda.TypedBindings _ (Agda.Arg _ _ [Agda.TBind _ a _])] declarations] 
--                  = map Agda.boundName a
        getFName [Agda.Module _ _ _ [Agda.TypeSig _ n _]]
                  = [n]
        getFName _ = []

--        isVisible [Agda.Module _ _ [Agda.TypedBindings _ (Agda.Arg _ _ [Agda.TBind _ a _])] declarations] 
--                    = True
        isVisible [Agda.Module _ _ _ [Agda.TypeSig _ n _]] = True
        isVisible _ = not isExercise

        (visible, hidden) = partition (isVisible . snd . snd) x

        toName n =  Ident $ show n

    return (concatMap fst visible, concatMap fst hidden, names)
-}

processHaskellLines :: Bool -> String -> IO ([String], [String], [Name])
processHaskellLines isExercise l_ = return (concatMap fst visible, concatMap fst hidden, names)
 where
    x = zip l $ map (parseDeclWithMode (defaultParseMode {fixities = []})  . unlines) l

    l = parts l_

    names = concatMap (getFName . snd) x

    getFName (ParseOk x) = case x of
        TypeSig _ a _             -> a
        PatBind _ (PVar a) _ _ _  -> [a]
        FunBind (Match _ a _ _ _ _ :_) ->  [a]
        TypeDecl _ a _ _          -> [a]
        DataDecl _ _ _ a _ x _    -> a: [n | QualConDecl _ _ _ y<-x, n <- getN y]
        _                         -> []
    getFName _ = []

    getN (ConDecl n _) = [n]
    getN (InfixConDecl _ n _) = [n]
    getN (RecDecl n l) = n: concatMap fst l

    isVisible (ParseOk (TypeSig _ _ _)) = True
    isVisible (ParseOk (InfixDecl _ _ _ _)) = True
    isVisible _ = not isExercise

    (visible, hidden) = partition (isVisible . snd) x


parts :: String -> [[String]]
parts = groupBy (const id `on` isIndented) . lines  where
    isIndented s | all isSpace s = True
    isIndented (' ':_) = True
    isIndented _ = False

------------------------------

getCommand :: String -> (String, String)
getCommand (':':'?': (dropSpace -> Just x)) 
    = ("?", x)
getCommand (':': (span isAlpha -> (c@(_:_), dropSpace -> Just x)))
    = (c, x)
getCommand s
    = ("", s)

dropSpace :: String -> Maybe String
dropSpace (' ':y) = Just $ dropWhile (==' ') y
dropSpace "" = Just ""
dropSpace _ = Nothing

parseQuickCheck :: String -> ([String], String)
parseQuickCheck s = case splitOn ";;" s of
    l -> (init l, last l)

printName :: Name -> String
printName (Ident x) = x
printName (Symbol x) = x




