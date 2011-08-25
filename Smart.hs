{-# LANGUAGE ViewPatterns, PatternGuards, OverloadedStrings #-}
module Smart where

import HoogleCustom
import Specialize
import Lang
import Result
import qualified Simple

import ActiveHs.Base (WrapData2 (..), WrapData(..))
import Graphics.Diagrams (Diagram)
import Graphics.Diagrams.SVG (render)
import Graphics.Diagrams.FunctionGraphs (displayFun, displayDiscreteFun, displayArc)

import qualified Data.Data.Eval as C
import qualified Data.Data.Compare as C
import Data.Data.GenRep hiding (Error)
import Data.Data.GenRep.Functions (mistify, numberErrors)
import Data.Data.GenRep.Doc (toDoc)

import Language.Haskell.Interpreter hiding (eval)
import Data.Digest.Pure.MD5
import Hoogle (Database, loadDatabase)
import System.FastLogger

import System.Locale (defaultTimeLocale)
import Data.Time (getCurrentTime, formatTime)
import Data.ByteString.UTF8 (fromString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.UTF8 as Lazy

import Control.Monad (join)
import Data.Dynamic hiding (typeOf)
import qualified Data.Data as D
import Data.List (nub)
import Data.Char (isAlpha)
import Prelude hiding (catch)


----------------------------------------------------------------------

data TaskChan 
    = TC 
        { logger    :: Logger
        , database  :: Maybe Database  -- for Hoogle searches
        , chan      :: Simple.TaskChan
        }

startGHCiServer :: [FilePath] -> FilePath -> FilePath -> IO TaskChan
startGHCiServer searchpaths logfilebase dbname = do
    ti <- getCurrentTime
    log <- newLogger $ logfilebase ++ "_" ++ formatTime defaultTimeLocale "%Y-%m-%d-%H:%M:%S" ti ++ ".log"
    db <- if (dbname == "") then return Nothing else fmap Just $ loadDatabase dbname
    ch <- Simple.startGHCiServer
            searchpaths
            (logMsgWithImportance 5 log . fromString) 
            (logMsgWithImportance 4 log . fromString)
    return $ TC
        { logger    = log
        , database  = db
        , chan      = ch
        }

restart :: TaskChan -> IO ()
restart ch = do
    Simple.restartGHCiServer (chan ch)

---------------

showErr :: Language -> InterpreterError -> [String]
showErr lang (WontCompile l)   = nub{-miért?? -} . map errMsg $ l
showErr lang (UnknownError s)  = [ translate lang "Unknown error: " ++ s]
showErr lang (NotAllowed s)    = [ translate lang "Not allowed: " ++ s]
showErr lang (GhcException s)  = [ translate lang "GHC exception: " ++ s]

----------------------------------------------------------------------

logMsgWithImportance :: Int -> Logger -> B.ByteString -> IO ()
logMsgWithImportance n ch x = do
    v <- timestampedLogEntry $ B.concat 
        [nn, "    ", x, "    ", nn]
    logMsg ch v 
 where
    nn = fromString $ replicate n '#'



----------------------------------------------------------------------

mkId :: String -> MD5Digest
mkId = md5 . Lazy.fromString

------------------

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


interp :: Bool -> MD5Digest -> Language -> TaskChan -> FilePath -> String 
    -> (String -> Interpreter (IO [Result])) -> IO [Result]
interp  verboseinterpreter (show -> idi) lang ch fn s@(getCommand -> (c, a)) xy 
    = case c of

    "?" ->  return $ query True (database ch) a

    "i" ->  return $ queryInfo lang True (database ch) a

    c | c `elem` ["t","k",""]
       -> join 
        $ fmap (either (return . map (Error True) . showErr lang) id)
        $ Simple.interpret (chan ch) fn
        $ case c of
            "t" -> catchE True $ do
                xx <- typeOf a
                return $ return [ExprType False a xx []]
            "k" -> catchE True $ do
                xx <- kindOf a
                return $ return [TypeKind a xx []]
            "" -> fmap (fmap ((if verboseinterpreter then id else filterResults) . concat) . sequence) $ sequence
                [ catchE False $ do
                    ty <- typeOf s
                    case specialize ty of
                      Left err -> return $ return [Error True err]
                      Right (ty',ty'') -> fmap (fmap concat . sequence) $ sequence
                        [ catchE False $ fmap (pprintData idi ty'') $
                            interpret ("wrapData (" ++ parens s ++ " :: " ++ ty'' ++")") (as :: WrapData)

                        , catchE False $ fmap (pprint idi) $
                            interpret ("toDyn (" ++ parens s ++ " :: " ++ ty' ++")") (as :: Dynamic)

                        , return $ return [ExprType True s ty []]
                        ]

                , catchE False $ do
                    k<- kindOf s
                    return $ return [TypeKind s k []]
                , catchE True $ xy a
                , return $ return $ query False (database ch) s
                , return $ return $ queryInfo lang False (database ch) s
                ]

    _   ->  return [Error True $ 
               translate lang "The" ++ " :" ++ c ++ " " ++ translate lang "command is not supported" ++ "."]

 where

    catchE b m 
        = m `Simple.catchError_fixed` \e -> 
            return $ return $ map (Error b) $ showErr lang e

--------------------


pprintData :: String -> String -> WrapData -> IO [Result]
pprintData idi y (WrapData x)
    | D.dataTypeName (D.dataTypeOf x) == "Diagram"
    = return []
pprintData idi y (WrapData x) = do
    a <- C.eval 1 700 x
    let ([p], es) = numberErrors [a]
    return $ [ExprType False (show $ toDoc p) y es]


pprint :: String -> Dynamic -> IO [Result]
pprint idi d
    | Just x <- fromDynamic d = ff x
    | Just x <- fromDynamic d = ff $ showFunc (x :: Double -> Double)
    | Just x <- fromDynamic d = ff $ showFunc (x :: Double -> Integer)
    | Just x <- fromDynamic d = ff $ showFunc $ fromIntegral . fromEnum . (x :: Double -> Bool)
    | Just x <- fromDynamic d = ff $ showFunc $ fromIntegral . fromEnum . (x :: Double -> Ordering)
    | Just x <- fromDynamic d = ff $ showFunc_ (x :: Integer -> Double)
    | Just x <- fromDynamic d = ff $ showFunc_ (x :: Integer -> Integer)
    | Just x <- fromDynamic d = ff $ showFunc_ $ fromIntegral . fromEnum . (x :: Integer -> Bool)
    | Just x <- fromDynamic d = ff $ showFunc_ $ fromIntegral . fromEnum . (x :: Integer -> Ordering)
    | Just x <- fromDynamic d = ff $ displayArc' (x :: Double -> (Double, Double))
    | Just (f,g) <- fromDynamic d = ff $ displayArc' ((\x -> (f x, g x)) :: Double -> (Double, Double))
    | otherwise = return []
 where
    ff = fmap g . render 10 (-16, -10) (16, 10) 0.5 1000 idi
    g (htm, err) = [Dia htm err]
    showFunc :: (RealFrac a, Real b) => (a -> b) -> Diagram
    showFunc = displayFun (-16,-10) (16,10)
    showFunc_ :: (Real b, Integral a) => (a -> b) -> Diagram
    showFunc_ = displayDiscreteFun (-16,-10) (16,10)
    displayArc' = displayArc (-16,-10) (16,10) (0,1) 

------------------------

wrap2 :: String -> String -> String
wrap2 a b = "WrapData2 " ++ parens a ++ " " ++ parens b

----------------

compareMistGen :: Language -> String -> WrapData2 -> String -> IO [Result]
compareMistGen lang idi (WrapData2 x y) goodsol
    | D.dataTypeName (D.dataTypeOf x) == "Diagram" 
    = return [Message (translate lang "Can't decide the equality of diagrams (yet).") Nothing]
compareMistGen lang idi (WrapData2 x y) goodsol = do
    (ans, a', b') <- C.compareData 0.8 0.2 700 x y
    return $ case ans of
        C.Yes -> [ Message (translate lang "Good solution! Another good solution:")
                            $ Just $ ExprType False goodsol "" []]
        _ ->
            let x = case ans of
                    C.Maybe _  -> "I cannot decide whether this is a good solution:"
                    C.No       -> "Wrong solution:"
            in [Message (translate lang x) $ Just $ showPair ans (a', mistify b')]


---------------------------------

compareClearGen :: Language -> String -> WrapData2 -> IO [Result]
compareClearGen lang idi (WrapData2 x y)
    | D.dataTypeName (D.dataTypeOf x) == "Diagram"
    = return [Message (translate lang "Can't decide the equality of diagrams (yet).") Nothing]
compareClearGen lang idi (WrapData2 x y) = do
    (ans, a', b') <- C.compareData 0.8 0.2 700 x y
    return $ case ans of
--        C.Yes -> []
        _ -> [showPair ans (a', b')]


showPair :: C.Answer -> (GenericData, GenericData) -> Result
showPair x (a, b) = Comparison (show (toDoc a')) x (show (toDoc b')) es
  where ([a', b'], es) = numberErrors [a, b]



