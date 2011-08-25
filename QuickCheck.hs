module QuickCheck where

import Smart
import ActiveHs.Base (WrapData2 (WrapData2), TestCase (TestCase))
import Lang
import Result
import Qualify (qualify)

import Data.Digest.Pure.MD5
import Language.Haskell.Interpreter hiding (eval)
import Test.QuickCheck hiding (Result)
import qualified Test.QuickCheck.Property as QC

import Data.Char (isLower)
import Data.List (intercalate)
import Control.Concurrent.MVar

---------------------------------------

quickCheck
    :: String -> MD5Digest -> Language -> TaskChan -> FilePath -> String -> [String] -> [([String],String)] 
    -> IO [Result]
quickCheck qualifier m5 lang ch fn ft funnames is
    = case allRight $ map (qualify qualifier funnames . snd) is of
        Left err -> return [Error True err]
        Right s_ -> interp False m5 lang ch fn "" $ \a ->
            do  m <- interpret (mkTestCases [(v,s,s') | ((v,s),s')<- zip is s_]) (as :: [TestCase])
                return $ qcs lang m

  where
    allRight :: [Either a b] -> Either a [b]
    allRight x = case [s | Left s<-x] of
        (y:_) -> Left y
        []    -> Right [s | Right s<-x]

    mkTestCases ss 
        = "[" ++ intercalate ", " (map mkTestCase ss) ++ "]"

    mkTestCase (vars, s, s2)  
        = "TestCase (\\qcinner " 
        ++ unwords ["(" ++ v ++ ")" | v<-vars] 
        ++ " -> qcinner (" ++ showTr vars s ++ ", " ++ parens s ++ ", " ++ parens s2 ++ "))"


    showTr vars s = "unwords [" ++ intercalate ", " (map g $ words s) ++ "]"  -- !!!
     where

        vs = concatMap f vars

        f = filter (isLower . head) . words

        g x | x `elem` vs = {- "\"(\" ++ -} " show " ++ x -- ++ " ++ \")\""
        g x = show x

qcs :: Language -> [TestCase] -> IO [Result]
qcs lang [] = return [Message (translate lang "All test cases are completed.") Nothing]
qcs lang (t:ts) = do
    s' <- qc lang t
    case s' of
        Just rep -> rep
        Nothing  -> qcs lang ts

-- test = qc $ TestCase $ \f (QCNat x) (QCInt y) -> f (show x ++ " + " ++ show y, min 10 (x + y), x + y)
-- test' = qc $ TestCase $ \f (QCInt x) -> f ("sqr " ++ show x, min 10 (x*x), x*x)

qc :: Language -> TestCase -> IO (Maybe (IO [Result]))
qc lang (TestCase p) = do
    v <- newMVar Nothing
    _ <- quickCheckWithResult (stdArgs { chatty = False }) $ {- QC.noShrinking $ -} p $ ff v
    takeMVar v
 where
    ff v (s, x, y)
        = QC.whenFail (modifyMVar_ v $ const $ return $ Just $ fmap (ModifyCommandLine s :) res)
        $ QC.morallyDubiousIOProperty
        $ fmap f res
      where
        res = compareClearGen lang "noId" $ WrapData2 x y

    f s | hasError s = QC.failed
    f s = QC.succeeded


