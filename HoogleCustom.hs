{-# LANGUAGE OverloadedStrings #-}
module HoogleCustom
    ( query
    , queryInfo
    ) where

import Result
import Lang

import Hoogle hiding (Language, Result)
import qualified Hoogle

import Data.List (nub)

-------------------------

query :: Bool -> Maybe Database -> String -> [Result]
query b ch a 
    = case parseQuery Haskell a of
        Right q  -> format $ nub $ map (self . snd) $ search' ch q
        Left err -> [Error b (errorMessage err)]

queryInfo :: Language -> Bool -> Maybe Database -> String -> [Result]
queryInfo lang b ch a 
    = case parseQuery Haskell a of
        Right q -> case search' ch q of
            ((_,r):_) -> [SearchResults False [showTagHTML $ docs r]]
            []        -> [Message (translate lang "No info for " ++ a) Nothing | b]
        Left err -> [Error b $ errorMessage err]

search' :: Maybe Database -> Query -> [(Score, Hoogle.Result)]
search' ch q 
    = case ch of
        Nothing -> []
        Just db -> search db q

format :: [TagStr] -> [Result]
format [] = []
format r = [SearchResults (not $ null b) (map showTagHTML a)]
  where
    (a, b) = splitAt 10 r


