{-# LANGUAGE OverloadedStrings #-}
module Result
    ( Result (..)
    , hasError
    , filterResults
    ) where

import Lang
import Data.Data.Compare

import Text.XHtml.Strict
import Control.DeepSeq

---------------------

type Err = (String, String)

data Result
    = ExprType Bool String String [Err]        
            -- expression with type and error messages
            -- True: this is an error (can't do better just show the type)
    | TypeKind String String [Err]        -- type with kind and error messages
    | Comparison String Answer String [Err]
    | SearchResults Bool [String]
    | Error Bool String         -- True: important error message
    | Dia Html [Err]
    | Message String (Maybe Result)
    | ModifyCommandLine String
    | ShowInterpreter Language Int String String{-Id-} Char String [Result]
        deriving (Show)

instance NFData Result where
    rnf (ExprType e a b l) = rnf (e,a,b,l)
    rnf (TypeKind a b l) = rnf (a,b,l)
    rnf (Comparison a x b l) = rnf (a,x,b,l)
    rnf (SearchResults b l) = rnf (b,l)
    rnf (Error b s) = rnf (b, s)
    rnf (Message s r) = rnf (s, r)
    rnf (Dia h e) = length (showHtmlFragment h) `seq` rnf e
    rnf (ModifyCommandLine s) = rnf s
    rnf (ShowInterpreter a b c d e f g) = rnf (a,b,c,d,e,f,g)

errors :: Result -> Bool
errors (ExprType _ _ _ l) = not $ null l
errors (TypeKind _ _ l) = not $ null l
errors (Comparison _ x _ l) = x /= Yes || not (null l)
errors (Dia _ l) = not $ null l
errors (Error i _) = i
errors (Message _ x) = maybe False errors x
errors (ShowInterpreter _ _ _ _ _ _ g) = any errors g
errors _ = False

filterResults :: [Result] -> [Result]
filterResults rs = case filter (not . weakError) rs of
    [] -> take 1 rs
    rs -> case filter (not . searchResult) rs of
        [] -> rs
        rs -> {- nubBy f -} rs
{-
 where
    f (ExprType _ _ _ _) (ExprType _ _ _ _) = True
    f _ _ = False
-}

hasError :: [Result] -> Bool
hasError rs = case filter (not . weakError) rs of
    [] -> True
    rs -> any errors rs

weakError :: Result -> Bool
weakError (Error _ _) = True
weakError (ExprType b _ _ _) = b
weakError _ = False

searchResult :: Result -> Bool
searchResult (SearchResults _ _) = True
searchResult _ = False

