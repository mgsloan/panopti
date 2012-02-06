module PartialParse where

import State
import Utils

import Control.Arrow (second)
import Control.Monad (liftM)
import Data.List (find, isPrefixOf)
import Data.Maybe (fromJust)
import Debug.Trace

import Language.Haskell.Exts hiding (parseType)
import qualified Language.Haskell.Exts.Annotated as A

import Language.Haskell.Exts.Lexer
import Language.Haskell.Exts.ParseMonad

-- While I do not think this is a particularly good way of auto-fixing syntax
-- errors, it does get us from zero type information to some type information!
-- The question is, do we prefer to avoid giving any potentially misleading
-- information? I think the answer is to design the visualization to be
-- relatively transparent about the fashion in which the user's program is being
-- interpreted.

partialParse 
  :: (Show a, Eq a)
  => (String -> ParseResult a)            -- ^ Parsing function
  -> String                               -- ^ String to run it on
  -> (Maybe a, [(Ivl, ParseRes)]) -- ^ A resolution to force parsing, if found
partialParse f t0 = case f t0 of
  ParseOk decl -> (Just decl, [])
  err -> (Nothing, [])

{-
partialParse f t0 = rec 10 Nothing t0
 where
  rec 0 _ _ = (Nothing, [])
  rec limit prior txt =
    case f txt of
      ParseOk decl -> (Just decl, [])
      err -> if prior == Just err
             then (Nothing, [])
             else handleError err
   where
    handleError err@(ParseFailed l e)

-- Include a record of the error that required resolution.
      = liftM (second ((errorEdits, e):))

-- Solve any further errors, passing in the current one to avoid re-attempting
-- an ineffective fix to the same error.
      . rec (limit - 1) (Just err) 

-- Attempt to resolve the error, if possible.
      $ applyEdits errorEdits txt

     where
-- Maps error messages to a naive edit to attempt as a resolution.
      errorEdits
        | "Parse error: EOF"             `isPrefixOf` e = [(eivl, balanced)]
        | "Improperly terminated string" `isPrefixOf` e = [(eivl, "\"")]
        | "Unterminated nested comment"  `isPrefixOf` e = [(eivl, '-' : "}")]
        | "Parse error:"                 `isPrefixOf` e = [(tivl, "")]
        | "Parse error in expression: "  `isPrefixOf` e = [(tivl, "")]
        | otherwise = trace e []

-- The interval at the end of the expression.  Usage of this interval in an edit
-- results in the insertion of the string.
      eivl = (end - 1, end)
      end = length txt

-- Singleton interval at the position that the error occured.
      ivl = (srcColumn l, srcColumn l)

-- Lex of the provided text.
-- TODO: return Nothing when this fails and tivl or balanced is needed.
      lexed = fromJust . preferOk $ lexify txt

-- Interval around the token which contains the location of the error.
      tivl = maybe ivl id 
           . find ((`ivlContains` ivl))
           $ map (colSpan . A.loc) lexed

-- TODO: make somewhat more intelligent - recognize bounds that parens can't 
--       pass.  This will probably be done by calling balanceParens, on lists
--       created by splitting on all of the Haskell tokens that cause a boundary
      balanced = balanceParens lexed
-}

-- Lexes a string into a list of tokens.
lexify = runParser lexRec 
 where
  lexRec = runL (Lex lexer) (\x -> case A.unLoc x of
      EOF -> return []
      _ -> liftM (x:) lexRec)

-- Given a token stream, discovers the disbalance in parenthesis / braces /
-- brackets, and returns, the symbols that need to be inserted, in the
-- appropriate order.
balanceParens :: [A.Loc Token] -> String
balanceParens = map tokChar . (`doToks`[]) . map A.unLoc
 where
  tokChar RightParen  = ')'
  tokChar RightSquare = ']'
  tokChar RightCurly  = '}'

  doToks [] s = s
  doToks (LeftParen :xs) s = doToks xs $ RightParen  : s
  doToks (LeftSquare:xs) s = doToks xs $ RightSquare : s
  doToks (LeftCurly :xs) s = doToks xs $ RightCurly  : s
  doToks (r:xs) s
    | r `elem` [RightParen, RightSquare, RightCurly]
    = doToks xs $ maybe s snd $ extractFirst (==r) s
  doToks (_:xs) s = doToks xs s
