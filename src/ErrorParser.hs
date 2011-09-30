{-# LANGUAGE TemplateHaskell, QuasiQuotes, ViewPatterns #-}

module ErrorParser where

import Utils

import Control.Applicative             ( liftA2 )
import Control.Arrow                   ( (&&&) )
import Control.Monad                   ( liftM )
import Data.Char                       ( isNumber, isSpace )
import Data.Function                   ( on )
import Data.List                       ( groupBy, inits, tails )
import Data.Maybe                      ( catMaybes, fromJust, listToMaybe )
import Language.Haskell.Exts.Annotated ( SrcSpan(..) )
import Text.Regex.PCRE.Rex             ( maybeRead )

data Tree = Tree String [Tree] deriving Show

toTrees = rec . map (length . takeWhile isSpace &&& trim) . lines
 where
      -- Extract tree structure
  rec = map (\((_,x):xs) -> Tree x (rec xs))
      . groupBy ((<) `on` fst)
      {-
      -- Merge overflowing lines into one line.
      . map (\xs@((i, x):_) -> (i, unwords $ map snd xs))
      . pairGroupBy (\x y -> (fst y - fst x) > 2)
      -}

concatTree (Tree x xs) = unwords $ x : map concatTree xs

-- These are errors we understand.
data GHCError = TypeError TypeError [AuxInfo] --SrcSpan
              | IdentifierError String [String] --SrcSpan
              | UnknownError String --SrcSpan
  deriving Show

data AuxInfo = AuxCtxt Ctxt
  deriving Show

data Ctxt = ExprCtxt String 
          | RecordCtxt String 
          | ArgCtxt Int String String
          | UnknownCtxt String
  deriving Show

data TypeError = InstanceError String (Maybe Arising)
               | EqualityError String String String String
  deriving Show

data Arising = Arising String (Maybe SrcSpan)
  deriving Show

-- `(?{ field }[^']*)'

parseGHCError :: String -> GHCError
parseGHCError inputErr = parseTop inputTrees (concatTree $ head inputTrees)
 where
  inputTrees = toTrees inputErr
  doTE trees t = TypeError t (catMaybes $ map parseAux trees)

  parseAux tr@(Tree [rex|^In the.*$|] _) = Just . AuxCtxt $ parseCtxt (concatTree tr)
  parseAux _ = Nothing

  parseTop (_:xs)  [rex|^Not in scope: `(?{ ident }[^']*)'|]
    = IdentifierError ident
    . maybe [] id . liftM (parseSugg . concatTree) $ listToMaybe xs

  parseTop (_:xs) [rex|^No instance(s)? for (?{rest}.*)$|] 
    = doTE xs $ InstanceError cs (parseArising a)
   where
    (0, cs, a) = parseParens rest

  parseTop (_:ea:xs) [rex|^Couldn't match expected type `(?{ eTy }[^']*)' 
                                       with actual type `(?{ aTy }[^']*)'|]
    = doTE xs $ EqualityError eTy aTy efTy afTy
   where
    (Tree [rex|^Expected type: (?{efTy}.*)$|]
     ((Tree [rex|^Actual type: (?{afTy}.*)$|] _):_)) = ea

  parseTop _ x = UnknownError x
  
{-   where
    (0, _) = parseParens (debug ctx) -}

repMatch :: Show a => (String -> Maybe (a, String)) -> String -> [a]
repMatch f s = case f s of
  Just (x, s') -> x : repMatch f s'
  Nothing -> []

parseSugg :: String -> [String]
parseSugg [rex|^Perhaps you meant one of these:\s*(?{xs}.*)$|] =
  repMatch [rex|`(?{ }[^']*)'[^`]*(?{ }.*)$|] xs
parseSugg r = []

parseArising [rex|^arising from(?{rest}.*)$|] =
  Just $ Arising (unwords pre) (parseSpan . unwords $ drop 1 loc)
 where
  (pre, loc) = span (/="at") $ words rest
parseArising _ = Nothing

-- ghc/compiler/hsSyn/HsExpr pprMatchContextNoun for more
parseCtxt :: String -> Ctxt
parseCtxt [rex|^In the expression: (?{ rest }.*)$|] = ExprCtxt rest
parseCtxt [rex|^In the `(?{ field }[^']*)' field of a record$|] = RecordCtxt field
parseCtxt [rex|^In the (?{ parseNth -> n }\S*) argument of
                `(?{ fun }[^']*)', namely (?{arg})$|] = ArgCtxt n fun arg
parseCtxt x = UnknownCtxt x

parseSpan :: String -> Maybe SrcSpan
parseSpan
 [rex|((?{ file }[^:]*):)?
      \((?{ maybeRead -> Just sline }\d*),(?{ maybeRead -> Just scol }\d*)\)-
      \((?{ maybeRead -> Just eline }\d*),(?{ maybeRead -> Just ecol })\)|]
  = Just $ SrcSpan file sline scol eline ecol
parseSpan
 [rex|((?{ file }[^:]*):)?
      (?{ maybeRead -> Just line }\d*):
      (?{ maybeRead -> Just scol }\d*)-(?{ maybeRead -> Just ecol })|]
  = Just $ SrcSpan file line scol line ecol
parseSpan
 [rex|((?{ file }[^:]*):)?
      (?{ maybeRead -> Just line }\d*):(?{ maybeRead -> Just col }\d*)|]
  = Just $ SrcSpan file line col line col
parseSpan _ = Nothing

findOrLast _ [x] = x
findOrLast f (x:xs) | f x = x
                    | otherwise = findOrLast f xs

parseParens "" = (0, "", "")
parseParens xs = findOrLast ((==0).fst3)
               $ zip3 (tail $ scanl pdelt 0 xs) (inits xs) (tails xs)
 where
  pdelt cnt '(' = cnt + 1
  pdelt cnt ')' = cnt - 1
  pdelt cnt _ = cnt

parseNth "first"  = 1
parseNth "second" = 2
parseNth "third"  = 3
parseNth "fourth" = 4
parseNth "fifth"  = 5
parseNth "sixth"  = 6
parseNth x = read (takeWhile isNumber x)

parseN "none"  = 0
parseN "one"   = 1
parseN "two"   = 2
parseN "three" = 3
parseN "four"  = 4
parseN "five"  = 5
parseN "six"   = 6
parseN x = read x

{- Initial conversion of origin serializer parser
parseOrigin :: CtOrigin -> SDoc parseOrigin [rex|^a use of `(?{name}[^']*)'|] = (OccurrenceOf name)   = hsep [ptext (sLit "a use of"), quotes (ppr name)] parseOrigin [rex| |] = AppOrigin             = ptext (sLit "an application")
parseOrigin [rex| |] = (SpecPragOrigin name) = hsep [ptext (sLit "a specialisation pragma for"), quotes (ppr name)]
parseOrigin [rex| |] = (IPOccOrigin name)    = hsep [ptext (sLit "a use of implicit parameter"), quotes (ppr name)]
parseOrigin [rex| |] = RecordUpdOrigin       = ptext (sLit "a record update")
parseOrigin [rex| |] = (AmbigOrigin name)    = ptext (sLit "the ambiguity check for") <+> quotes (ppr name)
parseOrigin [rex| |] = ExprSigOrigin         = ptext (sLit "an expression type signature")
parseOrigin [rex| |] = PatSigOrigin          = ptext (sLit "a pattern type signature")
parseOrigin [rex| |] = PatOrigin             = ptext (sLit "a pattern")
parseOrigin [rex| |] = ViewPatOrigin         = ptext (sLit "a view pattern")
parseOrigin [rex| |] = IfOrigin              = ptext (sLit "an if statement")
parseOrigin [rex| |] = (LiteralOrigin lit)   = hsep [ptext (sLit "the literal"), quotes (ppr lit)]
parseOrigin [rex| |] = (ArithSeqOrigin seq)  = hsep [ptext (sLit "the arithmetic sequence"), quotes (ppr seq)]
parseOrigin [rex| |] = (PArrSeqOrigin seq)   = hsep [ptext (sLit "the parallel array sequence"), quotes (ppr seq)]
parseOrigin [rex| |] = SectionOrigin	   = ptext (sLit "an operator section")
parseOrigin [rex| |] = TupleOrigin	   = ptext (sLit "a tuple")
parseOrigin [rex| |] = NegateOrigin	   = ptext (sLit "a use of syntactic negation")
parseOrigin [rex| |] = ScOrigin	           = ptext (sLit "the superclasses of an instance declaration")
parseOrigin [rex| |] = DerivOrigin	   = ptext (sLit "the 'deriving' clause of a data type declaration")
parseOrigin [rex| |] = StandAloneDerivOrigin = ptext (sLit "a 'deriving' declaration")
parseOrigin [rex| |] = DefaultOrigin	   = ptext (sLit "a 'default' declaration")
parseOrigin [rex| |] = DoOrigin	           = ptext (sLit "a do statement")
parseOrigin [rex| |] = MCompOrigin           = ptext (sLit "a statement in a monad comprehension")
parseOrigin [rex| |] = ProcOrigin	           = ptext (sLit "a proc expression")
parseOrigin [rex| |] = (TypeEqOrigin eq)     = ptext (sLit "an equality") <+> ppr eq
parseOrigin [rex| |] = AnnOrigin             = ptext (sLit "an annotation")
parseOrigin [rex| |] = FunDepOrigin          = ptext (sLit "a functional dependency")
-}

{-
parseTree = do
  whitespace <- many space
  xs <- getState
  let (popped, (top:rest)) = span ((>length whitespace) . fst) xs
  foldl1 (\((ix, x) (ix, (PprTree )) -> ) popped
  putState

parseErr xs = parse ghcErr "" (xs :: String)

ghcErr = do
  whitespace <- many space
  (length whitespace)
-}

--TODO: make these look at indent level to make parsing less error-prone.

{-
parseError [rex|^No instance(s?) for (?{id -> ctx}.*) .*
                  |]
  =

parseError [rex|^Overlapping instances for (?{id -> ctx}.*) \s* Matching instances:.*$|]
  =

parseError [rex|^Couldn't match expected type|]

-}
{-
parseSugg :: String -> [String]
parseSugg [rex|^Perhaps you meant( one of these:)?\s*(?{xs}.*)$|] =
  repMatch [rex|^`(?{ }[^']*)'[^`]*(?{ }.*)$|] xs
  
parseSugg r = [r]

parseError [rex|^Not in scope: `(?{ ident }[^']*)'\s*(?{ parseSugg -> sugg }.*)$|]
  = IdentifierError ident sugg

parseError [rex|^Not in scope: `(?{ ident }.*)'$|] = IdentifierError ident []
parseError x = UnknownError x


data ExprCtxt = InExpression String

-- parseExprCtxt = [rex|^In the (?{}.*) (?{}In the.*)$|]


repMatch :: Show a => (String -> Maybe (a, String)) -> String -> [a]
repMatch f s = case f s of
  Just (x, s') -> x : repMatch f s'
  Nothing -> []
-}

{-
-- Converts from GHC span to haskell-src-exts SrcSpan
convertSpan x = SrcSpan (G.unpackFS $ G.srcSpanFile x)
                        (G.srcSpanStartLine x) (G.srcSpanStartCol x)
                        (G.srcSpanEndLine x)   (G.srcSpanEndCol x)
-}
