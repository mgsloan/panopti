{-# LANGUAGE OverloadedStrings #-}
module Html 
    ( Html
    , renderResult
    , renderResults
    , renderResults_
    , showInterpreter
    , indent, delim, getOne, getTwo
    ) where

import Result
import Lang
import Data.Data.Compare

import Text.XHtml.Strict
import qualified Data.Text as T

---------------------

renderResults :: [Result] -> T.Text
renderResults 
    = T.intercalate "&#160;" . T.splitOn "&nbsp;" 
    . T.pack 
    . showHtmlFragment 
    . renderResults_

renderResults_ :: [Result] -> Html
renderResults_
    = foldr (|-|) noHtml 
    . map renderResult

renderResult :: Result -> Html
renderResult (ExprType _ e t err) 
    = showRes e "::" t $ map mkBott err
renderResult (TypeKind e t err) 
    = showRes e "::" t $ map mkBott err
renderResult (SearchResults b l)
    = foldr (|-|) noHtml $ map (showCode_ "search") l ++ [toHtml ("..." :: String) | b]
renderResult (Error _ s) 
    = showRes "" "" "" [showLines s]
renderResult (Message s r) 
    = toHtml s |-| maybe noHtml renderResult r
renderResult (Comparison a x b es)
    = showRes a (showAnswer x) b (map mkBott es)
renderResult (Dia htm err)
    = showResErr htm (map mkBott err)
renderResult (ModifyCommandLine _)
    = noHtml
renderResult (ShowInterpreter lang limit act i prompt exp res)
    = showInterpreter lang limit act i prompt exp res

(|-|) :: Html -> Html -> Html
a |-| b | isNoHtml a || isNoHtml b = a +++ b
a |-| b = a +++ br +++ b

showCode :: String -> String -> Html
showCode c x 
    | isNoHtml x'   = x'
    | null c        = f $ thecode << x'
    | otherwise     = f $ thecode ! [theclass c] << x'
  where
    x' = toHtml x
    f y | elem '\n' x = pre ! [theclass "normal"] << y
        | otherwise   = y

showCode_ :: String -> String -> Html
showCode_ c x 
    = thecode ! [theclass c] << primHtml x

showRes :: String -> String -> String -> [Html] -> Html
showRes e x b err
    = showResErr (p1
    +++ showCode "" (if null x || isNoHtml p1 || isNoHtml p2 then "" else " " ++ x ++ " ")
    +++ p2
    +++ showCode "comment" (if null c then c else " --" ++ c))
    err
 where
    p1 = showCode "result" a
    p2 = showCode (if x == "::" then "type" else "result") b
    (a, c) = splitComment e

showResErr :: Html -> [Html] -> Html
showResErr r err = r |-| me err
 where
    me [] = noHtml
    me x = thediv ! [theclass "error"] << foldr (|-|) noHtml err

showLines :: String -> Html
showLines e 
    | elem '\n' e  = pre ! [theclass "normal"] << toHtml e
    | otherwise    = toHtml e

mkBott :: (String, String) -> Html
mkBott (i, e) = toHtml ("  " ++ i ++ ": ") +++ showLines e

splitComment :: String -> (String, String)
splitComment x = case splitComment' x of
    Just (a,b) -> (a, b)
    _ -> (x, "")

splitComment' :: String -> Maybe (String, String)
splitComment' a = f [] a  where

    f ac ('-':'-':c) | isComment c = Just (reverse $ dropWhile (==' ') ac, c)  -- !!!
    f ac ('"':cs) = uncurry f $ skipString ('"':ac) cs
    f ac (c:cs) = f (c:ac) cs
    f ac [] = Nothing

    isComment ('-':c) = isComment c
    isComment (d:_) | isSymbol d = False
    isComment _ = True

    isSymbol d = False --- !!!

    skipString a ('"':cs) = ('"':a, cs)
    skipString a ('\\':'\\':cs) = skipString ('\\':'\\':a) cs
    skipString a ('\\':'"':cs) = skipString ('"':'\\':a) cs
    skipString a (c:cs) = skipString (c:a) cs
    skipString a [] = (a, [])


showInterpreter :: Language -> Int -> String -> String{-Id-} -> Char -> String -> [Result] -> Html
showInterpreter lang limit act i prompt exp res = indent $
    form
    ! [ theclass $ if prompt == 'R' || null exp then "interpreter" else "resetinterpreter"
      , action act ]
    << (onlyIf (prompt /= 'A')
      [ thecode ! [theclass "prompt"] << (translate lang (if prompt /= 'R' then "Test" else "Solution") ++ "> ")
      , input 
        ! [ theclass "interpreter"
          , thetype "text"
          , size $ show limit
          , maxlength 1000
          , identifier $ "tarea" ++ i
          , value $ if prompt == 'R' then "" else exp
          ]
      , br
      ] ++
      [ thediv
        ! [ theclass "answer"
          , identifier $ "res" ++ i
          ] << if prompt `notElem` ['R', 'F'] then renderResults_ res else noHtml
      ])

onlyIf :: Bool -> [a] -> [a]
onlyIf True l = l
onlyIf _ _ = []

indent :: HTML a => a -> Html
indent x = thediv ! [ theclass "indent" ]  << x

delim :: String
delim = "-----"

getOne :: String -> String -> String -> String -> String
getOne c f t x   = concat ["javascript:getOne('c=", c, "&f=", f, "','", t, "','", x, "');"]

getTwo :: String -> String -> String -> String -> String -> String
getTwo c f t x y = concat ["javascript:getTwo('c=", c, "&f=", f, "','", t, "','", x, "','", y, "');"]


