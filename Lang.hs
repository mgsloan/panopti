
module Lang 
    ( Language
    , translate
    ) where

import Data.List (elemIndex)

------------

type Language = String   -- "en", "hu"

translate :: Language -> String -> String
translate lang txt = 
    head $ [txts !! i | txts <- tail langTable, head txts == txt] ++ [txt]
  where
    i = maybe 0 id $ elemIndex lang (head langTable)

langTable :: [[String]]
langTable =
    [ ["en", "hu"]
    , ["Erroneous evaluation", "Hibás kiértékelés"]
    , ["Test", "Teszt"]
    , ["Solution", "Megoldás"]
    , ["Check", "Ellenőrzés"]
    , ["The", "A"]
    , ["command is not supported", "parancs itt nem támogatott"]
    , ["Erroneous solution", "Hibás megoldás"]
    , ["Interrupted evaluation.", "Félbehagyott kiértékelés."]
    , ["Inconsistency between server and client. Try to reload the page.", "Inkonzisztencia a kliens és a szerver között. Próbáld meg újratölteni az oldalt!"]
    , ["The site is overloaded.", "Az oldal leterhelt."]
    , ["Too long request.", "Túl hosszú kérés."]
    , ["Inconsistency between server and client.", "Inkonzisztencia a kliens és a szerver között."]
    , ["All test cases are completed.", "Minden általam ismert tesztesetnek megfelelt!"]
    , ["Good solution! Another good solution:", "Jó megoldás! Szintén helyes megoldás:"]
    , ["I cannot decide whether this is a good solution:", "Nem tudom eldönteni, hogy jó megoldás-e:"]
    , ["Wrong solution:", "Nem jó megoldás:"]
    , ["Unknown error: ", "Ismeretlen hiba: "]
    , ["Not allowed: ", "Nem engedélyezett: "]
    , ["GHC exception: ", "GHC kivétel: "]
    , ["Can't decide the equality of diagrams (yet).", "Lehet hogy jó, sajnos nem tudom az ábrák egyezőségét ellenőrizni (most még)."]
    , ["No info for ", "Nincs erről információ: "]
    ]

