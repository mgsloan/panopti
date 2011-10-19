module Language.Haskell.TH.

-- :: String -> String -> QuasiQuoter
-- = QuasiQuoter 

t' :: QuasiQuoter
t' = QuasiQuoter

parseIt = 
 where
  foldl process (uniq, [], []) xs
  process (x:xs) = 
  xs = parseLeft "" txt
  uniq = ["__" ++ show ix | ix <- [0..]]
  

type ParseChunk = Either String String

parseLeft :: String -> String -> [ParseChunk]
parseLeft s txt = case txt of
  [] -> [Left (reverse s)]
  ('?':'{':xs) -> Left (reverse s) : parseRight "" xs
  ('\\':'?':'{':xs) -> parseLeft ('{':'?':s) xs
  (x:xs) -> parseLeft (x:s) xs
    
parseRight :: String -> String -> [ParseChunk]
parseRight s txt = case txt of
  [] -> error "Expected terminating }?"
  ('}':'?':xs) -> Right (reverse s) : parseLeft "" xs
  ('\\':'}':'?':xs) -> parseRight ('?':'}':s) xs
  (x:xs) -> parseRight (x:s) xs
