module Crypt (encryption, decryption) where

import Data.Char

-- List of function pairs
-- f and g are isomorphic

-- function 1: reverse string
f1 :: String -> String
f1 = reverse

g1 :: String -> String
g1 = reverse

iso1 = (f1,g1)

----------------------------------------------------------------

-- function 2: inverse case
f2 :: String -> String
f2 = fmap f
      where f x
                | isUpper x = toLower x
                | otherwise = toUpper x

g2 :: String -> String
g2 = f2

iso2 = (f2,g2)

-----------------------------------------------------------------

-- function 3: inverse ascii value if letter
f3 :: String -> String
f3 = fmap inv
    where
        inv ch                                     
             | (97 <= (ord ch )) && ((ord ch) <= 122)   = chr $! (122 - ((ord ch)-97))
             | (65 <= (ord ch )) && ((ord ch) <= 90)    = chr $! (90  - ((ord ch)-65))
             | otherwise                                = ch




g3 :: String -> String
g3 = f3

iso3 = (f3,g3)

----------------------------------------------------------------

-- function 4: switch indicies
f4 :: String -> String
f4 = f ""
    where f str x
                | x == []         = str
                | (length x == 1) = str ++ x
                | otherwise       = f (str ++ [(x!!1),(x!!0)]) (drop 2 x)

g4 :: String -> String
g4 = f4

iso4 = (f4,g4)

-----------------------------------------------------------------

-- function 5: () [] abractions
-- this function is great becuase it is arbitrarily modifiable, as long as
------the pattern matching is done in this style. With few small changes 
------a whole new encryption can take place and still maintain isomorphism.
  
f5 :: String -> String
f5 = fmap switch 
    where
        switch '(' = ')'
        switch ')' = '('

        switch '[' = ']'
        switch ']' = '['

        switch '{' = '}'
        switch '}' = '{'

        switch  x  =  x



g5 :: String -> String
g5 = f5

iso5 = (f5,g5)

------------------------------------------------------------------


-- ================================================================
-- ================================================================

-- List of all isomorphic function pairs
lst :: [(String->String,String -> String)]
lst = [ iso1, iso2, iso3, iso4, iso5]

-- unzipp list into separate lists for f and g
fLst :: [String ->String]
fLst = fmap fst lst

gLst :: [String ->String]
gLst =  reverse $ fmap snd lst

-- fold each list into one big function using composition
finF :: String -> String
finF = foldl (.) id fLst

finG :: String -> String
finG = foldl (.) id gLst

-- let composition of f functions be 'encryption'
encryption = finF
-- let composition of g functions be 'decryption'
decryption = finG
