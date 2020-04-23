-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 3
--
-- Week 3(30-04 Oct.)
module Tutorial3 where

import           Data.Char
import           Data.List
import           Test.QuickCheck

import           Data.Function
import           Data.Maybe

-- 1.
halveEvensRec :: [Int] -> [Int]
halveEvensRec a =
  if length a == 0
    then []
    else if even (head a) == True
           then (div (head a) 2) : halveEvensRec (tail a)
           else halveEvensRec (tail a)

-- halveEvens :: [Int] -> [Int]
-- halveEvens xs = [x `div` 2 | x <- xs, x `mod` 2 == 0]
prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = [x `div` 2 | x <- xs, even x] == halveEvensRec xs

-- 2.
inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec lo hi xs =
  case lo > hi of
    True  -> [x | x <- xs, x >= hi && x <= lo]
    False -> [x | x <- xs, x >= lo && x <= hi]

-- inRange :: Int -> Int -> [Int] -> [Int]
-- inRange lo hi xs = [x | x <- xs, lo <= x, x <= hi]
prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange lo hi xs = undefined

-- 3.
countPositivesRec :: [Int] -> Int
countPositivesRec theList = length [a | a <- theList, a > 0]

-- countPositives :: [Int] -> Int
-- countPositives list = length [x | x <- list, x > 0]
prop_countPositives :: [Int] -> Bool
prop_countPositives l = undefined

-- 4.
getProduce :: Int -> Int
getProduce a =
  case len == 1 of
    False -> (div (a) (10 ^ (len - 1))) * getProduce (mod a (10 ^ (len - 1)))
    True  -> a
  where
    len = length (show a)

countDigits :: String -> Int
countDigits str = length ([x | x <- str, isDigit x == True])

multDigitsRec :: String -> Int
multDigitsRec str =
  case len == 0 of
    False -> getProduce (read ([x | x <- str, isDigit x == True]) :: Int)
    True  -> 1
  where
    len = countDigits str

-- multDigitsRec :: String -> Int
-- multDigitsRec str = product [digitToInt ch | ch <- str, isDigit ch]
prop_multDigits :: String -> Bool
prop_multDigits xs = undefined

-- These are some helper functions for makeKey and makeKey itself.
-- Exercises continue below.
rotate :: Int -> [Char] -> [Char]
rotate k list
  | 0 <= k && k <= length list = drop k list ++ take k list
  | otherwise = error "Argument to rotate too large or too small"

--  prop_rotate rotates a list of lenght l first an arbitrary number m times,
--  and then rotates it l-m times; together (m + l - m = l) it rotates it all
--  the way round, back to the original list
--
--  to avoid errors with 'rotate', m should be between 0 and l; to get m
--  from a random number k we use k `mod` l (but then l can't be 0,
--  since you can't divide by 0)
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
  where
    l = length str
    m =
      if l == 0
        then 0
        else k `mod` l

alphabet = ['A' .. 'Z']

makeKey :: Int -> [(Char, Char)]
makeKey k = zip alphabet (rotate k alphabet)

-- Ceasar Cipher Exercises
-- =======================
-- 5.
lookUp :: Char -> [(Char, Char)] -> Char
lookUp ch xs =
  if [snd x | x <- xs, fst x == ch] == []
    then ch
    else head [snd x | x <- xs, fst x == ch]

lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec ch xs =
  if length xs == 0
    then ch
    else if fst (head xs) == ch
           then snd (head xs)
           else lookUpRec ch (tail xs)

prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp c k = (lookUp c k) == (lookUpRec c k)

-- 6.
encipher :: Int -> Char -> Char
encipher k ch = lookUp ch (makeKey k)

-- 7.
normalize :: String -> String
normalize a =
  if length a == 0
    then a
    else if head a == ' '
           then normalize (tail a)
           else if isAlpha (head a)
                  then (toUpper (head a)) : (normalize (tail a))
                  else (head a) : (normalize (tail a))

encipherStr :: Int -> String -> String
encipherStr k str = 
  if length str == 0 
    then str
    else [lookUp x (makeKey k) | x <- normalize (str)]

-- 8.
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey a = zip (snd (unzip a)) (fst (unzip a))

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec a =
  if length a == 0
    then a
    else (snd (head a), fst (head a)) : reverseKeyRec (tail a)

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey a = reverseKeyRec a == reverseKey a

-- 9.
decipher :: Int -> Char -> Char
decipher k ch = lookUp ch (makeKey (26 - k))

decipherStr :: Int -> String -> String
decipherStr k str = [lookUp x (makeKey (26 - k)) | x <- normalize (str)]

-- Optional Material
-- =================
-- 10.
contain :: String -> String -> Bool
contain a b =
  if length b == 0
    then True
    else if length a == 0
           then False
           else if head a == head b
                  then if isPrefixOf b a
                         then True
                         else contain (tail a) b
                  else contain (tail a) b

-- 11.
clean ::[(Int, String)] ->[(Int, String)]
clean a = [x|x <-a, (fst x == 0)==False]

candidates :: String -> [(Int, String)]
candidates a =
  clean[ if contain (decipherStr i a) "THE" || contain (decipherStr i a) "AND"
    then (i, (decipherStr i a))
    else (0,"0")
  | i <- [1 .. 26]
  ]

-- 12.
splitEachFive :: String -> [String]
splitEachFive = undefined

prop_transpose :: String -> Bool
prop_transpose = undefined

-- 13.
encrypt :: Int -> String -> String
encrypt = undefined

-- 14.
decrypt :: Int -> String -> String
decrypt = undefined
