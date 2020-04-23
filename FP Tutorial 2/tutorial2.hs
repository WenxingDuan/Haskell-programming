-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 2
--
-- Week 2(23-27 Sep.)
import           Control.Monad   (guard)
import           Data.Char
import           Data.List
import           Data.String
import           Test.QuickCheck

-- 1. halveEvens
-- List-comprehension version
{-isEven :: Int -> Bool
isEven a = (mod a 2) == 0-}
halveEvens :: [Int] -> [Int]
halveEvens xs = [x `div` 2 | x <- xs, even x]

halveEvensReference :: [Int] -> [Int]
halveEvensReference =
  (>>= \x -> guard (x `mod` 2 == 0) >>= \_ -> return $ x `div` 2)

prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvens xs == halveEvensReference xs

inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs =
  case lo > hi of
    True  -> [x | x <- xs, x >= hi && x <= lo]
    False -> [x | x <- xs, x >= lo && x <= hi]

countPositives :: [Int] -> Int
countPositives theList = length [a | a <- theList, a > 0]

{-
readInt :: String -> Int
readInt a = read a :: Int

getNum :: [Char] -> [Char]
getNum a = [x | x <- a, isAlpha x == False]

multDigits :: String -> Int
multDigits str = getProduce (readInt (getNum (str)))
-}
getProduce :: Int -> Int
getProduce a =
  case len == 1 of
    False -> (div (a) (10 ^ (len - 1))) * getProduce (mod a (10 ^ (len - 1)))
    True  -> a
  where
    len = length (show a)

multDigits :: String -> Int
multDigits str =
  case len == 0 of
    False -> getProduce (read ([x | x <- str, isDigit x == True]) :: Int)
    True  -> 1
  where
    len = countDigits str

countDigits :: String -> Int
countDigits str = length ([x | x <- str, isDigit x == True])

prop_multDigits :: String -> Bool
prop_multDigits xs = (multDigits xs) <= 9 ^ (countDigits xs)

capitalise :: String -> String
capitalise s = (toUpper (head s)) : [(toLower x) | x <- (tail s)]

toLowerString :: String -> String
toLowerString a = [(toLower x) | x <- a]

title_mid :: [String] -> [String]
title_mid a
  | length a > 0 =
    case length (head a) > 3 of
      True  -> capitalise (head a) : title_mid (tail a)
      False -> toLowerString (head a) : title_mid (tail a)
  | otherwise = []

title :: [String] -> [String]
title a = capitalise (head a) : title_mid (tail a)

sign :: Int -> Char
sign a
  | a == 0 = '0'
  | a > 0 && a < 10 = '+'
  | a < 0 && a > (-10) = '-'
  | otherwise = error "not in list"

signs :: [Int] -> String
signs a
  | length a == 0 = []
  | head a > (-10) && head a < 10 = sign (head a) : signs (tail a)
  | otherwise = signs (tail a)

isLowerChar :: Char -> Int -> Int
isLowerChar a b =
  case (toUpper a) == a of
    True  -> b + 1
    False -> b

isAEIOU :: Char -> Int -> Int
isAEIOU a b =
  if (a == 'a' ||
      a == 'e' ||
      a == 'i' ||
      a == 'o' ||
      a == 'u' || a == 'A' || a == 'E' || a == 'I' || a == 'O' || a == 'U')
    then b + 1
    else b

isAlphaChar :: Char -> Int
isAlphaChar a =
  if isAlpha a
    then 1
    else 0

score :: Char -> Int
score a =
  case isAlpha a of
    True  -> (isLowerChar a (isAEIOU a (isAlphaChar a)))
    False -> 0

totalScore :: String -> Int
totalScore a = product [score b | b <- a, isAlpha b]

prop_totalScore_pos :: String -> Bool
prop_totalScore_pos a = (totalScore a) >= 1

pennypincher :: [Int] -> Int
pennypincher a = ((sum [x | x <- a]) * 9) `div` (10)

prop_pennypincher :: [Int] -> Bool
prop_pennypincher xs =
  if sum xs <= 0
    then True
    else pennypincher xs <= sum xs

crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind letter pos len words =
  if length words == 0
    then []
    else if ((head words) !! pos == letter) && (length (head words) == len)
           then (head words) : (crosswordFind letter pos len (tail words))
           else crosswordFind letter pos len (tail words)

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

--search :: String -> Char -> [Int]
--search str goal = []
prop_contain :: String -> Bool
prop_contain a =
  if length a == 0
    then True
    else contain a (tail a)
{-
-- This is for testing only. Do not try to understand this (yet).
halveEvensReference :: [Int] -> [Int]
halveEvensReference = (>>= \x -> guard (x `mod` 2 == 0) >>= \_ -> return $ x `div` 2)

crosswordFind 'k' 1 7 ["funky", "fabulous", "kite", "icky", "ukelele"]

-- -- Mutual test
prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = undefined


-- 2. inRange

-- List-comprehension version
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = undefined


-- 3. countPositives: sum up all the positive numbers in a list

-- List-comprehension version
countPositives :: [Int] -> Int
countPositives list = undefined


-- 4. multDigits

-- List-comprehension version
multDigits :: String -> Int
multDigits str = undefined

countDigits :: String -> Int
countDigits str = undefined

prop_multDigits :: String -> Bool
prop_multDigits xs = undefined


-- 5. capitalise

-- List-comprehension version
capitalise :: String -> String
capitalise s = undefined


-- 6. title

lowercase :: String -> String
lowercase xs = undefined

-- List-comprehension version
title :: [String] -> [String]
title _ = undefined


-- 7. signs

sign :: Int -> Char
sign i = undefined

signs :: [Int] -> String
signs xs = undefined


-- 8. score

score :: Char -> Int
score x  = undefined

totalScore :: String -> Int
totalScore xs = undefined

prop_totalScore_pos :: String -> Bool
prop_totalScore_pos xs = undefined

-- Tutorial Activity
-- 10. pennypincher

-- List-comprehension version.
pennypincher :: [Int] -> Int
pennypincher prices = undefined

-- -- And the test itself
prop_pennypincher :: [Int] -> Bool
prop_pennypincher xs = undefined

-- Optional Material

-- 11. crosswordFind

-- List-comprehension version
crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind letter pos len words = undefined


-- 12. search

-- List-comprehension version

search :: String -> Char -> [Int]
search str goal = undefined

-- Depending on the property you want to test, you might want to change the type signature
prop_search :: String -> Char -> Bool
prop_search str goal = undefined


-- 13. contains

contains :: String -> String -> Bool
contains str substr = undefined

-- Depending on the property you want to test, you might want to change the type signature
prop_contains :: String -> String -> Bool
prop_contains str1 str2 = undefined
-}
