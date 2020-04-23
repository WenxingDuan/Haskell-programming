-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 5
--
-- Week 5(14-18 Oct.)
module Tutorial5 where

import           Data.Char
import           Data.List
import           Data.Ratio
import           Test.QuickCheck

-- 1. Map
-- a.
doubles :: [Int] -> [Int]
doubles = \a -> map doub a
  where
    --doub :: Int -> Int
    doub = \a -> 2 * a

-- b.
penceToPounds :: [Int] -> [Float]
penceToPounds = \a -> map toPounds a
    --toPounds :: Int -> Float
  where
    toPounds b = ((fromIntegral b) * 0.01)

-- c.
uppersComp :: String -> String
uppersComp = \a -> map toUpper a

-- 2. Filter
-- a.
alphas :: String -> String
alphas = \a -> filter isAlpha a

-- b.
above :: Int -> [Int] -> [Int]
above a b = filter isGreaterThan b
    --isGreaterThan :: Int -> Bool
  where
    isGreaterThan c = c > a

-- c.
unequals :: [(Int, Int)] -> [(Int, Int)]
unequals b = filter (\c -> (fst c) /= (snd c)) b
    --isNotEqual :: (Int, Int) -> Bool

-- d.
rmCharComp :: Char -> String -> String
rmCharComp a b = filter (/= a) b

-- 3. Comprehensions vs. map & filter
-- a.
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

largeDoubles' :: [Int] -> [Int]
largeDoubles' a = map (* 2) (filter (> 3) a)

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs = largeDoubles xs == largeDoubles' xs

-- b.
reverseEven :: [String] -> [String]
reverseEven strs = [reverse s | s <- strs, even (length s)]

reverseEven' :: [String] -> [String]
reverseEven' a = filter isEvenLength (map reverse' a)
  where
    reverse' =
      \a ->
        if even (length a)
          then reverse a
          else a
    isEvenLength = \a -> even (length a)

prop_reverseEven :: [String] -> Bool
prop_reverseEven strs = reverseEven strs == reverseEven' strs

-- 4. Foldr
-- a.
andRec :: [Bool] -> Bool
andRec []     = True
andRec (x:xs) = x && andRec xs

andFold :: [Bool] -> Bool
andFold = \a -> foldr (&&) True a

prop_and :: [Bool] -> Bool
prop_and xs = andRec xs == andFold xs

-- b.
concatRec :: [[a]] -> [a]
concatRec [] = []
concatRec b  = head b ++ concatRec (tail b)

concatFold :: [[a]] -> [a]
concatFold b = foldr (++) [] b

prop_concat :: [String] -> Bool
prop_concat strs = concatRec strs == concatFold strs

-- c.
rmCharsRec :: String -> String -> String
rmCharsRec a [] = []
rmCharsRec [] b = b
rmCharsRec a b =
  if contains (head b) a == ' '
    then if head b == ' '
           then if elem ' ' a
                  then rmCharsRec a (tail b)
                  else ' ' : rmCharsRec a (tail b)
           else rmCharsRec a (tail b)
    else head b : rmCharsRec a (tail b)
  where
    contains :: Char -> String -> Char
    contains a [] = a
    contains a b =
      if a == head b
        then ' '
        else contains a (tail b)

rmCharsFold :: String -> String -> String
rmCharsFold [] b = b
rmCharsFold a b = rmCharsFold (tail a) x
  where
    x = foldr contains [] b
    contains :: Char -> String -> String
    contains c d =
      if c /= head a
        then c : d
        else d

prop_rmChars :: String -> String -> Bool
prop_rmChars chars str = rmCharsRec chars str == rmCharsFold chars str

type Matrix = [[Rational]]

-- 5
-- a.
uniform :: [Int] -> Bool
uniform = \a -> filter (/= a !! 0) a == []

-- b.
valid :: Matrix -> Bool
valid =
  \a -> uniform [length x | x <- a] && length a >= 1 && length (a !! 0) >= 1

-- 6.
matrixWidth :: Matrix -> Int
matrixWidth m = length (m !! 0)

matrixHeight :: Matrix -> Int
matrixHeight m = length m

plusM :: [[Rational]] -> [[Rational]] -> [[Rational]]
plusM a b = zipWith (zipWith (+)) a b

-- 7.
timesM :: Matrix -> Matrix -> Matrix
timesM mat1 mat2 =
  [[sum (zipWith (*) xa xb) | xb <- transpose mat1] | xa <- mat1]

-- 8.
-- b.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = undefined

-- c.
zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' f xs ys = undefined

-- -----------------------------------
-- -----------------------------------
-- -- Optional material
-- -----------------------------------
-- -----------------------------------
-- -- 9.
-- Mapping functions
mapMatrix :: (a -> b) -> [[a]] -> [[b]]
mapMatrix f = undefined

zipMatrix :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipMatrix f = undefined

-- All ways of deleting a single element from a list
removes :: [a] -> [[a]]
removes = undefined

-- Produce a matrix of minors from a given matrix
minors :: Matrix -> [[Matrix]]
minors m = undefined

-- A matrix where element a_ij = (-1)^(i + j)
signMatrix :: Int -> Int -> Matrix
signMatrix w h = undefined

determinant :: Matrix -> Rational
determinant = undefined

cofactors :: Matrix -> Matrix
cofactors m = undefined

scaleMatrix :: Rational -> Matrix -> Matrix
scaleMatrix k = undefined

inverse :: Matrix -> Matrix
inverse m = undefined

-- Tests
identity :: Int -> Matrix
identity n = undefined

prop_inverse2 :: Rational -> Rational -> Rational -> Rational -> Property
prop_inverse2 a b c d = undefined

type Triple a = (a, a, a)

prop_inverse3 ::
     Triple Rational -> Triple Rational -> Triple Rational -> Property
prop_inverse3 r1 r2 r3 = undefined
