-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 8
--
-- Week 8(04-08 Nov.)
-- Indexed data represented as a tree
module KeymapTree
  ( Keymap
  , size
  , depth
  , get
  , set
  , del
  , select
  , toList
  , fromList
  , merge
  , filterLT
  , filterGT
  ) where

import           Control.Monad
import           Data.List

-- Modules for testing
import           Test.QuickCheck

-- The data type
data Keymap k a
  = Leaf
  | Node k a (Keymap k a) (Keymap k a)

-- A test tree
testTree :: Keymap Int Int
testTree =
  Node 2 20 (Node 1 10 Leaf Leaf) (Node 3 30 Leaf (Node 4 40 Leaf Leaf))

-- Exercise 6
size :: Ord k => Keymap k a -> Int
size Leaf                  = 0
size (Node _ _ left right) = 1 + size left + size right

depth :: Ord k => Keymap k a -> Int
depth Leaf = 0
depth (Node _ _ left right) =
  if depth left > depth right
    then depth left + 1
    else depth right + 1

-- Exercise 7
toList :: Ord k => Keymap k a -> [(k, a)]
toList (Leaf)                = []
toList (Node k a left right) = [(k, a)] ++ (toList left) ++ (toList right)

-- Exercise 8
{-
set :: Ord k => k -> a -> Keymap k a -> Keymap k a
set key value c = f c
  where
    f Leaf = Node key value Leaf Leaf
    f (Node k v left right)
      | key == k = Node k value left right
      | key <= k = Node k v (f left) right
      | otherwise = Node k v left (f right)
-}
set :: Ord k => k -> a -> Keymap k a -> Keymap k a
set key value = f
  where
    f Leaf                  = Node key value Leaf Leaf
    f (Node k v left right) = Node k v (Node key value Leaf Leaf) right

-- Exercise 9
get :: Ord k => k -> Keymap k a -> Maybe a
get key (Node k v left right)
  | key == k = Just v
  | key > k = get key right
  | otherwise = get key left
get key Leaf = Nothing

prop_set_get :: Int -> Int -> Bool
prop_set_get k v = get k (set k v testTree) == Just v

-- Exercise 10
fromList :: Ord k => [(k, a)] -> Keymap k a
fromList [] = Leaf
fromList a  = set (fst (head a)) (snd (head a)) (fromList (tail a))

prop_toList_fromList :: [Int] -> [Int] -> Bool
prop_toList_fromList xs ys = sort (toList (fromList zs)) == sort zs
  where
    zs = zip (nub xs) ys

prop_toList_fromList_sorted :: [Int] -> [Int] -> Bool
prop_toList_fromList_sorted xs ys = toList (fromList zs) == sort zs
  where
    zs = zip (nub xs) ys

-- Optional Material -----------------------------------
-- Exercise 13
filterLT :: Ord k => k -> Keymap k a -> Keymap k a
filterLT = undefined

filterGT :: Ord k => k -> Keymap k a -> Keymap k a
filterGT = undefined

-- Exercise 14
merge :: Ord k => Keymap k a -> Keymap k a -> Keymap k a
merge = undefined

-- Exercise 15
del :: Ord k => k -> Keymap k a -> Keymap k a
del = undefined

-- Exercise 16
select :: Ord k => (a -> Bool) -> Keymap k a -> Keymap k a
select = undefined

-- Instances for QuickCheck -----------------------------
instance (Ord k, Show k, Show a) => Show (Keymap k a) where
  show = show . toList

instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (Keymap k a) where
  arbitrary = liftM fromList $ liftM2 zip (liftM nub arbitrary) arbitrary
