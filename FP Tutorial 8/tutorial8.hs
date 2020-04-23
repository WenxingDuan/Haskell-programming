-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 8
--
-- Week 8(04-08 Nov.)
module Tutorial8 where

import           System.Random

-- Importing the keymap module
import           KeymapTree

-- Type declarations
type Barcode = String

type Product = String

type Unit = String

type Item = (Product, Unit)

type Catalogue = Keymap Barcode Item



-- A little test catalog
testDB :: Catalogue
testDB =
  fromList
    [ ( "0265090316581"
      , ("The Macannihav'nmor Highland Single Malt", "75ml bottle"))
    , ("0903900739533", ("Bagpipes of Glory", "6-CD Box"))
    , ( "9780201342758"
      , ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book"))
    , ("0042400212509", ("Universal deep-frying pan", "pc"))
    ]

-- Exercise 1
longestProductLen :: [(Barcode, Item)] -> Int
longestProductLen a = longest [b | (c, (b, d)) <- a]
  where
    longest :: [Product] -> Int
    longest (x:y:ys) =
      if (length x) > (length y)
        then longest (x : ys)
        else longest (y : ys)
    longest (x:y) =
      if (length x) > (length y)
        then length x
        else length y

formatLine :: Int -> (Barcode, Item) -> String
formatLine a (barcode, (product, uni)) =
  barcode ++ "..." ++ (product_length product a) ++ "..." ++ uni
  where
    product_length :: Product -> Int -> String
    product_length product a =
      if (length product) >= a
        then take a product
        else product ++ (replicate (a - length product) '.')

showCatalogue :: Catalogue -> IO ()
showCatalogue a = mapM_ putStrLn [formatLine 10 c | c <- (toList a)]

--type Catalogue = Keymap Barcode Item
-- Exercise 2
maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just c) = [c]

listToMaybe :: [a] -> Maybe a
listToMaybe []  = Nothing
listToMaybe [c] = Just c

catMaybes :: [Maybe a] -> [a]
catMaybes a = [x | (Just x) <- a]

-- Exercise 3
getItems :: [Barcode] -> Catalogue -> [Item]
getItems code a = catMaybes [get theCode a | theCode <- code]

-- Exercise 4
-- For Exercises 6-10 check KeymapTree.hs
-- Exercise 12
-- Input-output ------------------------------------------
readDB :: IO Catalogue
readDB = do
  dbl <- readFile "database.csv"
  let db = fromList (map readLine $ lines dbl)
  putStrLn (size db >= 0 `seq` "Done")
  return db

readLine :: String -> (Barcode, Item)
readLine str = (a, (c, b))
  where
    (a, str2) = splitUpon ',' str
    (b, c) = splitUpon ',' str2

splitUpon :: Char -> String -> (String, String)
splitUpon _ "" = ("", "")
splitUpon c (x:xs)
  | x == c = ("", xs)
  | otherwise = (x : ys, zs)
  where
    (ys, zs) = splitUpon c xs

getSample :: Catalogue -> IO Barcode
getSample db = do
  g <- newStdGen
  return $ fst $ toList db !! fst (randomR (0, size db - 1) g)
