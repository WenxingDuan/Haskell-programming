import           Data.Char       (intToDigit)
import           Data.List       (delete, intercalate, intersperse, sortOn)
import           Data.Sort       (uniqueSort)
import           System.CPUTime
import           Test.QuickCheck
import           Text.Printf

data City
  = Perth
  | Brisbane
  | Hobart
  | Adelaide
  | Sydney
  | Darwin
  | Melbourne
  deriving (Ord, Eq, Show)

data Colour
  = Red
  | Green
  | Amber
  deriving (Ord, Eq, Show)

cities = [Perth, Brisbane, Hobart, Adelaide, Sydney, Darwin, Melbourne]

colours = [Red, Green, Amber]

adj :: City -> City -> Bool
adj x y =
  (x, y) `elem`
  [ (Perth, Darwin)
  , (Perth, Adelaide)
  , (Darwin, Adelaide)
  , (Darwin, Brisbane)
  , (Melbourne, Sydney)
  , (Adelaide, Sydney)
  , (Adelaide, Melbourne)
  , (Brisbane, Sydney)
  , (Brisbane, Adelaide)
  ]

type Colouring = City -> Colour -> Bool

eachCityHasAColour :: Colouring -> Bool
eachCityHasAColour colouring =
  and [or [colouring city paint | paint <- colours] | city <- cities]

adjacentCitiesNotSameColour :: Colouring -> Bool
adjacentCitiesNotSameColour paint = and[ not (and [paint a c, paint b c])| a <- cities, b <- cities, adj a b, c <- colours]

-- the code below allows you to use quickCheck to search for
-- a valuation that satsfies your constraints
-- we have 21 (City, Colour) pairs
-- we use 21 booleans to define a colouring
colourBy a b c d e f g h i j k l m n o p q r s t u =
  let pairs = [(city, colour) | city <- cities, colour <- colours]
      choices :: [((City, Colour), Bool)]
      choices =
        zip
          pairs -- pair each pair with one of the 21 booleans
          [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u]
      the [x] = x
   in (\city colour -> the [b | ((x, y), b) <- choices, x == city, y == colour])

-- to get quickCheck to search for a colouring that satisfies
-- the conditions, we hypothesise that one of our conditions always fails
test_prop a b c d e f g h i j k l m n o p q r s t u =
  let paint = colourBy a b c d e f g h i j k l m n o p q r s t u
   in not (adjacentCitiesNotSameColour paint) || not (eachCityHasAColour paint)

-- if you run quickCheck with a large enough number of tests e.g.
--        quickCheck (withMaxSuccess 12345 test_prop)
-- (but 12345 may not be big enough) you should find a
-- counter-example to our test proposition.
----------------------- Clausal Forms -----------------------------------
-----------Clausal Forms------------------------------------------
-- We will be using a particular represntation of CNF ------------
-- Literals are positive or negative atoms -----------------------
data Literal a
  = N a
  | P a
  deriving (Ord, Eq, Show)

newtype Clause a =
  Or [Literal a]
  deriving (Ord, Eq, Show)

newtype Form a =
  And [Clause a]
  deriving (Eq, Show)

type Val a = [Literal a] -- must be consistent

-- a (partial) valuation is a consistent list of literals  --------
-- it cannot contain both P a and N a                      --------
-- we say it asserts each literal in the list              --------
(<&&>) :: Form a -> Form a -> Form a
And xs <&&> And ys = And (xs ++ ys)

canonical :: Ord a => [Clause a] -> [Clause a]
canonical cs = uniqueSort $ map (\(Or xs) -> Or (uniqueSort xs)) cs

neg :: Literal a -> Literal a
neg (P a) = N a
neg (N a) = P a

--------------------------
data Paint =
  Paint City Colour
  deriving (Ord, Eq, Show)

formalEachCityHasColour :: Form Paint
formalEachCityHasColour =
  And [Or [P (Paint city colour) | colour <- colours] | city <- cities]

formalAdjacentCitiesNotSameColour :: Form Paint
formalAdjacentCitiesNotSameColour =
  And
    [ Or [neg (P (Paint a c)), neg (P (Paint b c))]
    | a <- cities
    , b <- cities
    , adj a b
    , c <- colours
    ] ----------------------------

(<<) :: Eq a => [Clause a] -> Literal a -> [Clause a]
cs << x = [Or (delete (neg x) ys) | Or ys <- cs, not $ x `elem` ys]

simple :: Ord a => Form a -> [Val a]
simple (And cs) =
  let search cs =
        case cs of
          [] -> [[]] -- no clauses; trivial solution
          Or []:_ -> [] -- empty clause; no models
          Or (x:_):_ ->
            [x : m | m <- search $ cs << x] ++
            [neg x : m | m <- search $ cs << neg x]
   in search (canonical cs)

------------------------------ latin squares -------------------------
-- conversion of valuation to squares
data Square =
  Square [[Int]]
  deriving (Show)

toSquare :: Int -> Val (Int, Int, Int) -> Square
toSquare n vs =
  let rows = [0 .. n - 1]
      columns = [0 .. n - 1]
      digits = [1 .. n]
   in Square
        [[d | c <- columns, d <- digits, P (r, c, d) `elem` vs] | r <- rows]

--
latin :: Int -> Form (Int, Int, Int)
latin n =
  let rows = [0 .. n - 1]
      columns = [0 .. n - 1]
      digits = [1 .. n]
      everyRowHasEveryDigit =
        And [Or [P (r, c, d) | c <- columns] | r <- rows, d <- digits]
      everyColumnHasEveryDigit =
        And [Or [P (r, c, d) | r <- rows] | c <- columns, d <- digits]
      noSquareHasTwoDigits =
        And
          [ Or [N (r, c, d), N (r, c, d')]
          | r <- rows
          , c <- columns
          , d <- digits
          , d' <- digits
          , d < d'
          ]
      noRowHasARepeatedDigit = undefined
      noColumnHasARepeatedDigit = undefined
   in everyRowHasEveryDigit <&&> everyColumnHasEveryDigit <&&>
      noSquareHasTwoDigits <&&>
      noRowHasARepeatedDigit <&&>
      noColumnHasARepeatedDigit

dpll :: Ord a => Form a -> [Val a]
dpll (And cs) =
  let models cs =
        case prioritise cs of
          [] -> [[]] -- no clauses; trivial solution
          Or []:_ -> [] -- empty clause; no models
  --      Or [x] : _    -> undefined -- unit clause
          Or (x:_):_ ->
            [x : m | m <- models $ cs << x] ++
            [neg x : m | m <- models $ cs << neg x]
   in models (canonical cs)
  where
    prioritise = id

-- this is for you to play with faster versions
speedy :: Eq a => Form a -> [[Literal a]]
speedy (And css) =
  let models [] = [[]]
      models cs = undefined
   in models css

-----------SUDOKU --------------------------
problems =
  [ "000000082600400000000000000400072000500000430000010000000800600081000000020000007"
  , "000000083000014000000200000000320000090000400000700000000006150308000000200000600"
  , "000000083000030010070000000000204000030000600000010000200600405000500700100000000"
  , "000000083020100000000000040000610200800000900004000000060300500100000070000008000"
  , "000000083020700000000000040000610200800000900004000000060300100500000070000008000"
  , "000000083040300000000500060300000400000700500208000000050060100000002000000080000"
  , "000000083400020000000000510002300000700000600000100040000075200010000000000800000"
  , "000000083500400000000100000000020700100000400000008000038600000020070000000000520"
  , "000000083900100000000000020100009700020080000000000100005700400003000060080000000"
  , "000000084000100000200000000000600130408000000050000000560000200000080007010030000"
  ]

sudoku :: Form (Int, Int, Int)
sudoku
  -- every square is filled
 =
  And [Or [P (i, j, k) | k <- [1 .. 9]] | i <- [1 .. 9], j <- [1 .. 9]] <&&> -- every row contains every digit
  And [Or [P (i, j, k) | j <- [1 .. 9]] | i <- [1 .. 9], k <- [1 .. 9]] <&&> -- every column contains every digit
  And [Or [P (i, j, k) | i <- [1 .. 9]] | j <- [1 .. 9], k <- [1 .. 9]] <&&> -- every big square contains every digit
  And
    [ Or [P (3 * p + q, 3 * r + s, k) | q <- [1 .. 3], s <- [1 .. 3]]
    | p <- [0 .. 2]
    , r <- [0 .. 2]
    , k <- [1 .. 9]
    ] <&&> -- no square is filled twice
  And
    [ Or [N (i, j, k), N (i, j, k')]
    | i <- [1 .. 9]
    , j <- [1 .. 9]
    , k <- [1 .. 9]
    , k' <- [1 .. (k - 1)]
    ] <&&> -- no row has repeated digit
  And
    [ Or [N (i, j, k), N (i, j', k)]
    | i <- [1 .. 9]
    , k <- [1 .. 9]
    , j <- [1 .. 9]
    , j' <- [1 .. (j - 1)]
    ] <&&> -- no column has repeated digit
  And
    [ Or [N (i, j, k), N (i', j, k)]
    | j <- [1 .. 9]
    , k <- [1 .. 9]
    , i <- [1 .. 9]
    , i' <- [1 .. (i - 1)]
    ] <&&> -- no bigsquare has repeated digit
  And
    [ Or [N (3 * p + q, 3 * r + s, k), N (3 * p + q', 3 * r + s', k)]
    | p <- [0 .. 2]
    , r <- [0 .. 2]
    , k <- [1 .. 9]
    , q <- [1 .. 3]
    , s <- [1 .. 3]
    , q' <- [1 .. q]
    , s' <- [1 .. 3]
    , q' < q || s' < s
    ]

entries :: String -> Form (Int, Int, Int)
entries str =
  And
    [ Or [P (i, j, k)]
    | i <- [1 .. 9]
    , j <- [1 .. 9]
    , k <- [1 .. 9]
    , str !! (9 * i + j - 10) == intToDigit k
    ]

showEntries :: [Literal (Int, Int, Int)] -> String
showEntries lits =
  let pos = [a | P a <- lits]
   in [ (intToDigit . last) [k | k <- [0 .. 9], (i, j, k) `elem` pos || k == 0]
      | i <- [1 .. 9]
      , j <- [1 .. 9]
      ]

-- pretty takes an 81 digit string and presents it in sudoku form
-- using unicode -- suitable for putStrLn
pretty :: String -> String
pretty =
  ((tl ++ dsh ++ dn ++ dsh ++ dn ++ dsh ++ tr ++ "\n" ++ vt ++ " ") ++) .
  (++ (" " ++ vt ++ " \n" ++ bl ++ dsh ++ up ++ dsh ++ up ++ dsh ++ br)) .
  intercalate
    (" " ++
     vt ++
     "\n" ++ vl ++ dsh ++ pl ++ dsh ++ pl ++ dsh ++ vr ++ " \n" ++ vt ++ " ") .
  map (intercalate (" " ++ vt ++ "\n" ++ vt ++ " ")) .
  byThree .
  map (intercalate (" " ++ vt ++ " ")) .
  byThree .
  map (intersperse ' ') .
  byThree .
  map
    (\d ->
       if d == '0'
         then '\x005F'
         else d)
  where
    byThree :: [a] -> [[a]]
    byThree (a:b:c:xs) = [a, b, c] : byThree xs
    byThree []         = []
    tl = "\x250F" -- topleft
    tr = "\x2513" -- topright
    bl = "\x2517" -- botleft
    br = "\x251B" -- botright
    dn = "\x2533"
    up = "\x253B"
    vl = "\x2523" -- vertleft
    vr = "\x252B" -- vertright
    vt = "\x2503" -- vertical
    pl = "\x254B" -- plus
    dsh = take 7 $ repeat '\x2501'

-- you can compile this program with
-- ghc -O2 CL6answers.hs
-- then run it with
-- ./CL6answers < sudokuExamples.txt
-- it will use your dpll program to
-- solve the 100 examples provided
-- You can replace dpll with speedy below
-- when you want to test your own variations.
main = do
  p <- getLine
  if length p == 81
    then do
      putStrLn "\nSudoku Problem:"
      putStrLn $ pretty p
      let problem = entries p <&&> sudoku
      start <- getCPUTime
      let solutions = dpll problem
      if null solutions
        then putStrLn "No Solution"
        else return ()
      end <- getCPUTime
      let diff = (fromIntegral (end - start)) / (10 ^ 12)
      printf "First solution: %0.3f sec\n" (diff :: Double)
      let report =
            case solutions of
              [] -> []
              [s] ->
                ["Unique Solution"] ++ [(pretty . showEntries) (head solutions)]
              s:ss ->
                ["Multiple Solutions : showing five"] ++
                map (pretty . showEntries) (take 5 solutions)
      putStrLn (unlines report)
      main
    else return ()
