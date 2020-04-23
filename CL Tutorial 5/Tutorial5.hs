import           Data.Char         (intToDigit, isDigit)
import qualified Data.Map.Strict   as Map
import           System.Random
import           Tutorial1Solution

every :: [t] -> (t -> Bool) -> Bool
some :: [t] -> (t -> Bool) -> Bool
every xs f = and [f x | x <- xs]

some ys f = or [f y | y <- ys]

neg :: (t -> Bool) -> (t -> Bool)
neg f x = not $ f x

data Person
  = Carmina
  | Malika
  | Margherita
  | Ginny
  | Loida
  | Rosalie
  | Nikki
  | Bea
  | Delores
  | Soledad
  | Sheba
  | Elsy
  | Lashanda
  | Carolynn
  | Easter
  | Lakenya
  | Glenda
  | Almeda
  | Crissy
  | Birgit
  | Lashaun
  | Ashely
  | Araceli
  | Nanci
  | Flo
  | Cristina
  | Myriam
  | Randa
  | Danika
  | Irene
  | Macy
  | Larue
  | Melaine
  | Jaye
  | Madalene
  | Catalina
  | Danille
  | Adrien
  | Mayra
  | Donella
  | Lajuana
  | Setsuko
  | Maile
  | Leontine
  | Jeanne
  | Yesenia
  | Sheri
  | Ladonna
  | Samira
  | Georgianne
  | Joey
  | Derick
  | Irwin
  | Truman
  | Alphonso
  | Colton
  | Clifton
  | Merle
  | Dewitt
  | Reid
  | Julius
  | Glenn
  | Alfonso
  | Freddy
  | Delmer
  | Bennie
  | Yong
  | Matthew
  | Irving
  | Danny
  | Darrell
  | Lazaro
  | Johnathon
  | Marion
  | Isreal
  | Emil
  | Benjamin
  | Stanton
  | Rudolf
  | Bradly
  | Elbert
  | Wilton
  | Noble
  | Ty
  | Jon
  | Brian
  | Dallas
  | Josh
  | Randall
  | Erich
  | Jackson
  | Sidney
  | Damien
  | Keven
  | Norman
  | Cristopher
  | Ricky
  | Cordell
  | Wilber
  | Rashad
  deriving (Eq, Ord, Show)

people :: [Person]
people =
  [ Carmina
  , Malika
  , Margherita
  , Ginny
  , Loida
  , Rosalie
  , Nikki
  , Bea
  , Delores
  , Soledad
  , Sheba
  , Elsy
  , Lashanda
  , Carolynn
  , Easter
  , Lakenya
  , Glenda
  , Almeda
  , Crissy
  , Birgit
  , Lashaun
  , Ashely
  , Araceli
  , Nanci
  , Flo
  , Cristina
  , Myriam
  , Randa
  , Danika
  , Irene
  , Macy
  , Larue
  , Melaine
  , Jaye
  , Madalene
  , Catalina
  , Danille
  , Adrien
  , Mayra
  , Donella
  , Lajuana
  , Setsuko
  , Maile
  , Leontine
  , Jeanne
  , Yesenia
  , Sheri
  , Ladonna
  , Samira
  , Georgianne
  , Joey
  , Derick
  , Irwin
  , Truman
  , Alphonso
  , Colton
  , Clifton
  , Merle
  , Dewitt
  , Reid
  , Julius
  , Glenn
  , Alfonso
  , Freddy
  , Delmer
  , Bennie
  , Yong
  , Matthew
  , Irving
  , Danny
  , Darrell
  , Lazaro
  , Johnathon
  , Marion
  , Isreal
  , Emil
  , Benjamin
  , Stanton
  , Rudolf
  , Bradly
  , Elbert
  , Wilton
  , Noble
  , Ty
  , Jon
  , Brian
  , Dallas
  , Josh
  , Randall
  , Erich
  , Jackson
  , Sidney
  , Damien
  , Keven
  , Norman
  , Cristopher
  , Ricky
  , Cordell
  , Wilber
  , Rashad
  ]

isHappy :: Person -> Bool
isScientist :: Person -> Bool
isArtist :: Person -> Bool
isPolitician :: Person -> Bool
loves :: Person -> Person -> Bool
---------------------------------------------------------------------
-- Implementation details that you don't need to know
-- Though you are welcome to ask questions regarding them
-- Ex1
likes R T = True
likes R W = True
likes S R = True
likes S X = True
likes T W = True
likes T U = True
likes U W = True
likes U Y = True
likes V T = True
likes V W = True
likes W U = True
likes W Z = True
likes X Y = True
likes X Z = True
likes Y U = True
likes Y V = True
likes Z S = True
likes Z Y = True
likes _ _ = False

-- Ex2
-- allPairs are NOT 100 choose 2, 100C2 = 4950
-- allPairs include (x,x) and (x,y)/(y,x)
--        It is reflexive and symmetric.
-- Thus, 100 + 5950 + 4950 = 10000, which is also length allPairs
-- Graph is directed and one can loves oneselef.
allPairs = [(x, y) | x <- people, y <- people]

-- Make random edges, there is an edge if True given a key
-- It really is deterministic with seed
-- Change the seed value for a different graph
gen :: Int -> Int -> Map.Map Person Bool
gen prob seed =
  Map.fromList $ zip people (map (== 1) (randomRs (1, prob) (mkStdGen seed)))

happy = gen 2 47

scientist = gen 2 42

politician = gen 10 46

artist = gen 3 33

-- Special case for the pairs
loveRel :: Map.Map (Person, Person) Bool
loveRel =
  Map.fromList $
  zip allPairs (map (== 1) (randomRs (1 :: Int, 6) (mkStdGen 21)))

isHappy = (happy Map.!)

isScientist = (scientist Map.!)

isArtist = (artist Map.!)

isPolitician = (politician Map.!)

loves a b = loveRel Map.! (a, b)

-- sudoku ----------------------
entries :: String -> Int -> Int -> Int -> Bool
entries str i j k
  | length str /= 81 || or [not (isDigit c) | c <- str] =
    error "Ill-formed string"
  | or [arg < 1 || arg > 9 | arg <- [i, j, k]] =
    error "index out of range [1..9]"
  | otherwise = k > 0 && str !! (9 * (i - 1) + (j - 1)) == intToDigit k

prob1 =
  entries
    "798342615365819472421675938679458321254193867183726594547931286832567149916284753"

prob2 =
  entries
    "792348615836915472451672938679854321245193867183726594567431289928567143314289765"

prob3 =
  entries
    "792348615836915472451672938679854321245193867183726594567431289928567143314289760"

check :: (Int -> Int -> Int -> Bool) -> Bool
check entered
  -- every square is filled
 =
  and [or [entered i j k | k <- [1 .. 9]] | i <- [1 .. 9], j <- [1 .. 9]] && -- no square is filled twice
  and
    [ or [not (entered i j k), not (entered i j k')]
    | i <- [1 .. 9]
    , j <- [1 .. 9]
    , k <- [1 .. 9]
    , k' <- [1 .. 9]
    ] && -- every row contains every digit
  and [or [entered i j k | j <- [1 .. 9]] | i <- [1 .. 9], k <- [1 .. 9]]
  -- every column contains every digit
  -- every big square contains every digit

reach1 :: Person -> Person -> Bool
reach1 x z = x `loves` z

reach2 :: Person -> Person -> Bool
reach2 x n = or [y `loves` n | y <- people, reach1 x y]

reach3 :: Person -> Person -> Bool
reach2 x n = or [y `loves` n | y <- people, reach2 x y]
