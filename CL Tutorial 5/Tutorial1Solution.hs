module Tutorial1Solution where
-- EXERCISE 1: BASIC TYPES IN HASKELL

-- True, False, 1, 2, 3.14, 'A', 'b', ' '
-- [True, True, False], [1,2,3], [1,2,3.14], ['A', 'b', ' '], "Ab "
-- (1,2), (1, 2, 3)
-- Just "Me", Nothing

-------------------------------------------------

-- EXERCISE 2: ROCK, PAPER, SCISSORS

data Move = Rock | Paper | Scissors deriving (Eq, Show)

-- Below is the definition of Ordering. 
-- Do NOT uncomment the line below!!!
-- data Ordering = LT | EQ | GT

order :: Move -> Move -> Ordering
order Rock Scissors = GT
order Paper Rock = GT
order Scissors Paper = GT
order Rock Rock = EQ
order Paper Paper = EQ
order Scissors Scissors = EQ
order _ _ = LT

-------------------------------------------------

-- EXERCISE 3: FRUITS AND THEIR COLOURS

data Fruit = Strawberry | Plum | Pear | Mandarin | 
             Grapes | Apple | Banana deriving (Show)

data Colour = Red | Purple | Yellow | Green | Orange deriving (Show, Eq)

colourOf :: Fruit -> Colour
colourOf Strawberry = Red
colourOf Plum = Purple
colourOf Pear = Green 
colourOf Mandarin = Orange
colourOf Grapes = Purple 
colourOf Apple = Red
colourOf Banana = Yellow

isRedFruit :: Fruit -> Bool
isRedFruit fruit = colourOf fruit == Red

isPurpleFruit :: Fruit -> Bool
isPurpleFruit fruit = colourOf fruit == Purple

isYellowFruit :: Fruit -> Bool
isYellowFruit fruit = colourOf fruit == Yellow

isGreenFruit :: Fruit -> Bool
isGreenFruit fruit = colourOf fruit == Green

isOrangeFruit :: Fruit -> Bool
isOrangeFruit fruit = colourOf fruit == Orange

-------------------------------------------------

-- EXERCISE 4: An Universe of Discs and Triangles

data Thing = R | S | T | U | V | W | X | Y | Z deriving (Show)
                                                         
things :: [Thing]
things = [R, S, T, U, V, W, X, Y, Z]

isRed      :: Thing -> Bool
isBlue     :: Thing -> Bool
isGreen    :: Thing -> Bool
isDisc     :: Thing -> Bool
isTriangle :: Thing -> Bool
isSmall    :: Thing -> Bool
isBig      :: Thing -> Bool

s1 = or [isRed x | x <- things, isBig x, isDisc x]
s2 = and [isSmall x | x <- things, isRed x, isTriangle x]
s3 = or [isGreen x | x <- things, isBig x, isTriangle x]
s4 = and [isRed x | x <- things, isSmall x, isDisc x]
s5 = not (or [isBlue x | x <- things, isRed x]) 




-- Implementation Details
isRed U = True
isRed V = True
isRed _ = False

isBlue T = True
isBlue X = True
isBlue Z = True
isBlue _ = False

isGreen x = not (isRed x || isBlue x)

isDisc R = True
isDisc U = True
isDisc Y = True
isDisc Z = True
isDisc _ = False

isTriangle x = not (isDisc x)

isSmall R = True
isSmall S = True
isSmall V = True
isSmall X = True
isSmall _ = False

isBig t = not (isSmall t)


