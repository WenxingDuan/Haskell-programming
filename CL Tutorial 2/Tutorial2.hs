import           Test.QuickCheck

-- Negation on predicates
neg :: (Thing -> Bool) -> (Thing -> Bool)
neg a x = not (a x)

(|=) :: (Thing -> Bool) -> (Thing -> Bool) -> Bool
(|=) a b = and [b x | x <- things, a x]

(|/=) :: (Thing -> Bool) -> (Thing -> Bool) -> Bool
(|/=) a b = not (a |= b)

--(|/=) a b = not (and [b x | x <- things, a x])
(|:|) :: (Thing -> Bool) -> (Thing -> Bool) -> (Thing -> Bool)
(|:|) a b x = (a x) || (b x)

(&:&) :: (Thing -> Bool) -> (Thing -> Bool) -> (Thing -> Bool)
(&:&) a b x = (a x) && (b x)

-- Implementation details
data Thing
  = R
  | S
  | T
  | U
  | V
  | W
  | X
  | Y
  | Z
  deriving (Eq, Show)

things :: [Thing]
things = [R, S, T, U, V, W, X, Y, Z]

isRed :: Thing -> Bool
isBlue :: Thing -> Bool
isGreen :: Thing -> Bool
isDisc :: Thing -> Bool
isTriangle :: Thing -> Bool
isSmall :: Thing -> Bool
isBig :: Thing -> Bool
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

instance CoArbitrary Thing where
  coarbitrary R = variant 0
  coarbitrary S = variant 1
  coarbitrary T = variant 2
  coarbitrary U = variant 3
  coarbitrary V = variant 4
  coarbitrary W = variant 5
  coarbitrary X = variant 6
  coarbitrary Y = variant 7
  coarbitrary Z = variant 8

instance Show (a -> b) where
  show f = "Sorry, your implementation is not quite correct:("

infixr 2 |:|

infixr 3 &:&

propEntail :: (Thing -> Bool) -> (Thing -> Bool) -> Bool
propEntail a b = (a |= b) == (foldr (&&) True $ map b $ filter a things)

propNotEntail :: (Thing -> Bool) -> (Thing -> Bool) -> Bool
propNotEntail a b =
  (a |/= b) == (foldr (||) False $ map (not . b) $ filter a things)
