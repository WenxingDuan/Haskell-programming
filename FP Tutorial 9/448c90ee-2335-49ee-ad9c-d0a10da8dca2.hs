-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 9
--
-- Week 9(11-15 Nov.)
module Tutorial9 where

import           Data.Char
import           Data.List
import           Test.QuickCheck

-- Type declarations
type FSM q = ([q], Alphabet, [q], [q], [Transition q])

type Alphabet = [Char]

type Transition q = (q, Char, q)

-- Example machines
m1 :: FSM Int
m1 =
  ( [0, 1, 2, 3, 4]
  , ['a', 'b']
  , [0]
  , [4]
  , [ (0, 'a', 1)
    , (0, 'b', 1)
    , (0, 'a', 2)
    , (0, 'b', 2)
    , (1, 'b', 4)
    , (2, 'a', 3)
    , (2, 'b', 3)
    , (3, 'b', 4)
    , (4, 'a', 4)
    , (4, 'b', 4)
    ])

m2 :: FSM Char
m2 =
  ( ['A', 'B', 'C', 'D']
  , ['0', '1']
  , ['B']
  , ['A', 'B', 'C']
  , [ ('A', '0', 'D')
    , ('A', '1', 'B')
    , ('B', '0', 'A')
    , ('B', '1', 'C')
    , ('C', '0', 'B')
    , ('C', '1', 'D')
    , ('D', '0', 'D')
    , ('D', '1', 'D')
    ])

dm1 :: FSM [Int]
dm1 =
  ( [[], [0], [1, 2], [3], [3, 4], [4]]
  , ['a', 'b']
  , [[0]]
  , [[3, 4], [4]]
  , [ ([], 'a', [])
    , ([], 'b', [])
    , ([0], 'a', [1, 2])
    , ([0], 'b', [1, 2])
    , ([1, 2], 'a', [3])
    , ([1, 2], 'b', [3, 4])
    , ([3], 'a', [])
    , ([3], 'b', [4])
    , ([3, 4], 'a', [4])
    , ([3, 4], 'b', [4])
    , ([4], 'a', [4])
    , ([4], 'b', [4])
    ])

-- 1.
states :: FSM q -> [q]
alph :: FSM q -> Alphabet
start :: FSM q -> [q]
final :: FSM q -> [q]
trans :: FSM q -> [Transition q]
states (k, _, _, _, _) = k

alph (_, a, _, _, _) = a

start (_, _, s, _, _) = s

final (_, _, _, f, _) = f

trans (_, _, _, _, t) = t

-- 2.
delta :: (Eq q) => FSM q -> [q] -> Char -> [q]
delta m s symbol = [a | (c, b, a) <- trans m, ss <- s, c == ss, symbol == b]

-- 3.
accepts :: (Eq q) => FSM q -> String -> Bool
accepts (_, _, [], _, _) _ = False
accepts (_, _, ss, fs, _) "" = or [q `elem` ss | q <- fs]
accepts thefsm (x:xs) =
  accepts
    ( states thefsm
    , alph thefsm
    , (delta thefsm (start thefsm) x)
    , final thefsm
    , trans thefsm)
    xs

{-
  where
    step :: (Eq q) => FSM q -> Char -> FSM q
    step theFSM theChar =
      ( states theFSM
      , alph theFSM
      , [ a
        | (c, b, a) <- (trans theFSM)
        , b == theChar
        , c == (start theFSM) !! 0
        ]
      , final theFSM
      , trans theFSM)
-}
{-accepts :: (Ord q) => FSM q -> String -> Bool
accepts (FSM _ _ _ [] _) _   = False
accepts (FSM _ _ _ ss fs) "" = or [q `elem` ss | q <- fs]
accepts fsm (x:xs)           = accepts (step fsm x) xs-}
acceptsFrom :: (Eq q) => FSM q -> [q] -> String -> Bool
acceptsFrom m q ""     = or [r `elem` final m | r <- q]
acceptsFrom m q (x:xs) = undefined

-- 4.
canonical :: (Ord q) => [q] -> [q]
canonical a = sort (nub a)

-- 5.
ddelta :: (Ord q) => FSM q -> [q] -> Char -> [q]
ddelta a b c = canonical (delta a b c)

-- 6.
next :: (Ord q) => FSM q -> [[q]] -> [[q]]
next a b = nub (b ++ [ddelta a bb cc | bb <- b, cc <- alph a])

-- 7.
reachable :: (Ord q) => FSM q -> [[q]] -> [[q]]
reachable a b =
  if next a b == next a d
    then next a b
    else nub (next a b ++ e)
  where
    d = next a b
    e = reachable a (next a b)

-- 8.
dfinal :: (Ord q) => FSM q -> [[q]] -> [[q]]
dfinal a b = [c | c <- b, d <- c, elem d (final a)]

-- 9.
dtrans :: (Ord q) => FSM q -> [[q]] -> [Transition [q]]
dtrans a b = [(c, alpha, (ddelta a c alpha)) | c <- b, alpha <- (alph a)]

-- 10.
deterministic :: (Ord q) => FSM q -> FSM [q]
deterministic a =
  ( reachable a [(start a)]
  , (alph a)
  , [(start a)]
  , (dfinal a (reachable a [(start a)]))
  , (dtrans a (reachable a [(start a)])))

--(the_states, the_alph, the_start, the_final, the_trans)
-- Optional Material
-- QuickCheck
safeString :: String -> String
safeString a = filter (`elem` ['a' .. 'z']) (map toLower a)

--11.
charFSM :: Char -> FSM Bool
charFSM = undefined

emptyFSM :: FSM ()
emptyFSM = undefined

--12.
concatFSM :: (Ord q, Ord q') => FSM q -> FSM q' -> FSM (Either q q')
concatFSM a b = undefined

prop_concatFSM :: String -> String -> String -> Bool
prop_concatFSM m n o = accepts fsm (s ++ t) && (accepts fsm u == (s ++ t == u))
  where
    fsm = concatFSM a b
    a = stringFSM s
    b = stringFSM t
    s = safeString m
    t = safeString n
    u = safeString o

--13.
intFSM :: Ord q => FSM q -> FSM Int
intFSM a = undefined

lookUp :: Eq q => q -> [(q, Int)] -> Int
lookUp q' qis = the [i | (q, i) <- qis, q == q']
  where
    the [q] = q

stringFSM :: String -> FSM Int
stringFSM = undefined

prop_stringFSM m n = accepts a s && accepts a t == (s == t)
  where
    a = stringFSM s
    s = safeString m
    t = safeString n

--14.
completeFSM :: (Ord q) => FSM q -> FSM (Maybe q)
completeFSM = undefined

unionFSM :: (Ord q, Ord q') => FSM q -> FSM q' -> FSM (Maybe q, Maybe q')
unionFSM = undefined

prop_unionFSM :: String -> String -> String -> Bool
prop_unionFSM m n o =
  accepts fsm u == (accepts a u || accepts b u) &&
  accepts fsm s && accepts fsm t
  where
    fsm = unionFSM a b
    a = stringFSM s
    b = stringFSM t
    c = stringFSM u
    s = safeString m
    t = safeString n
    u = safeString o

--15.
star :: (Ord q) => FSM q -> FSM q
star = undefined

prop_star :: String -> Int -> Bool
prop_star m n = accepts fsm (concat (replicate i s))
  where
    fsm = star (stringFSM s)
    s = safeString m
    i = abs n

--16.
complementFSM :: (Ord q) => FSM q -> FSM (Maybe q)
complementFSM = undefined

prop_complement :: String -> String -> Bool
prop_complement m n = not (accepts fsm s) && accepts fsm t == not (s == t)
  where
    fsm = complementFSM (stringFSM s)
    s = safeString m
    t = safeString n

intersectFSM :: (Ord q, Ord q') => FSM q -> FSM q' -> FSM (q, q')
intersectFSM = undefined

prop_intersectFSM1 m n = accepts fsm s && accepts fsm t == (s == t)
  where
    fsm = intersectFSM a a
    a = stringFSM s
    s = safeString m
    t = safeString n

prop_intersectFSM2 m n o = accepts fsm u == (accepts a u && accepts b u)
  where
    fsm = intersectFSM a b
    a = stringFSM s
    b = stringFSM t
    s = safeString m
    t = safeString n
    u = safeString o

prop_intersectFSM3 m n o = accepts fsm s && accepts fsm u == accepts a u
  where
    fsm = intersectFSM a (unionFSM a b)
    a = stringFSM s
    b = stringFSM t
    s = safeString m
    t = safeString n
    u = safeString o
