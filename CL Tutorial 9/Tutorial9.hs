import           Data.Char
import           Data.Set        (Set, empty, insert, member, toList)
import qualified Data.Set        as Set
import           Prelude         hiding (lookup)
import           Test.QuickCheck

(\/) :: Ord a => Set a -> Set a -> Set a
(\/) = Set.union

(/\) :: Ord a => Set a -> Set a -> Set a
(/\) = Set.intersection

(\\) :: Ord a => Set a -> Set a -> Set a
(\\) = Set.difference

mapS :: Ord b => (a -> b) -> Set a -> Set b
mapS = Set.map

set :: Ord q => [q] -> Set q
set = Set.fromList

cartesianProduct :: (Ord a, Ord b) => Set a -> Set b -> Set (a, b)
cartesianProduct a b = Set.fromList [(x, y) | x <- xs, y <- ys]
  where
    xs = Set.toList a
    ys = Set.toList b

-- 1.
-- return the set of nodes reached by any number of steps (including 0)
reach' :: Ord q => (Set q -> Set q) -> Set q -> Set q
reach' step gs =
  if gs' == gs
    then gs
    else reach' step gs'
  where
    gs' = gs \/ step gs

reach :: Ord q => (Set q -> Set q) -> Set q -> Set q
reach step qs =
  let add q qss
        | q `member` qss = qss
        | otherwise = undefined
         -- foldr add <done> <(todo>
   in foldr add empty qs

-- labelled transitions
type Sym = Char

type Trans q = (q, Sym, q)

-- -- 2.
toy_ts :: [Trans Int]
toy_ts = [(n, intToDigit m, m * n `mod` 10) | n <- [0 .. 9], m <- [3, 7]]

smallStep :: Set Int -> Set Int
smallStep qs = set [q' | (q, _, q') <- toy_ts, q `member` qs]

-- -- 3.
-- -- oneStep ts qs produces the states reached adjacent to qs
-- -- then can be reached by a single transition from ts
oneStep :: Ord q => [Trans q] -> Set q -> Set q
oneStep ts qs = set [q' | (q, _, q') <- ts, q `member` qs]

-- --   FSM states symbols transitions starting accepting
data FSM q =
  FSM (Set q) (Set Sym) [Trans q] (Set q) (Set q)
  deriving (Show)

mkFSM qs as ts ss fs = FSM qs' as' ts' ss' fs'
  where
    qs' = set qs
    as' = set as
    ts' =
      [t | t@(q, a, q') <- ts, q `member` qs', q' `member` qs', a `member` as']
    ss' = set ss /\ qs'
    fs' = set fs /\ qs'

-- toy examples
g0 :: [Int] -> [Int] -> FSM Int
g0 = mkFSM [0 .. 9] "37" toy_ts

eg0 = g0 [1] [9]

eg1 = g0 [1, 2] [9]

--
-- 4.
reachableFSM :: Ord q => FSM q -> Set q
reachableFSM (FSM _ _ ts ss _) = undefined

-- 5.
pruneFSM :: Ord q => FSM q -> FSM q
pruneFSM fsm@(FSM qs as ts ss fs) = FSM qs' as ts' ss fs'
  where
    qs' = reachableFSM fsm
    ts' = undefined
    fs' = undefined

-- 6.
reverseFSM :: Ord q => FSM q -> FSM q
reverseFSM (FSM qs as ts ss fs) = FSM qs as ts' ss' fs'
  where
    ts' = undefined
    ss' = undefined
    fs' = undefined

prop_reverseFSM :: String -> Bool
prop_reverseFSM s = acceptsFSM (reverseFSM $ stringFSM s) (reverse s)

-- -- 7.
tidyFSM :: Ord q => FSM q -> FSM q
tidyFSM = undefined . undefined . undefined . undefined

-- -- NFA adding epsilon-transitions
data NFA q =
  NFA (Set q) (Set Sym) [Trans q] [(q, q)] (Set q) (Set q)
  deriving (Show)

mkNFA qs as ts es ss fs = NFA qs' as' ts' es' ss' fs'
  where
    qs' = set qs
    as' = set as
    ts' =
      [t | t@(q, a, q') <- ts, q `member` qs', q' `member` qs', a `member` as']
    es' = [e | e@(q, q') <- es, q `member` qs', q' `member` qs', q /= q']
    ss' = set ss /\ qs'
    fs' = set fs /\ qs'

asNFA :: FSM q -> NFA q
asNFA (FSM qs as ts ss fs) = NFA qs as ts [] ss fs

asFSM :: NFA q -> FSM q
asFSM (NFA qs as ts es ss fs)
  | null es = FSM qs as ts ss fs
  | otherwise = error "has e-transittions"

m3 :: NFA Int
m3 =
  mkNFA
    [0 .. 5]
    "ab"
    [(1, 'a', 2), (3, 'b', 4)]
    [(0, 1), (2, 3), (0, 5), (4, 5), (4, 1)]
    [0]
    [5]

-- 8.
eStep :: Ord q => [(q, q)] -> Set q -> Set q
eStep es qs = undefined

eClose :: Ord q => [(q, q)] -> Set q -> Set q
eClose es = reach (eStep es)

-- 9.
-- following only transitions with a given label (cf. ddelta)
ddelta :: (Ord q) => [Trans q] -> Sym -> Set q -> Set q
ddelta ts a = oneStep undefined

-- -- moving the start states
moveFSM :: Ord q => FSM q -> Sym -> FSM q
moveFSM (FSM qs as ts ss fs) x = FSM qs as ts (ddelta ts x ss) fs

acceptsFSM :: (Ord q) => FSM q -> String -> Bool
acceptsFSM (FSM _ _ _ ss fs) "" = (not . Set.null) (ss /\ fs)
acceptsFSM fsm (x:xs)           = acceptsFSM (moveFSM fsm x) xs

-- 10.
eddelta :: (Ord q) => [Trans q] -> [(q, q)] -> Sym -> Set q -> Set q
eddelta ts es a = undefined . undefined . undefined

moveNFA :: Ord q => NFA q -> Sym -> NFA q
moveNFA (NFA qs as ts es ss fs) a = NFA qs as ts es (eddelta ts es a ss) fs

acceptsNFA :: (Ord q) => NFA q -> String -> Bool
acceptsNFA (NFA _ _ _ es ss fs) "" = (not . Set.null) (eClose es ss /\ fs)
acceptsNFA nfa (x:xs)              = acceptsNFA (moveNFA nfa x) xs

-- next is like step, but on superstates
next :: Ord q => [Trans q] -> (Set Sym) -> Set (Set q) -> Set (Set q)
next ts as qqs = set [ddelta ts a qs | qs <- toList qqs, a <- toList as]

fsm2dfa :: Ord q => FSM q -> FSM (Set q)
fsm2dfa (FSM qs as ts ss fs) = FSM qs' as ts' ss' fs'
  where
    qs' = reach (next ts as) ss'
    ts' = [(qs, a, ddelta ts a qs) | qs <- toList qs', a <- toList as]
    ss' = set [ss]
    fs' = set [qs | qs <- toList qs', (not . Set.null) (qs /\ fs)]

-- 11.
-- eNext is like next, but including e-transitions
eNext ::
     Ord q => [Trans q] -> [(q, q)] -> (Set Sym) -> Set (Set q) -> Set (Set q)
eNext ts es as qss = set undefined

-- 12.
nfa2dfa :: Ord q => NFA q -> NFA (Set q)
nfa2dfa (NFA qs as ts es ss fs) = NFA qs' as ts' es' ss' fs'
  where
    qs' = undefined
    ts' = undefined
    es' = undefined
    ss' = undefined
    fs' = undefined

-- 13.
intNFA :: (Ord q) => NFA q -> NFA Int
intNFA (NFA qs as ts es ss fs) = NFA qs' as ts' es' ss' fs'
  where
    qs' = undefined
    ts' = undefined
    es' = undefined
    ss' = undefined
    fs' = undefined
    qToInt = lookup (zip (toList qs) [0 ..])
    lookup :: (Ord q) => [(q, Int)] -> q -> Int
    lookup qis q' = the [i | (q, i) <- qis, q == q']
    the [q] = q
    the _   = error "lookup"

-- 14.
reverseNFA :: Ord q => NFA q -> NFA q
reverseNFA (NFA qs as ts es ss fs) = NFA qs as ts' es' ss' fs'
  where
    ts' = undefined
    es' = undefined
    ss' = undefined
    fs' = undefined

mapTrans :: (q -> r) -> [Trans q] -> [Trans r]
mapTrans f ts = [(f q, x, f q') | (q, x, q') <- ts]

tidyNFA :: Ord q => NFA q -> NFA Int
tidyNFA = intNFA . nfa2dfa . reverseNFA . nfa2dfa . reverseNFA

data QF q
  = Q
  | E q
  | F
  deriving (Eq, Ord, Show)

thompson :: Ord q => NFA q -> NFA (QF q)
thompson (NFA qs as ts es ss fs) = NFA qs' as ts' es' ss' fs'
  where
    qs' = ss' \/ fs' \/ mapS E qs
    ts' = mapTrans E ts
    es' =
      [(E q, E q') | (q, q') <- es] ++
      [(Q, E q) | q <- toList ss] ++ [(E q, F) | q <- toList fs]
    ss' = set [Q]
    fs' = set [F]

isThompson (NFA qs as ts es ss fs) =
  case (toList ss, toList fs) of
    ([s], [f]) ->
      (not . or) [q' == s || q == f | (q, _, q') <- ts] &&
      (not . or) [q' == s || q == f | (q, q') <- es]
    _ -> False

-- 15.
stringNFA :: String -> NFA Int
stringNFA xs = mkNFA qs as ts es ss fs
  where
    qs = [0 .. n]
    as = xs
    ts = undefined
    es = undefined
    ss = undefined
    fs = undefined
    n = length xs

nullNFA :: NFA Bool
nullNFA = mkNFA qs as ts es ss fs
  where
    qs = [False, True]
    as = []
    ts = undefined
    es = undefined
    ss = undefined
    fs = undefined

dotNFA :: NFA Bool
dotNFA = mkNFA qs as ts es ss fs
  where
    qs = [False, True]
    as = ['a' .. 'z']
    ts = undefined
    es = undefined
    ss = undefined
    fs = undefined

-- 16.
concatNFA :: (Ord a, Ord b) => NFA a -> NFA b -> NFA (Either a b)
concatNFA (NFA qs as ts es ss fs) (NFA qs' as' ts' es' ss' fs') =
  NFA qs'' as'' ts'' es'' ss'' fs''
  where
    qs'' = mapS Left qs \/ mapS Right qs'
    as'' = as \/ as'
    ts'' = mapTrans Left ts ++ mapTrans Right ts'
    es'' =
      [(Left q, Left q') | (q, q') <- es] ++
      [(Right q, Right q') | (q, q') <- es'] ++
      [(Left f, Right s) | f <- toList fs, s <- toList ss']
    ss'' = mapS Left ss
    fs'' = mapS Right fs'

tconcatNFA :: (Ord a, Ord b) => NFA a -> NFA b -> NFA (Either a b)
tconcatNFA (NFA qs as ts es ss fs) (NFA qs' as' ts' es' ss' fs') =
  NFA qs'' as'' ts'' es'' ss'' fs''
  where
    qs'' = mapS Left qs \/ mapS Right (qs' \\ ss')
    as'' = as \/ as'
    ts'' =
      mapTrans Left ts ++
      [undefined | (q, a, q') <- ts', q == the ss'] ++
      [undefined | (q, a, q') <- ts', q /= the ss']
    es'' =
      [(Left q, Left q') | (q, q') <- es] ++
      [undefined | (q, q') <- es', q == the ss'] ++
      [undefined | (q, q') <- es', q /= the ss']
    ss'' = mapS Left ss
    fs'' = mapS Right fs'
    the xs = (\[x] -> x) (toList xs) -- thompson

-- 17.
starNFA :: Ord q => NFA q -> NFA (QF q)
starNFA (NFA qs as ts es ss fs) = NFA qs' as ts' es' ss' fs'
  where
    qs' = ss' \/ fs' \/ mapS E qs
    ts' = mapTrans E ts
    es' =
      (Q, F) :
      [(E f, E q) | f <- toList fs, q <- toList ss] ++
      [undefined | (q, q') <- es] ++
      [undefined | q <- toList ss] ++ [undefined | q <- toList fs]
    ss' = set [Q]
    fs' = set [F]

-- 18.
unionNFA :: (Ord q, Ord q') => NFA q -> NFA q' -> NFA (Either q q')
unionNFA (NFA qs as ts es ss fs) (NFA qs' as' ts' es' ss' fs') =
  NFA qs'' as'' ts'' es'' ss'' fs''
  where
    qs'' = undefined
    as'' = undefined
    ts'' = undefined
    es'' = undefined
    ss'' = undefined
    fs'' = undefined

tunionNFA m n = thompson $ unionNFA m n

-- 19.
completeNFA :: Ord q => NFA q -> NFA (Maybe q)
completeNFA (NFA qs as ts es ss fs) = NFA qs' as ts' es' ss' fs'
  where
    qs' = undefined
    ts' = undefined
    es' = undefined
    ss' = undefined
    fs' = undefined

complementNFA :: Ord q => NFA q -> NFA (Maybe q)
complementNFA nfa =
  let NFA qs as ts es ss fs = completeNFA nfa
   in NFA qs as ts es ss (qs \\ fs)

productNFA :: (Ord q, Ord q') => NFA q -> NFA q' -> NFA (q, q')
productNFA (NFA qs as ts es ss fs) (NFA qs' as' ts' es' ss' fs') =
  NFA qs'' as'' ts'' es'' ss'' fs''
  where
    qs'' = undefined
    as'' = undefined
    ts'' = undefined
    es'' = undefined
    ss'' = undefined
    fs'' = undefined

intersectNFA :: (Ord q, Ord q') => NFA q -> NFA q' -> NFA (q, q')
intersectNFA = productNFA

-- 20.
-- Regex -- extended with Boolean operations
data Regex
  = S String
  | Regex0
  | Dot
  | (:|:) Regex Regex -- alternation
  | (:&:) Regex Regex -- intersection
  | (:>:) Regex Regex -- concatenation
  | Not Regex -- negation
  | Star Regex -- Kleene-star
  deriving (Show)

regex2nfa :: Regex -> NFA Int
regex2nfa (S s)     = undefined
regex2nfa Regex0    = undefined
regex2nfa Dot       = undefined
regex2nfa (r :|: s) = undefined
regex2nfa (r :&: s) = undefined
regex2nfa (r :>: s) = undefined
regex2nfa (Star r)  = undefined
regex2nfa (Not r)   = undefined

-- ---------------------------------------------------------
-- -- Code form previous tutorial and QuickCheck properties
-- ---------------------------------------------------------
-- --Basic FSMs
charFSM :: Char -> FSM Bool
charFSM c = mkFSM [False, True] [c] [(False, c, True)] [False] [True]

stringFSM :: String -> FSM Int
stringFSM xs =
  mkFSM [0 .. n] xs [(i, xs !! i, i + 1) | i <- [0 .. n - 1]] [0] [n]
  where
    n = length xs

epsFSM :: FSM Int -- FSM Int to give this as instance of stringFSM
epsFSM = stringFSM ""

nullFSM :: FSM ()
nullFSM = mkFSM [] [] [] [] []

-- -- operations directly on FSM
intFSM :: (Ord q) => FSM q -> FSM Int
intFSM (FSM qs as ts ss fs) =
  FSM (mapS qToInt qs) as (mapTrans qToInt ts) (mapS qToInt ss) (mapS qToInt fs)
  where
    qToInt = lookup (zip (toList qs) [0 ..])
    lookup :: (Ord q) => [(q, Int)] -> q -> Int
    lookup qis q' = the [i | (q, i) <- qis, q == q']
    the [q] = q
    the _   = error "lookup"

sumFSM :: (Ord q, Ord q') => FSM q -> FSM q' -> FSM (Either q q')
sumFSM (FSM qs as ts ss fs) (FSM qs' as' ts' ss' fs') =
  FSM qs'' as'' ts'' ss'' fs''
  where
    qs'' = mapS Left qs \/ mapS Right qs'
    as'' = as \/ as'
    ts'' = mapTrans Left ts ++ mapTrans Right ts'
    ss'' = mapS Left ss \/ mapS Right ss'
    fs'' = mapS Left fs \/ mapS Right fs'

unionFSM :: (Ord q, Ord q') => FSM q -> FSM q' -> FSM (Either q q')
unionFSM = sumFSM

productFSM :: (Ord q, Ord q') => FSM q -> FSM q' -> FSM (q, q')
productFSM (FSM qs as ts ss fs) (FSM qs' as' ts' ss' fs') =
  FSM qs'' as'' ts'' ss'' fs''
  where
    qs'' = cartesianProduct qs qs'
    as'' = (as \/ as')
    ts'' =
      [((x, y), a, (x', y')) | (x, a, x') <- ts, (y, a', y') <- ts', a == a']
    ss'' = cartesianProduct ss ss'
    fs'' = cartesianProduct fs fs'

intersectFSM :: (Ord q, Ord q') => FSM q -> FSM q' -> FSM (q, q')
intersectFSM = productFSM

completeFSM :: Ord q => FSM q -> FSM (Maybe q)
completeFSM (FSM qs as ts ss fs) -- add black hole state
 = FSM qs' as ts' ss' fs'
  where
    qs' = Nothing `insert` mapS Just qs
    ts' =
      mapTrans Just ts ++
      [(Nothing, x, Nothing) | x <- toList as] ++
      [ (Just q, x, Nothing)
      | q <- toList qs
      , x <- toList as
      , and [r /= q || x /= y | (r, y, _) <- ts]
      ]
    ss' = mapS Just ss
    fs' = mapS Just fs

complementFSM :: Ord q => FSM q -> FSM (Maybe q)
complementFSM fsm =
  let (FSM qs as ts ss fs) = completeFSM fsm
   in FSM qs as ts ss (qs \\ fs)

concatFSM :: (Ord q, Ord q') => FSM q -> FSM q' -> FSM (Either q q')
concatFSM (FSM qs as ts ss fs) (FSM qs' as' ts' ss' fs') =
  FSM qs'' as'' ts'' ss'' fs''
  where
    qs'' = mapS Left qs \/ mapS Right qs'
    as'' = as \/ as'
    ts'' =
      [ (Left q, sym, Right r')
      | q <- toList fs
      , (r, sym, r') <- ts'
      , r `member` ss'
      ] ++
      mapTrans Left ts ++ mapTrans Right ts'
    ss'' = mapS Left ss
    fs'' =
      if Set.null (fs' /\ ss')
        then mapS Right fs'
        else mapS Left fs \/ mapS Right fs'

starFSM :: (Ord q) => FSM q -> FSM q
starFSM (FSM qs as ts ss fs) = FSM qs as ts' ss' fs'
  where
    ts' = ts ++ [(x, a, y) | x <- toList fs, (z, a, y) <- ts, z `member` ss]
    ss' = ss \/ fs
    fs' = ss \/ fs

-- QuickCheck
safeString :: String -> String
safeString a = filter (`elem` ['a' .. 'z']) (map toLower a)

-- THESE ARE HERE TO ENABLE CHECKING
prop_stringFSM m n = acceptsFSM a s && (acceptsFSM a t == (s == t))
  where
    a = stringFSM s
    s = safeString m
    t = safeString n

prop_concatFSM :: String -> String -> String -> Bool
prop_concatFSM m n o =
  acceptsFSM fsm (s ++ t) && (acceptsFSM fsm u == (s ++ t == u))
  where
    fsm = concatFSM a b
    a = stringFSM s
    b = stringFSM t
    s = safeString m
    t = safeString n
    u = safeString o

prop_unionFSM :: String -> String -> String -> Bool
prop_unionFSM m n o =
  acceptsFSM fsm u == (acceptsFSM a u || acceptsFSM b u) &&
  acceptsFSM fsm s && acceptsFSM fsm t
  where
    fsm = unionFSM a b
    a = stringFSM s
    b = stringFSM t
    s = safeString m
    t = safeString n
    u = safeString o

prop_intersectFSM1 :: String -> String -> Bool
prop_intersectFSM1 m n = acceptsFSM fsm s && acceptsFSM fsm t == (s == t)
  where
    fsm = intersectFSM a a
    a = stringFSM s
    s = safeString m
    t = safeString n

prop_intersectFSM2 m n o =
  acceptsFSM fsm u == (acceptsFSM a u && acceptsFSM b u)
  where
    fsm = intersectFSM a b
    a = stringFSM s
    b = stringFSM t
    s = safeString m
    t = safeString n
    u = safeString o

prop_intersectFSM3 m n o =
  acceptsFSM fsm s && acceptsFSM fsm u == acceptsFSM a u
  where
    fsm = intersectFSM a (unionFSM a b)
    a = stringFSM s
    b = stringFSM t
    s = safeString m
    t = safeString n
    u = safeString o

prop_starFSM :: String -> Int -> Bool
prop_starFSM m n = acceptsFSM fsm (concat (replicate i s))
  where
    fsm = starFSM (stringFSM s)
    s = safeString m
    i = abs n

prop_stringNFA m n = acceptsNFA a s && (acceptsNFA a t == (s == t))
  where
    a = stringNFA s
    s = safeString m
    t = safeString n

prop_concatNFA :: String -> String -> String -> Bool
prop_concatNFA m n o =
  acceptsNFA fsm (s ++ t) && (acceptsNFA fsm u == (s ++ t == u))
  where
    fsm = concatNFA a b
    a = stringNFA s
    b = stringNFA t
    s = safeString m
    t = safeString n
    u = safeString o

prop_unionNFA :: String -> String -> String -> Bool
prop_unionNFA m n o =
  acceptsNFA fsm u == (acceptsNFA a u || acceptsNFA b u) &&
  acceptsNFA fsm s && acceptsNFA fsm t
  where
    fsm = unionNFA a b
    a = stringNFA s
    b = stringNFA t
    s = safeString m
    t = safeString n
    u = safeString o

prop_intersectNFA1 :: String -> String -> Bool
prop_intersectNFA1 m n = acceptsNFA fsm s && acceptsNFA fsm t == (s == t)
  where
    fsm = intersectNFA a a
    a = stringNFA s
    s = safeString m
    t = safeString n

prop_intersectNFA2 m n o =
  acceptsNFA fsm u == (acceptsNFA a u && acceptsNFA b u)
  where
    fsm = intersectNFA a b
    a = stringNFA s
    b = stringNFA t
    s = safeString m
    t = safeString n
    u = safeString o

prop_intersectNFA3 m n o =
  acceptsNFA fsm s && acceptsNFA fsm u == acceptsNFA a u
  where
    fsm = intersectNFA a (unionNFA a b)
    a = stringNFA s
    b = stringNFA t
    s = safeString m
    t = safeString n
    u = safeString o

prop_starNFA :: String -> Int -> Bool
prop_starNFA m n = acceptsNFA fsm (concat (replicate i s))
  where
    fsm = starNFA (stringNFA s)
    s = safeString m
    i = abs n

---- Stuff we may not need for thiis tutorial
m1 :: FSM Int
m1 =
  mkFSM
    [0, 1, 2, 3, 4] -- states
    "ab" -- alphabet
    [ (0, 'a', 1)
    , (0, 'b', 1)
    , (0, 'a', 2)
    , (0, 'b', 2)
    , (1, 'b', 4)
    , (2, 'a', 3)
    , (2, 'b', 3)
    , (3, 'b', 4)
    , (4, 'a', 4)
    , (4, 'b', 4)
    ]
    [0] -- start
    [4] -- accept

m2 :: FSM Char
m2 =
  mkFSM
    "ABCD" -- states
    "01" -- symbols
    [ ('A', '0', 'D')
    , ('A', '1', 'B')
    , ('B', '0', 'A')
    , ('B', '1', 'C')
    , ('C', '0', 'B')
    , ('C', '1', 'D')
    , ('D', '0', 'D')
    , ('D', '1', 'D')
    ]
    "B" -- start
    "ABC" -- accept

dm1 :: FSM [Int]
dm1 =
  mkFSM
    [[], [0], [1, 2], [3], [3, 4], [4]] -- states
    "ab" -- symbols
    [ ([], 'a', [])
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
    ]
    [[0]] -- start
    [[3, 4], [4]] -- accept
