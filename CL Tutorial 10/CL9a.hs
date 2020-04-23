module CL9a where
import Prelude hiding (lookup)
import Data.Set(Set, insert, empty, member, toList)
import qualified Data.Set as Set
import Test.QuickCheck
import Data.Char

(\/) :: Ord a => Set a -> Set a -> Set a
(\/)  = Set.union
(/\) :: Ord a => Set a -> Set a -> Set a
(/\) = Set.intersection
(\\) :: Ord a => Set a -> Set a -> Set a
(\\) = Set.difference
mapS :: Ord b => (a -> b) -> Set a -> Set b
mapS = Set.mapMonotonic 
set :: Ord q => [q] -> Set q
set = Set.fromList

-- 1.
-- return the set of nodes reached by any number of steps (including 0)
reach' :: Ord q => (Set q -> Set q) -> Set q -> Set q
reach' step gs = if gs' == gs then gs else reach' step gs'
  where gs' = gs \/ step gs

reach :: Ord q => (Set q -> Set q) -> Set q -> Set q
reach step qs =
  let add q qss
        | q `member` qss = qss
        | otherwise =
          foldr add (insert q qss) (step $ set[q])
  in foldr add empty qs

-- labelled transitions
type Sym = Char
type Trans q = (q, Sym, q)

-- -- 2.
toy_ts :: [Trans Int]
toy_ts = [ (n,intToDigit m, m*n `mod` 10) | n <- [0..9], m <- [3,7] ]

smallStep :: Set Int -> Set Int
smallStep qs = set[ q' | (q,_,q') <- toy_ts, q`member`qs ]  

-- -- 3.
-- -- oneStep ts qs produces the states reached adjacent to qs
-- -- then can be reached by a single transition from ts 
oneStep :: Ord q => [Trans q] -> Set q -> Set q
oneStep ts qs = set [ q' | (q, _, q') <- ts, q`member`qs ]

-- --   FSM states symbols transitions starting accepting 
data FSM q = FSM (Set q) (Set Sym) [Trans q] (Set q) (Set q) deriving Show 
mkFSM qs as ts ss fs = FSM qs' as' ts' ss' fs' where 
  qs' = set qs
  as' = set as
  ts' = [ t | t@(q,a,q') <- ts, q`member`qs',q'`member`qs',a`member`as' ]
  ss' = set ss /\ qs'
  fs' = set fs /\ qs'

-- -- toy examples
g0 :: [Int] -> [Int] -> FSM Int
g0 = mkFSM [0..9] "37" [ (n,intToDigit m, m*n `mod` 10) | n <- [0..9], m <- [3,7] ]
eg0 = g0 [1]   [9]
eg1 = g0 [1,2] [9]
-- 

-- -- 4.
reachableFSM :: Ord q => FSM q -> Set q
reachableFSM (FSM _ _ ts ss _) = reach (oneStep ts) ss

-- -- 5.
pruneFSM :: Ord q => FSM q -> FSM q
pruneFSM fsm@(FSM qs as ts ss fs) =  FSM qs' as ts' ss fs' where 
  qs' = reachableFSM fsm
  ts' = [ t | t@(q,_,_) <- ts, q`member`qs' ]
  fs' = fs /\ qs'

-- 6.
reverseFSM :: Ord q => FSM q -> FSM q
reverseFSM (FSM qs as ts ss fs) =
  FSM qs as ts' ss' fs' where 
    ts' = [ (q',a,q) | (q,a,q') <- ts ]
    ss' = fs
    fs' = ss

prop_reverseFSM :: String -> Bool -- require accepts FSM, to be defined below
prop_reverseFSM s = acceptsFSM (reverseFSM $ stringFSM s) (reverse s)

-- -- 7.
tidyFSM :: Ord q => FSM q -> FSM q
tidyFSM = reverseFSM.pruneFSM.reverseFSM.pruneFSM

-- -- NFA adding epsilon-transitions
data NFA q = NFA (Set q) (Set Sym) [Trans q] [(q,q)] (Set q)(Set q) deriving Show
mkNFA :: Ord q =>
         [q] -> [Sym] -> [(q, Sym, q)] -> [(q, q)] -> [q] -> [q] -> NFA q
mkNFA qs as ts es ss fs = NFA qs' as' ts' es' ss' fs' where
  qs' = set qs
  as' = set as
  ts' = [ t | t@(q,a,q')<-ts, q`member`qs',q'`member`qs',a`member`as']
  es' = [ e | e@(q,q') <- es, q`member`qs',q'`member`qs', q/=q']
  ss' = set ss /\ qs'
  fs' = set fs /\ qs'

asNFA :: FSM q -> NFA q
asNFA (FSM qs as ts ss fs) = NFA qs as ts [] ss fs
asFSM :: NFA q -> FSM q
asFSM  (NFA qs as ts es ss fs)
  | null es = FSM qs as ts ss fs
  | otherwise = error "has e-transittions"
  
-- 8.
eStep  :: Ord q =>  [(q,q)] -> Set q -> Set q
eStep es qs = set [ q' | (q,q') <- es, q`member`qs ]
eClose :: Ord q => [(q,q)] -> Set q -> Set q   
eClose es = reach (eStep es)

-- 9.
-- following only transitions with a given label (cf. ddelta)
ddelta :: (Ord q) =>  [Trans q] -> Sym -> Set q -> Set q
ddelta ts a = oneStep [ t | t@(_,a',_) <- ts, a'==a ]

-- -- moving the start states
moveFSM :: Ord q => FSM q -> Sym -> FSM q
moveFSM (FSM qs as ts ss fs) x = FSM qs as ts (ddelta ts x ss) fs

acceptsFSM :: (Ord q) => FSM q -> String -> Bool
acceptsFSM (FSM _ _ _ ss fs) "" = (not . Set.null)(ss /\ fs)
acceptsFSM fsm (x : xs) = acceptsFSM (moveFSM fsm x) xs

-- 10.
eddelta :: (Ord q) => [Trans q] -> [(q,q)] -> Sym -> Set q -> Set q
eddelta ts es a = eClose es . ddelta ts a . eClose es

moveNFA :: Ord q => NFA q -> Sym -> NFA q
moveNFA (NFA qs as ts es ss fs) a = NFA qs as ts es (eddelta ts es a ss) fs

acceptsNFA :: (Ord q) => NFA q -> String -> Bool
acceptsNFA (NFA _ _ _ es ss fs) "" = (not . Set.null)(eClose es ss /\ fs)
acceptsNFA nfa (x : xs) = acceptsNFA (moveNFA nfa x) xs

-- next is like step, but on superstates
next :: Ord q =>  [Trans q] -> (Set Sym)-> Set(Set q) -> Set(Set q)
next ts as qqs = set [ddelta ts a qs | qs <- toList qqs, a <- toList as ]

fsm2dfa :: Ord q => FSM q -> FSM (Set q)
fsm2dfa (FSM qs as ts ss fs) =
  FSM qs' as ts' ss' fs' where
  qs' = reach (next ts as) ss' 
  ts' = [ (qs, a, ddelta ts a qs) | qs <- toList qs', a <- toList as ]
  ss' = set [ss]
  fs' = set [ qs | qs <- toList qs', (not . Set.null)(qs/\fs) ]

-- 11.
-- eNext is like next, but including e-transitions
eNext :: Ord q =>  [Trans q] -> [(q,q)] -> (Set Sym)-> Set(Set q) -> Set(Set q)
eNext ts es as qss = set [eddelta ts es a qs | qs <- toList qss, a <- toList as ]

-- 12.
nfa2dfa :: Ord q => NFA q -> NFA (Set q)
nfa2dfa (NFA qs as ts es ss fs) =
  NFA qss as ts' es' ss' fs' where
  qss = reach (eNext ts es as) ss'
  ts' = [ (qs, a, eddelta ts es a qs) | qs <- toList qss, a <- toList as ]
  es' = []
  ss' = set [ eClose es ss ]
  fs' = set [ qs | qs <- toList qss, (not . Set.null) (fs/\qs) ]

-- 13.
intNFA :: (Ord q) => NFA q -> NFA Int
intNFA (NFA qs as ts es ss fs)  = 
  NFA qs' as ts' es' ss' fs' where
  qs' = mapS qToInt qs
  ts' = mapTrans qToInt ts
  es' = [ (qToInt q, qToInt q') | (q,q') <- es ]
  ss' = mapS qToInt ss
  fs' = mapS qToInt fs
  qToInt = lookup (zip (toList qs) [0..])
  lookup :: (Ord q) => [(q,Int)] -> q -> Int
  lookup qis q' = the [ i | (q,i) <- qis, q == q' ]
  the [q] = q
  the _ = error "lookup"

-- 14.
reverseNFA :: Ord q => NFA q -> NFA q
reverseNFA (NFA qs as ts es ss fs) =
  NFA qs as ts' es' ss' fs' where
  ts' = [ (q',a,q) | (q,a,q') <- ts ]
  es' = [ (q',q)   | (q,q') <- es ]
  ss' = fs
  fs' = ss

mapTrans :: (q -> r) -> [Trans q] -> [Trans r]
mapTrans f ts = [ (f q,x,f q') | (q,x,q') <- ts ]


tidyNFA :: Ord q => NFA q -> NFA Int
tidyNFA =  intNFA . nfa2dfa . reverseNFA . nfa2dfa . reverseNFA


data QF q = Q | E q | F deriving (Eq, Ord,Show)
thompson :: Ord q => NFA q -> NFA (QF q)
thompson (NFA qs as ts es ss fs) =
  NFA qs' as ts' es' ss' fs' where
   qs' = ss' \/ fs' \/ mapS E qs
   ts' = mapTrans E ts
   es' = [ (E q, E q') | (q,q') <- es] 
         ++ [ (Q,E q) | q <- toList ss ] ++ [(E q,F) | q <- toList fs ]
   ss' = set[Q]
   fs' = set[F]

isThompson :: Eq a => NFA a -> Bool
isThompson (NFA qs as ts es ss fs) = case (toList ss,toList fs) of
  ([s],[f]) -> (not . or) [ q'==s||q==f | (q,_,q') <- ts] &&
               (not . or) [ q'==s||q==f | (q,q') <- es]
  _ -> False

-- 15.
stringNFA :: String -> NFA Int
stringNFA xs = mkNFA qs as ts es ss fs where
  qs = [0..n]
  as = xs
  ts = [(i,xs!!i,i+1) | i <- [0..n-1]]
  es = []
  ss = [0]
  fs = [n]
  n = length xs
  
nullNFA :: NFA Bool
nullNFA = mkNFA [False,True] [] [] [] [True] [False]

dotNFA :: NFA Bool
dotNFA = mkNFA qs as ts es ss fs where
    qs = [False,True]
    as = ['a'..'z']
    ts = [(False,sym, True)| sym <- as]
    es = []
    ss = [False]
    fs = [True]

-- 16.
concatNFA :: (Ord a, Ord b) => NFA a -> NFA b -> NFA (Either a b)
concatNFA (NFA qs as ts es ss fs)(NFA qs' as' ts' es' ss' fs') =
  NFA qs'' as'' ts'' es'' ss'' fs'' where
    qs'' = mapS Left qs \/ mapS Right qs'
    as'' = as \/ as'
    ts'' = mapTrans Left ts ++ mapTrans Right ts'
    es'' = [ (Left q, Left q') | (q,q') <- es ]
           ++ [ (Right q, Right q') | (q,q') <- es' ]
           ++ [ (Left f, Right s) | f <- toList fs, s <- toList ss' ]
    ss'' = mapS Left ss
    fs'' = mapS Right fs'

tconcatNFA :: (Ord a, Ord b) => NFA a -> NFA b -> NFA (Either a b)
tconcatNFA (NFA qs as ts es ss fs)(NFA qs' as' ts' es' ss' fs') =
  NFA qs'' as'' ts'' es'' ss'' fs'' where
    qs'' = mapS Left qs \/ mapS Right (qs' \\ ss')
    as'' = as \/ as'
    ts'' = mapTrans Left ts
           ++ [(Left (the fs), a, Right q') | (q,a,q') <- ts', q==the ss']
           ++ [(Right q, a, Right q') | (q,a,q') <- ts', q/=the ss' ]
    es'' = [ (Left q, Left q') | (q,q') <- es ]
           ++ [ (Left (the fs), Right q') | (q,q') <- es', q==the ss']
           ++ [ (Right q, Right q') | (q,q') <- es', q/=the ss' ]
    ss'' = mapS Left ss
    fs'' = mapS Right fs'
    the xs = (\[x] -> x) (toList xs) -- thompson 
    
-- 17.
starNFA :: Ord q => NFA q -> NFA (QF q)
starNFA (NFA qs as ts es ss fs) =
  NFA qs' as ts' es' ss' fs'
  where qs' = ss'\/fs'\/ mapS E qs
        ts' = mapTrans E ts
        es' = (Q,F) : [(E f, E q) | f <- toList fs, q <- toList ss ]
              ++ [ (E q, E q') | (q,q') <- es] 
              ++ [ (Q,E q)     | q <- toList ss ]
              ++ [ (E q,F)     | q <- toList fs ]
        ss' = set [Q]
        fs' = set [F]

-- 18.
unionNFA :: (Ord q, Ord q') => NFA q -> NFA q' -> NFA (Either q q')
unionNFA (NFA qs as ts es ss fs) (NFA qs' as' ts' es' ss' fs') =
  NFA qs'' as'' ts'' es'' ss'' fs'' where
  qs'' = mapS Left qs \/ mapS Right qs'
  as'' = as \/ as'
  ts'' = mapTrans Left ts ++ mapTrans Right ts'
  es'' = [ (Left q, Left q') | (q,q') <- es ] ++ [ (Right q, Right q') | (q,q') <- es' ]
  ss'' = mapS Left ss \/ mapS Right ss'
  fs'' = mapS Left fs \/ mapS Right fs'

tunionNFA :: (Ord q, Ord q') => NFA q -> NFA q' -> NFA (QF (Either q q'))
tunionNFA m n = thompson $ unionNFA m n

-- 19.
completeNFA :: Ord q => NFA q -> NFA (Maybe q)
completeNFA (NFA qs as ts es ss fs) =
  NFA qs' as ts' es' ss' fs' where
  qs' = insert Nothing (mapS Just qs)
  ts' = mapTrans Just ts ++
    [ (Just q, a, Nothing) | q <- toList qs, a <- toList as,
      and[ q /= q' || a /= a' | (q', a', _) <- ts ] ]
  es' = [ (Just q, Just q') | (q,q') <- es ]
  ss' = mapS Just ss
  fs' = mapS Just fs
                                                   
complementNFA :: Ord q => NFA q -> NFA (Maybe q)
complementNFA nfa =
  let NFA qs as ts es ss fs = completeNFA nfa
  in NFA qs as ts es ss (qs \\ fs)

productNFA :: (Ord q, Ord q') => NFA q -> NFA q' -> NFA (q,q')
productNFA (NFA qs as ts es ss fs) (NFA qs' as' ts' es' ss' fs')
  = NFA qs'' as'' ts'' es'' ss'' fs'' where
    qs'' = Set.cartesianProduct qs qs'
    as'' = (as \/ as')
    ts'' = [((x,y), a, (x',y')) | (x,a,x') <- ts, (y,a',y') <- ts', a == a']
    es'' = [((x,y),(x',y')) | (x,x') <- es, (y,y') <- es'] ++
           [((x,y),(x',y)) | (x,x') <- es, y <- toList qs'] ++
           [((x,y),(x,y')) | (y,y') <- es', x <- toList qs]
    ss'' = Set.cartesianProduct ss ss' 
    fs'' = Set.cartesianProduct fs fs'

intersectNFA :: (Ord q, Ord q') => NFA q -> NFA q' -> NFA (q,q')
intersectNFA = productNFA
   
-- 20.
-- Regex -- extended with Boolean operations

data Regex =
  S String | Regex0 | Dot
  | Regex :|: Regex -- alternation
  | Regex :&: Regex -- intersection
  | Regex :>: Regex -- concatenation
  | Not  Regex        -- negation
  | Star Regex        -- Kleene-star
  deriving Show
infixr 4 :>:
infixr 3 :&:
infixr 2 :|:

regex2nfa :: Regex -> NFA Int
regex2nfa (S s)     = intNFA $ stringNFA s
regex2nfa Regex0    = intNFA $ nullNFA
regex2nfa Dot       = intNFA $ dotNFA
regex2nfa (r :|: s) = intNFA $ tunionNFA     (regex2nfa r)(regex2nfa s)
regex2nfa (r :&: s) = intNFA $ productNFA    (regex2nfa r)(regex2nfa s)
regex2nfa (r :>: s) = intNFA $ tconcatNFA    (regex2nfa r)(regex2nfa s)
regex2nfa (Star r)  = intNFA $ starNFA       (regex2nfa r)
regex2nfa (Not r)   = intNFA $ complementNFA (regex2nfa r)

minimalDFA :: Regex -> FSM Int
minimalDFA = tidyFSM . intFSM . asFSM . tidyNFA . regex2nfa


pretty regex = showsPrec 0 regex ""
  where
    showsPrec p (S s) = case length s of
      0 -> showString"\"\""
      1 -> showString s
      _ -> showParen (p>5) (showString s)
    showsPrec _ Dot   = showChar '.'
    showsPrec p (r :>: s)
      = showParen (p>5)
             (showsPrec 5 r . showsPrec 5 s)
    showsPrec p (r :|: s)
      = showParen (p>4)
             (showsPrec 4 r . showChar '|' . showsPrec 4 s)
    showsPrec p (r :&: s)
      = showParen (p>3)
             (showsPrec 3 r . showChar '&' . showsPrec 3 s)
    showsPrec p (Star r)
       = showParen (p>6)
             (showsPrec 6 r . showChar '*')
    showString :: String -> String -> String
    showString = (++)
    showChar :: Char -> String -> String
    showChar   = (:)
    showSpace :: String -> String
    showSpace  = showChar ' '
    showParen :: Bool -> (String -> String) -> (String -> String)
    showParen p s = if p then showChar '(' . s . showChar ')' else s

-- minimalDFA (foldr (:|:) Regex0 $ map S $ words sentence)

-- ---------------------------------------------------------
-- -- Code from previous tutorial and QuickCheck properties
-- ---------------------------------------------------------
-- -- DFA
-- isDFA :: Ord q => FSM q -> Bool
-- isDFA (FSM qs as ts ss fs) =
--   and[length[ q' | q' <- qs, (q, a, q')`member`ts ] == 1| q <- qs, a <- toList as ]
--   && (length ss == 1)

-- isComplete (FSM qs as ts ss fs) = and[or[(q,a,q')`member` ts | q' <- qs] |q<-qs,a<-as]


-- --Basic FSMs

charFSM :: Char -> FSM Bool
charFSM c =
  mkFSM [False,True] [c] [(False,c,True)] [False] [True]

stringFSM :: String -> FSM Int
stringFSM xs =
  mkFSM [0..n] xs [(i,xs!!i,i+1) | i <- [0..n-1]] [0] [n]
  where n = length xs

epsFSM :: FSM Int -- FSM Int to give this as instance of stringFSM
epsFSM = stringFSM ""

nullFSM :: FSM ()
nullFSM = mkFSM [] [] [] [] []

-- -- operations directly on FSM

intFSM :: (Ord q) => FSM q -> FSM Int
intFSM (FSM qs as ts ss fs)  = 
  FSM 
  (mapS qToInt qs)
  as
  (mapTrans qToInt ts)
  (mapS qToInt ss)
  (mapS qToInt fs)
  where
    qToInt = lookup (zip (toList qs) [0..])
    lookup :: (Ord q) => [(q,Int)] -> q -> Int
    lookup qis q' = the [ i | (q,i) <- qis, q == q' ]
    the [q] = q
    the _ = error "lookup"

unionFSM :: (Ord q, Ord q') => FSM q -> FSM q' -> FSM (Either q q')
unionFSM (FSM qs as ts ss fs) (FSM qs' as' ts' ss' fs') =
  FSM qs'' as'' ts'' ss'' fs'' where
  qs'' = mapS Left qs \/ mapS Right qs'
  as'' = as \/ as'
  ts'' = mapTrans Left ts ++ mapTrans Right ts'
  ss'' = mapS Left ss \/ mapS Right ss'
  fs'' = mapS Left fs \/ mapS Right fs'

productFSM :: (Ord q, Ord q') => FSM q -> FSM q' -> FSM (q,q')
productFSM (FSM qs as ts ss fs) (FSM qs' as' ts' ss' fs')
  = FSM qs'' as'' ts'' ss'' fs'' where
    qs'' = Set.cartesianProduct qs qs'
    as'' = (as \/ as') 
    ts'' = [((x,y), a, (x',y')) | (x,a,x') <- ts, (y,a',y') <- ts', a == a']
    ss'' = Set.cartesianProduct ss ss' 
    fs'' = Set.cartesianProduct fs fs'

intersectFSM :: (Ord q, Ord q') => FSM q -> FSM q' -> FSM (q,q')
intersectFSM = productFSM

completeFSM :: Ord q => FSM q -> FSM (Maybe q)
completeFSM (FSM qs as ts ss fs) = -- add black hole state
  FSM qs' as ts' ss' fs' where
  qs' = Nothing `insert` mapS Just qs
  ts' = mapTrans Just ts ++ [ (Nothing, x, Nothing) | x <- toList as ] ++
        [ (Just q,x,Nothing) | q <- toList qs, x <- toList as,
          and[ r/=q || x/= y | (r,y,_) <- ts]]
  ss' = mapS Just ss
  fs' = mapS Just fs

complementFSM :: Ord q => FSM q -> FSM (Maybe q)
complementFSM fsm =
  let (FSM qs as ts ss fs) = completeFSM fsm
  in FSM qs as ts ss (qs \\ fs)

concatFSM :: (Ord q, Ord q') => FSM q -> FSM q' -> FSM (Either q q')
concatFSM (FSM qs as ts ss fs)  (FSM qs' as' ts' ss' fs') =
  FSM qs'' as'' ts'' ss'' fs'' where
  qs'' = mapS Left qs \/ mapS Right qs'
  as'' = as \/ as'
  ts'' = [(Left q, sym, Right r') | q <- toList fs, (r,sym,r') <- ts', r`member`ss']
         ++ mapTrans Left ts ++ mapTrans Right ts'
  ss'' = mapS Left ss
  fs'' = if Set.null (fs'/\ ss')
         then mapS Right fs'
         else mapS Left fs \/ mapS Right fs' 
  
starFSM :: (Ord q) => FSM q -> FSM q
starFSM  (FSM qs as ts ss fs) =
  FSM qs as ts' ss' fs' where
   ts' = ts ++ [(x, a, y) | x <- toList fs, (z,a,y) <- ts, z`member`ss ]
   ss' = ss \/ fs
   fs' = ss \/ fs

-- QuickCheck

safeString :: String -> String
safeString a = filter (`elem` ['a'..'z']) (map toLower a)

-- THESE ARE HERE TO ENABLE CHECKING
prop_stringFSM m n =
  acceptsFSM a s
  && (acceptsFSM a t == (s == t))
  where
  a = stringFSM s
  s = safeString m
  t = safeString n
  
prop_concatFSM :: String -> String -> String -> Bool
prop_concatFSM m n o =
  acceptsFSM fsm (s ++ t)
  && (acceptsFSM fsm u == (s ++ t == u))
  where
  fsm = concatFSM a b
  a = stringFSM s
  b = stringFSM t
  s = safeString m
  t = safeString n
  u = safeString o
        
prop_unionFSM :: String -> String -> String -> Bool
prop_unionFSM m n o =
  acceptsFSM fsm u == (acceptsFSM a u || acceptsFSM b u)
  && acceptsFSM fsm s
  && acceptsFSM fsm t
  where
  fsm = unionFSM a b
  a = stringFSM s
  b = stringFSM t
  s = safeString m
  t = safeString n
  u = safeString o

prop_intersectFSM1 :: String -> String -> Bool   
prop_intersectFSM1 m n =
  acceptsFSM fsm s && acceptsFSM fsm t == (s == t)
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
  acceptsFSM fsm s
  && acceptsFSM fsm u == acceptsFSM a u
  where
  fsm = intersectFSM a (unionFSM a b)
  a = stringFSM s
  b = stringFSM t
  s = safeString m
  t = safeString n
  u = safeString o

prop_starFSM :: String -> Int -> Bool
prop_starFSM m n =
  acceptsFSM fsm (concat (replicate i s))
  where
  fsm = starFSM (stringFSM s)
  s = safeString m
  i = abs n

prop_stringNFA m n =
  acceptsNFA a s
  && (acceptsNFA a t == (s == t))
  where
  a = stringNFA s
  s = safeString m
  t = safeString n
  
prop_concatNFA :: String -> String -> String -> Bool
prop_concatNFA m n o =
  acceptsNFA fsm (s ++ t)
  && (acceptsNFA fsm u == (s ++ t == u))
  where
  fsm = concatNFA a b
  a = stringNFA s
  b = stringNFA t
  s = safeString m
  t = safeString n
  u = safeString o
        
prop_unionNFA :: String -> String -> String -> Bool
prop_unionNFA m n o =
  acceptsNFA fsm u == (acceptsNFA a u || acceptsNFA b u)
  && acceptsNFA fsm s
  && acceptsNFA fsm t
  where
  fsm = unionNFA a b
  a = stringNFA s
  b = stringNFA t
  s = safeString m
  t = safeString n
  u = safeString o

prop_intersectNFA1 :: String -> String -> Bool   
prop_intersectNFA1 m n =
  acceptsNFA fsm s && acceptsNFA fsm t == (s == t)
  where
  fsm = intersectNFA  a a
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
  acceptsNFA fsm s
  && acceptsNFA fsm u == acceptsNFA a u
  where
  fsm = intersectNFA a (unionNFA a b)
  a = stringNFA s
  b = stringNFA t
  s = safeString m
  t = safeString n
  u = safeString o

prop_starNFA :: String -> Int -> Bool
prop_starNFA m n =
  acceptsNFA fsm (concat (replicate i s))
  where
  fsm = starNFA (stringNFA s)
  s = safeString m
  i = abs n

