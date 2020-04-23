module CL7a where

import Data.List (nub, delete, sortOn, minimumBy)
import Data.Sort (uniqueSort)
import Data.Ord (comparing)
import Control.Monad (liftM, liftM2)
import Test.QuickCheck (quickCheck, withMaxSuccess, Arbitrary( arbitrary ), oneof, elements, sized)

-- we will use these as propositional letters in examples
data Atom = A | B | C | D | W | X | Y | Z deriving (Eq, Show, Ord)
wff1 = (V A :|: V B) :&: (V A :&: V B)
wff2 = (V A :&: (V B :|: V C)) :&: ((Not (V A) :|: Not (V B)) :&: (Not (V A) :|: Not (V C)))
wff3 = (V A :->: V B) :&: (V A :&: Not (V B))
wff4 = (V A :<->: V B) :&: ((V A :&: Not (V B)) :|: (Not (V A) :&: V B))

-- The function wffToForm converts the Wffs from FP 6 into Forms   
wffToForm :: Ord a => Wff a -> Form a
wffToForm = canonical.toForm.toCNFList where
  toForm :: [[Wff a]] -> Form a
  toForm              = And . map toClause 
  toClause            = Or  . map toLit
  toLit (Not(V a) )   = N a
  toLit (V a)         = P a
  toLit _             = error "expected Literal"

prop_form_equiv wff = satisfiable wff /= null (dpll (wffToForm wff))

-- Tseytin transformation which generates conjunctive equisatisfiable form (CEF) 
tseytinToForm p =
  let tt :: Eq a => Wff a -> [Clause (Wff a)]
      tt r@(Not a)     = [Or[P r, P a], Or[N r, N a]] ++ tt a
      tt r@(a :&: b)   = [Or[P r, N a, N b], Or[N r, P a], Or[N r, P b]]
                         ++ tt a ++ tt b
      tt r@(a :|: b)   = [Or[N r, P a, P b], Or[P r, N a], Or[P r, N b]]
                         ++ tt a ++ tt b
      tt r@(a :->:  b) = [Or[N r, N a, P b], Or[P r, P a], Or[P r, N b]]
                         ++ tt a ++ tt b
      tt r@(a :<->: b) = [Or[P r, P a, P b], Or[P r, N a, N b], 
                          Or[N r, P a, N b], Or[N r, N a, P b]]
                         ++ tt a ++ tt b
      tt F             = [Or[N F]] -- we must make F False
      tt T             = [Or[P T]] -- we must make T True
      tt (V _)         = [] -- no further constraints
      -- we remove trivial clauses
      nontriv (Or xs) = and [ x /= neg x' | x <- xs, x' <- xs]
  in canonical . And $ ( Or[P p] : filter nontriv (tt p) )

prop_tseytin_equiv :: Wff Atom -> Bool
prop_tseytin_equiv wff = satisfiable wff /= null (dpll (tseytinToForm wff))

prop_tseytin_bigger :: Wff Atom -> Bool
prop_tseytin_bigger wff = size (wffToForm wff) <= 10 * size (tseytinToForm wff)
  where size (And cs) = sum[ length lits | (Or lits) <- cs]





---------------------------------------------------
-- Below are code from last weeks' tutorials 
-- Have a look to remind yourself about what happened last wekk
---------------------------------------------------

-- Wffs from last week's FP tutorial with some changes
data Wff a = V a
           | T
           | F
           | Not (Wff a)
           | Wff a :|: Wff a
           | Wff a :&: Wff a
           | Wff a :->: Wff a
           | Wff a :<->: Wff a
           deriving (Eq, Ord)

infixr 3 :&:
infixr 2 :|:
infixr 1 :->:
infixr 0 :<->:

type Env a = [(a, Bool)]
lookUp :: Eq a => Env a -> a -> Bool
lookUp v x = the [ b | (x', b) <- v, x == x' ]
    where the [z] = z
          the []  = error ("valuation undefined")
          the _  = error ("multiple values")
-- we represent valuations abstractly as functions.
-- The code above generates such a function from an association list.


-- We give different implementations of substitute, evaluate, atoms ...
-- using the fact that they are all instances of a similar pattern
-- If you don't understand this, don't worry, the functions behave 
-- exactly like the versions given in FP Tutorial 6

-- universal definition of functions from (a -> b) to (Wff a -> b)
-- for any b equipped with interpretations for T F Not :&: :|: and atoms
univ :: b ->             -- T
        b ->             -- F
        (b -> b) ->      -- not
        (b -> b -> b) -> -- and
        (b -> b -> b) -> -- or
        (a -> b) ->      -- action on atoms
        (Wff a -> b)     -- the result
univ t f n aa oo atm = 
  let ev T = t
      ev F = f
      ev (Not p)     = n (ev p)
      ev (p :&: q)   = ev p `aa` ev q
      ev (p :|: q)   = ev p `oo` ev q
      ev (p :->: q)  = ev (Not p :|: q)
      ev (p :<->: q) = ev ((Not p :|: q) :&: (p :|: Not q))
      ev (V a)       = atm a
  in ev

atoms :: Ord a => Wff a -> [a]
atoms      = univ [] [] id (\/) (\/) pure -- (\x -> [x])
  where -- represent sets as ordered lists \/ is union
    (\/) :: Ord a => [a] -> [a] -> [a]
    [] \/ ys = ys
    xs \/ [] = xs
    xs@(x:xt) \/ ys@(y:yt) =
      case compare x y of
       LT -> x : (xt \/ ys)
       EQ -> x : (xt \/ yt)
       GT -> y : (xs \/ yt)

-- Note that this eliminates implications
substitute :: (a -> b) -> Wff a -> Wff b
substitute = univ T F Not (:&:) (:|:) . (V.)

evaluate :: Wff Bool -> Bool
evaluate   = univ True False not (&&) (||) id

join :: Wff (Wff a) -> Wff a
join = univ T F Not (:&:) (:|:) id

-- Evaluates a wff in a given environment
eval :: Eq a => Env a -> Wff a -> Bool
eval v wff = evaluate (substitute (lookUp v) wff)

-- Creates all possible truth assignments for a set of atoms
envs :: [a] -> [Env a]
envs []      =  [[]]
envs (x:xs)  =  [ (x,False):e | e <- envs xs ] ++ [ (x,True ):e | e <- envs xs ]

-- Checks whether a wff is satisfiable
satisfiable :: Ord a => Wff a -> Bool
satisfiable p  =  or [ eval e p | e <- envs (atoms p) ]

-- Wff to cnf
impElim :: Wff a -> Wff a
impElim (Not p)     = Not (impElim p)
impElim (p :|: q)   = impElim p          :|: impElim q
impElim (p :&: q)   = impElim p          :&: impElim q
impElim (p :->: q)  = Not (impElim p)    :|: impElim q
impElim (p :<->: q) = impElim (p :->: q) :&: impElim (q :->: p)
impElim  x          = x

toNNF :: Wff a -> Wff a
toNNF (Not T)         = F
toNNF (Not F)         = T
toNNF (Not (Not p))   = toNNF p
toNNF (Not (p :&: q)) = toNNF (Not p) :|: toNNF (Not q)
toNNF (Not (p :|: q)) = toNNF (Not p) :&: toNNF (Not q)
toNNF (p :&: q)       = toNNF p :&: toNNF q
toNNF (p :|: q)       = toNNF p :|: toNNF q
toNNF  p              = p

toCNFList ::  Eq a => Wff a -> [[Wff a]]
toCNFList p = cnf ((toNNF.impElim) p) 
    where
      cnf F           =  [[]]
      cnf T           =  []
      cnf (V n)       =  [[V n]]
      cnf (Not (V n)) =  [[Not (V n)]]
      cnf (p :&: q)   =  nub (cnf p ++ cnf q)
      cnf (p :|: q)   =  [nub $ x ++ y | x <- cnf p, y <- cnf q]
      cnf _           = error ("toCNFList : argument not in NNF")


-- Clausal Forms from last week's CL tutorial 
data Literal a = N a | P a deriving (Ord, Eq, Show)
newtype Clause a = Or [Literal a] deriving (Ord, Eq, Show)
newtype Form a = And [Clause a] deriving (Eq, Show)
type Val a = [Literal a] -- must be consistent, it cannot contain both P a and N a               

(<&&>) :: Form a -> Form a  -> Form a  
And xs <&&> And ys =  And (xs ++ ys)
                                                          
neg :: Literal a -> Literal a
neg (P a) = N a
neg (N a) = P a

canonical :: Ord a => Form a -> Form a
canonical (And cs) = (And . uniqueSort) $ map (\ (Or xs) -> Or(uniqueSort xs)) cs

-- DPLL Procedure
(<<) :: Eq a => [Clause a] -> Literal a -> [Clause a] 
cs << x = [ Or (delete (neg x) ys) | Or ys <- cs, not $ x `elem` ys ]
 
dpll :: Eq a => Form a -> [[Literal a]]
dpll (And css) =
  let models [] = [[]]
      models cs =
        case prioritise cs of
        Or []          -> []   -- empty clause: no models
        Or [lit]       -> [ lit : m | m <- models (cs << lit)] -- unit clause
        Or (lit : _)   -> 
          [    lit : m | m <- models (cs << lit)]
          ++
          [neg lit : m | m <- models (cs << neg lit)]
  in models css
  where prioritise = minimumBy (comparing (\(Or lits) -> length lits))

-- Arbitrary instance for QuickCheck
instance Show a => Show (Wff a) where
    show  =  showWff

instance Arbitrary Atom where
  arbitrary = oneof $ map return [ A, B, C, D, W, X, Y, Z]

instance Arbitrary a => Arbitrary (Wff a) where
    arbitrary  =  sized wff
        where
          wff n | n <= 0     =  liftM V atom
                | otherwise  =  oneof [ liftM V atom
                                       , liftM Not subform
                                       , liftM2 (:|:)   subform subform
                                       , liftM2 (:&:)   subform subform
                                       , liftM2 (:->:)  subform subform
                                       , liftM2 (:<->:) subform subform
                                       ]
                 where
                   atom = arbitrary
                   subform   =  wff (n `div` 2)

-- Representation and pretty printing 
showWff :: Show a => Wff a -> String
showWff e = showsPrec 0 e ""
  where
    showsPrec _ (V a) = showString (show a)
    showsPrec _  F    = showChar 'F'
    showsPrec _  T    = showChar 'T'
    showsPrec p (a :&: b)
      = showParen (p>3)
         (showsPrec 3 a .showSpace .
          showString "&" . showSpace . 
          showsPrec 3 b )
    showsPrec p (a :|: b)
      = showParen (p>2)
         (showsPrec 2 a .showSpace .
          showString "|" . showSpace . 
          showsPrec 2 b )
    showsPrec p (a :->: b)
      = showParen (p>1)
         (showsPrec 1 a .showSpace .
          showString "->" . showSpace . 
          showsPrec 2 b )
    showsPrec p (a :<->: b)
      = showParen (p>0)
         (showsPrec 0 a .showSpace .
          showString "<->" . showSpace . 
          showsPrec 0 b )
    showsPrec _ (Not a) = 
      showString "~" . showsPrec 11 a
    showString :: String -> String -> String
    showString = (++)
    showChar :: Char -> String -> String
    showChar   = (:)
    showSpace :: String -> String
    showSpace  = showChar ' '
    showParen :: Bool -> (String -> String) -> (String -> String)
    showParen p s = if p then showChar '(' . s . showChar ')' else s

pretty :: Show a => Wff a -> String
pretty e = showsPrec 0 e ""
  where
    showsPrec _ (V a) = showString (show a)
    showsPrec _  F    = showChar '\x22A4'
    showsPrec _  T    = showChar '\x22A5'
    showsPrec p (a :&: b)
      = showParen (p>3)
         (showsPrec 3 a .showSpace .
          showChar '\x2227' . showSpace . 
          showsPrec 3 b )
    showsPrec p (a :|: b)
      = showParen (p>2)
         (showsPrec 2 a .showSpace .
          showChar '\x2228' . showSpace . 
          showsPrec 2 b )
    showsPrec p (a :->: b)
      = showParen (p>1)
         (showsPrec 1 a .showSpace .
          showChar '\x2B62' . showSpace . 
          showsPrec 2 b )
    showsPrec p (a :<->: b)
      = showParen (p>0)
         (showsPrec 0 a .showSpace .
          showChar '\x2B64' . showSpace . 
          showsPrec 0 b )
    showsPrec _ (Not a) = 
      showChar '\x00AC' . showsPrec 11 a
    showString :: String -> String -> String
    showString = (++)
    showChar :: Char -> String -> String
    showChar   = (:)
    showSpace :: String -> String
    showSpace  = showChar ' '
    showParen :: Bool -> (String -> String) -> (String -> String)
    showParen p s = if p then showChar '(' . s . showChar ')' else s

-- For later when Phil introduced Monads
-- fmap :: (a -> b ) -> Wff a -> Wff b
instance Functor Wff where
  fmap = substitute

-- (<*>) :: Wff (a -> b ) -> Wff a -> Wff b
instance Applicative Wff where
  pure = V
  (<*>) pf pa = univ T F Not (:&:) (:|:) (<$>pa) pf

-- join = univ T F Not (:&:) (:|:) id          
-- (>>=) :: Wff a -> (a -> Wff b) -> Wff b
instance Monad Wff where
  (>>=) = flip (univ T F Not (:&:) (:|:))
