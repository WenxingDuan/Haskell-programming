-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 7
--
-- Week 7(29 Oct.- 01 Nov.)
-- module Main where
import           LSystem
import           Test.QuickCheck

pathExample = (Go 30 :#: Turn 120 :#: Go 30 :#: Turn 120 :#: Go 30)

-- 1a. split
split :: Command -> [Command]
split (command1 :#: command2) = split command1 ++ split command2
split Sit                     = []
split command3                = [command3]

-- 1b. join
join :: [Command] -> Command
join [] = Sit
join a  = head a :#: join (tail a)

-- 1c. equivalent
-- equivalent
equivalent =
  split ((Go 3 :#: Turn 4) :#: (Sit :#: Go 7)) ==
  split (((Sit :#: Go 3) :#: Turn 4) :#: Go 7)

-- 1d. testing join and split
-- prop_split_join
prop_split_join :: Command -> Bool
prop_split_join c = join (split c) == c

-- prop_split
prop_split :: Command -> Bool
prop_split a = check_split (split a)
  where
    check_split :: [Command] -> Bool
    check_split []        = True
    check_split [a :#: b] = False
    check_split (c:d)     = (c /= Sit) && (check_split d)

-- 2a. copy
copy :: Int -> Command -> Command
copy 0 cmd   = Sit
copy num cmd = cmd :#: (copy (num - 1) cmd)

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon = undefined

-- 2c. polygon
polygon :: Distance -> Command
polygon a = copy 5 (Go a :#: Turn 72.0)

-- 3. spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral dis 0 dis_inc ang = Sit
spiral dis num dis_inc ang =
  Go dis :#: Turn ang :#: (spiral (dis + dis_inc) (num - 1) dis_inc ang)

-- 4. pre_optimise
-- Remember that Go does not take negative arguments.
pre_optimise :: Command -> Command
pre_optimise (Go dis_1 :#: Turn ang :#: Go dis_2 :#: a) =
  pre_optimise (Go (dis_1 + dis_2) :#: Turn ang :#: (pre_optimise a))
pre_optimise (Go dis_1 :#: Turn ang_1 :#: Turn ang_2 :#: a) =
  pre_optimise (Go dis_1 :#: Turn (ang_1 + ang_2) :#: (pre_optimise a))
pre_optimise (Go dis_1 :#: Go dis_2 :#: a) =
  pre_optimise (Go (dis_1 + dis_2) :#: (pre_optimise a))
pre_optimise (Turn ang_1 :#: Go dis_2 :#: Turn ang_2 :#: a) =
  pre_optimise (Go dis_2 :#: Turn (ang_1 + ang_2) :#: (pre_optimise a))
pre_optimise (Turn ang_1 :#: Go dis_2 :#: Go dis_1 :#: a) =
  pre_optimise (Go (dis_1 + dis_2) :#: Turn (ang_1) :#: (pre_optimise a))
pre_optimise (Turn ang_1 :#: Turn ang_2 :#: a) =
  pre_optimise (Turn (ang_1 + ang_2) :#: (pre_optimise a))
pre_optimise (Go dis_1 :#: Turn ang :#: Go dis_2) =
  pre_optimise (Go (dis_1 + dis_2) :#: Turn ang)
pre_optimise (Go dis_1 :#: Turn ang_1 :#: Turn ang_2) =
  pre_optimise (Go dis_1 :#: Turn (ang_1 + ang_2))
pre_optimise (Go dis_1 :#: Go dis_2) = pre_optimise (Go (dis_1 + dis_2))
pre_optimise (Turn ang_1 :#: Go dis_2 :#: Turn ang_2) =
  pre_optimise (Go dis_2 :#: Turn (ang_1 + ang_2))
pre_optimise (Turn ang_1 :#: Go dis_2 :#: Go dis_1) =
  pre_optimise (Go (dis_1 + dis_2) :#: Turn (ang_1))
pre_optimise (Turn ang_1 :#: Turn ang_2) = pre_optimise (Turn (ang_1 + ang_2))
pre_optimise (Turn ang_1 :#: Go dis_2) = Turn ang_1 :#: Go dis_2
pre_optimise (Go dis_2 :#: Turn ang_1) = Go dis_2 :#: Turn ang_1
pre_optimise (Go dis_1) = Go dis_1
pre_optimise (Turn ang_1) = Turn ang_1

clear :: Command -> Command
clear (Sit :#: a) = clear a
clear (a :#: b)   = a :#: clear b
clear a           = a

optimise :: Command -> Command
optimise a = pre_optimise (clear a)

-- L-Systems
-- 5. arrowhead
arrowhead :: Int -> Command
arrowhead x = f x
  where
    f 0 = GrabPen red :#: Go 10
    f x = g (x - 1) :#: p :#: f (x - 1) :#: p :#: g (x - 1)
    g 0 = Go 10
    g x = f (x - 1) :#: n :#: g (x - 1) :#: n :#: f (x - 1)
    p = Turn (-60)
    n = Turn (60)

-- 6. snowflake
snowflake :: Int -> Command
snowflake x = f x :#: n :#: n :#: f x :#: n :#: n :#: f x :#: n :#: n
  where
    f 0 = Go 10
    f x =
      f (x - 1) :#: p :#: f (x - 1) :#: n :#: n :#: f (x - 1) :#: p :#:
      f (x - 1)
    p = Turn (60)
    n = Turn (-60)

-- 7. hilbert
hilbert :: Int -> Command
hilbert x = l x
  where
    l 0 = GrabPen red :#: Go 10
    l x =
      p :#: r (x - 1) :#: f (x - 1) :#: n :#: l (x - 1) :#: f (x - 1) :#:
      l (x - 1) :#:
      n :#:
      f (x - 1) :#:
      r (x - 1) :#:
      p
    r 0 = GrabPen blue :#: Go 10
    r x =
      n :#: l (x - 1) :#: f (x - 1) :#: p :#: r (x - 1) :#: f (x - 1) :#:
      r (x - 1) :#:
      p :#:
      f (x - 1) :#:
      l (x - 1) :#:
      n
    f x = GrabPen green :#: Go 10
    p = Turn (90)
    n = Turn (-90)

--------------------------------------------------
--------------------------------------------------
---------------- Optional Material ---------------
--------------------------------------------------
--------------------------------------------------
-- Bonus L-Systems
peanoGosper :: Int -> Command
peanoGosper x = f x
  where
    f 0 = Go 10
    f x =
      f (x - 1) :#: p :#: g (x - 1) :#: p :#: p :#: g (x - 1) :#: n :#:
      f (x - 1) :#:
      n :#:
      n :#:
      f (x - 1) :#:
      f (x - 1) :#:
      n :#:
      g (x - 1) :#:
      p
    g 0 = Go 10
    g x =
      n :#: f (x - 1) :#: p :#: g (x - 1) :#: g (x - 1) :#: p :#: p :#:
      g (x - 1) :#:
      p :#:
      f (x - 1) :#:
      n :#:
      n :#:
      f (x - 1) :#:
      n :#:
      g (x - 1)
    p = Turn (60)
    n = Turn (-60)

cross :: Int -> Command
cross x = f (x - 1) :#: n :#: f (x - 1) :#: n :#: f (x - 1) :#: n :#: f (x - 1)
  where
    f 0 = Go 10
    f x =
      f (x - 1) :#: n :#: f (x - 1) :#: p :#: f (x - 1) :#: p :#: f (x - 1) :#:
      f (x - 1) :#:
      n :#:
      f (x - 1) :#:
      n :#:
      f (x - 1) :#:
      p :#:
      f (x - 1)
    p = Turn (90)
    n = Turn (-90)

branch :: Int -> Command
branch x = g x
  where
    g 0 = Go 10
    g x =
      f (x - 1) :#: n :#: branch (branch (g (x - 1)) :#: n :#: g (x - 1)) :#: p :#:
      f (x - 1) :#:
      branch (p :#: f (x - 1) :#: g (x - 1)) :#:
      n :#:
      g (x - 1)
    f 0 = Go 10
    f x = f (x - 1) :#: f (x - 1)
    p = Turn (22.5)
    n = Turn (-22.5)

thirtytwo :: Int -> Command
thirtytwo x =
  f (x - 1) :#: p :#: f (x - 1) :#: p :#: f (x - 1) :#: p :#: f (x - 1)
  where
    f 0 = Go 10
    f x =
      n :#: f (x - 1) :#: p :#: f (x - 1) :#: n :#: f (x - 1) :#: n :#:
      f (x - 1) :#:
      p :#:
      f (x - 1) :#:
      p :#:
      f (x - 1) :#:
      f (x - 1) :#:
      n :#:
      f (x - 1) :#:
      p :#:
      f (x - 1) :#:
      p :#:
      f (x - 1) :#:
      f (x - 1) :#:
      p :#:
      f (x - 1) :#:
      n :#:
      f (x - 1) :#:
      n :#:
      f (x - 1) :#:
      f (x - 1) :#:
      p :#:
      f (x - 1) :#:
      f (x - 1) :#:
      n :#:
      f (x - 1) :#:
      f (x - 1) :#:
      p :#:
      f (x - 1) :#:
      p :#:
      f (x - 1) :#:
      n :#:
      f (x - 1) :#:
      f (x - 1) :#:
      n :#:
      f (x - 1) :#:
      n :#:
      f (x - 1) :#:
      p :#:
      f (x - 1) :#:
      f (x - 1) :#:
      n :#:
      f (x - 1) :#:
      n :#:
      f (x - 1) :#:
      p :#:
      f (x - 1) :#:
      p :#:
      f (x - 1) :#:
      n :#:
      f (x - 1) :#:
      p
    p = Turn (90)
    n = Turn (-90)

main :: IO ()
main = display pathExample
