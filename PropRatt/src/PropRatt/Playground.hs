{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Redundant lambda" #-}
module PropRatt.Playground where
import PropRatt.LTL (Atom (..))

-- Why are function types not stable?
-- Because they carry data in their closure.
-- What is a function closure??? 

-- Take an argument and return another function that "captures" the argument from its enviroment/context where it is defined.
-- makeAdder :: Int -> (Int -> Int)
-- makeAdder x = \y -> x + y

-- -- We supply the closure in the context/eviroment of "addFive"
-- add :: Int -> Int
-- add = makeAdder 5

-- -- But we can invoke addFive in another context, thereby acting on a value defined in another context.
-- testClosure :: Int
-- testClosure = addFive 10 

-- -- THIS IS WHY FUNCTION TYPES ARE NOT STABLE :D!
-- -- A function may capture stale data in its closure (data from an old timestep)
-- -- And be invoked in a future timstep. This would break causality!

------ applicative

ex1 :: Atom ts Int
ex1 = pure 5 -- det samme som (Pure 5)

ex2 :: Atom ts Int
ex2 = fmap (+2) ex1 -- Pure 7

myFunc :: Int -> Int
myFunc n = n * 2

functionInPure :: Atom ts (Int -> Int)
functionInPure = pure myFunc

ex3 :: Atom ts Int
ex3 = functionInPure <*> ex2 -- Pure (*2) :: Atom ts (Int -> Int) <*> Pure 7 == Pure (7*2) == Pure 14

-- chaining partial application using the sequence <*> function
ex4 :: Atom ts Int
ex4 = pure (*) <*> ex1 <*> ex2 -- Pure (Int -> Int -> Int) <*> Pure (5) <*> Pure (7) == Pure (5*7) == Pure 35

ex5 :: Atom ts (Int -> Int)
ex5 = (*) <$> ex1

ex6 :: Atom ts Int
ex6 = Apply 
       (Apply (Pure (*)) nested)  -- Apply the multiplication function to the result of nestedAdd
       (Pure 5)
    where
        nested = Apply (Apply (Pure (+)) (Pure 3)) (Pure 4)

ex7 :: Atom ts Int
ex7 = Apply (Pure (+)) (Pure 3) <*> Pure 5

-- nestede apply