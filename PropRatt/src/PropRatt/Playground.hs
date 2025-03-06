{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Redundant lambda" #-}
module PropRatt.Playground where
    
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
