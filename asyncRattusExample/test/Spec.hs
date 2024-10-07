{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}

import AsyncRattus
import AsyncRattus.Channels
import AsyncRattus.Signal hiding (map, zip)
import Control.Concurrent (forkIO)
import Lib
import Test.QuickCheck
import Prelude hiding (map)

-- Boxed function to provide to the map function
someFunction :: Box (Int -> Int)
someFunction = box (+ 100)

-- Signal generating function that starts from an initial value and increments every second
someSignal :: Int -> Sig Int
someSignal init = scan (box (\n _ -> n + 25)) init everySecondSig

-- trying to take the 10 first elements of a signal. Doesn't work
-- takeFirstN :: Int -> Sig Int -> [Int]
-- takeFirstN 0 _ = return [0] -- Base case: no more elements to collect
-- takeFirstN n (x ::: xs) = do
--   rest <- takeFirstN (n - 1) (adv xs)
--   return (x : rest) -- Combine current value with the rest of the list

-- Property to test if mapping the same function to the same signal gives the same result
prop_naiveTest :: Int -> Bool
prop_naiveTest initVal =
  let signal = someSignal initVal
      mapped = mapWithFunc2 someFunction signal -- Map the function over the signal
      res = take 10 mapped -- Trying to take the first 10 results, but we are not allowed to on Signal
   in all (\(x, n) -> n == (100 + initVal + (25 * x))) (zip [0 ..] res) -- Validate the transformation

-- Main function to run the test
main :: IO ()
main = do
  quickCheck prop_naiveTest
