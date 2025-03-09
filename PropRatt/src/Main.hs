{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# LANGUAGE TypeOperators, FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Main (main) where
import PropRatt.LTL
import AsyncRattus.Strict
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith, take)
import PropRatt.Value
import PropRatt.AsyncRat
import PropRatt.Utilities (getLater)
import PropRatt.Generators ()
import Test.QuickCheck (generate)
import AsyncRattus
import AsyncRattus.Signal

main :: IO ()
main = do
    intSignals <- generate $ generateSigs @[Int, Int]
    let switched = aRatSwitch (first intSignals) (getLater (second intSignals))
    let signalsUnderTest = prepend switched $ flatten intSignals
    print (evaluate (Until (Now (\ls -> first ls ?= second ls)) (Now (\ls -> first ls ?= third ls))) signalsUnderTest)

    let s1 = aRatZip (first intSignals) (second intSignals)
    let signalsUnderTest2 = prepend s1 $ flatten intSignals

    print (evaluate (Always (Or (Now (\hls -> (fst' (current' $ first hls) :: Int) == (current' (second hls) :: Int))) (Now (\hls -> (snd' (current' $ first hls) :: Int) == (current' (third hls) :: Int))))) signalsUnderTest2)

    -- Jump property (value is either equal to the original signal or equal to 10 (which is the number of the signal of the dummy function))
    let jumpedSig = aRatJump (box jumpFunc) (first intSignals)
    let signalsUnderTest3 = prepend jumpedSig $ flatten intSignals
    print (evaluate (Always (Or (Now (\hls -> (current' $ first hls) == (current' $ second hls))) (Now (\hls -> (current' $ first hls) == 10)))) signalsUnderTest3)

    -- Buffer: Now for original signal equals next in returned signal? we need history to test this property!
    let bufferedSig = aRatBuffer 10 (first intSignals)
    let signalsUnderTest4 = prepend bufferedSig $ flatten intSignals
    -- print (evaluate (Always ()) signalsUnderTest4)

    -- Interleave property (Either value is equal to sig 1 or sig 2 or value is the result of the function provided) 
    let interleavedSig = aRatInterleave (box interleaveFunc) (getLater $ first intSignals) (getLater $ second intSignals)
    let notALaterSig = 1 ::: interleavedSig -- Fails test atm, because we just naively put 1 in front of a later signal. We might want to consider a way of handling either signals or later signals in our implementation
    let signalsUnderTest5 = prepend notALaterSig $ flatten intSignals
    print (evaluate (Always (Or (Or (Now (\hls -> (current' $ first hls) == (current' $ second hls))) (Now (\hls -> (current' $ first hls) == (current' $ third hls)))) (Now (\hls -> (current' $ first hls) == ((current' $ second hls) * (current' $ third hls) + 2))))) signalsUnderTest5)

