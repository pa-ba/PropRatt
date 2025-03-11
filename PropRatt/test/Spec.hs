{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

import Test.QuickCheck
import PropRatt.Properties (prop_eventual_equality, prop_is_stuttering, prop_is_stuttering_failing, prop_eventual_equality_failing)
import PropRatt.LTL
import PropRatt.Generators
import PropRatt.Value
import PropRatt.AsyncRat
import AsyncRattus.InternalPrimitives
import PropRatt.Utilities hiding ()

instance Stable (Value a) where 
instance Stable Int where 

-- Zip example
    -- let s1 = aRatZip (first intSignals) (second intSignals)
    -- let signalsUnderTest2 = prepend s1 $ flatten intSignals
    --print (evaluate (Always (Or (Now (\hls -> (fst' (current' $ first hls) :: Int) == (current' (second hls) :: Int))) (Now (\hls -> (snd' (current' $ first hls) :: Int) == (current' (third hls) :: Int))))) signalsUnderTest2)

    -- -- Interleave property (Either value is equal to sig 1 or sig 2 or value is the result of the function provided) 
    -- let interleavedSig = aRatInterleave (box interleaveFunc) (getLater $ first intSignals) (getLater $ second intSignals)
    -- let notALaterSig = 1 ::: interleavedSig -- Fails test atm, because we just naively put 1 in front of a later signal. We might want to consider a way of handling either signals or later signals in our implementation
    -- let signalsUnderTest5 = prepend notALaterSig $ flatten intSignals
    -- print (evaluate (Always (Or (Or (Now (\hls -> (current' $ first hls) == (current' $ second hls))) (Now (\hls -> (current' $ first hls) == (current' $ third hls)))) (Now (\hls -> (current' $ first hls) == ((current' $ second hls) * (current' $ third hls) + 2))))) signalsUnderTest5)


-- Jump property (value is either equal to the original signal or equal to 10 (which is the number of the signal of the dummy function))
prop_jump :: Property
prop_jump = forAll (generateSignals @[Int, Int]) $ \intSignals ->
    let jumpSig = aRatJump (box jumpFunc) (first intSignals)
        signalsUnderTest = prepend jumpSig $ flatten intSignals
    in evaluate (Always (Or (Now (Equals First Second)) (Now (Equals First (Const 10))))) signalsUnderTest

-- A switched signal has values equal to the first signal until its values equal values from the third signal
prop_switchedSignal :: Property
prop_switchedSignal = forAll (generateSignals @[Int, Int]) $ \intSignals -> 
    let switched = aRatSwitch (first intSignals) (getLater (second intSignals))
        signalsUnderTest = prepend switched $ flatten intSignals
    in evaluate (Until (Now (Equals First Second)) (Now (Equals First Third))) signalsUnderTest

-- A buffered signal is always one tick behind
prop_buffer :: Property
prop_buffer = forAll (generateSignals @[Int, Int]) $ \intSignals ->
    let bufferedSig = aRatBuffer 10 (first intSignals)
        signalsUnderTest = prepend bufferedSig $ flatten intSignals
    in evaluate (Next (Always (Now (Equals First (Previous Second))))) signalsUnderTest

main :: IO ()
main = do
    quickCheck prop_switchedSignal
    quickCheck prop_buffer
    quickCheck prop_jump
    