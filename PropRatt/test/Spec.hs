{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

import Test.QuickCheck
import PropRatt.LTL 
import PropRatt.Generators
import PropRatt.Value
import PropRatt.AsyncRat
import AsyncRattus.InternalPrimitives
import PropRatt.Utilities hiding ()
import AsyncRattus.Signal

instance Stable (Value a) where 
instance Stable Int where 

prop_interleave :: Property
prop_interleave = forAll (generateSignals @[Int, Int]) $ \intSignals ->
    let interleavedSig = aRatInterleave (box (+)) (getLater $ first intSignals) (getLater $ second intSignals)
        notALaterSig = 0 ::: interleavedSig
        signalsUnderTest = prepend notALaterSig $ flatten intSignals
    in evaluate (Next (Always (Or (Or (Now ((Index First) |==| (Index Second))) (Now ((Index First) |==| (Index Third)))) (Now (((+) <$> (Index Second) <*> (Index Third)) |==| (Index First)))))) signalsUnderTest

-- Jump property (value is either equal to the original signal or equal to 10 (which is the number of the signal of the dummy function))
-- prop_jump :: Property
-- prop_jump = forAll (generateSignals @[Int, Int]) $ \intSignals ->
--     let jumpSig = aRatJump (box jumpFunc) (first intSignals)
--         signalsUnderTest = prepend jumpSig $ flatten intSignals
--     in evaluate (Always (Or (Now (Equals (Index First) (Index Second))) (Now (Equals (Index First) (Pure 10))))) signalsUnderTest

-- A switched signal has values equal to the first signal until its values equal values from the third signal
prop_switchedSignal :: Property
prop_switchedSignal = forAll (generateSignals @[Int, Int]) $ \intSignals -> 
    let switched = aRatSwitch (first intSignals) (getLater (second intSignals))
        signalsUnderTest = prepend switched $ flatten intSignals
    in evaluate (Until (Now ((Index First) |==| (Index Second))) (Now ((Index First) |==| (Index Third)))) signalsUnderTest

-- A buffered signal is always one tick behind
prop_buffer :: Property
prop_buffer = forAll (generateSignals @Int) $ \intSignals ->
    let bufferedSig = aRatBuffer 10 (first intSignals)
        signalsUnderTest = prepend bufferedSig $ flatten intSignals
    in evaluate (Next (Always (Now ((Index First) |==| (Index (Previous Second)))))) signalsUnderTest

prop_shouldFail :: Property
prop_shouldFail = forAll (generateSignals @Int) $ \intSignals ->
    let bufferedSig = aRatBuffer 10 (first intSignals)
        signalsUnderTest = prepend bufferedSig $ flatten intSignals
    in evaluate (Next (Always (Now ((Pure 10) |==| (Index (Previous (Second))))))) signalsUnderTest

main :: IO ()
main = do
    quickCheck prop_interleave
    quickCheck prop_switchedSignal
    quickCheck prop_buffer   
    quickCheck prop_shouldFail 