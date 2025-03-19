{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# HLINT ignore "Redundant bracket" #-}

import Test.QuickCheck
import PropRatt.LTL 
import PropRatt.Generators
import PropRatt.Value
import PropRatt.AsyncRat
import AsyncRattus.InternalPrimitives
import Prelude hiding (zip, map)
import AsyncRattus.Signal
import GHC.Natural (Natural)
import AsyncRattus.Strict
import System.IO.Unsafe (unsafePerformIO)
import PropRatt.Utilities (mkSigOne, getLater)

prop_interleave :: Property
prop_interleave = forAll (generateSignals @[Int, Int]) $ \intSignals ->
    let interleavedSig = interleave (box (+)) (getLater $ first intSignals) (getLater $ second intSignals)
        notALaterSig = 0 ::: interleavedSig
        signalsUnderTest = prepend notALaterSig $ flatten intSignals
    in (evaluate (Next (Always (
                ((Now ((Index First) |==| (Index Second))) 
                `Or`
                (Now ((Index First) |==| (Index Third))))
                `Or`
                (Now (((+) <$> (Index Second) <*> (Index Third)) |==| (Index First)))))) signalsUnderTest)

-- Jump property (value is either equal to the original signal or equal to 10 (which is the number of the signal of the dummy function))
prop_jump :: Property
prop_jump = forAll (generateSignals @Int) $ \intSignals ->
   let jumpFunc = box (\n -> if n > 10 then Just' mkSigOne else Nothing')
       jumpSig = jump jumpFunc (first intSignals)
       signalsUnderTest = prepend jumpSig $ flatten intSignals
    in evaluate (Always ( 
        (Now ((Index First) |==| (Index Second)))
        `Or` 
        (Now ((Index First) |==| (Pure 1))))) signalsUnderTest

-- prefix sum are monotonically increasing
-- only holds for nat numbers.. do we need another gen sig?
prop_scan :: Property
prop_scan =  forAll (generateSignals @Int) $ \intSignals -> 
    let prefixSum = scan (box (+)) 0 (first intSignals)
        signalsUnderTest = prepend prefixSum $ flatten intSignals
    in evaluate (Next (Always 
        (Now ((Index (Previous First)) |<| (Index First))))) signalsUnderTest

-- A switched signal has values equal to the first signal until its values equal values from the third signal
prop_switchedSignal :: Property
prop_switchedSignal = forAll (generateSignals @[Int, Int]) $ \intSignals -> 
    let switched = switch (first intSignals) (getLater (second intSignals))
        signalsUnderTest = prepend switched $ flatten intSignals
    in evaluate (Until 
        (Now ((Index First) |==| (Index Second))) 
        (Now ((Index First) |==| (Index Third)))) signalsUnderTest

-- A buffered signal is always one tick behind
prop_buffer :: Property
prop_buffer = forAll (generateSignals @Int) $ \intSignals ->
    let bufferedSig = buffer 10 (first intSignals)
        signalsUnderTest = prepend bufferedSig $ flatten intSignals
    in evaluate (Next (Always (Now ((Index First) |==| (Index (Previous Second)))))) signalsUnderTest

-- false positive, does not test anything useful since the stopped signal never ticks
prop_stop :: Property
prop_stop = forAll (generateSignals @Bool) $ \intSignals ->
    let stopped = stop (box (id)) (first intSignals)
        signalsUnderTest = prepend stopped $ flatten intSignals
    in evaluate (Always (
        (Now ((Index First) |==| (Pure True))) 
        `Implies`
        (Next (Now ((Index First) |==| (Pure True)))))) signalsUnderTest

-- A zipped signal (first signal) always has fst' values from second signal and snd' values from third signal
prop_zip :: Property
prop_zip = forAll (generateSignals @[Int, Int]) $ \intSignals -> 
    let s1 = zip (first intSignals) (second intSignals)
        signalsUnderTest = prepend s1 $ flatten intSignals
    in (evaluate (Always 
        (Now ((fst' <$> (Index First)) |==| (Index Second))) 
        `And` 
        (Now ((snd' <$> (Index First)) |==| (Index Third)))) 
        signalsUnderTest)

-- dummy failing property for sanity check :D 
prop_shouldFail :: Property
prop_shouldFail = forAll (generateSignals @Int) $ \intSignals ->
    let bufferedSig = buffer 10 (first intSignals)
        signalsUnderTest = prepend bufferedSig $ flatten intSignals
    in evaluate (Next (Always (Now ((Pure 10) |==| (Index (Previous (Second))))))) signalsUnderTest

main :: IO ()
main = do
    quickCheck prop_interleave
    quickCheck prop_switchedSignal
    quickCheck prop_buffer  
    quickCheck prop_zip 
    quickCheck prop_jump
    quickCheck prop_stop
    quickCheck prop_scan
    quickCheck prop_shouldFail