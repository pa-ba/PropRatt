{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

import Test.QuickCheck
import PropRatt.Properties (prop_eventual_equality, prop_is_stuttering, prop_is_stuttering_failing, prop_eventual_equality_failing)
import PropRatt.LTL
import PropRatt.Generators
import PropRatt.Value
import PropRatt.AsyncRat
import AsyncRattus.InternalPrimitives
import AsyncRattus.Signal
import PropRatt.Utilities hiding (first, second)

instance Stable (Value a) where 
instance Stable Int where 

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
    in evaluate (Next (Always (Now (Equals First (Prior Second))))) signalsUnderTest

main :: IO ()
main = do
    quickCheck prop_switchedSignal
    quickCheck prop_buffer
    