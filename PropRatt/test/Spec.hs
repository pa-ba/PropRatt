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

instance Stable (Value a) where 
instance Stable Int where 



main :: IO ()
main = do
    intSignals <- generate $ generateSigs @[Int, Int]
    let bufferedSig = aRatBuffer 10 (first intSignals)
    let signalsUnderTest4 = prepend bufferedSig $ flatten intSignals
    quickCheck (evaluate (Next (Always (Now (Equals First (Prior Second))))) signalsUnderTest4)
     


