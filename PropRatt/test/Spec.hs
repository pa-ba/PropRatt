import Test.QuickCheck
import PropRatt.Properties (prop_eventual_equality, prop_is_stuttering, prop_is_stuttering_failing, prop_eventual_equality_failing)
import PropRatt.LTL (Pred (..), evaluateLTLSig)

import AsyncRattus.InternalPrimitives
import AsyncRattus.Signal


prop_test :: Sig Int -> Bool
prop_test signall = do
    evaluateLTLSig (Always (Atom (<= 0))) signall


main :: IO ()
main = do
    -- quickCheck prop_is_stuttering_failing
    -- quickCheck prop_eventual_equality_failing
    -- quickCheck prop_is_stuttering
    -- quickCheck prop_eventual_equality

    quickCheck prop_test

