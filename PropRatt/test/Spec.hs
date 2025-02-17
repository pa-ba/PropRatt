import Test.QuickCheck
import PropRatt.Properties (prop_eventual_equality, prop_is_stuttering, prop_is_stuttering_failing, prop_eventual_equality_failing)
import PropRatt.LTL (Pred (..), evaluateLTLSig)

import AsyncRattus.InternalPrimitives
import AsyncRattus.Signal


main :: IO ()
main = do
    quickCheck prop_is_stuttering
    quickCheck prop_eventual_equality


