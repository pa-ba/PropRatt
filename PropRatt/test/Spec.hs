import Test.QuickCheck
import PropRatt.Properties (prop_eventual_equality, prop_is_stuttering, prop_is_stuttering_failing, prop_eventual_equality_failing)

main :: IO ()
main = do
    quickCheck prop_is_stuttering_failing
    quickCheck prop_eventual_equality_failing
    quickCheck prop_is_stuttering
    quickCheck prop_eventual_equality
