import Test.QuickCheck
import PropRatt.Properties (prop_eventual_equality, prop_is_stuttering)

main :: IO ()
main = do
    quickCheck prop_is_stuttering
    quickCheck prop_eventual_equality
