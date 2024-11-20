import Test.QuickCheck
import Name.Properties
import Name.Utilities
import Name.Generators

prop_sig_size_non_negative :: Sig Int -> Bool
prop_sig_size_non_negative sig = sizeSig sig 0 >= 0


main :: IO ()
main = do
    quickCheck (prop_zip_then_strip)
