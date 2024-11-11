import Test.QuickCheck
import Name.Properties
import Name.Utilities
import Name.Generators
import AsyncRattus.Signal
import Prelude hiding (const)

prop_sig_size_non_negative :: Sig Int -> Bool
prop_sig_size_non_negative sig = sizeSig sig 0 >= 0


main :: IO ()
main = do
    quickCheck prop_map_id_Float
    quickCheck prop_map_id_Int


