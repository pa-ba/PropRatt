import Test.QuickCheck
import Name.Properties (prop_sig_is_later_sig_after_tick_on_later_sig)

main :: IO ()
main = do
    quickCheck prop_sig_is_later_sig_after_tick_on_later_sig
