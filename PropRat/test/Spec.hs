import Test.QuickCheck
import PropRat.Properties (prop_sig_is_later_sig_after_tick_on_later_sig, prop_is_stuttering)

main :: IO ()
main = do
    quickCheck prop_is_stuttering
    quickCheck prop_sig_is_later_sig_after_tick_on_later_sig
