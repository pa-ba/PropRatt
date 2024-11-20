module Main (main) where
import Test.QuickCheck
import Name.ARat
import Name.Generators
import Name.Utilities
import Name.Properties


main :: IO ()
main = do
    -- The two arbitrarily generated signals
    value <- generate (arbitrary :: Gen (Sig Int))
    value2 <- generate (arbitrary :: Gen (Sig Int))

    -- a representation of the two signals zipped to a tuple signal
    let tupleSig = prop_zip_zipped value value2

    -- zipping the two signals and then stripping it to only get values from "original". This returns a boolean determining whether the result of zipping and stripping is a stutter of the original signal.
    let stutterSig = prop_zip_then_strip_sig value value2

    -- Boolean value determining whether the result of zipping and stripping is a stutter of the original signal
    let isStutter = prop_zip_is_stuttering_sig value stutterSig

    -- printing results for debugging:
    putStrLn (show value) -- original signal
    putStrLn (show value2) -- some signal 
    putStrLn (show tupleSig) -- the tuple sig; a result of zipping the two above
    putStrLn (show stutterSig) -- the stuttering; stripping the tuple sig to only have first values
    putStrLn (show isStutter) -- boolean; is it a stuttering?
