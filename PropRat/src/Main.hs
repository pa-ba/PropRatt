{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Main (main) where
import Test.QuickCheck
import PropRat.AsyncRat
import PropRat.Generators
import PropRat.Properties


main :: IO ()
main = do
    -- testing switch and tail equality
    startSignal <- generate (arbitrary :: Gen (Sig Int))
    laterSignal <- generate (arbitrary :: Gen (Sig Int))
    let later = getLater laterSignal
    let switched = aRatSwitch startSignal later
    let switchedCorrectly = prop_sig_is_later_sig_after_tick_on_later_sig startSignal laterSignal
    --putStrLn (show startSignal)
    --putStrLn (show laterSignal)
    --putStrLn (show switched)
    putStrLn (show switchedCorrectly)

    -- testing zip and stutter
    xs <- generate (arbitrary :: Gen (Sig Int))
    ys <- generate (arbitrary :: Gen (Sig Int))
    let stutter = stuttering xs ys
    let zipped = aRatZip xs ys
    let isStuttering = prop_is_stuttering xs ys
    --putStrLn (show xs)
    --putStrLn (show ys)
    --putStrLn (show zipped)
    --putStrLn (show stutter)
    putStrLn (show isStuttering)