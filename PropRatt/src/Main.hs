{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Main (main) where
import PropRatt.LTL
import AsyncRattus.Strict
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith, take)
import PropRatt.Value
import PropRatt.AsyncRat
import PropRatt.Utilities (getLater)
import PropRatt.Generators ()
import Test.QuickCheck (generate)
import AsyncRattus.InternalPrimitives
import AsyncRattus.Signal

main :: IO ()
main = do
    print "hej"
    -- intSignals <- generate $ generateSignals @Int
    -- let bufferedSig = aRatBuffer 10 (first intSignals)
    --     signalsUnderTest = prepend bufferedSig $ flatten intSignals
    
    -- print signalsUnderTest
    -- print (evaluate (Next (Always (Now ((Index First) |==| (Index (Previous Second)))))) signalsUnderTest)

    intSignals <- generate $ generateSignals @[Int, Int]
    -- Interleave:
    let interleavedSig = interleave (box (+)) (getLater $ first intSignals) (getLater $ second intSignals)
    let notALaterSig = 1 ::: interleavedSig -- Fails test atm, because we just naively put 1 in front of a later signal. We might want to consider a way of handling either signals or later signals in our implementation
    let signalsUnderTest5 = prepend notALaterSig $ flatten intSignals
    print (evaluate (Always (Or (Or (Now ((Index First) |==| (Index Second))) (Now ((Index First) |==| (Index Third)))) (Now (((+) <$> (Index Second) <*> (Index Third)) |==| (Index First))))) signalsUnderTest5)

    -- Zip:
    let s1 = zip (first intSignals) (second intSignals)
    let signalsUnderTest2 = prepend s1 $ flatten intSignals
    print (evaluate (Always (Or (Now ((fst' <$> (Index First)) |==| (Index Second))) (Now ((snd' <$> (Index First)) |==| (Index Third))))) signalsUnderTest2)
--    print (evaluate (Always (Or (Now (\hls -> (fst' (current' $ first hls) :: Int) == (current' (second hls) :: Int))) (Now (\hls -> (snd' (current' $ first hls) :: Int) == (current' (third hls) :: Int))))) signalsUnderTest2)
