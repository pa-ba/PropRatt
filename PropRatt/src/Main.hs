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

import System.IO.Unsafe (unsafePerformIO)

main :: IO ()
main = do
    intSignals <- generate $ generateSignals @Int

    let jumpFunc = box (\n ->
            let intSig = unsafePerformIO $ generate $ generateSignals @Int
                testmakesig = map (box (\_ -> 1)) (first intSig)
            in if n > 10 then Just' testmakesig else Nothing')

    let jumpSig = jump jumpFunc (first intSignals)
    let signalsUnderTest = prepend jumpSig $ flatten intSignals

    print (evaluate (Always ( 
        (Now ((Index First) |==| (Index Second)))
        `Or` 
        (Now ((Index First) |==| (Pure 1))))) signalsUnderTest)







    -- let interleavedSig = interleave (box (+)) (getLater $ first intSignals) (getLater $ second intSignals)
    --     notALaterSig = 0 ::: interleavedSig
    --     signalsUnderTest = prepend notALaterSig $ flatten intSignals
    --     testmakesig = mkSig (box (delay (1)))


    
    -- print signalsUnderTest
    -- print (evaluate (Next (Always (
    --     ((Now ((Index First) |==| (Index Second))) 
    --     `Or` 
    --     (Now ((Index First) |==| (Index Third))))
    --     `Or` 
    --     (Now (((+) <$> (Index Second) <*> (Index Third)) |==| (Index First)))))) signalsUnderTest)




    --print (evaluate (Always (Implies (NowHasTicked ((Index Second) |==| (Pure True))) (NowHasTicked ((Index First) |==| (Pure True))))) signalsUnderTest)

    -- let bufferedSig = buffer 10 (first intSignals)
    --     signalsUnderTest = prepend bufferedSig $ flatten intSignals
    
    -- print signalsUnderTest
    -- print (evaluate (Next (Always (Now ((Index First) |==| (Index (Previous Second)))))) signalsUnderTest)

    --intSignals <- generate $ generateSignals @Int
    -- Interleave:
    -- let interleavedSig = interleave (box (+)) (getLater $ first intSignals) (getLater $ second intSignals)
    -- let notALaterSig = 1 ::: interleavedSig -- Fails test atm, because we just naively put 1 in front of a later signal. We might want to consider a way of handling either signals or later signals in our implementation
    -- let signalsUnderTest5 = prepend notALaterSig $ flatten intSignals
    -- print (evaluate (Always (Or (Or (Now ((Index First) |==| (Index Second))) (Now ((Index First) |==| (Index Third)))) (Now (((+) <$> (Index Second) <*> (Index Third)) |==| (Index First))))) signalsUnderTest5)

    -- -- Zip:
    -- let s1 = zip (first intSignals) (second intSignals)
    -- let signalsUnderTest2 = prepend s1 $ flatten intSignals
    -- print (evaluate (Always (Or (Now ((fst' <$> (Index First)) |==| (Index Second))) (Now ((snd' <$> (Index First)) |==| (Index Third))))) signalsUnderTest2)

    -- let prefixSum = scan (box (+)) 0 (first intSignals)
    --     signalsUnderTest = prepend prefixSum $ flatten intSignals
    -- print signalsUnderTest
    -- print(evaluate (Next (Always (Now (Index (Previous First) |<| Index First)))) signalsUnderTest)
--    print (evaluate (Always (Or (Now (\hls -> (fst' (current' $ first hls) :: Int) == (current' (second hls) :: Int))) (Now (\hls -> (snd' (current' $ first hls) :: Int) == (current' (third hls) :: Int))))) signalsUnderTest2)
