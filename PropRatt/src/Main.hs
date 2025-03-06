{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE TypeOperators, FlexibleInstances #-}
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

main :: IO ()
main = do
    let ex = example
    print $ unWrap ex
    intSignals <- generate $ generateSigs @[Int, Int]
    let switched = aRatSwitch (first intSignals) (getLater (second intSignals))
    let signalsUnderTest = prepend switched $ flatten intSignals
    print (evaluate (Until (Now (\ls -> first ls ?= second ls)) (Now (\ls -> first ls ?= third ls))) signalsUnderTest)

    let s1 = aRatZip (first intSignals) (second intSignals)
    let signalsUnderTest2 = prepend s1 $ flatten intSignals

    print (evaluate (Always (Or (Now (\hls -> (fst' (current' $ first hls) :: Int) == (current' (second hls) :: Int))) (Now (\hls -> (snd' (current' $ first hls) :: Int) == (current' (third hls) :: Int))))) signalsUnderTest2)
