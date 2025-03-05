{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE TypeOperators, FlexibleInstances #-}
module Main (main) where
import PropRatt.LTL
import AsyncRattus.Strict
import AsyncRattus.InternalPrimitives
import AsyncRattus.Signal hiding (mkSig)
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith, take)
import PropRatt.Value
import PropRatt.AsyncRat
-- import PropRatt.AsyncRat (flattenToSignal, flattenToSignal', aRatSwitch, prepend)
import PropRatt.Utilities (getLater)

-- make list of n+1 arbitrary signals

instance Stable Int where
instance Stable (Value Int) where
instance Stable Char where
instance Stable Bool where
instance Stable (Int :* Int) where

main :: IO ()
main = do
    ex <- example
    --print $ show $ first ex
    --print $ show $ second ex

    --print $ show (flatten ex)
    let switched = aRatSwitch (first ex) (getLater (second ex))
    let twoSig = flatten ex
    let added = prepend switched twoSig
    print (evaluateLTLSigs (Until (Now (\ls -> first ls ?= second ls)) (Now (\ls -> first ls ?= third ls))) added)


    let s2 = first ex
    let s3 = second ex
    let s1 = aRatZip s2 s3
    let flattened = flatten ex
    let added2 = prepend s1 flattened
    --print added2

    print (evaluateLTLSigs (Always (Or (Now (\hls -> (fst' (current' $ first hls) :: Int) == (current' (second hls) :: Int))) (Now (\hls -> (snd' (current' $ first hls) :: Int) == (current' (third hls) :: Int))))) added2)
