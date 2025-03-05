{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE TypeOperators, FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Main (main) where
import PropRatt.LTL
import AsyncRattus.Strict
import AsyncRattus.InternalPrimitives
import AsyncRattus.Signal hiding (mkSig)
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith, take)
import PropRatt.Value
import PropRatt.AsyncRat
import Test.QuickCheck (Gen, Arbitrary (arbitrary), generate)
import PropRatt.Utilities (getLater)
import PropRatt.Generators ()

-- make list of n+1 arbitrary signals

instance Stable Int where
instance Stable (Value Int) where
instance Stable Char where
instance Stable Bool where
instance Stable (Int :* Int) where

example :: IO (HList '[Sig Int, Sig Int])
example = do
  first <- generate (arbitrary :: Gen (Sig Int))
  second <- generate (arbitrary :: Gen (Sig Int))
  return (first %: second %: HNil)

main :: IO ()
main = do
    ex <- example
    --print $ show $ first ex
    --print $ show $ second ex

    --print $ show (flatten ex)
    let switched = aRatSwitch (first ex) (getLater (second ex))
    let twoSig = flatten ex
    let added = prepend switched twoSig
    print (evaluate (Until (Now (\ls -> first ls ?= second ls)) (Now (\ls -> first ls ?= third ls))) added)

    let s2 = first ex
    let s3 = second ex
    let s1 = aRatZip s2 s3
    let flattened = flatten ex
    let added2 = prepend s1 flattened
    --print added2

    print (evaluate (Always (Or (Now (\hls -> (fst' (current' $ first hls) :: Int) == (current' (second hls) :: Int))) (Now (\hls -> (snd' (current' $ first hls) :: Int) == (current' (third hls) :: Int))))) added2)
