{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use const" #-}
module Name.Utilities (
    takeSig,
    takeSigExhaustive,
    takeSigAndClockExhaustive,
    sizeSig,
    ints,
    pickSmallestClock,
    first
) where

import AsyncRattus.Strict
import AsyncRattus.InternalPrimitives
import AsyncRattus.Signal
import qualified Data.IntSet as IntSet hiding (map)
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)

first :: Sig (Int :* Int) -> Sig Int
first a = do
  let boxed = Box fst'
  map boxed a

pickSmallestClock :: IntSet.IntSet -> Int
pickSmallestClock = IntSet.findMin

-- take n elements from sig
takeSig :: Int -> Sig a -> [a]
takeSig 0 _ = []
takeSig n (x ::: Delay _ f) = x : takeSig (n-1) (f (InputValue 0 ()))

-- take (force) all elements of sig
takeSigExhaustive :: Sig a -> [a]
takeSigExhaustive (x ::: Delay cl f) =
    if IntSet.null cl then
        [x]
    else x : takeSigExhaustive (f (InputValue (pickSmallestClock cl) ()))

takeSigAndClockExhaustive :: Sig a -> [(a, IntSet.IntSet)]
takeSigAndClockExhaustive (x ::: Delay cl f) =
    if IntSet.null cl then
        [(x, cl)]
    else (x, cl) : takeSigAndClockExhaustive (f (InputValue (pickSmallestClock cl) ()))

-- size of signal
sizeSig :: Sig a -> Int -> Int
sizeSig (_ ::: Delay cl f) acc =
    if IntSet.null cl then
        acc
    else sizeSig (f (InputValue 0 ())) (acc+1)

-- infitnite stream of 0
ints :: Sig Int
ints = 0 ::: Delay (IntSet.singleton 0) (\_ -> ints)