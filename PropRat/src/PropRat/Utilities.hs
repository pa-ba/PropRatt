{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use const" #-}
module PropRat.Utilities (
    takeSig,
    takeSigExhaustive,
    takeSigAndClockExhaustive,
    sizeSig,
    ints,
    pickSmallestClock,
    first,
    isStuttering,
    stuttering,
    isTailEqualToLaterSignal,
    getLater,
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

-- infinite stream of 0
ints :: Sig Int
ints = 0 ::: Delay (IntSet.singleton 0) (\_ -> ints)


-- Zip 
isStuttering :: Sig Int -> Sig Int -> Bool
isStuttering (x ::: Delay clx fx) (y ::: Delay cly fy)
  | x /= y = False
  | IntSet.null union = True
  | IntSet.member smallest clx && IntSet.member smallest cly =
      isStuttering (fx (InputValue smallest ())) (fy (InputValue smallest ()))
  | IntSet.member smallest clx =
      isStuttering (fx (InputValue smallest ())) (y ::: Delay cly fy)
  | otherwise =
      isStuttering (x ::: Delay clx fx) (fy (InputValue smallest ()))
  where
    union = IntSet.union clx cly
    smallest = IntSet.findMin union

stuttering :: Sig Int -> Sig Int -> [Int]
stuttering (x ::: Delay clx fx) (y ::: Delay cly fy)
  | x /= y = []
  | IntSet.null union = []
  | IntSet.member smallest clx && IntSet.member smallest cly =
      x : stuttering (fx (InputValue smallest ())) (fy (InputValue smallest ()))
  | IntSet.member smallest clx =
      x : stuttering (fx (InputValue smallest ())) (y ::: Delay cly fy)
  | otherwise =
      y : stuttering (x ::: Delay clx fx) (fy (InputValue smallest ()))
  where
    union = IntSet.union clx cly
    smallest = IntSet.findMin union



-- Switch
sigAEqualsSigB :: Sig Int -> Sig Int -> Bool
sigAEqualsSigB (x ::: Delay clx fx) (y ::: Delay cly fy)
  | x /= y = False
  | IntSet.null union = True
  | otherwise =
      sigAEqualsSigB (fx (InputValue smallest ())) (fy (InputValue smallest ()))
  where
    union = IntSet.union clx cly
    smallest = IntSet.findMin union

isTailEqualToLaterSignal :: Sig Int -> O (Sig Int) -> Bool
isTailEqualToLaterSignal (_ ::: Delay clx fx) (Delay cly fy)
  | IntSet.null union = False
  | IntSet.member smallest cly =
      sigAEqualsSigB (fx (InputValue smallest ())) (fy (InputValue smallest ()))
  | IntSet.member smallest clx =
      isTailEqualToLaterSignal (fx (InputValue smallest ())) (Delay cly fy)
  | otherwise = False
  where
    union = IntSet.union clx cly
    smallest = IntSet.findMin union

getLater :: Sig Int -> O (Sig Int)
getLater (_ ::: xs) = xs