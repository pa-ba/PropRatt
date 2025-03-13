{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use const" #-}
module PropRatt.Utilities (
    takeSig,
    takeSigExhaustive,
    takeSigAndClockExhaustive,
    sizeSig,
    pickSmallestClock,
    isStuttering,
    stuttering,
    isEventuallyEqual,
    getLater,
    sigAEqualsSigB,
) where

import AsyncRattus.Strict
import AsyncRattus.Signal (Sig(..), map)
import AsyncRattus.InternalPrimitives( O(..), Box(Box), InputValue(InputValue) )
import qualified Data.IntSet as IntSet
import Prelude hiding (map, zip, zipWith)

pickSmallestClock :: IntSet.IntSet -> Int
pickSmallestClock = IntSet.findMin

takeSig :: Int -> Sig a -> [a]
takeSig 0 _ = []
takeSig n (x ::: Delay _ f) = x : takeSig (n-1) (f (InputValue 0 ()))

takeSigExhaustive :: Sig a -> [a]
takeSigExhaustive (x ::: Delay cl f)
    | IntSet.null cl = [x]
    | otherwise = x : takeSigExhaustive (f (InputValue (pickSmallestClock cl) ()))

takeSigAndClockExhaustive :: Sig a -> [(a, IntSet.IntSet)]
takeSigAndClockExhaustive (x ::: Delay cl f)
    | IntSet.null cl = [(x, cl)]
    | otherwise = (x, cl) : takeSigAndClockExhaustive (f (InputValue (pickSmallestClock cl) ()))

sizeSig :: Sig a -> Int -> Int
sizeSig (_ ::: Delay cl f) acc
    | IntSet.null cl = acc
    | otherwise = sizeSig (f (InputValue 0 ())) (acc+1)

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

sigAEqualsSigB :: Sig Int -> Sig Int -> Bool
sigAEqualsSigB (x ::: Delay clx fx) (y ::: Delay cly fy)
    | x /= y = False
    | IntSet.null union = True
    | otherwise =
        sigAEqualsSigB (fx (InputValue smallest ())) (fy (InputValue smallest ()))
  where
    union = IntSet.union clx cly
    smallest = IntSet.findMin union

isEventuallyEqual :: Sig Int -> O (Sig Int) -> Bool
isEventuallyEqual (_ ::: Delay clx fx) (Delay cly fy)
    | IntSet.null union = False
    | IntSet.member smallest cly =
        sigAEqualsSigB (fx (InputValue smallest ())) (fy (InputValue smallest ()))
    | IntSet.member smallest clx =
        isEventuallyEqual (fx (InputValue smallest ())) (Delay cly fy)
    | otherwise = False
  where
    union = IntSet.union clx cly
    smallest = IntSet.findMin union

getLater :: Sig a -> O (Sig a)
getLater (_ ::: xs) = xs