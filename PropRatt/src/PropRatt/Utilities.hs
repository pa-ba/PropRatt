{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use const" #-}
module PropRatt.Utilities where

import AsyncRattus.Signal (Sig(..), map, zip, scan)
import AsyncRattus.Strict
import AsyncRattus.InternalPrimitives
import qualified Data.IntSet as IntSet
import Prelude hiding (map, zip, zipWith, take)

pickSmallestClock :: IntSet.IntSet -> Int
pickSmallestClock = IntSet.findMin

takeSig :: Int -> Sig a -> [a]
takeSig 0 _ = []
takeSig n (x ::: Delay _ f) = x : takeSig (n-1) (f (InputValue 0 ()))



toSig :: [(a, IntSet.IntSet)] -> Sig a
toSig [] = error "empty list"
toSig [(x, cl)]
    | IntSet.null cl = x ::: Delay cl (\_ -> error "forced evaluation of const sig")
    | otherwise = error "non empty clock for final element"
toSig ((x,cl) : xs) 
    | IntSet.null cl = error "empty clock"
    | otherwise = x ::: Delay cl (\_ -> toSig xs)

takeSigExhaustive :: Sig a -> [a]
takeSigExhaustive (x ::: Delay cl f)
    | IntSet.null cl = [x]
    | otherwise = x : takeSigExhaustive (f (InputValue (pickSmallestClock cl) ()))

takeSigAndClockExhaustive :: Sig a -> [(a, IntSet.IntSet)]
takeSigAndClockExhaustive (x ::: Delay cl f)
    | IntSet.null cl = [(x, cl)]
    | otherwise = (x, cl) : takeSigAndClockExhaustive (f (InputValue (pickSmallestClock cl) ()))

takeSigAndClock :: Int -> Sig a -> [(a, IntSet.IntSet)]
takeSigAndClock n (x ::: Delay cl f) = case n of
    0 -> [(x, cl)]
    _ -> (x, cl) : takeSigAndClock (n-1) (f (InputValue (pickSmallestClock cl) ()))

lengthSig :: Sig a -> Int -> Int
lengthSig (_ ::: Delay cl f) acc
    | IntSet.null cl = acc + 1
    | otherwise = lengthSig (f (InputValue 0 ())) (acc+1)

sigLength :: Sig a -> Int
sigLength sig = lengthSig sig 0

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

mkSigOne :: Sig Int
mkSigOne = 1 ::: Delay (IntSet.fromList [1]) (\_ -> mkSigOne)

stutter :: (Stable a, Stable b) => Sig a -> Sig b -> Sig a
stutter xs ys = map (box fst') (zip xs ys)

monotonic :: (Stable Int) => Sig Int -> Sig Int
monotonic xs = scan (box (+)) 0 (map (box abs) xs)