{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# HLINT ignore "Use const" #-}

module PropRatt.Arbitrary
  ( arbitrarySig,
    arbitrarySigWith,
    arbitrarySigWeighted,
    Sig (..),
    generateSignals,
    Map,
    ToList,
    take,
    shrinkSignal,
    shrinkOne,
    removes,
    drop,
    shrinkHls
  )
where

import AsyncRattus.InternalPrimitives
import AsyncRattus.Plugin.Annotation
import AsyncRattus.Signal hiding (map)
import qualified Data.IntSet as IntSet
import PropRatt.Utils
import Data.Kind (Type)
import Test.QuickCheck
import Prelude hiding (const)
import PropRatt.HList

type TSig a = [(a, IntSet.IntSet)]

instance (Arbitrary a) => Arbitrary (Sig a) where
  arbitrary = arbitrarySig 100
  shrink :: Sig a -> [Sig a]
  shrink sig = toSignal (shrinkSignal shrink sig) 

instance (Show a) => Show (Sig a) where
  show (x ::: xs) = show (toList (x ::: xs))

instance (Eq a) => Eq (Sig a) where
  (==) sig1 sig2 = toList sig1 == toList sig2

shrinkSignal :: (a -> [a]) -> Sig a -> [TSig a]
shrinkSignal shr sig@(_ ::: (Delay cly _)) =
  if IntSet.null cly
    then shrinkOne tupleList shr
    else concat [ removes k n tupleList | k <- takeWhile (>0) (iterate (`div`2) n) ]
    ++ shrinkOne tupleList shr
  where
    n = sigLength sig
    tupleList = sigToTupleList sig


{-# ANN shrinkOne AllowRecursion #-}
shrinkOne :: TSig a -> (a -> [a]) -> [TSig a]
shrinkOne ((x, cl) : []) shr = [ (x', cl) : [] | x'  <- shr x ]
shrinkOne ((x, cl) : xs) shr = [ (x', cl) : xs | x'  <- shr x ] ++ [ (x, cl) : xs' | xs' <- (shrinkOne xs shr) ]

{-# ANN removes AllowRecursion #-}
removes :: Int -> Int -> TSig a -> [TSig a]
removes k n tupleLs =
  if k >= n
    then []
    else let xs1 = take k tupleLs
             xs2 = drop k tupleLs
         in xs1 : xs2 : map (xs1 ++) (removes k (n-k) xs2)

{-# ANN toSignal AllowRecursion #-}
toSignal :: [TSig a] -> [Sig a]
toSignal [] = []
toSignal (x : []) = [tupleListToSig x]
toSignal (x : xs) = tupleListToSig x : toSignal xs

{-# ANN tupleListToSig AllowRecursion #-}
tupleListToSig :: TSig a -> Sig a
tupleListToSig ((x, cl) : []) = x ::: never
tupleListToSig ((x, cl) : xs) = 
  if IntSet.null cl 
    then x ::: never 
    else x ::: Delay cl (\_ -> tupleListToSig xs)

{-# ANN sigToTupleList AllowRecursion #-}
sigToTupleList :: Sig a -> TSig a
sigToTupleList (x ::: (Delay cl f)) =
  if IntSet.null cl 
    then [(x, IntSet.empty)] 
    else (x, cl) : sigToTupleList (f (InputValue (IntSet.findMin cl) ())) 

genClockChannel :: Gen Int
genClockChannel = chooseInt (9, 11)

genClockChannelWeighted :: Gen Int
genClockChannelWeighted = frequency [(1, pure 9), (1, pure 10), (50, pure 11)]

genClockList :: Gen [Int]
genClockList = do
  len <- chooseInt (1, 3)
  vectorOf len genClockChannel

genClockListWeighted :: Gen [Int]
genClockListWeighted = vectorOf 1 genClockChannelWeighted

{-# ANN arbitrarySig AllowRecursion #-}
arbitrarySig :: (Arbitrary a) => Int -> Gen (Sig a)
arbitrarySig n = do
  if n <= 0
    then error "Cannot create empty signals"
    else
      go n
      where
        go 1 = do
          x <- arbitrary
          return (x ::: never)
        go m = do
          x <- arbitrary
          cl <- genClockList
          xs <- go (m - 1)
          let later = Delay (IntSet.fromList cl) (\_ -> xs)
          return (x ::: later)

{-# ANN arbitrarySigWith AllowRecursion #-}
arbitrarySigWith :: (Arbitrary a) => Int -> Gen a -> Gen (Sig a)
arbitrarySigWith n gen = do
  if n <= 0
    then error "Cannot create empty signals"
    else
      go n
      where
        go 1 = do
          x <- gen
          return (x ::: never)
        go m = do
          x <- gen
          cl <- genClockList
          xs <- go (m - 1)
          let later = Delay (IntSet.fromList cl) (\_ -> xs)
          return (x ::: later)

{-# ANN arbitrarySigWeighted AllowRecursion #-}
arbitrarySigWeighted :: (Arbitrary a) => Int -> Gen (Sig a)
arbitrarySigWeighted n = do
  if n <= 0
    then error "Cannot create empty signals"
    else
      go n
      where
        go 1 = do
          x <- arbitrary
          return (x ::: never)
        go m = do
          x <- arbitrary
          cl <- genClockListWeighted
          xs <- go (m - 1)
          let later = Delay (IntSet.fromList cl) (\_ -> xs)
          return (x ::: later)

type family Map (f :: Type -> Type) (xs :: [Type]) :: [Type] where
  Map f '[] = '[]
  Map f (x ': xs) = f x ': Map f xs

-- Use polykinds to allow us to overload generateSignals to work for both Type and Type -> Type
type family ToList (a :: k) :: [Type] where
  ToList (a :: [Type]) = a
  ToList (a :: Type)   = '[a]

class HListGen (ts :: [Type]) where
  generateHList     :: Gen (HList (Map Sig ts))

instance HListGen '[] where
  generateHList = return HNil

instance (Arbitrary (Sig t), HListGen ts) => HListGen (t ': ts) where
  generateHList = do
    x <- arbitrary
    xs <- generateHList @ts
    return (x %: xs)

generateSignals :: forall a. HListGen (ToList a) => Gen (HList (Map Sig (ToList a)))
generateSignals = generateHList @(ToList a)

class ShrinkHList as where
  shrinkHls :: HList as -> [HList as]

instance ShrinkHList '[] where
  shrinkHls _ = []

instance (Arbitrary a, ShrinkHList as) => ShrinkHList (a ': as) where
  shrinkHls (HCons x xs) = 
    [ HCons x' xs | x'  <- shrink x ] ++ 
    [ HCons x xs' | xs' <- shrinkHls xs ] ++
    [ HCons x' xs' | x'  <- shrink x, xs' <- shrinkHls xs ]