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
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# HLINT ignore "Use const" #-}

module PropRatt.Arbitrary
  ( arbitrarySig,
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
import Prelude hiding (const, drop, take)
import PropRatt.HList

instance (Arbitrary a) => Arbitrary (Sig a) where
  arbitrary = arbitrarySig 100
  shrink :: Sig a -> [Sig a]
  shrink = shrinkSignal shrink

instance (Show a) => Show (Sig a) where
  show (x ::: xs) = show (toList (x ::: xs))

instance (Eq a) => Eq (Sig a) where
  (==) sig1 sig2 = toList sig1 == toList sig2


shrinkSignal :: (a -> [a]) -> Sig a -> [Sig a]
shrinkSignal shr sig@(y ::: ys@(Delay cly fy))
  | IntSet.null cly = shrinkOne sig shr
  | otherwise = concat [ removes k n sig | k <- takeWhile (>0) (iterate (`div`2) n) ]
    ++ shrinkOne sig shr
  where
    n = sigLength sig


{-# ANN shrinkOne AllowRecursion #-}
shrinkOne :: Sig a -> (a -> [a]) -> [Sig a]
shrinkOne (x ::: xs@(Delay cl f)) shr
  | IntSet.null cl = [ x' ::: xs | x'  <- shr x ]
  | otherwise = [ x' ::: xs | x'  <- shr x ]
                ++ [ x ::: Delay cl (\_ -> xs') | xs' <- (shrinkOne (f (InputValue (IntSet.findMin cl) ())) shr) ]

{-# ANN removes AllowRecursion #-}
removes :: Int -> Int -> Sig a -> [Sig a]
removes k n sig =
  if k >= n
    then []
    else let xs1 = take k sig
             xs2 = drop k sig
         in xs1 : xs2 : map (append xs1) (removes k (n-k) xs2)

{-# ANN take AllowRecursion #-}
take :: Int -> Sig a -> Sig a
take 1 (x ::: _)    = x ::: never
take k (x ::: xs)   = x ::: delay (take (k-1) (adv xs))

{-# ANN drop AllowRecursion #-}
drop :: Int -> Sig a -> Sig a
drop n sig@(x ::: (Delay cl f))
  | n <= 0    = sig
  | otherwise = drop (n-1) (f (InputValue (IntSet.findMin cl) ()))

{-# ANN append AllowRecursion #-}
append :: Sig a -> Sig a -> Sig a
append (x ::: xs@(Delay cl fx)) y
  | IntSet.null cl = x ::: Delay (IntSet.fromList [1,2,3]) (\_ -> (y))
  | otherwise      = x ::: Delay cl (\_ -> append (fx (InputValue (IntSet.findMin cl) ())) y)


genClockChannel :: Gen Int
genClockChannel = chooseInt (1, 3)

genClockList :: Gen [Int]
genClockList = do
  len <- chooseInt (1, 3)
  vectorOf len genClockChannel

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
  shrinkHls _ = [HNil]

instance (Arbitrary a, ShrinkHList as) => ShrinkHList (a ': as) where
  shrinkHls (HCons x xs) = [ HCons x' xs' | x' <- shrink x, xs' <- shrinkHls xs ]