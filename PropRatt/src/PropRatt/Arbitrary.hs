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
    shrinkHList,
    Map,
    ToList
  )
where

import AsyncRattus.InternalPrimitives
import AsyncRattus.Plugin.Annotation
import AsyncRattus.Signal hiding (map)
import qualified Data.IntSet as IntSet
import PropRatt.Utils
import Data.Kind (Type)
import Test.QuickCheck
import Prelude hiding (take)
import PropRatt.HList

instance (Arbitrary a) => Arbitrary (Sig a) where
  arbitrary = do
    len <- choose (100, 1000)
    arbitrarySig len
  shrink :: Sig a -> [Sig a]
  shrink = shrinkSig

instance (Show a) => Show (Sig a) where
  show (x ::: xs) = show (toList (x ::: xs))

instance (Eq a) => Eq (Sig a) where
  (==) sig1 sig2 = toList sig1 == toList sig2

shrinkSig :: Sig a -> [Sig a]
shrinkSig sig = shrinkHelper sig []

{-# ANN shrinkHelper AllowRecursion #-}
shrinkHelper :: Sig a -> [Sig a] -> [Sig a]
shrinkHelper sig acc =
  let len = sigLength sig `div` 2
      newSig = take sig len
      acc' = (newSig : acc)
  in if len <= 1
     then acc'
     else shrinkHelper newSig acc'

genClockChannel :: Gen Int
genClockChannel = chooseInt (1, 3)

genClockList :: Gen [Int]
genClockList = do
  len <- chooseInt (1, 3)
  vectorOf len genClockChannel

{-# ANN arbitrarySig AllowRecursion #-}
arbitrarySig :: (Arbitrary a) => Int -> Gen (Sig a)
arbitrarySig n = do
  go n
  where
    go 0 = do
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

class Halving a where
  halve :: HList a -> HList a

instance Halving '[] where
  halve :: HList '[] -> HList '[]
  halve _ =  HNil

instance (Halving as) => Halving (Sig a ': as) where
  halve :: HList (Sig a : as) -> HList (Sig a : as)
  halve (HCons x xs) = shrinkSigOnce x %: halve xs

{-# ANN shrinkHList AllowRecursion #-}
shrinkHList :: (Halving ts) => HList (Sig t ': ts) -> [HList (Sig t ': ts)]
shrinkHList hls = go hls []
  where
    go :: (Halving ts) => HList (Sig t ': ts) -> [HList (Sig t ': ts)] -> [HList (Sig t ': ts)]
    go curr acc =
      case curr of
        HCons x HNil -> (halve curr) : acc
        HCons x _ ->
          if sigLength x <= 1 then (halve curr) : acc else
          let shrunk = halve curr
          in go shrunk (shrunk : acc)
    
shrinkSigOnce :: Sig a -> Sig a
shrinkSigOnce sig = take sig $ halfLength sig

halfLength :: Sig a -> Int
halfLength sig = sigLength sig `div` 2

take :: Sig a -> Int -> Sig a
take (x ::: _) 1 = x ::: never
take (x ::: xs@(Delay cl _)) n = 
  if IntSet.null cl 
    then x ::: never 
    else x ::: delay (take (adv xs) (n-1))