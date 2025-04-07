{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}

module PropRatt.Generators
  ( arbitrarySig,
    Sig (..),
  )
where

import AsyncRattus.InternalPrimitives
import AsyncRattus.Signal
import qualified Data.IntSet as IntSet
import PropRatt.Utilities
import Test.QuickCheck
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)
import GHC.Generics
    ( Generic(..), Rec1(Rec1), K1(K1), type (:*:)(..), Rec0, R )


instance (Arbitrary a) => Arbitrary (Sig a) where
  arbitrary = do
    len <- choose (100, 200)
    arbitrarySig len
  shrink :: Arbitrary a => Sig a -> [Sig a]
  shrink s@(x ::: xs) = genericShrink s
instance (Arbitrary a) => Arbitrary (O (Sig a)) where
  arbitrary = do
    len <- choose (100, 200)
    arbitrarySigLater len
  -- shrink = genericShrink

instance Generic (Sig a) where
  type Rep (Sig a) = Rec0 a :*: Rec0 (O (Sig a))
  from :: Sig a -> Rep (Sig a) x
  from (x ::: xs) = K1 x :*: K1 xs
  to :: Rep (Sig a) x -> Sig a
  to (K1 x :*: K1 xs) = x ::: xs

newtype Natural = Natural Int

instance Arbitrary Natural where
  arbitrary = Natural <$> chooseInt (0, maxBound)

instance (Show a) => Show (Sig a) where
  show (x ::: xs) = show (takeSigAndClockExhaustive (x ::: xs))

instance (Eq a) => Eq (Sig a) where
  (==) sig1 sig2 = takeSigExhaustive sig1 == takeSigExhaustive sig2

genClockChannel :: Gen Int
genClockChannel = chooseInt (1, 3)

genClockList :: Gen [Int]
genClockList = do
  len <- chooseInt (1, 3)
  vectorOf len genClockChannel

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

arbitrarySigLater :: Arbitrary a => Int -> Gen (O (Sig a))
arbitrarySigLater n = do
  cl <- genClockList
  sig <- go n
  return $ Delay (IntSet.fromList cl) (\_ -> sig)
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