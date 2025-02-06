{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use const" #-}

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

instance (Arbitrary a) => Arbitrary (Sig a) where
  arbitrary = do
    len <- choose (10, 20) -- Introduced a lower limit of Signals of length 1, because when deconstructing and finding a later signal, from an original signal of length 0, an error is thrown
    arbitrarySig len

instance (Show a) => Show (Sig a) where
  show (x ::: xs) = "Sig: " ++ show (takeSigAndClockExhaustive (x ::: xs))

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