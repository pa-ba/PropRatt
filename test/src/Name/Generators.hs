{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Signals are orphaned because we want to export the implementation of the typeclasses.
-- Not ideal. Todo: possibly implement a Sig wrapper type.

module Name.Generators
  ( arbitrarySig,
    Sig (..),
  )
where

import AsyncRattus.InternalPrimitives
import AsyncRattus.Signal
import Data.Int (Int)
import qualified Data.IntSet as IntSet
import Name.Utilities
import Test.QuickCheck
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)

-- Use sized to avoid infinite recursion.

instance (Arbitrary a) => Arbitrary (Sig a) where
  arbitrary = arbitrarySig 3

instance (Show a) => Show (Sig a) where
  show (x ::: xs) = "Sig " ++ show (takeSigExhaustive (x ::: xs)) ++ " clock: " ++ show (extractClock xs)

instance (Eq a) => Eq (Sig a) where
  (==) sig1 sig2 = takeSigExhaustive sig1 == takeSigExhaustive sig2

genElem :: Gen Int
genElem = chooseInt (1, 3)

genList :: Gen [Int]
genList = do
  len <- chooseInt (1, 3)
  vectorOf len genElem

arbitrarySig :: (Arbitrary a) => Int -> Gen (Sig a)
arbitrarySig n = do
  cl <- genList
  go n cl
  where
    go 0 _ = do
      x <- arbitrary
      return (x ::: never)
    go m cl = do
      x <- arbitrary
      xs <- go (m - 1) cl
      let later = Delay (IntSet.fromList cl) (\_ -> xs)
      return (x ::: later)

-- Alternative solution. Doesn't work.
{- 
instance Arbitrary a ⇒ Arbitrary (Sig a) where
    arbitrary = do
        x <- arbitrary :: Gen a
        s <- getSize
        if s > 0 then
            return x ::: (do xs <- reSize(n-1) arbitrary)
            return $ x ::: Delay (Singleton 0) (\_ -> xs)
            else
            return x ::: (xs ::: never)
-}