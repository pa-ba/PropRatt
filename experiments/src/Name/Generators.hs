{-# OPTIONS_GHC -fno-warn-orphans #-}

-- define an orphan instance to expose typeclass implementations of sig to other modules
-- may refac to use wrapper type later

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

-- headache tracker
-- quickcheck requires the signal data type to derive (implement) show (aka .toString) and arbitrary
-- show is esentially an interface (typeclass) that defines functions the data type must implement (derive)
-- we must define some equality operator that compares first n elements of a signal using take function

-- we dont want the compiler plugin to type check our code, but our type checked async rattus code depends on non type checked code
-- how do we avoid a cyclic dependency between non-type checked and type checked code

-- we want to talk more about Arbitrary clock generation in general. How to do it? how do we know which clocks we advance on. etc.
-- we have an implementation where we have the singleton clock 0. Although when we advance on other clocks, we still get the signal. how come?

-- custom implementation of typeclass to define how to generate an arbitrary sig a
-- We supply arbitrarySig as the (fixed sized) generator Gen (Sig a). sized :: (Int -> Gen a )-> Gen A

instance (Arbitrary a) => Arbitrary (Sig a) where
  arbitrary = sized (arbitrarySig [1, 2]) -- Fix me. Should be a generated clockset for each signal instance

-- implement show typeclass for show Sig a
instance (Show a) => Show (Sig a) where
  show (x ::: xs) = "Sig " ++ show (takeSigExhaustive (x ::: xs)) ++ " zipped: "++ show (takeSigExhaustive (zip (x ::: xs) (0 ::: never))) ++" clocky: " ++ show (extractClock xs)


instance (Eq a) => Eq (Sig a) where
  (==) sig1 sig2 = takeSigExhaustive sig1 == takeSigExhaustive sig2

-- could also look into a probability based implementation that generates O (a) with lower probability

-- naive implementations reccurs infinitely
{- instance Arbitrary a ⇒ Arbitrary (Sig a) where
    arbitrary = do
        x <- arbitrary :: Gen a
        s <- getSize
        if s > 0 then
            return x ::: (do xs <- reSize(n-1) arbitrary)
            return $ x ::: Delay (Singleton 0) (\_ -> xs)

            else
            return x ::: (xs ::: never)
 -}

-- arbitrary class from quickcheck, defining a custom instance of the class to handle arbitrary generation of Sig a
-- we used "sized" from quickcheck to avoid infinite recursion

-- generates an int between 1 and 3
genElem :: Gen Int
genElem = chooseInt (1, 3)

-- generates a list of maxlength 3, from the genElem int generator
genList :: Gen [Int]
genList = do
  len <- chooseInt (1, 3)
  vectorOf len genElem

-- => type constraint, arguments must derive arbitrary typeclass
arbitrarySig :: (Arbitrary a) => [Int] -> Int -> Gen (Sig a)
-- base case
-- same as scala for comprehension, sugar to chain monadic calls in an imperative way
arbitrarySig clocks 0 = do
  x <- arbitrary
  return (x ::: never)
arbitrarySig clocks n = do
  x <- arbitrary
  xs <- arbitrarySig clocks (n - 1) -- not evaluated yet, forced in InputValue
  let later = Delay (IntSet.fromList clocks) (\_ -> xs)
  return (x ::: later)
