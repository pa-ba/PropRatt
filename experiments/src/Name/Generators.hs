{-# OPTIONS_GHC -fno-warn-orphans #-}
-- define an orphan instance to expose typeclass implementations of sig to other modules
-- may refac to use wrapper type later

module Name.Generators (
    arbitrarySig,
    Sig(..)
) where

import AsyncRattus.InternalPrimitives
import Test.QuickCheck
import Name.Utilities
import AsyncRattus.Signal
import qualified Data.IntSet as IntSet
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)

-- headache tracker
-- quickcheck requires the signal data type to derive (implement) show (aka .toString) and arbitrary
-- show is esentially an interface (typeclass) that defines functions the data type must implement (derive)
-- we must define some equality operator that compares first n elements of a signal using take function

-- we dont want the compiler plugin to type check our code, but our type checked async rattus code depends on non type checked code
-- how do we avoid a cyclic dependency between non-type checked and type checked code

-- custom implementation of typeclass to define how to generate an arbitrary sig a
-- We supply arbitrarySig as the (fixed sized) generator Gen (Sig a). sized :: (Int -> Gen a )-> Gen A

instance (Arbitrary a) => Arbitrary (Sig a) where
    arbitrary = sized arbitrarySig

-- implement show typeclass for show Sig a
instance Show a => Show (Sig a) where
    show (x ::: xs) = "Sig " ++ show (takeSigExhaustive (x ::: xs)) ++ " clocky: " ++ show (extractClock xs)

-- 30 should probably not be hard coded but it is for now
instance Eq a => Eq (Sig a) where
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

-- => type constraint, arguments must derive arbitrary typeclass
arbitrarySig :: Arbitrary a => Int -> Gen (Sig a)
-- base case
-- same as scala for comprehension, sugar to chain monadic calls in an imperative way
arbitrarySig 0 = do
    x <- arbitrary
    return (x ::: never)
arbitrarySig n = do
    x <- arbitrary
    xs <- arbitrarySig (n - 1) -- not evaluated yet, forced in InputValue
    list <- arbitrary :: Gen [Int]
    let later = Delay (IntSet.fromList list) (\_ -> xs)
    return (x ::: later)