{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE GADTs #-}

module PropRatt.Utils (smallest,toList,toListWithClock,toListOfLength,lengthSig,sigLength,mkSigOne,mkSigZero) where

import AsyncRattus.Signal (Sig(..))
import AsyncRattus.InternalPrimitives
import qualified Data.IntSet as IntSet
import Prelude hiding (map, zip, zipWith, take)


smallest :: IntSet.IntSet -> Int
smallest = IntSet.findMin


toList :: Sig a -> [a]
toList (x ::: Delay cl f)
    | IntSet.null cl = [x]
    | otherwise = x : toList (f (InputValue (smallest cl) ()))


toListWithClock :: Sig a -> [(a, IntSet.IntSet)]
toListWithClock (x ::: Delay cl f)
    | IntSet.null cl = [(x, cl)]
    | otherwise = (x, cl) : toListWithClock (f (InputValue (smallest cl) ()))


toListOfLength :: Int -> Sig a -> [a]
toListOfLength 0 _ = []
toListOfLength n (x ::: Delay cl f) = x : toListOfLength (n-1) (f (InputValue (smallest cl) ()))


lengthSig :: Sig a -> Int -> Int
lengthSig (_ ::: Delay cl f) acc
    | IntSet.null cl = acc + 1
    | otherwise = lengthSig (f (InputValue (smallest cl) ())) (acc+1)

sigLength :: Sig a -> Int
sigLength sig = lengthSig sig 0



mkSigOne :: Sig Int
mkSigOne = 1 ::: Delay (IntSet.fromList [1]) (\_ -> mkSigOne)



mkSigZero :: Sig Int
mkSigZero = 0 ::: Delay (IntSet.fromList [2]) (\_ -> mkSigZero)

