{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE GADTs #-}

module PropRatt.Utils where

import AsyncRattus.Plugin.Annotation
import AsyncRattus.Signal (Sig(..))
import AsyncRattus.InternalPrimitives
import qualified Data.IntSet as IntSet
import Prelude hiding (map, zip, zipWith, take)
import PropRatt.HList

smallest :: IntSet.IntSet -> Int
smallest = IntSet.findMin

{-# ANN toList AllowRecursion #-}
toList :: Sig a -> [a]
toList (x ::: Delay cl f)
    | IntSet.null cl = [x]
    | otherwise = x : toList (f (InputValue (smallest cl) ()))

{-# ANN toListWithClock AllowRecursion #-}
toListWithClock :: Sig a -> [(a, IntSet.IntSet)]
toListWithClock (x ::: Delay cl f)
    | IntSet.null cl = [(x, cl)]
    | otherwise = (x, cl) : toListWithClock (f (InputValue (smallest cl) ()))

{-# ANN takeSig AllowRecursion #-}
takeSig :: Int -> Sig a -> [a]
takeSig 0 _ = []
takeSig n (x ::: Delay cl f) = x : takeSig (n-1) (f (InputValue (smallest cl) ()))

{-# ANN lengthSig AllowRecursion #-}
lengthSig :: Sig a -> Int -> Int
lengthSig (_ ::: Delay cl f) acc
    | IntSet.null cl = acc + 1
    | otherwise = lengthSig (f (InputValue (smallest cl) ())) (acc+1)

sigLength :: Sig a -> Int
sigLength sig = lengthSig sig 0

{-# ANN lengthH AllowRecursion #-}
lengthH :: HList ts -> Int -> Int
lengthH HNil n = n
lengthH (HCons _ as) n = lengthH as (n+1)

hlistLen :: Sig (HList ts) -> Int
hlistLen (m ::: _) = lengthH m 0

getLater :: Sig a -> O (Sig a)
getLater (_ ::: xs) = xs

{-# ANN mkSigOne AllowRecursion #-}
mkSigOne :: Sig Int
mkSigOne = 1 ::: Delay (IntSet.fromList [1]) (\_ -> mkSigOne)