{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

-- AsyncRattus code goes here. 
-- The code is type checked by the AsyncRattus compiler plugin.

module PropRatt.AsyncRat (
    aRatZip,
    aRatSwitch,
    aRatParallel,
    aRatParallelAndZip,
    mkCurrentSig,
    mkCurrentSigSingle
) where


import AsyncRattus.Signal
import AsyncRattus.Strict
import AsyncRattus.InternalPrimitives
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)
import PropRatt.Current

aRatZip :: Sig Int -> Sig Int -> Sig (Int :* Int)
aRatZip a b = zip a b

aRatSwitch :: Sig a -> O(Sig a) -> Sig a
aRatSwitch a o = switch a o

aRatParallel :: Sig a -> Sig b -> Sig (Maybe' a :* Maybe' b) -- Sig (Current a) 
aRatParallel a b = parallel a b

mkCurrentSig :: (Stable a, Stable b) => Sig a -> Sig b -> Sig (Current a :* Current b)
mkCurrentSig a b =
  zipWith combine (parallel a b) (zip a b)
  where
    combine = box (\(ma :* mb) (aVal :* bVal) -> Current ma aVal :* Current mb bVal)

mkCurrentSigSingle :: (Stable a) => Sig a -> Sig (Current a)
mkCurrentSigSingle xs = map (box (\a -> Current (Just' a) a)) xs


aRatParallelAndZip :: (Stable a, Stable b) => Sig a -> Sig b -> Sig ((Maybe' a :* Maybe' b) :* (a :* b))
aRatParallelAndZip a b = zip (parallel a b) (zip a b)