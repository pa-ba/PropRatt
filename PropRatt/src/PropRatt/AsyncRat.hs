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
    aRatParallelAndZip
) where


import AsyncRattus.Signal
import AsyncRattus.Strict
import AsyncRattus.InternalPrimitives
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)

aRatZip :: Sig Int -> Sig Int -> Sig (Int :* Int)
aRatZip a b = zip a b

aRatSwitch :: Sig a -> O(Sig a) -> Sig a
aRatSwitch a o = switch a o

aRatParallel :: Sig a -> Sig b -> Sig (Maybe' a :* Maybe' b)
aRatParallel a b = parallel a b

aRatParallelAndZip :: (Stable a, Stable b) => Sig a -> Sig b -> Sig ((Maybe' a :* Maybe' b) :* (a :* b))
aRatParallelAndZip a b = zip (parallel a b) (zip a b)