{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
module PropRatt.RatUtils where

import AsyncRattus.Signal (Sig(..))
import AsyncRattus.InternalPrimitives
import qualified Data.IntSet as IntSet
import Prelude hiding (take)

take :: Int -> Sig a -> Sig a
take 1 (x ::: Delay cl f) = x ::: never
take n (x ::: xs@(Delay cl f)) = if IntSet.null cl then x ::: never else x ::: delay (take (n-1) (adv xs))