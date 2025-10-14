{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}

module PropRatt.Signal (hlistLen,takeN) where

import AsyncRattus.Signal
import AsyncRattus
import PropRatt.HList

hlistLen :: Sig (HList ts) -> Int
hlistLen (m ::: _) = lengthH m 0

{-# ANN takeN AllowRecursion #-}
takeN :: Int -> Sig a -> Sig a
takeN 1 (x ::: _) = x ::: never
takeN n (x ::: later) = x ::: delay (takeN (n-1) (adv later))