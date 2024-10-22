module Name.Utilities (
    takeSig,
    takeSigExhaustive,
    sizeSig,
    ints
) where

import AsyncRattus.InternalPrimitives
import AsyncRattus.Signal
import qualified Data.IntSet as IntSet
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)

-- take n elements from sig
takeSig :: Int -> Sig a -> [a]
takeSig 0 _ = []
takeSig n (x ::: Delay _ f) = x : takeSig (n-1) (f (InputValue 0 ()))

-- take (force) all elements of sig
takeSigExhaustive :: Sig a -> [a]
takeSigExhaustive (x ::: Delay cl f) = 
    if IntSet.null cl then
        []
    else x : takeSigExhaustive (f (InputValue 0 ()))

-- size of signal
sizeSig :: Sig a -> Int -> Int
sizeSig (_ ::: Delay cl f) acc =
    if IntSet.null cl then
        acc
    else sizeSig (f (InputValue 0 ())) (acc+1)

-- infitnite stream of 0
ints :: Sig Int
ints = 0 ::: Delay (IntSet.singleton 0) (\_ -> ints)