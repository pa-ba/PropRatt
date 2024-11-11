module Name.Utilities (
    takeSig,
    takeSigExhaustive,
    sizeSig,
    ints
) where

import AsyncRattus.InternalPrimitives
import AsyncRattus.Signal
import AsyncRattus.Strict
import qualified Data.IntSet as IntSet
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)



-- helper :: IntSet.IntSet -> a -> Sig a -> InputValue -> Sig a
-- helper clocks x xs (InputValue clock value) =
--   if clock `IntSet.member` clocks
--     then xs
--     else x ::: never -- x ::: Delay clocks (\_ -> xs) -- does this work???


{--
    - Vi vil gerne lave vores version af zip, for at kunne teste stuttering. 
    - lav function der skifter mellem at bruge clock fra det ene signal og det andet signal når vi Zipper
    - Derefter skal vi extracte tuple værdierne for det ene signal og checke at det er en stuttering af det ene signal. 
    - Dermed skal vi lave en "isSigAStutterofSigB" function. 
--}


-- recursive function for checking if a signal is a stuttering of other signal
-- isAStutterofB :: [a] -> [b] -> Bool
-- isAStutterofB (x::xs) (y::ys) = do
--     if(y == x) isAStutterofB((x::xs), ys)


-- helper :: IntSet.IntSet -> a -> Sig a -> InputValue -> Sig a
-- helper clocks x xs (InputValue clock value) =
--   if clock `IntSet.member` clocks
--     then xs
--     else x ::: never -- x ::: Delay clocks (\_ -> xs) -- does this work???


pickSmallestClock :: IntSet.IntSet -> Int
pickSmallestClock = IntSet.findMin

-- take n elements from sig
takeSig :: Int -> Sig a -> [a]
takeSig 0 _ = []
takeSig n (x ::: Delay _ f) = x : takeSig (n-1) (f (InputValue 0 ()))

-- take (force) all elements of sig
takeSigExhaustive :: Sig a -> [a]
takeSigExhaustive (x ::: Delay cl f) =
    if IntSet.null cl then
        []
    else x : takeSigExhaustive (f (InputValue (pickSmallestClock cl) ()))


-- size of signal
sizeSig :: Sig a -> Int -> Int
sizeSig (_ ::: Delay cl f) acc =
    if IntSet.null cl then
        acc
    else sizeSig (f (InputValue 0 ())) (acc+1)

-- infitnite stream of 0
ints :: Sig Int
ints = 0 ::: Delay (IntSet.singleton 0) (\_ -> ints)