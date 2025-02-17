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
) where


import AsyncRattus.Signal hiding (mkSig)
import AsyncRattus.Strict
import AsyncRattus.InternalPrimitives
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)
import PropRatt.Value (Value(..))

aRatZip :: Sig Int -> Sig Int -> Sig (Int :* Int)
aRatZip a b = zip a b

aRatSwitch :: Sig a -> O(Sig a) -> Sig a
aRatSwitch a o = switch a o

aRatParallel :: Sig a -> Sig b -> Sig (Maybe' a :* Maybe' b) -- Sig (Current a) 
aRatParallel a b = parallel a b

-- Create a signal of singleton values
singleton' :: Sig a -> Sig [Value a]
singleton' xs = map (box (\x -> [Current (Just' x) x])) xs

makeSig :: [Sig [Value a]] -> Sig [Value a]
makeSig ls = foldr1 prependSig ls

makeSignals :: [Sig a] -> [Sig [Value a]]
makeSignals [] = []
makeSignals [s] = singleton' s
makeSignals [s:ss] = singleton' s : makeSignals ss

helper :: Sig a -> Sig [Value a] -> Sig [Value a]
helper a b = prependSig (singleton' a) b

latest :: Value a
latest (Current _ a) = a

prependSig :: Sig a -> Sig [Value a] -> Sig [Value a]
prependSig siga@(a ::: as) sigl@(l ::: ls) = 
  case parallel siga sigl of -- (Maybe' [Value a] :* Maybe' [Value a])
    (Just' x :* Nothing') ::: pss -> (Current (Just' x) x : l) ::: prependSigAwait as ls
    (Nothing' :* Just' y) ::: pss -> (Current (Nothing') a : y) ::: prependSigAwait as ls
    (Just' x :* Just' y) ::: pss -> (Current (Just' x) x : y) ::: prependSigAwait as ls


prependSigAwait :: O (Sig a) -> O (Sig [Value a]) -> O (Sig [Value a])
prependSigAwait siga@(a ::: as) sigl@(l ::: ls) = 
  case parallelAwait siga sigl of
    (Just' x :* Nothing') ::: pss -> (Current (Just' x) x : l) ::: prependSigAwait as ls
    (Nothing':* Just' y) ::: pss -> (Current Nothing' a : y) ::: prependSigAwait as ls
    (Just' x :* Just' y) ::: pss -> (Current (Just' x) x : y) ::: prependSigAwait as ls







-- -- -- Flatten a list of signals to a signal of values
-- combineSigs :: [Sig a] -> Sig [Value a]
-- combineSigs [] = const []
-- combineSigs [s] = singleton' s   
-- combineSigs (s:ss) = map (box appendValue) (parallel s (combineSigs ss))

-- Add a signal to the mother list
-- prependSig :: (Stable a) => Sig a -> Sig [Value a] -> Sig [Value a]
-- prependSig siga@(a ::: as) sigl@(l ::: ls) = 
--   let single = singleton' siga
--   in
--   case parallel single sigl of
--     (Just' x :* Just' y) ::: pss -> Current (Just' x) x : y ::: pss
--     (Just' x :* Nothing') ::: pss -> Current (Just' x) x : l ::: pss
--     (Nothing':* Just' y) ::: pss -> Current Nothing' a : y ::: pss
--     (Nothing' :* Nothing') ::: pss -> Current Nothing' a : l ::: pss
        