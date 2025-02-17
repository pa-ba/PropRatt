{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWithd)
import PropRatt.Value (Value(..), makeNothings)

aRatZip :: Sig Int -> Sig Int -> Sig (Int :* Int)
aRatZip a b = zip a b

aRatSwitch :: Sig a -> O(Sig a) -> Sig a
aRatSwitch a o = switch a o

aRatParallel :: Sig a -> Sig b -> Sig (Maybe' a :* Maybe' b) -- Sig (Current a) 
aRatParallel a b = parallel a b

-- Create a signal of singleton values
singleton' :: Sig a -> Sig (List (Value a))
singleton' xs = map (box (\x -> singleton (Current (Just' x) x))) xs

-- makeSignals :: [Sig a] -> Sig [Value a]
-- makeSignals [s] = singleton' s
-- makeSignals (s:ss) = (singleton' s) : (makeSignals ss)

-- helper :: Sig a -> Sig [Value a] -> Sig [Value a]
-- helper a b = prependSig a b


valueCreator :: (Stable a, Stable (Value a), Stable (List a), Stable (List (Value a))) => Sig a -> Sig (List (Value a)) -> Sig (List (Value a))
valueCreator (x ::: xs) (y ::: ys) = 
   ((Current (Just' x) x) :! y) ::: valueCreatorAwait (box makeNothings) x xs y ys


valueCreatorAwait :: (Stable a, Stable (Value a), Stable (List a), Stable (List (Value a))) => Box (List (Value a) -> List (Value a)) -> a -> O (Sig a) -> List (Value a) -> O (Sig (List (Value a))) -> O (Sig (List (Value a)))
valueCreatorAwait fun x xs  y ys  = delay (
  case select xs ys of
     Fst (x' ::: xs')   ys'         -> ((Current (Just' x') x') :! ((unbox fun) y)) ::: valueCreatorAwait fun x' xs' y ys'
     Snd xs' (y' ::: ys')           -> (((Current Nothing' x )) :! y') ::: valueCreatorAwait fun x xs' y' ys'
     Both (x' ::: xs') (y' ::: ys') -> ((Current (Just' x') x') :! y')  ::: valueCreatorAwait fun x' xs' y' ys')
