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
    prepend,
    singleton',
    flattenToSignal,
    flattenToSignal'
) where

import AsyncRattus.Plugin.Annotation
import AsyncRattus.Signal hiding (mkSig)
import AsyncRattus.Strict
import AsyncRattus.InternalPrimitives
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)
import PropRatt.Value (Value(..))

data NonEmptyList a = NonEmptyList !a !(List a)

aRatZip :: Sig Int -> Sig Int -> Sig (Int :* Int)
aRatZip a b = zip a b

aRatSwitch :: Sig a -> O(Sig a) -> Sig a
aRatSwitch a o = switch a o

aRatParallel :: Sig a -> Sig b -> Sig (Maybe' a :* Maybe' b)
aRatParallel a b = parallel a b

singleton' :: Sig a -> Sig (List (Value a))
singleton' xs = map (box (\x -> Current (Just' x) x :! Nil)) xs

prepend :: (Stable a) => Sig a -> Sig (List (Value a)) -> Sig (List (Value a))
prepend (x ::: xs) (y ::: ys) =
   (Current (Just' x) x :! y) ::: prependAwait x xs y ys

prependAwait :: (Stable a) => a -> O (Sig a) -> List (Value a) -> O (Sig (List (Value a))) -> O (Sig (List (Value a)))
prependAwait x xs y ys  = delay (
  case select xs ys of
     Fst (x' ::: xs')   ys'         -> (Current (Just' x') x' :! map' (\(Current _ x) -> Current Nothing' x) y) ::: prependAwait x' xs' y ys'
     Snd xs' (y' ::: ys')           -> (Current Nothing' x :! y') ::: prependAwait x xs' y' ys'
     Both (x' ::: xs') (y' ::: ys') -> (Current (Just' x') x' :! y')  ::: prependAwait x' xs' y' ys')

{-# ANN flattenToSignal' AllowRecursion #-}
flattenToSignal' :: (Stable a) => List (Sig a) -> Sig (List (Value a))
flattenToSignal' Nil = undefined
flattenToSignal' (h :! Nil) = singleton' h
flattenToSignal' (h :! t)   = prepend h (flattenToSignal' t)

flattenToSignal :: (Stable a) => NonEmptyList (Sig a) -> Sig (List (Value a))
flattenToSignal (NonEmptyList h Nil) = singleton' h
flattenToSignal (NonEmptyList h t)   = prepend h (flattenToSignal' t)