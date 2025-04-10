{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module PropRatt.Core (
  prepend,
  prependLater,
  flatten,
  singletonHList
) where

import AsyncRattus.Signal
import AsyncRattus.Strict hiding (singleton)
import AsyncRattus.InternalPrimitives hiding (never)
import PropRatt.Value
import PropRatt.Arbitrary
import qualified Data.IntSet as IntSet
import PropRatt.HList

class Stable (HList v) => Flatten s v | s -> v, v -> s where
  flatten :: HList s -> Sig (HList v)

instance {-# OVERLAPPING #-} (Stable a, Stable (Value a)) => Flatten '[Sig a] '[Value a] where
  flatten (HCons h HNil) = singleton h

instance (Stable a, Stable (Value a), Flatten as v, Falsify v) => Flatten (Sig a ': as) (Value a ': v) where
  flatten (HCons h t) = prepend h (flatten t)

class Falsify a where
  toFalse :: HList a -> HList a

instance Falsify '[] where
  toFalse :: HList '[] -> HList '[]
  toFalse _ =  HNil

instance (Falsify as) => Falsify (Value a ': as) where
  toFalse :: HList (Value a : as) -> HList (Value a : as)
  toFalse (HCons (Current _ y) t) = Current (HasTicked False) y %: toFalse t

prependLater :: (Stable a, Stable (HList v), Falsify v) => O (Sig a) -> Sig (HList v) -> Sig (HList (Value a ': v))
prependLater xs (y ::: ys) =
  HCons (Current (HasTicked False) Nil) y ::: prependAwait Nil xs y ys

prepend :: (Stable a, Stable (HList v), Falsify v) => Sig a -> Sig (HList v) -> Sig (HList (Value a ': v))
prepend (x ::: xs) (y ::: ys) =
  HCons (Current (HasTicked True) (x :! Nil)) y ::: prependAwait (x :! Nil) xs y ys

prependAwait :: (Stable a, Stable ls, ls ~ HList v, Falsify v) => List a -> O (Sig a) -> ls -> O (Sig ls) -> O (Sig (HList (Value a ': v)))
prependAwait x xs y ys  = delay (
  case select xs ys of
     Fst (x' ::: xs')   ys'         -> (Current (HasTicked True) (x' :! x) %: toFalse y) ::: prependAwait (x':!x) xs' y ys'
     Snd xs' (y' ::: ys')           -> (Current (HasTicked False) x %: y') ::: prependAwait x xs' y' ys'
     Both (x' ::: xs') (y' ::: ys') -> (Current (HasTicked True) (x' :! x) %: y') ::: prependAwait (x':!x) xs' y' ys')

singleton ::  (Stable a) => Sig a -> Sig (HList (Map Value '[a]))
singleton sig = singletonAwait sig Nil

singletonAwait :: (Stable a) => Sig a -> List a -> Sig (HList (Map Value '[a]))
singletonAwait (h ::: t@(Delay cl _)) acc = if IntSet.null cl 
  then HCons (Current (HasTicked True) (h :! acc)) HNil ::: never
  else HCons (Current (HasTicked True) (h :! acc)) HNil ::: delay (singletonAwait (adv t) (h :! acc))

singletonHList :: (Stable a) => Sig a -> Sig (HList '[Value a])
singletonHList sig = flatten (sig %: HNil)

never :: O (Sig (HList (Map Value '[a])))
never = Delay IntSet.empty (error "Trying to adv on the 'never' delayed computation")