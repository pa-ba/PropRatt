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
  singletonH
) where

import AsyncRattus.Signal
import AsyncRattus.Strict hiding (singleton)
import AsyncRattus.InternalPrimitives hiding (never)
import PropRatt.Value
import PropRatt.HList
import Prelude hiding (const)

emptySig :: Sig (HList '[])
emptySig = const HNil

class Stable (HList vals) => Flatten sigs vals | sigs -> vals, vals -> sigs where
  flatten :: HList sigs -> Sig (HList vals)

instance Flatten '[] '[] where
  flatten :: HList '[] -> Sig (HList '[])
  flatten HNil = emptySig

instance (Stable a, Stable (Value a), Flatten as bs, Falsify bs) => Flatten (Sig a ': as) (Value a ': bs) where
  flatten :: HList (Sig a : as) -> Sig (HList (Value a : bs))
  flatten (h :% t) = prepend h (flatten t)

class Falsify ts where
  toFalse :: HList ts -> HList ts

instance Falsify '[] where
  toFalse :: HList '[] -> HList '[]
  toFalse _ =  HNil

instance (Falsify ts) => Falsify (Value t ': ts) where
  toFalse :: HList (Value t : ts) -> HList (Value t : ts)
  toFalse (Current _ x :% t) = Current (HasTick False) x :% toFalse t

-- | Like 'prepend', but the new head is delayed by one tick.
--   This emits a dummy value at the head on the first tick, then behaves like 'prepend' on subsequent ticks.
prependLater :: (Stable t, Stable (HList ts), Falsify ts) => O (Sig t) -> Sig (HList ts) -> Sig (HList (Value t ': ts))
prependLater xs (y ::: ys) =
  (Current (HasTick False) Nil :% y) ::: prependAwait Nil xs y ys

prepend :: (Stable t, Stable (HList ts), Falsify ts) => Sig t -> Sig (HList ts) -> Sig (HList (Value t ': ts))
prepend (x ::: xs) (y ::: ys) =
  (Current (HasTick True) (x :! Nil) :% y) ::: prependAwait (x :! Nil) xs y ys

prependAwait :: (Stable t, Stable hls, hls ~ HList ts, Falsify ts) => List t -> O (Sig t) -> hls -> O (Sig hls) -> O (Sig (HList (Value t ': ts)))
prependAwait x xs y ys  = delay (
  case select xs ys of
     Fst (x' ::: xs')   ys'         -> (Current (HasTick True) (x' :! x) :% toFalse y)  ::: prependAwait (x' :! x) xs' y ys'
     Snd xs' (y' ::: ys')           -> (Current (HasTick False) x :% y')                ::: prependAwait x xs' y' ys'
     Both (x' ::: xs') (y' ::: ys') -> (Current (HasTick True) (x' :! x) :% y')         ::: prependAwait (x' :! x) xs' y' ys')

singletonH :: (Stable t) => Sig t -> Sig (HList '[Value t])
singletonH sig = flatten (sig :% HNil)