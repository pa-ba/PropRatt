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
import PropRatt.Arbitrary
import qualified Data.IntSet as IntSet
import PropRatt.HList
import Prelude hiding (const)

emptySig :: Sig (HList '[])
emptySig = const HNil

class Stable (HList vals) => Flatten sigs vals | sigs -> vals, vals -> sigs where
  flatten :: HList sigs -> Sig (HList vals)

instance Flatten '[] '[] where
  flatten HNil = emptySig

instance (Stable a, Stable (Value a), Flatten as bs, Falsify bs) => Flatten (Sig a ': as) (Value a ': bs) where
  flatten :: HList (Sig a : as) -> Sig (HList (Value a : bs))
  flatten (HCons h t) = prepend h (flatten t)

class Falsify ts where
  toFalse :: HList ts -> HList ts

instance Falsify '[] where
  toFalse :: HList '[] -> HList '[]
  toFalse _ =  HNil

instance (Falsify ts) => Falsify (Value t ': ts) where
  toFalse :: HList (Value t : ts) -> HList (Value t : ts)
  toFalse (HCons (Current _ x) t) = Current (HasTicked False) x %: toFalse t

prependLater :: (Stable t, Stable (HList ts), Falsify ts) => O (Sig t) -> Sig (HList ts) -> Sig (HList (Value t ': ts))
prependLater xs (y ::: ys) =
  HCons (Current (HasTicked False) Nil) y ::: prependAwait Nil xs y ys

prepend :: (Stable t, Stable (HList ts), Falsify ts) => Sig t -> Sig (HList ts) -> Sig (HList (Value t ': ts))
prepend (x ::: xs) (y ::: ys) =
  HCons (Current (HasTicked True) (x :! Nil)) y ::: prependAwait (x :! Nil) xs y ys

prependAwait :: (Stable t, Stable hls, hls ~ HList ts, Falsify ts) => List t -> O (Sig t) -> hls -> O (Sig hls) -> O (Sig (HList (Value t ': ts)))
prependAwait x xs y ys  = delay (
  case select xs ys of
     Fst (x' ::: xs')   ys'         -> (Current (HasTicked True) (x' :! x) %: toFalse y)  ::: prependAwait (x' :! x) xs' y ys'
     Snd xs' (y' ::: ys')           -> (Current (HasTicked False) x %: y')                ::: prependAwait x xs' y' ys'
     Both (x' ::: xs') (y' ::: ys') -> (Current (HasTicked True) (x' :! x) %: y')         ::: prependAwait (x' :! x) xs' y' ys')

singleton ::  (Stable t) => Sig t -> Sig (HList (Map Value '[t]))
singleton sig = singletonAwait sig Nil

singletonAwait :: (Stable t) => Sig t -> List t -> Sig (HList (Map Value '[t]))
singletonAwait (h ::: t@(Delay cl _)) acc = if IntSet.null cl 
  then HCons (Current (HasTicked True) (h :! acc)) HNil ::: never
  else HCons (Current (HasTicked True) (h :! acc)) HNil ::: delay (singletonAwait (adv t) (h :! acc))

singletonH :: (Stable t) => Sig t -> Sig (HList '[Value t])
singletonH sig = flatten (sig %: HNil)

never :: O (Sig (HList (Map Value '[t])))
never = Delay IntSet.empty (error "Trying to adv on the 'never' delayed computation")