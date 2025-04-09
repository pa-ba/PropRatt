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
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module PropRatt.RatUtils where

import AsyncRattus.Signal (Sig(..))
import AsyncRattus.InternalPrimitives
import qualified Data.IntSet as IntSet
import Prelude hiding (take)
import PropRatt.HList
import PropRatt.Utilities


class Halving a where
  halve :: HList a -> HList a

instance Halving '[] where
  halve :: HList '[] -> HList '[]
  halve _ =  HNil

instance (Halving as) => Halving (Sig a ': as) where
  halve :: Halving as => HList (Sig a : as) -> HList (Sig a : as)
  halve (HCons x xs) = (shrinkSigOnce x ) %: halve xs


take :: Int -> Sig a -> Sig a
take 1 (x ::: Delay cl f) = x ::: never
take n (x ::: xs@(Delay cl f)) = if IntSet.null cl then x ::: never else x ::: delay (take (n-1) (adv xs))

shrinkHlistOfSig :: (Halving ts) => HList (Sig t ': ts) -> [HList (Sig t ': ts)]
shrinkHlistOfSig HNil = [HNil]
shrinkHlistOfSig (HCons x xs) = [ HCons (shrinkSigOnce x) ys | ys <- shrinkHlistOfSig xs ] 

shrinkSigOnce :: Sig a -> Sig a
shrinkSigOnce sig = 
  let len = sigLength sig `div` 2
      newSig = take len sig
  in newSig
