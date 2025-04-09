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
import AsyncRattus.Plugin.Annotation



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

{-# ANN shrinkHlistOfSig AllowRecursion #-}
shrinkHlistOfSig :: (Halving ts) => HList (Sig t ': ts) -> [HList (Sig t ': ts)]
shrinkHlistOfSig hls = go hls []
  where
    go :: (Halving ts) 
       => HList (Sig t ': ts) 
       -> [HList (Sig t ': ts)]  -- Accumulator for strictly shrunken candidates.
       -> [HList (Sig t ': ts)]
    go current acc =
      case current of
        HCons x HNil -> if sigLength x < 30 then acc else (halve current) : acc  -- Do not include the current candidate if it doesn't shrink further.
        HCons x xs ->
          if sigLength x < 30 then acc else
          let shrunk = halve current
          in go shrunk (shrunk : acc)
    
    headSignal :: HList (Sig t ': ts) -> Sig t
    headSignal (HCons y _) = y



shrinkSigOnce :: Sig a -> Sig a
shrinkSigOnce sig = 
  let len = sigLength sig `div` 2
      newSig = take len sig
  in newSig
