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

module PropRatt.HList where       
import AsyncRattus.Signal hiding (mkSig)
import AsyncRattus.Strict
import AsyncRattus.InternalPrimitives
import AsyncRattus.Plugin.Annotation
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)
import Data.Kind (Type)
import Test.QuickCheck (Arbitrary (arbitrary), Gen)
import qualified Data.IntSet as IntSet

data HList :: [Type] -> Type where
  HNil :: HList '[]
  HCons :: !x -> !(HList xs) -> HList (x ': xs)

instance Show (HList '[]) where
  show :: HList '[] -> String
  show HNil = "HNil"

type family Map (f :: Type -> Type) (xs :: [Type]) :: [Type] where
  Map f '[] = '[]
  Map f (x ': xs) = f x ': Map f xs

-- Use polykinds to allow us to overload generateSignals to work for both Type and Type -> Type
type family ToList (a :: k) :: [Type] where
  ToList (a :: [Type]) = a
  ToList (a :: Type)   = '[a]

class HListGen (ts :: [Type]) where
  generateHList :: Gen (HList (Map Sig ts))

instance HListGen '[] where
  generateHList = return HNil

instance (Arbitrary (Sig t), HListGen ts) => HListGen (t ': ts) where
  generateHList = do
    x <- arbitrary
    xs <- generateHList @ts
    return (x %: xs)

infixr 5 %:
(%:) :: x -> HList xs -> HList (x ': xs)
(%:) = HCons

instance (Show x, (Show (HList xs))) => Show (HList (x ': xs)) where
  show (HCons x xs) = show x ++ " %: " ++ show xs

instance Stable (HList '[]) where
instance (Stable a, Stable (HList as)) => Stable (HList (a ': as)) where
