{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module PropRatt.HList (HList(..), (%:), first,second,third,fourth,fifth,sixth,seventh,eighth,ninth,lengthH) where       
import AsyncRattus.InternalPrimitives ( Stable )
import Data.Kind (Type)

data HList :: [Type] -> Type where
  HNil :: HList '[]
  HCons :: !x -> !(HList xs) -> HList (x ': xs)

infixr 5 %:
(%:) :: x -> HList xs -> HList (x ': xs)
(%:) = HCons

instance Show (HList '[]) where
  show :: HList '[] -> String
  show HNil = "HNil"

instance (Show x, (Show (HList xs))) => Show (HList (x ': xs)) where
  show :: (Show x, Show (HList xs)) => HList (x : xs) -> String
  show (HCons x xs) = show x ++ " %: " ++ show xs

instance Stable (HList '[]) where
instance (Stable a, Stable (HList as)) => Stable (HList (a ': as)) where

first :: HList (a ': _) -> a
first (HCons h _) = h

second :: HList (_ ': a ': _) -> a
second (HCons _ (HCons h2 _)) = h2

third :: HList (_ ': _ ': a ': _) -> a
third (HCons _ (HCons _ (HCons h3 _))) = h3

fourth :: HList (_ ': _ ': _ ': a ': _) -> a
fourth (HCons _ (HCons _ (HCons _ (HCons h4 _)))) = h4

fifth :: HList (_ ': _ ': _ ': _ ': a ': _) -> a
fifth (HCons _ (HCons _ (HCons _ (HCons _ (HCons h5 _))))) = h5

sixth :: HList (_ ': _ ': _ ': _ ': _ ': a ': _) -> a
sixth (HCons _ (HCons _ (HCons _ (HCons _ (HCons _ (HCons h6 _)))))) = h6

seventh :: HList (_ ':_ ': _ ': _ ': _ ': _ ': a ': _) -> a
seventh (HCons _ (HCons _ (HCons _ (HCons _ (HCons _ (HCons _ (HCons h7 _))))))) = h7

eighth :: HList (_ ': _ ': _ ': _ ': _ ': _ ': _ ': a ': _) -> a
eighth (HCons _ (HCons _ (HCons _ (HCons _ (HCons _ (HCons _ (HCons _ (HCons h8 _)))))))) = h8

ninth :: HList (_ ': _ ': _ ': _ ': _ ': _ ': _ ': _ ': a ': _) -> a
ninth (HCons _ (HCons _ (HCons _ (HCons _ (HCons _ (HCons _ (HCons _ (HCons _ (HCons h9 _))))))))) = h9


lengthH :: HList ts -> Int -> Int
lengthH HNil n = n
lengthH (HCons _ as) n = lengthH as (n+1)