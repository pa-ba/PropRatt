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

module PropRatt.HList (HList(..), first,second,third,fourth,fifth,sixth,seventh,eighth,ninth,lengthH) where       
import AsyncRattus.InternalPrimitives ( Stable )
import Data.Kind (Type)

data HList :: [Type] -> Type where
  HNil :: HList '[]
  (:%) :: !x -> !(HList xs) -> HList (x ': xs)

infixr 5 :%

instance Show (HList '[]) where
  show :: HList '[] -> String
  show HNil = "\n"

instance (Show x, (Show (HList xs))) => Show (HList (x ': xs)) where
  show :: (Show x, Show (HList xs)) => HList (x : xs) -> String
  show (x :% xs) = show x ++ "; " ++ show xs

instance Stable (HList '[]) where
instance (Stable a, Stable (HList as)) => Stable (HList (a ': as)) where

first :: HList (a ': _) -> a
first (v :% _) = v

second :: HList (_ ': a ': _) -> a
second (_ :% v :% _) = v

third :: HList (_ ': _ ': a ': _) -> a
third (_ :% _ :% v :% _) = v

fourth :: HList (_ ': _ ': _ ': a ': _) -> a
fourth (_ :% _ :% _ :% v :% _) = v

fifth :: HList (_ ': _ ': _ ': _ ': a ': _) -> a
fifth (_ :% _ :% _ :% _ :% v :% _) = v

sixth :: HList (_ ': _ ': _ ': _ ': _ ': a ': _) -> a
sixth (_ :% _ :% _ :% _ :% _ :% v :% _) = v

seventh :: HList (_ ':_ ': _ ': _ ': _ ': _ ': a ': _) -> a
seventh (_ :% _ :% _ :% _ :% _ :% _ :% v :% _) = v

eighth :: HList (_ ': _ ': _ ': _ ': _ ': _ ': _ ': a ': _) -> a
eighth (_ :% _ :% _ :% _ :% _ :% _ :% _ :% v :% _) = v

ninth :: HList (_ ': _ ': _ ': _ ': _ ': _ ': _ ': _ ': a ': _) -> a
ninth (_ :% _ :% _ :% _ :% _ :% _ :% _ :% _ :% v :% _) = v


lengthH :: HList ts -> Int -> Int
lengthH HNil n = n
lengthH (_ :% as) n = lengthH as (n+1)