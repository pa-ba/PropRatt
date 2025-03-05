{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}

module PropRatt.AsyncRat where
import AsyncRattus.Signal hiding (mkSig)
import AsyncRattus.Strict
import AsyncRattus.InternalPrimitives
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)
import PropRatt.Value
import Data.Kind (Type)
import Data.Data (Proxy (Proxy))

aRatZip :: Sig Int -> Sig Int -> Sig (Int :* Int)
aRatZip a b = zip a b

aRatSwitch :: Sig a -> O (Sig a) -> Sig a
aRatSwitch a o = switch a o

aRatParallel :: Sig a -> Sig b -> Sig (Maybe' a :* Maybe' b)
aRatParallel a b = parallel a b

data HList :: [Type] -> Type where
  HNil :: HList '[]
  HCons :: !x -> !(HList xs) -> HList (x ': xs)

instance Show (HList '[]) where
  show :: HList '[] -> String
  show HNil = "HNil"

infixr 5 %:
(%:) :: x -> HList xs -> HList (x ': xs)
(%:) = HCons

instance (Show x, (Show (HList xs))) => Show (HList (x ': xs)) where
  show (HCons x xs) = show x ++ " %: " ++ show xs

type family Map (f :: Type -> Type) (xs :: [Type]) :: [Type] where
  Map f '[] = '[]
  Map f (x ': xs) = f x ': Map f xs

instance Stable (HList '[]) where
instance (Stable a, Stable (HList as)) => Stable (HList (a ': as)) where

class Stable (HList v) => Flatten sigs v | sigs -> v, v -> sigs where
  flatten :: HList sigs -> Sig (HList v)

class Nothingfy a b where
  toNothing :: HList a -> HList b

instance {-# OVERLAPPING #-} Stable a => Flatten '[Sig a] '[Value a] where
  flatten :: Stable a => HList '[Sig a] -> Sig (HList '[Value a])
  flatten (HCons h HNil) = singleton' h

instance (Stable a, Flatten sigs v, Nothingfy v v) => Flatten (Sig a ': sigs) (Value a ': v) where
  flatten :: (Stable a, Flatten sigs v, Nothingfy v v) => HList (Sig a : sigs) -> Sig (HList (Value a : v))
  flatten (HCons h t) = prepend h (flatten t)

instance {-# OVERLAPPING #-} Nothingfy '[Value a] '[Value a] where
  toNothing :: HList '[Value a] -> HList '[Value a]
  toNothing (HCons (Current _ y) HNil) = Current Nothing' y %: HNil

instance (Nothingfy as as) => Nothingfy (Value a ': as) (Value a ': as) where
  toNothing :: Nothingfy as as => HList (Value a : as) -> HList (Value a : as)
  toNothing (HCons (Current _ y) t) = Current Nothing' y %: toNothing t

-- TODO generalize?? perhaps make safe to use for empty hlist
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

prepend :: (Stable a, Stable (HList v), Nothingfy v v) => Sig a -> Sig (HList v) -> Sig (HList (Value a ': v))
prepend (x ::: xs) (y ::: ys) =
  HCons (Current (Just' x) x) y ::: prependAwait x xs y ys

prependAwait :: (Stable a, Stable ls, ls ~ HList v, Nothingfy v v) => a -> O (Sig a) -> ls -> O (Sig ls) -> O (Sig (HList (Value a ': v)))
prependAwait x xs y ys  = delay (
  case select xs ys of
     Fst (x' ::: xs')   ys'         -> (Current (Just' x') x' %: toNothing y) ::: prependAwait x' xs' y ys'
     Snd xs' (y' ::: ys')           -> (Current Nothing' x %: y') ::: prependAwait x xs' y' ys'
     Both (x' ::: xs') (y' ::: ys') -> (Current (Just' x') x' %: y')  ::: prependAwait x' xs' y' ys')

singleton' :: Sig a -> Sig (HList (Map Value '[a]))
singleton' = map (box (\p -> Current (Just' p) p %: HNil))