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
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module PropRatt.AsyncRat where
import AsyncRattus.Signal hiding (mkSig)
import AsyncRattus.Strict
import AsyncRattus.InternalPrimitives
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)
import PropRatt.Value
import Data.Kind (Type)
import Test.QuickCheck (Arbitrary (arbitrary), Gen)

aRatZip :: Sig Int -> Sig Int -> Sig (Int :* Int)
aRatZip a b = zip a b

aRatSwitch :: Sig a -> O (Sig a) -> Sig a
aRatSwitch a o = switch a o

aRatParallel :: Sig a -> Sig b -> Sig (Maybe' a :* Maybe' b)
aRatParallel a b = parallel a b

jumpFunc :: Int -> Maybe' (Sig Int)
jumpFunc x = if x > 5 then Nothing' else (Just' (10 ::: never))

aRatJump :: Box (a -> Maybe' (Sig a)) -> Sig a -> Sig a 
aRatJump f sig = jump f sig

aRatBuffer :: Stable a => a -> Sig a -> Sig a
aRatBuffer a x = buffer a x

interleaveFunc :: Int -> Int -> Int
interleaveFunc a1 a2 = a1 * a2 + 2

aRatInterleave :: Box (a -> a -> a) -> O (Sig a) -> O (Sig a) -> O (Sig a)
aRatInterleave f x y = interleave f x y

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

instance Stable (HList '[]) where
instance (Stable a, Stable (HList as)) => Stable (HList (a ': as)) where

type family Map (f :: Type -> Type) (xs :: [Type]) :: [Type] where
  Map f '[] = '[]
  Map f (x ': xs) = f x ': Map f xs

class HListGen (ts :: [Type]) where
  generateHList :: Gen (HList (Map Sig ts))

instance HListGen '[] where
  generateHList = return HNil

instance (Arbitrary (Sig t), HListGen ts) => HListGen (t ': ts) where
  generateHList = do
    x <- arbitrary
    xs <- generateHList @ts
    return (x %: xs)

class Stable (HList v) => Flatten s v | s -> v, v -> s where
  flatten :: HList s -> Sig (HList v)

class Nothingfy a where
  toNothing :: HList a -> HList a

instance {-# OVERLAPPING #-} Stable a => Flatten '[Sig a] '[Value a] where
  flatten (HCons h HNil) = singleton' h

instance (Stable a, Flatten as v, Nothingfy v) => Flatten (Sig a ': as) (Value a ': v) where
  flatten (HCons h t) = prepend h (flatten t)

instance Nothingfy '[] where
  toNothing _ =  HNil

instance (Nothingfy as) => Nothingfy (Value a ': as) where
  toNothing (HCons (Current _ y) t) = Current Nothing' y %: toNothing t

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

prepend :: (Stable a, Stable (HList v), Nothingfy v) => Sig a -> Sig (HList v) -> Sig (HList (Value a ': v))
prepend (x ::: xs) (y ::: ys) =
  HCons (Current (Just' x) x) y ::: prependAwait x xs y ys

prependAwait :: (Stable a, Stable ls, ls ~ HList v, Nothingfy v) => a -> O (Sig a) -> ls -> O (Sig ls) -> O (Sig (HList (Value a ': v)))
prependAwait x xs y ys  = delay (
  case select xs ys of
     Fst (x' ::: xs')   ys'         -> (Current (Just' x') x' %: toNothing y) ::: prependAwait x' xs' y ys'
     Snd xs' (y' ::: ys')           -> (Current Nothing' x %: y') ::: prependAwait x xs' y' ys'
     Both (x' ::: xs') (y' ::: ys') -> (Current (Just' x') x' %: y')  ::: prependAwait x' xs' y' ys')

singleton' :: Sig a -> Sig (HList (Map Value '[a]))
singleton' = map (box (\p -> Current (Just' p) p %: HNil))

generateSigs :: forall (ts :: [Type]). HListGen ts => Gen (HList (Map Sig ts))
generateSigs = generateHList @ts