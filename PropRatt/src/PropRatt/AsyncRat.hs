{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module PropRatt.AsyncRat where
import AsyncRattus.Signal hiding (mkSig)
import AsyncRattus.Strict
import AsyncRattus.InternalPrimitives
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)
import PropRatt.Value
import Data.Kind (Type)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, resize)
import qualified Data.IntSet as IntSet

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

class Stable (HList v) => Flatten s v | s -> v, v -> s where
  flatten :: HList s -> Sig (HList v)

instance {-# OVERLAPPING #-} (Stable a, Stable (Value a)) => Flatten '[Sig a] '[Value a] where
  flatten (HCons h HNil) = singletonWithHistory h

instance (Stable a, Stable (Value a), Flatten as v, Falsify v) => Flatten (Sig a ': as) (Value a ': v) where
  flatten (HCons h t) = prepend h (flatten t)

class Falsify a where
  toFalse :: HList a -> HList a

instance Falsify '[] where
  toFalse _ =  HNil

instance (Falsify as) => Falsify (Value a ': as) where
  toFalse (HCons (Current _ y) t) = Current (HasTicked False) y %: toFalse t

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

eigth :: HList (_ ': _ ': _ ': _ ': _ ': _ ': _ ': a ': _) -> a
eigth (HCons _ (HCons _ (HCons _ (HCons _ (HCons _ (HCons _ (HCons _ (HCons h8 _)))))))) = h8

ninth :: HList (_ ': _ ': _ ': _ ': _ ': _ ': _ ': _ ': a ': _) -> a
ninth (HCons _ (HCons _ (HCons _ (HCons _ (HCons _ (HCons _ (HCons _ (HCons _ (HCons h9 _))))))))) = h9

prepend :: (Stable a, Stable (HList v), Falsify v) => Sig a -> Sig (HList v) -> Sig (HList (Value a ': v))
prepend (x ::: xs) (y ::: ys) =
  HCons (Current (HasTicked True) (x :! Nil)) y ::: prependAwait (x :! Nil) xs y ys

prependAwait :: (Stable a, Stable ls, ls ~ HList v, Falsify v) => List a -> O (Sig a) -> ls -> O (Sig ls) -> O (Sig (HList (Value a ': v)))
prependAwait x xs y ys  = delay (
  case select xs ys of
     Fst (x' ::: xs')   ys'         -> (Current (HasTicked True) (x' :! x) %: toFalse y) ::: prependAwait (x':!x) xs' y ys'
     Snd xs' (y' ::: ys')           -> (Current (HasTicked False) x %: y') ::: prependAwait x xs' y' ys'
     Both (x' ::: xs') (y' ::: ys') -> (Current (HasTicked True) (x' :! x) %: y') ::: prependAwait (x':!x) xs' y' ys')

-- Make a singleton HList from an a
mkSingleton :: a -> HList '[Value a]
mkSingleton a = Current (HasTicked True) (a :! Nil) %: HNil

singleton' :: Sig a -> Sig (HList (Map Value '[a]))
singleton' = map (box mkSingleton)

singletonWithHistory :: (Stable (Value a), Stable a, ls ~ HList '[Value a]) => Sig a -> Sig ls
singletonWithHistory xs = combine (singleton' xs) (Current (HasTicked True) Nil %: HNil)

combine :: (Stable a, Stable ls, ls ~ HList '[Value a]) => Sig ls -> ls -> Sig ls
combine (h ::: t) oldValues = comb h oldValues ::: delay (combine (adv t) (comb h oldValues))

comb :: HList '[Value a] -> HList '[Value a] -> HList '[Value a]
comb (HCons (Current b newVals) HNil) (HCons (Current _ oldVals) HNil) = Current b (newVals +++ oldVals) %: HNil

------------------- HLIST SIGNAL GENERATION ----------------------

type family Map (f :: Type -> Type) (xs :: [Type]) :: [Type] where
  Map f '[]       = '[]
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
    x  <- arbitrary :: Gen (Sig t)
    xs <- generateHList @ts
    return (x %: xs)

-- Generate an hlist of signals of the given type or list of types
-- example: generateHList @Int
-- example: generateHList @[Bool, Char]
generateSignals :: forall a. HListGen (ToList a) => Gen (HList (Map Sig (ToList a)))
generateSignals = generateHList @(ToList a)