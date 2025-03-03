{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

-- AsyncRattus code goes here. 
-- The code is type checked by the AsyncRattus compiler plugin.

module PropRatt.AsyncRat (
    aRatZip,
    aRatSwitch,
    aRatParallel,
) where

import AsyncRattus.Plugin.Annotation
import AsyncRattus.Signal hiding (mkSig)
import AsyncRattus.Strict
import AsyncRattus.InternalPrimitives
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)
import PropRatt.Value
import Data.Kind (Type)


aRatZip :: Sig Int -> Sig Int -> Sig (Int :* Int)
aRatZip a b = zip a b

aRatSwitch :: Sig a -> O (Sig a) -> Sig a
aRatSwitch a o = switch a o

aRatParallel :: Sig a -> Sig b -> Sig (Maybe' a :* Maybe' b)
aRatParallel a b = parallel a b




-- strict HList
data HList :: [Type] -> Type where
  HNil :: HList '[]
  HCons :: !x -> !(HList xs) -> HList (x ': xs)
  
instance Show (HList '[]) where
  show HNil = "HNil"

list :: HList [Int, String, Int]
list = HCons 5 (HCons "Hej" (HCons 5 (HNil)))

infixr 5 %:
(%:) :: x -> HList xs -> HList (x ': xs)
(%:) = HCons

class Apply f a b where
  apply :: f -> a -> b

instance (Show x, (Show (HList xs))) => Show (HList (x ': xs)) where
  show (HCons x xs) = show x ++ " %: " ++ show xs 

class MapH f xs ys where
  mapH :: f -> HList xs -> HList ys

instance MapH f '[] '[] where
   mapH _ _ = HNil

instance (Apply f x y, MapH f xs ys) => MapH f (x ': xs) (y ': ys) where
  mapH f (HCons x xs) = apply f x %: mapH f xs

type family Map (f :: Type -> Type) (xs :: [Type]) :: [Type] where
  Map f '[] = '[]
  Map f (x ': xs) = f x ': Map f xs


-- class AllStable (as :: [Type]) where 

-- instance AllStable '[] where

-- instance (Stable a, AllStable as) => AllStable (a ': as) where

instance Stable (HList '[]) where
instance (Stable a, Stable (HList as)) => Stable (HList (a ': as)) where

class Stable (HList vals) => Flatten sigs vals | sigs -> vals, vals -> sigs where
  flatten :: HList sigs -> Sig (HList vals)

class Stable (HList vals1) => Nothingfy vals1 vals2 where
  makeNothings :: HList vals1 -> HList vals2

instance Stable a => Flatten '[Sig a] '[Value a] where
  flatten (HCons head HNil) = singleton' head

instance (Stable a, Flatten sigs vals, Nothingfy vals vals) => Flatten (Sig a ': sigs) (Value a ': vals) where
  flatten (HCons head tail) = prepend head (flatten tail)

instance Stable a => Nothingfy '[Value a] '[Value a] where
  makeNothings (HCons (Current x y) HNil) = Current Nothing' y %: HNil

instance (Stable a, Nothingfy as as) => Nothingfy (Value a ': as) (Value a ': as) where
  makeNothings (HCons (Current x y) tail) = Current Nothing' y %: makeNothings tail


prepend :: (Stable a, Flatten sigs vals, Nothingfy vals vals) => Sig a -> Sig (HList vals) -> Sig (HList (Value a ': vals))
prepend (x ::: xs) (y ::: ys) = 
  (HCons (Current (Just' x) x) y) ::: prependAwait x xs y ys


prependAwait :: (Stable a, Stable l, l ~ HList vals, Nothingfy vals vals) => a -> O (Sig a) -> l -> O (Sig l) -> O (Sig (HList (Value a ': vals)))
prependAwait x xs y ys  = delay (
  case select xs ys of
     Fst (x' ::: xs')   ys'         -> (Current (Just' x') x' %: makeNothings y) ::: prependAwait x' xs' y ys'
     Snd xs' (y' ::: ys')           -> (Current Nothing' x %: y') ::: prependAwait x xs' y' ys'
     Both (x' ::: xs') (y' ::: ys') -> (Current (Just' x') x' %: y')  ::: prependAwait x' xs' y' ys')


singleton' :: (Stable a) => Sig a -> Sig (HList (Map Value '[a])) -- (Sig (HList (Map Value xs))) - Gives a type error
singleton' xs = map (box (\p -> (Current (Just' p) p) %: HNil)) xs
