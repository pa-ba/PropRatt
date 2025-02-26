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
import Test.QuickCheck (functionShow)

-- data NonEmptyList a = NonEmptyList !a !(HList a)

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

instance (Show x, (Show (HList xs))) => Show (HList (x ': xs)) where
  show (HCons x xs) = show x ++ " %: " ++ show xs 

aRatZip :: Sig Int -> Sig Int -> Sig (Int :* Int)
aRatZip a b = zip a b

aRatSwitch :: Sig a -> O (Sig a) -> Sig a
aRatSwitch a o = switch a o

aRatParallel :: Sig a -> Sig b -> Sig (Maybe' a :* Maybe' b)
aRatParallel a b = parallel a b

singleton' :: Sig a -> Sig (HList '[Value a])
singleton' xs = map (box (\p -> Current (Just' p) p %: HNil)) xs

-- lacks maph constraint
prepend :: (Stable a) => Sig a -> Sig (HList (Map (a ': xs))) -> Sig (HList (Map (a ': xs)))
prepend q (y ::: ys) = do
  -- make input sig into same type as acc :: Sig (HList (Map (Value a) xs))
  let (x ::: xs) = singleton' q 
  --let (HCons x' HNil) = x
  (x %: y) ::: prependAwait xs ys

-- lacks maph constraint
-- lacks symbol function to map values to nothing
prependAwait :: (Stable a) => O (Sig (HList (Value a ': xs))) -> O (Sig (HList( Map (Value a ': xs)))) -> O (Sig (HList (Map (Value a ': xs))))
prependAwait xs ys = delay (
  case select xs ys of
     Fst (x' ::: xs')   ys'         -> (x' %: mapH (\(Current _ x) -> Current Nothing' x) y) ::: prependAwait xs' ys'
     Snd xs' (y' ::: ys')           -> mapH (\(Current _ x) -> Current Nothing' x) xs' %: y' ::: prependAwait xs' ys'
     Both (x' ::: xs') (y' ::: ys') -> (x' %: y')  ::: prependAwait xs' ys')

class Apply f a b where
  apply :: f -> a -> b

data Valify = Valify
instance Apply Valify a (Value a) where
  apply _ x = Current (Just' x) x

type family Map (xs :: [Type]) :: [Type] where
  Map '[]       = '[]
  Map '[Value a] = '[Value a]
  Map (x ': xs) = Wrap x ': Map xs

class MapH f xs ys where
  mapH :: f -> HList xs -> HList ys

class FoldrH f acc xs where
  foldrH :: f -> acc -> HList xs -> acc
  
instance MapH f '[] '[] where
   mapH _ _ = HNil

instance FoldrH f acc '[] where
   foldrH _ acc _ = acc

instance (Apply f x y, MapH f xs ys) => MapH f (x ': xs) (y ': ys) where
  mapH f (HCons x xs) = apply f x %: mapH f xs

instance (Apply f x (acc -> acc), FoldrH f acc xs) => FoldrH f acc (x ': xs) where
  foldrH f acc (HCons x xs) = apply f x $ foldrH f acc xs

-- type family AllStable (a ': as) 

class AllStable (as :: [Type]) where 

instance AllStable '[] where

instance (Stable a, AllStable as) => AllStable (a ': as) where

type family Wrap (a :: Type) :: Type where
  Wrap a = Value a

data Flatten = Flatten
instance (q ~ HList (Map xs), r ~ HList (Map xs)) => Apply Flatten (HList (Sig a ': xs)) (Sig q -> Sig r) where
  apply _ (HCons x xs) = prepend x -- partially apply prepend, cps

--{-# ANN flattenToSignal AllowRecursion #-}
flattenToSignal :: AllStable as => HList (Map as) -> Sig (HList (Map as))
flattenToSignal xs = foldrH Flatten (singleton' (HNil ::: never)) xs

-- flattenToSignal :: AllStable as => HList (Map Sig as) -> Sig (HList (Map Value as))
-- flattenToSignal (NonEmptyList h Nil) = singleton' h
-- flattenToSignal (NonEmptyList h t)   = prepend h (flattenToSignal' t)