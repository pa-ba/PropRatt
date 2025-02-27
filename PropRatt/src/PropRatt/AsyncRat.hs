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

data Valify = Valify
instance Apply Valify a (Value a) where
  apply _ x = Current (Just' x) x


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


class AllStable (as :: [Type]) where 

instance AllStable '[] where

instance (Stable a, AllStable as) => AllStable (a ': as) where



singleton' :: (Stable a) => Sig a -> Sig (HList (Map Value '[a])) -- (Sig (HList (Map Value xs))) - Gives a type error
singleton' xs = map (box (\p -> (Current (Just' p) p) %: HNil)) xs

-- Flatten as a recursive function
flattenToSignal' :: AllStable as => HList (Map Sig as) -> Sig (HList (Map Value as))
flattenToSignal' HNil = undefined
flattenToSignal' (HCons head HNil) = singleton' head
flattenToSignal' (HCons head tail) = prepend head (flattenToSignal' tail) 

-- Gives error: "could not deduce ‘x ~ Sig a3’ from the context: Map Sig xs ~ (x : xs4)" on "head" and "tail"
flattenToSignal :: AllStable as => HList (Map Sig as) -> Sig (HList (Map Value as))
flattenToSignal (HCons head tail) = prepend head (flattenToSignal' tail) 

prepend :: (Stable a, AllStable as) => Sig a -> Sig (HList (Map Value as)) -> Sig (HList (Map Value (a ': as)))
prepend (x ::: xs) (y ::: ys) = 
  (HCons (Current (Just' x) x) y) ::: never --prependAwait x xs y ys

-- Outcommented prependAwait. We need "flattenToSignal" and "prepend" to work first

-- prependAwait :: (Stable a, AllStable as) => a -> O (Sig a) -> HList (Map Value as) -> O (Sig (HList (Map Value as))) -> O (Sig (HList (Map Value (a ': as))))
-- prependAwait x xs y ys  = delay (
--   case select xs ys of
--      Fst (x' ::: xs')   ys'         -> (Current (Just' x') x' %: mapH (\(Current _ x) -> Current Nothing' x) y) ::: prependAwait x' xs' y ys'
--      Snd xs' (y' ::: ys')           -> (Current Nothing' x %: y') ::: prependAwait x xs' y' ys'
--      Both (x' ::: xs') (y' ::: ys') -> (Current (Just' x') x' %: y') ::: prependAwait x' xs' y' ys')
