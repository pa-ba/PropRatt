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

aRatSwitch :: Sig a -> O(Sig a) -> Sig a
aRatSwitch a o = switch a o

aRatParallel :: Sig a -> Sig b -> Sig (Maybe' a :* Maybe' b)
aRatParallel a b = parallel a b

-- singleton' :: forall a xs. (MapH ToValue xs (Map xs)) => Sig a -> Sig (HList (Map (Value a ': xs)))
-- singleton' xs = map (box (\x -> mapH ToValue (x %: HNil))) xs

prepend :: (Stable a) => Sig a -> Sig (WrappedMap (Value a ': xs)) -> Sig (WrappedMap (Value a ': xs))
prepend (x ::: xs) (y ::: ys) =
   (Current (Just' x) x %: y) ::: prependAwait x xs y ys

-- prependAwait :: forall a xs. (MapH ToValue xs (Map xs), Stable a) => a -> O (Sig a) -> HList (Value a ': xs) -> O (Sig (HList (Value a ': xs))) -> O (Sig (HList (Map (Value a ': xs))))
-- prependAwait x xs y ys  = delay (
--   case select xs ys of
--      Fst (x' ::: xs')   ys'         -> (Current (Just' x') x' %: mapH (\(Current _ x) -> Current Nothing' x) y) ::: prependAwait x' xs' y ys'
--      Snd xs' (y' ::: ys')           -> (Current Nothing' x %: y') ::: prependAwait x xs' y' ys'
--      Both (x' ::: xs') (y' ::: ys') -> (Current (Just' x') x' %: y')  ::: prependAwait x' xs' y' ys')

-- toValue :: a -> Value a
-- toValue a = Currrent (Just' a) a

data Pretty = Pretty
instance Show x => Apply Pretty x String where
  apply _ x = show x

data Concat = Concat
instance Apply Concat String (String -> String) where
  apply _ x = \y -> x ++ ", " ++ y

type family ConstMap (t :: *) (xs :: [*]) :: [*] where
  ConstMap _      '[]  = '[]
  ConstMap t (x ': xs) = t ': (ConstMap t xs)


prettyHList :: forall xs.
               ( MapH   Pretty xs     (ConstMap String xs)
               , FoldrH Concat String (ConstMap String xs)
               ) => HList xs -> String
prettyHList hlist = "[" ++ (foldrH Concat "" $ mapH @_ @_ @(ConstMap String xs) Pretty hlist) ++ "]"

-- Defunctionalization notes
-- https://www.geeksforgeeks.org/what-is-defunctionalization/


-- what
-- Defunctionalization is a compile-time conversion which removes higher-order functions by replacing the higher-order functions with a single first-order apply function. 
-- idea: convert higher order functions (a -> b -> c) into its first order representation
-- quick recap: what is a higher order function? HOF's means we can pass functions as arguments to other functions
-- we also say functions are first-class citizens because we pass them around as if they are values

-- Why
-- haskells type system does not support first-class functions. this is not supported: function :: (Type -> Type) -> (Type)
-- we need this functionality because the map function is a higher order function: map :: (Type -> Type) -> [Type] -> [Type]

-- instead of passing type level functions around, we represent them as data
-- then we can apply them with type families
-- to achieve this, we need multi parameter typeclasses. ie typeclases using more than one type variable
class Apply f a b where -- the typechecker checks that an instance of apply exists for the type variables before applying it.
  apply :: f -> a -> b

data Valify = Valify -- this is known as a symbol function. The "ToValue" constructor symbolizes the function
instance Apply Valify a (Value a) where
  apply _ x = Current (Just' x) x


-- The type family allows us to interpret the defunctionlizaed function by pattern matching on the data constructor to produce a result
-- This allows us to write higher order functions at the type level


-- Map takes a list of types
-- then we pattern match on the list of types, and map these to other types.
-- in this case, we map HList of with head of type Sig a to Value a recursively
type family Map (xs :: [Type]) :: [Type] where
  Map '[] = '[]
  Map (Sig a ': xs) = Value a ': Map xs

-- Why do we need this type family? Because the return type of intermediate mappings is not clear from mapH

-- EXAMPLES
-- 1) normal map and list: map :: (a -> b) -> [a] -> [b]
-- map (\a -> if a == 0 then False else True) [1,2,3] = [True, False, False]

-- 2) using HList and mapH: mapH :: (Type -> Type) -> [Type] -> [Type]
-- map (\a -> if a == 0 then False else True) [1,False,"Hello"]
-- How is the compiler supposed to know the return type of mapH? After each function applicaiton, the return type changes
-- we need to guide the compiler to promise that for all list of types, there exists an instance of apply


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

newtype WrappedMap xs = WrappedMap (HList (Map xs))

-- symbol function to collapse
data Flatten = Flatten
instance Apply Flatten (HList (Sig a ': xs)) (Sig (WrappedMap (Value a ': xs)) -> Sig (WrappedMap (Value a ': xs))) where
  apply _ (HCons x xs) = prepend x




-- {-# ANN flattenToSignal' AllowRecursion #-}
--  forall xs. (MapH ToValue xs (Map xs)) constraint
-- use forall as an existential type variable qualifier. ie. "there exists"
-- promise the compiler:
-- forall xs: when you apply ToValue xs, there exists a function from xs to another type

-- the constraint ensures xs can be mapped over
-- MapH f xs ys
-- MapH ToValue xs (Map xs), ie the return type ys is defined by (Map xs)
-- https://wasp.sh/blog/2021/09/01/haskell-forall-tutorial
-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/explicit_forall.html
-- flattenToSignal' :: forall a xs. 
--    (  MapH ToValue xs (Map xs),
--       FoldrH Flatten (Sig (HList (Map xs))) (Sig a ': xs),
--       Stable a
--    ) => HList (Sig a ': xs) -> Sig (HList (Map (Value a ': xs)))
-- flattenToSignal' xs = foldrH Flatten (singleton' (HNil ::: never)) xs


-- flattenToSignal :: (Stable a) => NonEmptyList (Sig a) -> Sig (HList (Value a))
-- flattenToSignal (NonEmptyList h Nil) = singleton' h
-- flattenToSignal (NonEmptyList h t)   = prepend h (flattenToSignal' t)