{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PropRatt.Value where
import AsyncRattus.Strict
import AsyncRattus.Signal hiding (current)
import PropRatt.Utilities
import Data.Kind ( Type )

data HList :: [Type] -> Type where
  HNil :: HList '[]
  HCons :: !x -> !(HList xs) -> HList (x ': xs)
  
instance Show (HList '[]) where
  show HNil = "HNil"

list :: HList [Integer, String]
list = HCons 5 (HCons "Hej" (HNil))

infixr 5 %:
(%:) :: x -> HList xs -> HList (x ': xs)
(%:) = HCons

instance (Show x, (Show (HList xs))) => Show (HList (x ': xs)) where
  show (HCons x xs) = show x ++ " %: " ++ show xs 

class Apply f a b where
  apply :: f -> a -> b

class MapH f xs ys where
  mapH :: f -> HList xs -> HList ys
  
instance (Apply f x y, MapH f xs ys) => MapH f (x ': xs) (y ': ys) where
  mapH f (HCons x xs) = apply f x %: mapH f xs

data Value a where
  Current :: !(Maybe' a) -> !a -> Value a

instance Show a => Show (Value a) where
  show (Current m y) =
    case m of
      Just' x  -> "J:" ++ show x ++ ",L:" ++ show y
      Nothing' -> "N,L:" ++ show y

instance Show a => Show (Sig [Value a]) where
  show sig = "Sig [Value a]: " ++ show (takeSigExhaustive sig) ++ "..."

current' :: Value a -> a
current' (Current (Just' x) _) = x
current' (Current Nothing' x) = x

(?=) :: Eq a => Value a -> Value a -> Bool
a ?= b = current' a == current' b