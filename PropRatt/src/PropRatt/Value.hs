{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables, UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

module PropRatt.Value where
import AsyncRattus.Strict
import AsyncRattus.Signal hiding (current)
import PropRatt.Utilities
import AsyncRattus

instance Stable (Value a) where 
instance Stable Int where 


newtype HasTicked = HasTicked Bool deriving Show

-- make value a functor?
data Value a where
  Current :: !HasTicked -> !(List a) -> Value a

instance Num a => Num (Value a) where
  (+) v1 v2 = pureVal (current v1 + current v2)
  (-) v1 v2 = pureVal (current v1 - current v2)
  (*) v1 v2 = pureVal (current v1 * current v2)
  negate v  = pureVal (negate (current v))
  abs v     = pureVal (abs (current v))
  signum v  = pureVal (signum (current v))
  fromInteger n = pureVal (fromInteger n)

pureVal :: a -> Value a
pureVal x = Current (HasTicked False) (x :! Nil)

instance Show a => Show (Value a) where
  show (Current t Nil) = show t
  show (Current t (h :! Nil)) = show t ++ "  " ++ show h
  show (Current t (h :! h2 :! _)) = show t ++ "  " ++ show h ++ "  " ++ show h2

instance Show a => Show (Sig [Value a]) where
  show sig = "Sig [Value a]: " ++ show (takeSigExhaustive sig) ++ "..."

instance Ord a => Ord (Value a) where
  compare v1 v2 = compare (current v1) (current v2)

instance Eq a => Eq (Value a) where
  v1 == v2 = current v1 == current v2

current :: Value a -> a
current (Current _ (h :! _)) = h
current _ = undefined

previous :: Value a -> a
previous (Current _ (_ :! Nil)) = undefined
previous (Current _ Nil) = undefined
previous (Current _ (_ :! y :! _)) = y

past :: Int -> Value a -> a
past _ (Current _ Nil) = undefined
past 0 (Current _ (h :! _)) = h 
past n (Current a (_ :! t)) = past (n-1) (Current a t)