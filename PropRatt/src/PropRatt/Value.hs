{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables, UndecidableInstances #-}

module PropRatt.Value where
import AsyncRattus.Strict
import AsyncRattus.Signal hiding (current)
import PropRatt.Utilities
  
newtype HasTicked = HasTicked Bool deriving Show

data Value a where
  Current :: !HasTicked -> !(List a) -> Value a

instance Show a => Show (Value a) where
  show (Current t h) = show t ++ show h

instance Show a => Show (Sig [Value a]) where
  show sig = "Sig [Value a]: " ++ show (takeSigExhaustive sig) ++ "..."

instance Ord a => Ord (Value a) where 
  (Current b1 (a1 :! xs)) `compare` (Current b2 (a2 :! ys)) = a1 `compare` a2

instance Eq a => Eq (Value a) where
  (Current _ Nil) == (Current _ Nil) = True
  (Current _ Nil) == _ = False
  _ == (Current _ Nil) = False
  (Current b1 (a1 :! xs)) == (Current b2 (a2 :! ys)) = a1 == a2


current' :: Value a -> a
current' (Current _ (h :! _)) = h
current' _ = undefined

previous :: Value a -> a
previous (Current _ (_ :! Nil)) = undefined
previous (Current _ Nil) = undefined
previous (Current _ (_ :! y :! _)) = y

past :: Int -> Value a -> a
past _ (Current _ Nil) = undefined
past 0 (Current _ (h :! _)) = h 
past n (Current a (_ :! t)) = past (n-1) (Current a t)

(?=) :: Eq a => Value a -> Value a -> Bool
a ?= b = current' a == current' b