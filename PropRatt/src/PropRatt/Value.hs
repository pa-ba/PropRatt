{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module PropRatt.Value (Value(..), makeNothings) where
import AsyncRattus.Strict
import AsyncRattus.Signal hiding (current)
import PropRatt.Utilities

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

makeNothings :: List (Value a) -> List (Value a)
makeNothings = map' (\(Current _ x) -> Current Nothing' x)

(?=) :: Eq a => Value a -> Value a -> Bool
a ?= b = current' a == current' b