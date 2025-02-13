{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module PropRatt.Value (Value(..)) where
import AsyncRattus.Strict
import AsyncRattus.Signal
import PropRatt.Utilities

data Value a where
  Current :: Maybe' a -> a -> Value a

instance Show a => Show (Value a) where
  show (Current m y) =
    case m of
      Just' x  -> "Current: Just' " ++ show x ++ ", Latest: " ++ show y
      Nothing' -> "Current: None, Latest: " ++ show y

instance Show a => Show (Sig [Value a]) where
  show sig = "Sig [Value a]: " ++ show (takeSigExhaustive sig) ++ "..."

-- latest' :: (Eq a) => Current a -> a
-- latest' (Current _ latest) = latest

-- (?=) :: Eq a => Current a -> Current a -> Bool
-- (Current m1 _) ?= (Current m2 _) =
--   case (m1, m2) of
--     (Just' x, Just' y) -> x == y
--     _ -> False