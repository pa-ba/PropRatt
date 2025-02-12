{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
module PropRatt.Current where
import AsyncRattus.Strict

data Current a where
  Current :: Maybe' a -> a -> Current a

latest' :: (Eq a) => Current a -> a
latest' (Current _ latest) = latest

(?=) :: Eq a => Current a -> Current a -> Bool
(Current m1 _) ?= (Current m2 _) =
  case (m1, m2) of
    (Just' x, Just' y) -> x == y
    _ -> False

-- xs :: Sig a =    1   2   3
-- ys :: Sig b =        5   7

-- Sig (Current a * Current b) = (Just 1 :* Nothing) (Just 2 :* Just 5) (Just 3 :* Just 7)