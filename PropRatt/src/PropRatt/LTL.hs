{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE GADTs, DataKinds, KindSignatures, MultiParamTypeClasses, RankNTypes #-}

module PropRatt.LTL
  ( Pred (..),
    evaluate
  )
where

import AsyncRattus.InternalPrimitives
import AsyncRattus.Signal
import qualified Data.IntSet as IntSet
import PropRatt.AsyncRat
import Data.Kind

data Pred (v :: [Type]) where
  Tautology    :: Pred v
  Contradiction :: Pred v
  Now         :: (HList v -> Bool) -> Pred v
  Not         :: Pred v -> Pred v
  And         :: Pred v -> Pred v -> Pred v
  Or          :: Pred v -> Pred v -> Pred v
  Until       :: Pred v -> Pred v -> Pred v
  Next        :: Pred v -> Pred v
  Implies     :: Pred v -> Pred v -> Pred v
  Always      :: Pred v -> Pred v
  Eventually  :: Pred v -> Pred v
  After       :: Int -> Pred v -> Pred v
  Release     :: Pred v -> Pred v -> Pred v

evaluate' :: Int -> Pred v -> Sig (HList v) -> Bool
evaluate' timestepsLeft formulae sig@(x ::: Delay cl f) =
  timestepsLeft <= 0
    || case formulae of
         Tautology       -> True
         Contradiction   -> False
         Now phi         -> phi x
         Not phi         -> not (eval phi sig)
         And phi psi     -> eval phi sig && eval psi sig
         Or phi psi      -> eval phi sig || eval psi sig
         Until phi psi   -> eval psi sig
                            || (eval phi sig && evaluateNext (phi `Until` psi) advance)
         Next phi        -> evaluateNext phi advance
         Implies phi psi -> not (eval phi sig && not (eval psi sig))
         Always phi      -> eval phi sig && evaluateNext (Always phi) advance
         Eventually phi  -> (eval phi sig || evaluateNext (Eventually phi) advance)
                            && not (timestepsLeft == 1 && not (eval phi sig))
         Release phi psi -> (eval psi sig && eval phi sig)
                            || (eval psi sig && evaluateNext (phi `Until` psi) advance)
         After n phi     -> if n <= 0 then eval phi sig else evaluateNext (After (n - 1) phi) sig
  where
    evaluateNext = evaluate' (timestepsLeft - 1)
    eval = evaluate' timestepsLeft
    smallest = IntSet.findMin
    advance = f (InputValue (smallest cl) ())

evaluate :: Pred v -> Sig (HList v) -> Bool
evaluate = evaluate' 20