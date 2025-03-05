{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE GADTs, DataKinds, KindSignatures, MultiParamTypeClasses, RankNTypes #-}



module PropRatt.LTL
  ( Pred (..),
    evaluateLTLSigs
  )
where

import AsyncRattus.InternalPrimitives
import AsyncRattus.Signal
import AsyncRattus.Strict
import qualified Data.IntSet as IntSet
import PropRatt.Value
import PropRatt.AsyncRat

data Pred (sigs :: [*]) (vals :: [*]) where
  Tautology    :: Pred sigs vals
  Contradiction :: Pred sigs vals
  Now         :: (Flatten sigs vals) => (HList vals -> Bool) -> Pred sigs vals
  Not         :: Pred sigs vals -> Pred sigs vals
  And         :: Pred sigs vals -> Pred sigs vals -> Pred sigs vals
  Or          :: Pred sigs vals -> Pred sigs vals -> Pred sigs vals
  Until       :: Pred sigs vals -> Pred sigs vals -> Pred sigs vals
  Next        :: Pred sigs vals -> Pred sigs vals
  Implies     :: Pred sigs vals -> Pred sigs vals -> Pred sigs vals
  Always      :: Pred sigs vals -> Pred sigs vals
  Eventually  :: Pred sigs vals -> Pred sigs vals
  After       :: Int -> Pred sigs vals -> Pred sigs vals
  Release     :: Pred sigs vals -> Pred sigs vals -> Pred sigs vals


evaluateLTLSigs' :: forall (sigs :: [*]) (vals :: [*]). Int -> Pred sigs vals -> Sig (HList vals) -> Bool
evaluateLTLSigs' amountOfPredCap formulae sig@(x ::: Delay cl f) =
  amountOfPredCap <= 0
    || case formulae of
         Tautology       -> True
         Contradiction   -> False
         Now phi         -> phi x
         Not phi         -> not (evaluate phi sig)
         And phi psi     -> evaluate phi sig && evaluate psi sig
         Or phi psi      -> evaluate phi sig || evaluate psi sig
         Until phi psi   -> evaluate psi sig 
                           || (evaluate phi sig && evaluateNext (phi `Until` psi) (f (InputValue (smallest cl) ())))
         Next phi        -> evaluateNext phi (f (InputValue (smallest cl) ()))
         Implies phi psi -> not (evaluate phi sig && not (evaluate psi sig))
         Always phi      -> evaluate phi sig && evaluateNext (Always phi) (f (InputValue (smallest cl) ()))
         Eventually phi  -> (evaluate phi sig || evaluateNext (Eventually phi) (f (InputValue (smallest cl) ())))
                             && not (amountOfPredCap == 1 && not (evaluate phi sig))
         Release phi psi -> (evaluate psi sig && evaluate phi sig) 
                           || (evaluate psi sig && evaluateNext (phi `Until` psi) (f (InputValue (smallest cl) ())))
         After n phi     -> if n <= 0 then evaluate phi sig else evaluateNext (After (n - 1) phi) sig
  where
    evaluateNext = evaluateLTLSigs' (amountOfPredCap - 1)
    evaluate = evaluateLTLSigs' amountOfPredCap
    smallest = IntSet.findMin

evaluateLTLSigs :: forall (sigs :: [*]) (vals :: [*]). Pred sigs vals -> Sig (HList vals) -> Bool
evaluateLTLSigs = evaluateLTLSigs' 20