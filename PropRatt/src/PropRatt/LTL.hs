{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module PropRatt.LTL
  ( Pred (..),
    evaluateLTL,
    evaluateTupleSig
  )
where

import AsyncRattus.InternalPrimitives
import AsyncRattus.Signal
import qualified Data.IntSet as IntSet

data Pred a where
  Tautology :: Pred a
  Contradiction :: Pred a
  Atom :: (a -> Bool) -> Pred a
  Atom2 :: (a -> b -> Bool) -> Pred a
  FST :: a
  SND :: a
  Not :: Pred a -> Pred a
  And :: Pred a -> Pred a -> Pred a
  Or :: Pred a -> Pred a -> Pred a
  Until :: Pred a -> Pred a -> Pred a
  Next :: Pred a -> Pred a
  Implies :: Pred a -> Pred a -> Pred a
  Always :: Pred a -> Pred a
  Eventually :: Pred a -> Pred a -- (Always True)
  After :: Int -> Pred a -> Pred a
  Release :: Pred a -> Pred a -> Pred a

evaluateLTL' :: Int -> Pred a -> [a] -> Bool
evaluateLTL' amountOfPredCap formulae ls@(x : xs) =
  amountOfPredCap <= 0
    || case formulae of
      Atom2 phi -> False
      Tautology -> True
      Contradiction -> False
      Atom phi -> phi x
      Not phi -> not (evaluate phi ls)
      And phi psi -> evaluate phi ls && evaluate psi ls
      Or phi psi -> evaluate phi ls || evaluate psi ls
      Until phi psi -> evaluate psi ls || (evaluate phi ls && evaluateNext (phi `Until` psi) xs) -- U (Until)
      Next phi -> evaluateNext phi xs -- X (Next)
      Implies phi psi -> not (evaluate phi ls && not (evaluate psi ls)) -- (phi: True, psi: False) = False (else True)
      Always phi -> evaluate phi ls && evaluateNext (Always phi) xs -- G (Globally)
      Eventually phi -> evaluate phi ls || evaluateNext (Eventually phi) xs -- F (Later) -- (Always True)
      Release phi psi -> (evaluate psi ls && evaluate phi ls) || (evaluate psi ls && evaluateNext (phi `Until` psi) xs) -- R (Release)
      After n phi -> if n <= 0 then evaluate phi ls else evaluateNext (After (n - 1) phi) ls
  where
    evaluateNext = evaluateLTL' (amountOfPredCap - 1)
    evaluate = evaluateLTL' amountOfPredCap

evaluateLTLSig' :: Int -> Pred a -> Sig a -> Bool
evaluateLTLSig' amountOfPredCap formulae sig@(x ::: Delay cl f) =
  amountOfPredCap <= 0
    || case formulae of
      Atom2 phi -> False
      Tautology -> True
      Contradiction -> False
      Atom phi -> phi x
      Not phi -> not (evaluate phi sig)
      And phi psi -> evaluate phi sig && evaluate psi sig
      Or phi psi -> evaluate phi sig || evaluate psi sig
      Until phi psi -> evaluate psi sig || (evaluate phi sig && evaluateNext (phi `Until` psi) (f (InputValue (smallest cl) ()))) -- U (Until)
      Next phi -> evaluateNext phi (f (InputValue (smallest cl) ())) -- X (Next)
      Implies phi psi -> not (evaluate phi sig && not (evaluate psi sig)) -- (phi: True, psi: Fasige) = Fasige (esige True)
      Always phi -> evaluate phi sig && evaluateNext (Always phi) (f (InputValue (smallest cl) ())) -- G (Globally)
      Eventually phi -> not (amountOfPredCap == 1 && not (evaluate phi sig)) && (evaluate phi sig || evaluateNext (Eventually phi) (f (InputValue (smallest cl) ()))) -- F (Later)
      Release phi psi -> (evaluate psi sig && evaluate phi sig) || (evaluate psi sig && evaluateNext (phi `Until` psi) (f (InputValue (smallest cl) ()))) -- R (Release)
      After n phi -> if n <= 0 then evaluate phi sig else evaluateNext (After (n - 1) phi) sig
  where
    evaluateNext = evaluateLTLSig' (amountOfPredCap - 1)
    evaluate = evaluateLTLSig' amountOfPredCap
    smallest = IntSet.findMin


-- Maybe': Refac Atom to take 2 input values, to evaluate the two tupleSignal values

evaluateTupleSig' :: Int -> Pred a -> Sig a -> Bool
evaluateTupleSig' amountOfPredCap formulae tupleSig@((maybes@(ma :* mb) :* latest@(la :* lb)) ::: Delay clxy fxy) =
  amountOfPredCap <= 0 || case formulae of 
        Tautology       -> True
        Contradiction   -> False

        -- FST             -> case ma of
        --     (Just' _) -> ma
        --     (Nothing' _) -> la
        -- SND             -> case mb of
        --     (Just' _) -> mb
        --     (Nothing' _) -> lb

        Atom2 phi       -> case maybes of
            (Just' q, Just' w) -> phi q w 
            (Nothing' q, Just' w) -> phi z w
            (Just' q, Nothing' w) -> phi q r
            (Nothing' q, Nothing' w) -> phi z r
        -- Not phi         -> not (evaluate phi sig)
        -- And phi psi     -> evaluate phi sig && evaluate psi sig
        -- Or phi psi      -> evaluate phi sig || evaluate psi sig
        -- Until phi psi   -> evaluate psi sig || (evaluate phi sig && evaluateNext (phi `Until` psi) (f (InputValue (smallest cl) ()))) -- U (Until)
        -- Next phi        -> evaluateNext phi (f (InputValue (smallest cl) ())) -- X (Next)
        -- Implies phi psi -> not (evaluate phi sig && not (evaluate psi sig)) -- (phi: True, psi: Fasige) = Fasige (esige True)
        -- Always phi      -> evaluate phi sig && evaluateNext (Always phi) (f (InputValue (smallest cl) ())) -- G (Globally)
        -- Eventually phi  -> not (amountOfPredCap == 1 && not (evaluate phi sig)) && (evaluate phi sig || evaluateNext (Eventually phi) (f (InputValue (smallest cl) ()))) -- F (Later)
        -- Release phi psi -> (evaluate psi sig && evaluate phi sig) || (evaluate psi sig && evaluateNext (phi `Until` psi) (f (InputValue (smallest cl) ()))) -- R (Release)
        -- After n phi     -> if n <= 0 then evaluate phi sig else evaluateNext (After (n - 1) phi) sig

evaluateLTL :: Pred a -> [a] -> Bool
evaluateLTL = evaluateLTL' 5

evaluateLTLSig :: Pred a -> Sig a -> Bool
evaluateLTLSig = evaluateLTLSig' 10

evaluateTupleSig :: Pred a -> Sig a -> Sig b -> Bool
evaluateTupleSig pred a b = evaluateTupleSig' 3 pred (aRatParallel a b)
