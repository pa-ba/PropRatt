{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module PropRatt.LTL (
    Pred (..),
    evaluateLTL,
    evaluateLTLSig
) where

import AsyncRattus.Signal
import AsyncRattus.InternalPrimitives
import qualified Data.IntSet as IntSet

data Pred a where
    Tautology :: Pred a
    Contradiction :: Pred a
    Atom :: (a -> Bool) -> Pred a
    Not :: Pred a -> Pred a
    And :: Pred a -> Pred a -> Pred a
    Or :: Pred a -> Pred a -> Pred a
    Until :: Pred a -> Pred a -> Pred a
    Next :: Pred a -> Pred a
    Implies :: Pred a -> Pred a -> Pred a
    Always :: Pred a -> Pred a
    Eventually :: Pred a -> Pred a
    After :: Int -> Pred a -> Pred a
    Release :: Pred a -> Pred a -> Pred a

evaluateLTL' :: Int -> Pred a -> [a] -> Bool
evaluateLTL' amountOfPredCap formulae ls@(x:xs) = amountOfPredCap <= 0
    || case formulae of
        Tautology       -> True
        Contradiction   -> False
        Atom phi        -> phi x
        Not phi         -> not (evaluate phi ls)
        And phi psi     -> evaluate phi ls && evaluate psi ls
        Or phi psi      -> evaluate phi ls || evaluate psi ls
        Until phi psi   -> evaluate psi ls || (evaluate phi ls && evaluateNext (phi `Until` psi) xs) -- U (Until)
        Next phi        -> evaluateNext phi xs -- X (Next)
        Implies phi psi -> not (evaluate phi ls && not (evaluate psi ls)) -- (phi: True, psi: False) = False (else True)
        Always phi      -> evaluate phi ls && evaluateNext (Always phi) xs -- G (Globally)
        Eventually phi  -> not (amountOfPredCap == 1 && not (evaluate phi ls)) && (evaluate phi ls || evaluateNext (Eventually phi) xs) -- F (Later)
        Release phi psi -> (evaluate psi ls && evaluate phi ls) || (evaluate psi ls && evaluateNext (phi `Until` psi) xs) -- R (Release)
        After n phi     -> if n <= 0 then evaluate phi ls else evaluateNext (After (n - 1) phi) ls
    where
        evaluateNext = evaluateLTL' (amountOfPredCap - 1)
        evaluate = evaluateLTL' amountOfPredCap

evaluateLTLSig' :: Int -> Pred a -> Sig a -> Bool
evaluateLTLSig' amountOfPredCap formulae ls@(x ::: Delay cl f) = amountOfPredCap <= 0
    || case formulae of
        Tautology       -> True
        Contradiction   -> False
        Atom phi        -> phi x
        Not phi         -> not (evaluate phi ls)
        And phi psi     -> evaluate phi ls && evaluate psi ls
        Or phi psi      -> evaluate phi ls || evaluate psi ls
        Until phi psi   -> evaluate psi ls || (evaluate phi ls && evaluateNext (phi `Until` psi) (f (InputValue (smallest cl) ()))) -- U (Until)
        Next phi        -> evaluateNext phi (f (InputValue (smallest cl) ())) -- X (Next)
        Implies phi psi -> not (evaluate phi ls && not (evaluate psi ls)) -- (phi: True, psi: False) = False (else True)
        Always phi      -> evaluate phi ls && evaluateNext (Always phi) (f (InputValue (smallest cl) ())) -- G (Globally)
        Eventually phi  -> not (amountOfPredCap == 1 && not (evaluate phi ls)) && (evaluate phi ls || evaluateNext (Eventually phi) (f (InputValue (smallest cl) ()))) -- F (Later)
        Release phi psi -> (evaluate psi ls && evaluate phi ls) || (evaluate psi ls && evaluateNext (phi `Until` psi) (f (InputValue (smallest cl) ()))) -- R (Release)
        After n phi     -> if n <= 0 then evaluate phi ls else evaluateNext (After (n - 1) phi) ls
    where
        evaluateNext = evaluateLTLSig' (amountOfPredCap - 1)
        evaluate = evaluateLTLSig' amountOfPredCap
        smallest = IntSet.findMin

evaluateLTL :: Pred a -> [a] -> Bool
evaluateLTL = evaluateLTL' 5

evaluateLTLSig :: Pred a -> Sig a -> Bool
evaluateLTLSig = evaluateLTLSig' 10