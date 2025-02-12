{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module PropRatt.LTL
  ( Pred (..),
    Pred2 (..),
    evaluateLTL,
    evaluateTupleSig,
  )
where

import AsyncRattus.InternalPrimitives
import AsyncRattus.Signal
import AsyncRattus.Strict
import qualified Data.IntSet as IntSet
import PropRatt.Utilities (getLater)
import PropRatt.AsyncRat (aRatParallel, mkCurrentSig, mkCurrentSigSingle)
import PropRatt.Current

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
  Eventually :: Pred a -> Pred a -- (Always True)
  After :: Int -> Pred a -> Pred a
  Release :: Pred a -> Pred a -> Pred a

data Pred2 a b where
  Tautology2 :: Pred2 a b
  Contradiction2 :: Pred2 a b
  Atom2 :: ((Current a :* Current b) -> Current a -> Current b -> Bool) -> Pred2 a b
  Not2 :: Pred2 a b -> Pred2 a b
  And2 :: Pred2 a b -> Pred2  a b -> Pred2  a b
  Or2 :: Pred2  a b -> Pred2  a b -> Pred2  a b
  Until2 :: Pred2 a b -> Pred2 a b -> Pred2  a b
  Next2 :: Pred2  a b -> Pred2  a b
  Implies2 :: Pred2  a b -> Pred2  a b -> Pred2  a b
  Always2 :: Pred2  a b -> Pred2  a b
  Eventually2 :: Pred2  a b -> Pred2  a b -- (Always True)
  After2 :: Int -> Pred2  a b -> Pred2  a b
  Release2 :: Pred2  a b -> Pred2  a b -> Pred2  a b

evaluateLTL' :: Int -> Pred a -> [a] -> Bool
evaluateLTL' amountOfPredCap formulae ls@(x : xs) =
  amountOfPredCap <= 0
    || case formulae of
      Tautology -> True
      Contradiction -> False
      Atom phi -> phi x
      Not phi -> not (evaluate phi ls)
      And phi psi -> evaluate phi ls && evaluate psi ls
      Or phi psi -> evaluate phi ls || evaluate psi ls
      Until phi psi -> evaluate psi ls || evaluate phi ls && evaluateNext (phi `Until` psi) xs -- U (Until)
      Next phi -> evaluateNext phi xs -- X (Next)
      Implies phi psi -> not (evaluate phi ls && not (evaluate psi ls)) -- (phi: True, psi: False) = False (else True)
      Always phi -> evaluate phi ls && evaluateNext (Always phi) xs -- G (Globally)
      Eventually phi -> evaluate phi ls || evaluateNext (Eventually phi) xs -- F (Later) -- (Always True)
      Release phi psi -> evaluate psi ls && evaluate phi ls || evaluate psi ls && evaluateNext (phi `Until` psi) xs -- R (Release)
      After n phi -> if n <= 0 then evaluate phi ls else evaluateNext (After (n - 1) phi) ls
  where
    evaluateNext = evaluateLTL' (amountOfPredCap - 1)
    evaluate = evaluateLTL' amountOfPredCap

evaluateLTLSig' :: Int -> Pred a -> Sig a -> Bool
evaluateLTLSig' amountOfPredCap formulae sig@(x ::: Delay cl f) =
  amountOfPredCap <= 0
    || case formulae of
      Tautology -> True
      Contradiction -> False
      Atom phi -> phi x
      Not phi -> not (evaluate phi sig)
      And phi psi -> evaluate phi sig && evaluate psi sig
      Or phi psi -> evaluate phi sig || evaluate psi sig
      Until phi psi -> evaluate psi sig || evaluate phi sig && evaluateNext (phi `Until` psi) (f (InputValue (smallest cl) ())) -- U (Until)
      Next phi -> evaluateNext phi (f (InputValue (smallest cl) ())) -- X (Next)
      Implies phi psi -> not (evaluate phi sig && not (evaluate psi sig)) -- (phi: True, psi: Fasige) = Fasige (esige True)
      Always phi -> evaluate phi sig && evaluateNext (Always phi) (f (InputValue (smallest cl) ())) -- G (Globally)
      Eventually phi -> not (amountOfPredCap == 1 && not (evaluate phi sig)) && (evaluate phi sig || evaluateNext (Eventually phi) (f (InputValue (smallest cl) ()))) -- F (Later)
      Release phi psi -> evaluate psi sig && evaluate phi sig || evaluate psi sig && evaluateNext (phi `Until` psi) (f (InputValue (smallest cl) ())) -- R (Release)
      After n phi -> if n <= 0 then evaluate phi sig else evaluateNext (After (n - 1) phi) sig
  where
    evaluateNext = evaluateLTLSig' (amountOfPredCap - 1)
    evaluate = evaluateLTLSig' amountOfPredCap
    smallest = IntSet.findMin

evaluateTupleSig' :: (Stable a, Stable b) => Int -> Pred2 a b -> Sig (Current a :* Current b) -> Sig (Current a) -> Sig (Current b) -> Bool
evaluateTupleSig' amountOfPredCap formulae siga@(a ::: Delay cla fa) sigb@(b ::: Delay clb fb) sigc@(c ::: Delay clc fc) =
  amountOfPredCap <= 0 || case formulae of
        Tautology2       -> True
        Contradiction2   -> False
        Atom2 phi        -> phi a b c
        Not2 phi         -> not (evaluate phi siga sigb sigc)
        And2 phi psi     -> evaluate phi siga sigb sigc && evaluate psi siga sigb sigc
        Or2 phi psi      -> evaluate phi siga sigb sigc || evaluate psi siga sigb sigc
        Until2 phi psi   -> evaluate psi siga sigb sigc || evaluate phi siga sigb sigc && evaluateNext (phi `Until2` psi) advA advB advC -- U (Until)
        Next2 phi        -> evaluateNext phi advA advB advC -- X (Next)
        Implies2 phi psi -> not (evaluate phi siga sigb sigc && not (evaluate psi siga sigb sigc))
        Always2 phi      -> evaluate phi siga sigb sigc && evaluateNext (Always2 phi) advA advB advC -- G (Globally)
        Eventually2 phi  -> not (amountOfPredCap == 1 && not (evaluate phi siga sigb sigc)) && (evaluate phi siga sigb sigc || evaluateNext (Eventually2 phi) advA advB advC)
        Release2 phi psi -> evaluate psi siga sigb sigc && evaluate phi siga sigb sigc || evaluate psi siga sigb sigc && evaluateNext (phi `Until2` psi) advA advB advC
        After2 n phi     -> if n <= 0 then evaluate phi siga sigb sigc else evaluateNext (After2 (n - 1) phi) advA advB advC
    where
        evaluateNext = evaluateTupleSig' (amountOfPredCap - 1)
        evaluate = evaluateTupleSig' amountOfPredCap
        smallest = IntSet.findMin (IntSet.unions [cla, clb, clc])
        advA = tickIfMember siga smallest
        advB = tickIfMember sigb smallest
        advC = tickIfMember sigc smallest

-- Is this nescessary? What happens if you supply a channelId to an (InputValue -> a) function? Ie. advance on a clock that doesn't have 

tickIfMember :: Sig a -> Int -> Sig a
tickIfMember sig@(a ::: Delay cl f) id = if id `channelMember` cl then f (InputValue id ()) else sig

-- Example 1:

-- s1 = zip xs ys
-- s2 = xs
-- s3 = ys

-- G (fst (current s1) = current s2 \/ snd (current s1) = current s3)

-- Example 2:

-- s1 = switch xs ys cl (1,2,3)
-- s2 = xs cl (2,3)
-- s3 = 0 ::: ys cl (1)

-- Until (current s1 = current s2) (current s1 = current ys)


evaluateLTL :: Pred a -> [a] -> Bool
evaluateLTL = evaluateLTL' 4

evaluateLTLSig :: (Stable a) => Pred a -> Sig a -> Bool
evaluateLTLSig = evaluateLTLSig' 10

evaluateTupleSig :: (Stable a, Stable b) => Pred2 a b -> Sig a -> Sig b -> Bool
evaluateTupleSig pred a b = evaluateTupleSig' 3 pred (mkCurrentSig a b) (mkCurrentSigSingle a) (mkCurrentSigSingle b)
