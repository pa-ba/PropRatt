{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE GADTs, DataKinds, KindSignatures, MultiParamTypeClasses, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE TypeOperators #-}

module PropRatt.LTL
  ( Pred (..),
    evaluate,
    Atom (..),
    Lookup (..),
    SafetyError,
    SafetyPred
  )
where

import AsyncRattus.InternalPrimitives
import AsyncRattus.Strict
import AsyncRattus.Signal hiding (const)
import qualified Data.IntSet as IntSet
import PropRatt.AsyncRat
import Data.Kind
import PropRatt.Value

data Pred (v :: [Type]) a where
  Tautology     :: Pred v a
  Contradiction :: Pred v a
  Now           :: Atom v a -> Pred v a
  Not           :: Pred v a -> Pred v a
  And           :: Pred v a -> Pred v a -> Pred v a
  Or            :: Pred v a -> Pred v a -> Pred v a
  Until         :: Pred v a -> Pred v a -> Pred v a
  Next          :: Pred v a -> Pred v a
  Implies       :: Pred v a -> Pred v a -> Pred v a
  Always        :: Pred v a -> Pred v a
  Eventually    :: Pred v a -> Pred v a
  After         :: Int -> Pred v a -> Pred v a
  Release       :: Pred v a -> Pred v a -> Pred v a 
  
data Atom (v :: [Type]) a where
  Equals :: Lookup v a -> Lookup v a -> Atom v a 
  LargerThan :: Lookup v a -> Lookup v a -> Atom v a
  GreaterThan :: Lookup v a -> Lookup v a -> Atom v a

data Lookup (v :: [Type]) a where 
  Const    :: a -> Lookup v a
  Previous :: Lookup v a -> Lookup v a
  First :: Lookup (Value a ': x) a
  Second :: Lookup (x1 ': Value a ': x2) a
  Third :: Lookup (x1 ': x2': Value a ': x3) a
  Fourth :: Lookup (x1 ': x2 ': x3 ': Value a ': x4) a
  Fifth :: Lookup (x1 ': x2 ': x3 ': x4 ': Value a ': x5) a
  Sixth :: Lookup (x1 ': x2 ': x3 ': x4 ': x5 ': Value a ': x6) a
  Seventh :: Lookup (x1 ': x2 ': x3 ': x4 ': x5 ': x6 ': Value a ': x7) a
  Eigth :: Lookup (x1 ': x2 ': x3 ': x4 ': x5 ': x6 ': x7 ': Value a ': x8) a
  Ninth :: Lookup (x1 ': x2 ': x3 ': x4 ': x5 ': x6 ': x7 ': x8 ': Value a ': x9) a

isSafetyPredicate :: Pred v a -> Bool
isSafetyPredicate Tautology       = True
isSafetyPredicate Contradiction   = True
isSafetyPredicate (Now _)         = True
isSafetyPredicate (Not p)         = isSafetyPredicate p
isSafetyPredicate (And q p)       = isSafetyPredicate q && isSafetyPredicate p
isSafetyPredicate (Or q p)        = isSafetyPredicate q || isSafetyPredicate p
isSafetyPredicate (Next p)        = isSafetyPredicate p
isSafetyPredicate (Implies q p)   = isSafetyPredicate q && isSafetyPredicate p
isSafetyPredicate (After _ p)     = isSafetyPredicate p
isSafetyPredicate (Until _ _)     = False
isSafetyPredicate (Always _ )     = False
isSafetyPredicate (Eventually _ ) = False
isSafetyPredicate (Release _ _)   = False

type SafetyPred v a = Either SafetyError (Pred v a)

newtype SafetyError = SafetyError String

mkSafePred :: Pred v a -> SafetyPred v a
mkSafePred p
  | isSafetyPredicate p = Right p
  | otherwise = Left $ SafetyError "Predicate is a safety property."

mkBinaryOp :: (Pred v a -> Pred v a -> Pred v a) -> Pred v a -> Pred v a -> SafetyPred v a
mkBinaryOp op p q = do
  p' <- mkSafePred p
  q' <- mkSafePred q
  return (op p' q')

mkAnd, mkOr, mkImplies :: Pred v a -> Pred v a -> SafetyPred v a
mkAnd = mkBinaryOp And
mkOr = mkBinaryOp Or
mkImplies = mkBinaryOp Implies

mkUnaryOp :: (Pred v a -> Pred v a) -> Pred v a -> SafetyPred v a
mkUnaryOp op = mkSafePred . op

mkNext, mkNow, mkTautology, mkContradiction :: Pred v a -> SafetyPred v a
mkNext = mkUnaryOp id
mkNow = mkUnaryOp id
mkTautology = mkUnaryOp id
mkContradiction = mkUnaryOp id

mkLivenessOp :: String -> Pred v a -> SafetyPred v a
mkLivenessOp op _ = Left $ SafetyError ("The '" ++ op ++ "' operator cannot be constructed in a safety property.")

mkAlways, mkUntil, mkEventually, mkRelease :: Pred v a -> SafetyPred v a
mkAlways = mkLivenessOp "Always"
mkUntil = mkLivenessOp "Until"
mkEventually = mkLivenessOp "Eventually"
mkRelease = mkLivenessOp "Release"

example :: SafetyPred '[Value a, Value a, Value a] a
example = do
  let p = Now (Equals First Second)
      q = Now (Equals First Third)
  safeP <- mkSafePred (Or p q)
  _ <- mkAlways safeP
  return safeP

evaluate' :: (Ord a) => Int -> Pred v a -> Sig (HList v) -> Bool
evaluate' timestepsLeft formulae sig@(x ::: Delay cl f) =
  timestepsLeft <= 0 || case formulae of
        Tautology       -> True
        Contradiction   -> False
        Now atom        -> evalAtom atom x
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
        After n phi      -> if n <= 0 then eval phi sig else evaluateNext (After (n - 1) phi) sig
  where
    evaluateNext = evaluate' (timestepsLeft - 1)
    eval = evaluate' timestepsLeft
    smallest = IntSet.findMin
    advance = f (InputValue (smallest cl) ())

evalAtom :: (Ord a) => Atom v a -> HList v -> Bool
evalAtom a hl = case a of
  Equals x y       -> evalLookup x hl == evalLookup y hl
  LargerThan x y   -> evalLookup x hl > evalLookup y hl
  GreaterThan x y  -> evalLookup x hl < evalLookup y hl

evalLookup :: (Ord a) => Lookup v a -> HList v -> Value a
evalLookup lu hl = case lu of
  Previous f       -> 
    let (Current b rest) = evalLookup f hl -- dangerous, out of bounds
    in case rest of
         _ :! v -> Current b v
         Nil    -> error "Previous: out of bounds"
  First         -> first hl
  Second        -> second hl
  Third         -> third hl
  Fourth       -> fourth hl
  Fifth        -> fifth hl
  Sixth        -> sixth hl
  Seventh      -> seventh hl
  Eigth        -> eigth hl
  Ninth        -> ninth hl
  Const a      -> Current (HasTicked True) (a :! Nil)

evaluate :: (Ord a) => Pred v a -> Sig (HList v) -> Bool
evaluate = evaluate' 20
