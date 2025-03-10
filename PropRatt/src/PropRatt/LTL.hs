{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE GADTs, DataKinds, KindSignatures, MultiParamTypeClasses, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE TypeOperators #-}

module PropRatt.LTL
  ( Pred (..),
    evaluate,
    -- example,
    unWrap,
    Atom (..),
    LookUp (..)
  )
where

import AsyncRattus.InternalPrimitives
import AsyncRattus.Strict
import AsyncRattus.Signal hiding (const)
import qualified Data.IntSet as IntSet
import PropRatt.AsyncRat
import Data.Kind
import Data.Data (Typeable)
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
  Equals :: LookUp v a -> LookUp v a -> Atom v a 
  LargerThan :: LookUp v a -> LookUp v a -> Atom v a
  GreaterThan :: LookUp v a -> LookUp v a -> Atom v a


data LookUp (v :: [Type]) a where 
  Prior :: LookUp v a -> LookUp v a
  First :: LookUp (Value a ': x) a
  Second :: LookUp (x1 ': Value a ': x2) a
  Third :: LookUp (x1 ': x2': Value a ': x3) a
  Fourth :: LookUp (x1 ': x2 ': x3 ': Value a ': x4) a
  Fifth :: LookUp (x1 ': x2 ': x3 ': x4 ': Value a ': x5) a
  Sixth :: LookUp (x1 ': x2 ': x3 ': x4 ': x5 ': Value a ': x6) a
  Seventh :: LookUp (x1 ': x2 ': x3 ': x4 ': x5 ': x6 ': Value a ': x7) a
  Eigth :: LookUp (x1 ': x2 ': x3 ': x4 ': x5 ': x6 ': x7 ': Value a ': x8) a
  Ninth :: LookUp (x1 ': x2 ': x3 ': x4 ': x5 ': x6 ': x7 ': x8 ': Value a ': x9) a

-- -- Maybe not eval? YES
-- eval :: Atom v -> Pred v
-- eval atom = case atom of
--   --Equal -> 
--   Prior selector hlist -> case selector hlist of 
--     (Current _ (_ :! a :! _)) -> a
--   First hls -> first hls
--   Second hls -> second hls
--   Third hls -> third hls
--   Fourth hls -> fourth hls
--   Fifth hls -> fifth hls
--   Sixth hls -> sixth hls
--   Seventh hls -> seventh hls
--   Eight hls -> eight hls
--   Ninth hls -> ninth hls


-- packman b = if b then Tautology else Contradiction

-- compare :: (a -> b -> Bool) -> Value a -> Value b -> Pred v
-- comp f a1 a2 = 

-- (??=) :: Value a -> Value b -> Bool v
-- (??=) (Current _ (a :! as)) (Current _ (b :! bs)) = a == b


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
-- Unsafe
isSafetyPredicate (Until _ _)     = False
isSafetyPredicate (Always _ )     = False
isSafetyPredicate (Eventually _ ) = False
isSafetyPredicate (Release _ _)   = False

type SafetyPred v a = Either SafetyError (Pred v a)

data SafetyError = LivenessOperatorError String | GenericError String

mkSafePred :: Pred v a -> SafetyPred v a
mkSafePred p
  | isSafetyPredicate p = Right p
  | otherwise = Left $ GenericError "Predicate is a safety property."

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
mkLivenessOp op _ = Left $ LivenessOperatorError ("The '" ++ op ++ "' operator cannot be constructed in a safety property.")

mkAlways, mkUntil, mkEventually, mkRelease :: Pred v a -> SafetyPred v a
mkAlways = mkLivenessOp "Always"
mkUntil = mkLivenessOp "Until"
mkEventually = mkLivenessOp "Eventually"
mkRelease = mkLivenessOp "Release"

-- Example usage:
-- Or (Now (Equals First Second) (Now (Equals First Third)))
-- example :: SafetyPred v a
-- example = do
--   let p = Now (Equals First Second)
--       q = Now (Equals First Third)
--      -- w = Now (Third (const False))
--   safeP <- mkSafePred (Or p q)
--   -- Attempting to add a liveness operator will cause an error:
--   _ <- mkAlways safeP
--   return safeP

unWrap :: SafetyPred v a -> String
unWrap s = case s of 
  Left s -> "err"
  Right s -> "no err"

evaluate' :: (Ord a) => Int -> Pred v a -> Sig (HList v) -> Bool
evaluate' timestepsLeft formulae sig@(x ::: Delay cl f) =
  timestepsLeft <= 0
    || case formulae of
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
         After n phi     -> if n <= 0 then eval phi sig else evaluateNext (After (n - 1) phi) sig
  where
    evaluateNext = evaluate' (timestepsLeft - 1)
    eval = evaluate' timestepsLeft
    smallest = IntSet.findMin
    advance = f (InputValue (smallest cl) ())

evalAtom :: (Ord a) => Atom v a -> HList v -> Bool
evalAtom a hl = case a of
  Equals x y       -> evalLookUp x hl == evalLookUp y hl
  LargerThan x y   -> evalLookUp x hl > evalLookUp y hl
  GreaterThan x y  -> evalLookUp x hl < evalLookUp y hl

evalLookUp :: (Ord a) => LookUp v a -> HList v -> Value a
evalLookUp lu hl = case lu of
  Prior f       -> 
    let (Current b (_ :! v)) = evalLookUp f hl
    in Current b v
  First         -> first hl
  Second        -> second hl
  Third         -> third hl
  Fourth       -> fourth hl
  Fifth        -> fifth hl
  Sixth        -> sixth hl
  Seventh      -> seventh hl
  Eigth        -> eigth hl
  Ninth        -> ninth hl

evaluate :: (Ord a) => Pred v a -> Sig (HList v) -> Bool
evaluate = evaluate' 20