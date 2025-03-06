{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE GADTs, DataKinds, KindSignatures, MultiParamTypeClasses, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE TypeOperators #-}

module PropRatt.LTL
  ( Pred (..),
    evaluate,
    example,
    unWrap
  )
where

import AsyncRattus.InternalPrimitives
import AsyncRattus.Signal hiding (const)
import qualified Data.IntSet as IntSet
import PropRatt.AsyncRat
import Data.Kind
import Data.Data (Typeable)
import PropRatt.Value

data Pred (v :: [Type]) where
  Tautology     :: Pred v
  Contradiction :: Pred v
  Now           :: Atom v -> Pred v
  Not           :: Pred v -> Pred v
  And           :: Pred v -> Pred v -> Pred v
  Or            :: Pred v -> Pred v -> Pred v
  Until         :: Pred v -> Pred v -> Pred v
  Next          :: Pred v -> Pred v
  Implies       :: Pred v -> Pred v -> Pred v
  Always        :: Pred v -> Pred v
  Eventually    :: Pred v -> Pred v
  After         :: Int -> Pred v -> Pred v
  Release       :: Pred v -> Pred v -> Pred v 
  
data Atom (v :: [Type]) a where
  Equal :: Value a -> Value b -> Atom v -- Atom v -> Atom v -> Atom v??
  Larger :: Value a -> Value b -> Atom v
  Greater :: Value a -> Value b -> Atom v

  Prior :: (Atom (Value a ': as) -> Value a) -> Atom v -> Atom v
  First :: Atom (v ': _) a
  Second :: Atom (_ ': v ': _) a
  Third :: Atom (_ ': _ ': v ': _) a
  Fourth :: Atom ( _ ': _ ': _ ': v ': _) a
  Fifth :: Atom ( _ ': _ ': _ ':  _ ': v ': _) a
  Sixth :: Atom ( _ ': _ ': _ ':  _ ': _ ': v ': _) a

-- Maybe not eval?
eval :: Atom v -> Pred v
eval atom = case atom of
  --Equal -> 
  Prior selector hlist -> case selector hlist of 
    (Current _ (_ :! a :! _)) -> a
  First hls -> first hls
  Second hls -> second hls
  Third hls -> third hls
  Fourth hls -> fourth hls
  Fifth hls -> fifth hls
  Sixth hls -> sixth hls
  Seventh hls -> seventh hls
  Eight hls -> eight hls
  Ninth hls -> ninth hls


packman b = if b then Tautology else Contradiction

-- compare :: (a -> b -> Bool) -> Value a -> Value b -> Pred v
-- comp f a1 a2 = 

-- (??=) :: Value a -> Value b -> Bool v
-- (??=) (Current _ (a :! as)) (Current _ (b :! bs)) = a == b


isSafetyPredicate :: Pred v -> Bool
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

type SafetyPred v = Either SafetyError (Pred v)

data SafetyError = LivenessOperatorError String | GenericError String

mkSafePred :: Pred v -> SafetyPred v
mkSafePred p
  | isSafetyPredicate p = Right p
  | otherwise = Left $ GenericError "Predicate is a safety property."

mkBinaryOp :: (Pred v -> Pred v -> Pred v) -> Pred v -> Pred v -> SafetyPred v
mkBinaryOp op p q = do
  p' <- mkSafePred p
  q' <- mkSafePred q
  return (op p' q')

mkAnd, mkOr, mkImplies :: Pred v -> Pred v -> SafetyPred v
mkAnd = mkBinaryOp And
mkOr = mkBinaryOp Or
mkImplies = mkBinaryOp Implies

mkUnaryOp :: (Pred v -> Pred v) -> Pred v -> SafetyPred v
mkUnaryOp op = mkSafePred . op

mkNext, mkNow, mkTautology, mkContradiction :: Pred v -> SafetyPred v
mkNext = mkUnaryOp id
mkNow = mkUnaryOp id
mkTautology = mkUnaryOp id
mkContradiction = mkUnaryOp id

mkLivenessOp :: String -> Pred v -> SafetyPred v
mkLivenessOp op _ = Left $ LivenessOperatorError ("The '" ++ op ++ "' operator cannot be constructed in a safety property.")

mkAlways, mkUntil, mkEventually, mkRelease :: Pred v -> SafetyPred v
mkAlways = mkLivenessOp "Always"
mkUntil = mkLivenessOp "Until"
mkEventually = mkLivenessOp "Eventually"
mkRelease = mkLivenessOp "Release"

-- Example usage:
example :: SafetyPred v
example = do
  let p = Now (const True)
      q = Now (const False)
      w = Now (const False)
  safeP <- mkSafePred (And p q)
  -- Attempting to add a liveness operator will cause an error:
  _ <- mkAlways safeP
  return safeP

unWrap :: SafetyPred s -> String
unWrap s = case s of 
  Left s -> "err"
  Right s -> "no err"

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