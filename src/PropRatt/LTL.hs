{-# LANGUAGE GADTs, DataKinds, MultiParamTypeClasses, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module PropRatt.LTL
  ( Pred (..),
    evaluate,
    evaluateWith,
    Expr (..),
    Lookup (..),
    (|<|),
    (|<=|),
    (|>|),
    (|>=|),
    (|==|),
  )
where

import AsyncRattus.InternalPrimitives
import AsyncRattus.Strict
import AsyncRattus.Signal hiding (const)
import qualified Data.IntSet as IntSet
import Data.Kind
import PropRatt.Value
import PropRatt.HList
import PropRatt.Utils

data Pred (ts :: [Type]) where
  Tautology     :: Pred ts
  Contradiction :: Pred ts
  Now           :: Expr ts Bool -> Pred ts
  Not           :: Pred ts -> Pred ts
  And           :: Pred ts -> Pred ts -> Pred ts
  Or            :: Pred ts -> Pred ts -> Pred ts
  Until         :: Pred ts -> Pred ts -> Pred ts
  Next          :: Pred ts -> Pred ts
  Implies       :: Pred ts -> Pred ts -> Pred ts
  Always        :: Pred ts -> Pred ts
  Eventually    :: Pred ts -> Pred ts
  After         :: Int -> Pred ts-> Pred ts
  Release       :: Pred ts -> Pred ts -> Pred ts

data Expr (ts :: [Type]) (t :: Type) where
  Pure    :: t -> Expr ts t
  Apply   :: Expr ts (t -> r) -> Expr ts t -> Expr ts r
  Index   :: Lookup ts t -> Expr ts t
  Ticked  :: Lookup ts t -> Expr ts Bool

data Lookup (ts :: [Type]) (t :: Type) where
  Previous  :: Lookup ts t -> Lookup ts t
  Prior     :: Int -> Lookup ts t -> Lookup ts t
  First     :: Lookup (Value t ': x) t
  Second    :: Lookup (x1 ': Value t ': x2) t
  Third     :: Lookup (x1 ': x2 ': Value t ': x3) t
  Fourth    :: Lookup (x1 ': x2 ': x3 ': Value t ': x4) t
  Fifth     :: Lookup (x1 ': x2 ': x3 ': x4 ': Value t ': x5) t
  Sixth     :: Lookup (x1 ': x2 ': x3 ': x4 ': x5 ': Value t ': x6) t
  Seventh   :: Lookup (x1 ': x2 ': x3 ': x4 ': x5 ': x6 ': Value t ': x7) t
  Eighth    :: Lookup (x1 ': x2 ': x3 ': x4 ': x5 ': x6 ': x7 ': Value t ': x8) t
  Ninth     :: Lookup (x1 ': x2 ': x3 ': x4 ': x5 ': x6 ': x7 ': x8 ': Value t ': x9) t

instance Functor (Expr ts) where
  fmap :: (t -> r) -> Expr ts t -> Expr ts r
  fmap f (Pure x)     = Pure (f x)
  fmap f (Apply g x)  = Apply (fmap (f .) g) x
  fmap f (Index lu)   = Apply (Pure f) (Index lu)
  fmap f (Ticked lu)  = Apply (Pure f) (Ticked lu)

instance Applicative (Expr ts) where
    pure :: t -> Expr ts t
    pure = Pure
    (<*>) :: Expr ts (t -> r) -> Expr ts t -> Expr ts r
    Pure f <*> x = fmap f x
    Apply f g <*> x = Apply (Apply f g) x
    (<*>) _ _ = error "Expr: unsupported constructor for applicative application."

instance Num t => Num (Expr ts t) where
  (+) :: Expr ts t -> Expr ts t -> Expr ts t
  (+) x y = (+) <$> x <*> y
  (-) :: Expr ts t -> Expr ts t -> Expr ts t
  (-) x y = (-) <$> x <*> y
  (*) :: Expr ts t -> Expr ts t -> Expr ts t
  (*) x y = (*) <$> x <*> y
  negate :: Expr ts t -> Expr ts t
  negate = fmap negate
  abs :: Expr ts t -> Expr ts t
  abs = fmap abs
  signum :: Expr ts t -> Expr ts t
  signum = fmap signum
  fromInteger :: Integer -> Expr ts t
  fromInteger n = pure (fromInteger n)

(|<|) :: (Applicative f, Ord t) => f t -> f t -> f Bool
x |<| y = (<) <$> x <*> y
(|<=|) :: (Applicative f, Ord t) => f t -> f t -> f Bool
x |<=| y = (<=) <$> x <*> y
(|>|) :: (Applicative f, Ord t) => f t -> f t -> f Bool
x |>| y = (>) <$> x <*> y
(|>=|) :: (Applicative f, Ord t) => f t -> f t -> f Bool
x |>=| y = (>=) <$> x <*> y
(|==|) :: (Applicative f, Eq t) => f t -> f t -> f Bool
x |==| y = (==) <$> x <*> y

-- | Checks whether the instances of "previous" is within scope of t "next" operator.
-- This prevents the evaluation from looking too far back in time.
checkScope :: Pred ts -> Bool
checkScope p = checkPred p 0

-- | Traverses the predicate supplied and exits early if it finds a subtree where the scope is negative.
-- The scope is incremented for each next constructor, and decremented for each previous or prior constructor.
checkPred :: Pred ts -> Int -> Bool
checkPred predicate scope =
  valid scope &&
  case predicate of
    Tautology       -> valid scope
    Contradiction   -> valid scope
    Now expr        -> valid (checkExpr expr scope)
    Not p           -> checkPred p scope
    And p1 p2       -> checkPred p1 scope && checkPred p2 scope
    Or p1 p2        -> checkPred p1 scope || checkPred p2 scope
    Until p1 p2     -> checkPred p1 scope && checkPred p2 scope
    Next p          -> checkPred p (scope + 1)
    Implies p1 p2   -> checkPred p1 scope && checkPred p2 scope
    Release p1 p2   -> checkPred p1 scope && checkPred p2 scope
    Always p        -> checkPred p scope
    Eventually p    -> checkPred p scope
    After n p       -> checkPred p (scope + n)
  where
    valid s = s >= 0

-- | Propegates the smallest scope found by traversing the expr.
checkExpr :: Expr ts t -> Int -> Int
checkExpr expr scope =
  case expr of
    Pure _        -> scope
    Apply fun arg -> min (checkExpr fun scope) (checkExpr arg scope)
    Index lu      -> checkLookup lu scope
    Ticked lu     -> checkLookup lu scope

checkLookup :: Lookup ts t -> Int -> Int
checkLookup lu scope =
  case lu of
    Previous lu'  -> checkLookup lu' (scope - 1)
    Prior n lu'   -> checkLookup lu' (scope - n)
    _             -> scope

-- Returns the amount of signal elements needed to evaluate the predicate.
minSigLengthForPred :: Pred ts -> Int -> Int
minSigLengthForPred predicate acc =
    case predicate of
      Not p           -> minSigLengthForPred p acc
      And p1 p2       -> minSigLengthForPred p1 acc `max` minSigLengthForPred p2 acc
      Or p1 p2        -> minSigLengthForPred p1 acc `max` minSigLengthForPred p2 acc
      Until p1 p2     -> minSigLengthForPred p1 acc `max` minSigLengthForPred p2 acc
      Next p          -> minSigLengthForPred p (acc + 1)
      Implies p1 p2   -> minSigLengthForPred p1 acc `max` minSigLengthForPred p2 acc
      Release p1 p2   -> minSigLengthForPred p1 acc `max` minSigLengthForPred p2 acc
      Always p        -> minSigLengthForPred p acc
      Eventually p    -> minSigLengthForPred p acc
      After n p       -> minSigLengthForPred p (acc + n)
      _               -> acc

nthPrevious :: Int -> Value t -> Maybe' (Value t)
nthPrevious n curr@(Current b history)
  | n <= 0    = Just' curr
  | otherwise =
      case history of
        _ :! xs -> nthPrevious (n - 1) (Current b xs)
        Nil     -> Nothing'

evalTicked :: Lookup ts t -> HList ts -> Bool
evalTicked lu hls = case lu of
  Previous _ -> errorTickedPast
  Prior _ _  -> errorTickedPast
  First      -> extract $ first hls
  Second     -> extract $ second hls
  Third      -> extract $ third hls
  Fourth     -> extract $ fourth hls
  Fifth      -> extract $ fifth hls
  Sixth      -> extract $ sixth hls
  Seventh    -> extract $ seventh hls
  Eighth     -> extract $ eighth hls
  Ninth      -> extract $ ninth hls
  where
    errorTickedPast                   = error "Cannot check if signal has ticked in the past."
    extract (Current (HasTicked b) _) = b

evalExpr :: Expr ts t -> HList ts -> Expr ts t
evalExpr (Pure x) _      = pure x
evalExpr (Apply f x) hls = (($) <$> evalExpr f hls) <*> evalExpr x hls
evalExpr (Index lu) hls  =
  case evalLookup lu hls of
    Just' (Current _ (h :! _)) -> pure h
    Just' (Current _ Nil)      -> error "History not found for signal."
    Nothing'                   -> error "Signal not found."
evalExpr (Ticked lu) hls = pure (evalTicked lu hls)

evalLookup :: Lookup ts t -> HList ts -> Maybe' (Value t)
evalLookup lu hls = case lu of
  Previous lu' ->
    case evalLookup lu' hls of
      Just' (Current b history) ->
        case history of
          _ :! xs -> Just' (Current b xs)
          Nil     -> Nothing'
      Nothing' -> Nothing'
  Prior n lu'  -> case evalLookup lu' hls of
    Just' v  -> nthPrevious n v
    Nothing' -> Nothing'
  First         -> Just' (first hls)
  Second        -> Just' (second hls)
  Third         -> Just' (third hls)
  Fourth        -> Just' (fourth hls)
  Fifth         -> Just' (fifth hls)
  Sixth         -> Just' (sixth hls)
  Seventh       -> Just' (seventh hls)
  Eighth        -> Just' (eighth hls)
  Ninth         -> Just' (ninth hls)

-- Evaluate a single timestep. Used exclusively for shrink cases.
evaluateSingle  :: Int -> Pred ts -> Sig (HList ts) -> Bool
evaluateSingle timestepsLeft formulae sig@(x ::: _) =
  timestepsLeft <= 0 || case formulae of
            Tautology       -> True
            Contradiction   -> False
            Now expr        ->
              case evalExpr expr x of
                Pure b -> b
                _ -> error "Unexpected error during evaluation."
            Not phi         -> not (eval phi sig)
            And phi psi     -> eval phi sig && eval psi sig
            Or phi psi      -> eval phi sig || eval psi sig
            Until phi psi   -> eval psi sig || eval phi sig
            Next _          -> True
            Implies phi psi -> not (eval phi sig && not (eval psi sig))
            Always phi      -> eval phi sig
            Eventually phi  -> eval phi sig 
            Release _ _     -> True 
            After _ _       -> True
        where
          eval = evaluateSingle timestepsLeft

evaluate' :: Int -> Pred ts -> Sig (HList ts) -> Bool
evaluate' timestepsLeft formulae sig@(x ::: Delay cl f) =
  if IntSet.null cl
    then evaluateSingle timestepsLeft formulae sig
    else timestepsLeft <= 0 || case formulae of
            Tautology       -> True
            Contradiction   -> False
            Now expr        ->
              case evalExpr expr x of
                Pure b -> b
                _ -> error "Unexpected error during evaluation."
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
        advance = f (InputValue (IntSet.findMin cl) ())

-- Finds the minimum length a signal must have for the pred to be tested.
-- If the length of the signal is too short, short circuit evaluation to true (shrink cases).
evaluate :: Pred ts -> Sig (HList ts) -> Bool
evaluate = evaluateWith 100

evaluateWith :: Int -> Pred ts -> Sig (HList ts) -> Bool
evaluateWith defaultTimeStepsToCheck p sig =
  let len       = sigLength sig
      min'      = minSigLengthForPred p 1
      tooShort  = len < min'
      scopeOk   = checkScope p
  in 
    if not scopeOk
      then error "Previous must be in scope of next" 
    else if min' > defaultTimeStepsToCheck
      then error ("Cannot evaluate more than " ++ show defaultTimeStepsToCheck ++ " values.\n" ++ "Predicate requires " ++ show min' ++ " timesteps. Consider using evaluateWith (>= " ++ show min' ++ ")")
    else
      tooShort || evaluate' (defaultTimeStepsToCheck `min` len) p sig