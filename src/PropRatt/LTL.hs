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
    tick1,tick2,tick3,tick4,
    sig1,sig2,sig3,sig4,
    prev, prevN
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
  TT    :: Pred ts
  FF    :: Pred ts
  Now   :: Expr ts Bool -> Pred ts
  Not   :: Pred ts -> Pred ts
  And   :: Pred ts -> Pred ts -> Pred ts
  Or    :: Pred ts -> Pred ts -> Pred ts
  U     :: Pred ts -> Pred ts -> Pred ts
  X     :: Pred ts -> Pred ts
  (:=>) :: Pred ts -> Pred ts -> Pred ts
  G     :: Pred ts -> Pred ts
  F     :: Pred ts -> Pred ts
  XN    :: Int -> Pred ts-> Pred ts
  R     :: Pred ts -> Pred ts -> Pred ts

data Expr (ts :: [Type]) (t :: Type) where
  Pure  :: t -> Expr ts t
  App   :: Expr ts (t -> r) -> Expr ts t -> Expr ts r
  Val   :: Lookup ts t -> Expr ts t
  Tick  :: Lookup ts t -> Expr ts Bool

data Lookup (ts :: [Type]) (t :: Type) where
  Prev   :: Lookup ts t -> Lookup ts t
  PrevN  :: Int -> Lookup ts t -> Lookup ts t
  Sig1   :: Lookup (Value t ': x) t
  Sig2   :: Lookup (x1 ': Value t ': x2) t
  Sig3   :: Lookup (x1 ': x2 ': Value t ': x3) t
  Sig4   :: Lookup (x1 ': x2 ': x3 ': Value t ': x4) t
  Sig5   :: Lookup (x1 ': x2 ': x3 ': x4 ': Value t ': x5) t
  Sig6   :: Lookup (x1 ': x2 ': x3 ': x4 ': x5 ': Value t ': x6) t
  Sig7   :: Lookup (x1 ': x2 ': x3 ': x4 ': x5 ': x6 ': Value t ': x7) t
  Sig8   :: Lookup (x1 ': x2 ': x3 ': x4 ': x5 ': x6 ': x7 ': Value t ': x8) t
  Sig9   :: Lookup (x1 ': x2 ': x3 ': x4 ': x5 ': x6 ': x7 ': x8 ': Value t ': x9) t

instance Functor (Expr ts) where
  fmap :: (t -> r) -> Expr ts t -> Expr ts r
  fmap f (Pure x)     = Pure (f x)
  fmap f (App g x)  = App (fmap (f .) g) x
  fmap f (Val lu)   = App (Pure f) (Val lu)
  fmap f (Tick lu)  = App (Pure f) (Tick lu)

instance Applicative (Expr ts) where
    pure :: t -> Expr ts t
    pure = Pure
    (<*>) :: Expr ts (t -> r) -> Expr ts t -> Expr ts r
    Pure f <*> x = fmap f x
    f <*> x = App f x

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

(|<|) :: Ord t => Expr ts t -> Expr ts t -> Pred ts
x |<| y = Now ((<) <$> x <*> y)
(|<=|) :: Ord t => Expr ts t -> Expr ts t -> Pred ts
x |<=| y = Now ((<=) <$> x <*> y)
(|>|) :: Ord t => Expr ts t -> Expr ts t -> Pred ts
x |>| y = Now ((>) <$> x <*> y)
(|>=|) :: Ord t => Expr ts t -> Expr ts t -> Pred ts
x |>=| y = Now ((>=) <$> x <*> y)
(|==|) :: Ord t => Expr ts t -> Expr ts t -> Pred ts
x |==| y = Now ((==) <$> x <*> y)

tick1 :: Pred (Value t ': ts)
tick1 = Now (Tick Sig1)

tick2 :: Pred (t1 ': Value t2 ': ts)
tick2 = Now (Tick Sig2)

tick3 :: Pred (t1 ': t2 ': Value t3 ': ts)
tick3 = Now (Tick Sig3)

tick4 :: Pred (t1 ': t2 ': t3 ': Value t4 ': ts)
tick4 = Now (Tick Sig4)


sig1 :: Expr (Value t ': ts) t
sig1 = Val Sig1

sig2 :: Expr (t1 ': Value t2 ': ts) t2
sig2 = Val Sig2

sig3 :: Expr (t1 ': t2 ': Value t3 ': ts) t3
sig3 = Val Sig3

sig4 :: Expr (t1 ': t2 ': t3 ': Value t4 ': ts) t4
sig4 = Val Sig4

prev :: Expr ts t -> Expr ts t
prev (Pure x)   = Pure x
prev (App f x)  = App (prev f) (prev x)
prev (Val lu)   = Val (Prev lu)
prev (Tick lu)  = Tick (Prev lu)

prevN :: Int -> Expr ts t -> Expr ts t
prevN _ (Pure x)   = Pure x
prevN n (App f x)  = App (prevN n f) (prevN n x)
prevN n (Val lu)   = Val (PrevN n lu)
prevN n (Tick lu)  = Tick (PrevN n lu)

-- | Checks whether the instances of "previous" is within scope of t "X" operator.
-- This prevents the evaluation from looking too far back in time.
checkScope :: Pred ts -> Bool
checkScope p = checkPred p 0

-- | Traverses the predicate supplied and exits early if it finds a subtree where the scope is negative.
-- The scope is incremented for each X constructor, and decremented for each previous or prior constructor.
checkPred :: Pred ts -> Int -> Bool
checkPred predicate scope =
  valid scope &&
  case predicate of
    TT        -> valid scope
    FF        -> valid scope
    Now expr  -> valid (checkExpr expr scope)
    Not p     -> checkPred p scope
    And p1 p2 -> checkPred p1 scope && checkPred p2 scope
    Or p1 p2  -> checkPred p1 scope || checkPred p2 scope
    U p1 p2   -> checkPred p1 scope && checkPred p2 scope
    X p       -> checkPred p (scope + 1)
    p1 :=> p2 -> checkPred p1 scope && checkPred p2 scope
    R p1 p2   -> checkPred p1 scope && checkPred p2 scope
    G p       -> checkPred p scope
    F p       -> checkPred p scope
    XN n p    -> checkPred p (scope + n)
  where
    valid s = s >= 0

-- | Propegates the smallest scope found by traversing the expr.
checkExpr :: Expr ts t -> Int -> Int
checkExpr expr scope =
  case expr of
    Pure _      -> scope
    App fun arg -> min (checkExpr fun scope) (checkExpr arg scope)
    Val lu      -> checkLookup lu scope
    Tick lu     -> checkLookup lu scope

checkLookup :: Lookup ts t -> Int -> Int
checkLookup lu scope =
  case lu of
    Prev lu'    -> checkLookup lu' (scope - 1)
    PrevN n lu' -> checkLookup lu' (scope - n)
    _           -> scope

-- Returns the amount of signal elements needed to evaluate the predicate.
minSigLengthForPred :: Pred ts -> Int -> Int
minSigLengthForPred predicate acc =
    case predicate of
      Not p     -> minSigLengthForPred p acc
      And p1 p2 -> minSigLengthForPred p1 acc `max` minSigLengthForPred p2 acc
      Or p1 p2  -> minSigLengthForPred p1 acc `max` minSigLengthForPred p2 acc
      U p1 p2   -> minSigLengthForPred p1 acc `max` minSigLengthForPred p2 acc
      X p       -> minSigLengthForPred p (acc + 1)
      p1 :=> p2 -> minSigLengthForPred p1 acc `max` minSigLengthForPred p2 acc
      R p1 p2   -> minSigLengthForPred p1 acc `max` minSigLengthForPred p2 acc
      G p       -> minSigLengthForPred p acc
      F p       -> minSigLengthForPred p acc
      XN n p    -> minSigLengthForPred p (acc + n)
      _         -> acc

nthPrev :: Int -> Value t -> Maybe' (Value t)
nthPrev n curr@(Current b history)
  | n <= 0    = Just' curr
  | otherwise =
      case history of
        _ :! xs -> nthPrev (n - 1) (Current b xs)
        Nil     -> Nothing'

evalTick :: Lookup ts t -> HList ts -> Bool
evalTick lu hls = case lu of
  Prev _    -> errorTickPast
  PrevN _ _ -> errorTickPast
  Sig1      -> extract $ first hls
  Sig2      -> extract $ second hls
  Sig3      -> extract $ third hls
  Sig4      -> extract $ fourth hls
  Sig5      -> extract $ fifth hls
  Sig6      -> extract $ sixth hls
  Sig7      -> extract $ seventh hls
  Sig8      -> extract $ eighth hls
  Sig9      -> extract $ ninth hls
  where
    errorTickPast                   = error "Cannot check if signal has ticked in the past."
    extract (Current (HasTick b) _) = b

evalExpr :: Expr ts t -> HList ts -> Expr ts t
evalExpr (Pure x) _      = pure x
evalExpr (App f x) hls = (($) <$> evalExpr f hls) <*> evalExpr x hls
evalExpr (Val lu) hls  =
  case evalLookup lu hls of
    Just' (Current _ (h :! _)) -> pure h
    Just' (Current _ Nil)      -> error "History not found for signal."
    Nothing'                   -> error "Signal not found."
evalExpr (Tick lu) hls = pure (evalTick lu hls)

evalLookup :: Lookup ts t -> HList ts -> Maybe' (Value t)
evalLookup lu hls = case lu of
  Prev lu' ->
    case evalLookup lu' hls of
      Just' (Current b history) ->
        case history of
          _ :! xs -> Just' (Current b xs)
          Nil     -> Nothing'
      Nothing' -> Nothing'
  PrevN n lu'  -> case evalLookup lu' hls of
    Just' v  -> nthPrev n v
    Nothing' -> Nothing'
  Sig1 -> Just' (first hls)
  Sig2 -> Just' (second hls)
  Sig3 -> Just' (third hls)
  Sig4 -> Just' (fourth hls)
  Sig5 -> Just' (fifth hls)
  Sig6 -> Just' (sixth hls)
  Sig7 -> Just' (seventh hls)
  Sig8 -> Just' (eighth hls)
  Sig9 -> Just' (ninth hls)

-- Evaluate a single timestep. Used exclusively for shrink cases.
evaluateSingle  :: Int -> Pred ts -> Sig (HList ts) -> Bool
evaluateSingle timestepsLeft formulae sig@(x ::: _) =
  timestepsLeft <= 0 || case formulae of
            TT -> True
            FF -> False
            Now expr        ->
              case evalExpr expr x of
                Pure b -> b
                _ -> error "Unexpected error during evaluation."
            Not phi     -> not (eval phi sig)
            And phi psi -> eval phi sig && eval psi sig
            Or phi psi  -> eval phi sig || eval psi sig
            U phi psi   -> eval psi sig || eval phi sig
            X _         -> True
            phi :=> psi -> not (eval phi sig && not (eval psi sig))
            G phi       -> eval phi sig
            F phi       -> eval phi sig 
            R _ _       -> True 
            XN _ _      -> True
        where
          eval = evaluateSingle timestepsLeft

evaluate' :: Int -> Pred ts -> Sig (HList ts) -> Bool
evaluate' timestepsLeft formulae sig@(x ::: Delay cl f) =
  if IntSet.null cl
    then evaluateSingle timestepsLeft formulae sig
    else timestepsLeft <= 0 || case formulae of
            TT   -> True
            FF   -> False
            Now expr ->
              case evalExpr expr x of
                Pure b -> b
                _ -> error "Unexpected error during evaluation."
            Not phi     -> not (eval phi sig)
            And phi psi -> eval phi sig && eval psi sig
            Or phi psi  -> eval phi sig || eval psi sig
            U phi psi   -> eval psi sig
                              || (eval phi sig && evaluateX (phi `U` psi) advance)
            X phi       -> evaluateX phi advance
            phi :=> psi -> not (eval phi sig && not (eval psi sig))
            G phi       -> eval phi sig && evaluateX (G phi) advance
            F phi       -> (eval phi sig || evaluateX (F phi) advance)
                                && not (timestepsLeft == 1 && not (eval phi sig))
            R phi psi   -> (eval psi sig && eval phi sig)
                                || (eval psi sig && evaluateX (phi `U` psi) advance)
            XN n phi    -> if n <= 0 then eval phi sig else evaluateX (XN (n - 1) phi) sig
      where
        evaluateX = evaluate' (timestepsLeft - 1)
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
      then error "Prev must be in scope of X" 
    else if min' > defaultTimeStepsToCheck
      then error ("Cannot evaluate more than " ++ show defaultTimeStepsToCheck ++ " values.\n" ++ "Predicate requires " ++ show min' ++ " timesteps. Consider using evaluateWith (>= " ++ show min' ++ ")")
    else
      tooShort || evaluate' (defaultTimeStepsToCheck `min` len) p sig