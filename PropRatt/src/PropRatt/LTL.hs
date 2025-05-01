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
    Atom (..),
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

data Pred (ts :: [Type]) (t :: Type) where
  Tautology     :: Pred ts t
  Contradiction :: Pred ts t
  Now           :: Atom ts Bool -> Pred ts Bool
  Not           :: Pred ts t -> Pred ts t
  And           :: Pred ts t -> Pred ts t -> Pred ts t
  Or            :: Pred ts t -> Pred ts t -> Pred ts t
  Until         :: Pred ts t -> Pred ts t -> Pred ts t
  Next          :: Pred ts t -> Pred ts t
  Implies       :: Pred ts t -> Pred ts t -> Pred ts t
  Always        :: Pred ts t -> Pred ts t
  Eventually    :: Pred ts t -> Pred ts t
  After         :: Int -> Pred ts t -> Pred ts t
  Release       :: Pred ts t -> Pred ts t -> Pred ts t

data Atom (ts :: [Type]) (t :: Type) where
  Pure    :: t -> Atom ts t
  Apply   :: Atom ts (t -> r) -> Atom ts t -> Atom ts r
  Index   :: Lookup ts t -> Atom ts t
  Ticked  :: Lookup ts t -> Atom ts Bool

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

instance Functor (Atom ts) where
  fmap :: (t -> r) -> Atom ts t -> Atom ts r
  fmap f (Pure x)     = Pure (f x)
  fmap f (Apply g x)  = Apply (fmap (f .) g) x
  fmap f (Index lu)   = Apply (Pure f) (Index lu)
  fmap f (Ticked lu)  = Apply (Pure f) (Ticked lu)

instance Applicative (Atom ts) where
    pure :: t -> Atom ts t
    pure = Pure
    (<*>) :: Atom ts (t -> r) -> Atom ts t -> Atom ts r
    Pure f <*> x = fmap f x
    Apply f g <*> x = Apply (Apply f g) x
    (<*>) _ _ = error "Atom: unsupported constructor for applicative application."

instance Num t => Num (Atom ts t) where
  (+) :: Atom ts t -> Atom ts t -> Atom ts t
  (+) x y = (+) <$> x <*> y
  (-) :: Atom ts t -> Atom ts t -> Atom ts t
  (-) x y = (-) <$> x <*> y
  (*) :: Atom ts t -> Atom ts t -> Atom ts t
  (*) x y = (*) <$> x <*> y
  negate :: Atom ts t -> Atom ts t
  negate = fmap negate
  abs :: Atom ts t -> Atom ts t
  abs = fmap abs
  signum :: Atom ts t -> Atom ts t
  signum = fmap signum
  fromInteger :: Integer -> Atom ts t
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
checkScope :: Pred ts t -> Bool
checkScope p = checkPred p 0

-- | Traverses the predicate supplied and exits early if it finds t subtree where the scope is negative.
-- The scope is incremented for each next constructor, and decremented for each previous or prior constructor.
checkPred :: Pred ts t -> Int -> Bool
checkPred predicate scope =
  valid scope &&
  case predicate of
    Tautology       -> valid scope
    Contradiction   -> valid scope
    Now atom        -> valid (checkAtom atom scope)
    Not p           -> checkPred p scope
    And p1 p2       -> checkPred p1 scope && checkPred p2 scope
    Or p1 p2        -> checkPred p1 scope || checkPred p2 scope
    Until p1 p2     -> checkPred p1 scope && checkPred p2 scope
    Next p          -> checkPred p (scope + 1)
    Implies p1 p2   -> checkPred p1 scope && checkPred p2 scope
    Release p1 p2   -> checkPred p1 scope && checkPred p2 scope
    Always p        -> checkPred p scope
    Eventually p    -> checkPred p scope
    After _ p       -> checkPred p scope
  where
    valid s = s >= 0

-- Returns the amount of signal elements needed, to evaluate the predicate at least once
minSigLengthForPred :: Pred ts t -> Int -> Int
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

-- | Propegates the smallest scope found by traversing the atom.
checkAtom :: Atom ts t -> Int -> Int
checkAtom atom scope =
  case atom of
    Pure _        -> scope
    Apply fun arg -> min (checkAtom fun scope) (checkAtom arg scope)
    Index lu      -> checkLookup lu scope
    Ticked lu     -> checkLookup lu scope

checkLookup :: Lookup ts t -> Int -> Int
checkLookup lu scope =
  case lu of
    Previous lu'  -> checkLookup lu' (scope - 1)
    Prior n lu'   -> checkLookup lu' (scope - n)
    _             -> scope

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

evalAtom :: Atom ts t -> HList ts -> Atom ts t
evalAtom (Pure x) _      = pure x
evalAtom (Apply f x) hls = (($) <$> evalAtom f hls) <*> evalAtom x hls
evalAtom (Index lu) hls  =
  case evalLookup lu hls of
    Just' (Current _ (h :! _)) -> pure h
    Just' (Current _ Nil)      -> error "History not found for signal."
    Nothing'                   -> error "Signal not found."
evalAtom (Ticked lu) hls = pure (evalTicked lu hls)

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

evaluateSingle  :: (Ord t) => Int -> Pred ts t -> Sig (HList ts) -> Bool
evaluateSingle timestepsLeft formulae sig@(x ::: Delay cl f) =
  timestepsLeft <= 0 || case formulae of
            Tautology       -> True
            Contradiction   -> False
            Now atom        ->
              case evalAtom atom x of
                Pure b -> b
                _ -> error "Unexpected error during evaluation."
            Not phi         -> not (eval phi sig)
            And phi psi     -> eval phi sig && eval psi sig
            Or phi psi      -> eval phi sig || eval psi sig
            Until phi psi   -> eval psi sig || eval phi sig
            Next phi        -> True
            Implies phi psi -> not (eval phi sig && not (eval psi sig))
            Always phi      -> eval phi sig
            Eventually phi  -> eval phi sig 
            Release phi psi -> True 
            After n phi     -> True
        where
          eval = evaluateSingle timestepsLeft

evaluate' :: (Ord t) => Int -> Pred ts t -> Sig (HList ts) -> Bool
evaluate' timestepsLeft formulae sig@(x ::: Delay cl f) =
  if IntSet.null cl 
    then evaluateSingle timestepsLeft formulae sig
    else timestepsLeft <= 0 || case formulae of
            Tautology       -> True
            Contradiction   -> False
            Now atom        ->
              case evalAtom atom x of
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

evaluate :: (Ord t) => Pred ts t -> Sig (HList ts) -> Bool
evaluate p sig =
  let len       = sigLength sig
      -- Find the minimum length that a signal must have, in order for the pred to be tested.
      min'      = minSigLengthForPred p 1
      tooShort  = len < min'
      scopeOk   = checkScope p
  in if (not scopeOk) then error "Previous must be in scope of next" else
    (tooShort || evaluate' (100 `min` len) p sig)