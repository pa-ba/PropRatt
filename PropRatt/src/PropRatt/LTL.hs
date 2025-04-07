{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE GADTs, DataKinds, MultiParamTypeClasses, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
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
    SafetyError,
    SafetyPred,
    (|<|),
    (|<=|),
    (|>|),
    (|>=|),
    (|==|),
    checkScope,
  )
where

import AsyncRattus.InternalPrimitives
import AsyncRattus.Strict
import AsyncRattus.Signal hiding (const)
import qualified Data.IntSet as IntSet
import PropRatt.AsyncRat
import Data.Kind
import PropRatt.Value

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

-- | Propegates the smallest scope found by traversing the atom.
checkAtom :: Atom ts t -> Int -> Int
checkAtom atom scope =
  case atom of
    Pure _        -> scope
    Apply fun arg -> min (checkAtom fun scope) (checkAtom arg scope)
    Index lookup  -> checkLookup lookup scope

checkLookup :: Lookup ts t -> Int -> Int
checkLookup lookup scope =
  case lookup of
    Previous lookup'  -> checkLookup lookup' (scope - 1)
    Prior n lookup'   -> checkLookup lookup' (scope - n)
    _                 -> scope

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
    errorTickedPast                   = error "Cannot check if t signal has ticked in the past."
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
  Previous lu ->
    case evalLookup lu hls of
      Just' (Current b history) ->
        case history of
          _ :! xs -> Just' (Current b xs)
          Nil     -> Nothing'
      Nothing' -> Nothing'
  Prior n lu -> case evalLookup lu hls of
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

evaluate' :: (Ord t) => Int -> Pred ts t -> Sig (HList ts) -> Bool
evaluate' timestepsLeft formulae sig@(x ::: Delay cl f) =
  timestepsLeft <= 0 || case formulae of
        Tautology       -> True
        Contradiction   -> False
        Now atom        -> 
          case evalAtom atom x of
            Pure b -> b
            _ -> error "Unexpected error during evaluation" -- unreachable
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

evaluate :: (Ord t) => Pred ts t -> Sig (HList ts) -> Bool
evaluate = evaluate' 50

-------------------------------

isSafetyPredicate :: Pred ts t -> Bool
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

type SafetyPred ts t = Either SafetyError (Pred ts t)

newtype SafetyError = SafetyError String

mkSafePred :: Pred ts t -> SafetyPred ts t
mkSafePred p
  | isSafetyPredicate p = Right p
  | otherwise = Left $ SafetyError "Predicate is t safety property."

mkBinaryOp :: (Pred ts t -> Pred ts t -> Pred ts t) -> Pred ts t -> Pred ts t -> SafetyPred ts t
mkBinaryOp op p q = do
  p' <- mkSafePred p
  q' <- mkSafePred q
  return (op p' q')

mkAnd, mkOr, mkImplies :: Pred ts t -> Pred ts t -> SafetyPred ts t
mkAnd = mkBinaryOp And
mkOr = mkBinaryOp Or
mkImplies = mkBinaryOp Implies

mkUnaryOp :: (Pred ts t -> Pred ts t) -> Pred ts t -> SafetyPred ts t
mkUnaryOp op = mkSafePred . op

mkNext, mkNow, mkTautology, mkContradiction :: Pred ts t -> SafetyPred ts t
mkNext = mkUnaryOp id
mkNow = mkUnaryOp id
mkTautology = mkUnaryOp id
mkContradiction = mkUnaryOp id

mkLivenessOp :: String -> Pred ts t -> SafetyPred ts t
mkLivenessOp op _ = Left $ SafetyError ("The '" ++ op ++ "' operator cannot be constructed in t safety property.")

mkAlways, mkUntil, mkEventually, mkRelease :: Pred ts t -> SafetyPred ts t
mkAlways = mkLivenessOp "Always"
mkUntil = mkLivenessOp "Until"
mkEventually = mkLivenessOp "Eventually"
mkRelease = mkLivenessOp "Release"