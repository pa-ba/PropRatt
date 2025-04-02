{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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
    (|>|),
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

data Pred (ts :: [Type]) a where
  Tautology     :: Pred ts a
  Contradiction :: Pred ts a
  Now           :: Atom ts Bool -> Pred ts Bool
  Not           :: Pred ts a -> Pred ts a
  And           :: Pred ts a -> Pred ts a -> Pred ts a
  Or            :: Pred ts a -> Pred ts a -> Pred ts a
  Until         :: Pred ts a -> Pred ts a -> Pred ts a
  Next          :: Pred ts a -> Pred ts a
  Implies       :: Pred ts a -> Pred ts a -> Pred ts a
  Always        :: Pred ts a -> Pred ts a
  Eventually    :: Pred ts a -> Pred ts a
  After         :: Int -> Pred ts a -> Pred ts a
  Release       :: Pred ts a -> Pred ts a -> Pred ts a

data Atom (ts :: [Type]) (t :: Type) where
  Pure :: t -> Atom ts t
  Apply :: Atom ts (t -> r) -> Atom ts t -> Atom ts r
  Index :: Lookup ts t -> Atom ts t
  Ticked :: Lookup ts t -> Atom ts Bool

data Lookup (ts :: [Type]) (t :: Type) where
  -- Ticked :: Lookup ts t -> Lookup ts Bool
  Previous :: Lookup ts t -> Lookup ts t
  Prior :: Int -> Lookup ts t -> Lookup ts t
  First :: Lookup (Value t ': x) t
  Second :: Lookup (x1 ': Value t ': x2) t
  Third :: Lookup (x1 ': x2 ': Value t ': x3) t
  Fourth :: Lookup (x1 ': x2 ': x3 ': Value t ': x4) t
  Fifth :: Lookup (x1 ': x2 ': x3 ': x4 ': Value t ': x5) t
  Sixth :: Lookup (x1 ': x2 ': x3 ': x4 ': x5 ': Value t ': x6) t
  Seventh :: Lookup (x1 ': x2 ': x3 ': x4 ': x5 ': x6 ': Value t ': x7) t
  Eighth :: Lookup (x1 ': x2 ': x3 ': x4 ': x5 ': x6 ': x7 ': Value t ': x8) t
  Ninth :: Lookup (x1 ': x2 ': x3 ': x4 ': x5 ': x6 ': x7 ': x8 ': Value t ': x9) t

instance Functor (Atom ts) where
  fmap :: (a -> b) -> Atom ts a -> Atom ts b
  fmap f (Pure x)     = Pure (f x)
  fmap f (Apply g x)  = Apply (fmap (f .) g) x
  fmap f (Index lu)   = Apply (Pure f) (Index lu)
  fmap f (Ticked lu)   = Apply (Pure f) (Ticked lu)


instance Applicative (Atom ts) where
    pure :: a -> Atom ts a
    pure = Pure
    (<*>) :: Atom ts (a -> b) -> Atom ts a -> Atom ts b
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
  
(|<|) :: (Applicative f, Ord a) => f a -> f a -> f Bool
x |<| y = (<) <$> x <*> y
(|>|) :: (Applicative f, Ord a) => f a -> f a -> f Bool
x |>| y = (>) <$> x <*> y
(|==|) :: (Applicative f, Eq a) => f a -> f a -> f Bool
x |==| y = (==) <$> x <*> y

checkScope :: Pred ts t -> Bool
checkScope p = checkPred p 0

checkPred :: Pred ts t -> Int -> Bool
checkPred predicate steps =
  steps >= 0 &&
  case predicate of
    Tautology       -> valid steps
    Contradiction   -> valid steps
    Now atom        -> valid (checkAtom atom steps)
    Not p           -> checkPred p steps
    And p1 p2       -> checkPred p1 steps && checkPred p2 steps
    Or p1 p2        -> checkPred p1 steps || checkPred p2 steps
    Until p1 p2     -> checkPred p1 steps && checkPred p2 steps
    Next p          -> checkPred p (steps + 1)
    Implies p1 p2   -> checkPred p1 steps && checkPred p2 steps
    Release p1 p2   -> checkPred p1 steps && checkPred p2 steps
    Always p        -> checkPred p steps
    Eventually p    -> checkPred p steps
    After _ p       -> checkPred p steps
  where
    valid s = s >= 0

checkAtom :: Atom ts t -> Int -> Int
checkAtom atom steps =
  case atom of
    Pure _        -> steps
    Apply fun arg -> min (checkAtom fun steps) (checkAtom arg steps)
    Index lookup  -> checkLookup lookup steps

checkLookup :: Lookup ts t -> Int -> Int
checkLookup lookup steps =
  case lookup of
    Previous lookup'  -> checkLookup lookup' (steps - 1)
    Prior n lookup'   -> checkLookup lookup' (steps - n)
    _                 -> steps

nthPrevious :: Int -> Value t -> Maybe' (Value t)
nthPrevious n curr@(Current b history)
  | n <= 0    = Just' curr
  | otherwise =
      case history of
        _ :! xs -> nthPrevious (n - 1) (Current b xs)
        Nil     -> Nothing'

extractHasTicked :: Value t -> Bool
extractHasTicked (Current (HasTicked b) _) = b

evalTicked :: Lookup ts t -> HList ts -> Bool
evalTicked lu hls = case lu of
  Previous lu -> error "No prev on HasTicked"
  Prior n lu -> error "No prev on HasTicked"
  First         -> extractHasTicked $ first hls
  Second        -> extractHasTicked $ second hls
  Third         -> extractHasTicked $ third hls
  Fourth        -> extractHasTicked $ fourth hls
  Fifth         -> extractHasTicked $ fifth hls
  Sixth         -> extractHasTicked $ sixth hls
  Seventh       -> extractHasTicked $ seventh hls
  Eighth        -> extractHasTicked $ eighth hls
  Ninth         -> extractHasTicked $ ninth hls

evalAtom :: Atom ts t -> HList ts -> Atom ts t
evalAtom (Pure x) _       = pure x
evalAtom (Apply f x) hls  = (($) <$> evalAtom f hls) <*> evalAtom x hls
evalAtom (Index lu) hls =
  case evalLookup lu hls of
    Just' (Current b (h :! t))  -> pure h
    Just' (Current b Nil)       -> error "No History. Buhu :)"
    Nothing'                    -> error "No Value, Extra buhu:)"
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
    Just' v -> nthPrevious n v
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

evaluate' :: (Ord a) => Int -> Pred ts a -> Sig (HList ts) -> Bool
evaluate' timestepsLeft formulae sig@(x ::: Delay cl f) =
  timestepsLeft <= 0 || case formulae of
        Tautology       -> True
        Contradiction   -> False
        Now atom        ->
          let m = evalAtom atom x
          in case m of
            Pure b -> b
            _ -> error "hej123"
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

evaluate :: (Ord a) => Pred ts a -> Sig (HList ts) -> Bool
evaluate = evaluate' 20

-------------------------------

isSafetyPredicate :: Pred ts a -> Bool
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

type SafetyPred ts a = Either SafetyError (Pred ts a)

newtype SafetyError = SafetyError String

mkSafePred :: Pred ts a -> SafetyPred ts a
mkSafePred p
  | isSafetyPredicate p = Right p
  | otherwise = Left $ SafetyError "Predicate is a safety property."

mkBinaryOp :: (Pred ts a -> Pred ts a -> Pred ts a) -> Pred ts a -> Pred ts a -> SafetyPred ts a
mkBinaryOp op p q = do
  p' <- mkSafePred p
  q' <- mkSafePred q
  return (op p' q')

mkAnd, mkOr, mkImplies :: Pred ts a -> Pred ts a -> SafetyPred ts a
mkAnd = mkBinaryOp And
mkOr = mkBinaryOp Or
mkImplies = mkBinaryOp Implies

mkUnaryOp :: (Pred ts a -> Pred ts a) -> Pred ts a -> SafetyPred ts a
mkUnaryOp op = mkSafePred . op

mkNext, mkNow, mkTautology, mkContradiction :: Pred ts a -> SafetyPred ts a
mkNext = mkUnaryOp id
mkNow = mkUnaryOp id
mkTautology = mkUnaryOp id
mkContradiction = mkUnaryOp id

mkLivenessOp :: String -> Pred ts a -> SafetyPred ts a
mkLivenessOp op _ = Left $ SafetyError ("The '" ++ op ++ "' operator cannot be constructed in a safety property.")

mkAlways, mkUntil, mkEventually, mkRelease :: Pred ts a -> SafetyPred ts a
mkAlways = mkLivenessOp "Always"
mkUntil = mkLivenessOp "Until"
mkEventually = mkLivenessOp "Eventually"
mkRelease = mkLivenessOp "Release"