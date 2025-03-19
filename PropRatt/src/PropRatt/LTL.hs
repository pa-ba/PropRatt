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
    SafetyPred,
    (|+|),
    (|-|),
    (|*|),
    (|<|),
    (|>|),
    (|==|)
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
  Now           :: (a ~ Bool) => Atom ts a -> Pred ts a
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


-- Can we merge atom and lookup..?
data Atom (ts :: [Type]) (t :: Type) where
  Pure :: t -> Atom ts t
  Apply :: Atom ts (t -> r) -> Atom ts t -> Atom ts r
  Index :: Lookup ts t -> Atom ts t

data Lookup (ts :: [Type]) (t :: Type) where
  Previous :: Lookup ts t -> Lookup ts t
  First :: Lookup (Value t ': x) t
  Second :: Lookup (x1 ': Value t ': x2) t
  Third :: Lookup (x1 ': x2 ': Value t ': x3) t
  Fourth :: Lookup (x1 ': x2 ': x3 ': Value t ': x4) t
  Fifth :: Lookup (x1 ': x2 ': x3 ': x4 ': Value t ': x5) t
  Sixth :: Lookup (x1 ': x2 ': x3 ': x4 ': x5 ': Value t ': x6) t
  Seventh :: Lookup (x1 ': x2 ': x3 ': x4 ': x5 ': x6 ': Value t ': x7) t
  Eigth :: Lookup (x1 ': x2 ': x3 ': x4 ': x5 ': x6 ': x7 ': Value t ': x8) t
  Ninth :: Lookup (x1 ': x2 ': x3 ': x4 ': x5 ': x6 ': x7 ': x8 ': Value t ': x9) t

instance Functor (Atom ts) where
  fmap f (Pure x)     = Pure (f x)
  fmap f (Apply g x)  = Apply (fmap (f .) g) x
  fmap f (Index lu)   = Apply (Pure f) (Index lu) -- huh

instance Applicative (Atom ts) where
    pure = Pure
    Pure f <*> x = fmap f x
    Apply f g <*> x = Apply (Apply f g) x

(|+|) :: (Applicative f, Num b) => f b -> f b -> f b
x |+| y = (+) <$> x <*> y
(|-|) :: (Applicative f, Num b) => f b -> f b -> f b
x |-| y = (-) <$> x <*> y
(|*|) :: (Applicative f, Num b) => f b -> f b -> f b
x |*| y = (*) <$> x <*> y
(|<|) :: (Applicative f, Ord a) => f a -> f a -> f Bool
x |<| y = (<) <$> x <*> y
(|>|) :: (Applicative f, Ord a) => f a -> f a -> f Bool
x |>| y = (>) <$> x <*> y
(|==|) :: (Applicative f, Eq a) => f a -> f a -> f Bool
x |==| y = (==) <$> x <*> y

evalAtom :: Atom ts t -> HList ts -> Atom ts t
evalAtom atom hls = case atom of
  Pure x -> Pure x 
  Apply f x ->
      case (evalAtom f hls, evalAtom x hls) of
        (Pure f' , Pure x') -> Pure (f' x')
        _ -> error "Atomic statement cannot be partially applied"
  Index lu -> 
    let m = evalLookup lu hls
    in case m of 
      Just' (Current b (h :! t)) -> Pure h
      Just' (Current b Nil) -> error "No History. Buhu :)"
      Nothing' -> error "No Value, Extra buhu:)" -- Should not be reachable??

evalLookup :: Lookup ts t -> HList ts -> Maybe' (Value t)
evalLookup lu hls = case lu of
  Previous lookup ->
    -- please make Maybe' a monad instance :)
    let m = evalLookup lookup hls
    in case m of
      Just' (Current b history) ->
        case history of
          _ :! xs -> Just' (Current b xs)
          Nil    -> Nothing'
      Nothing' -> Nothing' -- Should not be reachable??
  First         -> Just' (first hls)
  Second        -> Just' (second hls)
  Third         -> Just' (third hls)
  Fourth       -> Just' (fourth hls)
  Fifth        -> Just' (fifth hls)
  Sixth        -> Just' (sixth hls)
  Seventh      -> Just' (seventh hls)
  Eigth        -> Just' (eigth hls)
  Ninth        -> Just' (ninth hls)

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
        After n phi      -> if n <= 0 then eval phi sig else evaluateNext (After (n - 1) phi) sig
  where
    evaluateNext = evaluate' (timestepsLeft - 1)
    eval = evaluate' timestepsLeft
    smallest = IntSet.findMin
    advance = f (InputValue (smallest cl) ())

evaluate :: (Ord a) => Pred ts a -> Sig (HList ts) -> Bool
evaluate = evaluate' 5


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