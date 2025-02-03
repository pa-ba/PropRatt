{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module PropRatt.LTL (
    Pred (..),
    evaluateLTL
)where


data Pred a where
    Tautology :: Pred a
    Contradiction :: Pred a
    Atom :: (a -> Bool) -> Pred a
    Not :: Pred a -> Pred a
    And :: Pred a -> Pred a -> Pred a
    Or :: Pred a -> Pred a -> Pred a
    Until :: Pred a -> Pred a -> Pred a
    WUntil :: Pred a -> Pred a -> Pred a
    NextTime :: Pred a -> Pred a
    Implies :: Pred a -> Pred a -> Pred a
    Always :: Pred a -> Pred a
    Eventually :: Pred a -> Pred a
    After :: Int -> Pred a -> Pred a
    Release :: Pred a -> Pred a -> Pred a



evaluateLTL' :: Int -> Pred a -> [a] -> Bool
evaluateLTL' amountOfPredCap formulae ls@(x:xs) = amountOfPredCap <= 0
    || case formulae of
        Tautology       -> True
        Contradiction   -> False
        Atom phi        -> phi x
        Not phi         -> not (evaluate phi ls)
        And phi psi     -> evaluate phi ls && evaluate psi ls
        Or phi psi      -> evaluate phi ls || evaluate psi ls
        Until phi psi   -> evaluate psi ls || (evaluate phi ls && evaluateNext (phi `Until` psi) xs) -- U (Until)
        NextTime phi    -> evaluateNext phi xs -- X (Next)
        Implies phi psi -> not (evaluate phi ls && not (evaluate psi ls)) -- (phi: True, psi: False) = False (else True)
        Always phi      -> evaluate phi ls && evaluateNext (Always phi) xs -- G (In all future states)
        Eventually phi  -> evaluate phi ls || evaluateNext (Eventually phi) xs -- F (in some future timestep)
    where
        evaluateNext = evaluateLTL' (amountOfPredCap - 1)
        evaluate = evaluateLTL' amountOfPredCap

evaluateLTL :: Pred a -> [a] -> Bool
evaluateLTL = evaluateLTL' 20