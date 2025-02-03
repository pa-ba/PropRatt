module PropRatt.PropRatt.LTL where


data Predicate a where
    Tautology :: Predicate a
    Contradiction :: Predicate a
    Atom :: (a -> Bool) -> Predicate a
    Not :: Predicate a -> Predicate a
    And :: Predicate a -> Predicate a -> Predicate a
    Or :: Predicate a -> Predicate a -> Predicate a
    Until :: Predicate a -> Predicate a -> Predicate a
    WUntil :: Predicate a -> Predicate a -> Predicate a
    NextTime :: Predicate a -> Predicate a
    Implies :: Predicate a -> Predicate a -> Predicate a
    Always :: Predicate a -> Predicate a
    Eventually :: Predicate a -> Predicate a
    After :: Int -> Predicate a -> Predicate a
    Release :: Predicate a -> Predicate a -> Predicate a



evaluateLTL :: Int -> Predicate a -> [a] -> Bool
evaluateLTL amountOfPredCap formulae ls@(x:xs) = amountOfPredCap <= 0
     || case formulae of
         Tautology       -> True
         Contradiction   -> False
         Atom phi        -> phi x
         Not phi         -> not (evaluate phi ls)
         And phi psi     -> evaluate phi ls && evaluate psi ls
         Or phi psi      -> evaluate phi ls || evaluate psi ls
         Until phi psi   -> evaluate psi ls || (evaluate phi ls && evaluateNext (phi `Until` psi) xs) -- U (Until)
         WUntil phi psi  -> ? -- W (Weak-Until)
         NextTime phi    -> evaluateNext phi xs -- X (Next)
         Implies phi psi -> not (evaluate phi ls && not (evaluate psi ls)) -- (phi: True, psi: False) = False (else True)
         Always phi      -> evaluate phi ls && evaluateNext phi xs -- G (In all future states)
         Eventually phi  -> evaluate phi ls || evaluateNext phi xs -- F (in some future timestep)
         After i phi     -> evaluateNext phi xs -- Wrong
         Release phi psi -> not (evaluate ((not phi) `Until` (not psi)) ls) -- R (Release)
     where
         evaluateNext = evaluateLTL (amountOfPredCap - 1)
         evaluate = evaluateLTL amountOfPredCap
