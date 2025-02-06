{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Main (main) where
import PropRatt.LTL (Pred (..), evaluateLTL)

main :: IO ()
main = do
    -- print (evaluateLTL (Eventually (Implies (Atom odd) (Next (Atom (== 4))))) [2,2,2,3,4])
    -- print (evaluateLTL (Eventually (Atom odd)) [2,2,2,2,2,2])
    -- print (evaluateLTL (Implies
    --                        (And (Eventually (Atom (odd))) (Always (Atom (>= 20)))) --false
    --                        (Until (Atom (>= 20)) (Atom (== 50)))) -- false
    --                    [0,0,0,23,19])
    {- print (evaluateLTL (Or (
                            Eventually (
                                Implies (Atom id) (Always (Atom id))
                            ))
                            (
                                Until (Not (Atom id)) (Atom id)
                            ))
                            [False,True,True]) -- Pre-order traversal of Fig 3.1, formula: (F (p -> G r) v (neg q U p)) -}

    -- Atoms:
    -- p = Red light is activated
    -- q = Yellow light is activated
    -- r = Green light is activated

    -- States S = {s1,s2,s3,s4}
    -- L(s1) = p
    -- L(s2) = q
    -- L(s3) = r
    -- L(s4) = q

    -- Transitions
    -- s1 -> s2
    -- s2 -> s3
    -- s3 -> s4
    -- s4 -> s1

    let goodList = ["Green", "Yellow", "Red", "Yellow", "Green", "Yellow"]
    let badList = ["Yellow", "Yellow", "Red", "Green", "Green", "Yellow"]
    let badList2 = ["Purple", "Purple", "Purple", "Purple", "Purple"]

    -- Formulae
    -- 1. A least one light is activated at all times (G (p v q v r))
    print (evaluateLTL (Always (Or (Or (Atom (== "Green")) (Atom (== "Yellow"))) (Atom (== "Red")))) goodList)
    -- 2. A light can never have two lights activated at once (requires concurrency, can do this with a list..)
    -- 3. Red light is always followed by a yellow light (G (p -> X q))
    print (evaluateLTL (Always (Implies (Atom (== "Red")) (Next (Atom (== "Yellow"))))) goodList)
    -- 4. Yellow light is always followed by either a green or red light (G (q -> X (p v r)))
    print (evaluateLTL (Always (Implies (Atom (== "Yellow")) (Next (Or (Atom (== "Red")) (Atom (== "Green")))))) goodList)
    -- 5. Green light is always followed by a yellow light (G (r -> X q))
    print (evaluateLTL (Always (Implies (Atom (== "Green")) (Next (Atom (== "Yellow"))))) goodList)
    -- 6. A light eventuall turns green (G F p)
    print (evaluateLTL (Eventually (Atom (== "Green"))) goodList)
    -- 7. A light eventuall turns yellow (G F q)
    print (evaluateLTL (Eventually (Atom (== "Yellow"))) goodList)
    -- 8. A light eventuall turns red (G F r)
    print (evaluateLTL (Eventually (Atom (== "Red"))) goodList)

    print (evaluateLTL (Always (Or (Or (Atom (== "Green")) (Atom (== "Yellow"))) (Atom (== "Red")))) badList2) -- 1
    print (evaluateLTL (Always (Implies (Atom (== "Red")) (Next (Atom (== "Yellow"))))) badList) -- 3
    print (evaluateLTL (Always (Implies (Atom (== "Yellow")) (Next (Or (Atom (== "Red")) (Atom (== "Green")))))) badList) -- 4
    print (evaluateLTL (Always (Implies (Atom (== "Green")) (Next (Atom (== "Yellow"))))) badList) -- 5
    print (evaluateLTL (Always (Eventually (Atom (== "Green")))) badList2) -- 6
    print (evaluateLTL (Always (Eventually (Atom (== "Yellow")))) badList2) -- 7
    print (evaluateLTL (Always (Eventually (Atom (== "Red")))) badList2) -- 8


    --print (evaluateLTLSig (Eventually (Atom (=< 10))) goodList) )




