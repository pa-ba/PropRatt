
{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith, take)
import PropRatt.AsyncRat
import PropRatt.Generators ()
import AsyncRattus (box, unbox)
import AsyncRattus.Signal (filter, Sig ((:::)), const, map, scan)
import PropRatt.LTL
import PropRatt.Utilities 
import System.Exit (exitSuccess)
import AsyncRattus.Strict
import AsyncRattus.InternalPrimitives
import PropRatt.RatUtils
import Test.QuickCheck
import PropRatt.HList

cap :: Sig a -> Int -> Sig a
cap s@(x ::: xs) 1 = x ::: never
cap (x ::: xs) n = x ::: delay (cap (adv xs) (n-1))

main :: IO ()
main = do
    print "hej"
    let sig = cap (scan (box (+)) 1 (mkSigOne :: Sig Int)) 21
    print sig
    print "-------------------------------------------------\n"
    print "should be 10"
    --print (sigLength sig `div` 2)
    --print (take (sigLength sig `div` 2) sig)
    intSigs <- generate (generateSignals @[Int, Int])
    
    print "-------------------------------------------------\n"
    let h = halve intSigs
    print (h)
    print (halve h)
    print (halve (halve h))
    print (halve (halve (halve h)))
    print (shrinkHlistOfSig intSigs)

    let state       = flatten h
        predicate   = Always $ Now ((Ticked First) |==| (Pure True)) `And` (Next $ Not $ Now ((Ticked First) |==| (Pure True)))
        result      = evaluate predicate state
    print (result)




