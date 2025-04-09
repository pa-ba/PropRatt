
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
import Test.QuickCheck (shrink)
import AsyncRattus (box, unbox)
import AsyncRattus.Signal (filter, Sig ((:::)), const, map, scan)
import PropRatt.LTL
import PropRatt.Utilities 
import System.Exit (exitSuccess)
import AsyncRattus.Strict
import AsyncRattus.InternalPrimitives
import PropRatt.RatUtils

cap :: Sig a -> Int -> Sig a
cap s@(x ::: xs) 0 = x ::: never
cap (x ::: xs) n = x ::: delay (cap (adv xs) (n-1))

main :: IO ()
main = do
    print "hej"
    let sig = cap (scan (box (+)) 1 (mkSigOne :: Sig Int)) 20
    print sig
    print "-------------------------------------------------\n"
    print "should be 10"
    print (take (sigLength sig `div` 2) sig)
    print "-------------------------------------------------\n"
    print (shrink sig)


