
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
import Test.QuickCheck (generate)
import AsyncRattus (box, unbox)
import AsyncRattus.Signal (filter, Sig ((:::)))
import PropRatt.LTL
import PropRatt.Utilities 
import AsyncRattus.Channels (setOutput, startEventLoop, Producer (getCurrent, getNext), getNext, getCurrent)
import System.Exit (exitSuccess)
import AsyncRattus.Strict
import PropRatt.Value (Value)


main :: IO ()
main = do
    print "hej"
    let gSig  = makeGrowthSig mkNats
    let state = (prepend gSig $ flatten (HCons gSig HNil)) :: Sig (HList [Value Float, Value Float])
    print state

