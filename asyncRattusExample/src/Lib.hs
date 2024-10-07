{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
module Lib
  ( mapWithFunc,
    mapWithFunc2,
    everySecondSig,
  )
where

import AsyncRattus
import AsyncRattus.Channels
import AsyncRattus.Signal hiding (map)
import Control.Concurrent (forkIO)
import Prelude hiding (map, putStrLn)

map :: Box (a -> b) -> Sig a -> Sig b
map f (x ::: xs) = unbox f x ::: delay (map f (adv xs))

-- Do we always need a timer here as our clock??
everySecond :: Box (O ())
everySecond = timer 1000000

-- Creates a signal that creates a value every second
everySecondSig :: Sig ()
everySecondSig = () ::: mkSig everySecond

-- nats function generates all natural numbers as a Signal with everySecondSig as a clock
-- Init is the initial value to start incrementing from
nats :: Int -> Sig Int
nats init = scan (box (\n _ -> n + 1)) init everySecondSig

-- We pass in someFunction: The function that is applied to the Signal of all natural numbers.
mapWithFunc :: Box (Int -> Int) -> Sig Int -> IO ()
mapWithFunc someFunction someSignal = do
  let transformedSignal = map someFunction someSignal
  setOutput transformedSignal print
  startEventLoop

-- Same function as mapWithFunc, but with different output type for testing purposes
-- This version returns a signal instead of IO()
mapWithFunc2 :: Box (Int -> Int) -> Sig Int -> Sig Int
mapWithFunc2 someFunction someSignal = do
  let transformedSignal = map someFunction someSignal
  transformedSignal