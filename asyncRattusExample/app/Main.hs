{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
module Main (main) where

import AsyncRattus
import AsyncRattus.Channels
import AsyncRattus.Signal hiding (map)
import Control.Concurrent (forkIO)
import Lib
import Prelude hiding (map, putStrLn)

someFunction :: Box (Int -> Int)
someFunction = box (+ 1)

someSignal :: Int -> Sig Int
someSignal init = scan (box (\n _ -> n + 1)) init everySecondSig

main :: IO ()
main = do
  let theSig = someSignal 0
  mapWithFunc someFunction theSig
