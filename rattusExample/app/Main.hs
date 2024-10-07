{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS -fplugin=Rattus.Plugin #-}
module Main (main) where

import Control.Concurrent (forkIO)
import Lib
import Rattus
import Rattus.Stream hiding (map)
import Prelude hiding (map, putStrLn)

someFunction :: Box (Int -> Int)
someFunction = box (+ 100)

someStream :: Int -> Str Int
someStream init = scan (box (\n _ -> n + 1)) init syncStream

-- Function to print elements of a Stream, recursively consuming the stream and decrementing the count n, to stop after n elements in the stream
printNStream :: (Show a) => Int -> Str a -> IO ()
printNStream 0 _ = return ()
printNStream n (x ::: xs) = do
  print x
  printNStream (n - 1) (adv xs)

-- Function that maps a function over the stream and prints the resulting stream
main :: IO ()
main = do
  let aStream = map someFunction (someStream 0)
  printNStream 10 aStream