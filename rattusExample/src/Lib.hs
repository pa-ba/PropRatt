{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS -fplugin=Rattus.Plugin #-}
module Lib
  ( map,
    syncStream,
  )
where

import Rattus
import Rattus.Stream hiding (map)
import Prelude hiding (map, print)

-- Map function for synchronous Stream of data
map :: Box (a -> b) -> Str a -> Str b
map f (x ::: xs) = unbox f x ::: delay (map f (adv xs))

-- Stream of natural numbers starting from 'n'
nats :: Int -> Str Int
nats n = n ::: delay (nats (n + 1))

-- Example synchronous stream for testing purposes
syncStream :: Str Int
syncStream = nats 0
