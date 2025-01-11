{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use map once" #-}

module PropRatt.Properties
  ( 
    prop_is_stuttering,
    prop_is_stuttering_failing,
    stuttering,
    prop_eventual_equality,
    prop_eventual_equality_failing,
    getLater
  )
where

import AsyncRattus.InternalPrimitives
import AsyncRattus.Signal
import qualified Data.IntSet as IntSet
import PropRatt.AsyncRat (aRatSwitch, aRatZip)
import PropRatt.Generators ()
import PropRatt.Utilities
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)

-- Map
prop_map_id :: (Eq a) => Sig a -> Bool
prop_map_id sig = sig == map (box id) sig

prop_map_id_Int :: Sig Int -> Bool
prop_map_id_Int = prop_map_id

prop_map_id_Float :: Sig Float -> Bool
prop_map_id_Float = prop_map_id

prop_map_associative :: Box (Int -> Int) -> Sig Int -> Bool
prop_map_associative f sig = do
  let composed = unbox f . unbox f
  map f (map f sig) == map (box composed) sig

prop_map_size :: Box (Int -> Int) -> Sig Int -> Bool
prop_map_size f sig =
  sizeSig sig 0 == sizeSig (map f sig) 0

-- Zip
prop_is_stuttering :: Sig Int -> Sig Int -> Bool
prop_is_stuttering a b = do
  let zipped = aRatZip a b
  let stripped = first zipped
  isStuttering a stripped

  
prop_is_stuttering_failing :: Sig Int -> Sig Int -> Bool
prop_is_stuttering_failing a b = do
  isStuttering a b

prop_eventual_equality_failing :: Sig Int -> Sig Int -> Bool
prop_eventual_equality_failing startSignal laterSignal = do
  let later = getLater laterSignal
  isEventuallyEqual startSignal later


-- Switch
prop_eventual_equality :: Sig Int -> Sig Int -> Bool
prop_eventual_equality startSignal laterSignal = do
  let later = getLater laterSignal
  let switched = aRatSwitch startSignal later
  isEventuallyEqual switched later
