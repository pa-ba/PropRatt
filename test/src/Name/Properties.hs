{-# LANGUAGE TypeOperators #-}

module Name.Properties
  ( prop_map_id,
    prop_map_associative,
    prop_map_size,
    prop_map_id_Int,
    prop_map_id_Float,
    prop_zip_isStuttering,
    strip,
    prop_zip_then_strip,
    prop_zip_then_strip_sig,
    prop_zip_is_stuttering_sig,
  )
where

import AsyncRattus.InternalPrimitives
import AsyncRattus.Signal
import AsyncRattus.Strict
import Name.Generators
import Name.ARat
import Name.Utilities
import Test.QuickCheck
import qualified Data.IntSet as IntSet
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)
import Data.Bool (Bool(True))


strip :: Sig (Int :* Int) -> Sig Int
strip a = do
  let boxed = Box fst'
  map boxed a

instance Stable Int

-- 1.
prop_map_id :: (Eq a) => Sig a -> Bool
prop_map_id sig = sig == map (box id) sig

prop_map_id_Int :: Sig Int -> Bool
prop_map_id_Int = prop_map_id

prop_map_id_Float :: Sig Float -> Bool
prop_map_id_Float = prop_map_id

-- 2.
-- Property map is associative, f after g. map f sig (map g sig) == map (g . f) sig
-- prop_map_associative :: (Eq a, Arbitrary a, Show a) => Positive Int -> Sig a -> Bool

prop_map_associative :: Box (Int -> Int) -> Sig Int -> Bool
prop_map_associative f sig = do
  let composed = unbox f . unbox f
  map f (map f sig) == map (box composed) sig

-- 3.
-- Property map does not change the size
-- prop_map_size :: (Eq a, Arbitrary a, Show a) => Positive Int -> Sig a -> Bool

prop_map_size :: Box (Int -> Int) -> Sig Int -> Bool
prop_map_size f sig =
  sizeSig sig 0 == sizeSig (map f sig) 0


-- Prperties for Zip method:

-- List version of isStuttering:
prop_zip_isStuttering :: [Int] -> [Int] -> Bool
prop_zip_isStuttering a b
  | length b < length a = False
  | otherwise = prop_zip_isStuttering' a b 0

prop_zip_isStuttering' :: [Int] -> [Int] -> Int -> Bool
prop_zip_isStuttering' _ [] _ = True
prop_zip_isStuttering' [] (_ : _) _ = False
prop_zip_isStuttering' (x : xs) (y : ys) n
  | n == 2 = False
  | x == y = prop_zip_isStuttering' (x : xs) ys 0
  | otherwise = prop_zip_isStuttering' xs (y : ys) (n + 1)

prop_zip_then_strip :: Sig Int -> Sig Int -> Bool
prop_zip_then_strip a b = do
  let zipped = prop_zip_zipped a b
  let stripped = strip zipped
  let a' = takeSigExhaustive a
  let stripped' = takeSigExhaustive stripped
  prop_zip_isStuttering a' stripped'

-- Signal version of isStuttering:
prop_zip_is_stuttering_sig :: Sig Int -> Sig Int -> Bool
prop_zip_is_stuttering_sig a b = 
    prop_zip_is_stuttering_sig' a b 0

prop_zip_is_stuttering_sig' :: Sig Int -> Sig Int -> Int -> Bool
prop_zip_is_stuttering_sig' (x ::: Delay clx fx) (y ::: Delay cly fy) n

  -- if we've advanced on the original signal (x) twice without finding a match, we know its not a stuttering
  | n == 2 = False

  -- if the original signals clock is null, it means the signal will never advance again
  | IntSet.null clx = False

  -- if the stuttering signals clock is null, it must mean that we finished going through this signal, without returning false, and thereby the signal is a stuttering
  | IntSet.null cly = True

  -- if x and y are the same, we advance on the possible stuttering signal (y) and reset the n counter
  | x == y = prop_zip_is_stuttering_sig' (x ::: Delay clx fx) (fy (InputValue (pickSmallestClock cly) ())) 0

  -- in case x and y doesn't match, we advance on the original signal (x) and increment the n counter. Counter keeps track of how many times we do not find a match.
  | otherwise = prop_zip_is_stuttering_sig' (fx (InputValue (pickSmallestClock clx) ())) (y ::: Delay cly fy) (n + 1)

prop_zip_then_strip_sig :: Sig Int -> Sig Int -> Sig Int
prop_zip_then_strip_sig a b = do
  let zipped = prop_zip_zipped a b
  strip zipped
