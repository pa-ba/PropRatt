{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use map once" #-}

module Name.Properties
  ( prop_map_id,
    prop_map_associative,
    prop_map_size,
    prop_map_id_Int,
    prop_map_id_Float,
    prop_is_stuttering,
    stuttering,
    prop_sig_is_later_sig_after_tick_on_later_sig,
    getLater,
  )
where

import AsyncRattus.InternalPrimitives
import AsyncRattus.Signal
import qualified Data.IntSet as IntSet
import Name.AsyncRat (aRatSwitch, aRatZip)
import Name.Generators ()
import Name.Utilities
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)

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
isStuttering :: Sig Int -> Sig Int -> Bool
isStuttering (x ::: Delay clx fx) (y ::: Delay cly fy)
  | x /= y = False
  | IntSet.null union = True
  | IntSet.member smallest clx && IntSet.member smallest cly =
      isStuttering (fx (InputValue smallest ())) (fy (InputValue smallest ()))
  | IntSet.member smallest clx =
      isStuttering (fx (InputValue smallest ())) (y ::: Delay cly fy)
  | otherwise =
      isStuttering (x ::: Delay clx fx) (fy (InputValue smallest ()))
  where
    union = IntSet.union clx cly
    smallest = IntSet.findMin union

stuttering :: Sig Int -> Sig Int -> [Int]
stuttering (x ::: Delay clx fx) (y ::: Delay cly fy)
  | x /= y = []
  | IntSet.null union = []
  | IntSet.member smallest clx && IntSet.member smallest cly =
      x : stuttering (fx (InputValue smallest ())) (fy (InputValue smallest ()))
  | IntSet.member smallest clx =
      x : stuttering (fx (InputValue smallest ())) (y ::: Delay cly fy)
  | otherwise =
      y : stuttering (x ::: Delay clx fx) (fy (InputValue smallest ()))
  where
    union = IntSet.union clx cly
    smallest = IntSet.findMin union

prop_is_stuttering :: Sig Int -> Sig Int -> Bool
prop_is_stuttering a b = do
  let zipped = aRatZip a b
  let stripped = first zipped
  isStuttering a stripped

-- Switch
sigAEqualsSigB :: Sig Int -> Sig Int -> Bool
sigAEqualsSigB (x ::: Delay clx fx) (y ::: Delay cly fy)
  | x /= y = False
  | IntSet.null union = True
  | otherwise = sigAEqualsSigB (fx (InputValue smallest ())) (fy (InputValue smallest ()))
  where
    union = IntSet.union clx cly
    smallest = IntSet.findMin union

isTailEqualToLaterSignal :: Sig Int -> O (Sig Int) -> Bool
isTailEqualToLaterSignal (x ::: Delay clx fx) (Delay cly fy)
  | IntSet.null union = True
  | IntSet.member smallest cly = sigAEqualsSigB (fx (InputValue smallest ())) (fy (InputValue smallest ())) 
  | otherwise = isTailEqualToLaterSignal (fx (InputValue smallest ())) (Delay cly fy)
  where
    union = IntSet.union clx cly
    smallest = IntSet.findMin union

getLater:: Sig Int -> O(Sig Int)
getLater (x ::: xs) = xs

prop_sig_is_later_sig_after_tick_on_later_sig :: Sig Int -> Sig Int -> Bool
prop_sig_is_later_sig_after_tick_on_later_sig startSignal laterSignal  = do
  let later = getLater laterSignal
  let switched = aRatSwitch startSignal later
  isTailEqualToLaterSignal switched later


-- prop_is_switched :: Sig Int -> Sig Int -> Bool
-- prop_is_switched a b =
-- let c = switch a b
-- case c is equal to a, then at some point equal to b = true
-- (x :: xs) x == a
-- case c is equal to b, then at ssome point equal to b = true
-- otherwise false

-- eksempel
-- xs :           1 2 3
-- ys :               0 7 8
-- switch xs ys : 1 2 0 7 8