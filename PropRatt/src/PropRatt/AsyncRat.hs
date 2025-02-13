{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

-- AsyncRattus code goes here. 
-- The code is type checked by the AsyncRattus compiler plugin.

module PropRatt.AsyncRat (
    aRatZip,
    aRatSwitch,
    aRatParallel,
    mkSig,
) where


import AsyncRattus.Signal hiding (mkSig)
import AsyncRattus.Strict
import AsyncRattus.InternalPrimitives
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)
import PropRatt.Value (Value(..))

aRatZip :: Sig Int -> Sig Int -> Sig (Int :* Int)
aRatZip a b = zip a b

aRatSwitch :: Sig a -> O(Sig a) -> Sig a
aRatSwitch a o = switch a o

aRatParallel :: Sig a -> Sig b -> Sig (Maybe' a :* Maybe' b) -- Sig (Current a) 
aRatParallel a b = parallel a b

mkSig :: Sig a -> Sig [Value a]
mkSig xs = map (box (\x -> [Current (Just' x) x])) xs

flattenSig :: [Sig a] -> Sig [Value a]
flattenSig sigs =
  let current = [ Current (Just' x) x | (x ::: _) <- sigs ] -- [Value a] denne linje rigtig?
      tails   = [ xs | (_ ::: xs) <- sigs ] -- [0 (Sig a)] funky
  in current ::: delay (flattenSig tails)

  --let valSig@([xvs : xsvs] ::: Delay clvs fvs) = mkSig x 

  -- mkSig to get sig of list of as
  -- deconstruct, to take head of list, append to mother list, and append tail of recursisve call on rest
  
  -- ::: delay (box (flattenSig xs))

-- appendSig :: Sig a -> Sig [Value a] -> Sig [Value a]
-- appendSig sigx ls = 
--   -- Parallel call here :D 
--   let new = mkSig sigx
--   helper new ls

-- helper :: Sig [Value a] -> Sig [Value a] -> Sig [Value a]
-- helper sigx@(x ::: xs) sigl@(l ::: ls) = case l of
--   [] -> sigx
--   _ -> l:x ::: select xs ls of 
--     Fst xs' _ -> helper xs' sigl
--     Snd _ ls' -> helper sigx ls'
--     Both xs' ls' -> helper xs' ls' 


-- Sig []
-- mkCurrentSig :: (Stable a, Stable b) => Sig a -> Sig b -> Sig (Current a :* Current b)
-- mkCurrentSig a b =
--   zipWith combine (parallel a b) (zip a b)
--   where
--     combine = box (\(ma :* mb) (aVal :* bVal) -> Current ma aVal :* Current mb bVal)

-- mkCurrentSigSingle :: (Stable a) => Sig a -> Sig (Current a)
-- mkCurrentSigSingle xs = map (box (\a -> Current (Just' a) a)) xs

-- aRatParallelAndZip :: (Stable a, Stable b) => Sig a -> Sig b -> Sig ((Maybe' a :* Maybe' b) :* (a :* b))
-- aRatParallelAndZip a b = zip (parallel a b) (zip a b)