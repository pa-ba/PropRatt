{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# LANGUAGE TypeOperators #-}

-- AsyncRattus code goes here. 
-- The code is type checked by the AsyncRattus compiler plugin.

module Name.ARat (
    prop_zip_zipped
) where

import AsyncRattus.InternalPrimitives
import AsyncRattus.Signal
import AsyncRattus.Strict
import Prelude hiding (const, filter, getLine, map, null, putStrLn, zip, zipWith)

prop_zip_zipped :: Sig Int -> Sig Int -> Sig (Int :* Int)
prop_zip_zipped a b = zip a b