{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# LANGUAGE TypeApplications, FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.QuickCheck
import PropRatt.Arbitrary
import PropRatt.Core
import PropRatt.HList
import PropRatt.Signal

prop_shouldAddToHList :: Property
prop_shouldAddToHList = forAll (generateSignals @[Int, Int]) $ \intSignals ->
    let flat        = flatten intSignals
        before      = hlistLen flat
        state       = prepend (first intSignals) $ flatten intSignals
        after       = hlistLen state
        result      = (before + 1) == after
    in result

main :: IO ()
main = do
    quickCheck prop_shouldAddToHList