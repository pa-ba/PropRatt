{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# LANGUAGE TypeApplications, FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.QuickCheck
import PropRatt.LTL
import PropRatt.Generators
import PropRatt.Value
import PropRatt.AsyncRat
import AsyncRattus.InternalPrimitives
import Prelude hiding (zip, map, filter, const)
import AsyncRattus.Signal
import AsyncRattus.Strict
import PropRatt.Utilities
import Data.IntSet as IntSet hiding (map)

instance Stable (Sig Int) where

prop_interleave :: Property
prop_interleave = forAll (generateSignals @[Int, Int]) $ \intSignals ->
    let interleavedSig  = interleave (box (+)) (getLater $ first intSignals) (getLater $ second intSignals)
        notALaterSig    = 0 ::: interleavedSig
        state           = prepend notALaterSig $ flatten intSignals
        predicate       = Next $ Always $ ((Now ((Index First) |==| (Index Second)))
                                        `Or`
                                        (Now ((Index First) |==| (Index Third))))
                                        `Or`
                                        (Now (((Index Second) + (Index Third)) |==| (Index First)))
        result          = evaluate predicate state
    in result

-- Jump property (value is either equal to the original signal or equal to 10 (which is the number of the signal of the dummy function))
prop_jump :: Property
prop_jump = forAll (generateSignals @Int) $ \intSignals ->
   let  jumpFunc    = box (\n -> if n > 10 then Just' (const 1) else Nothing')
        jumpSig     = jump jumpFunc (first intSignals)
        state       = prepend jumpSig $ flatten intSignals
        predicate   = Always $
                        Now ((Index First) |==| (Index Second))
                        `Or`
                        Now ((Index First) |==| (Pure 1))
        result      = evaluate predicate state
    in result

prop_scan :: Property
prop_scan =  forAll (generateSignals @Int) $ \intSignals ->
    let prefixSum   = scan (box (+)) 0 (first intSignals)
        state       = prepend prefixSum $ flatten intSignals
        predicate   = Next $ Always $ Now $ Index (Previous First) |<| (Index First)
        result      = evaluate predicate state
    in counterexample ("must be nat numbers") result

-- A switched signal has values equal to the first signal until its values equal values from the third signal
prop_switchedSignal :: Property
prop_switchedSignal = forAll (generateSignals @[Int, Int]) $ \intSignals ->
    let switched    = switch (first intSignals) (getLater (second intSignals))
        state       = prepend switched $ flatten intSignals
        predicate   = Until (Now ((Index First) |==| (Index Second))) (Now ((Index First) |==| (Index Third)))
        result      = evaluate predicate state
    in result

-- A buffered signal is always one tick behind
prop_buffer :: Property
prop_buffer = forAll (generateSignals @Int) $ \intSignals ->
    let bufferedSig = buffer 10 (first intSignals)
        state       = prepend bufferedSig $ flatten intSignals
        predicate   = Next $ Always $ Now $ (Index First) |==| Index (Previous Second)
        result      = evaluate predicate state
    in result

prop_stop :: Property
prop_stop = forAll (generateSignals @Int) $ \intSignals ->
    let stopped     = stop (box (>100)) (first intSignals)
        state       = prepend stopped $ flatten intSignals
        predicate   = Always $ Implies (Now ((Index First) |>| (Pure 100))) (Always $ Next (Now (Index (Previous First) |==| (Index First))))
        result      = evaluate predicate state
    in result

-- A zipped signal (first signal) always has fst' values from second signal and snd' values from third signal
prop_zip :: Property
prop_zip = forAll (generateSignals @[Int, Int]) $ \intSignals ->
    let s1          = zip (first intSignals) (second intSignals)
        state       = prepend s1 $ flatten intSignals
        predicate   = Always $ Now ((fst' <$> (Index First)) |==| (Index Second)) `And` (Now ((snd' <$> (Index First)) |==| (Index Third)))
        result      = evaluate predicate state
    in result

prop_filter :: Property
prop_filter = forAll (generateSignals @Int) $ \charSignals ->
  let filtered      = filter'' (box (>= 10)) (first charSignals)
      state         = prepend filtered $ flatten charSignals
      predicate     = Always $ Now ((Index First) |>| Pure (Just' 9))
      result        = evaluate predicate state
  in result

prop_ticked :: Property
prop_ticked = forAll (generateSignals @Int) $ \charSignals ->
  let filtered      = filter'' (box (>= 10)) (first charSignals)
      state         = prepend filtered $ flatten charSignals
      predicate     = Always $ Now $ ((Ticked First)) |==| ((Pure True))
      result        = evaluate predicate state
  in result

prop_trigger_maybe :: Property
prop_trigger_maybe = forAll (generateSignals @[Int, Int]) $ \charSignals ->
  let triggered     = triggerMaybe (box (+)) (first charSignals) (second charSignals)
      state         = prepend triggered $ flatten charSignals
      predicate     = Always $ Implies (Now ((Ticked Second) |==| (Pure True))) (Now ((Ticked First) |==| (Pure True)))
      result        = evaluate predicate state
  in result

prop_map_gt :: Property
prop_map_gt = forAll (generateSignals @Int) $ \intSignal ->
    let mapped      = map (box (+1)) (first intSignal)
        state       = prepend mapped $ flatten intSignal
        predicate   = Always $ Now ((Index First) |>| (Index Second))
        result      = evaluate predicate state
    in result

prop_range :: Bool
prop_range =
    let gSig  = makeGrowthSig mkNats
        state = flatten (HCons gSig HNil)
        pred0 = Now ((Index First) |==| (Pure (0.0 :: Float)))
        pred1 = After 1 (Now ((Index First) |<| (Pure 10.0)))
        -- pred2 = After 10 (And ((Now ((Index First) |<| (Pure (100.0 :: Float))))) ((Now ((Index First) |>| (Pure (0.0 :: Float))))))
        -- pred3 = After 70 (Now ((Index First) |>| (Pure 20.0)))
        result = evaluate pred0 state -- && evaluate pred1 state && evaluate pred2 state -- && evaluate pred3 state
    in result

prop_parallel :: Property
prop_parallel = forAll (generateSignals @[Int, Int]) $ \intSignals ->
    let paralleled  = parallel (first intSignals) (second intSignals)
        state       = prepend paralleled $ flatten intSignals
        predicate   = Always $
            Implies (Now (Ticked Third)) (Now (Ticked First))
            `And`
            Implies (Now (Ticked Second)) (Now (Ticked First))
        result      = evaluate predicate state
    in result

prop_isStuttering :: Property
prop_isStuttering = forAll (generateSignals @[Int, Int]) $ \intSignals ->
    let stuttered  = stutter (first intSignals) (second intSignals)
        state       = prepend stuttered $ flatten intSignals
        predicate   = Always $
            Implies (Now (Ticked First)) (Now (Index First |==| Index Second))
            `And`
            Next (Implies (And (Now (Ticked Third)) (Not (Now (Ticked Second)))) (Now (Index (Previous First) |==| Index First)))
        result      = evaluate predicate state
    in result

prop_functionIsMonotonic :: Property
prop_functionIsMonotonic = forAll (generateSignals @Int) $ \intSignals ->
    let mono        = monotonic (first intSignals)
        state       = prepend mono $ flatten intSignals
        predicate   = Always $ Next (Now ((Index First) |>=| (Index (Previous First))))
        result      = evaluate predicate state
    in result

prop_shouldAddToHList :: Property
prop_shouldAddToHList = forAll (generateSignals @[Int, Int]) $ \intSignals ->
    let flat        = flatten intSignals
        before      = hlistLen flat
        state       = prepend (first intSignals) $ flatten intSignals
        after       = hlistLen state
        result      = (before + 1) == after
    in result

prop_singleSignalAlwaysTicks :: Property
prop_singleSignalAlwaysTicks = forAll (generateSignals @Int) $ \intSignal ->
    let state       = flatten intSignal
        predicate   = Always $ Now ((Ticked First) |==| (Pure True))
        result      = evaluate predicate state
    in result

-- Switched signal equals XS until YS has ticked, from then on the value is constant assuming ys has not produced another const signal
prop_switchR :: Property
prop_switchR = forAllShrink (generateSignals @Int) shrink $ \intSignals ->
    let xs                  = first intSignals
        (_ ::: ys)          = scan (box (+)) 1 (const (0 :: Int))
        zs                  = switchR xs (mapAwait (box (\_ -> const)) ys)
        state               = prepend zs $ prependLater ys $ flatten intSignals
        predicate           =   (Now ((Index First) |==| (Index Third)))
                                `Until`
                                ((Now ((Ticked Second) |==| (Pure True)))
                                `And`
                                (Next $ Implies
                                    (Always $ Now ((Ticked Second) |==| (Pure False)))
                                    (Now ((Index (Previous First)) |==| (Index First)))
                                    `And`
                                    (Not $ Now ((Index First) |==| (Index (Previous First)))) -- should be const
                                    ))
        result              = evaluate predicate state
    in result

-- limitation: cannot make property for switchS as it requires a later function with we cant prepend
-- prop_switchS :: Property
-- prop_switchS = forAll (generateSignals @Int) $ \intSignals ->
--     let xs                  = first intSignals
--         (_ ::: ys)          = scan (box (+)) 1 (const (0 :: Int))
--         zs                  = switchS xs (Delay (IntSet.singleton 2) (\ _ _ -> const 100))
--         state               = prepend zs $ prependLater ys $ flatten intSignals
--         predicate           =   (Now ((Index First) |==| (Index Third)))
--                                 `Until`
--                                 ((Now ((Ticked Second) |==| (Pure True)))
--                                 `And`
--                                 (Next $ Implies
--                                     (Always $ Now ((Ticked Second) |==| (Pure False)))
--                                     (Now ((Index First) |==| (Pure 100)))
--                                     `And`
--                                     (Not $ Now ((Index First) |==| (Index (Previous First)))) -- should be const
--                                     ))
--         result              = evaluate predicate state
--     in result

prop_sigLength :: Property
prop_sigLength = forAllShrink ((arbitrarySig 100) :: Gen (Sig Int)) shrink $ \(sig :: Sig Int) ->
        let     state   = flatten (sig %: HNil)
                pred    = Always $ Now ((Ticked First) |==| (Pure False))
                res     = evaluate pred state 
        in res


main :: IO ()
main = do
    quickCheck prop_interleave
    quickCheck prop_switchedSignal
    quickCheck prop_buffer
    quickCheck prop_zip
    quickCheck prop_jump
    quickCheck prop_stop
    quickCheck prop_scan
    quickCheck prop_filter
    quickCheck prop_ticked
    quickCheck prop_trigger_maybe
    quickCheck prop_map_gt
    quickCheck prop_range
    quickCheck prop_parallel
    quickCheck prop_isStuttering
    quickCheck prop_functionIsMonotonic
    quickCheck prop_shouldAddToHList
    quickCheck prop_switchR
    quickCheck prop_singleSignalAlwaysTicks
    quickCheck prop_sigLength
