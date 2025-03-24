{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# HLINT ignore "Redundant bracket" #-}

import Test.QuickCheck
import PropRatt.LTL 
import PropRatt.Generators
import PropRatt.Value
import PropRatt.AsyncRat
import AsyncRattus.InternalPrimitives
import Prelude hiding (zip, map, filter)
import AsyncRattus.Signal
import AsyncRattus.Strict
import PropRatt.Utilities (mkSigOne, getLater)
import Test.QuickCheck.Monadic (run, assert, pick, monadicIO)
import PropRatt.AsyncRat (prependLater)
import PropRatt.LTL (checkScope)

prop_interleave :: Property
prop_interleave = forAll (generateSignals @[Int, Int]) $ \intSignals ->
    let interleavedSig  = interleave (box (+)) (getLater $ first intSignals) (getLater $ second intSignals)
        notALaterSig    = 0 ::: interleavedSig
        state           = prepend notALaterSig $ flatten intSignals
        predicate       = Next (Always (((Now ((Index First) |==| (Index Second))) 
                                        `Or`
                                        (Now ((Index First) |==| (Index Third))))
                                        `Or`
                                        (Now (((Index Second) |+| (Index Third)) |==| (Index First)))))
        result          = evaluate predicate state
    in result

-- Jump property (value is either equal to the original signal or equal to 10 (which is the number of the signal of the dummy function))
prop_jump :: Property
prop_jump = forAll (generateSignals @Int) $ \intSignals ->
   let  jumpFunc    = box (\n -> if n > 10 then Just' mkSigOne else Nothing')
        jumpSig     = jump jumpFunc (first intSignals)
        state       = prepend jumpSig $ flatten intSignals
        predicate   = (Always (
                        (Now ((Index First) |==| (Index Second)))
                        `Or` 
                        (Now ((Index First) |==| (Pure 1)))))
        result      = evaluate predicate state
    in result

-- prefix sum are monotonically increasing
-- only holds for nat numbers.. do we need another gen sig?

prop_scan :: Property
prop_scan =  forAll (generateSignals @Int) $ \intSignals -> 
    let prefixSum   = scan (box (+)) 0 (first intSignals)
        state       = prepend prefixSum $ flatten intSignals
        predicate   = Next (Always (Now ((Index (Previous First)) |<| (Index First))))
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
        predicate   = Next (Always (Now ((Index First) |==| (Index (Previous Second)))))
        result      = evaluate predicate state
    in result

-- false positive, does not test anything useful since the stopped signal never ticks
prop_stop :: Property
prop_stop = forAll (generateSignals @Bool) $ \intSignals ->
    let stopped     = stop (box (id)) (first intSignals)
        state       = prepend stopped $ flatten intSignals
        predicate   = (Always ((Now ((Index First) |==| (Pure True))) `Implies` (Next (Now ((Index First) |==| (Pure True))))))
        result      = evaluate predicate state
    in result

-- A zipped signal (first signal) always has fst' values from second signal and snd' values from third signal
prop_zip :: Property
prop_zip = forAll (generateSignals @[Int, Int]) $ \intSignals -> 
    let s1          = zip (first intSignals) (second intSignals)
        state       = prepend s1 $ flatten intSignals
        predicate   = (Always (Now ((fst' <$> (Index First)) |==| (Index Second))) `And` (Now ((snd' <$> (Index First)) |==| (Index Third))))
        result      = evaluate predicate state
    in result

prop_filter :: Property
prop_filter = forAll (generateSignals @Int) $ \charSignals ->
  let filtered      = filter'' (box (>= 10)) (first charSignals)
      state         = prepend filtered $ flatten charSignals
      predicate     = Always (Now ((Index First) |>| ((Pure (Just' 9)))))
      result        = evaluate predicate state
  in result

prop_ticked :: Property
prop_ticked = forAll (generateSignals @Int) $ \charSignals ->
  let filtered      = filter'' (box (>= 10)) (first charSignals)
      state         = prepend filtered $ flatten charSignals
      predicate     = Always (Now ((Index (Ticked First)) |==| ((Pure True))))
      result        = evaluate predicate state
  in result

prop_trigger_maybe :: Property
prop_trigger_maybe = forAll (generateSignals @[Int, Int]) $ \charSignals ->
  let triggered      = triggerMaybe (box (+)) (first charSignals) (second charSignals)
      state         = prepend triggered $ flatten charSignals
      predicate     = Always (Implies (Now (Index (Ticked Second) |==| (Pure True))) (Now (Index (Ticked First) |==| (Pure True))))
      result        = evaluate predicate state
  in result

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