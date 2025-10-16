{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Test.QuickCheck
import PropRatt
import AsyncRattus.Strict
import AsyncRattus.Signal
import AsyncRattus.InternalPrimitives
import Prelude hiding (map, const, zipWith, zip, filter, getLine, putStrLn,null, max)
import qualified Data.IntSet as IntSet
import AsyncRattus.Plugin.Annotation


{-# ANN module AllowLazyData #-}

{-# ANN everySig2Sig AllowRecursion #-}
everySig2Sig :: O (Sig ())
everySig2Sig = Delay (IntSet.fromList [2]) (\_ -> () ::: everySig2Sig)

nats :: O (Sig ()) -> (Int :* Int) -> Sig (Int :* Int)
nats later (n :* max) = stop
    (box (\ (n' :* max') -> n' >= max'))
    (scanAwait (box (\ (n' :* max') _ -> (n' + 1) :* max')) (n :* max) later)

resetTuple :: (Int :* Int) -> (Int :* Int)
resetTuple (_ :* max) = (0 :* max)

setMax :: Int -> (Int :* Int) -> (Int :* Int)
setMax max' (n :* _) = ((min n max') :* max')

timerState :: Sig () -> Sig Int -> Sig (Int :* Int)
timerState (_ ::: rr) sliderSig@(_ ::: ss) =
    let     resetSig    = mapAwait (box (\ _ -> resetTuple)) rr
            currentMax  = current sliderSig
            setMaxSig   = mapAwait (box setMax) ss
            inputSig    = interleave (box (.)) resetSig setMaxSig
            inputSig'   = mapAwait (box ((nats everySig2Sig) .)) inputSig
            counterSig  = switchR ((nats everySig2Sig) (0 :* currentMax)) inputSig'
    in counterSig

prop_counterSigAlwaysLessThanMax :: Property
prop_counterSigAlwaysLessThanMax = forAll genDouble $ \(reset, slider) ->
        let counterSig  = timerState reset slider
            state       = prepend counterSig $ prepend reset $ singletonH slider
            predicate   = G $ Now ((fst' <$> Idx Sig1) |<=| (snd' <$> Idx Sig1))
            result      = evaluate predicate state
        in counterexample (show state) result
  where
    genDouble = do
      slider <- (arbitrarySigWith 100 (chooseInt (0, 100)) :: Gen (Sig Int))
      reset <- (arbitrarySigWeighted 100 :: Gen (Sig (())))
      return (reset, slider)

prop_maxAlwaysEqualsMax :: Property
prop_maxAlwaysEqualsMax = forAll genDouble $ \(reset, slider) ->
        let counterSig  = timerState reset slider
            state       = prepend counterSig $ prepend reset $ singletonH slider
            predicate   = G $ Now ((Idx Sig3) |==| (snd' <$> Idx Sig1))
            result      = evaluate predicate state
        in counterexample (show state) result
  where
    genDouble = do
      slider <- (arbitrarySigWith 100 (chooseInt (0, 100)) :: Gen (Sig Int))
      reset <- (arbitrarySigWeighted 100 :: Gen (Sig (())))
      return (reset, slider)

-- Concurrently resetting and dragging slider yields a reset signal with max value from slider.
prop_concurrentResetAndSlider :: Property
prop_concurrentResetAndSlider = forAll genDouble $ \(reset, slider) ->
        let counterSig  = timerState reset slider
            state       = prepend counterSig $ prepend reset $ singletonH slider
            predicate   = G $ 
                ((And (Now ((Tick Sig2))) (Now ((Tick Sig3)))) :=>
                ((Now (((Idx Sig3)) |==| (snd' <$> Idx Sig1)))
                `And`
                (Now ((Pure 0) |==| (fst' <$> Idx Sig1)))))
            result      = evaluate predicate state
        in counterexample (show state) result
  where
    genDouble = do
      slider <- (arbitrarySigWith 100 (chooseInt (0, 100)) :: Gen (Sig Int))
      reset <- (arbitrarySigWeighted 100 :: Gen (Sig (())))
      return (reset, slider)

prop_timerIsStrictlyMonotonicallyIncreasing :: Property
prop_timerIsStrictlyMonotonicallyIncreasing = forAll genDouble $ \(reset, slider) ->
        let counterSig  = timerState reset slider
            state       = prepend counterSig $ prepend reset $ singletonH slider
            predicate   = G $ X $
                (((Now (Tick Sig1)) `And` ((Not (Now (Tick Sig2)) `And` (Not (Now (Tick Sig3)))))) :=>
                (Now (((fst' <$> (Idx Sig1)) |>| (fst' <$> (Idx (Prev Sig1)))))))
            result      = evaluate predicate state
        in counterexample (show state) result
  where
    genDouble = do
      slider <- (arbitrarySigWith 100 (chooseInt (0, 100)) :: Gen (Sig Int))
      reset <- (arbitrarySigWeighted 100 :: Gen (Sig (())))
      return (reset, slider)

-- The initial state is set correctly.
prop_init :: Property
prop_init = forAll genDouble $ \(reset, slider) ->
        let counterSig  = timerState reset slider
            state       = prepend counterSig $ prepend reset $ singletonH slider
            predicate   = Now ((fst' <$> (Idx Sig1)) |==| (Pure 0)) `And` (Now ((snd' <$> (Idx Sig1)) |==| (Idx Sig3)))
            result      = evaluate predicate state
        in counterexample (show state) result
  where
    genDouble = do
      slider <- (arbitrarySigWith 100 (chooseInt (0, 100)) :: Gen (Sig Int))
      reset <- (arbitrarySigWeighted 100 :: Gen (Sig (())))
      return (reset, slider)

-- If the counter signal hits the max value it remains at the max value until it is reset or slider has been moved.
prop_counterSigStaysAtMaxValue :: Property
prop_counterSigStaysAtMaxValue = forAllShrink genDouble shrink $ \(reset, slider) ->
        let counterSig  = timerState reset slider
            state       = prepend counterSig $ prepend reset $ singletonH slider
            predicate   = G $
                
                    ((Now ((fst' <$> (Idx Sig1)) |==| (snd' <$> (Idx Sig1)))) :=>
                    (X $ (Now ((fst' <$> (Idx Sig1)) |==| (fst' <$> (Idx (Prev Sig1)))))
                    `U`
                    ((Now (Tick Sig2)) `Or` (Now (Tick Sig3)))))
            result      = evaluate predicate state
        in counterexample (show state) result
  where
    genDouble = do
      slider <- (arbitrarySigWith 100 (chooseInt (0, 100)) :: Gen (Sig Int))
      reset <- (arbitrarySigWeighted 100 :: Gen (Sig (())))
      return (reset, slider)

prop_counterSigAlwaysTicks :: Property
prop_counterSigAlwaysTicks = forAll genDouble $ \(reset, slider) ->
        let counterSig  = timerState reset slider
            state       = prepend counterSig $ prepend reset $ singletonH slider
            predicate   = G $ Now (Tick Sig1) `And` (X $ Now (Tick Sig1))
            result      = evaluate predicate state
        in counterexample (show state) result
  where
    genDouble = do
      slider <- (arbitrarySigWith 100 (chooseInt (0, 100)) :: Gen (Sig Int))
      reset <- (arbitrarySigWeighted 100 :: Gen (Sig (())))
      return (reset, slider)

main :: IO ()
main = do
    quickCheck prop_counterSigAlwaysLessThanMax
    quickCheck prop_maxAlwaysEqualsMax
    quickCheck prop_concurrentResetAndSlider
    quickCheck prop_timerIsStrictlyMonotonicallyIncreasing
    quickCheck prop_init
    quickCheck prop_counterSigStaysAtMaxValue
    quickCheck prop_counterSigAlwaysTicks