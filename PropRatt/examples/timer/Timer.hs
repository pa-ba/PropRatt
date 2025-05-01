{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Use const" #-}


module Main (main) where

import Test.QuickCheck
import PropRatt
import AsyncRattus.Strict
import AsyncRattus.Channels
import AsyncRattus.Signal
import AsyncRattus.InternalPrimitives
import Prelude hiding (map, const, zipWith, zip, filter, getLine, putStrLn,null)
import qualified Data.IntSet as IntSet
import AsyncRattus.Plugin.Annotation

{-# ANN everySecondSig AllowRecursion #-}
everySecondSig :: O (Sig ())
everySecondSig = Delay (IntSet.fromList [7]) (\_ -> () ::: everySecondSig) 

nats :: (Int :* Int) -> Sig (Int :* Int)
nats (n :* max) = stop
    (box (\ (n :* max) -> n >= max)) 
    (scanAwait (box (\ (n :* max) _ -> (n + 1) :* max)) (n :* max) everySecondSig)

reset :: (Int :* Int) -> (Int :* Int)
reset (_ :* max) = (0 :* max)

setMax :: Int -> (Int :* Int) -> (Int :* Int)
setMax max' (n :* _) = ((min n max') :* max')

timerState :: Sig () -> Sig Int -> Sig (Int :* Int)
timerState resetSig@(r ::: rr) sliderSig@(s ::: ss) =
    let     resetSig    = mapAwait (box (\ _ -> reset)) rr
            currentMax  = current sliderSig
            setMaxSig   = mapAwait (box setMax) ss
            inputSig    = interleave (box (.)) resetSig setMaxSig
            inputSig'   = mapAwait (box (nats .)) inputSig
            counterSig  = switchR (nats (0 :* currentMax)) inputSig'
            currentSig  = map (box fst') counterSig
            maxSig      = map (box snd') counterSig
    in counterSig

prop_alwaysLessThanMax :: Property
prop_alwaysLessThanMax = forAll genDouble $ \(reset, slider) ->
        let counterSig  = timerState reset slider
            state       = prepend counterSig $ prepend reset $ singletonH slider
            predicate   = Always $ Now ((fst' <$> Index First) |<=| (snd' <$> Index First))
            result      = evaluate predicate state 
        in counterexample (show state) result
  where
    genDouble = do
      slider <- (arbitrarySigWith 100 (chooseInt (0,100)) :: Gen (Sig Int))
      reset <- (arbitrarySigWeighted 100 :: Gen (Sig (())))
      return (reset, slider)

-- The max value of the counter signal always equals the slider signal
prop_alwaysEqualsMax :: Property
prop_alwaysEqualsMax = forAll genDouble $ \(reset, slider) ->
        let counterSig  = timerState reset slider
            state       = prepend counterSig $ prepend reset $ singletonH slider
            predicate   = Always $ Now ((abs <$> (Index Third)) |==| (snd' <$> Index First))
            result      = evaluate predicate state 
        in counterexample (show state) result
  where
    genDouble = do
      slider <- (arbitrarySigWith 100 (chooseInt (0,100)) :: Gen (Sig Int))
      reset <- (arbitrarySigWeighted 100 :: Gen (Sig (())))
      return (reset, slider)


-- Concurrently resetting and dragging slider yields reset signal with max value from slider
prop_resetAndSlider :: Property
prop_resetAndSlider = forAll genDouble $ \(reset, slider) ->
        let counterSig  = timerState reset slider
            state       = prepend counterSig $ prepend reset $ singletonH slider
            predicate   =Always $ Implies 
                (And (Now ((Ticked Second))) (Now ((Ticked Third)))) 
                ((Now (((Index Third)) |==| (snd' <$> Index First)))
                `And`
                (Now ((Pure 0) |==| (fst' <$> Index First))))
            result      = evaluate predicate state 
        in counterexample (show state) result
  where
    genDouble = do
      slider <- (arbitrarySigWith 100 (chooseInt (0,100)) :: Gen (Sig Int))
      reset <- (arbitrarySigWeighted 100 :: Gen (Sig (())))
      return (reset, slider)


prop_timerIsStrictlyMonotonicallyIncreasing :: Property
prop_timerIsStrictlyMonotonicallyIncreasing = forAll genDouble $ \(reset, slider) ->
        let counterSig  = timerState reset slider
            state       = prepend counterSig $ prepend reset $ singletonH slider
            predicate   = Always $ Next $ 
                Implies 
                ((Now (Ticked First)) `And` ((Not (Now (Ticked Second)) `And` (Not (Now (Ticked Third))))))
                (Now (((fst' <$> (Index First)) |>| (fst' <$> (Index (Previous First))))))
            result      = evaluate predicate state 
        in counterexample (show state) result
  where
    genDouble = do
      slider <- (arbitrarySigWith 100 (chooseInt (0,100)) :: Gen (Sig Int))
      reset <- (arbitrarySigWeighted 100 :: Gen (Sig (())))
      return (reset, slider)

-- The initial state is set correctly
prop_init :: Property
prop_init = forAll genDouble $ \(reset, slider) ->
        let counterSig  = timerState reset slider
            state       = prepend counterSig $ prepend reset $ singletonH slider
            predicate   = Now ((fst' <$> (Index First)) |==| (Pure 0)) `And` (Now ((snd' <$> (Index First)) |==| (Index Third)))
            result      = evaluate predicate state 
        in counterexample (show state) result
  where
    genDouble = do
      slider <- (arbitrarySigWith 100 (chooseInt (0,100)) :: Gen (Sig Int))
      reset <- (arbitrarySigWeighted 100 :: Gen (Sig (())))
      return (reset, slider)

-- If the counter signal hits the max value it remains at the max value until the slider is moved.
prop_counterSigStaysAtMaxValue :: Property
prop_counterSigStaysAtMaxValue = forAllShrink genDouble shrink $ \(reset, slider) ->
        let counterSig  = timerState reset slider
            state       = prepend counterSig $ prepend reset $ singletonH slider
            predicate   = Always $ 
                Implies 
                    (Now ((fst' <$> (Index First)) |==| (snd' <$> (Index First))))                 
                    (Next $ (((Now ((fst' <$> (Index First)) |==| (fst' <$> (Index (Previous First))))))        
                    `Until`
                    ((Now (Ticked Second)) `Or` (Now (Ticked Third)))))
            result      = evaluate predicate state 
        in counterexample (show state) result
  where
    genDouble = do
      slider <- (arbitrarySigWith 100 (chooseInt (0,100)) :: Gen (Sig Int))
      reset <- (arbitrarySigWeighted 100 :: Gen (Sig (())))
      return (reset, slider)


prop_experiment :: Property
prop_experiment = forAll genDouble $ \(reset, slider) ->
        let counterSig  = timerState reset slider
            state       = prepend counterSig $ prepend reset $ singletonH slider
            predicate   = Not $ Always $ (Now (Ticked First) `And` (Next $ Now (Ticked First)))
            result      = evaluate predicate state 
        in counterexample (show state) result
  where
    genDouble = do
      slider <- (arbitrarySigWith 10 (chooseInt (0,100)) :: Gen (Sig Int))
      reset <- (arbitrarySigWeighted 10 :: Gen (Sig (())))
      return (reset, slider)

main :: IO ()
main = do
    quickCheck prop_alwaysLessThanMax
    quickCheck prop_alwaysEqualsMax
    quickCheck prop_resetAndSlider
    quickCheck prop_timerIsStrictlyMonotonicallyIncreasing
    quickCheck prop_init
    quickCheck prop_counterSigStaysAtMaxValue
    quickCheck prop_experiment