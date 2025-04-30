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
import Data.Text hiding (filter, map, all)

everySecondSig :: O (Sig ())
everySecondSig = mkSig (timer 1000000)

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
prop_alwaysLessThanMax = forAllShrink (generateSignals @[(), Int]) shrinkHls $ \sig ->
        let absSig      = map (box abs) (second sig)
            counterSig  = timerState (first sig) absSig
            state       = prepend counterSig $ flatten sig
            predicate   = Always $ Now ((fst' <$> Index First) |<=| (snd' <$> Index First))
            result      = evaluate predicate state 
        in result

prop_alwaysEqualsMax :: Property
prop_alwaysEqualsMax = forAllShrink (generateSignals @[(), Int]) shrinkHls $ \sig ->
        let absSig      = map (box abs) (second sig)
            counterSig  = timerState (first sig) absSig
            state       = prepend counterSig $ flatten sig
            predicate   = Always $ Now ((abs <$> (Index Third)) |==| (snd' <$> Index First))
            result      = evaluate predicate state 
        in result

-- Concurrently resetting and dragging slider yields reset signal with max value from slider
prop_resetAndSlider :: Property
prop_resetAndSlider = forAllShrink (generateSignals @[(), Int]) shrinkHls $ \sig ->
        let absSig      = map (box abs) (second sig)
            counterSig  = timerState (first sig) absSig
            state       = prepend counterSig $ flatten sig
            predicate   = Always $ Implies 
                (And (Now ((Ticked Second))) (Now ((Ticked Third)))) 
                ((Now ((abs <$> (Index Third)) |==| (snd' <$> Index First)))
                `And`
                (Now ((Pure 0) |==| (fst' <$> Index First))))
            result      = evaluate predicate state 
        in result

prop_timerIsStrictlyMonotonicallyIncreasing :: Property
prop_timerIsStrictlyMonotonicallyIncreasing = forAllShrink (generateSignals @[(), Int]) shrinkHls $ \sig ->
        let absSig      = map (box abs) (second sig)
            counterSig  = timerState (first sig) absSig
            state       = prepend counterSig $ flatten sig
            predicate   = Always $ Next $ 
                Implies 
                ((Now (Ticked First)) `And` ((Not (Now (Ticked Second)) `And` (Not (Now (Ticked Third))))))
                (Now (((fst' <$> (Index First)) |>| (fst' <$> (Index (Previous First))))))
            result      = evaluate predicate state 
        in counterexample (show state) result

prop_initial_state :: Property
prop_initial_state = forAllShrink (generateSignals @[(), Int]) shrinkHls $ \sig ->
        let absSig      = map (box abs) (second sig)
            counterSig  = timerState (first sig) absSig
            state       = prepend counterSig $ flatten sig
            predicate   = Now ((fst' <$> (Index First)) |==| (Pure 0)) `And` (Now ((snd' <$> (Index First)) |==| (abs <$> (Index Third))))
            result      = evaluate predicate state 
        in counterexample (show state) result

prop_max_stutter :: Property
prop_max_stutter = forAllShrink (generateSignals @[(), Int]) shrinkHls $ \sig ->
        let absSig      = map (box abs) (second sig)
            counterSig  = timerState (first sig) absSig
            state       = prepend counterSig $ flatten sig
            predicate   = Always $ 
                Implies 
                    (Now ((fst' <$> (Index First)) |==| (snd' <$> (Index First))))                            -- if counter hits max value
                    ((Next (Now ((fst' <$> (Index First)) |==| (fst' <$> (Index (Previous First))))))         -- it stays there until slider is moved
                    `Until`
                    (Now (Ticked Second)))
            result      = evaluate predicate state 
        in counterexample (show state) result

main :: IO ()
main = do
    -- quickCheck prop_alwaysLessThanMax
    -- quickCheck prop_alwaysEqualsMax
    -- quickCheck prop_resetAndSlider
    -- quickCheck prop_timerIsStrictlyMonotonicallyIncreasing
    -- quickCheck prop_initial_state
    quickCheck prop_max_stutter