{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# LANGUAGE TypeApplications, FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use zipWith" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Use const" #-}

module Main where
    
import Test.QuickCheck
import PropRatt.LTL
import PropRatt.Arbitrary
import PropRatt.Core
import AsyncRattus.InternalPrimitives
import Prelude hiding (zip, map, filter, const)
import AsyncRattus.Signal
import AsyncRattus.Strict
import PropRatt.Utils
import PropRatt.HList

filter'' :: Box (a -> Bool) -> Sig a -> Sig (Maybe' a)
filter'' f (x ::: xs) = if unbox f x
  then Just' x ::: delay (filter'' f (adv xs))
  else Nothing' ::: delay (filter'' f (adv xs))

triggerMaybe :: (Stable a, Stable b) => Box (a -> b -> c) -> Sig a -> Sig b -> Sig (Maybe' c)
triggerMaybe f (a ::: as) bs@(b:::_) = Just' (unbox f a b) ::: triggerAwaitMaybe f as bs

triggerAwaitMaybe :: Stable b => Box (a -> b -> c) -> O (Sig a) -> Sig b -> O (Sig (Maybe' c))
triggerAwaitMaybe f as (b:::bs) = delay (case select as bs of
            Fst (a' ::: as') bs' -> Just' (unbox f a' b) ::: triggerAwaitMaybe f as' (b ::: bs')
            Snd as' bs' -> Nothing' ::: triggerAwaitMaybe f as' bs'
            Both (a' ::: as') (b' ::: bs') -> Just' (unbox f a' b') ::: triggerAwaitMaybe f as' (b' ::: bs'))

mkNats :: (Stable a, Floating a) => Sig a
mkNats = scan (box (\a _ -> a + 1)) (-1) (const 0)

stutter :: (Stable a, Stable b) => Sig a -> Sig b -> Sig a
stutter xs ys = map (box fst') (zip xs ys)

monotonic :: (Stable a, Num a) => Sig a -> Sig a
monotonic xs = scan (box (+)) 0 (map (box abs) xs)

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
prop_scan =  forAllShrink (generateSignals @Int) shrinkHList $ \intSignals ->
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
    let stuttered   = stutter (first intSignals) (second intSignals)
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

prop_singleSignalAlwaysTicks :: Property
prop_singleSignalAlwaysTicks = forAllShrink (generateSignals @Int) shrinkHList $ \intSignal ->
    let state       = flatten intSignal
        predicate   = Always $ Now ((Ticked First) |==| (Pure False))
        result      = evaluate predicate state
    in result

prop_firstElement :: Property
prop_firstElement = forAllShrink (generateSignals @[Int, Bool]) shrinkHList $ \intSignal ->
    let state       = flatten intSignal
        predicate   = Always $ (Now ((Index First) |<| (Pure 50))) `And` (Now ((Index Second) |==| (Pure True)))
        result      = evaluate predicate state
    in result

-- Switched signal equals XS until YS has ticked, from then on the value is constant assuming ys has not produced another const signal
prop_switchR :: Property
prop_switchR = forAllShrink (generateSignals @Int) shrinkHList $ \intSignals ->
    let xs                  = first intSignals
        (_ ::: ys)          = scan (box (+)) 1 (const (0 :: Int))
        zs                  = switchR xs (mapAwait (box (\_ -> const)) ys)
        state               = prepend zs $ prependLater ys $ flatten intSignals
        predicate           =   Not $ (Now ((Index First) |==| (Index Third)))
                                `Until`
                                ((Now ((Ticked Second) |==| (Pure True)))
                                `And`
                                (Next $ Implies
                                    (Always $ Now ((Ticked Second) |==| (Pure False)))
                                    (Now ((Index (Previous First)) |==| (Index First)))
                                    `And`
                                    (Not $ Now ((Index First) |==| (Index (Previous First))))
                                    ))
        result              = evaluate predicate state
    in result

prop_sigLength :: Property
prop_sigLength = forAllShrink (arbitrary :: Gen (Sig Int)) shrink $ \(sig :: Sig Int) ->
        let state   = singletonHList sig
            pred    = Always $ Now ((Ticked First) |==| (Pure False))
            result  = evaluate pred state
        in result

prop_sigIsPositive :: Property
prop_sigIsPositive = forAll (generateSignals @Int) $ \sig ->
        let mapped      = map (box (abs)) (first sig)
            state       = singletonHList mapped
            pred        = Always $ Now ((Index First) |>=| (Pure 0))
            result      = evaluate pred state 
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
    quickCheck prop_map_gt
    quickCheck prop_parallel
    quickCheck prop_isStuttering
    quickCheck prop_functionIsMonotonic
    quickCheck prop_switchR
    quickCheck prop_singleSignalAlwaysTicks
    quickCheck prop_firstElement
    quickCheck prop_sigLength
    quickCheck prop_sigIsPositive