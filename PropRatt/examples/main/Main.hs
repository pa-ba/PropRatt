{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# LANGUAGE TypeApplications, FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use zipWith" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Use const" #-}

module Main (main) where
    
import Test.QuickCheck
import PropRatt
import AsyncRattus.InternalPrimitives
import Prelude hiding (zip, map, const)
import AsyncRattus.Signal hiding (filter)
import AsyncRattus.Strict hiding (singleton)
import Prelude hiding (const, map, zip)
import AsyncRattus.Plugin.Annotation
import PropRatt.Utils
import qualified Data.IntSet as IntSet


filterM :: Box (a -> Bool) -> Sig a -> Sig (Maybe' a)
filterM f (x ::: xs) = if unbox f x
  then Just' x ::: delay (filterM f (adv xs))
  else Nothing' ::: delay (filterM f (adv xs))

triggerM :: (Stable a, Stable b) => Box (a -> b -> c) -> Sig a -> Sig b -> Sig (Maybe' c)
triggerM f (a ::: as) bs@(b:::_) = Just' (unbox f a b) ::: triggerMAwait f as bs

triggerMAwait :: Stable b => Box (a -> b -> c) -> O (Sig a) -> Sig b -> O (Sig (Maybe' c))
triggerMAwait f as (b:::bs) = delay (case select as bs of
            Fst (a' ::: as') bs' -> Just' (unbox f a' b) ::: triggerMAwait f as' (b ::: bs')
            Snd as' bs' -> Nothing' ::: triggerMAwait f as' bs'
            Both (a' ::: as') (b' ::: bs') -> Just' (unbox f a' b') ::: triggerMAwait f as' (b' ::: bs'))

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
                        Now ((Index First) |==| (Pure 1)) --
        result      = evaluate predicate state
    in result

-- Prefix sum is strictly monotonically increasing, but fails for non-natural numbers
prop_scan_failing :: Property
prop_scan_failing =  forAllShrink (generateSignals @Int) shrinkHls $ \intSignals ->
    let prefixSum   = scan (box (+)) 0 (first intSignals)
        state       = prepend prefixSum $ flatten intSignals
        predicate   = Next $ Always $ Now $ Index (Previous First) |<| (Index First)
        result      = evaluate predicate state
    in counterexample ("must be nat numbers") result

prop_scan :: Property
prop_scan =  forAllShrink (generateSignals @Int) shrinkHls $ \intSignals ->
    let absSig      = map (box (abs)) (first intSignals)
        prefixSum   = scan (box (+)) 0 absSig
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
  let filtered      = filterM (box (>= 10)) (first charSignals)
      state         = singletonH filtered
      predicate     = Always $ Now ((Index First) |>| Pure (Just' 9))
      result        = evaluate predicate state
  in result

prop_ticked :: Property
prop_ticked = forAll (generateSignals @Int) $ \charSignals ->
  let ticked        = filterM (box (>= 10)) (first charSignals)
      state         = singletonH ticked
      predicate     = Always $ Now $ ((Ticked First)) |==| ((Pure True))
      result        = evaluate predicate state
  in result

prop_triggerM :: Property
prop_triggerM = forAll (generateSignals @[Int, Int]) $ \intSignals ->
  let triggered     = triggerM (box (+)) (first intSignals) (second intSignals)
      state         = prepend triggered $ flatten intSignals
      predicate     = Always $ Implies (Now ((Ticked Second) |==| (Pure True))) (Now ((Ticked First) |==| (Pure True)))
      result        = evaluate predicate state
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
        state       = singletonH mono
        predicate   = Always $ Next (Now ((Index First) |>=| (Index (Previous First))))
        result      = evaluate predicate state
    in result

prop_singleSignalAlwaysTicks :: Property
prop_singleSignalAlwaysTicks = forAllShrink (arbitrary :: Gen (Sig Int)) shrink $ \sig ->
    let state       = singletonH sig
        predicate   = Always $ Now ((Ticked First) |==| (Pure True))
        result      = evaluate predicate state
    in result

prop_firstElement :: Property
prop_firstElement = forAllShrink (generateSignals @[Int, Bool, Bool, Int]) shrinkHls $ \intSignals ->
    let state       = flatten intSignals
        predicate   = Always $ (Now ((Index First) |<| (Pure 50))) -- `And` (Now ((Index Second) |==| (Pure True)))
        result      = evaluate predicate state
    in counterexample (show (first intSignals)) result

prop_firstElement2 :: Property
prop_firstElement2 = forAllShrink (arbitrary :: Gen (Sig Int)) shrink $ \intSignal ->
    let state       = singletonH intSignal
        predicate   = Always $ (Now ((Index First) |<| (Pure 50))) -- `And` (Now ((Index Second) |==| (Pure True)))
        result      = evaluate predicate state
    in counterexample (show intSignal) result

-- ZS == XS `Until` (HasTicked ys AND (Implies (Next (Always (Not (HasTicked ys)))) (Index (Previous ZS)) |==| (Index ZS))) AND (NOT (ZS |==| Previous (Index ZS)))

-- Switched signal equals XS until YS has ticked, from then on the value is constant assuming ys has not produced another const signal
prop_switchR :: Property
prop_switchR = forAll (generateSignals @Int)  $ \intSignals ->
    let xs                  = first intSignals
        gg@(_ ::: ys)       = (scan (box (\n _ -> n + 1)) 0 (takeSigSig (sigLength xs) mkSigZero)) :: Sig Int -- 1 2 3 4 5 6 
        zs                  = switchR xs (mapAwait (box (\b a -> const b)) ys) -- 1 1 1 1 1 2 2 2 2 
        state               = prepend zs $ prependLater ys $ flatten intSignals
        predicate           = (Now ((Index First) |==| (Index Third)))
                                `Until`
                                (Now ((Ticked Second) |==| (Pure True)))
                                `And` 
                                ((Always $ Next 
                                    (((Implies  (Not (Now (Ticked Second))) (Now ((Index (Previous First)) |==| (Index First))))))))
                                `Until`
                               (Next $ (Implies (Now (Ticked Second)) (Not (Now ((Index (Previous First)) |==| (Index First))))))
        result              = evaluate predicate state
    in counterexample (show state) result

prop_switchS :: Property
prop_switchS = forAllShrink (generateSignals @Int) shrinkHls $ \intSignals ->
    let xs                  = first intSignals
        gg@(_ ::: ys)       = (scan (box (\n _ -> n + 1)) 0 (takeSigSig (sigLength xs) mkSigZero)) :: Sig Int
        ggg                 = Delay (IntSet.fromList [1,2,3]) (\b a -> const a)
        zs                  = switchS xs ggg
        state               = prepend zs $ prependLater ys $ flatten intSignals
        predicate           =(Now ((Index First) |==| (Index Third)))
                                `Until`
                                (Now ((Ticked Second) |==| (Pure True)))
                                `And` 
                                ((Always $ Next 
                                    (((Implies  (Not (Now (Ticked Second))) (Now ((Index (Previous First)) |==| (Index First))))))))
                                `Until`
                               (Next $ (Implies (Now (Ticked Second)) (Not (Now ((Index (Previous First)) |==| (Index First))))))
        result              = evaluate predicate state
    in counterexample (show gg ++ show zs ++ show xs) result

prop_sigLength :: Property
prop_sigLength = forAllShrink (arbitrary :: Gen (Sig Int)) shrink $ \(sig :: Sig Int) ->
        let state   = singletonH (sig :: Sig Int)
            predicate    = Always $ (Now ((Index First) |<| (Pure 50)))  -- Always $ Now ((Ticked First) |==| (Pure False))
            result  = evaluate predicate state
        in result

prop_sigIsPositive :: Property
prop_sigIsPositive = forAll (generateSignals @Int) $ \sig ->
        let mapped      = map (box (abs)) (first sig)
            state       = singletonH mapped
            predicate   = Next $ Always $ Now ((Index (Prior 1 First)) |>=| (Pure 0))
            result      = evaluate predicate state 
        in result

prop_catchsubtle :: Property
prop_catchsubtle = forAllShrink (arbitrary :: Gen (Sig Int)) shrink $ \(sig :: Sig Int) ->
        let state   = singletonH (sig :: Sig Int)
            predicate    = Always $ Implies (Now ((Index First) |>| (Pure 80))) (Next $ (Now ((Index First) |<| (Index (Previous First)))))
            result  = evaluate predicate state
        in result


{-# ANN scramble AllowRecursion #-}
scramble :: Int -> Sig a -> Sig a
scramble n sig@(x ::: Delay clx fx) = 
    if IntSet.null clx 
        then x ::: never 
        else x ::: Delay (IntSet.singleton ((n `mod` 3) + 1)) (\_ -> (scramble (n+1) (fx (InputValue (IntSet.findMin clx) ()))))

main :: IO ()
main = do
    -- quickCheck prop_interleave
    -- quickCheck prop_switchedSignal
    -- quickCheck prop_buffer
    -- quickCheck prop_zip
    -- quickCheck prop_jump
    -- quickCheck prop_stop
    -- quickCheck prop_scan
    -- quickCheck prop_scan_failing
    -- quickCheck prop_filter
    -- quickCheck prop_ticked
    -- quickCheck prop_triggerM
    -- quickCheck prop_parallel
    -- quickCheck prop_isStuttering
    -- quickCheck prop_functionIsMonotonic
    -- quickCheck prop_singleSignalAlwaysTicks
    -- quickCheck prop_firstElement
    -- quickCheck prop_firstElement2
    -- quickCheck prop_sigLength
    -- quickCheck prop_sigIsPositive
    -- quickCheck prop_catchsubtle
    quickCheck prop_switchR
    quickCheck prop_switchS