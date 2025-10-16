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
import AsyncRattus.Plugin.Annotation
import Prelude hiding (const, map, zip)
import PropRatt.Signal
import qualified Data.IntSet as IntSet

{-# ANN module AllowLazyData #-}

filterM :: Box (a -> Bool) -> Sig a -> Sig (Maybe' a)
filterM f (x ::: xs) = if unbox f x
  then Just' x ::: delay (filterM f (adv xs))
  else Nothing' ::: delay (filterM f (adv xs))

triggerM :: (Stable b) => Box (a -> b -> c) -> Sig a -> Sig b -> Sig (Maybe' c)
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
    let interleaved     = interleave (box (+)) (future $ first intSignals) (future $ second intSignals)
        state           = prependLater interleaved $ flatten intSignals
        predicate       = X $ G $ ((Now ((Idx Sig1) |==| (Idx Sig2)))
                                        `Or`
                                        (Now ((Idx Sig1) |==| (Idx Sig3))))
                                        `Or`
                                        (Now (((Idx Sig2) + (Idx Sig3)) |==| (Idx Sig1)))
        result          = evaluate predicate state
    in result

-- Jumped signal should switch once the boxed predicate function is true.
prop_jump :: Property
prop_jump = forAll (generateSignals @Int) $ \intSignals ->
   let  jumpFunc    = box (\n -> if n > 10 then Just' (const 1) else Nothing')
        jumpSig     = jump jumpFunc (first intSignals)
        state       = prepend jumpSig $ flatten intSignals
        predicate   = G $
                        Now ((Idx Sig1) |==| (Idx Sig2))
                        `Or`
                        Now ((Idx Sig1) |==| (Pure 1)) --
        result      = evaluate predicate state
    in result

-- Prefix sum is strictly monotonically increasing, but fails for non-natural numbers.
prop_scan_failing :: Property
prop_scan_failing =  forAllShrink (generateSignals @Int) shrinkHls $ \intSignals ->
    let prefixSum   = scan (box (+)) 0 (first intSignals)
        state       = prepend prefixSum $ flatten intSignals
        predicate   = X $ G $ Now $ Idx (Prev Sig1) |<| (Idx Sig1)
        result      = evaluate predicate state
    in counterexample ("Must be natural numbers.") result

-- Prefix sum is strictly monotonically increasing is true for natural numbers.
prop_scan :: Property
prop_scan =  forAllShrink (generateSignals @Int) shrinkHls $ \intSignals ->
    let absSig      = map (box (\x -> (abs x + 1))) (first intSignals)
        prefixSum   = scan (box (+)) 0 absSig
        state       = prepend prefixSum $ flatten intSignals
        predicate   = X $ G $ Now $ Idx (Prev Sig1) |<| (Idx Sig1)
        result      = evaluate predicate state
    in result

-- A switched signal has values equal to the first signal until its values equal values from the third signal
prop_switchedSignal :: Property
prop_switchedSignal = forAll (generateSignals @[Int, Int]) $ \intSignals ->
    let switched    = switch (first intSignals) (future (second intSignals))
        state       = prepend switched $ flatten intSignals
        predicate   = U (Now ((Idx Sig1) |==| (Idx Sig2))) (Now ((Idx Sig1) |==| (Idx Sig3)))
        result      = evaluate predicate state
    in result

-- A buffered signal is always one tick behind.
prop_buffer :: Property
prop_buffer = forAll (generateSignals @Int) $ \intSignals ->
    let bufferedSig = buffer 10 (first intSignals)
        state       = prepend bufferedSig $ flatten intSignals
        predicate   = X $ G $ Now $ (Idx Sig1) |==| Idx (Prev Sig2)
        result      = evaluate predicate state
    in result

-- A signal becomes constant once the predicate function to "stop" is true.
prop_stop :: Property
prop_stop = forAll (generateSignals @Int) $ \intSignals ->
    let stopped     = stop (box (>100)) (first intSignals)
        state       = prepend stopped $ flatten intSignals
        predicate   = G ((Now ((Idx Sig1) |>| (Pure 100))) :=> (G $ X (Now (Idx (Prev Sig1) |==| (Idx Sig1)))))
        result      = evaluate predicate state
    in result

-- A zipped signal always has fst' values from second signal and snd' values from third signal.
prop_zip :: Property
prop_zip = forAll (generateSignals @[Int, Int]) $ \intSignals ->
    let s1          = zip (first intSignals) (second intSignals)
        state       = prepend s1 $ flatten intSignals
        predicate   = G $ Now ((fst' <$> (Idx Sig1)) |==| (Idx Sig2)) `And` (Now ((snd' <$> (Idx Sig1)) |==| (Idx Sig3)))
        result      = evaluate predicate state
    in result

prop_filter :: Property
prop_filter = forAll (generateSignals @Int) $ \intSignals ->
  let filtered      = filterM (box (>= 10)) (first intSignals)
      state         = prepend filtered $ flatten intSignals
      predicate     = G $
            ((Now ((Idx Sig2) |>=| Pure (10))) :=> (Now ((Idx Sig1) |>=| (Pure (Just' 10)))))
            `And`
            ((Now ((Idx Sig2) |<| Pure (10))) :=> (Now ((Idx Sig1) |==| (Pure Nothing'))))
      result        = evaluate predicate state
  in result

prop_triggerM :: Property
prop_triggerM = forAll (generateSignals @[Int, Int]) $ \intSignals ->
  let triggered     = triggerM (box (*)) (first intSignals) (second intSignals)
      state         = prepend triggered $ flatten intSignals
      predicate     = G 
            ((Now ((Tick Sig2) |==| (Pure True))) :=> 
            ((Now ((Tick Sig1) |==| (Pure True))) `And` (Now ((fromMaybe' 0 <$> (Idx Sig1)) |==| ((Idx Sig2) * (Idx Sig3))))))
      result        = evaluate predicate state
  in result

prop_parallel :: Property
prop_parallel = forAllShrink (generateSignals @[Int, Int]) shrinkHls $ \intSignals ->
    let paralleled  = parallel (first intSignals) (second intSignals)
        state       = prepend paralleled $ flatten intSignals
        predicate   = G $
            (Now (Tick Sig3) :=> Now (Tick Sig1))
            `And`
            (Now (Tick Sig2) :=> Now (Tick Sig1))
        result      = evaluate predicate state
    in result

prop_isStuttering :: Property
prop_isStuttering = forAll (generateSignals @[Int, Int]) $ \intSignals ->
    let stuttered   = stutter (first intSignals) (second intSignals)
        state       = prepend stuttered $ flatten intSignals
        predicate   = G $
            (Now (Tick Sig1) :=> Now (Idx Sig1 |==| Idx Sig2))
            `And`
            X ((And (Now (Tick Sig3)) (Not (Now (Tick Sig2))) :=> Now (Idx (Prev Sig1) |==| Idx Sig1)))
        result      = evaluate predicate state
    in result

prop_functionIsMonotonic :: Property
prop_functionIsMonotonic = forAll (generateSignals @Int) $ \intSignals ->
    let mono        = monotonic (first intSignals)
        state       = singletonH mono
        predicate   = G $ X (Now ((Idx Sig1) |>=| (Idx (Prev Sig1))))
        result      = evaluate predicate state
    in result

prop_singleSignalAlwaysTicks :: Property
prop_singleSignalAlwaysTicks = forAllShrink (arbitrary :: Gen (Sig Int)) shrink $ \sig ->
    let state       = singletonH sig
        predicate   = G $ Now ((Tick Sig1) |==| (Pure True))
        result      = evaluate predicate state
    in result

-- Switched signal equals XS until YS has ticked, from then on the value is constant assuming ys has not produced another const signal
prop_switchR :: Property
prop_switchR = forAllShrink (generateSignals @Int) shrinkHls $ \intSignals ->
    let xs                  = first intSignals
        (_ ::: ys)          = (scan (box (\n _ -> n + 1)) 0 (takeN (sigLength xs) mkSigZero)) :: Sig Int
        zs                  = switchR xs (mapAwait (box (\b _ -> const b)) ys)
        state               = prepend zs $ prependLater ys $ flatten intSignals
        predicate           = (Now ((Idx Sig1) |==| (Idx Sig3)))
                                `U`
                                (Now ((Tick Sig2) |==| (Pure True)))
                                `And` 
                                ((G $ X 
                                    ((((Not (Now (Tick Sig2)) :=> Now ((Idx (Prev Sig1)) |==| (Idx Sig1))))))))
                                `U`
                               (X $ ((Now (Tick Sig2) :=> Not (Now ((Idx (Prev Sig1)) |==| (Idx Sig1))))))
        result              = evaluate predicate state
    in counterexample (show state) result

prop_switchS :: Property
prop_switchS = forAllShrink (generateSignals @Int) shrinkHls $ \intSignals ->
    let xs                  = first intSignals
        gg@(_ ::: ys)       = (scan (box (\n _ -> n + 1)) 0 (takeN (sigLength xs) mkSigZero)) :: Sig Int
        ggg                 = Delay (IntSet.fromList [1,2,3]) (\_ a -> const a)
        zs                  = switchS xs ggg
        state               = prepend zs $ prependLater ys $ flatten intSignals
        predicate           =(Now ((Idx Sig1) |==| (Idx Sig3)))
                                `U`
                                (Now ((Tick Sig2) |==| (Pure True)))
                                `And` 
                                ((G $ X 
                                    ((((Not (Now (Tick Sig2)) :=> Now ((Idx (Prev Sig1)) |==| (Idx Sig1))))))))
                                `U`
                               (X $ ((Now (Tick Sig2) :=> Not (Now ((Idx (Prev Sig1)) |==| (Idx Sig1))))))
        result              = evaluate predicate state
    in counterexample (show gg ++ show zs ++ show xs) result

prop_sigLength :: Property
prop_sigLength = forAllShrink (arbitrary :: Gen (Sig Int)) shrink $ \(sig :: Sig Int) ->
        let state       = singletonH (sig :: Sig Int)
            predicate   = G $ (Now ((Idx Sig1) |<| (Pure 50)))
            result      = evaluate predicate state
        in result

prop_sigIsPositive :: Property
prop_sigIsPositive = forAll (generateSignals @Int) $ \sig ->
        let mapped      = map (box (abs)) (first sig)
            state       = singletonH mapped
            predicate   = X $ G $ Now ((Idx (PrevN 1 Sig1)) |>=| (Pure 0))
            result      = evaluate predicate state 
        in result

prop_catchsubtle :: Property
prop_catchsubtle = forAllShrink (arbitrary :: Gen (Sig Int)) shrink $ \(sig :: Sig Int) ->
        let state       = singletonH (sig :: Sig Int)
            predicate   = G ((Now ((Idx Sig1) |>| (Pure 80))) :=> (X $ (Now ((Idx Sig1) |<| (Idx (Prev Sig1))))))
            result      = evaluate predicate state
        in result

prop_predLengthOutsideDefault :: Property
prop_predLengthOutsideDefault =  forAllShrink (generateSignals @Int) shrinkHls $ \intSignals ->
    let prefixSum   = scan (box (+)) 0 (first intSignals)
        state       = prepend prefixSum $ flatten intSignals
        predicate   = XN 100 $ G $ Now $ Idx (Prev Sig1) |<| (Idx Sig1)
        result      = evaluate predicate state
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
    quickCheck prop_triggerM
    quickCheck prop_parallel
    quickCheck prop_isStuttering
    quickCheck prop_functionIsMonotonic
    quickCheck prop_singleSignalAlwaysTicks
    quickCheck prop_sigIsPositive
    quickCheck prop_switchS

    putStrLn "=== Failing tests ==="
    quickCheck prop_scan_failing
    putStrLn "====================="
    quickCheck prop_sigLength
    putStrLn "====================="
    quickCheck (withMaxSuccess 1000 prop_switchR)
    putStrLn "====================="
    quickCheck (withMaxSuccess 1000 prop_catchsubtle)
    putStrLn "====================="
    quickCheck prop_predLengthOutsideDefault