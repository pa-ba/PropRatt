{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Main (main) where
import Test.QuickCheck
import PropRatt.AsyncRat
import PropRatt.Generators
import PropRatt.Properties
import PropRatt.LTL (Pred (..), evaluateLTL)


main :: IO ()
main = do
    print (evaluateLTL (Eventually (Implies (Atom odd) (NextTime (Atom (== 4))))) [2,2,2,3,4])