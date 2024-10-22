module Main (main) where
import Test.QuickCheck
import Name.Generators
import Name.Utilities

main :: IO ()
main = do
    value <- generate (arbitrary :: Gen (Sig Int))
    print (show value)
