module PropRatt.Flatten (flattenToSignal', NonEmptyList(..)) where

import AsyncRattus.Signal
import AsyncRattus.Strict
import PropRatt.Value
import PropRatt.AsyncRat
import AsyncRattus.InternalPrimitives

data NonEmptyList a = NonEmptyList !a !(List a)

-- temporarily moved to its own module because of cyclic dependencies
-- doesnt typecheck with asyncrattus restrictions because of the recursion
flattenToSignal' :: (Stable a) => List (Sig a) -> Sig (List (Value a))
flattenToSignal' Nil = undefined
flattenToSignal' (h :! Nil) = singleton' h
flattenToSignal' (h :! t)   = prepend h (flattenToSignal' t)

-- could alternatively make the argument stronger to ensure totality
flattenToSignal :: (Stable a) => NonEmptyList (Sig a) -> Sig (List (Value a))
flattenToSignal (NonEmptyList h Nil) = singleton' h
flattenToSignal (NonEmptyList h t)   = prepend h (flattenToSignal' t)

