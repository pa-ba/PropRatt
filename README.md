# PropRatt

PropRatt is a Haskell framework for testing AsyncRattus using property-based testing.

# Overview

- The [timer example file](examples/timer/Timer.hs) contains the timer
  example from the paper.
- The [main example file](examples/main/Main.hs) contains further
  example specifications that test several signal combinators.
- The implementation of the specification language can be found in the
  [PropRatt.LTL](src/PropRatt/LTL.hs) module.


# Running examples

Using `stack`:
- `stack run main-example`
- `stack run timer-example`

Using `cabal`:
- `cabal run main-example`
- `cabal run timer-example`
