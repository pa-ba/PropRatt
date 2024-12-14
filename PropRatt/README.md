# PropRatt
## Property Based Testing for Functional Reactive Programming in AsyncRattus
This project explores PBT for AsyncRattus.
The following describes the folder structure of the project:

```
├── PropRatt
│   ├── src
│   │   ├── PropRatt
│   │   │    ├── AsyncRat.hs        // Functions executed with the AsyncRattus compiler plugin enabled
│   │   │    ├── Generators.hs      // Generator function for generating arbitrary signals and clocks
│   │   │    ├── Properties.hs      // Property functions for the defined properties to test
│   │   │    └── Utilities.hs       // All utility function used in the files above, such as checking stuttering or equality of signals
│   │   │
│   │   └── Main.hs                 // Main function for executing the tests on some arbitrary signals
│   │
│   └── test
│       └── Spec.hs                 // QuickCheck test, executing 100 tests for each property with a wide range of different input signals

```


### How to run
Stack run

### How to test
Stack test