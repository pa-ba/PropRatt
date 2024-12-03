# PBT-for-AsyncRattus

## Notes/questions before feedback session 5/12 12:30
Hi! Since last time, we have begun writing the report, as well as we have implemented property based testing for the zip and the switch method. 
For the feedback session this thursday, we would like to talk about whether the current implementations are sufficient as basis for the research project. 
For testing the switch method we are testing whether the result of calling the switch method is equal to the later signal initially passed to the switch method, when the clock on the later signal has ticked. 
Is this test sufficient or should we test additional properties on the switch method?
We would also like to talk about how much text we can have reviewed by you before handing in, and also talk about when the examination will take place.


## Notes/questions before feedback session 21/11 12:30
Hi there! Since last time we have mainly been working on implementing a method that compares two signals, and determines whether the second signal is a stuttering of the first signal.
For doing this, we started by implementing a list based version. That is taking X elements of two signal and zipping these together. 
The list based version might not be the way to go, because we then throw out the clock. Therefore we tried implementing a Signal based version, with a clock picking strategy
of always picking the smallest possible clock. You can find the code for the two versions in `test/src/Name/Properties.hs` from line 63. 

In the Main file (`test/src/Main.hs`) we have an example of calling our methods with arbitrarily generated Signals. This is currently limited to Signals of length 3, to test simple examples. Running `stack run` from the root of `test` folder should print; two arbitrary signals, the zipped tuple representation of them, the stutter signal and a boolean indicating whether the zipped and stripped signal is a stuttering of the first original signal.

This works to some extend, but we encountered a bug that only happens in some cases. When zipping two Signals with the zip method from AsyncRattus (we do that in here: `test/src/Name/ARat.hs`) we do in some cases get tuples where it contains elements that are not a part of either of the two initial signals. 
Below is examples of different output we get. The results here are structered as: `Signal1, Signal2, ZippedSignal, StutterSignal, IsStuttering bool`

A correct result. We advance on smallest clock 1, so we don't zip crooked
```
Sig [-21,24,-20] clock: fromList [2,3]
Sig [-21,-13,-20] clock: fromList [2]
Sig [(-21 :* -21),(24 :* -13),(-20 :* -20)] clock: fromList [2,3]
Sig [-21,24,-20] clock: fromList [2,3]
True
```

A correct result. We advance on smallest clock, which is 3 in the first signal and 1 in the second. This results in a crooked zipping, and we get a correct stuttering of the original signal.
```
Sig [16,0,-16] clock: fromList [2]
Sig [-14,-12,-12] clock: fromList [1]
Sig [(16 :* -14),(16 :* -12),(16 :* -12),(16 :* -28),(0 :* -28),(-16 :* -28)] clock: fromList [1,2]
Sig [16,16,16,16,0,-16] clock: fromList [1,2]
True
```

This is the wrong/problematic case; A wrong result. We have different smallest clocks in each Signal, so we should have a crooked zipping. However, the last 3 tuples contains the value 0 in first position, which comes from neither of the two initial signals. 
```
Sig [-24,10,-16] clock: fromList [1,2]
Sig [-29,-11,-8] clock: fromList [2]
Sig [(-24 :* -29),(10 :* -29),(-16 :* -29),(0 :* -29),(0 :* -11),(0 :* -8)] clock: fromList [1,2]
Sig [-24,10,-16,0,0,0] clock: fromList [1,2]
False
```


In the supervision session we would like to discuss whether we are going in the right direction with this, and if we are implementing somthing in a wrong way. 
We would also like to discuss how we can solve the problem with the wrong case described above.



## Notes/questions before feedback session 22/10 11:00
For this feedback session, we have been experimenting with a lot of AsyncRattus coding, especially working with creating an Arbitrary input generator for signals. We looked a bit into doing the same for clocks, and we a few questions we would like to discuss at the session. 
Take a look at some of the code if you have time, and else, we will discuss and look at it wednesday :D 



## Notes/questions before feedback session 9/10 11:00
We have tried to implement the map function, both in Rattus and AsyncRattus.
In this repo, you find one folder for each example, namely rattusExample and asyncRattusExample.
For both languages, the implementation of the map is more or less the same. The difference between the two is that the Rattus implementation works with a synchronous Stream and AsyncRattus works with a asynchronous Signal. 
The Rattus example works, and we are able to take 10 first elements of the stream to test on a finite number of elements. 
The AsyncRattus example works too, but we run into some complications, when trying to test on a finite number of elements. 
We want to be able to take a Signal, convert it into a finite list of items in order to test for "Probably Equality".

At this point, we have looked a bit into testing with QuickCheck, but we haven't been able to come up with a working example for testing on Signals in AsyncRattus.

Questions: 
- In our implementation in the AsyncRattus example, we implement a "timer", such that the Signal gets one more element every second. Is it correctly understood that this would be considered the clock? and that for each tick (second), we move forward in or program, such that the tail (next element of the tail), which in the first step was a delayed recursive call to the map function, is now calculated.

- In our Rattus example, we implement the `printNStream()` function with an accumulating parameter `n`, in order to count elements that we have mapped over and then return a finite list of n elements. Is it possible to do the same with AsyncRattus and Signals? 

- When narrowing down our scope to only look at the map function in AsyncRattus, we believe that the main difference between Rattus and AsyncRattus here is, that Rattus works with synchronous Streams of data, and AsyncRattus works with asynchronous Signals of data. Is this correctly understood? (So we should find a way to convert Signals into testable data type, and would it be best to voncert it into a list or something else?)

- If we test with QuickCheck by generating synchronous finite data to test with, won't we then be removing the asynchronous aspect? (is it possible to generate async testdata with QuickCheck? or is the goal to generate a lot of different finite data with QuickCheck, and then achieve "probably" bugfree code)
