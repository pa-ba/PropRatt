# PBT-for-AsyncRattus


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
