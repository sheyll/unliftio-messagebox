TODO
====

## Goal: Know Your Code 

* Write property tests exhibiting race
  conditions like shutdown races that
  provokes ThreadBlockedIndefinitelyOnMVar
  * **DONE** Not much to gain here... Unagi never seems to throw stuff the way we use it. 
  * make a `NoExc` newtype wrapper that catches all exceptions for `IsMessageBox` and `IsInput` 
* Write unit tests for untested code
* Remove unused code
* Write missing Documentation 

## Goal: Automatic Report Generation
* Write script to commit the results of benchmarks, tests, 
  test profiling and test code coverage reports
  to a branch of the git repo
* Add Weeder script
* Add Haddock script
* Add graphmod script  
* Add profiling test case execution script
* Reduce the number of benchmarks
* Make a profiling report based on a subset of the benchmarks
* Link to the benchmarks and reports from the README.md
* Make a benchmark for the Erlang code
* Make Erlang part of the nix-based environment

## Goal: Fast Code

* Try to reduce SYSTEM time
* benchmark TMVar vs MVar for reply box 
  * DONE: TMVar are faster because the timeout is based on registerDelay 
* benchmark type class methods vs. direct function calls
  * DONE: No big difference

## Goal: No Space Leaks

* Extract the media benchmark and create a long
  running example program that checks for memory leaks
* Use Weak References Code
  in combination with catching ThreadBlockedIndefinitelyInMVarOperation)

## Goal: Robust Code

* Add IsMessageBox/IsInput instances that wrap the calls in a `try_` block
* Add functions that throw runtime exceptions instead of
  returning 'Either', 'Bool' or 'Maybe': 
   'deliverOrThrow' and 'receiveOrThrow' 
* Add IsMessageBox/IsInput instances through newtype wrappers
  to Handle dead lock exceptions  
* Make usages of MVars or TVars e.g. `replyTo`, `handleMessage`, `receive`
  dead lock exception safe
* Add thread safe code to combine message box creation
  and spawning
* make a variant for 'Conc', 'Async' and plain 'forkIO'
* make the timeout in handleMessage pluggable
   (use newtype wrapper for IsMessageBox instances)

