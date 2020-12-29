TODO
====

## Goal: Fast Code

* Try to reduce SYSTEM time
* benchmark TMVar vs MVar for reply box 

## Goal: No Space Leaks

* Extract the media benchmark and create a long
  running example program that checks for memory leaks

## Goal: Robust Code

* Add IsInBox/IsOutBox instances that wrap the calls in a `try_` block
* Add functions that throw runtime exceptions instead of
  returning 'Either', 'Bool' or 'Maybe': 
   'deliverOrThrow' and 'receiveOrThrow' 
* Add IsInBox/IsOutBox instances through newtype wrappers
  to Handle dead lock exceptions  
* Make usages of MVars or TVars e.g. `replyTo`, `handleMessage`, `receive`
  dead lock exception safe
* Write unit tests for untested code
* Add thread safe code to combine message box creation
  and spawning
* make a variant for 'Conc', 'Async' and plain 'forkIO'
* make the timeout in handleMessage pluggable
   (use newtype wrapper for IsInBox instances)

## Goal: Nice Code

* Remove unused code, 
* Add a coverage report script
* Weak references

