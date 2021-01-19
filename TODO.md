TODO
====

## Rename and Refactor Project

* **DONE** Rename from unliftio-protocols to **unliftio-messagebox**
* **DONE** Move `Protocol.MessageBox.*` to `UnliftIO.MessageBox` 
* **DONE** Move `Protocol.Command` to `UnliftIO.MessageBox.Command`
* **DONE** Move `Protocol.CallId` to `UnliftIO.MessageBox.Util.CallId`
* **DONE** Move `Protocol.Fresh` to `UnliftIO.MessageBox.Util.Fresh`
* **DONE** Move `Protocol.Future` to `UnliftIO.MessageBox.Util.Future`
* **DONE** Add a `UnliftIO.MessageBox` that re-exports:
  * `UnliftIO.MessageBox.Class`
  * `UnliftIO.MessageBox.Limited(BlockingBox, NonBlockingBox, WaitingBox, MessageLimit)`
  * `UnliftIO.MessageBox.Unlimited(UnlimitedBox)`
  * `UnliftIO.MessageBox.CatchAll`
  * `UnliftIO.MessageBox.Command`
  * `UnliftIO.MessageBox.CallId`
  * `UnliftIO.MessageBox.Future`

## Goal: Know Your Code 

* **DONE** Write property tests exhibiting race
  conditions like shutdown races that
  provokes ThreadBlockedIndefinitelyOnMVar
  * **DONE** Not much to gain here... Unagi never seems to throw stuff the way we use it. 
  * **DONE** make a `NoExc` newtype wrapper that catches all exceptions for `IsMessageBox` and `IsInput` 
* **DONE** Apply lock contention test to Unlimited message box implementation  
* **DONE** Write unit tests for untested code 
  * **DONE** Protocol.Command
  * **DONE** Protocol.Command.CallId
  * **DONE**Protocol.Fresh
  * **DONE**Protocol.MessageBox.CatchAll
  * **DONE**Protocol.MessageBox.Limited
* **DONE** Remove unused code
* **UP NEXT** Write missing Documentation
  * **DONE** Document that in 'call' the timeout is not for message delivery, but only for 
    waiting for the reply.
* **DONE** receiveAfter tests
* **DONE** receiveAfter benchmarks (part of MediaBenchmark)
* **DONE** Implement async call API
  * **DONE** **REJECTED** Use the async call API in the sync call API.

## Goal: Automatic Report Generation
* **NOW** Write script to commit the results of benchmarks, tests, 
  test profiling and test code coverage reports
  to a branch of the git repo
* **DONE** **REJECTED: Weeder requires to much recompiling** Add Weeder script
* **NOW** Add Haddock script
* **DONE** Add graphmod script  
* **DONE** Add profiling test case execution script
* **DONE** Reduce the number of benchmarks
* **UP NEXT** Make a profiling report based on a subset of the benchmarks
* **NOW** Link to the reports from the README.md
* Make a benchmark for the Erlang code
* Make Erlang part of the nix-based environment

## Goal: Fast Code

* benchmark TMVar vs MVar for reply box 
  * **DONE**: TMVar are faster because the timeout is based on registerDelay 
* benchmark type class methods vs. direct function calls
  * **DONE**: No big difference

## Goal: No Space Leaks

* **DONE** Extract the media benchmark and create a long
  running example program that checks for memory leaks
* **DONE** **Goal achieved in a different way** Use Weak References for `Input`s
  * Add 'isActive'
  * Catch `ThreadBlockedIndefinitelyInMVarOperation` 

## Goal: Robust Code

* **DONE** Fix 'Command.call': Even when the result is there the call waits for the timeout
* Make sure that no messages are lost through async exceptions (e.g. timeout)
  when receiving
  * **DONE** Nothing that can be done! Make your application robust.
* **UP NEXT** Allow logging/cleanup of messages receive were to drop when an 
  async exception is received.
* Allow logging/cleanup of messages that are lost when an Async exception 
  was raised after a reply was taken from the reply box, but before it was returned.  
* Add IsMessageBox/IsInput instances that wrap the calls in a `try_` block
  * **DONE**
* Add IsMessageBox/IsInput instances through newtype wrappers
  to Handle dead lock exceptions
  * **DONE**  
* Make usages of MVars or TVars e.g. `replyTo`, `handleMessage`, `receive`
  dead lock exception safe
  * **DONE**
* make the timeout in handleMessage pluggable
   (use newtype wrapper for IsMessageBox instances)
   * **DONE** With Waiting..