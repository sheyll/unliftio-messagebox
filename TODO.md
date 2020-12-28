TODO
====

* Improve Media Benchmark
    * Try to reduce SYSTEM time
      * make the timeout in handleMessage pluggable
        (use newtype wrapper for IsInBox instances)
      * benchmark TMVar vs MVar for reply box  
* Improve safety
    * Add IsInBox/IsOutBox instances that wrap the calls in a 'try_' block
