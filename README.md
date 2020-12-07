# Fast and Safe Message Passing Between Concurrent Processes

A small library on top of `unliftio` that reduces the risk of 
live and dead locks and thread starvation, as well as 
bad performance in massively concurrent programs.

The library assumes an architecture with a large number 
of concurrent processes communicating either one-to-one or
few-to-many using messages passed to bounded queues.

