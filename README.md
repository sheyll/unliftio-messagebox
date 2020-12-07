# Typed, stateless protocols

A small library on top of `unliftio` that does **one thing**:

It reduces the risk of massively concurrent programs
failing and performing poorly.

The library assumes an architecture with a large number 
of concurrent processes communicating either one-to-one or
few-to-many using messages passed to bounded queues.

