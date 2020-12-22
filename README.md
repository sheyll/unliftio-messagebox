# Fast and Safe Message Passing Between Concurrent Processes

A small library on top of `unliftio` that reduces the risk of 
live and dead locks and thread starvation, as well as 
bad performance in massively concurrent programs.

The library assumes an architecture with a large number 
of concurrent processes communicating either one-to-one or
few-to-many using messages passed to bounded queues.


Benchmarks
----------

[Results](./command-benchmark.html)

### Media-Benchmark

This benchmark consists of several more complex use cases.

#### FetchDsps

In this use case a large number of concurrent clients all
request the current set of DSPs from a single server using
a blocking (RPC-Style) `FetchDsps` command.

The server processes all incoming requests sequentially.

#### Results

- very few data points (6 for bech with 1 client, 4 for bench with 1000 clients)
- The `100000 total fetches 1000 clients` example could be come congested (maximum time was 826ms) ???b



### BookStore-Benchmark

Observations:

