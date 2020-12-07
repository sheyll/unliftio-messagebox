# Typed, stateless protocols

A small library that does **one thing**:

It helps users to distribute messages between
processes, such that the compiler can validate
some very basic assumptions.

Think of it as a library to **reduce the boiler plate**
needed when a user wants to fork processes that share
MVars or TChans.

It offers a minimal API that...

* forks a process and creates communications MVars in one sweep
* offers a bit of type magic to have simple, stateless protocols

This library is based on `unliftio`.

