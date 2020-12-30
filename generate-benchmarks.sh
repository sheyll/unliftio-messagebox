#!/usr/bin/env bash

set -xe

nix build -f ./benchmark.nix -o result-bench 
./result-bench/bin/unliftio-protocols-bench -o ./benchmark-results/benchmark-1-CORES.html +RTS -N1
./result-bench/bin/unliftio-protocols-bench -o ./benchmark-results/benchmark-2-CORES.html +RTS -N2
./result-bench/bin/unliftio-protocols-bench -o ./benchmark-results/benchmark-ALL-CORES.html +RTS -N

