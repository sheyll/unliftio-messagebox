#!/usr/bin/env bash

set -xe

HERE=$(realpath $(dirname "$0"))
pushd $HERE

nix build -f ./test.nix -o result-test-profiling --arg withProfiling true
result-test-profiling/bin/unliftio-protocols-test


popd