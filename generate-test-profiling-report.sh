#!/usr/bin/env bash

set -xue

HERE=$(realpath $(dirname "$0"))

TARGET=${1:-/tmp}

rm -f ${TARGET}/test-profiling-report

nix build --max-jobs auto \ 
    -f ${HERE}/test-profiling-report.nix \
    -o ${TARGET}/test-profiling-report
