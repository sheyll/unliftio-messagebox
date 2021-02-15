#!/usr/bin/env bash

set -ue

HERE=$(realpath $(dirname "$0"))

TARGET=${1:-/tmp}

rm -f ${TARGET}/pool-memleak-test-report

nix build \
    --print-build-logs \
    --cores 0 \
    --max-jobs 1 \
    -o ${TARGET}/pool-memleak-test-report \
    -f ${HERE}/generate-pool-memleak-test-report.nix    
