#!/usr/bin/env bash

set -ue

HERE=$(realpath $(dirname "$0"))

TARGET=${1:-/tmp}

rm -f ${TARGET}/messagebox-memleak-test-report

nix build \
    --print-build-logs \
    --cores 0 \
    --max-jobs 1 \
    -o ${TARGET}/messagebox-memleak-test-report \
    -f ${HERE}/generate-messagebox-memleak-test-report.nix    
