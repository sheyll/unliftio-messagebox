#!/usr/bin/env bash

set -xue

HERE=$(realpath $(dirname "$0"))

TARGET=${1:-/tmp}

TARGET0=$(mktemp -u)

nix build \
    --print-build-logs \
    --cores  0 \
    --max-jobs 1 \
    -o ${TARGET0} \
    -f ${HERE}/generate-test-coverage-report.nix

rm -rf ${TARGET}/test-coverage-report 
mkdir -p ${TARGET}/test-coverage-report 
cp --no-preserve=mode \
   --no-preserve=ownership \
   --no-preserve=timestamps \
   --recursive --dereference \
    ${TARGET0}/share/hpc/vanilla/html/unliftio-messagebox-*/* \
    ${TARGET}/test-coverage-report

rm ${TARGET0}
