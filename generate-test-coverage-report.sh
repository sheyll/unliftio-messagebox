#!/usr/bin/env bash

set -xue

HERE=$(realpath $(dirname "$0"))

TARGET=${1:-/tmp}

TARGET0=$(mktemp -u)

nix-build \
    ${HERE}/test-coverage-report.nix \
    -o ${TARGET0}

rm -rf ${TARGET}/test-coverage-report 
mkdir -p ${TARGET}/test-coverage-report 
cp --no-preserve=mode \
   --no-preserve=ownership \
   --no-preserve=timestamps \
   --recursive --dereference \
    ${TARGET0}/test-coverage-report.link/share/hpc/vanilla/html/unliftio-protocols-* \
    ${TARGET}/test-coverage-report

rm ${TARGET0}
