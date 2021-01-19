#!/usr/bin/env bash

set -ue

HERE=$(realpath $(dirname "$0"))

TARGET=${1:-/tmp}
TARGET0=$(mktemp -d)

nix build \
    --print-build-logs \
    --cores 0 \
    --max-jobs 1 \
    -o ${TARGET0}/result \
    -f ${HERE}/memleak-test.nix

rm -f ${TARGET}/memleak-test.log
echo "Writing into: ${TARGET}/memleak-test.log"
echo ""
echo "This may take more than 13 minutes..."
time ${TARGET0}/result/bin/unliftio-messagebox-memleak-test 100 10 +RTS -T -N -M300m -RTS > ${TARGET}/memleak-test.log
rm -rf ${TARGET0}