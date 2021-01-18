#!/usr/bin/env bash

set -xe

HERE=$(realpath $(dirname "$0"))

rm -f ${TARGET}/benchmark-report

nix build \
    --cores  0 \
    --max-jobs auto \
    -o ${TARGET}/benchmark-report \
    -f ${HERE}/generate-benchmark-report.nix
