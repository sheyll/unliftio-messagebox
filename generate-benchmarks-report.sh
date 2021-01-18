#!/usr/bin/env bash

set -xe

HERE=$(realpath $(dirname "$0"))

rm -f ${TARGET}/benchmark-report

nix build  --max-jobs auto \
    -f ${HERE}/generate-benchmark-report.nix \
    -o ${TARGET}/benchmark-report
