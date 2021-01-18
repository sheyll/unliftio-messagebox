#!/usr/bin/env bash

set -xe

HERE=$(realpath $(dirname "$0"))

nix build --max-jobs auto -f ${HERE}/test-profiling-report.nix -o ${HERE}/test-profiling-report.link
rm -rf ${HERE}/test-profiling-report
cp --recursive --dereference ${HERE}/test-profiling-report.link ${HERE}/test-profiling-report
rm ${HERE}/test-profiling-report.link