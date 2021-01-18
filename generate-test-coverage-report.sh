#!/usr/bin/env bash

set -xe

HERE=$(realpath $(dirname "$0"))

rm -rf ${HERE}/test-coverage-report

#nix-build ${HERE}/test-coverage-report.nix -o ${HERE}/test-coverage-report.link

cp --no-preserve=mode --no-preserve=ownership --no-preserve=timestamps\
     --recursive --dereference \
    ${HERE}/test-coverage-report.link/share/hpc/vanilla/html/unliftio-protocols-* \
    ${HERE}/test-coverage-report

rm ${HERE}/test-coverage-report.link