#!/usr/bin/env bash

set -xe

HERE=$(realpath $(dirname "$0"))

nix build  --max-jobs auto -f ${HERE}/generate-benchmark-report.nix -o ${HERE}/benchmark-report.link 

mkdir -p ${HERE}/benchmark-report

cp ${HERE}/benchmark-report.link/*.html ${HERE}/benchmark-report/
rm ${HERE}/benchmark-report.link