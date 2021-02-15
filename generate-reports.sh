#!/usr/bin/env bash

#
# Generate all reports that reside in ./generated-reports.
#
# This report contains several sub reports, for examlpe: 
# test-coverage, test-profiling and benchmarks.
#
# In order to prevent unnecessary rebuilts due to a tainted
# workspace folder, all sub-reports are rendered to a
# temporary directory, before being copied back into the 
# source dircetory.

set -xe

HERE=$(realpath $(dirname "$0"))

TARGET=$(mktemp -d)

${HERE}/generate-haddock-report.sh ${TARGET}
${HERE}/generate-module-graph.sh ${TARGET}
${HERE}/generate-test-coverage-report.sh ${TARGET}
${HERE}/generate-test-profiling-report.sh ${TARGET}
${HERE}/generate-benchmark-report.sh ${TARGET}
${HERE}/generate-messagebox-memleak-test-report.sh ${TARGET}
${HERE}/generate-pool-memleak-test-report.sh ${TARGET}

# copy all reports to the workspace folder

rm -rf ${HERE}/generated-reports
mkdir -p ${HERE}/generated-reports

cp --no-preserve=mode \
   --no-preserve=ownership \
   --no-preserve=timestamps \
   --recursive \
   --dereference \
   ${TARGET}/* \
   ${HERE}/generated-reports/
