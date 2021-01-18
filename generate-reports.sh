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

${HERE}/generate-test-coverage-report.sh ${TARGET}
${HERE}/generate-test-profiling-report.sh ${TARGET}
${HERE}/generate-benchmark-report.sh ${TARGET}

# copy all reports to the workspace folder

rm -rf ${HERE}/generated-reports
mkdir -p ${HERE}/generated-reports

cp --no-preserve=mode \
   --no-preserve=ownership \
   --no-preserve=timestamps \
   --recursive \
   --dereference \
   ${TARGET} \ 
   ${HERE}/generated-reports/

if [[ -v GIT_COMMIT_REPORTS ]]
then
    git -C ${HERE} add generated-reports
    git -C ${HERE} commit "Update generated-reports"
    git -C ${HERE} push
fi

