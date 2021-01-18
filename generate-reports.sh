#!/usr/bin/env bash

set -xe

HERE=$(realpath $(dirname "$0"))

${HERE}/generate-test-coverage-report.sh
${HERE}/generate-test-profiling-report.sh
${HERE}/generate-benchmark-report.sh
