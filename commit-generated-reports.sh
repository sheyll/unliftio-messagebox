#!/usr/bin/env bash

#
# Git commit and push the ./generated-reports folder.
#

set -xe

HERE=$(realpath $(dirname "$0"))

git -C ${HERE} add generated-reports
git -C ${HERE} commit "Update generated-reports"
git -C ${HERE} push
