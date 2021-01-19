#!/usr/bin/env bash
#
# Build the software and generate reports.
#
# The only prequisite is that that Nix 2.x is
# available.
#
# This script takes long to execute.
#
set -xe

HERE=$(realpath $(dirname "$0"))

TARGET=${1:-/tmp}

rm -rf ${TARGET}/haddock 
mkdir -p ${TARGET}/haddock 

pushd ${HERE}
nix-shell \
   --run "cabal clean && cabal haddock --html-dir ${TARGET}/haddock > ${TARGET}/haddock/haddock.log"
popd

# nix-shell ${HERE}/nix/pkgs.nix \
#    --run ${HERE}/generate-reports.sh

