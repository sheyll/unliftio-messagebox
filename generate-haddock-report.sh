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

rm -rf ${TARGET}/haddock-report 
mkdir -p ${TARGET}/haddock-report 

pushd ${HERE}
nix-shell --run "cabal clean"
nix-shell --pure --run "cabal haddock --htmldir ${TARGET}/haddock-report > ${TARGET}/haddock-report/build.log"
HTML_DIR="$(dirname $(tail -n1 ${TARGET}/haddock-report/build.log) )"
if [[ -d "${HTML_DIR}" ]]
then
   cp -r --dereference ${HTML_DIR} ${TARGET}/haddock-report
fi   
popd

