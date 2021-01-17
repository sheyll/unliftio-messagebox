#!/usr/bin/env bash

set -xe

nix-build ./test-coverage-report.nix -o result-test-coverage
ln -snf ./result-test-coverage/share/hpc/vanilla/html/unliftio-protocols-*/unliftio-protocols-test result-test-coverage-html
