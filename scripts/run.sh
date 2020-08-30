#!/usr/bin/env bash

set -ex

export RUST_BACKTRACE=1

if [ -z "$1" ]; then
    "$WD/scripts/build.sh" test
    "$WD/bin/test"
else
    "$WD/scripts/build.sh"
    "$WD/bin/main" "$1"
fi
