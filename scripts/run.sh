#!/usr/bin/env bash

set -eux

export RUST_BACKTRACE=1

if [ "$1" = "test" ]; then
    "$WD/scripts/build.sh" test
    "$WD/bin/test"
else
    "$WD/scripts/build.sh" debug
    "$WD/bin/debug" "$1"
fi
