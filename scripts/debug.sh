#!/usr/bin/env bash

set -eux

export RUST_BACKTRACE=1

if [ -f "$1" ]; then
    "$WD/scripts/build.sh" debug
    "$WD/bin/debug" "$1"
fi
