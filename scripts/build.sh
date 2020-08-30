#!/usr/bin/env bash

set -eux

. "$WD/scripts/flags.sh"

if [ "$1" = "test" ]; then
    rustc -o "$WD/bin/test" "${FLAGS[@]}" --test "$WD/src/main.rs"
elif [ "$1" = "main" ]; then
    rustc -o "$WD/bin/main" "${FLAGS[@]}" "$WD/src/main.rs"
fi
