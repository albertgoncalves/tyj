#!/usr/bin/env bash

set -eux

. "$WD/scripts/flags.sh"

set +u

if [ "$1" = "test" ]; then
    rustc -o "$WD/bin/test" "${FLAGS[@]}" --test "$WD/src/main.rs"
else
    rustc -o "$WD/bin/main" "${FLAGS[@]}" "$WD/src/main.rs"
fi
