#!/usr/bin/env bash

set -eux

. "$WD/scripts/flags.sh"

if [ "$1" = "test" ]; then
    rustc \
        -C "incremental=$WD/cache/test" \
        -o "$WD/bin/test" \
        "${FLAGS[@]}" \
        --test \
        "$WD/src/main.rs"
elif [ "$1" = "main" ]; then
    rustc \
        -C "incremental=$WD/cache/main" \
        -o "$WD/bin/main" \
        "${FLAGS[@]}" \
        "$WD/src/main.rs"
fi
