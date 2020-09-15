#!/usr/bin/env bash

set -eux

. "$WD/scripts/flags.sh"

case "$1" in
    "test")
        rustc \
            -C "incremental=$WD/cache/test" \
            -o "$WD/bin/test" \
            "${FLAGS[@]}" \
            "${DEBUG_FLAGS[@]}" \
            --test \
            "$WD/src/main.rs"
        ;;
    "debug")
        rustc \
            -C "incremental=$WD/cache/debug" \
            -o "$WD/bin/debug" \
            "${FLAGS[@]}" \
            "${DEBUG_FLAGS[@]}" \
            "$WD/src/main.rs"
        ;;
    "release")
        rustc \
            -C "incremental=$WD/cache/release" \
            -o "$WD/bin/release" \
            "${FLAGS[@]}" \
            "${RELEASE_FLAGS[@]}" \
            "$WD/src/main.rs"
        ;;
esac
