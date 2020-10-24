#!/usr/bin/env bash

set -eux

read -r -a flags <<< "$FLAGS"
read -r -a debug_flags <<< "$DEBUG_FLAGS"
read -r -a release_flags <<< "$RELEASE_FLAGS"

case "$1" in
    "test")
        rustc \
            -C "incremental=$WD/cache/test" \
            -o "$WD/bin/test" \
            "${flags[@]}" \
            "${debug_flags[@]}" \
            --test \
            "$WD/src/main.rs"
        ;;
    "debug")
        rustc \
            -C "incremental=$WD/cache/debug" \
            -o "$WD/bin/debug" \
            "${flags[@]}" \
            "${debug_flags[@]}" \
            "$WD/src/main.rs"
        ;;
    "release")
        rustc \
            -C "incremental=$WD/cache/release" \
            -o "$WD/bin/release" \
            "${flags[@]}" \
            "${release_flags[@]}" \
            "$WD/src/main.rs"
        ;;
esac
