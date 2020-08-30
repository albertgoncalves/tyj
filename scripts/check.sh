#!/usr/bin/env bash

set -eux

. "$WD/scripts/flags.sh"

cd "$WD/src" || return
rustc \
    --emit "dep-info,metadata" \
    -C "embed-bitcode=no" \
    "${FLAGS[@]}" \
    "$WD/src/main.rs"
rm "$WD/src/libmain.rmeta"
rm "$WD/src/main.d"
clippy-driver \
    -D warnings \
    -W clippy::pedantic \
    -A clippy::similar-names \
    -A clippy::too_many_lines \
    -A unused-variables \
    --test \
    "$WD/src/main.rs"
rm "$WD/src/main"
rustfmt "$WD/src/main.rs"
