#!/usr/bin/env bash

set -eux

. "$WD/scripts/flags.sh"

rustc \
    --emit "dep-info,metadata" \
    -C "embed-bitcode=no" \
    "${FLAGS[@]}" \
    "$WD/src/main.rs"
rm libmain.rmeta
rm main.d
clippy-driver \
    -D warnings \
    -W clippy::pedantic \
    -A clippy::similar-names \
    -A clippy::too_many_lines \
    -A unused-variables \
    --test \
    "$WD/src/main.rs"
rm main
rustfmt "$WD/src/main.rs"
