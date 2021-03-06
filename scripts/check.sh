#!/usr/bin/env bash

set -eux

read -r -a flags <<< "$FLAGS"

rustc \
    --emit "dep-info,metadata" \
    -C "embed-bitcode=no" \
    "${flags[@]}" \
    "$WD/src/main.rs"
rm libmain.rmeta
rm main.d
clippy-driver \
    -W clippy::pedantic \
    -A clippy::similar-names \
    -A clippy::too_many_lines \
    -A dead-code \
    -A unreachable-code \
    -A unused-macros \
    -A unused-mut \
    -A unused-variables \
    -D warnings \
    --test \
    "$WD/src/main.rs"
rm main
rustfmt "$WD/src/main.rs"
