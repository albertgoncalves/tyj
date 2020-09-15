#!/usr/bin/env bash

export FLAGS=(
    --color "always"
    -W "absolute-paths-not-starting-with-crate"
    -W "anonymous-parameters"
    -W "deprecated-in-future"
    -W "explicit-outlives-requirements"
    -W "indirect-structural-match"
    -W "keyword-idents"
    -W "macro-use-extern-crate"
    -W "meta-variable-misuse"
    -W "missing-copy-implementations"
    -W "missing-debug-implementations"
    -W "non-ascii-idents"
    -W "trivial-casts"
    -W "trivial-numeric-casts"
    -W "unreachable-pub"
    -W "unused-extern-crates"
    -W "unused-import-braces"
    -W "unused-lifetimes"
    -W "unused-qualifications"
    -W "unused-results"
)
export DEBUG_FLAGS=(
    -C "opt-level=0"
    -C "overflow-checks=yes"
    -C "panic=unwind"
)
export RELEASE_FLAGS=(
    -C "codegen-units=1"
    -C "lto=fat"
    -C "opt-level=3"
    -C "overflow-checks=no"
    -C "panic=abort"
)
