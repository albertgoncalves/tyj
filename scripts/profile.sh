#!/usr/bin/env bash

set -euxo pipefail

"$WD/scripts/build.sh" release

sudo sh -c "echo 1 > /proc/sys/kernel/perf_event_paranoid"
sudo sh -c "echo 0 > /proc/sys/kernel/kptr_restrict"
perf record --call-graph fp "$WD/bin/release" "$1" > /dev/null 2>&1 \
    || true
perf report
valgrind --tool=cachegrind --branch-sim=yes "$WD/bin/release" "$1" 2>&1 \
    | less || true
rm cachegrind.out*
rm perf.data*
rm vgcore* || true
