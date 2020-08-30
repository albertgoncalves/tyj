#!/usr/bin/env bash

set -eux

"$WD/scripts/build.sh"

sudo sh -c "echo 1 > /proc/sys/kernel/perf_event_paranoid"
sudo sh -c "echo 0 > /proc/sys/kernel/kptr_restrict"

(
    set +e
    perf record --call-graph fp "$WD/bin/main" "$1" > /dev/null 2>&1
    set -e
)

perf report
rm perf.data*

(
    set +e
    valgrind --tool=cachegrind --branch-sim=yes "$WD/bin/main" "$1" 2>&1 | less
    set -e
)

rm cachegrind.out.*
