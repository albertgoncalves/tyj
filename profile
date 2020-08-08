#!/usr/bin/env bash

set -euo pipefail

sudo sh -c "echo 1 > /proc/sys/kernel/perf_event_paranoid"
sudo sh -c "echo 0 > /proc/sys/kernel/kptr_restrict"
perf record --call-graph fp "$WD/bin/main" < "$1" > /dev/null 2>&1
perf report
rm perf.data*

valgrind --tool=cachegrind --branch-sim=yes "$WD/bin/main" < "$1" 2>&1 | less
rm cachegrind.out.*
