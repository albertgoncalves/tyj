#!/usr/bin/env bash

set -eux

export RUST_BACKTRACE=0

"$WD/scripts/build.sh" test
"$WD/bin/test"
