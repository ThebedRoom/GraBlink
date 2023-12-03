#!/bin/bash

BASE="$(pwd)"
RUN_TEST="$(realpath run_test.bash)"

cd benchmarks

ANY_FAIL=no

for test in *; do
        cd ..
        "$RUN_TEST" "$test"
        if [ $? -eq 1 ]; then
            ANY_FAIL=yes
        fi
        cd benchmarks
done

cd "$BASE"

if [[ $ANY_FAIL == yes ]]; then
    exit 1
fi