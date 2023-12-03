#!/bin/bash

BASE="$(pwd)"
RUN_TEST="$(realpath run_test.bash)"

cd benchmarks

ANY_FAIL=no

for test in *; do
        
        cd ..
        if ! [ -d $(realpath run_tests/$test) ]; then {
            mkdir -p "run_tests/$test"
        }
        fi
        if ! [ -f $(realpath run_tests/$test/egraph.out) ]; then
            "$RUN_TEST" "$test" "egraph"
            if [ $? -eq 1 ]; then
                ANY_FAIL=yes
            fi
        fi
        # if ! [ -f "run_tests/$test/vsa.out" ]; then
        #     "$RUN_TEST" "$test" "vsa"
        #     if [ $? -eq 1 ]; then
        #         ANY_FAIL=yes
        #     fi
        # fi
        
        cd benchmarks
done

cd "$BASE"

if [[ $ANY_FAIL == yes ]]; then
    exit 1
fi