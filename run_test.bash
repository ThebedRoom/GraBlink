#!/bin/bash

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

mkdir -p "run_tests/$1"

TEST_DIR="$(realpath benchmarks/$1)"
RUN_DIR="$(realpath run_tests/$1)"
export GRABLINK="$(realpath ./target/release/grablink)"

"$GRABLINK" --time --$2 "$TEST_DIR" > "$RUN_DIR/$2.out" 2> "$RUN_DIR/$2.err"

printf "${GREEN}[FINISHED]${NC} ${1} ${2}\n"