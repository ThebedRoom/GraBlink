#!/bin/bash

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

mkdir -p "run_tests/$1"

TEST_DIR="$(realpath benchmarks/$1)"
RUN_DIR="$(realpath run_tests/$1)"
export GRABLINK="$(realpath ./target/release/grablink)"

"$GRABLINK" --time --egraph "$TEST_DIR" > "$RUN_DIR/egraph.out" 2> "$RUN_DIR/egraph.err"
"$GRABLINK" --time --vsa "$TEST_DIR" > "$RUN_DIR/vsa.out" 2> "$RUN_DIR/vsa.err"
# "$GRABLINK" --time --enum "$TEST_DIR" > "$RUN_DIR/enum.out" 2> "$RUN_DIR/enum.err"

printf "${GREEN}[FINISHED]${NC} ${1}\n"