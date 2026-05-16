#!/bin/sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
REPO_DIR=$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)

JULIA_BIN="${JULIA_BIN:-julia}"
TEST_FILE="${TEST_FILE:-$REPO_DIR/tests/run-julia-tests.jl}"
CHANNELS="${*:-1.8 1.10 release}"

if [ ! -x "$JULIA_BIN" ]; then
  echo "error: julia binary not found at $JULIA_BIN" >&2
  exit 1
fi

if [ ! -f "$TEST_FILE" ]; then
  echo "error: test file not found at $TEST_FILE" >&2
  exit 1
fi

for channel in $CHANNELS; do
  echo "==> Julia channel: $channel"
  "$JULIA_BIN" "+$channel" "$TEST_FILE"
  echo
done
