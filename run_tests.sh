#!/bin/bash
##
## run_tests.sh — Single-command test runner for clexical.
##
## Usage:
##   ./run_tests.sh                        # unit + integration tests
##   ./run_tests.sh --stress               # add stress/benchmark suite
##   ./run_tests.sh --stress -n 1000       # stress with 1000 req/worker
##   ./run_tests.sh --stress -n 1000 -c 50 # 1000 req x 50 workers
##
## Options:
##   --stress       Run the benchmark suite after unit/integration tests
##   -n <number>    Requests per worker (default: 200)
##   -c <number>    Concurrent workers  (default: 20)
##   --no-build     Skip docker build (use cached image)
##   --local        Run tests locally without Docker (requires Erlang + rebar3)
##
## Output:
##   CT HTML reports in _build/test/logs/
##

set -euo pipefail

# ---- Defaults ----
RUN_STRESS=0
STRESS_N=200
STRESS_C=20
NO_BUILD=0
USE_LOCAL=0

# ---- Parse arguments ----
while [[ $# -gt 0 ]]; do
    case "$1" in
        --stress)   RUN_STRESS=1; shift ;;
        -n)         STRESS_N="${2:?'-n requires a number'}"; shift 2 ;;
        -c)         STRESS_C="${2:?'-c requires a number'}"; shift 2 ;;
        --no-build) NO_BUILD=1; shift ;;
        --local)    USE_LOCAL=1; shift ;;
        -h|--help)
            sed -n '3,20p' "$0"    # print the usage comment
            exit 0
            ;;
        *)
            echo "Unknown option: $1  (use --help for usage)"
            exit 1
            ;;
    esac
done

echo ""
echo "╔══════════════════════════════════════════════════╗"
echo "║           clexical test runner                   ║"
echo "╠══════════════════════════════════════════════════╣"
printf "║  stress=%-5s  N=%-6s  C=%-6s               ║\n" \
    "$RUN_STRESS" "$STRESS_N" "$STRESS_C"
echo "╚══════════════════════════════════════════════════╝"
echo ""

# ---- Local run (no Docker) ----
if [[ "$USE_LOCAL" -eq 1 ]]; then
    REBAR="${REBAR3:-rebar3}"
    echo ">>> Running locally with rebar3..."
    "$REBAR" as test ct \
        --suite clexical_letter_SUITE,clexical_integration_SUITE \
        --readable true

    if [[ "$RUN_STRESS" -eq 1 ]]; then
        echo ""
        echo ">>> Running stress suite (N=${STRESS_N}  C=${STRESS_C})..."
        STRESS_N="$STRESS_N" STRESS_C="$STRESS_C" \
            "$REBAR" as test ct \
            --suite clexical_stress_SUITE \
            --readable true
    fi
    echo ""
    echo "Done. CT reports: _build/test/logs/"
    exit 0
fi

# ---- Docker run ----
COMPOSE_CMD="docker compose"
if ! command -v "docker" &>/dev/null; then
    echo "ERROR: docker not found. Install Docker or use --local flag."
    exit 1
fi

# Build image
if [[ "$NO_BUILD" -eq 0 ]]; then
    echo ">>> Building test image..."
    "$COMPOSE_CMD" -f docker-compose.test.yml build test
fi

# Run tests
echo ">>> Running tests in Docker..."
RUN_STRESS="$RUN_STRESS" \
STRESS_N="$STRESS_N"     \
STRESS_C="$STRESS_C"     \
    "$COMPOSE_CMD" -f docker-compose.test.yml run --rm test

STATUS=$?

echo ""
if [[ "$STATUS" -eq 0 ]]; then
    echo "✓ All tests passed."
else
    echo "✗ Tests failed (exit code $STATUS)."
fi
exit "$STATUS"
