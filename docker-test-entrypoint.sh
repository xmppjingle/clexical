#!/bin/sh
##
## docker-test-entrypoint.sh
## Runs Common Test suites inside the test container.
##
## Environment variables:
##   RUN_STRESS  — set to "1" to also run the stress benchmark suite
##   STRESS_N    — requests per worker (default 200)
##   STRESS_C    — concurrent workers  (default 20)
##

set -e

cd /app

echo ""
echo "======================================================"
echo " clexical test runner"
echo " RUN_STRESS=${RUN_STRESS:-0}  STRESS_N=${STRESS_N:-200}  STRESS_C=${STRESS_C:-20}"
echo "======================================================"
echo ""

# Run unit + integration tests
echo ">>> Running unit and integration tests..."
rebar3 as test ct \
    --suite clexical_letter_SUITE,clexical_integration_SUITE \
    --readable true

UNIT_RESULT=$?

if [ "${RUN_STRESS:-0}" = "1" ]; then
    echo ""
    echo ">>> Running stress / benchmark suite..."
    echo "    STRESS_N=${STRESS_N:-200}  STRESS_C=${STRESS_C:-20}"
    echo ""
    STRESS_N="${STRESS_N:-200}" STRESS_C="${STRESS_C:-20}" \
        rebar3 as test ct \
        --suite clexical_stress_SUITE \
        --readable true
    STRESS_RESULT=$?
else
    STRESS_RESULT=0
fi

echo ""
echo "======================================================"
if [ "$UNIT_RESULT" -eq 0 ] && [ "$STRESS_RESULT" -eq 0 ]; then
    echo " ALL TESTS PASSED"
    echo "======================================================"
    exit 0
else
    echo " TESTS FAILED  (unit=$UNIT_RESULT  stress=$STRESS_RESULT)"
    echo "======================================================"
    exit 1
fi
