#!/bin/bash
# Third stage-4 attempt: uses store_refresh_stage4_duckdb_no_hourly.R
# instead of scripts/03b_create_database.R (which fails on a genuine
# temp-disk exhaustion in the hourly block on this 16GB machine, unrelated
# to this refresh's y/m/d-only scope) -- see that script's header for the
# full recorded decision. Then proceeds to 04_qc.R/05_units.R/07_figures.R
# unmodified, same hard-failure-stops semantics.
set -uo pipefail
cd "$(dirname "$0")/../.."

LOG="logs/store_refresh_stage4_chain_20260920.log"
STAGE4_DIR="review/diagnostics/store_refresh_20260920/stage4"

echo "=== stage4 RETRY #3 (no-hourly duckdb update), started $(date) ===" >> "$LOG"

run_stage () {
  local name="$1"; shift
  local start=$(date +%s)
  echo "--- ${name} starting (retry3) $(date) ---" >> "$LOG"
  "$@" >> "logs/store_refresh_stage4_${name}_20260920_retry3.log" 2>&1
  local rc=$?
  local end=$(date +%s)
  echo "--- ${name} finished (retry3) $(date), exit ${rc}, elapsed $((end-start))s ---" >> "$LOG"
  echo "${name}_retry3,${rc},$((end-start))" >> "${STAGE4_DIR}/table_stage4_elapsed.csv"
  return $rc
}

run_stage "duckdb_no_hourly" Rscript scripts/diagnostics/store_refresh_stage4_duckdb_no_hourly.R
if [ $? -ne 0 ]; then
  echo "HARD FAILURE at duckdb_no_hourly -- stopping stage 4." >> "$LOG"
  echo "STAGE4_HARD_FAILURE:duckdb_no_hourly" > "${STAGE4_DIR}/STAGE4_STATUS.txt"
  exit 1
fi

run_stage "04_qc" Rscript scripts/04_qc.R
if [ $? -ne 0 ]; then
  echo "HARD FAILURE at 04_qc -- stopping stage 4." >> "$LOG"
  echo "STAGE4_HARD_FAILURE:04_qc" > "${STAGE4_DIR}/STAGE4_STATUS.txt"
  exit 1
fi

run_stage "05_units" Rscript scripts/05_units.R
if [ $? -ne 0 ]; then
  echo "HARD FAILURE at 05_units -- stopping stage 4." >> "$LOG"
  echo "STAGE4_HARD_FAILURE:05_units" > "${STAGE4_DIR}/STAGE4_STATUS.txt"
  exit 1
fi

run_stage "07_figures" Rscript scripts/07_figures.R
if [ $? -ne 0 ]; then
  echo "HARD FAILURE at 07_figures -- stopping stage 4." >> "$LOG"
  echo "STAGE4_HARD_FAILURE:07_figures" > "${STAGE4_DIR}/STAGE4_STATUS.txt"
  exit 1
fi

echo "STAGE4_COMPLETE" > "${STAGE4_DIR}/STAGE4_STATUS.txt"
echo "=== stage4 retry3 chain complete $(date) ===" >> "$LOG"
