#!/bin/bash
# Resumes stage 4 after the schema patch: re-runs duckdb_incremental (now
# expected to succeed), then 04_qc.R, 05_units.R, 07_figures.R, same
# hard-failure-stops semantics as store_refresh_stage4_chain.sh.
set -uo pipefail
cd "$(dirname "$0")/../.."

LOG="logs/store_refresh_stage4_chain_20260920.log"
STAGE4_DIR="review/diagnostics/store_refresh_20260920/stage4"

echo "=== stage4 RETRY after schema patch, started $(date) ===" >> "$LOG"

run_stage () {
  local name="$1"; shift
  local start=$(date +%s)
  echo "--- ${name} starting (retry) $(date) ---" >> "$LOG"
  "$@" >> "logs/store_refresh_stage4_${name}_20260920_retry.log" 2>&1
  local rc=$?
  local end=$(date +%s)
  echo "--- ${name} finished (retry) $(date), exit ${rc}, elapsed $((end-start))s ---" >> "$LOG"
  echo "${name}_retry,${rc},$((end-start))" >> "${STAGE4_DIR}/table_stage4_elapsed.csv"
  return $rc
}

run_stage "duckdb_incremental" Rscript scripts/03b_create_database.R
if [ $? -ne 0 ]; then
  echo "HARD FAILURE at duckdb_incremental (retry) -- stopping stage 4." >> "$LOG"
  echo "STAGE4_HARD_FAILURE:duckdb_incremental_retry" > "${STAGE4_DIR}/STAGE4_STATUS.txt"
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
echo "=== stage4 retry chain complete $(date) ===" >> "$LOG"
