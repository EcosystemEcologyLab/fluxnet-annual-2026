#!/bin/bash
# Waits for stage 3b to report a status, checks disk, then runs stage 4
# (incremental DuckDB update -> 04_qc.R -> 05_units.R -> 07_figures.R) in
# order. A hard failure in any of these stops the remaining ones (unlike
# stage 3, which must complete regardless of individual site failures).
#
# DECISION RECORDED: 03_read.R is deliberately NOT run in this stage.
# duckdb_update.R (sourced by 03b_create_database.R) reads directly from
# data/extracted/ via flux_discover_files() and compares against its own
# manifest table recorded in the DuckDB file -- it does not consume
# 03_read.R's RDS outputs (badm.rds/var_info.rds/flux_data_raw_*.rds) at
# all (confirmed by reading scripts/duckdb_update.R directly). Those RDS
# outputs are separately cached and 03_read.R silently SKIPS regenerating
# any resolution whose output .rds already exists -- so running it would
# not actually refresh anything without deleting those cache files first,
# and data/processed/ is outside data/raw/, which this run's permissions
# do not authorize deleting. 03_read.R is skipped; this is flagged in the
# closing report as a limitation for any BADM-derived output (e.g. KG
# classification) that depends on it, to be checked explicitly in stage 5.
set -uo pipefail
cd "$(dirname "$0")/../.."

LOG="logs/store_refresh_stage4_chain_20260920.log"
STAGE3_DIR="review/diagnostics/store_refresh_20260920/stage3"
STAGE4_DIR="review/diagnostics/store_refresh_20260920/stage4"
mkdir -p "$STAGE4_DIR"

echo "=== stage4 chain watchdog started $(date), waiting for stage 3b status ===" >> "$LOG"

# Wait for the stage3->stage3b chain (driven by store_refresh_stage3_chain.sh)
# to finish, i.e. for a stage3b status file to appear.
while [ ! -f "${STAGE3_DIR}/STAGE3B_STATUS.txt" ]; do
  sleep 30
done

STAGE3B_STATUS=$(cat "${STAGE3_DIR}/STAGE3B_STATUS.txt")
echo "Stage 3b status: ${STAGE3B_STATUS} ($(date))" >> "$LOG"

if [ "$STAGE3B_STATUS" = "STAGE3B_ABORTED_DISK" ]; then
  echo "ABORT: stage 3b was aborted for disk space -- not proceeding to stage 4." >> "$LOG"
  exit 1
fi

AVAIL_GB=$(df -g /System/Volumes/Data | tail -1 | awk '{print $4}')
echo "Disk available before stage 4: ${AVAIL_GB} GB" >> "$LOG"
if [ "$AVAIL_GB" -lt 100 ]; then
  echo "ABORT: disk below 100GB (${AVAIL_GB}GB) -- not proceeding to stage 4." >> "$LOG"
  exit 1
fi

run_stage () {
  local name="$1"; shift
  local start=$(date +%s)
  echo "--- ${name} starting $(date) ---" >> "$LOG"
  "$@" >> "logs/store_refresh_stage4_${name}_20260920.log" 2>&1
  local rc=$?
  local end=$(date +%s)
  echo "--- ${name} finished $(date), exit ${rc}, elapsed $((end-start))s ---" >> "$LOG"
  echo "${name},${rc},$((end-start))" >> "${STAGE4_DIR}/table_stage4_elapsed.csv"
  return $rc
}

echo "name,exit_code,elapsed_seconds" > "${STAGE4_DIR}/table_stage4_elapsed.csv"

run_stage "duckdb_incremental" Rscript scripts/03b_create_database.R
if [ $? -ne 0 ]; then
  echo "HARD FAILURE at duckdb_incremental -- stopping stage 4." >> "$LOG"
  echo "STAGE4_HARD_FAILURE:duckdb_incremental" > "${STAGE4_DIR}/STAGE4_STATUS.txt"
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
echo "=== stage4 chain complete $(date) ===" >> "$LOG"
