#!/bin/bash
# Waits for stage 4 to complete, then runs stage 5 (reconcile) and stage 6
# (cluster recompute) in sequence. Both are read-only against the refreshed
# DuckDB. The closing report/session-log/final commit is written manually
# afterward (needs synthesis of the actual numbers, not automatable here).
set -uo pipefail
cd "$(dirname "$0")/../.."

LOG="logs/store_refresh_stage56_chain_20260920.log"
STAGE4_DIR="review/diagnostics/store_refresh_20260920/stage4"

echo "=== stage56 chain watchdog started $(date), waiting for stage 4 status ===" >> "$LOG"

while [ ! -f "${STAGE4_DIR}/STAGE4_STATUS.txt" ]; do
  sleep 30
done

STAGE4_STATUS=$(cat "${STAGE4_DIR}/STAGE4_STATUS.txt")
echo "Stage 4 status: ${STAGE4_STATUS} ($(date))" >> "$LOG"

if [ "$STAGE4_STATUS" != "STAGE4_COMPLETE" ]; then
  echo "ABORT: stage 4 did not complete cleanly (${STAGE4_STATUS}) -- not running stage 5/6." >> "$LOG"
  exit 1
fi

echo "--- stage 5 starting $(date) ---" >> "$LOG"
Rscript scripts/diagnostics/store_refresh_stage5_reconcile.R >> logs/store_refresh_stage5_20260920.log 2>&1
echo "--- stage 5 finished $(date), exit $? ---" >> "$LOG"

echo "--- stage 6 starting $(date) ---" >> "$LOG"
Rscript scripts/diagnostics/store_refresh_stage6_cluster.R >> logs/store_refresh_stage6_20260920.log 2>&1
echo "--- stage 6 finished $(date), exit $? ---" >> "$LOG"

echo "STAGE56_COMPLETE" > "review/diagnostics/store_refresh_20260920/STAGE56_STATUS.txt"
echo "=== stage56 chain complete $(date) ===" >> "$LOG"
