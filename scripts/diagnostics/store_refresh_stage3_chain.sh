#!/bin/bash
# Waits for stage 3's download PID to exit, checks disk space and status,
# then launches stage 3b (the audit-gap top-up) automatically.
set -uo pipefail
cd "$(dirname "$0")/../.."

STAGE3_PID="$1"
LOG="logs/store_refresh_stage3_chain_20260920.log"

echo "=== chain watchdog started $(date), waiting on PID ${STAGE3_PID} ===" >> "$LOG"

while kill -0 "$STAGE3_PID" 2>/dev/null; do
  sleep 15
done

echo "=== stage 3 PID ${STAGE3_PID} exited $(date) ===" >> "$LOG"

AVAIL_GB=$(df -g /System/Volumes/Data | tail -1 | awk '{print $4}')
echo "Disk available: ${AVAIL_GB} GB" >> "$LOG"

if [ -f "review/diagnostics/store_refresh_20260920/stage3/STAGE3_STATUS.txt" ]; then
  STATUS=$(cat "review/diagnostics/store_refresh_20260920/stage3/STAGE3_STATUS.txt")
else
  STATUS="MISSING"
fi
echo "Stage 3 status file: ${STATUS}" >> "$LOG"

if [ "$AVAIL_GB" -lt 100 ]; then
  echo "ABORT: disk below 100GB (${AVAIL_GB}GB) -- not launching stage 3b or stage 4." >> "$LOG"
  echo "STAGE3B_ABORTED_DISK" > review/diagnostics/store_refresh_20260920/stage3/STAGE3B_STATUS.txt
  exit 1
fi

if [ "$STATUS" != "STAGE3_COMPLETE" ] && [ "$STATUS" != "STAGE3_COMPLETE_NOOP" ]; then
  echo "ABORT: stage 3 did not report a completion status (got '${STATUS}') -- not launching stage 3b." >> "$LOG"
  exit 1
fi

echo "Launching stage 3b..." >> "$LOG"
Rscript scripts/diagnostics/store_refresh_stage3b_audit_gap.R >> logs/store_refresh_stage3b_download_20260920.log 2>&1
echo "=== stage 3b finished $(date), exit code $? ===" >> "$LOG"
