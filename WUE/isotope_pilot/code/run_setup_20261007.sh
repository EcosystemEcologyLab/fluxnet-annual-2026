#!/bin/bash
# run_setup_20261007.sh — unattended setup run for the WUE isotope pilot.
# Runs 01-04 in order for all 13 sites. Deliberately does NOT use `set -e`:
# a hard failure in 01 or 02 must still be followed by 03 and 04 running on
# whatever exists, so the report states which step failed and for which
# sites, rather than the whole run aborting silently partway through.
#
# Usage (from repo root, in a plain terminal, not a Claude Code session):
#   nohup caffeinate -i bash WUE/isotope_pilot/code/run_setup_20261007.sh \
#     >> WUE/isotope_pilot/logs/setup_20261007.log 2>&1 & disown

set -uo pipefail

WUE_ROOT="WUE/isotope_pilot"
LOG="$WUE_ROOT/logs/setup_20261007.log"
mkdir -p "$WUE_ROOT/logs"

ts() { date -u +%Y-%m-%dT%H:%M:%SZ; }

echo "$(ts) === WUE isotope pilot setup run START ==="
echo "$(ts) Git: $(git rev-parse --short HEAD 2>/dev/null || echo unknown)"
echo "$(ts) Host: $(hostname)"

FAILED_STEPS=()

run_step() {
  local label="$1" script="$2"
  echo "$(ts) === START $label ($script) ==="
  Rscript "$script"
  local rc=$?
  if [ $rc -ne 0 ]; then
    echo "$(ts) !!! FAILED $label (exit $rc) -- continuing to next step regardless ==="
    FAILED_STEPS+=("$label")
  else
    echo "$(ts) === DONE  $label ==="
  fi
  return 0
}

run_step "01_download_extract" "$WUE_ROOT/code/01_download_extract.R"
run_step "02_read_subdaily"    "$WUE_ROOT/code/02_read_subdaily.R"

# 03 and 04 ALWAYS run, regardless of 01/02 outcome, so the report reflects
# whatever partial data exists.
run_step "03_fetch_treering"      "$WUE_ROOT/code/03_fetch_treering.R"
run_step "04_report_preanalysis"  "$WUE_ROOT/code/04_report_preanalysis.R"

echo "$(ts) === ALL STEPS ATTEMPTED ==="

mkdir -p "$WUE_ROOT/logs"
if [ ${#FAILED_STEPS[@]} -eq 0 ]; then
  echo "COMPLETE" > "$WUE_ROOT/logs/STATUS"
else
  echo "FAILED: ${FAILED_STEPS[*]}" > "$WUE_ROOT/logs/STATUS"
fi
echo "$(ts) STATUS: $(cat "$WUE_ROOT/logs/STATUS")"

# --- Commit + push tables and docs, whatever 04 managed to write -----------

echo "$(ts) === git add/commit/push ==="
git add "$WUE_ROOT/tables" "$WUE_ROOT/docs"

if git diff --cached --quiet; then
  echo "$(ts) Nothing staged in tables/ or docs/ -- skipping commit."
else
  git commit -m "$(cat <<'EOF'
WUE isotope pilot: unattended setup run output (tables + report)

Co-Authored-By: Claude Sonnet 5 <noreply@anthropic.com>
EOF
)"
  commit_rc=$?
  if [ $commit_rc -ne 0 ]; then
    echo "$(ts) !!! git commit failed (exit $commit_rc) ==="
  else
    if git push; then
      echo "$(ts) Push succeeded."
    else
      echo "$(ts) !!! git push FAILED -- commit left local, not retried, not forced ==="
    fi
  fi
fi

echo "$(ts) === WUE isotope pilot setup run END ==="
