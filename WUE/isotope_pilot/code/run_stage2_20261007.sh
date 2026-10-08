#!/bin/bash
# run_stage2_20261007.sh — unattended stage 2 run for the WUE isotope pilot.
# Runs 05-10 in order for the 12 stage-2 sites (CH-Dav dropped -- see
# docs/methods_memo.md). Deliberately does NOT use `set -e`, same reasoning
# as run_setup_20261007.sh: a failure partway through must still be followed
# by whichever later steps can run on partial output, so the report states
# what failed rather than the whole run aborting silently. Does NOT run
# 03_fetch_treering.R -- tree-ring data are out of scope for stage 2.
#
# Usage (from repo root, in a plain terminal, not a Claude Code session):
#   nohup caffeinate -i bash WUE/isotope_pilot/code/run_stage2_20261007.sh \
#     >> WUE/isotope_pilot/logs/stage2_20261007.log 2>&1 & disown

set -uo pipefail

WUE_ROOT="WUE/isotope_pilot"
LOG="$WUE_ROOT/logs/stage2_20261007.log"
mkdir -p "$WUE_ROOT/logs"

ts() { date -u +%Y-%m-%dT%H:%M:%SZ; }

echo "$(ts) === WUE isotope pilot STAGE 2 run START ==="
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

run_step "05_read_subdaily_wue"   "$WUE_ROOT/code/05_read_subdaily_wue.R"
run_step "06_build_site_years"    "$WUE_ROOT/code/06_build_site_years.R"
run_step "07_apply_screens"       "$WUE_ROOT/code/07_apply_screens.R"
run_step "08_compute_metrics"     "$WUE_ROOT/code/08_compute_metrics.R"
run_step "09_make_figures"        "$WUE_ROOT/code/09_make_figures.R"
run_step "10_report_stage2"       "$WUE_ROOT/code/10_report_stage2.R"

echo "$(ts) === ALL STEPS ATTEMPTED ==="

mkdir -p "$WUE_ROOT/logs"
if [ ${#FAILED_STEPS[@]} -eq 0 ]; then
  echo "COMPLETE" > "$WUE_ROOT/logs/STATUS_STAGE2"
else
  echo "FAILED: ${FAILED_STEPS[*]}" > "$WUE_ROOT/logs/STATUS_STAGE2"
fi
echo "$(ts) STATUS: $(cat "$WUE_ROOT/logs/STATUS_STAGE2")"

# --- Commit + push tables, figures, and docs, whatever completed ----------

echo "$(ts) === git add/commit/push ==="
git add "$WUE_ROOT/tables" "$WUE_ROOT/figures" "$WUE_ROOT/docs"

if git diff --cached --quiet; then
  echo "$(ts) Nothing staged in tables/, figures/, or docs/ -- skipping commit."
else
  git commit -m "$(cat <<'EOF'
WUE isotope pilot stage 2: screens, WUE metrics, figures, report

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

echo "$(ts) === WUE isotope pilot STAGE 2 run END ==="
