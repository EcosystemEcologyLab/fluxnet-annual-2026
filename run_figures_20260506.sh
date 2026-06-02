#!/bin/bash
# run_figures_20260506.sh
# Full figure regeneration — Phase 1 (7 scripts, sequential) + Phase 2 (candidates/ refresh)
# Log: logs/figures_full_regeneration_20260506.log
# Usage: nohup bash run_figures_20260506.sh & disown

set -euo pipefail

LOG="logs/figures_full_regeneration_20260506.log"
mkdir -p logs

# Redirect all output to log from this point on
exec >> "$LOG" 2>&1

ts() { date -u +%Y-%m-%dT%H:%M:%SZ; }

run_script() {
    local script="$1"
    echo "$(ts) === START $script ==="
    Rscript "$script"
    local rc=$?
    if [ $rc -ne 0 ]; then
        echo "$(ts) !!! FAILED $script (exit $rc) — aborting ==="
        exit $rc
    fi
    echo "$(ts) === DONE  $script ==="
}

echo "$(ts) === FIGURE REGENERATION START (Phase 1 + 2) ==="
echo "$(ts) Git: $(git rev-parse --short HEAD)"

# ── Phase 1: sequential figure generation ─────────────────────────────────────
run_script scripts/00_candidate_figures.R
run_script scripts/generate_env_response_era5.R
run_script scripts/generate_historical_comparison_figures.R
run_script scripts/generate_availability_heatmap.R
run_script scripts/generate_kg_availability_heatmaps.R
run_script scripts/generate_kg_anomaly_figures.R
run_script scripts/generate_gez_anomaly_figures.R

echo "$(ts) === PHASE 1 COMPLETE ==="

# ── Phase 2: refresh candidates/ copies ───────────────────────────────────────
# Source mappings confirmed by md5sum against existing candidates 2026-05-06
cp review/figures/network/fig_network_growth.png \
   review/figures/candidates/fig_01_network_growth.png
echo "$(ts) Copied fig_network_growth.png → candidates/fig_01_network_growth.png"

cp review/figures/network/fig_dur08_HistoricalOverlay.png \
   review/figures/candidates/fig_02_network_duration_profile.png
echo "$(ts) Copied fig_dur08_HistoricalOverlay.png → candidates/fig_02_network_duration_profile.png"

cp review/figures/network/fig_dur11_CumulativeSiteYears_IGBP.png \
   review/figures/candidates/fig_02b_cumulative_siteyears_igbp.png
echo "$(ts) Copied fig_dur11_CumulativeSiteYears_IGBP.png → candidates/fig_02b_cumulative_siteyears_igbp.png"

cp review/figures/maps/fig_map01_ShuttleFull.png \
   review/figures/candidates/fig_03_choropleth_current.png
echo "$(ts) Copied fig_map01_ShuttleFull.png → candidates/fig_03_choropleth_current.png"

cp review/figures/maps/fig_map09_ShuttleSnapshotsStack.png \
   review/figures/candidates/fig_04_choropleth_snapshots.png
echo "$(ts) Copied fig_map09_ShuttleSnapshotsStack.png → candidates/fig_04_choropleth_snapshots.png"

echo "$(ts) === PHASE 2 COMPLETE ==="
echo "$(ts) === ALL DONE ==="
