# WUE isotope pilot — side analysis

**This is not part of the FLUXNET Annual Paper 2026.** It is a standalone pilot
comparing flux-derived water use efficiency (WUE) with tree-ring isotope
estimates of intrinsic WUE. Requested by David Moore, 2026-10-07.

Everything for this analysis — code, data, figures, tables, and the methods
memo — lives under this directory (`WUE/isotope_pilot/`). Nothing here reads
from or writes to the repo-root `R/`, `scripts/`, `outputs/` directories used
by the Annual Paper, with two deliberate read-only exceptions: `data/snapshots/`
(the locked Annual Paper snapshot, used only to flag whether a site's Shuttle
product differs from it) and `review/diagnostics/` (used only to cross-check
this pilot's own precipitation-availability numbers against two existing
diagnostic packages). Nothing in this analysis writes outside
`WUE/isotope_pilot/`, except `SESSION_LOG.md` at the repo root.

## Standing rules for this analysis

1. **GPP is the nighttime partition only.** No `GPP_DT_*` or `RECO_DT_*`
   column is ever read, summarised, plotted, or compared anywhere in this
   analysis, including as a sensitivity check. The daytime partitioning
   method uses VPD to estimate GPP, so a VPD-weighted WUE built on it would
   be circular. Enforced in code by the column whitelist in
   `code/02_read_subdaily.R`/`code/05_read_subdaily_wue.R` and a defensive
   pattern guard in `code/00_config.R`. See `docs/methods_memo.md`.
2. **Revised for stage 2 (2026-10-07): this work computes WUE, inherent
   WUE (IWUE), underlying WUE (uWUE), and the VPD exponent k*, but does not
   interpret them.** No trend tests, no site rankings, no statements about
   what the series mean. Stage 1 ended at the pre-analysis report
   (`code/04_report_preanalysis.R`); stage 2 (`code/05`-`10`) applies the
   Zhou et al. (2015) screens and computes the metrics. See
   `docs/methods_memo.md`.
3. **Report what is found.** Sites and variables are never substituted,
   dropped, or added; an absent column is recorded as absent, not silently
   skipped.

Tree-ring data (`code/03_fetch_treering.R` and its helpers) remain out of
scope for stage 2 — left in place, not run, not touched.

## Sites (11 for stage 2; 13 for stage 1)

With published tree-ring isotopes (Guerrieri et al. 2019; Belmecheri et al. 2021):
`US-Ha1`, `US-Ho2`, `US-MMS`, `US-SP1`, `US-Bar`, `US-Slt`, `US-Dk2`, `US-Fuf`.

Flux only: `DE-Tha`, `BE-Vie`, `FI-Hyy`.

`CH-Dav` was dropped for stage 2 by PI decision (2026-10-07): stage 1 energy
balance closure slope 0.46 (r2 0.56), and three years with no nighttime GPP.
`NL-Loo` was also dropped by PI decision (2026-10-07), while attempting the
12-site full run: the only stage-2 site to fail `06_build_site_years.R`'s
P_ERA integrity check (-3.08%, threshold 2%). Both sites' downloaded/extracted
files are left on disk, just not read by stage 2's scripts (`WUE_SITES_STAGE2`
in `code/00_config.R`).

## Stage 2 — reconstructed screens (PI decision, 2026-10-07)

Replacing the earlier rain-screen decision, after two read-and-report side
analyses examined it directly: `docs/report_precip_compare_20261007.md`
(ERA5 `P_ERA` vs. the tower gauge, daily scale) and
`docs/report_screen_variants_20261007.md` (rain source x screen c radiation
column x GPP day-test reference, crossed on the 3 test sites). `07`'s rain
screen now sums `P_F` (as distributed: gauge where measured, `P_ERA` fill
where not), screen c tests `SW_IN_F >= 0` (not `NETRAD_filled`), and the
day-level GPP test uses `gpp_test = "halfhour"` (Zhou et al. 2015's own
wording: 10% of the site-year's maximum single-record GPP, not 10% of the
largest daily mean). These are now `run_zhou_screens()`'s defaults
(`code/zhou_screens.R`); full basis and numbers in
`docs/methods_memo.md` "Stage 2 — Zhou et al. (2015) screens (reconstructed,
PI decision 2026-10-07)".

## Data

Sub-daily (HH or HR, auto-detected per site) and daily (DD) eddy covariance
and meteorological data, downloaded and extracted fresh from the FLUXNET
Shuttle (`flux_listall()` + `flux_download()` + `flux_extract(resolutions =
c("h", "d"))`) into `data/raw/`, `data/extracted/`, `data/processed/` (all
gitignored, all scoped to this analysis via a local `FLUXNET_DATA_ROOT`
override — see `code/00_config.R`). The repo-root `data/` tree, which only
holds YY/MM/DD-resolution extracts for the Annual Paper, is never touched by
download or extract — it is only *read* (the locked snapshot CSV) for
comparison.

Tree-ring isotope reference data (`data/external/treering/`, gitignored) comes
from two sources, fetched by `code/03_fetch_treering.R`:

- Guerrieri et al. (2019, PNAS), deposited at the Environmental Data
  Initiative as package `edi.401` (newest revision resolved dynamically, not
  hardcoded).
- Belmecheri et al. (2021), `https://github.com/SBelmecheri/NE_Tree-Rings_Isotopes`.

The ITRDB is deliberately not searched for this pilot.

## Directory layout

```
code/       R scripts, numbered in run order, + the unattended run scripts
data/       raw/, extracted/, processed/, external/treering/ (gitignored; regenerated by code/)
figures/    plots (git-tracked)
tables/     CSV outputs (git-tracked)
docs/       methods_memo.md + report_back_<date>.md / report_back_stage2_<date>.md
logs/       nohup logs from the unattended runs (not git-tracked, except STATUS* which are also gitignored)
```

## Run order

```
code/00_config.R                 # sourced by every other script; not run directly
code/01_download_extract.R       # Shuttle download + sub-daily/DD extract, these 13 sites
code/02_read_subdaily.R          # one sub-daily RDS per site (no combined all-sites object)
code/03_fetch_treering.R         # edi.401 + Belmecheri GitHub repo
code/04_report_preanalysis.R     # pre-analysis report: inventory, availability, closure,
                                  # precip cross-checks, tree-ring inventory/site-mapping
                                  # (stage 1 end -- superseded by stage 2 below)
code/run_setup_20261007.sh       # runs 01-04 unattended, commits+pushes tables/ and docs/

# --- Stage 2 (2026-10-07): screens + WUE metrics, 11 sites (CH-Dav, NL-Loo dropped) ---
code/05_read_subdaily_wue.R      # re-read sub-daily FLUXMET, + P_ERA/NEE_CUT_REF/
                                  # NEE_CUT_REF_QC/RECO_NT_CUT_REF
code/06_build_site_years.R       # per-site VUT/CUT choice, P_ERA check (hard stop on
                                  # failure), NETRAD gap-fill fit, years_dropped.csv
code/07_apply_screens.R          # Zhou et al. (2015 section 2.1) rain/quality/daylight/
                                  # day-level screens -> valid days, screen_attrition.csv
code/08_compute_metrics.R        # WUE/IWUE/uWUE (daily + yearly), k* grid search,
                                  # units sanity check vs. Zhou et al. (2015)
code/09_make_figures.R           # annual WUE/IWUE/uWUE + valid-days figures
code/10_report_stage2.R          # docs/report_back_stage2_<date>.md
code/run_stage2_20261007.sh      # runs 05-10 unattended (NOT 03 -- tree-ring stays
                                  # out of scope), commits+pushes tables/figures/docs

# --- Shared helpers (sourced by stage 2 AND by the read-and-report side
# analyses below, so they can never drift into two independent copies) ---
code/rain_rule.R                 # pt_pet_mm_day() (Priestley-Taylor PET), rain_rule_excluded()
code/netrad_gapfill.R            # fit_netrad_gapfill(): per-site NETRAD ~ SW_IN_F OLS fill
code/zhou_screens.R              # run_zhou_screens(): the Zhou et al. (2015) screens a-d

# --- Read-and-report side analyses (do not rerun stage 2, do not change the
# rain rule, do not recompute WUE) ---
code/11_precip_compare.R         # ERA5 (P_ERA) vs. tower gauge, daily scale -- see
                                  # docs/report_precip_compare_<date>.md
code/12_screen_variants.R        # rain-source x screen-c-radiation variant check, 3
                                  # test sites -- see docs/report_screen_variants_<date>.md
```

`WUE_SITE_SUBSET` (space-separated site IDs) narrows the site list for a
smoke test without editing any script — see `code/00_config.R`. Stage 2's
own site list, `WUE_SITES_STAGE2`, additionally always excludes `CH-Dav` and `NL-Loo`.

Launch either unattended run from the repo root in a plain terminal (not a
Claude Code session):

```bash
nohup caffeinate -i bash WUE/isotope_pilot/code/run_setup_20261007.sh \
  >> WUE/isotope_pilot/logs/setup_20261007.log 2>&1 & disown

nohup caffeinate -i bash WUE/isotope_pilot/code/run_stage2_20261007.sh \
  >> WUE/isotope_pilot/logs/stage2_20261007.log 2>&1 & disown
```
