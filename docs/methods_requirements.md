# Methods Requirements — FLUXNET Annual Paper 2026

This document defines what each methods section must cover and identifies
the primary code files from which accurate methods text will be derived.
It is not a draft — it is a requirements specification.

**Status:** Requirements definition phase. Methods text will be drafted
when analyses are finalised. Do not draft prose from this document yet.

**Repository:** https://github.com/EcosystemEcologyLab/fluxnet-annual-2026

---

## 5.1 Dataset assembly and annual snapshot definition

**Purpose:** Describe how the annual dataset was assembled from regional
hubs and frozen as a reproducible snapshot. Highlight use of
content-addressable persistent identifiers (PIDs) for reproducibility.

**Must cover:**
- FLUXNET Shuttle as the data source (version, date of download)
- Which regional hubs were included (AmeriFlux, ICOS, TERN, others)
- How the snapshot was defined and frozen (product_id checksum-based PID)
- Number of sites downloaded, number passing initial extraction
- Temporal scope of the dataset
- Data license (CC-BY-4.0)

**Primary code files:**
- `scripts/01_download.R` — download logic and hub selection
- `scripts/batch_download.R` — batch download implementation
- `scripts/02_extract.R` — extraction and ZIP cleanup
- `R/snapshot.R` — snapshot detection and PID tracking
- `R/credentials.R` — hub authentication
- `data/snapshots/download_progress.csv` — download audit trail
- `R/pipeline_config.R` — configuration constants

**Key facts (verified 2026-10-02, figure stage 6 — see `review/figstage_status.md` Stage 6 entry
for sourcing detail):**
- Shuttle version: `0.3.7` git tag, which self-reports as `0.3.7.post0+dirty` — this is the
  string `FLUXNET_SHUTTLE_VERSION` and methods text should use (`R/pipeline_config.R`,
  `.env.example`; CLAUDE.md's "Shuttle version monitoring" note). TO CONFIRM: the current local
  `.env` on at least one machine is set to the bare `0.3.7` instead, which would fail the
  `check_pipeline_config()` version match — a local configuration issue, not a methods-text fact.
- Locked snapshot: `data/snapshots/fluxnet_shuttle_snapshot_20260901T094522.csv`, 781 sites,
  listed 2026-09-01 (`docs/shuttle_gap_download_20260901.md`). A later development-mode snapshot
  (`fluxnet_shuttle_snapshot_20260920T102211.csv`, also 781 sites) exists but was explicitly not
  promoted to locked status (`review/diagnostics/store_refresh_20260920/report.md`) — methods text
  must cite the 20260901 file, not the 20260920 one.
- Sites in the locked snapshot: 781 (not 672 — the 672 figure was from an April 2026 state of the
  network and is now stale throughout this document).
- Hubs (from the snapshot's own `data_hub` column — not the free-text, multi-affiliation
  `network` column, and not inferred from site-ID prefix per CLAUDE.md Hard Rule 2): AmeriFlux
  381, ICOS 348, TERN 52 (781 total, no other hubs).
- TO CONFIRM: no snapshot `.meta.json` sidecar and no committed `download_progress*.csv` scoped to
  the 2026-09-01 snapshot specifically were found (the three committed `download_progress*.csv`
  files on disk are dated 2026-04-14/2026-06-01, both older network states). The only audit trail
  for the 781-site snapshot's incremental download is the narrative doc
  `docs/shuttle_gap_download_20260901.md`.
- Data license: CC-BY-4.0, confirmed unchanged (CLAUDE.md "Data Use and Citation").

---

## 5.2 Site selection and metadata handling

**Purpose:** Define site inclusion criteria, time windows, biome
assignment, regional grouping, and metadata fields used in the analysis.

**Must cover:**
- Site inclusion criteria (CC-BY-4.0 only, sites with a qualifying annual NEE value)
- How sites with all-missing NEE were identified and excluded
  (ONEFlux 15-day gap rule — confirmed by Dario Papale 2026-04-16)
- Per-site VUT/CUT fallback (`scripts/04_qc.R`, commit `ad7464f`) for sites where
  `NEE_VUT_REF_QC` is unavailable
- IGBP classification source (BADM/BIF `igbp` field, PI-reported)
- UN subregional grouping and FAO Global Ecological Zone assignment — **status as of
  2026-10-02: neither is used by any currently-produced figure or table** (verified by grep of
  `scripts/07_figures.R`, `scripts/figure4_representativeness.R`,
  `scripts/build_draft_manuscript_v1.R` — zero matches for GEZ/UN-subregion code in any of the
  three). The only code paths that consume them (`R/historical_datasets.R`'s
  `.subregion_from_site_id()`, `scripts/step3_extract_gez.R`,
  `scripts/generate_gez_anomaly_figures.R`, `scripts/generate_kg_anomaly_figures.R`,
  `R/figures/fig_anomaly_context.R` via `fig_long_record_timeseries()`) were last modified
  2026-04-18 and are not listed with an export filename in `docs/figure_inventory.md` (shown as
  `—`, "not yet wired into `07_figures.R`"). Methods text should either omit this bullet or mark
  it explicitly as "assembled but not used in any reported figure" rather than imply it drives a
  published result.
- Koppen-Geiger climate classification — **two distinct, both-currently-valid uses, which must
  not be conflated in methods text:**
  1. Main pipeline / every figure except Figure 5 panel A's Geo-vs-Data side: ERA5-local
     classification (Beck et al. rule cascade applied to each site's own 1991-2020 ERA5 monthly
     climatology, `R/climate_classification.R`), output `data/snapshots/site_koppen_era5.csv`.
     BADM `CLIMATE_KOEPPEN` is retained there only as a QA/comparison column. (This is what the
     August 2026 note below described, and it is still current for this scope.)
  2. Figure 5 panel A's Geo-vs-Data side only (added 2026-10-02, see §5.8): PI-reported BADM
     `CLIMATE_KOEPPEN` used FIRST (603 of 781 sites), ERA5-local classification as a fallback only
     for the remaining 178 (147 of which pass the three precipitation-dependent exclusion rules
     below) — output to a *different* file, `data/snapshots/site_koppen_era5_fig4.csv`. This
     redesign followed a diagnostic (`review/diagnostics/koppen_pi_vs_era5/`) finding the
     ERA5-local class agreed with the PI-reported class only 59.5% of the time (n=603), while the
     PI-reported class agreed better with the independent Beck 2023 raster (69.2%).
  See `review/figures/representativeness/methods_koppen_era5.md` for both.
  (Historical-network comparisons and the global land-area backdrop use the Beck 2023 raster
  directly, unrelated to either per-site method above; BADM `CLIMATE_KOEPPEN` is retained as a QA
  comparison column outside use 2, as of 2026-08-20.)
- Functionally active site definition — **corrected 2026-10-02: the doc's previous "≥3 months
  valid NEE in last 4 years" does not match the implementation.** `R/utils.R::
  is_functionally_active()` (default `active_threshold = 4L`) actually requires: a site is active
  at year Y if it has at least one month with any non-NA value among NEE(VUT/CUT)/GPP/RECO/LE/H
  (not NEE-specifically) in at least one of the four years `[Y-3, Y]` — i.e. "≥1 month present in
  ≥1 of the last 4 years," not "≥3 months… in the last 4 years." The per-year `has_data` flag it
  consumes comes from `compute_site_year_presence()` (`R/utils.R`).
- Historical dataset site lists (Marconi, La Thuile, FLUXNET2015) — see §5.5 for verified counts.

**Primary code files:**
- `scripts/03_read.R` — site reading and BADM extraction
- `R/utils.R` — `compute_site_year_presence()`, `is_functionally_active()`
- `R/climate_classification.R` — ERA5-local Köppen-Geiger classification
- `R/site_annual_fluxes.R` — `compute_site_annual_fluxes()`, the shared per-site annual-flux
  function (see §5.3)
- `R/external_data.R` — GEZ, WorldClim, aridity index loading (GEZ currently unused — see above)
- `R/historical_datasets.R` — historical site list loading
- `data/snapshots/site_candidates_full.csv` — master site metadata table
- `data/snapshots/site_koppen_era5.csv`, `site_koppen_era5_fig4.csv` — the two Köppen outputs above
- `data/snapshots/site_year_data_presence.csv` — monthly data presence
- `docs/known_issues.md` — documented exclusions and their reasons

**Key facts (verified 2026-10-02, 781-site locked network — see §5.1):**
- Per-site VUT/CUT/neither split (NEE/GPP/RECO; `R/site_annual_fluxes.R::
  compute_site_annual_fluxes()`, `SESSION_LOG.md` 2026-10-02): VUT 616, CUT (fallback) 40, neither
  125 — sums to 781.
- Current-network (781-site) count of sites with no usable annual NEE: **125 of 781**
  (re-verified directly, close-out session 2026-10-02: `compute_site_annual_fluxes()`'s
  `site_summary$nee_median` is `NA` for exactly 125 sites). Definition used, stated explicitly
  because it differs from the pre-2026-10-02 methods text's definition: a site counts here if it
  has **zero QC_THRESHOLD_YY-qualifying annual NEE values** (per-site VUT, falling back to CUT —
  the same per-site rule as `scripts/04_qc.R` and the "VUT/CUT/neither" split above; this is the
  same 125 as "neither" there, not a separate count). This is NOT the doc's previous "all-missing
  NEE (ONEFlux 15-day-gap rule), 106 of 672" definition (raw-data completeness, not QC-threshold
  qualification) — that count is scoped to the April 2026, 672-site network (`docs/shuttle_team_
  report_20260414.md` and three other docs dated 2026-04-16/04-20/04-28), no current-network
  equivalent of that SPECIFIC definition was found in any committed table or script constant, and
  it should not be reused or rescaled for the 781-site network without recomputing it from raw
  ONEFlux gap flags (out of scope for this fix — see `docs/decisions_pending.md` if that specific
  definition is needed later).
- Functionally active threshold: 4-year window, ≥1 month of any flux variable present in ≥1 of
  those years — see corrected definition above (was previously misstated in this document).

---

## 5.3 Harmonisation and flux processing

**Purpose:** Describe QA/QC, variable harmonisation, gap-filling,
partitioning, and uncertainty handling. Cross-hub validation if applicable.

**Must cover:**
- ONEFlux processing pipeline (all hubs use same pipeline)
- QC gating: per-site VUT-first, CUT-fallback (never mixed within a site), threshold 0.50 at
  DD/WW/MM/YY resolution — see `scripts/04_qc.R` (commit `ad7464f`) and §5.2's VUT/CUT counts
- The shared per-site annual-flux function (see Key facts below) that now underlies every current
  figure/table needing a site's annual median flux value — methods text should name this function
  once rather than re-describe the same QC/aggregation logic per figure
- Variable naming conventions (FLUXNET standard)
- Unit handling: pre-integrated annual/monthly data passed through
  unchanged; energy variables converted (LE→mm, H→MJ)
- ERA5 climate variable integration (already in FLUXNET files)
- Known data quality issues: the ERA5-precipitation and FLUXNET-measured-precipitation (`P_F`)
  defects are each a *family* of issues, not a single "4 sites" statement — see Key facts below
  for the verified breakdown. Methods must address how these anomalies are handled in any figure
  or analysis that uses tower P as a primary axis or predictor.

**Primary code files:**
- `scripts/04_qc.R` — QC gating implementation
- `scripts/05_units.R` — unit conversion
- `R/qc.R` — QC helper functions
- `R/units.R` — unit conversion functions
- `R/site_annual_fluxes.R` — `compute_site_annual_fluxes()`/`compute_site_annual_fluxes_from_df()`,
  the shared function (see Key facts below)
- `R/pipeline_config.R` — QC threshold and precipitation-exclusion constants
- `docs/decisions_pending.md` — QC threshold decision rationale
- `docs/known_issues.md` §9/§9a/§9b/§9c — ERA5 and tower precipitation anomalies, ONEFlux gap rule

**Key facts (verified 2026-10-02):**
- QC threshold: `QC_THRESHOLD_DD = QC_THRESHOLD_WW = QC_THRESHOLD_MM = QC_THRESHOLD_YY = 0.50`
  (`R/pipeline_config.R`), applied per-site VUT-first/CUT-fallback, never mixed within a site
  (`scripts/04_qc.R`, confirmed unchanged since commit `ad7464f`).
- Shared function: `R/site_annual_fluxes.R::compute_site_annual_fluxes()` (Shuttle/DuckDB
  `annual` table) and `compute_site_annual_fluxes_from_df()` (pre-extracted comparison data, e.g.
  FLUXNET2015) are the canonical, single implementation of "a site's median annual flux value,
  QC-gated." Confirmed direct callers: `scripts/figure4_representativeness.R`,
  `scripts/figure_flux_representativeness_supp.R`, `scripts/generate_whittaker_ed_three_flux.R`,
  `scripts/generate_whittaker_alt_fig02_update.R`,
  `scripts/figure_flux_comparison_combo_alt_common_siteyears.R`,
  `scripts/assess_flux_data_by_igbp_shuttle.R`, `scripts/assess_flux_data_by_igbp_fluxnet2015.R`.
  (`scripts/figure_flux_comparison_six_panel.R` and `scripts/build_draft_manuscript_v1.R` re-plot
  those scripts' already-computed tables rather than calling the function themselves.)
- Unit convention: confirmed matching CLAUDE.md's table exactly — NEE/GPP/RECO → gC m⁻² per
  period; LE → mm H₂O; H and SW_IN → MJ m⁻²; TA → K (+273.15); VPD → kPa (÷10) (`R/units.R`,
  `scripts/05_units.R`).
- Precipitation data-quality defects (`docs/known_issues.md` §9/§9a/§9c) — the previous "4 sites"
  statement conflated several distinct, differently-sized rules; the verified breakdown (all as of
  2026-10-02, Figure 5 panels A and C, Geo vs Data only) is:
  1. `GRP_ERA_DOWN` — 172 sites with no usable P_ERA-vs-measured regression slope (sentinel
     `-9999` in the BIF `ERA_SLOPE` field).
  2. `P_ERA_MAX_RATIO = 3` — a further 10 sites (panel C) / 0 sites (panel A fallback-only) where
     mean annual P_ERA exceeds 3x every available reference (BADM MAP and WorldClim BIO12).
  3. `P_ERA_MIN_RATIO = 1/3` — a further 22 sites (panel C) / 3 sites (panel A fallback-only)
     where mean annual P_ERA falls below 1/3 of every available reference.
  4. 4 sites with physically impossible raw ERA5 inputs to the FAO-56 PET calculation (panel C,
     aridity, only): `CD-Ygb`, `DE-Zrk`, `FR-LBr`, `US-Sne`.
  Panel A (Köppen, Geo vs Data): rules 1-3 apply only to the 178 sites without a PI-reported class,
  never to a PI-sourced site — 31 of 178 excluded (750/781 total). Panel C (aridity): all three
  rules plus rule 4 apply to every site — 207 of 781 excluded (574/781 total). See §5.8 for the
  full per-panel exclusion counts.
  Separately, `docs/known_issues.md` §9a (unchanged since 2026-06-03) reports 225 of 6,108 FLUXMET
  site-years (3.7%) with `P_ERA > 5000 mm/yr` — a site-year count, not a unique-site count.
  TO CONFIRM: the tower-measured precipitation (`P_F`) defect (§9b) remains **unquantified** — no
  site count or magnitude exists anywhere in the repo as of 2026-10-02; `docs/known_issues.md`
  itself still lists this as an open, not-yet-characterised item. Do not state a site count for
  `P_F` in methods text.

---

## 5.4 Derived metrics and benchmark construction — DEPRECATED (not used by any current figure)

**Status: DEPRECATED (marked 2026-10-02, close-out session).** The anomaly-context figure
pipeline described below (`R/figures/fig_anomaly_context.R` and its GEZ/Köppen-stratified
callers) is **not wired into any currently-produced figure** — `scripts/07_figures.R` has zero
references to it, and `docs/figure_inventory.md` does not list it at all. Its only callers,
`scripts/generate_gez_anomaly_figures.R` and `scripts/generate_kg_anomaly_figures.R`, were last
modified 2026-04-18, predating the current figure set (figure stages 1-6, all 2026-10-02) by
roughly five months. This section's "Must cover"/"Key facts" below describe what that orphaned
code *would* do if wired in — they do not describe a result currently reported anywhere in the
manuscript. Do not draft methods prose from this section until/unless this analysis is actually
reinstated into the active pipeline. Code and data files are left in place (figure-stage rule 3:
do not delete files) — "deprecated" here describes this section's status as a methods-text source,
not a request to remove the underlying scripts.

**Purpose:** Explain how annual flux metrics, anomalies, and aggregated
summaries were calculated.

**Must cover:**
- Annual NEE, GPP, RECO, LE, H — source variables and any aggregation
- Anomaly calculation: deviation from long-term median per site
- Percentile position within long-term distribution (anomaly context figures)
- Recent period definition (2019–2024)
- Long-term baseline definition (all years before 2019)
- Minimum data requirements for anomaly analysis
  (≥8 valid NEE years, ≥4 sites per stratum)
- GEZ × IGBP × subregion stratification for anomaly figures
- Koppen-Geiger stratification (Level 1 and Level 2; classes sourced from
  `data/snapshots/site_koppen_era5.csv` as of 2026-08-20 — see §5.3 note above)

**Primary code files:**
- `R/figures/fig_anomaly_context.R` — anomaly calculation and plotting
- `R/figures/fig_network_growth.R` — site-year counting functions
- `data/snapshots/long_record_site_candidates_gez_kg.csv` — stratified
  site candidates
- `data/snapshots/site_year_data_presence.csv` — valid data years

**Key facts to include (update when finalised):**
- Recent anomaly period: 2019–2024
- Minimum record length for anomaly analysis: 8 valid NEE years
- Minimum sites per stratum: 4

---

## 5.5 External comparison datasets

**Purpose:** Document historical FLUXNET datasets and any remote sensing
or model outputs used for comparison.

**Must cover:**
- Marconi dataset (Falge et al. 2001): 35 sites, **96 site-years** (corrected 2026-10-02 — the
  previous "97" was off by one; recomputed directly from `data/lists/Marconi_to_Modern_SiteIDs.
  xlsx`'s "Years in Marconi" column via `R/historical_datasets.R`'s own
  `sum(last_year - first_year + 1)` logic), 1992–2000.
  Note: 3 of 38 original sites could not be matched to modern IDs
- La Thuile dataset (2007): 252 sites, 965 site-years (confirmed unchanged, recomputed from
  `data/lists/LaThuileList.xlsx`'s year-indicator columns — note that naively summing
  `last_year - first_year + 1` from the derived `years_la_thuile.csv` snapshot overestimates this
  at 1008, since La Thuile site records are not contiguous; 965 from the raw indicator columns is
  correct), 1991–2007. Figure 2 (`fig_02_cumulative_siteyears_igbp`) drew its La Thuile cumulative
  line from the 1008 span-based figure until 2026-10-04, when it was corrected to draw from the
  actual year-indicator matrix instead (`R/figures/fig_network_growth.R::fig_cumulative_siteyears_igbp()`'s
  new `la_thuile_year_matrix` argument, built in `scripts/generate_fig_cumulative_siteyears.R`); the
  line now ends at 965, matching this section. Marconi and FLUXNET2015 needed no such fix — each of
  their own site records is contiguous within its first/last year, so the span-based sum already
  equals the indicator-based count (96 and 1532 respectively; `fig_dur11_CumulativeSiteYears_IGBP`,
  which calls the same shared function without the new argument, still uses the span-based fallback
  for La Thuile and was not regenerated as part of this fix).
- FLUXNET2015 (Pastorello et al. 2020): 212 sites, 1532 site-years (confirmed unchanged),
  1991–2014
- Collection comparison tables (`scripts/collection_comparison_table.R`, 2026-10-04): sites and
  site-years, region, IGBP class, and sites-per-year breakdowns for all four collections side by
  side, each using the convention above. Outputs: `data/snapshots/collection_sites_siteyears.csv`,
  `collection_sites_by_region.csv` (Figure 1's four regional extents, reused verbatim from
  `scripts/generate_map_regional.R`, plus South and Central America / Africa / Other for sites
  outside all four, bucketed from the country code the site_id prefix encodes per CLAUDE.md Hard
  Rule 2 — not a hub/network inference), `collection_sites_by_igbp.csv` (counts per
  `PAPER_IGBP_ORDER` class, classes present, and any missing/non-standard class — one La Thuile
  site, CN-Xfs, carries class "TBD"), and `collection_sites_per_year.csv` (sites with data per
  year, 1991–2007, La Thuile vs current).
- **Current-network year window is data-driven, not hard-coded (2026-10-05):** the current
  network's site-year window and total previously stopped at a hard-coded 1991–2024, which had
  gone stale now that 2025 data exists. `R/utils.R::data_year_window(presence_df)` now computes
  the window directly from `data/snapshots/site_year_data_presence.csv` — the first and last
  calendar year in which any site has `has_data = TRUE` — ignoring the empty full-calendar-year
  padding rows ONEFlux writes for the current, still-incomplete year (see SESSION_LOG.md
  2026-10-05, "most recent data" investigation, for why those padding rows exist and how they were
  confirmed empty). It also returns the number of sites reporting the final year. As of this
  refresh: **window 1991–2025; 6,200 current-network site-years (6,061 through 2024 plus 139 in
  2025); 139 of 781 sites report 2025.** Marconi/La Thuile/FLUXNET2015 totals are unaffected (96,
  965, 1,532 — each collection's own fixed historical range). Both
  `scripts/generate_fig_cumulative_siteyears.R` (Figure 2) and
  `scripts/collection_comparison_table.R` (Table 1) now call this same helper for their
  current-network window/total, so the two outputs cannot silently disagree. Figure 2's legend
  reports the window, the total, and the final-year site count from the same computed values
  rather than typed text.
  **Not yet converted to the data-driven window** (hard-coded at a 2024 end year, unchanged in
  this pass — see SESSION_LOG.md 2026-10-05 for the full list): `scripts/07_figures.R`'s
  `fig_map_nee_delta(..., recent_years = 2020:2024)` call; `scripts/generate_duration_histograms.R`'s
  Dur09/Dur10 panels (`fig_siteyears_by_year()`/`fig_siteyears_by_year_igbp()` in
  `R/figures/fig_network_growth.R`, each with its own hard-coded `year_range = 1991:2024` default,
  separate from `fig_cumulative_siteyears_igbp()` above) and Dur11's own `year <= 2024L`
  site-years-plotted report line; `scripts/generate_kg_anomaly_figures.R` and
  `scripts/generate_gez_anomaly_figures.R` (`RECENT_YEARS <- 2019:2024`, matching
  `R/figures/fig_anomaly_context.R`'s own hard-coded `recent_years = 2019:2024` defaults in two
  separate functions). Note: Dur11's *plot* itself (which calls `fig_cumulative_siteyears_igbp()`
  without passing `year_range`) now extends to the same data-driven 2025 window as Figure 2, as a
  side effect of the default change above — only its console report line and site-year total text
  are still hard-coded at 2024.
- How historical site lists were obtained and standardised
- How historical sites not in Shuttle were handled (fallback metadata)
- WorldClim v2.1 bioclimatic variables (Fick & Hijmans 2017):
  bio1 (MAT), bio12 (MAP), 2.5 arc-minute resolution, 1970–2000 baseline
- CGIAR Global Aridity Index v3.1 (Zomer et al. 2022):
  30 arc-second resolution, divide by 10000 for true AI values (confirmed unchanged 2026-10-02,
  `review/figures/representativeness/methods_aridity_unep.md`). This remains the method for the
  global side and the Geo vs Geo comparison everywhere, including Figure 5 panel C. A SEPARATE,
  additional ERA5-based aridity calculation (1991–2020 P_ERA / FAO-56 PET) is used only for
  Figure 5 panel C's Geo vs Data side (`methods_aridity_era5.md`) — it supplements, not replaces,
  the CGIAR/UNEP method, and introduces the 1970–2000-vs-1991–2020 period mismatch noted in §5.8.
- FAO Global Ecological Zones 2010 shapefile — **status as of 2026-10-02: not used by any
  currently-produced figure or table** (same verification as §5.2's UN-subregion/GEZ note — zero
  references in `scripts/07_figures.R`, `scripts/figure4_representativeness.R`, or
  `scripts/build_draft_manuscript_v1.R`). Retained here as a documented external dataset
  (`data/external/` provenance, CLAUDE.md) but not as an active input to any reported result.

**Primary code files:**
- `R/historical_datasets.R` — historical site list loading
- `R/external_data.R` — WorldClim, aridity index, GEZ loading
- `R/figures/fig_network_growth.R::fig_cumulative_siteyears_igbp()` — Figure 2's panel function
- `scripts/generate_fig_cumulative_siteyears.R` — Figure 2 driver script
- `scripts/collection_comparison_table.R` — collection comparison tables (sites, site-years,
  region, IGBP, per-year)
- `data/snapshots/sites_marconi_clean.csv`
- `data/snapshots/sites_la_thuile_clean.csv`
- `data/snapshots/sites_fluxnet2015_clean.csv`
- `data/snapshots/years_marconi.csv`
- `data/snapshots/years_la_thuile.csv`
- `data/snapshots/years_fluxnet2015.csv`
- `data/snapshots/collection_sites_siteyears.csv`, `collection_sites_by_region.csv`,
  `collection_sites_by_igbp.csv`, `collection_sites_per_year.csv`
- `data/raw/Marconi_to_Modern_SiteIDs.xlsx` — site ID crosswalk

**Citations required:**
- Falge et al. (2001a,b) Agricultural and Forest Meteorology 107
- Pastorello et al. (2020) Scientific Data 7:225
- Fick & Hijmans (2017) International Journal of Climatology 37:4302
- Zomer et al. (2022) Scientific Data — doi:10.6084/m9.figshare.7504448

---

## 5.6 Statistical analysis and uncertainty estimation

**Purpose:** Document trend estimation, sensitivity analysis, and other
statistical methods.

**Must cover:** (to be defined when analyses are finalised)
- Trend estimation methods if used
- Uncertainty propagation
- Sensitivity to QC threshold choices
- Bootstrap or permutation methods if used

**Status:** Not yet implemented — placeholder for future analyses.

**Primary code files:** TBD

---

## 5.7 Reproducibility workflow

**Purpose:** Describe code organisation, versioning, and repository
structure for reproducing all figures and tables.

**Must cover:**
- Repository structure overview
- How to reproduce the full pipeline (scripts 01–07 in order)
- Dedicated figure generation scripts and their outputs
- renv for R package version locking
- Environment variables required (FLUXNET_DATA_ROOT, credentials)
- Codespace vs HPC workflow differences
- Data not included in repository (downloaded separately via Shuttle)
- Pipeline configuration constants in `R/pipeline_config.R`

**Primary code files:**
- `CLAUDE.md` — project context and workflow
- `README.md` (to be created)
- `R/pipeline_config.R` — all configurable constants
- `renv.lock` — locked R package versions
- `scripts/01_download.R` through `scripts/07_figures.R`
- `scripts/generate_whittaker.R`
- `scripts/generate_maps.R`
- `scripts/generate_duration_histograms.R`
- `.env.example` — required environment variables
- `docs/CODESPACE_SETUP.md` — Codespace setup instructions

---

## 5.8 Network representativeness (Figure 5 and Supplementary Figure S3)

(Renumbered from "Figure 4 and supplemental figure" under figure stage 6, 2026-10-02 — see
`docs/figure_inventory.md`. `scripts/figure4_representativeness.R`'s own name and its FIG_DIR
source basenames are unchanged; only the manuscript-facing numbering changed.)

**Purpose:** Describe how the representativeness of the current 781-site network was
assessed against global land across six independent axes, and the two-sided (Geo vs
Data / Geo vs Geo) comparison design.

**Must cover:**
- The six axes (Köppen-Geiger, IGBP land cover, aridity, biomass, NEE, ET) and why
  each uses the global product/bin scheme it does
- The "Geo vs Data" / "Geo vs Geo" distinction: each site's own measured or
  site-derived value vs. the gridded product's own value at the tower coordinate
  — stated once, since both the figure and the table below use it throughout
- The weighted Jaccard (Ruzicka) similarity metric, J = Σmin(p,q)/Σmax(p,q), and
  its interpretation (1 = identical distributions, 0 = no overlap)
- The three precipitation-dependent exclusion rules (panels A and C, Geo vs Data
  only) and why they exist — ERA5 precipitation data-quality issues, not a
  representativeness finding in themselves (see `docs/known_issues.md` §9c)
- Panel A's PI-reported-class-first design (added 2026-10-02): the exclusion
  rules above apply only to the minority of sites without a PI-reported Köppen
  class (BADM `CLIMATE_KOEPPEN`); a PI-sourced site is never excluded. Panel C
  has no PI-reported analogue and applies all three rules to every site.
- The period mismatch in panel C (CGIAR Aridity Index v3.1 baseline 1970–2000 vs.
  this figure's 1991–2020 ERA5-derived Geo vs Data side)
- That the previous "Figure 4" (`fig_rep001_current.png`, 767-site network, ESA CCI
  Land Cover and TRENDY NEE-IAV/ET-median axes — an older, unrelated figure-numbering
  generation, not the current Figure 5 renumbered from this section's own prior "Figure 4" name)
  is superseded, and is not a re-analysis target — methods text should describe only the current
  figures

**Primary code files:**
- `scripts/figure4_representativeness.R` — the complete production script (5
  phases; see its header comment and `SESSION_LOG.md` for phase-by-phase decisions)
- `review/figures/representativeness/methods_koppen_era5.md`,
  `methods_koppen_beck2023.md`, `methods_igbp.md`, `methods_aridity_era5.md`,
  `methods_aridity_unep.md`, `methods_biomass.md`, `methods_flux_bin_scheme.md`,
  `methods_precip_exclusions.md` — per-axis methods notes
- `data/snapshots/representativeness_metrics_fig4.csv` — authoritative n/J for
  all 12 panel × comparison combinations
- `docs/known_issues.md` §9c — the ERA5 data-quality defects the exclusion rules
  exist to work around

**Panel-by-panel map (current as of the 2026-10-02 print re-render; n and J from
`representativeness_metrics_fig4.csv`):**

| Panel | Axis | Global product | Site source — Geo vs Geo | Site source — Geo vs Data | Land grid (total km²) | Bin/class scheme | Exclusions (Geo vs Data only) | n, J — Geo vs Geo | n, J — Geo vs Data | Script section | Snapshot file(s) | Methods note(s) |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| A | Köppen-Geiger | Beck et al. (2023) 1 km KG raster | `site_koppen_beck2023.csv` (Beck class at tower) | `site_koppen_era5_fig4.csv` — PI-reported BADM `CLIMATE_KOEPPEN` (603 sites) first, ERA5-derived fallback (147 of the remaining 178) second | Beck 2023 1 km mask, 147,322,862 | 13 two-letter classes | ERA5-fallback sites only: GRP_ERA_DOWN (28) + P_ERA_MAX_RATIO (0) + P_ERA_MIN_RATIO (3) = 31 excluded; PI-sourced sites never excluded | 781/781, 0.372 | 750/781, 0.399 | Phase 1 | `site_koppen_beck2023.csv`, `site_koppen_era5_fig4.csv` | `methods_koppen_beck2023.md`, `methods_koppen_era5.md` |
| B | Land cover (IGBP) | MODIS MCD12C1.061 on Beck 2023 1 km mask | MODIS class at tower (0.05° native) | PI-reported BADM `igbp` | same mask, 147,322,862 | 15 PI-reported classes + Other | none | 781/781, 0.495 | 781/781, 0.346 | Phase 2 | `site_igbp_fig4.csv`, `igbp_mcd12c1_global_distribution.csv` | `methods_igbp.md` |
| C | Aridity | CGIAR Aridity Index v3.1 | CGIAR AI at tower | AI = 1991–2020 P_ERA / FAO-56 PET (ERA5) | CGIAR's own coverage, 134,761,545 | 7-class UNEP | GRP_ERA_DOWN (171) + P_ERA_MAX_RATIO (10) + P_ERA_MIN_RATIO (22) + invalid ERA5 input (4) = 207 excluded | 781/781, 0.666 | 574/781, 0.675 | Phase 3 | `site_aridity.csv`, `site_aridity_era5_fig4.csv` | `methods_aridity_unep.md`, `methods_aridity_era5.md` |
| D | Biomass | ESA CCI Biomass v7.0 (2024) | AGB at tower (1 km) | same (no separate data side) | Beck 2023 1 km (0.00833°) mask, same mask as panels A/B, 147,322,862 | 7-bin hybrid (0–5 fixed + 6 quantile) | none | 781/781, 0.636 | 781/781, 0.636 | Phase 4 | `site_biomass_cci_v7.csv` | `methods_biomass.md` |
| E | NEE | TRENDY v14 ensemble-median | model NEE at tower | Median of the site's `QC_THRESHOLD_YY`-qualifying annual NEE values, VUT→CUT per-site fallback chosen by the `scripts/04_qc.R` rule (`R/site_annual_fluxes.R::compute_site_annual_fluxes()`, revised 2026-10-02; 616 VUT, 40 CUT, 125 neither) | TRENDY land mask, 163,331,649 | 7-bin (bar1 model GPP<5 + 6 rounded sextiles) | sites without a qualifying annual NEE value (`require_own=TRUE`, applies to bar 1 too) | 781/781, 0.530 | 656/781, 0.165 | Phase 4 | `site_nee_fig4.csv`, `nee_et_fig4_global_distribution.csv` | `methods_flux_bin_scheme.md` |
| F | ET | TRENDY v14 ensemble-median | model ET at tower | Median of the site's `QC_THRESHOLD_YY`-qualifying annual ET values (`LE_F_MDS`, gated on its own `LE_F_MDS_QC`, independent of the NEE gate — no VUT/CUT fallback; `R/site_annual_fluxes.R::compute_site_annual_fluxes()`, revised 2026-10-02) | TRENDY land mask, 163,331,649 | 7-bin (bar1 own value<5 + 6 rounded sextiles) | sites without a qualifying annual ET value (`require_own=TRUE`, applies to bar 1 too) | 781/781, 0.456 | 665/781, 0.479 | Phase 4 | `site_et_fig4.csv`, `nee_et_fig4_global_distribution.csv` | `methods_flux_bin_scheme.md` |

**Key facts to include (update when finalised):**
- Network size: 781 sites (snapshot `fluxnet_shuttle_snapshot_20260901T094522.csv`)
- `P_ERA_MAX_RATIO = 3`, `P_ERA_MIN_RATIO = 1/3` (`R/pipeline_config.R`)
- Final artwork: 183 mm wide, 169.3 mm tall (both figures), Helvetica 7pt. Panel titles with unit
  exponents (d/e/f) are rendered from a plotmath expression, not a literal Unicode superscript-minus
  character — the latter has no usable glyph in the PDF export's base PostScript Helvetica font
  (confirmed via `mbcsToSbcs` conversion-failure warnings; the PNG, a TrueType Helvetica, renders it
  fine either way).

---

## 5.9 Supplementary record-length, sampling-ratio and Bowen-ratio tables

(Added 2026-10-06, unattended supplementary run — see `SESSION_LOG.md` and
`review/supp_run_status.md` for the full stage-by-stage report.)

**`tableS2_record_length_by_igbp.csv`** (`tableS_record_length_by_igbp.csv` before the 2026-10-08
supplementary material restructure, SESSION_LOG.md; `scripts/supp_stage1_record_length_by_igbp.R`): current
(781-site) network sites reaching ≥5/≥10/≥20 years, by IGBP class and in total, under two
independent record-length definitions — years with `has_data == TRUE`
(`compute_site_year_presence()`, R/utils.R) and years with a QC-qualifying annual NEE value
(`compute_site_annual_fluxes()$site_summary$n_years_nee`, R/site_annual_fluxes.R). Not
`data/snapshots/site_record_length.csv` (a different, stricter, QC-monthly-based definition for a
different purpose).

**Figure S6** (Figure S7 before the 2026-10-08 supplementary material restructure;
`scripts/supp_stage2_record_length_collections_figure.R`): snapshot
record-length histogram by IGBP (panel a) and share-of-sites-with-≥n-years step lines for all four
FLUXNET network generations — Marconi, La Thuile, FLUXNET2015, current (panel b). Per-site year
counts reuse `scripts/collection_comparison_table.R`'s own list-reading logic; validated against
`data/snapshots/collection_sites_siteyears.csv` (96/965/1,532 historical site-years, matching
current total) before the figure is produced. The legend states explicitly that "a year" means
different things across collections (published-table listing vs. any-flux-value-in-≥1-month) and
that Marconi's per-site values are first–last-year spans, not year-by-year records.

**`tableS_sampling_ratio_jaccard_check.csv` / `tableS_sampling_ratio_extremes.csv`** (moved from
`SupTables/` to `review/diagnostics/sampling_ratio_checks/` in the 2026-10-08 supplementary
material restructure, SESSION_LOG.md -- names unchanged, only the location moved;
`scripts/supp_stage3_sampling_ratios.R`): land share / tower share / sampling ratio / log2 ratio
per class, for Figure 5 / Figure S3's six representativeness axes, reconstructed strictly from
already-committed `site_*_fig4.csv` + `site_biomass_cci_v7.csv` tower files and
`*_global_distribution.csv` land files (no raster re-extraction). Recomputed weighted Jaccard
agrees with `data/snapshots/representativeness_metrics_fig4.csv` (not modified) to 6 decimals for
10 of 12 axis × comparison combinations; the Köppen geo-vs-geo panel is not reconstructable at all
from this restricted file set (its source column is entirely `NA` in the permitted file), and the
aridity geo-vs-geo panel is reconstructed here from an ERA5-derived proxy rather than its true
CGIAR-raster-at-tower source — both outside the permitted file set, both documented inline rather
than forced to agree. Because not all 12 agree, the full long table
(`tableS3_sampling_ratios_by_axis.csv`, `tableS_sampling_ratios_by_axis.csv` before the 2026-10-08
restructure) was withheld per instruction; the Jaccard-check and extremes
tables were written instead/regardless.

**`tableS4_bowen_ratio_by_igbp.csv`** (`tableS_bowen_ratio_by_igbp.csv` before the 2026-10-08
restructure; `scripts/supp_stage4_bowen_ratio_by_igbp.R`): Bowen ratio
(`H_F_MDS / LE_F_MDS`, both native W m⁻² mean rates) by IGBP class, from the pre-QC DuckDB `annual`
table with each variable gated independently on its own QC column
(`QC_THRESHOLD_YY`, same rule as `R/site_annual_fluxes.R`). Site value = median over that site's
own QC-qualifying, `LE_F_MDS > 0` site-years. `H_CORR`/`LE_CORR` exist in the `annual` table (437 /
781 and 436 / 781 current-network sites respectively have ≥1 non-NA value) but were not used in
this computation.

---

## 6. Data availability statement

**Template (fill in DATE and PID when snapshot is archived):**

FLUXNET data products used in this work were downloaded from the FLUXNET
Shuttle on [DATE]. The persistent identifier for the snapshot used is
[PID]. All data are shared under the CC-BY-4.0 license. Attribution
requirements vary by contributing network; complete citation information
for each site, including persistent identifiers, is provided in the
DATA_POLICY_LICENSE_AND_INSTRUCTIONS file included with each site's data
download and is available through network-specific portals (AmeriFlux:
https://ameriflux.lbl.gov; ICOS Carbon Portal: https://data.icos-cp.eu;
OzFlux/TERN: https://data.ozflux.org.au). Users of the dataset described
in this paper are required to follow the attribution guidelines at
data.fluxnet.org and to cite each site individually.
https://data.fluxnet.org/data-policy-license-and-instructions-for-attribution/

---

## 7. Code availability statement

**Template (fill in DOI when repository is archived):**

All code used to produce the figures and analyses in this paper is
available at https://github.com/EcosystemEcologyLab/fluxnet-annual-2026
(DOI: [ZENODO DOI]). The repository includes all R scripts, figure
generation code, and pipeline configuration. Raw FLUXNET data are not
included but can be reproduced by running scripts/01_download.R with
valid FLUXNET Shuttle credentials against the snapshot PID specified
in the data availability statement above.

---

## Instructions for Claude Code — drafting methods text

When asked to draft methods text for a specific section:
1. Read all primary code files listed for that section
2. Extract actual parameter values, thresholds, and counts from the code
   and data snapshots — do not invent or approximate values
3. Cross-check key facts against `docs/known_issues.md` and
   `docs/decisions_pending.md`
4. Draft prose in past tense, third person, suitable for a Nature-family
   methods section
5. Flag any values marked "update when finalised" with [TBD] in the draft
6. Do not draft methods text unless explicitly asked — this document is
   a requirements spec, not a drafting prompt
