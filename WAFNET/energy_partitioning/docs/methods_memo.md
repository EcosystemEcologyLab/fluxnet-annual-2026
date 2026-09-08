# WAFNET energy partitioning — methods memo

Side analysis, not part of the FLUXNET Annual Paper 2026 (see `../README.md`).
Every filtering decision and assumption made in `code/` is recorded here as
it is made — this file grows with the analysis rather than being written
retrospectively.

## Sites

Six sites spanning a humid-to-Sahelian gradient: `GH-Ank`, `BJ-Db1`,
`BJ-Bfg`, `BJ-Nhu`, `SN-Nkr`, `SN-Dhr`. See `../README.md` for names,
countries, and IGBP classes.

## Data acquisition

- **Source:** FLUXNET Shuttle, `flux_listall()` + `flux_download()`, same as
  the Annual Paper's primary data source (CLAUDE.md Hard Rule #1), by choice
  — not a rule requirement for this side analysis, but adopted for the same
  provenance/reproducibility reasons.
- **Resolution:** half-hourly (HH) only, extracted fresh via
  `flux_extract(resolutions = "h")`. The repo-root `data/` tree only ever
  extracted `y m d` for the Annual Paper (`R/pipeline_config.R`), so this
  required a **new download** — the raw ZIPs for these six sites had already
  been deleted after the Annual Paper's own extraction (`FLUXNET_DELETE_ZIPS`
  default `TRUE`, `scripts/02_extract.R`).
- **Isolation:** `FLUXNET_DATA_ROOT` is overridden to
  `WAFNET/energy_partitioning/data/` for every script in this directory
  (`code/00_config.R`). Nothing under the repo-root `data/` directory is
  written to by this analysis.
- **Ancillary site metadata** (canopy height, LAI, land cover, heat-flux
  instrumentation): read **read-only** from the per-site BIF files already
  extracted for the Annual Paper (`data/extracted/*_BIF_*.csv`), per the
  user's explicit choice (2026-09-08) rather than a separate ancillary
  download.

## Pre-analysis report (`code/03_report_variables_and_closure.R`)

Full numbers in `report_back_20260908.md`. Two data-quality flags surfaced
that must be resolved (or explicitly accepted) before Tasks 1–3 use these
sites' energy-balance terms:

- **BJ-Nhu:** `G_F_MDS` column exists in the FULLSET header but is 100% NA
  across all 175,344 half-hours — no soil heat flux measurement at all for
  this site. Any Rn−G-based quantity (closure, EF-corrected variant, Budyko
  dryness ratio using available energy) needs an explicit decision for this
  site: drop G from the available-energy term, or exclude BJ-Nhu from
  those specific outputs.
- **SN-Nkr:** the closure regression is not usable as fit (R² = 0.001,
  slope = 0.05, n = 69,224 measured half-hours). `G_F_MDS` swings to
  physically large magnitudes (25% of qualifying half-hours exceed
  ±200 W/m², up to ~860 W/m² at the extreme) consistently across all 7
  years (2018–2024) — not a transient fault, and not fixed by also gating
  on `G_F_MDS_QC == 0` (checked). Could be genuine sparse-canopy Sahelian
  soil-heat-flux dynamics, or a plate-siting/calibration issue; the Shuttle
  data and BADM don't distinguish these (no heat-flux instrumentation group
  documented for this site — see below). Do not use SN-Nkr's `G_F_MDS` or
  closure numbers until this is resolved with the PI or site team.

Filtering decisions baked into that script:

- **Energy balance closure regression:** OLS of `(LE_F_MDS + H_F_MDS) ~
  (NETRAD - G_F_MDS)`, restricted to half-hours with `LE_F_MDS_QC == 0 AND
  H_F_MDS_QC == 0` (measured only, no MDS gap-filling). Gap-filled records
  are excluded because MDS gap-filling itself draws on nearby measured
  energy-balance terms, which would inflate apparent closure if included.
  Sites with fewer than 30 qualifying half-hours are reported as too thin to
  fit, not silently regressed anyway.
- **G measurement / storage above the plates:** the Shuttle BADM (BIF files)
  were checked for a `GRP_HEATFLUX` / `GRP_SOILHEATFLUX` metadata group for
  each of the six sites — see `tables/g_measurement_notes.csv` for the
  per-site result. Where no such group exists, plate depth and whether soil
  heat storage above the plates is included in `G_F_MDS` are **not
  determinable from Shuttle metadata alone**; confirming this would require
  the site PI's own documentation or the AmeriFlux/ICOS site page, not
  something to infer from the FULLSET column names.

## Open questions / assumptions to revisit before Task 1–3

- **BJ-Nhu has no G data (decision needed):** exclude from Rn−G-dependent
  outputs, or substitute LE+H vs. NETRAD (no G) for this site specifically?
- **SN-Nkr closure/G is flagged, not resolved:** use anyway with a caveat,
  investigate against site-team documentation first, or exclude SN-Nkr from
  closure-dependent outputs (Task 1's corrected-EF variant, Task 1's
  Rn-based Budyko dryness ratio)? Task 1 explicitly asks for both corrected
  and uncorrected EF specifically so this choice is visible either way, but
  the *closure ratio itself* (not just EF) is unusable for SN-Nkr as things
  stand.
- **G storage above the plates is undocumented for all six sites** (Table 4,
  BADM has no `GRP_HEATFLUX`/`GRP_SOILHEATFLUX` group anywhere in this set).
  `G_F_MDS` should be treated as **shallow-plate flux, storage term unknown**
  for every site, not assumed storage-corrected, unless the PI can confirm
  otherwise from site-specific documentation.
- **Closure filter choice (QC=0 only) reduces n substantially** relative to
  including MDS gap-filled records — worth knowing when Task 2's variance
  decomposition needs enough site-years; see `tables/site_years.csv` for
  per-site-year half-hour counts before committing to aggregation windows.
