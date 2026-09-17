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

## Figure pack (round 2)

### Figure 2 — ground heat flux (G) diagnostic, priority: SN-Nkr

`code/04_figure2_g_diagnostic.R` → `figures/figure2_g_diagnostic_snnkr.{pdf,png}`.

- **No QC filtering, no completeness threshold.** This is a raw visual
  diagnostic of what the instrument reported, not a filtered analysis
  figure — MDS gap-filled half-hours are included in every panel; only
  rows where the relevant variable(s) are NA are dropped.
- **BJ-Nhu excluded from every panel** — `G_F_MDS` present in header, 100%
  NA (see pre-analysis report §5), not a genuine zero-amplitude signal.
- Panel (d)'s color scale is capped at ±300 W m⁻² (`oob = scales::squish`)
  so the extreme values noted in the pre-analysis (up to ~860 W m⁻²) don't
  wash out the rest of the record; stated explicitly in the panel title
  and caption, not just implied by the legend.
- Used `geom_tile()`, not `geom_raster()`, for panel (d) — the date axis
  has gaps (missing days within site-years, e.g. SN-Nkr 2018 has 15,711 of
  a possible 17,568 half-hours) and `geom_raster` assumes a complete
  regular grid, which shifts pixels out of place when the grid isn't
  actually complete.

**Finding (descriptive, not yet interpreted for the manuscript):** at
SN-Nkr, `G`'s diurnal amplitude (peak ≈ +450 to +480 W m⁻², night ≈ −150
W m⁻²) is comparable in magnitude to `NETRAD`'s diurnal amplitude at the
same site — unlike any of the other four sites with usable G data, where
`G`'s diurnal swing is a small fraction of `Rn`'s (GH-Ank and BJ-Db1
essentially flat; BJ-Bfg and SN-Dhr rise to a modest ~50–100 W m⁻² midday
bump against an ~450–550 W m⁻² `Rn` peak). The date × hour heatmap (panel
d) shows the same diurnal pattern present continuously across all 7 years
(2018–2024), not confined to particular dates — i.e. this is a persistent
feature of the record, not an episodic sensor fault. This is exactly the
pattern the pre-analysis flagged as needing PI/site-team review before
using SN-Nkr's `G_F_MDS` (see Open questions below) — the figure does not
by itself distinguish a unit/sign/siting error from a genuine sparse-
canopy signal, both of which are physically compatible with a persistent
(not episodic) pattern.

### Environment bug found and fixed while building Figure 2

`grDevices::cairo_pdf` (what `fig_helpers.R::save_figure()` originally
used for the vector PDF output) **silently produces no file at all** on
this machine — it dlopen()s system cairo/X11 libraries
(`/opt/X11/lib/libSM.6.dylib`, `libXrender.1.dylib`) that aren't present,
emits a "failed to load cairo DLL" *warning* (not an error), and the
script continues as if `ggsave()` had succeeded. Caught only by checking
`figures/` after the first run and finding no `.pdf` despite a "Saved
figure" message in the log.

Fixed in `fig_helpers.R` by switching `save_figure()` to the base
`grDevices::pdf` device, which does write a real file here. Trade-off:
base `pdf()` only supports 8-bit (Latin-1-range) text — a couple of
Unicode characters in Figure 2's plain-text titles (em dash `—`,
superscript minus `⁻`) were silently dropped/mis-substituted (`mbcsToSbcs`
warnings) before being replaced with ASCII equivalents (`-`, `^-2`).
`expression()`/plotmath axis labels (e.g. `expression(G~(W~m^-2))`) are
**not** affected by this — they render via R's graphics engine directly,
not via the text-encoding path, and are the safer way to get superscripts
into a plot rendered this way.

**House rule going forward, this figure pack:** keep all `ggtitle`/`labs`/
`plot_annotation` title and label strings ASCII-only; use `expression()`
for anything needing superscripts or non-Latin-1 symbols. `caption`
strings passed to `save_figure()` are stored as plain R character data
(not yet rendered through this text path) and still contain Unicode
(em dashes, ², ±, §) — fine for now, but the eventual
`figure_pack_20260908.pdf` assembly script will need its own unicode-safe
rendering path for caption text; not resolved here, flagged for that
script.

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
