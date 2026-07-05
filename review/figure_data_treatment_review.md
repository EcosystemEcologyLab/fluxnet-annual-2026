# Data-Treatment Code Review — draft_manuscript_v1 figures

**Scope:** the six figures assembled by `scripts/build_draft_manuscript_v1.R`
into `review/figures/draft_manuscript_v1/`, traced from that build script back
through each figure script, the `R/figures/` function it calls, and the data
objects it consumes, to source.

**Nature of this document:** read-and-report only. No analysis, figure, or
data file was modified. `git status` was captured before and after; the
working-tree diff is unchanged except for the two files this task is meant to
write (this review and the `SESSION_LOG.md` entry) — see
[Clean-diff confirmation](#clean-diff-confirmation).

**How to read it:** Part 1 is a plain-language narrative for co-authors and a
Methods section. Part 2 is a provenance appendix with exact `file:line`
citations to current code for a reviewer checking claims. All line numbers
refer to the working tree at commit `1cbca27`.

---

## Part 1 — Narrative

### 1.1 The six figures and their data paths at a glance

| Fig | Draft file | Shows | Reads flux data? | QC path |
|----|-----------|-------|------------------|---------|
| 1A | `fig_01a_map_current_network.png` | World map of current network site locations | No — coordinates + metadata only | n/a |
| 1B | `fig_01b_cumulative_siteyears_igbp.png` | Cumulative site-years by IGBP over time | Indirectly (pre-computed presence table) | n/a at figure stage |
| 2  | `fig_02_whittaker_current.png` | Network hexbin in MAT×MAP climate space, coloured by median NEE, with global land HDR contours | Yes — DuckDB `annual_converted` | **Pipeline default QC = 0.50** |
| 3  | `fig_03_flux_comparison_combo_nep_et_h.png` | FLUXNET2015 vs Shuttle per-IGBP median NEP/ET/H | Yes — raw extracted YY CSVs | **Override QC = 0.80** |
| 4  | `fig_04_current_network_sampling_ratios.png` | Sampling ratios (network/global) across 6 representativeness axes | No (uses pre-computed per-site class tables + global fractions) | n/a |
| 5  | `fig_05_jaccard_trajectory_with_counts.png` | Weighted Jaccard across 4 networks × 6 axes | No (uses pre-computed metrics table) | n/a |

The build script (`scripts/build_draft_manuscript_v1.R`) is a **pure copy/rename
utility**: it copies each already-rendered source PNG to a descriptive
draft-manuscript name and writes the legends (Fig 2's legend is hardcoded in
the build script; the other five are copied from source `.legend.txt` files
with a `DIMENSIONS:` line fix for Figs 4–5). It reads no data and applies no
treatment. All data decisions live upstream, in the figure scripts and the
functions they call.

### 1.2 The shared foundation: the frozen snapshot

Every figure's site list resolves, directly or indirectly, to a single frozen
snapshot CSV, **`data/snapshots/fluxnet_shuttle_snapshot_20260624T095651.csv`
(767 sites, 18 columns)**. That file was written by a single `flux_listall()`
call captured at one instant and stamped with a timestamp filename
(`R/snapshot.R` `write_snapshot()`); it was not assembled over a window. In
"development" mode the pipeline uses the live manifest as-is; "locked" mode
would read a pinned file (`R/snapshot.R` `resolve_snapshot()`), but the figure
scripts here do not run in locked mode.

**How each figure reaches the snapshot differs, and this matters for
reproducibility:**

- **Figs 1A, 1B, and 2** select the snapshot dynamically — a filename glob
  `fluxnet_shuttle_snapshot.*\.csv$` sorted descending, taking the newest. They
  are not pinned to the 20260624 file; if a newer snapshot appears in
  `data/snapshots/`, a re-run silently switches to it.
- **Fig 3** (its upstream flux-median scripts) hard-codes the exact snapshot
  path as a constant. It is pinned.
- **Figs 4 and 5** never read the snapshot directly. They read pre-computed
  per-site class-assignment CSVs and a metrics table, all keyed to the
  `current_767` network (767 sites baked in at build time of those CSVs).

A stale note worth flagging for the Methods author: the Fig 1A script header
still says "759-site network," and the committed Fig 1A/1B/2 PNGs were rendered
from an earlier 759-site snapshot. The code computes the on-figure site count
dynamically, so a re-run against the current snapshot would render **767** (all
767 have valid coordinates). This is a rendering-currency gap, not a code bug.

### 1.3 Two flux data paths, two QC thresholds — the central finding

The two figures that touch flux magnitudes (2 and 3) take **different data
paths with different QC thresholds**, and a Methods section must not describe
them as one treatment.

- **Fig 2 (NEE hexbin)** reads the `annual_converted` table from the project
  DuckDB database (`data/duckdb/fluxnet.duckdb`), filtered to the FLUXMET
  dataset. That table is the end of the pipeline chain
  `extracted CSVs → 03b (build DB) → 04_qc.R → 05_units.R`. QC gating happens in
  `04_qc.R` at the **pipeline default `QC_THRESHOLD_YY = 0.50`**: a site-year is
  dropped when its gap-filled fraction exceeds 0.50 (equivalently, kept when the
  measured/good-quality QC fraction is ≥ 0.50). So Fig 2's NEE colours rest on
  0.50-gated annual data.

- **Fig 3 (FLUXNET2015 vs Shuttle medians)** does **not** use the DuckDB
  pipeline at all. Its upstream script reads the **raw extracted YY CSVs**
  directly from `data/extracted/` and applies a local, stricter
  **`QC_THRESH = 0.80`**. The same 0.80 is applied identically to the
  FLUXNET2015 side, so the two axes of the comparison are internally consistent
  with each other — but not with Fig 2, nor with the rest of the pipeline.

The 0.80 is a deliberate, documented override of the 0.50 pipeline default (the
`R/pipeline_config.R` comment records that 0.50 itself was lowered from 0.75 "to
match FLUXNET published convention"). No numeric justification for choosing 0.80
specifically (over, say, 0.75) appears in the code — it is stated as a stricter
threshold applied consistently across the comparison, nothing more.

**Net effect for the Methods section:** Fig 2 and Fig 3 sit on different QC
thresholds (0.50 vs 0.80) and different code paths (DuckDB `annual_converted`
vs raw `data/extracted/` YY files). This is defensible — Fig 2 is a
climate-space density display where the exact threshold matters little, and
Fig 3 is a like-for-like network comparison where a stricter, symmetric
threshold is appropriate — but it must be stated, not glossed.

### 1.4 Fallback logic (VUT/CUT and NT/DT)

Both fallbacks exist in two places (the DuckDB QC step and the flux-median
script) and both are **per-site, not per-row/per-year**, which is the
scientifically correct choice (it avoids mixing partitioning conventions within
a single site's median).

- **VUT vs CUT** (which u*-threshold reference NEE/GPP/RECO come from): the
  preferred variant is VUT; CUT is used only when VUT is unavailable or fails
  QC. In the flux-median script this is decided per site-year (VUT if
  `NEE_VUT_REF_QC ≥ 0.80`, else CUT if it qualifies) and applied jointly to
  NEE/GPP/RECO for that year. In the DuckDB QC step the *QC-gating column* is
  chosen once per site (VUT if the site has any non-NA `NEE_VUT_REF_QC`, else
  CUT), never mixing the two within a site. Fig 2's climate function additionally
  coalesces `NEE_VUT_REF → NEE_CUT_REF` when forming its per-site median NEE, to
  recover the ~36 CUT-only sites.

- **NT vs DT** (nighttime vs daytime partitioning for GPP and TER): NT is
  preferred; DT is used as a whole-site fallback only when NT yields zero
  qualifying years for that site. The decision is per site and per flux (GPP and
  TER decided independently) and is recorded in `gpp_partition` / `ter_partition`
  output columns. This only affects Fig 3 (Fig 2 uses NEE only).

### 1.5 Derived values and unit conversions (Fig 3's upstream)

Quoting the actual formulas from `scripts/assess_flux_data_by_igbp_shuttle.R`:

- **NEP** = −NEE (sign flip; positive = net uptake). Units gC m⁻² yr⁻¹ (the YY
  product is pre-integrated).
- **GPP / TER**: taken from the partitioning source columns selected above
  (`GPP_NT_{VUT,CUT}_REF` / `RECO_NT_{VUT,CUT}_REF`, with DT fallback columns).
  Units gC m⁻² yr⁻¹, pre-integrated, no conversion.
- **ET** = `LE_F_MDS × SECS_YR / LAMBDA`, where `SECS_YR = 365.25 × 86400` and
  `LAMBDA = 2.45 × 10⁶ J kg⁻¹`. This yields mm yr⁻¹ (1 kg m⁻² = 1 mm depth).
- **H** = `H_F_MDS`, annual mean, **not converted**; units W m⁻².

Per-site value = the median across all QC-passing years for that variable. The
per-IGBP-class value plotted in Fig 3 is the **median of those site medians**,
with error bars = ±1 SD of the cross-site spread of site medians within the
class (not year-to-year, not measurement uncertainty).

### 1.6 Representativeness metrics and binning (Figs 4 and 5)

Both figures come from one script (`scripts/figure_representativeness_summary.R`)
but display different quantities and — importantly — **different binning for the
continuous axes**.

- **Sampling ratio** (Fig 4) = network fraction of a class ÷ global-land fraction
  of that class; > 1 over-sampled, < 1 under-sampled, `NA` when either side is
  zero (so structurally-absent classes are flagged, not plotted as 0). Displayed
  on a log₂ axis.
- **Weighted Jaccard** (Fig 5) = `Σ min(pₖ,qₖ) / Σ max(pₖ,qₖ)` (Ružička
  similarity), pₖ = global-land fraction, qₖ = network fraction. Bounded [0,1],
  1 = identical. This is the only metric Fig 5 plots.
- **Hellinger distance** = `(1/√2)·√Σ(√pₖ − √qₖ)²`. It **is** computed and stored
  (it sits in the `representativeness_metrics.csv` the summary script reads, and
  in every per-axis compute script) but is **not displayed in either
  manuscript figure** — the summary script never references it. So Hellinger is
  retained in provenance but retired from the manuscript figures.

**Binning drift is real and figure-specific.** For the three continuous axes
(Biomass, TRENDY NEE-IAV, TRENDY ET-median), the two figures use different
hybrid bin counts:

- **Fig 4** (sampling-ratio grid, the default 6-axis config) uses the
  **7-bin** hybrid.
- **Fig 5** (Jaccard trajectory) uses the **18-bin** hybrid.

Both are the same "1 fixed near-zero bin + N equal-area quantile bins"
construction, differing only in N. This means the two representativeness
figures in the same manuscript set are not on a common binning for the
continuous axes — deliberate (a coarse bar chart vs a finer trajectory) but it
must be stated in the Methods/caption, and it is the concrete substance behind
the "7-bin vs 18-bin" caution.

### 1.7 Site exclusions

- **Fig 3:** CVM excluded (absent from the FLUXNET2015 release — no
  x-coordinate); CSH excluded (n = 2 in FLUXNET2015, below an n ≥ 5 reliability
  threshold). Non-standard IGBP labels (BSV, DNF, SNO) excluded by construction
  (only the 12 standard IGBP classes are scored). 10 classes plotted:
  EBF, MF, DBF, ENF, OSH, WSA, SAV, GRA, WET, CRO. Sites with no QC-passing year
  for a variable contribute NA to that variable and drop out of its class
  statistics. FLUXNET2015 side: 206 of 212 release sites (6 Tier-2-only sites
  excluded to keep the download CC-BY-4.0-scoped).
- **Fig 2:** sites with no WorldClim climate match, or no finite annual NEE
  after the VUT/CUT coalesce, are dropped from both the hexbin and the point
  layer. No IGBP filter.
- **Fig 1A:** coordinate-validity filter (non-NA and within ±90 lat / ±180 lon)
  plus de-duplication on `site_id`. No IGBP filter, no exclusion list.
- **Fig 1B:** sites whose `igbp` is NA or outside the 15-class `IGBP_order` are
  silently dropped (≈ 749 of 767 retained); no site-count text is drawn.
- **Fig 4/5:** classes with zero network sites appear as structural zeros
  (sampling ratio NA / trajectory contribution 0).

### 1.8 Code-vs-doc drift to flag for the Methods author

Verified against current code; do **not** copy these from the docs:

1. **`docs/methods_requirements.md` carries stale counts and unfilled
   placeholders.** It states 672 sites downloaded / 530 with valid NEE_VUT_REF
   and a 2026-04-14 download date — all from an earlier, smaller snapshot; the
   current figures use the 767-site 20260624 snapshot. Its data- and
   code-availability statements contain literal `[PID]`, `[DATE]`, and
   `[ZENODO DOI]` placeholders — no persistent identifier exists yet for the
   snapshot or the repository.
2. **7-bin vs 18-bin:** see §1.6. Fig 4 = 7-bin, Fig 5 = 18-bin, in current
   code. Any per-axis `methods_*.md` that reports only 7-bin numbers as
   canonical is stale relative to Fig 5.
3. **Fig 2's hardcoded legend states "N = 767 sites | 4,236 site-years"** while
   the committed PNG was rendered from a 759-site snapshot; and it correctly
   describes the overlay's KDE/land-mask method but the *base-layer* NEE data is
   0.50-gated `annual_converted`, which the legend attributes to
   "04_qc.R / 05_units.R" (accurate) without stating the 0.50 threshold.

---

## Part 2 — Provenance appendix (file:line)

All paths relative to repo root; line numbers at commit `1cbca27`.

### A. Build script (copy/rename only)

- `scripts/build_draft_manuscript_v1.R:38-51` — figure copy map (source PNG →
  draft name) for all six figures.
- `:43-44` — Fig 2 source is `review/figures/whittaker/fig_whit_fig2_with_both_contours.png`.
- `:53-61` — copies each source PNG; `:90-101` writes legends 1A/1B/3/4/5 from
  source `.legend.txt` (with `:96-98` `DIMENSIONS:` line rewrite for Figs 4–5).
- `:110-212` — Fig 2 legend hardcoded here (not copied), because the source
  legend carries the retired Mahalanobis statistic (`:103-109` comment).
- No `read_csv` / DuckDB / data access anywhere in this script — confirmed pure
  file I/O.

### B. Snapshot construction (shared)

- `R/snapshot.R:17-39` — `resolve_snapshot()`: development mode returns the live
  `flux_listall()` manifest unchanged (`:38`); locked mode reads
  `FLUXNET_SNAPSHOT_FILE` (`:20-32`).
- `R/snapshot.R:53-61` — `write_snapshot()`: `timestamp <- format(Sys.time(),
  "%Y%m%dT%H%M%S")` (`:55`), filename `fluxnet_shuttle_snapshot_<ts>.csv`
  (`:56`) — single point-in-time write, not windowed.
- Snapshot file in use: `data/snapshots/fluxnet_shuttle_snapshot_20260624T095651.csv`
  (767 data rows, columns: `data_hub, site_id, site_name, location_lat,
  location_long, igbp, network, team_member_name, team_member_role,
  team_member_email, first_year, last_year, download_link, fluxnet_product_name,
  product_citation, product_id, oneflux_code_version, product_source_network`).

### C. Pipeline QC (feeds Fig 2), default threshold 0.50

- `R/pipeline_config.R:40,43,46,49` — `QC_THRESHOLD_DD/WW/MM/YY <- 0.50`;
  `:37` comment: "Lowered from 0.75 to 0.50 to match FLUXNET published
  convention".
- `scripts/04_qc.R:80-86` — annual gate uses `QC_THRESHOLD_YY`.
- `:105-127` — per-site (not per-row) QC column selection: VUT if the site has
  any non-NA `NEE_VUT_REF_QC` (`:123`), else CUT (`:124`), else no gate.
- `:140-147` — `p_gapfilled = 1 - <chosen QC>`; `qc_flagged = p_gapfilled >
  threshold`.
- `:155-157` — writes `<table>_qc` keeping `is.na(qc_flagged) | !qc_flagged`
  (i.e. retained when QC fraction ≥ 0.50).
- `scripts/05_units.R:56-58` — reads `<table>_qc`, writes `<table>_converted`
  (so `annual_qc → annual_converted`); carbon at YY passes through, unit
  conversions applied to LE/H/TA/etc.

### D. Figure 1A — current-network map

- Script: `scripts/generate_point_maps.R:42-49` — snapshot glob
  `fluxnet_shuttle_snapshot.*\.csv$` sorted descending `[[1]]`, then
  `readr::read_csv(snap_file)`.
- `:73` — `fig_map_point_network(metadata = shuttle_meta, ...)`; `:135` copies
  master PNG to `review/figures/candidates/fig_03_map_current.png`.
- Function: `R/figures/fig_maps.R:1104` `fig_map_point_network()`.
- `:1114-1120` — filter `!is.na(location_lat/long)`, `between(lat,-90,90)`,
  `between(long,-180,180)`, then `distinct(site_id, .keep_all = TRUE)`.
- `:1147` — on-figure count `paste0("n = ", nrow(sites_clean), " sites")`
  (dynamic; no hard-coded 759/767).
- No flux/QC/DuckDB access. Stale "759-site" header at
  `scripts/generate_point_maps.R:6-7,71`.

### E. Figure 1B — cumulative site-years by IGBP

- Script: `scripts/generate_duration_histograms.R:49-56` — same snapshot glob +
  read; `:88-93` reads `data/snapshots/site_year_data_presence.csv` into
  `presence_df`; `:325-337` calls the figure function.
- Function: `R/figures/fig_network_growth.R:839` `fig_cumulative_siteyears_igbp()`.
- `:851-854` — IGBP lookup filtered to `IGBP_order` (drops NA/out-of-set IGBP);
  `:856-862` — presence rows year-windowed and inner-joined to IGBP-matched
  sites (≈ 749 of 767 retained).
- Upstream: `site_year_data_presence.csv` is a committed table produced by
  `compute_site_year_presence()` at `R/utils.R:160` (invoked from
  `scripts/compute_site_record_length.R`), which reads monthly flux — so Fig 1B's
  flux dependency is one step upstream and pre-baked. No direct QC/DuckDB access
  in the figure function. No site-count text drawn.

### F. Figure 2 — Whittaker hexbin + HDR contours

- Script: `scripts/generate_whittaker_overlays.R:155-157` — reads DuckDB
  `annual_converted`, `filter(dataset == "FLUXMET")`, selects
  `site_id, TIMESTAMP, NEE_VUT_REF, NEE_CUT_REF`.
- `:162-173` — snapshot glob (newest) + `read_csv` for the site list
  (`shuttle_meta`).
- Base-layer function: `R/figures/fig_climate.R:115` `fig_whittaker_worldclim()`.
  - `:227-235` — per-site median NEE via `coalesce(NEE_VUT_REF, NEE_CUT_REF)`
    then `median(NEE_ref)`; comment `:227` notes recovery of ~36 CUT-only sites.
  - `:287-291` / `:471-473` — `stat_summary_hex(..., bins = 15)`, statistic =
    median.
  - `:210-215` — NEE colour limits from the 5th–95th percentile of the full
    (coalesced) NEE distribution.
- Overlay functions: `R/figures/fig_climate.R` `build_global_landclimate()`
  (`:364`, land mask = ESA CCI 2015, water/ice excluded `:386-390`, land-fraction
  ≥ 0.5 and lat ≥ −60 `:399-401`, cosine-latitude weight `:409-414`);
  `.weighted_density_grid()` (`:953`, default `gridsize = c(201,201)`, actually
  passed by `generate_whittaker_global.R`); `.hdr_levels()` for 95/99% contours.
- QC provenance: inherited from `annual_converted` = **0.50-gated** (see §C);
  the overlay's contour data is WorldClim/CCI, not flux.

### G. Figure 3 — FLUXNET2015 vs Shuttle NEP/ET/H combo

- Combo script: `scripts/figure_flux_comparison_combo.R:38` reads
  `data/snapshots/flux_comparison_fluxnet2015_vs_shuttle.csv`; `:43-47` panels
  NEP/ET/H; `:73` `filter(flux == flux_code, !excluded)`; `:85-113` plots
  x = `fluxnet2015_median`, y = `shuttle_median`, ±SD error bars both axes.
- Comparison-table builder: `scripts/figure_flux_comparison_fluxnet2015_vs_shuttle.R:43-46`
  reads `site_flux_medians_fluxnet2015.csv` + `site_flux_medians_shuttle.csv`,
  writes the comparison CSV (`:146`); `:89-97` per-class `median`, `sd`, `n_sites`
  = median-of-site-medians / SD of site medians / count; `:53` `EXCLUDED_CLASSES
  <- c("CVM","CSH")`; `:124-127` `excluded` flag and diff/pct columns.
- Shuttle per-site medians: `scripts/assess_flux_data_by_igbp_shuttle.R`:
  - `:43` `QC_THRESH <- 0.80`; `:44` `LAMBDA <- 2.45e6`; `:45` `SECS_YR <-
    365.25 * 86400`; `:47` pinned `SNAP_CSV` (the 20260624 snapshot).
  - `:91-93` reads snapshot, `distinct(site_id)`.
  - `:99-100` scans **`data/extracted/`** for `FLUXMET_YY.*\.csv$` (raw
    extracted files, bypassing DuckDB); `:118-124` dedup keeps the largest file
    per site.
  - `:165-168` VUT/CUT qualification (`NEE_*_REF_QC >= QC_THRESH`), `:170-185`
    selection; `:187-188` `NEP = -NEE`; `:190-194` `et_val <- le_val * SECS_YR /
    LAMBDA`; `:196-199` H = `H_F_MDS`, unconverted.
  - `:215-238` NT/DT **per-site** decision (NT if ≥1 qualifying year, else DT),
    GPP and TER independent; `:253-257` per-site medians.
  - `:334-337` non-standard IGBP → "OTHER"/"MISSING"; `:51-52` 12 standard IGBP.
- FLUXNET2015 per-site medians: `scripts/assess_flux_data_by_igbp_fluxnet2015.R:54`
  `QC_THRESH <- 0.80` (same override); `:26-27` 206 of 212 sites (6 Tier-2-only
  excluded); `:250-252` same VUT/CUT gate at 0.80.
- Output PNG: `figure_flux_comparison_combo.R:22,39`.

### H. Figure 4 — sampling-ratio grid (current network)

- Script: `scripts/figure_representativeness_summary.R:963-967` —
  `make_grid_fig("current_767", "…/fig_rep001_current.png", mode="single")`.
- `:274-286` `site_csv()` / `count_sites()` load per-site class tables
  (`site_<axis>[_<network>].csv`); `current_767` → no suffix.
- `:289-302` `merge_sr()`; sampling ratio at `:296-298`:
  `sampling_ratio = if_else(global_land_fraction > 0 & network_frac > 0,
  network_frac / global_land_fraction, NA_real_)`; `:300` `log2_sr`.
- Continuous-axis binning = **7-bin** in this default config:
  `:371-372` Biomass `m_agg = "7bin_hybrid"`, `:390-391` NEE-IAV `"7bin_hybrid"`,
  `:409-410` ET-median `"7bin_hybrid"`.
- `:832` `current_767 = 767L`. No flux/QC read in this script.

### I. Figure 5 — Jaccard trajectory with counts

- Script: `scripts/figure_representativeness_summary.R:1046-1052` —
  `make_traj_with_bars(traj_df_6, …, "…/fig_rep008_jaccard_trajectory_with_counts.png",
  width_in = 3.5, height_in = 4)`.
- `:266-268` loads `representativeness_metrics.csv`; `:305-310` `get_j()` pulls
  **only** `weighted_jaccard` (`:308`).
- Continuous-axis binning = **18-bin** here: `:1034-1036` Biomass / NEE-IAV /
  ET-median all `agg = "18bin_hybrid"` — contrast Fig 4's 7-bin.
- `grep -ni "hellinger" scripts/figure_representativeness_summary.R` → no matches
  (Hellinger computed/stored upstream but not displayed).

### J. Representativeness metric formulas (upstream of Figs 4–5)

- `scripts/figure_representativeness_landcover.R:325-329` — canonical
  `compute_repr_metrics(p, q)`:
  `weighted_jaccard = sum(pmin(p,q))/sum(pmax(p,q))`;
  `hellinger_distance = (1/sqrt(2)) * sqrt(sum((sqrt(p)-sqrt(q))^2))`.
  The same function/formula is replicated in the other per-axis compute scripts
  (`figure_representativeness_biomass.R`, `…_kg.R`, `…_aridity.R`,
  `…_trendy_compute.R`, `recompute_continuous_axes_multibin.R`,
  `recompute_continuous_axes_30bin.R`), which populate
  `data/snapshots/representativeness_metrics.csv` (the table Fig 5 reads and
  Fig 4's `get_j` queries).

### K. Drift table (code value vs doc claim)

| Claim location | Doc says | Current code says |
|---|---|---|
| `docs/methods_requirements.md:40,73` | 672 sites downloaded / 530 valid NEE_VUT_REF | 767-site snapshot (`…20260624T095651.csv`) |
| `docs/methods_requirements.md:39` | download date 2026-04-14 | snapshot stamped 20260624 |
| `docs/methods_requirements.md:234-235,254` | data/code availability | literal `[PID]`, `[DATE]`, `[ZENODO DOI]` placeholders — no PID/DOI exists |
| per-axis `methods_*.md` (Biomass/TRENDY) | 7-bin numbers as canonical | Fig 4 = 7-bin, **Fig 5 = 18-bin** (`summary.R:1034-1036`) |
| Fig 2 hardcoded legend (build script `:176-177`) | "N = 767 sites \| 4,236 site-years" | committed PNG rendered from a 759-site snapshot |

### Verification notes / explicit "not found"

- **Persistent identifier for the snapshot:** searched `docs/methods_requirements.md`,
  README/config/metadata — only unfilled `[PID]`/`[ZENODO DOI]` placeholders.
  Not found: no assigned snapshot-level or repo-level DOI/PID.
- **Hellinger in the summary figure script:** searched
  `scripts/figure_representativeness_summary.R` for `hellinger` — not found
  (computed and stored upstream, not displayed in Figs 4–5).
- **Fig 3 QC in the DuckDB pipeline:** searched — Fig 3's upstream reads
  `data/extracted/` YY CSVs directly (`assess_flux_data_by_igbp_shuttle.R:99-100`)
  and applies its own 0.80; it does **not** read `annual_converted`. Confirmed
  the two flux figures are on separate paths.
- **Locked-mode snapshot for the figure scripts:** searched — Figs 1A/1B/2 use a
  newest-file glob, not `resolve_snapshot()`/`FLUXNET_SNAPSHOT_FILE`; they are not
  pinned.

### Clean-diff confirmation

`git status` before and after this task shows an identical set of pre-existing
modified/untracked files (e.g. `data/snapshots/flux_comparison_fluxnet2015_vs_shuttle.csv.meta.json`,
`renv/activate.R`, `review/figures/climate/fig_environmental_response_era5.png`,
and a long list of untracked `logs/` files) — **none created or modified by this
review.** The only new tracked additions are `review/figure_data_treatment_review.md`
(this file) and the `SESSION_LOG.md` entry. No analysis, figure, or data file was
touched.
