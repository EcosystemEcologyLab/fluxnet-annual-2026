# ERA5 vs. BIO12 vs. BADM reference plots

Plotting task only, per instruction: no new verdict, no correction, no
reclassification, no pipeline edit. This report states what is plotted, the
sample size, and the axis/binning choices for each figure. It does not
characterise any site or group as an error, an artifact, or genuine.

Script: `scripts/diagnostics/era5_reference_plots.R`. Confirmed via `git
status`: zero changes to the pipeline, any pipeline script, any figure,
legend, or snapshot CSV, or any `era5_precip_units`/`_v2`/`_v3`/`_v4` output.
All new files are under `review/diagnostics/era5_reference_plots/`.

## Data and provenance

All 781 current-network sites. Per site: ERA5-derived mean annual
precipitation (MAP) as the existing pipeline computes it (sum of
`P_ERA x days_in_month` over complete calendar years, 1991-2020), WorldClim
BIO12 at the site pixel, and BADM PI-reported MAP.

- `era5_map_mm`, `bio12_mm`, `badm_map_mm`, `ratio_to_bio12`,
  `ratio_to_badm_map`, `data_hub`, `product_source_network`: unchanged
  values, originally computed in
  `review/diagnostics/era5_precip_units_v2/table_t2_ratios.csv` and carried
  through unmodified into
  `review/diagnostics/era5_precip_units_v3/table_b1_factor_estimates.csv`.
  This script reads only the v3 file (`table_b1_factor_estimates.csv`),
  after confirming beforehand that it is row-for-row identical to the v2
  file on every column the two files share (`identical()` on `site_id`
  order, `map_era5`, `bio12_mm`, and `badm_map_mm` all `TRUE`).
- `factor_estimate`, `n_refs`, `nearest_cluster`: original to
  `table_b1_factor_estimates.csv` (v3).
- `cluster_membership` (companion CSV only): a rename of `nearest_cluster`
  (`"4"` -> `"4x_cluster"`, `"8"` -> `"8x_cluster"`, else ->
  `"not_in_4x_or_8x_cluster"`). Not a recomputation.
- Spearman rho and n for each scatter panel: computed by this script
  (`cor.test(..., method = "spearman")`); not present in either input file.
- Per-panel exclusion sets (below): computed by this script by checking
  which values are non-positive or missing; not a recomputation of any MAP
  or ratio value.

Nothing else was recomputed.

### Missingness (all 781 sites, before any panel-specific exclusion)

| | n |
|---|---|
| Sites missing BIO12 | 0 |
| Sites missing BADM | 144 |
| Sites missing both | 0 |

All 144 BADM-missing sites are carried through in the companion CSV with
`badm_map_mm = NA`, `ratio_to_badm_map = NA`, not dropped.

### Two additional edge cases (present, not missing, but zero)

A log axis is undefined at a value of exactly zero, in addition to being
undefined for missing values. Two such cases exist in this network, neither
of which is a missing-data case:

- `era5_map_mm == 0` at 1 site: `CA-TP2`.
- `badm_map_mm == 0` (BADM present but recorded as zero) at 5 sites:
  `CZ-LnG`, `DE-Lnf`, `ES-Agu`, `ES-Amo`, `KE-Kpt`.

Both are carried through in the companion CSV at their recorded values (0),
not as NA and not dropped from the CSV. They are excluded only from the
specific figure panels listed below, where a value of zero cannot be placed
on a log axis. The full exclusion list with per-site, per-panel reasons is
also written to `table_panel_exclusions.csv`.

## Figure 1 — histograms of log10(ratio)

Two panels: ratio of ERA5 MAP to BIO12 (left), ratio of ERA5 MAP to BADM MAP
(right). X-axis is `log10(ratio)`; bin width = 0.1 log10 units (ratio
x1.259, i.e. ~26% per bin). Axis range is not clipped — each panel's x-axis
spans the full observed range of `log10(ratio)` for the sites plotted in
that panel, so the single most extreme site in each panel appears alone in
its own bin at the tail (site `US-HB4` in both panels: ratio_to_bio12 =
487.44, ratio_to_badm_map = 460.49). Dashed reference lines at ratio = 1x,
4x, 8x.

- **Left panel (ERA5/BIO12): n = 780.** Excludes 1 site (`CA-TP2`,
  `era5_map_mm == 0`).
- **Right panel (ERA5/BADM): n = 631.** Excludes 150 sites: the 144 with no
  BADM value, the 5 with `badm_map_mm == 0`, and the 1 (`CA-TP2`) with
  `era5_map_mm == 0`.

Files:
- `fig1_ratio_histograms_versionA.png` — one symbol/colour, no grouping.
- `fig1_ratio_histograms_versionB.png` — identical, with the 123 sites from
  `table_b1_factor_estimates.csv`'s 4x/8x clusters shown in a second
  colour. On-figure legend states the membership rule verbatim: `
  factor_estimate = median(ratio_to_bio12, ratio_to_badm_map); assigned to
  nearest of candidate factors {1, 4, 8, 24, 1000} if within 15% of it (tol
  = 0.15), else 'elsewhere'. Shown = sites with nearest_cluster %in%
  c('4','8') (n = 123: 113 near 4x + 10 near 8x).`

## Figure 2 — scatter plots

Three panels, all log-log axes: (a) ERA5 against BADM (y = ERA5 MAP, x =
BADM MAP), (b) ERA5 against BIO12 (y = ERA5 MAP, x = BIO12), (c) BADM
against BIO12 (y = BADM MAP, x = BIO12). Within each panel, x and y use an
identical range (not clipped — the exact min/max of the two variables
plotted in that panel). Solid line = 1:1; dashed lines = 4x and 0.25x
offset from 1:1; dotted lines = 8x and 0.125x offset from 1:1 (both
directions are drawn; the 4x/8x cluster analysis in v3 only tested the
ERA5-larger direction, but the offset lines here are drawn symmetrically
for all three panels as a plotting convention, not a claim about which
direction is expected).

| Panel | Comparison | n | Spearman rho | p | Axis range (mm/yr) | Sites excluded and why |
|---|---|---|---|---|---|---|
| (a) | ERA5 vs BADM | 631 | 0.578 | < 0.001 | [1.0, 658044.6] | 150: 144 no BADM value, 5 `badm_map_mm == 0`, 1 (`CA-TP2`) `era5_map_mm == 0` |
| (b) | ERA5 vs BIO12 | 780 | 0.592 | < 0.001 | [1.0, 658044.6] | 1: `CA-TP2`, `era5_map_mm == 0` |
| (c) | BADM vs BIO12 | 632 | 0.933 | < 0.001 | [18.0, 4500.0] | 149: 144 no BADM value, 5 `badm_map_mm == 0` |

Panels (a) and (b) share the same axis range because both include the same
site with the largest `era5_map_mm` value in the network; panel (c) does not
involve ERA5 and so has a much narrower range.

Files:
- `fig2_scatter_versionA.png` — one symbol/colour, no grouping.
- `fig2_scatter_versionB.png` — identical, with the same 123 4x/8x-cluster
  sites in a second colour and the same verbatim membership-rule legend as
  Figure 1 version B.

## Companion CSV

`table_site_reference_comparison.csv` — one row per site, all 781 sites
(no site dropped): `site_id`, `data_hub`, `product_source_network`,
`era5_map_mm`, `bio12_mm`, `badm_map_mm`, `ratio_to_bio12`,
`ratio_to_badm_map`, `cluster_membership`. Missing BADM is `NA`, not
dropped or imputed.

`table_panel_exclusions.csv` — the 6 sites excluded from one or more figure
panels for the zero-value reasons above, and which panels each applies to.
This is separate from, and does not affect, the companion CSV, which
carries all 781 sites including these 6.

## Outputs

| File | Contents |
|---|---|
| `fig1_ratio_histograms_versionA.png` | Figure 1, no grouping |
| `fig1_ratio_histograms_versionB.png` | Figure 1, 4x/8x cluster highlighted |
| `fig2_scatter_versionA.png` | Figure 2, no grouping |
| `fig2_scatter_versionB.png` | Figure 2, 4x/8x cluster highlighted |
| `table_site_reference_comparison.csv` | Companion CSV, all 781 sites |
| `table_panel_exclusions.csv` | Per-site, per-panel exclusion reasons |

All CSV and PNG outputs have a companion `.meta.json` (run datetime, git
commit, input sources, notes) per repository convention.
