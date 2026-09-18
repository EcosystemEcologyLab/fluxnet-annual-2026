# ERA5 vs. BIO12 vs. BADM vs. tower-measured reference plots

Plotting task only, per instruction: no new verdict, no correction, no
reclassification, no pipeline edit. This report states what is plotted, the
sample size, and the axis/binning choices for each figure. It does not
characterise any site or group as an error, an artifact, or genuine.

This is a **replacement** of the first version of this diagnostic. It adds
a fourth estimate (tower-measured MAP) and a month-matched ratio. Per
instruction, the figures were overwritten in place at the same file names
rather than added as a parallel set; the only genuinely new file is the
month-matched ratio histogram. See "Files replaced vs. new" below for the
full accounting.

Script: `scripts/diagnostics/era5_reference_plots.R` (same script file as
before, extended in place). Confirmed via `git status`: zero changes to the
pipeline, any pipeline script, any figure, legend, or snapshot CSV, any
`era5_precip_units`/`_v2`/`_v3`/`_v4` output, or `data/duckdb/fluxnet.duckdb`
itself (opened `read_only = TRUE`).

## Data and provenance

All 781 current-network sites. Four estimates per site:

1. **`era5_map_mm`** — as the existing pipeline computes it: sum of
   `P_ERA x days_in_month` over complete calendar years, 1991-2020
   (`KG_ERA5_PERIOD`). Unchanged value, reused (see below).
2. **`bio12_mm`** — WorldClim BIO12 at the site pixel. Unchanged value, reused.
3. **`badm_map_mm`** — BADM PI-reported MAP. Unchanged value, reused.
4. **`measured_map_mm`** (NEW) — tower P_F, restricted to calendar years
   where **all 12 months** have `P_F_QC >= 0.9`. `P_F_QC` at this
   (monthly) resolution is a fraction, not the HH-resolution integer flag
   (see CLAUDE.md's QC Flag Reference); 0.9 is the "genuinely measured"
   cutoff already used in `era5_precip_units_v3` Part A. Each qualifying
   year's annual total is `sum(P_F x days_in_month)` over its 12 months
   (P_F is a mean daily rate at this resolution, like P_ERA, so it needs
   the same day-weighting); `measured_map_mm` is the mean of that annual
   total across all qualifying years for the site. **No partial year is
   annualised** — a year contributes only if every one of its 12 months
   qualifies. **No calendar-year-window restriction is applied** (unlike
   estimate 1) — all of a site's available years are eligible, since
   restricting to 1991-2020 would only shrink an already-thin tower
   record. This means the years feeding `era5_map_mm` and
   `measured_map_mm` at a given site are not necessarily the same years.

A fifth per-site value, **`ratio_to_measured_month_matched`**, uses every
site-month with `P_F_QC >= 0.9` directly (no complete-year requirement, no
annualisation): the ratio of that month's `P_ERA` to that month's `P_F`,
aggregated to the per-site median.

### What came from which file, what was recomputed

- `era5_map_mm`, `bio12_mm`, `badm_map_mm`, `ratio_to_bio12`,
  `ratio_to_badm_map`, `data_hub`, `product_source_network`,
  `cluster_membership`: unchanged values, read from
  `review/diagnostics/era5_precip_units_v3/table_b1_factor_estimates.csv`
  (itself unmodified from `era5_precip_units_v2/table_t2_ratios.csv` on
  every shared column — confirmed when the first version of this script
  was written). Nothing here is recomputed.
- `measured_map_mm`, `n_complete_measured_years`, `n_matched_months`,
  `ratio_to_measured`, `ratio_to_measured_month_matched`: **new**,
  computed by this script directly from `data/duckdb/fluxnet.duckdb`
  (`monthly` table, `dataset = 'FLUXMET'` for `P_F`/`P_F_QC`, `dataset =
  'ERA5'` for `P_ERA`), read-only. Not present in any prior diagnostic
  output.
- Spearman rho/n per scatter panel and all per-panel exclusion sets:
  computed by this script, as in the prior version.

### Missingness (all 781 sites, before any panel-specific exclusion)

| | n |
|---|---|
| Sites missing BIO12 | 0 |
| Sites missing BADM | 144 |
| Sites missing tower-measured (no complete year) | 312 |
| Sites missing all three references (BIO12, BADM, measured) | 0 |

312 sites have no calendar year where all 12 months reach the 0.9
cutoff; 469 sites have at least one such year. Among those 469, the
distribution of `n_complete_measured_years` is: min 1, 1st quartile 2,
median 4, mean 5.41, 3rd quartile 8, max 26 (2,538 qualifying complete
site-years total).

All missing values are carried through in the companion CSV as `NA`, not
dropped.

### Additional edge cases (present, not missing, but zero)

A log axis is undefined at exactly zero as well as at missing values.
`measured_map_mm` was never found to be exactly 0 in this network. The two
zero cases from the prior version are unchanged:

- `era5_map_mm == 0` at 1 site: `CA-TP2` (which also has no complete
  measured year, so it is independently excluded from every panel
  involving either ERA5 or the measured estimate).
- `badm_map_mm == 0` (present but recorded as zero) at 5 sites: `CZ-LnG`,
  `DE-Lnf`, `ES-Agu`, `ES-Amo`, `KE-Kpt`.

The full exclusion list with per-site, per-panel reasons is in
`table_panel_exclusions.csv`.

### Month-matched ratio: coverage

47,069 site-months have `P_F_QC >= 0.9`, all of which join to an ERA5
monthly value. Of these, 2,604 are excluded from the month-matched ratio
because `P_F == 0` that month (a real zero-precipitation measured month,
undefined as a ratio denominator) — these are excluded month-by-month, not
site-by-site; a site with other qualifying, non-zero months is still
plotted. 606 sites have at least 1 valid matched month (175 have none, at
all, either because no month reaches the 0.9 cutoff or every such month
had `P_F == 0`). Among the 606, `n_matched_months` per site: min 1, 1st
quartile 28, median 52, mean 73.4, 3rd quartile 97, max 333.

## Figure 1 (overwritten: 2 panels -> 3 panels)

Three histogram panels of `log10(ratio)`: ERA5/BIO12, ERA5/BADM, ERA5/
tower-measured (new). Bin width 0.1 log10 units (ratio x1.259, ~26% per
bin), not clipped — each panel's x-axis spans the full observed range for
the sites plotted in that panel. Dashed reference lines at ratio = 1x, 4x,
8x.

| Panel | n | Sites excluded | Reason |
|---|---|---|---|
| ERA5/BIO12 | 780 | 1 | `era5_map_mm == 0` (`CA-TP2`) |
| ERA5/BADM | 631 | 150 | 144 no BADM value, 5 BADM==0, 1 `era5_map_mm==0` |
| ERA5/measured (new) | 469 | 312 | all 312: no complete measured year (this set already includes `CA-TP2`, whose `era5_map_mm==0` would separately exclude it) |

Files: `fig1_ratio_histograms_versionA.png` (no grouping),
`fig1_ratio_histograms_versionB.png` (123 4x/8x-cluster sites in a second
colour, legend states the membership rule verbatim — see Figure 2 caption
below for the exact text, unchanged from the prior version).

## Figure 2 (overwritten: 3 panels -> 6 panels)

Six log-log scatter panels, identical x/y range within each panel (not
clipped): solid 1:1 line; dashed = 4x/0.25x offset; dotted = 8x/0.125x
offset. y = first-named variable, x = second-named, matching Figure 1's
ERA5/reference direction where ERA5 is involved.

| Panel | Comparison | n | Spearman rho | p | Axis range (mm/yr) | Excluded | Reason |
|---|---|---|---|---|---|---|---|
| (a) | ERA5 vs BADM | 631 | 0.578 | < 0.001 | [1.0, 658044.6] | 150 | 144 no BADM, 5 BADM==0, 1 era5==0 |
| (b) | ERA5 vs BIO12 | 780 | 0.592 | < 0.001 | [1.0, 658044.6] | 1 | era5==0 (`CA-TP2`) |
| (c) | ERA5 vs measured (new) | 469 | 0.975 | < 0.001 | [12.9, 658044.6] | 312 | no complete measured year |
| (d) | BADM vs BIO12 | 632 | 0.933 | < 0.001 | [18.0, 4500.0] | 149 | 144 no BADM, 5 BADM==0 |
| (e) | BADM vs measured (new) | 375 | 0.900 | < 0.001 | [12.9, 9581.3] | 406 | 149 BADM missing/==0 union 312 no complete measured year, 55 sites in both sets (406 = 149+312-55) |
| (f) | BIO12 vs measured (new) | 469 | 0.869 | < 0.001 | [12.9, 9581.3] | 312 | no complete measured year |

`measured_map_mm`'s network maximum (9581.3 mm/yr) and `era5_map_mm`'s
network maximum (658044.6 mm/yr) both occur at the same site, `US-HB4`,
which is why panels (a) and (b) share a wider range than (d), and why
panel (c)'s range is wide on the ERA5 side but not on the measured side.

Files: `fig2_scatter_versionA.png` (no grouping), `fig2_scatter_versionB.png`
(123 4x/8x-cluster sites in a second colour). The on-figure legend for
version B states the membership rule verbatim:

> Membership rule (verbatim, table_b1_factor_estimates.csv):
> factor_estimate = median(ratio_to_bio12, ratio_to_badm_map);
> assigned to nearest of candidate factors {1, 4, 8, 24, 1000}
> if within 15% of it (tol = 0.15), else 'elsewhere'.
> Shown = sites with nearest_cluster %in% c('4','8')
> (n = 123: 113 near 4x + 10 near 8x)

## Figure 3 — month-matched ratio (NEW, did not exist in the prior version)

One histogram panel: per-site median of (that month's `P_ERA`) / (that
month's `P_F`), over every site-month with `P_F_QC >= 0.9` and `P_F != 0`.
No annualisation and no complete-year requirement — every qualifying month
is used directly. Same bin width (0.1 log10 units) and reference lines (1x,
4x, 8x) as Figure 1, for visual comparability, though not explicitly
requested for this panel.

- **n = 606 sites.** Excludes 175 sites with zero valid matched months
  (no month reaching the 0.9 cutoff, or every such month having `P_F ==
  0`).
- Median matched months per site (among the 606 plotted) = 52.
- Range not clipped; the single most extreme site (`US-HB4`, per-site
  median ratio 445.7) appears alone at the tail.

Files: `fig3_month_matched_ratio_versionA.png` (no grouping),
`fig3_month_matched_ratio_versionB.png` (123 4x/8x-cluster sites in a
second colour, same verbatim legend as Figure 2).

## Companion CSV (overwritten)

`table_site_reference_comparison.csv` — one row per site, all 781 sites:
`site_id`, `data_hub`, `product_source_network`, `era5_map_mm`,
`bio12_mm`, `badm_map_mm`, `measured_map_mm`, `n_complete_measured_years`,
`n_matched_months`, `ratio_to_bio12`, `ratio_to_badm_map`,
`ratio_to_measured`, `ratio_to_measured_month_matched`,
`cluster_membership`. Missing values (`badm_map_mm` at 144 sites,
`measured_map_mm`/`ratio_to_measured` at 312 sites,
`ratio_to_measured_month_matched` at 175 sites) are `NA`, not dropped or
imputed.

`table_panel_exclusions.csv` (overwritten) — every site excluded from one
or more figure panels, and why. Separate from, and does not affect, the
companion CSV, which carries all 781 sites regardless.

## Files replaced vs. new

| File | Status |
|---|---|
| `fig1_ratio_histograms_versionA.png` (+ `.meta.json`) | **Replaced** — 2 panels -> 3 panels |
| `fig1_ratio_histograms_versionB.png` (+ `.meta.json`) | **Replaced** — 2 panels -> 3 panels |
| `fig2_scatter_versionA.png` (+ `.meta.json`) | **Replaced** — 3 panels -> 6 panels |
| `fig2_scatter_versionB.png` (+ `.meta.json`) | **Replaced** — 3 panels -> 6 panels |
| `fig3_month_matched_ratio_versionA.png` (+ `.meta.json`) | **New** |
| `fig3_month_matched_ratio_versionB.png` (+ `.meta.json`) | **New** |
| `table_site_reference_comparison.csv` (+ `.meta.json`) | **Replaced** — 9 columns -> 14 columns |
| `table_panel_exclusions.csv` (+ `.meta.json`) | **Replaced** — extended to the new panels |
| `report.md` | **Replaced** (this file) |
| `scripts/diagnostics/era5_reference_plots.R` | **Replaced in place** — same script file, extended |

All CSV and PNG outputs have a companion `.meta.json` (run datetime, git
commit, input sources, notes) per repository convention.
