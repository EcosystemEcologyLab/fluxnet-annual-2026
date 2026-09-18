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

### Legibility reformat (second update, same day)

The three figures were then **replotted for legibility only** — same data,
same values, same membership rule; nothing in "Data and provenance" below
changed. What changed is presentation, in the same script file:

- The membership-rule text is no longer drawn inside every panel (it
  previously consumed roughly half the panel width). It now lives in a
  companion `<basename>.legend.txt` file for each version-B figure, a
  single one-line footnote beneath the whole figure, and one shared colour
  legend collected below all panels (via `patchwork::plot_layout(guides =
  "collect")`) instead of one legend per panel.
- Figure 1 and Figure 3's histograms now use a **fixed core x-range of
  0.1x-20x** with a single black-outlined overflow bin at each end for
  ratios outside it (labelled `<0.1x` / `>20x`), instead of a full 6-decade
  axis stretched by a handful of extreme sites. The exact site lists for
  every overflow bin are given below (previously omitted).
- Figure 2's six scatter panels now share **one common axis range, 10 to
  10,000 mm/yr,** chosen from the bulk of the pooled data, instead of a
  different per-panel range. Points outside it are shown clamped to the
  panel edge as a triangle (not dropped) and are named individually below.
- Scatter points are smaller (size 1.1, alpha 0.4-0.45) and, in version B,
  the 4x/8x-cluster points are drawn after (on top of) the rest.
- Base font sizes were increased (13 for the histograms, 12 for the
  6-panel scatter) and margins widened so no axis title is clipped at the
  image edge; the histogram's `4x`/`8x` tick labels no longer collide
  (dropping the previously-adjacent `10x` tick and widening the spacing
  around them).
- Figure dimensions were set so panels are close to square at ~1000px
  display width: Figure 1 10x4in (version A) / 10x4.7in (version B, extra
  height for the legend+footnote row), Figure 2 10x9in / 10x9.8in, Figure 3
  7x5in / 8x5.8in (version B widened slightly so its longer footnote fits),
  all at 300 dpi, white background. Exact final pixel dimensions and file
  sizes are in the table at the end of this report.

Every figure was rendered and re-opened after this reformat to confirm no
clipping, no overlapping text, and that the data fills the panel.

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

## Figure 1 (overwritten: 2 panels -> 3 panels; then reformatted)

Three histogram panels of `log10(ratio)`: ERA5/BIO12, ERA5/BADM, ERA5/
tower-measured. Bin width 0.1 log10 units (ratio x1.259, ~26% per bin).
**Core x-range fixed at 0.1x-20x** (dashed reference lines at ratio = 1x,
4x, 8x). Ratios outside the core range are not dropped and do not stretch
the axis — each is placed in a single black-outlined bin at that end
(`<0.1x` on the left, `>20x` on the right), with the bin's count printed
above it on the plot.

| Panel | n | Sites excluded from panel | `<0.1x` (n, sites) | `>20x` (n, sites) |
|---|---|---|---|---|
| ERA5/BIO12 | 780 | 1 (`era5_map_mm == 0`, `CA-TP2`) | 13: CN-GuT, US-A37, US-A39, US-Akn, US-CF1, US-CF2, US-CF3, US-CF4, US-EKH, US-EKN, US-EKP, US-Hn3, US-TLR | 2: IT-MBo, US-HB4 |
| ERA5/BADM | 631 | 150 (144 no BADM value, 5 BADM==0, 1 `era5_map_mm==0`) | 10: US-A39, US-CF1, US-CF2, US-CF3, US-CF4, US-EKH, US-EKN, US-EKP, US-Hn3, US-TLR | 2: JP-KaP, US-HB4 |
| ERA5/measured | 469 | 312 (no complete measured year; this set already includes `CA-TP2`) | 0 | 1: US-HB4 |

("Sites excluded from panel" is unchanged from the prior version — these
are the sites removed before plotting at all, e.g. `era5_map_mm == 0`. The
`<0.1x`/`>20x` columns are new: sites that ARE plotted, but land in the
marked overflow bin because their ratio falls outside the 0.1x-20x core
range.)

Files: `fig1_ratio_histograms_versionA.png` (no grouping, 3000x1200px),
`fig1_ratio_histograms_versionB.png` (123 4x/8x-cluster sites in a second
colour, shared legend below all panels, membership rule verbatim in the
legend text and in `fig1_ratio_histograms_versionB.legend.txt`,
3000x1410px).

## Figure 2 (overwritten: 3 panels -> 6 panels; then reformatted)

Six log-log scatter panels. **One common axis range shared across all six
panels: 10 to 10,000 mm/yr**, chosen from the bulk of the pooled data (not
the extremes) — solid 1:1 line; dashed = 4x/0.25x offset; dotted =
8x/0.125x offset. y = first-named variable, x = second-named, matching
Figure 1's ERA5/reference direction where ERA5 is involved. Points with
either coordinate outside 10-10,000 mm/yr are not dropped: they are drawn
clamped to the panel edge as a triangle (circles elsewhere), and are named
here with their true, unclamped (x, y) values in mm/yr.

| Panel | Comparison | n | Spearman rho | p | Excluded from panel | Off-scale (clamped, named) |
|---|---|---|---|---|---|---|
| (a) | ERA5 vs BADM | 631 | 0.578 | < 0.001 | 150 (144 no BADM, 5 BADM==0, 1 era5==0) | 4: IT-MBo (1365.0, 24150.4), US-CF1 (550.0, 9.0), US-HB4 (1429.0, 658044.6), US-TLR (469.4, 1.0) |
| (b) | ERA5 vs BIO12 | 780 | 0.592 | < 0.001 | 1 (era5==0, `CA-TP2`) | 5: IT-MBo (407, 24150.4), PE-QFR (2705, 10291.8), US-CF1 (611, 9.0), US-HB4 (1350, 658044.6), US-TLR (1276, 1.0) |
| (c) | ERA5 vs measured | 469 | 0.975 | < 0.001 | 312 (no complete measured year) | 2: IT-MBo (1341.8, 24150.4), US-HB4 (9581.3, 658044.6) |
| (d) | BADM vs BIO12 | 632 | 0.933 | < 0.001 | 149 (144 no BADM, 5 BADM==0) | none |
| (e) | BADM vs measured | 375 | 0.900 | < 0.001 | 406 (149 BADM missing/==0 union 312 no complete measured year, 55 in both) | none |
| (f) | BIO12 vs measured | 469 | 0.869 | < 0.001 | 312 (no complete measured year) | none |

("Excluded from panel" is unchanged from the prior version — sites removed
before plotting at all. "Off-scale" is new: sites that ARE plotted, at the
clamped edge position, because their true value lies outside the shared
10-10,000 mm/yr range.)

Files: `fig2_scatter_versionA.png` (no grouping, 3000x2700px),
`fig2_scatter_versionB.png` (123 4x/8x-cluster sites in a second colour,
drawn on top of the rest; shared legend below all panels; 3000x2940px).
The shared legend (collected once via patchwork, not repeated per panel)
and `fig2_scatter_versionB.legend.txt` state the membership rule verbatim:

> Membership rule (verbatim, table_b1_factor_estimates.csv):
> factor_estimate = median(ratio_to_bio12, ratio_to_badm_map);
> assigned to nearest of candidate factors {1, 4, 8, 24, 1000}
> if within 15% of it (tol = 0.15), else 'elsewhere'.
> Shown = sites with nearest_cluster %in% c('4','8')
> (n = 123: 113 near 4x + 10 near 8x)

## Figure 3 — month-matched ratio (NEW figure; reformatted along with 1 & 2)

One histogram panel: per-site median of (that month's `P_ERA`) / (that
month's `P_F`), over every site-month with `P_F_QC >= 0.9` and `P_F != 0`.
No annualisation and no complete-year requirement — every qualifying month
is used directly. Same core range (0.1x-20x), bin width (0.1 log10 units),
overflow-bin treatment, and reference lines (1x, 4x, 8x) as Figure 1.

- **n = 606 sites.** Excludes 175 sites with zero valid matched months
  (no month reaching the 0.9 cutoff, or every such month having `P_F ==
  0`).
- Median matched months per site (among the 606 plotted) = 52.
- `<0.1x` overflow bin: 0 sites. `>20x` overflow bin: 3 sites — `IT-MBo`,
  `US-HB4`, `US-KS3` (the same `US-HB4` that is the extreme site in Figure
  1 and Figure 2).

Files: `fig3_month_matched_ratio_versionA.png` (no grouping, 2100x1500px),
`fig3_month_matched_ratio_versionB.png` (123 4x/8x-cluster sites in a
second colour, legend below the panel, widened slightly to 8in so the
footnote fits; same verbatim membership rule as Figure 2 and in
`fig3_month_matched_ratio_versionB.legend.txt`, 2400x1740px).

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

This entry covers both updates made today: adding the tower-measured
estimate (2->3 / 3->6 panels, new companion columns) and then the pure
legibility reformat (axis ranges, legends, sizing) described above. Final
pixel dimensions and file sizes are from the reformatted PNGs actually on
disk.

| File | Status | Dimensions | Size |
|---|---|---|---|
| `fig1_ratio_histograms_versionA.png` (+ `.meta.json`) | **Replaced** — 2 panels -> 3 panels, then reformatted | 3000x1200px | 139 KB |
| `fig1_ratio_histograms_versionB.png` (+ `.meta.json`, `.legend.txt`) | **Replaced** — 2 panels -> 3 panels, then reformatted | 3000x1410px | 165 KB |
| `fig2_scatter_versionA.png` (+ `.meta.json`) | **Replaced** — 3 panels -> 6 panels, then reformatted | 3000x2700px | 834 KB |
| `fig2_scatter_versionB.png` (+ `.meta.json`, `.legend.txt`) | **Replaced** — 3 panels -> 6 panels, then reformatted | 3000x2940px | 848 KB |
| `fig3_month_matched_ratio_versionA.png` (+ `.meta.json`) | **New**, then reformatted | 2100x1500px | 90 KB |
| `fig3_month_matched_ratio_versionB.png` (+ `.meta.json`, `.legend.txt`) | **New**, then reformatted | 2400x1740px | 120 KB |
| `fig1_ratio_histograms_versionB.legend.txt` | **New** (this reformat) | — | 320 B |
| `fig2_scatter_versionB.legend.txt` | **New** (this reformat) | — | 320 B |
| `fig3_month_matched_ratio_versionB.legend.txt` | **New** (this reformat) | — | 320 B |
| `table_site_reference_comparison.csv` (+ `.meta.json`) | **Replaced** — 9 columns -> 14 columns; values unchanged by the reformat | — | — |
| `table_panel_exclusions.csv` (+ `.meta.json`) | **Replaced** — extended to the new panels; unchanged by the reformat | — | — |
| `report.md` | **Replaced** (this file) | — | — |
| `scripts/diagnostics/era5_reference_plots.R` | **Replaced in place** — same script file, extended, then reformatted | — | — |

All CSV and PNG outputs have a companion `.meta.json` (run datetime, git
commit, input sources, notes) per repository convention; each version-B
figure additionally has a `.legend.txt` with the verbatim membership rule.
Every PNG listed above was opened and visually checked after the reformat
for clipping, overlapping text, and that data fills the panel.
