# GSL figure rebuild — Stage 3 report back (2026-10-29)

Framework run-order step 6 (figures + benchmark table). Full prompt:
`logs/prompts/2026-10-29_gsl_stage3.md`. Main run throughout unless noted: `GPP_NT_VUT_REF`,
negative GPP set to 0, year screening d80, GSL counts `GPP > T` (strict). Scripts:
`code/05_fig1.R`–`08_fig4.R`, `09_benchmark.R`, `10_supplementary.R`. All checks in the prompt
passed — see "Checks" below.

## Figure 1 — mechanism at two representative sites

Sites (nearest the median skewness of the low and high thirds, never AU-ASM):
- Low third: **JP-Fjy**, skewness = 0.5123, T95 = 15.4% of P99.
- High third: **CA-Ca2**, skewness = 1.3043, T95 = 6.4% of P99.

Change in mean GSL from 5% to 10% of P99 (negative = fewer days qualify at the higher
threshold): JP-Fjy = **-26.47 days**, CA-Ca2 = **-53.43 days**. The high-skew site's GSL is
roughly twice as sensitive to this one threshold step as the low-skew site's, consistent with
the framework's takeaway that GSL changes fastest where days pile up near the threshold.

## Figure 2 — where sensitivity sits across all 62 sites

62 of 62 sites shown, skewness order. Terciles at the published boundaries (0.63, 0.88):
**low = 21, middle = 20, high = 21** (matches the framework's stated counts exactly). The
heat map and the per-third median-sensitivity line (panel d) both show sensitivity
concentrating at the low end of the threshold range and increasing with skewness, as the
framework's takeaway states.

## Figure 3 — consequences for GSL trends vs. sampling uncertainty

**21 trend sites** (>=10 years after d80 screening) — unchanged from the framework's 21, since
Stage 1 showed d80 screening keeps all 505 site-years. Threshold range: 5-20% of P99 (default).

Of the 21 trend sites, across that range:
- **6** have a sign change in the GSL trend.
- **6** have a significance change (p<0.05 threshold).
- **11** have either.

Just over half the trend sites (11/21) would reach a different qualitative conclusion about
their GSL trend — sign or significance — purely from where the threshold is set within the
commonly-used 5-20% of P99 range.

## Figure 4 — the fraction-of-annual-GPP method and its own sensitivity

Median common-unit sensitivity (days of GSL per 1% change in included GPP fraction), across all
62 sites: **Z17 (10% of P99) = 11.085**, **T95 (F=0.95) = 8.290**. The new method is, on median,
about 25% less sensitive in these common units than the fixed Z17 threshold.

## Benchmark table

For the 21 trend sites, correlation (Pearson) of annual GSL with annual GPP across each site's
own years:

| Group | n sites | Median r, Z17 | Median r, new method (T95) |
|---|---|---|---|
| deciduous forest | 3 | 0.419 | 0.348 |
| evergreen forest | 7 | 0.642 | 0.766 |
| grassland | 6 | 0.548 | 0.567 |
| woody-grass mosaic | 5 | 0.820 | 0.764 |
| **All 21 sites** | 21 | **0.612** | **0.683** |

Overall, the new (T95) method has a modestly higher median correlation with annual GPP than
the fixed Z17 threshold (0.683 vs. 0.612) — by this measure it explains interannual GPP
variation slightly better, network-wide. By functional group the picture is mixed: the new
method does better for evergreen forest, worse for deciduous forest and woody-grass mosaic, and
is similar for grassland. Neither definition is favoured by construction (both count days above
a fixed threshold), per the framework's own note.

## Supplementary figures

Figures 2 and 3 repeated with (i) `GPP_DT_VUT_REF` and (ii) negative GPP as-delivered (4
figures total: `fig_02_sensitivity_heatmap_supp_DT`, `_supp_asDelivered`,
`fig_03_trends_supp_DT`, `_supp_asDelivered`). Figure 3's sign/significance-change counts across
the 4 runs:

| Variant | Sign changes | Significance changes | Either |
|---|---|---|---|
| Main (NT, zero-floored) | 6 | 6 | 11 |
| DT, zero-floored | 6 | 7 | 13 |
| NT, as-delivered | 6 | 6 | 11 |

The sign-change count (6) is stable across all three variants checked; the significance-change
count is slightly higher under `GPP_DT_VUT_REF` (7 vs. 6) — a small difference, not tested
further here.

**Departure from the framework, noted explicitly**: the framework's original "Supplementary
outputs" section asked for Figures 2/3 repeated "with Tash's screening" as one of the two
variants; the Stage 3 prompt replaced this with "negative GPP as-delivered" instead, because
Tash's screening selects the same 505 site-years as the main d80-screened run (Stage 1/2) — a
separate "Tash screening" supplementary figure would be visually identical to the main one.

**Deferred, not built**: the current manuscript's own Figures 1-4, rebuilt from our run for
direct comparison (framework "Supplementary outputs," first bullet). The framework does not
describe their exact current form (axis ranges, specific panels, current threshold convention)
in enough detail to reproduce without the manuscript itself — attempting it would mean guessing
at a comparison target, which risks a misleading "comparison." Flagged for Dave/Tash: send the
manuscript's current Figures 1-4 (or close descriptions) if this comparison is still wanted.

## Checks performed

- Every figure inspected at full size. Two real rendering bugs were found and fixed before any
  figure was accepted: `geom_raster()` silently rendered Figure 2's heat map as near-blank under
  a log-x scale (fixed: `geom_tile()` with an explicit untransformed width); `geom_density()`'s
  default legend key glyph collided with `geom_line()`'s glyph in Figure 1 panel b (fixed:
  `key_glyph = "path"`). All in-panel explanatory text annotations were removed, per
  `R/nature_format.R`'s own documented rule that captions belong in `.legend.txt`, not drawn on
  the figure.
- Every number quoted above was cross-checked by an independent read of the underlying table
  (`derived_site.csv`, `benchmark_group_medians.csv`) against the `.legend.txt`/`.meta.json`
  text — all matched exactly (to the decimal places shown).
- Figure 2 confirmed to have all 62 sites, skewness order (`stopifnot` in `06_fig2.R`).
- Figure 3's trend-site count confirmed to match the screening table (`stopifnot(length(...)
  == 21L)` in `07_fig3.R`, reusing `GSL_SITES_GE10YR` from `00_config.R`, itself checked against
  the framework's stated 21 in `00_config.R`).

## What remains

- **Check 3** (the three Figure 1 correlations, r=0.63/0.49/0.34): still not checkable — no
  definition of what the three values correlate was ever supplied, in the framework or either
  stage-2/3 prompt.
- **The deferred current-manuscript Figures 1-4 rebuild**, pending the manuscript itself.
- **Tash's sensitivity site sets** (30 and 188 sites) and her other requested materials
  (snapshot, screened daily table/code, negative-GPP and `>`/`>=` answers) — still not received;
  Stage 2/3's decisions were made without them, by design, and remain subject to revision.
