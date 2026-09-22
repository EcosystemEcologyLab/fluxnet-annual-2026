# Precipitation downscaling provenance: a basis for treating unusable P_ERA

Provenance-based basis for deciding how to treat sites whose P_ERA is unusable for the
site-side Koppen classification. **This analysis does not decide inclusion or exclusion.**
`KG_ERA5_MAP_MAX_MM` (the current 5,000 mm/yr year-level screen in `R/pipeline_config.R`) is
never read, referenced, or reused in any form anywhere in this script or report.

Read-only: `R/pipeline_config.R`, every pipeline script (01-07), and every already-committed
figure are untouched (verified: `git status` before/after this script's run differs only by new
files). New code: `scripts/diagnostics/precip_downscaling_provenance.R`. Network-wide, all 781
current sites.

**Terminology note**: the task's background calls this metadata "AUXMETEO". The literal BIF
variable group name in this repo's extracted files is `GRP_ERA_DOWN`, not `GRP_AUXMETEO` or
similar -- used throughout below, flagged here rather than silently substituted.

## What this analysis cannot decide (stated up front)

1. Whether a site's P_ERA **should** be excluded, downweighted, or substituted for the paper's
   actual Koppen classification -- only what the BIF-recorded downscaling provenance and a
   substitution test show, not which policy to adopt.
2. What ERA_SLOPE, ERA_INTERCEPT, ERA_RMSE, and ERA_CORRELATION actually mean or in which
   direction they were fit. **This is not documented anywhere in this repository.** Every
   reading of these fields below -- including the provisional correlation with measured-year
   count -- is stated as provisional, not confirmed by any methods document found here.
3. Why the weighted Jaccard metric gets *slightly worse*, not better, under the Beck2023
   substitution (section 5) despite moving 228 sites' classes onto Beck's own product --
   reported as an observation, not diagnosed further.
4. Koppen classification for the 14 current-network sites `site_koppen_beck2023.csv` has no row
   for (section 5) -- this analysis cannot resolve them via this substitution method.
5. Anything about the ~123-site 4x/8x cluster/factor question or the it_mbo/US-HB4 single-site
   parismony question -- out of scope here, as in `precip_site_filter/report.md`.
6. Whether a numeric log-ratio threshold should be adopted. Section 4's distributions separate
   the two provenance groups with very little overlap, but the cleanest signal this analysis
   finds is not a ratio threshold at all -- it is the binary GRP_ERA_DOWN group membership
   itself (section 3). No numeric threshold is proposed.

## 1. Data sources

- **GRP_ERA_DOWN rows**: freshly grepped from each of the 781 currently-canonical BIF files
  (`fluxnet::flux_discover_files()`-deduplicated, same site list `precip_site_filter.R` used),
  not from any cached/stale extraction.
- **`p_era_mean_mm_1981_2025`, `bio12_mm`, `badm_map_mm`, `p_measured_mean_mm`,
  `n_years_measured`, `log_ratio_era_bio12`, etc.**: joined unmodified from
  `review/diagnostics/precip_site_filter/table_1_site_level_precip_estimates.csv` (the
  2026-09-22 window-labelled version), by `site_id`, using `p_era_mean_mm_1981_2025` as
  instructed.
- **Current Koppen classification**: `data/snapshots/site_koppen_era5.csv` (2026-09-20, 781
  sites, ERA5-normals-derived, current pipeline default `KG_ERA5_MAP_MAX_MM=5000` already baked
  in as-is -- not modified, not recomputed with the screen removed).
- **Beck2023 tower-cell classification**: `data/snapshots/site_koppen_beck2023.csv` (dated
  2026-06-24, 767 sites) -- **stale relative to the current 781-site network**, see section 5.
- **Beck2023 global land-area distribution / legend**: `data/snapshots/koppen_beck2023_global_distribution.csv`
  and `data/external/koppen_beck2023/legend.txt`, the same inputs
  `scripts/figure_representativeness_kg.R` uses (that script's `compute_repr_metrics()` formula
  is reused here, not modified, not re-derived).

## 2. Step 1: characterising GRP_ERA_DOWN before using it

Every one of the 781 canonical sites has exactly 8 `GRP_ERA_DOWN` rows in its BIF file, one per
`ERA_VARIABLE` (`TA, PA, VPD, WS, P, SW_IN, LW_IN, LW_IN_JSB`) -- 31,240 rows total, confirmed
**one row per site per variable, never per year** (no recalibration-year repetition; every
site's row count is exactly 8). No site is missing the block entirely, and no site is missing a
`P` row specifically -- there is no "no AUXMETEO record" site for `P` in this network.

**The task's stated "not fitted" signature (slope 1, other three = -9999) is real but
incomplete.** Two, and only two, value patterns exist for `ERA_VARIABLE = P` across all 781
sites:

| Pattern | ERA_SLOPE | ERA_INTERCEPT/RMSE/CORRELATION | n sites |
|---|---|---|---:|
| Matches the task's stated signature | 1.0 | all -9999 | 609 |
| A second, distinct sentinel pattern **not** in the stated signature | -9999.0 | all -9999 | 172 |
| Genuinely fitted (any real, non -9999 stat) | -- | -- | **0** |

**Zero of 781 sites have a genuinely fitted P regression.** This is not a partial or ambiguous
signal that stops the analysis -- it is a single, clean, network-wide fact, fully characterised
here -- but it means the task's three-way "not fitted / fitted with statistics / no AUXMETEO
record" classification has an **empty "fitted" bucket and an empty "no record" bucket** for `P`
specifically. This is reported explicitly below, not engineered around.

**Reference check, to confirm this is P-specific and not just how every BIF export looks**: `TA`
(the task's own worked example) has real, non -9999 `ERA_RMSE` at all 781/781 sites (slope also
always 1.0, but with genuinely fitted intercept/RMSE/correlation, unlike P). Precipitation is the
one variable in this block that is, network-wide, **never** bias-corrected against tower
measurements -- every site's P_ERA is either flagged not-fitted with a default identity slope,
or has no recorded slope at all.

`table_1_auxmeteo_structure.csv` / `table_1b_p_variable_patterns.csv` carry the full data behind
this section.

## 3. Step 2: site classification

Four categories, per the task's scheme, with the two empty ones stated rather than dropped:

| Group | n sites | Meaning |
|---|---:|---|
| `not_fitted_slope1` | 609 | ERA_SLOPE=1 (default identity), no real fit stats |
| `not_fitted_slope_9999` | 172 | ERA_SLOPE also sentinel -- no slope recorded at all |
| `fitted` | **0** | No site has a genuinely fitted P regression |
| `no_auxmeteo_record` | **0** | Every site has a P row in GRP_ERA_DOWN |

**Every one of the 781 sites is therefore "not fitted"** for P, split only by which of the two
sentinel flavors its BIF row carries.

**Provisional correlate** (stated as provisional -- ERA_SLOPE's meaning is undocumented,
instruction 2 above): cross-tabulated against `table_1`'s `n_years_measured` (years with
`P_F_QC > QC_THRESHOLD_YY`):

| p_group | has >=1 QC-measured precip year | n |
|---|---|---:|
| `not_fitted_slope1` | FALSE | 22 |
| `not_fitted_slope1` | TRUE | 587 |
| `not_fitted_slope_9999` | FALSE | **172 / 172** |

Every `not_fitted_slope_9999` site has **zero** QC-measured precipitation years -- a perfect
correspondence in that direction. It is not a perfect correspondence in the other direction: 22
`not_fitted_slope1` sites also have zero QC-measured years yet still carry the slope=1 default.
Read as suggestive, not as a confirmed mechanism.

`table_2_site_groups.csv` (full per-site join) and `table_2b_group_vs_measured_years_crosstab.csv`
carry this section's data.

## 4. Step 3: figures

1. **`fig_1_pairwise_comparison_by_group.png`** -- `precip_site_filter/fig_1_pairwise_comparison.png`
   redrawn unchanged except points coloured by `p_group` (orange = `not_fitted_slope_9999`, blue
   = `not_fitted_slope1`). **This directly identifies the systematically-offset upper band**
   `precip_site_filter/report.md` section 3 described in the `P_ERA vs. BADM/BIO12` panels as
   "two parallel streaks": the upper streak is almost entirely `not_fitted_slope_9999` (orange),
   the main-cluster streak is `not_fitted_slope1` (blue). The `measured vs. *` and `P_ERA vs.
   measured` panels are almost entirely blue, consistent with section 3's finding that the
   orange group has essentially no QC-measured data to plot.
2. **`fig_2_slope_vs_ratio_SUBSTITUTE.png`** -- the requested "ERA_SLOPE vs. log10(P_ERA/BIO12),
   fitted sites only" **cannot be drawn as specified: 0 sites qualify** (section 2). A labelled
   substitute is provided instead, plotting all 781 sites by their raw (sentinel) ERA_SLOPE
   value. **No unit-gradient / inflation-factor reading of ERA_SLOPE is possible** -- there is no
   fitted subset to assess it on, and the substitute figure shows exactly two vertical clusters
   (x=1, x=-9999), not a gradient.
3. **`fig_3_ratio_distribution_by_group.png`** -- distribution of `log10(P_ERA/BIO12)` by group,
   shared axis, medians and counts annotated. `not_fitted_slope1` (n=609): median -0.026 (~0.94x,
   near parity with BIO12). `not_fitted_slope_9999` (n=172): median **+0.595 (~3.94x)**, a narrow,
   tightly-clustered, systematically-inflated distribution with almost no overlap with the other
   group's central mass. This is the single cleanest signal in this analysis.
4. **`fig_4_stats_vs_measured_years.png`** -- ERA_SLOPE / ERA_RMSE / ERA_CORRELATION each against
   `n_years_measured` (NA recoded to 0, noted on the axis). **ERA_RMSE and ERA_CORRELATION panels
   are necessarily flat** (-9999 for every site, per section 2) -- this is the correct result of
   that fact, not a plotting error. The ERA_SLOPE panel shows the same story as the crosstab in
   section 3: all `not_fitted_slope_9999` (orange) points sit at 0 measured years; the
   `not_fitted_slope1` (blue) points span 0-~29.

## 5. Step 4: substitution test

For every site the grouping flags (both provenance groups -- `fitted` and `no_auxmeteo_record`
are empty, so this is all 781 sites, 100%), `koppen_class` from `site_koppen_era5.csv`
(ERA5-normals-derived, current pipeline default) is replaced with the Beck2023 tower-cell class
from `site_koppen_beck2023.csv`, where available.

**Staleness, flagged rather than worked around**: `site_koppen_beck2023.csv` is dated
2026-06-24 and covers 767 sites; the current canonical network (from `flux_discover_files()`,
same as `precip_site_filter.R` used) has 781. **14 current-network sites have no row in this
file** and cannot be resolved by this substitution: `ES-LgS, ES-Ln2, FI-Si2, HK-MPM, IT-Cpz,
IT-MtP, IT-Ro1, IT-Ro2, US-KLS, US-LS2, US-ZF1, US-xHA, US-xKA, US-xTA`. These 14 are left at
their current ERA5-derived class (`unresolved_no_beck = TRUE` in `table_3_substitution_test.csv`),
not dropped and not silently assumed unchanged for any other reason than "cannot be substituted."
767 of 781 flagged sites were actually substitutable.

**Sites whose KG class changes under substitution: 228 / 767 substituted (29.7%)** -- listed in
full in `table_3_substitution_test.csv` (`class_changed` column). This stands in sharp contrast
to `precip_site_filter/report.md` section 5's finding that **zero** sites changed class when the
5,000 mm/yr year-level screen was simply removed: that was a same-source (ERA5-normals),
threshold-removal test; this is a different-source (Beck2023 tower-cell) substitution test, and
the two are not comparable in magnitude for that reason -- the contrast itself is the point, not
a contradiction.

**Weighted Jaccard (30-class KG axis, same formula as `scripts/figure_representativeness_kg.R`'s
`compute_repr_metrics()`, reused not modified)**:

| Variant | Weighted Jaccard |
|---|---:|
| Current (ERA5-normals-derived, all 781 sites) | 0.3511 |
| Substituted (Beck2023 tower-cell for all 767 resolvable flagged sites) | 0.3452 |
| Change | -0.0059 |

**The substitution makes the network's aggregate distribution slightly *less* similar to Beck's
own global land-area distribution**, despite moving 228 individual sites' classes onto Beck's own
product. This is reported as-is (see "cannot decide" item 3) -- not diagnosed further; a
plausible non-mechanistic explanation is that the network's own geographic sampling pattern, not
the per-site class source, dominates this aggregate metric, but that is not verified here.

`table_3_substitution_test.csv` and `table_4_weighted_jaccard.csv` carry this section's data.

## 6. WorldClim monthly normals (not used)

The alternative substitution source named in the task -- WorldClim monthly climate normals --
would require a download: `data/external/worldclim/` currently holds only the 19 BIO variables
at 2.5 arc-min (`wc2.1_2.5m_bio.zip`, `climate/`), not the monthly normal layers Koppen
classification would need. Not downloaded or attempted here.

## 7. Files in this directory

| File | Contents |
|---|---|
| `table_1_auxmeteo_structure.csv` | One row per site x ERA_VARIABLE (8 x 781), raw GRP_ERA_DOWN values |
| `table_1b_p_variable_patterns.csv` | The 2 (of a possible many) value-pattern combinations observed for P, with counts |
| `table_2_site_groups.csv` | precip_site_filter's table_1, joined to this script's p_group + raw P downscaling stats |
| `table_2b_group_vs_measured_years_crosstab.csv` | p_group x whether the site has any QC-measured precip year (provisional) |
| `table_3_substitution_test.csv` | Per-site current vs. Beck2023-substituted KG class, which sites change, which are unresolved |
| `table_4_weighted_jaccard.csv` | Weighted Jaccard (30-class KG axis), current vs. substituted |
| `fig_1_pairwise_comparison_by_group.png` | precip_site_filter's fig_1, coloured by P downscaling group |
| `fig_2_slope_vs_ratio_SUBSTITUTE.png` | Substitute for the (empty) fitted-sites-only ERA_SLOPE vs. ratio figure |
| `fig_3_ratio_distribution_by_group.png` | log10(P_ERA/BIO12) distribution by group -- the cleanest signal here |
| `fig_4_stats_vs_measured_years.png` | ERA_SLOPE/RMSE/CORRELATION against measured-year count |
