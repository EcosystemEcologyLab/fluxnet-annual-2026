# Site-measured NEE vs. TRENDY-at-site: is the low Row E′ Jaccard an artifact?

**Type:** Read-and-report diagnostic investigation + new diagnostic code, for
a co-author decision. No existing script, figure, legend, snapshot CSV, or
`representativeness_metrics.csv` was modified. New code:
`scripts/diagnostics/nee_et_site_vs_trendy_core.R` (T1 percentile/ratio, T2,
T5, T6, T7) and `scripts/diagnostics/nee_et_site_vs_trendy_raster.R` (T1
raw-stack re-derivation, T3, T4). Outputs and run logs in this directory.

Throughout, "TRENDY-at-site" means the site-extracted `nee_median`/`et_median`
value (`site_trendy_nee_median.csv` / `site_trendy_et_median.csv`) — the
"mean |flux|" statistic Row E′/F′ of `Supp_compare_geospatial_vs_sitelevel_grid.png`
are actually built against — not the NEE-IAV axis used in that figure's
Row E, which is a different physical quantity (interannual variability, not
magnitude) and not what the co-author's question is about.

---

## Verdict

**No unit-conversion artifact is present — that is definitively ruled out
(T1: exact match, correlation = 1.000, zero difference, at every one of 34
years, globally). The low Row E′ Jaccard is real, and is explained by two
compounding, well-quantified effects: a statistical-definition mismatch
(measured NEP uses the median of signed annual values; TRENDY's `nee_median`
uses the mean of absolute annual values) and a genuine scale/cancellation
effect (point-tower vs. 2,500 km² grid-cell averaging), with the temporal
half of that cancellation (T3) far larger than the spatial half.** ET, which
uses the analogous but sign-unproblematic `mean(annual)` (no `abs()`, since
ET can't be negative), shows essentially none of this and validates the
method as sound when the underlying quantity doesn't oscillate in sign.

Sizes of each effect, in the figure's own units (gC m⁻² yr⁻¹ for NEE, mm
yr⁻¹ for ET):

- **T1 (unit/conversion):** ruled out. Median measured/TRENDY ratio is 4.07×
  for NEE, not near any screened conversion factor (12/30/365/1000/1:12);
  0.98× for ET (no issue). Raw-monthly-stack re-derivation matched the
  stored intermediate exactly once a genuine bug in the *diagnostic script
  itself* was found and fixed (see below) — not a pipeline bug.
- **T2 (bin saturation):** 73.6% of measured NEE sites fall in the single
  open-ended top bin (>70.2 gC m⁻² yr⁻¹) vs. 9.0% of TRENDY-at-site values —
  most of Row E′'s low J is a small number of bins doing all the work. ET:
  7.0% vs. 5.6% — negligible.
- **T3 (scale/cancellation):** spatial coarsening alone (0.5°→4°, a 64×
  area increase) reduces global mean |NBP| by only ~22% (35.4→27.6 gC m⁻²
  yr⁻¹) — a real but modest effect. Far larger: **temporal cancellation
  within a single 0.5° pixel across 34 years is ~4× (unweighted global
  median; the area-weighted mean is not usable here — see caveat below) —
  the ensemble's own long-run mean NBP hovers near zero at most pixels, so
  `mean(|annual|)` is inflated ~4× relative to `|mean(annual)|` by
  interannual sign-cancellation alone.** Ensemble order also matters
  substantially: computing the ensemble median of models' annual values
  *first*, then `mean(|·|)`, gives 19.6 gC m⁻² yr⁻¹ — barely half the
  stored method's 35.4. ET's equivalent cancellation ratio is 0.997 —
  essentially none, as expected for a strictly non-negative flux.
- **T4 (temporal window):** ruled out as a contributor. Recomputing the
  TRENDY statistic using only each site's own QC-qualifying measured years
  instead of the full 1990–2023 window changes the value by a median of
  +0.22 gC m⁻² yr⁻¹ (NEE, n=600) / +5.4 mm yr⁻¹ (ET, n=599) — negligible
  relative to the ~130 gC m⁻² yr⁻¹ median measured-minus-TRENDY gap.
- **T5 (management proxy):** the *opposite* of the naive hypothesis.
  Unmanaged forest classes show a **larger** median measured-minus-TRENDY
  gap (229.7 gC m⁻² yr⁻¹) than managed CRO+GRA+BADM-flagged-WET (117.8),
  Wilcoxon p=8.3×10⁻⁶. This is consistent with the T3 cancellation story:
  forests are exactly the class most likely to sit in a 0.5° cell whose
  long-run mean is genuinely near zero (heterogeneous stand ages/management
  histories averaging out), maximizing the cancellation-driven gap;
  croplands, with more spatially homogeneous, consistently-signed
  management-driven flux, cancel less.
- **T6 (siting bias):** a small, statistically detectable but practically
  minor effect. Spearman ρ=0.164 (p=3×10⁻⁵) between a site's biomass
  percentile within its 0.5° cell and its measured-minus-TRENDY gap for
  NEE; ρ=0.047 (p=0.23, not significant) for ET. Sites generally sit at
  the ~63rd percentile of their cell's biomass distribution (both axes) —
  towers are somewhat biased toward higher-biomass locations within their
  grid cell, as expected, but this explains only a small fraction of the
  NEE gap.

---

## One bug caught and fixed during this investigation, reported for the record

The first attempt at T1's raw-monthly-stack re-derivation (for CABLE-POP,
the representative monthly-native model) used `terra::time()` to label each
monthly layer's calendar year. This produced a **global correlation of
~0.07 against the stored intermediate for nbp**, while the identical code
path for evapotrans gave 0.98 — a red flag investigated rather than
shrugged off. Root cause, found by reading
`scripts/figure_representativeness_trendy_compute.R:118-122` directly:
that script explicitly documents and works around a `terra::time()` bug —
*"terra::time() misparses pre-1678 CF origins... using the Unix epoch 1970
as fallback, producing a systematic +270 year offset"* — and instead
computes years from a verified per-model `MODEL_START_YR` table
(`:91-93`; CABLE-POP's true start year is **1700**, not 1970). The
diagnostic script's first draft trusted `terra::time()` naively, silently
selecting a ~1720-1753 block of data while labeling it "1990-2023." Fixed
to replicate `get_years()`'s exact logic; re-run gave an exact match
(correlation = 1.000000, max abs diff = 0.0 at every year, both variables).
**This was a bug in the new diagnostic code, not in the pipeline** — the
pipeline's own handling of this is correct and already documented. Reported
here in full because it is exactly the kind of silent, plausible-looking
error this task exists to catch.

---

## T1 — Unit and conversion screen

| | NEE (measured `|NEP|` vs. TRENDY `|NBP|`) | ET (measured vs. TRENDY) |
|---|---|---|
| Measured P5/P25/P50/P75/P95 | 13.5 / 63.4 / 174.5 / 366.7 / 753.6 gC m⁻² yr⁻¹ | 190.0 / 373.2 / 509.9 / 670.2 / 1028.1 mm yr⁻¹ |
| TRENDY-at-site P5/P25/P50/P75/P95 | 19.6 / 36.0 / 46.5 / 58.5 / 82.0 gC m⁻² yr⁻¹ | 217.0 / 370.7 / 480.2 / 656.3 / 987.6 mm yr⁻¹ |
| Per-site ratio (measured/TRENDY): median | **4.07×** | 0.98× |
| Ratio IQR | [1.40, 7.20] | [0.83, 1.18] |
| Nearest screened conversion factor (12/30/365/1000/1:12) | 12 (log-distance 1.08 — **not close**) | 12 (log-distance 2.47 — not close) |
| Raw-monthly-stack re-derivation (CABLE-POP, full 1990-2023, global) | **Exact match**, r=1.000000, 0/60,271 cells differ, every year | **Exact match**, r=1.000000, 0/60,271 cells differ, every year |

**A conversion error is ruled out.** The median ratio (4.07×) is not close
to any of the screened slip factors on a log scale, and the independent
full re-derivation from raw monthly NetCDF data — after fixing the year-
indexing bug above — reproduces the stored intermediate GeoTIFF bit-for-bit
across the entire global 0.5° grid, all 34 years, both variables.

## T2 — Bin coverage

7-bin edges are stored in `trendy_nee_median_global_distribution.csv` /
`trendy_et_median_global_distribution.csv` (`min_value`/`max_value`
columns), unchanged.

| Bin (NEE, gC m⁻² yr⁻¹) | Measured frac. | TRENDY-at-site frac. | Bin (ET, mm yr⁻¹) | Measured frac. | TRENDY-at-site frac. |
|---|---|---|---|---|---|
| 1: 0–5 | 1.1% | 0.9% | 1: 0–5 | 0.0% | 0.0% |
| 2: 5.0–20.5 | 6.6% | 4.7% | 2: 5.0–143.4 | 3.7% | 2.4% |
| 3: 20.5–31.7 | 4.7% | 13.5% | 3: 143.4–263.9 | 8.4% | 6.3% |
| 4: 31.7–41.8 | 4.2% | 19.3% | 4: 263.9–394.0 | 16.9% | 23.8% |
| 5: 41.8–54.3 | 4.4% | 28.6% | 5: 394.0–634.7 | 40.7% | 39.5% |
| 6: 54.3–70.2 | 5.3% | 23.9% | 6: 634.7–968.6 | 23.3% | 22.4% |
| **7: >70.2 (open top bin)** | **73.6%** | **9.0%** | **7: >968.6** | **7.0%** | **5.6%** |
| n | 636 | 636 | 656 | 656 |

**Most of Row E′'s low J is bin saturation, not a shift in distribution
shape.** Nearly three-quarters of measured NEE sites fall in a single
open-ended bin designed (via the TRENDY distribution's own quantile
construction) to hold only ~13% of *global land area*; TRENDY-at-site
values populate that same bin at a rate close to its intended share. ET's
top-bin rates (7.0% vs. 5.6%) are close to each other and to the bin's
intended ~13% share of land area — no comparable saturation.

## T3 — Scale and cancellation

| | NEE (nbp) | ET (evapotrans) |
|---|---|---|
| Global area-weighted mean, 0.5° | 35.36 gC m⁻² yr⁻¹ | 457.2 mm yr⁻¹ |
| Global area-weighted mean, 1° | 33.20 | 474.9 |
| Global area-weighted mean, 2° | 30.77 | 507.0 |
| Global area-weighted mean, 4° | 27.65 | 558.6 |
| Change, 0.5°→4° (64× area) | **−21.8%** | +22.2% |
| mean(\|annual\|) / \|mean(annual)\|, native 0.5°, global median | **4.00** | (n/a — et is never negative) |
| Same ratio, area-weighted global mean | 2.4×10¹⁰ (**unreliable — see caveat**) | — |
| Same ratio, at NEE/ET site pixels, median [IQR] | **3.13 [1.89, 6.43]** | 0.997 (essentially 1) |
| Order of ensemble step: model-stat-first→median (stored method) | 35.36 | 457.15 |
| Order of ensemble step: median-across-models-first→stat | **19.57 (barely half)** | 457.19 (matches to within 0.01% — no order sensitivity) |

**Caveat on the area-weighted cancellation-ratio mean:** `mean(|annual|)`
and `|mean(annual)|` are both legitimately ≈0 at many pixels where the
long-run ensemble-median NBP is near carbon balance; dividing produces a
small number of enormous but *finite* ratios (418 pixels were exactly
non-finite and excluded; many more are merely very large) that make the
area-weighted mean meaningless (2.4×10¹⁰) despite being technically
computable. The **unweighted median (4.00 globally, 3.13 at the actual NEE
site pixels)** is the number that should be quoted and is robust to this —
reported plainly rather than hidden or silently substituted.

**Two real, compounding effects, temporal much larger than spatial.**
Spatial aggregation alone (blocking 0.5° cells up to 4°) reduces mean
|NBP| by only ~22% — real, but modest, and it's not even in a consistent
direction for ET (which *increases* with coarsening, likely a land-
composition/boundary-cell artifact of simple block-mean aggregation, not
itself a target of this investigation). Far more consequential: **within
a single, fixed 0.5° pixel, collapsing 34 years by mean(|annual|) instead
of |mean(annual)| inflates the value ~3-4×** at the same pixels the network
actually samples — this is the single largest quantified effect in the
whole investigation, and it is a **direct consequence of the `nee_median`
axis's own "mean absolute flux" definition**, not a resolution problem.
The ensemble-order finding compounds this: which stage you take the
across-model median at changes the answer by ~2×, confirming the current
method (per-model stat, then median) is itself just one of several
defensible choices, each producing materially different magnitudes.

## T4 — Temporal window

| | NEE | ET |
|---|---|---|
| Sites with ≥3 QC≥0.80 years in 1990-2023 | 600 / 636 | 599 / 656 |
| Median paired diff (measured-years stat − full-34yr stat) | **+0.22 gC m⁻² yr⁻¹** | **+5.42 mm yr⁻¹** |
| IQR of paired diff | [−4.05, +4.37] | [−5.90, +14.43] |

**Ruled out as a contributor.** "Measured years" is approximated here as
calendar years where DuckDB's `annual` table has `NEE_VUT_REF_QC >= 0.80`
or `NEE_CUT_REF_QC >= 0.80` — the same 0.80 threshold
`assess_flux_data_by_igbp_shuttle.R` uses, but not a byte-identical
reproduction of its per-site VUT/CUT fallback decision (stated explicitly,
not hidden). Even so, the effect size is two orders of magnitude smaller
than the ~130 gC m⁻² yr⁻¹ measured-minus-TRENDY gap and than T3's temporal-
cancellation effect — whichever years a tower happened to operate in
barely moves the TRENDY-side number.

## T5 — Management and definition proxy (opposite of the naive hypothesis)

BADM management coverage (`data/snapshots/badm_management_coverage.csv`)
covers the **767-site snapshot, not the current 781** — the 14 sites added
since (`ES-LgS, ES-Ln2, FI-Si2, HK-MPM, IT-Cpz, IT-MtP, IT-Ro1, IT-Ro2,
US-KLS, US-LS2, US-ZF1, US-xHA, US-xKA, US-xTA`) have no row at all, not
even NA — an explicit gap, not filled. No BADM field distinguishes managed
from natural wetlands specifically; `mgmt_drainage_wtd` (keyword-mined:
drain/water table/rewet/ditch) is the nearest available proxy and is used
here per the task's own framing ("WET if flagged as managed in BADM").

| IGBP class | n (NEE) | median diff, NEE | n (ET) | median diff, ET |
|---|---|---|---|---|
| EBF | 39 | 291.3 | 39 | −39.4 |
| DBF | 64 | 276.1 | 64 | −42.7 |
| MF | 22 | 236.4 | 22 | −64.2 |
| WSA | 15 | 215.0 | 17 | 45.0 |
| DNF | 6 | 209.6 | 6 | −99.5 |
| CRO | 108 | 164.0 | 110 | 39.9 |
| ENF | 95 | 157.4 | 102 | −23.9 |
| GRA | 126 | 83.2 | 129 | −15.3 |
| BSV | 7 | 72.8 | 7 | −8.7 |
| WET | 91 | 55.5 | 94 | 34.1 |
| OSH | 33 | 33.3 | 34 | −31.1 |
| CSH | 11 | 26.3 | 11 | 73.4 |
| SAV | 11 | 26.1 | 13 | 15.9 |
| CVM | 7 | 7.6 | 7 | −113.1 |
| SNO | 1 | −11.0 | 1 | −150.7 |

**Two-group test — managed (CRO+GRA+BADM-flagged-managed WET) vs.
unmanaged forest (ENF/EBF/DNF/DBF/MF):**

| | n | median diff | Wilcoxon W | p |
|---|---|---|---|---|
| NEE — managed | 234 | 117.8 | 20089 | **8.3×10⁻⁶** |
| NEE — unmanaged forest | 226 | **229.7** | | |
| ET — managed | 239 | 9.2 | 33891 | 4.5×10⁻⁵ |
| ET — unmanaged forest | 233 | −35.1 | | |

**The hypothesis (managed classes show a larger positive bias) is not
supported — the opposite pattern is what the data show, and it is
statistically decisive.** Unmanaged forest shows nearly double the median
gap of the managed group. This is coherent with T3: forest classes are
disproportionately likely to sit in a 0.5° TRENDY cell whose long-run mean
is close to zero (mixed stand ages, disturbance histories, and management
regimes within the cell canceling out), which is exactly the condition
that maximizes the `mean(|annual|)` vs. `|mean(annual)|` inflation found
in T3. As instructed, this is a **proxy, not a decomposition**: separating
harvest, fire, and land-use-change terms would require TRENDY variables
(`fFire`, `fHarvest`, `fLUC`) that were never downloaded for this repo
— confirmed absent from both `scripts/` references and the raw archive at
`data/external/trendy/v14-gcb2025/` (only `nbp`, `evapotrans`, `gpp`, `ra`,
`rh` were downloaded, per this session's investigation of
`figure_representativeness_trendy_compute.R`).

## T6 — Sub-grid siting bias

Biomass raster (0.01°) and the TRENDY 0.5° grid are confirmed perfectly
nested (50:1, same origin/CRS) — no resampling needed to compute each
site's percentile within its enclosing cell's 2,500 1 km pixels.

| | NEE sites | ET sites |
|---|---|---|
| n | 636 | 656 |
| Median biomass percentile within 0.5° cell | 62.7 | 62.8 |
| IQR | [36.1, 84.4] | [36.2, 84.2] |
| Spearman ρ (percentile vs. measured−TRENDY diff) | **0.164** | 0.047 |
| p | **3.2×10⁻⁵** | 0.226 (not significant) |

**A real but minor contributor for NEE, absent for ET (control).** Towers
sit, on average, above the median biomass of their surrounding grid cell
(as expected — sites are not randomly placed) and there is a weak,
statistically significant tendency for higher-biomass placement to
correlate with a larger measured-minus-TRENDY gap for NEE specifically —
consistent with towers being sited in relatively mature/productive stands
within a more heterogeneous 0.5° cell. The correlation is far too weak
(ρ=0.16) to be a primary driver next to T2/T3's effects.

## T7 — Paired figure

`fig_t7_measured_vs_trendy_paired.png` (this directory): two rows (NEE,
ET), each with a log-log scatter of measured vs. TRENDY-at-site (1:1 line,
OLS slope, median bias + IQR, points colored by IGBP) and marginal
distributions with the stored 7-bin edges overlaid.

- **NEE:** OLS slope (log-log) = 0.48 — points spread well off the 1:1
  line, systematically above it (median bias +132.0 gC m⁻² yr⁻¹, IQR
  [17.0, 301.0]). The bin-edge overlay sits entirely within the first ~5%
  of the visible measured-value range — visually confirming T2's bin-
  saturation finding.
- **ET:** OLS slope = 0.76, median bias −7.0 mm yr⁻¹, IQR [−81.1, 83.0] —
  scatter hugs the 1:1 line, only moderate spread, no systematic offset.
  Bin edges span the bulk of the distribution sensibly.

---

## For the co-author decision

**Column 1 (TRENDY-at-site vs. TRENDY-global, the original Fig 4/5 axis)
can claim:** internal consistency — both sides of the comparison are the
same model, same ensemble, same temporal-statistic convention
(`mean(|flux|)` for NEE). It answers "is the network's *modeled* carbon/
water flux representative of the *modeled* global distribution?" It
**cannot claim** to say anything about whether the network's actual
*measurements* are representative of anything, and (per T3) its own
`mean(|flux|)` convention is itself sensitive to an ensemble-order choice
that changes the answer ~2×.

**Column 2 (site-measured vs. TRENDY-global, the new Supp axis) can
claim:** it directly addresses what a reviewer will actually ask — "are
the towers' real measurements representative?" — using real data on one
side. It **cannot claim** apples-to-apples comparability with its own
global reference distribution, for two compounding, now-quantified
reasons: (a) a statistical-definition mismatch (median-of-signed vs.
mean-of-absolute, T1/T7), and (b) point-footprint vs. 2,500 km² grid-cell
averaging, of which the temporal-cancellation half is large (T3: ~3-4×)
and the spatial half is modest (T3: ~22% over 64× area). T2 shows the
practical consequence: bin saturation, not a real shift in distributional
shape, drives most of the reported J.

**Tests bearing on each column:**
- T1, T4: rule out conversion error and temporal-window mismatch for
  *both* columns — neither column suffers from these.
- T2, T3, T7: bear almost entirely on Column 2's validity — they quantify
  exactly why its global reference is not on the same footing as its site
  values.
- T5, T6: bear on Column 2's *interpretation* if kept — they show the
  residual gap is not explained by management (opposite direction) and
  only weakly by siting bias, meaning what's left after T2/T3 is
  substantially a genuine point-vs-grid-cell representativeness question,
  not an artifact needing further debugging.

**Options** (not chosen here):

1. **Re-bin on the measured distribution.** Build global bin edges from
   the measured-network-compatible quantity instead of reusing TRENDY's
   own quantile bins. Changes: a new global reference would need
   constructing (this repo has no independent global "measured-flux-like"
   product — TRENDY would still have to supply it, so this mainly re-bins
   TRENDY's *global* distribution using boundaries chosen to not saturate
   against the measured values' range, e.g. matched to measured
   percentiles). Affects `table_igbp_global_distribution_crosswalk.csv`
   is unrelated; would affect a new `trendy_nee_median`-derived global
   bin table, `Supp_sampling_ratio_siteKG_IGBP_NEE_ET.png` panel E,
   `Supp_jaccard_trajectory_siteKG_IGBP_NEE_ET.png`'s NEE line, and
   `Supp_compare_geospatial_vs_sitelevel_grid.png` row E′/F′.
2. **Drop the measured-flux axes from the representativeness figure
   entirely and present them as a paired comparison instead** (i.e., keep
   something like this diagnostic's own T7 figure — measured vs.
   TRENDY-at-site, 1:1 line, bias/IQR — as the co-author-facing artifact,
   rather than forcing it through the Jaccard/global-bin framework at
   all). Affects: `Supp_sampling_ratio_siteKG_IGBP_NEE_ET.png` and
   `Supp_jaccard_trajectory_siteKG_IGBP_NEE_ET.png` would lose their
   NEE/ET rows/lines; `Supp_compare_geospatial_vs_sitelevel_grid.png`
   would need a redesigned row E/E′, F/F′ (no longer a sampling-ratio bar
   pair, since there is no comparable site-side ratio to show).
3. **Keep both columns as they are, with a stated caveat** pointing to
   this report (or a condensed version of it) in the methods/caption text
   — e.g. "the site-measured NEE/ET axes compare a per-site median of
   signed annual flux against a spatially- and temporally-averaged
   absolute-flux model climatology; see [diagnostic] for why this
   produces a materially lower Jaccard than the model-vs-model axes."
   Affects: no figure changes; adds text to
   `Supp_sampling_ratio_siteKG_IGBP_NEE_ET.legend.txt`,
   `Supp_jaccard_trajectory_siteKG_IGBP_NEE_ET.legend.txt`,
   `Supp_methods_siteKG_IGBP_NEE_ET.txt`, and
   `Supp_compare_geospatial_vs_sitelevel_grid.legend.txt`.

---

## Not-found / code-vs-docs disagreement log

- **No global IGBP-area table, no management/land-use flux variables, no
  per-site "measured years" list stored anywhere** — all confirmed absent
  by direct search, not assumed; see T4/T5 sections above for exactly
  what was searched and the proxies used instead.
- **`CLM-FATES` has no intermediate annual GeoTIFF on disk** despite being
  named in the final-16-model ensemble list recorded in
  `logs/trendy_analysis_complete.marker` — searched
  `data/external/trendy/derived/intermediate/` (17 models × 2 vars = 34
  files; CLM-FATES is not among them) and confirmed this session's fork
  did not find a corresponding error in `logs/trendy_analysis_*.log`
  (not read in this pass — flagged, not resolved; out of scope for this
  diagnostic, which uses the 16-model list as recorded in the marker
  file, treating that as authoritative over any inference from the
  intermediate directory).
- **Code vs. a prior session's figure caption disagree on the ET
  "ceiling" mechanism.** A 2026-09 session's `fig_rep018` caption
  described a "histogram ceiling at 1000 mm yr⁻¹" causing one collapsed
  breakpoint. Reading `figure_representativeness_trendy_compute.R`
  directly (this session) shows the actual mechanism is a **general
  adaptive low-cut threshold** (the first bin's upper edge is halved or
  doubled if it would otherwise capture <1% or >70% of land area,
  `:507-520`) — a documented, general-purpose rule that happens to
  produce a near-1000mm effective ceiling for ET in this particular run,
  not a hardcoded 1000mm constant. Per the task's instruction, code is
  treated as authoritative: the mechanism is the adaptive land-fraction
  rule, not a fixed ceiling, even though the earlier caption's
  description of the *symptom* (an apparent ceiling near 1000mm) was not
  wrong for that specific dataset.
- **`badm_management_coverage.csv` predates the current 781-site
  network** (built against the 767-site snapshot) — the 14-site gap is
  listed explicitly above (T5), not silently absorbed as `FALSE`/NA.
