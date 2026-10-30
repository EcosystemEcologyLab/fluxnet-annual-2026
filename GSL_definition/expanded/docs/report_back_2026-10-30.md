# GSL expanded-site analysis — report back (2026-10-30)

Full prompt: `../../logs/prompts/2026-10-30_gsl_expanded.md`. Code: `code/00_config.R` through
`06_checks.R`. Tables: `tables/*.csv`. Figures: `figures/Figure *`, `figures/Supplemental Figure
S*`. This is a new, standalone analysis under `GSL_definition/expanded/`; **the existing 62-site
analysis (`GSL_definition/code/`, `tables/`, `figures/`) is untouched** — confirmed by rerunning
its driver script (`code/03_derive.R`) twice during this work and diffing outputs byte-for-byte
each time.

## Selection

Rule: manuscript rules (IGBP exclusion; 0.25 daily QC threshold, NA fails; rule 4 >= 5 years;
rule 5 <= 10% of daily GPP below -1) + the per-site VUT/CUT fallback (`scripts/04_qc.R`'s own
rule) + rule 4 evaluated under R2, N = 14 (a year passes with at most 14 failing days) — the
rule chosen in `docs/selection_rule_options_2026-10-30.md`.

- **151 sites**, not the prompt's "about 156". 161 candidates passed rule 1 + rule 4 under R2
  N=14; 9 were then dropped for having **zero non-NA GPP values at all** (not the 5 the prompt
  expected — see "Deviation" below), and 1 (US-Aud) for rule 5 (17.83% of its daily GPP below
  -1, well over the 10% cutoff — also newly checked here, not previously checked for this site).
- **All 505 GSL-62 site-years are included** (checked directly: `anti_join` against
  `00_config.R::GSL_SITES`'s own year lists returns zero rows — `tables/qa_checks.csv`).
- **US-Ha1 keeps 17 years**, as expected.
- **12 of 151 sites are CUT-fallback** (no VUT QC data at all).
- **32 long-record sites** (>=15yr, 12 months/yr coverage) not in the GSL 62 are recovered.
- **67 trend sites** (>=10 passing years), vs. the 62-site project's fixed 21.

### Deviation from the prompt's expectation (see SESSION_LOG, "selection check deviates...")

The "drop sites with no GPP values" rule removed **9** sites, not the prompt's expected 5
(AU-Stp, ES-LJu, ID-PaD, JP-Khw, JP-Ta2). The other 4 — **AU-Cow, AU-Rob, GF-Guy, ID-PaB** — are
not a bug: all four have non-NA `NEE_VUT_REF_QC` (correctly classified "VUT" by the stated
fallback rule, which is keyed to QC availability) but `GPP_NT_VUT_REF` is entirely NULL for all
four; three of the four also have zero `GPP_NT_CUT_REF`. The fourth, AU-Cow, has 603 non-NA
`GPP_NT_CUT_REF` values that the stated VUT-priority fallback rule does not reach. All four
already passed the manuscript's own VUT-only rule in the first selection audit — this is the
first time their GPP availability has been checked at all, since the earlier audits' rule
5/GPP-availability checks only ever covered sites newly added relative to the manuscript rule.

## Settings used throughout (main run)

Negative daily GPP set to 0; GSL counts GPP > T (strict); GSL counted on the full gap-filled
daily series; P99 = 99th percentile of pooled daily GPP; skewness = adjusted Fisher-Pearson G1
(`code/03_derive.R::g1_adjusted()`, reused by sourcing); NEP = -NEE (resolved VUT/CUT per site);
Z17 = 10% of P99 (fixed); T95 = first point on the 0.01-50% fine grid where mean included-GPP
fraction F rounds to <= 0.95.

**Skewness thirds recomputed on the expanded set**, not the 62-site project's fixed 0.63/0.88
cutoffs: terciles of this set's own 151-site skewness distribution, main variant: **low <= 0.679,
middle <= 0.91, high > 0.91** (51/50/50 sites). GPP_DT_REF variant thirds are separately
recomputed on that variant's own distribution (`derived_site.csv`'s `variant` column).

## Headline numbers (main variant, 151 sites unless noted)

| Quantity | Value |
|---|---|
| Mean P99 | 12.09 gC/m2/d (range 1.83-22.63) |
| Mean skewness | 0.858 |
| Mean Z17 GSL | 251.4 days |
| Mean T95 GSL | 239.4 days (mean T95 threshold: 13.76% of P99) |
| Mean share of annual GPP outside season, Z17 | 3.97% |
| Mean share of annual GPP outside season, T95 | 5.01% (~5% by construction) |
| Site-year x threshold combinations "above threshold all year" | 31 of 6,136 (0.5%) |
| Mean bounds-check spread | 2.77 days (max 14, by R2 N=14 construction) |

## Figure-by-figure numbers

**Figure 1** (US-Ha1, US-SRM — illustrative, not rule-selected; chosen by eye for contrasting
phenology, not by any numeric criterion: US-Ha1 is a humid temperate deciduous forest with a
single clear summer season visible in panel a's tile plot, US-SRM is a semi-arid woody-grass
mosaic with a messier, more fragmented in-season pattern and a lower, flatter GSL-vs-threshold
curve). US-Ha1: 17 years, deciduous forest, not in the GSL 62. US-SRM: 20 years, woody-grass
mosaic, in the GSL 62. Panel b: mean
GSL falls from ~315 d (US-Ha1) / ~285 d (US-SRM) at a near-zero threshold to ~90 d / ~25 d at 50%
of P99; T95 sits below Z17 for both (consistent with both sites' T95 pct being < 10%).

**Figure 2** (share of annual GPP outside the Z17 season vs. skewness/aridity). Panel a: strong
positive relationship (woody-grass mosaic sites reach the highest shares, up to 32%). Panel b:
much weaker/noisier relationship with the CGIAR aridity index; all 151 sites have an aridity
value (none omitted).

**Figure 3** (sensitivity, two units). Panel a: median sensitivity (days lost per 1% of P99) is
near zero for low-skewness sites across the whole 0.1-35% range, and strongly negative
(steepening with threshold) for middle/high-skewness sites. Panel b: sensitivity at Z17 vs.
skewness, r = -0.30 (weak-to-moderate; more negative — larger in magnitude — at higher skewness).
Panel c: "days per 1% of included GPP" (common-unit sensitivity) vs. skewness, Z17: r = -0.79
(much stronger). Both correlations are negative, but the two quantities **run in opposite
directions in magnitude terms**: panel b's values grow more negative (the day-count sensitivity
gets larger) with skewness, while panel c's values shrink toward zero (the per-unit-F
sensitivity gets smaller), because panel c normalises by how much the included-GPP fraction F
changes per % of P99, and at high-skewness sites a small threshold change corresponds to a
disproportionately large change in F.

**Figure 4** (season timing). Panel a: season start shifts ~0-100 days *later* and end shifts
~0-130 days *earlier* between the 5% and 20% of P99 thresholds (narrowing season at higher
thresholds), with no strong skewness dependence. Panel b: mean within-season gap days range
0-160, generally higher at middle skewness, similar between Z17 and T95.

**Figure 5** (carbon gain). Panel a: pooled GPP~GSL slope at Z17 falls from 7.20 gC/m2/day (low
third, 95% CI 5.51-8.89) to 5.20 (high third, 4.12-6.28); T95 gives closely similar slopes (7.38
to 4.90). NEP~GSL slopes are smaller and noisier (~1-2 gC/m2/day). Panel b: pooled slopes on
start/end date are an order of magnitude smaller and mostly indistinguishable from zero — a
single day's shift in the season boundary does not translate linearly into annual carbon gain
the way a day of total GSL does.

**Figure 6** (climate fingerprints). Panel a: temperature sensitivity of GSL is ~6-8 days/degC
for low/middle-skewness thirds, falling to ~3-4 days/degC for the high third (Z17 and T95
closely similar); start-date sensitivity is negative (~-1 to -3.5 days/degC, start comes earlier
as temperature rises) and end-date sensitivity is positive (~1-2 days/degC for most, strongly
negative for the high third specifically). Panel b: pooled GSL trends at T95 range from -1.42
days/yr (woody-grass mosaic, CI crosses zero) to +2.06 days/yr (grassland, clearly positive);
by third, low (+0.80) and middle (+0.63) are positive and significant, high is indistinguishable
from zero. Years-to-detect (Weatherhead et al. 1998, 95%/80% power) at T95: grassland 8 yr,
evergreen forest 17 yr, deciduous forest 28 yr, woody-grass mosaic 19 yr; low-third 13 yr,
middle-third 15 yr, high-third 215 yr (near-zero slope). Mixed forest's trend is not estimable
(only 2 of 7 sites have >= 10 years).

## Supplemental figures

- **S1**: site/site-year counts by group, expanded (151) vs. GSL 62; 12 CUT-fallback sites; 32
  recovered long-record sites named in the legend file.
- **S2**: bounds-check spread distribution; the Z17 and T95 lines coincide exactly (spread =
  n_fail_days, independent of threshold, by construction — not a plotting error). 15 sites with
  the largest mean spread at Z17, dominated by evergreen forest.
- **S3**: GSL 62 (n=62) overlaid on the rest of the expanded set (n=89, non-62): mean skewness
  0.913 (62) vs. 0.819 (non-62); mean T95 13.4% vs. 14.0%; mean sensitivity at Z17 -5.40 vs.
  -5.17 days/1%P99; mean Z17 GSL in `tables/S3_headline.csv`. The two distributions largely
  overlap, with the GSL 62 skewed slightly toward lower skewness / shorter T95.
- **S4**: per-site trends, 67 expanded trend sites (the Figure 3a analogue) — full forest plot.
- **S5**: robustness to GPP_DT_REF. 1 of the sensitivity-at-Z17 vs. skewness relationship
  (panel a) and the skewness-third assignment (panel b) are both qualitatively unchanged from
  GPP_NT_REF (most sites keep the same third). As-delivered negative GPP (1 table line): mean
  skewness and mean Z17 GSL are close to the zero-floored main run (both in the legend file). A
  full GPP_DT_REF AR(1) pooled-trend refit (the Figure 6 analogue) was out of scope — documented
  explicitly rather than silently skipped.
- **S6**: the headline GPP~GSL(Z17) pooled slope by third, with vs. without the 12 CUT-fallback
  sites — nearly identical point estimates and overlapping CIs at every third; the fallback is
  not driving the main result.
- **S7**: GSL against the target included-GPP fraction (0.80-0.99), all 151 sites, coloured by
  group.

## Checks

All 8 items in `tables/qa_checks.csv` pass, including GSL-62 containment and pooled-model
convergence (0 of 96 carbon-gain fits, 0 of 48 temperature-sensitivity fits, and 17 of 136 trend
fits non-converged — every one of those 17 is the mixed-forest group's 2-trend-site shortfall,
not a numerical failure; see `docs/methods_memo.md` for the fallback ladder this required).

## Scope not completed

A full GPP_DT_REF AR(1) pooled-trend refit (Figure 6's exact analogue, ~136 more model fits) was
out of scope for Supplemental Figure S5, which instead compares per-site Z17 trends and
third-assignment stability under GPP_DT_REF. See `docs/methods_memo.md`.
