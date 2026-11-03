# GSL duration curve and target-share sweep — proposed analyses for discussion with Tash (2026-11-03)

Three analyses proposed by the context agent and approved by Dave, run on Tash's 62-site
reproduction (`GSL_definition/code/12_duration_curve_and_target_sweep.R`; full prompt:
`logs/prompts/2026-11-03_gsl_duration_curve_target_sweep.md`). Main run throughout: `GPP_NT_VUT_REF`,
negative daily GPP set to 0, calendar years, `GSL_SITES`' own 62-site/505-site-year list, no winter
screen — the same run `04_reproduce.R` checks against the manuscript. No existing script, table, or
figure was changed; everything below reuses `fine_grid.rds`/`fraction_sweep.rds`/`derived_site.csv`
wherever they already have what's needed, recomputing only what's missing (listed per section).
These are draft figures for discussion, not candidates for the final figure set.

## 1. GPP-duration curve

For each site, the curve traced by mean GSL(T) (x, days per year above threshold, averaged over
years) against mean F(T) (y, share of annual GPP on those days, averaged over years) across the
fine grid (0.01–50% of P99, reused directly from `fine_grid.rds`). Z16 (10% of P99) and the 0.95
crossing were already in `derived_site.csv`; the 0.85 crossing was the one thing missing and was
recomputed from `fine_grid.rds` using the identical rule T95 already uses (first fine-grid point,
ascending, where mean F rounds to ≤ the target share) — checked that the recomputed 0.95 crossing
matches `derived_site.csv`'s own `t95_pct` exactly for all 62 sites before trusting the same
function for 0.85.

Every site's curve reaches both the 0.85 and the 0.95 share within the fine grid — no site has a
missing (`NA`) crossing at either share. Table: `tables/duration_curve_crossings.csv` (62 rows: site,
skewness, Z16 GSL/F, and GSL/threshold-%/F at the 0.85 and 0.95 crossings).

Two examples (Figure P1a): **AU-ASM** (skewness 3.88, the most skewed site in the set) needs a
threshold of only 5.2% of P99 to capture 95% of its annual GPP, in 298 days — but its Z16 point (a
fixed 10% of P99) only captures 75% of annual GPP in 196 days. **AU-How** (skewness 0.36, close to
the low end) needs a much higher threshold, 25.0% of P99, to capture 95% of its GPP in 317.5 days,
but its Z16 point already captures 99.9% of annual GPP — for this site a fixed 10%-of-P99 threshold
and a 95%-of-GPP threshold give almost the same answer. This is the mechanism the whole proposal is
about: a fixed threshold and a fixed-share threshold diverge sharply for skewed sites and barely
diverge for even sites.

Figure P1b shows the same curve for all 62 sites at once, coloured by skewness on a continuous
scale: low-skew curves sit high and reach the top-right corner (GSL near the full year, F near 1)
quickly; high-skew curves (brighter in the viridis scale) peel away earlier and sit lower, needing
many fewer days to pass through the same F range but also losing a given share of GPP at a much
lower GSL. AU-ASM's curve (brightest, skewness 3.88) is visibly the most bowed of all 62.

## 2. Tash's Figure 4 relationship across target shares

For each target share from 0.80 to 0.98 (step 0.01, 19 values — a subset of the existing
`fraction_sweep.rds`'s 0.80–0.99 range), each site's selected threshold (% of P99) was recomputed
from `fine_grid.rds` using the same share-crossing rule as section 1 (the one thing
`fraction_sweep.rds` doesn't carry — it has the resulting GSL, not the threshold % the method
actually selects). That threshold was regressed on skewness four ways per target — Pearson r,
linear fit, quadratic fit (`threshold ~ skewness + skewness²`, raw not orthogonal terms) — each with
and without AU-ASM. Full table: `tables/target_sweep_fits.csv` (38 rows: 19 targets × with/without
AU-ASM); the underlying per-site thresholds are in `tables/target_sweep_thresholds.csv`.

**Check at 0.95, against Tash's published r = −0.68, adjusted R² = 0.57 (0.56 without AU-ASM):**

| | r (published −0.68) | linear adj. R² | quadratic adj. R² (published 0.57 / 0.56) |
|---|---:|---:|---:|
| AU-ASM included (n=62) | −0.6781 (diff +0.0019) | 0.4508 | **0.5725** (diff +0.0025) |
| AU-ASM excluded (n=61) | −0.7229 (diff −0.0429, no published value to check against) | 0.5146 | **0.5581** (diff −0.0019) |

r matches the published value almost exactly when AU-ASM is included (difference 0.0019, already
known from `reproduction_report.md`'s own check). The **adjusted R² match is clean but only for the
quadratic fit** — 0.5725 and 0.5581 are within 0.002–0.003 of the published 0.57/0.56, while the
*linear* adjusted R² (0.45 / 0.51) is nowhere close. **This is new information: Tash's published
"adjusted R² = 0.57" almost certainly comes from a quadratic (curved), not a simple linear, fit of
threshold on skewness.** The quadratic term itself is significant at 0.95 (b₂ = 2.47, t = 4.25,
p = 7.6×10⁻⁵ with AU-ASM; b₂ = 3.52, t = 2.61, p = 0.012 without), consistent with a real curve, not
just a better fit to noise.

Excluding AU-ASM moves r *away* from the published value (−0.72 vs. −0.68), not toward it — removing
the most extreme point makes the relationship **stronger**, not weaker, at 0.95. This is worth
flagging to Tash rather than assuming a sign error on our part: with a single very-high-skewness
point in the sample, that point can anchor a correlation anywhere from making it look stronger to
making it look weaker depending on how far it sits from the rest of the fit, and here it sits
*closer* to the main cloud of points than a simple "it's an outlier" framing would suggest (see
Figure P2a's `*` markers).

**Does the relationship hold across every target share from 0.80 to 0.98? Yes, but it weakens
noticeably toward the top of the range.** r stays strongly negative (−0.73 to −0.76 with AU-ASM;
−0.75 to −0.76 without) across 0.80–0.93, then weakens steadily to −0.53 (with AU-ASM) / −0.58
(without) by 0.98 — the weakest point in the whole sweep (Figure P2b). The 95% CI widens over the
same range (±0.13 near 0.80 vs. ±0.18 near 0.98), so part of this is just fewer sites still able to
separate cleanly at a very high target share, not only a weaker true relationship. One site,
US-MMS, has no defined threshold at all at targets 0.80–0.81 (its curve never drops to F ≤ 0.80
within the 0.01–50%-of-P99 grid) — n = 61 instead of 62 at those two targets only, 62 everywhere
else.

**Figure P2**: panel a, selected threshold vs. skewness at five representative targets
(0.80/0.85/0.90/0.95/0.98), solid/dashed lines for the fit with/without AU-ASM (marked with a star);
panel b, r with its 95% CI across the full 0.80–0.98 sweep, both with and without AU-ASM, dotted
line at the 0.95 target used in the check above.

## 3. Two kinds of skew

Dave's hypothesis: at forest sites, much of the skewness comes from zero-GPP dormant days, which
sit below every threshold tested and so shouldn't drive how sensitive GSL is to threshold choice;
at dryland sites, it comes from low-but-nonzero days that *do* cross thresholds as the threshold
moves, and so should drive sensitivity. Two predictors were computed per site, at three near-zero
cuts (0.5%, 1%, 2% of P99, to check whether the exact cut matters):

- **near-zero share**: the share of pooled daily GPP below the cut (recomputed from
  `daily_extract.rds`, main-run convention).
- **partial skewness**: adjusted Fisher–Pearson skewness (`g1_adjusted()`, reused directly from
  `03_derive.R`) of only the days at or above the cut.

Tested against five outcomes: three sensitivity metrics — the interquartile range, across Tash's
22-point threshold grid (0.1%, 0.5%, 1–20% of P99), of (i) mean GSL, (ii) across-year GSL standard
deviation, (iii) across-year GSL trend slope, all newly computed here by reusing
`year_threshold_matrices()` on the already-cached daily extract, since `03_derive.R` only persists
the *pooled* mean at each grid point, not the per-year values needed for s.d./trend — and two
selected-threshold outcomes, the 0.85 and 0.95 crossings from section 1. Full-distribution skewness
is reported alongside both predictors for comparison. Tables: `tables/skew_predictors.csv` (per-site
values), `tables/skew_predictor_correlations.csv` (every predictor × outcome × cut Pearson r),
`tables/skew_predictor_two_predictor_regression.csv` (outcome ~ near-zero share + partial skewness,
per cut).

**Partial skewness predicts every outcome far better than near-zero share does, and does at least as
well as full-distribution skewness — this holds at all three cuts (0.5%, 1%, 2%), so the exact cut
does not change the conclusion.** At the 1% cut:

| Outcome | near-zero share r (p) | partial skewness r (p) | full skewness r (p) |
|---|---:|---:|---:|
| Sensitivity IQR, mean GSL | −0.11 (0.39, n.s.) | **0.69** (<0.001) | 0.63 (<0.001) |
| Sensitivity IQR, GSL s.d. | −0.34 (0.006) | **0.61** (<0.001) | 0.49 (<0.001) |
| Sensitivity IQR, GSL trend | −0.34 (0.008) | **0.46** (<0.001) | 0.35 (0.006) |
| Threshold at 0.85 | 0.15 (0.25, n.s.) | **−0.84** (<0.001) | −0.74 (<0.001) |
| Threshold at 0.95 | −0.23 (0.07, n.s.) | **−0.65** (<0.001) | −0.68 (<0.001) |

Near-zero share is not even statistically significant (p > 0.05) for the mean-GSL sensitivity or
either selected-threshold outcome, and only weakly significant for the s.d./trend sensitivity
metrics — far weaker than either skewness measure in every case. The two-predictor regressions
(`outcome ~ near-zero share + partial skewness`, both at once) confirm this is not a case where the
two predictors are just correlated with each other and splitting credit: with partial skewness held
constant, near-zero share's own coefficient is non-significant for three of the five outcomes
(sensitivity IQR mean-GSL, threshold at 0.85 — wrong sign and p = 0.03, borderline — and weakly
significant with the "wrong" sign for sd/trend IQR), while partial skewness remains highly
significant (p < 10⁻³) in every one of the five regressions.

**This does not confirm Dave's forest-vs-dryland framing as a clean two-way split** — it suggests
instead that what matters everywhere is the shape of the *above-threshold* distribution (how skewed
the non-dormant days are), not simply how many days are near zero. A forest site's near-zero
dormant-day share and a dryland site's near-zero share can predict sensitivity about equally
poorly; what distinguishes high- from low-sensitivity sites is the partial skewness of the days
actually being counted. Worth raising with Tash as a reframing, not a rejection, of the original
hypothesis — the near-zero/crossing mechanism she and Dave discussed may still be *why* partial
skewness differs between forest and dryland sites, even though near-zero share itself is not the
useful predictor.

## Figures checked before committing

Every PNG in `figures/drafts/proposal/` was opened and checked:

- **Figure P1a** (AU-ASM, AU-How): both curves, the highlighted 0.1–20%-of-P99 segment, and the
  three markers (Z16/0.85/0.95) are all visually distinct; panel subtitles state each site's
  skewness; legend does not overlap the curve or markers.
- **Figure P1b** (all 62 sites): 62 overlapping curves are expected and legible as a fan — the
  continuous skewness scale clearly separates the handful of high-skewness (bright) curves, including
  AU-ASM's, from the dense low-skewness cluster; axis/legend text does not overlap any curve.
  checked.
- **Figure P2**: first render had the panel letter "a" repeated once per facet strip (five times)
  because the in-panel `gsl_panel_letter()` annotation draws once per facet, not once per figure —
  caught on this review and fixed by switching to patchwork's figure-level tag mechanism instead,
  which tags once per top-level panel regardless of faceting; panel b's two confidence ribbons
  overlap where the with/without-AU-ASM estimates are close (expected, both colours remain
  distinguishable via the legend and the solid point/line colour).

## Files

**New script:** `code/12_duration_curve_and_target_sweep.R` (no existing script, table, or figure
changed).

**Tables** (`tables/`, all with a `.meta.json`): `duration_curve_crossings.csv`,
`target_sweep_thresholds.csv`, `target_sweep_fits.csv`, `skew_predictors.csv`,
`skew_predictor_correlations.csv`, `skew_predictor_two_predictor_regression.csv`.

**Figures** (`figures/drafts/proposal/`, each with `.png`, `.pdf`, `.legend.txt`, `.meta.json`):
`Figure P1a (duration curve examples)`, `Figure P1b (duration curve all sites)`,
`Figure P2 (target share sweep)`.
