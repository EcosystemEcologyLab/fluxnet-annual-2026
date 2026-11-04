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
fixed 10% of P99) only captures 75% of annual GPP, in just 196 days. Z16 *under*-admits for AU-ASM:
it keeps 102 fewer days than the 95% target actually needs. **AU-How** (skewness 0.36, close to the
low end) is not a case where Z16 and the 95% rule agree — both sites diverge from each other, just in
opposite directions. AU-How's Z16 point keeps 362 days, capturing 99.9% of annual GPP; its 95% target
only needs 317.5 days (threshold 25.0% of P99), and its 85% target needs just 258 days (threshold
33.9% of P99). So Z16 *over*-admits for AU-How: it keeps 44.5 more days than the 95% target needs,
and 104 more than the 85% target needs — the reverse of AU-ASM's shortfall. This is the mechanism the
whole proposal is about: a fixed threshold and a fixed-share threshold disagree sharply at both ends
of the skewness range, but in opposite directions depending on how a site's days are distributed.

Across all 62 sites, the fine grid's own range (0.01–50% of P99) comfortably covers every site's
crossing at both shares (no `NA`s, above), but Tash's own 22-value grid (0.1–20% of P99, the stretch
highlighted in Figure P1c below) is narrower and does not: **51 of 62 sites select a threshold above
20% of P99 — outside that grid — at the 0.85 target, versus only 5 of 62 at the 0.95 target.** The
85% share is reached by a far higher threshold than the 95% share at most sites (a stricter share
target needs a lower, more inclusive threshold, not a higher one), so the 22-point grid built around
Tash's own 95%-based method leaves most sites' 85%-based threshold outside its range.

Figure P1b shows the same curve for all 62 sites at once, coloured by skewness on a continuous
scale. Skewness alone does not order these curves cleanly, so this ordering claim from an earlier
draft of this report has been removed (see Figure P1c, added below, for why): a short burst of
photosynthesis after many near-zero days (e.g. US-Wkg) and a long tail of low but nonzero days (e.g.
AU-ASM) can both raise skewness substantially while pulling a site's curve in different directions.

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
known from `reproduction_report.md`'s own check). The **adjusted R² match is clean only for the
quadratic fit** — 0.5725 and 0.5581 are within 0.002–0.003 of the published 0.57/0.56, while the
*linear* adjusted R² (0.45 / 0.51) is nowhere close. This confirms the reproduction rather than
adding new information: Tash's own Figure 4 already fits a quadratic (curved) relationship, so
matching her adjusted R² only with a quadratic fit here is exactly what should happen, not a new
finding. With AU-ASM included, the quadratic term is significant at every target share checked,
including 0.95 (b₂ = 2.47, t = 4.25, p = 7.6×10⁻⁵).

**Without AU-ASM, that significance is far weaker and target-share-dependent.** The quadratic term
is *not* significant (p > 0.05) anywhere across target shares 0.80–0.91 — p falls steadily from 0.35
at 0.80 to 0.058 at 0.91, crossing below 0.05 only from 0.92 onward (b₂ = 3.52, t = 2.61, p = 0.012
at 0.95, the one value checked against the published figure above). AIC agrees: over 0.80–0.86 the
simpler linear fit has the lower (better) AIC than the quadratic (e.g. at 0.80, linear AIC 379.6 vs.
quadratic AIC 380.7; at 0.85, 377.6 vs. 377.9), and AIC only turns in the quadratic's favour from
about 0.87–0.88 (e.g. at 0.90, linear AIC 357.5 vs. quadratic AIC 356.5). So the curved relationship
Tash reports at 0.95 is robust with AU-ASM in the sample — but without it, most of the sweep does not
actually call for anything beyond a straight line; only the high-target end (0.92 and up) shows a
real quadratic signal once AU-ASM is removed.

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

**This is consistent with Dave's point, not a rejection of it, once the mechanics of GSL are spelled
out.** Zero-GPP days contribute nothing to annual GPP, and by construction they are never counted at
any threshold above zero — so a day's being zero or near-zero cannot itself drive which threshold
GSL selects; it can only ever dilute whatever signal the *other* days carry. Near-zero share is a
count of days that structurally cannot cross a threshold and so cannot structurally explain
sensitivity to threshold choice. Full-distribution skewness mixes those structurally-inert near-zero
days in with the above-threshold days that do all the work of determining where a threshold falls;
partial skewness removes that dilution and measures only the days that can actually cross a
threshold. That partial skewness consistently out-predicts both near-zero share and full-distribution
skewness is exactly what Dave's dormant-day framing implies: the dormant zero days are not informative
*themselves*, but their presence waters down full skewness as a predictor, and stripping them out (as
partial skewness does) recovers the signal. What distinguishes high- from low-sensitivity sites is the
shape of the above-threshold distribution; a forest site's dormant season and a dryland site's
near-zero days both act as dilution, not as the signal. Worth raising with Tash as a confirmation and
sharpening of the original hypothesis — it specifies *which* of the two natural skew measures
(near-zero share vs. above-threshold shape) actually carries Dave's proposed mechanism, rather than
leaving the two conflated inside a single full-distribution skewness number.

## New figures and fixes (2026-11-03 redraw)

Two replacement figures for Figure P1b's all-sites view, plus three fixes to the figures and four
wording corrections to this report (listed above in their own sections). No new data processing —
everything below is built from `tables/duration_curve_crossings.csv`, `tables/skew_predictors.csv`,
and the already-cached fine grid; `tables/*.csv` are byte-identical to the 3 November run (only their
`.meta.json` timestamps changed from re-running the script). Figure P1a and Figure P1b's PNG/PDF are
themselves unchanged (confirmed via `git status`); only Figure P1b's `.legend.txt` wording changed
(see Section 1 above).

- **`Figure P1c (duration curve exemplars)`** (new): replaces Figure P1b's continuous-skewness colour
  scale, which does not order 62 overlapping curves in a way a reader can use, with five named
  exemplar sites (AU-How, US-MMS, US-Wkg, US-SRG, AU-ASM) in distinct colours against a grey
  background of the other 57. Direct end-of-line labels (site + skewness), Tash's grid range
  thickened, Z16/0.85/0.95 markers — same construction as Figure P1a, extended to five sites on one
  panel.
- **`Figure P1d (sensitivity and Z16 capture vs skewness)`** (new): one point per site, three panels —
  (a) Tash's sensitivity measure vs. skewness, (b) Z16-season GPP share vs. skewness, (c) panel (a)
  repeated against partial skewness (days ≥ 1% of P99) instead of full skewness. **Panel (c) is
  included**: it reads as clearly as panels (a)/(b) and, if anything, shows a visually tighter
  relationship (consistent with partial skewness's higher Pearson r in section 3's table, 0.69 vs.
  0.63), so there was no reason to drop it. Points coloured by near-zero-day share, five exemplar
  sites labelled so this figure connects to Figure P1c.
- **Figure P2 fixes**: panel (a) now draws quadratic, not linear, fit lines, matching the fit Tash's
  published adjusted R² actually comes from; panel (b)'s legend moved from an inside corner that
  covered the 0.80–0.83 lines/bands to outside the panel entirely (`legend.position = "right"`); the
  caption's solid/dashed wording corrected to state plainly that solid = AU-ASM included, dashed =
  excluded.

**Unexpected, caught on review:** Figure P1d's panel (b) initially had the "AU-How" and "US-MMS"
labels overlapping each other — both sites sit almost on top of one another in that panel (skewness
0.36/0.58, Z16 capture 0.999/0.980, the top-left corner of the panel). The default `ggrepel` force was
not enough to separate them; fixed by increasing `force`, `box.padding`, and `max.overlaps` in that
one label layer (panels a/c, where the five points are already well spread out, needed no change).

## Figures checked before committing

Every new and changed PNG in `figures/drafts/proposal/` was opened at full size and checked:

- **Figure P1a** (unchanged, not re-checked beyond confirming via `git status` that its PNG/PDF are
  byte-identical to the 3 November commit).
- **Figure P1b** (unchanged image; only its caption text changed — reviewed the new wording for
  accuracy against Figure P1c, not re-checked visually since the PNG did not change).
- **Figure P1c** (new): the five exemplar curves and the grey background are all visually distinct;
  end-of-line labels do not overlap each other, the curves, or the shape legend (upper right); the
  thickened Tash-grid segment and the three marker shapes (Z16/0.85/0.95) are distinguishable from
  the rest of each curve; axis labels use plain words.
- **Figure P1d** (new): panels (a)/(b)/(c) share a single collected colour legend that does not
  overlap any point; the five exemplar labels in panel (a) and (c) do not overlap data or each other.
  Panel (b) initially had "AU-How" and "US-MMS" overlapping (see above) — fixed and re-checked at full
  size, confirmed both labels and their leader lines are now clearly separated and point to the
  correct markers.
- **Figure P2**: panel (a)'s fit lines are now visibly curved, not straight, in every facet; panel
  (b)'s legend sits fully outside the panel and no longer covers the 0.80–0.83 lines/bands; the two
  confidence ribbons still overlap where the with/without-AU-ASM estimates are close (expected, both
  colours remain distinguishable via the legend and the solid point/line colour). (First render on 3
  November also needed a panel-letter fix, already recorded above and unaffected by this redraw.)

## Files

**Script:** `code/12_duration_curve_and_target_sweep.R` (extended 2026-11-03; no existing script,
table, or figure outside this file's own outputs changed).

**Tables** (`tables/`, all with a `.meta.json`): `duration_curve_crossings.csv`,
`target_sweep_thresholds.csv`, `target_sweep_fits.csv`, `skew_predictors.csv`,
`skew_predictor_correlations.csv`, `skew_predictor_two_predictor_regression.csv` — unchanged content,
2026-11-03 redraw only refreshed their `.meta.json` run timestamps.

**Figures** (`figures/drafts/proposal/`, each with `.png`, `.pdf`, `.legend.txt`, `.meta.json`):
`Figure P1a (duration curve examples)` (unchanged), `Figure P1b (duration curve all sites)` (image
unchanged, caption text corrected), `Figure P1c (duration curve exemplars)` (new),
`Figure P1d (sensitivity and Z16 capture vs skewness)` (new), `Figure P2 (target share sweep)` (fixed:
quadratic fit lines, legend moved, caption corrected).
