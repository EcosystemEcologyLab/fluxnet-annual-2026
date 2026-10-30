# GSL figure rebuild — Stage 1 methods memo

Covers `GSL_definition/code/00_config.R` through `02_screen.R` — framework run-order steps 1-3
(`docs/framework.md`). No derived quantities (P99, skewness, GSL, thresholds) and no figures;
those are Stage 2 (after Tash's files arrive) and Stage 3. Full prompts:
`logs/prompts/2026-10-29_gsl_stage1.md` (original, blocked on the framework document) and the
resume instruction that supplied the corrected framework (`docs/framework.md`, which supersedes
`docs/corrections_20261029.md` — kept for the record, not deleted).

## Step 1 (logging)

Done and committed on its own before this stage resumed (`3c38ec7`, `main`): `R/utils.R`'s
`log_exclusion()`/`log_unknown()` gained an optional `log_path` argument, defaulting to the
existing `outputs/exclusion_log.csv`/`outputs/unknown_log.csv`. Every call in this side project
passes `log_path = GSL_EXCLUSION_LOG` / `GSL_UNKNOWN_LOG` (`GSL_definition/logs/`), set in
`00_config.R`. `unknown_log.csv` was initialized with headers even though no record was logged to
it in this stage (see "Negative GPP" below) — zero rows, matching the main pipeline's "both files
written even if empty" convention.

**A second, un-anticipated repo-root write was found and avoided, not fixed by a second R/utils.R
change.** `R/utils.R::write_output_metadata()` — required by the framework for every table's
`.meta.json` — always appends the current `sessionInfo()` to the repo-root
`outputs/session_info.txt` as a side effect, with no path argument at all. Calling it from this
side project (as first drafted) silently modified that tracked, committed file. This was caught
(reverted before committing) and fixed the same way `TowerTango/code/update_cards.R` already
solved the identical problem: a local `write_meta()` in `00_config.R`, writing only the
`.meta.json` (no session-info side effect at all), rather than extending
`write_output_metadata()` itself — the resume instruction authorized only the one R/utils.R
change in step 1 (the `log_path` argument), not a second one. Every table in this stage uses the
local `write_meta()`, not `write_output_metadata()`.

## Step 2 (00_config.R — the Sites table)

The 62-site, 505-site-year table is parsed directly out of `docs/framework.md` at runtime (a
small markdown-table parser in `00_config.R`), rather than hand-transcribed into R literals, to
avoid a copy that could silently drift from the saved framework or contain a transcription typo.
Each row's `n` is cross-checked against the year-range string's own expanded count; `00_config.R`
`stop()`s (not just warns) if any row disagrees, if the total isn't 505, if the group counts
aren't 17/20/12/12/1, or if the count of sites with ≥10 years isn't 21. All four checks pass as
run.

`QC_THRESHOLD_DD` (0.50) is read from `R/pipeline_config.R`, not hard-coded, per the framework.

## Step 2 (01_extract_sites.R — extraction)

Pulled `TIMESTAMP, GPP_NT_VUT_REF, GPP_DT_VUT_REF, NEE_VUT_REF, NEE_VUT_REF_QC, NEE_CUT_REF_QC,
TA_F, P_F, SWC_F_MDS_1` for the 62 sites from the DuckDB `daily` table, **filtered to `dataset =
'FLUXMET'`** (the table also carries `dataset = 'ERA5'` meteorology-only rows with no NEE QC —
the framework's own correction). Read-only connection, never written to. 403,244 daily rows
across all 62 of 62 sites (none missing). Saved to `data/daily_extract.rds` (gitignored).

**Carbon source (VUT/CUT), per-site rule from `scripts/04_qc.R`:** all 62 sites are VUT-gated.
Zero CUT-only sites, zero ungated sites.

**`product_id` comparison** (`tables/site_products.csv`): compared across the repo's 1 September
(`fluxnet_shuttle_snapshot_20260901T094522.csv`) and 20 September
(`fluxnet_shuttle_snapshot_20260920T102211.csv`) snapshots for all 62 sites. **3 sites changed**
— FR-Mej, IT-Tor, NL-Vkp — exactly matching the framework's own stated expectation. Comparison
against Tash's 17 August snapshot is **pending**: `GSL_definition/data/tash_snapshot.csv` does
not exist yet, so `product_id_tash`/`changed_tash_to_sep20`/`flagged` are all `NA` (not `FALSE`)
in the table, and no site has yet been dropped from any match check on this basis — there are no
match checks in Stage 1 to drop them from in any case (those are Stage 2, framework step 5). Per
the framework's correction, this script does **not** stop on a product difference; it only lists
and flags.

## Step 3 (02_screen.R)

### Project-standard screening (`tables/site_year_screening.csv`, 505 rows)

One row per the 62 sites' own 505 site-years (from the Sites table, not independently selected).
For each: `days_present`, `days_na_qc`, `share_qc_ge_025`/`share_qc_ge_050` (passing-day count
divided by `days_present`, i.e. **NA-QC days are in the denominator and do not count as
passing**, per the framework's correction — this is the one choice most load-bearing for every
number below it), `all_days_pass_025`/`050`, and counts of `GPP_NT_VUT_REF < 0` and `< -1` (the
"main GPP series" variable named in `docs/framework.md`'s Data required table).

`qc_passes()` (`R/qc.R`) is used directly, not reimplemented, via `sum(qc_passes(...), na.rm =
TRUE)` — summing with `na.rm = TRUE` correctly counts only `TRUE` values as passing, leaving `NA`
QC rows counted in neither the numerator nor excluded from the denominator (i.e. counted as not
passing), matching the correction exactly.

**Days with an NA QC flag: zero, across all 505 site-years.** This follows from Tash's own
selection rule (every day of every one of her years already had to pass `NEE_VUT_REF_QC >=
0.25`, which requires a non-NA value) — her 62 sites are, by construction, a QC-complete subset,
so `log_unknown()` was never called in this stage and `unknown_log.csv` has zero rows.

`share_qc_ge_050` ranges 94.25%-100% across the 505 site-years (median and mode 100%, mean
99.66%) — every one of Tash's sites already clears the project standard's 0.50 threshold on
nearly every day, which is why the year-screening options below look the way they do.

**Negative GPP:** 453 of 505 site-years (90%) have at least one day with `GPP_NT_VUT_REF < 0`
(17,825 of 184,459 days total, 9.66%); 212 site-years have at least one day `< -1` (1,387 days
total). The highest single-site rate is CH-Lae at 10.4% of days `< -1`, well below US-Aud's
reported 17.3% (the reason it was excluded by Tash) — no other of the 62 looks like a comparable
case by this metric alone, but this was not tested further in Stage 1.

### Year-screening options (`tables/year_screening_options.csv`)

No option is chosen here — counts only, per the framework's open decision.

| Option | Sites kept | Site-years kept | Sites ≥10yr | Trend sites (of 21) dropped below 10yr | Sites ≥5yr |
|---|---|---|---|---|---|
| (b) every day passes 0.50 | 55 | 276 | 3 | 18 of 21: AU-Cpr, AU-Cum, AU-How, CA-TP4, CH-Cha, CH-Dav, CH-Rtb, DE-Gri, DE-Tha, FR-Pue, JP-Fhk, NL-Loo, US-CMW, US-Mpj, US-Seg, US-SRM, US-Ton, US-UMd | 30 |
| (d) ≥80% pass 0.50 | 62 | 505 | 21 | 0 | 62 |
| (d) ≥70% pass 0.50 | 62 | 505 | 21 | 0 | 62 |
| (d) ≥90% pass 0.50 | 62 | 505 | 21 | 0 | 62 |
| (a) ≥80% pass 0.50 (flag level for scaling) | 62 | 505 | 21 | 0 | 62 |

(a) and (d)-at-80% report identical `sites_kept`/`site_years_kept` because they use the same
qualifying rule (≥80% of days pass 0.50); they differ only in how GSL is later computed from the
qualifying years (Stage 2, framework "Derived quantities"/step 4 — (a) would scale by valid days,
(d) counts on the full gap-filled series), which is out of scope for Stage 1.

The gap between option (b) (55 sites, 3 trend sites survive) and every (d)/(a) variant (62
sites, 21 trend sites survive, unchanged even at 90%) is the single most consequential number for
the open year-screening decision: given `share_qc_ge_050`'s distribution above, essentially every
site-year that fails (b) does so by a handful of days, not a large share — the project standard
and Tash's 100%-complete rule disagree almost entirely at the all-or-nothing boundary, not
gradually.

### Tash-rule network-wide reproduction (`tables/tash_rule_reproduction_diff.csv`)

Applied to every site in the 20 September snapshot with daily FLUXMET data (781 sites, 2,314,343
rows), not just the 62: `NEE_VUT_REF_QC >= 0.25` on every day of a year, ≥5 complete years,
US-Aud excluded. **"Complete year" is interpreted as every calendar day present (365 or 366,
leap-year-aware) and every one of them passing 0.25** — a year with missing days is not complete
even if every day it does have passes. This is a judgment call, stated here rather than tuned to
force a match, per the framework's explicit instruction.

**Result: 87 sites, 715 site-years — vs. the framework's 62 sites, 505 site-years.**

- **0 sites lost** — every one of the framework's 62 sites is reproduced.
- **25 sites added**, none overlapping the 62: AU-Cow, AU-Rob, BE-Lon, CA-DB2, CH-Oe2, CN-Dmn,
  CZ-KrP, CZ-wet, DE-Kli, DE-SfS, FR-Aur, GF-Guy, ID-PaB, IT-BFt, JP-Mse, KR-CRK, UK-AMo, US-ARM,
  US-Esm, US-MN1, US-MN2, US-Ne1, US-Ne2, US-Ne3, US-Ro1. No determinable reason from our side
  alone — these all satisfy the stated rule under our interpretation, but were not in Tash's
  Table S1. Possible explanations not tested here: an unstated additional criterion in her
  pipeline (e.g. IGBP restriction, a minimum-quality flag beyond `NEE_VUT_REF_QC`, or these
  sites/years were added to the network after her 17 August download). **Open question for
  Tash/Dave, not resolved in Stage 1.**
- **1 site-year lost**: IT-Tor 2012. IT-Tor is one of the 3 sites with a `product_id` change
  between our snapshots (see above) — plausible that our current product's 2012 record for this
  site has at least one missing calendar day that an earlier product version did not, which would
  fail the "complete year" test here even though the site overall still qualifies. Not confirmed
  by inspecting the raw data in this stage; flagged for follow-up once Tash's own snapshot/code is
  available.
- **211 site-years added** (at the 25 extra sites, and/or extra qualifying years at sites already
  in the 62 — not separately broken out in this stage; see the full list in
  `tables/tash_rule_reproduction_diff.csv`, `type == "site_year_added"`).

The rule was not adjusted to close either gap, per the framework's instruction.

## Choices made (summary)

- NA QC flag = not passing, at every threshold, project-standard and Tash's rule alike (framework
  correction, applied literally).
- `share_qc_ge_*` denominator is `days_present` (not non-NA days) — the same choice as the NA
  rule above, stated explicitly since it is easy to get backwards.
- "Complete year" (Tash-rule reproduction only) = full calendar-day coverage, not just all
  delivered days passing. Flagged as a judgment call above.
- Exclusion/unknown logs aggregated per site-year (one `log_exclusion()` call per site-year with
  a failing-day count), not per day — same principle as the main pipeline's 2026-10-29 "QC
  unknown follow-up" fix, applied here from the start rather than retrofitted.
- `GPP_NT_VUT_REF` used for the negative-GPP count (the framework's "main GPP series, as in the
  manuscript"), not `GPP_DT_VUT_REF`.

## Pending (not resolved by Stage 1, carried to Stage 2)

- Tash's 17 August snapshot, screened daily table/code, and her answers on negative-GPP handling
  and `>` vs. `>=` — requested, not yet received.
- Her two sensitivity site-year lists (30 and 188 sites) — requested, not yet received.
- The year-screening decision itself (options reported above; not chosen here).
- The 25-extra-site and IT-Tor-2012 discrepancies above.

## Stop (Stage 1)

Stopped after this memo, per the resume instruction ("stopping before step 4" = framework
run-order step 4, `03_derive.R`, Stage 2).

---

# Stage 2 methods memo (framework run-order steps 4-5)

Full prompt: `logs/prompts/2026-10-29_gsl_stage2.md`. Covers `code/03_derive.R` and
`code/04_reproduce.R`. No figures (Stage 3).

## Decisions (made 2026-10-29, after Stage 1 at `a1633a8`, without Tash's input)

Dave made the following calls, explicitly accepting the risk of a rerun if an assumption later
proves wrong. Each is a named switch in `code/00_config.R`, and `docs/framework.md`'s "Open
decisions" list has been replaced with these (recording that they were resolved after Stage 1,
not derived from Tash's own materials):

- **`GSL_YEAR_SCREEN = "d80"`**: GSL counted on the full gap-filled daily series; a year is kept
  when ≥80% of its days pass `QC_THRESHOLD_DD`. Stage 1 showed this keeps all 62 sites/505
  site-years, so the project-standard run and Tash's own selection cover identical site-years —
  only one run is carried through Stage 2's outputs. Option (b) (every day passes 0.50) is
  **not** carried through the derived-quantity tables; its Stage 1 counts (55 sites, 276
  site-years, 3 trend sites) are the strict alternative, reported only here and in the Stage 1
  section above.
- **`GSL_NEG_GPP_TO_ZERO_MAIN = TRUE`**: negative daily GPP set to 0 for the main run. The
  as-delivered version is computed for every quantity (`GSL_NEG_GPP_VARIANTS`), not dropped.
- **`GSL_STRICT_GT = TRUE`**: GSL counts `GPP > T`, not `GPP >= T`.
- **Site/year list**: the fixed 505-row Sites table, unchanged. IT-Tor 2012 is kept despite
  failing Stage 1's Tash-rule reproduction check; the 5 extra site-years Stage 1 found at
  already-included sites (CA-Ca1 2004/2010, CH-Dav 2011, DE-Tha 2025, NL-Vkp 2025) are not added.
- **`GSL_GPP_VARIANTS`**: `GPP_NT_VUT_REF` main, `GPP_DT_VUT_REF` supplementary.

## Step 4 (03_derive.R)

Computes every quantity in `docs/framework.md`'s "Derived quantities" table, per site, for all
4 combinations of `{main, supplementary} GPP variant x {zero-floored, as-delivered} negative-GPP
treatment` (248 rows = 62 sites x 4 variants in `tables/derived_site.csv`).

**Formulas used** (none specified down to the implementation level by the framework, so stated
here explicitly):
- **Skewness**: adjusted Fisher-Pearson G1 (matches `scipy.stats.skew(bias=False)`), implemented
  directly — `g1 = m3/m2^1.5`, `G1 = g1 * sqrt(n(n-1))/(n-2)`, no package.
- **L-skewness**: Hosking (1990) unbiased sample L-moment ratio `l3/l2`, via the standard
  order-statistic L-moment estimators.
- **Kelly's skewness**: `(P90 + P10 - 2*P50) / (P90 - P10)`, quantile type 7 (R's default,
  matching numpy's default linear interpolation closely but not guaranteed identically — flagged
  as a possible source of small differences, not tested against Tash's own values since her code
  isn't available).
- **P99**: 99th percentile, type 7, over the full gap-filled daily series across the site's
  `GSL_YEAR_SCREEN`-passing years (not just QC-passing individual days within those years — see
  Stage 1's `GSL_YEAR_SCREEN="d80"` rule: once a year qualifies, all its days are used).
- **T95**: first point on the fine grid (0.01-50% of P99, 0.01% steps, 5000 points) where the
  multi-year mean of F(y,T) rounds to ≤0.95.
- **Local sensitivity**: centered difference of mean GSL over ±0.05% of P99 (5 fine-grid steps),
  expressed as days per 1% of P99.
- **Common-unit sensitivity**: the same centered-difference approach applied to mean GSL vs. mean
  F directly (not via T), scaled to "per 1% change in F" (×0.01, since F is a 0-1 fraction).
- **Trend CI**: `confint()` on `lm(GSL ~ year)`, computed only for the 21 sites with ≥10 years
  (`GSL_SITES_GE10YR`), per the framework; the trend slope itself is computed for every site
  regardless of year count.

**Performance**: per-year GSL(T)/F(T) across the full fine grid uses `findInterval()` on each
year's sorted daily series (O(log n) per threshold via one vectorized call), not a loop over
5000 grid points — the whole script (62 sites x 4 variants) runs in ~9 seconds.

**Large outputs**: the fine grid (5000 points x 62 sites x 4 variants = 1,240,000 rows) and the
fraction sweep (20 target fractions x years x sites x variants) go to `data/fine_grid.rds` /
`data/fraction_sweep.rds` (gitignored), with summaries in `tables/fine_grid_summary.csv` /
`tables/fraction_sweep_summary.csv` (git-tracked).

## Step 5 (04_reproduce.R) — see `docs/reproduction_report.md` for the full report

**Checks 1, 2 and 4 all pass on the main run** (zero-floored `GPP_NT_VUT_REF`):
- Skewness range: 0.1932-3.8757, matching the published 0.19-3.88 to 2 decimals.
- Tercile counts at the published 0.63/0.88 boundaries: exactly 21/20/21 (all 62 sites).
- T95-vs-skewness correlation: r = -0.6781 vs. published -0.68 (difference 0.0019).

**The as-delivered (no zero-flooring) version is a materially worse match** on every checkable
statistic — most strikingly check 4's correlation weakens to r = -0.5524 (vs. -0.68 published).
This is the strongest evidence gathered in Stage 2 that `GSL_NEG_GPP_TO_ZERO_MAIN = TRUE` is the
right call, though it remains an inference (Tash's own code was not available to confirm
directly) consistent with, not proof of, the framework's own note that the manuscript's text
suggests she zeroed negative GPP.

**Check 3 (three Figure 1 correlations, r = 0.63/0.49/0.34) could not be attempted.** Neither the
Stage 2 prompt nor `docs/framework.md` states what two quantities each of the three values
correlates — the prompt itself left this blank (`[definition: ________]`). Reported as not
checkable in `docs/reproduction_report.md`, with what's needed to attempt it stated there, per
the Stage 2 instruction's own fallback.

**Check 5 (Tash's 22-point grid vs. the fine grid) is an internal consistency check only, not a
match to a published value** — Tash's own screened daily table/code has not been received, so
there is nothing external to compare against yet. Confirmed the two grids agree exactly
(difference 0 to the precision shown) when computed from the same data, which is a
code-correctness check on the fine-grid implementation, not independent validation.

**FR-Mej/IT-Tor/NL-Vkp (product_id changed between snapshots)**: every match-check table reports
each statistic with and without these 3 sites. Excluding them changes results only slightly
(e.g. check 4's r shifts from -0.6781 to -0.6790) — the product change does not appear to be
driving any of the checked statistics materially, though this was only tested on these four
specific checks, not on every derived quantity.

## Pending (carried from Stage 1, still not resolved)

Tash's 17 August snapshot, screened daily table/code, her negative-GPP and `>`/`>=` answers, her
two sensitivity site-year lists, and the three-correlation definition for check 3 above — all
still requested, not yet received. Stage 2's decisions were made without them, by design, and
are explicitly subject to revision if any later prove wrong.

## Stop (Stage 2)

Stopped after `docs/reproduction_report.md`, per the Stage 2 prompt ("Stop after step 5 for PI
sign-off"). No figure has been drawn.

---

# Stage 3 methods memo (framework run-order step 6: figures + benchmark table)

Full prompt: `logs/prompts/2026-10-29_gsl_stage3.md`. Dave signed off Stage 2
(`docs/reproduction_report.md`, checks 1/2/4 pass on the main run). Full results:
`docs/report_back_2026-10-29.md`.

## New quantities computed (not in 03_derive.R's output)

`03_derive.R` (Stage 2) only persisted *pooled* quantities across the fine grid (mean GSL, mean
F, local sensitivity, all already averaged across years) — not per-year values at arbitrary
thresholds. Three figure scripts needed per-year detail 03_derive.R didn't keep, computed fresh
here, always reusing already-computed scalars (P99, skewness, T95 etc.) rather than
re-deriving them:

- **Figure 3 / supplementary Figure 3** (`07_fig3.R`, `10_supplementary.R`): per-year GSL at a
  0.1%-step grid across 5-20% of P99 (plus each site's own T95 point), for the 21 trend sites,
  to fit an OLS trend slope + p-value at every threshold.
- **Figure 4 panel a** (`08_fig4.R`): per-year F(y,T)/GSL(y,T) pairs across a coarser 0.5%-step
  grid (0.1-60% of P99), for the two Figure 1 sites only.
- **Benchmark table** (`09_benchmark.R`): annual GPP sum per site-year, a simple aggregation not
  previously computed, for the 21 trend sites.

Figure 4 panels b and c, by contrast, needed **no new computation at all**: panel b reuses
`fraction_sweep.rds` and `derived_site.csv`'s `z17_mean_f`/`z17_mean_gsl` directly; panel c
reuses `z17_common_unit_sensitivity` and `t95_common_unit_sensitivity` directly — "common-unit
sensitivity at 0.95" is, by the Stage 2 definition of T95 (the threshold where mean F first
drops to <=0.95), exactly the sensitivity already computed at T95.

## Rendering bugs found and fixed by inspecting each figure at full size

1. **`geom_raster()` + `scale_x_log10()` silently rendered Figure 2's heat map as near-blank**,
   with thin vertical stripes instead of a continuous image. `geom_raster()` assumes a grid that
   is uniform *after* the scale transform; the fine grid is uniform in untransformed %P99 (0.01
   steps), not in log space. Fixed with `geom_tile(width = 0.01)` — tile width specified in
   untransformed data units renders correctly under a log coordinate transform, unlike raster.
2. **`scale_x_log10(limits = ...)` dropped ~2/3 of the low-threshold tile columns**, not just
   points outside the stated range — `limits=` on a scale drops any row whose data value (or, for
   tiles, apparently its rendered extent) falls outside, which removed far more than intended.
   Fixed with `coord_cartesian(xlim = ...)`, which clips the *view* without discarding data — the
   documented correct pattern for "zooming" vs. "filtering" in ggplot2.
3. **`geom_density()`'s default legend key glyph (a bordered box) collided with `geom_line()`'s
   glyph** when both were mapped to the same colour aesthetic in Figure 1 panel b, rendering as a
   visually broken combined key. Fixed with `key_glyph = "path"` on the density layer, plus
   separating the density/GSL distinction into its own `linetype` aesthetic with its own legend,
   rather than overloading colour for two different things.
4. **`panel_letter()`'s default `x = -Inf` is undefined under `scale_x_log10()`** (`log(-Inf)` is
   `NaN`), which silently dropped the panel letter in Figure 2 panel b. Fixed by passing an
   explicit `x` within the panel's actual range.
5. **patchwork auto-collected an individual panel's `legend.position = "top"` to the top of the
   whole composite figure**, not just above that one panel, in Figure 2 panel c. Fixed by placing
   the legend *inside* the panel with explicit `legend.position = c(x, y)` coordinates instead of
   relying on patchwork's automatic collection (also added `plot_layout(guides = "keep")`
   explicitly at both composition levels, to be safe against any other auto-collection).

None of these would have been caught without opening the rendered PNGs — all passed `ggsave()`
silently. Per the Stage 3 prompt's own checks requirement, every figure was inspected at full
size, not just confirmed to exist.

## No in-panel explanatory text

`R/nature_format.R`'s own header documents the rule ("no caption or explanatory text drawn
inside the figure itself — put it in the `.legend.txt` file instead"). The first draft of Figure
1 panel a violated this (text labels on each threshold line); removed in favour of unlabelled
dashed reference lines, with the thresholds named in `.legend.txt` instead.

## Verification

Every number quoted in `docs/report_back_2026-10-29.md` was independently re-derived from the
underlying tables (not just copied from script console output) and matched exactly — see that
document's "Checks performed" section.

## Departures from the framework (Stage 3)

- Supplementary Figures 2/3's second variant is "negative GPP as-delivered," not "with Tash's
  screening" as the original framework text specified — per the Stage 3 prompt's own instruction,
  because Tash's screening selects the identical 505 site-years as the main d80-screened run.
- The current manuscript's own Figures 1-4 (framework "Supplementary outputs," rebuilt for
  direct comparison) are deferred, not built — explicitly instructed in the Stage 3 prompt, since
  the framework doesn't describe their current form in enough detail to reproduce without the
  manuscript itself.

## Stop (Stage 3)

Stopped after `docs/report_back_2026-10-29.md` and this update, per the Stage 3 prompt. No work
beyond framework run-order step 6 (figures + benchmark table) has been done.

---

## Stage 3 revisions (2026-10-29)

Dave reviewed Stage 3's figures and requested six fixes (full prompt saved verbatim at
`logs/prompts/2026-10-29_gsl_stage3_revisions.md`). Affected scripts: `05_fig1.R`, `06_fig2.R`,
`07_fig3.R`, `08_fig4.R`, `10_supplementary.R`, `fig_helpers.R`.

### 1. CH-Dav bug in Figure 3a (real data-correctness bug, fixed)

Flagged: at CH-Dav, the Z17 (10% of P99) dot sat *outside* its own coloured bar for the 5-20%
threshold-range trend, which should always contain it (10% is itself a point in the 5-20% sweep).

Root cause, confirmed directly: `07_fig3.R`'s per-threshold trend function used
`sum(gy > T_abs, na.rm = TRUE)` to count days above threshold per year. `sum(..., na.rm = TRUE)`
over a vector that is **entirely NA** silently returns `0`, not `NA`. CH-Dav 2008 has 366 daily
rows but zero non-NA GPP values for the whole year — so that year contributed a fabricated
"GSL = 0" data point to the trend regression at every threshold in the sweep, badly distorting
the fitted slope relative to the slope reported by `03_derive.R` (which correctly excludes
all-NA years via `lm()`'s default `na.omit`, the same pattern used in its own
`year_threshold_matrices()`).

Checked every site in the trend set for the same failure mode (not just CH-Dav) — only CH-Dav is
affected; no other trend site has an all-NA year within its screened years. Fixed in `07_fig3.R`
and its duplicate in `10_supplementary.R`'s `build_fig3()` by marking an all-NA year's GSL as
`NA_real_` (not `0`) and letting `lm()`'s default exclusion handle it, matching `03_derive.R`.
Added a permanent `stopifnot()` regression guard in both files: Z17's trend (from
`derived_site.csv`) must lie within `[range_lo - eps, range_hi + eps]` of the sweep's own 5-20%
range, for every trend site, on every run.

**The headline count is unchanged by the fix**: of the 21 trend sites, 6 have a sign change and 6
have a significance change across 5-20% of P99 (11 either), identical before and after.

### 2. Figure 1 revisions

- **High-third site-selection rule changed.** Previously: nearest the high-third median skewness
  among all sites (excluding AU-ASM). Now: nearest the high-third median skewness **restricted to
  woody-grass mosaic and grassland sites** (the drylands groups the figure's takeaway is actually
  about), still excluding AU-ASM. New high-third site: **US-Ton** (woody-grass mosaic, skewness =
  1.212), replacing the previous selection. Low-third site is unchanged: **JP-Fjy** (skewness =
  0.512) — the rule change does not affect the low third, which was never restricted by group.
  This selection rule is reused as-is (not recomputed independently) everywhere Figure 1's two
  sites reappear, i.e. Figure 4 panel a.
- Panel a: each row now labelled with its site name and skewness.
- T95 drawn as a dotted firebrick line, visually distinct from the dashed 5/10/20%-of-P99
  reference lines (T95's threshold varies by site, so it can't share their fixed positions
  anyway).
- Panel b: a labelled vertical bracket at 7.5% of P99 shows the change in mean GSL between 5%
  and 10% of P99 for each site, in addition to being stated in `.legend.txt` (JP-Fjy = -26.40
  days, US-Ton = -34.70 days for the current site pair).
- Panel b's x-axis now covers 0-100% of P99 (`fine_grid.rds` only persists to 50%, so a new
  `site_mean_gsl_curve()` helper recomputes the mean-GSL-vs-threshold curve directly from
  `daily_extract.rds` out to 100%); y-axis labelled "Density".

### 3. One marker convention (Z17 = black circle, T95 = red triangle)

Centralised in `fig_helpers.R` as `GSL_MARKER_SHAPES`/`GSL_MARKER_COLORS`/
`scale_shape_gsl_marker()`, applied identically in Figures 2b, 2c, 3a and 4c (and their
`10_supplementary.R` duplicates for 2b/2c and 3a). The floating "T95"/"Z17" text labels
previously drawn inside Figure 2c are removed in favour of a proper `guide_legend()`.

One bug found while wiring this up: `guide_legend(override.aes = list(colour = ...))` matches its
`colour` vector to the legend's rows *positionally*, but ggplot2's default legend order is an
alphabetical sort of the aesthetic's string values ("T95" sorts before "Z17") — silently swapping
the override colours onto the wrong rows even though the shapes (driven by a named `values=`
vector) were correct. Fixed by giving `scale_shape_gsl_marker()` an explicit
`breaks = c("Z17", "T95")`, so the legend's row order always matches the colour vector's assumed
order.

### 4. Figure 2 heat map

- **Vertical striping at low thresholds removed.** The fine grid is uniform in *linear* %P99
  (0.01 steps); on a log-x axis that packs far too many raw columns into too few screen pixels at
  the low-threshold end, aliasing into stripes. Fixed with log-uniform binning (35 bins across
  0.1-35% of P99, `findInterval()` against log-spaced edges), shared between the heat map (panel
  b) and the bottom panel (panel d) so both read off the same bins. Bin count was tuned down from
  120 to 35 empirically — at higher bin counts, some bins near the low-threshold end became
  narrower than the fine grid's own 0.01% sampling resolution and contained zero raw points,
  reintroducing the same striping at a finer scale.
- **Sensitivity now shown as `|local_sensitivity|`** (days of GSL lost per 1% of P99, absolute
  value), with darker = more sensitive — previously the signed value was plotted, which is harder
  to read as a "how sensitive is this site" map since sign flips don't change how much a site's
  GSL estimate moves per percentage point. The signed value is still used in panel d (the
  low/middle/high skewness-third median curves), where the sign is informative (days lost, not
  just magnitude).
- **Panel d (bottom panel) is now explicitly smoothed**: same log-uniform bins as panel b, taking
  the median `local_sensitivity` per bin per skewness third, rather than the raw per-0.01%-step
  curve.
- **Found while re-rendering for this section**: panel d's `panel_letter("d")` call used the
  default `x = -Inf`, which (like item 4 above) is undefined (`NaN`) under `scale_x_log10()` —
  this silently dropped the "d" label from *every* Figure 2 variant, including the already-
  committed main run, without raising a warning that reached the console in the original Stage 3
  run. Found by directly reproducing the "NaNs produced" / "Removed 1 row ... geom_text()"
  warnings emitted by `10_supplementary.R`'s rerun during this revision pass, isolating them to a
  minimal `panel_letter("d")` + `scale_x_log10()` repro. Fixed in both `06_fig2.R` and
  `10_supplementary.R` with an explicit `x = 0.15, hjust = 0`, matching panel b's own fix from the
  original Stage 3 pass (see "Rendering bugs found and fixed" above) — the same bug, in the one
  panel that fix wasn't applied to.

### 5. Skewness colour scale capped at 2

`scale_color_gsl_skew()`/`scale_fill_gsl_skew()` in `fig_helpers.R`: fixed `limits = c(0, 2)`,
breaks at 0/0.5/1/1.5/2, with values above 2 squished to the top colour (`scales::squish`) and
labelled ">2" rather than greyed out as out-of-range. Most sites sit in 0.2-1.5; without the cap,
the handful of sites above 2 compressed that whole working range into a narrow, hard-to-read
sliver of the viridis palette. The same fixed scale (same limits/breaks/labels/palette) is used
everywhere skewness is coloured: Figures 1, 2 and 4.

### 6. Legend cleanup

- "mixed forest" is omitted from a figure's group legend whenever no plotted site belongs to that
  group — `scale_color/fill_gsl_group()` sets `drop = TRUE` with `breaks = GSL_GROUP_ORDER` (not
  `limits=`), so the colour→name mapping stays fixed across every figure while each legend only
  lists the groups actually present in its own data (e.g. Figure 3's 21 trend sites have no
  mixed-forest member).
- Figure 4 panels a and c had legends overlapping/cutting their panel borders. Fixed by anchoring
  each legend box's own corner (not its center) to its `legend.position` via
  `legend.justification` — e.g. `legend.position = c(0.97, 0.03), legend.justification = c(1, 0)`
  anchors the box's bottom-right corner just inside the panel's bottom-right corner, so the box
  grows inward instead of spilling past the edge — plus a white `legend.background` and smaller
  legend text/key sizing to keep the box compact.

### Re-verification after the revisions

- `07_fig3.R`'s `stopifnot()` consistency check passes for all 21 trend sites (main run) and all
  21 sites in both `10_supplementary.R` variants.
- All of Figures 1-4 and all four supplementary figures were regenerated and inspected at full
  size (not just confirmed to exist) after every code change in this section.
- `10_supplementary.R`'s `build_fig2()`/`build_fig3()` — near-duplicate copies of `06_fig2.R`/
  `07_fig3.R`'s plotting code, not shared functions — were manually re-synced with every fix in
  this section; they do not pick up future `06_fig2.R`/`07_fig3.R` changes automatically.

---

## PeerJ restyle (2026-10-30)

Full prompt: `logs/prompts/2026-10-30_gsl_peerj_restyle.md`, written after Dave read PeerJ's
author instructions on 2026-10-30. No `logs/prompts/*_gsl_fig2_fixes.md` was found, so this is a
fresh task, not a reconciliation with an earlier unsent prompt.

### Style: PeerJ conventions replace R/nature_format.R's Nature conventions

`GSL_definition/code/fig_helpers.R` no longer calls `nature_theme()` or `save_nature_figure()`
(both enforce Nature-specific limits — 5-7pt text, <=1pt lines, fixed 89/183mm widths — that
PeerJ does not share and that are now too small/narrow for PeerJ's own floor). It defines its
own independent `theme_gsl()` and `save_gsl_figure()` with PeerJ's constants instead:

- Figure width 170mm (PeerJ's "about 170mm"); height as needed, capped at 240mm to leave margin
  on an 8.5x11in (215.9x279.4mm) page.
- Text: tick labels/legend >=8pt (`PEERJ_BASE_PT`), axis titles 9pt (`PEERJ_AXIS_TITLE_PT`),
  panel letters 10pt bold (`PEERJ_PANEL_LETTER_PT`, applied via a new `gsl_panel_letter()`
  wrapper around `R/nature_format.R::panel_letter()` — that function's own `annotate("text",...)`
  placement logic is generic, not Nature-specific, so it is reused, just with PeerJ's size
  substituted for Nature's 8pt default).
- Lines: main data 1.8pt (`PEERJ_LWD_DATA`, within the requested 1.5-2pt range), reference/
  threshold lines ~1pt (`PEERJ_LWD_REF`), Figure 1's "other years" lines 0.6pt and `grey55` (up
  from 0.25pt/`grey75` — darker and thicker, but still visibly behind the highlighted year's
  1.8pt black line). `nature_lwd()`'s ggplot-linewidth-to-rendered-pt conversion is reused as-is
  (pure unit math, not a Nature-specific limit).
- Points/markers roughly doubled at every call site (e.g. Figure 2 panel b's Z17/T95 markers
  0.5pt -> 1pt; Figure 3's trend-range points 1pt -> 2pt; Figure 4 panel a's x-marker 1.5pt ->
  3pt).
- >=300dpi: `PEERJ_DPI <- 600` (unchanged from the Nature-era setting, already well above
  PeerJ's floor).
- One file per figure, named `"Figure 1.png"`/`"Figure 1.pdf"` … `"Figure 4"`, and
  `"Supplemental Figure S1"` … `"S4"` for the four `10_supplementary.R` figures (S-numbers
  assigned in citation order — the order `10_supplementary.R` builds them in: Figure 2's DT
  variant, Figure 2's as-delivered variant, Figure 3's DT variant, Figure 3's as-delivered
  variant — also the order listed in `GSL_definition/README.md` and
  `docs/data_and_code_availability.md`). `save_gsl_figure()`'s `stem` argument is now this
  literal display name, not the old `"fig_0N_..."` internal name; the old-named files were
  removed from `GSL_definition/figures/` in this commit (fully superseded, not kept alongside).

### Skewness colour: discrete three-way palette, replacing the continuous viridis scale

`fig_helpers.R`'s `scale_color_gsl_skew()`/`scale_fill_gsl_skew()` (continuous viridis, capped at
2) are removed, replaced by `scale_color_gsl_skew_third()`/`scale_fill_gsl_skew_third()`: three
discrete viridis stops (`viridisLite::viridis(3, begin = 0.12, end = 0.85)`), one per skewness
third (low/middle/high; same tercile boundaries as before, 0.63 and 0.88 — see
`docs/framework.md`). `begin`/`end` keep clear of viridis's near-black and near-yellow extremes
so all three colours stay visually distinct; confirmed monotonic greyscale luminance
(~60/109/180 of 255, computed via the standard 0.299R+0.587G+0.114B weighting) so the palette
also reads correctly in greyscale/print. Used everywhere skewness is shown by colour: Figure 1
panel b (the two representative sites' density/mean-GSL lines, now coloured by their own
low/high skewness-third colour rather than an ad hoc green/purple pair), Figure 2 panel a (the
skewness strip, now categorical instead of a continuous tile gradient) and panel d (the
low/middle/high median-sensitivity lines, previously an inconsistent ad hoc
green/light-purple/dark-purple scheme — now the same palette as panel a), and Figure 4 panels a
(same two representative sites, same convention as Figure 1b) and b (all 62 sites, now coloured
by skewness third instead of continuous skewness).

Exact numeric skewness values are not lost — they remain in every affected figure's
`.legend.txt` (Figures 1 and 4 state each representative site's own value; Figure 2's panel a
restates the tercile boundaries) and in `tables/derived_site.csv`, already cited as an input
source by every figure that uses this table.

### Item 1: Figure 1b density lines labelled by site name

`05_fig1.R`'s panel b previously coloured/labelled its two lines `"low third"`/`"high third"`
via an ad hoc `role` column. Replaced with direct `site_id` colouring/labelling (`JP-Fjy`,
`US-Ton`), using the skewness-third palette's low/high colours (the same two colours Figure 4
panel a now also uses for these same two sites, via a `site_colours` vector built from
`GSL_SKEW_THIRD_COLORS[["low"]]`/`[["high"]]`). Figure 4 panel a's own `role`-based legend
(previously also `"low third"`/`"high third"`) was changed the same way for consistency, even
though only Figure 1b was explicitly requested — both panels show the identical two sites under
the identical color mapping, so leaving one relabelled and the other not would have produced a
visibly inconsistent pair of figures.

### Item 2: Figure 2b colour-scale cap and x-axis start

- **Colour scale capped near the 95th percentile of the plotted values**, not a fixed number —
  `fig_helpers.R::scale_fill_gsl_sensitivity(values, cap_quantile = 0.95, ...)` computes
  `quantile(values, 0.95)` from whatever vector is actually being plotted (each figure's own
  `heat_data$abs_sensitivity`), builds `pretty()` breaks below the cap plus the cap itself, and
  squishes (`scales::squish`) anything above it into the top colour, labelled `">= <cap>"` (ASCII
  `>=`, not the U+2265 glyph — the base `pdf()` device's PostScript Helvetica cannot encode it and
  silently substitutes ASCII back in with an `mbcsToSbcs` warning, the same glyph-availability gap
  `R/nature_format.R::nature_minus_labels()` documents for the minus sign). Computed caps: main
  Figure 2 (NT, zero-floored) 24.3 days/%P99; Supplemental S1 (DT, zero-floored) 28.8; S2 (NT,
  as-delivered) 24.3.
- **x-axis now starts at 0.5% of P99**, not 0.1% — the log-uniform bin edges
  (`10^seq(log10(0.5), log10(35), length.out = N_BINS + 1)`), the `fine_grid` filter, and
  `coord_cartesian(xlim = c(0.5, 35))` all moved together (panels b and d share the same bins, as
  before).
- **Low-threshold structure remains visible** after both changes — confirmed by direct visual
  inspection of the regenerated heat map: tile shading still varies clearly across the 0.5-3%
  range rather than saturating to one flat colour, in both the main run and both supplementary
  variants.

### Item 3: Figure 2c/2d absolute values; floating Z17/T95 text; line distinguishability

- **Panels c and d now plot `|value|`**, matching panel b. Panel c (`z17_sensitivity`/
  `t95_sensitivity`, previously signed — both are negative in `tables/derived_site.csv`, since
  GSL shrinks as the threshold rises) is wrapped in `abs()`. Panel d's per-bin median was already
  computed from `local_sensitivity`; now `median(abs(local_sensitivity), ...)`, consistent with
  panel b's `mean(abs(local_sensitivity), ...)`.
- **The floating "Z17"/"T95" text inside panel c was already removed** in the original Stage 3
  pass (2026-10-29) — confirmed by grep across every `GSL_definition/code/*.R` file for
  `annotate("text", ... label = "Z17"` or `"T95"` style calls: none remain outside Figure 1's
  genuinely different site-name/bracket annotations. This item was a no-op confirmation, not a
  new fix.
- **Panel d's three lines are now thicker** (`PEERJ_LWD_DATA`, 1.8pt, up from 0.5pt) **and use
  the shared skewness-third palette** (replacing the ad hoc green/light-purple/dark-purple
  scheme) — both changes make the low/middle/high lines clearly distinguishable without relying
  on a legend lookup.
- A side effect found while re-rendering: with PeerJ's larger (>=8pt) tick text, panel c's
  default 4-5 x-axis breaks overlapped in the narrower-panel, wider-range supplementary variants
  (e.g. Supplemental Figure S1's 0/10/20/30/40). Fixed with an explicit
  `scale_x_continuous(breaks = scales::breaks_pretty(n = 3))` in both `06_fig2.R` and
  `10_supplementary.R`'s `build_fig2()`.

### Item 4: CH-Dav 2008 — GPP_NT_VUT_REF entirely missing

Checked every site-year (all 62 sites, all screened years in `GSL_SITES`) for a site-year where
one GPP variant (`GPP_NT_VUT_REF` or `GPP_DT_VUT_REF`) is entirely `NA` across every daily row
while the other is complete. **Exactly one**: **CH-Dav 2008** — 366 daily rows, `GPP_NT_VUT_REF`
`NA` on all 366, `GPP_DT_VUT_REF` `NA` on 0. No other site-year in the network shows this
pattern. This is a genuine data gap in the FLUXNET Shuttle snapshot used for this analysis, not a
QC exclusion (the year passes d80 screening — see `GSL_SITES`/`03_derive.R` — and Tash's Table
S1 lists CH-Dav 2008 as a usable year, consistent with the fact that `GPP_DT_VUT_REF` and the QC
flags are complete for that year; only `GPP_NT_VUT_REF` itself is missing in our data).

**How it is handled**: this is the same root cause already documented above under "1. CH-Dav bug
in Figure 3a" (`sum(gy > T_abs, na.rm = TRUE)` over an all-NA vector silently returning `0`
instead of `NA`, now fixed everywhere GSL is counted per site-year). With that fix in place,
CH-Dav 2008 correctly contributes `NA_real_`, not a fabricated `GSL = 0`, to every NT-based
quantity (`derived_site_year.csv`'s `gsl_z17` for CH-Dav/2008 is `NA`; confirmed directly).
**CH-Dav therefore has 15 screened years but 14 usable years for every NT-based quantity** (mean
GSL, the trend regression, sensitivity, etc.) — the 2008 row is excluded, not silently zeroed.
This does not affect CH-Dav's supplementary DT-variant run (`GPP_DT_VUT_REF` is complete for
2008, so Supplemental Figures S1/S3 use all 15 years for CH-Dav).

### Re-verification after the PeerJ restyle

- `07_fig3.R`'s and `10_supplementary.R`'s `stopifnot()` consistency check (Z17's trend within
  its own 5-20% range bar) still passes for all 21 trend sites in all three variants.
- Sign/significance-change counts are unchanged from the Stage 3 report: main 6/6/11, DT 6/7/13,
  as-delivered 6/6/11.
- Figure 1's representative-site numbers are unchanged (JP-Fjy/US-Ton, same skewness and GSL-
  change figures as the Stage 3 report); Figure 4 panel a continues to reuse the same pair.
- All of Figures 1-4 and all four supplementary figures were regenerated and inspected at full
  size after every code change in this section — this is how the panel-c tick-label overlap
  (item 3) was caught.

## Expanded-site analysis (2026-10-30)

A separate, standalone analysis under `GSL_definition/expanded/` (own `code/`, `tables/`,
`figures/`, `docs/`), per `logs/prompts/2026-10-30_gsl_expanded.md`. Reuses this directory's
`code/00_config.R`, `code/03_derive.R` and `code/fig_helpers.R` **by sourcing**, not copying —
see `expanded/code/00_config.R`'s header. **Does not touch** the 62-site analysis above; every
shared-code edit this required was verified byte-identical against the 62-site driver's own
output before being committed (see "Shared-code changes" below).

### Selection rule

Dave's choice from `docs/selection_rule_options_2026-10-30.md`: manuscript rule 1 (IGBP
exclusion) + the per-site VUT/CUT QC/GPP fallback (`scripts/04_qc.R`'s own rule, never mixed
within a site) + rule 4 (>=5 years) evaluated under **R2, N=14** (a year passes if it has at
most 14 failing days, 0.25 daily threshold, NA fails) + rule 5 unchanged (<=10% of daily GPP
below -1 gC/m2/d, whole-site) + an explicit new rule: drop any site with zero non-NA GPP values
at all (can't derive anything for it). Tropical sites get no special casing. Result: **151
sites**, all 505 GSL-62 site-years included, US-Ha1 keeps 17 years (`expanded/code/01_select.R`,
reusing the already-committed `tables/selection_audit_population.csv`,
`selection_audit_rule_options_qc_fallback.csv` and `selection_audit_rule_options_site_year.csv`
rather than re-screening the whole network from scratch).

**Deviation from the prompt's stated expectation**: 9 sites were dropped for zero GPP data, not
the expected 5. The extra 4 (AU-Cow, AU-Rob, GF-Guy, ID-PaB) have non-NA VUT QC but entirely
NULL `GPP_NT_VUT_REF` (confirmed by a direct DuckDB query); recorded in SESSION_LOG as a
"something went wrong" entry and in `expanded/docs/report_back_2026-10-30.md`.

### Definitions, as implemented

- **Settings**: negative daily GPP -> 0 (main run); GSL counts `GPP > T` (strict); GSL counted
  on the full gap-filled daily series; P99/skewness/Z17/T95 exactly as `03_derive.R::derive_one()`
  already computes them (reused unmodified, aside from the shared-code fixes below). NEP = -NEE,
  using each site's own resolved NEE column (`NEE_VUT_REF` or `NEE_CUT_REF`).
- **Skewness thirds recomputed on the expanded set**: terciles (`quantile(skew, c(1/3,2/3))`) of
  this set's own distribution, separately per GPP variant (main: low<=0.679, middle<=0.91;
  GPP_DT_REF: its own, separate breaks) — explicitly NOT the 62-site project's fixed 0.63/0.88
  cutoffs, which were themselves terciles of a different (smaller, differently-selected)
  population and would not be terciles of this one.
- **Share of annual GPP outside the season** = `1 - mean F` at Z17/T95; T95's is ~5% by
  construction (`t95_mean_f`, newly exposed from `derive_one()` — see below).
- **Season start/end/gap days**: start = first day of the first run of >=5 consecutive days with
  `gpp_use > T`; end = last day of the last such run; gap days = days below `T` strictly between
  start and end. A day with no gap-filled GPP value counts as "below" (same NA-as-failing
  convention used throughout this project). Computed at 5%, 10% (Z17), 20% of P99 and at each
  site's own T95. **Southern-hemisphere sites** (`location_lat < 0`) use a season-year running 1
  July (year Y) - 30 June (Y+1), labelled Y, and are only computed for a season-year when BOTH
  calendar years Y and Y+1 are in that site's own passing-year list; this is a deliberate
  simplification at record edges, not a data error. Sites with no day below threshold all year
  are flagged (`above_threshold_all_year`) rather than given an undefined start/end.
- **Bounds check**: `gsl_lower` = GSL if every QC-failing day (the fallback's own 0.25 test) is
  forced to count as below threshold regardless of its real GPP value; `gsl_upper` = the same,
  forced above. `spread = gsl_upper - gsl_lower` is, by construction, algebraically exactly
  `n_fail_days` (checked directly with `stopifnot`, not assumed) — and therefore **identical
  at Z17 and T95 for a given site-year**, since QC pass/fail does not depend on which GPP
  threshold is being tested. This is why Supplemental Figure S2a's two lines coincide exactly;
  documented in that figure's own legend so it does not read as a missing-series bug.
- **Carbon gain**: per-site OLS (trend sites, >=10yr) and pooled mixed models (`nlme::lme`,
  random site intercept + slope, REML) of annual GPP/NEP on GSL, on season start day-of-year, and
  on end day-of-year, by skewness third and by functional group, at Z17 and T95. For
  start/end-date regressions at Southern-hemisphere sites, the season-year label is joined to
  the SAME-NUMBERED calendar year's annual GPP/NEP/TA_F — a documented simplification, not an
  exact calendar match, affecting only the 14 Southern-hemisphere sites' start/end (not GSL)
  carbon-gain and temperature-sensitivity numbers.
- **Temperature sensitivity**: slope of GSL on annual mean `TA_F` (calendar-year mean); slope of
  start/end day-of-year on the mean `TA_F` over a **fixed, site-level** 60-day window ending the
  day before that site's own MEAN start/end date (averaged across its years) — the same window
  (by day-of-year) is re-evaluated against each year's own `TA_F` data, not that year's own
  particular start/end date, to avoid circularity. Per-site OLS and pooled mixed models as above.
- **Trends**: per-site OLS (trend sites) across a 5-20% of P99 sweep (1% steps, 16 points) plus
  T95 (via each site's own per-year `gsl_t95`, not a sweep). Pooled trends: `nlme::lme`, random
  site slope **+ AR(1) residuals** (`corAR1`), by third and by group, at every sweep point and at
  T95. **Years-to-detect each pooled trend**: Weatherhead et al. (1998)'s formula, generalised
  from their own 95%-confidence/90%-power "3.3" constant to this task's 95%/80% combination
  (`qnorm(0.975) + qnorm(0.80) = 2.801585`, replacing 3.3): `n* = [(Z * sigma_N / |slope|) *
  sqrt((1+phi)/(1-phi))]^(2/3)`. Per the prompt's "using each site's residual SD and lag-1
  autocorrelation" instruction, this is computed **per site within the pool, using the POOLED
  slope with that site's own sigma_N/phi** (from its own OLS year-residuals), then the figures
  and report show the **median** across the pool's sites — a specific interpretive choice for an
  otherwise underspecified instruction (a single pooled model has no one obvious "residual SD",
  since the pooled random-slope model already absorbs between-site variation into the random
  effect, not the residual), documented here rather than left implicit.
- **Aridity**: `data/snapshots/site_aridity.csv`'s `ai_value` (CGIAR Aridity Index), joined by
  `site_id`; all 151 sites have a value (none omitted from Figure 2b).

### Shared-code changes (`GSL_definition/code/03_derive.R`), each verified harmless

All four changes were verified by rerunning the unmodified 62-site driver (default
`gsl.skip_driver` unset) and diffing `tables/derived_site*.csv` against a pre-change copy —
byte-identical for the first three; the fourth only adds a new column, with every pre-existing
column confirmed byte-identical.

1. Wrapped the whole variant-grid driver (daily_extract.rds load + the 4-variant loop +
   every `write_csv`/`write_meta` call) in `if (!isTRUE(getOption("gsl.skip_driver")))`, so a
   caller can `options(gsl.skip_driver = TRUE); source(...)` to get `derive_one()` and its helper
   functions without re-running or overwriting the 62-site tables.
2. Added a `trend_site_ids` argument to `derive_one()` (default `GSL_SITES_GE10YR`, the 62-site
   project's own fixed 21-site list) — it was previously hardcoded, so a caller with a different
   >=10-year population (the expanded set's own 67) would have silently gotten the wrong CI
   inclusion decision.
3. **Bug fix**: `kelly_skewness()` crashed (`"missing value where TRUE/FALSE needed"`) on a site
   with zero non-NA input values, because `quantile(numeric(0), ...)` returns `NA` rather than
   erroring, and the function's `if (denom == 0)` check then received `NA`. None of the 62 sites
   hit this (CA-TP1 has `GPP_NT_VUT_REF` data but zero non-NA `GPP_DT_VUT_REF` — found running
   the expanded set's `dt` variant). Added the same `length(x) == 0` guard `g1_adjusted()` and
   `l_skewness()` already had.
4. **Bug fix**: the Z17 trend's `lm()` call could reach `lm.fit()` with 0 non-NA cases (same
   CA-TP1/`GPP_DT_REF` case: >=3 years on record but zero non-NA `gsl_z17` values), erroring
   `"0 (non-NA) cases"`. Added `sum(!is.na(gsl_z17_byyear)) >= 3` to the existing guard.
5. **Additive**: exposed `t95_mean_f` in `derive_one()`'s site-level output tibble (it was
   computed as a local variable, `t95_mean_gsl`'s sibling, but never returned) — needed for the
   expanded analysis's "share of annual GPP outside the season" at T95.

### Other bugs found and fixed during this analysis (expanded/code/ only)

- `pivot_wider()` with `values_from` naming only some of the non-key columns silently pulls every
  OTHER column into the implicit `id_cols`, splitting rows that should merge. Hit twice:
  `expanded/code/04_figures_main.R`'s Figure 4a date-shift panel (`mean_gap_days` left unlisted,
  unselected first — collapsed ~300 points down to 2, since gap days differ between the 5%/20%
  rows for the same site); the `season_wide` construction in `03_models.R` already avoided this
  by selecting only the exact columns needed first, which is now the standing pattern for every
  `pivot_wider()` call in `expanded/code/`.
- `bind_cols(tibble(), x)` with a **literal empty** `tibble()` (0 rows, 0 cols) forces the result
  to 0 rows — not a no-op, as it looks. Hit in `03_models.R`'s `trend_per_site` computation
  (`group_modify(~ bind_cols(tibble(), ols_site(...)))`), silently producing a 0-row table and a
  "0 of 0 trend sites have a sign change" message that looked like a (wrong) real finding rather
  than an empty result. Fixed by returning `ols_site()`'s tibble directly.
- `GSL_MARKER_COLORS`' names are `"Z17"`/`"T95"` (uppercase, the convention used everywhere in
  the 62-site figures), but `bounds_check.csv`'s `threshold_name` column is lowercase
  `"z17"`/`"t95"` (a different table's own convention, from `02_derive.R`'s `bounds_one()`).
  Passing the lowercase column straight into `scale_fill_manual(values = GSL_MARKER_COLORS)` in
  Supplemental Figure S2a had no matching names, so ggplot silently fell back to a single
  unmatched fill colour — caught only by the warning text ("No shared levels found...") and by
  the two overlapping, blended-colour histogram series looking wrong on inspection. Fixed by
  relabelling to uppercase before plotting. Separately, overlapping `geom_histogram(position =
  "identity")` fill is visually ambiguous even with matched colours (two semi-transparent series
  blend into a third colour); switched to `geom_freqpoly` (unfilled step lines), the standard fix
  for comparing two overlapping distributions.
- Found (not a bug, a genuine data property): the bounds-check spread is mathematically identical
  at Z17 and T95 for a given site-year, since it depends only on QC pass/fail, not the GPP
  threshold — Supplemental Figure S2a's two lines coincide exactly by construction; its legend
  text says so explicitly.
- `trend_per_site`/`trend_sweep`'s threshold labels are `"5pct"`...`"20pct"`/`"t95pct"` (no
  literal `"z17"` string); Supplemental Figure S4 and S5 originally filtered for
  `threshold_name == "z17"`, matching nothing, so every per-site Z17 point was silently `NA`
  (invisible in S4's forest plot — no black dots anywhere). Fixed to filter `"10pct"`.

### Scope reductions, documented rather than silently skipped

- **Supplemental Figure S5** ("Figures 3 and 6 with GPP_DT, and the thirds under GPP_DT") does
  not include a full GPP_DT_REF AR(1) pooled-trend refit (Figure 6's exact analogue — another
  ~136 `nlme::lme(..., correlation = corAR1(...))` fits). Implemented instead: the Figure-3
  analogue in full (sensitivity at Z17 vs. skewness, GPP_DT_REF's own recomputed thirds), and a
  check of whether each trend site's main-variant Z17 trend groups consistently by its
  GPP_DT_REF-variant third (i.e. whether the GPP variant changes which third a site falls into) —
  a cheaper but still substantive robustness check, with the gap stated explicitly in the figure's
  own legend and `notes`.
- **Trend pooled models' AR(1) + random-slope combination initially failed to converge for 95 of
  136 fits** (far more than the carbon-gain/temperature-sensitivity pooled models, which had zero
  failures) using a single optimizer and a 2-tier fallback (random slope -> random intercept,
  both still with AR1). Added a 6-tier fallback ladder (random slope/intercept x nlminb/optim x
  with/without AR1, most-faithful-to-the-request first), which brought non-convergence down to
  17 of 136 — every one of those 17 is the mixed-forest group's 2-trend-site shortfall (below the
  >=3-site floor for any pooled model, not a numerical failure). Each fit's `model_tier` and
  `note` record which tier succeeded, so a silently-degraded model is never indistinguishable
  from the one actually requested.

## Expanded-site analysis, session 2a (2026-10-30)

Full prompt: `logs/prompts/2026-10-30_gsl_expanded_session2a.md`; full report:
`expanded/docs/report_back_2026-10-30-session2a.md`. Implements Dave's decisions on the session 1
checks report (`expanded/docs/checks_report_2026-10-30.md`). Every change is scoped to
`GSL_definition/expanded/code/00_config.R` through `06_checks.R` -- `GSL_definition/code/` (the
62-site reproduction) was not touched this session. Each change is a named `EXP_CFG_*` switch in
`00_config.R`, defaulting to the new/decided behaviour (`a22e893` keeps the pre-session-2a state
for comparison).

**1. CUT fallback keyed to GPP, not just QC** (`EXP_CFG_CUT_FALLBACK_USE_GPP`): recomputed from
scratch in `01_select.R` (not patched onto the committed `selection_audit_rule_options_qc_fallback.csv`,
which is QC-only and out of scope to edit) -- `COUNT()` aggregates pulled directly from DuckDB per
population site, VUT chosen only if both `NEE_VUT_REF_QC` and `GPP_NT_VUT_REF` have >=1 non-NA
value, else CUT under the same test, else the site is dropped outright (a new, explicit category
a22e893 never had -- it always picked a column, even an unusable one). Rule 4 (R2, N=14) passing-
year counts are reused unchanged for every site whose fallback column didn't change (identical
column -> identical per-day pass/fail by construction); recomputed via a fresh calendar-grid pass
only for the 7 sites whose column flipped. Net effect: 153 sites (was 151); see item-1 table in
the report-back for the full recovered/changed/dropped list.

**2. Winter partitioning-artifact screen** (`EXP_CFG_WINTER_SCREEN`,
`EXP_CFG_WINTER_SCREEN_DT_PCT=10`): `GPP_NT_REF>0 & NEE_REF>=0 & TA_F<0 & GPP_DT_REF <
0.10*unscreened_P99` (all four non-missing) sets that day's `GPP_NT_REF` to 0 before every
downstream quantity in the `'main'` variant. `unscreened_P99` is each site's P99 computed from
the ORIGINAL, unscreened GPP_NT_REF -- a fixed reference, not recomputed after screening (avoids a
circular definition). A 4th `derive_one()` variant, `'main_unscreened'`, reruns the identical
pipeline on the unmodified column, kept alongside `'main'`/`'dt'`/`'negdelivered'` in every table
with a `variant` column -- this is how a22e893's numbers remain directly reproducible without a
second pipeline. Season boundaries, the bounds check and annual GPP (`02_derive.R` Steps B-D) all
switch their `daily_main` source to the screened extract when `EXP_CFG_WINTER_SCREEN=TRUE`, so
the screen is genuinely applied "before every derived quantity," not just in `derive_one()`'s own
GSL/skewness/P99 outputs.

**3. Skewness thirds**: recomputed on the final (item-1 site set, item-2 screened) `'main'`
distribution -- 0.666/0.907, vs session 1's 0.68/0.91 (computed before both changes). A dedicated
comparison table (`skewness_thirds_breaks.csv`) reports this delta for all three variants.

**4. Southern-Hemisphere season-year join** (`EXP_CFG_SH_SEASON_YEAR`): a new block in
`02_derive.R`'s Step D computes `annual_gpp`/`annual_nep`/`annual_mean_ta` over the Jul(y)-Jun(y+1)
window for the 16 SH sites (`location_lat<0`), replacing the calendar-year sums for those sites
only, using the identical inclusion rule as Step B's `season_for_site()` SH branch (both calendar
years must be in the site's own passing-year list). `03_models.R`'s `cg_data`/`ts_data` need no
change at all -- they already join `annual_inputs` by `(site_id, year)`, and `year` now correctly
means "season year" for SH rows without any special-casing downstream. 134 calendar-year SH
site-year rows become 86 season-year rows (48 dropped at record edges).

**5. Temperature sensitivity, GSL predictor** (no separate switch -- a direct data change in
`03_models.R`'s `ts_data`): `ta_window_avg_z17/t95 = (ta_pre_start + ta_pre_end) / 2`, replacing
`annual_mean_ta` as `TS_COMBOS`'s `gsl_on_ta_*` predictor. The annual-mean version is preserved as
`TS_COMBOS_ANNUAL`, fit with the same `fit_pooled_slope()` machinery, written to
`temperature_sensitivity_pooled_annualta.csv`/data column `annual_mean_ta` -- never plotted. Start/
end's own 60-day pre-window predictors are unchanged (the item said "existing window" for both).

**6. Local sensitivity window** (`EXP_CFG_LOCAL_SENS_HALFWIDTH_PCT=1.0`): `02_derive.R` recomputes
`local_sensitivity` directly from `fine_grid_main`/`fine_grid_dt`'s `mean_gsl` column (5000-point,
0.01%-step fine grid, already computed by the shared `derive_one()`) via its own centred-difference
loop (`sens_steps <- round(1.0/0.01) = 100` grid steps each side), overwriting the column in place
so every downstream consumer (Figure 3, `fine_grid_summary.csv`, Supplemental S3/S5) picks up the
wider window automatically. `GSL_definition/code/03_derive.R`'s own `WINDOW_STEPS=5`
(+/-0.05%) constant is untouched -- it is shared with the 62-site reproduction and out of scope.

**7. Trends**: `trend_pooled.csv` gains `mean_gsl_subset`/`slope_pct_per_decade` (`100 * slope *
10 / mean_gsl_subset`, each subset's own mean GSL as the denominator, not a network constant). A
new `years_to_detect_sweep.csv` sweeps `EXP_CFG_DETECT_TREND_GRID` (`seq(0.1, 1.0, by=0.1)`
days/yr) through the existing `weatherhead_years()` helper, at Z17/T95 by third/group -- reusing
`noise_by_site_threshold`'s already-computed per-site sigma_n/phi, no new per-site fitting
required. The years-to-detect-the-*observed*-trend column (`years_to_detect_median`, keyed to the
pooled slope itself) stays in `trend_pooled.csv` for reference but is no longer read by any figure
script.

**8. Bounds check redesign** (`EXP_CFG_BOUNDS_NEAR_PCT=0.02`): `bounds_one()` in `02_derive.R`
now computes `is_near = is_fail & abs(gpp_use - T_abs) <= 0.02*P99` per day; `gsl_lower` counts
all non-near days by their real value plus near days as below; `gsl_upper` is the same with near
days counted above. `spread = n_near_threshold_days` exactly (checked via `stopifnot`, replacing
a22e893's `spread = n_fail_days` identity check). `n_fail_days` (every QC-failing day, not just
near-threshold ones) is kept as a separate column for reference/continuity.

**9. Mixed forest excluded from trend subsets** (`EXP_CFG_TREND_DROP_GROUPS = c("mixed forest")`):
a new `TREND_SUBSET_DEFS <- SUBSET_DEFS[!(...)]` in `03_models.R`, used only by the trend-pooled
loop (`names(TREND_SUBSET_DEFS)` in place of `names(SUBSET_DEFS)`) and the new detectability
sweep -- carbon-gain and temperature-sensitivity pooled models still iterate the full
`SUBSET_DEFS` (including mixed forest, which already clears their >=3-site floor independent of
trend-site count). One bug found and fixed while wiring the sweep's inner `purrr::map_dfr(...,
.id="subset")`: passing `names(TREND_SUBSET_DEFS)` (an unnamed character vector) with `.id=`
makes purrr fall back to the integer row index as the id column (purrr's own documented behaviour
for an unnamed `.x`), silently producing a numeric `subset` column that then failed to `recode()`
against `SUBSET_LABELS` (a `recode.numeric()` "NAs introduced by coercion" warning, and every row
losing its subset label). Fixed with `purrr::set_names(names(TREND_SUBSET_DEFS))`.

### Figure bugs found while implementing the figure-spec changes

- **A `geom_text()` panel-letter layer in a second data frame, whose facet-variable column was a
  plain character rather than a factor matching the main layer's factor levels, silently
  re-sorted the WHOLE figure's facets alphabetically** (confirmed by rendering: Figure 6a's
  "GSL/Start date/End date" column order became "End date/GSL/Start date"; Figure 6c's
  thirds-then-groups order became fully A-Z; Figure 5b's "Start date/End date" columns swapped).
  ggplot2 reconciles a faceting variable's level order across every layer sharing that aesthetic;
  a character value in one layer forces the union back to default (alphabetical) ordering even
  though the other, larger layer explicitly set `levels=`. Fixed everywhere this pattern was used
  (Figures 1a, 5a/5b, 6a, 6c) by constructing each single-row "letter" data frame's facet column
  as an explicit factor with the SAME levels as the main plotting data, not a bare string.
- Figure 5b's first outlier-clamping design used the pooled model's own 95% CI range (padded 1.5x)
  as the "bulk" bound, which clamped 79 of 136 points (58%) -- far too aggressive, since per-site
  OLS slopes are inherently noisier than a pooled random-slope model's CI. Redefined "bulk" as the
  Tukey 1.5xIQR range of the PER-SITE slopes themselves (the same outlier convention session 1's
  check 2 already used), which clamps 58 of 536 points (~11%) across the full 4-predictor set --
  a much more defensible "most sites, a few flagged outliers" split.
