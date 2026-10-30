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
