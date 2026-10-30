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
