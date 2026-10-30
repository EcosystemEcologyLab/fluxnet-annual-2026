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

## Stop

Stopped after this memo, per the resume instruction ("stopping before step 4" = framework
run-order step 4, `03_derive.R`, Stage 2).
