# GSL site-selection rule options (2026-10-30)

Full prompt: `../../logs/prompts/2026-10-30_gsl_selection_rule_options.md`. Follow-on to the
site-selection audit (`selection_audit_2026-10-30.md`). Extends
`code/11_selection_audit.R` (Steps 8-10) and its tables (`tables/selection_audit_rule_options_*`)
— read-only on the repo-root DuckDB store (`dataset = 'FLUXMET'` only). **Does not change the
62-site GSL analysis, its code, or its existing outputs; no rule proposed here has been applied
to it.** This document stops for Dave's decision, per the prompt.

## Question

Starting from the manuscript's own rule (IGBP exclusions; daily `qc_passes(..., 0.25)` with NA
as failing; >= 5 passing years; rule 5's negative-GPP cap — all unchanged), what is the
least-relaxed change that recovers the long-record sites the prior audit found excluded, while
every one of the GSL 62's own 505 site-years still passes? Tropical sites are treated like any
other site throughout — no special-casing.

## Rules evaluated

All four rules share the daily 0.25 threshold, NA-as-failing, the full Jan1-Dec31 calendar grid
(a missing day fails, same convention as the first audit), the >= 5-year rule 4 gate, and rule
5 unchanged. They differ only in which QC/GPP column is tested and how a year's failing days are
tolerated:

- **R0** — the manuscript baseline: `NEE_VUT_REF_QC` only (no CUT fallback), every day of the
  year must pass. Identical to the first audit's `i_manuscript` variant; used here only as the
  reference point.
- **R1** — R0 plus the per-site VUT/CUT fallback exactly as `scripts/04_qc.R` already uses for
  the 62-site analysis: `NEE_VUT_REF_QC` if the site has any non-NA VUT QC, else
  `NEE_CUT_REF_QC`. Never mixed within a site. CUT sites use `GPP_NT_CUT_REF` wherever a GPP
  value is needed (R3's band, rule 5). Every population site has at least one of the two columns
  (checked: `qc_fallback` table, `stopifnot`) — there is no "neither" case to handle.
- **R2** — R1, but a year passes if its **total count** of failing days is `<= N`, for
  N = 3, 7, 14. This is a **flat day-count allowance**, not the first audit's `iv_gap*`
  variants (which tolerated the *longest consecutive run* of failing days, not the total count)
  — the two are different rules that happen to share similar labels; this report uses R2's flat
  count throughout.
- **R3** — R1, but a failing day is forgiven unconditionally if its gap-filled GPP lies
  **outside** the band `[0, 25% of the site's P99]`; a failing day **inside** that band still
  counts, up to an allowance of M = 0 or 3 such days per year. The band is deliberately the
  low-GPP range that brackets the GSL framework's own threshold grid (5-20% of P99, `T95` higher
  still — see `docs/framework.md`): a failing day far outside this range (very high or very low/
  negative GPP) cannot change whether that day sits above or below a season-boundary threshold,
  so it is forgiven regardless of count; a failing day near the threshold is the kind that could
  actually change a GSL estimate, so it is capped. **P99 per site** is computed from days that
  pass the daily test, in years that pass R1; if a site has no R1-passing year at all, P99 is
  computed from all its passing days instead (`p99_basis` column in
  `selection_audit_rule_options_p99.csv` states which — 283 of 507 population sites have >= 1
  R1-passing year, 224 fall back to the all-passing-days basis). A failing day with **no GPP
  value at all** (a missing calendar day) is conservatively treated as inside the band (counts
  toward M) — consistent with this audit's NA-as-failing convention, since there is no way to
  tell whether it would have fallen near the threshold.

## 1. Per-rule totals

| Rule | Sites kept | Site-years kept | Trend sites (>=10 yr) kept |
|---|---|---|---|
| R0 (manuscript) | 68 | 549 | 21 |
| R1 (+ CUT fallback) | 74 | 603 | 22 |
| R2, N=3 | 118 | 1,050 | 45 |
| R2, N=7 | 133 | 1,314 | 56 |
| R2, N=14 | 161 | 1,658 | 70 |
| R3, M=0 | 118 | 1,052 | 36 |
| R3, M=3 | 160 | 1,593 | 72 |

Source: `tables/selection_audit_rule_options_summary.csv`. "Kept" is gated on the >= 5-year rule
4 threshold throughout, same convention as the first audit.

## 2. GSL 62 containment — confirmed, with one exception

Every rule must contain the manuscript's own 505 GSL-62 site-years (`00_config.R::GSL_SITES`,
`docs/framework.md`'s Sites table). Checked directly, not assumed from the rules' nesting:

| Rule | Site-years checked | Passing | Missing/failing |
|---|---|---|---|
| R0 | 505 | 504 | **1** |
| R1 | 505 | 504 | **1** |
| R2, N=3/7/14 | 505 | 505 | 0 |
| R3, M=0/3 | 505 | 505 | 0 |

Source: `tables/selection_audit_rule_options_gsl62_check.csv`. The one exception at R0/R1 is
**IT-Tor 2012** (1 failing day, longest failing run of 1 day) — this is not a new finding: it is
the exact, already-documented anomaly in `00_config.R` ("IT-Tor 2012 is kept... despite failing
Stage 1's 'complete year' reproduction check"). It recovers at every R2/R3 setting tested
(any nonzero daily-failure allowance covers a single bad day).

**This means R1 alone — the fallback with zero added tolerance — does not satisfy the "contains
the GSL 62" requirement.** Some nonzero day-tolerance (R2 at N >= 1, or R3 at M >= 1) is the
minimum needed to keep the GSL 62 intact; of the settings tested, R2 at N=3 is the least-relaxed
one that both fixes this and adds any long-record recovery.

## 3. Long-record site recovery

The 37 long-record sites not in the GSL 62 (`selection_audit_population.csv`): how many are
kept under each rule (cumulative, not "newly added" — R0's own count of 2 reflects sites that
already pass the manuscript rule without any change, see `selection_audit_2026-10-30.md` §6).

| Rule | Long-record recovered (of 37) | Still lost |
|---|---|---|
| R0 | 2 (GF-Guy, US-Aud — already pass, not a GSL-62 membership question this audit resolves) | 35 |
| R1 | 7 | 30 |
| R2, N=3 | 22 | 15 |
| R2, N=7 | 30 | 7 |
| **R2, N=14** | **34** | **3** |
| R3, M=0 | 22 | 15 |
| R3, M=3 | 30 | 7 |

Source: `tables/selection_audit_rule_options_summary.csv` (full recovered/still-lost site lists
per rule). **R2 at N=14 recovers every long-record site that is recoverable under any rule
tested** — no other rule/setting combination, including R3 at M=3, recovers a site that R2,
N=14 misses.

### Still lost under every rule tested

Three long-record sites never reach 5 passing years under any rule, R1 through the loosest R2/R3
settings tested:

| Site | R1 (yr) | R2, N=14 (yr) | R3, M=3 (yr) | Why |
|---|---|---|---|---|
| BR-Ma2 | 0 | 2 | 0 | Has `GPP_NT_VUT_REF` QC data, but **zero non-NA GPP values at all** across its 18-year record — every failing day's GPP is NA, so under this audit's conservative NA convention every failing day is classified "inside the band" for R3, which collapses R3 to R1 for this site (no actual banding is possible). Even the flat day-count allowance (R2) recovers only 2 of 18 years (1999-2016, failing-day counts ranging 4-360/yr; the 2 years under 14 are 2007 at 11 days and 2009 at 4 days) — far short of 5. |
| US-ICh | 0 | 0 | 2 | 19 years of daily data, but failing days are both frequent (20-262/yr) and, under R3, mostly classified outside the band (real GPP data, genuinely high/low values most of the time) — R3 recovers 2 years at M=3, R2 recovers none even at N=14 (minimum annual failing-day count across its record exceeds 14). |
| US-WCr | 0 | 3 | 2 | 27 years of daily data; several years fail essentially every day (2007-2009, 2020: 331-366 failing days), and even its better years exceed both tolerances most of the time. Best case is 3 passing years (R2, N=14) — still short of 5. |

None of these three can be recovered by loosening further within the R2/R3 families tested here
without either more years of adequate data or a materially larger tolerance than was in scope
for this task.

### The 10 sites recovered specifically by the CUT fallback

Of the 34 recovered long-record sites, 10 have no VUT QC data at all and are recovered only
because R1's fallback gives them a CUT-gated QC/GPP column to test in the first place: **AU-Tum,
CA-Ca3, CA-LP1, DE-Lnf, FR-Hes, US-Ho2, US-NR1, US-Syv, US-Uaf, US-Whs**. This is 10 of the 11
sites the first audit found permanently excluded by the literal VUT-only rule — **US-WCr is the
one exception that remains excluded even with the fallback** (see table above: 3 passing years
at best, still short of 5).

## 4. R3 band-by-month (failing days only, whole population)

| Month | Inside band (counts toward M) | Outside band (always forgiven) |
|---|---|---|
| Jan | 28,940 | 5,018 |
| Feb | 25,194 | 3,666 |
| Mar | 25,235 | 3,510 |
| Apr | 21,041 | 3,835 |
| May | 18,070 | 5,149 |
| Jun | 14,778 | 6,266 |
| Jul | 14,237 | 6,731 |
| Aug | 13,386 | 6,167 |
| Sep | 13,697 | 4,921 |
| Oct | 16,993 | 4,113 |
| Nov | 19,790 | 3,641 |
| Dec | 23,366 | 4,754 |

Source: `tables/selection_audit_rule_options_r3_band_by_month.csv`. Numbers only, no
interpretation beyond what is already needed above (most failing days, in every month, fall
inside the low-GPP band and so count toward R3's M allowance rather than being forgiven — this
is the main reason R3 does not out-perform R2 in §5 below).

## 5. Rule 5 (negative GPP) re-check

Checked for every site newly kept under any of R1-R3 that was not already kept under R0 (104
sites, using each site's own fallback GPP column — `GPP_NT_VUT_REF` or `GPP_NT_CUT_REF`):

- **0 of 104 would fail rule 5.**
- 5 have no GPP data at all and are undetermined (NA), not FALSE: AU-Stp, ES-LJu, ID-PaD,
  JP-Khw, JP-Ta2 — none of these is one of the 34 recovered long-record sites.

Source: `tables/selection_audit_rule_options_rule5_check.csv`. No newly-added site, long-record
or otherwise, is one the manuscript would instead have dropped for excess negative GPP.

## 6. Recommendation

**R2 at N=14 (per-site VUT/CUT fallback + a flat allowance of up to 14 failing days per year)**
is the recommended rule, for three reasons:

1. **It recovers every long-record site that is recoverable at all.** 34 of 37; the remaining 3
   (BR-Ma2, US-ICh, US-WCr) are not recoverable under any rule tested, for the data-coverage and
   bursty-failure reasons given in §3 — no looser R2/R3 setting closes that gap.
2. **R2 dominates R3 at every matched level of long-record recovery.** Comparing the number of
   *newly* recovered long-record sites (recovered minus the 2 that already pass under R0) against
   the collateral cost (sites newly kept that are *not* among the 37 target long-record sites),
   derived from §1 and §3's tables:

   | Setting pair (same long-record recovery) | Newly recovered long-record | Collateral (non-target) sites added |
   |---|---|---|
   | R2, N=7  vs.  R3, M=3 | 28 vs. 28 (tied) | **37** vs. 64 |
   | R2, N=14 vs. R3, M=3  | **32** vs. 28 (R2 wins) | 61 vs. 64 |

   At the same recovery (N=7 vs. M=3), R2 brings in roughly half the collateral; at R2's maximum
   setting (N=14), R2 recovers *more* long-record sites than R3's maximum setting (M=3) while
   still adding *fewer* collateral sites. R3 is not preferred at any tested setting.
3. **R2 is the simpler rule.** It needs no per-site P99, no choice about which years' data feed
   that P99, and no degenerate-case handling for sites with no GPP data at all (§3's BR-Ma2 case,
   where R3 silently collapses to R1). A flat day-count is also easier to justify in a Methods
   section than a GPP-band exception.

**The floor:** R1 alone (fallback, zero added tolerance) is not viable regardless of the
long-record question, because it does not contain the GSL 62's own IT-Tor 2012 site-year (§2).
Some nonzero tolerance is required either way; R2 at N=3 is the least-relaxed setting tested that
restores full GSL-62 containment, but it recovers only 22 of the 37 long-record sites — short of
the 34 that are recoverable at all. Recommending N=14 rather than N=3 or N=7 is a judgement call
about how much of the recoverable set matters enough to justify a 14-day annual allowance, not a
repeat of the GSL-62 floor requirement (which only N>=1 is needed for).

**Not recommended:** adopting R3 in place of R2 at any setting tested, for the reasons in point 2
above. If a looser R3 (M > 3, untested here) is of interest because the GPP-band framing is
scientifically preferred for other reasons, that would need a fresh run — this task tested only
M = 0 and 3.

This analysis does not itself change the 62-site GSL pipeline or its code. If R2 at N=14 (or any
other setting) is adopted, that is a separate, explicit next step for Dave to authorize.
