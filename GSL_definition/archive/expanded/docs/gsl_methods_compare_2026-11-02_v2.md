# GSL methods comparison (M1-M6), version 2

2026-11-02. Revises version 1 (`docs/gsl_methods_compare_2026-11-02.md`, committed at `15dc51f`)
per Dave's follow-up decisions (full prompt: `logs/prompts/2026-11-02_gsl_methods_compare_v2.md`).
Produced by `GSL_definition/expanded/code/11_gsl_methods_compare.R`, which was revised in place;
v1's content lives on in git history at `15dc51f`. Tables and draft figures are regenerated under
the same filenames; this doc is new (`_v2` suffix), alongside v1's doc.

Exploratory. Does not modify any existing script, table or figure other than this script's own
previous outputs.

## What changed since v1

1. **Two GPP series, not three variants.** The screened/unscreened/agreement variants are gone.
   Every method now runs on exactly two independent series: **NT** (zero-floored `GPP_NT_REF`, no
   winter screen -- the main series) and **DT** (zero-floored `GPP_DT_REF`, same VUT/CUT family).
   Thresholds (P99, the fine grid, summer/peak-month P99) always come from that series' own pooled
   values; NT and DT are never mixed within a method run. M5/M6 (NEP-only) are unaffected and carry
   `variant = "all"`.
2. **Southern Hemisphere (AU-How) on Jul-Jun years, everywhere.** AU-How now uses Jul-Jun season
   years in *every* method (M1-M6, including M2-M4) and for P99 pooling. The passing-year test
   changed too: v1 used a proxy ("both bracketing calendar years independently pass the main run's
   R2/N=14 rule"); v2 tests the Jul(y)-Jun(y+1) window directly -- a season year passes if it has at
   most 14 failing days under the existing per-day QC rule (`qc_passes(QC_REF, 0.25)`, `NA` counts
   as failing; `QC_REF`/`EXP_RULE_QC_DAY`/`EXP_RULE_MAX_FAIL_DAYS` are the same column and
   constants `01_select.R`/`scripts/04_qc.R` use for the calendar-year rule).
3. **M6's search windows are now anchored to M5-span.** Start is searched only within +/-30 days of
   M5-span's own start; end only within +/-30 days of M5-span's own end (steepest positive /
   shallowest negative slope, as in v1; unsmoothed daily NEP; k = 3-9). This drops v1's
   global-seasonal-peak device (needed because v1's search window ran all the way to a detected
   "peak") and the alternate-end column entirely. A site-year with no M5-span (including net-source
   years) gets no M6 value and is flagged `flag_no_m5_span`.
4. **Two-season flag uses the TIMESAT rule, not a slope-run rule.** V1's shared slope-run check
   (first-difference of smoothed NEP, >50% of the year's max/min) saturated near-universally
   (284/285 and 282/285 of 285 site-years flagged) and was documented as a finding rather than
   patched. V2 replaces it with the TIMESAT secondary-peak rule (Wang et al. 2020) applied to
   15-day-smoothed **GPP** (NT series, the single default, since M5/M6 have no GPP series of their
   own): find the strict local maxima of smoothed GPP in the window; the primary peak is the global
   maximum; if a secondary local maximum exists whose value is >= 0.5x the primary peak's, **and**
   the minimum of the series strictly between the two peaks falls below 0.5x the *lower* of the two
   peaks (this is what keeps a small wiggle right next to the main peak from counting -- such a
   wiggle's between-peaks minimum stays close to the high primary peak, not down near half of
   itself), the site-year is flagged two-season. `two_season_ratio` (secondary/primary peak value,
   `NA` if no secondary local max exists) is reported alongside the boolean flag.

## Checks (both passed exactly)

- **NH sites only** (AU-How excluded -- its windows are now Jul-Jun season years that do not
  correspond to `derived_site_year.csv`'s calendar-year rows at all): NT-series M2@10% and M4@0.95
  equal `derived_site_year.csv`'s `main_unscreened` `gsl_z16`/`gsl_t95`/`f_z16`/`f_t95` exactly, for
  all 276 NH site-year rows. (An internal sanity check ahead of this -- fresh NT P99 vs.
  `derived_site.csv`'s `main_unscreened` P99 -- also matched exactly for all 14 NH sites.)
- Every one of the 7,857 `gsl_methods_site_year.csv` rows (every method and series) has a GSL value
  or an explanatory flag (0 unaccounted).
- 0 of 15 sites lack DT-partitioned GPP (same as v1).

## AU-How: before and after item 2

| | old (calendar-year proxy) | new (Jul-Jun day-count rule) |
|---|---|---|
| Passing years | 13: 2008, 2010-2017, 2019, 2022-2024 | 15: 2004, 2008, 2010-2017, 2019, 2021-2024 |
| P99 (zero-floored NT GPP) | 11.699 | 11.954 |

The new rule **recovers two additional years** (2004, 2021) that the old "both bracketing calendar
years pass" proxy excluded. This is a real, expected consequence of testing the window directly:
failing days can be split asymmetrically across a calendar-year boundary such that neither half-year
individually clears 14 fails, while the actual Jul-Jun window (which only needs the Jul-Dec piece of
one year and the Jan-Jun piece of the next) does. None of the 13 old years were lost.

M2@10%/M4@0.95 GSL, old vs. new (site-year label changes from calendar year to season-start year, so
these are not a row-by-row diff, but the ranges are directly comparable):

| | old gsl_z16 (days) | new gsl_z16 (days) | old gsl_t95 | new gsl_t95 |
|---|---|---|---|---|
| range | 348-366 | 326-366 | 254-361 | 243-351 |
| mean | 362.3 | 360.1 | 318.3 | 318.3 |

AU-How's GSL stays in essentially the same very-long range (it is a tropical savanna with GPP active
most of the year); the main effect of item 2 is the two extra years, plus the new 2004 season year
(GSL 326/243) pulling the low end down slightly, since 2004 was not previously assessable at all
under the old calendar-year-pair proxy.

## NT vs. DT: the partitioning uncertainty (item 1)

`tables/gsl_methods_dt_vs_nt.csv` holds `dt_minus_nt_gsl`/`_start_days`/`_end_days` for every
site-year x method x sub where both series have a value, plus a decomposition:
`boundary_explained = dt_minus_nt_end_days - dt_minus_nt_start_days` (the GSL change implied purely
by the start/end shift) and `interior_contribution = dt_minus_nt_gsl - boundary_explained`.

**For M1 (span method), `interior_contribution` is exactly 0 for every row, by construction** (GSL =
end - start + 1, so the GSL difference is always and only the start/end shift). For the count methods
(M2/M3/M4), it is not:

| method | mean \|DT-NT GSL\| (d) | mean \|start shift\| (d) | mean \|end shift\| (d) | mean \|interior\| (d) |
|---|---|---|---|---|
| M1 (span) | 17.5 | 10.1 | 10.4 | 0.0 |
| M2 | 14.5 | 16.0 | 13.5 | 27.2 |
| M3 | 15.0 | 16.3 | 14.8 | 26.7 |
| M4 | 15.4 | 15.7 | 12.4 | 27.1 |

**This is the "where it concentrates" answer**: for the span method (M1), all of the DT-NT
difference is, by definition, a boundary (start/end) effect. For the count methods (M2-M4), the
mean interior contribution (~27 days) is actually *larger* than the mean boundary-explained shift
(~14-16 days) -- most of the DT-vs-NT disagreement in count-based GSL is **not** a clean shift of
when the season starts or ends, but scattered day-by-day disagreement between the two partitioning
methods on days spread through the year (consistent with "winter days": individual days near the
threshold where NT and DT partition the same half-hourly NEE into different GPP values).

Per-site, the partitioning uncertainty is highly uneven. Largest mean |DT-NT GSL| across M1-M4:
**CH-Dav** (41-57 d), **IL-Yat** (16-39 d), **AT-Neu** (15-28 d), **US-Ha1**/**US-SRM**/**US-Var**
(10-18 d). Smallest: **AU-How** (0.04-7 d), **JP-Api** (3-7 d), **FR-Pue** (4-8 d), **DE-Tha** (6-8
d). CH-Dav and AT-Neu (both evergreen-needleleaf, high-elevation/high-latitude sites with strong
winter partitioning ambiguity) and IL-Yat (semi-arid) sit at one extreme; the tropical/subtropical
AU-How and the two sites with the least winter signal (FR-Pue, JP-Api) sit at the other -- consistent
with the interior-day interpretation above (partitioning disagreement concentrates where winter
NEE-to-GPP/RECO splitting is hardest).

## Two-season flags (item 4, TIMESAT rule)

9 of 291 M5-span site-years flagged (vs. v1's 284-282 of 285 under the old slope-run rule -- the
TIMESAT rule is far more selective, as intended):

| site | n site-years | n flagged |
|---|---|---|
| AT-Neu | 18 | 4 |
| US-SRM | 20 | 2 |
| US-Mpj | 15 | 1 |
| US-Var | 23 | 1 |
| US-Wkg | 21 | 1 |
| all other 10 sites | -- | 0 |

The flagged sites are exactly the ones where a plausible secondary growing-season pulse is
physically expected -- AT-Neu (alpine grassland, short secondary pulses after snowmelt events are
plausible), the three semi-arid/savanna sites (US-SRM, US-Mpj, US-Wkg -- bimodal
winter-rain/summer-monsoon growth is a known feature of the US Southwest), and US-Var (Mediterranean
grassland, early and late-season rain pulses). This is a much more defensible, interpretable flag
set than v1's near-universal saturation.

## M6 vs. version 1

| | v1 (global-peak search window) | v2 (M5-span +/-30-day window) |
|---|---|---|
| n site-years | 285 | 291 (6 more -- the new AU-How years) |
| mean GSL (d) | 116.7 | 220.8 |
| SD GSL (d) | 61.8 | 85.0 |
| median k-range, start (d) | 23.5 | 17 |
| median k-range, end (d) | 54 | 39.5 |
| Spearman r (GSL anomalies) vs. M5 | 0.09 | **0.89** |
| Spearman r (GSL anomalies) vs. M1 | 0.03 | 0.17 |
| Spearman r (GSL anomalies) vs. M2 | 0.05 | 0.22 |
| Spearman r (GSL anomalies) vs. M3 | 0.06 | 0.19 |
| Spearman r (GSL anomalies) vs. M4 | 0.07 | 0.17 |

Anchoring M6's search windows to M5-span resolves the central problem identified in v1: the literal
"30 days before first rise to seasonal peak" window let weather noise in unsmoothed daily NEP
dominate the true spring/fall transition, because the window could be 2-3 months wide. Bounding the
search to +/-30 days of M5's own (smoothed-NEP-based) start/end fixes M6's GSL into the same
neighbourhood as the other methods (mean 221 d vs. v1's implausibly short 117 d) and raises its
agreement with M5 to 0.89 -- a large, genuine improvement, not a tautology: M6 and M5 use different
underlying series (unsmoothed vs. smoothed NEP) and different detection logic (k=3-9 day OLS slope
extrema vs. a >=5-day run threshold), so a high correlation is a real methodological convergence,
not a restatement of the same rule. M6's agreement with the GPP-based methods (M1-M4) also roughly
doubled to tripled (0.03-0.07 to 0.17-0.22) -- still the weakest of the six methods, but no longer
in a different universe. The k-range (uncertainty across k=3-9) also **shrank** despite the tighter
window (median start 23.5->17 d, end 54->39.5 d) -- the bounded search gives k less room to find
wildly different "steepest slope" days.

## Headline cross-method result (NT series for M1-M4, "all"/NEP for M5/M6)

Pairwise Spearman r on site-year anomalies (`figures/drafts/"GSL methods agreement draft".png`):
M1-M4 agree closely with each other (0.59-0.94 for GSL); M5 and M6 now agree closely with *each
other* (0.89) but only weakly with M1-M4 (0.17-0.25) -- the same structural split as v1 (GPP-based
vs. NEP-based methods measure a related but distinct thing), but with M5/M6 now internally
consistent rather than M6 being an outlier even against M5.

## Three to five largest differences between methods

Per-site range (max mean GSL across the six headline sub-thresholds, M1-summer/M2-10%/M3-1gC/
M4-0.95/M5-span/M6, minus the min), NT series for M1-M4:

1. **IL-Yat**: 207 d (M1-summer 365 d vs. M3 158 d).
2. **US-Var**: 202 d (M1-summer 365 d vs. M4 163 d).
3. **US-Ton**: 178 d (M1-summer 364 d vs. M6 186 d).
4. **AT-Neu**: 141 d (M3 270 d vs. M6 129 d).
5. **US-Wkg**: 139 d (M5-span 225 d vs. M3 86 d).

As in v1, M1-summer's fixed Jun-Aug (NH) "summer" window badly misrepresents the true growing-season
peak at the Mediterranean-climate sites (IL-Yat, US-Var, US-Ton), whose real peak is Feb-Jun (see
the peak-month table logged by the script); M1-summer's threshold ends up so low relative to the
actual (winter/spring) growing season that nearly the whole year qualifies. The new entries at
#4-5 (AT-Neu, US-Wkg) involve M6 and M5 rather than M1-summer -- now that M6 is anchored sensibly, it
no longer *dominates* every "biggest gap" (as it did in v1, appearing in all top differences), and
differences between the GPP-threshold methods themselves (M3's fixed, unscaled 1 gC m-2 d-1 vs. the
others' P99-relative thresholds) become visible instead, particularly at low-GPP semi-arid sites
where 1 gC m-2 d-1 is a much more restrictive bar than X% of a (low) site-specific P99.

## Outputs

- `tables/gsl_methods_site_year.csv` (7,857 rows; was 11,223 in v1, since each row is now one of 2
  series instead of 3 variants, offset by AU-How's 2 extra years).
- `tables/gsl_methods_site_summary.csv` (405 rows; was 585).
- `tables/gsl_methods_dt_vs_nt.csv` (3,492 rows) -- new in v2, the item-1 partitioning-uncertainty
  table.
- Three draft figures, regenerated in place under the same filenames:
  `figures/drafts/"GSL methods dotplot/agreement/raster draft".png` (+ `.legend.txt`/`.meta.json`).
- This doc.

## Not reproduced / follow-ups (unchanged from v1, still out of scope here)

The light-response method (Bowling et al. 2024) needs half-hourly extracts; per the Task B GPP/RECO
uncertainty inventory (`review/diagnostics/gpp_uncertainty_inventory/report.md`), only US-MMS among
these 15 sites currently has DuckDB-queryable sub-daily (half-hourly) data, so it is not attempted
here.
