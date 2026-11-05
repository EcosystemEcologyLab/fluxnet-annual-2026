# GSL methods comparison (M1-M6): rules, choices, checks, findings

2026-11-02. Full prompt: `../../logs/prompts/2026-11-02_gsl_methods_compare.md`. Code:
`code/11_gsl_methods_compare.R` (standalone; does not modify any existing script, table or
figure). 15 sites (`tables/iav_15sites.csv`'s own list): AU-How, NL-Loo, DE-Tha, FR-Pue, CH-Dav,
US-MMS, US-Mpj, AT-Neu, US-Ha1, JP-Api, IL-Yat, US-Ton, US-Var, US-Wkg, US-SRM. Every R2/N=14
passing year used. Post-Z16-rename (commit `d43ae22`) column names throughout.

## Data and GPP variants

A fresh, read-only DuckDB pull (`dataset = 'FLUXMET'`) for exactly these 15 sites --
`GPP_NT_VUT/CUT_REF`, `GPP_DT_VUT/CUT_REF`, `NEE_VUT/CUT_REF` -- resolved to each site's own
VUT/CUT family via `sites.csv$gpp_col_used` (all 15 of these sites are VUT; the CUT branch is
implemented but not exercised here). The committed `winter_screen_days.csv` is applied to build
the **screened** series (main run's winter-screened `GPP_NT_REF`, zero-floored); the **unscreened**
series is the same column without that screen, zero-floored; the **agreement** series is
`pmin(zero-floored GPP_NT_REF, zero-floored GPP_DT_REF)` -- **all 15 sites have DT GPP** (checked
directly: 0 of 15 have zero non-NA `GPP_DT_REF`), so the NT-only fallback path is implemented but
unexercised for this site set. NEP = `-NEE_REF` (not zero-floored -- legitimately negative,
needed by M5/M6), identical across all three GPP variants.

**Correctness check (passed exactly, not approximately):** freshly computed P99 (screened and
unscreened) matches `derived_site.csv`'s own P99 for all 15 sites to the last bit. This confirms
the DuckDB pull, VUT/CUT resolution, winter screen and zero-flooring reproduce the main run
exactly before any new method logic is applied.

## The central interpretation choice: calendar year vs. Jul-Jun season year

The prompt's data-handling rules list both "as the main run" and "Southern Hemisphere sites on
Jul-Jun season years" without resolving a tension between them: `GSL_definition/code/03_derive.R`'s
own `derive_one()` -- the literal "main run" -- pools **calendar-year** windows for P99, skewness
and every GSL(T)/F(T) count, with no Jul-Jun adjustment anywhere; the Jul-Jun SH treatment is a
later, separate addition applied only to `season_boundaries.csv`/`annual_inputs.csv`/
`carbon_gain_data.csv`/`iav_15sites.csv` (2a item 4), not to `derive_one()` itself.

This script resolves the tension as follows, confirmed correct by the checks below:

- **M2, M3, M4** (day-count and cumulative-fraction methods): **calendar-year** windows
  throughout, for every site including AU-How -- this is required to exactly reproduce
  `derived_site_year.csv`'s `gsl_z16`/`gsl_t95`/`f_z16`/`f_t95`, and does (checked).
- **M1, M5, M6** (span/continuous-NEP methods): **Jul-Jun season years** for AU-How (the one
  Southern-Hemisphere site), calendar years elsewhere -- these methods detect a single
  start/end within one window, and a Northern-Hemisphere-style Jan-Dec window would artificially
  split a Southern-Hemisphere growing season across two site-years.
- AU-How's season years are restricted to calendar-label `y` where **both** `y` and `y+1` are
  themselves R2/N=14 passing years (same rule as `02_derive.R` item 4 / `10_figure1_alts.R`'s
  `effective_years()`) -- 9 of AU-How's 13 passing calendar-year labels qualify.
- The **annual GPP/NEP** used for every detrended-r correlation (in the summary table, and for
  the correlation check below) always uses the season-year-aware total (`annual_gpp_nep_corr_for()`,
  mirroring `carbon_gain_data.csv`'s own convention) -- **not** the calendar-year `annual_gpp`
  column carried in `gsl_methods_site_year.csv`, which exists only as M2/M3/M4's own F(T)
  denominator, matching `derived_site_year.csv`'s `f_z16`/`f_t95` convention exactly.

## Checks (all passed exactly)

1. **M2 at 10% == `gsl_z16`/`f_z16`, M4 at 0.95 == `gsl_t95`/`f_t95`**, for screened and
   unscreened variants, every one of 578 site-year rows, zero difference.
2. **Screened-variant detrended r (M2@10%, M4@0.95) vs. annual GPP == `iav_15sites.csv`'s
   `r_gpp_z16`/`r_gpp_t95`**, all 15 sites, zero difference.
3. **Every one of 11,223 rows in `gsl_methods_site_year.csv` has either a non-NA `gsl` or an
   explanatory flag** (`flag_no_season` or `flag_net_source`) -- checked, 0 unaccounted rows.

## M1 (Zscheischler et al. 2016 crossing-span): implementation choices

- **Three thresholds, all 10% of a P99**: `summer` (P99 of daily GPP in the fixed calendar
  months Jun-Aug for Northern-Hemisphere sites, Dec-Feb for AU-How, pooled across passing
  calendar years), `peak` (P99 of the site's own 3 consecutive wrapping calendar months with
  the highest mean GPP -- computed per site, reported below), `annual` (the same per-variant P99
  used throughout M2-M4).
- **Crossing detection**: within a (season-)year window, `start` = the first day the smoothed
  series is above threshold that was preceded by a below-threshold day (flagged
  `flag_no_start_crossing` if the window's first day is already above -- no visible crossing
  in-window); `end` = the mirror image at the window's end (`flag_no_end_crossing`). If no day is
  above threshold, `flag_no_season`; if every day is, `flag_whole_year`. GSL = `end - start + 1`
  (can exceed the count of above-threshold days if there are brief dips between start and end --
  expected for a span method, not a bug).
- **Peak 3-month window per site** (unscreened GPP, reused for agreement; screened GPP used for
  the screened-variant threshold): AU-How Dec-Jan-Feb, NL-Loo/CH-Dav/US-MMS/US-Ha1/JP-Api Jun-Jul-Aug,
  DE-Tha/AT-Neu May-Jun-Jul, FR-Pue/US-Ton Apr-May-Jun, US-Mpj/US-Wkg/US-SRM Jul-Aug-Sep,
  IL-Yat/US-Var Feb-Mar-Apr.
- **Finding -- M1-summer badly overestimates GSL at water-limited/Mediterranean-climate sites.**
  At US-Var, IL-Yat and US-Ton, the fixed Jun-Aug "summer" window falls in that site's *dry*,
  low-GPP season (their real peak is Feb-Jun), so the summer-pooled P99 is low and 10% of it is
  an easily-exceeded threshold almost year-round: mean M1-summer GSL is 354 (US-Var), 365
  (IL-Yat, i.e. every year flagged `flag_whole_year`) and 356 days (US-Ton), vs. 212/329/326 days
  for M1-peak at the same three sites -- gaps of 142, 36 and 30 days purely from which 3 months
  "summer" is defined as. This is exactly the problem M1-peak (and M1-annual) exist to avoid, and
  is reported here as a finding about the fixed-calendar-month convention, not fixed in the
  script (M1-summer is implemented exactly as specified).

## M5 (carbon-uptake period, smoothed NEP > 0)

`count` = days with smoothed NEP > 0; `span` = first day of the first >=5-day run to last day of
the last >=5-day run, both within the (season-)year window. **Net-source years** (fewer than 5
days of smoothed NEP > 0) are flagged and excluded from M5/M6 site means and their own
detrended-r -- **1 of 285 site-years** (US-SRM, one year), logged 3 times in
`expanded/logs/exclusion_log.csv` (once each for M5-count, M5-span and M6, since the flag applies
to all three method rows for that site-year); see `gsl_methods_site_summary.csv`'s
`n_flag_net_source` column for the per-site breakdown.

### The "two-season" rule: implemented as specified, found to be uninformative as specified

The prompt defines distinct rise/decline episodes via "separate runs where the slope exceeds 50%
of that year's maximum [or 50% of the minimum, for declines]." No slope definition is given for
this specific check (M6 has its own, picked per k below; M5 has none of its own). This script
uses the simplest available common series -- the first difference of smoothed NEP -- and
implements the rule exactly as stated (no additional merging/smoothing of the resulting runs).
**Result: 284 of 285 site-years flag multiple rise runs, 282 of 285 flag multiple decline runs**
-- i.e. the literal rule, on this slope definition, flags nearly every site-year, not the "rare,
genuinely bimodal site" signal the rule is presumably meant to surface. Day-to-day NEP is noisy
even after 15-day smoothing of the level (smoothing the level does not smooth its *derivative* by
the same amount), so many short runs exceed 50% of a single, often noise-driven, annual
max/min slope. Tried and rejected as not fixing this: merging runs within a 3-30 day gap
(reduces but does not resolve the saturation -- e.g. a 14-day merge gap still leaves 266/285 and
278/285 flagged). **Reported as a finding, not silently patched**: this specific flag, as
specified, does not usefully distinguish genuinely multi-season site-years from noise at these 15
sites with this slope definition; `n_flag_two_season_rise`/`n_flag_two_season_decline` in
`gsl_methods_site_summary.csv` should be read with this in mind, not as a reliable "this site has
two growing seasons per year" signal.

## M6 (Hu et al. 2010 NEP rate-of-change): implementation choices and a key finding

- **k-day OLS slope on UNSMOOTHED daily NEP**, k = 3 to 9, centred window (`floor((k-1)/2)`
  days before, the remainder after, for even k).
- **"First/last seasonal peak of smoothed NEP"**: tried several local-maxima + prominence/valley
  definitions (an absolute threshold at 50% of the window's global max; the same threshold with a
  30-90 day minimum peak-to-peak distance; a true saddle/valley depth test relative to either the
  lower of the two candidate peaks or the global max) -- **all of them flagged the large majority
  of site-years as multi-peaked** (e.g. 9-25 of a site's passing years, out of 9-27). Smoothed NEP,
  unlike smoothed GPP, is not reliably unimodal: short dry spells or cloudy stretches create many
  local maxima that satisfy any reasonably permissive prominence threshold. This script therefore
  uses the **single global maximum** of smoothed NEP in the window as both "first" and "last"
  seasonal peak -- a documented simplification, not a claim that these sites are strictly
  unimodal.
- **Start** = day of steepest positive slope, searched in `[first >=5-day NEP>0 run's start day -
  30, seasonal peak]`, per k.
- **End** = day of shallowest negative slope, searched in `[day of steepest decline after the
  peak, first day after that where smoothed NEP < 0]`, per k. **Alternate end** (search starting
  directly at the peak, skipping the steepest-decline anchor step) is also computed and reported
  (`start_date_alt_end` in `gsl_methods_site_year.csv`) -- median |difference| from the primary
  end across all site-years is **21 days**, confirming the anchor step matters, not a redundant
  detail.
- Start/end point estimates = **median date across k=3-9**; uncertainty = **range across k**.
- **Finding -- does k matter? Yes, substantially, and it is the smaller of two problems.** Median
  k-range is 23.5 days for start, 54 days for end (max 295 and 244 days respectively -- i.e. for
  some site-years different k values disagree by most of a year). **The larger problem**: because
  the Start search window spans from 30 days before the first NEP-positive run to the (single)
  seasonal peak -- often 2-4 months -- and unsmoothed daily NEP has large day-to-day
  weather-driven swings (a sunny day after a cloudy one can move NEP several gC m-2 d-1), the
  steepest k-day slope within that wide window is frequently a short weather-driven swing well
  after the true spring transition, not the green-up itself. Spot-checked directly on US-Ha1
  2004: raw NEP in mid-April is strongly negative (-1 to -4 gC m-2 d-1, true winter) with small
  k=7 slopes (max ~0.35); by late May, NEP swings from 0.5 to 10 gC m-2 d-1 day-to-day on
  weather alone, producing k=7 slopes up to 0.85 from noise, not trend. **Net effect, visible in
  every output**: M6's mean GSL is shorter than every other method at all 15 sites (gap vs.
  M1-summer ranges 61-296 days; see "Headline differences" below), and M6 correlates only weakly
  with every other method (Spearman r = 0.03-0.09 for GSL across method pairs vs. 0.52-0.93 among
  M1-M4) -- see "GSL methods agreement draft". This is implemented exactly as the prompt
  specifies; the finding is about what that specification produces on real, noisy sub-daily-
  resolution-free daily NEP, not an implementation shortfall.

## Flag counts (full detail in `tables/gsl_methods_site_summary.csv`)

| Method | Flag | Count (of applicable rows) |
|---|---|---|
| M1 (any sub) | `flag_no_season` | 2 of 285 per sub (6 total across 3 subs) |
| M1 (any sub) | `flag_whole_year` | 16 (annual), 14 (peak), 18 (summer) of 285 |
| M5/M6 | `flag_net_source` | 1 site-year of 285 (US-SRM; logged 3x, once per method row) |
| M5/M6 | `flag_two_season_rise` / `_decline` | see note above -- saturates (284/285, 282/285) |
| no-DT | sites flagged | 0 of 15 (all have DT GPP) |

## GPP-variant differences (M2@10%, screened vs. unscreened, mean GSL)

The winter screen only moves a handful of days at a handful of sites (full detail:
`tables/gsl_methods_site_summary.csv`, compare `screened`/`unscreened`/`agreement` rows for any
method): CH-Dav -8.3 days, AT-Neu -4.4, US-Ha1 -1.7, US-Mpj -0.5, NL-Loo -0.2, DE-Tha -0.1, and
zero difference at the remaining 9 of 15 sites (no winter-screened days at all for those sites in
this set, consistent with `winter_screen_site_effect.csv`). The **agreement** variant (requiring
both NT and DT to exceed the threshold) is much more consequential: it shortens mean M2@10% GSL
by 10-36 days relative to unscreened at every site (largest at FR-Pue, NL-Loo, US-Ton), because
DT and NT disagree at the shoulders of the season more than the winter artefact the screen
targets.

## Headline differences between methods (3-5 largest, agreement variant, M1-summer as reference)

1. **IL-Yat**: M1-summer 365 d (whole year, Mediterranean-climate artefact above) vs. M6 70 d --
   gap of 296 days, the largest in the comparison.
2. **AU-How**: M1-summer/M2/M5 all 362-365 d (this tropical savanna's wet season spans almost
   the whole Jul-Jun window for several years) vs. M6 131 d -- gap of 235 days.
3. **FR-Pue**: M1-summer 358 d vs. M6 135 d -- gap of 223 days.
4. **US-Ton**: M1-summer 356 d vs. M6 160 d -- gap of 196 days (and M1-summer vs. M1-peak alone
   is already a 30-day gap at this site, the Mediterranean-climate artefact compounding with M6's
   own shortfall).
5. **US-Var**: M1-summer 354 d vs. M6 171 d -- gap of 182 days (same compounding: 142 days of
   the gap is the summer-vs-peak artefact above, the rest is M6's own shortfall).

Excluding the Mediterranean-climate M1-summer artefact, the headline pattern is simpler and holds
at every one of the 15 sites without exception: **M1-peak/M1-annual, M2, M3 and M4 agree closely
with each other (pairwise Spearman r = 0.52-0.93 for GSL); M5 agrees weakly with all four
(r = 0.15-0.24); M6 agrees with none of them (r = 0.03-0.09)** -- see "GSL methods agreement
draft.png".

## What the light-response method (Bowling et al. 2024) would need

Not implemented here (explicitly out of scope). Bowling et al.'s method detects the growing
season from the shape of the light-response curve (NEE or GPP vs. PPFD/SW_IN), which needs
**half-hourly (HH) or hourly (HR) resolution data** to fit a within-day response curve per
window -- the daily-resolution `GPP_NT_REF`/`GPP_DT_REF`/`NEE_REF` series used throughout this
script cannot support it. Per the companion GPP/RECO uncertainty inventory
(`review/diagnostics/gpp_uncertainty_inventory/report.md`, 2026-11-02), sub-daily FLUXMET data is
extracted on disk for only 31 of 781 network sites and is queryable from the project's DuckDB
`hourly` table for only 1 (US-MMS) -- of this script's 15 sites, only US-MMS currently has any
HH/HR data at all, and even that is not in the DuckDB store the rest of this project's pipeline
reads from. A light-response comparison would need its own HH/HR extraction step for at least a
handful of these 15 sites before any method logic could be written.

## Not reproduced / possible follow-ups

- The "two-season" flag (M5/M6) needs a better slope definition or a genuine merge/prominence
  rule before it is useful as a multi-season-year signal -- see the dedicated note above.
- M6's "first/last seasonal peak" simplification (single global max) means M6 cannot currently
  flag a genuinely bimodal NEP year any differently from a unimodal one; a proper bimodality test
  would need the same slope-definition work as the two-season flag.
- M1-peak's chosen 3-month window is reported per site above but was not itself validated against
  independent phenology data (e.g. MODIS land-cover phenology products) -- it is simply "the 3
  consecutive months with highest mean GPP," which is by construction close to the true
  peak but not independently verified.
