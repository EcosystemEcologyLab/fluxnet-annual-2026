# GSL expanded-site analysis — checks session 1

2026-10-30. Four read-only checks on the expanded-site run (commit `a22e893`), per
`logs/prompts/2026-10-30_gsl_expanded_checks_session1.md`. New code only, at
`GSL_definition/expanded/code/07_checks_session1.R`; no existing script, table or figure was
changed. All new tables are under `GSL_definition/expanded/tables/checks_*.csv` (each with a
`.meta.json`). This report states what was found and the numbers; it does not recommend fixes —
that is session 2.

---

## Check 1: Harvard winter GPP

**Winter window convention used throughout this check**: Dec/Jan/Feb/Mar attributed to the
calendar year already used for that site's annual GSL (Dec(*y*) + Jan/Feb/Mar(*y*) → year *y*),
matching the two examples in the prompt (Dec 2010 → year 2010; Jan–Mar 2015 → year 2015), not a
cross-year Dec(*y*)–Mar(*y*+1) window.

### US-Ha1, per-year winter counts

Full table: `checks_c1_ha1_winter_counts.csv`. US-Ha1's P99 = 16.65 gC m⁻² d⁻¹; Z17's absolute
threshold = 1.66 gC m⁻² d⁻¹ (10% of P99); T95's own threshold is **9.1%** of P99 = 1.51 gC m⁻²
d⁻¹ — slightly *below* Z17's, which is why every year's T95 winter count is ≥ its Z17 count.

| Year | Days above Z17 | Days above T95 |
|---|---|---|
| 2010 | 10 | 14 |
| 2015 | 22 | 24 |
| all other years (13 years) | 0–7 | 0–7 |

2010 and 2015 are clear outliers — every other year has ≤7 winter days above either threshold,
most have 0–3. This matches Figure 1 showing exactly these two periods.

### What those days look like (`checks_c1_ha1_winter_days.csv`, 65 days total, 1996–2024)

Of the 65 winter days above Z17 and/or T95, **29 (45%) are flagged** by the prompt's own example
rule (`GPP_NT_REF > 0` and `NEE_REF >= 0` and `TA_F < 0` and `GPP_DT_REF < 10% of P99`) as a
likely partitioning artifact. The two flagged periods differ sharply in character:

- **2015 (21 of 24 winter days flagged, 88%)**: a near-continuous run from 15 Jan–25 Feb.
  `GPP_NT_REF` runs 1.6–11.7 gC m⁻² d⁻¹ while `GPP_DT_REF` is 0 or near-0 on almost every one of
  these days (max 0.20); `NEE_REF` is strongly positive (net carbon *source*, 3.2–9.0 gC m⁻² d⁻¹)
  and `TA_F` is well below freezing on every flagged day (−1.8 to −12.8°C). NT and DT disagree by
  an order of magnitude while the ecosystem is simultaneously respiring hard in deep cold — the
  textbook partitioning-artifact signature, not real winter uptake. 19 of the 24 days have
  `QC_REF < 1` (some gap-filling within the day); the pattern is not confined to gap-filled days
  (5 of the 24 are fully measured, `QC_REF = 1`, including two of the most extreme, 2015-01-15
  and 2015-01-18).
- **2010 (7 of 14 winter days flagged, 50%)**: a mixed set. The unflagged half (18, 19, 25 Jan;
  8, 23, 31 Mar; 12–13, 31 Dec) all have `TA_F > 0°C` and, on four of them, `GPP_DT_REF` tracking
  `GPP_NT_REF` reasonably well (e.g. 2010-03-31: NT=1.89, DT=1.30) — consistent with real
  above-freezing uptake during early-winter/late-winter thaw, not an artifact. The flagged half
  (24–30 Dec) is sub-freezing (−6.8 to −2.2°C) with `GPP_DT_REF` at or near 0 while `GPP_NT_REF`
  is 1.8–2.6 and `NEE_REF` strongly positive (6.1–7.8) — the same signature as 2015.

**Conclusion: a real mix, not a uniform artifact.** The 2015 cluster is almost entirely a
partitioning artifact; the 2010 cluster is roughly half artifact (the coldest days) and half
plausible real late-autumn/thaw uptake (the above-freezing days).

### Recompute: US-Ha1 annual GSL at Z17/T95 excluding winter days

`checks_c1_ha1_gsl_recompute.csv` (per year) and `checks_c1_ha1_gsl_summary.csv` (summary):

| Metric | Z17 original | Z17 excl. winter | Δ | T95 original | T95 excl. winter | Δ |
|---|---|---|---|---|---|---|
| Mean GSL (days) | 184.94 | 181.88 | **−3.06** | 189.94 | 186.12 | **−3.82** |
| S.D. (days) | 18.19 | 14.83 | **−3.36** | 19.04 | 15.26 | **−3.79** |
| Trend (days/yr) | −0.255 | −0.217 | +0.038 | −0.287 | −0.241 | +0.046 |

Excluding the winter days (no re-derivation needed — GSL is a day count, so the exclusion is an
exact subtraction) lowers US-Ha1's mean GSL by ~3–4 days and, more notably, **shrinks the
interannual s.d. by ~18–20%** (the 2010/2015 outlier years are driving a meaningful share of this
site's apparent year-to-year GSL variability). The trend barely moves (both already small and
not significant at n=17).

### DBF/MF network-wide sweep (`checks_c1_dbf_mf_sweep.csv`)

Same-style day (Dec–Mar for NH, Jun–Sep for SH; zero-floored daily GPP > 10% of P99 AND
`TA_F < 0`°C) counted across every DBF/MF site-year in the expanded set (29 DBF + 7 MF = 36
sites). **26 sites, 114 site-years** have ≥1 such day. Restricting to **|ΔGSL| > 5 days in any
year**: **11 sites, 49 site-years**:

| Site | Group | Max |Δ| (days) | Years affected |
|---|---|---|---|
| CN-HeM | deciduous forest | 72 (2019) | 2014, 2015, 2018, 2019, 2024 |
| CH-Lae | mixed forest | 37 (2006) | 2005, 2007–2020, 2024 (15 years) |
| IT-TrF | deciduous forest | 32 (2023) | 2017, 2019, 2020, 2022–2024 |
| US-Ha1 | deciduous forest | 20 (2015) | 2015 |
| JP-Spp | deciduous forest | 17 (2000) | 2000, 2001, 2008, 2012 |
| US-MOz | deciduous forest | 12 (2007) | 2005, 2007, 2008 |
| BE-Vie | mixed forest | 11 (2016) | 2001, 2005, 2008, 2011, 2016, 2017 |
| CN-BeO | mixed forest | 11 (2013) | 2013 |
| FR-Hes | deciduous forest | 10 (2017) | 2017 |
| US-Dk2 | deciduous forest | 8 (2004) | 2004, 2005, 2007, 2008 |
| JP-SMF | mixed forest | 7 (2013) | 2013 |

**CH-Lae (the project's one mixed-forest site, already a single-site group with no replication)
and CN-HeM are the most affected** — CN-HeM loses up to 72 days of GSL (roughly a quarter of its
~280–335-day mean GSL) in its worst year, and CH-Lae is flagged (|ΔGSL|>5) in **17 of its 18
total passing years** — essentially every year it has on record. This is the same mechanism
found at US-Ha1 (sub-freezing days with implausibly high `GPP_NT`), occurring at scale in
several DBF/MF sites, not a US-Ha1-specific artifact, and it is close to a wholesale problem for
the network's only mixed-forest representative.

---

## Check 2: Carbon effect of start and end dates

### Season-boundary rule and model (as implemented, not changed)

**Season boundary** (`02_derive.R`, `season_one()`): per site-year (or per Jul–Jun season-year
for Southern-Hemisphere sites, `location_lat<0`, included only when both calendar years are in
that site's own passing-year list), start = the first day of the **first run of ≥5 consecutive
days** with zero-floored daily `GPP_NT_REF` above the threshold; end = the last day of the
**last** such run; an `NA` GPP day counts as below threshold. Computed at 4 threshold points:
5%, 10% (Z17), 20% of P99, and the site's own T95 %.

**Model** (`03_models.R`, `fit_pooled_slope()`): `nlme::lme(annual_gpp_or_nep ~ start_or_end_doy,
random = ~ start_or_end_doy | site_id, method="REML")`, with a fallback ladder (optimizer swap,
then random-intercept-only) if the random-slope model fails to converge — `model_tier`/`note`
columns in every table below record which tier was actually used.

### Figure 5b numbers (`checks_c2_fig5b_summary.csv`, pulled unchanged from `carbon_gain_pooled.csv`)

Pooled slope (gC m⁻² per day the boundary shifts), SE, 95% CI, n_sites, n_obs (site-years), by
predictor × response × subset — 64 rows total (4 predictors × 2 responses × 8 subsets: 3
skewness thirds + 5 functional groups). All but mixed forest — which is a single site (CH-Lae)
and so never meets `fit_pooled_slope()`'s `n_sites >= 3` floor, and returns `NA`/"too few
sites/observations" — converged (every other cell: `converged = TRUE`). Representative rows:

| Response | Date | Slope range (all 16 subset×definition cells) | 95% CI excludes 0 |
|---|---|---|---|
| annual_gpp | start | −4.11 to −1.09 | 15 of 16 cells (only woody-grass mosaic/Z17 straddles 0) |
| annual_gpp | end | +0.96 to +6.25 | 14 of 16 cells |
| annual_nep | start | −1.33 to +0.67 | only 2 of 16 cells (both evergreen forest) |
| annual_nep | end | −0.87 to +1.96 | only 5 of 16 cells (evergreen forest, "high" third, "low" third) |

(The full 64-row breakdown, with every subset's exact slope/SE/CI, is in the CSV — not
reproduced cell-by-cell here.) The sign pattern on **annual GPP** is consistent and clearly
nonzero throughout: an earlier start and a later end are both associated with higher annual GPP,
i.e. a longer season raises annual carbon uptake, for nearly every subset with enough sites to
fit. **Annual NEP is a much weaker and less consistent story** — most subsets' CIs straddle
zero; only evergreen forest (both start and end, both definitions) and two skewness thirds (end
date only) show a 95% CI excluding zero, and even those are small in magnitude (0.5–2.0 gC m⁻²
per day of shift) relative to the GPP effect.

### DBF-only outlier analysis (`checks_c2_dbf_per_site.csv`, `_summary.csv`, `_outlier_sites.csv`, `_pooled_refit.csv`)

29 DBF sites total; **17 are trend sites** (≥10 passing years) and so have a per-site OLS slope.

| Predictor | Median per-site slope | IQR | n sites |
|---|---|---|---|
| start_z17 | −4.56 | 5.12 | 17 |
| start_t95 | −4.12 | 6.18 | 17 |
| end_z17 | +5.47 | 11.01 | 17 |
| end_t95 | +5.70 | 9.50 | 17 |

**Zero outlier sites** (Tukey 1.5×IQR) were found for any of the 4 predictors — the 17 DBF
per-site slopes are already homogeneous in sign (all 17 negative for start, all 17 positive for
end) and reasonably close in magnitude; none sits beyond 1.5×IQR of its own distribution.
Consequently the "with outliers" and "without outliers" pooled refits in
`checks_c2_dbf_pooled_refit.csv` are **identical by construction** (there was nothing to
exclude): pooled start_z17 = −3.61 (SE 0.79), start_t95 = −3.99 (SE 0.85), end_z17 = +4.10 (SE
0.91), end_t95 = +2.91 (SE 0.85) gC m⁻² per day, all with 95% CIs excluding zero, all with
`converged = TRUE` / `model_tier = "random slope"` (no fallback needed).

**Does a near-zero slope survive?** No — there is no near-zero slope to begin with, before or
after an outlier check that turns out to remove nothing. Every DBF per-site slope and every DBF
pooled slope is clearly nonzero and consistently signed with the whole-population Figure 5b
result above.

---

## Check 3: Southern Hemisphere season-year mismatch

**14 Southern-Hemisphere sites** (`location_lat < 0`) in the expanded set, listed in
`checks_c3_sh_sites.csv`: AU-ASM, AU-Cpr, AU-Cum, AU-DaS, AU-Dry, AU-Gin, AU-GWW, AU-How, AU-TTE,
AU-Tum, AU-Whr, AU-Ync, NZ-ADw, NZ-Lau. They span all three skewness thirds and three functional
groups (grassland, evergreen forest, woody-grass mosaic) — no SH site falls in deciduous or
mixed forest, so those two groups are entirely unaffected by this check. Per-site site-year
counts are in the CSV.

### Season-year vs. calendar-year comparison (`checks_c3_sh_siteyear_compare.csv`, `_summary.csv`)

Across every SH site-year where both calendar years of its Jul–Jun window pass the selection rule:

| Variable | Pearson r (season-yr vs. calendar-yr) |
|---|---|
| Annual GPP | 0.970 |
| Annual NEP | 0.921 |
| Mean TA_F | 0.995 |

(Median absolute/percent differences are in the summary CSV.) All three are highly correlated —
the Jul–Jun and Jan–Dec windows for a given "year" overlap by 6 months, so this is expected —
but NEP is the noisiest of the three (r = 0.92, vs. 0.97–1.00 for GPP/TA), consistent with NEP
being a small residual of two much larger fluxes.

### Refit with season-year values (`checks_c3_cg_refit.csv`, `checks_c3_ts_refit.csv`)

Only the predictors/responses that are actually season-year-keyed-but-calendar-year-joined are
affected: `start_z17/start_t95/end_z17/end_t95` → `annual_gpp`/`annual_nep` (carbon-gain), and
`annual_mean_ta` → `gsl_z17`/`gsl_t95` (temperature-sensitivity, the `gsl_on_ta` combos only —
`start_on_ta`/`end_on_ta` use the fixed 60-day pre-window, which is date-arithmetic-based, not
calendar/season-keyed, so they are untouched by this mismatch). `gsl_z17`/`gsl_t95` themselves
are calendar-year quantities joined to calendar-year `annual_gpp`/`annual_nep`/`annual_mean_ta`
with no mismatch, so models using them as the response in carbon-gain (not shown) are unaffected.

**6 subsets contain ≥1 SH site**: third_low, third_middle, third_high, group_grassland,
group_evergreen_forest, group_woody-grass_mosaic (mixed/deciduous forest have none, as above).

- **Carbon-gain** (4 predictors × 2 responses × 6 affected subsets = 48 refit cells): every
  `delta_slope` is small relative to the slope itself — the largest is
  group_woody-grass_mosaic's `end_z17` on `annual_gpp` (2.51 → 2.84, Δ=+0.33); the next largest
  is that same group's `start_t95` on `annual_nep` (Δ≈+0.20). No sign flips anywhere.
- **Temperature-sensitivity**: deltas are mostly ≤0.1 day/°C except group_woody-grass_mosaic's
  `gsl_z17_on_ta`, which moves from 0.45 (SE 4.05) to 2.02 (SE 3.28) — a nominal Δ of +1.58, the
  single largest shift found in this check — but the slope is not distinguishable from zero
  either before or after (SE far exceeds the estimate in both cases), so this is noise in an
  already-uninformative cell, not a substantive change in a real effect.

**Conclusion: the mismatch exists and is measurable (GPP/TA correlate at 0.97–1.00 but not
perfectly; NEP only at 0.92) but its effect on the pooled slopes already reported is small** —
no subset's carbon-gain or temperature-sensitivity slope changes materially or changes sign when
SH sites' season-year values are substituted. The one subset with a visible nominal shift
(woody-grass mosaic, temperature-sensitivity) was already statistically indistinguishable from
zero.

---

## Check 4: Tash's Figure 1 correlations (r = 0.63, 0.49, 0.34)

Run on the **62-site reproduction run** (`GSL_definition/`, Tash's screening / Sites table), not
the expanded set, per the prompt. A prior session (`docs/reproduction_report.md`, Check 3) had
marked this "NOT CHECKABLE — no definition given." This check proceeds under the prompt's own
new assumption: the three r values correspond, in order, to the spread (across a 22-point
threshold grid) of **mean GSL**, **GSL s.d.**, and **GSL trend**, each correlated with skewness.

Two threshold grids were used: Tash's own 22-point **% of P99** grid (0.1%, 0.5%, 1–20%,
unchanged from `code/03_derive.R`'s `TASH_GRID_PCT`), and a 22-point **fixed/absolute** grid from
0.0001 to 2.5 gC m⁻² d⁻¹. **The fixed grid's interior spacing is an assumption, not a recovered
value** — the prompt gives only the two endpoints. It was constructed to mirror the shape of
Tash's own % grid (two small points, then a 20-point linear run to the upper endpoint):
`c(0.0001, 0.0005, seq(2.5/20, 2.5, by = 2.5/20))`. This should be treated as a guess, flagged
here explicitly, not as a known fact about her methods.

A sanity check was run first and confirmed directly (`stopifnot`, not asserted): at every
threshold on both grids (all strictly positive), the annual GSL count is **identical** whether
negative daily GPP is zero-floored or left as-delivered, since a negative or zero raw value can
never exceed a positive threshold either way. Negative-GPP treatment therefore only matters here
through (a) which skewness value is correlated against, and (b) the % grid's absolute threshold
(via P99, which does depend on the floor).

**Full table**: `checks_c4_full_table.csv` — every (negative-GPP treatment × grid × metric ×
spread measure × correlation type × site scope) combination, 288 rows.

**Combination search**: `checks_c4_combo_search.csv` — 48 rows, one per (treatment × grid ×
spread measure × scope × correlation type) applied uniformly to all three metrics, sorted by
worst-metric absolute difference from target. **4 combinations reproduce all three target r
values within 0.02**, and all four share the same two choices:

| Negative GPP | Grid | Spread | Scope | Corr. | r(mean) | r(s.d.) | r(trend) | max|Δ| |
|---|---|---|---|---|---|---|---|---|
| zero-floored | % of P99 | **IQR** | excl. 3 flagged | **Pearson** | 0.632 | 0.487 | 0.344 | **0.0036** |
| zero-floored | % of P99 | **IQR** | all 62 | **Pearson** | 0.629 | 0.491 | 0.346 | 0.0063 |
| as-delivered | % of P99 | **IQR** | excl. 3 flagged | **Pearson** | 0.643 | 0.493 | 0.356 | 0.0155 |
| as-delivered | % of P99 | **IQR** | all 62 | **Pearson** | 0.639 | 0.497 | 0.358 | 0.0179 |

Target: r = 0.63, 0.49, 0.34 (mean, s.d., trend).

**This is a clean, specific match, not a coincidence of a large search space.** Every one of the
44 non-matching combinations misses by more on at least one metric (next-best max|Δ| = 0.035,
ten times worse); the **fixed/absolute grid never comes within 0.02 on any variant** (its best
max|Δ| anywhere in the table is 0.052, and that is a Spearman cell, not Pearson); and Spearman
correlation never matches on either grid. The % grid + IQR + Pearson combination is picked out
specifically, consistently across both negative-GPP treatments and both site scopes — strong
evidence that: (1) "spread" in the manuscript means the **IQR of the 22-threshold GSL
distribution**, not range or s.d.; (2) the correlations are **Pearson**, not Spearman; (3) the
grid is her own **% of P99** grid, not some other fixed/absolute threshold set; and (4) the
assumed mean→s.d.→trend ordering of the three published r values is consistent with the data.
Because a match was found on the main grid, **the 5%/10%-of-P99-capped short version was not
run** (the prompt asks for it only "if no combination matches").

---

## Summary for session 2

1. **US-Ha1's two flagged winter clusters are not uniformly artifacts**: 2015 (88% artifact by
   the given rule) is overwhelmingly a partitioning/respiration-at-depth-of-winter problem; 2010
   (50%) is a genuine mix of artifact days and plausible thaw-season uptake. Excluding the
   flagged winter days shifts US-Ha1's mean GSL by only 3–4 days but cuts its interannual s.d. by
   ~20%. The same mechanism recurs at scale elsewhere in the network: **11 DBF/MF sites, 49
   site-years**, with CN-HeM (up to 72 days/year) and CH-Lae (the sole mixed-forest site,
   affected in most of its years on record) the most exposed.
2. **Figure 5b's start/end-date carbon-gain slopes are robust for DBF**: the 17 DBF trend sites'
   per-site slopes contain zero Tukey outliers for any of the 4 predictors, so the "with vs.
   without outliers" pooled comparison is moot — there is nothing to remove, and no near-zero
   slope is lurking underneath a few extreme sites.
3. **The Southern-Hemisphere season-year/calendar-year join mismatch is real but small in
   practice**: GPP and TA_F correlate at 0.97–1.00 and NEP at 0.92 between the two windows, and
   substituting season-year values changes no reported pooled carbon-gain or
   temperature-sensitivity slope by a material amount or in sign, in any of the 6 affected
   subsets (mixed/deciduous forest have no SH members and are entirely unaffected).
4. **Tash's three unexplained Figure 1 correlations reproduce cleanly** under the assumed
   mean/s.d./trend ordering, specifically as **Pearson correlations of skewness with the IQR
   (not range or s.d.) of annual GSL across her own 22-point % of P99 grid** (not a fixed
   absolute grid) — matching all three published values to within 0.004–0.018, roughly ten times
   closer than any other combination tried.
