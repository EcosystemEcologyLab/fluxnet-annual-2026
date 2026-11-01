# Figure GSL-IAV (draft): missed GPP and interannual variability, 15 sites

2026-10-31. Full prompt: `logs/prompts/2026-10-31_gsl_figure_iav_15sites.md`. Dave's design.
Script: `GSL_definition/expanded/code/09_figure_iav_15sites.R` (new, standalone — does not modify
any existing script, table, or figure). Output: `figures/drafts/"Figure GSL-IAV draft.png"` (not
a numbered manuscript figure), `tables/iav_15sites.csv`, this note. Data source: the already-
committed session 2a "main" variant tables (`carbon_gain_data.csv`, `derived_site_year.csv`,
`sites.csv`, `derived_site.csv`) — all themselves built read-only against the repo-root DuckDB
store, `dataset = 'FLUXMET'` only. No raw daily pull was needed for this script.

## Sites and validation

15 sites, spread across the expanded set's skewness range (0.37–2.41), each required to have
≥12 R2/N=14 passing years. All 15 present; none fell below 12 years, so the script's `stop()`
guard did not trigger. Sorted by skewness:

| site | IGBP | skewness | n passing years |
|---|---|---:|---:|
| AU-How | WSA | 0.37 | 13 |
| NL-Loo | ENF | 0.42 | 17 |
| DE-Tha | ENF | 0.44 | 24 |
| FR-Pue | EBF | 0.58 | 19 |
| CH-Dav | ENF | 0.58 | 27 |
| US-MMS | DBF | 0.60 | 23 |
| US-Mpj | WSA | 0.73 | 15 |
| AT-Neu | GRA | 0.76 | 18 |
| US-Ha1 | DBF | 0.95 | 17 |
| JP-Api | DBF | 1.00 | 19 |
| IL-Yat | ENF | 1.19 | 13 |
| US-Ton | WSA | 1.22 | 20 |
| US-Var | GRA | 1.45 | 23 |
| US-Wkg | GRA | 2.10 | 21 |
| US-SRM | WSA | 2.41 | 20 |

"Use every passing year at each site": `missed_frac_*` (panel a) uses all of these passing years
(it only needs `f_z17`, from `derived_site_year.csv`, no GPP join). The detrended correlations and
Steiger's test (panel b and the table's `r_*`/`p_*`/`steiger_*` columns) additionally need a
non-NA `annual_gpp`/`annual_nep`/`gsl_z17`/`gsl_t95` for that year, which is not true of every
passing year at 3 sites, for two distinct, **pre-existing** reasons (surfaced by this script's
complete-case filter, not introduced by it):

1. **AU-How** (9 of 13 years used): `carbon_gain_data.csv`'s `annual_gpp`/`annual_nep` come from
   `annual_inputs.csv`, which only has AU-How's Jul(year)–Jun(year+1) Southern-Hemisphere season
   years where *both* calendar halves are themselves R2/N=14 passing years — 2008, 2017, 2019,
   2024 are not. Same documented simplification already in `carbon_gain_data.csv`'s own
   `.meta.json` (session 2a); not new here.
2. **CH-Dav** (24 of 27) **and IL-Yat** (12 of 13): genuine site-years with no underlying daily
   GPP data at all (2008/2010/2011 for CH-Dav, 2010 for IL-Yat). `derived_site_year.csv`'s
   `gsl_z17`/`gsl_t95` are correctly `NA` for these (an empty year is left `NA` by
   `year_threshold_matrices()`), but `annual_inputs.csv`'s `annual_gpp` for the *same* year is a
   trivial `0`, not `NA` (`sum(..., na.rm = TRUE)` over zero non-missing days returns `0`) — an
   inconsistency between the two already-committed source tables for an empty year, not a Jul-Jun
   artifact (both sites are Northern-Hemisphere, calendar-year-labelled). The `gsl_z17`/`gsl_t95`
   `NA` correctly excludes these years from the complete-case set either way.

## Panel a: fraction of annual GPP outside the Z17 GSL vs. skewness

One point per site, the across-year mean of `1 - f_z17`; error bar = interannual IQR (Q1–Q3);
dashed line at 5% (T95's by-construction excluded share). **Pearson r = 0.91, n = 15** (`p =
1.8e-06`) — skewness very strongly predicts how much annual GPP the Z17 season misses.

| site | mean missed GPP | Q1 | Q3 |
|---|---:|---:|---:|
| AU-How | 0.0012 | 0.0002 | 0.0009 |
| NL-Loo | 0.0219 | 0.0184 | 0.0268 |
| DE-Tha | 0.0242 | 0.0193 | 0.0292 |
| FR-Pue | 0.0140 | 0.0054 | 0.0149 |
| CH-Dav | 0.0162 | 0.0119 | 0.0204 |
| US-MMS | 0.0182 | 0.0156 | 0.0199 |
| US-Mpj | 0.0178 | 0.0046 | 0.0265 |
| AT-Neu | 0.0263 | 0.0230 | 0.0280 |
| US-Ha1 | 0.0451 | 0.0344 | 0.0592 |
| JP-Api | 0.0238 | 0.0199 | 0.0269 |
| IL-Yat | 0.0838 | 0.0393 | 0.1102 |
| US-Ton | 0.0319 | 0.0162 | 0.0458 |
| US-Var | 0.0433 | 0.0276 | 0.0543 |
| US-Wkg | 0.0977 | 0.0758 | 0.0962 |
| US-SRM | 0.1399 | 0.0697 | 0.1619 |

Only US-Wkg and US-SRM (skewness ≥ 2.1) clear the 5% line on their mean; IL-Yat's IQR reaches it
too despite a lower mean (0.084), reflecting its wide interannual spread (Q1 0.039–Q3 0.110, the
widest IQR of the 15 relative to its mean along with US-Mpj).

## Panel b: detrended GSL–GPP correlation vs. skewness, Z17 vs. T95

Per site: `annual_gsl` and `annual_gpp` each detrended (OLS on year) separately, then Pearson r
between the residuals, at Z17 (black) and T95 (red); filled point = p < 0.05.

| site | r (Z17) | p (Z17) | r (T95) | p (T95) | r (GSL_z17, GSL_t95) | Steiger z | Steiger p |
|---|---:|---:|---:|---:|---:|---:|---:|
| AU-How | 0.168 | 0.667 | −0.124 | 0.751 | 0.585 | 0.789 | 0.430 |
| NL-Loo | 0.698 | 0.0018 | 0.704 | 0.0016 | 0.894 | −0.077 | 0.939 |
| DE-Tha | 0.503 | 0.0122 | 0.548 | 0.0056 | 0.816 | −0.406 | 0.684 |
| FR-Pue | 0.766 | 1.3e-04 | 0.942 | 1.7e-09 | 0.876 | **−3.418** | **0.0006** |
| CH-Dav | 0.623 | 0.0011 | 0.778 | 7.7e-06 | 0.732 | −1.481 | 0.139 |
| US-MMS | 0.345 | 0.107 | 0.244 | 0.262 | 0.863 | 0.905 | 0.365 |
| US-Mpj | 0.710 | 0.0030 | 0.749 | 0.0013 | 0.973 | −0.861 | 0.389 |
| AT-Neu | 0.447 | 0.063 | 0.462 | 0.054 | 0.904 | −0.143 | 0.886 |
| US-Ha1 | 0.836 | 3.0e-05 | 0.847 | 1.8e-05 | 0.990 | −0.579 | 0.562 |
| JP-Api | 0.674 | 0.0016 | 0.717 | 5.6e-04 | 0.817 | −0.412 | 0.680 |
| IL-Yat | 0.930 | 1.2e-05 | 0.898 | 7.4e-05 | 0.973 | 1.069 | 0.285 |
| US-Ton | 0.723 | 3.2e-04 | 0.742 | 1.8e-04 | 0.972 | −0.490 | 0.624 |
| US-Var | 0.500 | 0.0151 | 0.531 | 0.0091 | 0.995 | −1.621 | 0.105 |
| US-Wkg | 0.829 | 3.4e-06 | 0.807 | 1.0e-05 | 0.974 | 0.725 | 0.468 |
| US-SRM | 0.775 | 5.9e-05 | 0.693 | 7.1e-04 | 0.957 | 1.739 | 0.082 |

Every site's GSL–GPP correlation is positive (expected: a longer growing season essentially
always tracks more GPP interannually) and significant (p < 0.05) at 12 of 15 sites for both Z17
and T95 — the same 3 exceptions at both thresholds: AU-How, US-MMS, and AT-Neu (AT-Neu's T95 p =
0.054, just above the line). **Only FR-Pue's Z17-vs-T95 difference is significant by Steiger's
test** (z =
−3.42, p = 0.0006): T95 tracks GPP distinctly better than Z17 there (r = 0.942 vs. 0.766). The
next-closest are US-SRM (z = 1.74, p = 0.082, Z17 slightly ahead) and US-Var (z = −1.62, p =
0.105, T95 slightly ahead) — neither reaches significance. NEP correlations (table only, not
plotted) are weaker and more often non-significant than GPP's, as expected since NEP nets out
ecosystem respiration as well as GPP.

## Table columns (`tables/iav_15sites.csv`)

`site_id, igbp, n_years, n_years_used, skewness, missed_frac_mean, missed_frac_q1,
missed_frac_q3, missed_frac_iqr, r_gpp_z17, p_gpp_z17, r_gpp_t95, p_gpp_t95, r_nep_z17, p_nep_z17,
r_nep_t95, p_nep_t95, r_gsl_z17_t95, steiger_z, steiger_p`. Full notes in the table's own
`.meta.json`.

## Steiger's (1980) Z1* test, as implemented

Neither `cocor` nor `psych` is in this project's renv lockfile, so the test is implemented
directly in `09_figure_iav_15sites.R` (`steiger1980_z()`) rather than adding a new dependency
(CLAUDE.md). Formula: Steiger, J.H. (1980), "Tests for comparing elements of a correlation
matrix", *Psychological Bulletin* 87:245–251, the Z1* statistic for two dependent, overlapping
correlations sharing one variable, as widely reproduced (e.g. Lee & Preacher 2013;
`cocor::cocor.dep.groups.overlap(test = "steiger1980")`). Inputs here: r12 = r(GSL_z17, GPP), r13
= r(GSL_t95, GPP) (both GPP-detrended-residual correlations), r23 = r(GSL_z17, GSL_t95) (the two
detrended GSL series against each other, as the prompt specifies), n = `n_years_used`.
