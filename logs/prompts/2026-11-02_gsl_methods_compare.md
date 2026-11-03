GSL_definition/expanded: preliminary comparison of six published GSL definitions at the 15 sites of the draft "Figure GSL-IAV". Exploratory; nothing here replaces existing outputs. The Z16 rename is done (d43ae22); use the z16 column names.

Constraints
- One new script: GSL_definition/expanded/code/11_gsl_methods_compare.R. Do not modify any existing script, table or figure.
- DuckDB read-only, dataset = 'FLUXMET' only; committed expanded tables read-only.
- Side-project conventions: .meta.json via the local write_meta(); log_exclusion() and log_unknown() with log_path inside GSL_definition/expanded/; never write_output_metadata() or the repo-root outputs/ logs.
- Data handling as the main run: zero-floored GPP_NT_REF; NEP = -NEE from the same VUT/CUT family as each site's GPP; R2/N=14 passing years; Southern Hemisphere sites on Jul-Jun season years. Run every GPP-based method on three GPP variants and carry a variant column throughout: (1) screened, the main run's winter-screened GPP; (2) unscreened (main_unscreened); (3) agreement, Dave's chosen replacement for the winter screen: the daily minimum of zero-floored GPP_NT_REF and GPP_DT_REF from the site's VUT/CUT family, so a day exceeds a threshold only if both partitioning methods do. Where a site has no DT GPP, use NT alone and flag the site. In the agreement variant, thresholds (P99 and the fine grid) and annual GPP for shares and correlations stay as in the unscreened variant; only the series compared with the threshold changes. NEP is the same in all three.
- Label methods M1 to M6 as below. In the report, state that M2 at 10% is Z16 and M4 at 0.95 is T95.

Sites (tables/iav_15sites.csv): AU-How, NL-Loo, DE-Tha, FR-Pue, CH-Dav, US-MMS, US-Mpj, AT-Neu, US-Ha1, JP-Api, IL-Yat, US-Ton, US-Var, US-Wkg, US-SRM. Every passing year.

Smoothing (M1 and M5, and the bounds for M6): 15-day centred running mean of daily GPP and daily NEP. At year edges use days from the neighbouring year where data exist, even if that year did not pass selection (smoothing only; flag those site-years); otherwise use the partial window.

Methods (GSL in days, start and end date per site-year; for count methods, start and end are the first and last qualifying days)
- M1, span between crossings of smoothed GPP (Zscheischler et al. 2016). Start = first upward crossing, end = last downward crossing, GSL = end - start + 1. Three thresholds, all 10% of a P99 over all passing years:
  M1-summer: P99 of summer daily GPP (Jun-Aug NH, Dec-Feb SH), as published.
  M1-peak: P99 of the site's three consecutive calendar months with the highest mean GPP, wrapping across the year end. Report the months chosen per site.
  M1-annual: the main run's whole-year P99.
- M2, count of days with raw daily GPP > X% of the main run's P99 (strict >), X = 5, 10, 20.
- M3, count of days with raw daily GPP > 0.5, 1 and 2 gC m-2 d-1.
- M4, cumulative fraction of annual GPP: the main run's rule on the fine grid, targets 0.90, 0.95, 0.99.
- M5, carbon uptake period from smoothed NEP > 0. M5-count = days with smoothed NEP > 0; M5-span = first day of the first run of >= 5 consecutive days with NEP > 0 to the last day of the last such run.
- M6, rate of change of NEP (Hu et al. 2010), on unsmoothed daily NEP as Hu did (an agreed exception to the shared smoothing). For k = 3 to 9 days, OLS slope of daily NEP on day over each centred k-day window.
  Start = day of the steepest positive slope on the first rising limb. Define the search window from 30 days before the first day of the first >= 5-day run of smoothed NEP > 0 to the first seasonal peak of smoothed NEP, and document it.
  End = day of the shallowest negative slope (Hu: "unlike spring, daily NEP during the fall gradually became negative"), searching only negative slopes between the day of steepest decline after the last seasonal peak and the first day after it that smoothed NEP falls below zero. Also report the end date with the search starting at the last peak, and how far it moves.
  Estimate = median date across k; uncertainty = range across k. Report whether the k range matters.

Two-season rule: for M5-span and M6, use the first rise and the last decline. Flag every site-year with more than one distinct rise or decline (separate runs where the slope exceeds 50% of that year's maximum) and record the number.
Net-source rule: flag site-years with fewer than 5 days of smoothed NEP > 0 as net-source years; report them separately and exclude them from M5 and M6 site means.
Other flags per site-year and method: no season detected; season covers the whole year.

Outputs
- tables/gsl_methods_site_year.csv: site, year, variant, method, GSL, start, end, share of annual GPP inside the season (for span methods, GPP summed from start to end), all flags.
- tables/gsl_methods_site_summary.csv: per site, variant and method: mean GSL, interannual SD, mean start and end, flagged years by type, and detrended Pearson r (with p) of annual GSL against annual GPP and against annual NEP (each detrended by OLS on year, as in the IAV draft).
- Figures (agreement variant; one variant per method: M1-summer, M2 10%, M3 1 gC, M4 0.95, M5-span, M6), all in figures/drafts/:
  "GSL methods dotplot draft": mean GSL with interannual SD by method for each site, sites sorted by skewness.
  "GSL methods agreement draft": Spearman correlations between methods' annual GSL, pooled across sites as site-year anomalies; the same for start and for end dates.
  "GSL methods raster draft": US-Ha1, US-Wkg and US-SRM, years by day of year, showing the season under M1-summer, M2 10%, M4 0.95, M5-span and M6, one row block per method.
- docs/gsl_methods_compare_<date>.md: rules as implemented, interpretation choices, checks, flag counts, headline differences, and what the light-response method (Bowling et al. 2024) would need (half-hourly extracts); it is out of scope here.

Checks to report: M2 at 10% and M4 at 0.95 equal the existing Z16 and T95 values in derived_site_year.csv for the screened and unscreened variants; their detrended r with GPP equals tables/iav_15sites.csv; every site-year has a value or a flag for every method; flag counts by site and method.

Report back: the implementation choices for M1, M5 and M6, the check results, flag counts, the differences between the three GPP variants (days moved into or out of each season, by site), the sites without DT, and the three to five largest differences between methods, with the sites involved.

Write SESSION_LOG entries at the start, if anything goes wrong, and at the end, committing and pushing each. Save this prompt as logs/prompts/<date>_gsl_methods_compare.md. Log, commit and push per CLAUDE.md.
