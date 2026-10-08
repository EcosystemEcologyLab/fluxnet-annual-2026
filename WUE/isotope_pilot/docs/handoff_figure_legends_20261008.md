# WUE isotope pilot — hand-off figure legends (2026-10-08)

Side analysis, not the FLUXNET Annual Paper 2026. Draft legends only, at most 200 words each.
No interpretation: no trend tests, no Sen's slopes, no statements about what a series means.

## Map of the 11 WUE isotope pilot sites (`fig0_map_sites`)

Panel a, North America; panel b, Europe. Each site is a large dot coloured by IGBP class (PAPER_IGBP_COLOURS/PAPER_IGBP_ORDER), labelled with its site code. No values are plotted here, only site location and vegetation class.

## WUE, IWUE, uWUE and k* by site, normalised to the site's own mean (`fig1_by_site_ratio_to_mean`)

One panel per site (11), ordered by IGBP then latitude, each titled with the site code and the site's IGBP/Koppen class in the top-right corner. Four coloured lines (WUE, IWUE, uWUE, sub-daily k*) show each metric's annual value divided by that site's own mean over its plotted years. k* excludes site-years where it sat on a grid-search limit (0 or 1.5). Site-years with fewer than 10 valid days are open points; kept years with zero valid days (US-Ho2 2012, 2013) show no points. Light grey bars (secondary axis, right) show valid days per year. Year labels on the x-axis are red where fewer than 80% of that year's days had a fully gauge-measured precipitation record. No trend tests or slopes are shown.

## WUE, IWUE, uWUE and k* by site, percent change from each site's first qualifying year (`fig1_by_site_pct_change_guerrieri`)

Same layout and panels as fig1_by_site_ratio_to_mean, but each metric is shown as percent change from the site's first year with a value and at least 10 valid days (0% baseline), following Guerrieri et al. (2019)'s normalisation. Open points, zero-valid-day years, background valid-day bars, and red gauge-coverage year labels as in the ratio version. No trend tests or slopes are shown.

## WUE, IWUE, uWUE and k* by plant functional type, normalised to each site's own mean (`fig2_by_pft_ratio_to_mean`)

Two panels, DBF and ENF (BE-Vie, the only MF site among the 11, is left out of this figure). Thin faint lines show each contributing site's own ratio-to-mean series; the bold line is the across-site median per calendar year, per metric. Grey background bars (secondary axis) show the mean valid days per year across contributing sites. The number of sites in the group is noted in the top-right corner. No trend tests or slopes are shown.

## WUE, IWUE, uWUE and k* by plant functional type, percent change from baseline (`fig2_by_pft_pct_change_guerrieri`)

Same layout as fig2_by_pft_ratio_to_mean, but each site's series is percent change from its own first year with a value and at least 10 valid days before the across-site median is taken, per calendar year and metric. No trend tests or slopes are shown.

## Rain source summary, 11 sites (`fig3_rain_source_summary`)

Panel a: share of each site's fully measured days with a daily total above 0 mm, by the tower gauge and by P_ERA (one pair of points per site, connected by a grey segment). Panel b: mean days removed per year by the stage 2 rain rule when driven by the gauge versus by P_ERA, over site-years with at least 350 fully measured days. Sites in both panels are ordered by IGBP then latitude. No cause is asserted.

## Days removed by the rain rule, by site-year and source (`fig4_rain_rule_by_year`)

One panel per site (11), days removed per year by the stage 2 rain rule, gauge- driven versus P_ERA-driven, for site-years with at least 350 fully measured days. Redraws figures/precip_compare/fig_days_removed_by_source.png in this hand-off's Nature format. No trend tests or slopes are shown.

