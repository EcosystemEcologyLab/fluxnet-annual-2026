GSL_definition/expanded: new draft figure on missed GPP and interannual variability, 15 sites. Dave's design. New script code/09_figure_iav_15sites.R; do not change any existing script, table or figure. Main variant from session 2a. DuckDB read-only, dataset = 'FLUXMET' only; local write_meta().

Sites (at least 12 passing years, spread across the skewness range): DE-Tha, NL-Loo, AU-How, CH-Dav, US-MMS, FR-Pue, AT-Neu, US-Mpj, JP-Api, US-Ha1, IL-Yat, US-Var, US-Ton, US-Wkg, US-SRM. Use every passing year at each site. Annual GPP and NEP as in carbon_gain_data.csv (Jul-Jun season years for AU-How). Report each site's skewness and years; if any site is missing or has fewer than 12 years, stop and report.

a. Fraction of annual GPP outside the Z17 GSL (1 - F at 10% of P99) against skewness: one point per site at the mean over years, with a bar for the interannual IQR. Dashed horizontal line at 5% (the share T95 leaves out by construction). Label points with site IDs; print Pearson r and n.
b. Interannual link between GSL and GPP, one summary per site: detrend annual GSL and annual GPP separately (OLS on year), then take Pearson r between the residuals, at Z17 and at T95. Plot r against skewness: Z17 in black and T95 in red, the two points for a site joined by a thin vertical segment; filled points where p < 0.05, open otherwise. Zero line.

Table tables/iav_15sites.csv: site, IGBP, years, skewness, mean and IQR of the missed fraction, and for both GPP and NEP at Z17 and T95 the detrended r and p. Add Steiger's test for the difference between each site's Z17 and T95 correlations with GPP (they share data, so use the correlation between the two detrended GSL series), z and p per site.

Save as figures/drafts/"Figure GSL-IAV draft.png" (do not overwrite the numbered figures). PeerJ format: 300 dpi, text >= 8 pt, panel labels a and b, fits 8.5 x 11 in. Write a short caption and a docs/figure_gsl_iav_<date>.md note with the numbers for each panel.

Write SESSION_LOG entries at the start, if anything goes wrong, and at the end, committing and pushing each. Save this prompt as logs/prompts/<date>_gsl_figure_iav_15sites.md. Log, commit and push per CLAUDE.md.
