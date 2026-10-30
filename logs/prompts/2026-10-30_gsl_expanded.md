GSL_definition: expanded-site analysis. Build it in a new subfolder, GSL_definition/expanded/ (code/, tables/, figures/, docs/), so the existing 62-site outputs stay unchanged; reuse code from GSL_definition/code/ by sourcing it, not copying it. Flux data only: no PhenoCam or other external data. Read-only on DuckDB, FLUXMET rows only.

Selection (the rule chosen from docs/selection_rule_options_2026-10-30.md):
- Manuscript rules kept: drop IGBP CRO, CVM, WET, SNO, BSV; daily test QC >= 0.25 (qc_passes(); NA fails); at least 5 passing years; drop sites with > 10% of daily GPP below -1.
- Per-site VUT/CUT fallback as in 04_qc.R. CUT sites use NEE_CUT_REF_QC, GPP_NT_CUT_REF and NEE_CUT_REF; never mix within a site.
- A year passes with at most 14 failing days (R2, N = 14).
- Drop sites with no GPP values (expect AU-Stp, ES-LJu, ID-PaD, JP-Khw, JP-Ta2); list them.
- Tropical sites treated like any other.
Expect about 156 sites; confirm all 505 of the GSL 62's site-years are included. US-Ha1 should keep 17 years.

Settings: negative daily GPP set to 0 (main); GSL counts GPP > T; GSL counted on the full gap-filled daily series; P99 and skewness as in 03_derive.R. NEP = -NEE. Recompute the skewness thirds on the expanded set. Z17 = 10% of P99; T95 as in the framework.

Derived quantities, per site and site-year, for Z17, T95 and the 5-20% of P99 range:
1. Everything in 03_derive.R.
2. Share of annual GPP outside the season (1 - F) at Z17 (and at T95, about 5% by construction).
3. Season start and end dates: the first day of the first run, and the last day of the last run, of at least 5 consecutive days above the threshold. Gap days: days below the threshold between start and end. For Southern Hemisphere sites use a July to June year for start, end and gap days only. Flag sites above the threshold all year.
4. Bounds check: GSL with every failing day counted as above the threshold, and again as below; report the spread per site-year.
5. Carbon gain: across years within each site, the slopes of annual GPP and annual NEP on GSL, on start date and on end date. Per-site slopes for sites with >= 10 years. Pooled slopes by third and by group with a mixed model (random site intercepts and slopes; nlme).
6. Temperature sensitivity: the slope of GSL on annual mean TA_F (days per °C); start date on mean TA_F over the 60 days before the site's mean start date; end date on the 60 days before its mean end date. Per site and pooled as in 5.
7. Trends: per-site OLS trends as before (sites with >= 10 years), and pooled trends by third and by group at each threshold in 5-20% of P99 and for Z17 and T95. Use a mixed model with random site slopes and AR(1) residuals (nlme::lme with corAR1). Years needed to detect each pooled trend at 95% with 80% power: the Weatherhead et al. (1998) formula, using each site's residual SD and lag-1 autocorrelation.
8. Aridity from data/snapshots/site_aridity.csv (CGIAR).

Main figures ("Figure 1.png" to "Figure 6.png", with PDF, .meta.json and .legend.txt; the PeerJ style in code/fig_helpers.R). Show Z17 and T95 as two series in every relevant panel, and use at most three panels per figure:
1. US-Ha1 and US-SRM, chosen as illustrations, not by a rule (say so in the legend file). (a) For each site, years against day of year, each day coloured by which definitions include it (both, Z17 only, T95 only, neither). (b) Mean GSL against threshold for both sites.
2. Share of annual GPP outside the Z17 season against (a) skewness and (b) aridity, coloured by group.
3. Sensitivity in two units. (a) Median days lost per 1% of P99 against threshold, by third. (b) The same at Z17 against skewness. (c) Days per 1% of included GPP, Z17 and T95, against skewness. The legend file states why (b) and (c) run in opposite directions.
4. Season timing. (a) The shift in start and end dates between 5% and 20% of P99, against skewness. (b) Gap days against skewness, Z17 and T95.
5. Carbon gain. (a) Pooled slopes of annual GPP and NEP on GSL (gC m-2 per day), by third, for Z17 and T95, with per-site slopes behind. (b) The same slopes on start date and on end date.
6. Climate fingerprints. (a) Temperature sensitivity (days per °C) of GSL, start and end, by third, Z17 and T95. (b) Pooled trends by third and by group across 5-20% of P99, with Z17 and T95 marked, each labelled with the years needed for detection.

Supplementary ("Supplemental Figure S1" etc.):
- S1: site and site-year counts by group, Tash's 62 against the expanded set; CUT-fallback sites flagged; recovered long-record sites listed.
- S2: the bounds-check spread per site-year at Z17 and T95; flag sites with large spreads.
- S3: the nested comparison. Skewness, T95 and sensitivity at Z17 for Tash's 62 overlaid on the expanded set, plus a table of the headline numbers for both sets.
- S4: per-site trends (the current Figure 3a).
- S5: Figures 3 and 6 with GPP_DT, and the thirds under GPP_DT. Add one table line on the as-delivered negative-GPP result.
- S6: the main numbers without the CUT-fallback sites.
- S7: GSL against target fraction for all sites.

Checks before finishing:
- Text >= 8 pt.
- No legend over data and no overlapping labels.
- Tick labels thinned where crowded.
- Three clearly separated colours for the thirds.
- Every number in the legend files traced to a table.
- The 505 GSL-62 site-years included.
- Pooled models converged (report any that did not).
Write docs/report_back_<date>.md with the numbers each figure shows, and update docs/methods_memo.md with the selection rule, every definition above and any departure from it. If a check fails, stop and report.

Write SESSION_LOG entries at the start, if anything goes wrong, and at the end, committing and pushing each. Save this prompt as logs/prompts/<date>_gsl_expanded.md. Commit and push per CLAUDE.md.
