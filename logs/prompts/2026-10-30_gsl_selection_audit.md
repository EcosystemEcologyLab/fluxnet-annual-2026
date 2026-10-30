GSL_definition: site-selection audit. Question: which long-running sites did the manuscript's selection rules exclude, and would a rule that drops days or years, rather than whole sites, keep them? Read-only on DuckDB, FLUXMET rows only. Write tables to GSL_definition/tables/selection_audit_*, one diagnostic figure to figures/, and a report to docs/selection_audit_<date>.md. No change to the 62-site analysis.

The manuscript's rules: (1) drop IGBP CRO, CVM, WET, SNO, BSV; (2) a day passes if NEE_VUT_REF_QC >= 0.25; (3) a year passes only if every day passes; (4) a site needs >= 5 passing years; (5) drop sites with > 10% of daily GPP below -1 gC m-2 d-1.

1. Population: every site in the 20 September snapshot passing rule 1 with daily data. Flag "long record" sites (>= 15 years with 12 months of data, from site_year_data_presence.csv) and whether each is in the GSL 62.
2. Per site-year, for every site in the population:
   - share of days passing 0.25 and 0.50;
   - number of failing days, and the longest run of consecutive failing days;
   - failing days by month.
   Use qc_passes(); NA counts as failing.
3. Rule variants. For each, give per site the number of passing years and whether the site has >= 5:
   (i) the manuscript rule (every day passes 0.25);
   (ii) >= 95%, >= 90% and >= 80% of days pass 0.25;
   (iii) the same at 0.50;
   (iv) every day passes 0.25 except gaps of at most 3, 7 and 14 consecutive failing days, each counted once.
   Report how many sites, long-record sites and site-years each variant adds relative to the manuscript rule. Name the long-record sites recovered (expect US-Ha1, BE-Vie, CA-Cbo, US-NR1, US-Ho2, RU-Fyo and others), and those still excluded with the reason.
4. Who the rule removes. Compare sites kept and lost under (i): record length, mean annual temperature (WorldClim, already in data/snapshots), latitude and IGBP. Check whether failing days concentrate in winter months at cold sites. Report numbers only; no interpretation beyond what the numbers show.
5. Check rule 5 for every site recovered by any variant (share of daily GPP below -1), so no recovered site is one the manuscript would have dropped for negative GPP.
6. Note any site that passes the manuscript rule on our data but is not in the GSL 62 (Stage 1 found AU-Cow, AU-Rob, GF-Guy, ID-PaB, IT-BFt among them). List these separately without resolving them.
7. Figure: one row per long-record site, years on the x-axis, each year coloured by the strictest variant it passes. Mark GSL sites.

Write SESSION_LOG entries at the start, if anything goes wrong, and at the end, committing and pushing each. Save this prompt as logs/prompts/<date>_gsl_selection_audit.md. Commit and push per CLAUDE.md.
