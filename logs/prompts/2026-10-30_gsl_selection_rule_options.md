SL_definition: choose an expanded site-selection rule. Goal: stay as close as possible to the manuscript's rules while recovering the long-record sites the "every day passes" rule loses. Extend the selection audit (code/11_selection_audit.R and its tables); read-only on DuckDB, FLUXMET rows only. Do not rerun the GSL analysis yet.

Unchanged from the manuscript: rule 1 (drop IGBP CRO, CVM, WET, SNO, BSV); daily test QC >= 0.25 (qc_passes(); NA fails); at least 5 passing years per site; rule 5 (drop sites with > 10% of daily GPP below -1). Tropical sites are treated like any other site.

Changes to evaluate:
- CUT fallback, per site as in scripts/04_qc.R: NEE_VUT_REF_QC if the site has any non-NA VUT QC, otherwise NEE_CUT_REF_QC. Never mixed within a site. CUT sites use GPP_NT_CUT_REF.
- Year rules:
  R0  manuscript: every day passes (VUT only; the baseline);
  R1  every day passes, with the CUT fallback;
  R2  R1, but allow up to N failing days per year, N = 3, 7, 14;
  R3  R1, but ignore failing days whose gap-filled GPP lies outside the band 0 to 25% of the site's P99, allowing M = 0 or 3 failing days inside the band. Compute P99 per site from days that pass the daily test, in years that pass R1, or from all passing days if the site has no R1 years; state which.

For each rule report:
- sites, site-years, and trend sites (>= 10 years);
- how many of the 37 long-record sites not in the GSL 62 are recovered, listing those still lost, with the reason;
- confirmation that all 505 site-years of the GSL 62 still pass (each rule must contain the manuscript's set);
- for R3, how many failing days fall inside the band, by month.
Check rule 5 for every newly added site.

Recommend the least-relaxed rule that recovers all long-record sites recoverable at all (or say which are unrecoverable and why), weighing R2 and R3 by how much each adds beyond the long-record sites. Write docs/selection_rule_options_<date>.md with a summary table, and stop for Dave's decision.

Write SESSION_LOG entries at the start, if anything goes wrong, and at the end, committing and pushing each. Save this prompt as logs/prompts/<date>_gsl_selection_rule_options.md. Commit and push per CLAUDE.md.
