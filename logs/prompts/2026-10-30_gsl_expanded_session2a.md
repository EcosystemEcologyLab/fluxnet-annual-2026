GSL_definition/expanded, session 2a: method changes and figure fixes, then a full rerun. Decisions from Dave after the session 1 checks (docs/checks_report_2026-10-30.md). Change the existing expanded code (00_config.R to 06_checks.R) with a config switch for each change, rerun the pipeline, and overwrite the expanded tables and figures; git history keeps a22e893. DuckDB read-only, dataset = 'FLUXMET' only; local write_meta(), never the repo-root outputs/ logs.

Data and selection
1. CUT fallback keyed to GPP as well as QC: per site, VUT if the site has VUT NEE QC and VUT GPP_NT values; else CUT if it has both for CUT; else drop. List every site recovered (AU-Cow expected) or changed.
2. Winter partitioning artifact screen (main run): a day is screened if GPP_NT_REF > 0, NEE_REF >= 0, TA_F < 0 and GPP_DT_REF < 10% of the site's unscreened P99. Days with any of those four missing are not screened. Set screened days' GPP_NT to 0 before every derived quantity (P99, skewness, GSL, F, T95, annual GPP). Keep an unscreened variant alongside the existing ones. Report screened days per site and site-year, and each site's change in P99, skewness, third membership, mean GSL at Z17 and T95.
3. Skewness thirds: recompute the cutoffs on the final site set (screened, main variant) and report them against 0.68 and 0.91.
4. Southern Hemisphere: annual GPP, NEP and temperature for the carbon-gain and temperature models summed or averaged over the Jul-Jun season year, not the calendar year.

Methods
5. Temperature sensitivity: start on mean TA_F over the 60 days before start (existing window), end on the 60 days before end, GSL on the average of those two window means. All three in Figure 6a. Write the annual-mean-temperature versions to a table only.
6. Local sensitivity in the new figures: centred difference over +/-1% of P99 (was +/-0.05%); plot absolute values throughout Figure 3.
7. Trends and detectability: keep the Weatherhead et al. (1998) calculation with each site's AR(1). Express pooled trends as % of mean GSL per decade as well as days per year. Compute years needed to detect a trend for trend sizes from 0.1 to 1.0 days per year, for Z17 and T95, by third and by group; mark 0.25 (typical) and 0.5 (strong). Drop the years-to-detect-the-observed-trend labels.
8. Bounds check (S2), redesigned: at Z17 and T95, take failing days (QC below 0.25 in passing years) whose gap-filled GPP lies within +/-2% of P99 of the threshold; report GSL with all of them counted above and all counted below, per site and pooled.
9. Mixed forest: only 2 of its 7 sites have 10 or more years, too few for a pooled trend. Drop it from trend panels and say so in the caption. Correct the session 1 report's statement that CH-Lae is the only mixed-forest site.

Figures (PeerJ: one file per figure, 300 dpi, text >= 8 pt, alphabetical panel labels, each fits 8.5 x 11 in)
- Figure 1: remove the empty "Z17 only" legend entry; unique panel letters; T95 markers in the legend's colour.
- Figure 3: +/-1% window, absolute values.
- Figure 4: label the two sites with 140 to 160 gap days.
- Figure 5: panel b axes limited to the bulk of site slopes with outliers marked at the edge; no repeated panel letters across facets; units gC m-2 per day.
- Figure 6: (a) temperature sensitivity as item 5; (b) pooled trends in % of mean GSL per decade by third and group across thresholds, with separate legends for thirds and groups; (c) years to detect against trend size for Z17 and T95.
- New supplementary figure: the main results of Figures 2, 5a and 6 with and without the winter screen.

QA: extend 06_checks.R so every existing check still runs, plus: no site mixes VUT and CUT; screened days have GPP 0 in every derived table; Southern Hemisphere models use season-year values; no figure text below 8 pt.

Report in docs/report_back_<date>.md: sites recovered; the screen's effect per site and on the headline numbers; the new thirds; every headline number beside its a22e893 value; anything that limits the results. Update the methods memo and README.

Write SESSION_LOG entries at the start, if anything goes wrong, and at the end, committing and pushing each. Save this prompt as logs/prompts/<date>_gsl_expanded_session2a.md. Log, commit and push per CLAUDE.md.
