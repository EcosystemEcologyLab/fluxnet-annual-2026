Fix the QC threshold semantics across the pipeline. At DD/WW/MM/YY the NEE, LE and H _QC flags are the fraction of measured or good-quality gap-filled data (Pastorello et al. 2020, SM1). The rule is therefore "keep when QC >= QC_THRESHOLD_*", and raising a threshold must be stricter. The code tests (1 - QC) <= threshold instead. That is equivalent only at 0.50; at 0.75 it keeps QC >= 0.25. Current results at 0.50 are expected to be unchanged by this fix, and the checks below must show that.

Work on a branch, qc-semantics-fix. Merge to main only if every check in step 6 passes.

1. Baseline, on main before any code change.
   - Use review/figures/FIGURE_INVENTORY.md to list the scripts behind every current main figure, supplementary figure and supplementary table, plus scripts/04_qc.R and the TowerTango and WAFNET/fluxcom_comparison scripts.
   - Run them twice. Record md5 hashes of every output PNG and CSV, the text of every .legend.txt, and the row counts of each *_qc table by resolution, in logs/qc_fix_baseline_<date>.csv.
   - Any output whose hash differs between the two baseline runs is nondeterministic. For those, record the underlying data values instead of the image hash.
   - Exclude PDF and meta.json files, which differ only by timestamp. Restore any tracked files the runs modified (git checkout) so the baseline leaves main unchanged.
   - If a listed script fails at baseline, record it and leave it out of the comparison.

2. Missing flags. Count, per resolution, the rows 04_qc.R keeps because the NEE QC flag is NA, including sites with no QC column. Report the counts and how many sites they cover.

3. The fix.
   - Add one helper in R/, qc_passes(qc, threshold): TRUE when qc >= threshold, FALSE when below, NA when qc is NA.
   - Replace every (1 - QC) gate with it. Known locations: scripts/04_qc.R, R/site_annual_fluxes.R, compute_site_record_length.R, supp_stage4_bowen_ratio_by_igbp.R, the dq_stage* diagnostics, TowerTango/code/update_cards.R, WAFNET/fluxcom_comparison/code/01_observations_qc_overlap.R. Find the rest with command grep (not plain grep) for "1 - .*_QC", "1-.*_QC" and "p_gapfilled".
   - NA handling stays as it is now. In 04_qc.R, rows with an NA flag are kept, because dropping them would remove the whole row (LE, H, meteorology) at sites without an NEE flag. Add a qc_unknown column marking them, and write their counts to the exclusion log as "QC unknown (NA), kept". Per-variable gates (compute_site_annual_fluxes() and the others) continue to treat NA as not qualifying.
   - Docs: in CLAUDE.md and R/pipeline_config.R, state the rule as "keep QC >= QC_THRESHOLD_*" wherever it appears (several lines say "> threshold"), and state the NA handling for both cases. Correct the pipeline_config.R comment "Lowered from 0.75 to 0.50" if step 7 shows it is misleading. Update rule wording that scripts write into legend files to "QC >= QC_THRESHOLD_*".

4. Tests in tests/ for qc_passes() at thresholds 0.50 and 0.75, with QC values 0.24, 0.25, 0.49, 0.50, 0.74, 0.75, 1.00 and NA. Expected: pass when QC >= threshold; NA returns NA.

5. Hard-coded 0.80. Correct the stale "0.80" legend text written by scripts/figure_flux_medians_by_igbp.R. Find the scripts that still hard-code a QC threshold of 0.80 (about six; use command grep). Switch each to qc_passes() with QC_THRESHOLD_YY, or move it to legacy/ if nothing current uses it. For each, say which applied and whether it feeds any output in the step 1 list.

6. Checks. All must pass:
   a. The tests pass.
   b. Rerun the step 1 list on the branch. Every PNG and CSV hash, every *_qc row count, and every nondeterministic output's data match the baseline. The only allowed differences are:
      - legend text changed only in the QC rule wording;
      - the new qc_unknown column;
      - outputs changed by step 5, each listed with before and after values.
   c. Dry run at 0.75, saving no outputs. Every record kept at 0.75 is also kept at 0.50, and counts fall at every resolution. Report the counts.
   d. command grep finds no remaining (1 - QC) gate outside legacy/.
   e. scripts/check_figure_format.R still passes.

7. History. Find the commit where the thresholds changed from 0.75 to 0.50. Say whether the code then used the (1 - QC) form, and so what 0.75 actually kept. List any outputs produced at 0.75 and any text describing them as a stricter filter.

If any check fails, do not merge. Push the branch and report which check failed and why.

Save this prompt as logs/prompts/<date>_qc_semantics_fix.md. Log, commit and push per CLAUDE.md.
