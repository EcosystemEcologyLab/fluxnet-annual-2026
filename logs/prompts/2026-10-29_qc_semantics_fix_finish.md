The 4-pass run is done; the main baseline in logs/qc_fix_baseline_20261029.csv stands and is not rerun. Finish the task on branch qc-semantics-fix:

1. Step 2: count, per resolution, the rows 04_qc.R keeps with qc_unknown = TRUE, and the sites they cover.
2. Two cleanups:
   a. In scripts/04_qc.R, replace the inlined p_gapfilled > (1 - threshold) gate with a direct comparison: select the per-site QC value (qc_val, via the existing case_when) and set qc_flagged = !qc_unknown & qc_val < threshold. Keep p_gapfilled only if something downstream reads it.
   b. Make R/site_annual_fluxes.R source R/qc.R itself (guarded so a second source is harmless), so no caller can break by forgetting it.
3. Rerun the branch passes only (one pass is enough), with nothing else writing to the database or output folders. Compare to the main baseline: same allowed differences as before (the two legend files), identical *_qc row counts.
4. Step 6c: dry run at 0.75 with no outputs saved. Counts fall at every resolution, and every record kept at 0.75 is kept at 0.50.
5. Steps 6d and 6e: command grep finds no (1 - QC) or p_gapfilled-based gate outside legacy/; scripts/check_figure_format.R passes.
6. Exclusion log size: report the path of outputs/exclusion_log.csv each pass wrote to (main worktree vs repo root). That will show whether the 29.5M to 1.5M drop was two different files.
7. If everything passes, merge to main and push. If not, push the branch and say which check failed.

Write a SESSION_LOG entry at the start, if anything goes wrong, and at the end (done, not done, next step), committing and pushing each one. Save this prompt as logs/prompts/<date>_qc_semantics_fix_finish.md.
