Follow-up to the merged QC semantics fix (09e621c).

1. Split the qc_unknown counts in each *_qc table by the dataset column (FLUXMET vs ERA5) and by resolution. Report how many FLUXMET rows are qc_unknown and at how many sites, and correct the explanation in the 2026-10-29 SESSION_LOG entry if the ERA5 rows account for most of the unknowns.
2. In scripts/04_qc.R, replace the row-by-row "QC unknown (NA), kept" logging with one row per site, resolution and dataset giving the count of kept unknown rows. Restrict it to FLUXMET rows, since ERA5 rows never carry NEE QC. Confirm the *_qc tables are unchanged.
3. Add a note to CLAUDE.md that outputs/exclusion_log.csv is append-only and grows on every 04_qc.R run. Either clear it at the start of each run or add a run_id column; choose one and say why.

Write SESSION_LOG entries at start and end, committing and pushing each. Save this prompt as logs/prompts/<date>_qc_unknown_followup.md.
