This folder holds the output of an **unmodified** re-run of
`scripts/diagnostics/era5_cumulative_test.R` against the current data store, run 2026-10-07.

The script itself always writes to `review/diagnostics/era5_cumulative_test/` (its own `OUTD`
constant, unchanged here). These files are a copy of that run's output, made immediately after
the run and before the original folder's committed (18 September) state was restored there via
`git checkout` — so the September outputs in `review/diagnostics/era5_cumulative_test/` were
never altered. See `../README.md`, section "Cumulative-total re-test", for the verdict
comparison and why this copy-then-restore approach was used instead of editing the script's
output path.
