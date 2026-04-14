> Technical bugs and data quality issues with external dependencies (fluxnet R package,
> FLUXNET Shuttle) are tracked in [docs/known_issues.md](known_issues.md), not here.

## QC threshold review needed — RESOLVED 2026-04-14

~~US-Ha1 (Harvard Forest) excluded entirely at QC_THRESHOLD_YY = 0.75 — all 35 FLUXMET YY records exceed gap-fill threshold. Threshold lowered to 0.50 temporarily.~~

**Decision (2026-04-14):** Row exclusion now gated on `NEE_VUT_REF_QC` only. Gating on all `_QC` columns caused 347 of 672 sites (52%) to lose every annual record because a single secondary variable (e.g., `RECO_NT_VUT_REF` with high winter gap-fill) contaminated all years even when NEE itself was well-observed. Secondary variable QC columns (GPP, RECO, LE, H) are retained in the output for per-variable filtering by downstream figure functions but do not drive row exclusion. Implemented in `R/qc.R` and `scripts/04_qc.R`.

**Still open:** Final agreement with co-authors on QC_THRESHOLD_YY = 0.50 vs. 0.75 vs. no threshold. Current pipeline uses 0.50.
