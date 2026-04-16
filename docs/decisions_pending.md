> Technical bugs and data quality issues with external dependencies (fluxnet R package,
> FLUXNET Shuttle) are tracked in [docs/known_issues.md](known_issues.md), not here.

## QC threshold review needed — RESOLVED 2026-04-14

~~US-Ha1 (Harvard Forest) excluded entirely at QC_THRESHOLD_YY = 0.75 — all 35 FLUXMET YY records exceed gap-fill threshold. Threshold lowered to 0.50 temporarily.~~

**Decision (2026-04-14):** Row exclusion now gated on `NEE_VUT_REF_QC` only. Gating on all `_QC` columns caused 347 of 672 sites (52%) to lose every annual record because a single secondary variable (e.g., `RECO_NT_VUT_REF` with high winter gap-fill) contaminated all years even when NEE itself was well-observed. Secondary variable QC columns (GPP, RECO, LE, H) are retained in the output for per-variable filtering by downstream figure functions but do not drive row exclusion. Implemented in `R/qc.R` and `scripts/04_qc.R`.

**Still open:** Final agreement with co-authors on QC_THRESHOLD_YY = 0.50 vs. 0.75 vs. no threshold. Current pipeline uses 0.50.

---

## NEE_CUT_REF fallback for sites missing NEE_VUT_REF — OPEN

Of 672 sites in the current dataset, 36 have valid `NEE_CUT_REF` (Constant U* Threshold)
but no `NEE_VUT_REF` (Variable U* Threshold). An additional 106 sites have neither.

**Decision needed:** Should the pipeline fall back to `NEE_CUT_REF` for the 36 sites
where VUT is absent but CUT is present? VUT is the standard ONEFlux product for the
Annual Paper, but excluding 36 sites purely due to a missing processing variant seems
avoidable.

**Current behaviour:** `NEE_VUT_REF` is the primary QC gate and primary analysis variable.
Sites missing `NEE_VUT_REF` are retained in the output with `NEE_VUT_REF = NA` throughout.
They appear in `outputs/sites_nee_cut_only.csv`.

**Options:**
1. Keep VUT-only (current) — simplest; 36 sites contribute no NEE data
2. Fall back to CUT for VUT-absent sites — adds 36 sites but mixes processing variants
3. Provide CUT as a parallel column — downstream figures choose which to use

**Action:** Raise with co-authors. Do not implement fallback until decision is reached.
See `docs/known_issues.md` Section 3 for full counts and file references.

---

## Methods section — sites excluded due to data gaps

Dario Papale confirmed (2026-04-16) that sites with all-missing `NEE_VUT_REF` have gaps
exceeding 15 consecutive days throughout their record — ONEFlux correctly withholds annual
estimates in these cases. This needs a paragraph in the paper methods section explaining:

1. The 15-day gap threshold in ONEFlux and why annual estimates are not computed when this
   threshold is exceeded for all years in a site's record
2. The number of sites excluded on this basis (106)
3. The use of `NEE_CUT_REF` as fallback for 36 additional sites where VUT could not be
   calculated due to insufficient u* threshold statistics
4. The final site count used in analysis (530 VUT + up to 36 CUT fallback, pending
   co-author decision on NEE_CUT_REF fallback — see entry above)

Contact Gilberto Pastorello (LBL) for ONEFlux processing documentation to support
methods writing (flagged by Dario Papale, 2026-04-16).
