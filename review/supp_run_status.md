# Supplementary run status

Unattended background run, four small supplementary outputs for the paper (no downloads, no
deletions, no changes to existing paper figures/snapshots/metrics files). Entries appended after
each stage; most recent stage last.

## Stage 1 — Record length, current network

**Script:** `scripts/supp_stage1_record_length_by_igbp.R`

Computed two independent per-site record-length measures for the 781-site current network,
joined to IGBP class (`PAPER_IGBP_ORDER`):
- "years with data": `compute_site_year_presence()` (`R/utils.R`) `has_data` count, reusing
  `data/snapshots/site_year_data_presence.csv` directly (confirmed identical 781-site set to the
  pinned current-network snapshot, last refreshed 2026-10-02 — not recomputed from DuckDB).
- "years with a qualifying annual NEE": `compute_site_annual_fluxes()$site_summary$n_years_nee`
  (`R/site_annual_fluxes.R`), QC_THRESHOLD_YY-gated.
- Did **not** use `data/snapshots/site_record_length.csv` (stale, different stricter definition),
  per instruction.

**Output:** `review/figures/draft_manuscript_v1/SupTables/tableS_record_length_by_igbp.csv` (+
`.meta.json`) — sites at >=5/>=10/>=20 years, by IGBP class and Total, for both measures.

**Checks:** all 781 sites classified by both measures; all 781 have a standard IGBP class (no
non-standard/missing rows to exclude); Total row (781 sites) sums correctly across both measures
and all three thresholds; counts decrease monotonically with threshold for every class, as
expected. **PASSED** — table written.

Status: **Stage 1 PASSED.**
