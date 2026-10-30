# Data and code availability

Draft statement for the MacBean and Moore growing season length (GSL) manuscript's Data and Code
Availability section, covering the FLUXNET Shuttle snapshot used, candidate Supplemental Data
groupings (PeerJ limits: 30MB per file, 50MB total), and the code-DOI requirement. Written
2026-10-30, alongside the PeerJ figure restyle
(`logs/prompts/2026-10-30_gsl_peerj_restyle.md`) — see `methods_memo.md`'s "PeerJ restyle"
section for the figure-level changes this accompanies.

## FLUXNET Shuttle snapshot and per-site product DOIs

All 62 sites' daily FLUXMET data for this analysis come from the FLUXNET Shuttle, via the
repo-root pipeline's `data/duckdb/fluxnet.duckdb` (`dataset = 'FLUXMET'` rows only — see
`docs/framework.md`'s "Data required"), itself built from:

- **Primary manifest: `data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv`** (20
  September 2026, 562KB). All 62 GSL sites are present. Each row carries, per site:
  `product_id` (the AmeriFlux FLUXNET-1F product DOI, e.g. `10.17190/AMF/2007163`) and
  `product_citation` (the full citation string: contributor name, product version/date range,
  AmeriFlux AMP, and the DOI URL). All 62 sites use ONEFlux code version v1.3.
- **Secondary manifest, comparison only: `data/snapshots/fluxnet_shuttle_snapshot_20260901T094522.csv`**
  (1 September 2026, 557KB). Used only to flag sites whose `product_id` changed between the two
  snapshots (3 found: FR-Mej, IT-Tor, NL-Vkp — see `tables/site_products.csv` and
  `docs/framework.md`), not as a data source in its own right.

Both snapshot CSVs live at the repo root (`data/snapshots/`), not under `GSL_definition/` — they
are shared, pipeline-wide manifests used by the main FLUXNET Annual Paper 2026 work as well as
this side analysis. **The per-site product DOI table a reader needs is already inside the
primary snapshot CSV** (`product_id`/`product_citation` columns, filterable to the 62 GSL site
IDs in `docs/framework.md`'s Sites table) — no separate DOI table has been built for this
analysis, to avoid a second copy that could drift from the authoritative snapshot.

For the manuscript's own reference list, every site's individual DOI must still be cited per the
FLUXNET data use policy (CLAUDE.md "Data Use and Citation"; https://data.fluxnet.org/?p=30), not
just listed in supplementary material.

## Candidate Supplemental Data groupings

All of `GSL_definition/tables/` (the git-tracked derived-table outputs; 15 CSVs, every one
machine-readable with a companion `.meta.json`) totals **~840KB**, trivially within PeerJ's
30MB-per-file and 50MB-total limits even bundled whole. Proposed grouping into logical
Supplemental Data files (S-numbers are placeholders — final numbering follows citation order in
the manuscript text, which does not yet exist in this repository; see methods_memo.md's "Re-
verification" notes on deferred manuscript figures):

| Candidate | Files | Combined size | Content |
|---|---|---|---|
| Supplemental Data S1 | `derived_site.csv`, `derived_site_year.csv` | 234KB | Core per-site and per-site-year results underlying Figures 1-4: P99, skewness (continuous and tercile), GSL/sensitivity/trend at Z17 and T95, per-site-year GSL(Z17). |
| Supplemental Data S2 | `fine_grid_summary.csv`, `fraction_sweep_summary.csv`, `tash_grid.csv` | 339KB | Threshold-sweep summaries underlying Figure 2's heat map/sensitivity curves and Figure 4's fraction-of-GPP sweep; `tash_grid.csv` is the 22-point grid used for the internal consistency check (methods_memo.md, "Check 5"). |
| Supplemental Data S3 | `benchmark_group_medians.csv`, `benchmark_site_correlations.csv` | 2KB | Figure 4/benchmark-table numbers (median common-unit sensitivity by group; per-site Z17/T95 correlations against the manuscript's published values). |
| Supplemental Data S4 | `match_check1_skewness_range.csv`, `match_check2_tercile_counts.csv`, `match_check4_t95_skew_correlation.csv`, `match_check5_grid_consistency.csv`, `tash_rule_reproduction_diff.csv`, `year_screening_options.csv`, `site_year_screening.csv`, `site_products.csv` | 149KB | Reproducibility/QA tables (Stage 1/2 match checks against the manuscript's published values, year-screening option comparison, product-ID drift flags) — supports the methods memo's reproduction checks but is not needed to read Figures 1-4 on their own. |

**Not proposed as Supplemental Data**: the large intermediate working files in
`GSL_definition/data/` (`daily_extract.rds` 10.7MB, `fine_grid.rds` 7.6MB,
`fraction_sweep.rds` 13KB) — these are gitignored R-binary caches, regenerable from the snapshot
via `01_extract_sites.R`/`03_derive.R`, and the result information a reader needs from them is
already in the much smaller `tables/*_summary.csv` files above (S2). Submitting the caches
themselves would add R-specific binary files without adding information.

## Code DOI

**`GSL_definition/` does not yet have its own archival DOI.** Before submission, this directory
(or the commit/tag covering it) needs a versioned Zenodo release — e.g. via Zenodo's GitHub
integration on `EcosystemEcologyLab/fluxnet-annual-2026`, scoped or noted as applying to the
`GSL_definition/` subdirectory specifically, since the repository as a whole also contains the
unrelated FLUXNET Annual Paper 2026 pipeline. The resulting DOI goes in the manuscript's Data and
Code Availability section alongside the FLUXNET Shuttle snapshot DOI information above. This has
not been done as of 2026-10-30.
