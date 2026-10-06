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

## Stage 2 — Record length per collection, and Figure S7

**Script:** `scripts/supp_stage2_record_length_collections_figure.R`

Reused `scripts/collection_comparison_table.R`'s own list-reading code (not reimplemented) for
per-site years in Marconi (`Years in Marconi` range span only), La Thuile (1991–2007
year-indicator columns of `LaThuileList.xlsx`, not `years_la_thuile.csv`'s span sum), and
FLUXNET2015 (two-digit year columns of `FLUXNET2015.xlsx`, second header row dropped — hit the
documented trap exactly: the dropped row's first cell is `"Site ID"` plus a trailing U+00A0
non-breaking space, not a plain trailing space — `trimws()` alone did not catch it, fixed with an
explicit `gsub(" ", "", ...)` first). Current network reused Stage 1's
`compute_site_year_presence()` measure.

**Validation gate (ran before writing anything):** computed site-year totals compared against
96 / 965 / 1,532 and `data/snapshots/collection_sites_siteyears.csv`'s own Current total (6,200).
All four matched exactly — **PASSED**, figure written.

**Output:** `review/figures/draft_manuscript_v1/SupFigs/figS7_record_length.{png,pdf,jpg}` +
`.legend.txt`, two panels (a: current-network years-with-data histogram stacked by IGBP, dashed
lines at 5/10/20 years labelled n=483/225/63, matching Stage 1's Total row exactly; b: share of
sites with ≥n years as step lines for all four collections/generations, Figure-2-matching colours,
current network dark grey, n sites in the legend key). Legend states the two different meanings
of "a year" and that Marconi values are spans, per instruction.

**Sizing note:** built at 180 mm wide (Extended-Data-style SupFigs limit), not the task's stated
183 mm — every existing SupFigs figure (S1–S6) is ≤179.9 mm wide, and
`scripts/check_figure_format.R` hard-fails anything over 180 mm in that directory (confirmed: a
first attempt at 183 mm failed the check). 183 mm-wide panels in this repo are main-text figures
in `draft_manuscript_v1/` itself, not `SupFigs/`. Treated 183 mm as the task's approximate "roughly
double-column width" rather than a literal, checker-breaking instruction.

Added `figS7_record_length` to `scripts/build_supplementary_pdf.R`'s `STEMS`, rebuilt
`supplementary_figures.pdf` (7 pages, 0.67 MB), and ran `scripts/check_figure_format.R`: **13/13
figures PASS**, including figS7.

Status: **Stage 2 PASSED.**
