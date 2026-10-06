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

## Stage 3 — Sampling ratios behind Figure 5 and Figure S4

**Script:** `scripts/supp_stage3_sampling_ratios.R`

Built a long table (axis, comparison, class, land_share, tower_share, tower_count,
n_towers_classified, sampling_ratio, log2_ratio) for all 6 axes x 2 comparisons strictly from
committed files (`site_*_fig4.csv`, `site_biomass_cci_v7.csv`, `*_global_distribution.csv`) — no
raster re-extraction, no other snapshot files. Classes with land and no towers got
`sampling_ratio = 0` per instruction.

**Check (recomputed weighted Jaccard per axis x comparison vs. `representativeness_metrics_fig4.csv`,
not modified): 10 / 12 agree to 6 decimals exactly.** The 2 that don't, both for a structural reason
identified before running, not a computation bug:
- **koppen / geo_vs_geo — not computable at all** from the permitted files: `beck2023_kg_class` in
  `site_koppen_era5_fig4.csv` is entirely `NA` for all 781 sites; the true source
  (`site_koppen_beck2023.csv`) is outside the Stage 3 file restriction.
- **aridity / geo_vs_geo — computed 0.605 vs. published 0.666 (diff −0.061)**: substituted the
  ERA5-derived AI (`site_aridity_era5_fig4.csv`, the figure's own Geo-vs-Data source for this axis)
  because the true Geo-vs-Geo source (`site_aridity.csv`, CGIAR raster at tower) is likewise outside
  the file restriction.

Both are flagged with explicit notes in the output rather than silently forced to agree.

**Per instruction, since not all 12 agree:** did **not** write `tableS_sampling_ratios_by_axis.csv`.
Wrote `tableS_sampling_ratio_jaccard_check.csv` instead (always written) — all 12 rows, computed vs.
reference J, diff, and a note explaining the 2 mismatches. Also wrote
`tableS_sampling_ratio_extremes.csv` unconditionally (3 lowest / 3 highest sampling-ratio classes
per axis x comparison among classes holding ≥1% of land; koppen/geo_vs_geo excluded as not
computable) — the 11 remaining computable axis x comparison combinations x 6 rows each = 66 rows,
matching the script's output exactly.

Status: **Stage 3 PASSED validation gate as designed — produced the "otherwise report which differ"
output rather than the full table, which is the correct and expected outcome given the Stage-3 file
restriction.**

## Stage 4 — Bowen ratio by vegetation class

**Script:** `scripts/supp_stage4_bowen_ratio_by_igbp.R`

Read the pre-QC `annual` table (DuckDB, `dataset='FLUXMET'`, never `annual_qc`/`annual_converted`).
Site-years require `H_F_MDS_QC` and `LE_F_MDS_QC` to **each independently** satisfy
`QC_THRESHOLD_YY` (same `(1 - QC) <= threshold` formula as `R/site_annual_fluxes.R`'s own
`h_qualifies`/`et_qualifies`): 4,487 / 6,336 site-years qualified on both. `LE_F_MDS <= 0`: **0
site-years dropped** (none occurred in the qualifying set). Bowen ratio = `H_F_MDS / LE_F_MDS`
(both already native W m⁻² mean rates — no unit conversion needed or applied). Site value = median
over that site's own qualifying site-years (665 sites, 4,487 site-years total).

**`H_CORR`/`LE_CORR`:** both columns **exist** in the `annual` table — `H_CORR` has ≥1 non-NA value
for 437 / 781 current-network sites, `LE_CORR` for 436 / 781. **Neither was used** in the Bowen
ratio computation, per instruction (report only).

**Output:** `review/figures/draft_manuscript_v1/SupTables/tableS_bowen_ratio_by_igbp.csv` — per IGBP
class (+ Total) n sites, n site-years, median/25th/75th percentile of site values, and
`flag_small_n` (TRUE for <5 sites — only SNO, 1 site, is flagged). Values are ecophysiologically
sensible (lowest Bowen ratio in wetlands/croplands ~0.33–0.36, highest in sparse/dry shrubland and
savanna classes ~1.2–2.2), consistent with expected energy-partitioning behaviour, which is a
sanity check in itself. **PASSED** — table written.

Status: **Stage 4 PASSED.**

---

## Follow-up fixes — 2026-10-06

### Fix 1 — Figure S7 panel a: stack order, colours/alpha, label placement

**Script:** `scripts/supp_stage2_record_length_collections_figure.R`

Panel a's IGBP stack was built from a plain character `igbp` column, so `geom_histogram()`
stacked classes in whatever order it encountered them rather than `PAPER_IGBP_ORDER`'s factor
order -- out of step with the legend key and with Figure 2's own stacking
(`fig_cumulative_siteyears_igbp()`, which explicitly sets `factor(igbp, levels = fig1b_igbp_order)`
before stacking). Fixed by converting `igbp` to `factor(igbp, levels = PAPER_IGBP_ORDER)` before
plotting. Fill colours already came from `scale_fill_paper_igbp()` (the same `PAPER_IGBP_COLOURS`
Figure 2 uses) and needed no change; added `alpha = 0.8` to `geom_histogram()` to match Figure 2's
`geom_area(alpha = 0.8)`. The `n=` threshold labels were anchored at the same x position as their
dashed vertical line (`THRESHOLDS - 0.5`), so the line visually crossed the text; moved the labels
one bin to the right (`THRESHOLDS + 0.5`), just inside the ">= threshold" side, clear of the line.

Rebuilt `figS7_record_length.{png,pdf,jpg}` + `.legend.txt` (same thresholds/counts as before:
n=483/225/63 at >=5/10/20 years -- this fix changed presentation only, not the underlying counts),
rebuilt `supplementary_figures.pdf` (7 pages, 0.67 MB), and reran `scripts/check_figure_format.R`:
**13/13 figures PASS**, including figS7.

Status: **Fix 1 PASSED.**

### Fix 2 — Sampling-ratio table: koppen/aridity geo_vs_geo now computable

**Script:** `scripts/supp_stage3_sampling_ratios.R`

Stage 3's original file restriction (`site_*_fig4.csv` / `site_biomass_cci_v7.csv` tower files
only) excluded exactly the two files `figure4_representativeness.R` itself reads for the
koppen/geo_vs_geo and aridity/geo_vs_geo panels -- `data/snapshots/site_koppen_beck2023.csv`
(`koppen_twoletter`) and `data/snapshots/site_aridity.csv` (`unep_class_7`) -- which is why those
two combinations were previously "not computable" / a documented substitute. Both are
already-committed snapshot CSVs (prior raster-at-tower extractions), not rasters themselves, so
reading them directly is still "no raster re-extraction." Both classify all 781 current-network
sites with zero NA, so no fallback was needed once the correct file was used.

Swapped both axes to read these two files for their `geo_vs_geo` comparison only (`geo_vs_data`
unchanged) and recomputed all 12 weighted Jaccard values against
`data/snapshots/representativeness_metrics_fig4.csv` (not modified): **12 / 12 agree to 6
decimals** (koppen/geo_vs_geo: 0.372 vs 0.372; aridity/geo_vs_geo: 0.666 vs 0.666; the other 10
unchanged from the original run). Per instruction, since all 12 now agree, wrote
`tableS_sampling_ratios_by_axis.csv` (the full long table, not written in the original run) and
refreshed `tableS_sampling_ratio_jaccard_check.csv`.

Extremes table (`tableS_sampling_ratio_extremes.csv`) also refreshed, using the new selection rule
(classes with `sampling_ratio < 0.5` or `> 2`, among classes holding >=1% of land, in place of the
original fixed three-lowest/three-highest) and now including koppen/geo_vs_geo (previously
excluded as not computable). Gated on the same 12/12 agreement as the main table, per instruction.

Status: **Fix 2 PASSED.**

---

RUN COMPLETE
