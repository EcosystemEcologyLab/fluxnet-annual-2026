# Store refresh and reconciliation, 2026-09-20 — technical record

Six-stage unattended run against the extracted-data store and DuckDB database. Stages 1, 3,
4, 5, 6 code and full numeric outputs are in this directory's subfolders
(`baseline/`, `stage2/`..`stage6/`); this file is the factual record of what each stage did
and produced, for entry into SESSION_LOG.md and for downstream reference.

## Stage 0-1: preconditions and baseline

Preconditions checked: AmeriFlux credentials present, `flux_listall()` returned 781
site-products across 18 columns, 102 GiB free disk (>100 GiB threshold). No stop condition
triggered. `fluxnet-shuttle` version 0.3.7 matches the configured pin exactly.

Baseline copied to `baseline/`: snapshot CSV (`fluxnet_shuttle_snapshot_20260901T094522.csv`),
`representativeness_metrics.csv`, `site_koppen_era5.csv`, 5 `flux_medians_by_igbp_*.csv`
files, and DuckDB row counts per table (`duckdb_row_counts_before.csv`). One item from the
requested baseline set was not found under any discoverable name ("per-site counts behind
draft figures" — searched `review/figures/*.csv`, `outputs/*.csv`) and is recorded as
missing in `baseline/baseline_notes.md`.

## Stage 2: duplicate extraction directories

Five sites had two extraction directories on disk: IT-MBo, FI-Hyy, DE-Hzd, GF-Guy, IE-Cra —
in each case a stale June-2026-dated directory and a fresh September-2026-dated one. Full
listing (path/mtime/note) in `stage2/duplicate_dirs_removed.csv`. The five stale directories
were removed; the fresh one was kept at each site. DE-Hzd and GF-Guy's fresh directories
carry meaningfully wider year ranges than their stale counterparts (e.g. DE-Hzd:
2022-2024 -> 2010-2025), not merely a re-download of the same years.

## Stage 3 and 3b: downloads

Stage 3 ran the pipeline's own compare-and-download logic unmodified (live manifest vs. the
2026-09-01 snapshot): 0 new sites, 16 extended-coverage, 1 reprocessed = 17 sites queued, all
17 downloaded and verified. No numeric queue-size cap exists in `scripts/01_download.R`,
`R/snapshot.R`, `R/sync.R`, or `fluxnet::flux_download()` (all read directly) -- none was
needed or bypassed.

This 17-site queue is a strict subset of the 61 sites `review/diagnostics/store_audit/`
independently found stale (comparing on-disk extractions directly against the live archive).
The gap exists because the pipeline's snapshot-diff chain only flags a change once, at the
snapshot where it first appears; a site that changed before 2026-09-01 but was never
re-downloaded is invisible to a diff against that same snapshot. Stage 3b
(`store_refresh_stage3b_audit_gap.R`, new code, `01_download.R`/`R/snapshot.R` not modified)
explicitly downloaded the remaining 44 sites from the audit's list. Result: 43/44 downloaded
and verified; `DE-RuW` failed twice and was recorded as skipped
(`logs/store_refresh_stage3b_progress.csv`). `DE-RuW`'s own row in the audit's table matches
its "live" product name to a different site ID (`DE-Kli`), suggesting a possible site
rename/consolidation in the archive rather than an ordinary transient failure.

Disk stayed above 99 GiB free throughout both stages (`FLUXNET_DELETE_ZIPS=TRUE` kept pace
with downloads).

## Stage 4: DuckDB rebuild, QC, units, figures

Three failures occurred before this stage completed; all three are recorded in git commit
messages and in the header comments of the scripts that resolved them.

1. `INSERT OR REPLACE INTO hourly` failed: `PPFD_OUT` column not present in the `hourly`
   table's schema. A first fix sampled one site's HH file (19 missing columns added via
   `ALTER TABLE ... ADD COLUMN`, typed to match the `daily` table) and was insufficient -- a
   second site's file needed `SWC_F_MDS_6`, not caught by the single-sample check.
2. Re-run, scanning all 62 currently-extracted HH/HR files for the complete missing-column
   set (45 more columns: deep soil-sensor profiles `TS_F_MDS_{6..17}`/`SWC_F_MDS_{6..17}`
   plus their `_QC` companions, and `SW_DIF`) -- all added. Retried: `annual`/`monthly`/
   `weekly`/`daily` upserted successfully (as they had on every attempt); `hourly` then
   failed differently: "Out of Memory Error ... 92.6 GiB/92.6 GiB used ... set by
   max_temp_directory_size" -- a genuine resource limit on this 16 GB RAM machine spilling a
   now-272-column, half-hourly-resolution upsert across 30+ sites to temp disk.
3. `hourly` is outside this refresh's scope regardless (`FLUXNET_EXTRACT_RESOLUTIONS=y m d`;
   the HH/HR files present are incidental leftovers from unrelated same-day diagnostic work)
   and is not read by `04_qc.R`, `05_units.R`, or `07_figures.R` (DD/WW/MM/YY-resolution
   only, per CLAUDE.md's documented QC thresholds and figure list). New code
   (`store_refresh_stage4_duckdb_no_hourly.R`) reproduces `duckdb_update.R`'s
   annual/monthly/weekly/daily SQL verbatim and omits only the `hourly` block;
   `duckdb_update.R` itself was not edited. The manifest table this script writes excludes
   HH/HR rows specifically, so a future unmodified run of `duckdb_update.R` will still treat
   those files as unsynced rather than silently believing `hourly` is caught up.

With that in place: `annual`/`monthly`/`weekly`/`daily` update succeeded (3020s);
`04_qc.R` succeeded (73s); `05_units.R` succeeded (72s, 5 resolution tables written);
`05_units.R` logged one pre-existing, documented warning (11 `NEE_VUT_REF` values exceeding
+/-2000 gC m-2 y-1 at IT-Lav/US-Bi2 -- see `docs/known_issues.md` section 2, not introduced by
this refresh); `07_figures.R` succeeded (25s, 19/19 figures OK). `07_figures.R` wrote to the
default `OUTPUT_DIR` (`figures/`, gitignored) since that environment variable was not set to
`review/figures/` for this run -- the committed production figures were not touched.

DuckDB row-count changes (`stage5/table_duckdb_row_counts_compare.csv`): `annual` +142,
`monthly` +1,704, `daily` +51,842, `weekly` and `hourly` unchanged, `manifest` -2 (it no
longer carries HH/HR rows at all, per the design above -- not a data loss, a scope
exclusion).

## Stage 5: reconciliation

| metric | before | after | difference |
|---|---:|---:|---:|
| network site count | 781 | 781 | 0 |
| KG classified count (net) | 755 | 755 | 0 |

Net-zero conceals two opposite changes (`stage5/table_kg_classification_compare.csv`):
`IT-MBo` newly classified (Dfc; 0 -> 30 complete 1991-2020 years used); `IT-Niv` lost its
classification (was ET/Polar-tundra with 30 years; now 0 years). Zero sites changed from one
class to a different class. IT-Niv's own manifest year range extended to 2019-2025 during
this refresh; its refreshed ERA5 record's 1991-2020 window apparently no longer has 30
complete months the way its stale copy did -- not diagnosed further here.

Weighted Jaccard, KG axis, against the Beck 2023 global land-area distribution (recomputed
after fixing a column-selection bug in this task's own comparison script -- an earlier
version matched `koppen_class_code` instead of `koppen_class` and raw
`global_land_area_km2` instead of `global_land_fraction`, silently returning 0 for both):
two-letter 0.420 -> 0.419 (delta -0.0013); five-class/main-group 0.454 -> 0.453 (delta -0.0014)
-- both consistent in magnitude with 2 classification changes out of 755 sites.

Aridity, biomass, land-cover, and IGBP classification axes are reported unchanged by
construction rather than re-derived: each depends only on site coordinates joined to static
external rasters or the manifest's IGBP field, and neither changed in this refresh (0 new
sites; only flux/ERA5 values at already-known site locations were refreshed). Per-IGBP flux
medians (NEE/GPP/LE/H) were recomputed from the refreshed DuckDB
(`stage5/table_igbp_flux_medians_after.csv`); a literal before/after diff was not
reproducible from stage 1's saved baseline files, which used a different variable/join
convention -- only "after" is reported, and this gap is recorded rather than worked around.

## Stage 6: network-wide cluster recompute

Same formula (`sum(P_ERA * days_in_month)` per complete 1991-2020 site-year, mean across
years) and clustering rule (nearest of candidate factors {1,4,8,24,1000} within 15%,
`era5_precip_units_v3_partB.R`, reused verbatim) applied to the refreshed store, for all 781
sites:

| bin | n_sites |
|---|---:|
| near 1x | 323 |
| near 4x or 8x | 123 |
| elsewhere / other factor | 330 |
| insufficient data | 5 |

Essentially unchanged from `era5_precip_units_v4`'s original 123-site cluster count. `IT-MBo`
and `US-HB4` individually (`stage6/table_cluster_after_refresh.csv`): `IT-MBo`'s ratio to
BIO12 moved from 17.7x to 2.79x and it now lands in `nearest_cluster = "elsewhere"`, no
longer an extreme outlier by the magnitude that flagged it originally. `US-HB4` is
numerically identical before and after (ratio ~487x, unchanged) -- confirms its defect is not
a staleness artifact.

## Cross-check against already-circulated material

`review/diagnostics/era5_share_for_coordination/README.md` and `site_list.csv` cite IT-MBo's
ERA5 MAP as ~24,150 mm/yr and its ratio-to-measured as ~18-24x. The refreshed, verified value
is 1,136.9 mm/yr (ratio to BIO12 2.79x) -- those specific IT-MBo figures no longer hold; the
file was already marked provisional (commit `b519779`) and this refresh converts that
caveat into a confirmed correction needed for IT-MBo specifically. `US-HB4`'s entry in the
same files is confirmed to still hold (unaffected). The remaining 121 sites in that
package's 123-site cluster list are not contradicted -- stage 6's independent, network-wide
recount lands at the same cluster size (123).

## Unresolved items

- `DE-RuW`: download failed twice; its audit-table row suggests a possible rename/merge into
  `DE-Kli` rather than an ordinary retry-able failure.
- `hourly` DuckDB table: not updated for the sites with incidental HH/HR data on disk (out of
  this refresh's scope; the manifest correctly still marks those files as pending for a
  future run).
- Per-IGBP flux-median before/after diff: only "after" was computed; no reconcilable
  "before" was available in the format stage 1 saved.
- `IT-Niv`'s lost KG classification: mechanically confirmed real, cause not diagnosed.
- A new development-mode snapshot was written
  (`data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv`); promoting it to the locked
  snapshot for final analysis (CLAUDE.md's "Locking the Dataset" procedure) was not done here
  and is a separate decision.

## File index

Scripts: `scripts/diagnostics/store_refresh_stage{2..6}*.R`, `*_chain.sh`,
`*_schema_patch.R`, `*_no_hourly.R`, `*_retry*.sh`. Outputs: `baseline/`, `stage2/`
through `stage6/` in this directory. Logs: `logs/store_refresh_*`.
