# WAFNET x FLUXCOM comparison — Stage 3b report back (2026-11-21)

Prompt: `logs/prompts/2026-11-21_wafnet_package_v2.md`.

Stage 3b rebuilt Stage 3's data package for Ossénatou Mamadou
(`package_ossenatou_2026-11-21/`, commit 2653513) as a fully traceable release: every
value in the new package (`package_ossenatou_2026-11-21_v2/`) carries a key back to a
provenance row, which in turn names an exact source file, checksum, and persistent
identifier. Same six sites, QC rule (monthly QC fraction >= 0.50), VUT/CUT choice, and
sign conventions as Stage 3. FLUXCOM-X-BASE remains non-Shuttle comparison data
(`CLAUDE.md` Hard Rule #1). Stage 3's own package is left on disk unchanged, marked
superseded in `WAFNET/fluxcom_comparison/README.md`.

This file is named `report_back_2026-11-21b.md`, distinct from Stage 3's own
`report_back_2026-11-21.md` (same calendar date, different stage) so as not to overwrite it.

## 1. Download

Before downloading anything, the 84 FLUXCOM-X-BASE 0.05 degree files (same ICOS SPARQL
discovery query as Stage 3) were compared file-by-file (name, expected size, SHA-256)
against Stage 3's own manifest (`tables/fluxcom_x_base_005_download_manifest.csv`). All
84 matched exactly — no mismatch, so the stop-and-report condition never triggered.

Re-downloaded the same 84 files (GPP, NEE, ET, ET_T x 2001-2021), one at a time
(download, sha256-verify, extract, delete global file), same anonymous ICOS
licence-accept mechanism as Stage 2/3 — no login, no credentials printed or stored.

- **Total download size: 84 files, 36.865 GB** (same total as Stage 3, as expected since
  these are the same source files).
- All 84 rows in the new manifest (`tables/fluxcom_x_base_005_download_manifest_v2.csv`)
  have `sha256_verified = TRUE` and `extracted = TRUE`.
- `data/external/fluxcom_x_base_005/` is empty of data afterwards (only the gitignored
  ICOS session-cookie file remains).

This time, instead of keeping only the cell containing each tower (Stage 3), the 3x3
block of cells (9 cells) centred on each tower's cell was extracted for all six sites —
confirmed empirically from the extracted coordinates that `row_offset = -1` is one cell
north and `col_offset = -1` is one cell west (not assumed). Raw extraction table
(`tables/xbase_005_neighbours_extraction_raw.csv`) has 54,432 rows = 6 sites x 9 cells x
4 variables x 21 years x 12 months, exactly as expected.

## 2. Snapshot match result, per site

`tables/tower_provenance.csv` checks, for each site, that the locally extracted FLUXMET
directory name matches the snapshot's own `fluxnet_product_name`. All six matched; none
triggered the stop condition (which would have required asking before substituting or
re-downloading tower data).

| Site | snapshot_version_match | product_version | oneflux_code_version | carbon_src |
|---|---|---|---|---|
| GH-Ank | TRUE | v1.3_r1 | v1.3 | VUT |
| BJ-Nhu | TRUE | v1.3_r1 | v1.3 | VUT |
| BJ-Bfg | TRUE | v1.3_r1 | v1.3 | VUT |
| SN-Dhr | TRUE | v1.3_r1 | v1.3 | VUT |
| ML-Kem | TRUE | v1.3_r1 | v1.3 | VUT |
| SN-Nkr | TRUE | v1.3_r1 | v1.3 | VUT |

All six sites use the VUT (variable u* threshold) method; none fell back to CUT.

## 3. Package contents

`package_ossenatou_2026-11-21_v2/` — 17 files, **4.045 MB** total (including
`MANIFEST.csv`), well under the 10 MB stop threshold. Zipped alongside it
(`package_ossenatou_2026-11-21_v2.zip`, 1.2 MB).

| File | Rows | Size |
|---|---|---|
| `tower_monthly.csv` | 516 | 121,395 B |
| `xbase_005_monthly.csv` | 1,512 | 332,512 B |
| `xbase_005_neighbours_monthly.csv` | 13,608 | 3,053,486 B |
| `sites.csv` | 6 | 2,104 B |
| `summary.csv` | 22 | 1,899 B |
| `tower_provenance.csv` | 6 | 3,279 B |
| `xbase_provenance.csv` | 84 | 34,183 B |
| `README.md` | — | 10,597 B |
| `check_figure_xbase_005_vs_tower_DRAFT.png` + `.legend.txt` | — | 468,342 + 616 B |
| `MANIFEST.csv` | 17 (listing every other file) | — |

**One gap caught and fixed before finalizing**: the first build of `16_package_ossenatou_v2.R`
wrote `tower_provenance.csv`/`xbase_provenance.csv` only to the analysis-level `tables/`
directory, not into the package itself, even though every data file in the package
references them by key. Fixed by copying both provenance tables (and their `.meta.json`
companions) into `package_ossenatou_2026-11-21_v2/` before writing `MANIFEST.csv`; the
package was then rebuilt (packaging -> check figure -> MANIFEST) from scratch with the fix
in place. The numbers in this report are from that rebuilt version.

## 4. Check figure

Opened `check_figure_xbase_005_vs_tower_DRAFT.png` (NEE/GPP/LE panels, monthly,
QC-passing months only, six sites coloured, X-BASE training sites marked with triangles,
1:1 dashed line). What was checked: the pattern is unchanged from Stage 3's own check
figure (expected, since the figure uses only centre-cell values, identical to Stage 3's
single extracted cell) — GPP and LE show a reasonably tight cloud around the 1:1 line
with the known X-BASE seasonal-amplitude compression; NEE shows the known sign/scatter
behaviour already documented in Stage 2/3. One visibly high LE outlier near
tower-LE ~350 W/m^2 (olive = BJ-Bfg) is consistent with the flagged BJ-Bfg March 2012
value (357.5 W/m^2, QC 0.515) documented in the README's flagged-values section — not a
new or unexplained point.

## 5. Three traced values (package -> key -> source)

**(a) Tower value.** `tower_monthly.csv`: GH-Ank, 2011-02, `NEE_VUT_REF = -2.73921`
(`NEE_VUT_REF_QC = 0.609375`, `NEE_pass_qc = TRUE`). Traced via `site_id` to
`tower_provenance.csv`'s GH-Ank row: `fluxmet_mm_file_name =
EUF_GH-Ank_FLUXNET_FLUXMET_MM_2011-2014_v1.3_r1.csv`,
`fluxmet_mm_file_sha256_hex = add835214942b2dda6739dd757f772eb50e78e243b27c91d62dd8c04a717c2b7`,
`dataset_pid_url = https://hdl.handle.net/11676/KGeOFn8sIXSOmMXfGprQynOP`. Checked
directly against the source FLUXMET file on disk
(`data/extracted/EUF_GH-Ank_FLUXNET_2011-2014_v1.3_r1/EUF_GH-Ank_FLUXNET_FLUXMET_MM_2011-2014_v1.3_r1.csv`):
row `TIMESTAMP = 201102` has `NEE_VUT_REF = -2.73921`, `NEE_VUT_REF_QC = 0.609375` —
exact match — and the file's own SHA-256 (`shasum -a 256`) is
`add835214942b2dda6739dd757f772eb50e78e243b27c91d62dd8c04a717c2b7` — exact match to the
provenance row.

**(b) X-BASE centre-cell value.** `xbase_005_monthly.csv`: GH-Ank, 2011-02,
`GPP_gC_m2_d1 = 5.218518`, `source_key_GPP = GPP_2011_005`. Traced via `source_key` to
`xbase_provenance.csv`'s `GPP_2011_005` row: `icos_object_pid_url =
https://meta.icos-cp.eu/objects/stof_RS_TceXdQMZkXPzXlUz`, `sha256_hex =
b2da1ffd14bf4dc7977503199173f35e553300f2ed183a298cf04f282a5f1bd8`,
`collection_doi = 10.18160/5NZG-JMJE`. Checked against the raw extraction table
(`tables/xbase_005_neighbours_extraction_raw.csv`, row_offset=0/col_offset=0): same value
(5.218518), same cell centre (lon -2.675, lat 5.275), distance 2.24 km from the tower.
Checked against the download manifest v2 row for GPP/2011: `expected_size_bytes =
actual_size_bytes = 440,020,609`, `sha256_hex` identical to the provenance row, exact
match. (The source `.nc` file itself was deleted after verified extraction, per the
same one-file-at-a-time disk-space design as Stage 3 — the chain is traced through the
recorded checksums and PID, not by re-opening the original binary.)

**(c) Neighbour-cell value.** `xbase_005_neighbours_monthly.csv`: GH-Ank, 2011-02,
`row_offset = -1`, `col_offset = 1` (one cell north, one cell east of the tower's cell),
`GPP_gC_m2_d1 = 5.182652`, `source_key_GPP = GPP_2011_005` (same source file as the
centre cell, different cell within it). Cell centre (lon -2.625, lat 5.325), 9.91 km from
the tower (GH-Ank tower at lon -2.694206, lat 5.268543) — the northward/eastward shift is
consistent with the documented offset convention. Matches the raw extraction table
exactly (same site/year/variable/month/row_offset/col_offset row).

## 6. Corrections made to the Stage 3 README, carried into v2

- The X-BASE method/product paper's own DOI (`10.5194/bg-21-5079-2024`, Nelson et al.
  2024, Biogeosciences) had been given a second time where the dataset DOI
  (`10.18160/5NZG-JMJE`) belonged. Split into two separate citation paragraphs.
- VUT/CUT was described as a GPP/RECO partitioning choice; corrected to describe it as
  the variable vs. constant u* (friction velocity) threshold method for
  storage/turbulence filtering, a different choice from daytime/nighttime partitioning.
- The QC rule is now stated as "our own threshold choice throughout this analysis"
  rather than implying it is a published convention.
- Pointers to internal repository documents replaced with the repository URL
  (`https://github.com/EcosystemEcologyLab/fluxnet-annual-2026`).

## 7. Anything unexpected

- The missing-provenance-tables-in-the-package gap (section 3 above) — caught before
  committing, fixed, and the package rebuilt from scratch with the fix.
- `ML-Kem`'s FLUXMET extract has no `SWC_F_MDS_1` column at all (present at the other
  five sites); `tower_monthly.csv` carries `NA` for that site/variable throughout, per
  the instruction to mark unavailable variables as missing, not zero.
- `LE_CORR`/`H_CORR` have no QC column of their own anywhere in this product (confirmed
  by grep against the FLUXMET header) — documented in the README as a genuine product
  limitation, not an extraction oversight.
- Four tower LE site-months pass QC but look seasonally high; flagged (not removed) via
  a new `LE_flagged_for_review` column, values confirmed against the raw FLUXMET data
  before implementing: BJ-Bfg 2012-03 = 357.537 W/m^2 (QC 0.515457); BJ-Nhu 2010-08 =
  204.572 (QC 0.919355), 2011-12 = 214.059 (QC 0.913306), 2012-01 = 213.851 (QC 0.906586).
- Nothing else unexpected: the pre-flight integrity check against Stage 3's manifest
  passed cleanly for all 84 files on the first attempt (no stop condition triggered), and
  the `system2()`/`curl` shell-quoting bug from Stage 3's first draft was avoided this
  time by writing with `shQuote()` from the start.

## 8. What is unchanged from Stage 3

Stage 2's outputs (`tables/`, `figures/`, `docs/report_back_2026-11-03.md`, all at 0.5
degree) and Stage 3's own package (`package_ossenatou_2026-11-21/`) are untouched —
confirmed via `git status` showing no modifications to any of those paths, only new
Stage 3b files (scripts `14`-`18`, new tables, the new package directory and its zip).
