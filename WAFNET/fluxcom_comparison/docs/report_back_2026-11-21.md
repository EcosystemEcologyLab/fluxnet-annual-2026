# WAFNET x FLUXCOM comparison — Stage 3 report back (2026-11-21)

Stage 3: download FLUXCOM-X-BASE at its native 0.05 degree monthly grid and package a
self-contained data deliverable for Ossénatou Mamadou's own analysis. This is not a new
figure set or a new statistical analysis of the Annual Paper — the deliverable is the
package itself (`package_ossenatou_2026-11-21/`), with one check figure to confirm the
package is sane before handing it over. Stage 2's outputs (`tables/`, `figures/`,
`docs/report_back_2026-11-03.md`, all at 0.5 degree) are unchanged; confirmed via
`git status` showing only new Stage 3 files (scripts `11`–`13`, new tables, the new
package directory and its zip).

## 1. Download

FLUXCOM-X-BASE monthly 0.05 degree collection
(`https://meta.icos-cp.eu/collections/zfwf1Ak2I7OlziGDTX8Xl6_T`, same DOI as Stage 2's
0.5 degree files, `10.18160/5NZG-JMJE`): **84 files, 36.87 GB total** (GPP, NEE, ET,
ET_T, 2001-2021, 21 years x 4 variables). No login was required — same anonymous
ICOS licence-click mechanism as Stage 2 (`GET /licence_accept?ids=[...]` sets a cookie,
then `GET /objects/<hash>` serves the file). No credentials were printed or stored at
any point.

Downloaded one file at a time (download, sha256-verify, extract the six site cells,
append to the manifest, delete the global file) so `data/external/fluxcom_x_base_005/`
never held more than one ~434-447 MB global file at once. All 84 files are recorded in
`tables/fluxcom_x_base_005_download_manifest.csv` with `sha256_verified = TRUE` and
`extracted = TRUE`; `data/external/fluxcom_x_base_005/` is now empty of data (only the
gitignored ICOS session-cookie file remains).

One stumbling block along the way, caught before it touched any real download: the
first script draft built its `curl` calls with `system2()` without shell-quoting the
SPARQL query string, which contains characters (`<`, `>`, `"`, newlines) that broke the
shell command line. Caught on the very first (small, fast-failing) metadata query —
no partial downloads resulted — and fixed by `shQuote()`-wrapping every argument passed
to `curl` in the download script.

## 2. Extraction

0.05 degree cell containing each tower's coordinates (from the **20 September 2026
snapshot**, `data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv` — identical
to the Stage 2 BIF-based coordinates for all six sites, no discrepancy to reconcile).
Grid convention confirmed against the file's own coordinate metadata (cell-centre
convention, 0.05 degree pitch, same longitude/latitude ordering as the 0.5 degree
files). Cell centres and distances from the tower:

| Site | Cell centre (lon, lat) | Distance from tower |
|---|---|---|
| `GH-Ank` | -2.675, 5.275 | 2.24 km |
| `BJ-Nhu` | 1.625, 9.725 | 3.21 km |
| `BJ-Bfg` | 1.725, 9.775 | 1.94 km |
| `SN-Dhr` | -15.425, 15.425 | 2.61 km |
| `ML-Kem` | -1.525, 15.275 | 2.38 km |
| `SN-Nkr` | -16.475, 14.475 | 3.27 km |

**Grid-nesting check** (`tables/xbase_005_grid_check.csv`, `GH-Ank`/GPP/2001-01): the
mean of the 100 (10x10) 0.05 degree cells within the 0.5 degree cell's footprint is
6.10676 gC m-2 d-1, against the 0.5 degree file's own point value of 6.10666 gC m-2
d-1 — a difference of 0.0001 gC m-2 d-1 (floating-point rounding only). The two
resolutions are consistent.

## 3. Package

`WAFNET/fluxcom_comparison/package_ossenatou_2026-11-21/` (0.31 MB before the check
figure, 0.78 MB after; zipped to 0.54 MB) — well under the "stop and report" threshold,
so no stop was needed.

Contents and row counts:
- `xbase_005_monthly.csv` — 1,512 rows (6 sites x 12 months x 21 years, 2001-2021, no gaps).
- `tower_monthly.csv` — 516 rows, restricted to each site's actual operating period
  (`BJ-Bfg`/`BJ-Nhu` 2008-2017, `GH-Ank` 2011-2014, `ML-Kem` 2007-2009, `SN-Dhr`
  2010-2021, `SN-Nkr` 2018-2021), same VUT/CUT choice and QC rule (>= 0.50) as Stage 2.
  `GH-Ank` has no nighttime-partitioned GPP/RECO at all (100% NA, a data-availability
  gap carried from Stage 2) and is excluded from GPP/RECO comparisons only — NEE and
  LE are unaffected.
- `sites.csv` — 6 rows: coordinates, cell centre/distance, country, IGBP class,
  training-set flag (`GH-Ank`, `SN-Dhr` = TRUE), and each site's own citation/handle
  pulled from the snapshot.
- `summary.csv` — 22 rows (6 sites x 4 fluxes, minus `GH-Ank` GPP/RECO) with
  0.05 degree bias/RMSE/R2 alongside the existing Stage 2 0.5 degree numbers.
- `README.md` — plain-English documentation for Ossénatou: file-by-file columns and
  units, sign convention, RECO/H caveats, QC rule, training-site and grid-cell caveats,
  and citations (X-BASE: Nelson et al. 2024, *Biogeosciences* 21, 5079-5115,
  doi:10.18160/5NZG-JMJE, CC BY 4.0; each tower site's own citation from `sites.csv`).
- `check_figure_xbase_005_vs_tower_DRAFT.png` — the one check figure (section 4).
- `package_ossenatou_2026-11-21.zip` — the whole folder zipped, alongside it.

## 4. Check figure

One figure, three panels (NEE, GPP, LE — no RECO, as specified), sites by colour
(shared Stage 2 palette), training sites (`GH-Ank`, `SN-Dhr`) marked as triangles, 1:1
dashed line, titled "DRAFT." Opened and checked before committing:
- All three panels render with correct axis labels/units and a visible 1:1 line.
- The scatter patterns match `summary.csv`'s bias signs: NEE points sit above the 1:1
  line at every site (all six have positive NEE bias, i.e. X-BASE is less negative/more
  net-release than the tower); GPP points fall below the line at high tower values for
  the non-training sites (consistent with negative GPP bias at five of six sites); LE
  shows the mixed-sign pattern the summary table reports (`GH-Ank`/`SN-Dhr`/`SN-Nkr`
  biased high, `BJ-Bfg`/`BJ-Nhu`/`ML-Kem` biased low).
- No rendering, legend, or axis problems.

## 5. Summary statistics: 0.05 vs. 0.5 degree

Full numbers are in `package_ossenatou_2026-11-21/summary.csv` (0.05 degree columns)
alongside the existing `tables/xbase_monthly_stats.csv` (0.5 degree, Stage 2). At 0.05
degree, R2 is lower than 0.5 degree for most site x flux combinations (e.g. `GH-Ank`
NEE R2 drops from 0.05 to 0.001, `BJ-Bfg` LE from 0.30 to 0.30 — essentially unchanged
there), and biases shift modestly in magnitude (e.g. `ML-Kem` LE bias shrinks from
-43.4 to -36.2 W m-2, about 17% smaller). In
general the finer 0.05 degree grid cell is a noisier, slightly less-averaged match to
the tower footprint than the coarser 0.5 degree cell, as expected — aggregating to 0.5
degree smooths out some of the grid-cell/tower-footprint mismatch that remains visible
at 0.05 degree.

## Anything unexpected

- The `system2()`/shell-quoting bug in section 1 — caught early, no data consequence,
  noted here for completeness since it was a genuine stumbling block during development.
- No login or additional access step was needed beyond Stage 2's existing mechanism.
- No other surprises: snapshot coordinates matched Stage 2's BIF coordinates exactly,
  the grid-nesting check passed cleanly, and the package size never approached the
  "few MB" stop-and-report threshold.
