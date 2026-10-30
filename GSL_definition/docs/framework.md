# GSL Figure Rebuild: Analysis Framework (corrected)

Oct 29, 2026 · @Dave. This version supersedes the original framework and the separate corrections note of the same date; the corrections are built into the text below.

## Purpose and scope

This side project rebuilds four proposed main-text figures for the MacBean and Moore growing season length (GSL) manuscript, using exactly the 62 sites and 505 site-years in its Table S1. It runs as a side analysis in the EcosystemEcologyLab/fluxnet-annual-2026 repository, with that repository's project-standard QA/QC applied to the fixed site list.

The four figures are: (1) the mechanism at two representative sites; (2) where threshold sensitivity sits across all 62 sites; (3) consequences for GSL trends compared with sampling uncertainty; and (4) the fraction-of-annual-GPP method and its own sensitivity. A benchmark table tests which GSL definition better explains interannual variation in annual GPP.

This work reproduces and extends Tash's analysis; it does not replace her pipeline. Every number we derive that she also reports (P99, skewness, selected thresholds) must match hers before any new figure is drawn.

## Sites

Use these 62 sites and these years only, copied from Table S1 of the manuscript supplement. Groups: grassland 17, evergreen forest 20, deciduous forest 12, woody-grass mosaic 12, mixed forest 1 (CH-Lae, kept in all-site analyses but in no group). Checked against the repository on 29 Oct 2026: the table sums to 505 site-years, 21 sites have 10 or more years, all 62 sites are in fluxnet_shuttle_snapshot_20260920T102211.csv with matching IGBP classes, and every listed year lies within the site's record.

| Site | IGBP | Group | Network | Years used | n |
| --- | --- | --- | --- | --- | --- |
| AT-Neu | GRA | grassland | European Fluxes Database | 2008, 2010-2011, 2013, 2015, 2017-2019 | 8 |
| AU-ASM | SAV | woody-grass mosaic | TERN | 2013-2020, 2025 | 9 |
| AU-Cpr | WSA | woody-grass mosaic | TERN | 2011-2012, 2015-2019, 2021, 2024-2025 | 10 |
| AU-Cum | EBF | evergreen forest | TERN | 2014-2017, 2019-2020, 2022-2025 | 10 |
| AU-DaS | WSA | woody-grass mosaic | TERN | 2012-2013, 2021, 2024-2025 | 5 |
| AU-GWW | EBF | evergreen forest | TERN | 2015-2016, 2019-2020, 2024 | 5 |
| AU-How | WSA | woody-grass mosaic | TERN | 2008, 2010-2012, 2014-2017, 2019, 2023-2024 | 11 |
| BE-Dor | GRA | grassland | ICOS | 2013-2014, 2020, 2022-2024 | 6 |
| BE-Maa | CSH | woody-grass mosaic | ICOS | 2017, 2020-2025 | 7 |
| CA-Ca1 | ENF | evergreen forest | AmeriFlux | 1999-2000, 2003, 2006, 2009 | 5 |
| CA-Ca2 | ENF | evergreen forest | AmeriFlux | 2001, 2004-2008, 2010 | 7 |
| CA-TP3 | ENF | evergreen forest | AmeriFlux | 2009, 2012-2015, 2017, 2020, 2022-2023 | 9 |
| CA-TP4 | ENF | evergreen forest | AmeriFlux | 2005-2007, 2010-2012, 2014, 2016, 2020, 2022 | 10 |
| CA-TPD | DBF | deciduous forest | AmeriFlux | 2012, 2014, 2017, 2021-2022 | 5 |
| CH-Cha | GRA | grassland | European Fluxes Database | 2006, 2010-2011, 2013-2015, 2019, 2021, 2023-2024 | 10 |
| CH-Dav | ENF | evergreen forest | ICOS | 2002-2003, 2005-2009, 2013-2015, 2017, 2019-2020, 2022, 2024 | 15 |
| CH-Lae | MF | mixed forest | European Fluxes Database | 2005, 2007-2008, 2014-2015, 2023 | 6 |
| CH-Oe3 | GRA | grassland | European Fluxes Database | 2003, 2005-2008 | 5 |
| CH-Rtb | GRA | grassland | European Fluxes Database | 2010-2013, 2015-2019, 2022 | 10 |
| CN-Ash | EBF | evergreen forest | ChinaFlux | 2009-2012, 2014, 2016 | 6 |
| CN-Sdq | OSH | woody-grass mosaic | ChinaFlux | 2015, 2017, 2021-2022, 2024 | 5 |
| CZ-Lnz | DBF | deciduous forest | ICOS | 2015-2017, 2021, 2024-2025 | 6 |
| CZ-RAJ | ENF | evergreen forest | European Fluxes Database | 2012, 2017, 2019-2020, 2022 | 5 |
| DE-Gri | GRA | grassland | ICOS | 2006, 2008, 2012-2013, 2015, 2018-2019, 2021-2023 | 10 |
| DE-GsB | GRA | grassland | ICOS | 2016-2017, 2019-2021 | 5 |
| DE-Hai | DBF | deciduous forest | ICOS | 2003-2008, 2012 | 7 |
| DE-Tha | ENF | evergreen forest | ICOS | 2001, 2007, 2009-2013, 2015-2018, 2023 | 12 |
| DK-Sor | DBF | deciduous forest | ICOS | 2006-2010, 2017-2018, 2020, 2023 | 9 |
| FI-Hyy | ENF | evergreen forest | ICOS | 2003, 2007-2008, 2013-2015 | 6 |
| FI-Var | ENF | evergreen forest | ICOS | 2016-2018, 2020-2022 | 6 |
| FR-LBr | ENF | evergreen forest | European Fluxes Database | 2001-2002, 2005-2007 | 5 |
| FR-Mej | GRA | grassland | ICOS | 2020-2024 | 5 |
| FR-Pue | EBF | evergreen forest | ICOS | 2005, 2007-2008, 2014-2015, 2017-2021 | 10 |
| IL-Yat | ENF | evergreen forest | European Fluxes Database | 2020-2024 | 5 |
| IT-Lav | ENF | evergreen forest | European Fluxes Database | 2005-2009, 2016, 2020 | 7 |
| IT-MBo | GRA | grassland | ICOS | 2004-2007, 2017 | 5 |
| IT-Tor | GRA | grassland | ICOS | 2012, 2017, 2019-2023 | 7 |
| JP-Fhk | DNF | deciduous forest | JapanFlux | 2006-2007, 2009-2010, 2013-2015, 2017, 2020, 2022 | 10 |
| JP-Fjy | ENF | evergreen forest | JapanFlux | 2001-2002, 2005, 2008-2017, 2019-2020 | 15 |
| JP-NsC | GRA | grassland | JapanFlux | 2005-2009 | 5 |
| JP-NsM | GRA | grassland | JapanFlux | 2006-2008, 2011-2012, 2014 | 6 |
| JP-Spp | DBF | deciduous forest | JapanFlux | 2006, 2008-2009, 2011, 2013-2016 | 8 |
| NL-Loo | ENF | evergreen forest | ICOS | 2002-2003, 2005-2007, 2010, 2012, 2017, 2022, 2024 | 10 |
| NL-Vkp | GRA | grassland | European Fluxes Database | 2015-2018, 2020, 2023-2024 | 7 |
| US-CMW | DBF | deciduous forest | AmeriFlux | 2003, 2005-2007, 2009-2013, 2015-2017, 2019-2021 | 15 |
| US-Dk2 | DBF | deciduous forest | AmeriFlux | 2002-2005, 2008 | 5 |
| US-HB3 | ENF | evergreen forest | AmeriFlux | 2020-2024 | 5 |
| US-MMS | DBF | deciduous forest | AmeriFlux | 2004-2005, 2013, 2017, 2019-2020, 2022 | 7 |
| US-MOz | DBF | deciduous forest | AmeriFlux | 2009, 2012, 2014-2018, 2021 | 8 |
| US-Mpj | WSA | woody-grass mosaic | AmeriFlux | 2009-2010, 2013, 2017, 2019-2025 | 11 |
| US-Rws | OSH | woody-grass mosaic | AmeriFlux | 2016-2018, 2020-2022 | 6 |
| US-SRG | GRA | grassland | AmeriFlux | 2011-2012, 2014, 2018-2022 | 8 |
| US-SRM | WSA | woody-grass mosaic | AmeriFlux | 2005, 2007-2010, 2012, 2014-2018, 2021-2022, 2024-2025 | 15 |
| US-Seg | GRA | grassland | AmeriFlux | 2007-2008, 2013-2015, 2017, 2020-2025 | 12 |
| US-Ses | OSH | woody-grass mosaic | AmeriFlux | 2010-2012, 2016-2017, 2019 | 6 |
| US-Slt | DBF | deciduous forest | AmeriFlux | 2006-2009, 2012 | 5 |
| US-Ton | WSA | woody-grass mosaic | AmeriFlux | 2002, 2004, 2006, 2008-2009, 2012, 2014, 2018-2020 | 10 |
| US-UMd | DBF | deciduous forest | AmeriFlux | 2008-2012, 2016, 2018, 2020-2023 | 11 |
| US-Var | GRA | grassland | AmeriFlux | 2006-2009, 2011, 2013-2016, 2019-2024 | 15 |
| US-Vcp | ENF | evergreen forest | AmeriFlux | 2010, 2014, 2022-2024 | 5 |
| US-Wjs | SAV | woody-grass mosaic | AmeriFlux | 2014, 2018-2022 | 6 |
| US-Wkg | GRA | grassland | AmeriFlux | 2005-2008, 2010-2025 | 20 |

Total: 505 site-years. Excluded by Tash and staying excluded: US-Aud (17.3% of daily GPP below -1 gC m-2 d-1). Site coordinates and data citations are in Table S1.

Site handling rules:

- Trend analyses (Figure 3) use only sites with 10 or more years in this table, following Bowling et al. (2024). That gives 21 sites; most have gaps between years, so trends are fitted on the years present. Under project-standard screening some of these sites may fall below 10 years; Figure 3 and the methods memo must report every site that drops out.
- Figure 1 uses the site nearest the median skewness of the low-skew third and of the high-skew third. AU-ASM is never a Figure 1 example.
- Tash's two screening sensitivity sets (30 sites and 188 sites) are not listed in the supplement. Request both site-year lists from her before running any robustness check.

## Data required

Use the daily (DD) FLUXNET Shuttle data already in the fluxnet-annual-2026 pipeline: the pre-QC `daily` table in `data/duckdb/fluxnet.duckdb`, built by `scripts/03b_create_database.R`. Open it read-only and never write to it. All 62 sites are in `data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv`, all processed with ONEFlux v1.3, and DD carbon is already in gC m-2 d-1 so no unit conversion is needed.

The `daily` table holds two rows per day for each site, `dataset = 'FLUXMET'` and `dataset = 'ERA5'`. ERA5 rows carry meteorology only and no NEE quality flag. Filter every query to `dataset = 'FLUXMET'`; otherwise P99, skewness, GSL and every count are computed on the wrong rows.

Tash downloaded on 17 August 2026; the repository's nearest snapshots are 1 and 20 September 2026. Compare each site's `product_id` in our snapshot against hers, list every site whose product changed, flag those sites, and continue. Flagged sites are dropped from the match checks only, not from the analysis. Product IDs already differ between our 1 and 20 September snapshots at FR-Mej, IT-Tor and NL-Vkp, so some differences from Tash's snapshot are expected.

| Variable | Resolution | Use | Required |
| --- | --- | --- | --- |
| TIMESTAMP | DD | dates, years | yes |
| GPP\_NT\_VUT\_REF | DD | main GPP series, as in the manuscript | yes |
| NEE\_VUT\_REF | DD | negative-GPP check, annual NEE | yes |
| NEE\_VUT\_REF\_QC | DD | day screening (project standard and Tash's rule) | yes |
| NEE\_CUT\_REF\_QC | DD | project-standard fallback for CUT-only sites | yes |
| GPP\_DT\_VUT\_REF | DD | partitioning sensitivity check | yes |
| TA\_F, P\_F, SWC\_F\_MDS\_1 | DD | interpreting low-GPP periods | optional |
| NEE\_VUT\_REF, PPFD\_IN or SW\_IN\_F | HH/HR | light-response check after Bowling et al. (2024); needs its own HH extract scoped to this side project, as WAFNET did | optional |

Items to get from Tash. Request them now; they are needed for the match checks (run-order step 5), not for steps 1 to 3:

- Her `fluxnet_shuttle_snapshot*.csv` from 17 August 2026, for the product comparison above.
- Her daily GPP table after screening, or her code, so our P99, skewness and selected thresholds can be checked against hers.
- Whether negative daily GPP values were set to zero before skewness and thresholds were calculated. The manuscript notes suggest yes; the Methods do not say. Every result at thresholds below about 1% of P99 depends on this.
- Whether GSL counts days with GPP > T or >= T.
- The site-year lists for her two sensitivity sets (30 and 188 sites).

PhenoCam transition dates are optional, for the benchmark test only, at sites with a camera in the tower footprint (US-SRM and US-Wkg are used in Yan et al., 2019). Under the repository's Hard Rule 1, any non-Shuttle data must be commented in code and labelled as comparison data in every output.

## QA/QC and data screening

Use the project-standard QC from `CLAUDE.md` and `R/pipeline_config.R`, applied to Tash's 62 sites and 505 site-years. The project standard is stricter at the daily level than Tash's rule, so some of her days will fail it. How those days are handled is the open decision below; stage 1 reports the counts under each option so it can be decided from numbers.

| Rule | Project standard | Tash's manuscript screening |
| --- | --- | --- |
| Daily QC variable | NEE\_VUT\_REF\_QC; NEE\_CUT\_REF\_QC for CUT-only sites (per site, never mixed within a site) | NEE\_VUT\_REF\_QC |
| Daily threshold | QC\_THRESHOLD\_DD = 0.50 (keep QC >= threshold) | 0.25 |
| Year completeness | not set by the project; see open decision | every day in the year must pass |
| Minimum record | not set by the project | 5 complete years |
| Site exclusions | none by default | US-Aud, for persistent negative GPP |
| Secondary QC (GPP flags) | retained, does not drive exclusion | not used |

Required project practice, whatever the decision:

- Read the thresholds from `R/pipeline_config.R`; never hard-code 0.50.
- Apply the daily rule with `qc_passes()` from `R/qc.R` (keep a day when QC >= threshold); do not reimplement it.
- Days with an NA QC flag count as not passing under every rule, ours and Tash's. Record this in the methods memo.
- Log every excluded day with `log_exclusion()` and every unassessable record with `log_unknown()` from `R/utils.R`, passing `log_path = "GSL_definition/logs/exclusion_log.csv"` and `"GSL_definition/logs/unknown_log.csv"`. Never write to the repo-root `outputs/` logs, which `scripts/04_qc.R` clears on every run.
- Call `check_pipeline_config()` at the start of every script and leave its warnings visible.
- Write a `.meta.json` companion with `write_output_metadata()` for every table and figure.

Run Tash's screening as well, once, as a reproduction check. Apply it to every site in the 20 September snapshot that has daily data, not only the 62: NEE\_VUT\_REF\_QC >= 0.25 on every day of a year, at least 5 complete years, US-Aud excluded. It should return exactly the 62 sites and 505 site-years in the Sites table. Report every site or year added or lost, with the reason; do not tune the rule to force a match.

## Derived quantities

Compute everything per site from screened daily GPP\_NT\_VUT\_REF over that site's years in the Sites table. Quantities marked "match" must reproduce Tash's values before any figure is drawn; differences larger than rounding get explained in the methods memo, not tuned away.

| Quantity | Definition | Match |
| --- | --- | --- |
| Negative GPP handling | Run twice: as delivered, and with negative daily GPP set to 0; use whichever Tash used as the main run | match |
| P99 | 99th percentile of all retained daily GPP for the site, whole year, all years pooled | match |
| Skewness | Adjusted Fisher-Pearson G1 (scipy.stats.skew, bias=False); implement the formula directly in R rather than adding a package | match |
| L-skewness, Kelly's skewness, % days below 5, 10, 20% of P99 | As in the manuscript Methods; supplementary only | match |
| Skewness thirds | Low 0.19-0.63 (21 sites), middle 0.63-0.88 (20), high 0.88-3.88 (21) | match |
| Annual GSL(y, T) | Days in year y with GPP above threshold T; > or >= as Tash confirms | match |
| Mean GSL, GSL s.d., GSL trend | Across retained years; trend = OLS slope in days per year on the years present | match |
| Threshold grids | Tash's 22-value grid (0.1%, 0.5%, 1-20% of P99) for checks; fine grid 0.01-50% of P99 in 0.01% steps for everything new | - |
| Local sensitivity | Change in mean GSL per 1% of P99, centered difference over +/-0.05% of P99 (Tash's window), on the fine grid | - |
| Included fraction F(y, T) | Sum of GPP on days above T divided by annual GPP sum, using the same negative-GPP handling as everything else | - |
| Selected threshold T95 | Tash's rule: step up the fine grid until the multi-year mean of F falls to 0.95 or below (rounded to 6 decimals) | match |
| Fraction sweep | GSL for target fractions 0.80 to 0.99 in 0.01 steps | - |
| Z17 implied fraction | Mean F at T = 10% of P99 | - |
| Common-unit sensitivity | Change in mean GSL per 1% change in included fraction, at Z17's T and at T95 | - |
| Trend uncertainty | 95% confidence interval of the OLS GSL trend at Z17's T; sites with 10 or more years only | - |

The match checks run on the reproduction run with Tash's screening, not the project-standard run, and exclude sites flagged for product changes. They are: P99 and skewness for all matched sites, the tercile boundaries, the three Figure 1 correlations (r = 0.63, 0.49, 0.34), and T95 against Figure 4 (r = -0.68 with skewness).

## Figure specifications

All figures use the project-standard run unless noted. Express thresholds as % of P99 on every axis, use one skewness color scale across all figures, and label any axis that counts days as "days above threshold".

### Figure 1. Mechanism at two representative sites

Takeaway: GSL changes fastest where days pile up near the threshold, and at dryland sites they pile up where conventional thresholds sit.

- Sites: the site nearest the median skewness of the low third and of the high third (never AU-ASM). Record both names and skewness values in the figure's metadata notes.
- Panel a, one row per site: daily GPP for the median-GSL year as a dark line, other years faint grey. Horizontal lines at 5%, 10% (Z17) and 20% of P99 and at the site's T95. Shade days between 0 and 10% of P99.
- Panel b, shared: histograms of daily GPP (x = % of P99) for both sites as overlaid outlines, the same thresholds as vertical lines, and mean GSL against threshold on a right-hand axis.
- Annotate each site with the change in mean GSL between 5% and 10% of P99, in days.

### Figure 2. Where sensitivity sits across all 62 sites

Takeaway: sensitivity concentrates at the low end of the distribution and rises with skewness.

- Main panel: heat map, rows = 62 sites sorted by skewness, columns = fine-grid thresholds 0.1-35% of P99 on a log axis, cell color = local sensitivity (days per 1% of P99), single-hue scale. Left strip: skewness value and functional group color.
- Overlays: a vertical line at 10% (Z17) and a black dot on each row at that site's T95.
- Right panel, row-aligned: sensitivity at Z17's T and at T95, joined by a segment.
- Bottom panel: median sensitivity per column for each skewness third, as three lines.

### Figure 3. Consequences for GSL trends

Takeaway: whether threshold choice changes trend conclusions more than ordinary sampling uncertainty does, and where.

- Sites: those with 10 or more years after screening (21 before screening). Thresholds: 5-20% of P99 only, unless the methods memo justifies a different range from published studies.
- Panel a, one row per site sorted by skewness: a pale grey bar for the 95% CI of the trend at Z17's T, a colored bar for the range of trend estimates across 5-20% of P99, a dot for Z17, a diamond for T95, and a zero line.
- Panel b: ratio of threshold range to CI width against skewness, colored by functional group. Annotate the count of sites where threshold choice changes the sign of the trend or whether it is significant.

### Figure 4. The fraction-of-annual-GPP method and its own sensitivity

Takeaway: the method fixes the share of annual GPP inside GSL across sites; panel c shows whether that also reduces sensitivity.

- Panel a: for the two Figure 1 sites, included fraction (y) against days above threshold (x), each year faint and the multi-year mean bold. A band from 0.90 to 0.99, a line at 0.95, and a marker where Z17 falls on each curve.
- Panel b: all 62 sites as lines of GSL against target fraction (0.80-0.99), colored by skewness, with a marker on each line at the site's Z17 implied fraction.
- Panel c: common-unit sensitivity at Z17 and at 0.95 against skewness, two point series.

### Benchmark table

For each site with 10 or more years, the correlation between annual GSL and annual GPP across years, for Z17 and for the new method, plus the median across sites by functional group. Both definitions count days above a fixed threshold, so neither is favored by construction, but say so in the table note. PhenoCam comparisons, if added, go in a separate labelled table.

### Supplementary outputs

- The current manuscript Figures 1-4, rebuilt from our run with Figure 2 on the % of P99 axis, as a direct comparison.
- Figures 2 and 3 repeated with Tash's screening and with GPP\_DT\_VUT\_REF.

## Layout, run order and open decisions

Build this as a standalone side analysis in `GSL_definition/` at the repository root, following the WAFNET pattern: it reads the root DuckDB store and snapshots but never writes to root `R/`, `scripts/`, `outputs/`, `review/` or `docs/`. The one exception, already made (commit 3c38ec7), is the optional `log_path` argument added to `log_exclusion()` and `log_unknown()` in `R/utils.R`. Claude Code does the work; follow the repository `CLAUDE.md` for session logging, commits and long-running jobs. Daily data for 62 sites runs in minutes; no background job is needed.

```
GSL_definition/
  README.md          not part of the Annual Paper; sites, data, run order
  code/              R scripts, numbered in run order
  data/              extracted daily data (gitignored)
  tables/            CSV outputs + .meta.json (git-tracked)
  figures/           PNG and PDF + .meta.json (git-tracked)
  docs/framework.md      this document
  docs/methods_memo.md   every screening choice, deviation and match result
  logs/              exclusion_log.csv, unknown_log.csv, nohup logs
```

Run order:

1. `00_config.R`: site list, groups and years from the Sites table; thresholds from `R/pipeline_config.R`; log paths. Sourced by every script. Confirms 505 site-years, groups 17/20/12/12/1 and 21 sites with 10 or more years.
2. `01_extract_sites.R`: pull the 62 sites from the pre-QC `daily` table (FLUXMET rows only, read-only connection) into `data/`. Compare `product_id` across our 1 and 20 September snapshots, and against Tash's snapshot when it is available at `GSL_definition/data/tash_snapshot.csv`; write `tables/site_products.csv` with changed sites flagged. Report any CUT-only sites.
3. `02_screen.R`: one row per site-year with days present, days with NA QC, share of days passing 0.25 and 0.50, whether every day passes at each level, and counts of daily GPP below 0 and below -1. Log every exclusion. Report sites, site-years, sites with 10 or more years (and which trend sites drop out) and sites with 5 or more years under each year-screening option in the open decision. Run Tash's screening across the whole network as the reproduction check described above.
4. `03_derive.R`: all quantities in Derived quantities, for both screenings and both negative-GPP treatments.
5. `04_reproduce.R`: the match checks; write `docs/reproduction_report.md`. Stop here for PI sign-off before any new figure.
6. `05_fig1.R` to `08_fig4.R`, `09_benchmark.R`, `10_supplementary.R`.

Stages:

- Stage 1: run-order steps 1 to 3. No derived quantities, no figures.
- Stage 2, after Tash's files and answers arrive and the year-screening decision is made: steps 4 and 5.
- Stage 3, after PI sign-off on the reproduction report: step 6.

Open decisions (resolved 2026-10-29, after Stage 1 at commit `a1633a8`):

Dave made the following calls without input from Tash, explicitly accepting the risk of a rerun
if an assumption later proves wrong. Each is implemented as a named switch in
`GSL_definition/code/00_config.R` (not hard-coded inline at each point of use) — see that file
and `docs/methods_memo.md` for the reasoning.

- [x] **Year screening: option (d) at 80%** (`GSL_YEAR_SCREEN = "d80"`). GSL is counted on the
  full gap-filled daily series; a year is kept when ≥80% of its days pass `QC_THRESHOLD_DD`
  (`qc_passes()`). Stage 1 showed this keeps all 62 sites and 505 site-years — the
  project-standard run and Tash's own selection cover the same site-years — so only this one run
  is carried through Stage 2, not two. Option (b) (every day passes 0.50) is reported once in
  `docs/methods_memo.md` as the strict alternative (55 sites, 276 site-years, 3 trend sites),
  not carried through the derived-quantity outputs.
- [x] **Negative daily GPP set to 0 for the main run** (`GSL_NEG_GPP_TO_ZERO_MAIN = TRUE`). The
  as-delivered version is computed alongside, for every quantity, not dropped.
- [x] **GSL counts days with `GPP > T`** (strict; `GSL_STRICT_GT = TRUE`), not `GPP >= T`.
- [x] **Site and year list: the fixed Sites table above**, unchanged. IT-Tor 2012 is kept even
  though it failed Stage 1's Tash-rule "complete year" reproduction check. The 5 extra
  site-years Stage 1 found passing Tash's rule at already-included sites (CA-Ca1 2004/2010,
  CH-Dav 2011, DE-Tha 2025, NL-Vkp 2025) are not added.
- [x] **GPP variable: `GPP_NT_VUT_REF` for the main run, `GPP_DT_VUT_REF` for the supplementary
  version** (`GSL_GPP_VARIANTS` in `00_config.R`).
- [ ] Threshold range for Figure 3 (default 5-20% of P99) — not yet needed; Stage 2 stops before
  any figure.
- [x] The `04_qc.R` threshold direction was fixed in the main pipeline on 29 October 2026
  (`qc_passes()`, merge `09e621c`); no action needed here.
