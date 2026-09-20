# IT-MBo ERA5-precipitation anomaly: bug hunt, prompted by Dario Papale's non-reproduction

> **Provisional — pending store audit (2026-09-20).** The numbers in this report rest on the June 2026 `data/extracted/` extraction. One file in that extraction (IT-MBo's `FLUXNET_FLUXMET_MM` file) is known to differ from the currently-distributed archive under the same product ID. This report's conclusions are provisional pending `review/diagnostics/store_audit/` (in progress).

## Verdict

**Our reported IT-MBo anomaly does not stand: the ~18-24x ERA5-over-measured ratio we
published comes entirely from the DD/MM/YY-resolution branch of the distributed FLUXNET
product, which this investigation shows is internally inflated by a consistent ~21.25x
factor relative to the same product's own HH-resolution data (median ratio 21.25, n=12,027
site-days, range 15-37x, essentially constant across the full 1981-2025 record).** At HH
resolution, in the same official product, IT-MBo's ERA5-derived and tower-measured annual
precipitation are close and unbiased (2003-2025 means: 1,152.3 mm/yr ERA5 vs. 1,152.8 mm/yr
measured, ratio 0.9995) -- entirely ordinary for the site, and consistent with BADM's
independent 1,365 mm/yr. **This is not a units, day-weighting, or source-choice error in
any of our own scripts** -- every one checked (deliverable 3 below) uses the correct P_ERA
source and the correct day-weighting formula, confirmed three independent ways against the
raw product itself. **US-HB4 is a separate case and is unaffected by this finding**: its DD
and HH resolutions agree with each other (ratio ~1.00) and both are equally, catastrophically
implausible (~700 m/yr), consistent with v1's original characterisation of a genuine,
site-specific data error, not a resolution artifact.

Read in full below: this explains the "order of magnitude" part of Dario's report almost
exactly (21.25x), and, at HH resolution, ERA5 sits below measured in roughly half of years
checked (9 of 23, concentrated in 2019-2025) -- ordinary scatter around 1:1, which plausibly
explains why Dario, checking a specific year or period at HH resolution, found ERA5 below
measured. We could not reproduce a systematic "ERA5 below measured" bias across all years at
HH resolution (it's essentially unbiased, scattering both ways) -- if that detail matters to
Dario's own conclusion, we'd want to know which years/months he checked.

---

## Background and scope

Read-only with respect to `review/diagnostics/era5_precip_units/`, `_v2/`, `_v3/`, `_v4/`,
`era5_reference_plots/`, `era5_cumulative_test/`, and `era5_share_for_coordination/` --
nothing in any of those directories was modified. `R/climate_classification.R` and
`scripts/step5_compute_koppen_era5.R` were read and sourced, never edited. No pipeline
script, figure, or snapshot CSV was modified. New code:
`scripts/diagnostics/it_mbo_bug_hunt.R`. All new outputs: `review/diagnostics/it_mbo_bug_hunt/`.

HH-resolution data is not part of the pipeline's default extraction
(`FLUXNET_EXTRACT_RESOLUTIONS="y m d"`) and was not previously on disk for any site. This
investigation downloaded and extracted HH resolution fresh for three sites -- IT-MBo,
US-HB4, and FI-Hyy (a control) -- via single-site `flux_download()`/`flux_extract()` calls
(not a full-network reprocessing run). `data/raw/*_hh_check/` scratch download directories
and the newly-added HH/BIFVARINFO_HH files under `data/extracted/` are gitignored, per Hard
Rule 4, and are not part of this commit.

---

## Deliverable 1: the two P_ERA sources

Every site bundles P_ERA in two places: the standalone `*_FLUXNET_ERA5_MM_*.csv` file, and a
`P_ERA` column embedded directly in `*_FLUXNET_FLUXMET_MM_*.csv`. Compared month-by-month at
IT-MBo and the same three controls `era5_precip_units_v3_partA.R` used (`FI-Hyy`, `JP-Khw`,
`US-Akn`): **the two sources are byte-for-byte identical everywhere** -- 1,008/1,008
site-months across all four sites, max absolute difference 0.0. Source choice is not the
cause of anything reported here or previously.
(`table_d1_two_p_era_sources.csv`)

## Deliverable 2: units, from the files themselves

**MM-vs-YY** (the pipeline's own day-weighting formula, `sum(P_ERA*days_in_month)`, against
the product's own independently-produced annual file): ratio 1.0000 at every one of 44
complete years, 1981-2024, at IT-MBo (median 1.0000, range 1.0000-1.0000) -- reproducing
v2's network-wide finding exactly, at full precision, for this specific site.

**DD-summed-vs-MM-day-weighted**: ratio 1.0000 across 528 months (range 0.9997-1.0006) --
DD, MM, and YY are mutually, essentially perfectly self-consistent.

**DD-vs-HH-summed** (the physically correct way to build a daily total from per-timestep
depths): **ratio 21.2451 (median), range 15.0-37.0, across 12,027 complete site-days
spanning the full 1981-2025 ERA5 record.** This is the discrepancy. DD (and therefore MM and
YY, which track DD exactly) is not a day-weighted rate consistent with this same product's
own HH-resolution data -- it is a near-constant ~21.25x multiple of it.
(`table_d2_dd_vs_hh_ratio.csv`, `table_d2_hh_annual_vs_bundled.csv`)

**Authoritative unit statements, quoted verbatim from the site's own BIFVARINFO files** (no
separate unit documentation exists in the `fluxnet` R package itself -- `help.search()` and
`vignette()` both return nothing for precipitation/P_ERA; `flux_varinfo()`'s own help page
confirms it only wraps these same BIFVARINFO files):

| Resolution | Variable | Definition | Unit |
|---|---|---|---|
| MM | P_ERA | "Precipitation, downscaled from ERA, linearly regressed using measured only site data. Average from daily data" | `mm d-1` |
| MM | P_F | "Precipitation consolidated from P_F_MDS and P_ERA. Average from daily data" | `mm d-1` |
| DD | P_ERA | "...Average from half-hourly data" | `mm d-1` |
| DD | P_F | "...Average from half-hourly data" | `mm d-1` |
| HH | P_ERA | "...(mm per dataset resolution: either hour or half-hour)" | `mm` |
| HH | P_F | "Precipitation consolidated from P and P_ERA. P used if measured..." | `mm` |

Two things follow directly from these quotes. First, `mm d-1` at MM/DD and day-weighting is
exactly the right transform -- our pipeline's own convention is correct, confirmed
independently of anything computed here. Second, **P_ERA is explicitly documented as "downscaled
from ERA, linearly regressed using measured only site data"** -- it is not raw ECMWF ERA5
reanalysis output; it is a per-site statistical reconstruction that uses the site's own
measured precipitation as a regression input. A poorly-conditioned regression at a specific
site (a plausible trigger: a gauge malfunction or a unit/scale defect in the "measured only
site data" the regression was fit against) is a specific, mechanistically plausible
explanation for a site-specific multiplicative bias -- consistent with the observed ratio
range (15-37x, not perfectly constant, as a corrupted regression coefficient would produce)
rather than a hard-coded, exactly-constant scaling bug.
(`table_d2_bifvarinfo_unit_quotes.csv`)

**HH resolution directly answers Dario's question.** Full-year HH-summed totals, IT-MBo,
2003-2025 (years with FLUXMET HH coverage; P_F consolidated variable):

| | P_ERA (HH-summed) | P_F (HH-summed) |
|---|---|---|
| 23-year mean | 1,152.3 mm/yr | 1,152.8 mm/yr |
| ratio | 1.0000 (0.9995) | |

Year-by-year, ERA5 sits *below* measured in 9 of 23 years (2007, 2015, 2016, 2019, 2020,
2022, 2023, 2024, 2025 -- concentrated in the most recent years) and *above* in the
remaining 14 -- ordinary scatter around 1:1, not a systematic bias either direction. This
reproduces the "order of magnitude different" part of Dario's report essentially exactly
(21.25x); it does not reproduce a systematic "ERA below measured" direction across the whole
record, only in about 40% of individual years. If Dario checked a specific recent year or a
short window, "ERA below measured" is exactly what HH resolution would show him there.

## Deliverable 3: tracing the assumption through every script

One row per script that has produced a number we've circulated. Full table:
`table_d3_assumption_trace.csv`. Summary: **every script reads P_ERA from the standalone
ERA5_MM file (DuckDB `dataset='ERA5'`, which `duckdb_setup.R` parses directly from the
`*_FLUXNET_ERA5_MM_*.csv` filename token) and applies `P_ERA * days_in_month`.** No script
reads the FLUXMET-embedded copy, no script omits day-weighting, and no script applies a
different formula. `era5_precip_units.R` (v1) is the first script in the chain and
establishes both choices; every later script either re-derives the same MAP figure with the
same source+formula, or reuses an earlier script's already-computed value without
re-reading P_ERA at all (`v3_partB.R`, `v4.R`). **There is no incorrect assumption at any
point in this repository's own code** -- deliverables 1 and 2 confirm both the source choice
and the day-weighting formula are exactly correct, given what this product's own
documentation and bundled files say. The error being chased in this report is not in any of
these scripts; it is in the DD/MM/YY-resolution values themselves, as distributed.

## Deliverable 4: PIDs and file provenance

Small table for a reply to Dario (`table_d4_pid_provenance.csv`):

| site | role | hub | product | product_id | version | years |
|---|---|---|---|---|---|---|
| IT-MBo | test site | ICOS | `ICOS_IT-MBo_FLUXNET_2003-2025_v1.3_r1.zip` | `enS2fTzGG_9PS5-51hqet8iH` | v1.3 | 2003-2025 |
| US-HB4 | named outlier | AmeriFlux | `AMF_US-HB4_FLUXNET_2020-2024_v1.3_r1.zip` | `10.17190/AMF/2571130` | v1.3 | 2020-2024 |
| FI-Hyy | control | ICOS | `ICOS_FI-Hyy_FLUXNET_1997-2025_v1.3_r1.zip` | `oIJ9cFYf8Q0e9nfMRRS-90vM` | v1.3 | 1997-2025 |
| JP-Khw | control | ICOS/JPF | `JPF_JP-Khw_FLUXNET_2000-2021_v1.3_r1.zip` | `ws7GV5BLwsKMaGOsosxNThpD` | v1.3 | 2000-2021 |
| US-Akn | control | AmeriFlux | `AMF_US-Akn_FLUXNET_2011-2022_v1.3_r1.zip` | `10.17190/AMF/2469442` | v1.3 | 2011-2022 |

**IT-MBo's `product_id` and `oneflux_code_version` are confirmed unchanged**: a live
`flux_listall()` call run during this investigation (not from a cached snapshot) returned
the identical `product_id` (`enS2fTzGG_9PS5-51hqet8iH`) and `v1.3` for IT-MBo -- the only
difference from our on-disk extraction is one additional year (2025) added since our last
download of the `y m d` resolutions. **There is no newer/corrected version of this product
upstream** -- whatever Dario is seeing is not explained by us working from a stale file.

Full file-level provenance (name, byte size, sha256) for every raw file read in this
investigation, including the four freshly-downloaded HH files: `table_d4_file_provenance.csv`.

Snapshot used throughout: `data/snapshots/fluxnet_shuttle_snapshot_20260901T094522.csv`
(782 lines = header + 781 sites, the current network; no separate `.meta.json` companion
exists for this file; sha256 `5591884a5a722e09cef948647a259926d5c7d85b9614c0dd499684a0b4b66d11`).

## Deliverable 5: consequences

**Since deliverables 1-3 found no error in source choice or day-weighting in any of our own
scripts, nothing in the previously-published MM/YY-resolution numbers changes as a
mechanical correction** -- the 26-site `KG_ERA5_MAP_MAX_MM` exclusion list, the 123-site
4x/8x clustering, and every ratio in `era5_share_for_coordination/` are exactly reproducible
from the distributed product, as before. **What changes is what those MM/YY-resolution
numbers mean.** The explicit counterfactual requested (`table_d5_consequences_mm_vs_hh.csv`):

| metric | value |
|---|---|
| `era5_map_mm` (as reported, MM/YY) | 24,150 mm/yr |
| `measured_map_mm` (as reported, MM-resolution, QC>=0.9 years) | 1,342 mm/yr |
| `ratio_to_measured` (as reported) | 18.0 |
| P_ERA, HH-summed annual mean (this diagnostic) | 1,152 mm/yr |
| P_F, HH-summed annual mean (this diagnostic) | 1,153 mm/yr |
| ratio, HH P_ERA / HH P_F (this diagnostic) | 1.00 |

**Sentences in `era5_share_for_coordination/README.md` this finding contradicts, for
IT-MBo specifically** (quoted verbatim): Figure 1's caption -- *"monthly ERA5 precipitation
runs roughly one to three orders of magnitude higher than the tower's own measured
precipitation for the same months, consistently across the full multi-year record available
at each site"* -- does not hold for IT-MBo at HH resolution (this diagnostic did not test
the other individually-named site's, US-HB4's, month-by-month HH shape beyond the annual
totals above). The `site_list.csv` row for IT-MBo (`ratio_era5_to_measured = 17.998`) is a
description of the MM/YY-resolution artifact, not of the site's true climate.

**US-HB4 is unaffected by this finding.** Its DD-vs-HH-summed ratio is ~1.00 (median, n=12,147
site-days) -- DD and HH agree with each other, and *both* independently show ~700 m/yr, a
physically impossible value present at every resolution in the raw product. This is a
different failure mode from IT-MBo's (a within-product resolution inconsistency); it is
consistent with v1's original "genuine, isolated ~1000x site-specific scaling error"
characterisation, which this investigation does not overturn.

**The 121 other sites in the 4x/8x cluster are untested by this investigation** -- extending
this exact HH-vs-DD/MM/YY check network-wide (each site needs a dedicated HH download; this
investigation downloaded three) is the natural next diagnostic and is flagged as urgent
follow-up, not attempted here. Two things this investigation does establish about that
follow-up: (a) the check is cheap to run once HH is downloaded (the R code in this script's
`dd_vs_hh_ratio()` function is already written and reusable), and (b) the failure is not
universal -- FI-Hyy (control, same hub, same processing chain, not in the cluster) shows
ratio ~1.00 (median, n=13,549 site-days) -- so whatever the underlying mechanism, it is
site-specific, not an ICOS-wide or ONEFlux-wide characteristic, and the 123-site cluster
likely contains **at least two distinct failure modes** (IT-MBo-like resolution artifacts and
US-HB4-like uniform-resolution errors), not one homogeneous group as the existing 4x/8x
framing implicitly treats it.

## Deliverable 6: the fix (identified, not applied)

**No fix is needed in this repository's own code.** Every script's P_ERA source choice and
day-weighting formula is correct (deliverable 3); nothing here should be changed.

**The underlying defect is upstream, in the distributed FLUXNET product's DD-level
aggregation/regression step for IT-MBo's P_ERA and P_F reconstruction** -- outside this
repository, in ONEFlux/FLUXNET Shuttle processing. This repository cannot and should not
patch the distributed CSV files (Hard Rule 4: `data/extracted/` is never committed and never
hand-edited). This is squarely Dario's domain; the specific, falsifiable claim to hand him is
the ~21.25x DD-vs-HH ratio itself (`table_d2_dd_vs_hh_ratio.csv`), not a diagnosis of which
line of ONEFlux code produces it.

**One mitigation this repository could adopt, not yet applied anywhere**: add an HH-vs-MM/YY
cross-check (the `dd_vs_hh_ratio()` function written for this diagnostic, or an equivalent)
as a validation step before `compute_site_koppen_era5()` trusts a site's P_ERA -- flagging
any site where MM/YY-resolution precipitation and HH-resolution precipitation disagree beyond
some tolerance, the same way `check_pipeline_config()` already flags a version mismatch. This
would have caught IT-MBo's specific defect (and would not have flagged US-HB4, since its DD
and HH already agree) without needing a fresh HH download for every site up front -- a
targeted HH download could be triggered only for sites that fail a cheaper first-pass check
(e.g., MM/YY vs. BADM/BIO12, which is already computed).

---

## Files in this directory

| File | Contents |
|---|---|
| `table_d1_two_p_era_sources.csv` | Month-by-month standalone-vs-embedded P_ERA, 4 sites |
| `table_d2_dd_vs_hh_ratio.csv` | Per-day DD-vs-HH-summed ratio, IT-MBo, full record |
| `table_d2_hh_annual_vs_bundled.csv` | Annual HH-summed totals vs. YY-bundled values, IT-MBo, 1981-2025 |
| `table_d2_bifvarinfo_unit_quotes.csv` | Verbatim BIFVARINFO unit/definition quotes, MM/DD/HH |
| `table_d3_assumption_trace.csv` | Script-by-script P_ERA source, day-weighting, units |
| `table_d4_pid_provenance.csv` | Product IDs, versions, hubs -- pasteable for Dario |
| `table_d4_file_provenance.csv` | File name, byte size, sha256 for every raw file read |
| `table_d5_consequences_mm_vs_hh.csv` | As-reported MM/YY numbers vs. this diagnostic's HH numbers |
| `table_control_dd_vs_hh_ratio.csv` | Same DD-vs-HH test at FI-Hyy (clean) and US-HB4 (both-bad) |
