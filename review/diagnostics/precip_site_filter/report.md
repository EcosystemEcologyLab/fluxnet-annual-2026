# Precipitation site-inclusion filter: evidence review

Evidence-building for a defensible site-inclusion rule for the precipitation input to the
site-side Koppen classification, to replace the flat `KG_ERA5_MAP_MAX_MM = 5000` cutoff in
`R/pipeline_config.R`. Read-only: `R/pipeline_config.R`, every pipeline script (01-07), and
every already-committed figure are untouched (verified: `git status` before and after this
script's run differs only by new, previously-untracked files). New code:
`scripts/diagnostics/precip_site_filter.R`. Network-wide, all 781 current sites.

**This analysis does not decide inclusion or exclusion, and does not extend to any axis other
than precipitation.**

## What this analysis cannot decide (stated up front)

1. Whether a site **should** be excluded -- only how many sites/site-years each candidate rule
   affects and how classification would shift, not whether that trade-off is worth accepting.
2. Whether "measured" `P_F` (years with `P_F_QC` above `QC_THRESHOLD_YY`) is itself unbiased --
   the same small-contaminated-fraction mechanism found at HH resolution for US-HB4
   (`review/diagnostics/it_mbo_parsimony/report.md`) can still operate inside a nominally
   "measured" YY value if the annual `P_F_QC` fraction is high but not exactly 1.0.
3. Anything about the ~123-site 4x/8x cluster/factor question
   (`review/diagnostics/store_refresh_20260920/report.md`, Stage 6) -- explicitly out of scope.
4. Whether WorldClim BIO12 or BADM MAP are themselves ground truth -- section 2 below shows they
   disagree with each other too; that disagreement is the reference for what "normal" looks like.

## Resolution scope

The core site-level precipitation table and plots 1-6 are built from YY-resolution DuckDB
tables only (`annual_converted`) -- DD, MM and HH FLUXMET/ERA5 files are **not** read for that
purpose. The one deliberate exception, flagged rather than silent: testing a candidate rule's
downstream effect on Koppen classification necessarily reuses `R/climate_classification.R`'s
own climate-normal machinery (`compute_era5_monthly_climatology()`), which operates on
MM-resolution ERA5 data by construction -- Koppen classification requires a 12-month
climatology and was not reimplemented at YY resolution here, since that would fork the
classification method itself, not just this diagnostic's precipitation input screen.

## 1. Data sources and vintage

- **P_ERA / P_F (YY)**: `data/duckdb/fluxnet.duckdb`, `annual_converted` table, dated
  2026-09-20 16:01 -- i.e. the current, post-store-refresh database
  (`review/diagnostics/store_refresh_20260920/report.md`), not a pre-refresh snapshot.
- **Site set / data_hub / network / oneflux_code_version (manifest) / product_id**: from
  `fluxnet::flux_discover_files()` run live against `data/extracted/` at the start of this
  script -- 781 canonical sites, after excluding 560 duplicate/stale files the function itself
  identifies and drops (it keeps the most recent release per site). This is the same
  deduplication logic `scripts/duckdb_setup.R`/`duckdb_update.R` already rely on to build the
  DuckDB tables, so the precipitation series and the site metadata come from the same,
  consistent, current file set.
- **BADM MAP / elevation / real ONEFlux processing version**: freshly extracted from each
  site's currently-canonical BIF file (same 781-file deduplicated list above) via a targeted
  grep for the `MAP`, `LOCATION_ELEV`, and `PRODUCT_ONEFLUX_VERSION` BADM variables --
  deliberately **not** `data/processed/badm.rds`, which was last built 2026-06-02, before the
  2026-09-20 refresh re-extracted several sites (including IT-MBo and FI-Hyy). Spot-checked
  against the three previously-studied sites: IT-MBo 1365, US-HB4 1429, FI-Hyy 711 mm/yr -- all
  match the values already reported in `it_mbo_parsimony/report.md`.
- **WorldClim BIO12**: reused from `data/snapshots/site_worldclim.csv` (2026-09-01) without
  re-extraction -- a static global raster keyed on site coordinates, not sensitive to the
  extraction-directory refresh.
- **Current Koppen classification** (for the "how many sites change class" comparison):
  `data/snapshots/site_koppen_era5.csv`, dated 2026-09-20 16:02 -- also current/post-refresh.

## 2. Site-level table and coverage

One row per site (`table_1_site_level_precip_estimates.csv`), n=781. Coverage
(`table_2_column_coverage.csv`):

| Column | Coverage |
|---|---|
| `p_era_mean_mm_1981_2025`, `p_era_cv`, `bio12_mm`, `record_length_years`, `data_hub`, `oneflux_code_version_manifest` | 100% (99.9% for `p_era_cv`, one site with a single ERA5 year) |
| `network` | 98.7% (771/781) |
| `oneflux_code_version` (BIF `PRODUCT_ONEFLUX_VERSION`) | 99.9% (780/781) |
| `badm_map_mm` | 83.6% (653/781) |
| `elevation_m` | 87.3% (682/781) |
| `p_measured_mean_mm`, `n_years_measured`, `frac_measured` | 75.2% (587/781) |

**A rule that depends on `p_measured_mean_mm` cannot be applied to the other 24.8% of the
network (194 sites) with no qualifying QC-measured year** -- this is the single biggest coverage
constraint on any candidate rule below. `bio12_mm` and `p_era_mean_mm_1981_2025` are the only two
quantities available for every site.

The manifest's `oneflux_code_version` field is uniform (`v1.3` for all 781 sites) and carries no
information; `oneflux_code_version_manifest` is retained in the table for completeness but is
**not** used for plot 6 or any rule. The genuinely-informative substitute,
`oneflux_code_version` from each site's BIF `PRODUCT_ONEFLUX_VERSION`, has 6 distinct values
(`1.3.2-rc` through `1.3.7-rc`) and is used instead -- an interpretive choice, stated here rather
than silently made.

**Empirical QC polarity check at YY** (CLAUDE.md flags the coarse-resolution fraction field as a
common source of errors; a prior polarity flip was found at MM resolution but YY had not
previously been checked in this repo): among site-years with `P_F_QC` in the bottom decile
(<=0), `P_F` is bit-identical to `P_ERA` 99.0% of the time; in the top decile (>=1), only 0.3% of
the time. **Higher `P_F_QC` = more measured is confirmed at YY**, network-wide.

## 2b. Averaging window, made explicit (2026-09-22 follow-up)

table_1's original `p_era_mean_mm` column has been renamed `p_era_mean_mm_1981_2025`, and a
second column, `p_era_mean_mm_tower_years` (with its own `n_years_era_tower_years`), has been
added alongside it -- see `scripts/diagnostics/precip_site_filter_tower_years.R`. Every other
column and every existing value in table_1 is unchanged from the original (verified
byte-identical in-script before writing).

**Every P_ERA number quoted anywhere in this report -- sections 2-6 above, every one of plots
1-6, `log_ratio_era_bio12`/`log_ratio_era_measured`, and the candidate-rule analysis in sections
4-6 -- uses the 1981-2025 window (`p_era_mean_mm_1981_2025`)**, i.e. the mean of a site's full
ERA5 record, not the tower-year subset. Figures 1-6 and tables 3-4 were **not** regenerated for
this follow-up and continue to reflect that window only.

Why this matters for a hand check: each site's extraction carries two annual files --
`ERA5_YY`, named for a fixed ~45-year range (1981-2025, or 1981-2024 depending on release
vintage) at *every* site, and `FLUXMET_YY`, named for the tower's own operating years. For
example `JP-Tak`'s FLUXMET_YY file is `..._FLUXMET_YY_1998-2021_...` (24 years) against its
ERA5_YY file's `..._ERA5_YY_1981-2025_...` (45 years). A hand check that opens the FLUXMET_YY
file and averages its own `P_ERA` column is therefore comparing against
`p_era_mean_mm_tower_years`, not `p_era_mean_mm_1981_2025` -- and the two will disagree by an
amount that grows as the tower record shortens relative to the 45-year ERA5 record, not because
either value is wrong. `spot_check_nine_sites.csv` and `table_5c_provenance_yy_files.csv` (both
new) give both means, both year counts, and both source file names side by side for exactly this
kind of check.

## 3. Plots

1. **`fig_1_pairwise_comparison.png`** -- all six pairwise comparisons among P_ERA, measured
   P_F, BIO12, BADM, log-log, 1:1 line. `measured vs. BADM/BIO12` and `BADM vs. BIO12` cluster
   tightly on the 1:1 line; `P_ERA vs. BADM/BIO12` shows a distinct, systematically-offset upper
   band of points above the line (visible as two parallel streaks) alongside the main cluster --
   `P_ERA vs. measured` is the tightest of all six panels.
2. **`fig_2_log_ratio_histograms.png`** -- four log-ratio histograms, Freedman-Diaconis bin width
   chosen per panel from that panel's own data (`graphics::hist(breaks = "FD")`); no
   integer-factor bin edges, no fitted factors, no tolerance windows. The `BADM/BIO12` reference
   panel (top-left, no tower data involved) is tight around 0 with a long, thin tail to about
   -0.8/+0.6 (roughly 0.16x-4x). `P_ERA/BIO12` (top-right) has the same central peak **plus** a
   distinct second mode around log10 = 0.5-0.7 (roughly 3-5x) -- a real network feature, not
   discussed further here since it borders the out-of-scope 123-site cluster question.
3. **`fig_3_sampling_envelope.png`** -- `abs(log10(P_ERA/measured))` against
   `CV(P_ERA)/sqrt(n_measured)`. 41/587 sites (7.0%) exceed the 1x envelope; 7/587 (1.2%) exceed
   the ~95% (1.96x) band -- most of the network's P_ERA-vs-measured disagreement is larger than
   short-record sampling error alone would predict, i.e. it is a real signal, not noise from
   averaging over few years.
4. **`fig_4_era_vs_measured.png`** -- P_ERA against QC-measured precipitation, log-log, 1:1 line,
   587 sites. **Interpretive choice, stated explicitly**: the task's wording ("computed only
   over the months the QC flags call measured") does not have a YY-resolution equivalent without
   reading MM data, which this analysis's own YY-only scope excludes. "Measured" here is read as
   the same annual, QC-selected series defined in section 2 (`P_F_QC > QC_THRESHOLD_YY`), used
   consistently everywhere in this script, not just this one plot.
5. **`fig_5_top10_ratio_timeseries.png`** -- the ten sites with the largest P_ERA/measured ratio
   (US-HB4, CA-PB1, US-ADR, CN-Wnb, BR-CST, US-xJR, US-ARc, US-ARb, JP-NsC, JP-NsM), full annual
   record of both series. Most show P_ERA sitting at a roughly constant, elevated level across
   years while measured P_F varies with real interannual weather -- consistent with a
   site-specific reanalysis-grid artifact (as already established for US-HB4 specifically in
   `it_mbo_parsimony/report.md`) rather than P_ERA tracking a genuinely wetter climate.
6. **`fig_6_ratio_vs_covariates.png`** -- `log10(P_ERA/BIO12)` (broadest-coverage ratio; see
   below) against BIO12, record length, fraction measured, data hub, and ONEFlux processing
   version. **Interpretive choice**: BIO12 is used here rather than "measured" because it is
   available for all 781 sites vs. 587 -- using the narrower ratio would silently drop this
   covariate scan to a 75% subsample. "Hub" uses the manifest's `data_hub` field
   (AmeriFlux/ICOS/TERN, three clean categories; not the multi-valued `network` field, and never
   inferred from `site_id` prefix per CLAUDE.md Hard Rule #2). Two observations: ICOS sites sit
   slightly above zero on average (median ~+0.05) relative to AmeriFlux/TERN, a small hub effect;
   and every one of the most extreme outliers (|log ratio| > 1) falls in BIF processing version
   `1.3.5-rc` -- noted as an observation, not diagnosed further here.

## 4. Baseline: quantifying the current `KG_ERA5_MAP_MAX_MM = 5000` filter

Recomputed fresh in this script from the current (post-refresh) DuckDB `monthly` ERA5 table,
using `R/climate_classification.R`'s own `compute_era5_monthly_climatology()` -- not reused from
any prior report.

- **Site-years removed**: 954, network-wide, out of a 30-candidate-year (1991-2020) basis per
  site.
- **Sites pushed below `KG_ERA5_MIN_YEARS` (20) by the filter alone** (i.e. every one of their
  30 candidate years exceeds 5000 mm/yr, so the filter zeroes them out entirely): **26 sites** --
  `AU-Fog, BR-Ji3, BR-SM1, CA-CF2, DE-SfS, IT-Niv, JP-Api, JP-Fmt, JP-KaP, JP-Kzw, JP-MBF,
  JP-Mse, JP-Nkm, JP-Nuf, JP-Om2, JP-SMF, JP-Shn, JP-Tak, JP-Tkb, JP-Yms, JP-Ynf, KH-Kmp, NO-And,
  PE-QFR, US-Cwt, US-HB4`. **This directly explains an item store_refresh_20260920/report.md
  left unresolved**: "`IT-Niv`'s lost KG classification: mechanically confirmed real, cause not
  diagnosed" -- IT-Niv's refreshed ERA5 record now has `p_era_mean_mm_1981_2025` = 5,963 mm/yr against a
  BIO12 of 1,469 mm/yr (log ratio 0.61, ~4.1x), i.e. all 30 of its candidate years now exceed the
  5000 mm/yr screen where its stale pre-refresh copy evidently did not.
- **Sites the current filter lets through despite implausible P_ERA**: using the 99th percentile
  of `|log10(BADM/BIO12)|` (0.870, ~7.41x) as the "natural disagreement" reference threshold
  (the same threshold reused for candidate Rule B below, not separately invented): 19 of the 717
  sites the year-level filter never touches at all still exceed this threshold in
  `log_ratio_era_bio12`.

## 5. Candidate site-inclusion rules

Each rule is a **once-per-site** decision (unlike the current year-level screen): excluded sites
contribute no years to classification; included sites' full ERA5 record is used with no
year-level MAP cap at all.

| Rule | Basis | Evaluable for | Excludes |
|---|---|---|---|
| A: CV envelope | `abs(log10(P_ERA/measured)) > 3x` the sampling-error envelope (`CV(P_ERA)/sqrt(n_measured)`) -- a generic statistical convention, not fitted to this dataset | 587/781 (75.2%) | 4 |
| B: BIO12 disagreement | `abs(log10(P_ERA/BIO12))` beyond the 99th-percentile natural BADM-vs-BIO12 disagreement (0.870, ~7.41x) | 781/781 (100%) | 22 |
| C: union of A or B | either rule flags the site | 781/781 | 23 |

**Downstream Koppen reclassification effect** (`table_4_candidate_rules.csv`): for sites each
rule keeps, comparing the current classification (`site_koppen_era5.csv`, computed with
`map_max_mm = 5000`) against a fresh reclassification with no year-level MAP screen at all:

| Rule | Sites excluded | Sites remaining | Comparable (classified both ways) | KG class changed |
|---|---:|---:|---:|---:|
| A | 4 | 777 | 752 | **0** |
| B | 22 | 759 | 736 | **0** |
| C | 23 | 758 | 735 | **0** |

**Zero sites change Koppen class under any candidate rule.** This is the load-bearing finding:
removing the year-level 5000 mm/yr screen entirely never flips a classification outcome for any
site that is classifiable both ways. The current filter's only real effect on the network is
binary -- it is the difference between 30/30 valid years and 0/30 valid years for the 26 sites
in section 4, not a graded correction that changes classification for anyone else.

## 6. Why none of A/B/C is recommended as a full replacement

Given section 5's finding, the real test of a candidate rule is not "does it change any
classifications" (none do) but "does it correctly identify the 26 sites the current filter
zeroes out, without collaterally excluding ordinary sites." It fails on both counts:

- **Rule B (natural-disagreement threshold, 7.41x) catches only 3 of the 26** known-corrupted
  sites (`CA-CF2`, and two others at higher ratios; `US-HB4` at 487x is the only unambiguous
  catch). The other 23 sit at `|log10(P_ERA/BIO12)|` between 0.42 and 0.69 (factor ~2.6x-4.9x) --
  **below** the 99th-percentile natural-disagreement reference, because a several-fold
  grid-vs-point disagreement is not actually rare in this network (BADM and BIO12 disagree by a
  similar amount at plenty of ordinary sites; elevation and local topography alone produce this).
- **Loosening the threshold to catch all 26 sites requires reaching down to the 73.5th
  percentile of the entire network's `|log10(P_ERA/BIO12)|` distribution** (the least-extreme of
  the 26, `JP-SMF`, sits at only 0.42). A threshold that low would flag **206 of 781 sites
  (26.4%)** network-wide -- far too broad to be a defensible "implausible" cutoff.
- **An absolute-magnitude alternative (site-mean P_ERA, available for 100% of sites) fares no
  better**: the top of the `p_era_mean_mm_1981_2025` distribution (after the single extreme, US-HB4, at
  657,077) declines smoothly from 10,395 down through the 4,000s with no natural gap -- sites at
  4,489-4,830 mm/yr that are *not* among the 26 known-corrupted sites sit immediately next to
  sites at 4,932-5,102 mm/yr that *are*. Any single absolute threshold, including the current
  round-number 5000, cuts through this continuum somewhat arbitrarily.

**Conclusion: the evidence does not support a single clean statistical threshold -- ratio-based
or magnitude-based -- that cleanly separates the small set of severely reanalysis-corrupted
sites from ordinary geographic disagreement between grid-scale ERA5 and point-scale
references.** The two populations overlap in both ratio-space and magnitude-space. Recommending
Rule A, B, or C as an automatic replacement would either miss most of the sites the current
filter was built to catch (Rule B/C as calibrated) or exclude roughly a quarter of the network
to catch them all.

**What the evidence does support**: the current filter's practical function is a narrow,
binary classifiability gate for a specific, small (26-site) set, not a classification-changing
correction for the network generally (section 5). Given that, the more defensible path is not a
network-wide statistical replacement but a direct look at those 26 sites individually -- e.g.
whether their ERA5 grid cell is known to be coastal, high-elevation, or otherwise a documented
ONEFlux/ERA5 mismatch location -- which is outside this analysis's read-only, quantification-only
scope and is not attempted here.

## 7. Files in this directory

| File | Contents |
|---|---|
| `table_1_site_level_precip_estimates.csv` | One row per site (n=781): five MAP estimates (P_ERA now split into `p_era_mean_mm_1981_2025` and `p_era_mean_mm_tower_years`, see section 2b), coverage/QC columns, log ratios |
| `table_2_column_coverage.csv` | Non-NA coverage of every column in table_1 |
| `table_3_baseline_filter_quantification.csv` | Per-site `n_years_used` with/without the current 5000 mm/yr screen |
| `table_4_candidate_rules.csv` | Exclusion counts and Koppen-reclassification effect for rules A/B/C |
| `table_5_provenance.csv` / `table_5b_provenance_bif_files.csv` | Path, mtime, size, sha256 (top-level inputs) / path, mtime, size (781 BIF files) |
| `table_5c_provenance_yy_files.csv` | Path, file name, mtime, size for every site's ERA5_YY and FLUXMET_YY file (added 2026-09-22, section 2b) |
| `spot_check_nine_sites.csv` | Both P_ERA means, both year counts, BIO12, BADM, measured mean/years, both source file names, for 9 named sites (added 2026-09-22, section 2b) |
| `fig_1_pairwise_comparison.png` | Four estimates against each other, log-log, 1:1 line |
| `fig_2_log_ratio_histograms.png` | Log-ratio histograms, Freedman-Diaconis bins |
| `fig_3_sampling_envelope.png` | Disagreement vs. expected sampling-error envelope |
| `fig_4_era_vs_measured.png` | P_ERA vs. QC-measured precipitation |
| `fig_5_top10_ratio_timeseries.png` | Full annual record, ten largest-ratio sites |
| `fig_6_ratio_vs_covariates.png` | Log ratio against BIO12, record length, fraction measured, hub, ONEFlux version |
