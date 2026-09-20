# Cluster resolution sample: does IT-MBo's resolution mismatch generalise?

> **Provisional — pending store audit (2026-09-20).** The numbers in this report rest on the June 2026 `data/extracted/` extraction. One file in that extraction (IT-MBo's `FLUXNET_FLUXMET_MM` file) is known to differ from the currently-distributed archive under the same product ID. This report's conclusions are provisional pending `review/diagnostics/store_audit/` (in progress).

## Verdict

**Zero of 19 sampled flagged sites, across all 8 networks the 4x/8x cluster spans, show the
monthly branch disagreeing with the half-hourly branch.** All 19 agree within the fixed
threshold (differs by more than 2x either direction); the closest any comes to that
threshold is a 0.58% difference (`DE-Hzd`, ratio 1.0058), and every other flagged site
agrees to within 0.03%. **The disagreement is not confined to particular networks -- it is
not present in any network sampled** (AMF, CNF, EUF, FLX, ICOS, JPF, KOF, TERN all show
0/n disagreement). **The sample supports situation (b): the branches agree, and the 4x/8x
flag reflects disagreement with the external references (BIO12/BADM), not an internal
resolution-consistency defect in the distributed product.** IT-MBo's ~21.25x DD/MM/YY-vs-HH
inflation (`it_mbo_bug_hunt/report.md`) does not generalise to this sample -- it looks like
an isolated, site-specific defect, not a general characteristic of the cluster.

**Implication for the Jaccard analysis: 0 of 19 sampled flagged sites (0%) show evidence that
their coarse-resolution ERA5 precipitation is internally unreliable, so nothing in this sample
supports excluding the 123-site cluster's climate classifications on IT-MBo's grounds** --
whatever drives the 4x/8x pattern against BIO12/BADM at these sites, it is very unlikely to be
the same DD/MM/YY-vs-HH artifact found at IT-MBo. This does not clear the cluster of the
original BIO12/BADM disagreement itself (untouched by this diagnostic, and still the open
question `era5_precip_units_v4` left unresolved), only of the specific internal-resolution
explanation this sample was designed to test.

---

## Background and scope

Follow-up to `it_mbo_bug_hunt/` and `it_mbo_parsimony/`, which found IT-MBo's reported ERA5
anomaly is a resolution-branch artifact specific to that site (median DD-vs-HH ratio 21.2451),
not present at a same-hub control (FI-Hyy). This left open whether IT-MBo is typical of the
wider 123-site 4x/8x cluster (`era5_precip_units_v4`) or unusual. This diagnostic answers that
with a sample, not the full 123 sites.

Read-only with respect to every existing diagnostic output and the pipeline itself: nothing in
`review/diagnostics/era5_precip_units*/`, `era5_reference_plots/`, `era5_cumulative_test/`,
`era5_share_for_coordination/`, `it_mbo_bug_hunt/`, or `it_mbo_parsimony/` was modified, and no
pipeline script, committed figure, or `R/climate_classification.R` was edited. No figure or
classification anywhere in the repo was changed.

New code: `scripts/diagnostics/cluster_resolution_sample_download.R` (sample selection +
background HH download/extract), `scripts/diagnostics/cluster_resolution_sample_retry.R`
(one-off retry for 3 sites whose first-attempt ZIP downloads were truncated -- confirmed via
Python `zipfile`, not a disk-space issue, 121 GiB free at the time), and
`scripts/diagnostics/cluster_resolution_sample_check.R` (the check itself). All outputs:
`review/diagnostics/cluster_resolution_sample/`.

All values in mm/yr throughout. No ratio statistics as headline numbers, no factor fitting.
The one agreement/disagreement criterion used in the verdict is a fixed, stated, round-number
threshold (differs by more than 2x) -- not derived from this sample.

## Sample selection

From `review/diagnostics/era5_reference_plots/table_site_reference_comparison.csv`'s own
`cluster_membership` column (`4x_cluster` / `8x_cluster` / `not_in_4x_or_8x_cluster` --
values read directly from the file, not assumed): grouped by `product_source_network` (the
network field; site ID prefixes were never used to infer network or hub, per Hard Rule 2).
Drew up to 3 flagged sites per network (all of them where a network has fewer than 3), plus 1
unflagged control site per network from the same file. **Fixed random seed 20260920**, drawn
and written to disk (`table_0_sample_selection.csv`) before any download started.

8 networks are present in the 123-site cluster: AMF (58 flagged), CNF (8), EUF (23), FLX (1),
ICOS (2), JPF (25), KOF (5), TERN (1). Sample: **27 sites total** -- 19 flagged (3 each from
AMF/CNF/EUF/JPF/KOF, all available from FLX (1)/ICOS (2)/TERN (1)) + 8 controls (1 per
network).

## Download

HH resolution was not on disk for any of the 27 sampled sites. Downloaded and extracted via
single-site `flux_download()`/`flux_extract(resolutions="h")` calls, launched with
`nohup ... & disown`, logged to `logs/cluster_resolution_sample_download_20260920T074741.log`
(PID 25339). 3 of 27 first-attempt downloads (`RU-Ege`, `MY-LHP`, `AU-Ya1`) produced truncated
ZIPs (`unzip -l`: "End-of-central-directory signature not found"; confirmed via Python
`zipfile.ZipFile()`: "File is not a zip file" -- not a disk-space issue, 121 GiB free at the
time) and were re-downloaded individually
(`logs/cluster_resolution_sample_retry_20260920T080336.log`, PID 25874); all 3 succeeded on
retry. Those same 3 sites also had no `y`/`m`/`d` resolution extracted (unlike the other 24
sampled sites, which already had it from the pipeline's normal network-wide extraction) --
extracted directly from the already-downloaded ZIPs (no further download) once the corrupted
files were replaced. `data/raw/` and `data/extracted/` are gitignored per Hard Rule 4 and are
not part of any commit.

## Check

For each of the 27 sampled sites, for both `P_ERA` and `P_F`: mean annual precipitation from
the half-hourly, daily, monthly, and annual files, using only calendar years with complete
coverage at that resolution (same methodology as `it_mbo_parsimony.R`: complete HH days require
48 half-hourly records; complete years require 365/366 complete days or 12 complete months as
appropriate). WorldClim BIO12 and BADM PI-reported MAP carried alongside as reference columns.
One row per site per variable (54 rows, 27 sites x 2 variables):
`table_1_mean_annual_by_resolution.csv`.

## Output

**Table**: `table_1_mean_annual_by_resolution.csv` -- exactly as described above.

**Figure**: `fig_mm_vs_hh_p_era.png` -- mean annual `P_ERA` from the monthly branch (x) against
the half-hourly branch (y), one point per site, log-log axes, 1:1 reference line (not fitted).
Flagged vs. control sites distinguished by color and shape. White background. `P_F` is in the
table but not this figure -- the 4x/8x flag is defined on `P_ERA`, and the brief specifies one
point per site, so `P_ERA` is the variable plotted.

Every point in the figure sits on or almost exactly on the 1:1 line, flagged and control sites
alike -- visually confirming the verdict above.

## Full per-site detail (flagged sites, MM-vs-HH ratio)

| site_id | network | mean_mm_mm | mean_hh_mm | ratio_mm_to_hh | disagrees (>2x) |
|---|---|---:|---:|---:|---|
| US-PFi | AMF | 3207.00 | 3207.07 | 0.99998 | FALSE |
| US-DFK | AMF | 3283.97 | 3284.05 | 0.99998 | FALSE |
| CA-Cbo | AMF | 3332.73 | 3332.80 | 0.99998 | FALSE |
| CN-HeD | CNF | 140.89 | 140.92 | 0.99980 | FALSE |
| CN-Dda | CNF | 1161.45 | 1161.50 | 0.99996 | FALSE |
| CN-Jng | CNF | 3012.10 | 3012.24 | 0.99995 | FALSE |
| DE-Zrk | EUF | 2660.75 | 2660.87 | 0.99996 | FALSE |
| FI-Nsv | EUF | 2625.20 | 2625.29 | 0.99997 | FALSE |
| DE-Akm | EUF | 2406.12 | 2406.24 | 0.99995 | FALSE |
| CN-Sb2 | FLX | 2107.92 | 2108.02 | 0.99995 | FALSE |
| IE-Cra | ICOS | 3463.37 | 3463.52 | 0.99996 | FALSE |
| DE-Hzd | ICOS | 3284.15 | 3265.15 | 1.00582 | FALSE |
| JP-Mse | JPF | 5102.71 | 5102.79 | 0.99999 | FALSE |
| JP-Nkm | JPF | 8249.44 | 8249.57 | 0.99998 | FALSE |
| RU-Ege | JPF | 1355.88 | 1356.00 | 0.99991 | FALSE |
| KR-TwC | KOF | 4665.45 | 4665.53 | 0.99998 | FALSE |
| KR-Nj1 | KOF | 4658.30 | 4658.39 | 0.99998 | FALSE |
| KR-Nj2 | KOF | 4654.28 | 4654.37 | 0.99998 | FALSE |
| AU-Fog | TERN | 5814.46 | 5814.54 | 0.99999 | FALSE |

Control sites (sanity check, same criterion): all 8 also agree (`US-Bar`, `CN-Aro`, `DE-SfS`,
`KE-Chk`, `MY-LHP`, `KR-HPK`, `AU-Ya1` all within 0.01% of 1:1; `GF-Guy` the largest control
deviation at ratio 0.8893, an 11% difference, still far inside the 2x threshold).

## Fixed disagreement criterion

Monthly-branch mean annual `P_ERA` differs from the half-hourly-branch mean by more than 2x in
either direction. Stated here as a simple, round-number rule, not fit to this sample -- the
actual ratios observed (0.9998-1.0058 for every flagged site) are far below what this or any
plausible threshold would flag, so the choice of exact cutoff does not affect the verdict.
