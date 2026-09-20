# IT-MBo/US-HB4/FI-Hyy precipitation parsimony check

Follow-up to `review/diagnostics/it_mbo_bug_hunt/report.md`, which found IT-MBo's reported
ERA5 anomaly is a ~21.25x DD/MM/YY-vs-HH resolution inconsistency in the distributed product,
not a bug in this repository's own code, and that US-HB4 (DD and HH agree, both wrong) is a
different, unaffected failure mode. This is a tighter, three-site check -- no ratios in the
headline numbers, no factor fitting, no tolerance windows, no extension to the wider network.
All values below are in mm/yr (or mm/month, stated explicitly where used).

Read-only diagnostic. Three sites only: IT-MBo (test), US-HB4 (named outlier), FI-Hyy (clean
control). No edits to the pipeline, any committed figure, or any earlier report. New code:
`scripts/diagnostics/it_mbo_parsimony.R`. HH data for all three sites was already extracted
by `it_mbo_bug_hunt.R` -- no new download.

## PIDs (plain text)

| site_id | role | product_id | version |
|---|---|---|---|
| IT-MBo | test site | `enS2fTzGG_9PS5-51hqet8iH` | v1.3 |
| US-HB4 | named outlier | `10.17190/AMF/2571130` | v1.3 |
| FI-Hyy | clean control | `oIJ9cFYf8Q0e9nfMRRS-90vM` | v1.3 |

(Reused from `it_mbo_bug_hunt/table_d4_pid_provenance.csv`, where IT-MBo's was independently
re-confirmed via a live `flux_listall()` call, not re-run here.)

## 1. Mean annual precipitation, four ways

One table, **6 data rows** (3 sites x {P_ERA, P_F}), resolution and BIO12/BADM as columns, per
the literal column instruction ("put WorldClim BIO12 and BADM PI-reported MAP in the same
table as two further columns"). The brief specified "eight rows, readable at a glance"; 8 is
not reachable from 3 sites x 2 variables without inventing rows, so none were added. If the
header + separator line of a markdown table were being counted alongside 6 data rows, that
reaches 8 lines of table source, which may be the origin of the number -- flagged, not guessed
further.

| site_id | variable | HH mean (n yrs) | DD mean (n yrs) | MM mean (n yrs) | YY mean (n yrs) | BIO12 | BADM |
|---|---|---|---|---|---|---|---|
| IT-MBo | P_ERA | 1,126 (45) | 23,905 (44) | 23,905 (44) | 23,905 (44) | 407 | 1,365 |
| IT-MBo | P_F | 1,153 (23) | 24,472 (22) | 24,472 (22) | 24,472 (22) | 407 | 1,365 |
| US-HB4 | P_ERA | 657,077 (45) | 657,077 (45) | 657,077 (45) | 657,077 (45) | 1,350 | 1,429 |
| US-HB4 | P_F | 24,572 (5) | 24,572 (5) | 24,572 (5) | 24,572 (5) | 1,350 | 1,429 |
| FI-Hyy | P_ERA | 709 (45) | 712 (45) | 712 (45) | 712 (45) | 663 | 711 |
| FI-Hyy | P_F | 700 (29) | 702 (28) | 702 (28) | 702 (28) | 663 | 711 |

("Complete coverage" = every HH record present for a day (48/48), every day present for a
year, every month present for a year, at that resolution; YY uses the file's own annual rows
directly. `n_years_hh` for IT-MBo/FI-Hyy is 45 because the HH file spans the full 1981-2025
ERA5 record even though tower FLUXMET HH coverage starts later -- see deliverable 2, which
restricts to years with actual tower data.)

Full table: `table_1_mean_annual_by_resolution.csv`.

**Reading the table**: at IT-MBo, HH sits close to BADM (1,126-1,153 vs. 1,365) while
DD/MM/YY sit ~21x above HH and far above both references. At US-HB4, P_ERA is ~657,000 at
*every* resolution -- no resolution break -- while P_F is ~24,572 at every resolution too
(internally consistent with itself, but still ~17-24x above BIO12/BADM). At FI-Hyy, all four
resolutions agree with each other and sit close to both references (700-712 vs. 663/711).

## 2. Measured or filled

Per site, per year: fraction of HH `P_F` records with `P_F_QC==0`, and the annual total built
from those measured-only records alone (not scaled up for missing months -- a partial-year
sum). Full table: `table_2_measured_or_filled.csv`.

Convention check, done empirically rather than assumed (the same check `it_mbo_bug_hunt`/
`era5_precip_units_v3` used at MM resolution, repeated here at HH): at all three sites, HH
`P_F_QC==0` records are ~0% identical to `P_ERA` when precipitation is nonzero (IT-MBo
0.02%, US-HB4 0%, FI-Hyy 0.13%), and `P_F_QC==2` records are 100% identical to `P_ERA`. **HH
resolution follows the documented System 2 convention correctly** (`0`=measured,
`2`=ERA-Interim-filled) -- the v3 polarity flip was specific to the MM-resolution fraction
field and is not present at HH.

| site_id | mean frac. measured | n years | mean measured-only annual total (mm, partial-year) |
|---|---|---|---|
| IT-MBo | 0.843 | 23 | 963 |
| US-HB4 | 0.982 | 5 | 1,563 |
| FI-Hyy | 0.702 (median 0.966 -- a few early, less-complete years pull the mean down) | 29 | 503 |

**US-HB4's own gauge is ordinary.** Year-by-year measured-only totals: 2020=1,243, 2021=959,
2022=987, 2023=1,302, 2024=3,327 mm -- all physically plausible, with 96-100% of each year's
HH records genuinely measured. The site's full HH `P_F` mean (24,572, table 1) is ~16-25x its
own measured-only years despite only 1-4% of records being ERA-filled, because the ERA-filled
minority carries `P_ERA`'s ~657,000 mm/yr-equivalent magnitude -- a small contaminated
fraction dominates the sum. The same mechanism inflates the MM-resolution `measured_map_mm`
(9,581 mm/yr, `era5_reference_plots/table_site_reference_comparison.csv`, 3 years at
`P_F_QC>=0.9`) well above the site's truly-measured HH values above: even a `P_F_QC>=0.9`
year can contain enough ERA-contaminated months to inflate the annual total several-fold.
**IT-MBo's own gauge is also ordinary**: 963 mm/yr measured-only (partial-year, undercounts
the true annual total by the ~16% unmeasured fraction), consistent with the 1,153 mm/yr full-year
HH `P_F` mean and BADM's 1,365 mm/yr.

## 3. The 2013 months at IT-MBo

Full table: `table_3_itmbo_2012_2014_months.csv`. The coordination package's `fig1` shows a
two-point square-topped spike in IT-MBo's **tower-measured (orange, P_F)** line, not the
ERA5 line -- confirmed by cropping and re-inspecting the figure directly (an initial read of
the uncropped figure misattributed it to ERA5; corrected here). The spike is **January and
June 2013**: `P_F_mm_monthly` = 4,017 and 4,058 mm/month respectively, both carrying
`P_F_QC_mm = 1.0` (the MM-resolution fraction field's maximum, "fully measured or good
gap-fill" on the documented scale). The flat plateau between them in the figure is a plotting
artifact: February-May 2013 have `P_F_QC_mm` of 0.44, 0.00, 0.57, 0.61 -- below the
package's 0.9 measured-quality cutoff -- so those months are dropped from the plotted series
and `geom_line()` draws a straight connector between the two months that do clear the cutoff.

**These two months appear only in the monthly branch.** The same two months' HH-resolution
`P_F` summed directly:

| month | P_F_mm_monthly (MM) | P_F_QC_mm | P_F_hh_summed (HH) | ratio MM/HH |
|---|---|---|---|---|
| 2013-01 | 4,017.4 | 1.0 | 57.5 | 69.8x |
| 2013-06 | 4,058.4 | 1.0 | 94.0 | 43.2x |

Both HH totals are ordinary monthly winter/summer rainfall for this site. Both ratios are far
above the general ~21.25x DD-vs-HH inflation `it_mbo_bug_hunt` found network-wide at IT-MBo --
these two specific months carry an additional, more severe, localized defect on top of the
general resolution inconsistency, and the MM-resolution QC flag (`=1.0`, "fully measured")
gives no warning that the value itself is wrong. This is a second, independent piece of
evidence (beyond the general DD-vs-HH ratio) that the MM/DD/YY branch of this product, not
this repository's code, is where the defect lives at IT-MBo.

## 4. Figure

`fig_mean_annual_by_resolution.png` -- mean annual precipitation by resolution (HH/DD/MM/YY),
both `P_ERA` and `P_F`, one panel per site, log y-axis, BIO12/BADM as horizontal reference
lines, white background.

## 5. Verdict

**IT-MBo**: HH resolution agrees closely between `P_ERA` and `P_F` (1,126 vs. 1,153 mm/yr,
both near BADM's 1,365) while DD/MM/YY agree with each other but sit ~21x above HH for both
variables -- the defect is in the DD-level aggregation/regression step of the distributed
product, not in the underlying ERA5 field, the tower gauge, or this repository's code. **The
number sent (`era5_map_mm` ~24,150-24,472, `ratio_to_measured` ~18-24x) was wrong**: it
describes a resolution artifact, not IT-MBo's true climate, which at HH resolution is
ordinary and closely matches its own gauge.

**US-HB4**: `P_ERA` agrees with itself at every resolution (~657,077 mm/yr, HH through YY) --
a genuine, resolution-independent defect in this site's ERA5 field, not an aggregation
artifact -- while the site's own genuinely-measured (`P_F_QC==0`) HH gauge readings are
ordinary (959-3,327 mm/yr across 2020-2024); the `P_F` "consolidated" annual totals only
balloon to ~24,572 mm/yr because a 1-4%-per-year ERA-filled minority inherits `P_ERA`'s
catastrophic magnitude. **The number sent about ERA5 being catastrophically high at this site
was right** (confirmed at every resolution, not a resolution artifact); a companion
`measured_map_mm` figure computed from MM-resolution `P_F_QC>=0.9` years (9,581 mm/yr) is
itself moderately inflated by the same small-contaminated-fraction mechanism relative to the
site's truly-measured-only HH values (959-3,327 mm/yr) -- **right about ERA5, right for the
wrong reason if `measured_map_mm` was quoted as a clean "true" baseline** without that caveat.

**FI-Hyy**: all four resolutions agree closely with each other and with both BIO12 and BADM
(700-712 mm/yr vs. 663/711) -- the product is internally consistent and accurate at this
control site, confirming the IT-MBo/US-HB4 defects are site-specific, not general
ICOS/ONEFlux characteristics. No number was sent for FI-Hyy; it is not part of `site_list.csv`.

## Provenance

File names, byte sizes, sha256 for every raw file read by this script: `table_provenance_file_hashes.csv`.
IT-MBo's MM/DD/YY files and both sites' new-download HH files are already hashed identically
in `review/diagnostics/it_mbo_bug_hunt/table_d4_file_provenance.csv`; this table adds FI-Hyy's
files and US-HB4's/FI-Hyy's DD/MM/YY files, not previously hashed. Snapshot:
`data/snapshots/fluxnet_shuttle_snapshot_20260901T094522.csv` (same snapshot used throughout
`it_mbo_bug_hunt`; sha256 `5591884a5a722e09cef948647a259926d5c7d85b9614c0dd499684a0b4b66d11`).

## Files in this directory

| File | Contents |
|---|---|
| `table_1_mean_annual_by_resolution.csv` | Mean annual precip by resolution, both variables, 3 sites, + BIO12/BADM |
| `table_2_measured_or_filled.csv` | Per site, per year: HH measured fraction and measured-only annual total |
| `table_3_itmbo_2012_2014_months.csv` | IT-MBo 2012-2014: monthly P_F/P_F_QC vs. HH-summed |
| `fig_mean_annual_by_resolution.png` | 3-panel figure, log y-axis, BIO12/BADM reference lines |
| `table_provenance_file_hashes.csv` | File name, byte size, sha256 for every raw file read here |
