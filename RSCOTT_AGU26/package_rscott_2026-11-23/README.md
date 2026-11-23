# Dryland GPP vs. precipitation extraction, for Russ Scott (AGU26 poster)

Prepared by the FLUXNET Annual Paper 2026 team (EcosystemEcologyLab, University of Arizona),
2026-11-23, from the FLUXNET Shuttle snapshot taken 2026-09-20. This package only extracts and
organizes data -- we have not done any GPP-vs-precipitation analysis or interpretation
ourselves; that's your call.

## What's in this folder

- `sites.csv` -- one row per kept site, with its metadata, screening numbers, and citation info.
- `site_years.csv` -- one row per site, time step (calendar or water year) and year, with every
  value, its QC flag, and whether it passed our screens. Includes years that did **not** pass,
  so you can see the full record and decide for yourself whether to use them.
- `excluded_sites.csv` -- every candidate dryland site we looked at but didn't keep, with the
  reason and the numbers behind that reason.
- `check_gpp_vs_precip_DRAFT.png` -- a draft scatterplot we made only to eyeball the data for
  obvious problems before sending it to you. Not a finished figure -- please don't use it as-is.

## How sites were selected

1. **Candidate list**: every FLUXNET Shuttle site whose IGBP land-cover class (as reported by
   the site team, not inferred from anything else) is Open Shrubland (OSH), Closed Shrubland
   (CSH), Grassland (GRA), Savanna (SAV), or Woody Savanna (WSA) -- 231 sites, either hemisphere.
2. **Precipitation screen**: a calendar year counts as having *measured* precipitation if at
   most 10% of its annual precipitation total is filled in from downscaled ERA5 reanalysis
   rather than the tower's own gauge (see "ERA-filled share" below). A site needs at least 2
   such years to even be screened. Its mean annual measured precipitation (averaged over just
   those measured years) then has to be below 800 mm.
3. **Keep rule**: among the site's years where GPP, ET, and measured precipitation **all**
   pass our quality checks (see below), the site needs at least 5 such years, in *either* the
   calendar-year or the water-year accounting (doesn't have to be both). We also report how many
   years would pass without the precipitation condition, so you can see what that 10% rule costs
   each site.

231 candidates -> 43 kept, 188 excluded (79 had too few measured-precipitation years to even
screen, 64 were screened but too wet on average, 45 passed the wetness screen but didn't have
5 good years in either time step). See `excluded_sites.csv` for the detail.

## Columns in `site_years.csv`

| Column | Meaning |
|---|---|
| `site_id` | FLUXNET site ID |
| `time_step` | `calendar` (Jan-Dec) or `water` (see water-year convention below) |
| `period_label` | the year (calendar) or `WY<year>` (water year, see labelling below) |
| `year` | the year number, same meaning as `period_label` |
| `carbon_src` | `VUT` or `CUT` -- which NEE/GPP partitioning variant this site uses (see below) |
| `nee_qc` | the site's NEE QC value for that calendar year (NA at water-year rows -- see `n_months_gpp_passing` instead) |
| `gpp_nt_gC_m2_yr` | GPP, nighttime-partitioning method, g C m-2 for that period |
| `gpp_dt_gC_m2_yr` | GPP, daytime-partitioning method, g C m-2 for that period |
| `et_le_f_mds_mm_yr` | evapotranspiration from LE_F_MDS (gap-filled latent heat flux), mm H2O for that period |
| `et_le_corr_mm_yr` | evapotranspiration from LE_CORR (energy-balance-corrected latent heat flux), mm H2O for that period. **Often missing** -- LE_CORR is only available for sites/years where FLUXNET computed an energy-balance closure correction; use `et_le_f_mds_mm_yr` as your primary ET column. |
| `LE_F_MDS_QC` | the site's LE_F_MDS QC value for that calendar year (NA at water-year rows) |
| `et_qc_source` | states explicitly that LE_CORR is quality-gated using the LE_F_MDS QC column, because LE_CORR has no QC column of its own |
| `p_f_mm_yr` | precipitation (tower + ERA5-filled combined), mm, for that period |
| `p_f_qc` | the site's raw P_F_QC value for that calendar year (NA at water-year rows -- the water-year ERA-filled share is built from daily data instead, see below) |
| `era_filled_share` | fraction (0-1) of `p_f_mm_yr` that is filled in from ERA5 rather than measured by the gauge. **Low = good** (more of the precipitation total is real tower data). |
| `n_months_present` | water-year rows only: how many of the 12 months had data at all |
| `n_months_gpp_passing` / `n_months_et_passing` | water-year rows only: of the months present, how many individually passed the monthly QC threshold for GPP/ET |
| `n_days_present` / `precip_complete` | water-year rows only: how many days of daily data went into the precipitation total, and whether that's the full water year (if not, `p_f_mm_yr`/`era_filled_share` are blank for that row -- we didn't want to report a partial-year total as if it were a full one) |
| `pass_gpp` / `pass_et` / `pass_precip_measured` | TRUE/FALSE pass flags used in our screening (see below) |

GPP and ET values are included for **every** site-year of a kept site, whether or not that
year passed -- the pass flags tell you which ones did. Units: GPP in g C m-2 per period; ET and
P in mm per period (a "period" is one calendar year or one water year, never a month or day).

## Sign and partitioning notes

- GPP is reported as a positive flux (uptake), the FLUXNET convention.
- `carbon_src` (in `sites.csv` this is implicit in which GPP numbers are populated; in
  `site_years.csv` it's an explicit column) records whether this site's NEE/GPP come from the
  VUT (variable-u*-threshold) or CUT (constant-u*-threshold) partitioning variant. This is
  decided **once per site**, using the same rule the Annual Paper itself uses: VUT if the site
  has any usable `NEE_VUT_REF_QC` value, otherwise CUT. A site's VUT and CUT values are never
  mixed within that site's record.
- GPP_NT and GPP_DT have no QC column of their own in the FLUXNET product -- both are
  quality-gated using the site's chosen NEE QC column (`nee_qc` at calendar year; the monthly
  NEE QC at water year, summarized in `n_months_gpp_passing`).

## The QC rule (`pass_gpp` / `pass_et`)

- **Calendar year**: passes if the relevant QC value (NEE QC for GPP; `LE_F_MDS_QC` for ET) is
  at or above 0.50 (the project's standard coarse-resolution QC threshold).
- **Water year**: passes only if **all 12 months** are present **and** every one of those 12
  months individually meets the same 0.50 threshold. `n_months_gpp_passing` /
  `n_months_et_passing` tell you how many months passed even when the overall year didn't, in
  case a near-miss is still useful to you.

## ERA-filled share -- what it is and how to use it

Tower precipitation (`P_F`) in the FLUXNET product is not always a real rain-gauge
measurement -- part of it can be filled in from a downscaled ERA5 reanalysis estimate when the
tower's own gauge record has gaps. `era_filled_share` is the fraction of the period's total
precipitation that came from that reanalysis fill, not the gauge. We treat a year as having
"measured precipitation" when `era_filled_share <= 0.10` (at most 10% reanalysis-filled). We
checked this interpretation carefully (the `P_F_QC` flag's meaning is not obvious and this
project's own records show someone got it backwards once before) -- see
`docs/report_back_2026-11-23.md` for exactly how we confirmed it. **If you want to be stricter
than our default**, you can recompute your own cutoff directly from `era_filled_share` in
`site_years.csv` -- we report the share itself, not just a pass/fail, for exactly this reason.

Every kept site has at least 2 years of genuinely-measured precipitation (that's required to be
kept at all). Several excluded candidates have essentially 100% ERA-filled precipitation in
every year on record (e.g. `AR-CCg`); those show up as "fewer than 2 years of measured
precipitation" in `excluded_sites.csv`.

## Water-year conventions and the driest-month column

Water years are built from monthly data and labelled by the year they **end in**:

- **Northern Hemisphere sites**: October through September, so "water year 2020" = Oct 2019
  through Sep 2020.
- **Southern Hemisphere sites**: July through June, so "water year 2020" = Jul 2019 through Jun
  2020.

Hemisphere is assigned by the sign of the tower's latitude (negative = Southern), in `sites.csv`.

**Calendar years split the wet season at Southern Hemisphere sites** whose wet season falls in
the austral summer (roughly Nov-Mar), because Jan 1 falls in the middle of it -- a calendar
year's precipitation total there is not a clean "one wet season" total the way a water year is.
This is a general property of the SH calendar-year convention and isn't itself something we
flag per-site; it's the reason the water-year columns exist at all for this comparison.

**The water-year convention itself can, less often, split the wet season too**, if a site's
rains straddle the Oct 1 (NH) or Jul 1 (SH) cut rather than sitting safely inside one water
year. `sites.csv`'s `driest_month_name` column gives each kept site's climatologically driest
calendar month (by measured precipitation only, averaged across all available years), and
`rainy_season_split_risk` flags TRUE when the month right before the water-year boundary *and*
the boundary month itself are **both** among that site's three wettest months -- i.e. the cut
falls inside a still-wet stretch, not at a clean wet-to-dry transition. Seven kept sites are
flagged this way (all Northern Hemisphere, all with precipitation arriving in more than one
season across the year): `US-Fwf`, `US-Seg`, `US-Jo2`, `US-Ses`, `US-Wjs`, `US-Mpj`, `ES-Agu`.
For these, treat water-year totals with a little more caution, or cross-check against the
calendar-year columns.

## Citing this data

**Every site's data must be cited by its own persistent identifier (PID/DOI)**, not just a
generic FLUXNET citation. `sites.csv` has `product_id` (the DOI) and `product_citation` (the
full citation string) for every kept site, taken directly from the FLUXNET Shuttle snapshot.
Please include all 43 citations (or a link to the full list) in your poster's references, per
the FLUXNET data use policy. Also cite Pastorello et al. (2020), *Scientific Data* 7:225, for
the underlying ONEFlux processing pipeline.

## Questions

This package only extracts and organizes data -- we haven't interpreted the GPP-precipitation
relationship itself. If anything here looks off, or you want a different screen/threshold, let
us know and we can rebuild the package rather than you working around it.
