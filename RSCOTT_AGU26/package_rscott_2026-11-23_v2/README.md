# Dryland GPP vs. precipitation extraction, for Russ Scott (AGU26 poster) -- v2

Prepared by the FLUXNET Annual Paper 2026 team (EcosystemEcologyLab, University of Arizona),
2026-11-23, from the FLUXNET Shuttle snapshot taken 2026-09-20. This package only extracts and
organizes data -- we have not done any GPP-vs-precipitation analysis or interpretation
ourselves; that's your call.

**This is v2, a widened version of the first package** (`package_rscott_2026-11-23/`, now
superseded -- see `../README.md`). Dave's guidance for this version: keep as many sites as are
likely to interest you, and give you columns to filter on yourself rather than us dropping sites
for you. The original, stricter package is left untouched if you'd rather start from it; v2
gives you the columns to reproduce it exactly (see "Reproducing the stricter v1 selection"
below), or to build your own cut.

## What's in this folder

- `sites.csv` -- one row per kept site (54 now, up from 43 in v1), with its metadata, screening
  numbers, new filtering columns (see below), and citation info.
- `site_years.csv` -- one row per site, time step (calendar or water year) and year, with every
  value, its QC flag, a per-year `precip_source`, and whether it passed our screens. Includes
  years that did **not** pass, so you can see the full record and decide for yourself.
- `excluded_sites.csv` -- every candidate dryland site we looked at but didn't keep even under
  the relaxed v2 rule, with the reason and the numbers behind it.
- `check_gpp_vs_precip_v2_DRAFT.png` -- a draft scatterplot, all 54 kept sites, to eyeball the
  data for obvious problems before sending it to you. Not a finished figure -- please don't use
  it as-is.

## What changed from v1 (the relaxed rules)

1. **Candidate list is unchanged**: still every FLUXNET Shuttle site with IGBP class OSH, CSH,
   GRA, SAV or WSA (site-team-reported) -- 231 sites, either hemisphere.
2. **Precipitation source is now a per-site label, not a filter.** A site is `measured` if it
   has **at least 1** calendar year (not the v1's 2) with an ERA-filled share of at most 10%;
   otherwise it's `ERA5-only` (its tower `P_F` is entirely or mostly filled in from downscaled
   ERA5 reanalysis rather than gauge measurements). The 800 mm screen uses the mean annual `P_F`
   from measured years for `measured` sites, and from **all** years for `ERA5-only` sites --
   neither kind of site is excluded just for its precipitation's provenance.
3. **The keep rule's precipitation condition is relaxed.** A site is kept if its mean annual P is
   below 800 mm **and** it has at least 5 years (calendar or water year, either one) in which
   GPP_NT and ET (LE_F_MDS) both pass QC **and `P_F` is simply present** -- it no longer has to
   be measured-quality precipitation. We separately report, in `sites.csv`, how many of those
   passing years also happen to have measured precipitation, so you can see what the stricter
   v1 rule would have cost each site.
4. **Nothing is dropped for being managed, cold, mesic, or flagged elsewhere.** `US-Aud` is kept
   (it already was in v1 too). Negative annual GPP values are kept, with their ordinary QC flags
   -- we never filter on the sign of GPP, only on data quality.

**231 candidates -> 54 kept (up from 43), 177 excluded** (112 screened but mean P >= 800 mm, 65
passed the wetness screen but had fewer than 5 relaxed-passing years in either time step; every
candidate has a usable `P_F` value in at least one calendar year, so there were no longer any
"unscreenable" sites under this relaxed rule). See `excluded_sites.csv` for the detail, and
`docs/report_back_2026-11-23_v2.md` for which of the 11 newly-added sites came from which
relaxation.

**ERA5-only precipitation -- please read before using it.** For the one `ERA5-only` kept site
(`CN-Sdq`), `P_F` was never adjusted to or validated against that tower's own rain gauge. It is a
downscaled ERA5 reanalysis estimate at roughly 25 km native grid spacing, and reanalysis
precipitation is known to be weak at representing convective, highly localized dryland rainfall
event-to-event and year-to-year -- use it with real caution for anything beyond a coarse
screening comparison. `site_years.csv`'s per-year `precip_source` column tells you exactly which
years at *any* kept site (not just `CN-Sdq`) are ERA-filled rather than measured, so you can
exclude or flag them yourself.

## Reproducing the stricter v1 selection from these columns

If you want v1's exact 43-site, measured-precipitation-only selection from this v2 package:
1. Keep only sites where `sites.csv$precip_source == "measured"` **and**
   `sites.csv$n_passing_calendar_relaxed_and_measured >= 5` (calendar) or
   `n_passing_water_relaxed_and_measured >= 5` (water year, same column at water-year resolution).
2. In `site_years.csv`, keep only rows where `precip_source == "measured"` (equivalently,
   `era_filled_share <= 0.10`) when computing anything that should match v1's precipitation
   treatment.
Or just use `package_rscott_2026-11-23/` directly -- it's left exactly as it was.

## New columns in `sites.csv` (v2 only), with sources

| Column | Meaning | Source |
|---|---|---|
| `precip_source` | `measured` (>=1 calendar year with ERA-filled share <= 10%) or `ERA5-only` | computed from `site_years_candidates_full.csv`'s calendar-year `era_filled_share` |
| `n_measured_years_calendar` | how many calendar years qualify as measured | same |
| `mean_annual_p_mm` | mean annual `P_F` used for the 800 mm screen (measured years only if `precip_source=measured`, all years if `ERA5-only`) | same |
| `n_passing_calendar_relaxed` / `n_passing_water_relaxed` | years (calendar / water) where GPP_NT, ET and `P_F`-presence all pass -- the v2 keep-rule counts | `RSCOTT_AGU26/code/06_v2_relaxed_selection.R` |
| `n_passing_calendar_relaxed_and_measured` / `n_passing_water_relaxed_and_measured` | the same counts, restricted to years that *also* have measured precipitation -- what the v1-style rule would have given this site | same |
| `mean_ta_f_degc` | mean annual air temperature, native degC, over calendar years passing QC (`qc_passes(TA_F_QC, QC_THRESHOLD_YY)`) | `data/duckdb/fluxnet.duckdb::annual` (raw `TA_F`, **not** `annual_converted` -- this column is native degC, not Kelvin) |
| `n_years_ta_f` | how many calendar years went into `mean_ta_f_degc` | same |
| `koppen_class_paper` | Koppen-Geiger class exactly as used by the Annual Paper's own network-representativeness figure (panel A): the site's PI-reported class (BADM `CLIMATE_KOEPPEN`) where available, otherwise its own ERA5-local classification | `data/snapshots/site_koppen_era5_fig4.csv` (`panel_a_class_used`/`panel_a_eligible`), built by `scripts/figure4_representativeness.R` |
| `koppen_na_reason` | why `koppen_class_paper` is blank, when it is | same |
| `aridity_index_paper` | aridity index exactly as used by the Annual Paper's representativeness figure (panel C): 1991-2020 mean `P_ERA` divided by FAO-56 Penman-Monteith reference evapotranspiration, computed from the **site's own** ERA5 meteorological record (not a global gridded product) | `data/snapshots/site_aridity_era5_fig4.csv` (`ai_value`), built by `scripts/figure4_representativeness.R`'s `fao56_et0_mm()`. **This is not the same thing as a CGIAR/WorldClim aridity-index raster you may see cited elsewhere in FLUXNET work** -- it's specific to this paper's own site-level ERA5 calculation. |
| `aridity_na_reason` | why `aridity_index_paper` is blank, when it is (an invalid raw ERA5 input, or the paper's own precipitation-reliability screen against independent references) | same |
| `n_siteyears_negative_gpp` | count of this site's site-year rows (both time steps, GPP_NT **or** GPP_DT) with a negative annual GPP value | `site_years_candidates_full.csv` |

All of `sites.csv`'s NA counts for the two new sourced columns, and which kept sites they affect,
are reported in `docs/report_back_2026-11-23_v2.md`.

## Columns in `site_years.csv`

Same as v1 (see below), plus:

| Column | Meaning |
|---|---|
| `pass_precip_present` | TRUE if `p_f_mm_yr` is present at all for that row, whatever its source -- this, not `pass_precip_measured`, is the precipitation condition in the v2 keep rule |
| `precip_source` | **per year**, not per site: `measured` if `era_filled_share <= 0.10`, `ERA-filled` otherwise, blank if the share itself couldn't be computed for that row. (Careful: `sites.csv`'s `precip_source` column is a *site-level* summary using the values `measured`/`ERA5-only` -- the two columns answer related but different questions.) |

| Column | Meaning |
|---|---|
| `site_id` | FLUXNET site ID |
| `time_step` | `calendar` (Jan-Dec) or `water` (see water-year convention below) |
| `period_label` | the year (calendar) or `WY<year>` (water year, see labelling below) |
| `year` | the year number, same meaning as `period_label` |
| `carbon_src` | `VUT` or `CUT` -- which NEE/GPP partitioning variant this site uses (see below) |
| `nee_qc` | the site's NEE QC value for that calendar year (NA at water-year rows -- see `n_months_gpp_passing` instead) |
| `gpp_nt_gC_m2_yr` | GPP, nighttime-partitioning method, g C m-2 for that period (can be negative in some site-years -- kept, with its QC flag; not filtered on sign) |
| `gpp_dt_gC_m2_yr` | GPP, daytime-partitioning method, g C m-2 for that period (same note) |
| `et_le_f_mds_mm_yr` | evapotranspiration from LE_F_MDS (gap-filled latent heat flux), mm H2O for that period |
| `et_le_corr_mm_yr` | evapotranspiration from LE_CORR (energy-balance-corrected latent heat flux), mm H2O for that period. **Often missing** -- use `et_le_f_mds_mm_yr` as your primary ET column. |
| `LE_F_MDS_QC` | the site's LE_F_MDS QC value for that calendar year (NA at water-year rows) |
| `et_qc_source` | states explicitly that LE_CORR is quality-gated using the LE_F_MDS QC column, because LE_CORR has no QC column of its own |
| `p_f_mm_yr` | precipitation (tower + ERA5-filled combined), mm, for that period |
| `p_f_qc` | the site's raw P_F_QC value for that calendar year (NA at water-year rows -- the water-year ERA-filled share is built from daily data instead) |
| `era_filled_share` | fraction (0-1) of `p_f_mm_yr` that is filled in from ERA5 rather than measured by the gauge. **Low = good.** |
| `n_months_present` | water-year rows only: how many of the 12 months had data at all |
| `n_months_gpp_passing` / `n_months_et_passing` | water-year rows only: of the months present, how many individually passed the monthly QC threshold |
| `n_days_present` / `precip_complete` | water-year rows only: daily coverage behind the precipitation total; if incomplete, `p_f_mm_yr`/`era_filled_share` are blank for that row rather than reporting a partial total |
| `pass_gpp` / `pass_et` | TRUE/FALSE QC pass flags (unchanged from v1) |
| `pass_precip_measured` | TRUE/FALSE, era_filled_share <= 10% -- v1's precipitation condition, still reported for reference |

GPP and ET values are included for **every** site-year of a kept site, whether or not that year
passed -- the pass flags tell you which ones did. Units: GPP in g C m-2 per period; ET and P in
mm per period (a "period" is one calendar year or one water year, never a month or day).

## Sign and partitioning notes

- GPP is reported as a positive flux (uptake) by convention, but **some site-years are
  genuinely negative** -- these are real values from the FLUXNET product, not errors, kept with
  their ordinary QC flags (4 of the 54 kept sites have at least one: see `n_siteyears_negative_gpp`
  in `sites.csv`). We have not filtered on the sign of GPP anywhere in this extraction.
- `carbon_src` records whether this site's NEE/GPP come from the VUT (variable-u*-threshold) or
  CUT (constant-u*-threshold) partitioning variant -- decided once per site, using the same rule
  the Annual Paper itself uses: VUT if the site has any usable `NEE_VUT_REF_QC` value, otherwise
  CUT. Never mixed within a site.
- GPP_NT and GPP_DT have no QC column of their own -- both are quality-gated using the site's
  chosen NEE QC column.

## The QC rule (`pass_gpp` / `pass_et`)

- **Calendar year**: passes if the relevant QC value (NEE QC for GPP; `LE_F_MDS_QC` for ET) is
  at or above 0.50 (the project's standard coarse-resolution QC threshold).
- **Water year**: passes only if **all 12 months** are present **and** every one of those 12
  months individually meets the same 0.50 threshold.

## ERA-filled share -- what it is and how to use it

Tower precipitation (`P_F`) in the FLUXNET product is not always a real rain-gauge measurement
-- part of it can be filled in from a downscaled ERA5 reanalysis estimate when the tower's own
gauge record has gaps. `era_filled_share` is the fraction of the period's total precipitation
that came from that reanalysis fill, not the gauge. We treat a year as having "measured
precipitation" when `era_filled_share <= 0.10`. We checked this interpretation carefully (the
`P_F_QC` flag's meaning is not obvious, and this project's own records show someone got it
backwards once before) -- see `docs/report_back_2026-11-23.md` (v1) for exactly how we confirmed
it; the same confirmation applies here unchanged. **If you want to be stricter than our
default**, recompute your own cutoff directly from `era_filled_share` (or from the per-year
`precip_source` column) in `site_years.csv`.

## Water-year conventions and the driest-month column

Water years are built from monthly data and labelled by the year they **end in**:

- **Northern Hemisphere sites**: October through September.
- **Southern Hemisphere sites**: July through June.

Hemisphere is assigned by the sign of the tower's latitude (negative = Southern), in `sites.csv`.

**Calendar years split the wet season at Southern Hemisphere sites** whose wet season falls in
the austral summer, because Jan 1 falls in the middle of it -- this is why the water-year columns
exist at all.

**The water-year convention itself can, less often, split the wet season too**, if a site's
rains straddle the Oct 1 (NH) or Jul 1 (SH) cut. `sites.csv`'s `driest_month_name` column gives
each kept site's climatologically driest calendar month (measured precipitation only), and
`rainy_season_split_risk` flags TRUE when the month right before the water-year boundary *and*
the boundary month itself are both among that site's three wettest months -- i.e. the cut falls
inside a still-wet stretch. Seven kept sites are flagged this way (all Northern Hemisphere, same
set as v1): `US-Fwf`, `US-Seg`, `US-Jo2`, `US-Ses`, `US-Wjs`, `US-Mpj`, `ES-Agu`. For these, treat
water-year totals with a little more caution, or cross-check against the calendar-year columns.

## Citing this data

**Every site's data must be cited by its own persistent identifier (PID/DOI)**, not just a
generic FLUXNET citation. `sites.csv` has `product_id` (the DOI) and `product_citation` (the
full citation string) for every kept site. Please include all 54 citations (or a link to the
full list) in your poster's references, per the FLUXNET data use policy. Also cite Pastorello et
al. (2020), *Scientific Data* 7:225, for the underlying ONEFlux processing pipeline.

## Questions

This package only extracts and organizes data -- we haven't interpreted the GPP-precipitation
relationship itself. If anything here looks off, or you want a different screen/threshold, let
us know and we can rebuild the package rather than you working around it.
