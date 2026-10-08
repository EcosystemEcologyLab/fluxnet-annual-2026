# WUE isotope pilot — methods memo

Side analysis, not the FLUXNET Annual Paper 2026. Requested by David Moore, 2026-10-07.

## Standing rule 1 — GPP is the nighttime partition only

FLUXNET distributes two GPP partitioning methods: nighttime (`GPP_NT_*`,
`RECO_NT_*`) and daytime (`GPP_DT_*`, `RECO_DT_*`). The daytime method fits a
VPD-dependent light-response curve to estimate daytime respiration and, by
difference, GPP — i.e. VPD is an input to the daytime partition. Any WUE
metric that weights or filters by VPD and is built on daytime-partitioned
GPP would therefore be circular: the denominator's own estimation already
depends on the variable used to interpret it.

This analysis therefore reads, summarises, plots, and compares **only**
`GPP_NT_VUT_REF` and `GPP_NT_CUT_REF` (and `RECO_NT_VUT_REF`). No
`GPP_DT_*`/`RECO_DT_*` column is read anywhere in this analysis, including as
a sensitivity check, now or in any later step that builds on this report.

Enforcement:
- `code/02_read_subdaily.R`'s column whitelist (`KEEP_COLS`) simply never
  lists a `_DT_` column, so it can never be read into the per-site RDS files
  this analysis's reports and any future analysis step are built from.
- `code/00_config.R` defines `WUE_FORBIDDEN_DT_PATTERN <- "_DT_"` and both
  `02_read_subdaily.R` and `04_report_preanalysis.R` assert
  (`stopifnot`) that none of their working column sets match it, as a
  defensive check against a future accidental edit widening the whitelist.

## Standing rule 2 — revised for stage 2 (2026-10-07)

Stage 1 (`code/04_report_preanalysis.R`) computed availability, inventory,
closure, and cross-check numbers only — no WUE, no screens. Stage 2
(`code/05`-`10`, below) applies the Zhou et al. (2015) screens and computes
WUE, inherent WUE (IWUE), underlying WUE (uWUE), and the VPD exponent k*.
**It still does not interpret them**: no trend tests, no site rankings, no
statements about what the series mean. That remains scoped for a later
session, after PI review of `tables/wue_annual.csv`,
`tables/wue_daily.csv.gz`, and `docs/report_back_stage2_<date>.md`.

Tree-ring data remain out of scope for stage 2 — `code/03_fetch_treering.R`
and `code/treering_report_helpers.R` are left in place, not run, not edited.

## Stage 2 — site list: CH-Dav and NL-Loo dropped

PI decision, 2026-10-07: `CH-Dav` is dropped from stage 2. Reasons from the
stage 1 report (`docs/report_back_20261007.md`): its energy-balance closure
slope was 0.46 (r2 = 0.56), the weakest of the 13 sites, and (per the
variable-availability-by-year table) it has three years with no nighttime
GPP at all. Its downloaded/extracted files are left on disk; `code/00_config.R`
defines `WUE_SITES_STAGE2` by excluding `CH-Dav` from whichever site list
`WUE_SITES`/`WUE_SITE_SUBSET` already resolves to, so every stage 2 script
simply never reads it.

PI decision, 2026-10-07 (attempting the 12-site full run): `NL-Loo` is also
dropped, the only one of the 12 stage-2 sites to fail
`06_build_site_years.R`'s P_ERA integrity check (mean annual sub-daily
`P_ERA` sum vs. `review/diagnostics/precip_site_filter/table_1_site_level_precip_estimates.csv`'s
`p_era_mean_mm_tower_years`, required within 2%): 1035.67 mm/yr here vs.
1068.62 mm/yr reference, -3.08%, a hard `stop()` per that check's design.
(The next-closest site, `FI-Hyy`, sits exactly at the -2.00% boundary and
passes; `DE-Tha` and `BE-Vie` are at -1.89% and -1.14%.) Added to
`WUE_SITES_STAGE2`'s exclusion list alongside `CH-Dav`; its files are
likewise left on disk, not investigated further.

## Stage 2 — product choice (VUT where available, CUT where not)

Per-site rule, identical to `R/site_annual_fluxes.R`'s
`.compute_site_annual_fluxes_core()` (read, not modified or called — that
function operates on annual DuckDB rows; stage 2 applies the same logic to
freshly-read sub-daily data): a site uses `NEE_VUT_REF`/`NEE_VUT_REF_QC` if
it has ANY non-NA `NEE_VUT_REF_QC` value anywhere in its sub-daily record,
else `NEE_CUT_REF`/`NEE_CUT_REF_QC` if it has any non-NA `NEE_CUT_REF_QC`,
else the site has no usable NEE product at all. This is a **per-site**
decision (never per site-year — a site's years are never a VUT/CUT mixture).
`GPP_NT_VUT_REF`/`GPP_NT_CUT_REF` and `RECO_NT_VUT_REF`/`RECO_NT_CUT_REF`
follow the same choice, so NEE, GPP, and the NEE quality flag used to gate
both always come from the same product. Implemented in
`code/06_build_site_years.R`; `tables/site_product.csv` records the choice
and the non-NA QC counts behind it. `US-Ho2` has no VUT data and runs on CUT
for the whole analysis.

## Stage 2 — units

GPP in g C m-2, ET in kg H2O m-2 (numerically identical to mm H2O — 1 kg
water over 1 m2 is 1 mm depth), VPD in hPa. GPP is converted from its native
HH/HR unit (µmol CO2 m-2 s-1) via `fluxnet_convert_units()` (`R/units.R`,
molar mass of C = 12 g/mol), the same function and formula the Annual Paper
uses. **VPD is deliberately NOT run through `fluxnet_convert_units()`** —
this pilot keeps VPD in its native hPa (matching the units Zhou et al. 2015
reports IWUE/uWUE in), not that function's kPa target unit. ET is computed
directly as `LE_F_MDS / lambda(TA_F)` rather than via
`fluxnet_convert_units()`'s fixed `lambda = 2.45e6 J/kg`: `lambda(TA_F) =
(2.501 - 0.002361 * TA_F) * 1e6` J/kg (FAO-56 / Allen et al. 1998), per the
user's explicit instruction that ET uses the temperature-dependent latent
heat of vaporisation. Both are deliberate, instructed departures from the
Annual Paper's default unit-conversion path, not omissions — documented here
per Standing Rule 3.

## Stage 2 — Priestley-Taylor PET

Daily PET (for the rain screen in `code/07_apply_screens.R`, shared via
`code/rain_rule.R`'s `pt_pet_mm_day()`) uses Priestley-Taylor with
alpha = 1.26, soil heat flux G = 0, from daily mean net radiation (the
NETRAD gap-fill series below) and daily mean air temperature. The
saturation-vapor-pressure slope (Delta) and psychrometric constant (gamma)
formulas are the standard FAO-56 (Allen et al. 1998) forms; Zhou et al.
(2015) section 2.1 does not give the exact formula, so this is this
analysis's own reading, not a reproduction of a cited equation — same
caveat as the k* method below.

**Pressure, revised 2026-10-08:** `pt_pet_mm_day()` takes an explicit
`pressure_kpa` argument for the psychrometric constant. Both
`07_apply_screens.R` and `code/11_precip_compare.R` pass the site's own
daily mean `PA_F` (native unit already kPa, no conversion needed) — not the
fixed standard sea-level pressure (101.3 kPa) the first stage 2 run used.
The fixed 101.3 kPa value remains the fallback, applied only where a day's
mean `PA_F` is `NA`. Both callers pass `PA_F` through the identical
`pt_pet_mm_day()` call shape so they can never drift apart on this point.

## Stage 2 — net radiation gap-fill

NETRAD is missing in a large share of records at several sites (per stage 1:
`NL-Loo` 11%, `US-MMS` 60%, `US-Ha1` 75%). Where missing, it is estimated
from that site's own `SW_IN_F` via a per-site OLS fit (`NETRAD ~ SW_IN_F`,
both present, any QC — a magnitude fit, not a QC-gated analysis). The filled
series (observed where present, fitted prediction where not) feeds both the
daily PET calculation and the negative-NETRAD daylight screen; every record
resting on the fitted value is flagged (`netrad_estimated`), carried to the
day level (`day_netrad_estimated`) and summarised per site-year as
`frac_estimated_netrad` in `tables/wue_annual.csv`. Each site's fit
(n, slope, intercept, r-squared) is in `tables/netrad_fits.csv`.

## Stage 2 — completeness screen (years dropped)

A site-year is dropped if fewer than 80% of its calendar-year sub-daily
timesteps have a non-NA nighttime GPP (the chosen product's `GPP_NT_*`), or
if the year is 2026. This is independent of the Zhou screens below — it is
a data-completeness gate over the whole year, decided before any day is
screened. `code/06_build_site_years.R`; `tables/years_dropped.csv`.

## Stage 2 — Zhou et al. (2015) screens (reconstructed, PI decision 2026-10-07)

Applied in `code/07_apply_screens.R` via `run_zhou_screens()` (`code/zhou_screens.R`),
in order. **This replaces an earlier rain-screen decision**, made the same day the
3-site smoke test was first run, after two side analyses examined it directly:
`docs/report_precip_compare_20261007.md` (ERA5 `P_ERA` vs. the tower gauge, daily
scale) and `docs/report_screen_variants_20261007.md` (rain source x screen c
radiation column x GPP day-test reference maximum, crossed on the 3 test sites).
The reconstructed screens are now `run_zhou_screens()`'s defaults, so
`07_apply_screens.R`'s call (every argument at its default) uses them without
change to the call site itself.

- **a. Rain** — exclude every day with `P_F` (midnight-to-midnight sum, **as
  distributed**: gauge-measured where available, `P_ERA`-filled where not) > 0
  (no threshold, MY DECISION 4 — unchanged). Also exclude the two following days
  when P > 2×PET, or the one following day when P > PET — unchanged. Propagated
  across each site's full continuous date range, not reset at calendar-year
  boundaries. **Changed from `P_ERA` alone**: `report_precip_compare_20261007.md`
  found `P_ERA` reports a day as wet far more often than the gauge does at every
  one of the 12 sites (`share_era_wet` 0.47–0.87 vs. `share_gauge_wet` 0.31–0.64,
  all months) despite annual `P_ERA`-to-gauge total ratios close to 1 everywhere —
  a daily-frequency disagreement, not a magnitude one. PET is unchanged: still
  Priestley-Taylor from daily mean `NETRAD_filled`, `TA_F`, and `PA_F`.
- **b. Quality** — keep records where the chosen-product NEE QC flag,
  `LE_F_MDS_QC`, and `VPD_F_QC` are each 0 or 1 (NA fails) — unchanged.
- **c. Daylight** — keep records with `TIMESTAMP_START` local-standard-time
  hour-of-day in [05:00, 21:00] (unchanged). Exclude records with negative
  **`SW_IN_F`** (NOT `NETRAD_filled`), GPP, ET, or VPD. **Changed from
  `NETRAD_filled`**: Zhou et al. (2015)'s own text specifies "net solar
  radiation" for this test, and their US-Goo example figure keeps days that
  cannot reach 24 half-hours of positive net radiation — i.e. a test that
  rarely excludes daylight-window records, unlike `NETRAD_filled` (full net
  radiation, routinely negative near dawn/dusk within the window even after
  gap-filling). `report_screen_variants_20261007.md`'s
  `records_in_window_by_month.csv` found the median in-window record count
  with `NETRAD_filled >= 0` was only ~17–25 of 33 possible half-hours at the
  3 test sites, vs. a full 33 for `SW_IN_F >= 0` — `NETRAD_filled` was
  discarding far more daylight-window records than Zhou's own method implies.
- **d. Day level** — a day is valid only if it has >=24 surviving records
  (HH sites) or >=12 (HR sites: `US-Ha1`, `US-MMS`) — unchanged — AND its mean
  GPP over those records is >=10% of the maximum **single-record** GPP over
  every record passing screens a-c that site-year (`gpp_test = "halfhour"`).
  **Changed from the daily-mean reference** (`gpp_test = "daymean"`: 10% of the
  largest daily mean among candidate days): Zhou et al. (2015)'s own wording
  specifies the single maximum half-hourly (or hourly) GPP value, not a daily
  mean. `report_screen_variants_20261007.md` found `"halfhour"` is mechanically
  always at least as restrictive as `"daymean"` (a site-year's max single
  record is always >= its max daily mean), with the difference concentrated in
  shoulder-season months and negligible at peak growing season.

The 80% nighttime-GPP completeness rule (`code/06_build_site_years.R`) is
unchanged by this decision.

`tables/screen_attrition.csv` records, per site-year, how many days are
removed at each stage, plus (added 2026-10-07) `share_days_gauge_measured` —
the share of days that year with `P_F_QC == 0` at every expected timestep,
independent of which column actually drives the rain screen.

## Stage 2 — k* (VPD exponent)

Per site-year, at the sub-daily scale (surviving records within valid days)
and the daily scale (the valid-day GPP_d/VPD_d/ET_d values): the exponent k
in a grid from 0 to 1.5 (step 0.01) that maximises the Pearson correlation
of `GPP * VPD^k` against `ET`. Zhou et al. (2015) cites Zhou et al. (2014)
for this method; this analysis does not have access to that paper, so the
grid-search implementation in `code/08_compute_metrics.R` is this analysis's
own reading of the method description in Zhou et al. (2015), not a
reproduction of Zhou et al. (2014)'s own code or exact procedure.

## Stage 2 — units check

Zhou et al. (2015)'s 123 site-years gave yearly uWUE 3.50-15.83 (mean 9.47)
g C hPa^0.5 kg H2O-1 and yearly IWUE 5.32-62.31 (mean 33.62) g C hPa
kg H2O-1. `code/08_compute_metrics.R` flags (warns, does not stop on) any
site-year whose computed uWUE_y/IWUE_y falls more than an order of magnitude
outside those ranges, as a unit-error sanity check.

## Standing rule 3 — report what is found

Sites or variables are never substituted, dropped, or added relative to what
was requested. Where a listed column is absent at a site, every table that
would have used it records that absence explicitly (a `note` field, or an
`NA` with surrounding context) rather than omitting the row.

## Site list and provenance

13 sites: 8 with published tree-ring isotope estimates (Guerrieri et al. 2019,
PNAS, EDI package `edi.401`; Belmecheri et al. 2021, GitHub) — `US-Ha1`,
`US-Ho2`, `US-MMS`, `US-SP1`, `US-Bar`, `US-Slt`, `US-Dk2`, `US-Fuf` — plus 5
flux-only sites for broader context — `DE-Tha`, `BE-Vie`, `NL-Loo`, `FI-Hyy`,
`CH-Dav`.

`US-Ho1` was checked against the live Shuttle manifest during preflight
(2026-10-07) and found absent; it was not added to the site list, per
instructions.

## Data source

FLUXNET Shuttle only (`flux_listall()` + `flux_download()`), same convention
as the Annual Paper and the WAFNET energy-partitioning side analysis. No
FLUXNET2015, LaThuile, AmeriFlux-FLUXNET, ICOS-FLUXNET, or other prior static
release is used as primary data for this pilot.

## Tree-ring site -> tower mapping

`code/treering_report_helpers.R` matches each `edi.401` site name against an
expected name fragment per tower (Harvard/`US-Ha1`, Howland/`US-Ho2`,
Bartlett/`US-Bar`, Morgan Monroe/`US-MMS`, Silas Little/`US-Slt`, Duke
hardwood/`US-Dk2`, Austin Cary/`US-SP1`, Flagstaff/`US-Fuf`), reading the
candidate name list from the downloaded EML metadata. `US-SP1` and `US-Fuf`
were prior inferences from the paper's author list, not stated matches;
`tables/treering_site_map.csv`'s `mapping_basis` column and
`docs/report_back_<date>.md` record whether the EML metadata confirms,
contradicts, or is silent on each mapping found during the actual fetch.

## Energy balance closure method

Same method as `WAFNET/energy_partitioning/code/03_report_variables_and_closure.R`:
OLS of `(LE_F_MDS + H_F_MDS) ~ (NETRAD - G_F_MDS)`, restricted to half-hours
measured (not gap-filled) at both `LE_F_MDS_QC == 0` and `H_F_MDS_QC == 0`.
Gap-filled half-hours are excluded because MDS gap-filling itself uses nearby
measured energy-balance terms, which would artificially inflate apparent
closure.
