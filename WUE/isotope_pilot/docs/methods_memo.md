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

## Stage 2 — site list: CH-Dav dropped

PI decision, 2026-10-07: `CH-Dav` is dropped from stage 2. Reasons from the
stage 1 report (`docs/report_back_20261007.md`): its energy-balance closure
slope was 0.46 (r2 = 0.56), the weakest of the 13 sites, and (per the
variable-availability-by-year table) it has three years with no nighttime
GPP at all. Its downloaded/extracted files are left on disk; `code/00_config.R`
defines `WUE_SITES_STAGE2` (12 sites) by excluding `CH-Dav` from whichever
site list `WUE_SITES`/`WUE_SITE_SUBSET` already resolves to, so every stage 2
script simply never reads it.

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

Daily PET (for the rain screen in `code/07_apply_screens.R`) uses
Priestley-Taylor with alpha = 1.26, soil heat flux G = 0, from daily mean
net radiation (the NETRAD gap-fill series below) and daily mean air
temperature. The user's instructions name only these two inputs (plus
G = 0) — no atmospheric pressure — so the psychrometric constant here uses
a **fixed standard sea-level pressure (101.3 kPa)**, not a site-specific,
elevation-adjusted `PA_F`. The saturation-vapor-pressure slope (Delta) and
psychrometric constant (gamma) formulas are the standard FAO-56 (Allen et
al. 1998) forms; Zhou et al. (2015) section 2.1 does not give the exact
formula either, so this is this analysis's own reading, not a reproduction
of a cited equation — same caveat as the k* method below.

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

## Stage 2 — Zhou et al. (2015) screens

Applied in `code/07_apply_screens.R`, in order:

- **a. Rain** — exclude every day with P_ERA (midnight-to-midnight sum) > 0
  (no threshold, per instruction). Also exclude the two following days when
  P > 2×PET, or the one following day when P > PET. Propagated across each
  site's full continuous date range, not reset at calendar-year boundaries.
- **b. Quality** — keep records where the chosen-product NEE QC flag,
  `LE_F_MDS_QC`, and `VPD_F_QC` are each 0 or 1 (NA fails).
- **c. Daylight** — keep records with `TIMESTAMP_START` local-standard-time
  hour-of-day in [05:00, 21:00]; exclude records with negative (gap-filled)
  net radiation, GPP, ET, or VPD.
- **d. Day level** — a day is valid only if it has >=24 surviving records
  (HH sites) or >=12 (HR sites: `US-Ha1`, `US-MMS`), AND its mean GPP over
  those records is >=10% of the maximum such mean among that site-year's
  day candidates that already passed the record-count test.

`tables/screen_attrition.csv` records, per site-year, how many days are
removed at each stage.

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
