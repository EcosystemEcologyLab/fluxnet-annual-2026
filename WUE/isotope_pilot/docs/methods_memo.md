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

## Standing rule 2 — this work ends at the pre-analysis report

`code/04_report_preanalysis.R` computes availability, inventory, closure, and
cross-check numbers only. It does not compute flux-derived WUE, tree-ring
intrinsic WUE, "underlying" WUE, or any VPD exponent, and it does not apply a
rain-day screen or a GPP-magnitude screen. Those steps are scoped for a later
session, after PI review of this report's tables and
`docs/report_back_<date>.md`.

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
