# TowerTango — Update Cards report

Run: 2026-10-13 06:53:12 MST | Snapshot used: `fluxnet_shuttle_snapshot_20260920T102211.csv` | Sites: 781

## Coverage (non-blank count per card column)

| data_hub | card_site_name | card_regional_network | card_site_id | card_year_established | card_sink_strength_nep_gC_m2_yr | card_nep_n_years | card_et_mm_yr | card_et_mm_d | card_et_n_years | card_tower_height_m | card_tower_height_date | card_tower_height_variable | card_instrument_model | card_tower_height_source | card_canopy_height_max_m | card_canopy_height_date | card_canopy_height_n_records | card_map_mm_yr | card_mat_degC | card_site_description | card_site_description_words |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| ALL | 781 | 771 | 781 | 781 | 656 | 781 | 665 | 665 | 781 | 777 | 777 | 777 | 777 | 777 | 459 | 416 | 465 | 781 | 781 | 624 | 624 |
| AmeriFlux | 381 | 381 | 381 | 381 | 303 | 381 | 309 | 309 | 381 | 381 | 381 | 381 | 381 | 381 |  83 |  76 |  83 | 381 | 381 | 372 | 372 |
| ICOS | 348 | 338 | 348 | 348 | 305 | 348 | 308 | 308 | 348 | 348 | 348 | 348 | 348 | 348 | 331 | 305 | 331 | 348 | 348 | 252 | 252 |
| TERN |  52 |  52 |  52 |  52 |  48 |  52 |  48 |  48 |  52 |  48 |  48 |  48 |  48 |  48 |  45 |  35 |  51 |  52 |  52 |   0 |   0 |

TERN's `card_site_description` coverage is genuinely 0/52, not a join bug: TERN's BIF
export carries no `SITE_DESC`/`GRP_SITE_DESC` group at all (confirmed directly against a
TERN site's raw BIF file, e.g. AU-GWW: `VARIABLE_GROUP` values present are GRP_ONEFLUX,
HEADER, HEIGHTC, IGBP, LOCATION, NETWORK, TEAM_MEMBER, UTC_OFFSET, GRP_ERA_DOWN,
GRP_UST_THR, GRP_FLUX_REF -- no description group among them).

## SITE_DESC word counts

Minimum: 1 | Q1: 27.0 | Median: 54.5 | Q3: 100.0 | Maximum: 484

Sites under 50 words: 290 | 50-150 words: 280 | over 150 words: 54

Sites with several SITE_DESC entries: 1

- DE-Rns: 2 entries

All of the above are byte-identical duplicate text repeated under different `GROUP_ID`s
within the same BIF export -- not dated versions and not separate pieces of description
(confirmed by direct comparison of the DATAVALUE text for each GROUP_ID). Joined with
" | " per instructions regardless, so the duplicate text appears twice in
`card_site_description` for these sites.

## Tower height (VAR_INFO_HEIGHT) coverage and spot-check

777 of 781 sites have a `VAR_INFO_HEIGHT` for NEE_VUT_REF, NEE_CUT_REF or USTAR in their
YY-resolution BIFVARINFO file (coverage by data_hub is in the main coverage table above).
88 site(s) have more than one distinct height value across the dated VAR_INFO groups
of whichever variable was used for them (i.e. the EC instrument height changed over the
site's record) -- `card_tower_height_m` always takes the latest-dated value.

| Site | Hand height (m) | Hand variable | Hand date | Card height (m) | Card variable | Card date |
|---|---|---|---|---|---|---|
| CA-TP1 | 10 | NEE_VUT_REF | 201408 | 10 | NEE_VUT_REF | 201408 |
| US-Ha1 | 29 | NEE_VUT_REF | 199110280100 | 29 | NEE_VUT_REF | 199110280100 |
| DE-Tha | 42 | NEE_VUT_REF | 202101010000 | 42 | NEE_VUT_REF | 202101010000 |

Hand values are re-derived directly from `var_info_groups` (NEE_VUT_REF, else NEE_CUT_REF,
else USTAR, latest `VAR_INFO_DATE`), independently of the `tower_height` build code above --
all three match the card value exactly.

## Canopy height (HEIGHTC) coverage, by data_hub and IGBP class

HEIGHTC_STATISTIC filter: 7057 record(s) across 93 site(s) removed for having a
statistic other than Mean, Median or Expert estimate (or absent) -- breakdown by statistic:

| HEIGHTC_STATISTIC | records removed |
|---|---|
| Standard Deviation | 1583 |
| 90th Percentile | 1236 |
| Maximum | 1079 |
| Minimum | 1044 |
| Single observation | 679 |
| 25th Percentile | 655 |
| 75th Percentile | 646 |
| 5th Percentile | 47 |
| 95th Percentile | 39 |
| 50th Percentile | 32 |
| 10th Percentile | 6 |
| 99th Percentile | 6 |
| 1st Percentile | 5 |

465 of 781 sites have at least one HEIGHTC record (459 with a usable numeric maximum --
excludes sites whose only record is the non-numeric placeholder 'variable'). The 2026-10-08
BADM vegetation-structure search reported 482 sites (344 ICOS + 86 AmeriFlux + 51 TERN); the
any-record count below is close to but not identical to that figure -- expected, since that
search read every extracted BIF file under data/extracted/ directly (naive glob, no one-file-
per-site resolution) and documented its own site-list anomalies (e.g. a casing mismatch,
NZ-CLa vs. the snapshot's NZ-Cla, and 15 blank-SITE_ID rows), whereas this run resolves
exactly one BIF file per site via `fluxnet::flux_discover_files()` (see README.md) -- a
difference of a site or two either way is attributable to that, not a new error.

By data_hub:

| data_hub | sites | any HEIGHTC record | numeric maximum |
|---|---|---|---|
| AmeriFlux | 381 |  83 |  83 |
| ICOS | 348 | 331 | 331 |
| TERN |  52 |  51 |  45 |

By IGBP class:

| igbp | sites | any HEIGHTC record | numeric maximum |
|---|---|---|---|
| GRA | 146 | 107 | 102 |
| ENF | 114 |  77 |  77 |
| CRO | 139 |  60 |  60 |
| WET | 117 |  51 |  51 |
| DBF |  81 |  46 |  46 |
| EBF |  44 |  38 |  38 |
| OSH |  41 |  23 |  23 |
| MF |  24 |  17 |  17 |
| DNF |  13 |  12 |  12 |
| WSA |  18 |  12 |  11 |
| SAV |  14 |  11 |  11 |
| CSH |  12 |   5 |   5 |
| BSV |   7 |   3 |   3 |
| CVM |   9 |   2 |   2 |
| SNO |   2 |   1 |   1 |

## Spot-check: 3 sites from different hubs, hand-recomputed from source

| Hub | Site | Hand NEP (gC/m2/yr) | Card NEP | Hand ET (mm/yr) | Card ET | Hand tower height (m) | Card tower height | Hand canopy height max (m) | Card canopy height max | Hand canopy n | Card canopy n |
|---|---|---|---|---|---|---|---|---|---|---|---|
| AmeriFlux | CA-Ca3 | 455.1 | 455.1 | 381.3 | 381.3 | 21 | 21 | 7.57 | 7.57 |   1 |   1 |
| ICOS | DE-Tha | 470.0 | 470.0 | 395.9 | 395.9 | 42 | 42 | 33.93 | 33.93 | 134 | 134 |
| TERN | AU-GWW | 170.0 | 170.0 | 240.7 | 240.7 | 36 | 36 | 18.00 | 18.00 |   1 |   1 |

Hand computation re-derives NEP and ET directly from the `annual` table's raw NEE_VUT_REF/
NEE_CUT_REF/LE_F_MDS columns with the same VUT-with-CUT-fallback and
`(1 - QC) <= QC_THRESHOLD_YY` gate as `compute_site_annual_fluxes()`, independently of that
function's own code path. Tower height is re-read directly from the site's `VAR_INFO_HEIGHT`
groups (see the dedicated tower-height spot-check above for CA-TP1/US-Ha1/DE-Tha). Canopy
height max/n are re-read directly from the site's raw HEIGHTC rows in the BIF file
(GRP_HEIGHTC/HEIGHTC, plus JP-Shn's own non-standard group), with the same HEIGHTC_STATISTIC
keep-list applied again here, independently of the card_canopy_height_* build code above.

## Out-of-range values (listed, not dropped)

| site_id | data_hub | check | value |
|---|---|---|---|
| US-Bi2 | AmeriFlux | NEP beyond +/-1500 gC/m2/yr | -1677.8 |
| CN-Sb1 | ICOS | NEP beyond +/-1500 gC/m2/yr | 1829.8 |
| IT-Lav | ICOS | NEP beyond +/-1500 gC/m2/yr | 1962.1 |
| CH-Rh2 | ICOS | Canopy height above 60 m | 226.3 |
| CH-Rh1 | ICOS | Canopy height above 60 m | 209.9 |
| CH-Oe3 | ICOS | Canopy height above 60 m | 96.5 |
| AU-Wac | TERN | Canopy height above 60 m | 100.0 |
| FI-Nsv | ICOS | Canopy height below 0.05 m | 0.0 |
| DE-Zrk | ICOS | Canopy height below 0.05 m | 0.0 |

