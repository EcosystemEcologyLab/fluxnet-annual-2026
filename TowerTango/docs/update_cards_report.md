# TowerTango — Update Cards report

Run: 2026-10-13 08:11:08 MST | Snapshot used: `fluxnet_shuttle_snapshot_20260920T102211.csv` | Sites: 781

## Coverage (non-blank count per card column)

| data_hub | card_site_name | card_regional_network | card_site_id | card_year_established | card_sink_strength_nep_gC_m2_yr | card_nep_n_years | card_et_mm_yr | card_et_mm_d | card_et_n_years | card_tower_height_m | card_tower_height_date | card_tower_height_variable | card_instrument_model | card_tower_height_source | card_canopy_height_max_m | card_canopy_height_date | card_canopy_height_n_records | card_height_check | card_map_mm_yr | card_mat_degC | card_site_description | card_site_description_words |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| ALL | 781 | 771 | 781 | 781 | 656 | 781 | 665 | 665 | 781 | 777 | 777 | 777 | 777 | 777 | 477 | 427 | 483 | 102 | 781 | 781 | 624 | 624 |
| AmeriFlux | 381 | 381 | 381 | 381 | 303 | 381 | 309 | 309 | 381 | 381 | 381 | 381 | 381 | 381 |  86 |  79 |  86 |  41 | 381 | 381 | 372 | 372 |
| ICOS | 348 | 338 | 348 | 348 | 305 | 348 | 308 | 308 | 348 | 348 | 348 | 348 | 348 | 348 | 346 | 313 | 346 |  58 | 348 | 348 | 252 | 252 |
| TERN |  52 |  52 |  52 |  52 |  48 |  52 |  48 |  48 |  52 |  48 |  48 |  48 |  48 |  48 |  45 |  35 |  51 |   3 |  52 |  52 |   0 |   0 |

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

No HEIGHTC_STATISTIC filter is applied -- every dated HEIGHTC record is used exactly as
recorded in BADM, including dispersion/percentile/extremum statistics. (A filter was added,
then removed again, both on 2026-10-13 -- see SESSION_LOG.md.) Unusual-looking results are
instead surfaced in "Height flags" below, not excluded here.

483 of 781 sites have at least one HEIGHTC record (477 with a usable numeric maximum --
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
| AmeriFlux | 381 |  86 |  86 |
| ICOS | 348 | 346 | 346 |
| TERN |  52 |  51 |  45 |

By IGBP class:

| igbp | sites | any HEIGHTC record | numeric maximum |
|---|---|---|---|
| GRA | 146 | 109 | 104 |
| ENF | 114 |  78 |  78 |
| CRO | 139 |  72 |  72 |
| WET | 117 |  51 |  51 |
| DBF |  81 |  47 |  47 |
| EBF |  44 |  39 |  39 |
| OSH |  41 |  24 |  24 |
| MF |  24 |  17 |  17 |
| DNF |  13 |  12 |  12 |
| WSA |  18 |  12 |  11 |
| SAV |  14 |  11 |  11 |
| CSH |  12 |   5 |   5 |
| BSV |   7 |   3 |   3 |
| CVM |   9 |   2 |   2 |
| SNO |   2 |   1 |   1 |

## Height flags (card_height_check)

Both height columns are used exactly as reported in BADM -- nothing is excluded or altered.
This column instead flags sites whose recorded height looks unusual, for a by-hand check
against the source BADM record before use; blank when nothing is flagged.

102 of 781 sites flagged for at least one check.

| Flag | Sites |
|---|---|
| canopy height above 60 m | 4 |
| canopy height of 0 m | 2 |
| canopy height greater than tower height | 26 |
| instrument height changed over the record | 88 |

| site_id | data_hub | card_tower_height_m | card_canopy_height_max_m | card_height_check |
|---|---|---|---|---|
| AR-CCa | AmeriFlux | 4 | NA | instrument height changed over the record (2-4 m) |
| AU-Lon | TERN | 4 | 0.50 | instrument height changed over the record (2-4 m) |
| AU-Wac | TERN | 95 | 100.00 | canopy height above 60 m; canopy height greater than tower height |
| AU-Wom | TERN | 33 | 23.00 | instrument height changed over the record (30-35 m) |
| BE-Dor | ICOS | 2 | 0.30 | instrument height changed over the record (2-2 m) |
| BE-Lcr | ICOS | 7 | 5.49 | instrument height changed over the record (7-11 m) |
| BE-Lon | ICOS | 2 | 1.04 | instrument height changed over the record (2-3 m) |
| BE-Maa | ICOS | 3 | 1.21 | instrument height changed over the record (3-3 m) |
| BE-Vie | ICOS | 51 | 45.37 | instrument height changed over the record (40-52 m) |
| BR-ITA | AmeriFlux | 8 | 3.67 | instrument height changed over the record (6-8 m) |
| BR-Sa1 | AmeriFlux | 58 | 57.85 | canopy height greater than tower height |
| CA-Na1 | AmeriFlux | 20 | NA | instrument height changed over the record (20-20 m) |
| CA-SCC | AmeriFlux | 15 | NA | instrument height changed over the record (15-15 m) |
| CA-TP1 | AmeriFlux | 10 | 6.50 | instrument height changed over the record (4-10 m) |
| CA-TP3 | AmeriFlux | 20 | 16.20 | instrument height changed over the record (16-20 m) |
| CA-TP4 | AmeriFlux | 34 | 23.40 | instrument height changed over the record (28-34 m) |
| CH-Oe3 | ICOS | 1 | 96.55 | canopy height above 60 m; canopy height greater than tower height |
| CH-Rh1 | ICOS | 2 | 209.91 | canopy height above 60 m; canopy height greater than tower height |
| CH-Rh2 | ICOS | 2 | 226.30 | canopy height above 60 m; canopy height greater than tower height |
| CN-BeO | ICOS | 15 | 11.50 | instrument height changed over the record (12-15 m) |
| CZ-BK1 | ICOS | 25 | 18.90 | instrument height changed over the record (13-25 m) |
| CZ-KrP | ICOS | 4 | 3.00 | instrument height changed over the record (3-4 m) |
| CZ-Lnz | ICOS | 44 | 39.01 | instrument height changed over the record (44-48 m) |
| CZ-wet | ICOS | 2 | 1.70 | instrument height changed over the record (2-3 m) |
| DE-Hai | ICOS | 44 | 40.60 | instrument height changed over the record (44-44 m) |
| DE-HoH | ICOS | 45 | 37.10 | instrument height changed over the record (45-49 m) |
| DE-Hzd | ICOS | 17 | 12.00 | instrument height changed over the record (5-17 m) |
| DE-Msr | ICOS | 10 | 3.00 | instrument height changed over the record (7-10 m) |
| DE-Rns | ICOS | 5 | 1.00 | instrument height changed over the record (5-7 m) |
| DE-RuC | ICOS | 7 | 2.80 | instrument height changed over the record (3-7 m) |
| DE-RuM | ICOS | 2 | 1.75 | instrument height changed over the record (2-2 m) |
| DE-RuS | ICOS | 2 | 1.06 | instrument height changed over the record (2-3 m) |
| DE-Zrk | ICOS | 3 | 0.00 | canopy height of 0 m |
| DK-Fou | ICOS | 2 | 2.60 | canopy height greater than tower height; instrument height changed over the record (2-4 m) |
| ES-Agu | ICOS | 4 | 0.30 | instrument height changed over the record (4-4 m) |
| ES-Mcd | ICOS | 5 | 3.00 | instrument height changed over the record (3-5 m) |
| FI-Hyy | ICOS | 27 | 26.10 | instrument height changed over the record (24-46 m) |
| FI-Let | ICOS | 27 | 19.60 | instrument height changed over the record (26-27 m) |
| FI-Nsv | ICOS | 3 | 0.00 | canopy height of 0 m |
| FR-Aur | ICOS | 3 | 1.64 | instrument height changed over the record (3-4 m) |
| FR-CLt | ICOS | 3 | 0.30 | instrument height changed over the record (3-5 m) |
| FR-EM2 | ICOS | 2 | 2.15 | canopy height greater than tower height; instrument height changed over the record (2-2 m) |
| FR-FBn | ICOS | 17 | 14.94 | instrument height changed over the record (17-17 m) |
| FR-Gri | ICOS | 2 | 2.33 | canopy height greater than tower height; instrument height changed over the record (2-3 m) |
| FR-Hes | ICOS | 31 | 31.24 | canopy height greater than tower height; instrument height changed over the record (18-31 m) |
| FR-Lam | ICOS | 3 | 3.11 | canopy height greater than tower height; instrument height changed over the record (3-3 m) |
| FR-Mej | ICOS | 2 | 2.60 | canopy height greater than tower height |
| GL-NuF | ICOS | 2 | 0.11 | instrument height changed over the record (2-2 m) |
| ID-PaB | ICOS | 14 | 0.80 | instrument height changed over the record (3-14 m) |
| IE-Gwr | ICOS | 3 | 1.38 | instrument height changed over the record (3-4 m) |
| IT-BCi | ICOS | 2 | 3.27 | canopy height greater than tower height; instrument height changed over the record (1-2 m) |
| IT-CA3 | ICOS | 6 | 7.50 | canopy height greater than tower height |
| IT-Cp2 | ICOS | 21 | 21.96 | canopy height greater than tower height; instrument height changed over the record (20-21 m) |
| IT-Lav | ICOS | 36 | 38.71 | canopy height greater than tower height |
| IT-MBo | ICOS | 2 | 0.55 | instrument height changed over the record (2-2 m) |
| IT-Niv | ICOS | 4 | 0.10 | instrument height changed over the record (4-4 m) |
| IT-PT1 | ICOS | 33 | 24.00 | instrument height changed over the record (28-33 m) |
| IT-Ren | ICOS | 34 | 31.00 | instrument height changed over the record (34-40 m) |
| IT-SRo | ICOS | 22 | 18.70 | instrument height changed over the record (22-22 m) |
| IT-Tor | ICOS | 3 | 0.16 | instrument height changed over the record (2-3 m) |
| JP-Mse | ICOS | 4 | 1.14 | instrument height changed over the record (3-4 m) |
| JP-Tef | ICOS | 12 | 18.00 | canopy height greater than tower height; instrument height changed over the record (5-32 m) |
| JP-Tmd | ICOS | 12 | 3.00 | instrument height changed over the record (3-12 m) |
| NL-Loo | ICOS | 38 | 19.59 | instrument height changed over the record (26-38 m) |
| SE-Nor | ICOS | 12 | 32.08 | canopy height greater than tower height; instrument height changed over the record (12-36 m) |
| SE-Ros | ICOS | 24 | 19.00 | instrument height changed over the record (20-24 m) |
| SE-Svb | ICOS | 34 | 25.97 | instrument height changed over the record (32-34 m) |
| UK-AMo | ICOS | 4 | 0.82 | instrument height changed over the record (4-4 m) |
| US-Aud | AmeriFlux | 4 | 15.00 | canopy height greater than tower height |
| US-Cwt | AmeriFlux | 44 | NA | instrument height changed over the record (37-44 m) |
| US-EA4 | AmeriFlux | 12 | NA | instrument height changed over the record (11-12 m) |
| US-Fo1 | AmeriFlux | 4 | NA | instrument height changed over the record (3-4 m) |
| US-GLE | AmeriFlux | 23 | 27.70 | canopy height greater than tower height |
| US-HB1 | AmeriFlux | 4 | NA | instrument height changed over the record (4-4 m) |
| US-HWB | AmeriFlux | 5 | NA | instrument height changed over the record (3-5 m) |
| US-Ho2 | AmeriFlux | 28 | 20.00 | instrument height changed over the record (28-28 m) |
| US-MN1 | AmeriFlux | 2 | NA | instrument height changed over the record (2-2 m) |
| US-MN2 | AmeriFlux | 2 | NA | instrument height changed over the record (2-2 m) |
| US-MN3 | AmeriFlux | 2 | NA | instrument height changed over the record (2-7 m) |
| US-Mo1 | AmeriFlux | 2 | NA | instrument height changed over the record (2-2 m) |
| US-Mo3 | AmeriFlux | 2 | NA | instrument height changed over the record (2-2 m) |
| US-Myb | AmeriFlux | 5 | 48.00 | canopy height greater than tower height; instrument height changed over the record (3-6 m) |
| US-NC1 | AmeriFlux | 10 | 0.35 | instrument height changed over the record (6-10 m) |
| US-NC2 | AmeriFlux | 12 | 14.00 | canopy height greater than tower height; instrument height changed over the record (12-27 m) |
| US-NC3 | AmeriFlux | 9 | NA | instrument height changed over the record (4-9 m) |
| US-NC4 | AmeriFlux | 33 | NA | instrument height changed over the record (30-33 m) |
| US-Ne1 | AmeriFlux | 3 | 3.51 | canopy height greater than tower height; instrument height changed over the record (3-3 m) |
| US-Ne2 | AmeriFlux | 3 | 3.43 | canopy height greater than tower height; instrument height changed over the record (3-3 m) |
| US-Ne3 | AmeriFlux | 3 | 3.05 | canopy height greater than tower height; instrument height changed over the record (3-3 m) |
| US-OWC | AmeriFlux | 5 | NA | instrument height changed over the record (3-5 m) |
| US-Oho | AmeriFlux | 34 | 24.50 | instrument height changed over the record (32-34 m) |
| US-Rms | AmeriFlux | 2 | NA | instrument height changed over the record (2-2 m) |
| US-Ro1 | AmeriFlux | 2 | NA | instrument height changed over the record (2-2 m) |
| US-Ro2 | AmeriFlux | 2 | NA | instrument height changed over the record (2-3 m) |
| US-Ro5 | AmeriFlux | 2 | NA | instrument height changed over the record (2-3 m) |
| US-Rpf | AmeriFlux | 6 | NA | instrument height changed over the record (3-6 m) |
| US-Tw1 | AmeriFlux | 5 | 29.00 | canopy height greater than tower height |
| US-UTW | AmeriFlux | 3 | NA | instrument height changed over the record (3-3 m) |
| US-Vcp | AmeriFlux | 28 | NA | instrument height changed over the record (24-28 m) |
| US-YK1 | AmeriFlux | 3 | NA | instrument height changed over the record (2-3 m) |
| US-YK2 | AmeriFlux | 3 | NA | instrument height changed over the record (2-3 m) |
| ZA-Spk | ICOS | 6 | 8.00 | canopy height greater than tower height |

## Spot-check: 3 sites from different hubs, hand-recomputed from source

| Hub | Site | Hand NEP (gC/m2/yr) | Card NEP | Hand ET (mm/yr) | Card ET | Hand tower height (m) | Card tower height | Hand canopy height max (m) | Card canopy height max | Hand canopy n | Card canopy n |
|---|---|---|---|---|---|---|---|---|---|---|---|
| AmeriFlux | CA-Ca3 | 455.1 | 455.1 | 381.3 | 381.3 | 21 | 21 | 7.57 | 7.57 |   1 |   1 |
| ICOS | DE-Tha | 470.0 | 470.0 | 395.9 | 395.9 | 42 | 42 | 37.50 | 37.50 | 513 | 513 |
| TERN | AU-GWW | 170.0 | 170.0 | 240.7 | 240.7 | 36 | 36 | 18.00 | 18.00 |   1 |   1 |

Hand computation re-derives NEP and ET directly from the `annual` table's raw NEE_VUT_REF/
NEE_CUT_REF/LE_F_MDS columns with the same VUT-with-CUT-fallback and
`(1 - QC) <= QC_THRESHOLD_YY` gate as `compute_site_annual_fluxes()`, independently of that
function's own code path. Tower height is re-read directly from the site's `VAR_INFO_HEIGHT`
groups (see the dedicated tower-height spot-check above for CA-TP1/US-Ha1/DE-Tha). Canopy
height max/n are re-read directly from the site's raw HEIGHTC rows in the BIF file
(GRP_HEIGHTC/HEIGHTC, plus JP-Shn's own non-standard group), no statistic filter, every
dated record counted, independently of the card_canopy_height_* build code above.

## Out-of-range values (listed, not dropped)

| site_id | data_hub | check | value |
|---|---|---|---|
| US-Bi2 | AmeriFlux | NEP beyond +/-1500 gC/m2/yr | -1677.8 |
| CN-Sb1 | ICOS | NEP beyond +/-1500 gC/m2/yr | 1829.8 |
| IT-Lav | ICOS | NEP beyond +/-1500 gC/m2/yr | 1962.1 |

