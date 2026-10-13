# TowerTango — Update Cards report

Run: 2026-10-12 17:04:47 MST | Snapshot used: `fluxnet_shuttle_snapshot_20260920T102211.csv` | Sites: 781

## Coverage (non-blank count per card column)

| data_hub | card_site_name | card_regional_network | card_site_id | card_year_established | card_sink_strength_nep_gC_m2_yr | card_nep_n_years | card_et_mm_yr | card_et_mm_d | card_et_n_years | card_tower_height_m | card_tower_height_source | card_canopy_height_max_m | card_canopy_height_date | card_canopy_height_n_records | card_map_mm_yr | card_mat_degC | card_site_description | card_site_description_words |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| ALL | 781 | 771 | 781 | 781 | 656 | 781 | 665 | 665 | 781 | 2 | 2 | 477 | 427 | 483 | 781 | 781 | 624 | 624 |
| AmeriFlux | 381 | 381 | 381 | 381 | 303 | 381 | 309 | 309 | 381 | 2 | 2 |  86 |  79 |  86 | 381 | 381 | 372 | 372 |
| ICOS | 348 | 338 | 348 | 348 | 305 | 348 | 308 | 308 | 348 | 0 | 0 | 346 | 313 | 346 | 348 | 348 | 252 | 252 |
| TERN |  52 |  52 |  52 |  52 |  48 |  52 |  48 |  48 |  52 | 0 | 0 |  45 |  35 |  51 |  52 |  52 |   0 |   0 |

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

## Canopy height (HEIGHTC) coverage, by data_hub and IGBP class

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

## Spot-check: 3 sites from different hubs, hand-recomputed from source

| Hub | Site | Hand NEP (gC/m2/yr) | Card NEP | Hand ET (mm/yr) | Card ET | Hand tower height (m) | Card tower height | Hand canopy height max (m) | Card canopy height max | Hand canopy n | Card canopy n |
|---|---|---|---|---|---|---|---|---|---|---|---|
| AmeriFlux | CA-Ca3 | 455.1 | 455.1 | 381.3 | 381.3 | 31 | 31 | 7.57 | 7.57 |   1 |   1 |
| ICOS | DE-Tha | 470.0 | 470.0 | 395.9 | 395.9 | NA | NA | 37.50 | 37.50 | 513 | 513 |
| TERN | AU-GWW | 170.0 | 170.0 | 240.7 | 240.7 | NA | NA | 18.00 | 18.00 |   1 |   1 |

Hand computation re-derives NEP and ET directly from the `annual` table's raw NEE_VUT_REF/
NEE_CUT_REF/LE_F_MDS columns with the same VUT-with-CUT-fallback and
`(1 - QC) <= QC_THRESHOLD_YY` gate as `compute_site_annual_fluxes()`, independently of that
function's own code path. Tower height is re-read directly from the site's BADM
`FLUX_MEASUREMENTS_COMMENT` text. Canopy height max/n are re-read directly from the site's raw
HEIGHTC rows in the BIF file (GRP_HEIGHTC/HEIGHTC, plus JP-Shn's own non-standard group),
independently of the card_canopy_height_* build code above.

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

