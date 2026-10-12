# TowerTango — Update Cards report

Run: 2026-10-12 16:02:03 MST | Snapshot used: `fluxnet_shuttle_snapshot_20260920T102211.csv` | Sites: 781

## Coverage (non-blank count per card column)

| data_hub | card_site_name | card_regional_network | card_site_id | card_year_established | card_sink_strength_nep_gC_m2_yr | card_nep_n_years | card_et_mm_yr | card_et_mm_d | card_et_n_years | card_tower_height_m | card_tower_height_source | card_map_mm_yr | card_mat_degC | card_site_description | card_site_description_words |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| ALL | 781 | 771 | 781 | 781 | 656 | 781 | 665 | 665 | 781 | 2 | 2 | 781 | 781 | 624 | 624 |
| AmeriFlux | 381 | 381 | 381 | 381 | 303 | 381 | 309 | 309 | 381 | 2 | 2 | 381 | 381 | 372 | 372 |
| ICOS | 348 | 338 | 348 | 348 | 305 | 348 | 308 | 308 | 348 | 0 | 0 | 348 | 348 | 252 | 252 |
| TERN |  52 |  52 |  52 |  52 |  48 |  52 |  48 |  48 |  52 | 0 | 0 |  52 |  52 |   0 |   0 |

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

## Spot-check: 3 sites from different hubs, hand-recomputed from source

| Hub | Site | Hand NEP (gC/m2/yr) | Card NEP | Hand ET (mm/yr) | Card ET | Hand tower height (m) | Card tower height |
|---|---|---|---|---|---|---|---|
| AmeriFlux | CA-Ca3 | 455.1 | 455.1 | 381.3 | 381.3 | 31 | 31 |
| ICOS | DE-Tha | 470.0 | 470.0 | 395.9 | 395.9 | NA | NA |
| TERN | AU-GWW | 170.0 | 170.0 | 240.7 | 240.7 | NA | NA |

Hand computation re-derives NEP and ET directly from the `annual` table's raw NEE_VUT_REF/
NEE_CUT_REF/LE_F_MDS columns with the same VUT-with-CUT-fallback and
`(1 - QC) <= QC_THRESHOLD_YY` gate as `compute_site_annual_fluxes()`, independently of that
function's own code path. Tower height is re-read directly from the site's BADM
`FLUX_MEASUREMENTS_COMMENT` text.

## Out-of-range values (listed, not dropped)

| site_id | data_hub | check | value |
|---|---|---|---|
| US-Bi2 | AmeriFlux | NEP beyond +/-1500 gC/m2/yr | -1677.8 |
| CN-Sb1 | ICOS | NEP beyond +/-1500 gC/m2/yr | 1829.8 |
| IT-Lav | ICOS | NEP beyond +/-1500 gC/m2/yr | 1962.1 |

