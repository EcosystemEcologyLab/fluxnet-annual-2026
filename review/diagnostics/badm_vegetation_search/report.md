# BADM vegetation-structure and growth search — read-only

Read-only search of the BIF (site metadata) and BIFVARINFO files shipped inside each site's
FLUXNET Shuttle download package, for vegetation structure and growth information. No data,
code, figure, snapshot or DuckDB content was modified, and nothing was downloaded; this is a
search of what is already extracted on disk.

**Scope.** All 781 sites in the snapshot of record, `data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv`.
For each site, its own `*_BIF_*.csv` (site metadata) and `*_BIFVARINFO_YY_*.csv` (variable
definitions) files were located under `data/extracted/<product_name>/` by that site's exact
`fluxnet_product_name`, not by a site-ID glob (`data/extracted` holds some additional, non-snapshot
product versions for a few sites; matching on the snapshot's own product name avoids picking up a
stale duplicate). All 781 sites resolved to exactly one BIF file and one BIFVARINFO_YY file each.
BIF-reading code follows the existing pattern in `scripts/investigate_badm_management.R`
(`SITE_ID, GROUP_ID, VARIABLE_GROUP, VARIABLE, DATAVALUE`); no new parser was written.

## Summary

- The 781 BIF files contain **69 raw `VARIABLE_GROUP` labels** and 3.17 million rows combined
  (Table 1).
- Of the 24 requested keywords, **11 match a structured `VARIABLE_GROUP` or `VARIABLE` name**:
  AGE, BASAL, BIOMASS, CANOPY, DBH, HEIGHT, LAI, LITTER, ROOT, SPP, TREE. The other **13 — AG_BIOMASS,
  WOOD, STEM, SPECIES, STAND, DENSITY, NPP, GROWTH, ALLOM, DISTURB, HARVEST, THIN, MANAGE — never
  appear as a group or variable name.** DISTURB does appear once as free-text category content
  ("Undisturbed", inside the `DOM_DIST_MGMT` field); the other 12 do not appear in any structured
  field at all, only as coincidental substrings inside unrelated free text (Table 3; see §2).
- Genuine vegetation-structure data **does exist**: canopy height, leaf/plant area index, biomass,
  stem diameter (DBH), basal area, tree count, litter, rooting depth and (overstory) species
  composition all have dedicated BADM groups, each with its own date, species, vegetation-type,
  life-stage and measurement-organ qualifier fields (Tables 2 and 5).
- **Coverage is small and hub-concentrated.** Every one of those groups except canopy height is
  carried **only by ICOS-hub sites'** BIF exports (0 AmeriFlux, 0 TERN sites). Canopy height is the
  one variable present at all three hubs (344 ICOS + 86 AmeriFlux + 51 TERN = 482 of 781 sites).
  AmeriFlux's own BIF export carries no biomass/DBH/basal-area/trees-count/litter/LAI/species-
  composition data at all; its one vegetation-adjacent field is the coarse categorical
  `DOM_DIST_MGMT` (disturbance/management type, 277 AmeriFlux sites, 0 ICOS/TERN).
- **No BADM/vegetation variable is defined in BIFVARINFO.** That file is a FLUXMET (flux/met)
  variable catalogue (`NEE_VUT_REF`, `TA_F_MDS`, `PPFD_IN`, …), confirmed by inspecting every one
  of the 251 raw keyword hits returned there: all are coincidental substrings unrelated to
  vegetation ("density" inside "photosynthetic photon flux density", "stand" inside "standard
  deviation", "age" inside "percentage", "stem" inside "system"; Table 4). `definition`/`units` for
  every BADM vegetation variable below are therefore "not given" except where the BIF itself
  carries a companion `*_UNIT` field (biomass, litter, species-percent).
- **There is no numeric stand/tree age anywhere in the files.** The closest field is `*_LIFESTAGE`,
  a two-value category (Mature / Sapling), attached to a subset of biomass/DBH/canopy-height/
  basal-area/trees-count/species records.
- **Repeated measurements (≥2 distinct years) are rare.** Across the forest classes
  (ENF/EBF/DNF/DBF/MF, 276 sites), 191 (69%) have canopy height at all, but only 55 (20%) have any
  of biomass/DBH/basal-area/canopy-height/lifestage/species-composition on two or more distinct
  years (Table 6).

## 1. VARIABLE_GROUP inventory (Table 1, `table1_variable_group_inventory.csv`)

All 69 raw `VARIABLE_GROUP` labels found across the 781 BIF files, with the number of sites and
records carrying each, ranked by site count. The top of the list is processing/QC/site-header
metadata (`GRP_ERA_DOWN`, `GRP_FLUX_REF`, `GRP_ONEFLUX`, `GRP_UST_THR`, `SITE_CHAR`, `HEADER`,
`IGBP`, `LOCATION`, `NETWORK`, `TEAM_MEMBER`, …, present at 330–780 sites); soil groups
(`GRP_SOIL_TEX`, `GRP_SOIL_CHEM`, `GRP_SOIL_STOCK`, etc., 6–43 sites) sit in the middle; the
vegetation-structure groups relevant to this search sit lower still (7–345 sites — see §3).

Three anomalies in the raw group-label field itself, left as found (read-only):
- **`C:\Users\difio\Desktop\R\MakeBIF\GRP_HEIGHTC`** and **`...\GRP_LAI`** (1 site, `JP-Shn`, 4
  records each): an absolute Windows file path has leaked into the `VARIABLE_GROUP` field for this
  one site's canopy-height and LAI blocks. The underlying values are intact and readable
  (`HEIGHTC=24.5`, species `Chamaecyparis…`, date `20090701`; `LAI=6`, type `LAI`, date
  `20090701`) but are not counted under the normal `GRP_HEIGHTC`/`GRP_LAI` labels in Table 1/2/5
  because of this corrupted label.
- **`"Site_ID"`** (1 site, `AU-Ync`, 1 record): a stray row where `VARIABLE_GROUP`, `VARIABLE` and
  `DATAVALUE` are all literally `"Site_ID"`/`"Site_ID"`/`"AU-Ync"` — a malformed extra row, not a
  real BADM group.

Separately, the combined table has **783 distinct raw `SITE_ID` values** against 781 snapshot
sites: one is a casing variant (`NZ-CLa` vs. the snapshot's `NZ-Cla`, confined to that site's own
`GRP_ONEFLUX` product-metadata rows), one is a single mistyped row (`CCH-Aws`, 1 record, `GRP_NETWORK`/`NETWORK`="Swiss FluxNet"), and 15 rows have a blank/`NA` `SITE_ID` (evidently from the
`AU-Dry`, "Dry River", OzFlux site's own file, based on their `SITE_NAME`/`LOCATION`/`NETWORK`
content). None of these 17 anomalous rows fall inside any of the vegetation-structure groups
analysed in §3, so they do not affect the counts there.

## 2. Keyword search (Tables 2–4)

**Table 2** (`table2_keyword_structured_matches.csv`, 215 rows) — every `(keyword, VARIABLE_GROUP,
VARIABLE)` combination where the keyword is a case-insensitive substring of the group or variable
*name itself*. 15 raw group labels are touched; of those, 12 are genuine vegetation-structure
groups (`GRP_BASAL_AREA`, `GRP_BIOMASS`, `GRP_DBH`, `GRP_HEIGHTC`, `GRP_LAI`, `GRP_LITTER`,
`GRP_ROOT_DEPTH`, `GRP_SPP`, `GRP_SPP_O`, `GRP_TREES_NUM`, `GRP_VEG_CHEM`, `HEIGHTC`), 2 are the
`JP-Shn` corrupted-path duplicates above, and 1 is noise (`REFERENCE_PAPER`'s own
`REFERENCE_USAGE` variable matches AGE only because "usage" contains the substring "age").
`GRP_VEG_CHEM` (foliar nutrient chemistry: C, Ca, Cu, Fe, K, Mg, Mn, N, P, Zn concentrations, dry
ratio, LMA; 29 sites, ICOS-only) is included only because its `VEG_CHEM_SPP` variable matches SPP —
none of the chemistry variables themselves match a keyword, so it is not carried into §3/Table 5 as
its own row, but is noted here for completeness.

**Table 3** (`table3_keyword_datavalue_matches.csv`, 477 rows) — the same search repeated against
free-text `DATAVALUE` content for all 24 keywords, not just the ones with a structured match. This
is dominated by coincidental substring noise in unrelated fields: `TEAM_MEMBER_ROLE` ("Manager"
contains "age" and "manage"), `REFERENCE_PAPER`, `RESEARCH_TOPIC`, `ACKNOWLEDGEMENT`,
`LAND_OWNERSHIP`, `UTC_OFFSET_COMMENT` ("standard" time contains "stand"), `SOIL_CHEM_*`
(unrelated), `LOCATION_COMMENT`, `IGBP_COMMENT`. Two genuinely informative DATAVALUE-only results:
  - **`DOM_DIST_MGMT`** (the one AmeriFlux-only group, 277 sites, 363 records) carries a controlled
    vocabulary: `Agriculture` (97), `Forestry` (42), `Fire` (41), **`Undisturbed`** (40, the DISTURB
    keyword's only structured-category match), `Grazing` (32), `Hydrologic event` (25), `Drought`
    (24), `Land cover change` (22), `Storm or wind` (19), `Temperature extreme` (13), `Pests and
    disease` (8).
  - The free-text `*_APPROACH`/`*_COMMENT` fields *inside* the already-identified vegetation
    groups (e.g. `BIOMASS_APPROACH`, `HEIGHTC_APPROACH`, `DBH_APPROACH`) do carry genuine
    methodology narrative — allometry, harvest/destructive sampling, thinning, species, density —
    but as unstructured prose describing *how* a measurement was made, not as separate BADM
    variables in their own right.
  - `SITE_DESC` free text (a per-site narrative field, 372–623 sites depending on the raw group
    label) mentions several of the 13 unmatched keywords in places (species, harvest, thinning,
    management, growth, disturbance, density); this is prose, not structured data, and was not
    parsed further.

**Table 4** (`table4_bifvarinfo_keyword_matches.csv`, 251 rows from 320,928 `BIFVARINFO_YY`
definition rows across all 781 sites) — every keyword hit against `VAR_INFO_VARNAME`/
`VAR_INFO_DEFINITION`. All 251 are flux/met variables (`NEE_*`, `RECO_*`, `TA_F_MDS*`, `PPFD_*`,
…); zero are a BADM/vegetation variable. `BIFVARINFO_YY` documents the FLUXMET data product's own
columns, not the BIF metadata fields — it has no entries for `HEIGHTC`, `BIOMASS`, `DBH`, `LAI`, or
any other BADM variable found in §3.

## 3. Matched groups and variables in detail (Table 5, `table5_vegetation_variable_detail.csv`)

One row per matched concept: its raw group label(s), primary measurement variable, definition (as
established in §2, none exist in BIFVARINFO), units (only where a companion `*_UNIT` field exists
in the BIF itself), site count, sites by hub, sites by IGBP class, record count, the year range and
site counts with/without a recorded date, sites with ≥2 distinct years, and up to 5 example values.

| Concept | Raw group(s) | Variable | Units (in-file) | Sites | By hub | Records | Year range | Sites w/ date | Sites ≥2 distinct years | Example values |
|---|---|---|---|---|---|---|---|---|---|---|
| Canopy height | `GRP_HEIGHTC` / `HEIGHTC` | `HEIGHTC` | not given | 482 | ICOS=344; AmeriFlux=86; TERN=51 | 15,722 | 1993–2026 | 466 | 150 | 1.36; 3.67; 1.65; 0.52; 35 |
| Leaf/plant area index | `GRP_LAI` | `LAI` | not given | 158 | ICOS=158 | 10,716 | 1993–2030* | 158 | 86 | 2.75; 3.84; 3.9; 5.3; 6.4 |
| Biomass | `GRP_BIOMASS` | `BIOMASS` | gC m⁻²; kgDM m⁻² | 82 | ICOS=82 | 15,246 | 1993–2026 | 82 | 68 | 0.1799; 0.0999; 0.0329; 0.0602; 0.1066 |
| Stem diameter (DBH) | `GRP_DBH` | `DBH` | not given | 42 | ICOS=42 | 4,913 | 1994–2026 | 42 | 26 | 15.007; 3.229; 9.599; 17.45; 14.713 |
| Basal area | `GRP_BASAL_AREA` | `BASAL_AREA` | not given | 36 | ICOS=36 | 5,799 | 1994–2025 | 36 | 23 | 1.14; 1.362; 0.275; 3.541; 0.369 |
| Tree count/density | `GRP_TREES_NUM` | `TREES_NUM` | not given | 42 | ICOS=42 | 5,846 | 2001–2026 | 42 | 24 | 104; 185.4; 10; 435; 15 |
| Litter | `GRP_LITTER` | `LITTER` | kgDM m⁻² | 15 | ICOS=15 | 16,670 | 2016–2026 | 15 | 14 | 0.00041; 0.00036; 0.00098; 0.00013; 0 |
| Rooting depth | `GRP_ROOT_DEPTH` | `ROOT_DEPTH` | not given | 7 | ICOS=7 | 7 | 2003–2025 | 8† | 0 | 50; 85; 25; 100; 13 |
| Species composition (dominant) | `GRP_SPP` | `SPP_O` | not given | 40 | ICOS=40 | 1,311 | 2003–2026 | 44† | 22 | *Agrostis stolonifera*; *Anthoxanthum odoratum*; *Aulacomnium palustre*; *Avenella flexuosa*; *Calluna vulgaris* |
| Species composition (overstory) | `GRP_SPP_O` | `SPP_O_SPP` | not given | 23 | ICOS=23 | 4,508 | 2014–2025 | 23 | 20 | *Betula pendula*; *Picea abies*; *Pinus sylvestris*; *Salix caprea*; *Sorbus aucuparia* |
| Disturbance/management category | `DOM_DIST_MGMT` | `DOM_DIST_MGMT` | not given (categorical) | 277 | AmeriFlux=277 | 363 | n/a — no date field | 0 | 0 | Agriculture; Grazing; Land cover change; Drought; Undisturbed |

\* The LAI date range's upper bound (2030) is a single forward-dated record; not corrected (read-only).
† `ROOT_DEPTH_DATE`/`SPP_DATE` are present for slightly more sites (8, 44) than the primary value
variable itself (7, 40) — a few sites recorded a measurement date without the corresponding value
field being populated in the extracted file; left as found.

**IGBP breakdown**, all 11 concepts (sites by IGBP class, from the full snapshot's `igbp` field):
Canopy height — GRA 109, ENF 77, CRO 72, WET 51, DBF 47, EBF 39, OSH 24, MF 16, DNF 12, WSA 12, SAV
11, CSH 5, BSV 3, CVM 2, SNO 1. Biomass/DBH/basal area/trees/litter/rooting depth/species
composition are forest- and grassland-leaning (ENF the largest single class in all seven) but
individually small (7–42 sites); full per-concept breakdowns are in Table 5's `n_sites_by_igbp`
column.

**LAI sub-type note.** Of the 10,716 LAI-concept records, only 783 are tagged `LAI_TYPE=="LAI"`
(true leaf area index); 7,395 are `GAI` (green area index) and 2,541 `PAI` (plant area index) —
related but distinct canopy indices, reported together here because they share the `GRP_LAI` group
and `LAI` variable name, exactly as recorded.

**Biomass organ note.** Of 5,712 `BIOMASS_ORGAN`-tagged records, the large majority are aboveground
(`Total AG`/`total AG` 3,780 — note the inconsistent capitalisation in the raw data, left as found
— `Foliage` 770, `Stems` 625, `Wood AG` 161, `Total aboveground` 47); belowground/root biomass is
rare (`Roots` 28, `Total BG` 6, `Total belowground` 2 — 36 of 5,712, 0.6%).

**Vegetation-type note.** The `*_VEGTYPE` qualifier shows most, but not all, biomass/DBH/basal-
area/trees-count/litter records are explicitly tagged `Tree` (e.g. 6,169/9,516 BIOMASS_VEGTYPE
records; DBH/basal-area/trees-count are ≥99% Tree). Canopy height spans many vegetation types
(Tree 6,714/12,012 — also Crop, Grass, C3 Grass, etc.), since the group covers non-forest canopy
heights (crop/grass canopies) as well as tree canopies.

## 4. Forest focus (Table 6, `table6_forest_focus.csv`)

Forest IGBP classes: ENF (114 sites), EBF (44), DNF (13), DBF (81), MF (24) — 276 sites total.

| IGBP | Sites | Any biomass | Any DBH | Any basal area | Any canopy height | Any lifestage (age proxy) | Any species comp. | Any of these | ≥2 distinct years, any |
|---|---|---|---|---|---|---|---|---|---|
| DBF | 81 | 9 | 8 | 6 | 47 | 6 | 10 | 47 | 17 |
| DNF | 13 | 1 | 1 | 1 | 12 | 1 | 1 | 12 | 1 |
| EBF | 44 | 3 | 4 | 3 | 39 | 1 | 2 | 39 | 9 |
| ENF | 114 | 23 | 24 | 24 | 77 | 20 | 19 | 77 | 23 |
| MF | 24 | 3 | 2 | 2 | 16 | 2 | 2 | 16 | 5 |
| **Total (forest)** | **276** | **39** | **39** | **36** | **191** | **30** | **34** | **191** | **55** |
| *Total (non-forest, for context)* | *505* | *43* | *3* | *0* | *290* | *2* | *29* | *290* | *93* |

Canopy height drives most of "any of these" (191/276, 69%) since it is the one variable with
broader hub coverage; the other five concepts are each present at only 14–14% of forest sites
(36–39 of 276). Repeated measurement (≥2 distinct years, needed for anything about growth) on any
of the six concepts reaches only 55 of 276 forest sites (20%) — 23 of those in ENF alone.

Restricting biomass specifically to records tagged `BIOMASS_VEGTYPE=="Tree"` (rather than any
vegetation type) drops the forest-class count from 39 to **24 of 276** — 15 forest-class sites'
biomass records are tagged a non-tree vegetation type (understory, shrub, moss, etc.), not the
forest overstory itself.

## 5. What the product BIF files do and do not carry

The BIF files shipped in this product carry only a **subset** of possible BADM content, and that
subset is uneven across hubs:

- **Present:** canopy height (482 sites, all three hubs); biomass, DBH, basal area, tree
  count/density, litter, LAI/GAI/PAI, rooting depth and species composition (7–158 sites each, **ICOS
  hub only**); a coarse disturbance/management category, `DOM_DIST_MGMT` (277 sites, **AmeriFlux
  hub only**).
- **Not present anywhere in these files, under any name:** `AG_BIOMASS`, `WOOD`, `STEM`,
  `SPECIES` (as a structured field — species names only appear inside the SPP/SPP_O groups above,
  not under a variable literally named SPECIES), `STAND` (as a structured field — no stand-level
  aggregate variable exists under that name), a numeric `AGE`/stand-age value, `DENSITY` (as a
  structured field — wood/stem density is not recorded; `TREES_NUM` is a count, not a density
  variable, despite the name), `NPP`, `GROWTH` (as a structured field — no growth-rate or
  increment variable), `ALLOM` (no allometric-coefficient variable; "allometric" appears only in
  free-text `*_APPROACH` methodology comments), `HARVEST`/`THIN` (as structured fields — these only
  appear in free text), and `MANAGE` (as a structured field — `DOM_DIST_MGMT`'s own name uses
  "MGMT", not "MANAGE", and its categories are disturbance *types*, not management *actions*).
- **BIFVARINFO defines none of the above** — it is a FLUXMET variable catalogue, not a BADM
  variable dictionary (§2, Table 4).
- TERN-hub sites carry essentially no vegetation-structure BADM beyond canopy height (51 sites);
  no TERN site has biomass, DBH, basal area, trees-count, litter or species-composition data in
  this product's BIF files.

No inference is drawn here about what other BADM sources (e.g. the full AmeriFlux BADM template
outside this product's reduced BIF export) might hold — only what is actually present in
`data/extracted` for these 781 sites is reported.

## Files

- `table1_variable_group_inventory.csv` — all 69 raw `VARIABLE_GROUP` labels, sites, records.
- `table2_keyword_structured_matches.csv` — keyword × (group, variable) name matches, 215 rows.
- `table3_keyword_datavalue_matches.csv` — keyword × (group, variable) free-text matches, 477 rows.
- `table4_bifvarinfo_keyword_matches.csv` — keyword hits against BIFVARINFO_YY definitions, 251 rows (all non-vegetation).
- `table5_vegetation_variable_detail.csv` — the 11-row detail table underlying §3.
- `table6_forest_focus.csv` — the forest-class table underlying §4, plus the non-forest comparison row.
