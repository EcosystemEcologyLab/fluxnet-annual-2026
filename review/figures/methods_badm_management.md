# Methods: BADM Land-Management Coverage Across the FLUXNET Shuttle Network

**Script:** `scripts/investigate_badm_management.R`
**Outputs:** `data/snapshots/badm_management_coverage.csv`,
`data/snapshots/badm_management_summary.csv`,
`review/figures/candidates/fig_supp_badm_management_by_igbp.png`
**Snapshot date:** 2026-07-01 (network snapshot `fluxnet_shuttle_snapshot_20260624T095651.csv`, 767 sites)

## What BADM data was found

BADM ("Biological, Ancillary, Disturbance and Metadata") is included in every
FLUXNET Shuttle site download package as a **BIF** file
(`data/extracted/<SITE_DIR>/*_BIF_*.csv`), a long-format table of
`SITE_ID, GROUP_ID, VARIABLE_GROUP, VARIABLE, DATAVALUE` rows. A cached,
pre-concatenated copy also exists at `data/processed/badm.rds` (built by
`scripts/03_read.R`); this investigation reads the BIF CSVs directly from
`data/extracted/` instead, so the result reflects the currently extracted
site set and is reproducible without depending on that cache.

759 of 767 shuttle-network sites (99.0%) have an extracted BIF file. The
remaining 8 sites have not yet been extracted and are logged to
`outputs/unknown_log.csv` (management status unknown, not "no management").

**Critical finding: the standard AmeriFlux/FLUXNET event-based management
BADM templates are absent from every single BIF file in this network.** A
full-text search across all 759 files for the variable-group tokens `DM`,
`HARV`, `HARV_M`, `TILL`, `TILL_M`, `FERT`, `FERT_M`, `GRZ`, `GRZ_M`, `IRR`,
`IRR_M`, `BURN`, `THIN`, `LU`, `DRA` returned **zero matches**. These are the
templates AmeriFlux's site-level BADM system uses to record discrete,
dated management events (e.g. one row per fertilization application with a
rate and date). None of that event-level detail is present in the FLUXNET
Shuttle's BIF export for any site in the network.

The only management-relevant structured field actually present is:

- **`DOM_DIST_MGMT`** — a single coarse categorical tag (or small set of
  tags) per site, drawn from: `Agriculture`, `Fire`, `Forestry`, `Grazing`,
  `Hydrologic event`, `Drought`, `Land cover change`, `Storm or wind`,
  `Temperature extreme`, `Pests and disease`, `Undisturbed`. Present for
  272 of 762 sites in the cached badm.rds (fresh extraction gives similar
  counts). This records *that* a dominant disturbance/management type
  applies, never *what* was done, *when*, or *how much*.

- **`GRP_WTD`** — water-table-depth measurement records (found at only 3
  sites in the cached badm.rds). This is an environmental *measurement*
  group, not a management record — a site can report WTD purely as a
  covariate without any active drainage/rewetting intervention. It is
  reported separately (`wtd_group_present` column) and deliberately
  **excluded** from all management flags.

Because the structured fields cannot distinguish tillage from fertilization
from irrigation, or harvest from thinning, this analysis supplements them
with **case-insensitive keyword mining of the free-text `SITE_DESC` field**
(present for 366 of 759 sites), which frequently contains prose management
detail the structured BADM fields do not capture (e.g. *"Managed grassland
which is harvested 3-4 times per year"*, *"grazing is rotational"*, *"typical
rotation of the region: corn / soybean / wheat-soybean"*).

## How coverage was interpreted

For each site, ten management categories are flagged `TRUE`/`FALSE`/`NA`:

| Category | Source |
|---|---|
| `mgmt_harvest` | `SITE_DESC` keyword: `harvest` |
| `mgmt_tillage` | `SITE_DESC` keyword: `till`, `plow`/`plough`, `disk(ed/ing)` |
| `mgmt_fertilization` | `SITE_DESC` keyword: `fertili[sz]e` |
| `mgmt_grazing` | `SITE_DESC` keyword (`graz`, `livestock`, `cattle`, `sheep`, `pasture`) **OR** `DOM_DIST_MGMT == "Grazing"` |
| `mgmt_irrigation` | `SITE_DESC` keyword: `irrigat` |
| `mgmt_thinning` | `SITE_DESC` keyword: `thinn` |
| `mgmt_burning` | `SITE_DESC` keyword (`burn`, `fire`, `prescribed fire`) **OR** `DOM_DIST_MGMT == "Fire"` |
| `mgmt_drainage_wtd` | `SITE_DESC` keyword (`drain`, `water table`, `rewet`, `ditch`) |
| `mgmt_forestry_unspecified` | `DOM_DIST_MGMT == "Forestry"` with no `harvest`/`thinn` keyword hit — a forest is flagged as under some form of silvicultural management, but harvest vs. thinning cannot be distinguished |
| `mgmt_other` | `DOM_DIST_MGMT` of `Agriculture` or `Land cover change` with no specific subtype resolved by any keyword above |

**Coverage definition:** "any record present in this category" = a positive
match by either the structured `DOM_DIST_MGMT` tag or the `SITE_DESC`
text-mining rule above. `any_management` = the union (logical OR) of all ten
category flags. Sites with no extracted BIF file at all get `NA` (unknown)
for every flag, never `FALSE` — a site is only counted as "not managed" if
its BADM record was actually inspected and found no management evidence.
Natural-disturbance-only `DOM_DIST_MGMT` tags (`Drought`, `Storm or wind`,
`Temperature extreme`, `Pests and disease`) and `Undisturbed` do **not**
count toward any management category, since they are not management.

**Region** is a 6-way bucket (N. America / S. America / Europe / Asia /
Africa / Australia) derived from the site ID's ISO 3166-1 country-code
prefix via `countrycode::countrycode(..., "un.regionsub.name")`, following
the same convention as `.iso2_to_continent()` in
`R/figures/fig_timeseries.R`. This is geographic derivation from the site's
own country code, not the hub/region inference forbidden by CLAUDE.md
(which concerns network/hub membership, not country). One simplification:
the installed `countrycode` version's `un.regionsub.name` field lumps all of
Central America, South America, the Caribbean, and Mexico into one label
("Latin America and the Caribbean"), which is folded here entirely into
"S. America" — Mexico is therefore counted with South America rather than
North America in this regional breakdown.

**IGBP class** is taken from the canonical shuttle snapshot's `igbp` column
(the network's standard IGBP source — see `scripts/07_figures.R`), not
re-derived from BADM. The network's BADM data include three sites tagged
with IGBP codes outside the repo's standard 15-class palette
(`R/plot_constants.R::IGBP_order`): `BSV` (7 sites), `CVM` (9 sites), `SNO`
(2 sites). These are included in the coverage table and cross-tab but were
initially dropped from the first draft of the supplementary figure by an
`intersect()` against the standard palette order — fixed so all IGBP codes
present in the data appear in the figure (non-standard codes get a neutral
grey tick label instead of a palette colour).

## Results summary

- **759 / 767 sites (99.0%)** have an extracted BIF/BADM file.
- **336 / 759 BADM sites (44.3%)** carry at least one management-relevant
  record by the definition above.
- By category (of 759 BADM sites): grazing 86 (11.3%), burning 77 (10.1%),
  drainage/water-table 53 (7.0%), tillage 52 (6.9%), other/unclassified 51
  (6.7%), harvest 43 (5.7%), fertilization 31 (4.1%), forestry-unspecified 31
  (4.1%), irrigation 27 (3.6%), thinning 5 (0.7%).
- Best-documented IGBP classes: `CVM` (78%, n=9 — small sample), `CRO`
  croplands (68.6%, n=137), `ENF` evergreen needleleaf forest (52.7%,
  n=112), `OSH` open shrubland (51.3%, n=39).
- Least-documented: `EBF` evergreen broadleaf forest (14.3%, n=42), `BSV`/
  `SNO` (0%, small n), `DNF` (23.1%, n=13).
- Grasslands (`GRA`, 45.0%, n=140) and wetlands (`WET`, 32.5%, n=114) sit
  in the middle — wetlands are documented mainly through drainage/water-table
  text mentions (19 of 114), not a dedicated wetland-management template.
- Regional coverage (`any_management`): N. America 66.4% (n=354), S. America
  51.7% (n=29), Africa 43.5% (n=23), Europe 29.6% (n=196), Asia 16.3%
  (n=104), **Australia 1.9% (n=53)** — by far the weakest.

## Known limitations

1. **No event-level management data exists in this network's BADM at all.**
   This analysis cannot answer "how many fertilization applications" or
   "when was this stand last harvested" — only "does BADM mention this
   management type anywhere for this site, ever." Any prose built on this
   result should describe it as *documentation coverage*, not management
   *intensity* or *frequency*.
2. **Text mining is a heuristic, not a controlled vocabulary.** `SITE_DESC`
   is free-text authored independently by hundreds of site teams; keyword
   matches can be false positives (e.g. "tillage" mentioned in a historical
   land-use sentence for a site that is not currently managed) or false
   negatives (a site is grazed but the describer used a term outside the
   keyword list, e.g. "browsed by wildlife" would not match `graz`). No
   manual verification of individual matches was performed beyond the five
   illustrative examples below.
3. **`DOM_DIST_MGMT` conflates disturbance and management** in a single
   field with no way to separate a natural fire from a prescribed burn, or
   natural windthrow from planned forest harvest, from the tag alone (text
   mining partially resolves this for burning, not for forestry).
4. **Absence of a record is not evidence of absence of management.** Only
   366 of 759 sites (48%) have any `SITE_DESC` text at all; a site with no
   `SITE_DESC` and no `DOM_DIST_MGMT` tag is coded `any_management = NA`,
   correctly reflecting "we don't know," but readers should not interpret
   the 44.3% coverage figure as "56% of sites are known to be unmanaged."
   Many are simply undocumented in this field.
5. **The regional pattern may partly reflect metadata-submission practice
   per network/hub, not actual land management.** Australia's 1.9%
   `any_management` rate (vs. North America's 66.4%) is a striking outlier
   and should be checked against TERN's BADM submission conventions before
   being cited as evidence that Australian sites are less intensively
   managed — it more plausibly reflects that TERN's BIF exports rarely
   populate `SITE_DESC` or `DOM_DIST_MGMT` at all, not that Australian
   flux towers sit in more pristine landscapes.
6. **Regional bucketing folds Mexico, Central America, and the Caribbean
   into "S. America"** due to a `countrycode` package limitation described
   above — a minor mislabelling for the small number of Mexican sites.
7. This BADM-derived coverage is a **proxy for whether the network
   documents management**, not a proxy for whether management is actually
   occurring. A site can be intensively managed with zero mention in BADM
   (undocumented), and the inverse is not possible to construct from this
   data (BADM does not record negatives — there is no "confirmed
   unmanaged" tag other than the sparse `Undisturbed` `DOM_DIST_MGMT`
   value, present at only 40 sites).

## Illustrative examples

**US-Ne1 — irrigated cropland (Mead, Nebraska, USA), IGBP `CRO`**
`DOM_DIST_MGMT: Agriculture`. `SITE_DESC`: "...located at the University of
Nebraska Agricultural Research and Development Center near Mead, Nebraska.
This site is irrigated with a center pivot system..." Text mining also
confirms fertilization and tillage records for this site — one of the
three classic Mead maize/soybean irrigation-gradient sites (US-Ne1/Ne2/Ne3),
which together are the network's clearest fertilization + irrigation +
tillage example.

**US-Bar — managed temperate forest (Bartlett Experimental Forest, New
Hampshire, USA), IGBP `DBF`**
`DOM_DIST_MGMT: Forestry`. `SITE_DESC`: "...located within the White
Mountains National Forest... established in 1931 and is managed by the USDA
Forest Service..." Flagged `mgmt_forestry_unspecified` — BADM confirms
active silvicultural management but does not specify harvest vs. thinning
regime or dates.

**FR-Mej — grazed grassland with rotational cropping (Méjusseaume, Brittany,
France), IGBP `GRA`**
`SITE_DESC`: "...located on managed agricultural land (grazed grassland with
occasional maize cropping seasons)... part of the INRAE-PEGASE dairy
experimental farm..." Grazing confirmed by text mining (no `DOM_DIST_MGMT`
tag recorded for this site — an example of text mining recovering a signal
the structured field misses).

**DK-Skj — rewetted/restored wetland meadow (Skjern Meadows, Jutland,
Denmark), IGBP `WET`**
`SITE_DESC`: "...the Skjern Meadows is a recently rewetted meadow, which
during the 1960s was drained and used for intensive agriculture. In 2002 a
large restoration project was finished..." Flagged `mgmt_drainage_wtd` — a
textbook example of the drainage-history-then-rewetting narrative that the
task's wetland water-table-management category is meant to capture, and
which only free-text mining (not any structured field) records.

**US-ARb — prescribed-burn tallgrass prairie (ARM SGP, Oklahoma, USA), IGBP
`GRA`**
`SITE_DESC`: "...located in the native tallgrass prairies of the USDA
Grazinglands Research Laboratory... the US-ARb plot was burned on
2005/03/08. The second plot, US-ARc, was left unburned as the control..."
No `DOM_DIST_MGMT` tag recorded — flagged `mgmt_burning` and `mgmt_grazing`
purely by text mining. Paired with its unburned control (US-ARc), this is
the network's clearest experimental fire-management example, and shows the
value of the `SITE_DESC` supplement: the structured field alone would have
missed this site entirely.
