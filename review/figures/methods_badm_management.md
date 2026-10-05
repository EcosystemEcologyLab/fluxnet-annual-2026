# Methods: BADM Land-Management Coverage Across the FLUXNET Shuttle Network

**Script:** `scripts/investigate_badm_management.R`
**Outputs:** `data/snapshots/badm_management_coverage.csv`,
`data/snapshots/badm_management_summary.csv`,
`review/figures/candidates/fig_supp_badm_management_by_igbp.png`
**Snapshot date:** 2026-10-05 (network snapshot `fluxnet_shuttle_snapshot_20260901T094522.csv`, 781
sites — locked 1 September snapshot, pinned explicitly rather than the newest snapshot in
`data/snapshots/`). Supersedes the 2026-07-01 run on `fluxnet_shuttle_snapshot_20260624T095651.csv`
(767 sites); see `SESSION_LOG.md` (2026-10-05 entry) for the full old-vs-new comparison. Category
definitions and keyword lists are unchanged between the two runs.

## What BADM data was found

BADM ("Biological, Ancillary, Disturbance and Metadata") is included in every
FLUXNET Shuttle site download package as a **BIF** file
(`data/extracted/<SITE_DIR>/*_BIF_*.csv`), a long-format table of
`SITE_ID, GROUP_ID, VARIABLE_GROUP, VARIABLE, DATAVALUE` rows. A cached,
pre-concatenated copy also exists at `data/processed/badm.rds` (built by
`scripts/03_read.R`); this investigation reads the BIF CSVs directly from
`data/extracted/` instead, so the result reflects the currently extracted
site set and is reproducible without depending on that cache.

781 of 781 shuttle-network sites (100.0%) in the locked 1 September snapshot have an extracted
BIF file — no sites are logged to `outputs/unknown_log.csv` for missing BIF in this run. (At the
prior 2026-07-01 run, 759 of 767 sites (99.0%) had an extracted BIF file; the remaining 8 have
since been extracted.)

**Critical finding: the standard AmeriFlux/FLUXNET event-based management
BADM templates are absent from every single BIF file in this network.** A
full-text search across all 781 BIF files (locked 1 September snapshot) for
the variable-group tokens `DM`, `HARV`, `HARV_M`, `TILL`, `TILL_M`, `FERT`,
`FERT_M`, `GRZ`, `GRZ_M`, `IRR`, `IRR_M`, `BURN`, `THIN`, `LU`, `DRA`
**still returns zero matches**, confirming the 2026-07-01 finding holds at
the larger, locked snapshot. These are the
templates AmeriFlux's site-level BADM system uses to record discrete,
dated management events (e.g. one row per fertilization application with a
rate and date). None of that event-level detail is present in the FLUXNET
Shuttle's BIF export for any site in the network.

The only management-relevant structured field actually present is:

- **`DOM_DIST_MGMT`** — a single coarse categorical tag (or small set of
  tags) per site, drawn from: `Agriculture`, `Fire`, `Forestry`, `Grazing`,
  `Hydrologic event`, `Drought`, `Land cover change`, `Storm or wind`,
  `Temperature extreme`, `Pests and disease`, `Undisturbed`. Present for
  277 of 781 sites (35.5%) in the 2026-10-05 run (272 of 759 BADM sites,
  35.8%, at the 2026-07-01 run). This records *that* a dominant
  disturbance/management type applies, never *what* was done, *when*, or
  *how much*.

- **`GRP_WTD`** — water-table-depth measurement records (found at only 3
  sites in the cached badm.rds). This is an environmental *measurement*
  group, not a management record — a site can report WTD purely as a
  covariate without any active drainage/rewetting intervention. It is
  reported separately (`wtd_group_present` column) and deliberately
  **excluded** from all management flags.

Because the structured fields cannot distinguish tillage from fertilization
from irrigation, or harvest from thinning, this analysis supplements them
with **case-insensitive keyword mining of the free-text `SITE_DESC` field**
(present for 624 of 781 sites, 79.9%, in the 2026-10-05 run), which
frequently contains prose management detail the structured BADM fields do
not capture (e.g. *"Managed grassland which is harvested 3-4 times per
year"*, *"grazing is rotational"*, *"typical rotation of the region: corn /
soybean / wheat-soybean"*).

> **Correction (2026-10-05):** the 2026-07-01 version of this document
> stated SITE_DESC was present for "366 of 759 sites" (48%). That figure was
> a documentation error — the archived output of that same 2026-07-01 run
> (`data/snapshots/badm_management_coverage.csv` at commit `782b693`)
> actually shows `has_site_desc = TRUE` for **612 of 759 sites (80.6%)**,
> consistent with the 79.9% found here. The 44.3%/336-site `any_management`
> headline number from that run was not affected by this error (it is
> correct), but the "48% SITE_DESC coverage" framing in the limitations
> section below has been corrected to the true ~80% figure.

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

**2026-10-05 run (pinned to the locked 1 September snapshot, 781 sites)** — current numbers, with
the 2026-07-01 run (759 BADM sites) shown alongside for comparison:

- **781 / 781 sites (100.0%)** have an extracted BIF/BADM file (was 759 / 767, 99.0%).
- **342 / 781 BADM sites (43.8%)** carry at least one management-relevant record by the definition
  above (was 336 / 759, 44.3%).
- By category, new run (of 781 BADM sites) vs. old run (of 759 BADM sites):

  | Category | New: n (%) | Old: n (%) | Δ sites |
  |---|---|---|---|
  | any_management | 342 (43.8%) | 336 (44.3%) | +6 |
  | grazing | 89 (11.4%) | 86 (11.3%) | +3 |
  | burning | 78 (10.0%) | 77 (10.1%) | +1 |
  | drainage/water-table | 53 (6.8%) | 53 (7.0%) | 0 |
  | tillage | 52 (6.7%) | 52 (6.9%) | 0 |
  | other/unclassified | 52 (6.7%) | 51 (6.7%) | +1 |
  | harvest | 45 (5.8%) | 43 (5.7%) | +2 |
  | forestry-unspecified | 33 (4.2%) | 31 (4.1%) | +2 |
  | fertilization | 31 (4.0%) | 31 (4.1%) | 0 |
  | irrigation | 29 (3.7%) | 27 (3.6%) | +2 |
  | thinning | 5 (0.6%) | 5 (0.7%) | 0 |

  Only `any_management` changed by more than 5 sites between the two runs (+6); every individual
  category shifted by 3 sites or fewer. The larger `any_management` delta reflects sites that are
  newly positive in more than one category simultaneously (the union grows faster than any single
  category).

- **Three-way split** (structured `DOM_DIST_MGMT` tag only / `SITE_DESC` keyword only / both),
  2026-10-05 run, of the 342 `any_management` sites: structured-only 92, keyword-only 133, both 117.
  `fertilization`, `irrigation`, and `thinning` have **zero** structured-tag contribution by
  construction (the category definitions OR a keyword against `DOM_DIST_MGMT` only for grazing and
  burning) — all 31 fertilization, 29 irrigation, and 5 thinning positives are keyword-only or
  (6, 21, 4 respectively) corroborated by an unrelated Agriculture/Forestry tag as "both" where that
  tag happens to co-occur. This retroactive three-way split is not recoverable from the archived
  2026-07-01 output (only the combined `mgmt_*` flags were written, not the intermediate
  `txt_*`/`dom_dist_*` sub-flags).
- **38 sites** enter `any_management` *only* through `mgmt_burning` (a `DOM_DIST_MGMT == "Fire"`
  tag or a burn/fire `SITE_DESC` keyword, with no other category positive) — unchanged from the
  2026-07-01 run (also 38).
- Best-documented IGBP classes: `CVM` (77.8%, n=9 — small sample), `CRO`
  croplands (68.3%, n=139), `ENF` evergreen needleleaf forest (52.6%,
  n=114), `OSH` open shrubland (48.8%, n=41).
- Least-documented: `EBF` evergreen broadleaf forest (13.6%, n=44), `BSV`/
  `SNO` (0%, small n), `DNF` (23.1%, n=13).
- Grasslands (`GRA`, 45.2%, n=146) and wetlands (`WET`, 31.6%, n=117) sit
  in the middle.
- Regional coverage by `any_management` was not recomputed in this run (region bucketing is
  unaffected by the snapshot pin); see the 2026-07-01 figures above for the last computed values.

**`any_management` by IGBP class, 2026-10-05 run (count / n sites with BIF / %):**

| IGBP | n_positive | n | % |
|---|---|---|---|
| CVM | 7 | 9 | 77.8 |
| CRO | 95 | 139 | 68.3 |
| ENF | 60 | 114 | 52.6 |
| OSH | 20 | 41 | 48.8 |
| GRA | 66 | 146 | 45.2 |
| MF | 9 | 24 | 37.5 |
| CSH | 4 | 12 | 33.3 |
| WSA | 6 | 18 | 33.3 |
| WET | 37 | 117 | 31.6 |
| DBF | 25 | 81 | 30.9 |
| SAV | 4 | 14 | 28.6 |
| DNF | 3 | 13 | 23.1 |
| EBF | 6 | 44 | 13.6 |
| BSV | 0 | 7 | 0.0 |
| SNO | 0 | 2 | 0.0 |

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
4. **Absence of a record is not evidence of absence of management.** 624 of
   781 sites (79.9%) have any `SITE_DESC` text at all (corrected 2026-10-05 —
   see the correction note above; this was previously mis-stated as 48%); a
   site with no `SITE_DESC` and no `DOM_DIST_MGMT` tag is coded
   `any_management = NA`, correctly reflecting "we don't know," but readers
   should not interpret the 43.8% coverage figure as "56% of sites are known
   to be unmanaged." Some sites remain undocumented in this field, but most
   of the shortfall below 100% is genuine absence-of-management-record
   within a *present* `SITE_DESC`, not a missing `SITE_DESC`.
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
