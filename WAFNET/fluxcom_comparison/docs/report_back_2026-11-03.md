# WAFNET x FLUXCOM-X-BASE comparison — Stage 2 report (2026-11-03)

Requested by Ossénatou Mamadou (WAF-Net paper), via David Moore. Builds on the
Stage 1 pre-analysis report (`report_back_20261014.md`) and its code
(`code/00_config.R`–`03_product_overlap.R`), using the same metadata and
logging conventions. Produced by `code/04_download_xbase.sh`,
`05_download_ilamb_backup.sh`, `06_tower_data_update.R`,
`07_training_site_check.R`, `08_extract_statistics.R`, and
`09_figures_draft.R`.

**FLUXCOM-X-BASE and the ILAMB-hosted original-FLUXCOM backup copy are
non-Shuttle comparison data under CLAUDE.md Hard Rule #1** — every table and
figure below is labelled accordingly, and neither feeds into the FLUXNET
Annual Paper 2026.

## Why X-BASE, not the originally-chosen product

Ossénatou's 16 October reply chose the original FLUXCOM product
(RS+METEO). That product is no longer obtainable from its native route: the
MPI-BGC data-exchange portal link given in the FLUXCOM papers returns
HTTP 404, and the institute's data-products page no longer lists it. Dave
decided to proceed with **FLUXCOM-X-BASE** (Nelson et al. 2024,
*Biogeosciences* 21:5079–5115, doi:10.5194/bg-21-5079-2024; dataset
doi:10.18160/5NZG-JMJE, ICOS Carbon Portal, CC BY 4.0, 2001–2021) and to keep
the original product as an option if it becomes available again. A backup
copy of four original-FLUXCOM variables was pulled from the ILAMB reference
collection for documentation purposes (section 2) — **not used in the
comparison below.**

## Sites

Five agreed sites: `GH-Ank`, `BJ-Nhu`, `BJ-Bfg`, `SN-Dhr`, and **`ML-Kem`**
(added at Ossénatou's request — Kelma, Mali, WSA, 15.2875°N/1.543056°W,
2007–2009). Two sites are re-checked here and kept **separate, flagged
"pending Ossénatou"**, in every table and figure: `BJ-Db1` and `SN-Nkr`,
excluded from the agreed set in Stage 1 only because they had zero overlap
with the original FLUXCOM product — X-BASE's longer period (2001–2021 vs.
the original product's 2001–2015/2017) changes that for one of the two (see
section 4).

## 1. FLUXCOM-X-BASE download

`code/04_download_xbase.sh` discovers the file list dynamically via the ICOS
metadata SPARQL endpoint (`meta.icos-cp.eu/sparql`), filtering on the
`_050_monthly.nc` filename suffix (ICOS's naming convention: `050` = 0.5°,
`005` = 0.05° — a decimal-degree code, not a direct resolution string).
**84 files** were found and downloaded: the product's four distributed
variables (**GPP, NEE, ET, ET_T** [transpiration] — matching the "expected"
set in the prompt) × 21 years (2001–2021). This directly confirms the
ICOS-archive bound of Stage 1's unresolved "2001–2020 (paper headline) vs.
2001–2021 (archive)" discrepancy: **all 21 years through 2021 exist and
downloaded successfully** — the archive bound is the correct one to use
going forward, not the paper's own in-text headline.

**Download mechanism (a licence gate, reported before proceeding, per the
prompt's instruction):** every ICOS object is served only after a CC BY 4.0
licence click-through — `GET /licence_accept?ids=["<hash>"]` sets a
per-object cookie, after which `GET /objects/<hash>` serves the file. This
was confirmed interactively to require **no account, login, email, or any
personal credential** before the download script was run; Dave confirmed
proceeding. No credential or token of any kind was printed or stored, in
the repo or in logs.

All 84 files were sha256-verified against the checksum the SPARQL endpoint
itself reports (`WAFNET/fluxcom_comparison/tables/fluxcom_x_base_download_manifest.csv`
— filename, object hash, size, sha256, download timestamp, licence, and
citation per row; an initial padding bug in the verification comparison was
found and fixed, then all 84 re-verified as `TRUE`). Total ≈ 432 MB, saved
to `data/external/fluxcom_x_base/` (gitignored, re-downloadable via the
script).

Units, read directly from the downloaded files (`terra::rast()`/`ncdf4`),
not assumed from the paper text:

| Variable | Units (as distributed) |
|---|---|
| GPP, NEE | `gC m-2 d-1` (a monthly **mean daily rate**, not an integrated total — same convention as the tower's own MM-resolution FLUXMET extract) |
| ET, ET_T | `mm hr-1` (a monthly **mean hourly rate**, not an integrated total) |

## 2. Backup copy of the original FLUXCOM ensemble (ILAMB) — NOT used in the comparison

`code/05_download_ilamb_backup.sh` pulled the four requested files from
`https://www.ilamb.org/ILAMB-Data/DATA/` (anonymous HTTP, no login or
licence click-through found) into `data/external/fluxcom_original_ilamb/`
(gitignored). Metadata read directly from each file's own NetCDF attributes:

| File | Setup | Forcing | Method | Period | Resolution | Units (as distributed) | Citation (file's own `references` attr) |
|---|---|---|---|---|---|---|---|
| `gpp.nc` | RS+METEO | CRUNCEPv6 | Artificial Neural Networks | 1979-12 – 2013-11 (408 months) | 0.5° | `g m-2 day-1` | Jung et al. (2019)/Tramontana et al. (2016) — see note below |
| `reco.nc` | RS+METEO | CRUNCEPv6 | Artificial Neural Networks | 1979-12 – 2013-11 (408 months) | 0.5° | `g m-2 day-1` | same as `gpp.nc` |
| `le.nc` | RS+METEO | GSWP3v1 | ML ensemble (Jung et al. 2019 energy product) | 1979-12 – 2014-11 (420 months) | 0.5° | `watt/m2` (converted from native `MJ/m2/day`, per the file's own `comments` attribute) | Jung et al. (2019)/Tramontana et al. (2016) |
| `sh.nc` | RS+METEO | GSWP3v1 | ML ensemble (Jung et al. 2019 energy product) | 1979-12 – 2014-11 (420 months) | 0.5° | `watt/m2` | Jung et al. (2019)/Tramontana et al. (2016) |

**Metadata quirk, reported as observed, not resolved:** `gpp.nc`'s and
`reco.nc`'s own `references` NetCDF attribute cites Jung et al. (2019, the
*energy* ensemble paper) and Tramontana et al. (2016), not Jung et al.
(2020) — the carbon RS+METEO product paper that actually matches their
`title`/`source` attributes ("FLUXCOM (RS+METEO) Global Land Carbon Fluxes
using CRUNCEP climate data"). This looks like an ILAMB packaging artifact
(boilerplate reference block reused across variables) rather than a real
product change; cite Jung et al. (2020, *Biogeosciences* 17:1343–1365,
doi:10.5194/bg-17-1343-2020) for `gpp.nc`/`reco.nc` if this backup copy is
ever used, not the file's own `references` attribute.

Manifest with sha256 checksums: `tables/fluxcom_original_ilamb_manifest.csv`.

## 3. Product details: units, GPP partitioning, training sites

**GPP partitioning — resolved, high confidence.** ICOS's own product-level
metadata (`meta.icos-cp.eu/objects/<hash>` `description` field, read
directly, not inferred from the paper's un-OCR'd Table 1 image) states
verbatim: *"The GPP estimates from the eddy covariance data was based on the
Nighttime Partitioning method."* This **supersedes Stage 1's "NOT FOUND"**
for X-BASE's partitioning — X-BASE trains on **NT only**, not DT, and not
both. Every GPP/RECO comparison below therefore uses the tower's
**`GPP_NT_VUT_REF`/`RECO_NT_VUT_REF`** (or `_CUT_REF` per site), not the
`GPP_DT`/`RECO_DT` series Stage 1 used — a deliberate change from Stage 1's
default, required by this step's finding, not an inconsistency.

**Training sites — resolved, high confidence.** `code/07_training_site_check.R`
reads Nelson et al. (2024) Table 1 directly from the publisher's own XLSX
download (`bg-21-5079-2024-t01.xlsx`, caption "Citation data for the 294
sites used in the X-BASE products") — 294 codes read, matching the paper's
stated count exactly. This **supersedes Stage 1's medium-confidence
reference-list proxy** (the paper's HTML Table 1 rendered as an un-OCR'd
image).

| Site | In X-BASE training set? |
|---|---|
| `GH-Ank` | **YES** (confirmed, direct Table 1 match) |
| `SN-Dhr` | **YES** (confirmed, direct Table 1 match) |
| `BJ-Nhu` | No |
| `BJ-Bfg` | No |
| `ML-Kem` | No |
| `BJ-Db1` (pending) | No |
| `SN-Nkr` (pending) | No |

Other African sites in the 294-site list, for context: `CG-Tch` (Congo),
`SD-Dem` (Sudan), alongside `GH-Ank`/`SN-Dhr`. Full table:
`tables/fluxcom_x_base_training_sites_stage2.csv`.

## 4. Variables: sign convention, derivation, unit conversion

**NEE sign convention — checked empirically, not assumed.** X-BASE's own
metadata has no explicit sign-convention attribute, so this was checked
against a known strong carbon-sink site: at `GH-Ank` (tropical evergreen
forest) in 2001, X-BASE GPP is positive (6.1–7.1 gC m⁻² d⁻¹) and X-BASE NEE
is **negative in every month** (−0.50 to −1.39 gC m⁻² d⁻¹). This is the same
**atmospheric convention as the tower's `NEE_VUT_REF`/`NEE_CUT_REF`**
(negative = net uptake by the ecosystem, positive = net release) — **no sign
flip is needed** between X-BASE and tower NEE.

**RECO — derived, labelled as such everywhere.** X-BASE has no respiration
variable. `RECO_derived = GPP_xbase + NEE_xbase`, consistent with the shared
atmospheric sign convention above: in a month of net uptake, NEE is negative,
so adding it to GPP subtracts the uptake magnitude (GPP + NEE = GPP − |NEE|);
in a month of net release, NEE is positive and adding it increases RECO above
GPP. Every table/figure that shows RECO marks it `derived`.

**H — never compared.** X-BASE has no sensible-heat variable at all. Stated
here and in every output that touches energy fluxes.

**ET → LE conversion.** X-BASE's `ET`/`ET_T` are mean rates in `mm hr-1`
(confirmed from the file itself, not assumed — section 1). Converted to LE
(`W m-2`) using the **same latent heat of vaporization as `R/units.R`**,
λ = 2.45×10⁶ J/kg: 1 mm of water over 1 m² is exactly 1 kg m⁻² (at
ρ_water = 1000 kg/m³ — the identity underlying "mm of water" itself), so
`LE[W/m2] = ET[mm/hr] * λ / 3600`. Compared against tower `LE_F_MDS`
(uncorrected), per the Stage 1 convention. `ET_T` (transpiration) was
extracted but has no tower equivalent to compare against and is not used in
the statistics below.

**No unit conversion needed for carbon variables at monthly resolution** —
X-BASE's GPP/NEE are already `gC m-2 d-1` monthly mean daily rates, the same
unit the tower's native MM FLUXMET extract uses (confirmed independently in
both Stage 1 and by reading the downloaded netCDF units attribute). They are
compared **as distributed**, not multiplied by days-in-month — that
`R/units.R` conversion is for turning a mean rate into a period *total*,
which this comparison does not need.

**GH-Ank has no NT-partitioned GPP/RECO tower data at all** (checked:
`GPP_NT_VUT_REF`/`RECO_NT_VUT_REF` are 100% `NA` across its whole MM
extract, while `GPP_DT_VUT_REF`/`RECO_DT_VUT_REF` are present for 34 of 48
rows) — an ICOS/EUF-processing difference from the other EUF site, `SN-Dhr`,
which does have NT. **`GH-Ank` is therefore skipped for GPP/RECO only, not
for NEE or LE.** X-BASE does not combine NT and DT (section 3), so there is
no "compare against both" case to report here.

## 5. Tower data update

`code/06_tower_data_update.R` extends `tables/site_carbon_source.csv` and
`tables/site_variable_qc_overlap_{yy,mm}.csv` to all seven sites (five
agreed + two pending), adding a `site_status` column and `GPP_NT`/`RECO_NT`
variable rows alongside the existing `GPP_DT`/`RECO_DT` rows (not replacing
them — the DT rows stay, numerically unchanged from Stage 1, for any future
DT-partitioned product comparison). `ML-Kem` resolves to **VUT**
(1/1 years with non-NA `NEE_VUT_REF_QC`), same rule as the other six sites.

Same QC rule as Stage 1: `qc_passes()` at `QC_THRESHOLD_MM`/`QC_THRESHOLD_YY`
(0.50), GPP/RECO gated on the site's NEE QC column (neither has a QC column
of its own), LE on its own QC column, NA counts as not passing.

**`BJ-Db1` and `SN-Nkr` overlap with X-BASE, computed and reported as
requested:**

- **`BJ-Db1`: zero overlap, confirmed again.** Its only QC-qualifying record
  (2022-05–2024-12, 32 months / 2023–2024, 2 years) is entirely after
  X-BASE's 2021 archive end — the longer X-BASE period does not help here.
  `BJ-Db1` has **no rows at all** in any Stage 2 X-BASE table or figure; it
  cannot currently be compared against any FLUXCOM release.
- **`SN-Nkr`: now has usable overlap**, where it had none against the
  original product in Stage 1. Of its 80 QC-qualifying months
  (2018-03–2024-12), **46 fall within X-BASE's 2001–2021 archive**
  (2019-03–2021-12); of its 6 qualifying years, **3** (2019–2021) overlap.
  `SN-Nkr` is carried through sections 6–8 below, flagged `pending
  Ossénatou` throughout.

## 6. Extraction and statistics

`code/08_extract_statistics.R` extracts the X-BASE 0.5° grid cell containing
each tower (`terra::extract()` at the site's own BIF-file coordinates,
read-only) for every year where the tower's QC-qualifying month falls in
2001–2021. **Monthly statistics are the main result**; annual statistics are
a secondary look, computed only for site × flux combinations with **≥ 3
overlapping annual values** (Stage 1/Ossénatou decision) and only from
calendar years with all 12 months present. **Bias = product (X-BASE) minus
tower**, stated in every row/output.

### Monthly (main result) — `tables/xbase_monthly_stats.csv`

| Flux | Site | Training? | n months | Bias (X-BASE − tower) | RMSE | R² |
|---|---|---|---:|---:|---:|---:|
| NEE | GH-Ank | **yes** | 22 | +1.49 | 1.87 | 0.05 |
| NEE | BJ-Nhu | no | 86 | +1.41 | 2.11 | 0.32 |
| NEE | BJ-Bfg | no | 70 | +3.15 | 3.46 | 0.43 |
| NEE | SN-Dhr | **yes** | 70 | +0.92 | 1.53 | 0.27 |
| NEE | ML-Kem | no | 22 | +1.72 | 2.92 | 0.40 |
| NEE | SN-Nkr (pending) | no | 46 | +1.15 | 1.34 | 0.12 |
| GPP | BJ-Nhu | no | 78 | −1.16 | 2.22 | 0.62 |
| GPP | BJ-Bfg | no | 69 | −3.30 | 3.60 | 0.66 |
| GPP | SN-Dhr | **yes** | 70 | −0.98 | 2.48 | 0.68 |
| GPP | ML-Kem | no | 22 | −3.72 | 4.85 | 0.59 |
| GPP | SN-Nkr (pending) | no | 46 | −1.25 | 1.80 | 0.65 |
| GPP | GH-Ank | **yes** | **not compared — no tower NT data (section 4)** | | | |
| RECO (derived) | BJ-Nhu | no | 78 | +0.26 | 1.36 | 0.47 |
| RECO (derived) | BJ-Bfg | no | 69 | −0.28 | 1.16 | 0.24 |
| RECO (derived) | SN-Dhr | **yes** | 70 | −0.04 | 1.26 | 0.77 |
| RECO (derived) | ML-Kem | no | 22 | −2.15 | 2.66 | 0.34 |
| RECO (derived) | SN-Nkr (pending) | no | 46 | −0.12 | 0.80 | 0.80 |
| LE | GH-Ank | **yes** | 24 | +32.12 | 33.85 | 0.70 |
| LE | BJ-Nhu | no | 100 | −2.92 | 42.95 | 0.24 |
| LE | BJ-Bfg | no | 84 | −19.93 | 38.45 | 0.30 |
| LE | SN-Dhr | **yes** | 77 | +9.15 | 22.36 | 0.82 |
| LE | ML-Kem | no | 23 | −43.36 | 58.59 | 0.62 |
| LE | SN-Nkr (pending) | no | 46 | +12.42 | 22.32 | 0.62 |

(NEE/GPP/RECO in gC m⁻² d⁻¹; LE in W m⁻².)

### Annual (secondary look, ≥ 3 overlapping years only) — `tables/xbase_annual_stats.csv`

Only `BJ-Nhu` LE (n=3), `SN-Dhr` LE (n=3), and `SN-Nkr` (pending; NEE, GPP,
RECO, LE, all n=3) reach the ≥ 3-year threshold — every other site × flux
combination has fewer than 3 full qualifying calendar years within
2001–2021 and is correctly absent from this table (not forced).

A within-site observation, reported rather than resolved: `BJ-Nhu`'s LE bias
flips sign between the full monthly sample (−2.92 W/m², all overlapping
months) and the annual secondary look (+13.49 W/m², restricted to the 3
years with a complete calendar-year record) — the two are different
samples (different years), not a contradiction, but worth knowing before
either number is quoted on its own.

## 7. Draft figures (DRAFT, training sites marked)

- `figures/draft_monthly_scatter.png` — X-BASE vs. tower, one panel per
  flux, sites by colour, 1:1 line, training sites as triangles, pending
  sites as squares.
- `figures/draft_site_map_bias.png` — site map, symbols sized/coloured by
  mean monthly bias, one panel per flux. **Each panel has its own
  colour/size scale** — an initial shared scale across all four panels made
  the carbon panels' bubbles nearly invisible next to LE's much larger
  magnitude; found and fixed before committing.
- `figures/draft_taylor_diagram_{NEE,GPP,RECO,LE}.png` — multi-site Taylor
  diagrams, monthly scale. No Taylor-diagram package (`openair`, `plotrix`)
  is in the renv lockfile; implemented as a small self-contained base-R
  function per CLAUDE.md's "no new dependencies without discussion," rather
  than adding one.

**Checked before committing, per the prompt:** sites are distinguishable by
colour/label in every figure; the training-site triangle marker and
pending-site square marker are visible and legible in all three figure
types; no overlapping labels. The site-map figure specifically needed a
second pass — the shared-scale issue above was caught on first view and
fixed (see figure captions/this section). **Three further issues were found
on this same review pass and fixed in a 2026-11-03 follow-up — see section 8.**

## 8. Fixes and new figures (2026-11-03 follow-up)

Four issues found on review of the Stage 2 outputs above, fixed in place
(`logs/prompts/2026-11-03_wafnet_fluxcom_x_stage2_fixes.md`); no new
downloads, same data/sites/rules as Stage 2:

1. **Wording error.** This report and `docs/methods_memo.md` said RECO was
   derived as "GPP minus net ecosystem exchange." The code (and the rest of
   this report, e.g. section 4's formula) always computed `GPP + NEE` — with
   NEE negative during net uptake (section 4's sign-convention check), `GPP +
   NEE` is GPP **minus the uptake magnitude**, not "GPP minus NEE" directly.
   Fixed in section 4 above, in the plain methods paragraph below, and in
   `08_extract_statistics.R`'s header comment (`methods_memo.md`'s own RECO
   entry was already correct and needed no change).
2. **One shared site colour palette** (`STAGE2_SITE_COLORS`, `00_config.R`)
   now used by the scatter, site-bias map, Taylor diagrams, and the new
   climatology figures below — previously each figure picked its own colours
   independently, so a site's colour changed between figures.
3. **Site map overlap.** `BJ-Nhu`/`BJ-Bfg` (0.12° apart) and `SN-Dhr`/`SN-Nkr`
   (~1.0–1.4° apart, but still overlapping at this map's original scale) had
   symbols sitting on top of each other. Both pairs are now nudged apart by a
   small fixed offset with a thin dashed leader line (site colour) back to
   the true coordinate; the map switched to fill-capable symbols (circle/
   square/triangle with a border) so the **border colour carries site
   identity** (shared palette) while the **fill still carries bias**. The
   map's extent is now trimmed to the sites' own footprint (±2–2.5°) instead
   of a fixed West-Africa-wide box, removing most of the empty margin above
   and below the panels.
4. **Taylor diagram "tower" label.** It sits just below the x-axis (y
   slightly less than 0), which is outside the default R graphics clip
   region — that is what clipped it, not a small-font/overlap issue. Fixed by
   drawing that one annotation with `par(xpd = NA)`.

### New figures: mean seasonal cycle (climatology)

`code/10_climatology.R`, one figure per site (`figures/draft_climatology_<site>.png`,
titled DRAFT): the five agreed sites plus `SN-Nkr` (labelled "pending
Ossénatou" in its own title); `BJ-Db1` has zero X-BASE-overlapping
site-months (section 5) and has no figure. One panel per flux (NEE, GPP,
RECO — titled "RECO (derived)", LE); `GH-Ank`'s GPP and RECO panels are
left out, with the reason given in that figure's caption (no NT-partitioned
tower data at all, section 4). Each panel: tower (shared site colour) and
X-BASE (one fixed colour, all sites) monthly means, built from **exactly the
same QC-passing/2001–2021 site-months already used for the Stage 2
statistics** (both series are columns of the same filtered row set, not
independently filtered — they cover identical months by construction).
Months with ≥ 3 contributing years get a mean ± 1 SD ribbon; thinner months
show the individual years as open points instead of a ribbon. The number of
contributing years is printed along the bottom of every panel. Monthly
means, SDs, and year counts: `tables/xbase_climatology.csv`.

**How the X-BASE seasonal cycle differs from the tower's, one sentence per
site (numbers from `tables/xbase_climatology.csv`):**

- **`GH-Ank`** (training site; NEE/LE only): X-BASE's NEE seasonal cycle is
  far flatter than the tower's (amplitude 1.21 vs. 3.60 gC m⁻² d⁻¹) and its
  LE is both flatter (amplitude 29.6 vs. 54.1 W m⁻²) and consistently higher
  than the tower across the year (X-BASE 78–108 W m⁻² vs. tower 32–86 W m⁻²),
  consistent with this site's strong positive LE bias in section 6.
- **`BJ-Nhu`**: X-BASE's GPP/RECO peak one month earlier than the tower's
  (August vs. September) and both are damped (GPP amplitude 4.78 vs. 7.34,
  RECO 2.34 vs. 3.83 gC m⁻² d⁻¹); X-BASE's LE seasonal cycle peaks in **May**
  while the tower's peaks in **September** — a clearly different shape, not
  just a damped version of the same one — and is damped overall (amplitude
  57.0 vs. 77.0 W m⁻²).
- **`BJ-Bfg`**: same August-vs-September GPP/RECO peak shift as `BJ-Nhu`
  (GPP amplitude 4.85 vs. 6.01 gC m⁻² d⁻¹); LE again peaks far earlier in
  X-BASE (May) than the tower (October), damped (58.1 vs. 65.7 W m⁻²).
- **`SN-Dhr`** (training site): the only site where GPP, RECO, and LE all
  peak in the **same month** (September) in both series, but every
  amplitude is still roughly half the tower's (GPP 4.69 vs. 8.92, RECO 3.81
  vs. 5.68 gC m⁻² d⁻¹; LE 73.9 vs. 99.2 W m⁻²) — X-BASE tracks the timing
  well here but under-states the swing.
- **`ML-Kem`**: the largest damping of any site — X-BASE's NEE amplitude is
  0.84 gC m⁻² d⁻¹ against the tower's 7.39 (about **11%** of the tower's
  swing), and its GPP amplitude (3.11) is about 26% of the tower's (11.79);
  peak timing is also off by one to two months for GPP/RECO (August vs.
  September/October). The dry-season (Jan–Mar) GPP/RECO/LE values are
  broadly comparable between the two series; nearly the entire discrepancy
  is in how much each series rises in the wet season.
- **`SN-Nkr`** (pending Ossénatou): GPP/RECO/LE peak month matches (September)
  as at `SN-Dhr`, with the same damped-amplitude pattern (GPP 4.14 vs. 6.51,
  RECO 3.18 vs. 4.65 gC m⁻² d⁻¹; LE 62.2 vs. 81.5 W m⁻²).

**Pattern across sites:** X-BASE's seasonal amplitude is smaller than the
tower's at every site and flux checked here — this is the dominant, most
consistent difference, more consistent across sites than any peak-timing
shift. Peak-month timing matches at the two training sites and `SN-Nkr`
(all September) but is one to two months early at the two Bénin sites and
`ML-Kem`, and LE's seasonal *shape* (not just amplitude) differs outright at
`BJ-Nhu`/`BJ-Bfg`, where X-BASE peaks in May against the tower's
September/October peak.

**Figures checked before committing:** every regenerated and new PNG was
opened and checked for: symbols/labels not overlapping (including the
nudged map pairs and the climatology panels' per-month year counts, all
legible at the saved resolution), both series and their bands visible in
every climatology panel (including the all-thin-month case, `ML-Kem`, where
every month falls back to individual-year points rather than a band), and
site colours matching across the scatter, map, Taylor, and climatology
figures (spot-checked directly against each other, not just against the
palette definition). One further issue was caught and fixed during this
check: the climatology figures' bottom caption text ran off the right edge
of the image on the first render (no automatic wrapping) — fixed with
`stringr::str_wrap()`.

## How the product change affects the comparison

Relative to the original FLUXCOM product Stage 1 scoped out (and the
decisions-needed list it left open):

- **Reco can no longer be compared directly.** The original FLUXCOM carbon
  line has its own TER/Reco variable; X-BASE does not, so Reco here is
  **derived** (GPP + NEE), not measured-against-measured.
- **H can never be compared with X-BASE at all** — the original product's
  energy line had H; X-BASE has no energy variable beyond the ET/ET_T water
  fluxes converted here to LE.
- **GPP/Reco are NT-only**, not the DT comparison Stage 1 had assumed would
  be available from the original product's DT member — this is a forced,
  not a chosen, partition match.
- **`SN-Nkr` gains a usable comparison** (46 months / 3 years) it never had
  against the original product (zero overlap at any resolution, Stage 1).
  `BJ-Db1` gains nothing — its record postdates even X-BASE's longer period.
- **`GH-Ank`'s training-site caveat is unchanged** in kind (it was
  training-set-likely for the original product too) but is now
  **confirmed**, not "likely," and is compounded by its missing NT tower
  data, which Stage 1 could not have anticipated since that check requires
  the specific NT/DT partition this step established X-BASE needs.

## Plain methods paragraph (for Ossénatou's paper)

> We compared the five WAF-Net eddy-covariance sites (plus a sixth and
> seventh re-checked for a shorter overlap) against FLUXCOM-X-BASE (Nelson
> et al., 2024), a 0.5°, monthly, machine-learning-based global carbon and
> water flux product covering 2001–2021, obtained from the ICOS Carbon
> Portal (CC BY 4.0). Tower gross primary productivity and ecosystem
> respiration were matched to the product's nighttime-partitioning method,
> since X-BASE's own documentation states its GPP training targets came
> from nighttime partitioning only; X-BASE has no respiration variable of
> its own, so its respiration was derived as GPP plus net ecosystem
> exchange (equivalently, GPP minus net uptake, since net ecosystem
> exchange is negative during net uptake in the sign convention used here,
> confirmed empirically to match the tower's own convention). Evapotranspiration was converted to latent
> heat flux using a standard latent heat of vaporization
> (2.45 × 10⁶ J kg⁻¹); sensible heat flux has no equivalent in X-BASE and
> was not compared. Bias (product minus tower), RMSE, and R² were computed
> per site and flux at monthly resolution (the primary comparison) and, for
> sites with at least three complete overlapping years, at annual resolution
> as a secondary check. Two of the five agreed sites (`GH-Ank`, `SN-Dhr`)
> were confirmed to be in X-BASE's own training set, so the comparison at
> those sites is partly within-sample rather than fully independent; this is
> marked in every figure.

## Checks before writing this report

- All 84 X-BASE files sha256-verified (section 1).
- NEE sign convention checked against actual downloaded data, not assumed
  (section 4).
- GPP partitioning and training-site answers read directly from
  machine-readable sources (ICOS metadata description field; publisher's
  own Table 1 XLSX), not inferred — both supersede Stage 1's lower-confidence
  answers (sections 3).
- Figures opened and checked for legibility before committing; one real
  issue (shared bias scale across panels) was found and fixed (section 7).
- **2026-11-03 follow-up:** every regenerated figure (scatter, map, four
  Taylor diagrams) and every new climatology figure (six sites) opened and
  checked again after the fixes in section 8 — site colours cross-checked
  against each other across all four figure types, map leader-line
  separation and climatology panel/caption legibility confirmed, one further
  caption-wrapping issue found and fixed in the same pass (section 8).

---

## Sources consulted (Stage 2, in addition to Stage 1's list)

ICOS Carbon Portal metadata SPARQL endpoint (`meta.icos-cp.eu/sparql`) and
object/collection landing pages; `data.icos-cp.eu` licence-acceptance and
object-download endpoints; Nelson et al. (2024) full text
(`bg.copernicus.org/articles/21/5079/2024/`) and its Table 1 XLSX; ILAMB
reference data collection (`ilamb.org/ILAMB-Data/DATA/`); the downloaded
netCDF files themselves (`terra`, `ncdf4`), read directly for units, time
coverage, and global attributes rather than assumed from any paper text.
