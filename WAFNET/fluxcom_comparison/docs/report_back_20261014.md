# WAFNET x FLUXCOM comparison — Stage 1 pre-analysis report (2026-10-14)

Requested by Ossénatou Mamadou (WAF-Net paper), via David Moore. Produced by
`code/01_observations_qc_overlap.R`, `code/02_grid_landcover_context.R`, and
`code/03_product_overlap.R`, plus web research into the FLUXCOM product
family (see "Sources consulted" at the end).

**No gridded FLUXCOM product has been downloaded and no comparison statistic
(bias/RMSE/R²/Taylor diagram) has been computed.** This report only
establishes what is available, what overlaps, and what needs to be decided
before Stage 2 (download + compute) begins. **FLUXCOM is non-Shuttle
comparison data under CLAUDE.md Hard Rule #1** — every table and figure this
analysis eventually produces must be labelled as comparing against a FLUXCOM
comparison dataset, never as using FLUXCOM as primary Annual Paper data.

Sites: `GH-Ank`, `BJ-Db1`, `BJ-Nhu`, `BJ-Bfg`, `SN-Nkr`, `SN-Dhr` (see
`../README.md`). Variables: NEE, GPP (daytime partition, `GPP_DT`), Reco
(daytime partition, `RECO_DT`), H, LE. VUT/CUT per site follows the Annual
Paper's rule in `R/site_annual_fluxes.R` (VUT if the site has any non-NA
`NEE_VUT_REF_QC`, else CUT) — **all six WAFNET sites resolve to VUT**, so no
VUT/CUT mixing question arises here (see `tables/site_carbon_source.csv`).

---

## 1. FLUXCOM products

Full detail, including per-forcing-dataset breakdowns for RS+METEO, is in
`tables/fluxcom_products.csv`. Summary:

| Product | Variables | Reco/TER direct? | H direct? | Period | Spatial res. | Temporal res. (distributed) | Daytime-partition member? | Citation (DOI) |
|---|---|---|---|---|---|---|---|---|
| **FLUXCOM-X-BASE** | GPP, NEE, ET, ETT | **NO** (only implicit as NEE − GPP) | **NO** (ET only, no LE/H) | 2001–2020 (paper) / 2001–2021 (ICOS archive — discrepancy not resolved) | 0.05° (~5.5 km) headline; 0.5° also archived | Monthly (easy access); hourly exists as Zarr, access path not confirmed | **not found** — partitioning is nighttime-only (Reichstein et al. 2005) | Nelson et al. (2024), 10.5194/bg-21-5079-2024 |
| **Carbon RS** | GPP, TER, NEE (+MAD) | **YES** | n/a | 2001–2015 | 0.0833° (~9 km) | Monthly via FTP; finer by request | **YES** — `FP-DT`/`FP-NT`/`FP-ALL`/`FP-NONE` label | Tramontana et al. (2016), 10.5194/bg-13-4291-2016 |
| **Carbon RS+METEO** | GPP, TER, NEE (+MAD, member count); reuses RS-trained models | **YES** | n/a | Forcing-dependent: CRU JRA 1950–2017 (longest), GSWP3 1950–2010, WFDEI 1979–2013, ERA5 1979–2018, CERES-GPCP 2001–2013 | 0.5° (~55 km) | Monthly via FTP; finer by request | **YES** — same `FP-DT`/`FP-NT`/`FP-ALL` labelling | Jung et al. (2020), 10.5194/bg-17-1343-2020 |
| **Energy RS** | LE, H, Rn (+MAD) | n/a | **YES** | 2001–2015 (headline stats to 2013) | 0.0833° native; 0.5° also distributed | Monthly via FTP; finer by request | **Not applicable** — no day/night axis for energy; ensemble axes are ML method × energy-balance-closure correction | Tramontana et al. (2016) method; Jung et al. (2019) product, 10.1038/s41597-019-0076-8 |
| **Energy RS+METEO** | LE, H, Rn (+MAD, member count); reuses RS-trained models | n/a | **YES** | Forcing-dependent: CRUNCEPv8 1950–2016 (longest), WFDEI 1979–2013, GSWP3 1950–2014, CERES-GPCP 2001–2014 | 0.5° (~55 km) | Monthly via FTP; finer by request | **Not applicable**, same reasoning as Energy RS | Jung et al. (2019), 10.1038/s41597-019-0076-8 |

Key points for this comparison:

- **FLUXCOM-X-BASE has no Reco/TER and no H.** It cannot be used for the
  Reco or H comparisons at all; NEE and GPP only, and LE would have to be
  approximated from its `ET`/`ETT` water-flux variables (not an energy
  variable in native units — a conversion/assumption, not a direct match).
- **Only original FLUXCOM (both RS and RS+METEO, carbon line) has a true
  daytime-partitioned (`FP-DT`) GPP/TER member**, directly analogous to the
  Annual Paper's `GPP_DT`/`RECO_DT`. FLUXCOM-X-BASE does not — a like-for-like
  daytime-partition comparison is therefore only possible against the
  **original** FLUXCOM carbon line, not FLUXCOM-X-BASE.
- **No product exposes a point-extraction API** (no THREDDS/OPeNDAP/point
  service was found for any line); every option requires full-grid file
  downloads even to extract six points. Access-burden order of magnitude:
  X-BASE ~5 MB/variable/year at 0.5° (0.05° unmeasured, plausibly 100s of
  MB/file); original FLUXCOM carbon+energy combined FTP archives are
  **hundreds of GB** at monthly/0.5°–0.0833° resolution.
- Licence is **CC BY 4.0** across every line found.

---

## 2. Training sites

Full detail in `tables/fluxcom_training_sites.csv`. No product distributes a
machine-readable training-site list; the answer for original FLUXCOM comes
from a direct text-search of the Tramontana et al. (2016) supplement Table
S1 (high confidence); the answer for FLUXCOM-X-BASE comes from matching the
paper's reference list, since Table 1 itself renders as an un-OCR'd image in
the HTML article (medium confidence only).

| Site | FLUXCOM-X-BASE | Original FLUXCOM (carbon & energy, RS & RS+METEO — identical training set across all four) |
|---|---|---|
| `GH-Ank` | likely **YES** | **YES** (confirmed in Table S1) |
| `BJ-Db1` | likely NOT included | **NOT found** |
| `BJ-Nhu` | likely NOT included | **NOT found** |
| `BJ-Bfg` | likely NOT included | **NOT found** |
| `SN-Nkr` | likely NOT included | **NOT found** |
| `SN-Dhr` | likely **YES** | **NOT found** |

Other African sites found in each training set, for calibration: X-BASE
includes `CG-Tch` (Congo) in addition to `GH-Ank`/`SN-Dhr`; original FLUXCOM
includes nine African sites (`BW-Ghg`, `BW-Ghm`, `BW-Ma1`, `CG-Hin`, `CG-Kis`,
`CG-Tch`, `GH-Ank`, `ML-AgG`, `SD-Dem`). `ZM-Mon` (Zambia) was explicitly
searched for in both and **not found** in either.

**Implication:** `GH-Ank` was very likely used to train every FLUXCOM
product line being considered, and `SN-Dhr` was likely used to train
FLUXCOM-X-BASE specifically. A comparison at these two sites is partly a
**within-sample** check, not an independent validation, for those product
lines. The other four sites (`BJ-Db1`, `BJ-Nhu`, `BJ-Bfg`, `SN-Nkr`) were not
found in any training list and give a genuine out-of-sample comparison.

---

## 3. Observations (repo-root YY/MM extracts, read-only)

Per-site, per-variable QC-passing years and months, using the same gating
rule as `R/site_annual_fluxes.R` (`(1 - QC) <= QC_THRESHOLD_YY/MM`, currently
0.50 at both resolutions), with GPP_DT/RECO_DT gated on the site's NEE QC
column (neither has a QC column of its own at MM/YY — confirmed against the
FLUXMET header). Full qualifying-period lists are in
`tables/site_variable_qc_overlap_{yy,mm}.csv`.

**Annual (YY):**

| Site | NEE/GPP_DT/RECO_DT n years (range) | LE n years (range) | H n years (range) | Native units |
|---|---|---|---|---|
| `GH-Ank` | 1 (2014) | 1 (2012) | 3 (2011–2014) | carbon: gC m⁻² y⁻¹; LE/H: W m⁻² |
| `BJ-Db1` | 2 (2023–2024) | 2 (2023–2024) | 2 (2023–2024) | same |
| `BJ-Nhu` | 6 (2008–2017) | 6 (2008–2017) | 7 (2008–2017) | same |
| `BJ-Bfg` | 3 (2009–2014) | 4 (2009–2017) | 4 (2009–2017) | same |
| `SN-Nkr` | 6 (2019–2024) | 6 (2019–2024) | 6 (2019–2024) | same |
| `SN-Dhr` | 3 (2012–2022) | 4 (2012–2022) | 4 (2012–2022) | same |

**Monthly (MM):**

| Site | NEE/GPP_DT/RECO_DT n months (range) | LE n months (range) | H n months (range) | Native units |
|---|---|---|---|---|
| `GH-Ank` | 22 (2011-02–2014-11) | 24 (2011-02–2014-11) | 28 (2011-01–2014-12) | carbon: gC m⁻² d⁻¹ (mean daily rate); LE/H: W m⁻² |
| `BJ-Db1` | 32 (2022-05–2024-12) | 32 | 32 | same |
| `BJ-Nhu` | 86 (2008-01–2017-11) | 100 | 106 | same |
| `BJ-Bfg` | 69–70 (2008-07–2017-10/11) | 84 | 84 | same |
| `SN-Nkr` | 80 (2018-03–2024-12) | 80 | 80 | same |
| `SN-Dhr` | 82 (2010-08–2022-12) | 89 | 90 | same |

**Conversions to apply** (per `R/units.R` / CLAUDE.md's unit table, via
`fluxnet_convert_units()` — not yet run, since Stage 1 computes nothing):
NEE/GPP_DT/RECO_DT at YY pass through unchanged (already a pre-integrated
gC m⁻² yr⁻¹ total); at MM, the native `gC m⁻² d⁻¹` mean daily rate must be
multiplied by the actual number of days in that calendar month to get a
monthly total, for comparison against FLUXCOM's monthly GPP/NEE/TER (also
distributed as totals, not mean daily rates — to be confirmed exactly once
Stage 2 reads an actual FLUXCOM file). LE (`W m⁻²`) converts to mm H₂O per
period; H (`W m⁻²`) converts to MJ m⁻² per period if compared on an energy
basis, or is left as a mean rate if compared directly against FLUXCOM's own
`W m⁻²` `H`/`LE` outputs (likely the simpler and preferred choice, since
FLUXCOM's energy variables are themselves mean-rate `W m⁻²`, not integrated
totals — to be confirmed in Stage 2).

**Note on `BJ-Db1` and `SN-Nkr`:** at YY resolution, `BJ-Db1`'s first year
(2022) does not qualify at all — only 2023–2024 do — even though monthly data
exists from 2022-05 onward (32 qualifying months starting 2022-05). `SN-Nkr`
similarly starts qualifying at YY only from **2019**, not 2018, despite
monthly qualifying data existing from 2018-03. Both are consistent with a
partial first calendar year not accumulating enough measured/well-gap-filled
half-hours to clear the annual QC threshold, even though individual months
within that year do clear the monthly threshold.

---

## 4. Overlap

Full per-variable breakdown (including all five RS+METEO/forcing variants)
is in `tables/site_variable_product_overlap_{yy,mm}.csv`. The table below
uses, for each RS+METEO line, its single longest-available forcing variant
("best case" — `CRU JRA` for carbon 1950–2017, `CRUNCEPv8` for energy
1950–2016); every other forcing variant gives equal or **less** overlap.
Counts are identical across NEE/GPP_DT/RECO_DT (same QC gate) and are shown
once; LE and H are shown separately since their QC gate (and, for `BJ-Nhu`/
`BJ-Bfg`/`SN-Dhr`, their qualifying-year count) differs from NEE's.

**Annual (YY) overlapping years / total qualifying years:**

| Site | X-BASE (2001–2021) | Carbon RS (2001–2015) | Carbon RS+METEO best (1950–2017) | Energy RS (2001–2015) | Energy RS+METEO best (1950–2016) |
|---|---|---|---|---|---|
| `GH-Ank` NEE/GPP/RECO | 1/1 | 1/1 | 1/1 | 1/1 | 1/1 |
| `GH-Ank` LE | 1/1 | 1/1 | 1/1 | 1/1 | 1/1 |
| `GH-Ank` H | 3/3 | 3/3 | 3/3 | 3/3 | 3/3 |
| **`BJ-Db1` NEE/GPP/RECO** | **0/2** | **0/2** | **0/2** | **0/2** | **0/2** |
| **`BJ-Db1` LE, H** | **0/2** | **0/2** | **0/2** | **0/2** | **0/2** |
| `BJ-Nhu` NEE/GPP/RECO | 6/6 | 4/6 | 6/6 | 4/6 | 5/6 |
| `BJ-Nhu` LE | 6/6 | 4/6 | 6/6 | 4/6 | 5/6 |
| `BJ-Nhu` H | 7/7 | 5/7 | 7/7 | 5/7 | 6/7 |
| `BJ-Bfg` NEE/GPP/RECO | 3/3 | 3/3 | 3/3 | 3/3 | 3/3 |
| `BJ-Bfg` LE, H | 4/4 | 3/4 | 4/4 | 3/4 | 3/4 |
| **`SN-Nkr` NEE/GPP/RECO** | **3/6** | **0/6** | **0/6** | **0/6** | **0/6** |
| **`SN-Nkr` LE, H** | **3/6** | **0/6** | **0/6** | **0/6** | **0/6** |
| `SN-Dhr` NEE/GPP/RECO | 2/3 | 1/3 | 2/3 | 1/3 | 1/3 |
| `SN-Dhr` LE | 3/4 | 1/4 | 3/4 | 1/4 | 2/4 |
| `SN-Dhr` H | 3/4 | 1/4 | 3/4 | 1/4 | 2/4 |

**`BJ-Db1` (requested flag): zero annual overlap with every FLUXCOM product
line considered**, for every variable — its only qualifying years (2023,
2024) postdate even the most generous product period (X-BASE's ICOS-archive
bound, 2001–2021). Monthly overlap is also zero against every product for
the same reason (its qualifying months, 2022-05–2024-12, are entirely after
2021-12). `BJ-Db1` cannot be compared against any current FLUXCOM release at
either resolution unless a future FLUXCOM-X release extends past 2021, or the
comparison is restricted to a different variable/method entirely (e.g. a
near-real-time product not covered by this report).

**`SN-Nkr` (requested flag): only FLUXCOM-X-BASE overlaps at all** (3 of 6
qualifying years — 2019, 2020, 2021 — within its 2001–2021 archive bound;
only 2 of 6 — 2019, 2020 — if the stricter 2001–2020 paper-text bound is
used instead; see the X-BASE period discrepancy noted in section 1).
Original FLUXCOM (any line, any forcing) has **zero** overlap with `SN-Nkr`
at either resolution, since the site's own qualifying record starts in 2018
(monthly) / 2019 (annual), after every original-FLUXCOM period ends
(2015–2017 depending on line/forcing).

**Monthly (MM) overlapping months / total qualifying months**, same column
order:

| Site | X-BASE | Carbon RS | Carbon RS+METEO best | Energy RS | Energy RS+METEO best |
|---|---|---|---|---|---|
| `GH-Ank` NEE/GPP/RECO | 22/22 | 22/22 | 22/22 | 22/22 | 22/22 |
| `GH-Ank` LE | 24/24 | 24/24 | 24/24 | 24/24 | 24/24 |
| `GH-Ank` H | 28/28 | 28/28 | 28/28 | 28/28 | 28/28 |
| **`BJ-Db1` (all vars)** | **0/32** | **0/32** | **0/32** | **0/32** | **0/32** |
| `BJ-Nhu` NEE/GPP/RECO | 86/86 | 67/86 | 86/86 | 67/86 | 78/86 |
| `BJ-Nhu` LE | 100/100 | 79/100 | 100/100 | 79/100 | 90/100 |
| `BJ-Nhu` H | 106/106 | 86/106 | 106/106 | 86/106 | 97/106 |
| `BJ-Bfg` NEE | 70/70 | 58/70 | 70/70 | 58/70 | 63/70 |
| `BJ-Bfg` GPP_DT/RECO_DT | 69/69 | 57/69 | 69/69 | 57/69 | 62/69 |
| `BJ-Bfg` LE, H | 84/84 | 67/84 | 84/84 | 67/84 | 73/84 |
| **`SN-Nkr` (all vars)** | **46/80** | **0/80** | **0/80** | **0/80** | **0/80** |
| `SN-Dhr` NEE/GPP/RECO | 70/82 | 31/82 | 49/82 | 31/82 | 40/82 |
| `SN-Dhr` LE | 77/89 | 32/89 | 56/89 | 32/89 | 44/89 |
| `SN-Dhr` H | 78/90 | 33/90 | 57/90 | 33/90 | 45/90 |

(The `n_qualifying_total` and `n_overlapping_with_product` columns for
`GPP_DT`/`RECO_DT` occasionally differ from `NEE` by 1 at `BJ-Bfg` monthly —
one month has a qualifying NEE but a missing `GPP_DT`/`RECO_DT` value; see
`tables/site_variable_qc_overlap_mm.csv`, `BJ-Bfg` rows.)

---

## 5. Grid scale

No land-cover product in this repo has been aggregated to any FLUXCOM
product's native grid cell (0.05°/0.0833°/0.5°, depending on line) for these
six sites — the closest proxies already computed are:

- **MCD12C1 MODIS IGBP** (`data/snapshots/site_igbp_fig4.csv`): a genuine
  **0.05° (~5.6 km) grid-cell** dominant-class value — close in scale to
  FLUXCOM-X-BASE's 0.05° grid, somewhat finer than its 0.0833°-distributed
  variant, and much finer than original FLUXCOM's 0.5° grid.
- **ESA CCI Land Cover** (`data/snapshots/site_landcover_cci.csv` [v2.0.7,
  2015] and `site_landcover_cci_current_767.csv` [v2.1.1, 2022]): the
  **exact 300 m pixel** at the tower coordinate, not a dominant-class
  aggregate over any larger cell.

| Site | PI IGBP | MODIS-cell IGBP (0.05°) | PI/MODIS agree? | CCI 2015 class (300 m pixel) | CCI 2022 class (300 m pixel) |
|---|---|---|---|---|---|
| `GH-Ank` | EBF | EBF | **TRUE** | Tree cover, broadleaved evergreen, closed-open (>15%) | same |
| `BJ-Db1` | CRO | CVM (cropland/vegetation mosaic) | **FALSE** | Mosaic cropland(>50%)/natural veg | Cropland, rainfed |
| `BJ-Nhu` | SAV | CRO | **FALSE** | Shrubland | Herbaceous cover |
| `BJ-Bfg` | DBF | CRO | **FALSE** | Tree cover, broadleaved deciduous, open (15-40%) | same |
| `SN-Nkr` | CRO | GRA | **FALSE** | Cropland, rainfed | same |
| `SN-Dhr` | GRA | GRA | **TRUE** | Mosaic cropland(>50%)/natural veg | same |

**Four of six sites (`BJ-Db1`, `BJ-Nhu`, `BJ-Bfg`, `SN-Nkr`) show PI-IGBP vs.
MODIS-grid-cell disagreement**, even at 0.05° — a scale where the mismatch
is already a known heterogeneous-tropical-savanna/cropland-mosaic problem for
these sites (and matches the manifest's IGBP-classification caveats
elsewhere in the paper's own representativeness work, e.g.
`scripts/figure4_representativeness.R`). At FLUXCOM's coarser 0.5° grid
(original FLUXCOM carbon/energy products), the mismatch is expected to be at
least as large, likely larger, given the known fine-grained patchwork of
cropland/savanna/forest at these sites — but this has not been computed,
since no 0.5°-aggregated land-cover layer exists in the repo for these
points.

---

## 6. Decisions needed

1. **Which product/combination per variable.** FLUXCOM-X-BASE is the only
   line with a current period that reaches `SN-Nkr`'s (partial) and remains
   closest to `BJ-Db1`'s record — but it has no Reco/TER and no H, and its
   2020-vs-2021 end-year discrepancy between the paper text and the ICOS
   archive needs resolving before any overlap number from it is treated as
   final. Original FLUXCOM (RS or RS+METEO, carbon line) is the only source
   with a true daytime-partitioned Reco/GPP member, but its period (2001–
   2015/2017 depending on forcing) never reaches `BJ-Db1` or `SN-Nkr` at all,
   and only partially covers the later years at `BJ-Nhu`/`BJ-Bfg`/`SN-Dhr`.
   **No single FLUXCOM product line covers every variable at every site** —
   a decision is needed on whether to (a) use X-BASE for NEE/GPP only and
   original FLUXCOM for Reco/LE/H where its period allows, accepting a
   different product per variable, (b) restrict the whole comparison to the
   four sites/periods where overlap is adequate and report `BJ-Db1`/`SN-Nkr`
   as "insufficient overlap" rather than force a number, or (c) something
   else (e.g. request the finer, by-request-only FLUXCOM temporal products
   to see whether period coverage is better than publicly listed — unlikely,
   since period is a training/output constraint, not a resolution one).
2. **How to show training-site membership in figures.** `GH-Ank` (all
   products) and `SN-Dhr` (X-BASE only) were very likely in-sample for the
   product(s) being compared against. Needs a decision on whether to mark
   these points distinctly (e.g. open vs. filled symbol) in every scatter/
   Taylor-diagram figure, exclude them from a "validation" framing while
   still showing them, or caveat them only in text — my recommendation
   would be a distinct symbol, but this is the PI's call, not assumed here.
3. **What to do where overlap is short.** `BJ-Db1` has zero usable overlap
   at any resolution against any FLUXCOM line and cannot be compared under
   current product releases — decide whether to drop it entirely from the
   FLUXCOM comparison (keeping it in any other WAFNET analysis), flag it as
   "excluded, no temporal overlap" in the site map/tables, or wait for a
   future product release. `SN-Nkr` has usable overlap only against X-BASE
   (3 of 6 years / 46 of 80 months, and only if the generous 2001–2021 bound
   is accepted) — decide whether a 3-year annual sample is long enough to
   report a per-site bias/RMSE/R² at all, or whether it should be flagged as
   low-confidence/not reported at annual scale while still usable at monthly
   scale.
4. **The X-BASE period discrepancy (2001–2020 vs. 2001–2021) needs resolving**
   before Stage 2 — it directly changes `SN-Nkr`'s annual overlap count
   above (3 of 6 years under the 2001–2021 archive bound vs. 2 of 6 under
   the 2001–2020 paper-text bound; `SN-Dhr` is unaffected either way, since
   its qualifying years, 2012/2017/2022, straddle both bounds the same way).
   This should be confirmed against the actual downloaded file's time
   dimension in Stage 2, not assumed from the paper text or the archive
   listing alone.
5. **Energy-variable unit/basis to compare on** (noted in section 3): decide
   whether H/LE are compared as native `W m⁻²` mean rates (simpler, and
   matches FLUXCOM's own likely native units) or converted to integrated
   `MJ m⁻²`/`mm H₂O` totals per `R/units.R` convention — to be confirmed once
   an actual FLUXCOM energy file's units are inspected in Stage 2.

---

## Sources consulted (FLUXCOM research)

Successfully opened: Nelson et al. (2024, *Biogeosciences* 21:5079–5115,
10.5194/bg-21-5079-2024); ICOS Carbon Portal FLUXCOM-X-BASE collection
(10.18160/5NZG-JMJE) and an example data object; Jung et al. (2019,
*Scientific Data* 6:74, 10.1038/s41597-019-0076-8) via PMC and its arXiv
preprint; fluxcom.org energy and carbon product/download pages; Tramontana
et al. (2016, *Biogeosciences* 13:4291–4313, 10.5194/bg-13-4291-2016) main
text and supplement PDF (text-extracted for the training-site table); Jung
et al. (2020, *Biogeosciences* 17:1343–1365, 10.5194/bg-17-1343-2020) main
text and supplement PDF.

Tried but could not usefully access: the FLUXCOM-X data Zarr/DKRZ GitLab
repository (README did not render; Zarr/S3 point-access mechanism for
FLUXCOM-X-BASE is unconfirmed); the ICOS data-portal SPA shell; Nelson et
al. (2024) Table 1 (rendered as an image, not OCR-read — the X-BASE
training-site answer above is a reference-list proxy, not a direct read of
that table); `fluxcom.org/EF-ReleaseInfo/` (HTTP 404); the MPI-BGC data
portal registration page (HTTP 404; not logged into, per read-only research
scope); the exact FileID/portal URL for the original FLUXCOM **carbon**
product specifically (the energy product's FileID 257 was confirmed via
arXiv:1812.04951, but no carbon equivalent was found).
