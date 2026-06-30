# Methods: IGBP-class flux median analysis

*Living document — decisions appended as they are made.*
*First created: 2026-06-30. Source script: `scripts/figure_flux_medians_by_igbp.R`.*

---

## Data source

All flux data are drawn from the FLUXNET Shuttle YY (yearly) product
(FLUXMET_YY v1.3_r1). The active snapshot is
`fluxnet_shuttle_snapshot_20260624T095651.csv` (767 sites). Per-site annual
medians were computed by `scripts/assess_flux_data_by_igbp_shuttle.R` and
stored in `data/snapshots/site_flux_medians_shuttle.csv`.

---

## Variables and source preferences

**NEP.** Net ecosystem production is computed as NEP = −NEE (sign flip; positive
values indicate net carbon uptake). The VUT (variable ustar threshold)
partitioning variant is preferred: `NEE_VUT_REF`. Sites where VUT is unavailable
or fails QC fall back to `NEE_CUT_REF` (constant ustar threshold). The
VUT/CUT decision is made per site-year and applied jointly to NEE, GPP, and TER
for that year.

**GPP.** Gross primary productivity uses nighttime (NT) partitioning as the
first preference: `GPP_NT_VUT_REF`, with VUT/CUT fallback following the NEE
decision. When NT partitioning yields zero qualifying years for a site, the
daytime (DT) partitioning product is used as a site-level fallback:
`GPP_DT_VUT_REF` (or `GPP_DT_CUT_REF` if the CUT fallback applies). The
choice between NT and DT is made per site, not per year, to avoid mixing
partitioning methods within a site's median calculation. The method used is
recorded in the `gpp_partition` column ("NT" or "DT").

**TER.** Total ecosystem respiration follows the same NT-preferred, DT-fallback
policy as GPP. Source columns: `RECO_NT_VUT_REF` preferred, DT fallback via
`RECO_DT_VUT_REF`. Method recorded in `ter_partition`.

**ET.** Evapotranspiration is derived from the gap-filled latent heat flux:
`LE_F_MDS [W m⁻²] × 31 557 600 [s yr⁻¹] / λ [J kg⁻¹]`, where
λ = 2.45 × 10⁶ J kg⁻¹ is the latent heat of vaporisation of water. This
yields ET in mm yr⁻¹ (using the identity 1 kg m⁻² = 1 mm water depth).
Annual means of LE_F_MDS are used; the QC flag is `LE_F_MDS_QC`.

**H.** Sensible heat flux uses `H_F_MDS [W m⁻²]`; no unit conversion is
applied. The annual value is the mean over gap-filled hourly/half-hourly
records. QC flag is `H_F_MDS_QC`.

---

## QC threshold

A QC fraction threshold of **≥ 0.80** is applied to each variable's QC flag
independently. For NEE, GPP (NT), and TER (NT), the relevant flag is
`NEE_VUT_REF_QC` (or its CUT equivalent). The DT partitioning columns
(`GPP_DT_VUT_REF`, `RECO_DT_VUT_REF`) carry no dedicated QC flag at the YY
resolution; their quality is therefore gated by the same NEE QC flag used in
the VUT/CUT decision. For LE and H, the QC flags `LE_F_MDS_QC` and
`H_F_MDS_QC` are applied independently at the same 0.80 threshold.

Site-years that do not meet the threshold for a given variable are excluded
from that variable's median calculation; they are not excluded from other
variables. A site-year can contribute to ET but not to NEP if only the LE QC
threshold is met.

---

## Per-site aggregation

The median across all qualifying years (years passing the QC threshold) is
computed separately for each variable at each site. Sites with zero qualifying
years for a variable contribute NA to the cross-site summary for that variable.
The per-site median is the unit of analysis in all cross-class comparisons; it
de-emphasises temporal outliers and year-to-year variability within a site.

---

## IGBP class scheme

The 12 standard FLUXNET IGBP classes are used: EBF, MF, DBF, ENF, CSH, OSH,
WSA, SAV, GRA, WET, CRO, CVM. An additional 22 shuttle sites carry
non-standard labels (BSV — barren/sparse vegetation; DNF — deciduous
needleleaf forest; SNO — permanent snow/ice). These sites are excluded from
the 12-class summary and reported collectively as "other" in figure legends.

---

## Cross-class aggregation

The reported class value is the **median of site medians** — the median across
all sites in a class, where each site contributes its own per-site median.
This estimator weights each site equally regardless of record length, which is
appropriate for a network representativeness assessment. Standard deviation
reported alongside each class median is likewise computed across site medians
(not across site-years).

---

## Site-count caveat for EBF

Evergreen broadleaf forest sites present a partitioning data challenge.
Of 42 EBF sites in the shuttle network, 38 have qualifying NEP data. For GPP
and TER, NT partitioning succeeds at 22 sites; an additional 15 sites are
recovered via DT fallback, giving 37 total. The 15 DT-recovered sites are
predominantly tropical sites in Africa, Southeast Asia, and the Americas where
nighttime partitioning is unreliable. Including DT-recovered sites raises the
EBF GPP class median from 1845 to 2326 gC m⁻² yr⁻¹ (+26%). This shift
should be noted in figure legends; the companion table records `gpp_partition`
for each site so readers can reproduce NT-only or DT-only subsets.

---

## Class ordering convention

The fixed class order used in all flux figures is, reading left-to-right in
two rows:

- **Top row:** EBF, MF, DBF, ENF, CSH, OSH
- **Bottom row:** WSA, SAV, GRA, WET, CRO, CVM

This order places tall-stature forested ecosystems first, followed by
shrublands and open ecosystems. It matches the reference figure from
Moore AGU 2025 (BG22G-08) and will carry through to the silhouette versions
of these figures.

---

## Figure normalization

Each flux figure is independently normalised: the IGBP class with the highest
median value within that flux is scaled to a height of 1.0, and all other
classes are scaled proportionally. Heights are therefore **not comparable
across flux figures** — a rectangle of height 0.5 in the GPP figure represents
a different absolute value than a rectangle of height 0.5 in the H figure.
The normalization maximum for each flux is as follows:

| Flux | Maximum class | Value |
|------|--------------|-------|
| NEP  | EBF | 382 gC m⁻² yr⁻¹ |
| GPP  | EBF | 2326 gC m⁻² yr⁻¹ |
| TER  | EBF | 1826 gC m⁻² yr⁻¹ |
| ET   | EBF | 825 mm yr⁻¹ |
| H    | WSA | 58 W m⁻² |

For H, the maximum class is WSA (woody savanna), not EBF, reflecting the
strong sensible heat partitioning in open woody savanna. For all carbon and
water fluxes, EBF (tropical forests) sets the normalization maximum.

---

## Companion table

Each flux figure ships with a CSV table
(`data/snapshots/flux_medians_by_igbp_*.csv`) containing one row per IGBP
class in the fixed grid order. Columns:

| Column | Content |
|--------|---------|
| `igbp_class` | IGBP code (e.g., "EBF") |
| `igbp_name` | Full class name |
| `median` | Median of site medians (flux units) |
| `n_sites` | Number of sites contributing |
| `total_site_years` | Sum of qualifying site-years across contributing sites |
| `std_dev` | Standard deviation of site medians |
| `normalized_height` | `median / max(median)` — the value used in the figure |

Unit conventions: NEP, GPP, TER in gC m⁻² yr⁻¹; ET in mm yr⁻¹; H in W m⁻².
