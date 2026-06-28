# Methods: Land Use / Land Cover Representativeness (ESA CCI LC v2.1.1)

## Axis description

This axis measures how well the FLUXNET tower network samples the major land
use / land cover (LULC) types found across global land. Three aggregation
levels are analysed in parallel: (1) the high-level 10-class scheme (ESA CCI
PUG Table 2 cross-walk), (2) the intermediate Level 2 22-class LCCS hierarchy
(published sub-divisions by leaf type, canopy openness, water regime, etc.),
and (3) the full native 37-class LCCS map. Finer aggregation reveals within-
class heterogeneity that the coarser levels collapse — the same pattern seen
for the Koppen-Geiger climate axis across 5-class, 13-class, and 30-class
aggregations (where J decreases and H increases at finer levels).

## Data source

ESA Climate Change Initiative — Land Cover project, version 2.1.1, year 2022.
v2.1.1 is the Copernicus C3S continuation of ESA CCI LC, using an identical
algorithm, 300 m resolution, and LCCS class system as v2.0.7 (1992-2015).
The legend and aggregation scheme are unchanged between versions.

Distribution: Copernicus Climate Data Store (CDS).
File: C3S-LC-L4-LCCS-Map-300m-P1Y-2022-v2.1.1.nc (NetCDF-4, 300m, LCCS integer classes).
Citation: ESA Climate Change Initiative — Land Cover project. ESA CCI Land
Cover Product User Guide v2.0. European Space Agency. http://www.esa-landcover-cci.org

## Three aggregation levels

### Level 1 — High-level (10 classes, ESA CCI PUG Table 2)

Standard cross-walk published by ESA CCI, identical across v2.0.7 and v2.1.1:

  1  Cropland    : 10 11 12 20 30
  2  Forest      : 50, 60-62, 70-72, 80-82, 90, 100
  3  Shrubland   : 120 121 122 150 151 152 153 (incl. sparse vegetation)
  4  Grassland   : 110 130
  5  Wetland     : 160 170 180
  6  Settlement  : 190
  7  Bare        : 200 201 202
  8  Snow/Ice    : 220
  9  Water       : 210
  10 Other       : 40 140

### Level 2 — ESA CCI LCCS intermediate hierarchy (22 classes)

The LCCS class naming convention embeds a hierarchy of discriminators
(life form, water regime, leaf type, canopy openness) that maps directly to
22 intermediate classes. This Level 2 is consistent with the subdivisions
described in the ESA CCI LC Product User Guide sections on class definitions.

| L2 code | Level 2 name | LCCS native codes | HL group |
|---|---|---|---|
| 1 | Rainfed cropland | 10, 11, 12 | Cropland |
| 2 | Irrigated cropland | 20 | Cropland |
| 3 | Mosaic cropland/nat-veg | 30 | Cropland |
| 4 | Mosaic nat-veg/cropland | 40 | Other |
| 5 | BL evergreen forest | 50 | Forest |
| 6 | BL deciduous forest | 60, 61, 62 | Forest |
| 7 | NL evergreen forest | 70, 71, 72 | Forest |
| 8 | NL deciduous forest | 80, 81, 82 | Forest |
| 9 | Mixed forest | 90 | Forest |
| 10 | Mosaic tree-shrub/herb | 100 | Forest |
| 11 | Mosaic herb/tree-shrub | 110 | Grassland |
| 12 | Shrubland | 120, 121, 122 | Shrubland |
| 13 | Grassland | 130 | Grassland |
| 14 | Lichens and mosses | 140 | Other |
| 15 | Sparse vegetation | 150, 151, 152, 153 | Shrubland |
| 16 | Flooded forest (freshwater) | 160 | Wetland |
| 17 | Flooded forest (saline) | 170 | Wetland |
| 18 | Flooded shrub/herb | 180 | Wetland |
| 19 | Urban areas | 190 | Settlement |
| 20 | Bare areas | 200, 201, 202 | Bare |
| 21 | Water bodies | 210 | Water |
| 22 | Permanent snow and ice | 220 | Snow/Ice |

Key discriminators at Level 2 vs. Level 1:
  - Cropland: rainfed vs. irrigated vs. mosaic (important for water cycling)
  - Forest: leaf type (broadleaved / needleleaved) and phenology
    (evergreen / deciduous) are ecologically distinct for flux magnitude
  - Wetland: fresh/brackish vs. saline flooded forest; flooded shrub/herb
  - Other: mosaic nat-veg/cropland (code 40) and lichens/mosses (code 140)
    are distinct ecosystems despite sharing a high-level code

### Level 3 — Native LCCS (37 classes)

The full set of native LCCS integer codes from the ESA CCI LC map,
as read directly from the product. Codes 61/62 subdivide the broadleaved
deciduous forest by canopy openness (closed >40% / open 15-40%); similarly
for codes 71/72 (NL evergreen), 81/82 (NL deciduous), 121/122 (shrubland),
151/152/153 (sparse vegetation), 201/202 (bare areas). Class 0 (No data)
is excluded from all analysis.

## Aggregation lookup table

A complete three-level lookup table mapping each native LCCS code to
Level 2 and high-level codes is saved to:
  data/snapshots/cci_landcover_aggregation_lookup.csv

## Per-site classification

Sites: 767 (snapshot fluxnet_shuttle_snapshot_20260624T095651.csv).
The native LCCS code per site (cci_native_class) was extracted previously
from the 300m raster via terra::extract() with nearest-land recovery within
3 degrees for NA / No-data sites. No re-extraction was performed for this
update. Level 2 and high-level codes were assigned from the lookup table.

## Global distribution

The native 300m NetCDF was resampled to the KG 0.00833° grid (Beck 2023,
1991-2020, 147.3 M km² total land) using method = 'near' (nearest-neighbour;
preserves integer class codes). The resampled raster is cached at:
  data/external/cci_landcover/v2.1.1/cci_lc_2022_kg_aligned_native.tif
Classification rasters for Level 2 and high-level were produced via
terra::classify() from the cached native raster. terra::cellSize() +
terra::zonal(fun='sum') were used to compute area per class at all three
levels. Class 0 excluded; water (class 210) retained as class 9 at all
aggregation levels.

## Colors

Native (37 classes): RGB values from the published ESACCI-LC-Legend.csv.
Level 2 (22 classes): unweighted mean RGB of the constituent native classes
within each Level 2 group (legend RGB only; no area weighting at color
computation stage). Snow/Ice overridden to #e0f3f8 (pale blue) at all
three levels for visibility on white backgrounds.
High-level (10 classes): same unweighted mean approach as Level 2.

## Representativeness metrics

Weighted Jaccard (J) and Hellinger distance (H) as described in
methods_koppen_beck2023.md. p_k = global land fraction in class k;
q_k = fraction of 767-site network in class k.

| Aggregation | n classes | J | H |
|---|---|---|---|
| High-level | 10 | 0.556 | 0.337 |
| Level 2    | 22 | 0.478 | 0.368 |
| Native     | 37 | 0.441 | 0.395 |

J decreases and H increases with finer aggregation, consistent with the
KG pattern: the coarser 10-class scheme masks within-group heterogeneity.
Notable patterns visible at Level 2 but hidden at Level 1:
  - Within Forest: needleleaved evergreen (boreal conifer) is over-sampled;
    broadleaved evergreen (tropical) is under-sampled relative to global area.
  - Within Cropland: rainfed vs. irrigated vs. mosaic breakdown reveals
    whether the network captures irrigated agricultural ecosystems.
  - Within Wetland: the three flooded-tree and flooded-shrub classes can be
    separated to show which freshwater vs. saline wetland types are covered.

## Why ESA CCI rather than MODIS IGBP

ESA CCI LC provides an external, satellite-derived classification applied
consistently across the global land surface. MODIS IGBP is the network's
self-classification and has known site-level accuracy issues. Using an
independent product avoids circularity. Disagreements between site IGBP and
CCI classification are expected and outside the scope of this analysis.
