# Methods: Land Use / Land Cover Representativeness (ESA CCI Land Cover v2.1.1)

## Axis description

This axis measures how well the FLUXNET tower network samples the major land
use / land cover (LULC) types found across global land. It is most relevant
for stakeholders concerned with land-management classification, including:
  - Agricultural emissions and carbon flux partitioning (cropland)
  - Urban heat resilience and anthropogenic CO2 (settlement)
  - Land-use change tracking (forest-to-cropland, wetland drainage)
  - GHG inventory and IPCC national reporting categories

The settlement class, even at low absolute global area, is retained as a
separate visible class given its high stakeholder relevance.

## Data source

ESA Climate Change Initiative — Land Cover project, version 2.1.1, year 2022.
v2.1.1 is the continuation of the ESA CCI LC programme under Copernicus C3S,
using an identical algorithm, 300m resolution, and LCCS class system as v2.0.7.
The class legend and aggregation scheme are unchanged between v2.0.7 and v2.1.1.

Distribution: Copernicus Climate Data Store (CDS).
Dataset: satellite-land-cover (version v2_1_1, year 2022).
Licence: ESA CCI Land Cover licence, accepted via CDS user account.
Downloaded: 2026-06-24.
File: C3S-LC-L4-LCCS-Map-300m-P1Y-2022-v2.1.1.nc (NetCDF-4, 300m, 37 LCCS integer classes).

Note: v2.0.7 (1992-2015) was the version used in an earlier draft of this
analysis; it was the most recent year available via anonymous CEDA download.
v2.1.1 (2016-2022) requires CDS account authentication (free registration).
The substantive findings are unchanged between the two versions — see
SESSION_LOG.md (2026-06-25) for the comparison table.

Citation: ESA Climate Change Initiative — Land Cover project. Land Cover CCI
Product User Guide v2.0. European Space Agency.
http://www.esa-landcover-cci.org

## Why ESA CCI rather than MODIS IGBP

ESA CCI LC provides an external, satellite-derived classification applied
consistently across the entire global land surface. MODIS IGBP is the
network's self-classification scheme and has known accuracy issues at the
site level (Sulla-Menashe & Friedl 2018 Rem. Sens. Env.). Using an
independent external product avoids circularity: the representativeness
question is whether the network covers the range of land cover types, not
whether the sites have been correctly self-labeled.

Disagreements between site IGBP and CCI classification are expected and
outside the scope of this analysis — treat CCI as an independent reference
for the global distribution, not as a site validator.

## Aggregation scheme

The 37 native LCCS class codes were aggregated to 10 high-level categories
following the ESA CCI LC Product User Guide Table 2 (standard cross-walk
published by the project; identical between v2.0.7 and v2.1.1):

  1.  Cropland   : 10, 11, 12, 20, 30
  2.  Forest     : 50, 60-62, 70-72, 80-82, 90, 100
  3.  Shrubland  : 120, 121, 122, 150, 151, 152, 153
                   (incl. sparse vegetation per ESA CCI category definition)
  4.  Grassland  : 110, 130
  5.  Wetland    : 160, 170, 180
  6.  Settlement : 190
  7.  Bare       : 200, 201, 202
  8.  Snow/Ice   : 220
  9.  Water      : 210
  10. Other      : 40, 140
                   (lichens/mosses; mosaic nat-veg/crop not crop-dominated)

Notes on ambiguous classes:
  - Class 30 (Mosaic cropland >50% / nat veg <50%) -> Cropland (crop-dominated)
  - Class 40 (Mosaic nat veg >50% / cropland <50%) -> Other (not crop-dominated)
  - Class 100 (Mosaic tree/shrub >50% / herbaceous <50%) -> Forest (tree-dominated)
  - Class 110 (Mosaic herbaceous >50% / tree/shrub <50%) -> Grassland
  - Class 140 (Lichens and mosses) -> Other (ESA CCI definition)
  - Classes 150-153 (Sparse vegetation) -> Shrubland (ESA CCI: Shrubland incl.
    sparse vegetation)
  - Class 0 (No data) excluded from all analysis

## Per-site extraction

Sites: 767 (snapshot fluxnet_shuttle_snapshot_20260624T095651.csv).
terra::extract() at site lat/lon from the native 300m raster.
Sites returning NA or class 0 (No data) were recovered with nearest-land
search within a 3-degree window using terra::distance().
Recovery sites: 0; final NA sites: 0.
Extraction method recorded in the landcover_method column:
  'exact' = direct extraction; 'nearest_land_Xkm' = recovered at distance X km.

## Global distribution and land mask

The native 300m CCI LC raster was resampled to the KG 0.00833 deg grid
(Beck 2023, 1991-2020) using method = 'near' (nearest-neighbour), which
preserves discrete integer class values without introducing spurious
intermediate codes (do not use bilinear for categorical data).

The KG land mask was applied to restrict analysis to 147.3 M km2 of
global land, consistent with the KG, biomass, and aridity axes.
Inland water pixels within the KG mask (class 9: Water bodies) represent 2.48% of total land area — retained as a separate visible class.

terra::cellSize(kg_rast, mask=TRUE, unit='km') + terra::zonal(fun='sum')
were used to compute area per class.

## Colors

High-level class colors are area-weighted means of the constituent native
class RGB values from the published ESACCI-LC-Legend.csv. Snow/Ice was
substituted from white (#ffffff) to pale blue (#e0f3f8) for visibility in
stacked bar charts on white backgrounds; all other colors match the product
legend.

## Representativeness metrics

Weighted Jaccard (J) and Hellinger distance (H) as described in
methods_koppen_beck2023.md. p_k = global land fraction in class k;
q_k = fraction of 767-site FLUXNET network in class k.

  J = 0.5557  (0 = no overlap, 1 = identical distribution)
  H = 0.3372  (0 = identical, 1 = completely different)

## Interpretive note

The land cover axis complements the biomass axis: biomass measures carbon
stock density gradient, while LULC measures functional ecosystem type. A
network that scores well on both axes provides robust coverage for both
carbon-stock upscaling and land-management attribution. The settlement class
is intentionally kept visible despite its small global area fraction because
urban-flux representation is a known gap with high policy relevance.
