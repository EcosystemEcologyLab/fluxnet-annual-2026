# Methods: Above-Ground Biomass Representativeness (ESA CCI Biomass v7.0)

## Axis description

This axis measures network coverage of the global aboveground biomass density
distribution. Biomass is framed as a continuous stock variable (Mg dry matter
per hectare) rather than as a biome stratification: the question is whether the
FLUXNET tower network samples the full range of biomass densities found across
global land, not whether it samples named biome types.

This framing is most relevant for:
  - Carbon stock estimation and upscaling from flux measurements
  - Biomass product validation (ESA CCI, GEDI, ICESat-2)
  - Forest-carbon accounting, where representativeness across the biomass
    density gradient determines the reliability of tower-based benchmarks

## Data source

ESA Climate Change Initiative — Biomass project, version 7.0 (March 2026).
Product: ESA Biomass Climate Change Initiative (Biomass_cci): Global datasets
of forest above-ground biomass for the years 2005-2012 and 2015-2024, v7.0.
CEDA catalogue: https://catalogue.ceda.ac.uk/uuid/6429d1aafe1e43b9b414e4a5a7f8b903

Citation: Santoro, M.; Cartus, O. (2024): ESA Biomass Climate Change Initiative
(Biomass_cci): Global datasets of forest above-ground biomass for the years
2005-2012 and 2015-2024, v7.0. NERC EDS Centre for Environmental Data Analysis.
doi:10.5285/6429d1aafe1e43b9b414e4a5a7f8b903

Variable: above-ground biomass (AGB), Mg/ha. Native resolution: 100 m,
distributed as 1 deg x 1 deg tiles (~300 tiles per year, ~18 GB). The
pre-aggregated 1 km product (single global file, 1.4 GB) was used for this
analysis. Band used: band 18 (year 2024).

Resolution rationale: EC tower footprints extend 0.5-3 km depending on wind
speed and stability; a single 100 m pixel does not represent the flux footprint
better than a 1 km mean. Using the 1 km product aligns naturally with the
Beck 2023 KG land mask (0.00833 deg ~ 1 km), enabling direct grid alignment.
The 1 km file was downloaded from the CEDA aggregated/ directory via anonymous
HTTP (no authentication required).

## Hybrid bin scheme

Seven bins using a hybrid fixed + equal-area quantile scheme:

  Bin 1: 0-5 Mg/ha (fixed) — separates near-zero / bare / ice / desert land
         from vegetated land. Eddy covariance is rarely deployed on bare or
         ice-covered surfaces; a single class absorbs all non-vegetated land.

  Bins 2-7: six equal-area quantile bins computed from the global distribution
         of KG-land pixels with biomass >= 5 Mg/ha (area-weighted). Each bin
         contains approximately 1/6 of the total area of vegetated land, producing
         six equally-sized slices of the biomass density gradient. This ensures
         that no single bin dominates the figure and that the full range from
         sparse to dense vegetation is represented without any pre-specified
         ecological interpretation tied to the breakpoints.

  Quantile breakpoints (Mg/ha): q1 = 13, q2 = 27, q3 = 51, q4 = 94, q5 = 171

  Bin 1:  0-5 Mg/ha          (fixed lower cut)
  Bin 2:  5-13 Mg/ha       (1st sixth of vegetated land)
  Bin 3:  13-27 Mg/ha      (2nd sixth)
  Bin 4:  27-51 Mg/ha     (3rd sixth)
  Bin 5:  51-94 Mg/ha    (4th sixth)
  Bin 6:  94-171 Mg/ha   (5th sixth)
  Bin 7:  >171 Mg/ha        (6th sixth / top sixth)

These breakpoints are data-dependent and will be recomputed if the biomass product
is updated or replaced. The values above were computed from ESA CCI Biomass v7.0,
band 18 (year 2024), using 995 Mg/ha histogram bins over the
range 5--1000 Mg/ha plus a catch-all.

## Quantile computation method

Area-weighted quantiles were computed from the global raster as follows:
  1. terra::resample(biomass, kg_rast, method='bilinear') to align biomass to
     the KG 0.00833 deg grid.
  2. Apply KG land mask: KG-land pixels with NA biomass -> assigned value 0
     (-> bin 1) via terra::ifel().
  3. Mask to pixels with biomass >= 5 Mg/ha.
  4. Classify into fine histogram bins (1 Mg/ha step, range 5--1000 Mg/ha
     plus catch-all) using terra::classify().
  5. Sum pixel area per histogram bin: terra::zonal(cellSize(kg_rast,
     mask=TRUE, unit='km'), ..., fun='sum').
  6. Compute cumulative area; find values at cumulative fractions 1/6 through
     5/6 of total vegetated area (upper edge of the crossing histogram bin).
  7. Round each breakpoint to 1 decimal place (canonical values used for all
     downstream classification and reporting).

Total vegetated land (>= 5 Mg/ha): 70,162,294 km^2.

## Per-site classification

Per-site biomass values (Mg/ha) were extracted previously from ESA CCI Biomass
v7.0 band 18 at each site's reported lat/lon using terra::extract() (nearest
pixel, 1 km raster). Bin assignments were recomputed here from those stored
values using the new hybrid breakpoints; no re-extraction was necessary.

Site coordinates from fluxnet_shuttle_snapshot_20260624T095651.csv (767 sites).
Sites returning NA (ocean pixels, ice sheets, or biomass product coverage
gaps) were assigned to bin 1 with biomass_method = 'na_assigned_low'.
NA sites in this extraction: 0 of 767.

## Global distribution and land mask

The Beck 2023 KG raster (1991-2020, 0.00833 deg grid, 147,322,862 km^2
total land) is used as the land mask for cross-axis consistency with the KG
and aridity representativeness axes.

Method:
  1. terra::resample(biomass, kg_rast, method='bilinear') to align grids.
  2. KG-land pixels where resampled biomass is NA -> assigned to value 0
     (-> bin 1) using terra::ifel().
  3. terra::classify() with the 7-bin hybrid matrix.
  4. terra::cellSize(kg_rast, mask=TRUE, unit='km') for pixel areas.
  5. terra::zonal(cell_areas, biomass_bins, fun='sum') to sum area per bin.

Total land area: 147,322,862 km^2 (aligned to KG land mask baseline).

## Representativeness metrics

Weighted Jaccard (J) and Hellinger distance (H) as described in
methods_koppen_beck2023.md. p_k = global land fraction in bin k;
q_k = fraction of 767-site FLUXNET network in bin k.

  J = 0.6385  (0 = no overlap, 1 = identical distribution)
  H = 0.1656  (0 = identical, 1 = completely different)

For comparison, the previous fixed-bin scheme (0-5, 5-25, 25-50, 50-100,
100-200, 200-400, >400 Mg/ha) yielded J = 0.6262, H = 0.1684.

## Interpretive note

Under the hybrid binning scheme, bins 2-7 each represent approximately 1/6 of
global vegetated land. A perfectly representative network would place an equal
fraction of towers in each vegetated bin. Deviations from equal sampling reveal
where the network is over- or under-represented across the biomass density
gradient. This axis is most actionable for:
  - Identifying biomass density ranges where new tower placement would improve
    global carbon synthesis coverage
  - Validating satellite biomass products across a representative density range
  - Assessing whether tower-based upscaling correctly weights high-biomass
    forests (which store disproportionate carbon)
