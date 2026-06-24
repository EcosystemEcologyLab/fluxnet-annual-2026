# Methods: Above-Ground Biomass Representativeness (ESA CCI Biomass v7.0)

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
distributed as 1°x1° tiles (~300 tiles per year, ~18 GB). The pre-aggregated
1km product (single global file, 1.4 GB) was used for this analysis.

Resolution rationale: EC tower footprints extend 0.5-3 km depending on wind
speed and stability; a single 100m pixel does not represent the flux footprint
better than a 1km mean. Using the 1km product also aligns naturally with the
Beck 2023 KG land mask (0.00833° ~1km), enabling direct grid alignment.
The 1km file was downloaded from the CEDA aggregated/ directory via anonymous
HTTP (no authentication required).

## Bin definitions

Seven log-spaced bins (Mg/ha):

  Bin 1:  0-5    Bare/ice/desert
  Bin 2:  5-25   Sparse to open vegetation
  Bin 3: 25-50   Shrubland/savanna
  Bin 4: 50-100  Open forest
  Bin 5: 100-200 Temperate forest
  Bin 6: 200-400 Wet/boreal forest
  Bin 7: >400    Tropical forest

Breakpoints at 25, 50, 100, 200, 400 Mg/ha follow conventions used in IPCC
AR6 Working Group I (Chapter 2: Changing State of the Climate System) and in
major above-ground carbon stock assessments (Spawn et al. 2020 Sci Data;
Santoro et al. 2021 Earth Syst Sci Data). The 0-5 and 5-25 Mg/ha cuts are
pragmatic: bin 1 captures deserts, ice sheets, and bare/unvegetated land
where eddy covariance is unlikely or impossible; bin 2 captures dryland
grasslands, open shrublands, and sparsely vegetated semi-arid zones. These
two lower bins are judgment-based choices that are not drawn from literature
conventions, and reviewers should be aware of this.

## Per-site extraction

Site coordinates from fluxnet_shuttle_snapshot_20260624T095651.csv (767 sites).
terra::extract() with the 1km raster at each site's reported lat/lon.
Sites returning NA (ocean pixels, ice sheets, or biomass product coverage
gaps) were assigned to bin 1 with biomass_method = 'na_assigned_low'.
This is consistent with the all-land framing: NA within the biomass product
indicates no significant above-ground woody biomass.
NA sites in this extraction: 0 of 767.

## Global distribution and land mask

Cross-axis consistency requires using the same land mask as the KG and aridity
axes. The Beck 2023 KG raster (1991-2020, 0.00833° grid, 147,322,862 km^2
total land) is used as the land mask.

Method:
  1. terra::resample(biomass, kg_rast, method='bilinear') to align grids.
  2. KG-land pixels where resampled biomass is NA -> assigned to value 0
     (-> bin 1) using terra::ifel().
  3. terra::classify() with the 7-bin matrix.
  4. terra::cellSize(kg_rast, mask=TRUE, unit='km') for pixel areas.
  5. terra::zonal(cell_areas, biomass_bins, fun='sum') to sum area per bin.

Total land area: 147,322,862 km^2 (aligned to KG land mask baseline).

## Representativeness metrics

Weighted Jaccard (J) and Hellinger distance (H) as described in
methods_koppen_beck2023.md. p_k = global land fraction in bin k;
q_k = fraction of 767-site FLUXNET network in bin k.

  J = 0.6262  (0 = no overlap, 1 = identical distribution)
  H = 0.1684  (0 = identical, 1 = completely different)

## Interpretive note

Bins 1-2 (0-25 Mg/ha) capture deserts, ice, and sparse dryland vegetation.
These biomes are intrinsically difficult to sample with eddy covariance (EC)
towers due to logistical challenges and the low carbon flux signal. The
representativeness signal that is most actionable for tower network design
lies in bins 3-7 (>25 Mg/ha): shrublands, forests, and tropical vegetation.
Under-representation in these bins points to specific forest types or
geographic regions where new towers would improve global synthesis coverage.
