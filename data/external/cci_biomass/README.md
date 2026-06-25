# ESA CCI Biomass v7.0 — Above-Ground Biomass

## Source

ESA Climate Change Initiative — Biomass project
Version: **7.0** (issued March 2026, latest as of 2026-06-24)

Product name: ESA Biomass Climate Change Initiative (Biomass_cci):
Global datasets of forest above-ground biomass for the years
2005-2012 and 2015-2024, v7.0

CEDA catalogue: https://catalogue.ceda.ac.uk/uuid/6429d1aafe1e43b9b414e4a5a7f8b903

## Variable

Above-ground biomass (AGB), unit: **Mg/ha** (megagrams dry mass per hectare)

## Coverage and resolution

- Global, -90° to +90° latitude, -180° to +180° longitude
- Native resolution: **100 m** (distributed as 1°×1° tiles)
- Pre-aggregated product used here: **1000 m** (single global file)
- Years available: 2005-2012 and 2015-2024

## Files in this directory

| File | Description | Size | Source |
|------|-------------|------|--------|
| ESACCI-BIOMASS-L4-AGB-MERGED-1000m-fv7.0.tif | AGB Mg/ha, 1km, MERGED | ~1.4 GB | CEDA aggregated/ |

File downloaded from:
http://data.ceda.ac.uk/neodc/esacci/biomass/data/agb/maps/v7.0/geotiff/aggregated/

Anonymous HTTP access; no authentication required.

## Why 1km is used (not 100m native)

The 100m product is distributed as ~599 tiles × ~year, totalling ~18 GB per year.
The 1km pre-aggregated product (1.4 GB) is used for:
- Per-site extraction: EC tower footprints extend 0.5-3 km; 100m sub-pixel
  precision adds no meaningful information for coarse-bin classification.
- Global area distribution: resampled to match the Beck 2023 KG land mask
  at 0.00833° (~1km), preserving cross-axis consistency.

## Citation

Santoro, M.; Cartus, O. (2024): ESA Biomass Climate Change Initiative
(Biomass_cci): Global datasets of forest above-ground biomass for the years
2005-2012 and 2015-2024, v7.0. NERC EDS Centre for Environmental Data
Analysis. doi:10.5285/6429d1aafe1e43b9b414e4a5a7f8b903

## Licence

ESA CCI Biomass Terms and Conditions v2 (free for research use with attribution).
https://artefacts.ceda.ac.uk/licences/specific_licences/esacci_biomass_terms_and_conditions_v2.pdf

## Download date

2026-06-24
