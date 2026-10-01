Panel B of the new Figure 4 (`scripts/figure4_representativeness.R`) classifies land cover as IGBP —
distinct from the existing ESA CCI Land Cover v2.1.1 ("land cover (high-level)") axis already used by
`figure_representativeness_summary.R`'s panel B, which this new panel does not touch or replace.

**Global side.** MODIS MCD12C1.061 `Majority_Land_Cover_Type_1` (the IGBP classification band, 0.05°
native resolution; the file already in `data/external/modis_landcover/`), resampled (nearest-neighbour,
categorical) onto the Beck et al. (2023) 1 km Köppen land-mask grid and masked to it, so the land total
reproduces panel A's own 147,322,862 km² exactly, rather than MODIS's own native-resolution footprint.
MODIS's HDF4 CRS metadata mislabels the datum ("Clarke 1866 ellipsoid"); both rasters share the identical
−180/180/−90/90 lon/lat extent (a known MCD12C1 climate-modeling-grid quirk, not a real projection
mismatch), so MODIS's CRS is set to EPSG:4326 before resampling.

**Allowable classes.** The vocabulary is the IGBP classes PIs actually report in BIF metadata — the
`igbp` column already present in the pinned current-network snapshot CSV (there is no free-text "IGBP"
BADM `VARIABLE`). All 781 current-network sites report one of 15 classes: ENF, EBF, DNF, DBF, MF, CSH,
OSH, WSA, SAV, GRA, WET, CRO, CVM, BSV, SNO — every standard IGBP class except Water (code 0) and
Urban-and-built-up (code 13); no flux tower is sited on open water or in a city. MODIS pixels classified
0 or 13 are folded into a 16th "Other" bin, counted in the land total (confirmed: keep MODIS water and
urban cells in the land total) but never populated by a site, since no PI reports either class.

**Colours.** The standard MCD12Q1 `LC_Type1` legend palette, sourced from Google Earth Engine's
documented default visualization palette for `MODIS/061/MCD12Q1`
(https://developers.google.com/earth-engine/datasets/catalog/MODIS_061_MCD12Q1, fetched 2026-10-02).
"Other" (the merged Water+Urban bin, with no official single colour in the source legend) uses Urban's
own grey rather than Water's blue, which would read as open water and mislead.

**Geo vs Geo** = the MODIS class at each tower, extracted at native 0.05° resolution (not degraded
through the 1 km resample used for the area accounting above). **Geo vs Data** = each site's PI-reported
IGBP class.

**PI vs. MODIS disagreement.** 505 of 781 sites (64.7%) disagree between their PI-reported class and the
MODIS class at the tower coordinate — consistent with the well-known flux-footprint-vs.-5.6 km-pixel
mismatch: PIs site towers in locally homogeneous patches often too small to dominate a coarse MODIS
pixel. Wetland (96.6% disagreement) and needleleaf forest classes (ENF 83.3%, DNF among the 100%-disagree
small-n classes) disagree most; cropland (27.3%) disagrees least, as expected for a large-patch,
spectrally distinct cover type. Full by-class breakdown in SESSION_LOG.md (2026-10-02 Phase 2 entry).

Outputs: `data/snapshots/site_igbp_fig4.csv`, `igbp_mcd12c1_global_distribution.csv`.
