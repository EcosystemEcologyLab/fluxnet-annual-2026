# Beck 2023 Köppen-Geiger Maps — data/external/koppen_beck2023/

## Source

Beck, H.E., McVicar, T.R., Vergopolan, N. et al.  
High-resolution (1 km) Köppen-Geiger maps for 1901–2099 based on constrained CMIP6 projections.  
*Scientific Data* **10**, 724 (2023).  
doi: [10.1038/s41597-023-02549-6](https://doi.org/10.1038/s41597-023-02549-6)

## Figshare dataset

DOI: [10.6084/m9.figshare.21789074.v2](https://doi.org/10.6084/m9.figshare.21789074.v2)  
Version: 2 (published 2026-01-14; corrects a calculation error in v1)  
License: CC BY 4.0

Download URL used: `https://ndownloader.figshare.com/files/61012822` (`koppen_geiger_tif.zip`, ~125 MB)

## Download date

2026-06-24

## File structure

```
koppen_beck2023/
├── README.md           ← this file (committed)
├── legend.txt          ← numeric value → KG class code + name (committed)
├── koppen_geiger_tif.zip  ← source archive (gitignored)
├── 1901_1930/          ← historical period (gitignored)
├── 1931_1960/          ← historical period (gitignored)
├── 1961_1990/          ← historical period (gitignored)
├── 1991_2020/          ← present-day, used for site extraction (gitignored)
│   ├── koppen_geiger_0p00833333.tif   ← 1 km resolution (used)
│   ├── koppen_geiger_0p1.tif          ← 0.1° resolution
│   ├── koppen_geiger_0p5.tif          ← 0.5° resolution
│   └── koppen_geiger_1p0.tif          ← 1.0° resolution
├── 2041_2070/          ← future projections (gitignored)
└── 2071_2099/          ← future projections (gitignored)
```

## File used for per-site extraction

`1991_2020/koppen_geiger_0p00833333.tif` — 1 km resolution KG classification for 1991–2020.  
Pixel size: 0.00833333° (~1 km at equator).  
CRS: EPSG:4326 (WGS84).  
Values: integers 1–30, mapped to KG classes via `legend.txt`.

## Class legend

30 classes (see `legend.txt` for full mapping with RGB colors).  
Main climate groups: A (tropical), B (arid), C (temperate), D (cold), E (polar).

## Extraction output

`data/snapshots/site_koppen_beck2023.csv` — per-site KG class extracted at all 767 FLUXNET  
sites from snapshot `fluxnet_shuttle_snapshot_20260624T095651.csv`.  
Extraction script: `scripts/step4_extract_koppen_beck2023.R`.

## Underlying climate inputs

Our provenance records for this product (`CLAUDE.md`'s External Data table, and the Source
section above) note only Beck et al. (2023) itself and the figshare download URL — not the
climate datasets that paper's own maps are built from. This section documents those inputs,
from Beck et al. (2023)'s Methods section (*Scientific Data* **10**, 724,
doi:[10.1038/s41597-023-02549-6](https://doi.org/10.1038/s41597-023-02549-6)).

**What the paper states.** Quoted verbatim from the Methods:

> "The air temperature climatic datasets used were: (i) WorldClim V2 (covering 1970-2000);
> (ii) Climatologies at High resolution for the Earth's Land Surface Areas (CHELSA) V1.2
> (1979-2013); and (iii) CHELSA V2.1 (1981-2010)."

> "The precipitation climatic datasets used were: (i) WorldClim V2 (1970-2000); (ii) CHELSA
> V1.2 (1979-2013); (iii) CHELSA V2.1 (1981-2010); and (iv) Climate Hazards Group's
> Precipitation Climatology (CHPclim) V1 (1980-2009)."

> "All these datasets have a 0.01 degree resolution, except CHPclim V1, which has a 0.05
> degree resolution."

> "We subsequently generated Koppen-Geiger maps at 0.01 degree resolution for each historical
> period and combination of adjusted air temperature and precipitation climatic datasets.
> Next, we created for each historical period a final Koppen-Geiger map from the ensemble of
> 12 (4x3) maps by selecting the mode for each grid-cell."

> "To compute the air temperature offsets, we used Climatic Research Unit (CRU) Time Series
> (TS) V4.07 air temperature data"

> "To compute the precipitation factors, we used the Global Precipitation Climatology
> Centre (GPCC) Full Data Reanalysis (FDR) V2022."

The historical target periods are 1901-1930, 1931-1960, 1961-1990 and 1991-2020. Future maps
apply CMIP6 projections to the 1991-2020 baseline by delta change. (This section records only
what the paper states; no resampling step for CHPclim, or any other processing detail beyond
these quotations, is described here because the Methods text quoted above does not state one.)

**The period we use, and the pipeline's own window.** We extract the 1991-2020 map (see "File
used for per-site extraction" above), Beck et al.'s most recent historical period and the one
ending closest to present. This repository's own site-side climate normals use the same
1991-2020 window: `R/pipeline_config.R:56` sets
`KG_ERA5_PERIOD <- c(1991L, 2020L) # matches Beck et al. (2023) present-day window`, which
`R/climate_classification.R`'s `compute_site_koppen_era5()` (see its `period = KG_ERA5_PERIOD`
default, `R/climate_classification.R:226`) uses to classify each site's own ERA5-derived
climate normal. The two 1991-2020 windows (Beck's map, and this pipeline's ERA5-based
per-site classification) are the same calendar period by construction, not by coincidence.

**Per-input-dataset source and citation**, where the Beck paper or the dataset's own
documentation states it plainly:

| Dataset | Citation | Notes |
|---|---|---|
| WorldClim V2 | Fick, S.E. & Hijmans, R.J. (2017). WorldClim 2: new 1-km spatial resolution climate surfaces for global land areas. *International Journal of Climatology* 37(12):4302-4315. doi:[10.1002/joc.5086](https://doi.org/10.1002/joc.5086) | Already the citation used for our own separate `worldclim/` extraction (see `CLAUDE.md`'s provenance table); same dataset, cited the same way here. |
| CHELSA V1.2 | Karger, D.N., Conrad, O., Böhner, J., Kawohl, T., Kreft, H., Soria-Auza, R.W., Zimmermann, N.E., Linder, H.P., Kessler, M. (2017). Climatologies at high resolution for the earth's land surface areas. *Scientific Data* 4:170122. doi:[10.1038/sdata.2017.122](https://doi.org/10.1038/sdata.2017.122) | This paper (its arXiv preprint, [1607.00217](https://arxiv.org/abs/1607.00217)) states plainly that CHELSA's downscaled temperature and precipitation estimates are derived from **ERA-Interim** climatic reanalysis: "downscaled model output temperature and precipitation estimates of the ERA Interim climatic reanalysis." |
| CHELSA V2.1 | Same methodology paper as V1.2 (Karger et al. 2017, doi:10.1038/sdata.2017.122); the V2.1 data release itself is versioned via EnviDat, doi:[10.16904/envidat.228](https://www.doi.org/10.16904/envidat.228) | We did not find a source read for this task stating V2.1's forcing/reanalysis basis explicitly (unlike V1.2's stated ERA-Interim basis above) — left out rather than assumed to be identical to V1.2. |
| CHPclim V1 | Funk, C., Verdin, A., Michaelsen, J., Peterson, P., Pedreros, D., Husak, G. (2015). A global satellite-assisted precipitation climatology. *Earth System Science Data* 7:275-287. doi:[10.5194/essd-7-275-2015](https://doi.org/10.5194/essd-7-275-2015). Dataset doi: 10.15780/G2159X | — |
| CRU TS V4.07 | Harris, I., Osborn, T.J., Jones, P., Lister, D. (2020). Version 4 of the CRU TS monthly high-resolution gridded multivariate climate dataset. *Scientific Data* 7:109. doi:[10.1038/s41597-020-0453-3](https://doi.org/10.1038/s41597-020-0453-3) | Version 4.07 specifically: CEDA catalogue record, uuid `5fda109ab71947b6b7724077bf7eb753`. |
| GPCC Full Data Reanalysis V2022 | Schneider, U., Hänsel, S., Finger, P., Rustemeier, E., Ziese, M. (2022). GPCC Full Data Monthly Product Version 2022 at \<resolution\>: Monthly Land-Surface Precipitation from Rain-Gauges built on GTS-based and Historical Data. DWD. | DOI varies by grid resolution (e.g. 0.25°: `10.5676/DWD_GPCC/FD_M_V2022_025`; 1.0°: `10.5676/DWD_GPCC/FD_M_V2022_100`); Beck et al. (2023) does not state which resolution they used, so no single DOI is asserted here. |

Where a dataset's own basis was not stated plainly in a source read for this task (CHELSA
V2.1's forcing dataset; CHPclim's, CRU's, and GPCC's own further upstream station/model
lineage), it is left out here rather than inferred.
