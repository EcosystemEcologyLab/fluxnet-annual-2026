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
