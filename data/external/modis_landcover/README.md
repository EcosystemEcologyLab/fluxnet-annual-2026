# MODIS Land Cover (MCD12C1) — data/external/modis_landcover/

## Source

Product: **MCD12C1** — MODIS/Terra+Aqua Land Cover Type Yearly L3 Global 0.05 Degree
CMG (Climate Modeling Grid).
Collection: **061** (current collection at time of download; supersedes Collection 6).
Year: **2022** (`RANGEBEGINNINGDATE=2022-01-01`, `RANGEENDINGDATE=2022-12-31`), chosen
to match the ESA CCI land cover epoch already in use in this repository.

DOI: `10.5067/MODIS/MCD12C1.061`

Archive host: NASA LP DAAC, distributed via NASA's Earthdata Cloud
(`data.lpdaac.earthdatacloud.nasa.gov`), not the legacy `e4ftl01.cr.usgs.gov` Data Pool
host (that host returned 404 for this product/collection when checked directly; the
correct file location was resolved via NASA's CMR granule search, not assumed).

Source URL (resolved via CMR granule search for short_name=MCD12C1, version=061,
temporal 2022):
```
https://data.lpdaac.earthdatacloud.nasa.gov/lp-prod-protected/MCD12C1.061/MCD12C1.A2022001.061.2023244164746/MCD12C1.A2022001.061.2023244164746.hdf
```

**A NASA Earthdata Login was required.** Authenticated via `~/.netrc`
(`machine urs.earthdata.nasa.gov`) using `curl -n`, with the session cookie jar at
`data/external/.urs_cookies` (gitignored) to carry state through the URS OAuth redirect
chain (URS -> `data.lpdaac.earthdatacloud.nasa.gov/login` -> presigned CloudFront/S3
URL). See CLAUDE.md, "NASA Earthdata Downloads", for the general mechanism.

## Download date

2026-09-18.

## File

| File | Type | Size | Tracked |
|---|---|---|---|
| `MCD12C1.A2022001.061.2023244164746.hdf` | HDF4-EOS | 1,244,259,897 bytes (~1.16 GiB) | No — gitignored (see below) |

File size on disk was checked against the HTTP response's reported size
(`Content-Length` from the final `200 OK`) and matches exactly.

## Subdatasets (from GDAL info; verbatim)

Three independent land-cover classification schemes are present, each with a majority
class layer, an assessment layer, and a per-class percent-cover layer:

```
HDF4_EOS:EOS_GRID:"...":MOD12C1:Majority_Land_Cover_Type_1              [3600x7200]     -- IGBP scheme, primary majority class
HDF4_EOS:EOS_GRID:"...":MOD12C1:Majority_Land_Cover_Type_1_Assessment   [3600x7200]
HDF4_EOS:EOS_GRID:"...":MOD12C1:Land_Cover_Type_1_Percent               [3600x7200x17]  -- per-class percent cover, IGBP (17 classes)
HDF4_EOS:EOS_GRID:"...":MOD12C1:Majority_Land_Cover_Type_2              [3600x7200]     -- UMD scheme majority class
HDF4_EOS:EOS_GRID:"...":MOD12C1:Majority_Land_Cover_Type_2_Assessment   [3600x7200]
HDF4_EOS:EOS_GRID:"...":MOD12C1:Land_Cover_Type_2_Percent               [3600x7200x14]  -- per-class percent cover, UMD (14 classes)
HDF4_EOS:EOS_GRID:"...":MOD12C1:Majority_Land_Cover_Type_3              [3600x7200]     -- LAI/FPAR scheme majority class
HDF4_EOS:EOS_GRID:"...":MOD12C1:Majority_Land_Cover_Type_3_Assessment   [3600x7200]
HDF4_EOS:EOS_GRID:"...":MOD12C1:Land_Cover_Type_3_Percent               [3600x7200x11]  -- per-class percent cover, LAI/FPAR (11 classes)
```

All bands are 8-bit unsigned integer, 3600 rows x 7200 columns (0.05deg global CMG),
lon/lat, WGS84-adjacent (Clarke 1866-based) datum as reported by GDAL.

## HDF4 readability on this machine — no GeoTIFF conversion needed

`terra::rast()` on this machine (mini) opens the HDF4-EOS subdatasets directly,
confirmed for both `Majority_Land_Cover_Type_1` (single band) and
`Land_Cover_Type_1_Percent` (17 bands). No `gdal_translate` conversion to GeoTIFF was
performed — only the original `.hdf` file is present in this directory.

## Not yet done

No distribution (e.g. global class-area summary, comparison against ESA CCI) has been
computed from this file yet. This download only fetches and verifies the source file.

## More years

The catalog lists this product as available annually from 2001 through 2022 (24
granules total across that span at the time of this check); only 2022 was downloaded
here. Additional years would be cheap to add — same mechanism, only the year in the CMR
query and resulting granule ID change.
