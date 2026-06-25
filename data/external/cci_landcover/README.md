# ESA CCI Land Cover — data/external/cci_landcover/

## Current version (authoritative for paper analysis)

**ESA CCI / C3S Land Cover v2.1.1, year 2022.**

Distributed by the Copernicus Climate Data Store (CDS) under the Copernicus
C3S branding (C3S-LC). The algorithm, 300 m resolution, and LCCS class system
are identical to v2.0.7. Year 2022 is the most recent available as of mid-2026
(CDS temporal extent: 1992–2022; 2023/2024 updates expected during 2026 per
the CDS change notice dated 2025-07-04).

Download requires a free CDS account and API key (`~/.cdsapirc`).
Licence accepted via CDS user profile (one-time per dataset).
Downloaded: 2026-06-24.

## Previous version (historical reference)

**ESA CCI Land Cover v2.0.7, year 2015.**

Most recent year available via anonymous CEDA HTTP. Used in an earlier draft
of this analysis before CDS credentials were configured. Replaced by v2.1.1
(2022). The substantive findings (wetland over-sampling, bare under-sampling)
were unchanged between the two versions — see SESSION_LOG.md (2026-06-25) for
the comparison table.

## Files

### v2.1.1 (committed / gitignored)

| File | Description | Size | Tracked |
|------|-------------|------|---------|
| `v2.1.1/C3S-LC-L4-LCCS-Map-300m-P1Y-2022-v2.1.1.nc` | Annual global land cover map, 300m NetCDF-4. 5 variables; use `lccs_class` band. **gitignored.** | 2.2 GB | No |
| `v2.1.1/cci_lc_v2.1.1_2022.zip` | Downloaded archive from CDS. **gitignored.** | 2.2 GB | No |

### v2.0.7 (historical reference)

| File | Description | Size | Tracked |
|------|-------------|------|---------|
| `ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif` | Annual global land cover map, 300m GeoTIFF. **gitignored.** | 312 MB | No |

### Legend / metadata (committed)

| File | Description | Size | Tracked |
|------|-------------|------|---------|
| `ESACCI-LC-Legend.csv` | Native class legend (37 LCCS codes, RGB, labels). Semicolon-delimited. **Unchanged between v2.0.7 and v2.1.1.** | 2 KB | Yes |
| `ESACCI-LCMapsColorLegend.qml` | QGIS QML color ramp file (per-native-class hex colors). | 5 KB | Yes |

## Download

### v2.1.1 via CDS API (current)

```python
# Requires ~/.cdsapirc with CDS API key
# Licence must be accepted at:
# https://cds.climate.copernicus.eu/datasets/satellite-land-cover?tab=download#manage-licences
import cdsapi, zipfile, os

client = cdsapi.Client()
result = client.retrieve(
    "satellite-land-cover",
    {"variable": "all", "version": "v2_1_1", "year": "2022"},
)
result.download("v2.1.1/cci_lc_v2.1.1_2022.zip")

with zipfile.ZipFile("v2.1.1/cci_lc_v2.1.1_2022.zip", "r") as zf:
    zf.extractall("v2.1.1/")
```

**Important parameter note:** The CDS API version string uses underscores
(`v2_1_1`), not dots (`v2.1.1`). Using dots returns a 400 Bad Request.

### v2.0.7 via CEDA anonymous HTTP (historical)

```bash
curl -L -o ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif \
  "https://dap.ceda.ac.uk/neodc/esacci/land_cover/data/land_cover_maps/v2.0.7/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif"
```

## Raster specification (both versions)

- **CRS:** WGS 84 (EPSG:4326 / CRS84)
- **Resolution:** 300 m (approximately 0.002778° at equator)
- **Coverage:** Global (-180 to 180 lon, -90 to 90 lat)
- **Dimension:** 64800 rows × 129600 columns
- **v2.0.7:** Single-band GeoTIFF (uint8, LCCS class codes)
- **v2.1.1:** NetCDF-4, 5 variables — use `lccs_class` band (terra selects by name)
- **Values:** Integer LCCS class codes, 0–220 (see legend)
- **No-data:** class 0

## High-level aggregation (10 classes, ESA CCI PUG Table 2)

Aggregation scheme is **identical between v2.0.7 and v2.1.1** (same LCCS class
system throughout the product family).

| Code | Class | Native LCCS codes |
|------|-------|-------------------|
| 1 | Cropland | 10, 11, 12, 20, 30 |
| 2 | Forest | 50, 60, 61, 62, 70, 71, 72, 80, 81, 82, 90, 100 |
| 3 | Shrubland | 120, 121, 122, 150, 151, 152, 153 |
| 4 | Grassland | 110, 130 |
| 5 | Wetland | 160, 170, 180 |
| 6 | Settlement | 190 |
| 7 | Bare | 200, 201, 202 |
| 8 | Snow/Ice | 220 |
| 9 | Water | 210 |
| 10 | Other | 40, 140 |

Class 40 (Mosaic nat veg >50% / cropland <50%) → Other (not crop-dominated).
Class 140 (Lichens and mosses) → Other (ESA CCI Other category).
Classes 150–153 (Sparse vegetation) → Shrubland (ESA CCI: "Shrubland incl. sparse veg").

## terra usage note

```r
# v2.1.1 NetCDF — select lccs_class layer by name
lc_raw  <- terra::rast("v2.1.1/C3S-LC-L4-LCCS-Map-300m-P1Y-2022-v2.1.1.nc")
lc_rast <- lc_raw[["lccs_class"]]  # or: lc_raw[[which(grepl("lccs_class", names(lc_raw)))]]

# terra::classify() — use right = NA for exact integer code matching
# right = TRUE creates empty half-open intervals and silently misclassifies all
# but the lowest code. right = NA gives exact [from, to] = [code, code] matching.
lc_hl <- terra::classify(lc_rast, rcl_matrix, others = NA, right = NA)
```

## Citation

ESA Climate Change Initiative — Land Cover project, 2017. Land Cover CCI
Product User Guide v2.0. European Space Agency.
http://www.esa-landcover-cci.org

Li, W., et al. (2018). Gross and net land cover changes in the main plant
functional types derived from the annual ESA CCI land cover maps (1992–2015).
Earth Syst. Sci. Data, 10, 219–234. doi:10.5194/essd-10-219-2018

Copernicus Climate Change Service (C3S), 2019. Land cover classification
gridded maps from 1992 to present derived from satellite observations.
Copernicus Climate Change Service (C3S) Climate Data Store (CDS).
doi:10.24381/cds.006f2c9a
