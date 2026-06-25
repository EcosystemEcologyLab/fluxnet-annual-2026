# ESA CCI Land Cover — data/external/cci_landcover/

## Product

**ESA Climate Change Initiative — Land Cover project, version 2.0.7, year 2015.**

Note: v2.1.1 (extending to 2020) is distributed via the Copernicus Climate Data
Store (CDS) and requires authenticated API access. v2.0.7 (2015) is the most
recent year available via anonymous CEDA HTTP. The LCCS class legend and
high-level aggregation scheme are identical across v2.0.7 and v2.1.1.

## Files

| File | Description | Size |
|------|-------------|------|
| `ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif` | Annual global land cover map, 300m GeoTIFF. Integer band: LCCS class codes 0–220. **gitignored.** | 312 MB |
| `ESACCI-LC-Legend.csv` | Native class legend (37 LCCS codes, RGB, labels). Semicolon-delimited. | 2 KB |
| `ESACCI-LCMapsColorLegend.qml` | QGIS QML color ramp file (per-native-class hex colors). | 5 KB |

## Download

```bash
# 300m GeoTIFF (2015, v2.0.7) from CEDA — anonymous HTTP
curl -L -o ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif \
  "https://dap.ceda.ac.uk/neodc/esacci/land_cover/data/land_cover_maps/v2.0.7/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif"

# Legend (same version URL pattern)
curl -L -o ESACCI-LC-Legend.csv \
  "https://data.ceda.ac.uk/neodc/esacci/land_cover/data/land_cover_maps/v2.0.7/ESACCI-LC-Legend.csv"
curl -L -o ESACCI-LCMapsColorLegend.qml \
  "https://data.ceda.ac.uk/neodc/esacci/land_cover/data/land_cover_maps/v2.0.7/ESACCI-LCMapsColorLegend.qml"
```

To upgrade to v2.1.1 (2020) via CDS API:
```r
# Requires ~/.cdsapirc with CDS API key
# pak::pak("ecmwfr")
# ecmwfr::wf_request(
#   user = "your-uid",
#   request = list(
#     dataset_short_name = "satellite-land-cover",
#     variable = "all",
#     year = "2020",
#     format = "zip"
#   ),
#   transfer = TRUE,
#   path = "."
# )
```

## Raster specification

- **CRS:** WGS 84 (EPSG:4326)
- **Resolution:** 300 m (approximately 0.002778° at equator)
- **Coverage:** Global
- **Dimension:** 64800 rows × 129600 columns (single band, uint8)
- **Values:** Integer LCCS class codes, 0–220 (see legend)
- **No-data:** class 0

## High-level aggregation (10 classes, ESA CCI PUG Table 2)

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

## Citation

ESA Climate Change Initiative — Land Cover project, 2017. Land Cover CCI
Product User Guide v2.0. Accessed via CEDA Data Archive.
http://www.esa-landcover-cci.org

Li, W., MacBean, N., Ciais, P., Defourny, P., Lamarche, C., Bontemps, S.,
Houghton, R. A., and Peng, S. (2018). Gross and net land cover changes in
the main plant functional types derived from the annual ESA CCI land cover
maps (1992–2015). Earth Syst. Sci. Data, 10, 219–234.
doi:10.5194/essd-10-219-2018
