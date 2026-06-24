# Data Inventory — FLUXNET Annual Paper 2026

This document is a snapshot inventory of all datasets, rasters, processed files, and
external data products currently available in this repository. It was generated on
**2026-06-24** by walking the repository file system and querying the DuckDB database.
It reflects the state of the analysis as of the 2026-06-02 pipeline run on the 759-site
dataset. As new external products are downloaded or the pipeline is re-run, this file
should be regenerated.

**Why this exists:** The inventory was created to support planning for the network
representativeness figure work and to serve as a template for similar inventories in
future synthesis projects.

## How to use this document

| Section | What you'll find |
|---|---|
| [data/snapshots/](#datasnapshots) | FLUXNET shuttle snapshots, site metadata, historical dataset site lists |
| [data/lists/](#datalists) | Raw source files for historical FLUXNET datasets (Excel) |
| [data/extracted/](#dataextracted) | Per-site extracted CSV files (6.9 GB) |
| [data/raw/](#dataraw) | Raw downloads (currently empty) |
| [data/processed/](#dataprocessed) | Intermediate RDS files from the R pipeline |
| [data/duckdb/](#dataduckdb) | Canonical DuckDB database (14 GB, 16 tables) |
| [data/external/](#dataexternal) | External rasters and shapefiles for site characterisation |
| [outputs/](#outputs) | Pipeline output data products (authorship, citations, QC logs) |
| [Gaps](#gaps) | Data products needed for representativeness work that are not yet present |

---

## data/snapshots/

The canonical committable part of `data/`. Contains FLUXNET shuttle metadata snapshots,
derived site-level characterisation tables, historical dataset site lists, and download
tracking files. All CSVs here are either directly from `flux_listall()` or produced by
analysis scripts and written via `write_output_metadata()`.

### FLUXNET shuttle snapshots

| Path | Type | Size | Description |
|---|---|---|---|
| `fluxnet_shuttle_snapshot_20260601T224043.csv` | CSV | 539 KB | **Canonical snapshot.** Most recent shuttle metadata snapshot; 759 sites, 18 columns from `flux_listall()`. Written by `write_snapshot()` in `01_download.R` before the 2026-06-01 download run. This is the authoritative citation and metadata source for the paper. |
| `fluxnet_shuttle_snapshot_20260601T161559.csv` | CSV | 539 KB | Second 2026-06-01 snapshot (gap-fill re-download run); 759 sites. Differs from the canonical by timestamp only. |
| 16 × `fluxnet_shuttle_snapshot_20260414T*.csv` | CSV | 477 KB each | Snapshots from the April 2026 bulk download session; 672 sites each. Pre-TERN; superseded by the June snapshots. |
| `fluxnet_shuttle_snapshot_20260428T231049.csv` | CSV | 507 KB | Snapshot taken 2026-04-28; between April and May lineages. |
| 2 × `fluxnet_shuttle_snapshot_20260519T*.csv` | CSV | 513 KB each | Snapshots taken 2026-05-19/20; TERN added, ~720 sites. |
| 2 × `fluxnet_shuttle_snapshot_20260402T*.csv` | CSV | 214 KB each | Early April snapshots; smaller network coverage. |
| 2 × `fluxnet_shuttle_snapshot_20260328T*.csv` | CSV | 205 KB each | Earliest snapshots (2026-03-28); pilot download, ~672 sites. |
| `fluxnet_shuttle_snapshot_20260412T024606.csv` | CSV | 208 KB | Single mid-April snapshot. |

**Total:** 26 shuttle snapshot CSVs, spanning 2026-03-28 to 2026-06-01.

### Site-level characterisation tables

These tables are derived from the pipeline and join external data onto shuttle site IDs.
All cover 759 sites unless noted.

| Path | Type | Size | Rows | Description |
|---|---|---|---|---|
| `site_year_data_presence.csv` | CSV | 624 KB | 32,018 | Per-site per-year valid data counts. Columns: `site_id`, `year`, `n_months_present`, `has_data`. Produced by `compute_site_year_presence()` in `03_read.R`. Authoritative input for `is_functionally_active()`. |
| `site_gez_lookup.csv` | CSV | 30 KB | 759 | FAO Global Ecological Zone assignment per site. Columns: `site_id`, `gez_name`, `gez_code`, `gez_method`. Produced by `step3_extract_gez.R` from the GEZ 2010 shapefile. |
| `site_worldclim.csv` | CSV | 22 KB | 759 | WorldClim v2.1 bioclimatic variables per site. Columns: `site_id`, `mat_worldclim` (bio1, °C), `map_worldclim` (bio12, mm yr⁻¹). Produced by `step1_extract_worldclim.R`. |
| `site_aridity.csv` | CSV | 10 KB | 759 | CGIAR Global Aridity Index v3.1 per site. Columns: `site_id`, `aridity_index` (true AI = raw/10000). Produced by `step2_extract_aridity.R`. |
| `site_candidates_full.csv` | CSV | 85 KB | **569** | Master site metadata table joining shuttle metadata, GEZ, KG, WorldClim, and aridity. 20 columns including `currently_selected`, `n_years_valid_nee`, `kg_class`. **STALE — 569 rows reflects the April 716-site lineage, not the current 759-site dataset.** Rebuild after updating NEE presence data. |
| `long_record_site_candidates_gez_kg.csv` | CSV | 64 KB | **569** | Subset of `site_candidates_full.csv` with long-record sites stratified by GEZ and Köppen-Geiger. 17 columns (excludes `mat_worldclim`, `map_worldclim`, `aridity_index`, `aridity_class` vs. full table). **STALE — same lineage issue as above.** |
| `long_record_site_candidates_gez.csv` | CSV | 59 KB | **569** | Similar to above without Köppen-Geiger columns. **STALE.** |
| `long_record_sites.csv` | CSV | 824 B | 25 | Short list of long-record candidate site IDs; produced during candidate selection. |

### Historical FLUXNET dataset site lists

These tables allow cross-referencing the current Shuttle dataset against the three prior
global FLUXNET synthesis datasets. Format for all: CSV with `site_id` (plus `first_year`,
`last_year` in the `years_*` files).

| Path | Type | Size | Rows | Description |
|---|---|---|---|---|
| `sites_marconi_clean.csv` | CSV | 3.5 KB | **35** | Marconi Conference dataset (Falge et al. 2001): 35 sites, matched to modern FLUXNET IDs. 3 of the original 38 Marconi sites could not be crosswalked. |
| `years_marconi.csv` | CSV | 624 B | 35 | Site-year coverage list for Marconi sites; `site_id`, `first_year`, `last_year`. |
| `sites_la_thuile_clean.csv` | CSV | 25 KB | **252** | La Thuile synthesis dataset (2007): 252 sites with cleaned metadata. |
| `years_la_thuile.csv` | CSV | 4.2 KB | 252 | Site-year coverage list for La Thuile. |
| `sites_fluxnet2015_clean.csv` | CSV | 14 KB | **212** | FLUXNET2015 release (Pastorello et al. 2020): 212 sites with cleaned metadata. |
| `years_fluxnet2015.csv` | CSV | 3.5 KB | 212 | Site-year coverage list for FLUXNET2015; 1,532 site-years. |

### Download tracking

| Path | Type | Size | Description |
|---|---|---|---|
| `download_progress.csv` | CSV | 5.9 KB | Batch download progress tracking for the April 2026 run; 7 columns (`batch_num`, `n_sites`, `status`, `started_at`, `completed_at`, `disk_free_gb`). |
| `download_progress_local.csv` | CSV | 5.4 KB | Download progress for the 2026-06-01 local re-download run. |
| `download_progress_gap_20260601.csv` | CSV | 465 B | Gap-site re-download tracking (2026-06-01). |

### Archive

| Path | Type | Size | Description |
|---|---|---|---|
| `archive_20260507/site_year_data_presence_pre_regeneration.csv` | CSV | 557 KB | Backup of `site_year_data_presence.csv` taken before the 2026-05-07 `presence_df` refactor. Retained for lineage tracing. Do not use in analysis. |

---

## data/lists/

Raw source files for the three historical FLUXNET datasets. These Excel/XLS files are the
upstream sources that `R/historical_datasets.R` reads to produce the cleaned CSVs in
`data/snapshots/`.

| Path | Type | Size | Description |
|---|---|---|---|
| `Marconi_to_Modern_SiteIDs.xlsx` | XLSX | 10 KB | Crosswalk from Marconi Conference original site IDs to current FLUXNET site IDs. Primary reference for the 35-site Marconi list. |
| `FLUXNET2015.xlsx` | XLSX | 28 KB | FLUXNET2015 site metadata table (212 sites). Source for `sites_fluxnet2015_clean.csv`. |
| `FLUXNET2015_SiteLocations.xlsx` | XLSX | 55 KB | FLUXNET2015 site locations and extended metadata. |
| `LaThuileList.xlsx` | XLSX | 29 KB | La Thuile site list (252 sites). Source for `sites_la_thuile_clean.csv`. |
| `LaThuile_SiteMetadata.xls` | XLS | 218 KB | Extended La Thuile site metadata including IGBP, coordinates, and contact information. |

---

## data/extracted/

Extracted per-site CSV files produced by `flux_extract()` in `02_extract.R`. Not committed
to git; regenerated by re-running `01_download.R` → `02_extract.R`.

| Path | Type | Size | Description |
|---|---|---|---|
| `data/extracted/` (759 site directories) | Directory tree | **6.9 GB** | One directory per site, named `{HUB}_{SITE_ID}_FLUXNET_{YEARS}_v1.3_r1`. |

**Per-site directory contents** (example: `AMF_AR-Bal_FLUXNET_2012-2013_v1.3_r1/`):

| File pattern | Description |
|---|---|
| `*_BIF_*.csv` | BADM/BIF site metadata (IGBP, coordinates, team members, measurement heights, etc.) |
| `*_BIFVARINFO_{DD,MM,YY}_*.csv` | Variable-level metadata (units, measurement height, QC flags) per resolution |
| `*_ERA5_{DD,MM,YY}_*.csv` | ERA5 reanalysis climate variables for the site location, 1981–2025 |
| `*_FLUXMET_{DD,MM,YY}_*.csv` | Measured flux and meteorological data at daily, monthly, and annual resolution |

Sites with sub-daily data include `_FLUXMET_HH_` (AmeriFlux half-hourly) or `_FLUXMET_HR_`
(ICOS/AmeriFlux hourly) files in addition to the coarser resolutions above.

---

## data/raw/

| Path | Type | Size | Description |
|---|---|---|---|
| `data/raw/` | Directory | **0 B** | Empty. Downloaded ZIP files were extracted directly without intermediate storage in this pipeline run. ZIP files are not retained after extraction. |

---

## data/processed/

Intermediate RDS files produced by the R pipeline scripts. These are regenerable from
`data/extracted/` and the DuckDB database and are gitignored. The DuckDB tables in
`data/duckdb/` are the canonical source for analysis; these RDS files are retained as
a convenience cache and for the MM/YY resolutions where the in-memory RDS path predates
the DuckDB pipeline.

| Path | Type | Size | Description |
|---|---|---|---|
| `badm.rds` | RDS | 6.9 MB | Full BADM data frame for all 759 sites, produced by `flux_badm()` in `03_read.R`. Contains site metadata, team information, measurement details from BIF files. |
| `var_info.rds` | RDS | 2.3 MB | Variable information data frame from `flux_varinfo()` across all sites and resolutions. |
| `file_inventory.rds` | RDS | 258 KB | File discovery manifest produced by `flux_discover_files()`. **Note:** metadata columns reflect a fresh `flux_listall()` call at the time `02_extract.R` was run, not the locked snapshot. Use `data/snapshots/fluxnet_shuttle_snapshot_20260601T224043.csv` for citation-critical metadata. |
| `flux_data_raw_yy.rds` | RDS | 10 MB | Raw annual (YY) flux data for all sites before QC. |
| `flux_data_raw_mm.rds` | RDS | 123 MB | Raw monthly (MM) flux data for all sites before QC. |
| `flux_data_raw_dd_partial.rds` | RDS | 411 MB | Partial daily (DD) read — pipeline was interrupted at site ~150/759 due to 16 GB memory ceiling before the DuckDB path was implemented. Superseded by `data/duckdb/fluxnet.duckdb`. Retained for lineage reference only. |
| `flux_data_raw_dd_done_sites.rds` | RDS | 388 B | Tiny companion to `flux_data_raw_dd_partial.rds` recording which sites completed before the OOM. |
| `flux_data_qc_yy.rds` | RDS | 10 MB | Annual data after QC gating (NEE_VUT_REF_QC ≥ 0.50). |
| `flux_data_qc_mm.rds` | RDS | 117 MB | Monthly data after QC gating. |
| `flux_data_converted_yy.rds` | RDS | 12 MB | Annual data after unit conversion. Companion `.meta.json` present. |
| `flux_data_converted_mm.rds` | RDS | 133 MB | Monthly data after unit conversion. Companion `.meta.json` present. |

---

## data/duckdb/

The canonical analysis database. Produced by `03b_create_database.R` → `04_qc.R` →
`05_units.R`. All figure scripts in `07_figures.R` read from this database.

| Path | Type | Size | Description |
|---|---|---|---|
| `fluxnet.duckdb` | DuckDB | **14 GB** | Single-file DuckDB database containing all flux data across resolutions and pipeline stages. |

**Tables and row counts:**

| Table | Rows | Description |
|---|---|---|
| `manifest` | 4,558 | File-level manifest (one row per extracted CSV file, not per site). |
| `annual` | 40,087 | Raw annual (YY) flux and climate data, all sites. |
| `annual_qc` | 40,062 | Annual data after NEE QC gating (NEE_VUT_REF_QC ≥ 0.50). |
| `annual_converted` | 40,062 | Annual data after unit conversion. **Primary analysis table for annual figures.** |
| `monthly` | 481,044 | Raw monthly (MM) data, all sites. |
| `monthly_qc` | 476,875 | Monthly data after QC gating. |
| `monthly_converted` | 476,875 | Monthly data after unit conversion. |
| `weekly` | 3,640 | Raw weekly (WW) data. |
| `weekly_qc` | 3,603 | Weekly data after QC gating. |
| `weekly_converted` | 3,603 | Weekly data after unit conversion. |
| `daily` | 14,641,764 | Raw daily (DD) data, all sites (~14.6 M rows). |
| `daily_qc` | 14,440,004 | Daily data after QC gating. |
| `daily_converted` | 14,440,004 | Daily data after unit conversion. **Primary analysis table for seasonal figures.** |
| `hourly` | 613,608 | Sub-daily (HH/HR) data. |
| `hourly_qc` | 527,439 | Sub-daily data after QC gating. |
| `hourly_converted` | 527,439 | Sub-daily data after unit conversion. |

---

## data/external/

External reference datasets downloaded from third-party sources. Used for site
characterisation and figure backgrounds. Total: **2.7 GB**.

### CGIAR Global Aridity Index v3.1

**Source:** Zomer et al. (2022), Scientific Data. doi:10.6084/m9.figshare.7504448  
**Temporal coverage:** Long-term mean (1970–2000 baseline)  
**Spatial resolution:** 30 arc-second (~1 km)  
**Location:** `data/external/aridity/Global-AI_ET0__annual_v3_1/`

| File | Type | Size | Variable | Units |
|---|---|---|---|---|
| `ai_v31_yr.tif` | GeoTIFF | 386 MB | Annual aridity index (AI = P/ET₀) | Dimensionless × 10,000 (divide by 10,000 for true AI) |
| `et0_v31_yr.tif` | GeoTIFF | 211 MB | Annual reference evapotranspiration (ET₀) | mm yr⁻¹ × 100 |
| `et0_v31_yr_sd.tif` | GeoTIFF | 62 MB | ET₀ inter-annual standard deviation | mm yr⁻¹ × 100 |
| `Global-AI_ET0__annual_v3_1.zip` | ZIP | — | Source archive (retained) | — |

Site-level AI values extracted to `data/snapshots/site_aridity.csv`.

### FAO Global Ecological Zones 2010

**Source:** FAO (2012), Global Ecological Zones for FAO Forest Reporting: 2010 Update  
**Temporal reference:** 2010  
**Spatial resolution:** Vector (polygon shapefile)  
**Location:** `data/external/gez/`

| File | Type | Size | Description |
|---|---|---|---|
| `gez_2010_wgs84.shp` | Shapefile | 91 MB | GEZ polygon boundaries in WGS84. 14 GEZ classes (Tropical rainforest, Tropical moist, Tropical dry, Tropical mountain, Subtropical humid, Subtropical dry, Subtropical steppe, Subtropical mountain, Temperate oceanic, Temperate continental, Temperate mountain, Boreal, Polar, Water). |
| `gez_2010_wgs84.{dbf,prj,sbn,sbx,shx}` | Shapefile components | — | Attribute table, projection, spatial indices. |
| `gez2010.zip` | ZIP | — | Source archive (retained) | |

Site-level GEZ assignments extracted to `data/snapshots/site_gez_lookup.csv`.

### WorldClim v2.1 Bioclimatic Variables

**Source:** Fick & Hijmans (2017), International Journal of Climatology 37:4302  
**Temporal coverage:** 1970–2000 baseline  
**Spatial resolution:** 2.5 arc-minute (~5 km)  
**Location:** `data/external/worldclim/climate/wc2.1_2.5m/`

| File | Type | Size | Variable | Units |
|---|---|---|---|---|
| `wc2.1_2.5m_bio_1.tif` | GeoTIFF | 48 MB | BIO1: Annual mean temperature (MAT) | °C × 10 |
| `wc2.1_2.5m_bio_2.tif` | GeoTIFF | 46 MB | BIO2: Mean diurnal range | °C × 10 |
| `wc2.1_2.5m_bio_3.tif` | GeoTIFF | 43 MB | BIO3: Isothermality | % |
| `wc2.1_2.5m_bio_4.tif` | GeoTIFF | 47 MB | BIO4: Temperature seasonality | SD × 100 |
| `wc2.1_2.5m_bio_5.tif` | GeoTIFF | 44 MB | BIO5: Max temperature of warmest month | °C × 10 |
| `wc2.1_2.5m_bio_6.tif` | GeoTIFF | 47 MB | BIO6: Min temperature of coldest month | °C × 10 |
| `wc2.1_2.5m_bio_7.tif` | GeoTIFF | 46 MB | BIO7: Temperature annual range | °C × 10 |
| `wc2.1_2.5m_bio_8.tif` | GeoTIFF | 46 MB | BIO8: Mean temperature of wettest quarter | °C × 10 |
| `wc2.1_2.5m_bio_9.tif` | GeoTIFF | 44 MB | BIO9: Mean temperature of driest quarter | °C × 10 |
| `wc2.1_2.5m_bio_10.tif` | GeoTIFF | 44 MB | BIO10: Mean temperature of warmest quarter | °C × 10 |
| `wc2.1_2.5m_bio_11.tif` | GeoTIFF | 45 MB | BIO11: Mean temperature of coldest quarter | °C × 10 |
| `wc2.1_2.5m_bio_12.tif` | GeoTIFF | 48 MB | BIO12: Annual precipitation (MAP) | mm yr⁻¹ |
| `wc2.1_2.5m_bio_13.tif` | GeoTIFF | 46 MB | BIO13: Precipitation of wettest month | mm |
| `wc2.1_2.5m_bio_14.tif` | GeoTIFF | 42 MB | BIO14: Precipitation of driest month | mm |
| `wc2.1_2.5m_bio_15.tif` | GeoTIFF | 44 MB | BIO15: Precipitation seasonality | CV |
| `wc2.1_2.5m_bio_16.tif` | GeoTIFF | 46 MB | BIO16: Precipitation of wettest quarter | mm |
| `wc2.1_2.5m_bio_17.tif` | GeoTIFF | 43 MB | BIO17: Precipitation of driest quarter | mm |
| `wc2.1_2.5m_bio_18.tif` | GeoTIFF | 45 MB | BIO18: Precipitation of warmest quarter | mm |
| `wc2.1_2.5m_bio_19.tif` | GeoTIFF | 45 MB | BIO19: Precipitation of coldest quarter | mm |
| `wc2.1_2.5m_bio.zip` | ZIP | — | Source archive (retained) | — |

Only BIO1 (MAT) and BIO12 (MAP) are currently extracted to site level (see
`data/snapshots/site_worldclim.csv`). The remaining 17 variables are available on disk
for future use (e.g., climate-space representativeness figures using PCA across bio variables).

---

## outputs/

Non-figure data products produced by the analysis pipeline. These files are gitignored and
regenerated each run, but are listed here as they are data products that feed downstream
analyses.

### Authorship outputs

| Path | Type | Size | Description |
|---|---|---|---|
| `authorship/site_authors.csv` | CSV | 22 KB | Site-level author assignments: one row per site, columns `site_id`, author counts by role, `author_row` (rubric row assignment). |
| `authorship/authorship_invitations.csv` | CSV | 191 KB | Full invitation list with one row per team member per site; includes name, email, role, hub, and invitation status. |
| `authorship/authors_by_network.csv` | CSV | 151 B | Summary: author counts by hub (AmeriFlux, ICOS, TERN). |
| `authorship/diagnostics/` | CSVs | — | 6 diagnostic tables: contact coverage comparison, fallback by network, fallback site details, sensitivity analysis (5-yr boundary), sites with no presence data, year-2025 availability by network. Each has a companion `.meta.json`. |

### QC and pipeline logs

| Path | Type | Description |
|---|---|---|
| `exclusion_log.csv` | CSV | Records every site-year excluded from analysis, reason, and QC column used. Regenerated by `04_qc.R`. |
| `unknown_log.csv` | CSV | Records sites/records that could not be assessed (missing data, failed download). Regenerated by `04_qc.R`. |

### Citation outputs

| Path | Type | Description |
|---|---|---|
| `full_site_citations.bib` | BibTeX | Per-site citation entries for all 759 sites + mandated references. Produced by `scripts/generate_fluxnet_citations.R`. |
| `full_site_citations_acknowledgments.md` | Markdown | Network acknowledgment boilerplate for the paper. |
| `full_site_citations_review_flags.md` | Markdown | Per-site human-review flags (preserved typos, missing identifiers, no-author entries). |

---

## Gaps

This section inventories data products that are not currently present in the repository
but have been identified as potentially needed for the **network representativeness figure**
work and related analyses.

| Product | Status | Notes |
|---|---|---|
| **WorldClim v2.1** (bio_1 – bio_19) | **Present** — `data/external/worldclim/` | All 19 variables at 2.5 arc-min. Only bio_1 and bio_12 currently extracted to site level; remaining 17 available on disk. |
| **FAO Global Ecological Zones 2010** | **Present** — `data/external/gez/` | Polygon shapefile; all 759 sites assigned via spatial join. |
| **CGIAR Aridity Index v3.1** | **Present** — `data/external/aridity/` | Annual AI raster at 30 arc-sec; all 759 sites extracted. |
| **Köppen-Geiger classification (from FLUXNET BADM)** | **Present** — `data/snapshots/site_candidates_full.csv` | KG class extracted from BADM CLIMATE_KOEPPEN variable; available in `kg_class`, `kg_second`, `kg_main` columns. Level 1–3 classification for 569 sites (stale). |
| **Beck et al. 2023 Köppen-Geiger raster** (1991–2020, present-day) | **Not present** — needs download | The Beck 2023 gridded KG product (30 arc-sec) for present-day climate. More consistent for spatial representativeness analysis than the site-level BADM values. Source: doi:10.1038/s41597-023-02549-6 |
| **Beck et al. 2023 KG future projections** (SSP scenarios) | **Not present** — needs download | Future KG under SSP1-2.6, SSP2-4.5, SSP5-8.5 through 2100. Same source as above. |
| **ESA CCI Land Cover** (any year, 2000–2020) | **Not present** — needs download | 300 m annual land cover maps. Useful for landcover-class representativeness. Source: ESA CCI Land Cover viewer / climate.esa.int |
| **ESA CCI Biomass v5** | **Not present** — needs download | 100 m aboveground biomass; 2010, 2017, 2018, 2019, 2020. Source: CEDA archive. |
| **TRENDY v11 model outputs** | **Not present** — needs download | Multi-model ensemble of terrestrial carbon cycle simulations (GPP, NEE, Reco) for benchmarking flux tower observations against global models. Requires data agreement; request from TRENDY coordinators. |
| **CMIP6 climate projections** | **Not present** — needs download | GCM climate outputs (e.g., GFDL-ESM4, MPI-ESM1-2-HR) for future climate representativeness analysis. Available from ESGF nodes. |

### Summary for representativeness figure planning

**Available now (no download needed):**
- Climate space: WorldClim BIO1/BIO12 per site ✓; full 19-variable BIO rasters on disk ✓
- Aridity gradient: CGIAR AI v3.1 per site ✓
- Biome/ecosystem: GEZ 2010 per site ✓; KG from BADM per site (stale set) ✓
- Historical reference networks: Marconi (35), La Thuile (252), FLUXNET2015 (212) ✓

**Needs download before representativeness figures can be finalised:**
- Beck 2023 KG raster — needed for spatially consistent KG coverage maps
- ESA CCI Land Cover — needed if landcover is used as a representativeness axis
- ESA CCI Biomass — needed if biomass is a representativeness axis

**Lower priority / long-lead:**
- TRENDY v11 — needs institutional data agreement
- CMIP6 — large volumes; needed only for future-climate representativeness

### Notable findings during inventory

1. **`data/raw/` is empty (0 B).** ZIP files from `flux_download()` were extracted in-place and not retained. This is by design but means re-extraction requires a full re-download.
2. **`site_candidates_full.csv` and both `long_record_site_candidates_*` files are stale** (569 rows, April 716-site lineage). These need rebuilding at 759 sites before any figure that joins on `currently_selected` or long-record status is finalised. See `docs/decisions_pending.md` Priority 3.
3. **`flux_data_raw_dd_partial.rds` (411 MB) is a dead artifact.** It predates the DuckDB path and was an interrupted run. It can be deleted to reclaim 411 MB once the DuckDB pipeline is confirmed stable.
4. **26 shuttle snapshot CSVs are retained in `data/snapshots/`.** The 16 from 2026-04-14 (477 KB each) and the two 2026-03-28 files collectively account for ~8 MB; they are retained for lineage tracing but could be archived or pruned if space becomes an issue.
5. **`data/external/` contains both ZIP archives and extracted rasters.** The three ZIP files (`Global-AI_ET0__annual_v3_1.zip`, `gez2010.zip`, `wc2.1_2.5m_bio.zip`) collectively account for significant disk space. Once extraction is confirmed complete, the ZIPs could be deleted.
6. **Only two of the 19 WorldClim bioclimatic variables are currently extracted to site level** (BIO1 and BIO12). The remaining 17 TIFs are present on disk and can be extracted with a single `step1_extract_worldclim.R` run if a richer climate-space PCA is needed for representativeness figures.
