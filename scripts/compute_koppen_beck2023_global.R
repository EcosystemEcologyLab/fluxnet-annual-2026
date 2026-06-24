## compute_koppen_beck2023_global.R
## Compute global area-weighted Köppen-Geiger land area distribution from the
## Beck 2023 1991-2020 1 km raster.
##
## Method: terra::cellSize() computes geodesic pixel area in km² (latitude-
## aware; pixels at high latitudes are smaller than equatorial pixels).
## terra::zonal() sums areas by KG class without materialising the full raster.
## Ocean pixels are NA in the source raster and are excluded automatically.
##
## Output: data/snapshots/koppen_beck2023_global_distribution.csv
##
## Source raster:
##   Beck et al. (2023) Scientific Data 10:724. doi:10.1038/s41597-023-02549-6
##   Figshare v2: doi:10.6084/m9.figshare.21789074.v2
##   File: data/external/koppen_beck2023/1991_2020/koppen_geiger_0p00833333.tif

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
source("R/utils.R")
check_pipeline_config()

library(terra)
library(dplyr)
library(readr)

# ---- Paths ------------------------------------------------------------------
kg_dir    <- file.path("data", "external", "koppen_beck2023")
rast_path  <- file.path(kg_dir, "1991_2020", "koppen_geiger_0p00833333.tif")
leg_path   <- file.path(kg_dir, "legend.txt")
sites_path <- file.path(FLUXNET_DATA_ROOT, "snapshots", "site_koppen_beck2023.csv")
out_path   <- file.path(FLUXNET_DATA_ROOT, "snapshots",
                        "koppen_beck2023_global_distribution.csv")

for (p in c(rast_path, leg_path, sites_path)) {
  if (!file.exists(p)) stop("Required file not found: ", p, call. = FALSE)
}

# ---- Legend -----------------------------------------------------------------
message("Parsing legend ...")
leg_lines <- readLines(leg_path)
leg_data  <- leg_lines[grepl("^\\s*[0-9]+:", leg_lines)]

legend_df <- do.call(rbind, lapply(leg_data, function(ln) {
  m <- regmatches(ln, regexec(
    "^\\s*(\\d+):\\s+(\\S+)\\s+(.+?)\\s+\\[",
    ln, perl = TRUE
  ))[[1]]
  if (length(m) < 4L) return(NULL)
  data.frame(
    koppen_class_code = as.integer(m[2]),
    koppen_class      = trimws(m[3]),
    koppen_class_name = trimws(m[4]),
    stringsAsFactors  = FALSE
  )
}))

main_map <- c(A = "Tropical", B = "Arid", C = "Temperate", D = "Cold", E = "Polar")
legend_df <- legend_df |>
  dplyr::mutate(
    koppen_main      = substr(koppen_class, 1L, 1L),
    koppen_main_name = main_map[koppen_main]
  )

# ---- Load raster ------------------------------------------------------------
message("Loading KG raster: ", rast_path)
kg_rast <- terra::rast(rast_path)
message("  Dimensions: ", nrow(kg_rast), " × ", ncol(kg_rast), " cells",
        "  |  CRS: EPSG:", terra::crs(kg_rast, describe = TRUE)$code)

# ---- Compute cell areas (km²) -----------------------------------------------
# mask = TRUE: ocean (NA) pixels get NA area, so they are excluded from zonal()
message("Computing cell areas (latitude-weighted, km²) ...")
cell_areas <- terra::cellSize(kg_rast, mask = TRUE, unit = "km")

# ---- Zonal sum: total land area per KG class --------------------------------
message("Running zonal sum by KG class (may take a minute on 1 km global raster) ...")
t0         <- proc.time()
zone_areas <- terra::zonal(cell_areas, kg_rast, fun = "sum", na.rm = TRUE)
elapsed    <- round((proc.time() - t0)[["elapsed"]], 1)
names(zone_areas) <- c("koppen_class_code", "global_land_area_km2")
message("  Done in ", elapsed, " s  |  ", nrow(zone_areas), " zones found")

# Keep only valid KG classes (1–30); drop NA or 0 zones if present
zone_areas <- zone_areas |>
  dplyr::filter(koppen_class_code >= 1L, koppen_class_code <= 30L)

total_land_km2 <- sum(zone_areas$global_land_area_km2)
message("  Total land area: ", format(round(total_land_km2), big.mark = ","), " km²",
        "  (Earth ~148,940,000 km² — sanity check)")

# ---- Assemble 30-class output -----------------------------------------------
dist_30 <- legend_df |>
  dplyr::left_join(zone_areas, by = "koppen_class_code") |>
  dplyr::mutate(
    global_land_area_km2 = dplyr::coalesce(global_land_area_km2, 0),
    global_land_fraction = global_land_area_km2 / total_land_km2
  ) |>
  dplyr::select(
    koppen_class_code, koppen_class, koppen_class_name,
    koppen_main, koppen_main_name,
    global_land_area_km2, global_land_fraction
  ) |>
  dplyr::arrange(koppen_class_code)

# ---- 5-class aggregation ----------------------------------------------------
dist_5 <- dist_30 |>
  dplyr::group_by(koppen_main, koppen_main_name) |>
  dplyr::summarise(
    global_land_area_km2 = sum(global_land_area_km2),
    global_land_fraction = sum(global_land_fraction),
    .groups = "drop"
  ) |>
  dplyr::arrange(koppen_main)

# ---- Site-level comparison --------------------------------------------------
sites         <- readr::read_csv(sites_path, show_col_types = FALSE)
n_sites_total <- nrow(sites)

site_main <- sites |>
  dplyr::count(koppen_main, name = "n_sites") |>
  dplyr::mutate(site_fraction = n_sites / n_sites_total)

comparison <- dist_5 |>
  dplyr::left_join(site_main, by = "koppen_main") |>
  dplyr::mutate(
    n_sites       = dplyr::coalesce(n_sites, 0L),
    site_fraction = dplyr::coalesce(site_fraction, 0),
    # >1 = over-sampled relative to global area; <1 = under-sampled
    sampling_ratio = dplyr::if_else(
      global_land_fraction > 0,
      site_fraction / global_land_fraction,
      NA_real_
    )
  )

# ---- Console output ---------------------------------------------------------
cat("\n=== TOP 10 KG CLASSES BY GLOBAL LAND AREA ===\n")
top10 <- dist_30 |>
  dplyr::arrange(dplyr::desc(global_land_fraction)) |>
  dplyr::slice_head(n = 10) |>
  dplyr::mutate(
    area_Mkm2 = round(global_land_area_km2 / 1e6, 2),
    pct       = round(global_land_fraction * 100, 2)
  ) |>
  dplyr::select(koppen_class, koppen_class_name, area_Mkm2, pct)
print(as.data.frame(top10), row.names = FALSE)

cat("\n=== 5-CLASS SUMMARY: GLOBAL AREA vs NETWORK SITES ===\n")
comp_print <- comparison |>
  dplyr::mutate(
    area_pct  = round(global_land_fraction * 100, 1),
    site_pct  = round(site_fraction * 100, 1),
    ratio     = round(sampling_ratio, 2)
  ) |>
  dplyr::select(koppen_main, koppen_main_name, area_pct, n_sites, site_pct, ratio)
names(comp_print) <- c("Class", "Name", "Global area %", "Sites (n)",
                       "Network %", "Sampling ratio")
print(as.data.frame(comp_print), row.names = FALSE)
cat("\nSampling ratio > 1 = over-sampled; < 1 = under-sampled (vs global land area)\n")

cat("\n=== FULL 30-CLASS DISTRIBUTION ===\n")
full_print <- dist_30 |>
  dplyr::mutate(
    area_Mkm2 = round(global_land_area_km2 / 1e6, 2),
    pct       = round(global_land_fraction * 100, 2)
  ) |>
  dplyr::select(koppen_class, koppen_class_name, area_Mkm2, pct) |>
  dplyr::arrange(dplyr::desc(pct))
print(as.data.frame(full_print), row.names = FALSE)

# ---- Save CSV ---------------------------------------------------------------
readr::write_csv(dist_30, out_path)
message("\nSaved: ", out_path, " (", nrow(dist_30), " rows)")

write_output_metadata(
  out_path,
  input_sources = c(
    rast_path,
    paste0(
      "Beck et al. (2023) KG 1991-2020 1 km raster — ",
      "figshare doi:10.6084/m9.figshare.21789074.v2, file 61012822"
    )
  ),
  notes = paste0(
    "Global land area per KG class computed via terra::cellSize(mask=TRUE, unit='km') ",
    "summed with terra::zonal(fun='sum', na.rm=TRUE). Ocean pixels are NA in source ",
    "raster and are excluded. Total land area: ",
    format(round(total_land_km2), big.mark = ","), " km². ",
    "Elapsed (zonal step): ", elapsed, " s."
  )
)
