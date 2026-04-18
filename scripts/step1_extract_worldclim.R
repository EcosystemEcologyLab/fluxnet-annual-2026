## step1_extract_worldclim.R
## Extract WorldClim bio1 (MAT) and bio12 (MAP) at FLUXNET site locations.
## Output: data/snapshots/site_worldclim.csv

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
library(terra)
library(dplyr)
library(readr)

# ---- Load WorldClim raster --------------------------------------------------
wc_dir <- file.path("data", "external", "worldclim", "climate", "wc2.1_2.5m")
tif_files <- sort(list.files(wc_dir, pattern = "\\.tif$", full.names = TRUE))
message("Loading ", length(tif_files), " WorldClim TIF files ...")
wc_rast <- terra::rast(tif_files)
message("  Layers: ", paste(names(wc_rast)[c(1, 12)], collapse = ", "), " ...")

# ---- Load site coordinates from latest snapshot ----------------------------
snap_file <- sort(
  list.files(file.path(FLUXNET_DATA_ROOT, "snapshots"),
             pattern = "fluxnet_shuttle_snapshot.*\\.csv$",
             full.names = TRUE),
  decreasing = TRUE
)[[1]]
message("Snapshot: ", basename(snap_file))
snapshot <- readr::read_csv(snap_file, show_col_types = FALSE)

site_coords <- snapshot |>
  dplyr::distinct(site_id, location_lat, location_long) |>
  dplyr::filter(!is.na(location_lat), !is.na(location_long))

message("Extracting WorldClim at ", nrow(site_coords), " sites ...")

# ---- Extract at site locations ----------------------------------------------
pts <- terra::vect(
  data.frame(x = site_coords$location_long,
             y = site_coords$location_lat),
  geom = c("x", "y"),
  crs  = "EPSG:4326"
)
wc_vals <- as.data.frame(terra::extract(wc_rast, pts, ID = FALSE))

# Identify bio1 and bio12 columns (robust to layer naming)
bio1_col <- grep(
  "bio[_.]?0?1([^0-9]|$)", names(wc_vals),
  value = TRUE, perl = TRUE
)[1]
bio12_col <- grep(
  "bio[_.]?12([^0-9]|$)", names(wc_vals),
  value = TRUE, perl = TRUE
)[1]

if (is.na(bio1_col) || is.na(bio12_col)) {
  stop("Cannot identify bio1/bio12 in layers: ",
       paste(head(names(wc_vals), 25), collapse = ", "), call. = FALSE)
}
message("  bio1 column: ", bio1_col, "  |  bio12 column: ", bio12_col)

# WorldClim 2.1 stores MAT in °C directly; 1.x stores °C × 10.
mat_raw  <- wc_vals[[bio1_col]]
mat_vals <- if (max(abs(mat_raw), na.rm = TRUE) > 70) mat_raw / 10 else mat_raw
map_vals <- wc_vals[[bio12_col]]

# ---- Assemble output -------------------------------------------------------
site_worldclim <- dplyr::bind_cols(
  dplyr::select(site_coords, site_id),
  data.frame(
    mat_worldclim = mat_vals,
    map_worldclim = map_vals
  )
)

# ---- Summary ---------------------------------------------------------------
cat("\n--- mat_worldclim (°C) ---\n")
print(summary(site_worldclim$mat_worldclim))
cat("\n--- map_worldclim (mm yr-1) ---\n")
print(summary(site_worldclim$map_worldclim))

na_sites <- site_worldclim |>
  dplyr::filter(is.na(mat_worldclim) | is.na(map_worldclim)) |>
  dplyr::pull(site_id)

if (length(na_sites) > 0L) {
  cat("\nWARNING — sites with NA extraction (", length(na_sites), "):\n",
      paste(sort(na_sites), collapse = ", "), "\n")
} else {
  cat("\nAll ", nrow(site_worldclim), " sites extracted without NA.\n")
}

# ---- Save ------------------------------------------------------------------
out_path <- file.path(FLUXNET_DATA_ROOT, "snapshots", "site_worldclim.csv")
readr::write_csv(site_worldclim, out_path)
message("Saved: ", out_path)
