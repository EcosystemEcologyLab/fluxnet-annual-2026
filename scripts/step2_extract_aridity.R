## step2_extract_aridity.R
## Extract CGIAR Global Aridity Index v3.1 at all FLUXNET site locations and
## build the master site candidates table with all metadata joined.
##
## Outputs:
##   data/snapshots/site_aridity.csv          — site_id, aridity_index
##   data/snapshots/site_candidates_full.csv  — master site metadata table:
##     IGBP, UN subregion, GEZ, KG, record length, valid NEE years,
##     WorldClim MAT/MAP, aridity index + class

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
source("R/external_data.R")

library(terra)
library(dplyr)
library(readr)

# ---- Load snapshot metadata -------------------------------------------------
snap_file <- sort(
  list.files(file.path(FLUXNET_DATA_ROOT, "snapshots"),
             pattern = "fluxnet_shuttle_snapshot.*\\.csv$",
             full.names = TRUE),
  decreasing = TRUE
)[[1]]
message("Snapshot: ", snap_file)
snapshot_meta <- readr::read_csv(snap_file, show_col_types = FALSE)
message("Sites in snapshot: ", nrow(snapshot_meta))

# ---- Load aridity index raster ----------------------------------------------
message("\nLoading CGIAR aridity index raster ...")
ai_rast <- load_aridity_index(scale = FALSE)   # raw integers, scale after extract
message("Raster: ", terra::nrow(ai_rast), " rows × ", terra::ncol(ai_rast), " cols")

# ---- Extract at site locations -----------------------------------------------
site_coords <- snapshot_meta |>
  dplyr::select(site_id, location_lat, location_long) |>
  dplyr::filter(!is.na(location_lat), !is.na(location_long))

message("\nExtracting aridity index at ", nrow(site_coords), " site locations ...")
pts <- terra::vect(
  data.frame(x = site_coords$location_long, y = site_coords$location_lat),
  geom = c("x", "y"),
  crs  = "EPSG:4326"
)
extracted <- terra::extract(ai_rast, pts, ID = FALSE)

site_aridity <- dplyr::bind_cols(
  dplyr::select(site_coords, "site_id"),
  dplyr::rename(extracted, raw_value = 1)
) |>
  dplyr::mutate(aridity_index = raw_value / 10000) |>
  dplyr::select(site_id, aridity_index)

# ---- Summary and NA check ---------------------------------------------------
n_total <- nrow(site_aridity)
n_na    <- sum(is.na(site_aridity$aridity_index))

message("\n── Aridity index extraction summary ──")
message(sprintf("  Sites extracted:  %d", n_total))
message(sprintf("  Sites with NA:    %d", n_na))
if (n_na > 0L) {
  message("  NA sites: ",
          paste(site_aridity$site_id[is.na(site_aridity$aridity_index)],
                collapse = ", "))
}
message(sprintf(
  "  Range:  %.4f – %.4f  (median %.4f)",
  min(site_aridity$aridity_index, na.rm = TRUE),
  max(site_aridity$aridity_index, na.rm = TRUE),
  stats::median(site_aridity$aridity_index, na.rm = TRUE)
))

# ---- Save site_aridity.csv --------------------------------------------------
out_aridity <- file.path(FLUXNET_DATA_ROOT, "snapshots", "site_aridity.csv")
readr::write_csv(site_aridity, out_aridity)
message("\nSaved: ", out_aridity)

# ============================================================
# Build master site candidates table
# ============================================================

candidates_file <- file.path(
  FLUXNET_DATA_ROOT, "snapshots", "long_record_site_candidates_gez_kg.csv"
)
worldclim_file  <- file.path(FLUXNET_DATA_ROOT, "snapshots", "site_worldclim.csv")

if (!file.exists(candidates_file)) {
  stop("Candidates file not found: ", candidates_file, call. = FALSE)
}
if (!file.exists(worldclim_file)) {
  stop("WorldClim file not found: ", worldclim_file, call. = FALSE)
}

candidates  <- readr::read_csv(candidates_file,  show_col_types = FALSE)
site_wc     <- readr::read_csv(worldclim_file,    show_col_types = FALSE)

# ---- Aridity UNEP class thresholds ------------------------------------------
aridity_class <- function(ai) {
  dplyr::case_when(
    is.na(ai)     ~ NA_character_,
    ai <  0.05    ~ "Hyper-arid",
    ai <  0.20    ~ "Arid",
    ai <  0.50    ~ "Semi-arid",
    ai <  0.65    ~ "Dry sub-humid",
    .default      = "Humid"
  )
}

# ---- Join all metadata -------------------------------------------------------
site_candidates_full <- candidates |>
  dplyr::left_join(site_wc,      by = "site_id") |>
  dplyr::left_join(site_aridity, by = "site_id") |>
  dplyr::mutate(aridity_class = aridity_class(aridity_index))

message("\n── site_candidates_full.csv summary ──")
message(sprintf("  Rows: %d  |  Columns: %d",
                nrow(site_candidates_full), ncol(site_candidates_full)))
message("  Columns: ", paste(names(site_candidates_full), collapse = ", "))
message("\nAridity class distribution:")
print(table(site_candidates_full$aridity_class, useNA = "ifany"))

na_wc  <- sum(is.na(site_candidates_full$mat_worldclim))
na_ai  <- sum(is.na(site_candidates_full$aridity_index))
if (na_wc  > 0L) message("  WorldClim NA sites: ", na_wc)
if (na_ai  > 0L) message("  Aridity NA sites: ", na_ai)

# ---- Save -------------------------------------------------------------------
out_full <- file.path(FLUXNET_DATA_ROOT, "snapshots", "site_candidates_full.csv")
readr::write_csv(site_candidates_full, out_full)
message("\nSaved: ", out_full)

message("\nDone.")
