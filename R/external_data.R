# R/external_data.R
# Loaders for external reference datasets used in the FLUXNET Annual Paper 2026.
#
# Functions:
#   load_worldclim()     — load WorldClim v2.1 bioclim SpatRaster from the local
#                          cache downloaded by geodata::worldclim_global()
#   load_aridity_index() — load CGIAR Global Aridity Index v3.1 SpatRaster
#                          (ai_v31_yr.tif) from the local cache

library(terra)

# Expected path written by geodata::worldclim_global(var="bio", res=2.5,
# path="data/external/worldclim/"):
.WORLDCLIM_DEFAULT_SUBPATH <- file.path(
  "external", "worldclim", "climate", "wc2.1_2.5m"
)

#' Load WorldClim v2.1 bioclim raster
#'
#' Loads all 19 bioclimatic variable TIF files from the local WorldClim cache
#' into a \pkg{terra} \code{SpatRaster}.  The default path matches where
#' \code{geodata::worldclim_global(var = "bio", res = 2.5,
#' path = "data/external/worldclim/")} writes its files.
#'
#' Download with:
#' \preformatted{
#' geodata::worldclim_global(
#'   var  = "bio",
#'   res  = 2.5,
#'   path = "data/external/worldclim/"
#' )
#' }
#'
#' Variables in the returned raster (layer names \code{wc2.1_2.5m_bio_1} …
#' \code{wc2.1_2.5m_bio_19}):
#' \itemize{
#'   \item bio1 — Annual Mean Temperature (°C)
#'   \item bio12 — Annual Precipitation (mm)
#'   \item bio1–bio19 — full set of WorldClim bioclimatic variables
#' }
#'
#' @param path Character. Full path to the directory containing the WorldClim
#'   TIF files.  Defaults to
#'   \code{<FLUXNET_DATA_ROOT>/external/worldclim/climate/wc2.1_2.5m/}.
#'
#' @return A \pkg{terra} \code{SpatRaster} with 19 layers at 2.5 arcminute
#'   resolution.
#'
#' @examples
#' \dontrun{
#' wc <- load_worldclim()
#' terra::nlyr(wc)   # 19
#' terra::names(wc)  # "wc2.1_2.5m_bio_1", ..., "wc2.1_2.5m_bio_19"
#' }
#'
#' @export
load_worldclim <- function(path = NULL) {
  if (is.null(path)) {
    path <- file.path(
      Sys.getenv("FLUXNET_DATA_ROOT", unset = "data"),
      .WORLDCLIM_DEFAULT_SUBPATH
    )
  }

  if (!dir.exists(path)) {
    stop(
      "WorldClim directory not found: '", path, "'\n",
      "Download with:\n",
      "  geodata::worldclim_global(var='bio', res=2.5,",
      " path='data/external/worldclim/')",
      call. = FALSE
    )
  }

  tif_files <- sort(list.files(path, pattern = "\\.tif$",
                                full.names = TRUE, recursive = FALSE))
  if (length(tif_files) == 0L) {
    stop(
      "No TIF files found in '", path, "'\n",
      "Download with:\n",
      "  geodata::worldclim_global(var='bio', res=2.5,",
      " path='data/external/worldclim/')",
      call. = FALSE
    )
  }

  message("load_worldclim(): reading ", length(tif_files),
          " TIF files from ", path)
  terra::rast(tif_files)
}

# Expected path for CGIAR Global Aridity Index v3.1
# Source: https://figshare.com/articles/dataset/Global_Aridity_Index_and_Potential_Evapotranspiration_ET0_Climate_Database_v3/7504448
# Downloaded via: https://ndownloader.figshare.com/files/56300327
# File: Global-AI_ET0__annual_v3_1.zip → Global-AI_ET0__annual_v3_1/ai_v31_yr.tif
# Values are scaled integers: divide by 10000 to get actual aridity index (0–~6.5).
# AI = MAP / PET; values < 0.03 = hyper-arid, 0.03–0.2 = arid, 0.2–0.5 = semi-arid,
# 0.5–0.65 = dry sub-humid, > 0.65 = humid.
.ARIDITY_DEFAULT_SUBPATH <- file.path(
  "external", "aridity",
  "Global-AI_ET0__annual_v3_1", "ai_v31_yr.tif"
)

#' Load CGIAR Global Aridity Index v3.1 raster
#'
#' Loads \code{ai_v31_yr.tif} from the local CGIAR aridity index cache into a
#' \pkg{terra} \code{SpatRaster}.  The default path matches where the ZIP
#' downloaded from figshare article 7504448 is extracted.
#'
#' Download with:
#' \preformatted{
#' dir.create("data/external/aridity", recursive = TRUE, showWarnings = FALSE)
#' download.file(
#'   "https://ndownloader.figshare.com/files/56300327",
#'   "data/external/aridity/Global-AI_ET0__annual_v3_1.zip",
#'   mode = "wb"
#' )
#' unzip("data/external/aridity/Global-AI_ET0__annual_v3_1.zip",
#'       exdir = "data/external/aridity/")
#' }
#'
#' @section Value scaling:
#' Raw pixel values are scaled integers.  Divide by \code{10000} to obtain the
#' true aridity index (MAP / PET).  Typical breakpoints:
#' \itemize{
#'   \item \code{< 0.03} — hyper-arid
#'   \item \code{0.03–0.20} — arid
#'   \item \code{0.20–0.50} — semi-arid
#'   \item \code{0.50–0.65} — dry sub-humid
#'   \item \code{> 0.65} — humid
#' }
#'
#' @param path Character. Full path to \code{ai_v31_yr.tif}.  Defaults to
#'   \code{<FLUXNET_DATA_ROOT>/external/aridity/Global-AI_ET0__annual_v3_1/ai_v31_yr.tif}.
#' @param scale Logical. If \code{TRUE} (default), divide raw values by 10000
#'   to return the true aridity index.
#'
#' @return A single-layer \pkg{terra} \code{SpatRaster} at ~1 km (~0.00833°)
#'   resolution, WGS84, extent \code{[-180, 180, -60, 90]}.
#'
#' @examples
#' \dontrun{
#' ai <- load_aridity_index()
#' terra::global(ai, fun = "range", na.rm = TRUE)  # roughly 0–6.5
#' }
#'
#' @export
load_aridity_index <- function(path = NULL, scale = TRUE) {
  if (is.null(path)) {
    path <- file.path(
      Sys.getenv("FLUXNET_DATA_ROOT", unset = "data"),
      .ARIDITY_DEFAULT_SUBPATH
    )
  }

  if (!file.exists(path)) {
    stop(
      "Aridity index TIF not found: '", path, "'\n",
      "Download and extract with:\n",
      "  download.file('https://ndownloader.figshare.com/files/56300327',\n",
      "    'data/external/aridity/Global-AI_ET0__annual_v3_1.zip', mode='wb')\n",
      "  unzip('data/external/aridity/Global-AI_ET0__annual_v3_1.zip',\n",
      "    exdir = 'data/external/aridity/')",
      call. = FALSE
    )
  }

  message("load_aridity_index(): reading ", path)
  r <- terra::rast(path)

  if (scale) {
    r <- r / 10000
    terra::units(r) <- "AI (MAP/PET)"
    message("load_aridity_index(): values scaled by 1/10000 (true aridity index)")
  }

  r
}

# ---- Per-site aridity extraction -------------------------------------------

#' UNEP 5-class aridity classification levels, in canonical order
#'
#' Thresholds (World Atlas of Desertification, UNEP 1992): Hyper-Arid < 0.05,
#' Arid 0.05-0.20, Semi-Arid 0.20-0.50, Dry Sub-Humid 0.50-0.65, Humid >= 0.65.
.ARIDITY_CLASS5_LEVELS <- c("Hyper-Arid", "Arid", "Semi-Arid", "Dry Sub-Humid", "Humid")

#' UNEP 5-class + FAO humid-subdivision 7-class aridity levels, in order
.ARIDITY_CLASS7_LEVELS <- c("Hyper-Arid", "Arid", "Semi-Arid", "Dry Sub-Humid",
                            "Humid (low)", "Humid (moderate)", "Hyper-Humid")

#' Classify an aridity index (AI = P/PET) into the 5 UNEP dryness classes
#'
#' @param ai Numeric vector of mean annual aridity index values.
#' @return Factor with levels [[.ARIDITY_CLASS5_LEVELS]]; `NA` where `ai` is `NA`.
#' @export
classify_aridity_index <- function(ai) {
  factor(
    dplyr::case_when(
      is.na(ai) ~ NA_character_,
      ai < 0.05 ~ "Hyper-Arid",
      ai < 0.20 ~ "Arid",
      ai < 0.50 ~ "Semi-Arid",
      ai < 0.65 ~ "Dry Sub-Humid",
      TRUE      ~ "Humid"
    ),
    levels = .ARIDITY_CLASS5_LEVELS
  )
}

#' Classify an aridity index into the 7-class scheme (5 UNEP + humid split)
#'
#' Extends [classify_aridity_index()] with FAO-derived subdivisions of the
#' Humid class at AI = 1.0 and 2.0. Kept alongside the 5-class scheme for
#' schema parity with `data/snapshots/site_aridity.csv` as previously written
#' by `scripts/figure_representativeness_aridity.R`.
#'
#' @param ai Numeric vector of mean annual aridity index values.
#' @return Factor with levels [[.ARIDITY_CLASS7_LEVELS]]; `NA` where `ai` is `NA`.
#' @export
classify_aridity_index_7 <- function(ai) {
  factor(
    dplyr::case_when(
      is.na(ai) ~ NA_character_,
      ai < 0.05 ~ "Hyper-Arid",
      ai < 0.20 ~ "Arid",
      ai < 0.50 ~ "Semi-Arid",
      ai < 0.65 ~ "Dry Sub-Humid",
      ai < 1.00 ~ "Humid (low)",
      ai < 2.00 ~ "Humid (moderate)",
      TRUE      ~ "Hyper-Humid"
    ),
    levels = .ARIDITY_CLASS7_LEVELS
  )
}

#' Extract per-site CGIAR aridity index at exact tower coordinates
#'
#' Point-extracts the CGIAR Global Aridity Index v3.1 (mean annual AI =
#' P/PET) at each site's exact lat/long, with a buffer-then-nearest-land
#' fallback for coastal sites that land on ocean pixels (raw value 0, stored
#' as NoData in this raster). This is the canonical per-site point-extraction
#' step for aridity-derived tables and figures — consolidates logic
#' previously duplicated between `scripts/step2_extract_aridity.R` and
#' `scripts/figure_representativeness_aridity.R`.
#'
#' Designed to be extended with more per-site covariates: run this once,
#' write the result (e.g. to `data/snapshots/site_aridity.csv`), then
#' `dplyr::left_join()` further per-site extractions (e.g. a human influence
#' index for a later task) onto the same `site_id` key rather than
#' re-deriving coordinates or classification logic.
#'
#' @param site_meta Data frame with `site_id`, `location_lat`,
#'   `location_long` (one row per site; extra columns are dropped).
#' @param ai_rast Optional pre-loaded `terra::SpatRaster` of raw (unscaled)
#'   AI values, i.e. `load_aridity_index(scale = FALSE)`. Loaded internally
#'   from the default path when `NULL`.
#' @param buffer_degs Numeric vector of buffer radii in degrees, tried in
#'   order, for sites whose exact pixel is ocean/NoData (default
#'   `c(0.01, 0.05, 0.1, 0.25, 0.5)`, roughly 1-55 km).
#'
#' @return A tibble: `site_id`, `location_lat`, `location_long`, `ai_value`
#'   (mean annual AI = P/PET), `unep_class_5` (factor, see
#'   [classify_aridity_index()]), `unep_class_7` (factor, see
#'   [classify_aridity_index_7()]), `aridity_method` (`"exact"`,
#'   `"buffer_<km>km"`, or `"nearest_land_<km>km"`).
#'
#' @examples
#' \dontrun{
#' meta <- readr::read_csv("data/snapshots/fluxnet_shuttle_snapshot_*.csv")
#' site_aridity <- extract_site_aridity(meta)
#' table(site_aridity$unep_class_5)
#' }
#'
#' @export
extract_site_aridity <- function(site_meta, ai_rast = NULL,
                                  buffer_degs = c(0.01, 0.05, 0.1, 0.25, 0.5)) {
  required <- c("site_id", "location_lat", "location_long")
  missing  <- setdiff(required, names(site_meta))
  if (length(missing) > 0L) {
    stop("site_meta is missing required columns: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  if (is.null(ai_rast)) ai_rast <- load_aridity_index(scale = FALSE)

  site_coords <- site_meta |>
    dplyr::select("site_id", "location_lat", "location_long") |>
    dplyr::filter(!is.na(.data$location_lat), !is.na(.data$location_long)) |>
    dplyr::distinct(.data$site_id, .keep_all = TRUE)

  pts <- terra::vect(
    data.frame(x = site_coords$location_long, y = site_coords$location_lat),
    geom = c("x", "y"), crs = "EPSG:4326"
  )

  raw_val <- terra::extract(ai_rast, pts, ID = FALSE)[[1L]]
  # Ocean pixels are stored as 0, not NoData, in this raster.
  raw_val[!is.na(raw_val) & raw_val == 0L] <- NA_integer_
  method <- rep("exact", length(raw_val))

  na_idx <- which(is.na(raw_val))
  if (length(na_idx) > 0L) {
    int_mode <- function(x) {
      x <- x[!is.na(x) & x != 0]
      if (length(x) == 0L) return(NA_integer_)
      as.integer(names(sort(table(x), decreasing = TRUE))[[1L]])
    }

    still_na  <- na_idx
    recovered <- logical(length(still_na))
    for (j in seq_along(still_na)) {
      pt_j <- pts[still_na[j], ]
      for (buf in buffer_degs) {
        buf_vals <- terra::extract(ai_rast, pt_j, buffer = buf, ID = TRUE)
        mode_val <- int_mode(buf_vals[[2L]])
        if (!is.na(mode_val)) {
          raw_val[still_na[j]] <- mode_val
          method[still_na[j]]  <- paste0("buffer_", round(buf * 111, 0), "km")
          recovered[j] <- TRUE
          break
        }
      }
    }
    still_na <- still_na[!recovered]

    if (length(still_na) > 0L) {
      for (j in seq_along(still_na)) {
        idx <- still_na[j]
        lng <- site_coords$location_long[idx]
        lat <- site_coords$location_lat[idx]
        local_rast <- terra::crop(ai_rast, terra::ext(lng - 3, lng + 3,
                                                       lat - 3, lat + 3))
        local_rast[local_rast == 0] <- NA
        land_pts <- terra::as.points(local_rast, na.rm = TRUE)
        if (terra::nrow(land_pts) == 0L) next
        dists        <- terra::distance(pts[idx, ], land_pts)
        nearest_idx  <- which.min(dists)
        raw_val[idx] <- as.integer(terra::values(land_pts)[[1L]][nearest_idx])
        method[idx]  <- paste0("nearest_land_", round(min(dists) / 1000, 1), "km")
      }
    }
  }

  still_na_final <- which(is.na(raw_val))
  if (length(still_na_final) > 0L) {
    warning(length(still_na_final), " site(s) remain NA after fallback: ",
            paste(site_coords$site_id[still_na_final], collapse = ", "),
            call. = FALSE)
  }

  ai_value <- raw_val * 0.0001

  dplyr::tibble(
    site_id        = site_coords$site_id,
    location_lat   = site_coords$location_lat,
    location_long  = site_coords$location_long,
    ai_value       = ai_value,
    unep_class_5   = classify_aridity_index(ai_value),
    unep_class_7   = classify_aridity_index_7(ai_value),
    aridity_method = method
  )
}
