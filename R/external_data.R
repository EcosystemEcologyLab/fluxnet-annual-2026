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
