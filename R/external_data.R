# R/external_data.R
# Loaders for external reference datasets used in the FLUXNET Annual Paper 2026.
#
# Functions:
#   load_worldclim() — load WorldClim v2.1 bioclim SpatRaster from the local
#                      cache downloaded by geodata::worldclim_global()

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
