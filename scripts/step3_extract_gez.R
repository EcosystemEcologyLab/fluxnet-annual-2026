## step3_extract_gez.R
## Extract FAO Global Ecological Zones (GEZ 2010) at FLUXNET site locations
## via spatial join.  Output: data/snapshots/site_gez_lookup.csv
##
## FAO GEZ 2010 shapefile — direct download URL:
##   https://storage.googleapis.com/fao-maps-catalog-data/uuid/
##   2fb209d0-fd34-4e5e-a3d8-a13c241eb61b/resources/gez2010.zip
##   (61.6 MB; extract to data/external/gez/gez_2010_wgs84.shp)
##
## Catalog landing page:
##   https://data.apps.fao.org/catalog/dataset/2fb209d0-fd34-4e5e-a3d8-a13c241eb61b
##
## Method: sf_use_s2(FALSE) + st_within; st_nearest_feature fallback for the
## small number of site points that fall outside all GEZ polygons (ocean/coast
## edge effects).  The gez_method column records which join was used per site.

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
source("R/utils.R")
check_pipeline_config()

library(sf)
library(dplyr)
library(readr)

# ---- Load latest snapshot for site coordinates ------------------------------
snap_file <- sort(
  list.files(
    file.path(FLUXNET_DATA_ROOT, "snapshots"),
    pattern    = "fluxnet_shuttle_snapshot.*\\.csv$",
    full.names = TRUE
  ),
  decreasing = TRUE
)[[1]]
message("Snapshot: ", basename(snap_file))
snapshot <- readr::read_csv(snap_file, show_col_types = FALSE)

site_coords <- snapshot |>
  dplyr::distinct(site_id, location_lat, location_long) |>
  dplyr::filter(!is.na(location_lat), !is.na(location_long))
N <- nrow(site_coords)
message("Sites with coordinates: ", N)

# ---- Check GEZ shapefile exists --------------------------------------------
gez_path <- file.path("data", "external", "gez", "gez_2010_wgs84.shp")
if (!file.exists(gez_path)) {
  stop(
    "GEZ shapefile not found: ", gez_path, "\n",
    "Download from:\n",
    "  https://storage.googleapis.com/fao-maps-catalog-data/uuid/",
    "2fb209d0-fd34-4e5e-a3d8-a13c241eb61b/resources/gez2010.zip\n",
    "(61.6 MB; extract to data/external/gez/)",
    call. = FALSE
  )
}

# ---- Load GEZ shapefile ----------------------------------------------------
sf::sf_use_s2(FALSE)
gez <- sf::st_read(gez_path, quiet = TRUE)
message("GEZ polygons: ", nrow(gez), "  CRS: EPSG:", sf::st_crs(gez)$epsg)

nmf <- grep("name", names(gez), ignore.case = TRUE, value = TRUE)[1]
cdf <- grep("code", names(gez), ignore.case = TRUE, value = TRUE)[1]
message("  name field: ", nmf, "  |  code field: ", cdf)

# ---- Spatial join: st_within + st_nearest_feature fallback -----------------
sites_sf <- sf::st_as_sf(
  site_coords,
  coords = c("location_long", "location_lat"),
  crs    = 4326
)
if (!is.na(sf::st_crs(gez)) && sf::st_crs(gez) != sf::st_crs(sites_sf)) {
  sites_sf <- sf::st_transform(sites_sf, sf::st_crs(gez))
}

within <- sf::st_within(sites_sf, gez)
wid    <- vapply(
  within,
  function(x) if (length(x) >= 1L) x[[1L]] else NA_integer_,
  integer(1L)
)
method <- ifelse(is.na(wid), "nearest_feature", "within")

na_idx <- which(is.na(wid))
if (length(na_idx) > 0L) {
  wid[na_idx] <- sf::st_nearest_feature(sites_sf[na_idx, ], gez)
}

site_gez_lookup <- dplyr::tibble(
  site_id    = site_coords$site_id,
  gez_name   = as.character(gez[[nmf]])[wid],
  gez_code   = gez[[cdf]][wid],
  gez_method = method
)

# ---- Summary ---------------------------------------------------------------
fb_sites <- site_gez_lookup$site_id[site_gez_lookup$gez_method == "nearest_feature"]
gez_na   <- site_gez_lookup$site_id[is.na(site_gez_lookup$gez_name)]

cat("\n--- GEZ assignment summary ---\n")
cat(sprintf("  Total sites assigned:         %d\n", N))
cat(sprintf("  Assigned via st_within:       %d\n", sum(method == "within")))
cat(sprintf("  Assigned via nearest_feature: %d\n", length(fb_sites)))
if (length(fb_sites) > 0L) {
  cat("    Fallback sites:", paste(sort(fb_sites), collapse = ", "), "\n")
}
cat(sprintf("  NA gez_name:                  %d\n", length(gez_na)))
if (length(gez_na) > 0L) {
  cat("    NA sites:", paste(sort(gez_na), collapse = ", "), "\n")
}

cat("\n--- GEZ distribution ---\n")
print(sort(table(site_gez_lookup$gez_name), decreasing = TRUE))

# ---- Save ------------------------------------------------------------------
out_path <- file.path(FLUXNET_DATA_ROOT, "snapshots", "site_gez_lookup.csv")
readr::write_csv(site_gez_lookup, out_path)
message("\nSaved: ", out_path)

write_output_metadata(
  out_path,
  input_sources = c(
    snap_file,
    paste0(
      "FAO Global Ecological Zones 2010 — data/external/gez/gez_2010_wgs84.shp",
      " (FAO catalog 2fb209d0-fd34-4e5e-a3d8-a13c241eb61b, gez2010.zip)"
    )
  ),
  notes = paste0(
    "Site->GEZ spatial join: sf_use_s2(FALSE), st_within primary, ",
    "st_nearest_feature fallback for ", length(fb_sites), " site(s)",
    if (length(fb_sites) > 0L) {
      paste0(": ", paste(sort(fb_sites), collapse = ", "))
    } else {
      ""
    },
    ". gez_method column flags join method per site."
  )
)
