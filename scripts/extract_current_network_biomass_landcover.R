## extract_current_network_biomass_landcover.R
##
## Fresh per-site raw-value extraction (biomass, land cover) for the CURRENT
## FLUXNET Shuttle network, at the pinned 2026-09-01 snapshot (781 sites).
##
## Why this script exists: scripts/figure_representativeness_biomass.R and
## scripts/figure_representativeness_landcover.R both read their *raw*
## per-site values (biomass_value_mg_ha; cci_native_class/cci_high_level_class)
## from the existing bare site_biomass_cci_v7.csv / site_landcover_cci.csv
## rather than re-extracting from the rasters themselves ("Step 2:
## Re-classify sites from existing site CSV" / "Step 6: Per-site
## classification (no re-extraction)") — those scripts only recompute BIN
## ASSIGNMENTS from an already-populated raw value. The original one-off
## script that first populated those raw columns for the 767-site network is
## not present in this repo (or was folded into an earlier revision of these
## two scripts before the "no re-extraction" refactor). This script fills
## that gap for the 781-site network, using the identical extraction method
## (terra::extract with nearest-land recovery within 3°) already established
## and reused in scripts/extract_historical_sites_representativeness.R for
## the historical (Marconi/La Thuile/FLUXNET2015) networks.
##
## Aridity is NOT included here: figure_representativeness_aridity.R already
## does its own fresh terra::extract() from the pinned snapshot each run, no
## gap to fill. TRENDY axes are NOT included here either:
## figure_representativeness_trendy_compute.R does its own fresh extraction,
## reading site coordinates from site_biomass_cci_v7.csv (fixed by this
## script) — so fixing biomass's coordinate list is sufficient for TRENDY too.
## KG present-day for the current network is ERA5-based (step5_compute_
## koppen_era5.R via DuckDB), not raster-based — also out of scope here.
##
## Output: overwrites (bare, "current" convention — see SESSION_LOG.md
## 2026-09-01):
##   data/snapshots/site_biomass_cci_v7.csv   (site_id, location_lat,
##     location_long, biomass_value_mg_ha, biomass_method)
##   data/snapshots/site_landcover_cci.csv    (site_id, location_lat,
##     location_long, cci_native_class, cci_native_class_name,
##     cci_high_level_class, cci_high_level_class_name, landcover_method)
## The pre-existing 767-site versions were archived to *_current_767.csv
## before this script first ran (see SESSION_LOG.md 2026-09-01).
##
## Downstream: scripts/figure_representativeness_biomass.R and
## scripts/figure_representativeness_landcover.R re-derive bins/aggregation
## levels from these raw columns and must be re-run after this script.

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(terra)
  library(dplyr)
  library(readr)
})

SNAP_DIR <- "data/snapshots"
EXT_DIR  <- "data/external"

# Pinned explicitly -- see SESSION_LOG.md 2026-09-01.
snap_path <- file.path(SNAP_DIR, "fluxnet_shuttle_snapshot_20260901T094522.csv")
if (!file.exists(snap_path)) stop("Pinned snapshot not found: ", snap_path)

snapshot <- readr::read_csv(snap_path, show_col_types = FALSE)
sites <- snapshot |>
  dplyr::distinct(site_id, location_lat, location_long) |>
  dplyr::filter(!is.na(location_lat), !is.na(location_long))
message("Sites (pinned 20260901 snapshot): ", nrow(sites))

# ---- Helper: extract + nearest-land recovery (within 3 deg) -----------------
# Identical method to scripts/extract_historical_sites_representativeness.R
extract_with_fallback <- function(r_map, sites) {
  pts <- terra::vect(
    data.frame(x = sites$location_long, y = sites$location_lat),
    geom = c("x", "y"), crs = "EPSG:4326"
  )
  raw     <- terra::extract(r_map, pts, ID = FALSE)
  vals    <- raw[[1]]
  methods <- rep("exact", nrow(sites))

  na_idx <- which(is.na(vals) | !is.finite(vals))
  if (length(na_idx) > 0L) {
    message("    Nearest-land recovery for ", length(na_idx), " NA site(s) ...")
    for (i in na_idx) {
      sx <- sites$location_long[i]
      sy <- sites$location_lat[i]
      window  <- terra::ext(sx - 3, sx + 3, sy - 3, sy + 3)
      r_crop  <- terra::crop(r_map, window)
      lv      <- terra::values(r_crop)[, 1L]
      ok      <- !is.na(lv) & is.finite(lv)
      if (!any(ok)) {
        methods[i] <- "no_land_within_3deg"
        message("      ", sites$site_id[i], ": no land within 3 deg -- remains NA")
        next
      }
      land_cells <- which(ok)
      land_xy    <- terra::xyFromCell(r_crop, land_cells)
      dists      <- sqrt((land_xy[, 1L] - sx)^2 + (land_xy[, 2L] - sy)^2)
      best       <- which.min(dists)
      vals[i]    <- lv[land_cells[[best]]]
      methods[i] <- sprintf("nearest_land_%.3fdeg", dists[[best]])
      message("      ", sites$site_id[i], ": recovered at ",
              round(dists[[best]], 3), " deg (val=", round(vals[i], 4), ")")
    }
  }

  data.frame(
    site_id       = sites$site_id,
    location_lat  = sites$location_lat,
    location_long = sites$location_long,
    value         = vals,
    method        = methods,
    stringsAsFactors = FALSE
  )
}

# ---- Biomass: ESA CCI Biomass v7.0, band 18 (2024) — same band as
#      figure_representativeness_biomass.R and extract_historical_*.R -------
bio_path <- file.path(EXT_DIR, "cci_biomass",
                       "ESACCI-BIOMASS-L4-AGB-MERGED-1000m-fv7.0.tif")
if (!file.exists(bio_path)) stop("Raster not found: ", bio_path)
message("\n=== Biomass (ESA CCI v7.0, band 18) ===")
bio_rast <- terra::rast(bio_path)[[18L]]
ex_bio <- extract_with_fallback(bio_rast, sites)

site_biomass_out <- data.frame(
  site_id             = ex_bio$site_id,
  location_lat        = ex_bio$location_lat,
  location_long       = ex_bio$location_long,
  biomass_value_mg_ha = ex_bio$value,
  biomass_method      = ex_bio$method,
  stringsAsFactors    = FALSE
)
out_bio <- file.path(SNAP_DIR, "site_biomass_cci_v7.csv")
readr::write_csv(site_biomass_out, out_bio)
message("Saved: ", out_bio, " (", nrow(site_biomass_out), " rows)")
message("  NA biomass: ", sum(is.na(site_biomass_out$biomass_value_mg_ha)))

# ---- Land cover: ESA CCI Land Cover v2.1.1, 2022 (native LCCS + high-level) -
lulc_path <- file.path(EXT_DIR, "cci_landcover", "v2.1.1",
                        "cci_lc_2022_kg_aligned_native.tif")
if (!file.exists(lulc_path)) stop("Raster not found: ", lulc_path)
lulc_lookup_path <- file.path(SNAP_DIR, "cci_landcover_aggregation_lookup.csv")
if (!file.exists(lulc_lookup_path)) stop("Lookup not found: ", lulc_lookup_path)

message("\n=== Land cover (ESA CCI v2.1.1, 2022) ===")
lulc_rast   <- terra::rast(lulc_path)
lulc_lookup <- readr::read_csv(lulc_lookup_path, show_col_types = FALSE)

ex_lulc <- extract_with_fallback(lulc_rast, sites)
lulc_raw <- data.frame(
  site_id          = ex_lulc$site_id,
  location_lat     = ex_lulc$location_lat,
  location_long    = ex_lulc$location_long,
  cci_native_class = as.integer(ex_lulc$value),
  landcover_method = ex_lulc$method,
  stringsAsFactors = FALSE
)
lulc_out <- lulc_raw |>
  dplyr::left_join(
    lulc_lookup |> dplyr::select(lulc_native, lulc_native_name,
                                  lulc_highlevel, lulc_highlevel_name),
    by = c("cci_native_class" = "lulc_native")
  ) |>
  dplyr::mutate(
    cci_native_class_name     = lulc_native_name,
    cci_high_level_class      = lulc_highlevel,
    cci_high_level_class_name = lulc_highlevel_name
  ) |>
  dplyr::select(site_id, location_lat, location_long,
                cci_native_class, cci_native_class_name,
                cci_high_level_class, cci_high_level_class_name,
                landcover_method)

out_lulc <- file.path(SNAP_DIR, "site_landcover_cci.csv")
readr::write_csv(lulc_out, out_lulc)
message("Saved: ", out_lulc, " (", nrow(lulc_out), " rows)")
message("  NA native class: ", sum(is.na(lulc_out$cci_native_class)))

message("\nDone. Re-run figure_representativeness_biomass.R and ",
        "figure_representativeness_landcover.R next to rebuild bins/figures ",
        "from these raw values.")
