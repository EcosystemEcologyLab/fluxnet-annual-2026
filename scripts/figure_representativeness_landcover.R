## figure_representativeness_landcover.R
## Land use / land cover representativeness — three aggregation levels.
## Data: ESA CCI Land Cover v2.1.1, year 2022 (Copernicus CDS download).
##
## Outputs:
##   data/snapshots/cci_landcover_aggregation_lookup.csv
##   data/snapshots/site_landcover_cci.csv        (extended with 3-level cols)
##   data/snapshots/landcover_cci_highlevel_global_distribution.csv
##   data/snapshots/landcover_cci_level2_global_distribution.csv
##   data/snapshots/landcover_cci_native_global_distribution.csv
##   representativeness_metrics.csv               (3 rows: highlevel/level2/native)
##   review/figures/representativeness/fig_representativeness_landcover_highlevel.png
##   review/figures/representativeness/fig_representativeness_landcover_level2.png
##   review/figures/representativeness/fig_representativeness_landcover_native.png
##   review/figures/representativeness/methods_landcover.md
##
## Replaces: data/snapshots/landcover_cci_global_distribution.csv (renamed)
##           review/figures/representativeness/fig_representativeness_landcover.png
##
## ---- AGGREGATION SCHEME OVERVIEW ----------------------------------------
##
## Native (37): LCCS integer codes directly from ESA CCI map
##
## Level 2 (22): ESA CCI intermediate LCCS hierarchy
##   1  Rainfed cropland          10 11 12
##   2  Irrigated cropland        20
##   3  Mosaic cropland/nat-veg   30
##   4  Mosaic nat-veg/cropland   40
##   5  BL evergreen forest       50
##   6  BL deciduous forest       60 61 62
##   7  NL evergreen forest       70 71 72
##   8  NL deciduous forest       80 81 82
##   9  Mixed forest              90
##  10  Mosaic tree-shrub/herb    100
##  11  Mosaic herb/tree-shrub    110
##  12  Shrubland                 120 121 122
##  13  Grassland                 130
##  14  Lichens and mosses        140
##  15  Sparse vegetation         150 151 152 153
##  16  Flooded forest (fresh/brackish) 160
##  17  Flooded forest (saline)   170
##  18  Flooded shrub/herb        180
##  19  Urban areas               190
##  20  Bare areas                200 201 202
##  21  Water bodies              210
##  22  Permanent snow and ice    220
##
## High-level (10): ESA CCI PUG Table 2 standard cross-walk
##   1 Cropland, 2 Forest, 3 Shrubland, 4 Grassland, 5 Wetland,
##   6 Settlement, 7 Bare, 8 Snow/Ice, 9 Water, 10 Other
## -------------------------------------------------------------------------

if (file.exists(".env")) dotenv::load_dot_env()
source("R/pipeline_config.R")
source("R/utils.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(terra)
  library(dplyr)
  library(readr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
})

# ---- Paths ------------------------------------------------------------------
lc_dir      <- file.path("data", "external", "cci_landcover", "v2.1.1")
nc_files    <- list.files(lc_dir, pattern = "\\.nc$", full.names = TRUE)
if (length(nc_files) == 0)
  stop("No NetCDF in ", lc_dir, " — run dl_cci_lc_v211.py first.",
       call. = FALSE)
lc_path     <- nc_files[which.max(file.size(nc_files))]
message("Using NetCDF: ", basename(lc_path),
        " (", round(file.size(lc_path) / 1e6), " MB)")

CACHED_KG_TIF <- file.path(lc_dir, "cci_lc_2022_kg_aligned_native.tif")

legend_path <- file.path("data", "external", "cci_landcover",
                         "ESACCI-LC-Legend.csv")
kg_path     <- file.path("data", "external", "koppen_beck2023", "1991_2020",
                         "koppen_geiger_0p00833333.tif")
snap_path   <- file.path(FLUXNET_DATA_ROOT, "snapshots",
                         "fluxnet_shuttle_snapshot_20260624T095651.csv")
SNAP_DIR    <- file.path(FLUXNET_DATA_ROOT, "snapshots")
site_csv    <- file.path(SNAP_DIR, "site_landcover_cci.csv")
out_dir     <- file.path("review", "figures", "representativeness")
metrics_out <- file.path(SNAP_DIR, "representativeness_metrics.csv")

for (p in c(legend_path, kg_path, snap_path, site_csv)) {
  if (!file.exists(p)) stop("Required file not found: ", p, call. = FALSE)
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

LC_VERSION  <- "2.1.1"
LC_YEAR     <- 2022L
LC_FILENAME <- basename(lc_path)

# =============================================================================
# ---- 1. Aggregation mappings ------------------------------------------------
# =============================================================================

# ---- Native → Level 2 (22-class ESA CCI LCCS intermediate hierarchy) -------
NATIVE_TO_L2 <- c(
  "10" = 1L, "11" = 1L, "12" = 1L,              # Rainfed cropland
  "20" = 2L,                                     # Irrigated cropland
  "30" = 3L,                                     # Mosaic cropland/nat-veg
  "40" = 4L,                                     # Mosaic nat-veg/cropland
  "50" = 5L,                                     # BL evergreen forest
  "60" = 6L, "61" = 6L, "62" = 6L,             # BL deciduous forest
  "70" = 7L, "71" = 7L, "72" = 7L,             # NL evergreen forest
  "80" = 8L, "81" = 8L, "82" = 8L,             # NL deciduous forest
  "90" = 9L,                                     # Mixed forest
  "100" = 10L,                                   # Mosaic tree-shrub/herb
  "110" = 11L,                                   # Mosaic herb/tree-shrub
  "120" = 12L, "121" = 12L, "122" = 12L,       # Shrubland
  "130" = 13L,                                   # Grassland
  "140" = 14L,                                   # Lichens and mosses
  "150" = 15L, "151" = 15L, "152" = 15L, "153" = 15L, # Sparse vegetation
  "160" = 16L,                                   # Flooded forest freshwater
  "170" = 17L,                                   # Flooded forest saline
  "180" = 18L,                                   # Flooded shrub/herb
  "190" = 19L,                                   # Urban areas
  "200" = 20L, "201" = 20L, "202" = 20L,       # Bare areas
  "210" = 21L,                                   # Water bodies
  "220" = 22L                                    # Permanent snow and ice
)

L2_CODES <- 1:22
L2_NAMES <- c(
  "Rainfed cropland",             #  1  → HL 1 Cropland
  "Irrigated cropland",           #  2  → HL 1
  "Mosaic cropland/nat-veg",      #  3  → HL 1
  "Mosaic nat-veg/cropland",      #  4  → HL 10 Other
  "BL evergreen forest",          #  5  → HL 2 Forest
  "BL deciduous forest",          #  6  → HL 2
  "NL evergreen forest",          #  7  → HL 2
  "NL deciduous forest",          #  8  → HL 2
  "Mixed forest",                 #  9  → HL 2
  "Mosaic tree-shrub/herb",       # 10  → HL 2
  "Mosaic herb/tree-shrub",       # 11  → HL 4 Grassland
  "Shrubland",                    # 12  → HL 3
  "Grassland",                    # 13  → HL 4
  "Lichens and mosses",           # 14  → HL 10 Other
  "Sparse vegetation",            # 15  → HL 3
  "Flooded forest (freshwater)",  # 16  → HL 5 Wetland
  "Flooded forest (saline)",      # 17  → HL 5
  "Flooded shrub/herb",           # 18  → HL 5
  "Urban areas",                  # 19  → HL 6 Settlement
  "Bare areas",                   # 20  → HL 7 Bare
  "Water bodies",                 # 21  → HL 9 Water
  "Permanent snow and ice"        # 22  → HL 8 Snow/Ice
)

# ---- Native → High-level (10-class ESA CCI PUG Table 2 cross-walk) ---------
NATIVE_TO_HL <- c(
  "10"  = 1L, "11"  = 1L, "12"  = 1L, "20"  = 1L, "30"  = 1L,
  "50"  = 2L, "60"  = 2L, "61"  = 2L, "62"  = 2L,
  "70"  = 2L, "71"  = 2L, "72"  = 2L,
  "80"  = 2L, "81"  = 2L, "82"  = 2L, "90"  = 2L, "100" = 2L,
  "120" = 3L, "121" = 3L, "122" = 3L,
  "150" = 3L, "151" = 3L, "152" = 3L, "153" = 3L,
  "110" = 4L, "130" = 4L,
  "160" = 5L, "170" = 5L, "180" = 5L,
  "190" = 6L,
  "200" = 7L, "201" = 7L, "202" = 7L,
  "220" = 8L,
  "210" = 9L,
  "40"  = 10L, "140" = 10L
)

HL_CODES <- 1:10
HL_NAMES <- c(
  "Cropland", "Forest", "Shrubland", "Grassland", "Wetland",
  "Settlement", "Bare", "Snow/Ice", "Water", "Other"
)

# =============================================================================
# ---- 2. Colors and legend ---------------------------------------------------
# =============================================================================

legend_raw <- readr::read_delim(legend_path, delim = ";",
                                show_col_types = FALSE) |>
  dplyr::rename(code = NB_LAB, label = LCCOwnLabel) |>
  dplyr::filter(code != 0) |>
  dplyr::mutate(
    code_str = as.character(code),
    l2_class = NATIVE_TO_L2[code_str],
    hl_class = NATIVE_TO_HL[code_str],
    hex_native = dplyr::if_else(
      code == 220L,
      "#e0f3f8",            # Snow/Ice: pale blue instead of white
      sprintf("#%02x%02x%02x", R, G, B)
    )
  ) |>
  dplyr::filter(!is.na(hl_class))

# Native colors: directly from legend
native_colors_df <- legend_raw |>
  dplyr::select(code, label, hex_native) |>
  dplyr::rename(hex = hex_native)

# Level 2 colors: unweighted mean RGB of constituent native classes
l2_colors_df <- legend_raw |>
  dplyr::group_by(l2_class) |>
  dplyr::summarise(
    R_mean = round(mean(R)), G_mean = round(mean(G)), B_mean = round(mean(B)),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    hex = dplyr::case_when(
      l2_class == 22L ~ "#e0f3f8",   # Snow/Ice override
      TRUE ~ sprintf("#%02x%02x%02x", R_mean, G_mean, B_mean)
    )
  )

# High-level colors: same unweighted mean RGB approach
hl_colors_df <- legend_raw |>
  dplyr::group_by(hl_class) |>
  dplyr::summarise(
    R_mean = round(mean(R)), G_mean = round(mean(G)), B_mean = round(mean(B)),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    hex = dplyr::case_when(
      hl_class == 8L ~ "#e0f3f8",    # Snow/Ice override
      TRUE ~ sprintf("#%02x%02x%02x", R_mean, G_mean, B_mean)
    )
  )

HL_COLORS <- setNames(
  hl_colors_df$hex[match(HL_CODES, hl_colors_df$hl_class)],
  HL_NAMES
)
HL_COLORS[is.na(HL_COLORS)] <- "#cccccc"

L2_COLORS <- setNames(
  l2_colors_df$hex[match(L2_CODES, l2_colors_df$l2_class)],
  L2_NAMES
)
L2_COLORS[is.na(L2_COLORS)] <- "#cccccc"

native_codes_sorted <- as.integer(legend_raw$code)[order(legend_raw$code)]
native_names_sorted <- legend_raw$label[order(legend_raw$code)]
NATIVE_COLORS <- setNames(
  native_colors_df$hex[match(native_codes_sorted, native_colors_df$code)],
  as.character(native_codes_sorted)
)

message("High-level class colors:")
for (i in seq_along(HL_NAMES))
  message("  ", i, ": ", HL_NAMES[i], " -> ", HL_COLORS[i])

# =============================================================================
# ---- 3. Aggregation lookup table --------------------------------------------
# =============================================================================

lookup_df <- legend_raw |>
  dplyr::arrange(code) |>
  dplyr::mutate(
    lulc_native      = code,
    lulc_native_name = label,
    lulc_level2      = l2_class,
    lulc_level2_name = L2_NAMES[l2_class],
    lulc_highlevel   = hl_class,
    lulc_highlevel_name = HL_NAMES[hl_class]
  ) |>
  dplyr::select(lulc_native, lulc_native_name,
                lulc_level2, lulc_level2_name,
                lulc_highlevel, lulc_highlevel_name,
                R, G, B)

lookup_out <- file.path(SNAP_DIR, "cci_landcover_aggregation_lookup.csv")
readr::write_csv(lookup_df, lookup_out)
message("Saved lookup table: ", lookup_out)
cat("\n=== AGGREGATION LOOKUP TABLE ===\n")
print(as.data.frame(lookup_df[,
  c("lulc_native","lulc_native_name","lulc_level2","lulc_level2_name",
    "lulc_highlevel","lulc_highlevel_name")]),
  row.names = FALSE)

# =============================================================================
# ---- 4. Load rasters + resample to KG grid (with cache) --------------------
# =============================================================================

message("\nLoading KG land mask ...")
kg_rast <- terra::rast(kg_path)

if (file.exists(CACHED_KG_TIF)) {
  message("Loading cached KG-aligned native raster: ", basename(CACHED_KG_TIF))
  lc_aligned <- terra::rast(CACHED_KG_TIF)
} else {
  message("\nLoading CCI Land Cover NetCDF ...")
  lc_rast_raw <- terra::rast(lc_path)
  lyr_idx <- grep("lccs_class", names(lc_rast_raw), ignore.case = TRUE)
  if (length(lyr_idx) == 0) lyr_idx <- 1L
  lc_rast <- lc_rast_raw[[lyr_idx[1]]]
  message("  Layer: ", names(lc_rast), "  Dims: ", nrow(lc_rast),
          " x ", ncol(lc_rast))
  rm(lc_rast_raw); gc(verbose = FALSE)

  message("Resampling 300m -> KG grid (nearest-neighbour, may take a few min) ...")
  t0 <- proc.time()
  lc_aligned <- terra::resample(lc_rast, kg_rast, method = "near",
                                threads = TRUE)
  message("  Elapsed: ", round((proc.time() - t0)[["elapsed"]], 1), " s")

  message("Saving cached native raster: ", basename(CACHED_KG_TIF))
  terra::writeRaster(lc_aligned, CACHED_KG_TIF,
                     datatype = "INT2U", gdal = "COMPRESS=DEFLATE",
                     overwrite = TRUE)
  rm(lc_rast); gc(verbose = FALSE)
}

# Apply KG land mask
lc_land <- terra::mask(lc_aligned, kg_rast)

message("Computing cell areas ...")
cell_areas <- terra::cellSize(kg_rast, mask = TRUE, unit = "km")

# =============================================================================
# ---- 5. Global distributions at all three levels ---------------------------
# =============================================================================

compute_repr_metrics <- function(p, q) {
  p[is.na(p)] <- 0; q[is.na(q)] <- 0
  list(
    weighted_jaccard   = sum(pmin(p, q)) / sum(pmax(p, q)),
    hellinger_distance = (1 / sqrt(2)) * sqrt(sum((sqrt(p) - sqrt(q))^2))
  )
}

# ---- Helper: reclassify native raster to a target coding -------------------
make_rcl <- function(native_to_target) {
  nms  <- names(native_to_target)
  codes <- as.integer(nms)
  do.call(rbind, lapply(seq_along(codes), function(i)
    c(codes[i], codes[i], native_to_target[[i]])
  ))
}

# ---- 5a: Native (37 classes) ------------------------------------------------
message("\n=== Step 5a: Global distribution — native 37 classes ===")

native_codes_all <- sort(as.integer(names(NATIVE_TO_HL)))
# Zonal sum per native code
zone_native <- terra::zonal(cell_areas, lc_land, fun = "sum", na.rm = TRUE)
names(zone_native) <- c("lulc_native", "global_land_area_km2")
zone_native <- zone_native[!is.na(zone_native$lulc_native) &
                             zone_native$lulc_native %in% native_codes_all, ]

total_land_km2 <- sum(zone_native$global_land_area_km2)
message("  Total land: ", format(round(total_land_km2), big.mark = ","), " km2")

dist_native <- data.frame(lulc_native = native_codes_all) |>
  dplyr::left_join(
    lookup_df |> dplyr::select(lulc_native, lulc_native_name,
                                lulc_level2, lulc_level2_name,
                                lulc_highlevel, lulc_highlevel_name),
    by = "lulc_native"
  ) |>
  dplyr::left_join(zone_native, by = "lulc_native") |>
  dplyr::mutate(
    global_land_area_km2 = dplyr::coalesce(global_land_area_km2, 0),
    global_land_fraction = global_land_area_km2 / total_land_km2
  )

glob_native_out <- file.path(SNAP_DIR,
                             "landcover_cci_native_global_distribution.csv")
readr::write_csv(dist_native, glob_native_out)
message("Saved: ", glob_native_out)
write_output_metadata(
  glob_native_out,
  input_sources = c(lc_path, kg_path, legend_path),
  notes = paste0(
    "Global land area per ESA CCI LC v", LC_VERSION, " native LCCS class ",
    "(37 classes, year ", LC_YEAR, "). Resampled to KG 0.00833 deg (nearest-",
    "neighbour). KG land mask applied. Class 0 excluded. ",
    "Total land: ", format(round(total_land_km2), big.mark=","), " km2."
  )
)

# ---- 5b: Level 2 (22 classes) -----------------------------------------------
message("\n=== Step 5b: Global distribution — Level 2 (22 classes) ===")

rcl_l2 <- make_rcl(NATIVE_TO_L2)
lc_l2  <- terra::classify(lc_land, rcl_l2, others = NA, right = NA)

zone_l2 <- terra::zonal(cell_areas, lc_l2, fun = "sum", na.rm = TRUE)
names(zone_l2) <- c("lulc_level2", "global_land_area_km2")
zone_l2 <- zone_l2[!is.na(zone_l2$lulc_level2) &
                     zone_l2$lulc_level2 %in% L2_CODES, ]

dist_l2 <- data.frame(
  lulc_level2      = L2_CODES,
  lulc_level2_name = L2_NAMES
) |>
  dplyr::left_join(zone_l2, by = "lulc_level2") |>
  dplyr::mutate(
    global_land_area_km2 = dplyr::coalesce(global_land_area_km2, 0),
    global_land_fraction = global_land_area_km2 / total_land_km2
  )

n_l2_present <- sum(dist_l2$global_land_fraction > 0)
message("  Level 2 classes with non-zero area: ", n_l2_present, " of 22")

glob_l2_out <- file.path(SNAP_DIR,
                         "landcover_cci_level2_global_distribution.csv")
readr::write_csv(dist_l2, glob_l2_out)
message("Saved: ", glob_l2_out)
write_output_metadata(
  glob_l2_out,
  input_sources = c(lc_path, kg_path, legend_path),
  notes = paste0(
    "Global land area per ESA CCI LC v", LC_VERSION,
    " Level 2 class (22 classes, year ", LC_YEAR,
    "). Level 2 derived from native LCCS codes using published LCCS hierarchy.",
    " KG 0.00833 deg grid, nearest-neighbour resample. ",
    "Total land: ", format(round(total_land_km2), big.mark=","), " km2."
  )
)

# ---- 5c: High-level (10 classes) --------------------------------------------
message("\n=== Step 5c: Global distribution — high-level (10 classes) ===")

rcl_hl <- make_rcl(NATIVE_TO_HL)
lc_hl  <- terra::classify(lc_land, rcl_hl, others = NA, right = NA)

zone_hl <- terra::zonal(cell_areas, lc_hl, fun = "sum", na.rm = TRUE)
names(zone_hl) <- c("cci_high_level_class", "global_land_area_km2")
zone_hl <- zone_hl[!is.na(zone_hl$cci_high_level_class) &
                    zone_hl$cci_high_level_class %in% HL_CODES, ]

dist_hl <- data.frame(
  cci_high_level_class      = HL_CODES,
  cci_high_level_class_name = HL_NAMES
) |>
  dplyr::left_join(zone_hl, by = "cci_high_level_class") |>
  dplyr::mutate(
    global_land_area_km2 = dplyr::coalesce(global_land_area_km2, 0),
    global_land_fraction = global_land_area_km2 / total_land_km2
  )

water_frac <- dist_hl$global_land_fraction[dist_hl$cci_high_level_class == 9]

glob_hl_out <- file.path(SNAP_DIR,
                         "landcover_cci_highlevel_global_distribution.csv")
readr::write_csv(dist_hl, glob_hl_out)
message("Saved: ", glob_hl_out)
write_output_metadata(
  glob_hl_out,
  input_sources = c(lc_path, kg_path, legend_path),
  notes = paste0(
    "Global land area per CCI LC high-level class (10 classes). ",
    "ESA CCI LC v", LC_VERSION, " (year ", LC_YEAR,
    ") resampled to KG 0.00833 deg grid (nearest-neighbour). ",
    "KG land mask applied. Class 0 excluded. ",
    "Total land: ", format(round(total_land_km2), big.mark=","), " km2. ",
    "Supersedes landcover_cci_global_distribution.csv (renamed)."
  )
)

cat("\n=== GLOBAL DISTRIBUTION SUMMARY ===\n")
print(as.data.frame(dplyr::mutate(dist_hl,
  pct = round(global_land_fraction * 100, 1)
)[, c("cci_high_level_class", "cci_high_level_class_name", "pct")]),
row.names = FALSE)

# =============================================================================
# ---- 6. Per-site classification at all three levels -------------------------
# =============================================================================

message("\n=== Step 6: Per-site classification (no re-extraction) ===")

site_df <- readr::read_csv(site_csv, show_col_types = FALSE)
N_sites <- nrow(site_df)
message("  ", N_sites, " sites from ", basename(site_csv))

# Verify: cci_high_level_class should match NATIVE_TO_HL applied to cci_native_class
expected_hl <- as.integer(NATIVE_TO_HL[as.character(site_df$cci_native_class)])
n_mismatch  <- sum(site_df$cci_high_level_class != expected_hl,
                   na.rm = TRUE)
message("  HL class verification: ", n_mismatch, " mismatches (expect 0)")

site_df <- site_df |>
  dplyr::mutate(
    lulc_native      = cci_native_class,
    lulc_native_name = cci_native_class_name,
    lulc_level2      = as.integer(NATIVE_TO_L2[as.character(cci_native_class)]),
    lulc_level2_name = L2_NAMES[lulc_level2],
    lulc_highlevel   = cci_high_level_class,
    lulc_highlevel_name = cci_high_level_class_name
  )

cat("\n=== SITE DISTRIBUTION — HIGH LEVEL ===\n")
print(as.data.frame(dplyr::count(site_df, lulc_highlevel,
                                  lulc_highlevel_name) |>
  dplyr::arrange(lulc_highlevel) |>
  dplyr::mutate(pct = round(100*n/N_sites, 1))), row.names = FALSE)

cat("\n=== SITE DISTRIBUTION — LEVEL 2 ===\n")
print(as.data.frame(dplyr::count(site_df, lulc_level2,
                                  lulc_level2_name) |>
  dplyr::arrange(lulc_level2) |>
  dplyr::mutate(pct = round(100*n/N_sites, 1))), row.names = FALSE)

cat("\n=== SITE DISTRIBUTION — NATIVE ===\n")
print(as.data.frame(dplyr::count(site_df, lulc_native,
                                  lulc_native_name) |>
  dplyr::arrange(lulc_native) |>
  dplyr::mutate(pct = round(100*n/N_sites, 1))), row.names = FALSE)

readr::write_csv(site_df, site_csv)
message("Saved: ", site_csv)

write_output_metadata(
  site_csv,
  input_sources = c(snap_path, lc_path, legend_path),
  notes = paste0(
    "Per-site land cover at three aggregation levels. ",
    "Native LCCS code (lulc_native = cci_native_class) from original extraction. ",
    "Level 2 (lulc_level2, 22 classes) derived from native using LCCS hierarchy. ",
    "High-level (lulc_highlevel, 10 classes) from ESA CCI PUG Table 2. ",
    "No re-extraction performed for this update. ",
    "HL verification: ", n_mismatch, " mismatch(es) vs. stored cci_high_level_class."
  )
)

# =============================================================================
# ---- 7. Representativeness metrics ------------------------------------------
# =============================================================================

message("\n=== Step 7: Representativeness metrics ===")

valid_sites <- dplyr::filter(site_df, !is.na(lulc_highlevel))
n_valid <- nrow(valid_sites)

# High-level
p_hl <- dist_hl$global_land_fraction
q_hl <- vapply(HL_CODES, function(b)
  sum(valid_sites$lulc_highlevel == b, na.rm = TRUE) / n_valid, numeric(1))
m_hl <- compute_repr_metrics(p_hl, q_hl)
message(sprintf("  High-level (10-class): J = %.4f, H = %.4f",
                m_hl$weighted_jaccard, m_hl$hellinger_distance))

# Level 2
valid_l2 <- dplyr::filter(site_df, !is.na(lulc_level2))
n_l2 <- nrow(valid_l2)
p_l2 <- dist_l2$global_land_fraction
q_l2 <- vapply(L2_CODES, function(b)
  sum(valid_l2$lulc_level2 == b, na.rm = TRUE) / n_l2, numeric(1))
m_l2 <- compute_repr_metrics(p_l2, q_l2)
message(sprintf("  Level 2 (22-class): J = %.4f, H = %.4f",
                m_l2$weighted_jaccard, m_l2$hellinger_distance))

# Native
valid_nat <- dplyr::filter(site_df, !is.na(lulc_native) & lulc_native != 0)
n_nat <- nrow(valid_nat)
p_nat <- dist_native$global_land_fraction
q_nat <- vapply(native_codes_all, function(b)
  sum(valid_nat$lulc_native == b, na.rm = TRUE) / n_nat, numeric(1))
m_nat <- compute_repr_metrics(p_nat, q_nat)
message(sprintf("  Native (37-class):  J = %.4f, H = %.4f",
                m_nat$weighted_jaccard, m_nat$hellinger_distance))

n_hl_present  <- sum(q_hl > 0 | p_hl > 0)
n_l2_present  <- sum(q_l2 > 0 | p_l2 > 0)
n_nat_present <- sum(q_nat > 0 | p_nat > 0)

old_met <- if (file.exists(metrics_out)) {
  readr::read_csv(metrics_out, show_col_types = FALSE) |>
    dplyr::filter(axis != "landcover_cci")
} else {
  data.frame(axis = character(), aggregation_level = character(),
             n_classes = integer(), weighted_jaccard = numeric(),
             hellinger_distance = numeric())
}

metrics_new <- dplyr::bind_rows(
  old_met,
  data.frame(
    axis = rep("landcover_cci", 3),
    aggregation_level = c("high_level", "level2", "native"),
    n_classes = c(n_hl_present, n_l2_present, n_nat_present),
    weighted_jaccard = c(m_hl$weighted_jaccard, m_l2$weighted_jaccard,
                         m_nat$weighted_jaccard),
    hellinger_distance = c(m_hl$hellinger_distance, m_l2$hellinger_distance,
                           m_nat$hellinger_distance)
  )
)
readr::write_csv(metrics_new, metrics_out)
message("Saved: ", metrics_out)

cat("\n=== FULL METRICS TABLE ===\n")
print(as.data.frame(dplyr::mutate(metrics_new,
  J = round(weighted_jaccard, 3), H = round(hellinger_distance, 3)
)[, c("axis", "aggregation_level", "J", "H")]), row.names = FALSE)

# =============================================================================
# ---- 8. Figures -------------------------------------------------------------
# =============================================================================

base_theme <- theme_minimal(base_size = 10, base_family = "sans") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.ticks.x       = element_blank(),
    legend.position    = "right",
    plot.background    = element_rect(fill = "white", colour = NA),
    panel.background   = element_rect(fill = "white", colour = NA)
  )

make_lulc_fig <- function(class_codes, class_names, class_colors,
                          p_global, q_net,
                          J, H,
                          fig_title, fig_out,
                          fig_width = 8.5, fig_height = 7.5,
                          xaxis_angle = 35, xaxis_size = 7.5,
                          label_min_frac = 0.02) {

  # LCCS published ordering: class_codes are already in LCCS order
  class_factor <- factor(class_names, levels = class_names)
  colors_named <- setNames(class_colors, class_names)

  plot_long <- data.frame(
    class_id = class_factor,
    global   = p_global,
    network  = q_net,
    stringsAsFactors = FALSE
  ) |>
    tidyr::pivot_longer(c(global, network),
                        names_to = "bar", values_to = "fraction") |>
    dplyr::mutate(
      bar = factor(bar, levels = c("global", "network"),
                   labels = c("Global land", "FLUXNET\n(767 sites)")),
      label = dplyr::case_when(
        fraction >= 0.07              ~ sprintf("%.0f%%", fraction * 100),
        fraction >= label_min_frac    ~ sprintf("%.1f%%", fraction * 100),
        TRUE                          ~ NA_character_
      )
    )

  bars_panel <- ggplot(plot_long,
                       aes(x = bar, y = fraction, fill = class_id)) +
    geom_bar(stat = "identity", width = 0.6,
             colour = "white", linewidth = 0.15) +
    geom_text(aes(label = label),
              position = position_stack(vjust = 0.5),
              size = 2.2, family = "sans", colour = "black",
              na.rm = TRUE) +
    scale_fill_manual(
      values = colors_named,
      breaks = class_names,
      name   = NULL,
      guide  = guide_legend(ncol = 1, override.aes = list(
        colour = "grey50", linewidth = 0.3))
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.01)),
      labels = scales::percent_format(accuracy = 1),
      name   = "Fraction of total"
    ) +
    scale_x_discrete(name = NULL) +
    labs(title = fig_title) +
    annotate("text", x = Inf, y = Inf,
             label  = sprintf("J = %.2f\nH = %.2f", J, H),
             hjust  = 1.08, vjust = 1.5,
             size   = 2.9, family = "sans",
             colour = "grey25", lineheight = 1.2) +
    base_theme +
    theme(
      legend.key.size = unit(0.32, "cm"),
      legend.text     = element_text(size = 6.5, family = "sans")
    )

  ratio_df <- data.frame(
    class_id       = class_factor,
    p              = p_global,
    q              = q_net,
    stringsAsFactors = FALSE
  ) |>
    dplyr::mutate(
      sampling_ratio = dplyr::if_else(p > 0, q / p, NA_real_),
      label = dplyr::if_else(
        is.finite(sampling_ratio),
        sprintf("%.2f×", sampling_ratio),
        NA_character_
      )
    )

  finite_ratios <- ratio_df$sampling_ratio[is.finite(ratio_df$sampling_ratio)]
  y_lo <- max(0.04, min(0.8, min(finite_ratios, na.rm = TRUE)) * 0.5)
  y_hi <- min(60,  max(6,   max(finite_ratios, na.rm = TRUE)) * 1.6)

  ratio_panel <- ggplot(ratio_df,
                        aes(x = class_id, y = sampling_ratio,
                            colour = class_id)) +
    geom_hline(yintercept = 1, linetype = "dashed",
               colour = "grey50", linewidth = 0.5) +
    geom_segment(aes(xend = class_id, yend = 1),
                 colour = "grey75", linewidth = 0.5, na.rm = TRUE) +
    geom_point(data = dplyr::filter(ratio_df, is.finite(sampling_ratio)),
               size = 2.5) +
    geom_point(data = dplyr::filter(ratio_df,
                                     !is.finite(sampling_ratio) |
                                       is.na(sampling_ratio)),
               shape = 4, size = 2.5, alpha = 0.45) +
    geom_text(aes(label = label), vjust = -0.65, size = 2.0,
              family = "sans", colour = "black", na.rm = TRUE) +
    scale_colour_manual(values = colors_named, guide = "none",
                        na.value = "grey50") +
    scale_y_continuous(
      name   = "Sampling ratio\n(network / global)",
      trans  = "log2",
      breaks = c(0.0625, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16, 32),
      labels = c("0.06×","0.13×","0.25×","0.5×","1×",
                 "2×","4×","8×","16×","32×"),
      limits = c(y_lo, y_hi)
    ) +
    scale_x_discrete(name = NULL) +
    base_theme +
    theme(
      panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3),
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(size = xaxis_size, angle = xaxis_angle,
                                  hjust = 1),
      axis.title.y = element_text(size = 8),
      legend.position = "none"
    )

  fig <- (bars_panel / ratio_panel +
    plot_layout(heights = c(3.5, 2.2), guides = "keep")) &
    theme(plot.background = element_rect(fill = "white", colour = NA))

  ggsave(fig_out, plot = fig, width = fig_width, height = fig_height,
         dpi = 200, bg = "white")
  message("Saved: ", fig_out)
}

# ---- Figure 1: High-level (10 classes) in LCCS order -----------------------
message("\n--- Figure: High-level ---")
make_lulc_fig(
  class_codes  = HL_CODES,
  class_names  = HL_NAMES,
  class_colors = HL_COLORS,
  p_global     = dist_hl$global_land_fraction,
  q_net        = q_hl,
  J = m_hl$weighted_jaccard,
  H = m_hl$hellinger_distance,
  fig_title = "LULC — High-level (10-class, ESA CCI PUG Table 2)",
  fig_out   = file.path(out_dir,
                        "fig_representativeness_landcover_highlevel.png"),
  fig_width = 8.0, fig_height = 7.0,
  xaxis_angle = 25, xaxis_size = 8.0, label_min_frac = 0.02
)

# ---- Figure 2: Level 2 (22 classes) in LCCS order --------------------------
message("\n--- Figure: Level 2 ---")
make_lulc_fig(
  class_codes  = L2_CODES,
  class_names  = L2_NAMES,
  class_colors = L2_COLORS,
  p_global     = dist_l2$global_land_fraction,
  q_net        = q_l2,
  J = m_l2$weighted_jaccard,
  H = m_l2$hellinger_distance,
  fig_title = "LULC — Level 2 (22-class ESA CCI LCCS intermediate)",
  fig_out   = file.path(out_dir,
                        "fig_representativeness_landcover_level2.png"),
  fig_width = 10.0, fig_height = 8.0,
  xaxis_angle = 40, xaxis_size = 7.0, label_min_frac = 0.015
)

# ---- Figure 3: Native (37 classes) in LCCS code order ----------------------
message("\n--- Figure: Native ---")
# Use LCCS code as x-axis label (class names are too long for 37-class plot)
native_factor_levels <- as.character(native_codes_sorted)
native_factor_labels <- native_names_sorted

# Build named colors vector with names = class name strings
native_colors_vec <- NATIVE_COLORS
names(native_colors_vec) <- native_names_sorted

make_lulc_fig(
  class_codes  = native_codes_sorted,
  class_names  = native_names_sorted,
  class_colors = native_colors_vec,
  p_global     = dist_native$global_land_fraction,
  q_net        = q_nat,
  J = m_nat$weighted_jaccard,
  H = m_nat$hellinger_distance,
  fig_title = "LULC — Native LCCS (37-class ESA CCI v2.1.1)",
  fig_out   = file.path(out_dir,
                        "fig_representativeness_landcover_native.png"),
  fig_width = 13.0, fig_height = 9.0,
  xaxis_angle = 50, xaxis_size = 5.5, label_min_frac = 0.01
)

# =============================================================================
# ---- 9. Sampling ratio summaries -------------------------------------------
# =============================================================================

cat("\n=== SAMPLING RATIOS — HIGH-LEVEL ===\n")
print(as.data.frame(data.frame(
  class = HL_NAMES, p_pct = round(p_hl*100,1), q_pct = round(q_hl*100,1),
  ratio = round(q_hl/p_hl, 2))), row.names = FALSE)

cat("\n=== SAMPLING RATIOS — LEVEL 2 (notable deviations) ===\n")
l2_sr <- data.frame(
  class = L2_NAMES, p = round(p_l2*100,1), q = round(q_l2*100,1),
  ratio = round(q_l2/p_l2, 2)
) |> dplyr::arrange(desc(abs(ratio - 1)))
print(as.data.frame(head(l2_sr, 12)), row.names = FALSE)

cat("\n=== SAMPLING RATIOS — NATIVE (top over- and under-sampled) ===\n")
nat_sr <- data.frame(
  code = native_codes_all, name = dist_native$lulc_native_name,
  p = round(p_nat*100, 2), q = round(q_nat*100, 2),
  ratio = round(q_nat/p_nat, 2)
) |> dplyr::arrange(desc(ratio))
cat("  Most over-sampled:\n"); print(as.data.frame(head(nat_sr, 6)),
  row.names = FALSE)
nat_sr2 <- nat_sr |> dplyr::arrange(ratio)
cat("  Least sampled (present in global):\n")
print(as.data.frame(head(dplyr::filter(nat_sr2, p > 0), 6)),
  row.names = FALSE)

# =============================================================================
# ---- 10. Methods text -------------------------------------------------------
# =============================================================================

methods_out <- file.path(out_dir, "methods_landcover.md")

methods_lines <- c(
  "# Methods: Land Use / Land Cover Representativeness (ESA CCI LC v2.1.1)",
  "",
  "## Axis description",
  "",
  "This axis measures how well the FLUXNET tower network samples the major land",
  "use / land cover (LULC) types found across global land. Three aggregation",
  "levels are analysed in parallel: (1) the high-level 10-class scheme (ESA CCI",
  "PUG Table 2 cross-walk), (2) the intermediate Level 2 22-class LCCS hierarchy",
  "(published sub-divisions by leaf type, canopy openness, water regime, etc.),",
  "and (3) the full native 37-class LCCS map. Finer aggregation reveals within-",
  "class heterogeneity that the coarser levels collapse — the same pattern seen",
  "for the Koppen-Geiger climate axis across 5-class, 13-class, and 30-class",
  "aggregations (where J decreases and H increases at finer levels).",
  "",
  "## Data source",
  "",
  "ESA Climate Change Initiative — Land Cover project, version 2.1.1, year 2022.",
  "v2.1.1 is the Copernicus C3S continuation of ESA CCI LC, using an identical",
  "algorithm, 300 m resolution, and LCCS class system as v2.0.7 (1992-2015).",
  "The legend and aggregation scheme are unchanged between versions.",
  "",
  "Distribution: Copernicus Climate Data Store (CDS).",
  paste0("File: ", LC_FILENAME, " (NetCDF-4, 300m, LCCS integer classes)."),
  "Citation: ESA Climate Change Initiative — Land Cover project. ESA CCI Land",
  "Cover Product User Guide v2.0. European Space Agency. http://www.esa-landcover-cci.org",
  "",
  "## Three aggregation levels",
  "",
  "### Level 1 — High-level (10 classes, ESA CCI PUG Table 2)",
  "",
  "Standard cross-walk published by ESA CCI, identical across v2.0.7 and v2.1.1:",
  "",
  "  1  Cropland    : 10 11 12 20 30",
  "  2  Forest      : 50, 60-62, 70-72, 80-82, 90, 100",
  "  3  Shrubland   : 120 121 122 150 151 152 153 (incl. sparse vegetation)",
  "  4  Grassland   : 110 130",
  "  5  Wetland     : 160 170 180",
  "  6  Settlement  : 190",
  "  7  Bare        : 200 201 202",
  "  8  Snow/Ice    : 220",
  "  9  Water       : 210",
  "  10 Other       : 40 140",
  "",
  "### Level 2 — ESA CCI LCCS intermediate hierarchy (22 classes)",
  "",
  "The LCCS class naming convention embeds a hierarchy of discriminators",
  "(life form, water regime, leaf type, canopy openness) that maps directly to",
  "22 intermediate classes. This Level 2 is consistent with the subdivisions",
  "described in the ESA CCI LC Product User Guide sections on class definitions.",
  "",
  "| L2 code | Level 2 name | LCCS native codes | HL group |",
  "|---|---|---|---|",
  "| 1 | Rainfed cropland | 10, 11, 12 | Cropland |",
  "| 2 | Irrigated cropland | 20 | Cropland |",
  "| 3 | Mosaic cropland/nat-veg | 30 | Cropland |",
  "| 4 | Mosaic nat-veg/cropland | 40 | Other |",
  "| 5 | BL evergreen forest | 50 | Forest |",
  "| 6 | BL deciduous forest | 60, 61, 62 | Forest |",
  "| 7 | NL evergreen forest | 70, 71, 72 | Forest |",
  "| 8 | NL deciduous forest | 80, 81, 82 | Forest |",
  "| 9 | Mixed forest | 90 | Forest |",
  "| 10 | Mosaic tree-shrub/herb | 100 | Forest |",
  "| 11 | Mosaic herb/tree-shrub | 110 | Grassland |",
  "| 12 | Shrubland | 120, 121, 122 | Shrubland |",
  "| 13 | Grassland | 130 | Grassland |",
  "| 14 | Lichens and mosses | 140 | Other |",
  "| 15 | Sparse vegetation | 150, 151, 152, 153 | Shrubland |",
  "| 16 | Flooded forest (freshwater) | 160 | Wetland |",
  "| 17 | Flooded forest (saline) | 170 | Wetland |",
  "| 18 | Flooded shrub/herb | 180 | Wetland |",
  "| 19 | Urban areas | 190 | Settlement |",
  "| 20 | Bare areas | 200, 201, 202 | Bare |",
  "| 21 | Water bodies | 210 | Water |",
  "| 22 | Permanent snow and ice | 220 | Snow/Ice |",
  "",
  "Key discriminators at Level 2 vs. Level 1:",
  "  - Cropland: rainfed vs. irrigated vs. mosaic (important for water cycling)",
  "  - Forest: leaf type (broadleaved / needleleaved) and phenology",
  "    (evergreen / deciduous) are ecologically distinct for flux magnitude",
  "  - Wetland: fresh/brackish vs. saline flooded forest; flooded shrub/herb",
  "  - Other: mosaic nat-veg/cropland (code 40) and lichens/mosses (code 140)",
  "    are distinct ecosystems despite sharing a high-level code",
  "",
  "### Level 3 — Native LCCS (37 classes)",
  "",
  "The full set of native LCCS integer codes from the ESA CCI LC map,",
  "as read directly from the product. Codes 61/62 subdivide the broadleaved",
  "deciduous forest by canopy openness (closed >40% / open 15-40%); similarly",
  "for codes 71/72 (NL evergreen), 81/82 (NL deciduous), 121/122 (shrubland),",
  "151/152/153 (sparse vegetation), 201/202 (bare areas). Class 0 (No data)",
  "is excluded from all analysis.",
  "",
  "## Aggregation lookup table",
  "",
  "A complete three-level lookup table mapping each native LCCS code to",
  "Level 2 and high-level codes is saved to:",
  "  data/snapshots/cci_landcover_aggregation_lookup.csv",
  "",
  "## Per-site classification",
  "",
  paste0("Sites: 767 (snapshot fluxnet_shuttle_snapshot_20260624T095651.csv)."),
  "The native LCCS code per site (cci_native_class) was extracted previously",
  "from the 300m raster via terra::extract() with nearest-land recovery within",
  "3 degrees for NA / No-data sites. No re-extraction was performed for this",
  "update. Level 2 and high-level codes were assigned from the lookup table.",
  "",
  "## Global distribution",
  "",
  "The native 300m NetCDF was resampled to the KG 0.00833° grid (Beck 2023,",
  "1991-2020, 147.3 M km² total land) using method = 'near' (nearest-neighbour;",
  "preserves integer class codes). The resampled raster is cached at:",
  "  data/external/cci_landcover/v2.1.1/cci_lc_2022_kg_aligned_native.tif",
  "Classification rasters for Level 2 and high-level were produced via",
  "terra::classify() from the cached native raster. terra::cellSize() +",
  "terra::zonal(fun='sum') were used to compute area per class at all three",
  "levels. Class 0 excluded; water (class 210) retained as class 9 at all",
  "aggregation levels.",
  "",
  "## Colors",
  "",
  "Native (37 classes): RGB values from the published ESACCI-LC-Legend.csv.",
  "Level 2 (22 classes): unweighted mean RGB of the constituent native classes",
  "within each Level 2 group (legend RGB only; no area weighting at color",
  "computation stage). Snow/Ice overridden to #e0f3f8 (pale blue) at all",
  "three levels for visibility on white backgrounds.",
  "High-level (10 classes): same unweighted mean approach as Level 2.",
  "",
  "## Representativeness metrics",
  "",
  "Weighted Jaccard (J) and Hellinger distance (H) as described in",
  "methods_koppen_beck2023.md. p_k = global land fraction in class k;",
  "q_k = fraction of 767-site network in class k.",
  "",
  "| Aggregation | n classes | J | H |",
  "|---|---|---|---|",
  sprintf("| High-level | %d | %.3f | %.3f |",
          n_hl_present,  m_hl$weighted_jaccard,  m_hl$hellinger_distance),
  sprintf("| Level 2    | %d | %.3f | %.3f |",
          n_l2_present,  m_l2$weighted_jaccard,  m_l2$hellinger_distance),
  sprintf("| Native     | %d | %.3f | %.3f |",
          n_nat_present, m_nat$weighted_jaccard, m_nat$hellinger_distance),
  "",
  "J decreases and H increases with finer aggregation, consistent with the",
  "KG pattern: the coarser 10-class scheme masks within-group heterogeneity.",
  "Notable patterns visible at Level 2 but hidden at Level 1:",
  "  - Within Forest: needleleaved evergreen (boreal conifer) is over-sampled;",
  "    broadleaved evergreen (tropical) is under-sampled relative to global area.",
  "  - Within Cropland: rainfed vs. irrigated vs. mosaic breakdown reveals",
  "    whether the network captures irrigated agricultural ecosystems.",
  "  - Within Wetland: the three flooded-tree and flooded-shrub classes can be",
  "    separated to show which freshwater vs. saline wetland types are covered.",
  "",
  "## Why ESA CCI rather than MODIS IGBP",
  "",
  "ESA CCI LC provides an external, satellite-derived classification applied",
  "consistently across the global land surface. MODIS IGBP is the network's",
  "self-classification and has known site-level accuracy issues. Using an",
  "independent product avoids circularity. Disagreements between site IGBP and",
  "CCI classification are expected and outside the scope of this analysis."
)

writeLines(methods_lines, methods_out)
message("Saved: ", methods_out)
message("\nAll LULC outputs complete.")
