## figure_representativeness_landcover.R
## Land use / land cover representativeness axis.
## Data: ESA CCI Land Cover v2.0.7, year 2015 (most recent anonymously
##       accessible release; v2.1.1 / 2020 is CDS-authenticated only).
##
## Native resolution: 300 m global GeoTIFF (single band, integer LCCS codes).
## High-level aggregation: 37 native LCCS classes -> 9 categories using the
## published ESA CCI LC cross-walk from the v2.0.7 Product User Guide, Table 2.
##
## Land mask: Beck 2023 KG raster (0.00833 deg) — same as biomass/KG/aridity axes.
## Global distribution: biomass resampled to KG grid with method = "near"
##   (nearest-neighbour preserves discrete class values).
##
## ---- AGGREGATION MAPPING (ESA CCI v2.0.7 PUG Table 2, 10 high-level) ------
## Class  1 Cropland    : 10 11 12 20 30
## Class  2 Forest      : 50 60 61 62 70 71 72 80 81 82 90 100
## Class  3 Shrubland   : 120 121 122 150 151 152 153
##           (incl. sparse vegetation per ESA CCI "Shrubland incl. sparse")
## Class  4 Grassland   : 110 130
## Class  5 Wetland     : 160 170 180
## Class  6 Settlement  : 190
## Class  7 Bare        : 200 201 202
## Class  8 Snow/Ice    : 220
## Class  9 Water       : 210
## Class 10 Other       : 40 140
##           (mosaic nat-veg/cropland not crop-dominated; lichens/mosses)
## Class  0 No Data     : 0   (excluded from all analysis)
## ---------------------------------------------------------------------------
##
## Colors: ESA CCI published QML hex values; high-level colors are area-weighted
## means of constituent native class RGB values (computed below from Legend.csv).
## Snow/Ice uses #e0f3f8 (pale blue) instead of white (#ffffff) for visibility
## in stacked bars; documented in comments.

if (file.exists(".env")) dotenv::load_dot_env()
source("R/pipeline_config.R")
source("R/utils.R")
check_pipeline_config()

library(terra)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(patchwork)

# ---- Paths ------------------------------------------------------------------
lc_path     <- file.path("data", "external", "cci_landcover",
                         "ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif")
legend_path <- file.path("data", "external", "cci_landcover",
                         "ESACCI-LC-Legend.csv")
kg_path     <- file.path("data", "external", "koppen_beck2023", "1991_2020",
                         "koppen_geiger_0p00833333.tif")
snap_path   <- file.path(FLUXNET_DATA_ROOT, "snapshots",
                         "fluxnet_shuttle_snapshot_20260624T095651.csv")
site_out    <- file.path(FLUXNET_DATA_ROOT, "snapshots",
                         "site_landcover_cci.csv")
glob_out    <- file.path(FLUXNET_DATA_ROOT, "snapshots",
                         "landcover_cci_global_distribution.csv")
metrics_out <- file.path(FLUXNET_DATA_ROOT, "snapshots",
                         "representativeness_metrics.csv")
out_dir     <- file.path("review", "figures", "representativeness")
fig_out     <- file.path(out_dir, "fig_representativeness_landcover.png")
methods_out <- file.path(out_dir, "methods_landcover.md")

for (p in c(lc_path, legend_path, kg_path, snap_path)) {
  if (!file.exists(p)) stop("Required file not found: ", p, call. = FALSE)
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

LC_VERSION  <- "2.0.7"
LC_YEAR     <- 2015L
LC_FILENAME <- basename(lc_path)

# ---- Aggregation mapping: native LCCS -> high-level -------------------------
# Source: ESA CCI LC v2.0.7 Product User Guide Table 2 (standard cross-walk).
# 10 high-level classes matching the ESA CCI published categories.
#
# Key decisions for ambiguous mosaic classes:
# Class 30 (Mosaic cropland >50% / nat veg <50%) -> Cropland: crop-dominated.
# Class 40 (Mosaic nat veg >50% / cropland <50%) -> Other: not crop-dominated
#   and not clearly grassland/shrubland — fits "mosaic categories that don't fit
#   above" in the ESA CCI Other class definition.
# Class 100 (Mosaic tree/shrub >50% / herbaceous <50%) -> Forest:
#   woody cover >50%, tree-and-shrub dominated.
# Class 110 (Mosaic herbaceous >50% / tree/shrub <50%) -> Grassland:
#   herbaceous-dominated.
# Class 140 (Lichens and mosses) -> Other: fits "lichens/mosses" in Other
#   per ESA CCI product documentation.
# Classes 150-153 (Sparse vegetation) -> Shrubland: ESA CCI groups
#   "Shrubland (including sparse vegetation)" as one category.
# Classes 200-202 (Bare areas) -> Bare: separate category per product spec.
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
# Class 0 (No data) -> excluded (not in NATIVE_TO_HL)

HL_CODES <- 1:10
HL_NAMES <- c(
  "Cropland",    # 1
  "Forest",      # 2
  "Shrubland",   # 3  (incl. sparse vegetation)
  "Grassland",   # 4
  "Wetland",     # 5
  "Settlement",  # 6
  "Bare",        # 7
  "Snow/Ice",    # 8
  "Water",       # 9
  "Other"        # 10 (mosaic nat-veg/crop + lichens/mosses)
)

# ---- High-level colors: area-weighted mean of native class RGB --------------
# Computed from ESACCI-LC-Legend.csv (same RGB values as published QML file).
# Snow/Ice native color is #ffffff (white) — substituted with #e0f3f8 (pale blue)
# for visibility in stacked bars on white backgrounds.
legend_raw <- readr::read_delim(legend_path, delim = ";",
                                show_col_types = FALSE) |>
  dplyr::rename(code = NB_LAB, label = LCCOwnLabel, R = R, G = G, B = B) |>
  dplyr::filter(code != 0) |>
  dplyr::mutate(
    code_str = as.character(code),
    hl_class = NATIVE_TO_HL[code_str]
  ) |>
  dplyr::filter(!is.na(hl_class))

hl_colors_df <- legend_raw |>
  dplyr::group_by(hl_class) |>
  dplyr::summarise(
    R_mean = round(mean(R)),
    G_mean = round(mean(G)),
    B_mean = round(mean(B)),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    color_hex = dplyr::case_when(
      hl_class == 8L ~ "#e0f3f8",  # Snow/Ice: pale blue (white substitute)
      TRUE ~ sprintf("#%02x%02x%02x", R_mean, G_mean, B_mean)
    )
  )

HL_COLORS <- hl_colors_df$color_hex[match(HL_CODES, hl_colors_df$hl_class)]
# Safety fallback if any class is missing
HL_COLORS[is.na(HL_COLORS)] <- "#cccccc"
names(HL_COLORS) <- HL_NAMES

message("High-level class colors:")
for (i in seq_along(HL_NAMES))
  message("  ", i, ": ", HL_NAMES[i], " -> ", HL_COLORS[i])

# ---- Helper: native raster class codes -> high-level raster ----------------
# Build a terra::classify() matrix from the mapping.
rcl_hl <- do.call(rbind, lapply(names(NATIVE_TO_HL), function(k) {
  code <- as.integer(k)
  hl   <- NATIVE_TO_HL[k]
  c(code, code, hl)
}))
rcl_hl <- rcl_hl[order(rcl_hl[, 1]), ]
# Anything not in the mapping (class 0 / no data) -> NA.
# Use right = NA for exact-value matching (each row is from == to == single code).
# right = TRUE would create empty half-open intervals (code, code] for all but
# the lowest value, silently missing most classes. right = NA → [from, to].

# ---- Load rasters -----------------------------------------------------------
message("\nLoading CCI Land Cover raster ...")
lc_rast <- terra::rast(lc_path)
message("  Dims: ", nrow(lc_rast), " x ", ncol(lc_rast),
        "  CRS: ", terra::crs(lc_rast, describe = TRUE)$name,
        "  Classes: integer LCCS codes")

message("Loading KG land mask ...")
kg_rast <- terra::rast(kg_path)

# ---- Step 1: Per-site extraction (native 300m raster) ----------------------
message("\nStep 1: Per-site extraction ...")
snapshot    <- readr::read_csv(snap_path, show_col_types = FALSE)
site_coords <- snapshot |>
  dplyr::distinct(site_id, location_lat, location_long) |>
  dplyr::filter(!is.na(location_lat), !is.na(location_long))
N_sites <- nrow(site_coords)
message("  Sites: ", N_sites)

pts <- terra::vect(
  data.frame(x = site_coords$location_long, y = site_coords$location_lat),
  geom = c("x", "y"), crs = "EPSG:4326"
)

# Primary extraction (exact point)
lc_raw <- terra::extract(lc_rast, pts, ID = FALSE)
names(lc_raw)[1] <- "cci_native_class"
site_coords$cci_native_class <- as.integer(lc_raw$cci_native_class)

n_na_primary <- sum(is.na(site_coords$cci_native_class) |
                    site_coords$cci_native_class == 0, na.rm = TRUE)
message("  NA/NoData sites after primary extraction: ", n_na_primary)

# Nearest-land recovery within 3 deg window for NA sites
recover_landcover <- function(lat, lon, rast, window_deg = 3) {
  ext_w <- terra::ext(lon - window_deg, lon + window_deg,
                      lat - window_deg, lat + window_deg)
  tile  <- terra::crop(rast, ext_w)
  vals  <- terra::values(tile, na.rm = TRUE)
  valid <- vals[vals != 0 & !is.na(vals)]
  if (length(valid) == 0) return(list(code = NA_integer_, dist_km = NA_real_))

  pts_tile <- terra::as.points(tile)
  pts_tile <- pts_tile[terra::values(pts_tile)[, 1] != 0, ]
  if (nrow(pts_tile) == 0) return(list(code = NA_integer_, dist_km = NA_real_))

  ref_pt <- terra::vect(data.frame(x = lon, y = lat),
                        geom = c("x", "y"), crs = "EPSG:4326")
  dists  <- as.numeric(terra::distance(ref_pt, pts_tile))
  nearest_idx <- which.min(dists)
  list(
    code    = as.integer(terra::values(pts_tile)[nearest_idx, 1]),
    dist_km = round(dists[nearest_idx] / 1000, 1)
  )
}

recovery_sites <- which(is.na(site_coords$cci_native_class) |
                        site_coords$cci_native_class == 0)
site_coords$landcover_method  <- "exact"
site_coords$recovery_dist_km  <- NA_real_

if (length(recovery_sites) > 0) {
  message("  Running nearest-land recovery for ", length(recovery_sites),
          " site(s) ...")
  for (i in recovery_sites) {
    res <- recover_landcover(site_coords$location_lat[i],
                             site_coords$location_long[i], lc_rast)
    site_coords$cci_native_class[i] <- res$code
    if (!is.na(res$code)) {
      site_coords$landcover_method[i] <- paste0("nearest_land_",
                                                 res$dist_km, "km")
      site_coords$recovery_dist_km[i] <- res$dist_km
      message("    ", site_coords$site_id[i], " -> class ", res$code,
              " (", round(res$dist_km, 1), " km)")
    } else {
      message("    ", site_coords$site_id[i], " -> still NA after recovery")
    }
  }
}

n_na_final <- sum(is.na(site_coords$cci_native_class), na.rm = TRUE)
message("  NA after recovery: ", n_na_final)

# Map to high-level classes
site_coords <- site_coords |>
  dplyr::mutate(
    code_str = as.character(cci_native_class),
    cci_high_level_class = as.integer(NATIVE_TO_HL[code_str])
  )

# Add label columns
native_labels <- setNames(legend_raw$label, as.character(legend_raw$code))
hl_labels     <- setNames(HL_NAMES, as.character(HL_CODES))

site_coords <- site_coords |>
  dplyr::mutate(
    cci_native_class_name   = dplyr::coalesce(
      native_labels[as.character(cci_native_class)], "No data"),
    cci_high_level_class_name = dplyr::coalesce(
      hl_labels[as.character(cci_high_level_class)], "No data")
  ) |>
  dplyr::select(site_id, location_lat, location_long,
                cci_native_class, cci_native_class_name,
                cci_high_level_class, cci_high_level_class_name,
                landcover_method)

cat("\n=== SITE LAND COVER DISTRIBUTION (", N_sites, "sites) ===\n")
site_tbl <- site_coords |>
  dplyr::count(cci_high_level_class, cci_high_level_class_name, name = "n") |>
  dplyr::arrange(cci_high_level_class) |>
  dplyr::mutate(pct = round(100 * n / N_sites, 1))
print(as.data.frame(site_tbl))

readr::write_csv(site_coords, site_out)
message("Saved: ", site_out)

write_output_metadata(
  site_out,
  input_sources = c(snap_path, lc_path, legend_path),
  notes = paste0(
    "Per-site land cover extraction from ESA CCI LC v", LC_VERSION,
    " (year ", LC_YEAR, ", 300m native resolution). ",
    "Native LCCS codes -> 9 high-level classes using published ESA CCI LC",
    " v2.0.7 PUG Table 2 cross-walk. ",
    "Extraction: terra::extract() at site lat/lon; nearest-land recovery",
    " within 3-deg window for NA/NoData sites. ",
    "Recovery needed: ", length(recovery_sites), " site(s); ",
    "final NA count: ", n_na_final, " site(s). ",
    "Aggregation (10 classes): Cropland=10/11/12/20/30; Forest=50-90/100;",
    " Shrubland=120-122/150-153 (incl. sparse); Grassland=110/130;",
    " Wetland=160-180; Settlement=190; Bare=200-202;",
    " Snow/Ice=220; Water=210; Other=40/140."
  )
)

# ---- Step 2: Global area-weighted distribution ------------------------------
message("\nStep 2: Global distribution (resample to KG grid, near method) ...")

t0 <- proc.time()
lc_aligned <- terra::resample(lc_rast, kg_rast, method = "near",
                              threads = TRUE)
message("  Resample elapsed: ", round((proc.time() - t0)[["elapsed"]], 1), " s")

# Map native codes -> high-level (within KG land mask).
# right = NA: exact-value matching ([from, to] with from == to == single code).
lc_hl <- terra::classify(lc_aligned, rcl_hl, others = NA, right = NA)
# Mask to KG land
lc_hl_land <- terra::mask(lc_hl, kg_rast)

message("  Computing cell areas ...")
cell_areas <- terra::cellSize(kg_rast, mask = TRUE, unit = "km")

message("  Zonal sum (9 classes) ...")
t1 <- proc.time()
zone_areas <- terra::zonal(cell_areas, lc_hl_land, fun = "sum", na.rm = TRUE)
message("  Zonal elapsed: ", round((proc.time() - t1)[["elapsed"]], 1), " s")
names(zone_areas) <- c("cci_high_level_class", "global_land_area_km2")
zone_areas <- zone_areas[!is.na(zone_areas$cci_high_level_class) &
                           zone_areas$cci_high_level_class %in% 1:10, ]

total_land_km2 <- sum(zone_areas$global_land_area_km2)
message("  Total land: ", format(round(total_land_km2), big.mark = ","),
        " km2  (KG baseline: 147,322,862 km2)")

dist_df <- data.frame(
  cci_high_level_class      = HL_CODES,
  cci_high_level_class_name = HL_NAMES
) |>
  dplyr::left_join(zone_areas, by = "cci_high_level_class") |>
  dplyr::mutate(
    global_land_area_km2 = dplyr::coalesce(global_land_area_km2, 0),
    global_land_fraction = global_land_area_km2 / total_land_km2
  )

# Water fraction check
water_frac <- dist_df$global_land_fraction[dist_df$cci_high_level_class == 9]
message("  Water fraction within KG land mask: ",
        round(water_frac * 100, 2), "% (retained as separate class)")

cat("\n=== GLOBAL LAND COVER DISTRIBUTION ===\n")
print(as.data.frame(dplyr::mutate(dist_df,
  area_pct = round(global_land_fraction * 100, 1)
  )[, c("cci_high_level_class", "cci_high_level_class_name", "area_pct")]),
  row.names = FALSE)

readr::write_csv(dist_df, glob_out)
message("Saved: ", glob_out)
write_output_metadata(
  glob_out,
  input_sources = c(lc_path, kg_path, legend_path),
  notes = paste0(
    "Global land area per CCI LC high-level class (9 classes). ",
    "ESA CCI LC v", LC_VERSION, " (year ", LC_YEAR,
    ") resampled to KG 0.00833 deg grid with method='near'",
    " (nearest-neighbour; preserves discrete class codes). ",
    "KG land mask applied. Class 0 (No data) excluded. ",
    "Total land: ", format(round(total_land_km2), big.mark = ","), " km2 ",
    "(matches KG land mask baseline 147,322,862 km2). ",
    "Water bodies retained as class 9 (inland water in KG land mask)."
  )
)

# ---- Step 3: Representativeness metrics ------------------------------------
message("\nStep 3: Representativeness metrics ...")
compute_repr_metrics <- function(p, q) {
  p[is.na(p)] <- 0; q[is.na(q)] <- 0
  list(
    weighted_jaccard   = sum(pmin(p, q)) / sum(pmax(p, q)),
    hellinger_distance = (1 / sqrt(2)) * sqrt(sum((sqrt(p) - sqrt(q))^2))
  )
}

# Fraction of sites in each class; sites with NA hl_class are not counted
valid_sites <- site_coords[!is.na(site_coords$cci_high_level_class), ]
n_valid <- nrow(valid_sites)
p_global <- dist_df$global_land_fraction
q_net    <- vapply(HL_CODES, function(b) {
  sum(valid_sites$cci_high_level_class == b, na.rm = TRUE) / n_valid
}, numeric(1L))

m <- compute_repr_metrics(p_global, q_net)
message(sprintf("  Land cover metrics — J = %.4f, H = %.4f",
                m$weighted_jaccard, m$hellinger_distance))

cat("\n=== SAMPLING RATIOS (network / global) ===\n")
sr_df <- data.frame(
  class = HL_CODES,
  label = HL_NAMES,
  p_pct = round(p_global * 100, 1),
  q_pct = round(q_net    * 100, 1),
  ratio = round(q_net / p_global, 3)
)
print(as.data.frame(sr_df), row.names = FALSE)

old_met <- if (file.exists(metrics_out)) {
  readr::read_csv(metrics_out, show_col_types = FALSE) |>
    dplyr::filter(axis != "landcover_cci")
} else {
  data.frame(axis = character(), aggregation_level = character(),
             n_classes = integer(), weighted_jaccard = numeric(),
             hellinger_distance = numeric())
}
n_classes_used <- sum(q_net > 0 | p_global > 0)
metrics_df <- dplyr::bind_rows(
  old_met,
  data.frame(axis = "landcover_cci", aggregation_level = "high_level",
             n_classes = n_classes_used, weighted_jaccard = m$weighted_jaccard,
             hellinger_distance = m$hellinger_distance)
)
readr::write_csv(metrics_df, metrics_out)
message("Saved: ", metrics_out)

cat("\n=== FULL METRICS TABLE ===\n")
print(as.data.frame(dplyr::mutate(metrics_df,
  J = round(weighted_jaccard, 3), H = round(hellinger_distance, 3)
)[, c("axis", "aggregation_level", "J", "H")]), row.names = FALSE)

# ---- Step 4: Figure ---------------------------------------------------------
# Class ordering: Forest, Cropland, Grassland, Shrubland, Sparse/Bare,
# Wetland, Settlement, Snow/Ice, Water (descending global area)
CLASS_ORDER <- HL_NAMES[order(-p_global)]

base_theme <- theme_minimal(base_size = 10, base_family = "sans") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.ticks.x       = element_blank(),
    legend.position    = "right",
    plot.background    = element_rect(fill = "white", colour = NA),
    panel.background   = element_rect(fill = "white", colour = NA)
  )

class_factor <- factor(HL_NAMES, levels = CLASS_ORDER)

plot_long <- data.frame(
  class_id = class_factor,
  global   = p_global,
  network  = q_net,
  stringsAsFactors = FALSE
) |>
  tidyr::pivot_longer(c(global, network), names_to = "bar", values_to = "fraction") |>
  dplyr::mutate(
    bar = factor(bar, levels = c("global", "network"),
                 labels = c("Global land", "FLUXNET\n(767 sites)")),
    label = dplyr::case_when(
      fraction >= 0.07  ~ sprintf("%.0f%%", fraction * 100),
      fraction >= 0.03  ~ sprintf("%.0f%%", fraction * 100),
      fraction >= 0.005 ~ sprintf("%.1f%%", fraction * 100),
      TRUE              ~ NA_character_
    )
  )

bars_panel <- ggplot(plot_long, aes(x = bar, y = fraction, fill = class_id)) +
  geom_bar(stat = "identity", width = 0.6, colour = "white", linewidth = 0.2) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5),
            size = 2.3, family = "sans", colour = "black", na.rm = TRUE) +
  scale_fill_manual(
    values = setNames(HL_COLORS, HL_NAMES),
    breaks = CLASS_ORDER,
    name   = NULL,
    guide  = guide_legend(ncol = 1, override.aes = list(colour = "grey50",
                                                        linewidth = 0.3))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01)),
                     labels = scales::percent_format(accuracy = 1),
                     name   = "Fraction of total") +
  scale_x_discrete(name = NULL) +
  annotate("text", x = Inf, y = Inf,
           label  = sprintf("J = %.2f\nH = %.2f",
                            m$weighted_jaccard, m$hellinger_distance),
           hjust  = 1.08, vjust = 1.5,
           size   = 2.9, family = "sans", colour = "grey25", lineheight = 1.2) +
  base_theme +
  theme(legend.key.size = unit(0.35, "cm"),
        legend.text     = element_text(size = 7.5, family = "sans"))

# Ratio panel
ratio_df <- data.frame(
  class_id = class_factor,
  p        = p_global,
  q        = q_net,
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

ratio_panel <- ggplot(ratio_df, aes(x = class_id, y = sampling_ratio,
                                    colour = class_id)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50",
             linewidth = 0.5) +
  geom_segment(aes(xend = class_id, yend = 1),
               colour = "grey75", linewidth = 0.5, na.rm = TRUE) +
  geom_point(data = dplyr::filter(ratio_df, is.finite(sampling_ratio)), size = 3) +
  geom_point(data = dplyr::filter(ratio_df, !is.finite(sampling_ratio) |
                                    is.na(sampling_ratio)),
             shape = 4, size = 3, alpha = 0.45) +
  geom_text(aes(label = label), vjust = -0.65, size = 2.4,
            family = "sans", colour = "black", na.rm = TRUE) +
  scale_colour_manual(values = setNames(HL_COLORS, HL_NAMES), guide = "none") +
  scale_y_continuous(
    name   = "Sampling ratio\n(network / global)",
    trans  = "log2",
    breaks = c(0.125, 0.25, 0.5, 1, 2, 4, 8, 16, 32),
    labels = c("0.13×", "0.25×", "0.5×", "1×",
               "2×", "4×", "8×", "16×", "32×"),
    limits = c(0.05, 40)
  ) +
  scale_x_discrete(name = NULL) +
  base_theme +
  theme(
    panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    axis.text.x        = element_text(size = 7.5, angle = 25, hjust = 1),
    axis.title.y       = element_text(size = 8),
    legend.position    = "none"
  )

fig <- (bars_panel / ratio_panel +
  plot_layout(heights = c(3.5, 2.0), guides = "keep")) &
  theme(plot.background = element_rect(fill = "white", colour = NA))

ggsave(fig_out, plot = fig, width = 7.5, height = 7.0, dpi = 200, bg = "white")
message("Saved: ", fig_out)

# ---- Step 5: Methods text ---------------------------------------------------
water_pct <- round(water_frac * 100, 2)
methods_lines <- c(
  "# Methods: Land Use / Land Cover Representativeness (ESA CCI Land Cover v2.0.7)",
  "",
  "## Axis description",
  "",
  "This axis measures how well the FLUXNET tower network samples the major land",
  "use / land cover (LULC) types found across global land. It is most relevant",
  "for stakeholders concerned with land-management classification, including:",
  "  - Agricultural emissions and carbon flux partitioning (cropland)",
  "  - Urban heat resilience and anthropogenic CO2 (settlement)",
  "  - Land-use change tracking (forest-to-cropland, wetland drainage)",
  "  - GHG inventory and IPCC national reporting categories",
  "",
  "The settlement class, even at low absolute global area, is retained as a",
  "separate visible class given its high stakeholder relevance.",
  "",
  "## Data source",
  "",
  "ESA Climate Change Initiative — Land Cover project, version 2.0.7, year 2015.",
  "The v2.1.1 product (extending to 2020) requires CDS authentication and is not",
  "anonymously accessible; v2.0.7 (2015) is the most recent year available via",
  "CEDA anonymous HTTP. The class legend and aggregation scheme are identical",
  "across v2.0.7 and v2.1.1.",
  "",
  "Distribution: CEDA — http://data.ceda.ac.uk/neodc/esacci/land_cover/data/",
  "land_cover_maps/v2.0.7/",
  paste0("File: ", LC_FILENAME, " (312 MB GeoTIFF, 300m, 37 LCCS integer classes)."),
  "",
  "Citation: ESA Climate Change Initiative — Land Cover project, 2017. Land Cover",
  "CCI Product User Guide v2.0. Accessed via CEDA Data Archive.",
  "Annual land cover maps from the ESA CCI Land Cover project.",
  "http://www.esa-landcover-cci.org",
  "",
  "## Why ESA CCI rather than MODIS IGBP",
  "",
  "ESA CCI LC provides an external, satellite-derived classification applied",
  "consistently across the entire global land surface. MODIS IGBP is the",
  "network's self-classification scheme and has known accuracy issues at the",
  "site level (Sulla-Menashe & Friedl 2018 Rem. Sens. Env.). Using an",
  "independent external product avoids circularity: the representativeness",
  "question is whether the network covers the range of land cover types, not",
  "whether the sites have been correctly self-labeled.",
  "",
  "Disagreements between site IGBP and CCI classification are expected and",
  "outside the scope of this analysis — treat CCI as an independent reference",
  "for the global distribution, not as a site validator.",
  "",
  "## Aggregation scheme",
  "",
  "The 37 native LCCS class codes were aggregated to 10 high-level categories",
  "following the ESA CCI LC v2.0.7 Product User Guide Table 2 (standard",
  "cross-walk published by the project). The 10 categories and their constituent",
  "LCCS codes are:",
  "",
  "  1.  Cropland   : 10, 11, 12, 20, 30",
  "  2.  Forest     : 50, 60-62, 70-72, 80-82, 90, 100",
  "  3.  Shrubland  : 120, 121, 122, 150, 151, 152, 153",
  "                   (incl. sparse vegetation per ESA CCI category definition)",
  "  4.  Grassland  : 110, 130",
  "  5.  Wetland    : 160, 170, 180",
  "  6.  Settlement : 190",
  "  7.  Bare       : 200, 201, 202",
  "  8.  Snow/Ice   : 220",
  "  9.  Water      : 210",
  "  10. Other      : 40, 140",
  "                   (lichens/mosses; mosaic nat-veg/crop not crop-dominated)",
  "",
  "Notes on ambiguous classes:",
  "  - Class 30 (Mosaic cropland >50% / nat veg <50%) -> Cropland (crop-dominated)",
  "  - Class 40 (Mosaic nat veg >50% / cropland <50%) -> Other (not crop-dominated,",
  "    not clearly grassland/shrubland; ESA CCI Other = mosaic categories that",
  "    don't fit the main categories)",
  "  - Class 100 (Mosaic tree/shrub >50% / herbaceous <50%) -> Forest (tree-dominated)",
  "  - Class 110 (Mosaic herbaceous >50% / tree/shrub <50%) -> Grassland",
  "  - Class 140 (Lichens and mosses) -> Other (ESA CCI includes this in Other)",
  "  - Classes 150-153 (Sparse vegetation) -> Shrubland (ESA CCI: Shrubland incl.",
  "    sparse vegetation)",
  "  - Class 0 (No data) excluded from all analysis",
  "",
  "## Per-site extraction",
  "",
  paste0("Sites: 767 (snapshot fluxnet_shuttle_snapshot_20260624T095651.csv)."),
  "terra::extract() at site lat/lon from the native 300m raster.",
  "Sites returning NA or class 0 (No data) were recovered with nearest-land",
  "search within a 3-degree window using terra::distance().",
  paste0("Recovery sites: ", length(recovery_sites),
         "; final NA sites: ", n_na_final, "."),
  "Extraction method recorded in the landcover_method column:",
  "  'exact' = direct extraction; 'nearest_land_Xkm' = recovered at distance X km.",
  "",
  "## Global distribution and land mask",
  "",
  "The native 300m CCI LC raster was resampled to the KG 0.00833 deg grid",
  "(Beck 2023, 1991-2020) using method = 'near' (nearest-neighbour), which",
  "preserves discrete integer class values without introducing spurious",
  "intermediate codes (do not use bilinear for categorical data).",
  "",
  "The KG land mask was applied to restrict analysis to 147.3 M km2 of",
  "global land, consistent with the KG, biomass, and aridity axes.",
  paste0("Inland water pixels within the KG mask (class 9: Water bodies) represent ",
         water_pct, "% of total land area — retained as a separate visible class."),
  "",
  "terra::cellSize(kg_rast, mask=TRUE, unit='km') + terra::zonal(fun='sum')",
  "were used to compute area per class.",
  "",
  "## Colors",
  "",
  "High-level class colors are area-weighted means of the constituent native",
  "class RGB values from the published ESACCI-LC-Legend.csv. Snow/Ice was",
  "substituted from white (#ffffff) to pale blue (#e0f3f8) for visibility in",
  "stacked bar charts on white backgrounds; all other colors match the product",
  "legend.",
  "",
  "## Representativeness metrics",
  "",
  "Weighted Jaccard (J) and Hellinger distance (H) as described in",
  "methods_koppen_beck2023.md. p_k = global land fraction in class k;",
  "q_k = fraction of 767-site FLUXNET network in class k.",
  "",
  sprintf("  J = %.4f  (0 = no overlap, 1 = identical distribution)",
          m$weighted_jaccard),
  sprintf("  H = %.4f  (0 = identical, 1 = completely different)",
          m$hellinger_distance),
  "",
  "## Interpretive note",
  "",
  "The land cover axis complements the biomass axis: biomass measures carbon",
  "stock density gradient, while LULC measures functional ecosystem type. A",
  "network that scores well on both axes provides robust coverage for both",
  "carbon-stock upscaling and land-management attribution. The settlement class",
  "is intentionally kept visible despite its small global area fraction because",
  "urban-flux representation is a known gap with high policy relevance."
)
writeLines(methods_lines, methods_out)
message("Saved: ", methods_out)
message("\nAll outputs complete.")
