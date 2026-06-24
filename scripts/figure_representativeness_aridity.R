## figure_representativeness_aridity.R
## Aridity axis for the FLUXNET representativeness figure.
## Runs two parallel binning schemes on CGIAR Aridity Index v3.1 data:
##   (1) 5-class canonical UNEP scheme
##   (2) 7-class extended scheme with humid subdivisions (FAO-derived)
##
## Steps executed in order:
##   1. Per-site AI extraction for all 767 sites (20260624 snapshot)
##   2. Global area-weighted distributions for both schemes
##   3. Representativeness metrics for both schemes; update representativeness_metrics.csv
##   4. Two stacked-bar + ratio-panel figures
##   5. Methods text (single file covering both schemes)
##
## Raster: data/external/aridity/Global-AI_ET0__annual_v3_1/ai_v31_yr.tif
##   INT2U; raw integer × 0.0001 = AI (P/PET); value 0 = ocean/no-data.
##   Extent: 60°S to 90°N (no Antarctic coverage; explains ~12.6 M km² difference
##   vs the KG raster total of 147.3 M km²).
##
## UNEP 5-class thresholds (canonical, World Atlas of Desertification 1992):
##   Hyper-Arid < 0.05 | Arid 0.05–0.20 | Semi-Arid 0.20–0.50
##   Dry Sub-Humid 0.50–0.65 | Humid ≥ 0.65
##
## Extended 7-class thresholds (lower 4 = canonical UNEP; upper 3 = FAO-derived):
##   ...Dry Sub-Humid 0.50–0.65 | Humid (low) 0.65–1.0
##   Humid (moderate) 1.0–2.0 | Hyper-Humid ≥ 2.0
##
## Sources:
##   Zomer et al. (2022) Sci Data 9:409. doi:10.1038/s41597-022-01493-1
##   Figshare: doi:10.6084/m9.figshare.7504448
##   UNEP (1992) World Atlas of Desertification. Arnold, London.

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
ar_dir      <- file.path("data", "external", "aridity",
                         "Global-AI_ET0__annual_v3_1")
rast_path   <- file.path(ar_dir, "ai_v31_yr.tif")
snap_path   <- file.path(FLUXNET_DATA_ROOT, "snapshots",
                         "fluxnet_shuttle_snapshot_20260624T095651.csv")
site_out    <- file.path(FLUXNET_DATA_ROOT, "snapshots", "site_aridity.csv")
glob5_out   <- file.path(FLUXNET_DATA_ROOT, "snapshots",
                         "aridity_unep5_global_distribution.csv")
glob7_out   <- file.path(FLUXNET_DATA_ROOT, "snapshots",
                         "aridity_unep7_global_distribution.csv")
metrics_out <- file.path(FLUXNET_DATA_ROOT, "snapshots",
                         "representativeness_metrics.csv")
out_dir     <- file.path("review", "figures", "representativeness")
fig5_out    <- file.path(out_dir, "fig_representativeness_aridity_unep5.png")
fig7_out    <- file.path(out_dir, "fig_representativeness_aridity_unep7.png")
methods_out <- file.path(out_dir, "methods_aridity_unep.md")

for (p in c(rast_path, snap_path)) {
  if (!file.exists(p)) stop("Required file not found: ", p, call. = FALSE)
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Class definitions ------------------------------------------------------

classes5   <- c("Hyper-Arid", "Arid", "Semi-Arid", "Dry Sub-Humid", "Humid")
ai_min5    <- c(0.00, 0.05, 0.20, 0.50, 0.65)
ai_max5    <- c(0.05, 0.20, 0.50, 0.65, Inf)

classes7   <- c("Hyper-Arid", "Arid", "Semi-Arid", "Dry Sub-Humid",
                "Humid (low)", "Humid (moderate)", "Hyper-Humid")
ai_min7    <- c(0.00, 0.05, 0.20, 0.50, 0.65, 1.00, 2.00)
ai_max7    <- c(0.05, 0.20, 0.50, 0.65, 1.00, 2.00,  Inf)

ai_to_unep5 <- function(ai) {
  dplyr::case_when(
    is.na(ai) | ai <= 0  ~ NA_character_,
    ai <  0.05           ~ "Hyper-Arid",
    ai <  0.20           ~ "Arid",
    ai <  0.50           ~ "Semi-Arid",
    ai <  0.65           ~ "Dry Sub-Humid",
    TRUE                 ~ "Humid"
  )
}

ai_to_unep7 <- function(ai) {
  dplyr::case_when(
    is.na(ai) | ai <= 0  ~ NA_character_,
    ai <  0.05           ~ "Hyper-Arid",
    ai <  0.20           ~ "Arid",
    ai <  0.50           ~ "Semi-Arid",
    ai <  0.65           ~ "Dry Sub-Humid",
    ai <  1.00           ~ "Humid (low)",
    ai <  2.00           ~ "Humid (moderate)",
    TRUE                 ~ "Hyper-Humid"
  )
}

# ---- Load raster and snapshot -----------------------------------------------
message("Loading AI raster: ", rast_path)
ai_rast <- terra::rast(rast_path)
message("  Dims: ", nrow(ai_rast), " × ", ncol(ai_rast),
        "  |  CRS: EPSG:", terra::crs(ai_rast, describe = TRUE)$code)

snapshot    <- readr::read_csv(snap_path, show_col_types = FALSE)
site_coords <- snapshot |>
  dplyr::distinct(site_id, location_lat, location_long) |>
  dplyr::filter(!is.na(location_lat), !is.na(location_long))
N <- nrow(site_coords)
message("Sites with coordinates: ", N)

# ---- Step 1: Per-site extraction (767 sites) --------------------------------
message("\nExtracting AI at exact site coordinates ...")
pts <- terra::vect(
  data.frame(x = site_coords$location_long, y = site_coords$location_lat),
  geom = c("x", "y"), crs = "EPSG:4326"
)

raw_vals <- terra::extract(ai_rast, pts, ID = FALSE)[[1L]]
site_coords$raw_val        <- raw_vals
site_coords$aridity_method <- "exact"

# Value 0 = ocean pixel → NA; apply nearest-land fallback
site_coords$raw_val[!is.na(site_coords$raw_val) &
                    site_coords$raw_val == 0L] <- NA_integer_
na_idx <- which(is.na(site_coords$raw_val))
message("NA after exact extraction: ", length(na_idx), " site(s)")

if (length(na_idx) > 0L) {
  int_mode <- function(x) {
    x <- x[!is.na(x) & x != 0]
    if (length(x) == 0L) return(NA_integer_)
    as.integer(names(sort(table(x), decreasing = TRUE))[[1L]])
  }

  buffer_degs   <- c(0.01, 0.05, 0.1, 0.25, 0.5)
  still_na      <- na_idx
  recovered_any <- logical(length(still_na))

  for (j in seq_along(still_na)) {
    pt_j <- pts[still_na[j], ]
    for (buf in buffer_degs) {
      buf_vals <- terra::extract(ai_rast, pt_j, buffer = buf, ID = TRUE)
      mode_val <- int_mode(buf_vals[[2L]])
      if (!is.na(mode_val)) {
        site_coords$raw_val[still_na[j]] <- mode_val
        site_coords$aridity_method[still_na[j]] <-
          paste0("buffer_", round(buf * 111, 0), "km")
        message("  ", site_coords$site_id[still_na[j]],
                " recovered at buffer ", buf, "°: raw ", mode_val,
                " (AI ", round(mode_val * 0.0001, 4), ")")
        recovered_any[j] <- TRUE
        break
      }
    }
  }
  still_na <- still_na[!recovered_any]

  if (length(still_na) > 0L) {
    message(length(still_na), " site(s) still NA — nearest-land search ...")
    for (j in seq_along(still_na)) {
      idx <- still_na[j]
      lng <- site_coords$location_long[idx]
      lat <- site_coords$location_lat[idx]
      local_rast <- terra::crop(ai_rast, terra::ext(lng - 3, lng + 3,
                                                     lat - 3, lat + 3))
      local_rast[local_rast == 0] <- NA
      land_pts <- terra::as.points(local_rast, na.rm = TRUE)
      if (terra::nrow(land_pts) == 0L) {
        message("  No land pixels within 3° of ", site_coords$site_id[idx])
        next
      }
      dists       <- terra::distance(pts[idx, ], land_pts)
      nearest_idx <- which.min(dists)
      nearest_val <- as.integer(terra::values(land_pts)[[1L]][nearest_idx])
      nearest_km  <- round(min(dists) / 1000, 1)
      site_coords$raw_val[idx]        <- nearest_val
      site_coords$aridity_method[idx] <- paste0("nearest_land_", nearest_km, "km")
      message("  ", site_coords$site_id[idx], " → raw ", nearest_val,
              " (AI ", round(nearest_val * 0.0001, 4), ") at ", nearest_km, " km")
    }
    still_na <- still_na[is.na(site_coords$raw_val[still_na])]
  }
  if (length(still_na) > 0L) {
    warning(length(still_na), " site(s) remain NA: ",
            paste(site_coords$site_id[still_na], collapse = ", "))
  }
}

out_sites <- site_coords |>
  dplyr::mutate(
    ai_value     = raw_val * 0.0001,
    unep_class_5 = factor(ai_to_unep5(ai_value), levels = classes5),
    unep_class_7 = factor(ai_to_unep7(ai_value), levels = classes7)
  ) |>
  dplyr::select(site_id, location_lat, location_long,
                ai_value, unep_class_5, unep_class_7, aridity_method)

n_sites <- nrow(out_sites)

cat("\n=== SITE DISTRIBUTION (", n_sites, " sites) ===\n")
cat("--- 5-class ---\n")
cls5_tbl <- out_sites |>
  dplyr::count(unep_class_5, name = "n", .drop = FALSE) |>
  dplyr::mutate(pct = round(100 * n / sum(n), 1))
print(as.data.frame(cls5_tbl))

cat("--- 7-class ---\n")
cls7_tbl <- out_sites |>
  dplyr::count(unep_class_7, name = "n", .drop = FALSE) |>
  dplyr::mutate(pct = round(100 * n / sum(n), 1))
print(as.data.frame(cls7_tbl))

cat("NA: ", sum(is.na(out_sites$ai_value)), "\n")
cat("AI range: [", round(min(out_sites$ai_value, na.rm = TRUE), 4), ",",
    round(max(out_sites$ai_value, na.rm = TRUE), 4), "]\n")

fb_sites <- out_sites |>
  dplyr::filter(aridity_method != "exact") |>
  dplyr::select(site_id, ai_value, unep_class_5, unep_class_7, aridity_method)
if (nrow(fb_sites) > 0L) {
  cat("--- Fallback sites ---\n")
  print(as.data.frame(fb_sites))
}

readr::write_csv(out_sites, site_out)
message("Saved: ", site_out)

write_output_metadata(
  site_out,
  input_sources = c(snap_path, rast_path),
  notes = paste0(
    "767-site re-extraction (snapshot 20260624T095651). ",
    "ai_value = raw_integer * 0.0001 per CGIAR v3.1. ",
    "Ocean pixels (raw value 0) treated as NA; recovered via buffer/nearest-land fallback. ",
    "unep_class_5: canonical UNEP 5-class (thresholds 0.05/0.20/0.50/0.65). ",
    "unep_class_7: 5-class + FAO humid subdivisions at 1.0 and 2.0. ",
    "Replaces prior version (which had single unep_class column)."
  )
)

# ---- Step 2: Global distributions -------------------------------------------
# classify() matrix convention: right=FALSE → intervals [from, to); value 0 → NA

rcl5 <- matrix(c(
  0,     1,      NA,   # ocean (value 0 only)
  1,     500,     1,   # Hyper-Arid
  500,   2000,    2,   # Arid
  2000,  5000,    3,   # Semi-Arid
  5000,  6500,    4,   # Dry Sub-Humid
  6500,  65536,   5    # Humid
), ncol = 3, byrow = TRUE)

rcl7 <- matrix(c(
  0,      1,      NA,  # ocean
  1,      500,     1,  # Hyper-Arid
  500,    2000,    2,  # Arid
  2000,   5000,    3,  # Semi-Arid
  5000,   6500,    4,  # Dry Sub-Humid
  6500,   10000,   5,  # Humid (low)
  10000,  20000,   6,  # Humid (moderate)
  20000,  65536,   7   # Hyper-Humid
), ncol = 3, byrow = TRUE)

message("\nClassifying raster → 5-class ...")
ar5 <- terra::classify(ai_rast, rcl5, right = FALSE)
message("Computing cell areas (km²) ...")
cell_areas <- terra::cellSize(ar5, mask = TRUE, unit = "km")

t0 <- proc.time()
z5 <- terra::zonal(cell_areas, ar5, fun = "sum", na.rm = TRUE)
message("  5-class zonal done in ", round((proc.time() - t0)[["elapsed"]], 1), " s")
names(z5) <- c("class_code", "global_land_area_km2")
z5 <- z5[z5$class_code >= 1L & z5$class_code <= 5L, ]

total_km2 <- sum(z5$global_land_area_km2)
message("  Total land area: ", format(round(total_km2), big.mark = ","), " km²")

dist5 <- data.frame(class_code = 1:5, unep_class = classes5,
                    ai_min = ai_min5, ai_max = ai_max5,
                    stringsAsFactors = FALSE) |>
  dplyr::left_join(z5, by = "class_code") |>
  dplyr::mutate(
    global_land_area_km2 = dplyr::coalesce(global_land_area_km2, 0),
    global_land_fraction = global_land_area_km2 / total_km2,
    unep_class = factor(unep_class, levels = classes5)
  ) |>
  dplyr::select(unep_class, ai_min, ai_max, global_land_area_km2, global_land_fraction)

cat("\n=== GLOBAL 5-CLASS DISTRIBUTION ===\n")
print(as.data.frame(dplyr::mutate(dist5,
  area_Mkm2 = round(global_land_area_km2 / 1e6, 2),
  pct = round(global_land_fraction * 100, 1)
)[, c("unep_class", "area_Mkm2", "pct")]))

message("\nClassifying raster → 7-class ...")
ar7 <- terra::classify(ai_rast, rcl7, right = FALSE)
t0  <- proc.time()
z7  <- terra::zonal(cell_areas, ar7, fun = "sum", na.rm = TRUE)
message("  7-class zonal done in ", round((proc.time() - t0)[["elapsed"]], 1), " s")
names(z7) <- c("class_code", "global_land_area_km2")
z7 <- z7[z7$class_code >= 1L & z7$class_code <= 7L, ]

dist7 <- data.frame(class_code = 1:7, unep_class = classes7,
                    ai_min = ai_min7, ai_max = ai_max7,
                    stringsAsFactors = FALSE) |>
  dplyr::left_join(z7, by = "class_code") |>
  dplyr::mutate(
    global_land_area_km2 = dplyr::coalesce(global_land_area_km2, 0),
    global_land_fraction = global_land_area_km2 / total_km2,
    unep_class = factor(unep_class, levels = classes7)
  ) |>
  dplyr::select(unep_class, ai_min, ai_max, global_land_area_km2, global_land_fraction)

cat("\n=== GLOBAL 7-CLASS DISTRIBUTION ===\n")
print(as.data.frame(dplyr::mutate(dist7,
  area_Mkm2 = round(global_land_area_km2 / 1e6, 2),
  pct = round(global_land_fraction * 100, 1)
)[, c("unep_class", "area_Mkm2", "pct")]))

readr::write_csv(dist5, glob5_out)
readr::write_csv(dist7, glob7_out)
message("Saved: ", glob5_out)
message("Saved: ", glob7_out)

glob_meta_path <- sub("unep5_", "unep_", glob5_out) |>
  sub("distribution.csv", "distribution.meta.json", x = _)
glob_meta_path <- file.path(FLUXNET_DATA_ROOT, "snapshots",
                            "aridity_unep_global_distribution.meta.json")
write_output_metadata(
  glob5_out,
  input_sources = rast_path,
  notes = paste0(
    "Global area-weighted UNEP aridity distribution (5-class and 7-class) via ",
    "terra::classify() + terra::cellSize(mask=TRUE, unit='km') + ",
    "terra::zonal(fun='sum'). Value 0 set to NA (ocean). ",
    "Total land area: ", format(round(total_km2), big.mark = ","), " km² ",
    "(aridity raster covers 60°S–90°N; excludes Antarctica)."
  )
)

# ---- Step 3: Metrics + update representativeness_metrics.csv ---------------
compute_repr_metrics <- function(p, q) {
  p[is.na(p)] <- 0; q[is.na(q)] <- 0
  list(
    weighted_jaccard   = sum(pmin(p, q)) / sum(pmax(p, q)),
    hellinger_distance = (1 / sqrt(2)) * sqrt(sum((sqrt(p) - sqrt(q))^2))
  )
}

q5 <- vapply(classes5, function(cl)
  sum(out_sites$unep_class_5 == cl, na.rm = TRUE) / n_sites, numeric(1L))
q7 <- vapply(classes7, function(cl)
  sum(out_sites$unep_class_7 == cl, na.rm = TRUE) / n_sites, numeric(1L))

m5 <- compute_repr_metrics(dist5$global_land_fraction, q5)
m7 <- compute_repr_metrics(dist7$global_land_fraction, q7)

message(sprintf("\n5-class metrics: J = %.4f, H = %.4f",
                m5$weighted_jaccard, m5$hellinger_distance))
message(sprintf("7-class metrics: J = %.4f, H = %.4f",
                m7$weighted_jaccard, m7$hellinger_distance))

# Sampling ratios for reporting
samp5 <- data.frame(unep_class = classes5, global = dist5$global_land_fraction,
                    network = q5, ratio = q5 / dist5$global_land_fraction)
samp7 <- data.frame(unep_class = classes7, global = dist7$global_land_fraction,
                    network = q7, ratio = q7 / dist7$global_land_fraction)
cat("\n=== SAMPLING RATIOS — 5-class ===\n")
print(as.data.frame(dplyr::mutate(samp5,
  g = round(global*100,1), n = round(network*100,1), r = round(ratio,2)
)[, c("unep_class","g","n","r")]))
cat("\n=== SAMPLING RATIOS — 7-class ===\n")
print(as.data.frame(dplyr::mutate(samp7,
  g = round(global*100,1), n = round(network*100,1), r = round(ratio,2)
)[, c("unep_class","g","n","r")]))

# Update metrics CSV: keep KG rows, replace any aridity rows, append new ones
old_met <- if (file.exists(metrics_out)) {
  readr::read_csv(metrics_out, show_col_types = FALSE) |>
    dplyr::filter(!grepl("^aridity", axis))  # drop old aridity rows
} else {
  data.frame(axis = character(), aggregation_level = character(),
             n_classes = integer(), weighted_jaccard = numeric(),
             hellinger_distance = numeric())
}

metrics_df <- dplyr::bind_rows(
  old_met,
  data.frame(axis = "aridity_unep5", aggregation_level = "unep5", n_classes = 5L,
             weighted_jaccard = m5$weighted_jaccard,
             hellinger_distance = m5$hellinger_distance),
  data.frame(axis = "aridity_unep7", aggregation_level = "unep7", n_classes = 7L,
             weighted_jaccard = m7$weighted_jaccard,
             hellinger_distance = m7$hellinger_distance)
)
readr::write_csv(metrics_df, metrics_out)
message("Saved: ", metrics_out)

cat("\n=== FULL METRICS TABLE ===\n")
print(as.data.frame(dplyr::mutate(metrics_df,
  J = round(weighted_jaccard, 3), H = round(hellinger_distance, 3)
)[, c("axis", "aggregation_level", "n_classes", "J", "H")]))

# ---- Step 4: Figures --------------------------------------------------------
# Color palettes (documented):
# 5-class: Humid = #cccccc (pale gray) instead of white-no-fill because the
#   Humid segment is the global majority and white-on-white is structurally
#   invisible. Uniform thin grey border (grey70) on all segments.
# 7-class: Humid trio = blues (light → dark → near-black) completing the
#   red-to-blue colorbar across the full aridity continuum.

color5 <- c(
  "Hyper-Arid"    = "#d73027",
  "Arid"          = "#fc8d59",
  "Semi-Arid"     = "#ffff33",
  "Dry Sub-Humid" = "#66bd63",
  "Humid"         = "#cccccc"
)
color7 <- c(
  "Hyper-Arid"       = "#d73027",
  "Arid"             = "#fc8d59",
  "Semi-Arid"        = "#ffff33",
  "Dry Sub-Humid"    = "#66bd63",
  "Humid (low)"      = "#74add1",
  "Humid (moderate)" = "#4575b4",
  "Hyper-Humid"      = "#313695"
)

labels5 <- c(
  "Hyper-Arid"    = "Hyper-Arid (AI < 0.05)",
  "Arid"          = "Arid (0.05–0.20)",
  "Semi-Arid"     = "Semi-Arid (0.20–0.50)",
  "Dry Sub-Humid" = "Dry Sub-Humid (0.50–0.65)",
  "Humid"         = "Humid (AI ≥ 0.65)"
)
labels7 <- c(
  "Hyper-Arid"       = "Hyper-Arid (AI < 0.05)",
  "Arid"             = "Arid (0.05–0.20)",
  "Semi-Arid"        = "Semi-Arid (0.20–0.50)",
  "Dry Sub-Humid"    = "Dry Sub-Humid (0.50–0.65)",
  "Humid (low)"      = "Humid, low (0.65–1.0)",
  "Humid (moderate)" = "Humid, moderate (1.0–2.0)",
  "Hyper-Humid"      = "Hyper-Humid (AI ≥ 2.0)"
)

base_theme <- theme_minimal(base_size = 10, base_family = "sans") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.ticks.x       = element_blank(),
    legend.position    = "right",
    plot.background    = element_rect(fill = "white", colour = NA),
    panel.background   = element_rect(fill = "white", colour = NA)
  )

# Helper: build long-format plot data from a distribution + site fractions
make_plot_df <- function(dist_df, q_vec, class_levels, bar_label_fn) {
  site_df <- data.frame(unep_class = factor(class_levels, levels = class_levels),
                        network = q_vec)
  dplyr::tibble(unep_class = factor(class_levels, levels = class_levels)) |>
    dplyr::left_join(
      dplyr::select(dist_df, unep_class, global = global_land_fraction),
      by = "unep_class"
    ) |>
    dplyr::left_join(site_df, by = "unep_class") |>
    dplyr::mutate(global  = dplyr::coalesce(global,  0),
                  network = dplyr::coalesce(network, 0)) |>
    tidyr::pivot_longer(c(global, network), names_to = "bar", values_to = "fraction") |>
    dplyr::mutate(
      bar = factor(bar,
                   levels = c("global", "network"),
                   labels = c("Global land", "FLUXNET\n(767 sites)")),
      label = bar_label_fn(as.character(unep_class), fraction)
    )
}

# Helper: build sampling ratio data frame
make_ratio_df <- function(dist_df, q_vec, class_levels) {
  dplyr::tibble(unep_class = factor(class_levels, levels = class_levels)) |>
    dplyr::left_join(
      dplyr::rename(dist_df, global_frac = global_land_fraction),
      by = "unep_class"
    ) |>
    dplyr::mutate(
      network_frac   = q_vec,
      sampling_ratio = network_frac / global_frac,
      label          = sprintf("%.2f×", sampling_ratio)
    )
}

# Helper: build figure (bars / ratio panel)
make_figure <- function(plot_df, ratio_df, color_map, class_levels,
                        legend_labels, metrics_list, ratio_limits) {
  p_bars <- ggplot(plot_df, aes(x = bar, y = fraction, fill = unep_class)) +
    geom_bar(stat = "identity", width = 0.55, colour = "grey70", linewidth = 0.25) +
    geom_text(
      aes(label = label),
      position = position_stack(vjust = 0.5),
      size = 2.9, family = "sans", colour = "black", lineheight = 0.9, na.rm = TRUE
    ) +
    scale_fill_manual(
      values = color_map, breaks = class_levels, labels = legend_labels,
      name = NULL,
      guide = guide_legend(ncol = 1, reverse = FALSE,
                           override.aes = list(colour = "grey70", linewidth = 0.25))
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.01)),
                       labels = scales::percent_format(accuracy = 1),
                       name = "Fraction of total") +
    scale_x_discrete(name = NULL) +
    annotate("text", x = Inf, y = Inf,
             label = sprintf("J = %.2f\nH = %.2f",
                             metrics_list$weighted_jaccard,
                             metrics_list$hellinger_distance),
             hjust = 1.08, vjust = 1.5,
             size = 2.9, family = "sans", colour = "grey25", lineheight = 1.2) +
    base_theme +
    theme(legend.key.size = unit(0.42, "cm"),
          legend.text     = element_text(size = 8))

  p_ratio <- ggplot(ratio_df,
                    aes(x = unep_class, y = sampling_ratio, colour = unep_class)) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50", linewidth = 0.5) +
    geom_segment(aes(xend = unep_class, yend = 1),
                 colour = "grey75", linewidth = 0.5) +
    geom_point(size = 3.5) +
    geom_text(aes(label = label), vjust = -0.65, size = 2.7, family = "sans",
              colour = "black") +
    scale_colour_manual(values = color_map[class_levels], guide = "none") +
    scale_y_continuous(
      name = "Sampling ratio\n(network / global)",
      trans = "log2",
      breaks = c(0.0625, 0.25, 0.5, 1, 2, 4, 8),
      labels = c("0.06×", "0.25×", "0.5×", "1×",
                 "2×", "4×", "8×"),
      limits = ratio_limits
    ) +
    scale_x_discrete(name = NULL) +
    base_theme +
    theme(
      panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3),
      panel.grid.major.x = element_blank(),
      axis.text.x        = element_text(size = 7.5, angle = 12, hjust = 0.7),
      axis.title.y       = element_text(size = 8),
      legend.position    = "none"
    )

  p_bars / p_ratio +
    plot_layout(heights = c(3, 1.8), guides = "keep") &
    theme(plot.background = element_rect(fill = "white", colour = NA))
}

# Label helpers: tiered thresholds
label_fn5 <- function(cls, frac) {
  dplyr::case_when(
    frac >= 0.08  ~ sprintf("%s\n%.1f%%", cls, frac * 100),
    frac >= 0.03  ~ sprintf("%s  %.1f%%", cls, frac * 100),
    frac >  0     ~ cls,
    TRUE          ~ NA_character_
  )
}
label_fn7 <- function(cls, frac) {
  dplyr::case_when(
    frac >= 0.07  ~ sprintf("%s\n%.1f%%", cls, frac * 100),
    frac >= 0.025 ~ sprintf("%s  %.1f%%", cls, frac * 100),
    frac >  0     ~ cls,
    TRUE          ~ NA_character_
  )
}

# Compute the ratio y-axis limits dynamically to include all finite ratios
all_ratios5 <- q5 / dist5$global_land_fraction
all_ratios7 <- q7 / dist7$global_land_fraction
finite5 <- all_ratios5[is.finite(all_ratios5) & all_ratios5 > 0]
finite7 <- all_ratios7[is.finite(all_ratios7) & all_ratios7 > 0]
ylim5 <- c(min(finite5) * 0.55, max(finite5) * 2.2)
ylim7 <- c(min(finite7) * 0.55, max(finite7) * 2.2)

# 5-class figure
pd5    <- make_plot_df(dist5, q5, classes5, label_fn5)
rd5    <- make_ratio_df(dist5, q5, classes5)
fig_p5 <- make_figure(pd5, rd5, color5, classes5, labels5, m5, ylim5)
ggsave(fig5_out, plot = fig_p5, width = 7, height = 6.5, dpi = 200, bg = "white")
message("Saved: ", fig5_out)

# 7-class figure
pd7    <- make_plot_df(dist7, q7, classes7, label_fn7)
rd7    <- make_ratio_df(dist7, q7, classes7)
fig_p7 <- make_figure(pd7, rd7, color7, classes7, labels7, m7, ylim7)
ggsave(fig7_out, plot = fig_p7, width = 7.5, height = 6.5, dpi = 200, bg = "white")
message("Saved: ", fig7_out)

# ---- Step 5: Methods text ---------------------------------------------------
pct_humid5_g  <- round(dist5$global_land_fraction[dist5$unep_class == "Humid"]    * 100, 1)
pct_ha_g      <- round(dist5$global_land_fraction[dist5$unep_class == "Hyper-Arid"] * 100, 1)
pct_hh_g      <- round(dist7$global_land_fraction[dist7$unep_class == "Hyper-Humid"] * 100, 1)
pct_humid7_n  <- round(sum(q7[classes7 %in% c("Humid (low)","Humid (moderate)","Hyper-Humid")]) * 100, 1)

methods_lines <- c(
  "Per-site aridity values were extracted from the CGIAR Global Aridity Index",
  "and Potential Evapotranspiration Dataset, Version 3.1 (Zomer et al. 2022;",
  "doi:10.1038/s41597-022-01493-1; Figshare doi:10.6084/m9.figshare.7504448).",
  "The dataset provides annual mean aridity index (AI) values at 30 arc-second",
  "(~1 km) resolution, derived from long-term (1970-2000) climate averages. The",
  "AI is defined as P/PET (annual precipitation divided by annual potential",
  "evapotranspiration computed by the Penman-Monteith equation) and is",
  "dimensionless. Values below one indicate more evaporative demand than",
  "precipitation supply; values above one indicate a water surplus. The raster",
  "stores 16-bit unsigned integers scaled by 10,000 (multiply by 0.0001 to obtain",
  "the true AI). Ocean and water-body pixels carry raw value 0 (not a formal NA",
  "flag); these were treated as missing data in all computations.",
  "",
  "Sites were classified into two parallel aridity schemes. The five-class",
  "canonical UNEP scheme follows the World Atlas of Desertification (UNEP 1992):",
  "Hyper-Arid (AI < 0.05), Arid (0.05 to 0.20), Semi-Arid (0.20 to 0.50), Dry",
  "Sub-Humid (0.50 to 0.65), and Humid (AI >= 0.65). The seven-class extended",
  "scheme retains these four lower thresholds and subdivides the Humid class",
  "following FAO usage: Humid (low) (0.65 to 1.0), Humid (moderate) (1.0 to 2.0),",
  "and Hyper-Humid (AI >= 2.0). The 1.0 threshold approximately marks the",
  "transition from energy-limited to water-excess conditions; the 2.0 threshold",
  "separates the moderately humid zone from rainforest-scale moisture regimes. Both",
  "schemes are shown to support a decision on which granularity goes into the final",
  "paper figure.",
  "",
  "Per-site AI values were extracted using terra::extract() at the exact reported",
  "coordinates of each FLUXNET site. Three sites (US-KS3, US-TaS, CN-SnB) are",
  "wetland sites whose coordinates fall on the raster ocean mask. For these, a",
  "nearest-land pixel was identified within a 3-degree search window using",
  "terra::as.points() and terra::distance(); all three were recovered at 0.6-1.0 km",
  "and assigned to the Humid class (AI 0.74-0.97). The aridity_method column in",
  "site_aridity.csv records the extraction method per site.",
  "",
  "The area-weighted global distribution was computed geodesically. The raster was",
  "classified to integer class codes using terra::classify() (value 0 mapped to NA",
  "to exclude ocean). terra::cellSize(mask=TRUE, unit='km') computed per-pixel land",
  "area in km2, correctly accounting for meridional convergence at high latitudes.",
  "terra::zonal(fun='sum') accumulated land area per class without materialising the",
  sprintf("full raster. Total land area: %s km2 (aridity raster covers 60 degrees S",
          format(round(total_km2), big.mark = ",")),
  "to 90 degrees N; excludes Antarctica, explaining the ~12.6 M km2 difference from",
  "the KG raster total of 147.3 M km2). For the 7-class scheme, the same cell-area",
  "raster was reused and only the zonal summation was re-run.",
  "",
  "The sampling ratio for each class is the network fraction divided by the global",
  "land fraction (values above 1 = over-sampled, below 1 = under-sampled). Two",
  "scalar metrics summarise overall representativeness. The weighted Jaccard",
  "(Ruzicka) similarity J = sum(min(p,q)) / sum(max(p,q)) and Hellinger distance",
  "H = (1/sqrt(2)) * sqrt(sum((sqrt(p) - sqrt(q))^2)) are both bounded [0,1].",
  sprintf("For the 5-class scheme: J = %.2f, H = %.2f. For the 7-class scheme:",
          m5$weighted_jaccard, m5$hellinger_distance),
  sprintf("J = %.2f, H = %.2f. Jaccard is interpretable as an overlap fraction;",
          m7$weighted_jaccard, m7$hellinger_distance),
  "Hellinger is more sensitive to class-specific mismatches.",
  "",
  "Color palette note: the 5-class figure uses pale gray (#cccccc) for the Humid",
  "class with a uniform thin grey border on all segments, substituting the original",
  sprintf("'white/no-fill' specification. White-on-white is structurally invisible"),
  sprintf("when the Humid class represents %.1f%% of global land area. The 7-class", pct_humid5_g),
  "figure uses a blue gradient for the humid trio (Humid low: #74add1, moderate:",
  "#4575b4, Hyper-Humid: #313695), completing a red-to-blue colorbar that maps the",
  "full aridity continuum from extreme drought to extreme moisture surplus."
)
writeLines(methods_lines, methods_out)
message("Saved: ", methods_out)
message("\nAll outputs complete.")
