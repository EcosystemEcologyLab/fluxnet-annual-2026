## figure_representativeness_aridity.R
## Aridity axis for the FLUXNET representativeness figure.
## Uses CGIAR Aridity Index v3.1 (Zomer et al. 2022) and UNEP 5-class binning.
##
## Steps executed in order:
##   1. Per-site AI extraction for all 767 sites (20260624 snapshot)
##   2. Global area-weighted UNEP class distribution
##   3. Representativeness metrics (weighted Jaccard, Hellinger); rename and
##      extend koppen_beck2023_representativeness_metrics.csv →
##      representativeness_metrics.csv
##   4. Stacked bar + sampling-ratio panel figure
##   5. Methods text (review/figures/representativeness/methods_aridity_unep.md)
##
## Raster: data/external/aridity/Global-AI_ET0__annual_v3_1/ai_v31_yr.tif
##   - CRS: EPSG:4326; resolution: 0.00833333° (~1 km); INT2U
##   - Raw integer values × 0.0001 = aridity index (dimensionless ratio P/PET)
##   - Value 0 = ocean / no-data (not NA in the raster; explicit exclusion below)
##   - Extent: -60° to 90° latitude (no Antarctic coverage)
##
## Source: Zomer, R.J. et al. (2022) Version 3 of the Global Aridity Index and
##   Potential Evapotranspiration Database. Sci Data 9, 409.
##   doi:10.1038/s41597-022-01493-1
##   Figshare: doi:10.6084/m9.figshare.7504448

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
glob_out    <- file.path(FLUXNET_DATA_ROOT, "snapshots",
                         "aridity_unep_global_distribution.csv")
old_metrics <- file.path(FLUXNET_DATA_ROOT, "snapshots",
                         "koppen_beck2023_representativeness_metrics.csv")
metrics_out <- file.path(FLUXNET_DATA_ROOT, "snapshots",
                         "representativeness_metrics.csv")
out_dir     <- file.path("review", "figures", "representativeness")
fig_out     <- file.path(out_dir, "fig_representativeness_aridity_unep.png")
methods_out <- file.path(out_dir, "methods_aridity_unep.md")

for (p in c(rast_path, snap_path)) {
  if (!file.exists(p)) stop("Required file not found: ", p, call. = FALSE)
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- UNEP class definitions -------------------------------------------------
# Raw integer × 0.0001 = AI (P/PET, dimensionless).
# Value 0 is ocean/no-data; minimum observed Hyper-Arid land value is ~1.
# In raw integer space (right-exclusive intervals, i.e. [from, to)):
#   [0, 1)     → ocean (NA)
#   [1, 500)   → Hyper-Arid   AI < 0.05
#   [500, 2000) → Arid         0.05 ≤ AI < 0.20
#   [2000, 5000) → Semi-Arid   0.20 ≤ AI < 0.50
#   [5000, 6500) → Dry Sub-Humid 0.50 ≤ AI < 0.65
#   [6500, 65536) → Humid      AI ≥ 0.65

unep_classes <- c("Hyper-Arid", "Arid", "Semi-Arid", "Dry Sub-Humid", "Humid")
unep_ai_min  <- c(0.00, 0.05, 0.20, 0.50, 0.65)
unep_ai_max  <- c(0.05, 0.20, 0.50, 0.65, Inf)

ai_to_unep <- function(ai) {
  dplyr::case_when(
    is.na(ai) | ai <= 0 ~ NA_character_,
    ai <  0.05          ~ "Hyper-Arid",
    ai <  0.20          ~ "Arid",
    ai <  0.50          ~ "Semi-Arid",
    ai <  0.65          ~ "Dry Sub-Humid",
    TRUE                ~ "Humid"
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

# ---- Step 1: Per-site extraction --------------------------------------------
message("\nExtracting AI at exact site coordinates ...")
pts <- terra::vect(
  data.frame(x = site_coords$location_long, y = site_coords$location_lat),
  geom = c("x", "y"), crs = "EPSG:4326"
)

raw_vals <- terra::extract(ai_rast, pts, ID = FALSE)[[1L]]
site_coords$raw_val        <- raw_vals
site_coords$aridity_method <- "exact"

# Value 0 = ocean pixel → treat as NA for fallback
site_coords$raw_val[!is.na(site_coords$raw_val) &
                    site_coords$raw_val == 0L] <- NA_integer_

na_idx <- which(is.na(site_coords$raw_val))
message("NA after exact extraction: ", length(na_idx), " site(s)")

if (length(na_idx) > 0L) {
  # Mode of non-zero, non-NA integer values
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
                " recovered at buffer ", buf, "° (~", round(buf * 111, 0),
                " km): raw ", mode_val, " → AI ", round(mode_val * 0.0001, 4))
        recovered_any[j] <- TRUE
        break
      }
    }
  }
  still_na <- still_na[!recovered_any]

  if (length(still_na) > 0L) {
    message(length(still_na), " site(s) still NA — nearest-land pixel search ...")
    for (j in seq_along(still_na)) {
      idx <- still_na[j]
      lng <- site_coords$location_long[idx]
      lat <- site_coords$location_lat[idx]
      search_ext <- terra::ext(lng - 3, lng + 3, lat - 3, lat + 3)
      local_rast <- terra::crop(ai_rast, search_ext)
      local_rast[local_rast == 0] <- NA  # mask ocean
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
              " (AI ", round(nearest_val * 0.0001, 4), ") at ",
              nearest_km, " km")
    }
    still_na <- still_na[is.na(site_coords$raw_val[still_na])]
  }

  if (length(still_na) > 0L) {
    warning(length(still_na), " site(s) remain NA: ",
            paste(site_coords$site_id[still_na], collapse = ", "))
  }
}

# Build output data frame
out_sites <- site_coords |>
  dplyr::mutate(
    ai_value   = raw_val * 0.0001,
    unep_class = ai_to_unep(ai_value),
    unep_class = factor(unep_class, levels = unep_classes)
  ) |>
  dplyr::select(site_id, location_lat, location_long,
                ai_value, unep_class, aridity_method)

n_sites <- nrow(out_sites)

# Summary
cat("\n=== UNEP CLASS DISTRIBUTION (", n_sites, " sites) ===\n")
cls_tbl <- out_sites |>
  dplyr::count(unep_class, name = "n", .drop = FALSE) |>
  dplyr::mutate(pct = round(100 * n / sum(n), 1))
print(as.data.frame(cls_tbl))
cat("NA remaining:", sum(is.na(out_sites$unep_class)), "\n")
cat("AI range: [", round(min(out_sites$ai_value, na.rm=TRUE), 4), ",",
    round(max(out_sites$ai_value, na.rm=TRUE), 4), "]\n")

fb_sites <- out_sites |>
  dplyr::filter(aridity_method != "exact") |>
  dplyr::select(site_id, ai_value, unep_class, aridity_method)
if (nrow(fb_sites) > 0L) {
  cat("\n--- Fallback extraction sites ---\n")
  print(as.data.frame(fb_sites))
}

# Write
readr::write_csv(out_sites, site_out)
message("Saved: ", site_out)
write_output_metadata(
  site_out,
  input_sources = c(snap_path, rast_path),
  notes = paste0(
    "767-site re-extraction (snapshot 20260624T095651). ",
    "ai_value = raw_integer * 0.0001 per CGIAR v3.1 documentation. ",
    "Ocean/no-data pixels have raw value 0; these were treated as NA and ",
    "recovered via progressive-buffer nearest-land fallback. ",
    "UNEP class breakpoints: Hyper-Arid AI < 0.05, Arid 0.05–0.20, ",
    "Semi-Arid 0.20–0.50, Dry Sub-Humid 0.50–0.65, Humid >= 0.65. ",
    "Overwrites prior 759-site version from snapshot 20260428."
  )
)

# ---- Step 2: Global area-weighted UNEP distribution -------------------------
message("\nClassifying raster to UNEP classes ...")

# Classify matrix: [from, to) → class code (NA for ocean value 0)
rcl <- matrix(c(
  0,    1,     NA,  # ocean (value 0 only; [0, 1) captures integer 0)
  1,    500,    1,  # Hyper-Arid:     [1, 500) → raw 1–499
  500,  2000,   2,  # Arid:           [500, 2000) → raw 500–1999
  2000, 5000,   3,  # Semi-Arid:      [2000, 5000) → raw 2000–4999
  5000, 6500,   4,  # Dry Sub-Humid:  [5000, 6500) → raw 5000–6499
  6500, 65536,  5   # Humid:          [6500, 65536) → raw 6500–65535
), ncol = 3, byrow = TRUE)

ar_class   <- terra::classify(ai_rast, rcl, right = FALSE)
cell_areas <- terra::cellSize(ar_class, mask = TRUE, unit = "km")

message("Computing zonal sums ...")
t0         <- proc.time()
zone_areas <- terra::zonal(cell_areas, ar_class, fun = "sum", na.rm = TRUE)
elapsed    <- round((proc.time() - t0)[["elapsed"]], 1)
names(zone_areas) <- c("class_code", "global_land_area_km2")
message("  Done in ", elapsed, " s  |  ", nrow(zone_areas), " UNEP zones found")

# Keep valid codes only (1–5)
zone_areas <- zone_areas |>
  dplyr::filter(class_code >= 1L, class_code <= 5L)

total_land_km2 <- sum(zone_areas$global_land_area_km2)
message("  Total land area: ",
        format(round(total_land_km2), big.mark = ","), " km²",
        "  (KG raster: 147,322,862 km² for reference)")

dist_global <- data.frame(
  class_code = 1:5,
  unep_class = unep_classes,
  ai_min     = unep_ai_min,
  ai_max     = unep_ai_max,
  stringsAsFactors = FALSE
) |>
  dplyr::left_join(zone_areas, by = "class_code") |>
  dplyr::mutate(
    global_land_area_km2 = dplyr::coalesce(global_land_area_km2, 0),
    global_land_fraction = global_land_area_km2 / total_land_km2
  ) |>
  dplyr::select(unep_class, ai_min, ai_max, global_land_area_km2, global_land_fraction)

cat("\n=== GLOBAL UNEP CLASS DISTRIBUTION ===\n")
print(as.data.frame(dplyr::mutate(dist_global,
  area_Mkm2 = round(global_land_area_km2 / 1e6, 2),
  pct       = round(global_land_fraction * 100, 1)
)[, c("unep_class", "area_Mkm2", "pct")]))

readr::write_csv(dist_global, glob_out)
message("Saved: ", glob_out)
write_output_metadata(
  glob_out,
  input_sources = rast_path,
  notes = paste0(
    "Area-weighted global UNEP aridity class distribution via ",
    "terra::classify() + terra::cellSize(mask=TRUE, unit='km') + ",
    "terra::zonal(fun='sum'). Value 0 (ocean) set to NA via classify matrix. ",
    "Total land area: ", format(round(total_land_km2), big.mark = ","), " km². ",
    "Elapsed (classify + zonal): ", elapsed, " s."
  )
)

# ---- Step 3: Metrics + extend representativeness CSV -----------------------
compute_repr_metrics <- function(p, q) {
  p[is.na(p)] <- 0; q[is.na(q)] <- 0
  list(
    weighted_jaccard   = sum(pmin(p, q)) / sum(pmax(p, q)),
    hellinger_distance = (1 / sqrt(2)) * sqrt(sum((sqrt(p) - sqrt(q))^2))
  )
}

p_ar <- dist_global$global_land_fraction
q_ar <- vapply(unep_classes, function(cl)
  sum(out_sites$unep_class == cl, na.rm = TRUE) / n_sites,
  numeric(1L))

m_ar <- compute_repr_metrics(p_ar, q_ar)
message(sprintf("\nAridity metrics: J = %.4f, H = %.4f",
                m_ar$weighted_jaccard, m_ar$hellinger_distance))

# Load existing KG metrics and migrate schema (add axis + n_classes columns)
if (file.exists(old_metrics)) {
  old_df <- readr::read_csv(old_metrics, show_col_types = FALSE) |>
    dplyr::mutate(
      axis      = "koppen_beck2023",
      n_classes = dplyr::case_when(
        aggregation_level == "5class"            ~ 5L,
        aggregation_level == "13class_twoletter" ~ 13L,
        aggregation_level == "30class"           ~ 30L,
        TRUE                                     ~ NA_integer_
      )
    ) |>
    dplyr::select(axis, aggregation_level, n_classes,
                  weighted_jaccard, hellinger_distance)
} else {
  old_df <- data.frame(axis = character(), aggregation_level = character(),
                       n_classes = integer(), weighted_jaccard = numeric(),
                       hellinger_distance = numeric())
}

new_row <- data.frame(
  axis               = "aridity_unep",
  aggregation_level  = "5class",
  n_classes          = 5L,
  weighted_jaccard   = m_ar$weighted_jaccard,
  hellinger_distance = m_ar$hellinger_distance,
  stringsAsFactors   = FALSE
)

metrics_df <- dplyr::bind_rows(old_df, new_row)
readr::write_csv(metrics_df, metrics_out)
message("Saved: ", metrics_out)

cat("\n=== REPRESENTATIVENESS METRICS (all axes) ===\n")
print(as.data.frame(metrics_df))

# Sampling ratios for reporting
samp_ratio_ar <- dplyr::tibble(
  unep_class   = unep_classes,
  global_frac  = p_ar,
  network_frac = q_ar,
  sampling_ratio = q_ar / p_ar
)
cat("\n=== ARIDITY SAMPLING RATIOS ===\n")
print(as.data.frame(dplyr::mutate(samp_ratio_ar,
  global_pct  = round(global_frac  * 100, 1),
  network_pct = round(network_frac * 100, 1),
  ratio       = round(sampling_ratio, 2)
)[, c("unep_class", "global_pct", "network_pct", "ratio")]))

# ---- Step 4: Figure ---------------------------------------------------------
# Color palette — user spec with one substitution documented here:
# Original spec for Humid: white / no fill. In a stacked bar where Humid
# is the majority global class, white produces structural visual issues
# (invisible segment against white background; no border cue for segment extent).
# Substituted #cccccc (pale gray) with a uniform thin grey border (grey70) on
# all segments for visual consistency. This is documented in the methods text.
color_ar <- c(
  "Hyper-Arid"    = "#d73027",
  "Arid"          = "#fc8d59",
  "Semi-Arid"     = "#ffff33",
  "Dry Sub-Humid" = "#66bd63",
  "Humid"         = "#cccccc"
)

# Long format for stacked bars
site_ar <- out_sites |>
  dplyr::count(unep_class, name = "n", .drop = FALSE) |>
  dplyr::mutate(fraction = n / n_sites,
                unep_class = factor(unep_class, levels = unep_classes))

dist_global <- dist_global |>
  dplyr::mutate(unep_class = factor(unep_class, levels = unep_classes))

plot_ar <- dplyr::tibble(unep_class = factor(unep_classes, levels = unep_classes)) |>
  dplyr::left_join(
    dplyr::select(dist_global, unep_class, global = global_land_fraction),
    by = "unep_class"
  ) |>
  dplyr::left_join(
    dplyr::select(site_ar, unep_class, network = fraction),
    by = "unep_class"
  ) |>
  dplyr::mutate(
    global  = dplyr::coalesce(global,  0),
    network = dplyr::coalesce(network, 0)
  ) |>
  tidyr::pivot_longer(
    cols      = c(global, network),
    names_to  = "bar",
    values_to = "fraction"
  ) |>
  dplyr::mutate(
    bar = factor(bar,
                 levels = c("global", "network"),
                 labels = c("Global land", "FLUXNET\n(767 sites)")),
    label = dplyr::case_when(
      fraction >= 0.08  ~ sprintf("%s\n%.1f%%",
                                  as.character(unep_class), fraction * 100),
      fraction >= 0.03  ~ sprintf("%s  %.1f%%",
                                  as.character(unep_class), fraction * 100),
      fraction >  0     ~ as.character(unep_class),
      TRUE              ~ NA_character_
    )
  )

# Sampling ratio data (driest → most humid, left to right in panel)
ratio_ar <- dplyr::tibble(unep_class = factor(unep_classes, levels = unep_classes)) |>
  dplyr::left_join(
    dplyr::rename(dist_global, global_frac = global_land_fraction),
    by = "unep_class"
  ) |>
  dplyr::left_join(
    dplyr::rename(site_ar, network_frac = fraction),
    by = "unep_class"
  ) |>
  dplyr::mutate(
    network_frac   = dplyr::coalesce(network_frac, 0),
    sampling_ratio = network_frac / global_frac,
    label          = sprintf("%.2f×", sampling_ratio)
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

pAR_bars <- ggplot(plot_ar, aes(x = bar, y = fraction, fill = unep_class)) +
  geom_bar(stat = "identity", width = 0.55, colour = "grey70", linewidth = 0.25) +
  geom_text(
    aes(label = label),
    position   = position_stack(vjust = 0.5),
    size       = 3.0,
    family     = "sans",
    colour     = "black",
    lineheight = 0.9,
    na.rm      = TRUE
  ) +
  scale_fill_manual(
    values = color_ar,
    breaks = unep_classes,
    labels = c(
      "Hyper-Arid"    = "Hyper-Arid (AI < 0.05)",
      "Arid"          = "Arid (0.05–0.20)",
      "Semi-Arid"     = "Semi-Arid (0.20–0.50)",
      "Dry Sub-Humid" = "Dry Sub-Humid (0.50–0.65)",
      "Humid"         = "Humid (AI ≥ 0.65)"
    ),
    name  = NULL,
    guide = guide_legend(ncol = 1, reverse = FALSE,
                         override.aes = list(colour = "grey70", linewidth = 0.25))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.01)),
    labels = scales::percent_format(accuracy = 1),
    name   = "Fraction of total"
  ) +
  scale_x_discrete(name = NULL) +
  annotate(
    "text",
    x     = Inf, y = Inf,
    label = sprintf("J = %.2f\nH = %.2f",
                    m_ar$weighted_jaccard, m_ar$hellinger_distance),
    hjust = 1.08, vjust = 1.5,
    size  = 2.9, family = "sans", colour = "grey25", lineheight = 1.2
  ) +
  base_theme +
  theme(
    legend.key.size = unit(0.45, "cm"),
    legend.text     = element_text(size = 8)
  )

pAR_ratio <- ggplot(ratio_ar,
                    aes(x = unep_class, y = sampling_ratio,
                        colour = unep_class)) +
  geom_hline(yintercept = 1, linetype = "dashed",
             colour = "grey50", linewidth = 0.5) +
  geom_segment(aes(xend = unep_class, yend = 1),
               colour = "grey75", linewidth = 0.5) +
  geom_point(size = 3.5) +
  geom_text(
    aes(label = label),
    vjust  = -0.65,
    size   = 2.9,
    family = "sans",
    colour = "black"
  ) +
  scale_colour_manual(values = color_ar, guide = "none") +
  scale_y_continuous(
    name   = "Sampling ratio\n(network / global)",
    trans  = "log2",
    breaks = c(0.0625, 0.25, 0.5, 1, 2, 4),
    labels = c("0.06×", "0.25×", "0.5×", "1×", "2×", "4×"),
    limits = c(0.03, 6)
  ) +
  scale_x_discrete(name = NULL) +
  base_theme +
  theme(
    panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    axis.text.x        = element_text(size = 8, angle = 10, hjust = 0.7),
    axis.title.y       = element_text(size = 8),
    legend.position    = "none"
  )

pAR <- pAR_bars / pAR_ratio +
  plot_layout(heights = c(3, 1.8), guides = "keep") &
  theme(plot.background = element_rect(fill = "white", colour = NA))

ggsave(fig_out, plot = pAR, width = 7, height = 6.5, dpi = 200, bg = "white")
message("Saved: ", fig_out)

# ---- Step 5: Methods text ---------------------------------------------------
global_pct_hu <- round(dist_global$global_land_fraction[dist_global$unep_class == "Humid"]  * 100, 1)
global_pct_ha <- round(dist_global$global_land_fraction[dist_global$unep_class == "Hyper-Arid"] * 100, 1)

methods_text <- sprintf(
'Per-site aridity values were extracted from the CGIAR Global Aridity Index
and Potential Evapotranspiration Dataset, Version 3.1 (Zomer et al. 2022).
The dataset provides annual mean aridity index (AI) values at 30 arc-second
(~1 km) resolution, derived from long-term (1970–2000) averages of precipitation
and reference evapotranspiration computed by the Penman-Monteith equation. Full
citation: Zomer, R.J., Trabucco, A., Bossio, D.A., van Straaten, O., Verchot,
L.V. (2022). Version 3 of the Global Aridity Index and Potential
Evapotranspiration Database. Scientific Data 9, 409.
doi:10.1038/s41597-022-01493-1. Dataset archived at Figshare:
doi:10.6084/m9.figshare.7504448.

The aridity index is defined as the ratio of annual precipitation to annual
potential evapotranspiration (AI = P/PET), and is dimensionless. Values less
than one indicate more evaporative demand than precipitation supply (i.e., a
water deficit), while values greater than one indicate surplus. The raster
stores values as 16-bit unsigned integers scaled by 10,000, so that raw integer
values must be multiplied by 0.0001 to obtain the true AI.

Sites were classified into the five standard UNEP aridity categories as defined
in the UNEP World Atlas of Desertification (UNEP 1992): Hyper-Arid (AI < 0.05),
Arid (0.05 <= AI < 0.20), Semi-Arid (0.20 <= AI < 0.50),
Dry Sub-Humid (0.50 <= AI < 0.65), and Humid (AI >= 0.65). These five classes
partition a continuous gradient from the most xeric environments to perhumid
conditions.

Per-site AI values were extracted using terra::extract() at the exact reported
latitude and longitude of each FLUXNET site. The CGIAR raster assigns a value
of zero to ocean and water-body pixels (rather than a formal NA flag), so pixels
with raw value zero were treated as missing and a nearest-land recovery procedure
was applied: a progressive circular buffer (0.01 to 0.5 degrees) was searched for
non-zero pixel values, and if unsuccessful a nearest-land pixel search within a
3-degree window was used (terra::as.points(na.rm=TRUE) and terra::distance()
to locate the closest non-zero cell). The aridity_method column in
site_aridity.csv records whether each site was assigned by exact extraction or
by the fallback procedure.

The area-weighted global UNEP class distribution was computed using the same
geodesic approach applied to the Köppen-Geiger axis. The raster was first
classified into five class codes (zero set to NA to exclude ocean) using
terra::classify(), then per-pixel land area in km² was computed with
terra::cellSize(mask=TRUE, unit="km"), accounting for convergence of meridians
at high latitudes. terra::zonal(fun="sum") accumulated land area per class.
Total land area from this raster: %s km² (the aridity raster covers 60°S to
90°N; the small difference from the KG total of 147,322,862 km² reflects
differences in land mask and coverage).

The sampling ratio for each class is the network fraction divided by the global
land fraction: values above 1 indicate over-representation of that climate type
in the FLUXNET network relative to its global land area; values below 1 indicate
under-representation.

Two scalar metrics summarise overall representativeness. The weighted Jaccard
similarity (also known as the Ruzicka index):

    J = sum(min(p_k, q_k)) / sum(max(p_k, q_k))

where p_k is the global land fraction and q_k is the network fraction of class k,
is bounded [0, 1]; J = 1 means the distributions are identical and J = 0 means
no overlap. For the UNEP aridity axis at five-class aggregation, J = %.2f and
the Hellinger distance H = %.2f. Hellinger distance:

    H = (1/sqrt(2)) * sqrt(sum((sqrt(p_k) - sqrt(q_k))^2))

is also bounded [0, 1]; H = 0 means identical distributions and H = 1 means
maximal divergence. Hellinger is more sensitive than Jaccard to specific class
mismatches where one distribution has a large fraction in a class that the other
has near zero.

The Humid class (AI >= 0.65) constitutes %.1f%% of global land area by this
analysis but is represented by a moderate share of the network (temperate and
boreal forests, which support high tower density). The Hyper-Arid class
(AI < 0.05) covers %.1f%% of global land area and is structurally under-sampled
for the same physical reasons as polar EF in the Köppen-Geiger analysis: eddy
covariance towers in hyperarid environments require substantial logistical
infrastructure and are rarely established in the most remote desert interiors.

Color palette for the figure: Hyper-Arid #d73027, Arid #fc8d59, Semi-Arid
#ffff33, Dry Sub-Humid #66bd63. The original color specification called for
white (no fill) for the Humid class. In a stacked bar where Humid is the
majority global class, a white fill is structurally invisible against the white
panel background. The Humid class is therefore displayed in pale gray (#cccccc)
with a thin grey border (grey70) applied uniformly to all segments.
',
  format(round(total_land_km2), big.mark = ","),
  m_ar$weighted_jaccard,
  m_ar$hellinger_distance,
  global_pct_hu,
  global_pct_ha
)

writeLines(methods_text, methods_out)
message("Saved: ", methods_out)
message("\nAll outputs complete.")
