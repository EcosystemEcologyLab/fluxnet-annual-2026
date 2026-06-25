## figure_representativeness_biomass.R
## Above-ground biomass (AGB) representativeness axis.
## Data: ESA CCI Biomass v7.0, AGB Mg/ha.
##
## HYBRID BINNING SCHEME:
##   Bin 1: 0-5 Mg/ha (fixed) — separates near-zero / bare / ice / desert
##           from vegetated land.
##   Bins 2-7: six equal-area quantile bins computed from the area-weighted
##           global distribution of KG-land pixels with biomass >= 5 Mg/ha.
##
## Quantile breakpoints are computed at runtime from the global raster.
## The five breakpoints (at cumulative area fractions 1/6 through 5/6 of
## vegetated land) are documented in methods_biomass.md and site meta.
##
## Land mask: Beck 2023 KG raster (1991-2020) at 0.00833 deg, total land
## 147.3 M km2 — consistent with KG and aridity axes.

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
biomass_path <- file.path("data", "external", "cci_biomass",
                          "ESACCI-BIOMASS-L4-AGB-MERGED-1000m-fv7.0.tif")
kg_path      <- file.path("data", "external", "koppen_beck2023", "1991_2020",
                          "koppen_geiger_0p00833333.tif")
snap_path    <- file.path(FLUXNET_DATA_ROOT, "snapshots",
                          "fluxnet_shuttle_snapshot_20260624T095651.csv")
site_in      <- file.path(FLUXNET_DATA_ROOT, "snapshots",
                          "site_biomass_cci_v7.csv")
site_out     <- file.path(FLUXNET_DATA_ROOT, "snapshots",
                          "site_biomass_cci_v7.csv")
glob_out     <- file.path(FLUXNET_DATA_ROOT, "snapshots",
                          "biomass_cci_v7_global_distribution.csv")
metrics_out  <- file.path(FLUXNET_DATA_ROOT, "snapshots",
                          "representativeness_metrics.csv")
out_dir      <- file.path("review", "figures", "representativeness")
fig_out      <- file.path(out_dir, "fig_representativeness_biomass.png")
methods_out  <- file.path(out_dir, "methods_biomass.md")

for (p in c(biomass_path, kg_path, snap_path, site_in)) {
  if (!file.exists(p)) stop("Required file not found: ", p, call. = FALSE)
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Load rasters -----------------------------------------------------------
message("Loading biomass raster ...")
bio_rast_all <- terra::rast(biomass_path)
n_layers  <- terra::nlyr(bio_rast_all)
lyr_names <- names(bio_rast_all)
message("  Dims: ", nrow(bio_rast_all), " x ", ncol(bio_rast_all),
        "  Layers: ", n_layers,
        "  CRS: ", terra::crs(bio_rast_all, describe = TRUE)$name)
message("  Layer names: ", paste(lyr_names, collapse = ", "))

BAND_YEAR <- 18L   # band 18 = 2024
if (BAND_YEAR > n_layers)
  stop("Band ", BAND_YEAR, " not found: raster has ", n_layers, " layers.")
selected_name <- lyr_names[BAND_YEAR]
bio_rast <- bio_rast_all[[BAND_YEAR]]
message("  Using band ", BAND_YEAR, ": '", selected_name, "' (2024)")
rm(bio_rast_all); gc()

message("Loading KG land mask raster ...")
kg_rast <- terra::rast(kg_path)

# ---- Resample biomass to KG grid and apply land mask ------------------------
message("\nResampling biomass to KG 0.00833 deg grid (bilinear) ...")
t0 <- proc.time()
bio_aligned <- terra::resample(bio_rast, kg_rast, method = "bilinear",
                               threads = TRUE)
message("  Elapsed: ", round((proc.time() - t0)[["elapsed"]], 1), " s")

# KG-land pixels with NA biomass -> assign 0 (-> bin 1)
bio_land <- terra::ifel(!is.na(kg_rast) & is.na(bio_aligned), 0, bio_aligned)

# Cell areas masked to KG land
message("Computing cell areas ...")
cell_areas <- terra::cellSize(kg_rast, mask = TRUE, unit = "km")

# ---- Step 1: Area-weighted quantile breakpoints ----------------------------
message("\nStep 1: Computing area-weighted quantile breakpoints (biomass >= 5 Mg/ha) ...")

# Fine histogram: 1 Mg/ha bins from 5 to 1000; catch-all for >= 1000
HIST_MAX  <- 1000L
HIST_STEP <- 1L
hist_lo  <- seq(5, HIST_MAX - HIST_STEP, by = HIST_STEP)
hist_hi  <- hist_lo + HIST_STEP
hist_ids <- seq_along(hist_lo)
catch_id <- max(hist_ids) + 1L

hist_rcl <- rbind(
  cbind(hist_lo, hist_hi, as.numeric(hist_ids)),
  c(HIST_MAX, 1e6, as.numeric(catch_id))
)

bio_veg <- terra::ifel(bio_land >= 5, bio_land, NA)
message("  Classifying to fine histogram bins (", length(hist_ids),
        " bins at ", HIST_STEP, " Mg/ha, plus catch-all) ...")
t1 <- proc.time()
bio_hist_bins <- terra::classify(bio_veg, hist_rcl, right = FALSE,
                                 include.lowest = TRUE)
message("  Classify elapsed: ", round((proc.time() - t1)[["elapsed"]], 1), " s")

message("  Zonal sum (area per fine bin) ...")
t2 <- proc.time()
hist_areas <- terra::zonal(cell_areas, bio_hist_bins, fun = "sum", na.rm = TRUE)
message("  Zonal elapsed: ", round((proc.time() - t2)[["elapsed"]], 1), " s")
names(hist_areas) <- c("hist_bin", "area_km2")
hist_areas <- hist_areas[!is.na(hist_areas$hist_bin), ]

# Map bin IDs to lower edges; sort and compute cumulative area
bin_lo_vec <- c(hist_lo, HIST_MAX)
hist_areas$lo <- bin_lo_vec[hist_areas$hist_bin]
hist_areas <- hist_areas[order(hist_areas$lo), ]
hist_areas$cum_area <- cumsum(hist_areas$area_km2)
total_veg_area_km2 <- tail(hist_areas$cum_area, 1)
message("  Vegetated land (>= 5 Mg/ha): ",
        format(round(total_veg_area_km2), big.mark = ","), " km2")

# 5 quantile breakpoints at 1/6, 2/6, 3/6, 4/6, 5/6 of vegetated area
Q_PROBS <- (1:5) / 6
q_raw <- vapply(Q_PROBS, function(f) {
  target <- f * total_veg_area_km2
  idx    <- which(hist_areas$cum_area >= target)[1]
  hist_areas$lo[idx] + HIST_STEP  # upper edge of the crossing bin
}, numeric(1))
q_breaks <- round(q_raw, 1)  # canonical breakpoints to 1 decimal place

message("  Quantile breakpoints (Mg/ha): ",
        paste(paste0("q", 1:5, " = ", q_breaks), collapse = ", "))

# ---- Dynamic bin definitions ------------------------------------------------
BIN_BREAKS <- c(0, 5, q_breaks, Inf)
BIN_CODES  <- 1:7
BIN_COLORS <- c(
  "#f7f4f9",   # bin 1: 0-5     near-zero/bare/ice
  "#f0e1c4",   # bin 2: 5-q1   very sparse
  "#d4d491",   # bin 3: q1-q2
  "#a3c585",   # bin 4: q2-q3
  "#6cb375",   # bin 5: q3-q4
  "#2e8b57",   # bin 6: q4-q5
  "#14532d"    # bin 7: >q5    dense forest
)

fmt_q <- function(x) sprintf("%.1f", x)
BIN_LABELS <- c(
  "0–5 Mg/ha",
  sprintf("5–%s Mg/ha",           fmt_q(q_breaks[1])),
  sprintf("%s–%s Mg/ha", fmt_q(q_breaks[1]), fmt_q(q_breaks[2])),
  sprintf("%s–%s Mg/ha", fmt_q(q_breaks[2]), fmt_q(q_breaks[3])),
  sprintf("%s–%s Mg/ha", fmt_q(q_breaks[3]), fmt_q(q_breaks[4])),
  sprintf("%s–%s Mg/ha", fmt_q(q_breaks[4]), fmt_q(q_breaks[5])),
  sprintf(">%s Mg/ha",                 fmt_q(q_breaks[5]))
)

classify_biomass <- function(agb) {
  bin <- findInterval(agb, BIN_BREAKS[-length(BIN_BREAKS)], left.open = FALSE)
  bin[bin == 0] <- 1L
  bin[bin > 7]  <- 7L
  as.integer(bin)
}

# ---- Step 2: Re-classify sites from existing site CSV ----------------------
message("\nStep 2: Re-classifying sites under hybrid binning ...")
site_coords <- readr::read_csv(site_in, show_col_types = FALSE)
n_sites <- nrow(site_coords)
n_na    <- sum(is.na(site_coords$biomass_value_mg_ha))
message("  Sites: ", n_sites, "  |  NA biomass -> bin 1: ", n_na)

site_coords <- site_coords |>
  dplyr::mutate(
    agb_for_bin       = dplyr::if_else(is.na(biomass_value_mg_ha), 0,
                                       biomass_value_mg_ha),
    biomass_bin       = classify_biomass(agb_for_bin),
    biomass_bin_label = factor(BIN_LABELS[biomass_bin], levels = BIN_LABELS)
  ) |>
  dplyr::select(site_id, location_lat, location_long,
                biomass_value_mg_ha, biomass_bin, biomass_bin_label,
                biomass_method)

cat("\n=== SITE BIOMASS DISTRIBUTION (", n_sites, "sites, hybrid bins) ===\n")
bin_tbl <- site_coords |>
  dplyr::count(biomass_bin, biomass_bin_label, name = "n") |>
  dplyr::arrange(biomass_bin) |>
  dplyr::mutate(pct = round(100 * n / n_sites, 1))
print(as.data.frame(bin_tbl))

readr::write_csv(site_coords, site_out)
message("Saved: ", site_out)

write_output_metadata(
  site_out,
  input_sources = c(snap_path, biomass_path),
  notes = paste0(
    "Per-site AGB re-classified under hybrid binning scheme. ",
    "Biomass values unchanged from original extraction (ESA CCI Biomass v7.0, ",
    "band ", BAND_YEAR, " = '", selected_name, "'). ",
    "Hybrid bins: fixed bin 1 (0-5 Mg/ha); bins 2-7 are six equal-area quantile ",
    "bins from global KG-land distribution with biomass >= 5 Mg/ha. ",
    "Quantile breakpoints (Mg/ha): ",
    paste(paste0("q", 1:5, " = ", q_breaks), collapse = ", "), ". ",
    "NA values (", n_na, " site(s)) assigned to bin 1 (biomass_method = na_assigned_low). ",
    "Citation: Santoro & Cartus (2024) CEDA doi:10.5285/6429d1aafe1e43b9b414e4a5a7f8b903"
  )
)

# ---- Step 3: Global area-weighted distribution under hybrid bins -----------
message("\nStep 3: Global distribution under hybrid 7-bin scheme ...")
rcl_7 <- matrix(c(
  0,           5,           1,
  5,           q_breaks[1], 2,
  q_breaks[1], q_breaks[2], 3,
  q_breaks[2], q_breaks[3], 4,
  q_breaks[3], q_breaks[4], 5,
  q_breaks[4], q_breaks[5], 6,
  q_breaks[5], 1e6,         7
), ncol = 3, byrow = TRUE)
bio_bins <- terra::classify(bio_land, rcl_7, right = FALSE, include.lowest = TRUE)

message("  Zonal area sum (7 bins) ...")
t3 <- proc.time()
zone_areas <- terra::zonal(cell_areas, bio_bins, fun = "sum", na.rm = TRUE)
message("  Elapsed: ", round((proc.time() - t3)[["elapsed"]], 1), " s")
names(zone_areas) <- c("biomass_bin", "global_land_area_km2")
zone_areas <- zone_areas[!is.na(zone_areas$biomass_bin) &
                           zone_areas$biomass_bin %in% 1:7, ]

total_land_km2 <- sum(zone_areas$global_land_area_km2)
message("  Total land: ", format(round(total_land_km2), big.mark = ","),
        " km2  (KG baseline: 147,322,862 km2)")

dist_df <- data.frame(
  biomass_bin       = 1:7,
  biomass_bin_label = BIN_LABELS,
  biomass_min_mg_ha = c(0, 5, q_breaks),
  biomass_max_mg_ha = c(5, q_breaks, NA_real_)
) |>
  dplyr::left_join(zone_areas, by = "biomass_bin") |>
  dplyr::mutate(
    global_land_area_km2 = dplyr::coalesce(global_land_area_km2, 0),
    global_land_fraction = global_land_area_km2 / total_land_km2
  )

cat("\n=== GLOBAL BIOMASS DISTRIBUTION (hybrid bins) ===\n")
veg_area_per_bin <- total_veg_area_km2 / 6
print(as.data.frame(dplyr::mutate(dist_df,
  area_pct      = round(global_land_fraction * 100, 1),
  target_pct    = round(c(NA_real_, rep(veg_area_per_bin / total_land_km2 * 100, 6)), 1)
  )[, c("biomass_bin", "biomass_bin_label", "area_pct", "target_pct")]),
  row.names = FALSE)
cat("  (target_pct = equal-area ideal for bins 2-7)\n")

readr::write_csv(dist_df, glob_out)
message("Saved: ", glob_out)
write_output_metadata(
  glob_out,
  input_sources = c(biomass_path, kg_path),
  notes = paste0(
    "Global land area per AGB bin (7 hybrid bins). ",
    "Hybrid scheme: fixed bin 1 (0-5 Mg/ha); bins 2-7 are equal-area quantile bins ",
    "for land with biomass >= 5 Mg/ha. ",
    "Quantile breakpoints (Mg/ha): ",
    paste(paste0("q", 1:5, " = ", q_breaks), collapse = ", "), ". ",
    "Vegetated land (>= 5 Mg/ha): ",
    format(round(total_veg_area_km2), big.mark = ","), " km2. ",
    "Biomass raster band ", BAND_YEAR, " (2024) resampled to KG 0.00833 deg grid (bilinear). ",
    "KG-land pixels with NA biomass -> assigned value 0 (-> bin 1). ",
    "terra::cellSize(kg_rast, mask=TRUE) + terra::zonal(fun='sum'). ",
    "Total land: ", format(round(total_land_km2), big.mark = ","), " km2 ",
    "(matches KG land mask baseline 147,322,862 km2)."
  )
)

# ---- Step 4: Representativeness metrics ------------------------------------
message("\nStep 4: Representativeness metrics ...")
compute_repr_metrics <- function(p, q) {
  p[is.na(p)] <- 0; q[is.na(q)] <- 0
  list(
    weighted_jaccard   = sum(pmin(p, q)) / sum(pmax(p, q)),
    hellinger_distance = (1 / sqrt(2)) * sqrt(sum((sqrt(p) - sqrt(q))^2))
  )
}

p_global <- dist_df$global_land_fraction
q_net    <- vapply(1:7, function(b) {
  n <- sum(site_coords$biomass_bin == b, na.rm = TRUE)
  n / n_sites
}, numeric(1L))

m <- compute_repr_metrics(p_global, q_net)
message(sprintf("  Biomass metrics (hybrid) — J = %.4f, H = %.4f",
                m$weighted_jaccard, m$hellinger_distance))
message("  Previous fixed-bin metrics:  J = 0.6262, H = 0.1684")

cat("\n=== SAMPLING RATIOS (network / global, hybrid bins) ===\n")
sr_df <- data.frame(
  bin   = 1:7,
  label = BIN_LABELS,
  p_pct = round(p_global * 100, 1),
  q_pct = round(q_net    * 100, 1),
  ratio = round(q_net / p_global, 3)
)
print(as.data.frame(sr_df), row.names = FALSE)

old_met <- if (file.exists(metrics_out)) {
  readr::read_csv(metrics_out, show_col_types = FALSE) |>
    dplyr::filter(axis != "biomass_cci_v7")
} else {
  data.frame(axis = character(), aggregation_level = character(),
             n_classes = integer(), weighted_jaccard = numeric(),
             hellinger_distance = numeric())
}
metrics_df <- dplyr::bind_rows(
  old_met,
  data.frame(axis = "biomass_cci_v7", aggregation_level = "7bin_hybrid",
             n_classes = 7L, weighted_jaccard = m$weighted_jaccard,
             hellinger_distance = m$hellinger_distance)
)
readr::write_csv(metrics_df, metrics_out)
message("Saved: ", metrics_out)

cat("\n=== FULL METRICS TABLE ===\n")
print(as.data.frame(dplyr::mutate(metrics_df,
  J = round(weighted_jaccard, 3), H = round(hellinger_distance, 3)
)[, c("axis", "aggregation_level", "J", "H")]), row.names = FALSE)

# ---- Step 5: Figure ---------------------------------------------------------
base_theme <- theme_minimal(base_size = 10, base_family = "sans") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.ticks.x       = element_blank(),
    legend.position    = "right",
    plot.background    = element_rect(fill = "white", colour = NA),
    panel.background   = element_rect(fill = "white", colour = NA)
  )

bin_factor <- factor(BIN_LABELS, levels = BIN_LABELS)

plot_long <- data.frame(
  class_id = bin_factor,
  global   = p_global,
  network  = q_net,
  stringsAsFactors = FALSE
) |>
  tidyr::pivot_longer(c(global, network), names_to = "bar", values_to = "fraction") |>
  dplyr::mutate(
    bar = factor(bar, levels = c("global", "network"),
                 labels = c("Global land", "FLUXNET\n(767 sites)")),
    label = dplyr::case_when(
      fraction >= 0.07  ~ sprintf("%d\n%.1f%%", as.integer(class_id), fraction * 100),
      fraction >= 0.03  ~ sprintf("%d  %.1f%%", as.integer(class_id), fraction * 100),
      fraction >= 0.005 ~ as.character(as.integer(class_id)),
      TRUE              ~ NA_character_
    )
  )

bars_panel <- ggplot(plot_long, aes(x = bar, y = fraction, fill = class_id)) +
  geom_bar(stat = "identity", width = 0.6, colour = "white", linewidth = 0.2) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5),
            size = 2.3, family = "sans", colour = "black", na.rm = TRUE) +
  scale_fill_manual(
    values = setNames(BIN_COLORS, BIN_LABELS),
    breaks = BIN_LABELS,
    labels = paste0(1:7, ": ", BIN_LABELS),
    name   = NULL,
    guide  = guide_legend(ncol = 1, override.aes = list(colour = NA))
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
        legend.text     = element_text(size = 7, family = "sans"))

# x-axis labels for the ratio panel (abbreviated ranges)
xlab_ratio <- c(
  sprintf("1\n(0–5)"),
  sprintf("2\n(5–%s)", fmt_q(q_breaks[1])),
  sprintf("3\n(%s–%s)", fmt_q(q_breaks[1]), fmt_q(q_breaks[2])),
  sprintf("4\n(%s–%s)", fmt_q(q_breaks[2]), fmt_q(q_breaks[3])),
  sprintf("5\n(%s–%s)", fmt_q(q_breaks[3]), fmt_q(q_breaks[4])),
  sprintf("6\n(%s–%s)", fmt_q(q_breaks[4]), fmt_q(q_breaks[5])),
  sprintf("7\n(>%s)",        fmt_q(q_breaks[5]))
)

ratio_df <- data.frame(
  class_id       = bin_factor,
  sampling_ratio = q_net / p_global,
  stringsAsFactors = FALSE
) |>
  dplyr::mutate(label = sprintf("%.2f×", sampling_ratio))

ratio_panel <- ggplot(ratio_df, aes(x = class_id, y = sampling_ratio,
                                    colour = class_id)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50", linewidth = 0.5) +
  geom_segment(aes(xend = class_id, yend = 1),
               colour = "grey75", linewidth = 0.5) +
  geom_point(data = dplyr::filter(ratio_df, is.finite(sampling_ratio)), size = 3) +
  geom_point(data = dplyr::filter(ratio_df, !is.finite(sampling_ratio)),
             shape = 4, size = 3, alpha = 0.45) +
  geom_text(aes(label = label), vjust = -0.65, size = 2.4,
            family = "sans", colour = "black", na.rm = TRUE) +
  scale_colour_manual(values = setNames(BIN_COLORS, BIN_LABELS), guide = "none") +
  scale_y_continuous(
    name   = "Sampling ratio\n(network / global)",
    trans  = "log2",
    breaks = c(0.125, 0.25, 0.5, 1, 2, 4, 8, 16),
    labels = c("0.13×", "0.25×", "0.5×", "1×",
               "2×", "4×", "8×", "16×"),
    limits = c(0.03, 24)
  ) +
  scale_x_discrete(name = NULL, labels = xlab_ratio) +
  base_theme +
  theme(
    panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    axis.text.x        = element_text(size = 7),
    axis.title.y       = element_text(size = 8),
    legend.position    = "none"
  )

fig <- (bars_panel / ratio_panel +
  plot_layout(heights = c(3.5, 1.8), guides = "keep")) &
  theme(plot.background = element_rect(fill = "white", colour = NA))

ggsave(fig_out, plot = fig, width = 7.5, height = 6.5, dpi = 200, bg = "white")
message("Saved: ", fig_out)

# ---- Step 6: Methods text ---------------------------------------------------
methods_lines <- c(
  "# Methods: Above-Ground Biomass Representativeness (ESA CCI Biomass v7.0)",
  "",
  "## Axis description",
  "",
  "This axis measures network coverage of the global aboveground biomass density",
  "distribution. Biomass is framed as a continuous stock variable (Mg dry matter",
  "per hectare) rather than as a biome stratification: the question is whether the",
  "FLUXNET tower network samples the full range of biomass densities found across",
  "global land, not whether it samples named biome types.",
  "",
  "This framing is most relevant for:",
  "  - Carbon stock estimation and upscaling from flux measurements",
  "  - Biomass product validation (ESA CCI, GEDI, ICESat-2)",
  "  - Forest-carbon accounting, where representativeness across the biomass",
  "    density gradient determines the reliability of tower-based benchmarks",
  "",
  "## Data source",
  "",
  "ESA Climate Change Initiative — Biomass project, version 7.0 (March 2026).",
  "Product: ESA Biomass Climate Change Initiative (Biomass_cci): Global datasets",
  "of forest above-ground biomass for the years 2005-2012 and 2015-2024, v7.0.",
  "CEDA catalogue: https://catalogue.ceda.ac.uk/uuid/6429d1aafe1e43b9b414e4a5a7f8b903",
  "",
  "Citation: Santoro, M.; Cartus, O. (2024): ESA Biomass Climate Change Initiative",
  "(Biomass_cci): Global datasets of forest above-ground biomass for the years",
  "2005-2012 and 2015-2024, v7.0. NERC EDS Centre for Environmental Data Analysis.",
  "doi:10.5285/6429d1aafe1e43b9b414e4a5a7f8b903",
  "",
  "Variable: above-ground biomass (AGB), Mg/ha. Native resolution: 100 m,",
  "distributed as 1 deg x 1 deg tiles (~300 tiles per year, ~18 GB). The",
  "pre-aggregated 1 km product (single global file, 1.4 GB) was used for this",
  "analysis. Band used: band 18 (year 2024).",
  "",
  "Resolution rationale: EC tower footprints extend 0.5-3 km depending on wind",
  "speed and stability; a single 100 m pixel does not represent the flux footprint",
  "better than a 1 km mean. Using the 1 km product aligns naturally with the",
  "Beck 2023 KG land mask (0.00833 deg ~ 1 km), enabling direct grid alignment.",
  "The 1 km file was downloaded from the CEDA aggregated/ directory via anonymous",
  "HTTP (no authentication required).",
  "",
  "## Hybrid bin scheme",
  "",
  "Seven bins using a hybrid fixed + equal-area quantile scheme:",
  "",
  "  Bin 1: 0-5 Mg/ha (fixed) — separates near-zero / bare / ice / desert land",
  "         from vegetated land. Eddy covariance is rarely deployed on bare or",
  "         ice-covered surfaces; a single class absorbs all non-vegetated land.",
  "",
  "  Bins 2-7: six equal-area quantile bins computed from the global distribution",
  "         of KG-land pixels with biomass >= 5 Mg/ha (area-weighted). Each bin",
  "         contains approximately 1/6 of the total area of vegetated land, producing",
  "         six equally-sized slices of the biomass density gradient. This ensures",
  "         that no single bin dominates the figure and that the full range from",
  "         sparse to dense vegetation is represented without any pre-specified",
  "         ecological interpretation tied to the breakpoints.",
  "",
  sprintf("  Quantile breakpoints (Mg/ha): %s",
          paste(paste0("q", 1:5, " = ", q_breaks), collapse = ", ")),
  "",
  paste0("  Bin 1:  0-5 Mg/ha          (fixed lower cut)"),
  paste0("  Bin 2:  5-", q_breaks[1], " Mg/ha       (1st sixth of vegetated land)"),
  paste0("  Bin 3:  ", q_breaks[1], "-", q_breaks[2], " Mg/ha      (2nd sixth)"),
  paste0("  Bin 4:  ", q_breaks[2], "-", q_breaks[3], " Mg/ha     (3rd sixth)"),
  paste0("  Bin 5:  ", q_breaks[3], "-", q_breaks[4], " Mg/ha    (4th sixth)"),
  paste0("  Bin 6:  ", q_breaks[4], "-", q_breaks[5], " Mg/ha   (5th sixth)"),
  paste0("  Bin 7:  >", q_breaks[5], " Mg/ha        (6th sixth / top sixth)"),
  "",
  "These breakpoints are data-dependent and will be recomputed if the biomass product",
  "is updated or replaced. The values above were computed from ESA CCI Biomass v7.0,",
  sprintf("band 18 (year 2024), using %d Mg/ha histogram bins over the",
          length(hist_lo)),
  sprintf("range 5--%d Mg/ha plus a catch-all.", HIST_MAX),
  "",
  "## Quantile computation method",
  "",
  "Area-weighted quantiles were computed from the global raster as follows:",
  "  1. terra::resample(biomass, kg_rast, method='bilinear') to align biomass to",
  "     the KG 0.00833 deg grid.",
  "  2. Apply KG land mask: KG-land pixels with NA biomass -> assigned value 0",
  "     (-> bin 1) via terra::ifel().",
  "  3. Mask to pixels with biomass >= 5 Mg/ha.",
  sprintf("  4. Classify into fine histogram bins (%d Mg/ha step, range 5--%d Mg/ha",
          HIST_STEP, HIST_MAX),
  "     plus catch-all) using terra::classify().",
  "  5. Sum pixel area per histogram bin: terra::zonal(cellSize(kg_rast,",
  "     mask=TRUE, unit='km'), ..., fun='sum').",
  "  6. Compute cumulative area; find values at cumulative fractions 1/6 through",
  "     5/6 of total vegetated area (upper edge of the crossing histogram bin).",
  "  7. Round each breakpoint to 1 decimal place (canonical values used for all",
  "     downstream classification and reporting).",
  "",
  sprintf("Total vegetated land (>= 5 Mg/ha): %s km^2.",
          format(round(total_veg_area_km2), big.mark = ",")),
  "",
  "## Per-site classification",
  "",
  "Per-site biomass values (Mg/ha) were extracted previously from ESA CCI Biomass",
  "v7.0 band 18 at each site's reported lat/lon using terra::extract() (nearest",
  "pixel, 1 km raster). Bin assignments were recomputed here from those stored",
  "values using the new hybrid breakpoints; no re-extraction was necessary.",
  "",
  "Site coordinates from fluxnet_shuttle_snapshot_20260624T095651.csv (767 sites).",
  "Sites returning NA (ocean pixels, ice sheets, or biomass product coverage",
  "gaps) were assigned to bin 1 with biomass_method = 'na_assigned_low'.",
  sprintf("NA sites in this extraction: %d of %d.", n_na, n_sites),
  "",
  "## Global distribution and land mask",
  "",
  "The Beck 2023 KG raster (1991-2020, 0.00833 deg grid, 147,322,862 km^2",
  "total land) is used as the land mask for cross-axis consistency with the KG",
  "and aridity representativeness axes.",
  "",
  "Method:",
  "  1. terra::resample(biomass, kg_rast, method='bilinear') to align grids.",
  "  2. KG-land pixels where resampled biomass is NA -> assigned to value 0",
  "     (-> bin 1) using terra::ifel().",
  "  3. terra::classify() with the 7-bin hybrid matrix.",
  "  4. terra::cellSize(kg_rast, mask=TRUE, unit='km') for pixel areas.",
  "  5. terra::zonal(cell_areas, biomass_bins, fun='sum') to sum area per bin.",
  "",
  sprintf("Total land area: %s km^2 (aligned to KG land mask baseline).",
          format(round(total_land_km2), big.mark = ",")),
  "",
  "## Representativeness metrics",
  "",
  "Weighted Jaccard (J) and Hellinger distance (H) as described in",
  "methods_koppen_beck2023.md. p_k = global land fraction in bin k;",
  "q_k = fraction of 767-site FLUXNET network in bin k.",
  "",
  sprintf("  J = %.4f  (0 = no overlap, 1 = identical distribution)",
          m$weighted_jaccard),
  sprintf("  H = %.4f  (0 = identical, 1 = completely different)",
          m$hellinger_distance),
  "",
  "For comparison, the previous fixed-bin scheme (0-5, 5-25, 25-50, 50-100,",
  "100-200, 200-400, >400 Mg/ha) yielded J = 0.6262, H = 0.1684.",
  "",
  "## Interpretive note",
  "",
  "Under the hybrid binning scheme, bins 2-7 each represent approximately 1/6 of",
  "global vegetated land. A perfectly representative network would place an equal",
  "fraction of towers in each vegetated bin. Deviations from equal sampling reveal",
  "where the network is over- or under-represented across the biomass density",
  "gradient. This axis is most actionable for:",
  "  - Identifying biomass density ranges where new tower placement would improve",
  "    global carbon synthesis coverage",
  "  - Validating satellite biomass products across a representative density range",
  "  - Assessing whether tower-based upscaling correctly weights high-biomass",
  "    forests (which store disproportionate carbon)"
)
writeLines(methods_lines, methods_out)
message("Saved: ", methods_out)
message("\nAll outputs complete.")
