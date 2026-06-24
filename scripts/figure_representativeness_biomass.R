## figure_representativeness_biomass.R
## Above-ground biomass (AGB) representativeness axis.
## Data: ESA CCI Biomass v7.0, AGB Mg/ha.
##
## Resolution used: 1km pre-aggregated product (ESACCI-BIOMASS-L4-AGB-MERGED-1000m-fv7.0.tif).
## The native 100m product is distributed as ~300 tiles per year (~18 GB). The
## pre-aggregated 1km file (1.4 GB) is used for both site extraction and global
## distribution. EC tower footprints extend 0.5-3 km, so 1km matches the
## flux-relevant scale better than any single 100m pixel.
##
## Land mask: Beck 2023 KG raster (1991-2020) aligned to 0.00833° grid ensures
## total land = 147.3 M km², consistent with the KG and aridity axes.
##
## Seven-bin classification (Mg/ha):
##   Bin 1:  0- 5   Bare/ice/desert
##   Bin 2:  5-25   Sparse to open vegetation
##   Bin 3: 25-50   Shrubland/savanna
##   Bin 4: 50-100  Open forest
##   Bin 5: 100-200 Temperate forest
##   Bin 6: 200-400 Wet/boreal forest
##   Bin 7: >400    Tropical forest
##
## Breakpoints above 25 Mg/ha follow IPCC AR6 and major assessment conventions.
## The 0-5 and 5-25 splits are pragmatic: they separate non-vegetated land from
## sparsely-vegetated dryland/grassland. Document this in methods.
##
## NA handling:
##   - Sites with NA biomass → classified as bin 1 (biomass_method = na_assigned_low)
##   - KG-land pixels with NA biomass in global distribution → assigned to bin 1

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
site_out     <- file.path(FLUXNET_DATA_ROOT, "snapshots",
                          "site_biomass_cci_v7.csv")
glob_out     <- file.path(FLUXNET_DATA_ROOT, "snapshots",
                          "biomass_cci_v7_global_distribution.csv")
metrics_out  <- file.path(FLUXNET_DATA_ROOT, "snapshots",
                          "representativeness_metrics.csv")
out_dir      <- file.path("review", "figures", "representativeness")
fig_out      <- file.path(out_dir, "fig_representativeness_biomass.png")
methods_out  <- file.path(out_dir, "methods_biomass.md")

for (p in c(biomass_path, kg_path, snap_path)) {
  if (!file.exists(p)) stop("Required file not found: ", p, call. = FALSE)
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Bin definitions --------------------------------------------------------
BIN_BREAKS  <- c(0, 5, 25, 50, 100, 200, 400, Inf)
BIN_CODES   <- 1:7
BIN_LABELS  <- c(
  "Bare/ice/desert",       # 0-5
  "Sparse vegetation",     # 5-25
  "Shrubland/savanna",     # 25-50
  "Open forest",           # 50-100
  "Temperate forest",      # 100-200
  "Wet/boreal forest",     # 200-400
  "Tropical forest"        # >400
)
BIN_COLORS  <- c(
  "#f7f4f9",   # 0-5   very light gray/lavender
  "#f0e1c4",   # 5-25  pale yellow
  "#d4d491",   # 25-50 yellow-green
  "#a3c585",   # 50-100 light green
  "#6cb375",   # 100-200 medium green
  "#2e8b57",   # 200-400 forest green
  "#14532d"    # >400  dark green
)

classify_biomass <- function(agb) {
  bin <- findInterval(agb, BIN_BREAKS[-length(BIN_BREAKS)], left.open = FALSE)
  bin[bin == 0] <- 1L   # anything < 0 → bin 1 (clamp)
  bin[bin > 7]  <- 7L   # clamp upper
  as.integer(bin)
}

# ---- Load rasters -----------------------------------------------------------
message("Loading biomass raster ...")
bio_rast_all <- terra::rast(biomass_path)
message("  Dims: ", nrow(bio_rast_all), " × ", ncol(bio_rast_all),
        "  Layers: ", terra::nlyr(bio_rast_all),
        "  CRS: ", terra::crs(bio_rast_all, describe = TRUE)$name)
message("  Layer names: ", paste(names(bio_rast_all), collapse = ", "))

# v7.0 1km MERGED contains all available years as bands.
# Select the last layer (most recent year).
n_layers  <- terra::nlyr(bio_rast_all)
lyr_names <- names(bio_rast_all)
selected_layer <- n_layers
selected_name  <- lyr_names[selected_layer]
bio_rast <- bio_rast_all[[selected_layer]]
message("  Using layer ", selected_layer, ": '", selected_name, "' (most recent)")

message("Loading KG land mask raster ...")
kg_rast <- terra::rast(kg_path)

# ---- Step 1: Per-site biomass extraction ------------------------------------
message("\nLoading snapshot ...")
snapshot    <- readr::read_csv(snap_path, show_col_types = FALSE)
site_coords <- snapshot |>
  dplyr::distinct(site_id, location_lat, location_long) |>
  dplyr::filter(!is.na(location_lat), !is.na(location_long))
N <- nrow(site_coords)
message("Sites: ", N)

message("Extracting biomass at site coordinates ...")
pts <- terra::vect(
  data.frame(x = site_coords$location_long, y = site_coords$location_lat),
  geom = c("x", "y"), crs = "EPSG:4326"
)
bio_raw <- terra::extract(bio_rast, pts, ID = FALSE)
names(bio_raw)[1] <- "biomass_value_mg_ha"
site_coords$biomass_value_mg_ha <- bio_raw$biomass_value_mg_ha

n_na <- sum(is.na(site_coords$biomass_value_mg_ha))
message("  NA sites: ", n_na, " / ", N, " → assigned to bin 1 (biomass_method = na_assigned_low)")

site_coords <- site_coords |>
  dplyr::mutate(
    biomass_method   = dplyr::if_else(is.na(biomass_value_mg_ha),
                                      "na_assigned_low", "exact"),
    agb_for_bin      = dplyr::if_else(is.na(biomass_value_mg_ha), 0,
                                      biomass_value_mg_ha),
    biomass_bin      = classify_biomass(agb_for_bin),
    biomass_bin_label = factor(BIN_LABELS[biomass_bin], levels = BIN_LABELS)
  ) |>
  dplyr::select(site_id, location_lat, location_long,
                biomass_value_mg_ha, biomass_bin, biomass_bin_label,
                biomass_method)

n_sites <- nrow(site_coords)

cat("\n=== SITE BIOMASS DISTRIBUTION (", n_sites, "sites) ===\n")
bin_tbl <- site_coords |>
  dplyr::count(biomass_bin, biomass_bin_label, name = "n") |>
  dplyr::arrange(biomass_bin) |>
  dplyr::mutate(pct = round(100 * n / n_sites, 1))
print(as.data.frame(bin_tbl))

na_sites <- site_coords[site_coords$biomass_method == "na_assigned_low", "site_id"]
if (length(na_sites$site_id) > 0)
  message("NA sites: ", paste(na_sites$site_id, collapse = ", "))

readr::write_csv(site_coords, site_out)
message("Saved: ", site_out)
write_output_metadata(
  site_out,
  input_sources = c(snap_path, biomass_path),
  notes = paste0(
    "Per-site AGB extraction from ESA CCI Biomass v7.0 1km pre-aggregated product. ",
    "Raster layer used: '", selected_name, "' (layer ", selected_layer, " of ", n_layers, "). ",
    "Bin definitions: 0-5, 5-25, 25-50, 50-100, 100-200, 200-400, >400 Mg/ha. ",
    "NA values (", n_na, " site(s)) assigned to bin 1 (biomass_method = na_assigned_low). ",
    "1km resolution chosen: EC tower footprints 0.5-3 km; 100m sub-pixel gives no ",
    "additional information for coarse bin classification. Snapshot: 20260624T095651 ",
    "(", N, " sites). Citation: Santoro & Cartus (2024) CEDA doi:10.5285/6429d1aafe1e43b9b414e4a5a7f8b903"
  )
)

# ---- Step 2: Global area-weighted distribution ------------------------------
message("\nAligning biomass raster to KG land mask ...")
t0 <- proc.time()
# Resample biomass to KG grid (bilinear for continuous values)
bio_aligned <- terra::resample(bio_rast, kg_rast, method = "bilinear",
                               threads = TRUE)
message("  Resample elapsed: ", round((proc.time() - t0)[["elapsed"]], 1), " s")

# For KG-land pixels where biomass is NA → assign 0 (→ bin 1)
bio_land <- terra::ifel(!is.na(kg_rast) & is.na(bio_aligned), 0, bio_aligned)

message("Classifying to 7 bins ...")
rcl <- matrix(c(
  0,   5,  1,
  5,  25,  2,
  25,  50,  3,
  50, 100,  4,
  100, 200,  5,
  200, 400,  6,
  400, 1e6,  7
), ncol = 3, byrow = TRUE)
bio_bins <- terra::classify(bio_land, rcl, right = FALSE, include.lowest = TRUE)

message("Computing cell areas and zonal sums ...")
cell_areas <- terra::cellSize(kg_rast, mask = TRUE, unit = "km")
t1 <- proc.time()
zone_areas <- terra::zonal(cell_areas, bio_bins, fun = "sum", na.rm = TRUE)
elapsed_z  <- round((proc.time() - t1)[["elapsed"]], 1)
names(zone_areas) <- c("biomass_bin", "global_land_area_km2")
zone_areas <- zone_areas[!is.na(zone_areas$biomass_bin) &
                           zone_areas$biomass_bin %in% 1:7, ]

total_land_km2 <- sum(zone_areas$global_land_area_km2)
message("  Total land: ", format(round(total_land_km2), big.mark = ","), " km²",
        "  (KG baseline: 147,322,862 km²)")
message("  Elapsed (zonal): ", elapsed_z, " s")

dist_df <- data.frame(
  biomass_bin       = 1:7,
  biomass_bin_label = BIN_LABELS,
  biomass_min_mg_ha = head(BIN_BREAKS, -1),
  biomass_max_mg_ha = c(BIN_BREAKS[-c(1, length(BIN_BREAKS))], NA_real_)
) |>
  dplyr::left_join(zone_areas, by = "biomass_bin") |>
  dplyr::mutate(
    global_land_area_km2 = dplyr::coalesce(global_land_area_km2, 0),
    global_land_fraction = global_land_area_km2 / total_land_km2
  )

cat("\n=== GLOBAL BIOMASS DISTRIBUTION ===\n")
print(as.data.frame(dplyr::mutate(dist_df,
  area_pct = round(global_land_fraction * 100, 1))[,
  c("biomass_bin", "biomass_bin_label", "biomass_min_mg_ha", "area_pct")]),
  row.names = FALSE)

readr::write_csv(dist_df, glob_out)
message("Saved: ", glob_out)
write_output_metadata(
  glob_out,
  input_sources = c(biomass_path, kg_path),
  notes = paste0(
    "Global land area per AGB bin (7 bins). Biomass raster resampled to KG 0.00833° grid. ",
    "KG-land pixels with NA biomass assigned to bin 1. ",
    "terra::cellSize(kg_rast, mask=TRUE) + terra::zonal(fun='sum'). ",
    "Total land: ", format(round(total_land_km2), big.mark = ","), " km²  ",
    "(matches KG land mask baseline 147,322,862 km²). ",
    "Resample: bilinear. Zonal elapsed: ", elapsed_z, " s."
  )
)

# ---- Step 3: Metrics --------------------------------------------------------
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
message(sprintf("\nBiomass metrics — J = %.4f, H = %.4f", m$weighted_jaccard, m$hellinger_distance))

# Cross-class sampling ratios
cat("\n=== SAMPLING RATIOS (network / global) ===\n")
sr_df <- data.frame(
  bin   = 1:7,
  label = BIN_LABELS,
  p     = round(p_global * 100, 1),
  q     = round(q_net    * 100, 1),
  ratio = round(q_net / p_global, 3)
)
print(as.data.frame(sr_df), row.names = FALSE)

# Append to metrics CSV
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
  data.frame(axis = "biomass_cci_v7", aggregation_level = "7bin",
             n_classes = 7L, weighted_jaccard = m$weighted_jaccard,
             hellinger_distance = m$hellinger_distance)
)
readr::write_csv(metrics_df, metrics_out)
message("Saved: ", metrics_out)
cat("\n=== FULL METRICS TABLE ===\n")
print(as.data.frame(dplyr::mutate(metrics_df,
  J = round(weighted_jaccard, 3), H = round(hellinger_distance, 3)
)[, c("axis", "aggregation_level", "J", "H")]), row.names = FALSE)

# ---- Step 4: Figure ---------------------------------------------------------
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
bar_labels_map <- c(
  "global"  = "Global land",
  "network" = "FLUXNET\n(767 sites)"
)

plot_long <- data.frame(
  class_id = bin_factor,
  global   = p_global,
  network  = q_net,
  stringsAsFactors = FALSE
) |>
  tidyr::pivot_longer(c(global, network), names_to = "bar", values_to = "fraction") |>
  dplyr::mutate(
    bar = factor(bar, levels = c("global", "network"),
                 labels = c(bar_labels_map["global"], bar_labels_map["network"])),
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
    labels = c("1: 0–5 Mg/ha  (bare/ice)", "2: 5–25  (sparse)",
               "3: 25–50  (shrub/savanna)", "4: 50–100  (open forest)",
               "5: 100–200  (temperate)", "6: 200–400  (wet/boreal)",
               "7: >400  (tropical)"),
    name  = NULL,
    guide = guide_legend(ncol = 1, override.aes = list(colour = NA))
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
    labels = c("0.13×", "0.25×", "0.5×", "1×", "2×", "4×", "8×", "16×"),
    limits = c(0.03, 24)
  ) +
  scale_x_discrete(name = NULL,
                   labels = c("1\n(0-5)", "2\n(5-25)", "3\n(25-50)",
                              "4\n(50-100)", "5\n(100-200)", "6\n(200-400)", "7\n(>400)")) +
  base_theme +
  theme(
    panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    axis.text.x        = element_text(size = 7.5),
    axis.title.y       = element_text(size = 8),
    legend.position    = "none"
  )

fig <- (bars_panel / ratio_panel +
  plot_layout(heights = c(3.5, 1.8), guides = "keep")) &
  theme(plot.background = element_rect(fill = "white", colour = NA))

ggsave(fig_out, plot = fig, width = 7.5, height = 6.5, dpi = 200, bg = "white")
message("Saved: ", fig_out)

# ---- Step 5: Methods text ---------------------------------------------------
methods_lines <- c(
  "# Methods: Above-Ground Biomass Representativeness (ESA CCI Biomass v7.0)",
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
  "distributed as 1°x1° tiles (~300 tiles per year, ~18 GB). The pre-aggregated",
  "1km product (single global file, 1.4 GB) was used for this analysis.",
  "",
  "Resolution rationale: EC tower footprints extend 0.5-3 km depending on wind",
  "speed and stability; a single 100m pixel does not represent the flux footprint",
  "better than a 1km mean. Using the 1km product also aligns naturally with the",
  "Beck 2023 KG land mask (0.00833° ~1km), enabling direct grid alignment.",
  "The 1km file was downloaded from the CEDA aggregated/ directory via anonymous",
  "HTTP (no authentication required).",
  "",
  "## Bin definitions",
  "",
  "Seven log-spaced bins (Mg/ha):",
  "",
  "  Bin 1:  0-5    Bare/ice/desert",
  "  Bin 2:  5-25   Sparse to open vegetation",
  "  Bin 3: 25-50   Shrubland/savanna",
  "  Bin 4: 50-100  Open forest",
  "  Bin 5: 100-200 Temperate forest",
  "  Bin 6: 200-400 Wet/boreal forest",
  "  Bin 7: >400    Tropical forest",
  "",
  "Breakpoints at 25, 50, 100, 200, 400 Mg/ha follow conventions used in IPCC",
  "AR6 Working Group I (Chapter 2: Changing State of the Climate System) and in",
  "major above-ground carbon stock assessments (Spawn et al. 2020 Sci Data;",
  "Santoro et al. 2021 Earth Syst Sci Data). The 0-5 and 5-25 Mg/ha cuts are",
  "pragmatic: bin 1 captures deserts, ice sheets, and bare/unvegetated land",
  "where eddy covariance is unlikely or impossible; bin 2 captures dryland",
  "grasslands, open shrublands, and sparsely vegetated semi-arid zones. These",
  "two lower bins are judgment-based choices that are not drawn from literature",
  "conventions, and reviewers should be aware of this.",
  "",
  "## Per-site extraction",
  "",
  "Site coordinates from fluxnet_shuttle_snapshot_20260624T095651.csv (767 sites).",
  "terra::extract() with the 1km raster at each site's reported lat/lon.",
  "Sites returning NA (ocean pixels, ice sheets, or biomass product coverage",
  "gaps) were assigned to bin 1 with biomass_method = 'na_assigned_low'.",
  "This is consistent with the all-land framing: NA within the biomass product",
  "indicates no significant above-ground woody biomass.",
  sprintf("NA sites in this extraction: %d of %d.", n_na, n_sites),
  "",
  "## Global distribution and land mask",
  "",
  "Cross-axis consistency requires using the same land mask as the KG and aridity",
  "axes. The Beck 2023 KG raster (1991-2020, 0.00833° grid, 147,322,862 km^2",
  "total land) is used as the land mask.",
  "",
  "Method:",
  "  1. terra::resample(biomass, kg_rast, method='bilinear') to align grids.",
  "  2. KG-land pixels where resampled biomass is NA -> assigned to value 0",
  "     (-> bin 1) using terra::ifel().",
  "  3. terra::classify() with the 7-bin matrix.",
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
  "## Interpretive note",
  "",
  "Bins 1-2 (0-25 Mg/ha) capture deserts, ice, and sparse dryland vegetation.",
  "These biomes are intrinsically difficult to sample with eddy covariance (EC)",
  "towers due to logistical challenges and the low carbon flux signal. The",
  "representativeness signal that is most actionable for tower network design",
  "lies in bins 3-7 (>25 Mg/ha): shrublands, forests, and tropical vegetation.",
  "Under-representation in these bins points to specific forest types or",
  "geographic regions where new towers would improve global synthesis coverage."
)
writeLines(methods_lines, methods_out)
message("Saved: ", methods_out)
message("\nAll outputs complete.")
