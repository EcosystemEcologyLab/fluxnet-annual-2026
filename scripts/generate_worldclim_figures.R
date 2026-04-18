## generate_worldclim_figures.R
## Generate WorldClim environmental response and Whittaker figures.
##
## Outputs:
##   review/figures/climate/fig_environmental_response_worldclim.png
##   review/figures/climate/fig_whittaker_hexbin_worldclim.png
##   review/figures/climate/fig_whittaker_hexbin_worldclim_snapshots.png

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
source("R/plot_constants.R")
source("R/external_data.R")
source("R/figures/fig_climate.R")
source("R/figures/fig_environmental_response.R")

library(patchwork)
library(dplyr)
library(readr)
library(terra)

out_dir <- file.path("review", "figures", "climate")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ---- Load data --------------------------------------------------------------
processed_dir <- file.path(FLUXNET_DATA_ROOT, "processed")
data_yy <- readRDS(file.path(processed_dir, "flux_data_converted_yy.rds"))

snap_file <- sort(
  list.files(file.path(FLUXNET_DATA_ROOT, "snapshots"),
             pattern = "fluxnet_shuttle_snapshot.*\\.csv$",
             full.names = TRUE),
  decreasing = TRUE
)[[1]]
snapshot_meta <- readr::read_csv(snap_file, show_col_types = FALSE)

site_worldclim <- readr::read_csv(
  file.path(FLUXNET_DATA_ROOT, "snapshots", "site_worldclim.csv"),
  show_col_types = FALSE
)

site_aridity <- readr::read_csv(
  file.path(FLUXNET_DATA_ROOT, "snapshots", "site_aridity.csv"),
  show_col_types = FALSE
)

# ---- Figure 8: WorldClim environmental response ----------------------------
message("\n── Fig 8: Environmental response (WorldClim) ──")
p_resp <- fig_environmental_response_worldclim(
  data_yy        = data_yy,
  worldclim_data = site_worldclim,
  aridity_data   = site_aridity,
  metadata       = snapshot_meta
)
out_resp <- file.path(out_dir, "fig_environmental_response_worldclim.png")
# 3 columns (MAT, MAP, AI) × 3 rows (NEE, LE, H)
ggplot2::ggsave(out_resp, plot = p_resp, width = 14, height = 12,
                units = "in", dpi = 150, bg = "white")
message("Saved: ", out_resp)

# ---- Whittaker WorldClim — load raster --------------------------------------
message("\n── Loading WorldClim raster ──")
wc_rast <- load_worldclim()
message("  Layers: ", terra::nlyr(wc_rast),
        "  |  CRS: ", terra::crs(wc_rast, describe = TRUE)$code)

# ---- Whittaker WorldClim — current network (all years) ---------------------
message("\n── Whittaker WorldClim — current network ──")
p_wc <- fig_whittaker_hexbin_worldclim(
  data_yy        = data_yy,
  worldclim_data = wc_rast,
  metadata       = snapshot_meta,
  flux_var       = "NEE_VUT_REF"
)
out_wc <- file.path(out_dir, "fig_whittaker_hexbin_worldclim.png")
ggplot2::ggsave(out_wc, plot = p_wc, width = 9, height = 7,
                units = "in", dpi = 150, bg = "white")
message("Saved: ", out_wc)

# ---- Whittaker WorldClim — 3-panel snapshots --------------------------------
message("\n── Whittaker WorldClim — 3-panel snapshots ──")

# Join first_year from snapshot onto data_yy (needed for year_cutoff filtering)
data_yy_with_meta <- dplyr::left_join(
  data_yy,
  dplyr::select(snapshot_meta, site_id, first_year, location_lat, location_long),
  by = "site_id"
)

# Snapshot years correspond to major FLUXNET data releases:
# La Thuile (2007), FLUXNET2015 (2015), Shuttle/modern (2025)
cutoffs <- c(2007L, 2015L, 2025L)
panels_snap <- lapply(cutoffs, function(yr) {
  message("  year_cutoff = ", yr)
  fig_whittaker_hexbin_worldclim(
    data_yy        = data_yy_with_meta,
    worldclim_data = wc_rast,
    metadata       = snapshot_meta,
    flux_var       = "NEE_VUT_REF",
    year_cutoff    = yr
  ) + fluxnet_theme(base_size = 11)
})

pw_snap <- (patchwork::wrap_plots(panels_snap, ncol = 1) +
  patchwork::plot_layout(axes = "collect", guides = "collect")) &
  ggplot2::theme(legend.position = "bottom",
                 plot.margin     = ggplot2::margin(0, 5, 0, 5))

out_snap <- file.path(out_dir, "fig_whittaker_hexbin_worldclim_snapshots.png")
ggplot2::ggsave(out_snap, plot = pw_snap, width = 8, height = 15,
                units = "in", dpi = 150, bg = "white")
message("Saved: ", out_snap)

message("\nDone.")
