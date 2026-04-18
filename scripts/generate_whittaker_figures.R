## generate_whittaker_figures.R
## Regenerate all four Whittaker hexbin review PNGs with:
##   - symmetric diverging colour scale (5th-95th pct, squished)
##   - markdown axis labels (via fluxnet_theme)
##   - vertical-stack 4-panel snapshots with inset year labels
##
## Outputs (review/figures/climate/):
##   fig_whittaker_hexbin_era5.png
##   fig_whittaker_hexbin_era5_snapshots.png
##   fig_whittaker_hexbin_worldclim.png
##   fig_whittaker_hexbin_worldclim_snapshots.png

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
source("R/plot_constants.R")
source("R/external_data.R")
source("R/figures/fig_climate.R")

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

# Join first_year and coordinates from snapshot — needed for year_cutoff
# filtering in both ERA5 and WorldClim functions.
data_yy_full <- dplyr::left_join(
  data_yy,
  dplyr::select(snapshot_meta, site_id, first_year,
                location_lat, location_long),
  by = "site_id"
)

# ---- Load WorldClim raster --------------------------------------------------
message("Loading WorldClim raster ...")
wc_rast <- load_worldclim()

cutoffs <- c(2010L, 2015L, 2020L, 2025L)

# ============================================================
# 1. ERA5 — single panel (all sites, all years)
# ============================================================
message("\n── ERA5 single panel ──")
p_era5 <- fig_whittaker_hexbin_era5(
  data_yy  = data_yy_full,
  flux_var = "NEE_VUT_REF"
)
out_era5 <- file.path(out_dir, "fig_whittaker_hexbin_era5.png")
ggplot2::ggsave(out_era5, plot = p_era5, width = 9, height = 7,
                units = "in", dpi = 150, bg = "white")
message("Saved: ", out_era5)

# ============================================================
# 2. ERA5 — 4-panel vertical stack (2010 / 2015 / 2020 / 2025)
# ============================================================
message("\n── ERA5 4-panel snapshots ──")
panels_era5 <- lapply(cutoffs, function(yr) {
  message("  year_cutoff = ", yr)
  fig_whittaker_hexbin_era5(data_yy_full, flux_var = "NEE_VUT_REF",
                            year_cutoff = yr)
})

pw_era5_snap <-
  (patchwork::wrap_plots(panels_era5, ncol = 1) +
     patchwork::plot_layout(axes = "collect", guides = "collect")) &
  ggplot2::theme(
    legend.position = "bottom",
    plot.margin     = ggplot2::margin(0, 5, 0, 5)
  )

out_era5_snap <- file.path(out_dir, "fig_whittaker_hexbin_era5_snapshots.png")
ggplot2::ggsave(out_era5_snap, plot = pw_era5_snap,
                width = 8, height = 20,
                units = "in", dpi = 150, bg = "white")
message("Saved: ", out_era5_snap)

# ============================================================
# 3. WorldClim — single panel (all sites, all years)
# ============================================================
message("\n── WorldClim single panel ──")
p_wc <- fig_whittaker_hexbin_worldclim(
  data_yy        = data_yy_full,
  worldclim_data = wc_rast,
  metadata       = snapshot_meta,
  flux_var       = "NEE_VUT_REF"
)
out_wc <- file.path(out_dir, "fig_whittaker_hexbin_worldclim.png")
ggplot2::ggsave(out_wc, plot = p_wc, width = 9, height = 7,
                units = "in", dpi = 150, bg = "white")
message("Saved: ", out_wc)

# ============================================================
# 4. WorldClim — 4-panel vertical stack (2010 / 2015 / 2020 / 2025)
# ============================================================
message("\n── WorldClim 4-panel snapshots ──")
panels_wc <- lapply(cutoffs, function(yr) {
  message("  year_cutoff = ", yr)
  fig_whittaker_hexbin_worldclim(
    data_yy        = data_yy_full,
    worldclim_data = wc_rast,
    metadata       = snapshot_meta,
    flux_var       = "NEE_VUT_REF",
    year_cutoff    = yr
  )
})

pw_wc_snap <-
  (patchwork::wrap_plots(panels_wc, ncol = 1) +
     patchwork::plot_layout(axes = "collect", guides = "collect")) &
  ggplot2::theme(
    legend.position = "bottom",
    plot.margin     = ggplot2::margin(0, 5, 0, 5)
  )

out_wc_snap <- file.path(out_dir,
                          "fig_whittaker_hexbin_worldclim_snapshots.png")
ggplot2::ggsave(out_wc_snap, plot = pw_wc_snap,
                width = 8, height = 20,
                units = "in", dpi = 150, bg = "white")
message("Saved: ", out_wc_snap)

message("\nAll Whittaker figures done.")
