## 07_figures.R — Generate paper figures
## Output files go to figures/. Directory is gitignored.
## For review runs, set OUTPUT_DIR env var to redirect (e.g. review/figures/).

source("R/pipeline_config.R")
check_pipeline_config()

library(ggplot2)
library(patchwork)
library(dplyr)
library(lubridate)
library(fs)
if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}

# Source all figure modules
source("R/figures/fig_igbp.R")
source("R/figures/fig_maps.R")
source("R/figures/fig_seasonal.R")
source("R/figures/fig_climate.R")
source("R/figures/fig_latitudinal.R")
source("R/figures/fig_growing_season.R")

# Output directory — default figures/; override via OUTPUT_DIR env var
out_dir <- Sys.getenv("OUTPUT_DIR", unset = "figures")
fs::dir_create(out_dir)
message("Figures output directory: ", out_dir)

# ── Helper: save with error handling ─────────────────────────────────────────
save_fig <- function(p, name, width = 10, height = 8) {
  path <- file.path(out_dir, name)
  tryCatch({
    ggplot2::ggsave(path, plot = p, width = width, height = height,
                    units = "in", dpi = 150, limitsize = FALSE)
    message("  SAVED: ", name)
    invisible(TRUE)
  }, error = function(e) {
    message("  ERROR saving ", name, ": ", conditionMessage(e))
    invisible(FALSE)
  })
}

report <- character(0)

# ── Load data ─────────────────────────────────────────────────────────────────
processed_dir <- file.path(FLUXNET_DATA_ROOT, "processed")
message("Loading processed data...")

data_yy_raw <- readRDS(file.path(processed_dir, "flux_data_converted_yy.rds"))
data_mm_raw <- readRDS(file.path(processed_dir, "flux_data_converted_mm.rds"))
data_dd_raw <- readRDS(file.path(processed_dir, "flux_data_converted_dd.rds"))

# Snapshot for metadata
snap_file <- sort(list.files(file.path(FLUXNET_DATA_ROOT, "snapshots"),
                              pattern = "fluxnet_shuttle_snapshot_.*\\.csv$",
                              full.names = TRUE),
                  decreasing = TRUE)[[1]]
snapshot_meta <- readr::read_csv(snap_file, show_col_types = FALSE)

# ── Prepare YY data ───────────────────────────────────────────────────────────
# All YY figure functions expect column TIMESTAMP (integer year).
# flux_read() renames it to YEAR; add TIMESTAMP alias.
# Use FLUXMET dataset only for flux analyses (ERA5 has climate only).
data_yy_flux <- data_yy_raw |>
  filter(dataset == "FLUXMET") |>
  mutate(TIMESTAMP = YEAR)

data_yy_all <- data_yy_raw |>
  mutate(TIMESTAMP = YEAR)

# ── Prepare DD data ───────────────────────────────────────────────────────────
# flux_read() renames TIMESTAMP → DATE (Date object) for DD resolution.
# Add TIMESTAMP (YYYYMMDD integer) for fig_growing_season_nee().
# Add DOY for fig_seasonal_cycle().
data_dd_flux <- data_dd_raw |>
  filter(dataset == "FLUXMET") |>
  mutate(
    DOY       = lubridate::yday(DATE),
    TIMESTAMP = as.integer(format(DATE, "%Y%m%d"))
  )

# ── Metadata ──────────────────────────────────────────────────────────────────
# For IGBP-join functions: need uppercase IGBP column.
meta_igbp <- snapshot_meta |>
  select(site_id, IGBP = igbp, LOCATION_LAT = location_lat,
         LOCATION_LONG = location_long) |>
  distinct(site_id, .keep_all = TRUE)

# For map functions: lowercase igbp, plus data_hub.
meta_map <- snapshot_meta |>
  distinct(site_id, .keep_all = TRUE)

message("Data loaded: YY=", nrow(data_yy_flux), " rows (FLUXMET); ",
        "DD=", nrow(data_dd_flux), " rows (FLUXMET); ",
        "25 sites")

# ═══════════════════════════════════════════════════════════════════════════════
# SECTION 1 — IGBP figures
# ═══════════════════════════════════════════════════════════════════════════════

message("\n── IGBP figures ──")

# 1a: IGBP boxplot composite (NEE)
out_igbp_nee <- tryCatch(
  fig_flux_by_igbp(data_yy_flux, flux_var = "NEE_VUT_REF", metadata = meta_igbp),
  error = function(e) { message("  ERROR fig_flux_by_igbp (NEE): ", e$message); NULL }
)
if (!is.null(out_igbp_nee)) {
  save_fig(out_igbp_nee$composite, "igbp_nee_composite.png", width = 10, height = 12)
  report <- c(report, "igbp_nee_composite.png — IGBP boxplot composite (NEE)")
}

# 1b: IGBP boxplot composite (GPP)
out_igbp_gpp <- tryCatch(
  fig_flux_by_igbp(data_yy_flux, flux_var = "GPP_NT_VUT_REF", metadata = meta_igbp),
  error = function(e) { message("  ERROR fig_flux_by_igbp (GPP): ", e$message); NULL }
)
if (!is.null(out_igbp_gpp)) {
  save_fig(out_igbp_gpp$composite, "igbp_gpp_composite.png", width = 10, height = 12)
  report <- c(report, "igbp_gpp_composite.png — IGBP boxplot composite (GPP)")
}

# 1c: IGBP time series faceted
p_ts_nee <- tryCatch(
  fig_flux_timeseries_by_igbp(data_yy_flux, flux_var = "NEE_VUT_REF", metadata = meta_igbp),
  error = function(e) { message("  ERROR fig_flux_timeseries_by_igbp: ", e$message); NULL }
)
if (!is.null(p_ts_nee)) {
  save_fig(p_ts_nee, "igbp_nee_timeseries.png", width = 14, height = 10)
  report <- c(report, "igbp_nee_timeseries.png — IGBP-faceted NEE time series")
}

# 1d: IGBP timeslice boxplot
out_igbp_ts <- tryCatch(
  fig_flux_by_igbp_timeslice(data_yy_flux, flux_var = "NEE_VUT_REF",
                              metadata = meta_igbp, bin_width = 10),
  error = function(e) { message("  ERROR fig_flux_by_igbp_timeslice: ", e$message); NULL }
)
if (!is.null(out_igbp_ts)) {
  save_fig(out_igbp_ts$flux_plot, "igbp_nee_timeslice.png", width = 12, height = 7)
  report <- c(report, "igbp_nee_timeslice.png — IGBP × time-slice boxplot (NEE)")
}

# 1e: Biome-group boxplots
out_biome <- tryCatch(
  fig_flux_by_biome_group(data_yy_flux, flux_var = "NEE_VUT_REF", metadata = meta_igbp),
  error = function(e) { message("  ERROR fig_flux_by_biome_group: ", e$message); NULL }
)
if (!is.null(out_biome)) {
  for (grp in names(out_biome)) {
    fname <- paste0("biome_nee_", tolower(grp), ".png")
    save_fig(out_biome[[grp]], fname, width = 12, height = 8)
    report <- c(report, paste0(fname, " — biome-group NEE boxplot (", grp, ")"))
  }
}

# ═══════════════════════════════════════════════════════════════════════════════
# SECTION 2 — Map figures
# ═══════════════════════════════════════════════════════════════════════════════

message("\n── Map figures ──")

# 2a: Global site map by hub
p_map_hub <- tryCatch(
  fig_map_global(meta_map, color_by = "data_hub"),
  error = function(e) { message("  ERROR fig_map_global (hub): ", e$message); NULL }
)
if (!is.null(p_map_hub)) {
  save_fig(p_map_hub, "map_global_hub.png", width = 12, height = 7)
  report <- c(report, "map_global_hub.png — global site map coloured by hub")
}

# 2b: Global site map by IGBP
p_map_igbp <- tryCatch(
  fig_map_global(meta_map, color_by = "igbp"),
  error = function(e) { message("  ERROR fig_map_global (igbp): ", e$message); NULL }
)
if (!is.null(p_map_igbp)) {
  save_fig(p_map_igbp, "map_global_igbp.png", width = 12, height = 7)
  report <- c(report, "map_global_igbp.png — global site map coloured by IGBP")
}

# 2c: NEE mean map (min_years = 5 for test set coverage)
p_map_nee <- tryCatch(
  fig_map_nee_mean(data_yy_flux, meta_map, min_years = 5L),
  error = function(e) { message("  ERROR fig_map_nee_mean: ", e$message); NULL }
)
if (!is.null(p_map_nee)) {
  save_fig(p_map_nee, "map_nee_mean.png", width = 12, height = 7)
  report <- c(report, "map_nee_mean.png — global map of long-term mean NEE")
}

# 2d: delta NEE map
p_map_delta <- tryCatch(
  fig_map_nee_delta(data_yy_flux, meta_map, recent_years = 2020:2024, min_years = 5L),
  error = function(e) { message("  ERROR fig_map_nee_delta: ", e$message); NULL }
)
if (!is.null(p_map_delta)) {
  save_fig(p_map_delta, "map_nee_delta.png", width = 12, height = 7)
  report <- c(report, "map_nee_delta.png — global map of ΔNEE (2020-2024 vs historic)")
}

# ═══════════════════════════════════════════════════════════════════════════════
# SECTION 3 — Seasonal cycle figures
# ═══════════════════════════════════════════════════════════════════════════════

message("\n── Seasonal figures ──")

out_seasonal_gpp <- tryCatch(
  fig_seasonal_cycle(data_dd_flux, flux_var = "GPP_NT_VUT_REF", metadata = meta_igbp),
  error = function(e) { message("  ERROR fig_seasonal_cycle (GPP): ", e$message); NULL }
)
if (!is.null(out_seasonal_gpp)) {
  for (grp in names(out_seasonal_gpp)) {
    fname <- paste0("seasonal_gpp_", tolower(grp), ".png")
    save_fig(out_seasonal_gpp[[grp]], fname, width = 12, height = 7)
    report <- c(report, paste0(fname, " — DOY seasonal cycle GPP (", grp, ")"))
  }
}

out_seasonal_nee <- tryCatch(
  fig_seasonal_cycle(data_dd_flux, flux_var = "NEE_VUT_REF", metadata = meta_igbp),
  error = function(e) { message("  ERROR fig_seasonal_cycle (NEE): ", e$message); NULL }
)
if (!is.null(out_seasonal_nee)) {
  for (grp in names(out_seasonal_nee)) {
    fname <- paste0("seasonal_nee_", tolower(grp), ".png")
    save_fig(out_seasonal_nee[[grp]], fname, width = 12, height = 7)
    report <- c(report, paste0(fname, " — DOY seasonal cycle NEE (", grp, ")"))
  }
}

# ═══════════════════════════════════════════════════════════════════════════════
# SECTION 4 — Climate scatter figures
# ═══════════════════════════════════════════════════════════════════════════════

message("\n── Climate figures ──")

out_climate <- tryCatch(
  fig_climate_scatter(data_yy_flux, metadata = meta_map),
  error = function(e) { message("  ERROR fig_climate_scatter: ", e$message); NULL }
)
if (!is.null(out_climate)) {
  save_fig(out_climate$precip_vs_flux, "climate_precip_vs_nee.png", width = 9, height = 7)
  save_fig(out_climate$temp_vs_flux,   "climate_temp_vs_gpp.png",   width = 9, height = 7)
  report <- c(report,
              "climate_precip_vs_nee.png — precipitation vs NEE scatter",
              "climate_temp_vs_gpp.png — temperature vs GPP scatter")
}

# ═══════════════════════════════════════════════════════════════════════════════
# SECTION 5 — Latitudinal gradient figures
# ═══════════════════════════════════════════════════════════════════════════════

message("\n── Latitudinal figures ──")

p_lat_nee <- tryCatch(
  fig_latitudinal_flux(data_yy_flux, metadata = meta_map,
                       flux_var = "NEE_VUT_REF"),
  error = function(e) { message("  ERROR fig_latitudinal_flux (NEE): ", e$message); NULL }
)
if (!is.null(p_lat_nee)) {
  save_fig(p_lat_nee, "latitudinal_nee.png", width = 8, height = 10)
  report <- c(report, "latitudinal_nee.png — latitudinal gradient of NEE")
}

p_lat_gpp <- tryCatch(
  fig_latitudinal_flux(data_yy_flux, metadata = meta_map,
                       flux_var = "GPP_NT_VUT_REF"),
  error = function(e) { message("  ERROR fig_latitudinal_flux (GPP): ", e$message); NULL }
)
if (!is.null(p_lat_gpp)) {
  save_fig(p_lat_gpp, "latitudinal_gpp.png", width = 8, height = 10)
  report <- c(report, "latitudinal_gpp.png — latitudinal gradient of GPP")
}

# ═══════════════════════════════════════════════════════════════════════════════
# SECTION 6 — Growing season figures
# ═══════════════════════════════════════════════════════════════════════════════

message("\n── Growing season figures ──")

out_gs <- tryCatch(
  fig_growing_season_nee(
    data_yy  = data_yy_flux,
    data_dd  = data_dd_flux,
    min_years = 5L,
    metadata  = meta_map
  ),
  error = function(e) { message("  ERROR fig_growing_season_nee: ", e$message); NULL }
)
if (!is.null(out_gs)) {
  save_fig(out_gs$count, "growing_season_count.png", width = 9, height = 7)
  save_fig(out_gs$span,  "growing_season_span.png",  width = 9, height = 7)
  report <- c(report,
              "growing_season_count.png — uptake days vs annual NEE",
              "growing_season_span.png — first-to-last uptake span vs annual NEE")
}

# ═══════════════════════════════════════════════════════════════════════════════
# REPORT
# ═══════════════════════════════════════════════════════════════════════════════

all_files <- list.files(out_dir, pattern = "\\.png$")
cat("\n══════════════════════════════════════════════\n")
cat("07_figures.R — COMPLETE\n")
cat("Output dir: ", out_dir, "\n")
cat("Files written:", length(all_files), "\n")
cat("──────────────────────────────────────────────\n")
for (r in report) cat("  ✓", r, "\n")
cat("══════════════════════════════════════════════\n")
