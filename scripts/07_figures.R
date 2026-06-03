## scripts/07_figures_duckdb.R — DuckDB-backed figure generation
##
## Parallel to 07_figures.R (which operates on RDS files). This script reads
## from *_converted tables in data/duckdb/fluxnet.duckdb. One connection is
## opened at the top and shared across all figures; never open/close per figure.
##
## Includes DD-dependent figures (seasonal cycle, growing season) that were
## blocked by the 16 GB OOM on the RDS path. With DuckDB, DD data (14M rows)
## is filtered and column-selected in DuckDB SQL before collection into R.
##
## Per-figure profiling: elapsed time, memory delta (gc() MB), rows collected.
## If any figure collects >4 GB, a warning is emitted — that indicates the
## lazy-query discipline was not followed and the collect needs more filtering.
##
## Hard Rule #5: check_pipeline_config() is called near the top.
## Fail loud: stops if any *_converted table is missing.
## Do NOT adopt as 07_figures.R until test run reviewed; this script is a
## parallel probe, not a replacement.

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}

source("R/pipeline_config.R")
check_pipeline_config()

source("R/utils.R")
source("R/plot_constants.R")
source("R/figures/fig_igbp.R")
source("R/figures/fig_maps.R")
source("R/figures/fig_seasonal.R")
source("R/figures/fig_climate.R")
source("R/figures/fig_latitudinal.R")
source("R/figures/fig_growing_season.R")
source("R/figures/fig_network_growth.R")

library(dplyr)
library(duckdb)
library(ggplot2)
library(patchwork)
library(readr)
library(lubridate)
library(fs)

# ── Output directory ──────────────────────────────────────────────────────────
out_dir <- Sys.getenv("OUTPUT_DIR", unset = "figures")
fs::dir_create(out_dir)
message("Output directory: ", out_dir)

# ── Open single DuckDB connection (shared across all figures) ─────────────────
db_path <- file.path(FLUXNET_DATA_ROOT, "duckdb/fluxnet.duckdb")
if (!file.exists(db_path)) {
  stop("DuckDB database not found: ", db_path,
       "\nRun 03b_create_database.R, 04_qc.R, 05_units_duckdb.R first.")
}
con <- dbConnect(duckdb(), db_path, read_only = TRUE)

# Fail loudly if any *_converted table is absent
required_tables <- c("annual_converted", "monthly_converted",
                      "weekly_converted",  "daily_converted",
                      "hourly_converted")
cur_tables   <- dbListTables(con)
missing_tbls <- setdiff(required_tables, cur_tables)
if (length(missing_tbls) > 0L) {
  dbDisconnect(con)
  stop(
    "Required tables missing from DuckDB:\n  ",
    paste(missing_tbls, collapse = ", "),
    "\nRun 03b_create_database.R → 04_qc.R → 05_units_duckdb.R first."
  )
}
message("DuckDB connection open. Tables verified: ", paste(required_tables, collapse = ", "))

# ── Profiler ──────────────────────────────────────────────────────────────────
# Wraps each figure call and records: elapsed time, memory delta, rows collected.
# Memory delta is approximate (gc Vcells * 8 bytes + Ncells * 56 bytes → MB).
# Rows collected must be supplied by the caller via the `rows` argument since
# figure functions do not expose their internal collect counts.

.gc_mb <- function() {
  m <- gc(verbose = FALSE)
  sum(m[, "(Mb)"], na.rm = TRUE)
}

profile_log <- list()

profile_fig <- function(label, rows_collected, fn) {
  gc(verbose = FALSE)
  mem_before <- .gc_mb()
  t0         <- proc.time()["elapsed"]

  result <- tryCatch(
    fn(),
    error = function(e) {
      message("  ERROR [", label, "]: ", conditionMessage(e))
      NULL
    }
  )

  elapsed_s  <- as.numeric(proc.time()["elapsed"] - t0)
  gc(verbose = FALSE)
  mem_after  <- .gc_mb()
  mem_delta  <- mem_after - mem_before

  status <- if (is.null(result)) "FAILED" else "OK"
  cat(sprintf("  [%-40s] %s  %.1fs  delta %.0f MB  rows %s\n",
              label, status, elapsed_s, mem_delta,
              format(rows_collected, big.mark = ",")))

  if (mem_delta > 4000) {
    warning(label, ": memory delta >4 GB (", round(mem_delta), " MB) — ",
            "review pre-collect filtering.", call. = FALSE)
  }

  profile_log[[label]] <<- data.frame(
    figure       = label,
    status       = status,
    elapsed_s    = round(elapsed_s, 1),
    mem_delta_mb = round(mem_delta, 0),
    rows_collected = rows_collected,
    stringsAsFactors = FALSE
  )

  invisible(result)
}

# ── Helper: save figure ───────────────────────────────────────────────────────
save_fig <- function(p, name, width = 10, height = 8) {
  path <- file.path(out_dir, name)
  tryCatch({
    ggplot2::ggsave(path, plot = p, width = width, height = height,
                    units = "in", dpi = 150, limitsize = FALSE, bg = "white")
    message("  SAVED: ", name)
    TRUE
  }, error = function(e) {
    message("  ERROR saving ", name, ": ", conditionMessage(e))
    FALSE
  })
}

# ── Snapshot metadata (in-memory, 759 rows) ───────────────────────────────────
snap_file <- sort(list.files(
  file.path(FLUXNET_DATA_ROOT, "snapshots"),
  pattern   = "fluxnet_shuttle_snapshot_.*\\.csv$",
  full.names = TRUE
), decreasing = TRUE)[[1]]
snapshot_meta <- readr::read_csv(snap_file, show_col_types = FALSE)

meta_igbp <- snapshot_meta |>
  dplyr::distinct(site_id, .keep_all = TRUE) |>
  dplyr::select(site_id, IGBP = igbp,
                LOCATION_LAT = location_lat, LOCATION_LONG = location_long)

meta_map <- snapshot_meta |> dplyr::distinct(site_id, .keep_all = TRUE)

# ── Collect YY data once (FLUXMET rows only, all columns) ─────────────────────
# annual_converted has 40,062 rows — trivial to load.
message("\nCollecting annual_converted (FLUXMET rows)...")
data_yy <- tbl(con, "annual_converted") |>
  dplyr::filter(dataset == "FLUXMET") |>
  collect() |>
  # DuckDB uses TIMESTAMP for the integer year; add YEAR alias for figure
  # function compatibility (RDS pipeline's flux_read() renames to YEAR).
  dplyr::mutate(YEAR = as.integer(TIMESTAMP))
message("  annual_converted: ", format(nrow(data_yy), big.mark = ","), " rows collected")
YY_ROWS <- nrow(data_yy)

cat("\n=========================================\n")
cat("  07_figures_duckdb.R\n")
cat("=========================================\n\n")

report <- character(0)

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 0 — Network and choropleth figures (metadata only)
# ══════════════════════════════════════════════════════════════════════════════
message("\n── Network and choropleth figures (metadata only) ──")

p_net_growth <- profile_fig("network_growth", nrow(meta_map), function() {
  fig_network_growth(meta_map)
})
if (!is.null(p_net_growth)) {
  save_fig(p_net_growth, "network_growth.png", width = 12, height = 8)
  report <- c(report, "network_growth.png")
}

p_subregion <- profile_fig("choropleth_snapshots", nrow(meta_map), function() {
  fig_map_subregion_sites(meta_map)
})
if (!is.null(p_subregion)) {
  save_fig(p_subregion, "choropleth_snapshots.png", width = 12, height = 22)
  report <- c(report, "choropleth_snapshots.png")
}

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 1 — IGBP figures (YY)
# ══════════════════════════════════════════════════════════════════════════════
message("\n── IGBP figures ──")

p_igbp_nee <- profile_fig("igbp_nee_composite", YY_ROWS, function() {
  fig_flux_by_igbp(data_yy, flux_var = "NEE_VUT_REF", metadata = meta_igbp)
})
if (!is.null(p_igbp_nee)) {
  save_fig(p_igbp_nee$composite, "igbp_nee_composite.png", width = 10, height = 12)
  report <- c(report, "igbp_nee_composite.png")
}

p_igbp_gpp <- profile_fig("igbp_gpp_composite", YY_ROWS, function() {
  fig_flux_by_igbp(data_yy, flux_var = "GPP_NT_VUT_REF", metadata = meta_igbp)
})
if (!is.null(p_igbp_gpp)) {
  save_fig(p_igbp_gpp$composite, "igbp_gpp_composite.png", width = 10, height = 12)
  report <- c(report, "igbp_gpp_composite.png")
}

p_ts_nee <- profile_fig("igbp_nee_timeseries", YY_ROWS, function() {
  fig_flux_timeseries_by_igbp(data_yy, flux_var = "NEE_VUT_REF", metadata = meta_igbp)
})
if (!is.null(p_ts_nee)) {
  save_fig(p_ts_nee, "igbp_nee_timeseries.png", width = 14, height = 10)
  report <- c(report, "igbp_nee_timeseries.png")
}

p_ts_igbp <- profile_fig("igbp_nee_timeslice", YY_ROWS, function() {
  fig_flux_by_igbp_timeslice(data_yy, flux_var = "NEE_VUT_REF",
                              metadata = meta_igbp, bin_width = 10)
})
if (!is.null(p_ts_igbp)) {
  save_fig(p_ts_igbp$flux_plot, "igbp_nee_timeslice.png", width = 12, height = 7)
  report <- c(report, "igbp_nee_timeslice.png")
}

p_biome <- profile_fig("biome_nee_groups", YY_ROWS, function() {
  fig_flux_by_biome_group(data_yy, flux_var = "NEE_VUT_REF", metadata = meta_igbp)
})
if (!is.null(p_biome)) {
  for (grp in names(p_biome)) {
    fname <- paste0("biome_nee_", tolower(grp), ".png")
    save_fig(p_biome[[grp]], fname, width = 12, height = 8)
    report <- c(report, fname)
  }
}

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 2 — Map figures (YY + metadata)
# ══════════════════════════════════════════════════════════════════════════════
message("\n── Map figures ──")

p_hub <- profile_fig("map_global_hub", nrow(meta_map), function() {
  fig_map_global(meta_map, color_by = "data_hub")
})
if (!is.null(p_hub)) {
  save_fig(p_hub, "map_global_hub.png", width = 12, height = 7)
  report <- c(report, "map_global_hub.png")
}

p_igbp_map <- profile_fig("map_global_igbp", nrow(meta_map), function() {
  fig_map_global(meta_map, color_by = "igbp")
})
if (!is.null(p_igbp_map)) {
  save_fig(p_igbp_map, "map_global_igbp.png", width = 12, height = 7)
  report <- c(report, "map_global_igbp.png")
}

p_nee_map <- profile_fig("map_nee_mean", YY_ROWS, function() {
  fig_map_nee_mean(data_yy, meta_map, min_years = 5L)
})
if (!is.null(p_nee_map)) {
  save_fig(p_nee_map, "map_nee_mean.png", width = 12, height = 7)
  report <- c(report, "map_nee_mean.png")
}

p_delta_map <- profile_fig("map_nee_delta", YY_ROWS, function() {
  fig_map_nee_delta(data_yy, meta_map, recent_years = 2020:2024, min_years = 5L)
})
if (!is.null(p_delta_map)) {
  save_fig(p_delta_map, "map_nee_delta.png", width = 12, height = 7)
  report <- c(report, "map_nee_delta.png")
}

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 3 — Climate scatter (YY)
# ══════════════════════════════════════════════════════════════════════════════
message("\n── Climate figures ──")

p_climate <- profile_fig("climate_scatter", YY_ROWS, function() {
  fig_climate_scatter(data_yy, metadata = meta_map)
})
if (!is.null(p_climate)) {
  save_fig(p_climate$precip_vs_flux, "climate_precip_vs_nee.png", width = 9, height = 7)
  save_fig(p_climate$temp_vs_flux,   "climate_temp_vs_gpp.png",   width = 9, height = 7)
  report <- c(report, "climate_precip_vs_nee.png", "climate_temp_vs_gpp.png")
}

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 4 — Latitudinal gradient (YY)
# ══════════════════════════════════════════════════════════════════════════════
message("\n── Latitudinal figures ──")

p_lat_nee <- profile_fig("latitudinal_nee", YY_ROWS, function() {
  fig_latitudinal_flux(data_yy, metadata = meta_map, flux_var = "NEE_VUT_REF")
})
if (!is.null(p_lat_nee)) {
  save_fig(p_lat_nee, "latitudinal_nee.png", width = 8, height = 10)
  report <- c(report, "latitudinal_nee.png")
}

p_lat_gpp <- profile_fig("latitudinal_gpp", YY_ROWS, function() {
  fig_latitudinal_flux(data_yy, metadata = meta_map, flux_var = "GPP_NT_VUT_REF")
})
if (!is.null(p_lat_gpp)) {
  save_fig(p_lat_gpp, "latitudinal_gpp.png", width = 8, height = 10)
  report <- c(report, "latitudinal_gpp.png")
}

p_lat_multi <- profile_fig("latitudinal_nee_le_h", YY_ROWS, function() {
  fig_latitudinal_multi(data_yy, metadata = meta_map)
})
if (!is.null(p_lat_multi)) {
  save_fig(p_lat_multi, "latitudinal_nee_le_h.png", width = 8, height = 18)
  report <- c(report, "latitudinal_nee_le_h.png")
}

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 5 — Seasonal cycle figures (DD)
# Filter + select in DuckDB; join IGBP via copy=TRUE; collect minimal columns.
# daily_converted has 14,440,004 rows × 401 cols. We collect 14M × 5 cols.
# ══════════════════════════════════════════════════════════════════════════════
message("\n── Seasonal figures (DD) — collecting from DuckDB ──")

meta_igbp_mini <- meta_igbp |> dplyr::select(site_id, igbp = IGBP)

message("  Collecting daily_converted (FLUXMET, seasonal columns)...")
# DOY is not stored; compute from TIMESTAMP (DuckDB DATE) after collect.
# Collecting TIMESTAMP, NEE_VUT_REF, GPP_NT_VUT_REF avoids pulling 401 columns.
dd_seasonal <- tbl(con, "daily_converted") |>
  dplyr::filter(dataset == "FLUXMET") |>
  dplyr::select(site_id, TIMESTAMP, NEE_VUT_REF, GPP_NT_VUT_REF) |>
  dplyr::left_join(meta_igbp_mini, by = "site_id", copy = TRUE) |>
  dplyr::rename(IGBP = igbp) |>
  collect() |>
  dplyr::mutate(DOY = lubridate::yday(TIMESTAMP))
DD_SEASONAL_ROWS <- nrow(dd_seasonal)
message("  Collected: ", format(DD_SEASONAL_ROWS, big.mark = ","), " rows  ",
        format(object.size(dd_seasonal), units = "Mb"))

p_sea_gpp <- profile_fig("seasonal_gpp", DD_SEASONAL_ROWS, function() {
  fig_seasonal_cycle(dd_seasonal, flux_var = "GPP_NT_VUT_REF")
})
if (!is.null(p_sea_gpp)) {
  for (grp in names(p_sea_gpp)) {
    fname <- paste0("seasonal_gpp_", tolower(grp), ".png")
    save_fig(p_sea_gpp[[grp]], fname, width = 12, height = 7)
    report <- c(report, fname)
  }
}

p_sea_nee <- profile_fig("seasonal_nee", DD_SEASONAL_ROWS, function() {
  fig_seasonal_cycle(dd_seasonal, flux_var = "NEE_VUT_REF")
})
if (!is.null(p_sea_nee)) {
  for (grp in names(p_sea_nee)) {
    fname <- paste0("seasonal_nee_", tolower(grp), ".png")
    save_fig(p_sea_nee[[grp]], fname, width = 12, height = 7)
    report <- c(report, fname)
  }
}

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 6 — Seasonal weekly (DD)
# Reuses dd_seasonal already in memory.
# ══════════════════════════════════════════════════════════════════════════════
p_weekly <- profile_fig("seasonal_weekly_gpp", DD_SEASONAL_ROWS, function() {
  fig_seasonal_weekly(dd_seasonal, flux_var = "GPP_NT_VUT_REF")
})
if (!is.null(p_weekly)) {
  save_fig(p_weekly, "seasonal_weekly_gpp.png", width = 12, height = 7)
  report <- c(report, "seasonal_weekly_gpp.png")
}

# Free dd_seasonal before the growing season collect (different columns needed)
rm(dd_seasonal); gc(verbose = FALSE)

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 7 — Growing season NEE (YY + DD)
# fig_growing_season_nee() uses lubridate::ymd() on TIMESTAMP internally, so
# the full daily frame (site_id, TIMESTAMP, NEE_VUT_REF, IGBP) must be
# collected. 4-column collect of 14M rows ≈ 350-400 MB.
# ══════════════════════════════════════════════════════════════════════════════
message("\n── Growing season figure (DD) — collecting from DuckDB ──")

message("  Collecting daily_converted (FLUXMET, growing-season columns)...")
# TIMESTAMP is a DuckDB DATE; fig_growing_season_nee() internally calls
# .parse_date_gs(TIMESTAMP) = lubridate::ymd(as.character(TIMESTAMP)).
# An R Date collected from DuckDB satisfies this (as.character(date) = "YYYY-MM-DD").
dd_gs <- tbl(con, "daily_converted") |>
  dplyr::filter(dataset == "FLUXMET", !is.na(NEE_VUT_REF)) |>
  dplyr::select(site_id, TIMESTAMP, NEE_VUT_REF) |>
  dplyr::left_join(meta_igbp_mini, by = "site_id", copy = TRUE) |>
  dplyr::rename(IGBP = igbp) |>
  collect()
DD_GS_ROWS <- nrow(dd_gs)
message("  Collected: ", format(DD_GS_ROWS, big.mark = ","), " rows  ",
        format(object.size(dd_gs), units = "Mb"))

p_gs <- profile_fig("growing_season_nee", DD_GS_ROWS, function() {
  fig_growing_season_nee(
    data_yy   = data_yy,
    data_dd   = dd_gs,
    min_years = 5L,
    metadata  = meta_igbp
  )
})
if (!is.null(p_gs)) {
  save_fig(p_gs$count, "growing_season_count.png", width = 9, height = 7)
  save_fig(p_gs$span,  "growing_season_span.png",  width = 9, height = 7)
  report <- c(report, "growing_season_count.png", "growing_season_span.png")
}

rm(dd_gs); gc(verbose = FALSE)

# ── Close connection ──────────────────────────────────────────────────────────
dbDisconnect(con)
message("\nDuckDB connection closed.")

# ══════════════════════════════════════════════════════════════════════════════
# SUMMARY
# ══════════════════════════════════════════════════════════════════════════════
profile_df <- do.call(rbind, profile_log)
rownames(profile_df) <- NULL

log_path <- file.path("logs", paste0("07_figures_duckdb_profile_",
                       format(Sys.time(), "%Y%m%dT%H%M%S"), ".csv"))
readr::write_csv(profile_df, log_path)

cat("\n══════════════════════════════════════════════\n")
cat("07_figures_duckdb.R — COMPLETE\n")
cat("Output dir: ", out_dir, "\n")
cat("Files written:", length(report), "\n")
cat("Profile log: ", log_path, "\n")
cat("──────────────────────────────────────────────\n")
print(profile_df[, c("figure", "status", "elapsed_s", "mem_delta_mb", "rows_collected")])
cat("══════════════════════════════════════════════\n")

# Session info
if (!dir.exists("outputs")) dir.create("outputs", recursive = TRUE)
writeLines(capture.output(sessionInfo()),
           file.path("outputs", "session_info.txt"))
