## scripts/generate_duration_histograms.R
## Generates all 10 canonical deployment duration histogram figures.
## Run from repo root: Rscript scripts/generate_duration_histograms.R
##
## Outputs (review/figures/network/):
##   fig_dur01_ShuttleFull.png         — FLUXNET Shuttle 2025 (full network)
##   fig_dur02_Marconi.png             — Marconi 2000
##   fig_dur03_LaThuile.png            — La Thuile 2007
##   fig_dur04_FLUXNET2015.png         — FLUXNET2015
##   fig_dur05_ShuttleSnapshot2000.png — Shuttle snapshot 2000
##   fig_dur06_ShuttleSnapshot2007.png — Shuttle snapshot 2007
##   fig_dur07_ShuttleSnapshot2015.png — Shuttle snapshot 2015
##   fig_dur08_HistoricalOverlay.png   — Shuttle snapshots vs historical datasets (3-panel overlay)
##   fig_dur09_SiteYearsByYear.png     — Site-years per calendar year, Shuttle vs historical
##   fig_dur10_SiteYearsByYear_IGBP.png — Site-years per calendar year, Shuttle coloured by IGBP
##
## NOTE on data sources: Dur02-04, Dur08-10 use non-Shuttle historical site
## lists for development/comparison purposes only — labelled per CLAUDE.md §1.
##
## Architecture mirrors scripts/generate_whittaker.R:
##   - Shared xlim/ylim for Dur01–07 computed once from all 7 datasets
##   - Single core function fig_duration_historical() for Dur01–07
##   - Dur08 overlay assembled via fig_duration_overlay()
##   - Dur09 time series assembled via fig_siteyears_by_year()
##   Both in R/figures/fig_network_growth.R

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
source("R/utils.R")
source("R/plot_constants.R")
source("R/figures/fig_network_growth.R")

library(dplyr)
library(readr)
library(patchwork)
library(ggplot2)

check_pipeline_config()

out_dir <- file.path("review", "figures", "network")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ---- Load data ---------------------------------------------------------------

snap_file <- sort(
  list.files(file.path(FLUXNET_DATA_ROOT, "snapshots"),
             pattern = "fluxnet_shuttle_snapshot.*\\.csv$",
             full.names = TRUE),
  decreasing = TRUE
)[[1]]
message("Using snapshot: ", snap_file)
shuttle_meta <- readr::read_csv(snap_file, show_col_types = FALSE)

# NOTE: Historical site lists are comparison-only — non-Shuttle data (see CLAUDE.md §1).
# first_year/last_year sourced from separate year lookup tables in data/snapshots/,
# built from data/lists/ source files (see scripts/build_year_lookup_tables.R).

# Marconi 2000 — first_year = first year contributed to Marconi synthesis (proxy for
# establishment; conservative — may underestimate by 0-2 years)
sites_marconi <- readr::read_csv("data/snapshots/sites_marconi_clean.csv",
                                  show_col_types = FALSE) |>
  dplyr::left_join(readr::read_csv("data/snapshots/years_marconi.csv",
                                    show_col_types = FALSE),
                   by = "site_id")

# La Thuile 2007 — first_year = actual site establishment year from metadata
# (most accurate of the three historical sources)
sites_la_thuile <- readr::read_csv("data/snapshots/sites_la_thuile_clean.csv",
                                    show_col_types = FALSE) |>
  dplyr::left_join(readr::read_csv("data/snapshots/years_la_thuile.csv",
                                    show_col_types = FALSE),
                   by = "site_id")

# FLUXNET2015 — first_year = first year in release year matrix (conservative —
# may underestimate by 0-2 years due to matrix starting at 1991)
sites_fluxnet2015 <- readr::read_csv("data/snapshots/sites_fluxnet2015_clean.csv",
                                      show_col_types = FALSE) |>
  dplyr::left_join(readr::read_csv("data/snapshots/years_fluxnet2015.csv",
                                    show_col_types = FALSE),
                   by = "site_id")

# Shuttle site-year presence table — used by Shuttle figures (is_shuttle = TRUE)
# to count observed valid site-years rather than estimating from first/last year.
presence_df <- readr::read_csv(
  file.path(FLUXNET_DATA_ROOT, "snapshots", "site_year_data_presence.csv"),
  show_col_types = FALSE
) |>
  dplyr::mutate(year = as.integer(.data$year),
                has_data = as.logical(.data$has_data))
message("Loaded presence_df: ", nrow(presence_df), " rows")

# ---- Compute shared axis limits ----------------------------------------------
# xlim: 0 to max record length across all 7 datasets at their respective
#       snapshot years. Shuttle 2025 drives the upper bound.
# ylim: 0 to max bin count across all 7 datasets (Dur01–07) so individual
#       panels share identical axes regardless of dataset size.

message("\nComputing shared axis limits...")

shuttle_sites <- shuttle_meta |>
  dplyr::distinct(.data$site_id, .keep_all = TRUE) |>
  dplyr::filter(!is.na(.data$first_year)) |>
  dplyr::mutate(first_year = as.integer(.data$first_year),
                last_year  = as.integer(.data$last_year))

# Max record length: Shuttle 2025 panel has the longest possible records
max_record_length <- max(
  2025L - shuttle_sites$first_year[shuttle_sites$first_year <= 2025L],
  na.rm = TRUE
)

# Max bin count: maximum across all 7 datasets (4 Shuttle snapshots +
# Marconi/La Thuile/FLUXNET2015) so every panel shares the same y axis.
.max_bin_count <- function(first_years, snapshot_yr,
                           bin_width = DUR_STYLE$bin_width) {
  fy      <- as.integer(first_years)
  records <- snapshot_yr - fy[!is.na(fy) & fy <= snapshot_yr]
  if (length(records) == 0L) return(0L)
  max_r  <- max(records) + bin_width
  breaks <- seq(0, max_r, by = bin_width)
  counts <- as.integer(table(cut(records, breaks = breaks, right = FALSE)))
  max(counts)
}

max_bin_count <- max(
  # Shuttle snapshot years (Dur01, Dur05, Dur06, Dur07)
  .max_bin_count(shuttle_sites$first_year, 2025L),
  .max_bin_count(shuttle_sites$first_year, 2015L),
  .max_bin_count(shuttle_sites$first_year, 2007L),
  .max_bin_count(shuttle_sites$first_year, 2000L),
  # Historical datasets (Dur02, Dur03, Dur04)
  .max_bin_count(sites_marconi$first_year,     2000L),
  .max_bin_count(sites_la_thuile$first_year,   2007L),
  .max_bin_count(sites_fluxnet2015$first_year, 2015L)
)

# Apply computed limits to a copy of DUR_STYLE — passed to every call
style <- DUR_STYLE
style$xlim <- c(0, max_record_length + DUR_STYLE$bin_width)
style$ylim <- c(0, ceiling(max_bin_count * 1.08))

message("Shared xlim: [", style$xlim[1], ", ", style$xlim[2], "] years")
message("Shared ylim: [", style$ylim[1], ", ", style$ylim[2], "] sites")

# ---- Helper: save a single duration histogram panel --------------------------
save_dur <- function(p, name, s = style) {
  path <- file.path(out_dir, paste0(name, ".png"))
  ggplot2::ggsave(path, plot = p,
                  width  = s$width_in,
                  height = s$height_in,
                  units  = "in", dpi = 150, bg = "white")
  message("Saved: ", path)
  invisible(path)
}

# ============================================================
# Dur01 — FLUXNET Shuttle 2025 (full network)
# ============================================================
message("\n── Dur01: FLUXNET Shuttle 2025 ──")
dur01 <- fig_duration_historical(
  site_meta     = shuttle_meta,
  snapshot_year = 2025L,
  detail_label  = "FLUXNET Shuttle 2025",
  is_shuttle    = TRUE,
  presence_df   = presence_df,
  style         = style
)
save_dur(dur01, "fig_dur01_ShuttleFull")

# ============================================================
# Dur02 — Marconi 2000
# NOTE: comparison figure only — non-Shuttle data (see CLAUDE.md §1)
# ============================================================
message("\n── Dur02: Marconi 2000 ──")
dur02 <- fig_duration_historical(
  site_meta     = sites_marconi,
  snapshot_year = 2000L,
  detail_label  = "Marconi 2000",
  style         = style
)
save_dur(dur02, "fig_dur02_Marconi")

# ============================================================
# Dur03 — La Thuile 2007
# NOTE: comparison figure only — non-Shuttle data (see CLAUDE.md §1)
# ============================================================
message("\n── Dur03: La Thuile 2007 ──")
dur03 <- fig_duration_historical(
  site_meta     = sites_la_thuile,
  snapshot_year = 2007L,
  detail_label  = "La Thuile 2007",
  style         = style
)
save_dur(dur03, "fig_dur03_LaThuile")

# ============================================================
# Dur04 — FLUXNET2015
# NOTE: comparison figure only — non-Shuttle data (see CLAUDE.md §1)
# ============================================================
message("\n── Dur04: FLUXNET2015 ──")
dur04 <- fig_duration_historical(
  site_meta     = sites_fluxnet2015,
  snapshot_year = 2015L,
  detail_label  = "FLUXNET2015",
  style         = style
)
save_dur(dur04, "fig_dur04_FLUXNET2015")

# ============================================================
# Dur05 — Shuttle snapshot 2000
# ============================================================
message("\n── Dur05: Shuttle snapshot 2000 ──")
shuttle_2000 <- dplyr::filter(shuttle_meta,
                               as.integer(.data$first_year) <= 2000L)
dur05 <- fig_duration_historical(
  site_meta     = shuttle_2000,
  snapshot_year = 2000L,
  detail_label  = "Shuttle snapshot 2000",
  is_shuttle    = TRUE,
  presence_df   = presence_df,
  style         = style
)
save_dur(dur05, "fig_dur05_ShuttleSnapshot2000")

# ============================================================
# Dur06 — Shuttle snapshot 2007
# ============================================================
message("\n── Dur06: Shuttle snapshot 2007 ──")
shuttle_2007 <- dplyr::filter(shuttle_meta,
                               as.integer(.data$first_year) <= 2007L)
dur06 <- fig_duration_historical(
  site_meta     = shuttle_2007,
  snapshot_year = 2007L,
  detail_label  = "Shuttle snapshot 2007",
  is_shuttle    = TRUE,
  presence_df   = presence_df,
  style         = style
)
save_dur(dur06, "fig_dur06_ShuttleSnapshot2007")

# ============================================================
# Dur07 — Shuttle snapshot 2015
# ============================================================
message("\n── Dur07: Shuttle snapshot 2015 ──")
shuttle_2015 <- dplyr::filter(shuttle_meta,
                               as.integer(.data$first_year) <= 2015L)
dur07 <- fig_duration_historical(
  site_meta     = shuttle_2015,
  snapshot_year = 2015L,
  detail_label  = "Shuttle snapshot 2015",
  is_shuttle    = TRUE,
  presence_df   = presence_df,
  style         = style
)
save_dur(dur07, "fig_dur07_ShuttleSnapshot2015")

# ============================================================
# Dur08 — Shuttle snapshots vs historical datasets (3-panel overlay)
# NOTE: comparison figure only — non-Shuttle data (see CLAUDE.md §1)
# ============================================================
message("\n── Dur08: historical overlay ──")
dur08 <- fig_duration_overlay(
  shuttle_meta      = shuttle_meta,
  sites_marconi     = sites_marconi,
  sites_la_thuile   = sites_la_thuile,
  sites_fluxnet2015 = sites_fluxnet2015
)
path08 <- file.path(out_dir, "fig_dur08_HistoricalOverlay.png")
ggplot2::ggsave(path08, plot = dur08,
                width = 14, height = 18, units = "in", dpi = 150, bg = "white")
message("Saved: ", path08)

# Delete legacy stack PNGs superseded by the overlay figure
for (legacy in c("fig_dur08_HistoricalStack.png",
                 "fig_dur09_ShuttleSnapshotsStack.png")) {
  f <- file.path(out_dir, legacy)
  if (file.exists(f)) {
    file.remove(f)
    message("Deleted legacy file: ", f)
  }
}

# ============================================================
# Dur09 — Site-years per calendar year (Shuttle vs historical)
# NOTE: comparison figure only — non-Shuttle data (see CLAUDE.md §1)
# ============================================================
message("\n── Dur09: site-years by calendar year ──")
dur09 <- fig_siteyears_by_year(
  presence_df       = presence_df,
  sites_marconi     = sites_marconi,
  sites_la_thuile   = sites_la_thuile,
  sites_fluxnet2015 = sites_fluxnet2015
)
path09 <- file.path(out_dir, "fig_dur09_SiteYearsByYear.png")
ggplot2::ggsave(path09, plot = dur09,
                width = 14, height = 7, units = "in", dpi = 150, bg = "white")
message("Saved: ", path09)

# ============================================================
# Dur10 — Site-years by calendar year, Shuttle coloured by IGBP
# NOTE: comparison figure only — non-Shuttle historical data (see CLAUDE.md §1)
# ============================================================
message("\n── Dur10: site-years by calendar year (IGBP) ──")
dur10 <- fig_siteyears_by_year_igbp(
  presence_df       = presence_df,
  shuttle_meta      = shuttle_meta,
  sites_marconi     = sites_marconi,
  sites_la_thuile   = sites_la_thuile,
  sites_fluxnet2015 = sites_fluxnet2015
)
path10 <- file.path(out_dir, "fig_dur10_SiteYearsByYear_IGBP.png")
ggplot2::ggsave(path10, plot = dur10,
                width = 14, height = 7, units = "in", dpi = 150, bg = "white")
message("Saved: ", path10)

message("\nDone. All 10 figures generated: Dur01-10.")
