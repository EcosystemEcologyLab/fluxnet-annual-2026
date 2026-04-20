## scripts/generate_duration_histograms.R
## Generates all 9 canonical deployment duration histogram figures.
## Run from repo root: Rscript scripts/generate_duration_histograms.R
##
## Outputs (review/figures/network/):
##   fig_dur01_ShuttleFull.png           — FLUXNET Shuttle 2025 (full network)
##   fig_dur02_Marconi.png               — Marconi 2000           [BLOCKED]
##   fig_dur03_LaThuile.png              — La Thuile 2007         [BLOCKED]
##   fig_dur04_FLUXNET2015.png           — FLUXNET2015            [BLOCKED]
##   fig_dur05_ShuttleSnapshot2000.png   — Shuttle snapshot 2000
##   fig_dur06_ShuttleSnapshot2007.png   — Shuttle snapshot 2007
##   fig_dur07_ShuttleSnapshot2015.png   — Shuttle snapshot 2015
##   fig_dur08_HistoricalStack.png       — Dur02/Dur03/Dur04 stacked [BLOCKED]
##   fig_dur09_ShuttleSnapshotsStack.png — Dur05/Dur06/Dur07 stacked
##
## Blocked figures:
##   Dur02-04: historical CSVs (Marconi, La Thuile, FLUXNET2015) lack
##             first_year/last_year columns — enrich before unblocking.
##   Dur08:    depends on Dur02-04.
##
## NOTE on data sources: Dur02-04 use non-Shuttle historical site lists for
## development/comparison purposes only — clearly labelled per CLAUDE.md §1.
##
## Architecture mirrors scripts/generate_whittaker.R:
##   - Shared xlim/ylim computed once from all available datasets
##   - Single core function fig_duration_historical() for all panels
##   - Stack figures assembled via .make_dur_stack()

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

# NOTE: Historical site lists are comparison-only — non-Shuttle data.
# NOTE: These CSVs lack first_year/last_year — Dur02-04 remain BLOCKED.
# sites_marconi     <- readr::read_csv("data/snapshots/sites_marconi_clean.csv",
#                                      show_col_types = FALSE)
# sites_la_thuile   <- readr::read_csv("data/snapshots/sites_la_thuile_clean.csv",
#                                      show_col_types = FALSE)
# sites_fluxnet2015 <- readr::read_csv("data/snapshots/sites_fluxnet2015_clean.csv",
#                                      show_col_types = FALSE)

# ---- Compute shared axis limits ----------------------------------------------
# xlim: 0 to max record length across all datasets at their respective snapshot
#       years. Shuttle 2025 drives the upper bound; snapshot years give shorter
#       records. Revisit upper bound when Dur02-04 are unblocked.
# ylim: 0 to max bin count across all Shuttle snapshot years, with bin_width=3.

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

# Max bin count: maximum across all four Shuttle snapshot years (2000/2007/2015/2025)
.max_bin_count <- function(first_years, snapshot_yr,
                           bin_width = DUR_STYLE$bin_width) {
  records <- snapshot_yr - first_years[first_years <= snapshot_yr]
  if (length(records) == 0L) return(0L)
  max_r  <- max(records) + bin_width
  breaks <- seq(0, max_r, by = bin_width)
  counts <- as.integer(table(cut(records, breaks = breaks, right = FALSE)))
  max(counts)
}

max_bin_count <- max(
  .max_bin_count(shuttle_sites$first_year, 2025L),
  .max_bin_count(shuttle_sites$first_year, 2015L),
  .max_bin_count(shuttle_sites$first_year, 2007L),
  .max_bin_count(shuttle_sites$first_year, 2000L)
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

# ---- Stack helper ------------------------------------------------------------
.make_dur_stack <- function(p_top, p_mid, p_bot, s = style) {
  patchwork::wrap_plots(p_top, p_mid, p_bot, ncol = 1) +
    patchwork::plot_layout(axes = "collect") &
    ggplot2::theme(plot.margin = ggplot2::margin(0, 5, 0, 5))
}

# ============================================================
# Dur01 — FLUXNET Shuttle 2025 (full network)
# ============================================================
message("\n── Dur01: FLUXNET Shuttle 2025 ──")
dur01 <- fig_duration_historical(
  site_meta     = shuttle_meta,
  snapshot_year = 2025L,
  detail_label  = "FLUXNET Shuttle 2025",
  style         = style
)
save_dur(dur01, "fig_dur01_ShuttleFull")

# ============================================================
# Dur02 — Marconi 2000
# BLOCKED: sites_marconi_clean.csv lacks first_year/last_year.
# NOTE: comparison figure only — non-Shuttle data (see CLAUDE.md §1)
# ============================================================
# message("\n── Dur02: Marconi 2000 ──")
# dur02 <- fig_duration_historical(
#   site_meta     = sites_marconi,
#   snapshot_year = 2000L,
#   detail_label  = "Marconi 2000",
#   style         = style
# )
# save_dur(dur02, "fig_dur02_Marconi")

# ============================================================
# Dur03 — La Thuile 2007
# BLOCKED: sites_la_thuile_clean.csv lacks first_year/last_year.
# NOTE: comparison figure only — non-Shuttle data (see CLAUDE.md §1)
# ============================================================
# message("\n── Dur03: La Thuile 2007 ──")
# dur03 <- fig_duration_historical(
#   site_meta     = sites_la_thuile,
#   snapshot_year = 2007L,
#   detail_label  = "La Thuile 2007",
#   style         = style
# )
# save_dur(dur03, "fig_dur03_LaThuile")

# ============================================================
# Dur04 — FLUXNET2015
# BLOCKED: sites_fluxnet2015_clean.csv lacks first_year/last_year.
# NOTE: comparison figure only — non-Shuttle data (see CLAUDE.md §1)
# ============================================================
# message("\n── Dur04: FLUXNET2015 ──")
# dur04 <- fig_duration_historical(
#   site_meta     = sites_fluxnet2015,
#   snapshot_year = 2015L,
#   detail_label  = "FLUXNET2015",
#   style         = style
# )
# save_dur(dur04, "fig_dur04_FLUXNET2015")

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
  style         = style
)
save_dur(dur07, "fig_dur07_ShuttleSnapshot2015")

# ============================================================
# Dur08 — historical datasets stack (Dur02 / Dur03 / Dur04)
# BLOCKED: depends on Dur02-04 (historical CSVs lack first_year/last_year).
# NOTE: comparison figure only — non-Shuttle data (see CLAUDE.md §1)
# ============================================================
# message("\n── Dur08: historical datasets stack ──")
# dur08 <- .make_dur_stack(dur02, dur03, dur04)
# ggplot2::ggsave(
#   file.path(out_dir, "fig_dur08_HistoricalStack.png"),
#   plot   = dur08,
#   width  = style$width_in,
#   height = style$height_in * 3,
#   units  = "in", dpi = 150, bg = "white"
# )
# message("Saved: ", file.path(out_dir, "fig_dur08_HistoricalStack.png"))

# ============================================================
# Dur09 — Shuttle snapshots stack (Dur05 / Dur06 / Dur07)
# ============================================================
message("\n── Dur09: Shuttle snapshots stack (Dur05/06/07) ──")
dur09 <- .make_dur_stack(dur05, dur06, dur07)
path09 <- file.path(out_dir, "fig_dur09_ShuttleSnapshotsStack.png")
ggplot2::ggsave(
  path09,
  plot   = dur09,
  width  = style$width_in,
  height = style$height_in * 3,
  units  = "in", dpi = 150, bg = "white"
)
message("Saved: ", path09)

message("\nDone. Dur01, Dur05-07, Dur09 generated.",
        "\nDur02-04 and Dur08 remain BLOCKED pending first_year/last_year",
        " enrichment in historical site CSVs.")
