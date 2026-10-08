## generate_fig02_historical_only.R
## Presentation "reveal" companion to Figure 2 (2026-10-06): identical in
## every way to fig_02_cumulative_siteyears_igbp.png/.pdf EXCEPT that the
## FLUXNET Shuttle (current-network) IGBP-stacked area is omitted entirely --
## only the Marconi/La Thuile/FLUXNET2015 historical lines are drawn. The x
## and y axes (range and breaks) are identical to Figure 2: the Shuttle data
## still drives the y-axis range even though it is not drawn, via
## fig_cumulative_siteyears_igbp(show_current_network = FALSE)'s invisible
## layer -- see that function's own doc in R/figures/fig_network_growth.R.
## Intended for a talk: show this slide first, then advance to the real
## Figure 2 on the same axes to reveal the current network's growth.
##
## NOT a numbered Supplementary Information figure for journal submission --
## moved out of SupFigs/ into review/figures/presentation_figures/, a
## talk/presentation companion pool beside draft_manuscript_v1/ (supplementary
## material restructure, 2026-10-08, SESSION_LOG.md; previously lived in
## SupFigs/ under a name that deliberately did not claim an S-number).
## check_figure_format.R still validates it (same Extended Data size/font/
## text/line/edge rules), run explicitly against this new directory.
##
## Output: review/figures/presentation_figures/fig_02_historical_only.png/.pdf/.jpg/.legend.txt

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
source("R/utils.R")
source("R/plot_constants.R")
source("R/figures/fig_network_growth.R")
source("R/nature_format.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(ggplot2); library(readxl); library(tidyr)
})

msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)
msg("=== Figure: cumulative site-years, historical collections only (Figure 2 reveal companion) ===")

OUT_DIR  <- file.path("review", "figures", "presentation_figures")
OUT_STEM <- file.path(OUT_DIR, "fig_02_historical_only")
fs::dir_create(OUT_DIR)

## ---- Same pinned inputs as scripts/generate_fig_cumulative_siteyears.R -----
snap_file <- file.path(FLUXNET_DATA_ROOT, "snapshots", "fluxnet_shuttle_snapshot_20260901T094522.csv")
if (!file.exists(snap_file)) stop("Pinned snapshot not found: ", snap_file, call. = FALSE)
msg("Using snapshot (pinned): ", snap_file)
shuttle_meta <- read_csv(snap_file, show_col_types = FALSE)

sites_marconi     <- read_csv("data/snapshots/sites_marconi_clean.csv", show_col_types = FALSE) |>
  left_join(read_csv("data/snapshots/years_marconi.csv", show_col_types = FALSE), by = "site_id")
sites_la_thuile   <- read_csv("data/snapshots/sites_la_thuile_clean.csv", show_col_types = FALSE) |>
  left_join(read_csv("data/snapshots/years_la_thuile.csv", show_col_types = FALSE), by = "site_id")
sites_fluxnet2015 <- read_csv("data/snapshots/sites_fluxnet2015_clean.csv", show_col_types = FALSE) |>
  left_join(read_csv("data/snapshots/years_fluxnet2015.csv", show_col_types = FALSE), by = "site_id")

presence_df <- read_csv(file.path(FLUXNET_DATA_ROOT, "snapshots", "site_year_data_presence.csv"),
                         show_col_types = FALSE) |>
  mutate(year = as.integer(.data$year), has_data = as.logical(.data$has_data))
msg("Loaded presence_df: ", nrow(presence_df), " rows")

la_thuile_xlsx       <- read_excel("data/lists/LaThuileList.xlsx")
la_thuile_yr_cols    <- names(la_thuile_xlsx)[grepl("^[0-9]{4}$", names(la_thuile_xlsx))]
la_thuile_year_matrix <- la_thuile_xlsx |>
  dplyr::select(site_id = "SITE", dplyr::all_of(la_thuile_yr_cols)) |>
  tidyr::pivot_longer(cols = dplyr::all_of(la_thuile_yr_cols), names_to = "year", values_to = "flag") |>
  dplyr::mutate(year = as.integer(.data$year), present = !is.na(.data$flag) & .data$flag == 1)
msg("Loaded La Thuile year-indicator matrix: ",
    sum(la_thuile_year_matrix$present), " site-years (expect 965)")

year_window <- data_year_window(presence_df)
msg("Data-driven year window (identical to Figure 2): ", year_window$first_year, "-",
    year_window$last_year)

## ---- Build panel: show_current_network = FALSE -----------------------------
## Same call as scripts/generate_fig_cumulative_siteyears.R, same base_size,
## same theme additions, same right-margin fix -- the ONLY difference is
## show_current_network = FALSE.
panel <- fig_cumulative_siteyears_igbp(
  presence_df            = presence_df,
  shuttle_meta           = shuttle_meta,
  sites_marconi          = sites_marconi,
  sites_la_thuile        = sites_la_thuile,
  sites_fluxnet2015      = sites_fluxnet2015,
  base_size              = 9L,
  la_thuile_year_matrix  = la_thuile_year_matrix,
  show_current_network   = FALSE
) +
  ggplot2::theme(legend.key.size = grid::unit(7, "pt")) +
  nature_theme() +
  # Same right-margin fix as Figure 2 (SESSION_LOG.md 2026-10-05) -- the last
  # x-axis tick label is identical on both figures, so it needs the same fix.
  ggplot2::theme(plot.margin = ggplot2::margin(t = 5.5, r = 14, b = 5.5, l = 5.5, unit = "pt"))

saved <- save_nature_figure(panel, OUT_STEM, width_mm = NATURE_WIDTH_SINGLE_MM,
                             height_mm = NATURE_WIDTH_SINGLE_MM, extended_data = TRUE)
msg("Saved: ", saved$png, ", ", saved$pdf, ", ", saved$jpeg)

## ---- Historical line end points (reported in the legend, same as Figure 2) -
marconi_end     <- sites_marconi |>
  dplyr::distinct(site_id, .keep_all = TRUE) |>
  dplyr::filter(!is.na(first_year), !is.na(last_year)) |>
  dplyr::summarise(n = sum(last_year - first_year + 1L)) |> dplyr::pull(n)
la_thuile_end   <- sum(la_thuile_year_matrix$present)
fluxnet2015_end <- sites_fluxnet2015 |>
  dplyr::distinct(site_id, .keep_all = TRUE) |>
  dplyr::filter(!is.na(first_year), !is.na(last_year)) |>
  dplyr::summarise(n = sum(last_year - first_year + 1L)) |> dplyr::pull(n)
msg("Historical line end points -- Marconi: ", marconi_end,
    ", La Thuile: ", la_thuile_end, ", FLUXNET2015: ", fluxnet2015_end)

## ---- Legend ------------------------------------------------------------------
legend_lines <- c(
  "FIGURE LEGEND — fig_02_historical_only.png",
  strrep("=", 60), "",
  "TITLE: Figure 2 (historical collections only) — presentation reveal companion",
  "",
  "PURPOSE: NOT a numbered Supplementary Information figure for journal submission.",
  "A presentation aid: show this slide, then advance to the real Figure 2",
  "(fig_02_cumulative_siteyears_igbp.png, draft_manuscript_v1/) on an identical x/y",
  "axis to reveal the snapshot's growth on top of the historical context.",
  "",
  "DESCRIPTION:",
  "Identical to Figure 2 in every respect -- same data, same x/y axis range and breaks,",
  "same dashed release-year reference lines (2000/2007/2015), same line colours/styling,",
  "same 89 x 89 mm Nature-format dimensions -- EXCEPT that the snapshot's",
  "IGBP-stacked area and its 'IGBP' legend are omitted entirely: only",
  "the Marconi 2000 / La Thuile 2007 / FLUXNET2015 cumulative site-year lines are drawn.",
  "The y axis is identical to Figure 2's even though the snapshot's data is not drawn: the",
  "same snapshot cumulative totals still drive the axis range via an invisible layer",
  "(fig_cumulative_siteyears_igbp(show_current_network = FALSE) in",
  "R/figures/fig_network_growth.R), so this figure is a strict visual subset of Figure 2",
  "on the same axes, not a separately-scaled plot.",
  "(comparison data only, not primary data -- see CLAUDE.md Hard Rule 1).",
  "",
  "SITE-YEAR RULE (each series counts site-years by its own collection's convention --",
  "same as Figure 2):",
  paste0("  Marconi: sum(last_year - first_year + 1) over its own 'Years in Marconi' ranges",
         " -- ends at ", marconi_end, "."),
  paste0("  La Thuile: count of 1s in its own year-indicator matrix (1991-2007), NOT the",
         " first-to-last span (which over-counts at 1008) -- ends at ", la_thuile_end, "."),
  paste0("  FLUXNET2015: count of non-NA cells in its own year-presence matrix (1991-2014)",
         " -- ends at ", fluxnet2015_end, "."),
  "",
  paste0("AXIS WINDOW (data-driven, data_year_window() in R/utils.R -- identical to Figure 2): ",
         year_window$first_year, "-", year_window$last_year, "."),
  "",
  "SOURCE: scripts/generate_fig02_historical_only.R, calling the same",
  "R/figures/fig_network_growth.R::fig_cumulative_siteyears_igbp() function as Figure 2",
  "(scripts/generate_fig_cumulative_siteyears.R), with show_current_network = FALSE.",
  paste0("DIMENSIONS: ", NATURE_WIDTH_SINGLE_MM, " x ", NATURE_WIDTH_SINGLE_MM,
         " mm, 600 dpi PNG + vector PDF + 300 ppi JPEG,"),
  "Helvetica, white background. All text 5-7pt."
)
writeLines(legend_lines, paste0(OUT_STEM, ".legend.txt"))
msg("Saved: ", paste0(OUT_STEM, ".legend.txt"))
msg("=== Done ===")
