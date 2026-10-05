## generate_fig_cumulative_siteyears.R
## Figure stage 2 (logs/figstage_prompt.md): cumulative site-years by IGBP
## class promoted to its OWN main-text figure, 89mm wide, no panel letter
## (it is no longer panel b of a merged Figure 1 -- see figure stage 2).
## Calls the same fig_cumulative_siteyears_igbp() panel-building function and
## the same pinned snapshot/historical inputs as
## scripts/generate_duration_histograms.R (Dur11) and the now-retired
## scripts/generate_fig01_merged.R, so this file's content is pixel-for-pixel
## the same plot, just without panel_letter("b") and under a new name.
##
## Output: review/figures/draft_manuscript_v1/fig_02_cumulative_siteyears_igbp.png/.pdf/.legend.txt
## (renumbered from fig_cumulative_siteyears_igbp.* under figure stage 6
## numbering, 2026-10-02; this script's own name is unchanged -- see
## docs/figure_inventory.md)

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
msg("=== Figure: cumulative site-years by IGBP (standalone, no panel letter) ===")

OUT_DIR  <- file.path("review", "figures", "draft_manuscript_v1")
OUT_STEM <- file.path(OUT_DIR, "fig_02_cumulative_siteyears_igbp")
fs::dir_create(OUT_DIR)

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

## ---- La Thuile actual year-indicator presence (not first-to-last span) -----
## Drives the La Thuile cumulative line to its correct total of 965 site-years
## (span-based expansion of years_la_thuile.csv over-counts at 1008 -- La
## Thuile site records are not contiguous within their first/last year; see
## docs/methods_requirements.md 5.5 and scripts/collection_comparison_table.R).
la_thuile_xlsx       <- read_excel("data/lists/LaThuileList.xlsx")
la_thuile_yr_cols    <- names(la_thuile_xlsx)[grepl("^[0-9]{4}$", names(la_thuile_xlsx))]
la_thuile_year_matrix <- la_thuile_xlsx |>
  dplyr::select(site_id = "SITE", dplyr::all_of(la_thuile_yr_cols)) |>
  tidyr::pivot_longer(cols = dplyr::all_of(la_thuile_yr_cols), names_to = "year", values_to = "flag") |>
  dplyr::mutate(year = as.integer(.data$year), present = !is.na(.data$flag) & .data$flag == 1)
msg("Loaded La Thuile year-indicator matrix: ",
    sum(la_thuile_year_matrix$present), " site-years (expect 965)")

## ---- Build panel (no panel_letter -- this is a standalone figure) -----------
panel <- fig_cumulative_siteyears_igbp(
  presence_df            = presence_df,
  shuttle_meta           = shuttle_meta,
  sites_marconi          = sites_marconi,
  sites_la_thuile        = sites_la_thuile,
  sites_fluxnet2015      = sites_fluxnet2015,
  base_size              = 9L,
  la_thuile_year_matrix  = la_thuile_year_matrix
) +
  ggplot2::theme(legend.key.size = grid::unit(7, "pt")) +
  nature_theme()

saved <- save_nature_figure(panel, OUT_STEM, width_mm = NATURE_WIDTH_SINGLE_MM,
                             height_mm = NATURE_WIDTH_SINGLE_MM)
msg("Saved: ", saved$png, ", ", saved$pdf)

## ---- Data-driven year window (replaces the hard-coded 1991-2024 filter) ----
## data_year_window() (R/utils.R) reads the first/last calendar year with any
## has_data = TRUE row in presence_df, ignoring empty ONEFlux padding years --
## so this figure (and its legend, below) extend automatically as new years
## of data arrive, with no further editing here.
year_window <- data_year_window(presence_df)
msg("Data-driven year window: ", year_window$first_year, "-", year_window$last_year,
    " (", year_window$n_sites_last_year, " sites report ", year_window$last_year, ")")

## ---- Report site-years actually plotted (parity with Dur11's own report) ----
fig_igbp_order_report <- c("ENF", "EBF", "DNF", "DBF", "MF", "CSH", "OSH", "WSA",
                            "SAV", "GRA", "WET", "CRO", "CVM", "BSV", "SNO")
siteyears_plotted <- presence_df |>
  dplyr::filter(has_data, year >= year_window$first_year, year <= year_window$last_year) |>
  dplyr::left_join(
    shuttle_meta |> dplyr::distinct(site_id, .keep_all = TRUE) |> dplyr::select(site_id, igbp),
    by = "site_id"
  ) |>
  dplyr::filter(!is.na(igbp), igbp %in% fig_igbp_order_report)
total_site_years <- nrow(siteyears_plotted)
msg("Total site-years plotted (", year_window$first_year, "-", year_window$last_year, "): ",
    total_site_years)

## ---- Historical line end points (reported in the legend) -------------------
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

## ---- Legend --------------------------------------------------------------------
legend_lines <- c(
  "FIGURE LEGEND — fig_02_cumulative_siteyears_igbp.png",
  strrep("=", 60), "",
  "TITLE: Figure 2. Cumulative site-years of the FLUXNET Shuttle network through time,",
  "by IGBP land-cover class", "",
  "DESCRIPTION:",
  "Cumulative site-years of the current network through time, stacked by IGBP class",
  "(paper-wide palette, R/plot_constants.R::PAPER_IGBP_COLOURS), with Marconi 2000/La",
  "Thuile 2007/FLUXNET2015 cumulative site-year lines overlaid for historical context",
  "(comparison data only, not primary data -- see CLAUDE.md Hard Rule 1).",
  "No panel letter -- this is a standalone single-panel figure, not part of a composite.",
  "",
  "SITE-YEAR RULE (each series counts site-years by its own collection's convention):",
  paste0("  Current network: compute_site_year_presence() -- a site-year counts if any of its"),
  paste0("    flux variables (NEE/GPP/RECO/LE/H) has a non-NA monthly value; has_data = TRUE, ",
         year_window$first_year, "-", year_window$last_year, "."),
  paste0("  Marconi: sum(last_year - first_year + 1) over its own 'Years in Marconi' ranges",
         " -- ends at ", marconi_end, "."),
  paste0("  La Thuile: count of 1s in its own year-indicator matrix (1991-2007), NOT the",
         " first-to-last span (which over-counts at 1008) -- ends at ", la_thuile_end, "."),
  paste0("  FLUXNET2015: count of non-NA cells in its own year-presence matrix (1991-2014)",
         " -- ends at ", fluxnet2015_end, "."),
  "",
  paste0("YEAR WINDOW (data-driven, data_year_window() in R/utils.R -- not hard-coded): ",
         year_window$first_year, "-", year_window$last_year, ". ", year_window$n_sites_last_year,
         " of ", dplyr::n_distinct(shuttle_meta$site_id), " network sites report data in ",
         year_window$last_year, ", the most recent year with any site data (ignoring empty",
         " ONEFlux padding years)."),
  paste0("TOTAL SITE-YEARS PLOTTED (", year_window$first_year, "-", year_window$last_year, "): ",
         total_site_years),
  "",
  "SOURCE: scripts/generate_fig_cumulative_siteyears.R, calling",
  "R/figures/fig_network_growth.R::fig_cumulative_siteyears_igbp() -- the same function",
  "and pinned inputs as scripts/generate_duration_histograms.R (Dur11), which continues",
  "to stage fig_dur11_CumulativeSiteYears_IGBP.png in review/figures/network/ under its",
  "own canonical name. Dur11 still uses the La Thuile span-based fallback (not passed",
  "la_thuile_year_matrix) -- see R/figures/fig_network_growth.R. Dur11 does not pass",
  "year_range either, so as of 2026-10-05 it also now plots through the same data-driven",
  "window as this figure (previously both were independently hard-coded at 2024) -- see",
  "SESSION_LOG.md 2026-10-05.",
  paste0("DIMENSIONS: ", NATURE_WIDTH_SINGLE_MM, " x ", NATURE_WIDTH_SINGLE_MM,
         " mm, 600 dpi PNG + vector PDF,"),
  "Helvetica, white background. All text 5-7pt."
)
writeLines(legend_lines, paste0(OUT_STEM, ".legend.txt"))
msg("Saved: ", paste0(OUT_STEM, ".legend.txt"))
msg("=== Done ===")
