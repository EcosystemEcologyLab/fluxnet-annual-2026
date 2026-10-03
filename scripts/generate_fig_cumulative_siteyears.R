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
## Output: review/figures/draft_manuscript_v1/fig_cumulative_siteyears_igbp.png/.pdf/.legend.txt

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
  library(dplyr); library(readr); library(ggplot2)
})

msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)
msg("=== Figure: cumulative site-years by IGBP (standalone, no panel letter) ===")

OUT_DIR  <- file.path("review", "figures", "draft_manuscript_v1")
OUT_STEM <- file.path(OUT_DIR, "fig_cumulative_siteyears_igbp")
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

## ---- Build panel (no panel_letter -- this is a standalone figure) -----------
panel <- fig_cumulative_siteyears_igbp(
  presence_df       = presence_df,
  shuttle_meta      = shuttle_meta,
  sites_marconi     = sites_marconi,
  sites_la_thuile   = sites_la_thuile,
  sites_fluxnet2015 = sites_fluxnet2015,
  base_size         = 9L
) +
  ggplot2::theme(legend.key.size = grid::unit(7, "pt")) +
  nature_theme()

saved <- save_nature_figure(panel, OUT_STEM, width_mm = NATURE_WIDTH_SINGLE_MM,
                             height_mm = NATURE_WIDTH_SINGLE_MM)
msg("Saved: ", saved$png, ", ", saved$pdf)

## ---- Report site-years actually plotted (parity with Dur11's own report) ----
fig_igbp_order_report <- c("ENF", "EBF", "DNF", "DBF", "MF", "CSH", "OSH", "WSA",
                            "SAV", "GRA", "WET", "CRO", "CVM", "BSV", "SNO")
siteyears_plotted <- presence_df |>
  dplyr::filter(has_data, year >= 1991L, year <= 2024L) |>
  dplyr::left_join(
    shuttle_meta |> dplyr::distinct(site_id, .keep_all = TRUE) |> dplyr::select(site_id, igbp),
    by = "site_id"
  ) |>
  dplyr::filter(!is.na(igbp), igbp %in% fig_igbp_order_report)
total_site_years <- nrow(siteyears_plotted)
msg("Total site-years plotted (1991-2024): ", total_site_years)

## ---- Legend --------------------------------------------------------------------
legend_lines <- c(
  "FIGURE LEGEND — fig_cumulative_siteyears_igbp.png",
  strrep("=", 60), "",
  "TITLE: Cumulative site-years of the FLUXNET Shuttle network through time,",
  "by IGBP land-cover class", "",
  "DESCRIPTION:",
  "Cumulative site-years of the current network through time, stacked by IGBP class",
  "(paper-wide palette, R/plot_constants.R::PAPER_IGBP_COLOURS), with Marconi 2000/La",
  "Thuile 2007/FLUXNET2015 cumulative site-year lines overlaid for historical context",
  "(comparison data only, not primary data -- see CLAUDE.md Hard Rule 1).",
  "No panel letter -- this is a standalone single-panel figure, not part of a composite.",
  "",
  paste0("TOTAL SITE-YEARS PLOTTED (1991-2024): ", total_site_years),
  "",
  "SOURCE: scripts/generate_fig_cumulative_siteyears.R, calling",
  "R/figures/fig_network_growth.R::fig_cumulative_siteyears_igbp() -- the same function",
  "and pinned inputs as scripts/generate_duration_histograms.R (Dur11), which continues",
  "to stage fig_dur11_CumulativeSiteYears_IGBP.png in review/figures/network/ under its",
  "own canonical name.",
  paste0("DIMENSIONS: ", NATURE_WIDTH_SINGLE_MM, " x ", NATURE_WIDTH_SINGLE_MM,
         " mm, 600 dpi PNG + vector PDF,"),
  "Helvetica, white background. All text 5-7pt."
)
writeLines(legend_lines, paste0(OUT_STEM, ".legend.txt"))
msg("Saved: ", paste0(OUT_STEM, ".legend.txt"))
msg("=== Done ===")
