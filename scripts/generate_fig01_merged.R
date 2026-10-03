## generate_fig01_merged.R
## Merged Figure 1: panel a (the Equal Earth current-network map) stacked
## above panel b (cumulative site-years by IGBP class) in ONE file, 89mm
## wide -- task 9, 2026-10-02. Calls the same two panel-building functions
## (fig_map_point_network(), fig_cumulative_siteyears_igbp()) and the same
## pinned snapshot/historical inputs as generate_point_maps.R and
## generate_duration_histograms.R, so this file's panels are pixel-for-pixel
## the same content as the separately-staged fig_01a/fig_01b files those
## scripts still produce (NOT regenerated from different inputs). Does not
## touch the pre-existing hand-assembled Fig01_AB files -- none were found
## anywhere in this repository; see SESSION_LOG.md 2026-10-02.
##
## Output: review/figures/draft_manuscript_v1/fig_01.png/.pdf/.legend.txt

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
source("R/utils.R")
source("R/plot_constants.R")
source("R/figures/fig_maps.R")
source("R/figures/fig_network_growth.R")
source("R/nature_format.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(ggplot2); library(patchwork)
})

msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)
msg("=== Merged Figure 1 (panel a map + panel b cumulative site-years) ===")

OUT_DIR  <- file.path("review", "figures", "draft_manuscript_v1")
OUT_STEM <- file.path(OUT_DIR, "fig_01")
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

## ---- Panel a: Equal Earth current-network map -------------------------------
panel_a <- fig_map_point_network(
  metadata = shuttle_meta, backdrop = "white", pt_size = 1.0, pt_alpha = 0.65
) + ggplot2::labs(subtitle = NULL) + panel_letter("a")
h_a <- equal_earth_height_mm(NATURE_WIDTH_SINGLE_MM)
msg("Panel a height at ", NATURE_WIDTH_SINGLE_MM, "mm wide: ", round(h_a, 2), "mm")

## ---- Panel b: cumulative site-years by IGBP ---------------------------------
panel_b <- fig_cumulative_siteyears_igbp(
  presence_df       = presence_df,
  shuttle_meta      = shuttle_meta,
  sites_marconi     = sites_marconi,
  sites_la_thuile   = sites_la_thuile,
  sites_fluxnet2015 = sites_fluxnet2015,
  base_size         = 9L
) +
  ggplot2::theme(legend.key.size = grid::unit(7, "pt")) +
  nature_theme() +
  panel_letter("b")
h_b <- NATURE_WIDTH_SINGLE_MM  # square, as the standalone fig_01b is

## ---- Combine -----------------------------------------------------------------
combo <- panel_a / panel_b + patchwork::plot_layout(heights = c(h_a, h_b))
total_h <- h_a + h_b
msg("Combined height: ", round(total_h, 2), "mm (limit ", NATURE_MAX_HEIGHT_MM, "mm)")

saved <- save_nature_figure(combo, OUT_STEM, width_mm = NATURE_WIDTH_SINGLE_MM, height_mm = total_h)
msg("Saved: ", saved$png, ", ", saved$pdf)

## ---- Legend --------------------------------------------------------------------
n_sites <- shuttle_meta |> filter(!is.na(location_lat), !is.na(location_long)) |>
  distinct(site_id) |> nrow()
legend_lines <- c(
  "FIGURE LEGEND — fig_01.png",
  strrep("=", 60), "",
  "TITLE: Figure 1. The FLUXNET Shuttle 2025 network", "",
  "DESCRIPTION:",
  paste0("Panel a: Equal Earth (EPSG:8857) map of all ", n_sites, " current-network towers, "),
  "filled, no outline, semi-transparent points (overlap reads as darker). Country borders",
  "thinner/lighter than coastlines. Antarctica and the high Arctic (above 85N) excluded.",
  "Panel b: cumulative site-years of the current network through time, stacked by IGBP class",
  "(paper-wide palette, R/plot_constants.R::PAPER_IGBP_COLOURS), with Marconi 2000/La Thuile",
  "2007/FLUXNET2015 cumulative site-year lines overlaid for historical context (comparison",
  "data only, not primary data -- see CLAUDE.md Hard Rule 1).",
  "",
  "SOURCE: scripts/generate_fig01_merged.R, combining R/figures/fig_maps.R::",
  "fig_map_point_network() and R/figures/fig_network_growth.R::fig_cumulative_siteyears_igbp(),",
  "the same two functions and pinned inputs as scripts/generate_point_maps.R (panel a) and",
  "scripts/generate_duration_histograms.R (panel b), which continue to stage the separate",
  "fig_01a/fig_01b files this merged file does not replace.",
  paste0("DIMENSIONS: ", NATURE_WIDTH_SINGLE_MM, " x ", round(total_h, 1),
         " mm (panel a ", round(h_a, 1), "mm + panel b ", round(h_b, 1), "mm), 600 dpi PNG + vector PDF,"),
  "Helvetica, white background. All text 5-7pt except the bold lower-case panel letters (8pt)."
)
writeLines(legend_lines, paste0(OUT_STEM, ".legend.txt"))
msg("Saved: ", paste0(OUT_STEM, ".legend.txt"))
msg("=== Done ===")
