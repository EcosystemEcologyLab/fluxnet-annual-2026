## generate_whittaker_alt_fig02.R
## ALTERNATIVE ("ALT") candidate version of Figure 2
## (review/figures/draft_manuscript_v1/fig_02_whittaker_current.png), built in
## preparation for two requested changes -- reviewed and fixed here via new,
## backward-compatible parameters on fig_whittaker_worldclim() (default
## values unchanged, so every other existing caller/figure is unaffected):
##
##  1. Hexagon shape. fig_02's hexagons are NOT regular -- they render
##     elongated on the vertical (MAP) axis. Root cause: ggplot2::
##     stat_summary_hex()'s default binwidth splits `bins` evenly across the
##     FULL, unclipped MAT/MAP data range, and GeomHex draws each hexagon
##     assuming 1 MAT data-unit and 1 MAP data-unit render at an EQUAL
##     physical length -- true only by coincidence, dependent on whatever the
##     panel's incidental physical aspect ratio happens to be once legend and
##     axis-label margins are subtracted from the requested canvas size (here
##     3.5 x 3.5in). Since MAT spans ~50 degC and MAP spans ~4000 mm/yr, that
##     coincidence does not hold, and hexagons stretch. Fix (fig_climate.R,
##     fig_whittaker_worldclim(..., hex_regular = TRUE)): binwidth is computed
##     explicitly from style$xlim/style$ylim (not the full data range), and
##     ggplot2::coord_fixed(ratio = diff(xlim)/diff(ylim)) is used in place of
##     coord_cartesian(), which pins the physical-length ratio between one MAT
##     unit and one MAP unit by construction -- regular by design, not by
##     accident of panel layout. With equal hex_bins in both directions this
##     also yields an exactly square panel.
##
##  2. Point layering and size. The base figure draws per-site MAT/MAP points
##     BEHIND the hexagons (visible only where they poke out -- see
##     fig_whittaker_worldclim()'s original comment). This ALT version calls
##     fig_whittaker_worldclim(..., points_in_front = TRUE, point_size = 0.7)
##     -- points at 50% of the original 1.4 size (0.7), drawn on top of the
##     hexagons instead of underneath.
##
## Everything else (contour overlay, colour scale, NEE data, site snapshot,
## axis ranges) is identical to scripts/generate_whittaker_overlays.R's build
## of fig_whit_fig2_with_both_contours.png / fig_02_whittaker_current.png --
## same cached density_grid, same Mahalanobis coverage reference. Neither
## fig_02_whittaker_current.png nor fig_whittaker_worldclim()'s DEFAULT
## behaviour is touched by this script; the new parameters default to FALSE/
## unchanged, so every other existing caller (generate_whittaker.R's whit01-
## whit07, 00_candidate_figures.R, generate_whittaker_overlays.R) is
## unaffected.
##
## NON-SHUTTLE DATA NOTICE: WorldClim v2.1 / ESA CCI land cover (2015) are
## used only as background/context (climate-space envelope), per CLAUDE.md
## Hard Rule #1. FLUXNET site positions come from the Shuttle snapshot CSV
## named in the run log below.
##
## Output: review/figures/candidates/ALT_fig_02_whittaker_current.png (+.txt)

out_dir <- file.path("review", "figures", "candidates")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)
msg("=== ALT Figure 2: regular hexagons + points-in-front, 50% smaller ===")

if (file.exists(".env")) {
  suppressMessages(library(dotenv))
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
source("R/plot_constants.R")
source("R/figures/fig_climate.R")
suppressMessages({
  library(dplyr); library(ggplot2); library(colorspace); library(duckdb); library(readr)
})
check_pipeline_config()

# ---- Reuse the cached global density surface (same as generate_whittaker_overlays.R) ----
cache_dir       <- "data/processed"
densitygrid_rds <- file.path(cache_dir, "whittaker_global_density_grid.rds")
if (!file.exists(densitygrid_rds)) {
  stop("Missing cache: ", densitygrid_rds,
       " -- run scripts/generate_whittaker_global.R first.", call. = FALSE)
}
density_grid <- readRDS(densitygrid_rds)
msg("Loaded cached density_grid (", length(density_grid$xbin), "x", length(density_grid$ybin), ")")

# ---- Load Figure 2 inputs (identical to generate_whittaker_overlays.R) ----
db_path <- file.path(FLUXNET_DATA_ROOT, "duckdb/fluxnet.duckdb")
if (!file.exists(db_path)) stop("DuckDB database not found: ", db_path, call. = FALSE)
con <- dbConnect(duckdb(), db_path, read_only = TRUE)
data_yy <- dplyr::tbl(con, "annual_converted") |>
  dplyr::filter(dataset == "FLUXMET") |>
  dplyr::select(site_id, TIMESTAMP, NEE_VUT_REF, NEE_CUT_REF) |>
  dplyr::collect() |>
  dplyr::mutate(YEAR = as.integer(TIMESTAMP))
dbDisconnect(con)

snap_files <- sort(
  list.files(file.path(FLUXNET_DATA_ROOT, "snapshots"),
             pattern = "fluxnet_shuttle_snapshot.*\\.csv$", full.names = TRUE),
  decreasing = TRUE
)
if (length(snap_files) == 0L) stop("No Shuttle snapshot CSV found.", call. = FALSE)
snap_file    <- snap_files[[1]]
shuttle_meta <- readr::read_csv(snap_file, show_col_types = FALSE)
msg("Loaded data_yy (", format(nrow(data_yy), big.mark = ","), " rows) and shuttle_meta (",
    format(nrow(shuttle_meta), big.mark = ","), " sites) from ", snap_file)

style_3x3 <- utils::modifyList(WHITTAKER_STYLE, list(
  width_in          = 3.5,
  height_in         = 3.5,
  axis_text_size    = 7,
  axis_title_size   = 8,
  legend_text_size  = 6,
  legend_title_size = 7,
  detail_text_size  = 2.3,
  colorbar_width    = grid::unit(1.3, "in"),
  colorbar_height   = grid::unit(0.12, "in")
))

# ---- Base layer: regular hexagons, points in front at 50% size ----
msg("Building base plot via fig_whittaker_worldclim(hex_regular=TRUE, points_in_front=TRUE, point_size=0.7)")
fig2_alt_base <- fig_whittaker_worldclim(
  data_yy         = data_yy,
  site_meta       = shuttle_meta,
  detail_label    = "FLUXNET Shuttle 2025",
  style           = style_3x3,
  hex_regular     = TRUE,
  points_in_front = TRUE,
  point_size      = 0.7
)

# ---- Contour overlay: identical geometry/method to generate_whittaker_overlays.R ----
msg("Computing contour_df via fig_whittaker_global_contour(density_grid=density_grid)")
contour_result <- fig_whittaker_global_contour(style = style_3x3, probs = c(0.95, 0.99),
                                                density_grid = density_grid)
contour_df <- contour_result$contour_df
line_map   <- c("95%" = "solid", "99%" = "dashed")
contour_df$prob_label <- droplevels(contour_df$prob_label)

fig2_alt <- fig2_alt_base +
  ggplot2::geom_path(
    data = contour_df,
    ggplot2::aes(x = .data$x, y = .data$y,
                 group = interaction(.data$prob, .data$piece),
                 linetype = .data$prob_label),
    colour      = "black",
    linewidth   = 0.7,
    inherit.aes = FALSE
  ) +
  ggplot2::scale_linetype_manual(
    values = line_map[levels(contour_df$prob_label)],
    guide  = "none"
  )

out_fig <- file.path(out_dir, "ALT_fig_02_whittaker_current.png")
ggplot2::ggsave(out_fig, fig2_alt, width = style_3x3$width_in, height = style_3x3$height_in,
                units = "in", dpi = 300, bg = "white")
msg("Saved: ", out_fig, " (", style_3x3$width_in, " x ", style_3x3$height_in, " in, 300 dpi)")

# ---- Legend / caption ----
out_txt <- file.path(out_dir, "ALT_fig_02_whittaker_current.txt")
writeLines(c(
  "ALT Figure 2 -- regular hexagons, points in front at 50% size",
  "",
  "Alternative to review/figures/draft_manuscript_v1/fig_02_whittaker_current.png,",
  "prepared for review before deciding whether to adopt these changes in the",
  "production figure. Same base layer (FLUXNET network hexbin coloured by",
  "median site NEE) and same 95% solid / 99% dashed global ice-free-land HDR",
  "contour overlay as fig_02 -- only two things differ:",
  "",
  "1. Hexagons are now REGULAR (equilateral), not vertically elongated. Root",
  "   cause: ggplot2::stat_summary_hex()'s default binwidth divides the FULL",
  "   unclipped MAT/MAP data range by `bins`, and draws each hexagon assuming",
  "   1 MAT unit and 1 MAP unit render at equal physical length -- true only",
  "   by coincidence of the panel's incidental aspect ratio after legend/",
  "   axis-label margins are subtracted from the 3.5x3.5in canvas. Fixed via",
  "   two new fig_whittaker_worldclim() parameters (R/figures/fig_climate.R):",
  "   hex_regular = TRUE computes binwidth explicitly from style$xlim/ylim",
  "   and swaps coord_cartesian() for coord_fixed(ratio = diff(xlim)/",
  "   diff(ylim)), which pins the MAT:MAP physical-length ratio by",
  "   construction. Both new parameters default to FALSE/unchanged, so no",
  "   other existing figure (generate_whittaker.R's whit01-07,",
  "   00_candidate_figures.R, fig_02_whittaker_current.png itself) is",
  "   affected by this change.",
  "",
  "2. Per-site points are drawn IN FRONT of the hexagons (points_in_front =",
  "   TRUE), not behind, and are 50% smaller (point_size = 0.7 vs the",
  "   original 1.4).",
  "",
  "Data sources: identical to fig_02_whittaker_current.png -- see",
  "review/figures/whittaker/fig_whit_fig2_with_both_contours.legend.txt for",
  "the full method (WorldClim v2.1, ESA CCI land cover 2015, DuckDB",
  "annual_converted FLUXMET, Shuttle snapshot, Mahalanobis coverage stat).",
  paste0("Shuttle snapshot: ", snap_file, " (", nrow(shuttle_meta), " sites)."),
  "",
  "Source script: scripts/generate_whittaker_alt_fig02.R",
  "Function change: fig_whittaker_worldclim() in R/figures/fig_climate.R",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
), out_txt)
msg("Saved: ", out_txt)

msg("=== ALT Figure 2 complete ===")
