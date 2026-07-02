## scripts/generate_whittaker_global.R
## Generates two global ice-free-land Whittaker background figures
## (WorldClim v2.1 MAT/MAP climate space), for use as talk-slide backgrounds
## under the network-distribution Whittaker panels (fig_whit01_ShuttleFull.png
## / fig_02_whittaker_current.png). These figures represent global land area,
## not FLUXNET sites, and carry no NEE information.
##
## NON-SHUTTLE DATA NOTICE: this script uses WorldClim v2.1 (BIO1/BIO12) and
## ESA CCI land cover (2015) global rasters -- neither is FLUXNET Shuttle
## data. Per CLAUDE.md Hard Rule #1 these are used only as background/context
## layers for talk slides, not as primary data for the Annual Paper.
##
## Outputs (review/figures/whittaker/):
##   fig_whit_global_frequency.png        + .legend.txt
##   fig_whit_global_contour.png          + .legend.txt
##
## Run log (written continuously, independent of terminal output):
##   review/figures/whittaker/RUN_LOG_whittaker_global.txt
##
## Run from repo root: Rscript scripts/generate_whittaker_global.R

out_dir      <- file.path("review", "figures", "whittaker")
run_log_path <- file.path(out_dir, "RUN_LOG_whittaker_global.txt")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

run_log_con <- file(run_log_path, open = "a")
rl <- function(...) {
  ts   <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  line <- paste0(ts, " ", paste0(..., collapse = ""))
  cat(line, "\n", sep = "", file = run_log_con)
  flush(run_log_con)
  message(line)
}

# The whole pipeline runs inside a function (not at the top level) so that
# tryCatch()/on.exit() below reliably fire on both success and failure --
# on.exit() registered directly at Rscript top level does not run when the
# script simply finishes, only when an enclosing function call exits.
run_pipeline <- function() {

  rl("attempting: source R/pipeline_config.R, R/plot_constants.R, R/figures/fig_climate.R and run check_pipeline_config()")
  if (file.exists(".env")) {
    suppressMessages(library(dotenv))
    dotenv::load_dot_env()
  }
  source("R/pipeline_config.R")
  source("R/plot_constants.R")
  source("R/figures/fig_climate.R")
  suppressMessages({
    library(dplyr)
    library(ggplot2)
    library(colorspace)
  })
  check_pipeline_config()
  rl("completed: pipeline config sourced and checked")

  # ---- Locate WorldClim source rasters (same paths as the fig_whittaker_worldclim() fallback) ----
  bio1_path      <- "data/external/worldclim/climate/wc2.1_2.5m/wc2.1_2.5m_bio_1.tif"
  bio12_path     <- "data/external/worldclim/climate/wc2.1_2.5m/wc2.1_2.5m_bio_12.tif"
  landcover_path <- "data/external/cci_landcover/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif"

  rl("attempting: check WorldClim BIO1 raster exists at ", bio1_path)
  if (!file.exists(bio1_path)) {
    rl("MISSING INPUT: WorldClim BIO1 (MAT) raster not found at expected path ", bio1_path,
       ". Not downloading, substituting, or approximating -- stopping.")
    stop("WorldClim BIO1 raster not found: ", bio1_path, call. = FALSE)
  }
  rl("found: ", bio1_path)

  rl("attempting: check WorldClim BIO12 raster exists at ", bio12_path)
  if (!file.exists(bio12_path)) {
    rl("MISSING INPUT: WorldClim BIO12 (MAP) raster not found at expected path ", bio12_path,
       ". Not downloading, substituting, or approximating -- stopping.")
    stop("WorldClim BIO12 raster not found: ", bio12_path, call. = FALSE)
  }
  rl("found: ", bio12_path)

  rl("attempting: check ESA CCI land-cover raster (ice/water mask source) exists at ", landcover_path)
  if (!file.exists(landcover_path)) {
    rl("MISSING INPUT: ESA CCI land-cover raster not found at expected path ", landcover_path,
       ". Not downloading, substituting, or approximating -- stopping.")
    stop("Land-cover raster not found: ", landcover_path, call. = FALSE)
  }
  rl("found: ", landcover_path)

  # ---- Build global ice-free-land MAT/MAP pixel table --------------------------
  rl("attempting: build_global_landclimate() -- reclassify CCI land cover (class 210 water, 220 ice -> excluded; ",
     "class 0 no-data -> NA; all other classes -> ice-free land), aggregate to WorldClim grid (factor 15, exact), ",
     "threshold at >=0.5 ice-free-land fraction, apply Antarctica lat < -60 backstop, compute cos(lat) area weights. ",
     "This reads/streams the full 300 m global CCI raster (~312 MB on disk) and may take several minutes.")
  t0 <- Sys.time()
  land_climate <- build_global_landclimate(
    bio1_path      = bio1_path,
    bio12_path     = bio12_path,
    landcover_path = landcover_path
  )
  elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  rl("completed: build_global_landclimate() returned ", format(nrow(land_climate), big.mark = ","),
     " ice-free land pixels (of ", format(4320L * 8640L, big.mark = ","), " total WorldClim grid cells) in ",
     elapsed, " s. MAT range [", round(min(land_climate$mat), 2), ", ", round(max(land_climate$mat), 2),
     "] degC. MAP range [", round(min(land_climate$map), 1), ", ", round(max(land_climate$map), 1), "] mm/yr.")

  total_weight <- sum(land_climate$weight)

  # ---- Style: reuse the exact 3.5x3.5in/300dpi override used for fig_whit01_ShuttleFull.png ----
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
  rl("computation: axis ranges read from WHITTAKER_STYLE (R/figures/fig_climate.R) -- xlim = [",
     style_3x3$xlim[1], ", ", style_3x3$xlim[2], "] degC, ylim = [", style_3x3$ylim[1], ", ",
     style_3x3$ylim[2], "] mm/yr, hex bins = 15, panel = 3.5 x 3.5 in @ 300 dpi (matching fig_whit01_ShuttleFull.png).")

  mat_in  <- land_climate$mat  >= style_3x3$xlim[1] & land_climate$mat  <= style_3x3$xlim[2]
  map_in  <- land_climate$map  >= style_3x3$ylim[1] & land_climate$map  <= style_3x3$ylim[2]
  frac_mat_clipped <- 1 - sum(land_climate$weight[mat_in]) / total_weight
  frac_map_clipped <- 1 - sum(land_climate$weight[map_in]) / total_weight
  frac_both_kept   <- sum(land_climate$weight[mat_in & map_in]) / total_weight
  rl("computation: clip diagnostics vs Figure 2 axis window -- ", sprintf("%.2f%%", 100 * frac_mat_clipped),
     " of global land-area weight falls outside MAT range [-15,35] (mostly cold tundra/taiga MAT < -15), ",
     sprintf("%.2f%%", 100 * frac_map_clipped), " falls outside MAP range [0,4000] (wet tropical tails MAP > 4000). ",
     sprintf("%.2f%%", 100 * frac_both_kept), " of global ice-free land area is shown within the displayed axis window.")

  # ---- Figure 1: frequency hexbin ----------------------------------------------
  rl("attempting: build fig_whittaker_global_frequency() (area-weighted hexbin, log10 fill, bins=15, clipped to axis window before binning)")
  freq_result <- fig_whittaker_global_frequency(land_climate, style = style_3x3)
  freq_path   <- file.path(out_dir, "fig_whit_global_frequency.png")
  ggplot2::ggsave(freq_path, plot = freq_result$plot,
                  width = style_3x3$width_in, height = style_3x3$height_in,
                  units = "in", dpi = 300, bg = "white")
  rl("completed: wrote ", freq_path, " (3.5 x 3.5 in, 300 dpi, white bg). ",
     sprintf("%.2f%%", 100 * freq_result$frac_kept), " of global land-area weight shown after axis clipping.")

  # ---- Figure 2: HDR contour envelope -------------------------------------------
  rl("attempting: build fig_whittaker_global_contour() (weighted KDE over FULL unclipped global distribution, ",
     "95%/99% highest-density-region contours, then clipped to the Figure 2 axis window for display)")
  contour_result <- fig_whittaker_global_contour(land_climate, style = style_3x3, probs = c(0.95, 0.99))
  contour_path   <- file.path(out_dir, "fig_whit_global_contour.png")
  ggplot2::ggsave(contour_path, plot = contour_result$plot,
                  width = style_3x3$width_in, height = style_3x3$height_in,
                  units = "in", dpi = 300, bg = "white")

  contour_x_range <- range(contour_result$contour_df$x)
  contour_y_range <- range(contour_result$contour_df$y)
  contour_open_at_edge <- contour_x_range[1] < style_3x3$xlim[1] || contour_x_range[2] > style_3x3$xlim[2] ||
                          contour_y_range[1] < style_3x3$ylim[1] || contour_y_range[2] > style_3x3$ylim[2]
  rl("completed: wrote ", contour_path, " (3.5 x 3.5 in, 300 dpi, white bg). ",
     "Contour coordinate range: MAT [", round(contour_x_range[1], 1), ", ", round(contour_x_range[2], 1),
     "], MAP [", round(contour_y_range[1], 1), ", ", round(contour_y_range[2], 1), "]. ",
     if (contour_open_at_edge) {
       "At least one contour extends beyond the Figure 2 axis window and is visibly clipped/open at a plot edge."
     } else {
       "Both contours close fully within the Figure 2 axis window."
     })

  # ---- Legends -------------------------------------------------------------------
  rl("attempting: write legend .txt files for both figures")

  legend_common <- paste0(
    "Data source: WorldClim v2.1, 2.5 arc-minute BIO1 (mean annual temperature) and BIO12 (annual precipitation).\n",
    "  wc2.1_2.5m_bio_1.tif / wc2.1_2.5m_bio_12.tif -- NOT FLUXNET Shuttle data (see CLAUDE.md Hard Rule #1);\n",
    "  used here only as a global background/context layer, not as primary data for the Annual Paper.\n",
    "Land mask: ALL ICE-FREE LAND (not the vegetated mask used elsewhere in this paper). Built from ESA CCI\n",
    "  land cover 2015 (300 m, ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif): class 210 (water bodies) and\n",
    "  class 220 (permanent snow/ice) excluded; class 0 (no data) excluded; all other classes retained as land.\n",
    "  Aggregated to the WorldClim 2.5 arc-min grid (exact 15x integer factor, no reprojection) and thresholded\n",
    "  at >=50% ice-free-land fraction per WorldClim cell. A latitude < -60 degree backstop additionally excludes\n",
    "  Antarctica.\n",
    "Area weighting: cosine-of-latitude pixel weighting (weight = cos(latitude in radians)), applied per\n",
    "  WorldClim pixel, because WorldClim is an equal-angle (not equal-area) grid and raw pixel counts would\n",
    "  over-weight high latitudes. Every density/frequency calculation below uses this weight, not pixel counts.\n",
    "Axis ranges: MAT (x) = [", style_3x3$xlim[1], ", ", style_3x3$xlim[2], "] degC, MAP (y) = [",
    style_3x3$ylim[1], ", ", style_3x3$ylim[2], "] mm/yr -- read directly from WHITTAKER_STYLE in\n",
    "  R/figures/fig_climate.R and identical to Figure 2 (fig_whit01_ShuttleFull.png / fig_02_whittaker_current.png),\n",
    "  so this figure registers with Figure 2 when layered as a slide background. Panel = 3.5 x 3.5 in, 300 dpi,\n",
    "  matching fig_whit01_ShuttleFull.png exactly.\n",
    "Clipping: ", sprintf("%.2f%%", 100 * frac_mat_clipped), " of global ice-free land-area weight falls outside\n",
    "  the MAT axis range (cold tundra/taiga colder than -15 degC MAT) and ", sprintf("%.2f%%", 100 * frac_map_clipped),
    " falls outside\n  the MAP axis range (wet tropical areas above 4000 mm/yr); neither axis was extended to\n",
    "  accommodate these tails -- per instruction, the Figure 2 axis window was matched exactly instead.\n",
    "Source script: scripts/generate_whittaker_global.R (functions build_global_landclimate(),\n",
    "  fig_whittaker_global_frequency(), fig_whittaker_global_contour() in R/figures/fig_climate.R).\n",
    "Generated: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), "\n"
  )

  freq_legend <- paste0(
    "fig_whit_global_frequency.png -- Global ice-free land, Whittaker frequency hexbin\n",
    "================================================================================\n",
    legend_common,
    "\n",
    "Color scale: neutral single-hue sequential ramp (colorspace 'Blues 3'), NOT the diverging NEE scale used\n",
    "  in Figure 2 -- this background represents land area, which has no NEE. Fill = area-weighted land-pixel\n",
    "  frequency summed within each hexagon, log10 scale (log10 chosen because land area is extremely\n",
    "  right-skewed across climate space -- most hexagons contain little area, a few contain a large fraction).\n",
    "  Empty hexagons (zero land pixels) are not drawn.\n",
    "Hex bin domain: pixel data pre-clipped to the MAT/MAP axis ranges above before hexbinning (bins = 15, same\n",
    "  as fig_whittaker_worldclim()), so hex-cell geometry matches Figure 2's hexbin grid exactly for overlay\n",
    "  registration.\n",
    sprintf("%.2f%% of global ice-free land-area weight is shown within the displayed axis window.\n", 100 * freq_result$frac_kept)
  )
  writeLines(freq_legend, file.path(out_dir, "fig_whit_global_frequency.legend.txt"))
  rl("completed: wrote ", file.path(out_dir, "fig_whit_global_frequency.legend.txt"))

  contour_legend <- paste0(
    "fig_whit_global_contour.png -- Global ice-free land, Whittaker density-contour envelope\n",
    "==========================================================================================\n",
    legend_common,
    "\n",
    "Color scale: none -- white background, no fill. Two contour lines only (solid = 95%, dashed = 99%),\n",
    "  colour grey20, intended as a clean envelope layer to sit under network points on talk slides.\n",
    "Contour method: highest-density-region (HDR) contours enclosing 95% and 99% of GLOBAL (unclipped)\n",
    "  ice-free land-area weight, from a weighted 2D kernel density estimate (linear binning onto a 201x201\n",
    "  grid + separable Gaussian smoothing, bandwidth via a weighted Scott's-rule reference using the\n",
    "  effective sample size of the area weights; implemented in .weighted_density_grid()/.hdr_levels() in\n",
    "  R/figures/fig_climate.R -- no new package dependency). The density estimate itself uses the FULL,\n",
    "  unclipped global distribution so the 95%/99% figures are honest to 'global ice-free land area'; the\n",
    "  same Figure 2 axis window is then applied for display only.\n",
    if (contour_open_at_edge) {
      paste0("  NOTE: at least one contour line extends beyond the displayed axis window and appears open/clipped\n",
             "  at a plot edge on this panel -- this reflects real land area outside the Figure 2 climate-space\n",
             "  window (see clipping percentages above), not a rendering error.\n")
    } else {
      "  Both the 95% and 99% contours close fully within the displayed axis window.\n"
    }
  )
  writeLines(contour_legend, file.path(out_dir, "fig_whit_global_contour.legend.txt"))
  rl("completed: wrote ", file.path(out_dir, "fig_whit_global_contour.legend.txt"))

  invisible(TRUE)
}

pipeline_error <- tryCatch({
  run_pipeline()
  NULL
}, error = function(e) e)

if (is.null(pipeline_error)) {
  rl("outcome: SUCCESS -- both PNGs and both legend .txt files written.")
  message("\nDone: fig_whit_global_frequency.png, fig_whit_global_contour.png and their legends written to ", out_dir)
} else {
  rl("ERROR: ", conditionMessage(pipeline_error))
  rl("outcome: FAILED -- see the last 'attempting' line above for what was in progress when the script stopped.")
}
close(run_log_con)
if (!is.null(pipeline_error)) stop(pipeline_error)
