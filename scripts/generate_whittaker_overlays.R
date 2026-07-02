## scripts/generate_whittaker_overlays.R
## Companion to scripts/generate_whittaker_global.R. Reuses the density
## surface computed there (data/processed/whittaker_global_density_grid.rds,
## cached by that script -- falls back to recomputing via the same functions
## if the cache is absent) for two purposes:
##
##  Task 2: overlay the existing 95%/99% highest-density-region (HDR) global
##  ice-free-land contours on top of the CURRENT Figure 2
##  (fig_whit01_ShuttleFull.png / fig_02_whittaker_current.png, the network
##  hexbin coloured by median NEE), built by calling the UNMODIFIED
##  fig_whittaker_worldclim() and adding geom_path() layers on top of its
##  returned ggplot object. Neither the source Figure 2 nor
##  fig_whittaker_worldclim() is modified.
##
##  Task 3: climate-space coverage statistics -- how much of the global
##  ice-free-land HDR envelope (same envelope the contours draw) the FLUXNET
##  network's site climate positions reach, at cell-count and area-weighted
##  resolution, for both the 95% and 99% envelopes, plus a bin-resolution
##  sensitivity sweep.
##
## NON-SHUTTLE DATA NOTICE: WorldClim v2.1 / ESA CCI land cover (2015) are
## used only as background/context (climate-space envelope), per CLAUDE.md
## Hard Rule #1. The FLUXNET site positions themselves come from the Shuttle
## snapshot CSV named in the run log / coverage CSV metadata.
##
## Outputs (review/figures/whittaker/):
##   fig_whit_fig2_with_99contour.png    + .legend.txt
##   fig_whit_fig2_with_95contour.png    + .legend.txt
##   fig_whit_fig2_with_both_contours.png + .legend.txt
##
## Outputs (data/snapshots/, committed):
##   whittaker_climate_coverage.csv + .meta.json
##
## Run log (written continuously, independent of terminal output; shared with
## the orchestrating agent's own entries for this task):
##   review/figures/whittaker/RUN_LOG_whittaker_overlays.txt
##
## Run from repo root: Rscript scripts/generate_whittaker_overlays.R

out_dir      <- file.path("review", "figures", "whittaker")
run_log_path <- file.path(out_dir, "RUN_LOG_whittaker_overlays.txt")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

run_log_con <- file(run_log_path, open = "a")
rl <- function(...) {
  ts   <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  line <- paste0(ts, " ", paste0(..., collapse = ""))
  cat(line, "\n", sep = "", file = run_log_con)
  flush(run_log_con)
  message(line)
}

# Whole pipeline runs inside a function called via tryCatch() so the final
# outcome line is written unconditionally -- on.exit() registered at Rscript
# top level does not fire on normal completion (confirmed the hard way in the
# companion script's earlier session; see RUN_LOG_whittaker_global.txt).
run_pipeline <- function() {

  rl("attempting: source R/pipeline_config.R, R/plot_constants.R, R/figures/fig_climate.R and run check_pipeline_config()")
  if (file.exists(".env")) {
    suppressMessages(library(dotenv))
    dotenv::load_dot_env()
  }
  source("R/pipeline_config.R")
  source("R/plot_constants.R")
  source("R/figures/fig_climate.R")
  source("R/utils.R")
  suppressMessages({
    library(dplyr)
    library(ggplot2)
    library(colorspace)
    library(duckdb)
    library(readr)
  })
  check_pipeline_config()
  rl("completed: pipeline config sourced and checked")

  # ---- Reuse the density surface from generate_whittaker_global.R (cache) ----
  cache_dir       <- "data/processed"
  landclimate_rds <- file.path(cache_dir, "whittaker_global_landclimate.rds")
  densitygrid_rds <- file.path(cache_dir, "whittaker_global_density_grid.rds")

  rl("attempting: check for cached land_climate/density_grid from scripts/generate_whittaker_global.R at ",
     landclimate_rds, " and ", densitygrid_rds, " (avoids recomputing the density surface)")
  if (file.exists(landclimate_rds) && file.exists(densitygrid_rds)) {
    land_climate <- readRDS(landclimate_rds)
    density_grid <- readRDS(densitygrid_rds)
    rl("found: both RDS caches present. Loaded land_climate (", format(nrow(land_climate), big.mark = ","),
       " rows) and density_grid (", length(density_grid$xbin), "x", length(density_grid$ybin),
       ") without recomputation.")
  } else {
    rl("MISSING CACHE: one or both of ", landclimate_rds, " / ", densitygrid_rds,
       " not found. Falling back to rebuilding via the SAME functions used by scripts/generate_whittaker_global.R ",
       "(build_global_landclimate() + .weighted_density_grid()) -- not a separately defined computation, ",
       "but not a reuse of a prior run's exact object either. Run scripts/generate_whittaker_global.R first to avoid this.")
    bio1_path      <- "data/external/worldclim/climate/wc2.1_2.5m/wc2.1_2.5m_bio_1.tif"
    bio12_path     <- "data/external/worldclim/climate/wc2.1_2.5m/wc2.1_2.5m_bio_12.tif"
    landcover_path <- "data/external/cci_landcover/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif"
    for (p in c(bio1_path, bio12_path, landcover_path)) {
      if (!file.exists(p)) {
        rl("MISSING INPUT: ", p, " not found. Not downloading, substituting, or approximating -- stopping.")
        stop("Required raster not found: ", p, call. = FALSE)
      }
    }
    land_climate <- build_global_landclimate(bio1_path = bio1_path, bio12_path = bio12_path,
                                             landcover_path = landcover_path)
    density_grid <- .weighted_density_grid(land_climate$mat, land_climate$map, land_climate$weight,
                                           gridsize = c(201, 201))
    rl("completed: rebuilt land_climate (", format(nrow(land_climate), big.mark = ","),
       " rows) and density_grid (201x201) from source rasters.")
  }
  total_weight <- sum(land_climate$weight)

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
  rl("computation: reusing the same 3.5x3.5in/300dpi style override as scripts/generate_whittaker_global.R ",
     "and fig_whit01_ShuttleFull.png -- xlim=[", style_3x3$xlim[1], ",", style_3x3$xlim[2],
     "] degC, ylim=[", style_3x3$ylim[1], ",", style_3x3$ylim[2], "] mm/yr.")

  mat_in <- land_climate$mat >= style_3x3$xlim[1] & land_climate$mat <= style_3x3$xlim[2]
  map_in <- land_climate$map >= style_3x3$ylim[1] & land_climate$map <= style_3x3$ylim[2]
  frac_outside_axes <- 1 - sum(land_climate$weight[mat_in & map_in]) / total_weight
  rl("computation: ", sprintf("%.2f%%", 100 * frac_outside_axes),
     " of global ice-free land-area weight falls outside the displayed Figure 2 axis window ",
     "(previous session's fig_whit_global_* run reported 1.18% total outside the axes -- see below for the exact match/comparison).")

  # ---- Load Figure 2 inputs (same pattern as scripts/generate_whittaker.R's whit01) ----
  rl("attempting: load Figure 2 inputs -- data/duckdb/fluxnet.duckdb::annual_converted (FLUXMET) and the latest data/snapshots/fluxnet_shuttle_snapshot*.csv, to reconstruct the exact fig_whit01_ShuttleFull.png plot object via the unmodified fig_whittaker_worldclim()")
  db_path <- file.path(FLUXNET_DATA_ROOT, "duckdb/fluxnet.duckdb")
  if (!file.exists(db_path)) {
    rl("MISSING INPUT: DuckDB database not found at ", db_path, ". Not substituting -- stopping.")
    stop("DuckDB database not found: ", db_path, call. = FALSE)
  }
  con <- dbConnect(duckdb(), db_path, read_only = TRUE)
  if (!"annual_converted" %in% dbListTables(con)) {
    dbDisconnect(con)
    rl("MISSING INPUT: annual_converted table absent from ", db_path, ". Not substituting -- stopping.")
    stop("annual_converted table missing.", call. = FALSE)
  }
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
  if (length(snap_files) == 0L) {
    rl("MISSING INPUT: no fluxnet_shuttle_snapshot*.csv found under ", file.path(FLUXNET_DATA_ROOT, "snapshots"),
       ". Not substituting -- stopping.")
    stop("No Shuttle snapshot CSV found.", call. = FALSE)
  }
  snap_file    <- snap_files[[1]]
  shuttle_meta <- readr::read_csv(snap_file, show_col_types = FALSE)
  rl("completed: loaded data_yy (", format(nrow(data_yy), big.mark = ","), " rows, FLUXMET annual_converted) and ",
     "shuttle_meta from ", snap_file, " (", format(nrow(shuttle_meta), big.mark = ","), " sites).")

  rl("attempting: build the Figure 2 base plot via the unmodified fig_whittaker_worldclim(data_yy, shuttle_meta, ...) -- identical call to scripts/generate_whittaker.R's Whit01")
  fig2_base <- fig_whittaker_worldclim(
    data_yy      = data_yy,
    site_meta    = shuttle_meta,
    detail_label = "FLUXNET Shuttle 2025",
    style        = style_3x3
  )
  rl("completed: fig2_base built (unmodified fig_whittaker_worldclim() call; not saved directly -- used only as the base layer for the three overlays below).")

  # ---- Shared contour geometry (reusing density_grid -- no recompute) --------
  rl("attempting: fig_whittaker_global_contour(density_grid = density_grid) to obtain the shared contour_df (95%/99% HDR lines) for the three overlays")
  contour_result <- fig_whittaker_global_contour(style = style_3x3, probs = c(0.95, 0.99),
                                                 density_grid = density_grid)
  contour_df <- contour_result$contour_df
  rl("completed: contour_df has ", nrow(contour_df), " rows, probs present: ",
     paste(sort(unique(contour_df$prob), decreasing = TRUE), collapse = ", "))

  line_map <- c("95%" = "solid", "99%" = "dashed")

  add_contour_layer <- function(base_plot, df) {
    df$prob_label <- droplevels(df$prob_label)
    base_plot +
      ggplot2::geom_path(
        data = df,
        ggplot2::aes(x = .data$x, y = .data$y,
                    group = interaction(.data$prob, .data$piece),
                    linetype = .data$prob_label),
        colour    = "black",
        linewidth = 0.7,
        inherit.aes = FALSE
      ) +
      ggplot2::scale_linetype_manual(
        values = line_map[levels(df$prob_label)],
        name   = "Global ice-free\nland area (HDR)"
      )
  }

  # ---- Task 2: three Figure 2 + contour overlays ------------------------------
  rl("attempting: build and save fig_whit_fig2_with_99contour.png (Figure 2 + 99% HDR contour only)")
  p_99 <- add_contour_layer(fig2_base, dplyr::filter(contour_df, .data$prob == 0.99))
  path_99 <- file.path(out_dir, "fig_whit_fig2_with_99contour.png")
  ggplot2::ggsave(path_99, plot = p_99, width = style_3x3$width_in, height = style_3x3$height_in,
                  units = "in", dpi = 300, bg = "white")
  rl("completed: wrote ", path_99, " (3.5 x 3.5 in, 300 dpi, white bg).")

  rl("attempting: build and save fig_whit_fig2_with_95contour.png (Figure 2 + 95% HDR contour only)")
  p_95 <- add_contour_layer(fig2_base, dplyr::filter(contour_df, .data$prob == 0.95))
  path_95 <- file.path(out_dir, "fig_whit_fig2_with_95contour.png")
  ggplot2::ggsave(path_95, plot = p_95, width = style_3x3$width_in, height = style_3x3$height_in,
                  units = "in", dpi = 300, bg = "white")
  rl("completed: wrote ", path_95, " (3.5 x 3.5 in, 300 dpi, white bg).")

  rl("attempting: build and save fig_whit_fig2_with_both_contours.png (Figure 2 + both 95% solid and 99% dashed HDR contours)")
  p_both <- add_contour_layer(fig2_base, contour_df)
  path_both <- file.path(out_dir, "fig_whit_fig2_with_both_contours.png")
  ggplot2::ggsave(path_both, plot = p_both, width = style_3x3$width_in, height = style_3x3$height_in,
                  units = "in", dpi = 300, bg = "white")
  rl("completed: wrote ", path_both, " (3.5 x 3.5 in, 300 dpi, white bg).")

  # ---- Task 2 legends ----------------------------------------------------------
  rl("attempting: write legend .txt files for the three overlay figures")
  overlay_legend_common <- paste0(
    "Base layer: Figure 2 (fig_whit01_ShuttleFull.png / fig_02_whittaker_current.png) -- the FLUXNET network\n",
    "  hexbin, coloured by median site NEE, built by calling the UNMODIFIED fig_whittaker_worldclim() with the\n",
    "  latest Shuttle snapshot (", snap_file, ", ", format(nrow(shuttle_meta), big.mark = ","), " sites) and\n",
    "  data/duckdb/fluxnet.duckdb::annual_converted (dataset == 'FLUXMET'). Neither the source Figure 2 file nor\n",
    "  fig_whittaker_worldclim() was modified -- these overlays are a geom_path() layer added on top of its\n",
    "  returned ggplot object, then re-saved under a new filename.\n",
    "Overlay layer: highest-density-region (HDR) contour(s) of GLOBAL (unclipped) ice-free-land area, from the\n",
    "  SAME density_grid used by fig_whit_global_density.png and fig_whit_global_contour.png (WorldClim v2.1\n",
    "  BIO1/BIO12 x ESA CCI LC 2015 ice-free-land mask, cosine-of-latitude area weighting, weighted 2D KDE,\n",
    "  201x201 base grid -- see fig_whit_global_contour.legend.txt for the full method). Drawn in solid black\n",
    "  here (rather than fig_whit_global_contour.png's grey20) for visibility over Figure 2's coloured hexbins.\n",
    "Axis ranges: MAT (x) = [", style_3x3$xlim[1], ", ", style_3x3$xlim[2], "] degC, MAP (y) = [",
    style_3x3$ylim[1], ", ", style_3x3$ylim[2], "] mm/yr -- identical to Figure 2 (this IS Figure 2, layered).\n",
    "Panel = 3.5 x 3.5 in, 300 dpi.\n",
    "Source script: scripts/generate_whittaker_overlays.R (fig_whittaker_worldclim(), fig_whittaker_global_contour()\n",
    "  in R/figures/fig_climate.R).\n",
    "Generated: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), "\n"
  )

  writeLines(paste0(
    "fig_whit_fig2_with_99contour.png -- Figure 2 with the 99% global ice-free-land HDR contour overlaid\n",
    "========================================================================================================\n",
    overlay_legend_common,
    "\nLine: dashed black line = the boundary enclosing 99% of global ice-free land-area weight in MAT/MAP space.\n"
  ), file.path(out_dir, "fig_whit_fig2_with_99contour.legend.txt"))
  rl("completed: wrote ", file.path(out_dir, "fig_whit_fig2_with_99contour.legend.txt"))

  writeLines(paste0(
    "fig_whit_fig2_with_95contour.png -- Figure 2 with the 95% global ice-free-land HDR contour overlaid\n",
    "========================================================================================================\n",
    overlay_legend_common,
    "\nLine: solid black line = the boundary enclosing 95% of global ice-free land-area weight in MAT/MAP space.\n"
  ), file.path(out_dir, "fig_whit_fig2_with_95contour.legend.txt"))
  rl("completed: wrote ", file.path(out_dir, "fig_whit_fig2_with_95contour.legend.txt"))

  writeLines(paste0(
    "fig_whit_fig2_with_both_contours.png -- Figure 2 with both 95% and 99% global ice-free-land HDR contours\n",
    "=============================================================================================================\n",
    overlay_legend_common,
    "\nLines: solid black = 95% HDR boundary (inner, smaller envelope). Dashed black = 99% HDR boundary (outer,\n",
    "  larger envelope). The two are visually distinguished by line type only (both black), labelled in a small\n",
    "  in-panel legend ('Global ice-free land area (HDR)').\n"
  ), file.path(out_dir, "fig_whit_fig2_with_both_contours.legend.txt"))
  rl("completed: wrote ", file.path(out_dir, "fig_whit_fig2_with_both_contours.legend.txt"))

  # ---- Task 3: climate-space coverage statistics ------------------------------
  rl("attempting: load per-site WorldClim MAT/MAP (data/snapshots/site_worldclim.csv) and join to the current network site list for coverage statistics")
  worldclim_csv <- "data/snapshots/site_worldclim.csv"
  if (!file.exists(worldclim_csv)) {
    rl("MISSING INPUT: ", worldclim_csv, " not found. Not substituting -- stopping.")
    stop("site_worldclim.csv not found: ", worldclim_csv, call. = FALSE)
  }
  site_wc <- readr::read_csv(worldclim_csv, show_col_types = FALSE)
  site_climate <- shuttle_meta |>
    dplyr::distinct(.data$site_id, .keep_all = TRUE) |>
    dplyr::inner_join(site_wc, by = "site_id") |>
    dplyr::filter(!is.na(.data$mat_worldclim), !is.na(.data$map_worldclim))
  rl("completed: ", nrow(site_climate), " of ", nrow(shuttle_meta),
     " network sites have known WorldClim MAT/MAP (site_worldclim.csv join); these are the site positions used for coverage.")

  rl("attempting: whittaker_hdr_coverage() at BASE resolution (201x201, the density_grid already loaded/cached above) -- computes the four headline coverage numbers a-d")
  cov_base <- whittaker_hdr_coverage(density_grid, site_climate$mat_worldclim, site_climate$map_worldclim,
                                     probs = c(0.95, 0.99))
  cov_base$gridsize_x <- length(density_grid$xbin)
  cov_base$gridsize_y <- length(density_grid$ybin)
  cov_base$is_base    <- TRUE
  for (i in seq_len(nrow(cov_base))) {
    rl("statistic: prob=", cov_base$prob[i], " hdr_level=", signif(cov_base$hdr_level[i], 6),
       " n_cells_region=", cov_base$n_cells_region[i], " n_sites_in_region_cells=", cov_base$n_sites[i],
       " cell_coverage=", sprintf("%.6f", cov_base$cell_coverage[i]),
       " area_weighted_coverage=", sprintf("%.6f", cov_base$area_weighted_coverage[i]),
       " (base resolution 201x201)")
  }
  a_val <- cov_base$cell_coverage[cov_base$prob == 0.95]
  b_val <- cov_base$cell_coverage[cov_base$prob == 0.99]
  c_val <- cov_base$area_weighted_coverage[cov_base$prob == 0.95]
  d_val <- cov_base$area_weighted_coverage[cov_base$prob == 0.99]
  rl("computation: HEADLINE NUMBERS (base resolution 201x201) -- ",
     "(a) cell-count coverage of 95% HDR region = ", sprintf("%.4f", a_val), "; ",
     "(b) cell-count coverage of 99% HDR region = ", sprintf("%.4f", b_val), "; ",
     "(c) area-weighted coverage of 95% HDR region = ", sprintf("%.4f", c_val), "; ",
     "(d) area-weighted coverage of 99% HDR region = ", sprintf("%.4f", d_val), ".")

  # ---- Bin-sensitivity sweep ---------------------------------------------------
  rl("attempting: bin-sensitivity sweep -- recompute the four numbers at gridsizes 181x181, 191x191, 201x201(base), 211x211, 221x221")
  sweep_sizes <- list(c(181L, 181L), c(191L, 191L), c(201L, 201L), c(211L, 211L), c(221L, 221L))
  sweep_results <- lapply(sweep_sizes, function(gs) {
    if (identical(gs, c(201L, 201L))) {
      dg_i <- density_grid  # reuse the already-computed base grid, no recompute
      rl("computation: gridsize ", gs[1], "x", gs[2], " -- reused the cached base density_grid (no recompute).")
    } else {
      t0 <- Sys.time()
      dg_i <- .weighted_density_grid(land_climate$mat, land_climate$map, land_climate$weight, gridsize = gs)
      el   <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
      rl("computation: gridsize ", gs[1], "x", gs[2], " -- .weighted_density_grid() recomputed in ", el, " s (deliberate, for the sensitivity sweep; same land_climate as the base run).")
    }
    cov_i <- whittaker_hdr_coverage(dg_i, site_climate$mat_worldclim, site_climate$map_worldclim,
                                    probs = c(0.95, 0.99))
    cov_i$gridsize_x <- gs[1]
    cov_i$gridsize_y <- gs[2]
    cov_i$is_base    <- identical(gs, c(201L, 201L))
    for (i in seq_len(nrow(cov_i))) {
      rl("statistic: gridsize=", gs[1], "x", gs[2], " prob=", cov_i$prob[i],
         " cell_coverage=", sprintf("%.6f", cov_i$cell_coverage[i]),
         " area_weighted_coverage=", sprintf("%.6f", cov_i$area_weighted_coverage[i]))
    }
    cov_i
  })
  sweep_df <- do.call(rbind, sweep_results)
  rl("completed: bin-sensitivity sweep produced ", nrow(sweep_df), " rows (5 gridsizes x 2 probs).")

  range_a <- range(sweep_df$cell_coverage[sweep_df$prob == 0.95])
  range_b <- range(sweep_df$cell_coverage[sweep_df$prob == 0.99])
  range_c <- range(sweep_df$area_weighted_coverage[sweep_df$prob == 0.95])
  range_d <- range(sweep_df$area_weighted_coverage[sweep_df$prob == 0.99])
  rl("computation: SENSITIVITY RANGE across gridsizes 181..221 -- ",
     "(a) cell_coverage@95%: [", sprintf("%.4f", range_a[1]), ", ", sprintf("%.4f", range_a[2]), "]; ",
     "(b) cell_coverage@99%: [", sprintf("%.4f", range_b[1]), ", ", sprintf("%.4f", range_b[2]), "]; ",
     "(c) area_weighted_coverage@95%: [", sprintf("%.4f", range_c[1]), ", ", sprintf("%.4f", range_c[2]), "]; ",
     "(d) area_weighted_coverage@99%: [", sprintf("%.4f", range_d[1]), ", ", sprintf("%.4f", range_d[2]), "].")

  # ---- Write committed CSV + .meta.json ----------------------------------------
  rl("attempting: write data/snapshots/whittaker_climate_coverage.csv (full sensitivity-sweep table) and its .meta.json")
  csv_path <- "data/snapshots/whittaker_climate_coverage.csv"
  sweep_df_out <- sweep_df[, c("gridsize_x", "gridsize_y", "is_base", "prob", "hdr_level",
                               "n_cells_region", "n_sites", "cell_coverage", "area_weighted_coverage")]
  readr::write_csv(sweep_df_out, csv_path)

  notes <- paste0(
    "FLUXNET-network coverage of global ice-free-land HDR climate-space envelopes (MAT x MAP Whittaker space). ",
    "One row per (grid resolution x HDR probability). is_base=TRUE marks the 201x201 base resolution used by ",
    "fig_whit_global_density.png / fig_whit_global_contour.png / the Figure 2 overlays. ",
    "HEADLINE NUMBERS (base 201x201): (a) cell-count coverage of the 95% HDR region = ", sprintf("%.4f", a_val),
    "; (b) cell-count coverage of the 99% HDR region = ", sprintf("%.4f", b_val),
    "; (c) area-weighted coverage of the 95% HDR region = ", sprintf("%.4f", c_val),
    "; (d) area-weighted coverage of the 99% HDR region = ", sprintf("%.4f", d_val), ". ",
    "Bin-sensitivity range across gridsizes {181,191,201,211,221} (all square): ",
    "cell_coverage@95% in [", sprintf("%.4f", range_a[1]), ",", sprintf("%.4f", range_a[2]), "]; ",
    "cell_coverage@99% in [", sprintf("%.4f", range_b[1]), ",", sprintf("%.4f", range_b[2]), "]; ",
    "area_weighted_coverage@95% in [", sprintf("%.4f", range_c[1]), ",", sprintf("%.4f", range_c[2]), "]; ",
    "area_weighted_coverage@99% in [", sprintf("%.4f", range_d[1]), ",", sprintf("%.4f", range_d[2]), "]. ",
    sprintf("%.2f%%", 100 * frac_outside_axes), " of global ice-free land-area weight falls outside the displayed ",
    "Figure 2 axis window (MAT -15..35 degC, MAP 0..4000 mm/yr). ",
    "HDR envelope definition: highest-density-region contour of a weighted 2D kernel density estimate (linear ",
    "binning + separable Gaussian smoothing, weighted Scott's-rule bandwidth) over the FULL unclipped global ",
    "ice-free-land pixel distribution; probs=c(0.95,0.99); implemented in .weighted_density_grid()/.hdr_levels() ",
    "in R/figures/fig_climate.R. Coverage definition: a site's MAT/MAP position is snapped to the nearest ",
    "density-grid node; cell_coverage = fraction of grid cells with density>=hdr_level that contain >=1 site; ",
    "area_weighted_coverage = fraction of the density mass within the HDR region that falls in site-containing ",
    "cells; implemented in whittaker_hdr_coverage() in R/figures/fig_climate.R. ",
    "Land mask: ESA CCI land cover 2015 (ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif), class 210 (water) and ",
    "220 (permanent snow/ice) excluded, class 0 (no data) excluded, all else = ice-free land; aggregated to the ",
    "WorldClim 2.5 arc-min grid (exact 15x factor) and thresholded at >=50% ice-free-land fraction per cell; ",
    "latitude < -60 deg backstop additionally excludes Antarctica. Area weighting: cosine of latitude ",
    "(WorldClim is an equal-angle, not equal-area, grid). ",
    "FLUXNET snapshot used for site positions: ", snap_file, " (", nrow(shuttle_meta), " network sites; ",
    nrow(site_climate), " with known WorldClim MAT/MAP via data/snapshots/site_worldclim.csv). ",
    "Non-Shuttle background data (WorldClim v2.1 BIO1/BIO12, ESA CCI LC 2015) per CLAUDE.md Hard Rule #1 -- ",
    "used only to define the climate-space envelope, not as primary Annual Paper data. ",
    "Source script: scripts/generate_whittaker_overlays.R."
  )
  write_output_metadata(
    csv_path,
    input_sources = c(snap_file, worldclim_csv,
                      "data/external/worldclim/climate/wc2.1_2.5m/wc2.1_2.5m_bio_1.tif",
                      "data/external/worldclim/climate/wc2.1_2.5m/wc2.1_2.5m_bio_12.tif",
                      "data/external/cci_landcover/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif"),
    notes = notes
  )
  rl("completed: wrote ", csv_path, " (", nrow(sweep_df_out), " rows) and its .meta.json companion.")

  invisible(TRUE)
}

pipeline_error <- tryCatch({
  run_pipeline()
  NULL
}, error = function(e) e)

if (is.null(pipeline_error)) {
  rl("outcome: SUCCESS -- three Figure 2 contour overlays + legends, the coverage CSV + meta.json, all written.")
  message("\nDone: fig_whit_fig2_with_99contour.png, fig_whit_fig2_with_95contour.png, ",
          "fig_whit_fig2_with_both_contours.png and their legends written to ", out_dir,
          "; data/snapshots/whittaker_climate_coverage.csv + .meta.json written.")
} else {
  rl("ERROR: ", conditionMessage(pipeline_error))
  rl("outcome: FAILED -- see the last 'attempting' line above for what was in progress when the script stopped.")
}
close(run_log_con)
if (!is.null(pipeline_error)) stop(pipeline_error)
