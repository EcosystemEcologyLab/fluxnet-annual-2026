## scripts/generate_whittaker_coverage_mahalanobis.R
## Recomputes the FLUXNET climate-space coverage statistic using a
## Mahalanobis-distance definition, replacing the grid-cell coverage numbers
## from the previous session (data/snapshots/whittaker_climate_coverage.csv,
## which this script marks as superseded but does not delete or modify).
##
## Reuses (does not rebuild) the cached global ice-free-land pixel table and
## weighted density surface written by scripts/generate_whittaker_global.R
## (data/processed/whittaker_global_landclimate.rds,
## whittaker_global_density_grid.rds) via the new function
## whittaker_mahalanobis_coverage() in R/figures/fig_climate.R.
## whittaker_hdr_coverage() (the grid-cell metric) is left untouched in
## R/figures/fig_climate.R for provenance but its output is not used here.
##
## NON-SHUTTLE DATA NOTICE: WorldClim v2.1 / ESA CCI land cover (2015) define
## the climate-space envelope only, per CLAUDE.md Hard Rule #1 -- not primary
## Annual Paper data. FLUXNET site positions come from the named Shuttle
## snapshot below.
##
## Outputs (data/snapshots/, committed):
##   whittaker_climate_coverage_mahalanobis.csv + .meta.json
##   whittaker_climate_coverage.meta.json gains a "superseded_by" note
##     (the CSV itself is untouched)
##
## Run log (written continuously, independent of terminal output; shared with
## the orchestrating agent's own entries for this task):
##   review/figures/whittaker/RUN_LOG_whittaker_coverage_mahalanobis.txt
##
## Run BEFORE scripts/generate_whittaker_overlays.R for this task -- the
## overlay figures' legends cite the reference (d=0.5) coverage number
## written here.
##
## Run from repo root: Rscript scripts/generate_whittaker_coverage_mahalanobis.R

out_dir      <- file.path("review", "figures", "whittaker")
run_log_path <- file.path(out_dir, "RUN_LOG_whittaker_coverage_mahalanobis.txt")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

run_log_con <- file(run_log_path, open = "a")
rl <- function(...) {
  ts   <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  line <- paste0(ts, " ", paste0(..., collapse = ""))
  cat(line, "\n", sep = "", file = run_log_con)
  flush(run_log_con)
  message(line)
}

# Pipeline body runs inside a function called via tryCatch() so the final
# outcome line is written unconditionally -- on.exit() at Rscript top level
# does not fire on normal completion (confirmed in an earlier session; see
# RUN_LOG_whittaker_overlays.txt).
run_pipeline <- function() {

  rl("attempting: source R/pipeline_config.R, R/plot_constants.R, R/figures/fig_climate.R, R/utils.R and run check_pipeline_config()")
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
    library(readr)
    library(jsonlite)
  })
  check_pipeline_config()
  rl("completed: pipeline config sourced and checked")

  # ---- Reuse the cached density surface -- stop if absent, do not rebuild ----
  cache_dir       <- "data/processed"
  landclimate_rds <- file.path(cache_dir, "whittaker_global_landclimate.rds")
  densitygrid_rds <- file.path(cache_dir, "whittaker_global_density_grid.rds")

  rl("attempting: check for cached land_climate/density_grid at ", landclimate_rds, " and ", densitygrid_rds)
  if (!file.exists(landclimate_rds)) {
    rl("MISSING CACHE: ", landclimate_rds, " not found. Not rebuilding/substituting -- stopping. ",
       "Run scripts/generate_whittaker_global.R first to produce this cache.")
    stop("Missing cache: ", landclimate_rds, call. = FALSE)
  }
  if (!file.exists(densitygrid_rds)) {
    rl("MISSING CACHE: ", densitygrid_rds, " not found. Not rebuilding/substituting -- stopping. ",
       "Run scripts/generate_whittaker_global.R first to produce this cache.")
    stop("Missing cache: ", densitygrid_rds, call. = FALSE)
  }
  land_climate <- readRDS(landclimate_rds)
  density_grid <- readRDS(densitygrid_rds)
  rl("found: both RDS caches present. Loaded land_climate (", format(nrow(land_climate), big.mark = ","),
     " rows) and density_grid (", length(density_grid$xbin), "x", length(density_grid$ybin),
     ") without recomputation.")

  total_weight <- sum(land_climate$weight)
  style_xlim <- WHITTAKER_STYLE$xlim
  style_ylim <- WHITTAKER_STYLE$ylim
  mat_in <- land_climate$mat >= style_xlim[1] & land_climate$mat <= style_xlim[2]
  map_in <- land_climate$map >= style_ylim[1] & land_climate$map <= style_ylim[2]
  frac_outside_axes <- 1 - sum(land_climate$weight[mat_in & map_in]) / total_weight
  rl("computation: ", sprintf("%.2f%%", 100 * frac_outside_axes),
     " of global ice-free land-area weight falls outside the Figure 2 display window (MAT [-15,35], MAP [0,4000]) -- ",
     "this fraction is NOT excluded from the coverage denominator below (unrestricted 'all' region uses the FULL ",
     "unclipped land_climate, consistent with the previous run's clip-accounting convention).")

  # ---- FLUXNET snapshot: the exact file named in the task ---------------------
  snap_file <- "data/snapshots/fluxnet_shuttle_snapshot_20260624T095651.csv"
  rl("attempting: check FLUXNET snapshot exists at ", snap_file, " (named explicitly for this task)")
  if (!file.exists(snap_file)) {
    rl("MISSING INPUT: ", snap_file, " not found. Not substituting -- stopping.")
    stop("Snapshot not found: ", snap_file, call. = FALSE)
  }
  shuttle_meta <- readr::read_csv(snap_file, show_col_types = FALSE)
  rl("found: ", snap_file, " (", nrow(shuttle_meta), " network sites).")

  worldclim_csv <- "data/snapshots/site_worldclim.csv"
  rl("attempting: check ", worldclim_csv, " exists (per-site WorldClim MAT/MAP)")
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
     " network sites have known WorldClim MAT/MAP -- these exact site points (not gridded) are used for the ",
     "Mahalanobis distance calculation below.")

  # ---- Mahalanobis coverage: the single expensive computation, done once -----
  thresholds <- c(0.25, 0.5, 1.0)
  reference_threshold <- 0.5
  rl("attempting: whittaker_mahalanobis_coverage(land_climate, site MAT/MAP, thresholds=c(0.25,0.5,1.0), density_grid, hdr_probs=c(0.95,0.99)) -- ",
     "computes the area-weighted covariance of (mat,map) over the FULL unclipped land_climate, whitens land pixels ",
     "and exact site points by that covariance, computes each of the ", format(nrow(land_climate), big.mark = ","),
     " land pixels' minimum Mahalanobis distance to any of the ", nrow(site_climate), " sites ONCE, then tabulates ",
     "area-weighted coverage for 3 thresholds x 3 regions (all / 95% HDR / 99% HDR).")
  t0 <- Sys.time()
  cov_result <- whittaker_mahalanobis_coverage(
    land_climate = land_climate,
    site_mat     = site_climate$mat_worldclim,
    site_map     = site_climate$map_worldclim,
    thresholds   = thresholds,
    density_grid = density_grid,
    hdr_probs    = c(0.95, 0.99)
  )
  elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  rl("completed: whittaker_mahalanobis_coverage() finished in ", elapsed, " s.")

  Sigma <- cov_result$covariance
  mu    <- cov_result$mean
  rl("computation: area-weighted covariance matrix Sigma of the global ice-free-land (mat,map) distribution -- ",
     "Sigma[mat,mat]=", signif(Sigma["mat", "mat"], 8), ", Sigma[map,map]=", signif(Sigma["map", "map"], 8),
     ", Sigma[mat,map]=Sigma[map,mat]=", signif(Sigma["mat", "map"], 8),
     ". Weighted mean: mat=", signif(mu["mat"], 6), " degC, map=", signif(mu["map"], 6), " mm/yr. ",
     "Covariance source: the AREA-WEIGHTED GLOBAL LAND-CLIMATE distribution (land_climate$weight = cos(latitude)), ",
     "NOT the FLUXNET tower distribution.")

  for (i in seq_len(nrow(cov_result$table))) {
    row <- cov_result$table[i, ]
    rl("statistic: region=", row$region, " threshold=", row$threshold,
       " area_weighted_coverage=", sprintf("%.6f", row$area_weighted_coverage),
       if (row$threshold == reference_threshold) " [REFERENCE THRESHOLD]" else "")
  }

  ref_all <- cov_result$table$area_weighted_coverage[
    cov_result$table$region == "all" & cov_result$table$threshold == reference_threshold]
  ref_95  <- cov_result$table$area_weighted_coverage[
    cov_result$table$region == "hdr_95" & cov_result$table$threshold == reference_threshold]
  ref_99  <- cov_result$table$area_weighted_coverage[
    cov_result$table$region == "hdr_99" & cov_result$table$threshold == reference_threshold]
  rl("computation: HEADLINE NUMBERS at reference threshold d=", reference_threshold, " -- ",
     "unrestricted (all global ice-free land) = ", sprintf("%.4f", ref_all), "; ",
     "within 95% HDR region = ", sprintf("%.4f", ref_95), "; ",
     "within 99% HDR region = ", sprintf("%.4f", ref_99), ".")

  # ---- Write the new CSV + meta.json -------------------------------------------
  csv_path <- "data/snapshots/whittaker_climate_coverage_mahalanobis.csv"
  rl("attempting: write ", csv_path)
  out_tbl <- cov_result$table
  out_tbl$is_reference <- out_tbl$threshold == reference_threshold
  readr::write_csv(out_tbl, csv_path)
  rl("completed: wrote ", csv_path, " (", nrow(out_tbl), " rows: 3 thresholds x 3 regions).")

  notes <- paste0(
    "FLUXNET-network climate-space coverage of global ice-free land (MAT x MAP Whittaker space), MAHALANOBIS-",
    "DISTANCE DEFINITION -- replaces the grid-cell coverage metric in data/snapshots/whittaker_climate_coverage.csv ",
    "(retired, not deleted; see that file's own .meta.json for a superseded_by note). ",
    "Metric definition: a land pixel counts as 'sampled' if its exact (MAT,MAP) position lies within Mahalanobis ",
    "distance d of at least one FLUXNET site's exact (MAT,MAP) position (site positions are NEVER snapped to a ",
    "grid cell; only the land distribution, already gridded at native WorldClim 2.5 arc-min resolution, is ",
    "aggregated). Only area-weighted coverage is reported (no cell-count coverage). ",
    "Covariance source: the AREA-WEIGHTED GLOBAL ICE-FREE-LAND climate distribution (cosine-of-latitude weights), ",
    "NOT the FLUXNET tower distribution. Sigma (2x2, mat/map order) = [[",
    signif(Sigma["mat", "mat"], 8), ", ", signif(Sigma["mat", "map"], 8), "], [",
    signif(Sigma["map", "mat"], 8), ", ", signif(Sigma["map", "map"], 8), "]]. Weighted mean (mat, map) = (",
    signif(mu["mat"], 6), ", ", signif(mu["map"], 6), "). ",
    "Thresholds: d in {0.25, 0.5, 1.0}; REFERENCE threshold = 0.5. ",
    "Regions: 'all' = full unclipped global ice-free land (denominator NOT restricted to the Figure 2 display ",
    "window -- ", sprintf("%.2f%%", 100 * frac_outside_axes), " of global land-area weight falls outside that ",
    "window and remains in the 'all' denominator, consistent with the previous session's unclipped-denominator ",
    "convention); 'hdr_95'/'hdr_99' = restricted to the existing 95%/99% highest-density-region envelopes (same ",
    "envelopes fig_whit_global_contour.png and the Figure 2 overlays draw), via .hdr_levels() on the cached ",
    "201x201 density_grid, with land pixels (not sites) snapped to their nearest grid node purely for region ",
    "classification. ",
    "Land mask: ESA CCI land cover 2015 (ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif), class 210 (water) and ",
    "220 (permanent snow/ice) excluded, class 0 (no data) excluded, all else = ice-free land; aggregated to the ",
    "WorldClim 2.5 arc-min grid (exact 15x factor) and thresholded at >=50% ice-free-land fraction per cell; ",
    "latitude < -60 deg backstop additionally excludes Antarctica. ",
    "Area weighting: cosine of latitude (WorldClim is an equal-angle, not equal-area, grid). ",
    "FLUXNET snapshot: ", snap_file, " (", nrow(shuttle_meta), " network sites; ", nrow(site_climate),
    " with known WorldClim MAT/MAP via data/snapshots/site_worldclim.csv, used as exact site points). ",
    "Non-Shuttle background data (WorldClim v2.1 BIO1/BIO12, ESA CCI LC 2015) per CLAUDE.md Hard Rule #1 -- used ",
    "only to define the climate-space envelope, not as primary Annual Paper data. ",
    "Implementation: whittaker_mahalanobis_coverage() in R/figures/fig_climate.R (whittaker_hdr_coverage(), the ",
    "prior grid-cell metric, is retained in the same file for provenance but its output is not used here). ",
    "Source script: scripts/generate_whittaker_coverage_mahalanobis.R."
  )
  write_output_metadata(
    csv_path,
    input_sources = c(snap_file, worldclim_csv, landclimate_rds, densitygrid_rds),
    notes = notes
  )
  rl("completed: wrote ", tools::file_path_sans_ext(csv_path), ".meta.json")

  # ---- Retire (do not delete) the old grid-cell coverage CSV ------------------
  old_csv  <- "data/snapshots/whittaker_climate_coverage.csv"
  old_meta <- "data/snapshots/whittaker_climate_coverage.meta.json"
  rl("attempting: mark ", old_csv, " as superseded via a note added to its existing ", old_meta,
     " (the old CSV itself is left completely untouched, not deleted or modified)")
  if (!file.exists(old_meta)) {
    rl("NOTE: ", old_meta, " not found -- cannot add a retirement note (old CSV, if present, is still left untouched). Continuing.")
  } else {
    old_meta_obj <- jsonlite::fromJSON(old_meta, simplifyVector = TRUE)
    old_meta_obj$superseded_by <- paste0(
      "data/snapshots/whittaker_climate_coverage_mahalanobis.csv (", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      "). The grid-cell coverage metric in THIS file (whittaker_climate_coverage.csv) is retired -- not deleted, ",
      "kept for provenance -- and replaced by an area-weighted Mahalanobis-distance coverage metric. Do not cite ",
      "cell_coverage/area_weighted_coverage from this file in new work; use the Mahalanobis CSV instead."
    )
    writeLines(jsonlite::toJSON(old_meta_obj, pretty = TRUE, auto_unbox = TRUE), old_meta)
    rl("completed: added superseded_by note to ", old_meta, " (", old_csv, " itself unchanged -- verified no write to that path in this script).")
  }

  invisible(list(reference = c(all = ref_all, hdr_95 = ref_95, hdr_99 = ref_99),
                table = out_tbl, covariance = Sigma, mean = mu))
}

pipeline_result <- NULL
pipeline_error <- tryCatch({
  pipeline_result <- run_pipeline()
  NULL
}, error = function(e) e)

if (is.null(pipeline_error)) {
  rl("outcome: SUCCESS -- data/snapshots/whittaker_climate_coverage_mahalanobis.csv + .meta.json written; ",
     "old whittaker_climate_coverage.csv retained and marked superseded in its .meta.json.")
  message("\nDone: data/snapshots/whittaker_climate_coverage_mahalanobis.csv + .meta.json written.")
} else {
  rl("ERROR: ", conditionMessage(pipeline_error))
  rl("outcome: FAILED -- see the last 'attempting' line above for what was in progress when the script stopped.")
}
close(run_log_con)
if (!is.null(pipeline_error)) stop(pipeline_error)
