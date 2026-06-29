## recompute_continuous_axes_30bin.R
##
## Extends the 5-axis continuous representativeness analysis to 30 bins:
##   Bin 1:     near-zero fixed cut (< 5 in axis units)
##   Bins 2-30: 29 equal-area quantile bins derived from the global KG-land
##              distribution above the near-zero cut
##
## Axes:  Biomass CCI v7, TRENDY NEE-IAV, TRENDY ET-IAV,
##         TRENDY NEE-median, TRENDY ET-median
## Networks: current_767, marconi, la_thuile, fluxnet2015
##
## Steps:
##   1. Compute 30-bin global distribution per axis (raster classify + zonal)
##   2. Classify per-site values to 30 bins; add {axis}_bin_30 column to
##      existing site CSVs (no new raster extraction)
##   3. Compute Jaccard + Hellinger; append 20 rows to
##      representativeness_metrics.csv
##
## Binning details:
##   28 interior area-weighted quantile breakpoints divide vegetated land
##   (>= near-zero cut) into 29 equal-area bins (Q_PROBS = (1:28)/29).
##   Total documented breakpoints: 29  (low_cut + 28 quantile breaks).
##   Full boundary vector: c(0, low_cut, q1..q28, Inf) — 31 values, 30 bins.

suppressPackageStartupMessages({
  library(terra)
  library(dplyr)
  library(readr)
  library(fs)
})

source("R/utils.R")
source("R/pipeline_config.R")
check_pipeline_config()

DATA_ROOT <- Sys.getenv("FLUXNET_DATA_ROOT", "data")
SNAP <- file.path(DATA_ROOT, "snapshots")
EXT  <- "data/external"

LOG_PATH <- file.path("logs",
  sprintf("recompute_30bin_%s.log", format(Sys.time(), "%Y%m%d_%H%M%S")))
fs::dir_create("logs")

log_msg <- function(...) {
  msg <- paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", ..., "\n")
  cat(msg)
  cat(msg, file = LOG_PATH, append = TRUE)
}

log_msg("=== 30-bin hybrid representativeness ===")
log_msg("Log: ", LOG_PATH)

# ---- KG raster paths ----------------------------------------------------------
KG_FINE   <- file.path(EXT, "koppen_beck2023/1991_2020/koppen_geiger_0p00833333.tif")
KG_COARSE <- file.path(EXT, "koppen_beck2023/1991_2020/koppen_geiger_0p5.tif")

for (p in c(KG_FINE, KG_COARSE)) {
  if (!file.exists(p)) stop("KG raster not found: ", p)
}

# ---- Axis configuration -------------------------------------------------------
AXES <- list(
  biomass = list(
    axis_key   = "biomass_cci_v7",
    rast_path  = file.path(EXT, "cci_biomass",
                           "ESACCI-BIOMASS-L4-AGB-MERGED-1000m-fv7.0.tif"),
    kg_res     = "fine",
    band       = 18L,
    low_cut    = 5.0,
    unit_str   = "Mg/ha",
    hist_step  = 1.0,
    hist_max   = 1000.0,
    val_col    = "biomass_value_mg_ha",
    bin_col_7  = "biomass_bin",
    bin_col_30 = "biomass_bin_30",
    site_base  = "site_biomass_cci_v7",
    glob_out   = file.path(SNAP, "biomass_cci_v7_global_distribution_30bin.csv")
  ),
  nee_iav = list(
    axis_key   = "trendy_nee_iav",
    rast_path  = file.path(EXT, "trendy/derived/trendy_nee_iav.tif"),
    kg_res     = "coarse",
    band       = NULL,
    low_cut    = 5.0,
    unit_str   = "gC m-2 yr-1",
    hist_step  = 0.1,
    hist_max   = 1000.0,
    val_col    = "trendy_nee_iav_value",
    bin_col_7  = "trendy_nee_iav_bin",
    bin_col_30 = "trendy_nee_iav_bin_30",
    site_base  = "site_trendy_nee_iav",
    glob_out   = file.path(SNAP, "trendy_nee_iav_global_distribution_30bin.csv")
  ),
  et_iav = list(
    axis_key   = "trendy_et_iav",
    rast_path  = file.path(EXT, "trendy/derived/trendy_et_iav.tif"),
    kg_res     = "coarse",
    band       = NULL,
    low_cut    = 5.0,
    unit_str   = "mm yr-1",
    hist_step  = 0.1,
    hist_max   = 1000.0,
    val_col    = "trendy_et_iav_value",
    bin_col_7  = "trendy_et_iav_bin",
    bin_col_30 = "trendy_et_iav_bin_30",
    site_base  = "site_trendy_et_iav",
    glob_out   = file.path(SNAP, "trendy_et_iav_global_distribution_30bin.csv")
  ),
  nee_median = list(
    axis_key   = "trendy_nee_median",
    rast_path  = file.path(EXT, "trendy/derived/trendy_nee_median.tif"),
    kg_res     = "coarse",
    band       = NULL,
    low_cut    = 5.0,
    unit_str   = "gC m-2 yr-1",
    hist_step  = 0.1,
    hist_max   = 1000.0,
    val_col    = "trendy_nee_median_value",
    bin_col_7  = "trendy_nee_median_bin",
    bin_col_30 = "trendy_nee_median_bin_30",
    site_base  = "site_trendy_nee_median",
    glob_out   = file.path(SNAP, "trendy_nee_median_global_distribution_30bin.csv")
  ),
  et_median = list(
    axis_key   = "trendy_et_median",
    rast_path  = file.path(EXT, "trendy/derived/trendy_et_median.tif"),
    kg_res     = "coarse",
    band       = NULL,
    low_cut    = 5.0,
    unit_str   = "mm yr-1",
    hist_step  = 0.1,
    hist_max   = 1000.0,
    val_col    = "trendy_et_median_value",
    bin_col_7  = "trendy_et_median_bin",
    bin_col_30 = "trendy_et_median_bin_30",
    site_base  = "site_trendy_et_median",
    glob_out   = file.path(SNAP, "trendy_et_median_global_distribution_30bin.csv")
  )
)

# ---- Network config -----------------------------------------------------------
NETWORKS  <- c("current_767", "marconi", "la_thuile", "fluxnet2015")
NET_SIZES <- c(current_767 = 767L, marconi = 35L,
               la_thuile   = 252L, fluxnet2015 = 212L)

site_csv_path <- function(site_base, network) {
  suffix <- if (network == "current_767") "" else paste0("_", network)
  file.path(SNAP, paste0(site_base, suffix, ".csv"))
}

# ---- Jaccard + Hellinger ------------------------------------------------------
compute_repr <- function(p, q) {
  p[is.na(p)] <- 0; q[is.na(q)] <- 0
  list(
    weighted_jaccard   = sum(pmin(p, q)) / sum(pmax(p, q)),
    hellinger_distance = (1 / sqrt(2)) * sqrt(sum((sqrt(p) - sqrt(q))^2))
  )
}

# ---- Preload KG rasters + cell areas (done once each) -------------------------
log_msg("")
log_msg("=== Loading KG rasters and computing cell areas ===")

kg_fine   <- terra::rast(KG_FINE)
kg_coarse <- terra::rast(KG_COARSE)

t0 <- proc.time()
cell_areas_fine <- terra::cellSize(kg_fine, mask = TRUE, unit = "km")
log_msg("  Fine cell areas computed: ",
        round((proc.time() - t0)[["elapsed"]], 1), " s")

t0 <- proc.time()
cell_areas_coarse <- terra::cellSize(kg_coarse, mask = TRUE, unit = "km")
log_msg("  Coarse cell areas computed: ",
        round((proc.time() - t0)[["elapsed"]], 1), " s")

total_land_fine   <- terra::global(cell_areas_fine,   fun = "sum", na.rm = TRUE)[[1]]
total_land_coarse <- terra::global(cell_areas_coarse, fun = "sum", na.rm = TRUE)[[1]]
log_msg("  Fine KG land:   ", format(round(total_land_fine),   big.mark = ","), " km2")
log_msg("  Coarse KG land: ", format(round(total_land_coarse), big.mark = ","), " km2")

# ============================================================================
log_msg("")
log_msg("=== Step 1: 30-bin global distributions ===")
# ============================================================================

all_breakpoints <- list()   # axis_key -> 29-element documented break vector
all_bin_breaks  <- list()   # axis_key -> 31-element full boundary vector
all_dist_dfs    <- list()   # axis_key -> global distribution data frame

for (ax_name in names(AXES)) {
  ax <- AXES[[ax_name]]
  log_msg("")
  log_msg("+++ ", ax$axis_key, " +++")

  if (!file.exists(ax$rast_path))
    stop("Raster not found: ", ax$rast_path)

  log_msg("  Loading raster ...")
  r_all <- terra::rast(ax$rast_path)
  if (!is.null(ax$band)) {
    lyr_names <- names(r_all)
    r_all     <- r_all[[ax$band]]
    log_msg("  Band ", ax$band, ": '", lyr_names[ax$band], "'")
  }

  kg_rast    <- if (ax$kg_res == "fine") kg_fine    else kg_coarse
  cell_areas <- if (ax$kg_res == "fine") cell_areas_fine else cell_areas_coarse
  total_land_km2 <- if (ax$kg_res == "fine") total_land_fine else total_land_coarse

  if (ax$kg_res == "fine") {
    log_msg("  Resampling to KG fine grid (bilinear) ...")
    t0 <- proc.time()
    r_aligned <- terra::resample(r_all, kg_rast, method = "bilinear",
                                 threads = TRUE)
    log_msg("  Resample: ", round((proc.time() - t0)[["elapsed"]], 1), " s")
    # KG-land pixels with no biomass data -> treat as near-zero (bin 1)
    r_land <- terra::ifel(!is.na(kg_rast) & is.na(r_aligned), 0, r_aligned)
  } else {
    r_land <- terra::mask(r_all, kg_rast)
    # Floor negative values to 0 (IAV values are always >= 0; guard clause)
    r_land <- terra::ifel(!is.na(r_land) & r_land < 0, 0, r_land)
  }
  rm(r_all); gc(verbose = FALSE)

  # -- Fine histogram above low_cut ------------------------------------------
  hist_lo  <- seq(ax$low_cut, ax$hist_max - ax$hist_step, by = ax$hist_step)
  hist_hi  <- hist_lo + ax$hist_step
  hist_ids <- seq_along(hist_lo)
  catch_id <- max(hist_ids) + 1L
  hist_rcl <- rbind(
    cbind(hist_lo, hist_hi, as.numeric(hist_ids)),
    c(ax$hist_max, 1e9, as.numeric(catch_id))
  )

  r_veg <- terra::ifel(r_land >= ax$low_cut, r_land, NA)

  log_msg("  Fine histogram classify (",
          length(hist_lo), " bins @ ", ax$hist_step, " step) ...")
  t0 <- proc.time()
  r_hist <- terra::classify(r_veg, hist_rcl, right = FALSE,
                             include.lowest = TRUE)
  log_msg("  Classify: ", round((proc.time() - t0)[["elapsed"]], 1), " s")

  log_msg("  Zonal sum (fine histogram) ...")
  t0 <- proc.time()
  hist_areas_df <- terra::zonal(cell_areas, r_hist, fun = "sum", na.rm = TRUE)
  log_msg("  Zonal: ", round((proc.time() - t0)[["elapsed"]], 1), " s")
  names(hist_areas_df) <- c("bin_id", "area_km2")
  hist_areas_df <- hist_areas_df[!is.na(hist_areas_df$bin_id), ]

  bin_lo_vec            <- c(hist_lo, ax$hist_max)
  hist_areas_df$lo      <- bin_lo_vec[hist_areas_df$bin_id]
  hist_areas_df         <- hist_areas_df[order(hist_areas_df$lo), ]
  hist_areas_df$cum_area <- cumsum(hist_areas_df$area_km2)
  total_veg_area        <- tail(hist_areas_df$cum_area, 1L)
  log_msg("  Vegetated land (>= ", ax$low_cut, " ", ax$unit_str, "): ",
          format(round(total_veg_area), big.mark = ","), " km2 (",
          round(100 * total_veg_area / total_land_km2, 1), "% of KG land)")

  # -- 28 equal-area quantile breakpoints (-> 29 equal-area quantile bins) ----
  # Q_PROBS[k] = k/29 for k=1..28 divides vegetated area into 29 equal parts.
  Q_PROBS  <- (1:28) / 29
  q_breaks <- vapply(Q_PROBS, function(f) {
    target <- f * total_veg_area
    idx    <- which(hist_areas_df$cum_area >= target)[1L]
    hist_areas_df$lo[idx] + ax$hist_step   # upper edge of the crossing bin
  }, numeric(1))
  q_breaks <- round(q_breaks, 1)

  # 29 documented breakpoints = low_cut + 28 interior quantile breaks
  documented_breaks <- c(ax$low_cut, q_breaks)
  all_breakpoints[[ax$axis_key]] <- documented_breaks
  log_msg("  29 breakpoints (", ax$unit_str, "): ",
          paste(sprintf("%.1f", documented_breaks), collapse = ", "))

  # Full boundary vector: 31 values defining 30 bins
  BIN_BREAKS_FULL <- c(0, documented_breaks, 1e9)
  all_bin_breaks[[ax$axis_key]] <- BIN_BREAKS_FULL

  lower_b       <- BIN_BREAKS_FULL[-length(BIN_BREAKS_FULL)]   # 30 lower bounds
  upper_b_raw   <- BIN_BREAKS_FULL[-1]                         # 30 upper bounds (last=1e9)
  bin_max_disp  <- c(documented_breaks, NA_real_)              # display: last bin open

  # -- Classify full land raster to 30 bins ----------------------------------
  rcl_30 <- cbind(lower_b, upper_b_raw, seq_len(30L))

  log_msg("  Classifying land pixels to 30 bins ...")
  t0 <- proc.time()
  r_bins30 <- terra::classify(r_land, rcl_30, right = FALSE,
                               include.lowest = TRUE)
  log_msg("  Classify: ", round((proc.time() - t0)[["elapsed"]], 1), " s")

  log_msg("  Zonal sum (30 bins) ...")
  t0 <- proc.time()
  zone_areas <- terra::zonal(cell_areas, r_bins30, fun = "sum", na.rm = TRUE)
  log_msg("  Zonal: ", round((proc.time() - t0)[["elapsed"]], 1), " s")
  names(zone_areas) <- c("bin", "global_land_area_km2")
  zone_areas <- zone_areas[!is.na(zone_areas$bin) &
                              zone_areas$bin %in% 1:30, ]

  rm(r_land, r_veg, r_hist, r_bins30); gc(verbose = FALSE)

  # -- Bin labels -------------------------------------------------------------
  fmt_q <- function(x) sprintf("%.1f", x)
  bin_labels <- character(30L)
  bin_labels[1L] <- paste0("0–", fmt_q(ax$low_cut), " ", ax$unit_str)
  for (k in 2:29) {
    bin_labels[k] <- paste0(fmt_q(lower_b[k]), "–",
                             fmt_q(bin_max_disp[k]), " ", ax$unit_str)
  }
  bin_labels[30L] <- paste0(">", fmt_q(lower_b[30L]), " ", ax$unit_str)

  # -- Build and save global distribution data frame -------------------------
  dist_df <- data.frame(
    bin       = 1:30,
    bin_label = bin_labels,
    min_value = lower_b,
    max_value = bin_max_disp,
    stringsAsFactors = FALSE
  ) |>
    dplyr::left_join(zone_areas, by = "bin") |>
    dplyr::mutate(
      global_land_area_km2 = dplyr::coalesce(global_land_area_km2, 0.0),
      global_land_fraction = global_land_area_km2 / total_land_km2
    )

  all_dist_dfs[[ax$axis_key]] <- dist_df

  readr::write_csv(dist_df, ax$glob_out)
  write_output_metadata(
    ax$glob_out,
    input_sources = c(ax$rast_path,
                      if (ax$kg_res == "fine") KG_FINE else KG_COARSE),
    notes = paste0(
      "30-bin hybrid global distribution for ", ax$axis_key, ". ",
      "Bin 1: near-zero (< ", ax$low_cut, " ", ax$unit_str, "). ",
      "Bins 2–30: 29 equal-area quantile bins above ", ax$low_cut,
      " ", ax$unit_str, " (Q_PROBS = (1:28)/29). ",
      "KG land mask: ", basename(if (ax$kg_res == "fine") KG_FINE else KG_COARSE),
      ". Total KG land area: ",
      format(round(total_land_km2), big.mark = ","), " km2. ",
      "29 documented breakpoints (", ax$unit_str, "): ",
      paste(sprintf("%.1f", documented_breaks), collapse = ", "), "."
    )
  )
  log_msg("  Saved: ", basename(ax$glob_out))
}

# ============================================================================
log_msg("")
log_msg("=== Step 2: Per-site 30-bin classification ===")
# ============================================================================

for (ax_name in names(AXES)) {
  ax              <- AXES[[ax_name]]
  BIN_BREAKS_FULL <- all_bin_breaks[[ax$axis_key]]
  # findInterval breaks: the 30 lower bounds (excludes the trailing 1e9)
  breaks_for_fi   <- BIN_BREAKS_FULL[-length(BIN_BREAKS_FULL)]
  doc_bp          <- all_breakpoints[[ax$axis_key]]

  log_msg("")
  log_msg("+++ ", ax$axis_key, " +++")

  for (net in NETWORKS) {
    sp <- site_csv_path(ax$site_base, net)
    if (!file.exists(sp)) {
      log_msg("  SKIP (not found): ", basename(sp))
      next
    }

    site_df <- readr::read_csv(sp, show_col_types = FALSE)
    raw_val <- site_df[[ax$val_col]]

    # NA -> 0 (near-zero/bare); negative -> 0 (IAV values are always >= 0)
    val_for_bin <- dplyr::if_else(is.na(raw_val) | raw_val < 0,
                                  0.0, as.double(raw_val))

    bin_30 <- findInterval(val_for_bin, breaks_for_fi,
                            left.open = FALSE, rightmost.closed = FALSE)
    bin_30[bin_30 < 1L]  <- 1L
    bin_30[bin_30 > 30L] <- 30L

    site_df[[ax$bin_col_30]] <- as.integer(bin_30)

    readr::write_csv(site_df, sp)

    write_output_metadata(
      sp,
      input_sources = c(ax$rast_path,
                        if (ax$kg_res == "fine") KG_FINE else KG_COARSE),
      notes = paste0(
        "Per-site ", ax$axis_key, " site classifications (all networks). ",
        "Column '", ax$bin_col_7,  "': existing 7-bin hybrid (unchanged). ",
        "Column '", ax$bin_col_30, "': new 30-bin hybrid added. ",
        "30-bin scheme: bin 1 = < ", ax$low_cut, " ", ax$unit_str,
        "; bins 2–30 = 29 equal-area quantile bins. ",
        "29 breakpoints (", ax$unit_str, "): ",
        paste(sprintf("%.1f", doc_bp), collapse = ", "), "."
      )
    )

    n_sites <- nrow(site_df)
    bin1_n  <- sum(site_df[[ax$bin_col_30]] == 1L, na.rm = TRUE)
    bin_rng <- range(site_df[[ax$bin_col_30]], na.rm = TRUE)
    log_msg("  ", net, " (n=", n_sites, "): bin_30 range ",
            bin_rng[1], "–", bin_rng[2],
            ";  bin1=", bin1_n, " (", round(100 * bin1_n / n_sites, 1), "%)")
  }
}

# ============================================================================
log_msg("")
log_msg("=== Step 3: Representativeness metrics (30-bin) ===")
# ============================================================================

metrics_path     <- file.path(SNAP, "representativeness_metrics.csv")
existing_metrics <- readr::read_csv(metrics_path, show_col_types = FALSE)
log_msg("Existing rows: ", nrow(existing_metrics))

new_rows <- list()

for (ax_name in names(AXES)) {
  ax      <- AXES[[ax_name]]
  dist_df <- all_dist_dfs[[ax$axis_key]]
  p_global <- dist_df$global_land_fraction   # length-30

  for (net in NETWORKS) {
    sp <- site_csv_path(ax$site_base, net)
    if (!file.exists(sp)) next

    site_df  <- readr::read_csv(sp, show_col_types = FALSE)
    n_sites  <- nrow(site_df)
    bin_col  <- ax$bin_col_30

    q_net <- vapply(1:30, function(b)
      sum(site_df[[bin_col]] == b, na.rm = TRUE) / n_sites,
      numeric(1))

    repr <- compute_repr(p_global, q_net)

    new_rows[[length(new_rows) + 1L]] <- data.frame(
      axis               = ax$axis_key,
      aggregation_level  = "30bin_hybrid",
      n_classes          = 30L,
      weighted_jaccard   = repr$weighted_jaccard,
      hellinger_distance = repr$hellinger_distance,
      network            = net,
      n_sites            = as.integer(NET_SIZES[[net]]),
      stringsAsFactors   = FALSE
    )

    log_msg("  ", ax$axis_key, " / ", net,
            ": J=", round(repr$weighted_jaccard, 4),
            "  H=", round(repr$hellinger_distance, 4))
  }
}

new_metrics_df   <- dplyr::bind_rows(new_rows)
updated_metrics  <- dplyr::bind_rows(existing_metrics, new_metrics_df)
readr::write_csv(updated_metrics, metrics_path)
log_msg("Updated metrics: ", nrow(existing_metrics), " + ", nrow(new_metrics_df),
        " = ", nrow(updated_metrics), " rows")

# ============================================================================
log_msg("")
log_msg("=== Summary ===")
log_msg("")
# ============================================================================

# Print breakpoints table
log_msg("Quantile breakpoints (29 per axis, including low_cut = 5):")
for (ax_name in names(AXES)) {
  ax  <- AXES[[ax_name]]
  bps <- all_breakpoints[[ax$axis_key]]
  log_msg("  ", ax$axis_key, " (", ax$unit_str, "):")
  log_msg("    ", paste(sprintf("%.1f", bps), collapse = ", "))
}

log_msg("")
log_msg("Jaccard comparison: 7-bin vs 30-bin (current_767):")
log_msg(sprintf("  %-25s  %8s  %8s  %8s", "axis", "J(7-bin)", "J(30-bin)", "delta"))
for (ax_name in names(AXES)) {
  ax <- AXES[[ax_name]]
  j7  <- existing_metrics |>
    dplyr::filter(axis == ax$axis_key,
                  aggregation_level == "7bin_hybrid",
                  network == "current_767") |>
    dplyr::pull(weighted_jaccard)
  j30 <- new_metrics_df |>
    dplyr::filter(axis == ax$axis_key,
                  network == "current_767") |>
    dplyr::pull(weighted_jaccard)
  if (length(j7) && length(j30)) {
    log_msg(sprintf("  %-25s  %8.4f  %8.4f  %+8.4f",
                    ax$axis_key, j7, j30, j30 - j7))
  }
}

log_msg("")
log_msg("Done!")
log_msg("Log: ", LOG_PATH)
