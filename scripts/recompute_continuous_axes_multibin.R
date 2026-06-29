## recompute_continuous_axes_multibin.R
##
## Extends continuous-axis representativeness to 12-bin, 18-bin, and 20-bin
## hybrid schemes for all five continuous axes × four networks.
##
## Scheme: bin 1 = near-zero (< 5); bins 2..N = (N-1) equal-area quantile bins
## derived from the global KG-land distribution above the near-zero cut.
## Near-zero cut = 5 for all axes (matches 7-bin and 30-bin schemes).
##
## BIN_COUNTS = c(12L, 18L, 20L)
##   12-bin: bin 1 + 11 quantile bins, Q_PROBS = (1:10)/11 (10 interior breaks)
##   18-bin: bin 1 + 17 quantile bins, Q_PROBS = (1:16)/17 (16 interior breaks)
##   20-bin: bin 1 + 19 quantile bins, Q_PROBS = (1:18)/19 (18 interior breaks)
##
## Outputs per axis × bin count:
##   global distribution CSV + meta.json (15 new files)
##   {axis}_bin_N column added to each per-site CSV (20 CSVs × 3 columns)
##   60 new rows in representativeness_metrics.csv (96 → 156)
##
## Efficiency: fine histogram computed once per axis; breakpoints and raster
## classifications for all 3 bin counts derived from that single histogram.

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
  sprintf("recompute_multibin_%s.log", format(Sys.time(), "%Y%m%d_%H%M%S")))
fs::dir_create("logs")

log_msg <- function(...) {
  msg <- paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", ..., "\n")
  cat(msg)
  cat(msg, file = LOG_PATH, append = TRUE)
}

log_msg("=== Multi-bin hybrid representativeness (12, 18, 20) ===")
log_msg("Log: ", LOG_PATH)

# ---- Bin counts --------------------------------------------------------------
# For N bins: (N-1) quantile bins; (N-2) interior breaks at Q_PROBS = (1:(N-2))/(N-1)
BIN_COUNTS <- c(12L, 18L, 20L)

# ---- KG raster paths ---------------------------------------------------------
KG_FINE   <- file.path(EXT, "koppen_beck2023/1991_2020/koppen_geiger_0p00833333.tif")
KG_COARSE <- file.path(EXT, "koppen_beck2023/1991_2020/koppen_geiger_0p5.tif")

for (p in c(KG_FINE, KG_COARSE)) {
  if (!file.exists(p)) stop("KG raster not found: ", p)
}

# ---- Axis configuration ------------------------------------------------------
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
    bin_prefix = "biomass_bin",
    site_base  = "site_biomass_cci_v7"
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
    bin_prefix = "trendy_nee_iav_bin",
    site_base  = "site_trendy_nee_iav"
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
    bin_prefix = "trendy_et_iav_bin",
    site_base  = "site_trendy_et_iav"
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
    bin_prefix = "trendy_nee_median_bin",
    site_base  = "site_trendy_nee_median"
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
    bin_prefix = "trendy_et_median_bin",
    site_base  = "site_trendy_et_median"
  )
)

# ---- Network config ----------------------------------------------------------
NETWORKS   <- c("current_767", "marconi", "la_thuile", "fluxnet2015")
NET_SIZES  <- c(current_767 = 767L, marconi = 35L,
                la_thuile   = 252L, fluxnet2015 = 212L)

site_csv_path <- function(site_base, network) {
  suffix <- if (network == "current_767") "" else paste0("_", network)
  file.path(SNAP, paste0(site_base, suffix, ".csv"))
}

glob_dist_path <- function(axis_key, n_bins) {
  file.path(SNAP, sprintf("%s_global_distribution_%dbin.csv", axis_key, n_bins))
}

# ---- Jaccard + Hellinger -----------------------------------------------------
compute_repr <- function(p, q) {
  p[is.na(p)] <- 0; q[is.na(q)] <- 0
  list(
    weighted_jaccard   = sum(pmin(p, q)) / sum(pmax(p, q)),
    hellinger_distance = (1 / sqrt(2)) * sqrt(sum((sqrt(p) - sqrt(q))^2))
  )
}

# ---- Quantile breakpoints from histogram ------------------------------------
# Returns (N-2) interior breaks → N-1 equal-area quantile bins above low_cut.
derive_breaks <- function(hist_areas_df, total_veg_area, n_bins, hist_step) {
  n_quantile <- n_bins - 1L            # quantile bins (not counting near-zero)
  n_interior <- n_quantile - 1L        # interior breaks
  q_probs    <- (seq_len(n_interior)) / n_quantile
  breaks <- vapply(q_probs, function(f) {
    target <- f * total_veg_area
    idx    <- which(hist_areas_df$cum_area >= target)[1L]
    hist_areas_df$lo[idx] + hist_step
  }, numeric(1))
  round(breaks, 1)
}

# ---- Preload KG rasters + cell areas -----------------------------------------
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
log_msg("=== Step 1: Global distributions for all axes × bin counts ===")
# ============================================================================

# Store breakpoints for later use in per-site classification
# all_bp[[axis_key]][[n_bins]] = c(low_cut, q1, ..., q_{N-2})  (N-1 values)
all_bp <- list()

for (ax_name in names(AXES)) {
  ax <- AXES[[ax_name]]
  log_msg("")
  log_msg("+++ ", ax$axis_key, " +++")

  if (!file.exists(ax$rast_path)) stop("Raster not found: ", ax$rast_path)

  # -- Load + KG-mask raster (once per axis) ----------------------------------
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
    r_aligned <- terra::resample(r_all, kg_rast, method = "bilinear", threads = TRUE)
    log_msg("  Resample: ", round((proc.time() - t0)[["elapsed"]], 1), " s")
    r_land <- terra::ifel(!is.na(kg_rast) & is.na(r_aligned), 0, r_aligned)
  } else {
    r_land <- terra::mask(r_all, kg_rast)
    r_land <- terra::ifel(!is.na(r_land) & r_land < 0, 0, r_land)
  }
  rm(r_all); gc(verbose = FALSE)

  # -- Fine histogram above low_cut (computed once; shared across all bin counts)
  hist_lo  <- seq(ax$low_cut, ax$hist_max - ax$hist_step, by = ax$hist_step)
  hist_hi  <- hist_lo + ax$hist_step
  hist_ids <- seq_along(hist_lo)
  catch_id <- max(hist_ids) + 1L
  hist_rcl <- rbind(
    cbind(hist_lo, hist_hi, as.numeric(hist_ids)),
    c(ax$hist_max, 1e9, as.numeric(catch_id))
  )

  r_veg <- terra::ifel(r_land >= ax$low_cut, r_land, NA)
  log_msg("  Fine histogram classify (", length(hist_lo), " bins @ ",
          ax$hist_step, " step) ...")
  t0 <- proc.time()
  r_hist <- terra::classify(r_veg, hist_rcl, right = FALSE, include.lowest = TRUE)
  log_msg("  Classify: ", round((proc.time() - t0)[["elapsed"]], 1), " s")

  log_msg("  Zonal sum (fine histogram) ...")
  t0 <- proc.time()
  hist_areas_df <- terra::zonal(cell_areas, r_hist, fun = "sum", na.rm = TRUE)
  log_msg("  Zonal: ", round((proc.time() - t0)[["elapsed"]], 1), " s")
  names(hist_areas_df) <- c("bin_id", "area_km2")
  hist_areas_df <- hist_areas_df[!is.na(hist_areas_df$bin_id), ]

  bin_lo_vec             <- c(hist_lo, ax$hist_max)
  hist_areas_df$lo       <- bin_lo_vec[hist_areas_df$bin_id]
  hist_areas_df          <- hist_areas_df[order(hist_areas_df$lo), ]
  hist_areas_df$cum_area <- cumsum(hist_areas_df$area_km2)
  total_veg_area         <- tail(hist_areas_df$cum_area, 1L)
  log_msg("  Vegetated land (>= ", ax$low_cut, " ", ax$unit_str, "): ",
          format(round(total_veg_area), big.mark = ","), " km2 (",
          round(100 * total_veg_area / total_land_km2, 1), "% of KG land)")

  rm(r_veg, r_hist); gc(verbose = FALSE)

  # Initialise storage for this axis
  all_bp[[ax$axis_key]] <- list()
  fmt_q <- function(x) sprintf("%.1f", x)

  # -- Derive breaks + classify + save for each bin count ---------------------
  for (n_bins in BIN_COUNTS) {
    log_msg("  --- ", n_bins, "-bin ---")

    q_breaks   <- derive_breaks(hist_areas_df, total_veg_area, n_bins, ax$hist_step)
    doc_bp     <- c(ax$low_cut, q_breaks)                  # N-1 documented breakpoints
    all_bp[[ax$axis_key]][[as.character(n_bins)]] <- doc_bp

    BIN_BREAKS_FULL <- c(0, doc_bp, 1e9)                   # N+1 boundary values
    lower_b      <- BIN_BREAKS_FULL[-length(BIN_BREAKS_FULL)]
    upper_b_raw  <- BIN_BREAKS_FULL[-1]
    bin_max_disp <- c(doc_bp, NA_real_)

    # Check for ceiling collapse (bins with equal breakpoints, e.g. ET-median hist_max)
    n_collapsed <- sum(duplicated(doc_bp))
    if (n_collapsed > 0)
      log_msg("    Note: ", n_collapsed, " collapsed breakpoint(s) at hist_max ceiling")

    log_msg("  Breakpoints (", ax$unit_str, "): ",
            paste(sprintf("%.1f", doc_bp), collapse = ", "))

    # Classify to N bins
    rcl_N <- cbind(lower_b, upper_b_raw, seq_len(n_bins))
    t0 <- proc.time()
    r_bins_N <- terra::classify(r_land, rcl_N, right = FALSE, include.lowest = TRUE)
    log_msg("    Classify: ", round((proc.time() - t0)[["elapsed"]], 1), " s")

    t0 <- proc.time()
    zone_areas <- terra::zonal(cell_areas, r_bins_N, fun = "sum", na.rm = TRUE)
    log_msg("    Zonal: ", round((proc.time() - t0)[["elapsed"]], 1), " s")
    names(zone_areas) <- c("bin", "global_land_area_km2")
    zone_areas <- zone_areas[!is.na(zone_areas$bin) & zone_areas$bin %in% seq_len(n_bins), ]
    rm(r_bins_N)

    # Bin labels
    bin_labels <- character(n_bins)
    bin_labels[1L] <- paste0("0–", fmt_q(ax$low_cut), " ", ax$unit_str)
    for (k in seq(2, n_bins - 1)) {
      bin_labels[k] <- paste0(fmt_q(lower_b[k]), "–",
                               fmt_q(bin_max_disp[k]), " ", ax$unit_str)
    }
    bin_labels[n_bins] <- paste0(">", fmt_q(lower_b[n_bins]), " ", ax$unit_str)

    dist_df <- data.frame(
      bin       = seq_len(n_bins),
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

    out_csv <- glob_dist_path(ax$axis_key, n_bins)
    readr::write_csv(dist_df, out_csv)
    write_output_metadata(
      out_csv,
      input_sources = c(ax$rast_path,
                        if (ax$kg_res == "fine") KG_FINE else KG_COARSE),
      notes = paste0(
        n_bins, "-bin hybrid global distribution for ", ax$axis_key, ". ",
        "Bin 1: near-zero (< ", ax$low_cut, " ", ax$unit_str, "). ",
        "Bins 2–", n_bins, ": ", n_bins - 1L, " equal-area quantile bins. ",
        "Q_PROBS = (1:", n_bins - 2L, ")/", n_bins - 1L, ". ",
        "KG land mask: ", basename(if (ax$kg_res == "fine") KG_FINE else KG_COARSE),
        ". Total KG land: ",
        format(round(total_land_km2), big.mark = ","), " km2. ",
        n_bins - 1L, " documented breakpoints (", ax$unit_str, "): ",
        paste(sprintf("%.1f", doc_bp), collapse = ", "), "."
      )
    )
    log_msg("    Saved: ", basename(out_csv))
  }

  rm(r_land); gc(verbose = FALSE)
}

# ============================================================================
log_msg("")
log_msg("=== Step 2: Per-site classification (all axes × networks × bin counts) ===")
# ============================================================================

for (ax_name in names(AXES)) {
  ax <- AXES[[ax_name]]
  log_msg("")
  log_msg("+++ ", ax$axis_key, " +++")

  # Precompute findInterval breaks for each bin count (30 lower-bound vectors)
  fi_breaks_list <- lapply(as.character(BIN_COUNTS), function(bc) {
    doc_bp  <- all_bp[[ax$axis_key]][[bc]]
    n_bins  <- as.integer(bc)
    BIN_BREAKS_FULL <- c(0, doc_bp, 1e9)
    BIN_BREAKS_FULL[-length(BIN_BREAKS_FULL)]   # N lower bounds for findInterval
  })
  names(fi_breaks_list) <- as.character(BIN_COUNTS)

  for (net in NETWORKS) {
    sp <- site_csv_path(ax$site_base, net)
    if (!file.exists(sp)) {
      log_msg("  SKIP (not found): ", basename(sp))
      next
    }

    site_df <- readr::read_csv(sp, show_col_types = FALSE)
    raw_val <- site_df[[ax$val_col]]
    val_for_bin <- dplyr::if_else(is.na(raw_val) | raw_val < 0, 0.0, as.double(raw_val))

    # Add all three bin columns in one pass (single read/write per CSV)
    for (bc in as.character(BIN_COUNTS)) {
      n_bins       <- as.integer(bc)
      bin_col_name <- paste0(ax$bin_prefix, "_", bc)
      breaks_fi    <- fi_breaks_list[[bc]]

      bin_vec <- findInterval(val_for_bin, breaks_fi,
                              left.open = FALSE, rightmost.closed = FALSE)
      bin_vec[bin_vec < 1L]      <- 1L
      bin_vec[bin_vec > n_bins]  <- n_bins
      site_df[[bin_col_name]]    <- as.integer(bin_vec)
    }

    readr::write_csv(site_df, sp)

    # Summarise for log
    log_parts <- vapply(as.character(BIN_COUNTS), function(bc) {
      col <- paste0(ax$bin_prefix, "_", bc)
      b1  <- sum(site_df[[col]] == 1L, na.rm = TRUE)
      sprintf("%s-bin: bin1=%d", bc, b1)
    }, character(1))

    write_output_metadata(
      sp,
      input_sources = c(ax$rast_path,
                        if (ax$kg_res == "fine") KG_FINE else KG_COARSE),
      notes = paste0(
        "Per-site ", ax$axis_key, " site classifications (all networks). ",
        "Existing columns: ", ax$bin_prefix, " (7-bin), ",
        ax$bin_prefix, "_30 (30-bin). ",
        "New columns: ", paste(paste0(ax$bin_prefix, "_", BIN_COUNTS), collapse = ", "),
        ". Near-zero cut = ", ax$low_cut, " ", ax$unit_str, "."
      )
    )

    log_msg("  ", net, " (n=", nrow(site_df), "): ",
            paste(log_parts, collapse = "; "))
  }
}

# ============================================================================
log_msg("")
log_msg("=== Step 3: Representativeness metrics (12/18/20-bin) ===")
# ============================================================================

metrics_path     <- file.path(SNAP, "representativeness_metrics.csv")
existing_metrics <- readr::read_csv(metrics_path, show_col_types = FALSE)
log_msg("Existing rows: ", nrow(existing_metrics))

new_rows <- list()

for (ax_name in names(AXES)) {
  ax <- AXES[[ax_name]]

  for (n_bins in BIN_COUNTS) {
    bc       <- as.character(n_bins)
    dist_df  <- readr::read_csv(glob_dist_path(ax$axis_key, n_bins),
                                 show_col_types = FALSE)
    p_global <- dist_df$global_land_fraction
    bin_col  <- paste0(ax$bin_prefix, "_", bc)
    agg_lbl  <- paste0(bc, "bin_hybrid")

    for (net in NETWORKS) {
      sp <- site_csv_path(ax$site_base, net)
      if (!file.exists(sp)) next

      site_df <- readr::read_csv(sp, show_col_types = FALSE)
      n_sites <- nrow(site_df)

      q_net <- vapply(seq_len(n_bins), function(b)
        sum(site_df[[bin_col]] == b, na.rm = TRUE) / n_sites,
        numeric(1))

      repr <- compute_repr(p_global, q_net)

      new_rows[[length(new_rows) + 1L]] <- data.frame(
        axis               = ax$axis_key,
        aggregation_level  = agg_lbl,
        n_classes          = n_bins,
        weighted_jaccard   = repr$weighted_jaccard,
        hellinger_distance = repr$hellinger_distance,
        network            = net,
        n_sites            = as.integer(NET_SIZES[[net]]),
        stringsAsFactors   = FALSE
      )

      log_msg("  ", ax$axis_key, " / ", agg_lbl, " / ", net,
              ": J=", round(repr$weighted_jaccard, 4),
              "  H=", round(repr$hellinger_distance, 4))
    }
  }
}

new_metrics_df  <- dplyr::bind_rows(new_rows)
updated_metrics <- dplyr::bind_rows(existing_metrics, new_metrics_df)
readr::write_csv(updated_metrics, metrics_path)
log_msg("Updated metrics: ", nrow(existing_metrics), " + ", nrow(new_metrics_df),
        " = ", nrow(updated_metrics), " rows")

# ============================================================================
log_msg("")
log_msg("=== Step 4: Summary ===")
# ============================================================================

log_msg("")
log_msg("Breakpoints per axis × bin count:")
for (ax_name in names(AXES)) {
  ax <- AXES[[ax_name]]
  log_msg("  ", ax$axis_key, " (", ax$unit_str, "):")
  for (bc in as.character(BIN_COUNTS)) {
    bp <- all_bp[[ax$axis_key]][[bc]]
    n_collapsed <- sum(duplicated(bp))
    collapse_note <- if (n_collapsed > 0)
      sprintf(" [%d collapsed at ceiling %.1f]", n_collapsed, ax$hist_max) else ""
    log_msg("    ", bc, "-bin: ",
            paste(sprintf("%.1f", bp), collapse = ", "),
            collapse_note)
  }
}

log_msg("")
log_msg("Jaccard by bin count (current_767):")
log_msg(sprintf("  %-25s  %8s  %8s  %8s  %8s  %8s",
                "axis", "7-bin", "12-bin", "18-bin", "20-bin", "30-bin"))
for (ax_name in names(AXES)) {
  ax <- AXES[[ax_name]]
  get_j <- function(agg_lbl) {
    v <- updated_metrics |>
      dplyr::filter(axis == ax$axis_key, aggregation_level == agg_lbl,
                    network == "current_767") |>
      dplyr::pull(weighted_jaccard)
    if (length(v)) round(v[[1L]], 4) else NA_real_
  }
  j_vals <- vapply(c("7bin_hybrid","12bin_hybrid","18bin_hybrid","20bin_hybrid","30bin_hybrid"),
                   get_j, numeric(1))
  log_msg(sprintf("  %-25s  %8.4f  %8.4f  %8.4f  %8.4f  %8.4f",
                  ax$axis_key, j_vals[1], j_vals[2], j_vals[3], j_vals[4], j_vals[5]))
}

log_msg("")
log_msg("Done!")
log_msg("Log: ", LOG_PATH)
