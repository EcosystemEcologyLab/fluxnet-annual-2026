## figure_representativeness_trendy_compute.R
## TRENDY v14-gcb2025 IAV and median representativeness analysis
##
## Ensemble: 19 models (CARDAMOM excluded — only 22 years, too short for
## detrended-SD analysis comparable to the rest of the ensemble).
## Analysis window: 1990-2023 (34 years), constrained by model intersection.
##
## Outputs (to data/external/trendy/derived/ and data/snapshots/):
##   Intermediate per-model regridded NetCDFs (0.5°, 34 annual layers)
##   trendy_nee_iav.nc, trendy_et_iav.nc (ensemble-median detrended SD)
##   trendy_nee_median.nc, trendy_et_median.nc (ensemble-median mean |flux|)
##   site_trendy_nee_iav.csv, site_trendy_et_iav.csv
##   site_trendy_nee_median.csv, site_trendy_et_median.csv
##   *_global_distribution.csv for each axis
##   Rows appended to representativeness_metrics.csv
##   logs/trendy_analysis_complete.marker on success

suppressPackageStartupMessages({
  library(terra)
  library(dplyr)
  library(readr)
  library(jsonlite)
})

# Simple inline meta.json writer (avoids sourcing R/utils.R which has a
# conflicting sink() call for session_info.txt)
write_meta <- function(output_path, input_sources, notes = "") {
  meta <- list(
    run_datetime_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    pipeline_version = system("git rev-parse --short HEAD", intern = TRUE),
    input_sources    = as.list(input_sources),
    notes            = notes
  )
  meta_path <- paste0(tools::file_path_sans_ext(output_path), ".meta.json")
  writeLines(jsonlite::toJSON(meta, pretty = TRUE, auto_unbox = TRUE), meta_path)
  invisible(meta_path)
}

# ---- Logging ----------------------------------------------------------------
LOG_START <- format(Sys.time(), "%Y%m%d_%H%M%S")
LOG_FILE  <- file.path("logs", paste0("trendy_analysis_", LOG_START, ".log"))
con <- file(LOG_FILE, open = "wt")
sink(con, type = "output")
sink(con, type = "message", append = TRUE)
on.exit({
  sink(type = "message")
  sink(type = "output")
  close(con)
}, add = TRUE)

msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)

msg("=== TRENDY v14 IAV/Median representativeness compute ===")
msg("Log: ", LOG_FILE)

# ---- Configuration ----------------------------------------------------------
BASE_DIR    <- "data/external/trendy/v14-gcb2025"
DERIVED_DIR <- "data/external/trendy/derived"
INTER_DIR   <- file.path(DERIVED_DIR, "intermediate")
SNAP_DIR    <- "data/snapshots"
KG_PATH     <- file.path("data", "external", "koppen_beck2023", "1991_2020",
                         "koppen_geiger_0p5.tif")
SITE_CSV    <- file.path(SNAP_DIR, "site_biomass_cci_v7.csv")
METRICS_CSV <- file.path(SNAP_DIR, "representativeness_metrics.csv")

YEAR_START  <- 1990L
YEAR_END    <- 2023L
N_YEARS     <- YEAR_END - YEAR_START + 1L     # 34
SECS_MONTH  <- 30.4375 * 86400               # 2 629 800 s/month
SECS_YEAR   <- 365.25  * 86400               # 31 557 600 s/year
KG_TO_G     <- 1000.0

# Near-zero bin thresholds (gC m-2 yr-1 for nbp vars, mm yr-1 for et vars)
NBP_LOW_CUT <- 5.0
ET_LOW_CUT  <- 5.0

# 19-model ensemble (CARDAMOM excluded)
MODELS_19 <- c(
  "CABLE-POP", "CLASSIC", "CLM", "CLM-FATES", "DLEM", "ED",
  "ELM", "ELM-FATES", "IBIS", "ISAM", "JSBACH", "JULES-ES",
  "LPJ-GUESS", "LPJml", "LPJwsl", "LPX-Bern", "ORCHIDEE", "TEM", "VISIT-UT"
)

# Models with NBP stored as annual time steps (all others are monthly)
ANNUAL_NBP_MODELS <- c("DLEM", "LPJ-GUESS", "LPJml")

# Models with 0-360 longitude (require terra::rotate())
LON360_MODELS <- c("CLM", "ISAM", "ELM-FATES")

# Model start years (for fallback time-index computation)
MODEL_START_YR <- c(
  "CABLE-POP"=1700L, "CLASSIC"=1700L, "CLM"=1700L, "CLM-FATES"=1701L,
  "DLEM"=1700L, "ED"=1700L, "ELM"=1698L, "ELM-FATES"=1701L,
  "IBIS"=1700L, "ISAM"=1700L, "JSBACH"=1701L, "JULES-ES"=1700L,
  "LPJ-GUESS"=1700L, "LPJml"=1700L, "LPJwsl"=1699L, "LPX-Bern"=1700L,
  "ORCHIDEE"=1700L, "TEM"=1700L, "VISIT-UT"=1700L
)

dir.create(INTER_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(DERIVED_DIR, showWarnings = FALSE)

# Target 0.5° global grid
TARGET <- rast(nrows = 360L, ncols = 720L,
               xmin = -180, xmax = 180, ymin = -90, ymax = 90,
               crs = "EPSG:4326")

# ---- Helper: find NetCDF file -----------------------------------------------
find_nc <- function(model, var) {
  d <- file.path(BASE_DIR, model, "S3")
  hits <- list.files(d, pattern = paste0(".*_S3_", var, "\\.nc$"),
                     full.names = TRUE)
  if (length(hits) == 0L) return(NA_character_)
  hits[[1L]]
}

# ---- Helper: get year vector from model config ------------------------------
# terra::time() misparses pre-1678 CF origins (e.g. "1700-1-16") using
# the Unix epoch 1970 as fallback, producing a systematic +270 year offset.
# We bypass terra::time() entirely and compute years from verified model
# start years (cross-checked via Python/netCDF4 inspection, 2026-06-25).
get_years <- function(r, model, is_annual_nbp = FALSE) {
  n  <- nlyr(r)
  sy <- MODEL_START_YR[[model]]
  if (is_annual_nbp || n %in% c(325L, 326L)) {
    # Annual: one layer per year
    return(sy:(sy + n - 1L))
  } else {
    # Monthly: 12 layers per year
    n_full_years <- ceiling(n / 12L)
    yr_vec <- rep(seq(sy, by = 1L, length.out = n_full_years),
                  each = 12L)[seq_len(n)]
    return(as.integer(yr_vec))
  }
}

# ---- Helper: load + unit-convert + sum to annual ----------------------------
# Returns SpatRaster with N_YEARS layers in gC m-2 yr-1 (nbp) or mm yr-1 (et)
load_annual <- function(model, var) {
  path <- find_nc(model, var)
  if (is.na(path)) stop("File not found: ", model, " ", var)

  msg("  Loading ", model, " ", var, " (", basename(path), ")")
  r <- rast(path)

  # Rotate 0-360 → -180-180 before any subsetting
  if (model %in% LON360_MODELS) {
    msg("    Rotating longitude 0-360 → -180-180")
    r <- rotate(r)
  }

  is_annual <- (var == "nbp" && model %in% ANNUAL_NBP_MODELS)
  yr_all <- get_years(r, model, is_annual_nbp = is_annual)

  if (is_annual) {
    idx <- which(yr_all >= YEAR_START & yr_all <= YEAR_END)
    if (length(idx) == 0L)
      stop(model, " ", var, ": no annual layers in ", YEAR_START, "-", YEAR_END)
    r_sub <- r[[idx]]
    # Rate (kg m-2 s-1) × sec/yr × 1000 → gC m-2 yr-1
    r_conv <- r_sub * (SECS_YEAR * KG_TO_G)
    names(r_conv) <- as.character(YEAR_START:YEAR_END)
    msg("    ", model, " nbp: annual × ", formatC(SECS_YEAR * KG_TO_G, format = "e"),
        " → gC m-2 yr-1  (", length(idx), " annual layers)")
    return(r_conv)
  }

  # Monthly: sum 12 layers per year
  annual_list <- vector("list", N_YEARS)
  for (i in seq_len(N_YEARS)) {
    y <- YEAR_START + i - 1L
    mo_idx <- which(yr_all == y)
    if (length(mo_idx) == 0L) {
      msg("  WARNING: ", model, " ", var, " has no data for year ", y)
      annual_list[[i]] <- r[[1L]] * NA_real_
      names(annual_list[[i]]) <- as.character(y)
      next
    }
    if (length(mo_idx) != 12L) {
      msg("  NOTE: ", model, " ", var, " year ", y, " has ", length(mo_idx),
          " months (expected 12) — summing available months")
    }
    r_mo <- r[[mo_idx]]
    if (var == "evapotrans") {
      r_yr <- sum(r_mo * SECS_MONTH, na.rm = FALSE)   # mm yr-1
    } else {
      r_yr <- sum(r_mo * (SECS_MONTH * KG_TO_G), na.rm = FALSE)  # gC m-2 yr-1
    }
    names(r_yr) <- as.character(y)
    annual_list[[i]] <- r_yr
  }
  r_annual <- rast(annual_list)
  unit_str <- if (var == "evapotrans") "mm yr-1" else "gC m-2 yr-1"
  msg("    ", model, " ", var, ": monthly × ", formatC(SECS_MONTH, format = "e"),
      " summed to annual → ", unit_str)
  r_annual
}

# ---- Helper: vectorised linear-detrend SD (per pixel) ----------------------
# Input: SpatRaster with n layers; Output: 1-layer SD of detrended residuals
compute_detrended_sd <- function(r) {
  n <- nlyr(r)
  vals <- values(r)    # ncells × n matrix
  yr_idx <- seq_len(n)

  # OLS residual projection: P = I - X(X'X)^-1 X'
  X <- cbind(1, yr_idx)
  H <- X %*% tcrossprod(solve(crossprod(X)), X)   # n × n hat matrix
  P <- diag(n) - H                                 # n × n residual projection

  # Identify complete rows (no NA in any year)
  complete <- rowSums(is.na(vals)) == 0L
  out_vals <- rep(NA_real_, nrow(vals))

  if (any(complete)) {
    resids  <- vals[complete, , drop = FALSE] %*% t(P)   # ncomp × n
    # SD with df = n-2 (residuals from linear fit)
    out_vals[complete] <- sqrt(rowSums(resids^2) / (n - 2L))
  }

  r_out <- r[[1L]]
  values(r_out) <- out_vals
  names(r_out) <- "detrended_sd"
  r_out
}

# ---- Helper: vectorised row mean of absolute values ------------------------
compute_mean_abs <- function(r) {
  vals <- values(r)
  complete <- rowSums(is.na(vals)) == 0L
  out_vals <- rep(NA_real_, nrow(vals))
  if (any(complete)) {
    out_vals[complete] <- rowMeans(abs(vals[complete, , drop = FALSE]))
  }
  r_out <- r[[1L]]
  values(r_out) <- out_vals
  names(r_out) <- "mean_abs"
  r_out
}

# ---- Helper: vectorised row means ------------------------------------------
compute_mean <- function(r) {
  vals <- values(r)
  complete <- rowSums(is.na(vals)) == 0L
  out_vals <- rep(NA_real_, nrow(vals))
  if (any(complete)) {
    out_vals[complete] <- rowMeans(vals[complete, , drop = FALSE])
  }
  r_out <- r[[1L]]
  values(r_out) <- out_vals
  names(r_out) <- "mean"
  r_out
}

# ---- Load KG land mask (0.5°) -----------------------------------------------
msg("Loading KG land mask (0.5°) ...")
kg_05 <- rast(KG_PATH)
msg("  KG mask: ", nrow(kg_05), " × ", ncol(kg_05))

# ---- Load site coordinates --------------------------------------------------
msg("Loading site coordinates ...")
sites <- read_csv(SITE_CSV, show_col_types = FALSE)
n_sites <- nrow(sites)
msg("  ", n_sites, " sites from ", basename(SITE_CSV))

# ---- Step 1: Per-model regridded intermediates ------------------------------
msg("\n=== STEP 1: Per-model regridding ===")

mdl_ok_nbp <- character(0)
mdl_ok_et  <- character(0)

for (mdl in MODELS_19) {
  for (v in c("nbp", "evapotrans")) {
    inter_path <- file.path(INTER_DIR, paste0(mdl, "_", v, "_regridded.tif"))

    if (file.exists(inter_path)) {
      ok <- tryCatch({
        r_chk <- rast(inter_path)
        ncol(r_chk) == 720L && nrow(r_chk) == 360L && nlyr(r_chk) == N_YEARS
      }, error = function(e) FALSE)
      if (ok) {
        msg("  Skipping ", mdl, " ", v, " (already complete)")
        if (v == "nbp")        mdl_ok_nbp <- c(mdl_ok_nbp, mdl)
        if (v == "evapotrans") mdl_ok_et  <- c(mdl_ok_et,  mdl)
        next
      }
      msg("  WARNING: ", basename(inter_path), " exists but failed validation — reprocessing")
    }

    r_annual <- tryCatch(
      load_annual(mdl, v),
      error = function(e) {
        msg("  ERROR loading ", mdl, " ", v, ": ", conditionMessage(e))
        return(NULL)
      }
    )
    if (is.null(r_annual)) next

    # Regrid to 0.5° target if not already aligned
    if (!isTRUE(all.equal(res(r_annual), c(0.5, 0.5))) ||
        !isTRUE(all.equal(as.vector(ext(r_annual)), c(-180, 180, -90, 90)))) {
      msg("    Resampling to 0.5° grid (bilinear) ...")
      r_annual <- resample(r_annual, TARGET, method = "bilinear", threads = TRUE)
    }

    # Apply KG land mask
    r_annual <- mask(r_annual, kg_05)

    # Write intermediate GeoTIFF (ncdf4 not in renv; GeoTIFF is sufficient)
    writeRaster(r_annual, inter_path, gdal = "COMPRESS=DEFLATE",
                overwrite = TRUE)
    msg("  Saved: ", basename(inter_path))

    rm(r_annual); gc(verbose = FALSE)
    if (v == "nbp")        mdl_ok_nbp <- c(mdl_ok_nbp, mdl)
    if (v == "evapotrans") mdl_ok_et  <- c(mdl_ok_et,  mdl)
  }
}

# Only include models where BOTH nbp and evapotrans succeeded
MODELS_OK <- intersect(unique(mdl_ok_nbp), unique(mdl_ok_et))
# ELM excluded: lacks 2023 data, so its per-pixel stat maps are all-NA under
# the complete-row requirement. Excluding here makes the exclusion explicit
# rather than implicit, so a future TRENDY back-fill of ELM's 2023 layer
# doesn't silently re-include ELM without conscious decision.
MODELS_OK <- setdiff(MODELS_OK, "ELM")
n_models  <- length(MODELS_OK)
msg("\nModels with both nbp and evapotrans intermediates (excl. ELM): ", n_models)
if (n_models < 14L) {
  stop("Too few models (", n_models, ") — check errors above before continuing.")
}

# ---- Step 2: Per-pixel statistics per model, then ensemble median -----------
msg("\n=== STEP 2: Per-pixel statistics + ensemble median ===")

FINAL_FILES <- list(
  nee_iav    = file.path(DERIVED_DIR, "trendy_nee_iav.tif"),
  et_iav     = file.path(DERIVED_DIR, "trendy_et_iav.tif"),
  nee_median = file.path(DERIVED_DIR, "trendy_nee_median.tif"),
  et_median  = file.path(DERIVED_DIR, "trendy_et_median.tif")
)

compute_ensemble <- function(stat_key, var, stat_fn) {
  out_path <- FINAL_FILES[[stat_key]]
  if (file.exists(out_path)) {
    msg("  SKIP (exists): ", basename(out_path))
    return(rast(out_path))
  }

  msg("  Computing ", stat_key, " across ", n_models, " models ...")
  model_stats <- vector("list", n_models)

  for (i in seq_along(MODELS_OK)) {
    mdl <- MODELS_OK[[i]]
    inter_path <- file.path(INTER_DIR, paste0(mdl, "_", var, "_regridded.tif"))
    if (!file.exists(inter_path)) {
      msg("    SKIP ", mdl, " — intermediate missing")
      next
    }
    r <- rast(inter_path)
    stat_r <- stat_fn(r)
    names(stat_r) <- mdl
    model_stats[[i]] <- stat_r
    rm(r); gc(verbose = FALSE)
    msg("    ", mdl, " done")
  }

  model_stats <- Filter(Negate(is.null), model_stats)
  n_used <- length(model_stats)
  msg("  Stacking ", n_used, " model layers for ", stat_key, " ...")
  stk <- rast(model_stats)
  ens <- app(stk, fun = function(v) median(v, na.rm = TRUE))
  names(ens) <- stat_key

  writeRaster(ens, out_path, gdal = "COMPRESS=DEFLATE", overwrite = TRUE)
  msg("  Saved: ", basename(out_path))
  ens
}

r_nee_iav    <- compute_ensemble("nee_iav",    "nbp",        compute_detrended_sd)
r_et_iav     <- compute_ensemble("et_iav",     "evapotrans", compute_detrended_sd)
r_nee_median <- compute_ensemble("nee_median", "nbp",        compute_mean_abs)
r_et_median  <- compute_ensemble("et_median",  "evapotrans", compute_mean)

# Reload if skipped
if (!inherits(r_nee_iav,    "SpatRaster")) r_nee_iav    <- rast(FINAL_FILES$nee_iav)
if (!inherits(r_et_iav,     "SpatRaster")) r_et_iav     <- rast(FINAL_FILES$et_iav)
if (!inherits(r_nee_median, "SpatRaster")) r_nee_median <- rast(FINAL_FILES$nee_median)
if (!inherits(r_et_median,  "SpatRaster")) r_et_median  <- rast(FINAL_FILES$et_median)

# ---- Step 3: Per-site extraction -------------------------------------------
msg("\n=== STEP 3: Per-site extraction ===")

extract_sites <- function(r_map, kg_mask, sites, val_col, low_cut, axis_label) {
  coords_mat <- as.matrix(sites[, c("location_long", "location_lat")])

  raw <- terra::extract(r_map, coords_mat, method = "simple")
  vals    <- raw[[1]]
  methods <- rep("exact", nrow(sites))

  # Nearest-land recovery (within 3°) for NA sites
  na_idx <- which(is.na(vals))
  if (length(na_idx) > 0L) {
    msg("  Nearest-land recovery for ", length(na_idx), " NA sites ...")
    for (i in na_idx) {
      sx <- coords_mat[i, 1L]
      sy <- coords_mat[i, 2L]
      window   <- ext(sx - 3, sx + 3, sy - 3, sy + 3)
      r_crop   <- crop(r_map, window)
      kg_crop  <- crop(kg_mask, window)
      r_land   <- mask(r_crop, kg_crop)
      lv       <- values(r_land)[, 1L]
      ok       <- !is.na(lv) & is.finite(lv)
      if (!any(ok)) {
        methods[i] <- "no_land_within_3deg"
        next
      }
      land_cells <- which(ok)
      land_xy    <- xyFromCell(r_land, land_cells)
      dists      <- sqrt((land_xy[, 1L] - sx)^2 + (land_xy[, 2L] - sy)^2)
      best       <- which.min(dists)
      vals[i]    <- lv[land_cells[[best]]]
      methods[i] <- sprintf("nearest_land_%.3fdeg", dists[[best]])
    }
  }

  site_df <- sites |>
    dplyr::select(site_id, location_lat, location_long) |>
    dplyr::mutate(
      !!val_col    := vals,
      !!paste0(axis_label, "_method") := methods
    )
  site_df
}

# Classify into hybrid bins
make_bins <- function(global_dist, low_cut) {
  veg_dist <- global_dist[global_dist$value >= low_cut, ]
  if (nrow(veg_dist) == 0L) stop("No pixels above low_cut=", low_cut)
  total_veg_area <- sum(veg_dist$area_km2)
  cum_area <- cumsum(veg_dist$area_km2)
  q_breaks <- vapply(seq(1, 5) / 6, function(f) {
    target <- f * total_veg_area
    idx    <- which(cum_area >= target)[[1L]]
    veg_dist$value[[idx]]
  }, numeric(1))
  q_breaks <- round(q_breaks, 2)
  list(breaks = c(0, low_cut, q_breaks, Inf), q_breaks = q_breaks,
       total_veg_area = total_veg_area)
}

classify_val <- function(x, breaks) {
  b <- findInterval(x, breaks[-length(breaks)], left.open = FALSE)
  b[b == 0L] <- 1L
  b[b > 7L]  <- 7L
  as.integer(b)
}

make_bin_labels <- function(breaks, q_breaks, low_cut, unit) {
  c(
    sprintf("0–%.0f %s", low_cut, unit),
    sprintf("%.1f–%.1f %s", low_cut, q_breaks[1], unit),
    sprintf("%.1f–%.1f %s", q_breaks[1], q_breaks[2], unit),
    sprintf("%.1f–%.1f %s", q_breaks[2], q_breaks[3], unit),
    sprintf("%.1f–%.1f %s", q_breaks[3], q_breaks[4], unit),
    sprintf("%.1f–%.1f %s", q_breaks[4], q_breaks[5], unit),
    sprintf(">%.1f %s", q_breaks[5], unit)
  )
}

# Load KG land mask at 0.5° and compute cell areas once
msg("Computing cell areas ...")
cell_areas_05 <- cellSize(kg_05, mask = TRUE, unit = "km")

process_axis <- function(r_map, axis_key, val_col, low_cut, unit_str,
                          site_out, glob_out) {
  msg("\n--- Axis: ", axis_key, " ---")

  # --- Global area-weighted distribution ---
  msg("  Building global histogram (0.5° grid) ...")
  r_land <- mask(r_map, kg_05)

  # Fine histogram: 0.1 unit bins up to 1000 + catch-all
  HIST_STEP <- 0.1
  HIST_MAX  <- 1000
  hist_lo   <- seq(0, HIST_MAX - HIST_STEP, by = HIST_STEP)
  hist_hi   <- hist_lo + HIST_STEP
  hist_ids  <- seq_along(hist_lo)
  catch_id  <- max(hist_ids) + 1L
  hist_rcl  <- rbind(
    cbind(hist_lo, hist_hi, as.numeric(hist_ids)),
    c(HIST_MAX, 1e9, as.numeric(catch_id))
  )
  r_hist <- classify(ifel(r_land < 0, 0, r_land), hist_rcl,
                     right = FALSE, include.lowest = TRUE)
  hist_areas <- zonal(cell_areas_05, r_hist, fun = "sum", na.rm = TRUE)
  names(hist_areas) <- c("bin_id", "area_km2")
  hist_areas <- hist_areas[!is.na(hist_areas$bin_id), ]

  bin_lo_vec <- c(hist_lo, HIST_MAX)
  hist_areas$value <- bin_lo_vec[hist_areas$bin_id]
  hist_areas <- hist_areas[order(hist_areas$value), ]

  total_land_km2 <- sum(hist_areas$area_km2)
  msg("  Total land: ", format(round(total_land_km2), big.mark = ","), " km2 ",
      "(target ~147,322,862)")

  # Verify and possibly adjust near-zero cut
  low_area  <- sum(hist_areas$area_km2[hist_areas$value < low_cut])
  low_frac  <- low_area / total_land_km2
  msg("  Near-zero bin (<", low_cut, " ", unit_str, "): ",
      round(low_frac * 100, 1), "% of land")
  if (low_frac < 0.01) {
    low_cut <- low_cut * 0.5
    msg("  ADJUSTED near-zero cut to ", low_cut,
        " (original cut captured < 1% of land)")
  } else if (low_frac > 0.70) {
    low_cut <- low_cut * 2.0
    msg("  ADJUSTED near-zero cut to ", low_cut,
        " (original cut captured > 70% of land)")
  }

  # Quantile bins from vegetated land
  bins_info <- make_bins(hist_areas, low_cut)
  BIN_BREAKS <- bins_info$breaks
  Q_BREAKS   <- bins_info$q_breaks
  bin_labels <- make_bin_labels(BIN_BREAKS, Q_BREAKS, low_cut, unit_str)

  msg("  Quantile breakpoints (", unit_str, "): ",
      paste(round(Q_BREAKS, 2), collapse = ", "))

  # Global distribution under 7-bin hybrid scheme
  rcl_7 <- matrix(c(
    -1e9,        low_cut,      1,
    low_cut,     Q_BREAKS[1],  2,
    Q_BREAKS[1], Q_BREAKS[2],  3,
    Q_BREAKS[2], Q_BREAKS[3],  4,
    Q_BREAKS[3], Q_BREAKS[4],  5,
    Q_BREAKS[4], Q_BREAKS[5],  6,
    Q_BREAKS[5], 1e9,          7
  ), ncol = 3L, byrow = TRUE)

  # Handle negative NBP values (sink): assign to bin 2 (above low_cut)
  # nbp can be negative (source); we use abs for nee_median but raw for nee_iav
  # For IAV (SD), values are always positive; for median of |nbp|, always positive.
  # So no negative values expected after step 2 for these derived maps.
  r_bins <- classify(ifel(is.na(r_land), NA, ifel(r_land < 0, 0, r_land)),
                     rcl_7, right = FALSE, include.lowest = TRUE)
  zone_areas <- zonal(cell_areas_05, r_bins, fun = "sum", na.rm = TRUE)
  names(zone_areas) <- c("bin", "global_land_area_km2")
  zone_areas <- zone_areas[!is.na(zone_areas$bin) & zone_areas$bin %in% 1:7, ]

  dist_df <- data.frame(
    bin       = 1:7,
    bin_label = bin_labels,
    min_value = c(0, low_cut, Q_BREAKS),
    max_value = c(low_cut, Q_BREAKS, NA_real_)
  ) |>
    dplyr::left_join(zone_areas, by = "bin") |>
    dplyr::mutate(
      global_land_area_km2 = dplyr::coalesce(global_land_area_km2, 0),
      global_land_fraction = global_land_area_km2 / total_land_km2
    )

  ensemble_nc_path <- file.path(DERIVED_DIR, paste0(axis_key, ".tif"))
  readr::write_csv(dist_df, glob_out)
  write_meta(glob_out,
    input_sources = c(ensemble_nc_path, KG_PATH),
    notes = paste0("Ensemble-median TRENDY v14 ", axis_key,
                   " global distribution. Analysis window ", YEAR_START, "-", YEAR_END,
                   ". ", n_models, " models. Near-zero bin cut: ", low_cut, " ", unit_str,
                   ". Quantile breakpoints: ", paste(round(Q_BREAKS, 2), collapse=", "), "."))
  msg("  Saved global distribution: ", basename(glob_out))

  # --- Per-site extraction ---
  site_df <- extract_sites(r_land, kg_05, sites,
                            val_col = val_col,
                            low_cut = low_cut,
                            axis_label = axis_key)
  site_df <- site_df |>
    dplyr::mutate(
      raw_for_bin = dplyr::if_else(
        is.na(.data[[val_col]]) | .data[[val_col]] < 0, 0, .data[[val_col]]
      ),
      bin       = classify_val(raw_for_bin, BIN_BREAKS),
      bin_label = factor(bin_labels[bin], levels = bin_labels)
    ) |>
    dplyr::select(-raw_for_bin)

  names(site_df)[names(site_df) == "bin"]       <- paste0(axis_key, "_bin")
  names(site_df)[names(site_df) == "bin_label"] <- paste0(axis_key, "_bin_label")

  readr::write_csv(site_df, site_out)
  write_meta(site_out,
    input_sources = c(ensemble_nc_path, SITE_CSV),
    notes = paste0("Per-site extraction of ensemble-median TRENDY v14 ", axis_key,
                   ". ", n_sites, " sites. Nearest-land recovery within 3 deg for NA sites.",
                   " 7-bin hybrid scheme: fixed near-zero bin 0-", low_cut, " ", unit_str,
                   "; 6 equal-area quantile bins above. Breakpoints: ",
                   paste(round(Q_BREAKS, 2), collapse=", "), "."))
  msg("  Saved site CSV: ", basename(site_out))

  # --- Representativeness metrics ---
  p_global <- dist_df$global_land_fraction
  bin_col  <- paste0(axis_key, "_bin")
  q_net    <- vapply(1:7, function(b)
    sum(site_df[[bin_col]] == b, na.rm = TRUE) / n_sites, numeric(1))

  compute_repr <- function(p, q) {
    p[is.na(p)] <- 0; q[is.na(q)] <- 0
    list(
      weighted_jaccard   = sum(pmin(p, q)) / sum(pmax(p, q)),
      hellinger_distance = (1 / sqrt(2)) * sqrt(sum((sqrt(p) - sqrt(q))^2))
    )
  }
  m <- compute_repr(p_global, q_net)
  msg("  ", axis_key, " metrics: J = ", round(m$weighted_jaccard, 4),
      "  H = ", round(m$hellinger_distance, 4))

  list(dist_df = dist_df, site_df = site_df,
       J = m$weighted_jaccard, H = m$hellinger_distance,
       bin_breaks = BIN_BREAKS, q_breaks = Q_BREAKS, low_cut_used = low_cut)
}

# Run four axes
res_nee_iav <- process_axis(
  r_nee_iav, "trendy_nee_iav", "trendy_nee_iav_value",
  NBP_LOW_CUT, "gC m-2 yr-1",
  file.path(SNAP_DIR, "site_trendy_nee_iav.csv"),
  file.path(SNAP_DIR, "trendy_nee_iav_global_distribution.csv")
)
res_et_iav <- process_axis(
  r_et_iav, "trendy_et_iav", "trendy_et_iav_value",
  ET_LOW_CUT, "mm yr-1",
  file.path(SNAP_DIR, "site_trendy_et_iav.csv"),
  file.path(SNAP_DIR, "trendy_et_iav_global_distribution.csv")
)
res_nee_median <- process_axis(
  r_nee_median, "trendy_nee_median", "trendy_nee_median_value",
  NBP_LOW_CUT, "gC m-2 yr-1",
  file.path(SNAP_DIR, "site_trendy_nee_median.csv"),
  file.path(SNAP_DIR, "trendy_nee_median_global_distribution.csv")
)
res_et_median <- process_axis(
  r_et_median, "trendy_et_median", "trendy_et_median_value",
  ET_LOW_CUT, "mm yr-1",
  file.path(SNAP_DIR, "site_trendy_et_median.csv"),
  file.path(SNAP_DIR, "trendy_et_median_global_distribution.csv")
)

# ---- Step 4: Append representativeness metrics ------------------------------
msg("\n=== STEP 4: Representativeness metrics ===")

existing_metrics <- if (file.exists(METRICS_CSV)) {
  readr::read_csv(METRICS_CSV, show_col_types = FALSE) |>
    # Remove only the four rows this script is about to rewrite:
    # current_767 network, 7bin_hybrid aggregation, four TRENDY axes.
    # Historical-network rows and other aggregation levels are preserved.
    dplyr::filter(!(axis %in% c("trendy_nee_iav", "trendy_et_iav",
                                 "trendy_nee_median", "trendy_et_median") &
                    aggregation_level == "7bin_hybrid" &
                    network == "current_767"))
} else {
  data.frame(axis = character(), aggregation_level = character(),
             n_classes = integer(), weighted_jaccard = numeric(),
             hellinger_distance = numeric(), network = character(),
             n_sites = integer())
}

new_rows <- data.frame(
  axis = c("trendy_nee_iav", "trendy_et_iav",
           "trendy_nee_median", "trendy_et_median"),
  aggregation_level = "7bin_hybrid",
  n_classes = 7L,
  weighted_jaccard = c(res_nee_iav$J, res_et_iav$J,
                       res_nee_median$J, res_et_median$J),
  hellinger_distance = c(res_nee_iav$H, res_et_iav$H,
                         res_nee_median$H, res_et_median$H),
  network = "current_767",
  n_sites = n_sites
)

metrics_final <- dplyr::bind_rows(existing_metrics, new_rows)
readr::write_csv(metrics_final, METRICS_CSV)
msg("Saved metrics: ", METRICS_CSV)
print(as.data.frame(dplyr::mutate(metrics_final,
  J = round(weighted_jaccard, 3),
  H = round(hellinger_distance, 3)
)[, c("axis", "aggregation_level", "J", "H")]))

# ---- Completion marker -------------------------------------------------------
msg("\n=== Writing completion marker ===")
marker_path <- "logs/trendy_analysis_complete.marker"
marker_lines <- c(
  paste0("timestamp: ",         format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
  paste0("analysis_window: ",   YEAR_START, "-", YEAR_END),
  paste0("n_years: ",           N_YEARS),
  paste0("n_models: ",          n_models),
  paste0("models_included: ",   paste(MODELS_OK, collapse = ",")),
  paste0("dlem_status: included (NaN fill, no _FillValue attr — terra handles naturally)"),
  paste0("log_file: ",          LOG_FILE),
  "",
  "outputs_produced:",
  paste0("  ", unlist(FINAL_FILES)),
  paste0("  ", file.path(SNAP_DIR, "site_trendy_nee_iav.csv")),
  paste0("  ", file.path(SNAP_DIR, "site_trendy_et_iav.csv")),
  paste0("  ", file.path(SNAP_DIR, "site_trendy_nee_median.csv")),
  paste0("  ", file.path(SNAP_DIR, "site_trendy_et_median.csv")),
  paste0("  ", file.path(SNAP_DIR, "trendy_nee_iav_global_distribution.csv")),
  paste0("  ", file.path(SNAP_DIR, "trendy_et_iav_global_distribution.csv")),
  paste0("  ", file.path(SNAP_DIR, "trendy_nee_median_global_distribution.csv")),
  paste0("  ", file.path(SNAP_DIR, "trendy_et_median_global_distribution.csv")),
  paste0("  ", METRICS_CSV),
  "",
  "metrics_summary:",
  paste(sprintf("  %-20s J=%.4f  H=%.4f", new_rows$axis,
                new_rows$weighted_jaccard, new_rows$hellinger_distance),
        collapse = "\n")
)
writeLines(marker_lines, marker_path)
msg("Completion marker written: ", marker_path)
msg("=== TRENDY compute complete ===")
