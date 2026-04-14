## 04_qc.R — Apply QC filtering per resolution
## Uses: flux_qc(), fluxnet_qc_hh()
## See CLAUDE.md QC Flag Reference for flag definitions.
##
## Loops over FLUXNET_EXTRACT_RESOLUTIONS; reads flux_data_raw_<res>.rds and
## writes flux_data_qc_<res>.rds for each resolution.
##
## Two-stage QC strategy:
##   Stage 1 (flux_qc): at DD/MM/WW/YY resolution, _QC values are fractions
##     0–1. flux_qc() adds qc_flagged (TRUE if p_gapfilled > 1-threshold) and
##     p_gapfilled columns; flagged rows are dropped and logged as exclusions.
##     Rows with qc_flagged == NA are kept. Stage 1 is skipped for HH/HR data.
##
##   Stage 2 (fluxnet_qc_hh): at HH/HR resolution, _QC values are integers
##     0–3. Rows with any _QC > max_qc are dropped. At DD/MM/WW/YY this is a
##     no-op (all fractions ≤ 1 pass max_qc = 1L) but the call is retained so
##     the script is resolution-agnostic.

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}

source("R/pipeline_config.R")
check_pipeline_config()

source("R/qc.R")
source("R/utils.R")

library(fluxnet)
library(dplyr)

processed_dir <- file.path(FLUXNET_DATA_ROOT, "processed")

# Initialise log files so they exist even if nothing is excluded/unknown.
if (!dir.exists("outputs")) dir.create("outputs", recursive = TRUE)
excl_log_path    <- file.path("outputs", "exclusion_log.csv")
unknown_log_path <- file.path("outputs", "unknown_log.csv")
if (!file.exists(excl_log_path)) {
  utils::write.csv(
    data.frame(site_id = character(), variable = character(),
               timestamp = character(), reason = character(),
               threshold = character(), excluded_by = character()),
    excl_log_path, row.names = FALSE
  )
}
if (!file.exists(unknown_log_path)) {
  utils::write.csv(
    data.frame(record_id = character(), reason = character(),
               logged_by = character()),
    unknown_log_path, row.names = FALSE
  )
}

# Per-resolution fractional QC thresholds (DD/MM/WW/YY).
# HH/HR data uses integer flag filtering (Stage 2 only).
qc_threshold_for <- list(
  yy = QC_THRESHOLD_YY,
  mm = QC_THRESHOLD_MM,
  ww = QC_THRESHOLD_WW,
  dd = QC_THRESHOLD_DD,
  hh = NA_real_
)

# Resolutions that carry integer _QC flags (Stage 2 only; skip Stage 1).
hh_suffixes <- c("hh", "hr")

# Map extract resolution codes to output file suffixes.
res_to_suffix <- c(y = "yy", m = "mm", w = "ww", d = "dd", h = "hh")

for (res_code in FLUXNET_EXTRACT_RESOLUTIONS) {
  suffix   <- res_to_suffix[[res_code]]
  raw_path <- file.path(processed_dir, paste0("flux_data_raw_", suffix, ".rds"))

  out_path <- file.path(processed_dir, paste0("flux_data_qc_", suffix, ".rds"))

  if (file.exists(out_path)) {
    message("QC output already exists for resolution '", res_code, "' (", suffix,
            ") — skipping: ", out_path)
    next
  }

  if (!file.exists(raw_path)) {
    message("No raw file for resolution '", res_code, "' (", suffix,
            ") — skipping.")
    next
  }

  flux_data <- readRDS(raw_path)
  cat("\n--- 04_qc.R: resolution", toupper(suffix), "---\n")

  is_hh        <- suffix %in% hh_suffixes
  qc_thresh    <- qc_threshold_for[[suffix]]
  thresh_label <- paste0("QC_THRESHOLD_", toupper(suffix))

  # --- Stage 1 + 2: per-site loop with disk-based chunk accumulation ---
  # flux_qc() runs purrr::map2/reduce across all rows at once; for resolutions
  # like MM (425 k rows) this causes a fatal memory spike. Processing per-site
  # (~600 rows each) eliminates the spike and provides progress output.
  #
  # Every 50 sites the current batch is bound and written to a numbered chunk
  # RDS in a temporary directory, then the in-memory accumulator is cleared.
  # After the loop, the raw frame is freed before the chunks are read back and
  # combined sequentially, keeping peak memory below ~1.5 GB at all resolutions.
  n_before_stage1 <- nrow(flux_data)

  site_ids <- unique(flux_data$site_id)
  n_sites  <- length(site_ids)

  if (!is_hh) {
    qc_col_names <- grep("_QC$", names(flux_data), value = TRUE)
    qc_vars      <- sub("_QC$", "", qc_col_names)
  }

  chunk_dir <- file.path(processed_dir, paste0("qc_chunks_", suffix))
  if (!dir.exists(chunk_dir)) dir.create(chunk_dir)
  chunk_n       <- 0L
  accum         <- list()
  n_excluded_s1 <- 0L
  n_excluded_s2 <- 0L

  message("  Applying QC to ", n_sites, " sites..."); flush(stderr())

  for (s_i in seq_along(site_ids)) {
    site_d <- flux_data[flux_data$site_id == site_ids[s_i], , drop = FALSE]

    # Stage 1: fractional threshold (coarser resolutions only)
    if (!is_hh) {
      site_qc     <- flux_qc(site_d, qc_vars = qc_vars,
                             max_gapfilled = 1 - qc_thresh)
      flagged_idx <- which(!is.na(site_qc$qc_flagged) & site_qc$qc_flagged)
      n_excluded_s1 <- n_excluded_s1 + length(flagged_idx)
      for (i in flagged_idx) {
        ts_col <- intersect(c("YEAR", "DATE", "DATETIME"), names(site_qc))
        ts_val <- if (length(ts_col) > 0)
                    as.character(site_qc[[ts_col[[1]]]][i])
                  else "ALL"
        log_exclusion(
          site_id     = site_qc$site_id[i],
          variable    = "ALL",
          timestamp   = ts_val,
          reason      = paste0("flux_qc: p_gapfilled > ",
                               round(1 - qc_thresh, 2), " (",
                               thresh_label, " = ", qc_thresh, ")"),
          threshold   = paste0(thresh_label, "=", qc_thresh),
          excluded_by = "04_qc.R"
        )
      }
      site_qc <- site_qc[is.na(site_qc$qc_flagged) | !site_qc$qc_flagged, ]
    } else {
      site_qc <- site_d
    }

    # Stage 2: integer flag filter (no-op at coarser resolutions)
    n_s2_before   <- nrow(site_qc)
    site_qc       <- fluxnet_qc_hh(site_qc, max_qc = 1L)
    n_excluded_s2 <- n_excluded_s2 + (n_s2_before - nrow(site_qc))
    accum[[length(accum) + 1L]] <- site_qc

    if (length(accum) >= 50L || s_i == n_sites) {
      message("  Progress: ", s_i, "/", n_sites, " sites"); flush(stderr())
      chunk_n   <- chunk_n + 1L
      chunk_dat <- dplyr::bind_rows(accum)
      saveRDS(chunk_dat,
              file.path(chunk_dir, sprintf("chunk_%04d.rds", chunk_n)))
      accum <- list()
      rm(chunk_dat); gc()
    }
  }

  # Free the raw frame before combining chunks — eliminates the ~1 GB overlap
  # between the raw and QC'd frames that caused OOM on the previous approach.
  rm(flux_data); gc()

  # Bind chunks sequentially so peak memory stays bounded to
  # (accumulated result so far) + (one new chunk ~30 K rows).
  chunk_files <- sort(list.files(chunk_dir, pattern = "\\.rds$",
                                 full.names = TRUE))
  message("  Combining ", length(chunk_files), " chunks..."); flush(stderr())
  flux_data_qc <- NULL
  for (cf in chunk_files) {
    chunk_dat    <- readRDS(cf)
    flux_data_qc <- if (is.null(flux_data_qc)) chunk_dat
                    else dplyr::bind_rows(flux_data_qc, chunk_dat)
    rm(chunk_dat); gc()
  }
  unlink(chunk_dir, recursive = TRUE)

  n_after_stage1  <- n_before_stage1 - n_excluded_s1
  n_before_stage2 <- n_after_stage1
  n_after_stage2  <- n_after_stage1 - n_excluded_s2
  n_excluded_hh   <- n_excluded_s2

  saveRDS(flux_data_qc, out_path)

  cat("Records input:                  ", n_before_stage1, "\n")
  if (!is_hh) {
    cat(sprintf("Excluded by flux_qc (stage 1):   %d (%s = %s)\n",
                n_before_stage1 - n_after_stage1, thresh_label, qc_thresh))
  }
  cat("Excluded by fluxnet_qc_hh (s2): ", n_excluded_hh,
      " (max_qc = 1)\n")
  cat("Records output:                 ", nrow(flux_data_qc), "\n")
  cat("Saved:", out_path, "\n")
}

cat("\n--- 04_qc.R: log file summary ---\n")
cat("exclusion_log.csv:", excl_log_path,
    if (file.exists(excl_log_path))
      paste0("(written, ", nrow(read.csv(excl_log_path)), " row(s))")
    else "(not written)", "\n")
cat("unknown_log.csv:  ", unknown_log_path,
    if (file.exists(unknown_log_path))
      paste0("(written, ", nrow(read.csv(unknown_log_path)), " row(s))")
    else "(not written)", "\n")
