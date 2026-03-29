## 04_qc.R — Apply QC filtering
## Uses: flux_qc(), fluxnet_qc_hh()
## See CLAUDE.md QC Flag Reference for flag definitions.
##
## Two-stage QC strategy:
##   Stage 1 (flux_qc): at DD/MM/WW/YY resolution, _QC values are fractions
##     0–1. flux_qc() adds qc_flagged (TRUE if p_gapfilled > 1-QC_THRESHOLD_YY)
##     and p_gapfilled columns, then records with qc_flagged == TRUE are dropped
##     and logged as exclusions.  Rows with qc_flagged == NA (e.g. ERA5 rows
##     that have no _QC columns) are kept.
##
##   Stage 2 (fluxnet_qc_hh): at HH/HR resolution, _QC values are integers
##     0–3. Rows with any _QC > max_qc are dropped. At DD/MM/WW/YY this is a
##     no-op (all fractions ≤ 1 pass max_qc = 1L) but the call is retained so
##     the script is resolution-agnostic.

source("R/pipeline_config.R")
check_pipeline_config()

source("R/qc.R")
source("R/utils.R")

library(fluxnet)
library(dplyr)
if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}

processed_dir <- file.path(FLUXNET_DATA_ROOT, "processed")

# Initialise both log files so they exist even if nothing is excluded/unknown
if (!dir.exists("outputs")) dir.create("outputs", recursive = TRUE)
excl_log_path    <- file.path("outputs", "exclusion_log.csv")
unknown_log_path <- file.path("outputs", "unknown_log.csv")
if (!file.exists(excl_log_path)) {
  utils::write.csv(
    data.frame(site_id=character(), variable=character(), timestamp=character(),
               reason=character(), threshold=character(), excluded_by=character()),
    excl_log_path, row.names = FALSE
  )
}
if (!file.exists(unknown_log_path)) {
  utils::write.csv(
    data.frame(record_id=character(), reason=character(), logged_by=character()),
    unknown_log_path, row.names = FALSE
  )
}

flux_data <- readRDS(file.path(processed_dir, "flux_data_raw.rds"))

# --- Stage 1: fractional _QC threshold (DD/MM/WW/YY) ---
# Derive qc_vars from _QC columns present in the data (strips _QC suffix).
# flux_qc() aborts on HH data; safe here because flux_read() defaults to YY.
qc_col_names <- grep("_QC$", names(flux_data), value = TRUE)
qc_vars      <- sub("_QC$", "", qc_col_names)
flux_data_qc <- flux_qc(flux_data, qc_vars = qc_vars, max_gapfilled = 1 - QC_THRESHOLD_YY)

# Drop rows flagged by flux_qc() and log them
n_before_stage1 <- nrow(flux_data_qc)
flagged_idx     <- which(!is.na(flux_data_qc$qc_flagged) & flux_data_qc$qc_flagged)

for (i in flagged_idx) {
  ts_col <- intersect(c("YEAR", "DATE", "DATETIME"), names(flux_data_qc))
  ts_val <- if (length(ts_col) > 0) as.character(flux_data_qc[[ts_col[[1]]]][i]) else "ALL"
  log_exclusion(
    site_id     = flux_data_qc$site_id[i],
    variable    = "ALL",
    timestamp   = ts_val,
    reason      = paste0("flux_qc: p_gapfilled > ", round(1 - QC_THRESHOLD_YY, 2),
                         " (QC_THRESHOLD_YY = ", QC_THRESHOLD_YY, ")"),
    threshold   = paste0("QC_THRESHOLD_YY=", QC_THRESHOLD_YY),
    excluded_by = "04_qc.R"
  )
}

flux_data_qc  <- flux_data_qc[is.na(flux_data_qc$qc_flagged) | !flux_data_qc$qc_flagged, ]
n_after_stage1 <- nrow(flux_data_qc)

# --- Stage 2: integer _QC flag filter (HH/HR) ---
# No-op at YY/DD/MM/WW (all fractions ≤ 1), but retained for when HH data
# is passed through this script.
n_before_stage2 <- nrow(flux_data_qc)
flux_data_qc    <- fluxnet_qc_hh(flux_data_qc, max_qc = 1L)
n_after_stage2  <- nrow(flux_data_qc)
n_excluded_hh   <- n_before_stage2 - n_after_stage2

saveRDS(flux_data_qc, file.path(processed_dir, "flux_data_qc.rds"))

# Summary
n_total_excluded <- (n_before_stage1 - n_after_stage1) + n_excluded_hh
cat("\n--- 04_qc.R summary ---\n")
cat("Records input:                  ", n_before_stage1, "\n")
cat("Excluded by flux_qc (stage 1):  ", n_before_stage1 - n_after_stage1,
    " (QC_THRESHOLD_YY =", QC_THRESHOLD_YY, ")\n")
cat("Excluded by fluxnet_qc_hh (s2): ", n_excluded_hh,
    " (max_qc = 1)\n")
cat("Records output:                 ", nrow(flux_data_qc), "\n")
cat("exclusion_log.csv:", excl_log_path,
    if (file.exists(excl_log_path))
      paste0("(written, ", nrow(read.csv(excl_log_path)), " row(s))")
    else "(not written)", "\n")
cat("unknown_log.csv:  ", unknown_log_path,
    if (file.exists(unknown_log_path))
      paste0("(written, ", nrow(read.csv(unknown_log_path)), " row(s))")
    else "(not written)", "\n")
