## store_audit_stage3_rederivation.R -- independent re-derivation, no repo helpers.
## Computes mean annual precipitation (P_ERA, P_F) and mean annual NEE_VUT_REF
## for 5 sites directly from the raw *_FLUXNET_FLUXMET_YY_*.csv files on disk,
## using base R only (no R/units.R, R/utils.R, or any pipeline script), and
## compares against the pipeline's DuckDB annual_converted table for the same
## sites. Sites chosen from Stage 1's "unchanged metadata" set, so any
## divergence found here implicates pipeline logic, not file staleness.
##
## Usage: Rscript scripts/diagnostics/store_audit_stage3_rederivation.R

if (file.exists(".env")) { library(dotenv); dotenv::load_dot_env() }
source("R/pipeline_config.R")
check_pipeline_config()

SITES <- c("IT-Lav", "AU-Ctr", "BE-Lcr", "US-KS2", "AU-DaP")
QC_THRESHOLD_YY_INDEPENDENT <- 0.50  # hard-coded here, not sourced from pipeline_config.R -- keeps this check independent

find_yy_file <- function(site_id) {
  dirs <- list.dirs("data/extracted", recursive = FALSE, full.names = TRUE)
  d <- dirs[grepl(paste0("_", site_id, "_FLUXNET_"), basename(dirs))]
  if (length(d) == 0) return(NA_character_)
  f <- list.files(d[[1]], pattern = "FLUXMET_YY_.*\\.csv$", full.names = TRUE)
  if (length(f) == 0) return(NA_character_)
  f[[1]]
}

# Minimal, from-scratch CSV reader -- base R only, no readr.
read_csv_base <- function(path) {
  con <- file(path, "r")
  on.exit(close(con))
  header <- strsplit(readLines(con, n = 1), ",", fixed = TRUE)[[1]]
  lines <- readLines(con)
  lines <- lines[nzchar(lines)]
  split_lines <- strsplit(lines, ",", fixed = TRUE)
  m <- do.call(rbind, split_lines)
  colnames(m) <- header
  as.data.frame(m, stringsAsFactors = FALSE)
}

results <- list()
for (site_id in SITES) {
  f <- find_yy_file(site_id)
  if (is.na(f)) { cat(site_id, ": no YY file found\n"); next }
  df <- read_csv_base(f)

  num <- function(col) suppressWarnings(as.numeric(df[[col]]))
  p_era <- num("P_ERA"); p_f <- num("P_F")
  nee <- num("NEE_VUT_REF"); nee_qc <- num("NEE_VUT_REF_QC")

  # Precipitation: annual resolution -- values are already per-year totals
  # (CLAUDE.md: "P | mm per timestep | mm per period | Sum, not average";
  # at YY resolution the timestep IS the year, no day-weighting needed).
  valid_p_era <- p_era[!is.na(p_era) & p_era != -9999]
  valid_p_f   <- p_f[!is.na(p_f) & p_f != -9999]
  mean_p_era <- if (length(valid_p_era) > 0) mean(valid_p_era) else NA_real_
  mean_p_f   <- if (length(valid_p_f) > 0) mean(valid_p_f) else NA_real_

  # NEE: annual resolution, carbon flux pre-integrated at DD/MM/WW/YY per
  # CLAUDE.md ("carbon at DD/MM/WW/YY passes through unchanged"), QC is a
  # 0-1 fraction at YY resolution -- keep years with QC > 0.50, independent
  # threshold chosen to mirror (not reuse) R/pipeline_config.R's QC_THRESHOLD_YY.
  keep <- !is.na(nee) & nee != -9999 & !is.na(nee_qc) & nee_qc > QC_THRESHOLD_YY_INDEPENDENT
  mean_nee <- if (sum(keep) > 0) mean(nee[keep]) else NA_real_
  n_nee_years <- sum(keep)

  results[[site_id]] <- data.frame(
    site_id = site_id, file = f,
    n_years_total = nrow(df),
    n_years_p = length(valid_p_era),
    mean_p_era_independent = mean_p_era,
    mean_p_f_independent = mean_p_f,
    n_years_nee = n_nee_years,
    mean_nee_independent = mean_nee
  )
  cat(sprintf("%s: mean_P_ERA=%.2f (n=%d yr)  mean_P_F=%.2f  mean_NEE=%.3f (n=%d yr, QC>%.2f)\n",
              site_id, mean_p_era, length(valid_p_era), mean_p_f, mean_nee, n_nee_years, QC_THRESHOLD_YY_INDEPENDENT))
}
independent_df <- do.call(rbind, results)

# --- Compare against the pipeline's own DuckDB annual_converted table ---
suppressPackageStartupMessages(library(DBI))
con <- DBI::dbConnect(duckdb::duckdb(), "data/duckdb/fluxnet.duckdb", read_only = TRUE)
## annual_converted stores TWO rows per year-slot per site: dataset='ERA5'
## (standalone ERA5_YY file, full 1981-2025 record) and dataset='FLUXMET'
## (FLUXMET_YY file's own embedded P_ERA column, tower-operational years
## only). This script reads FLUXMET_YY independently above, so it must
## compare against dataset='FLUXMET' rows only -- an unfiltered query
## blends both and produces a spurious ~2-10% "divergence" (see note below).
ac <- DBI::dbGetQuery(con, sprintf(
  "SELECT site_id, TIMESTAMP, P_ERA, P_F, NEE_VUT_REF, NEE_VUT_REF_QC FROM annual_converted WHERE site_id IN (%s) AND dataset = 'FLUXMET'",
  paste(sprintf("'%s'", SITES), collapse = ",")
))
DBI::dbDisconnect(con, shutdown = TRUE)

pipeline_summary <- do.call(rbind, lapply(SITES, function(s) {
  sub <- ac[ac$site_id == s, ]
  p_era_v <- sub$P_ERA[!is.na(sub$P_ERA) & sub$P_ERA != -9999]
  p_f_v   <- sub$P_F[!is.na(sub$P_F) & sub$P_F != -9999]
  keep <- !is.na(sub$NEE_VUT_REF) & sub$NEE_VUT_REF != -9999 &
          !is.na(sub$NEE_VUT_REF_QC) & sub$NEE_VUT_REF_QC > 0.50
  data.frame(site_id = s,
             mean_p_era_pipeline = if (length(p_era_v) > 0) mean(p_era_v) else NA_real_,
             mean_p_f_pipeline   = if (length(p_f_v) > 0) mean(p_f_v) else NA_real_,
             n_years_nee_pipeline = sum(keep),
             mean_nee_pipeline = if (sum(keep) > 0) mean(sub$NEE_VUT_REF[keep]) else NA_real_)
}))

comparison <- merge(independent_df, pipeline_summary, by = "site_id")
comparison$diff_p_era <- comparison$mean_p_era_independent - comparison$mean_p_era_pipeline
comparison$diff_nee   <- comparison$mean_nee_independent - comparison$mean_nee_pipeline

fs::dir_create("review/diagnostics/store_audit")
write.csv(comparison, "review/diagnostics/store_audit/table_stage3_independent_rederivation.csv", row.names = FALSE)

cat("\n=== Stage 3: independent-derivation vs pipeline (DuckDB annual_converted) ===\n")
print(comparison[, c("site_id","mean_p_era_independent","mean_p_era_pipeline","diff_p_era",
                      "mean_nee_independent","mean_nee_pipeline","diff_nee")])
cat("\nMax |diff_p_era|:", max(abs(comparison$diff_p_era), na.rm = TRUE), "\n")
cat("Max |diff_nee|:", max(abs(comparison$diff_nee), na.rm = TRUE), "\n")

# ============================================================================
# NOTE (kept for transparency): an earlier version of this script's DuckDB
# query did not filter by `dataset`, and found apparent P_ERA divergences up
# to 412 mm/yr. That was a bug in THIS script, not the pipeline:
# annual_converted stores TWO rows per year-slot per site -- dataset='ERA5'
# (from the standalone ERA5_YY file, full 1981-2025 reanalysis record) and
# dataset='FLUXMET' (from the FLUXMET_YY file's own embedded P_ERA column,
# tower-operational years only) -- and an unfiltered SELECT blends both.
# Any query against annual/annual_qc/annual_converted MUST filter by
# `dataset` (see era5_precip_units_v2's report, which independently found
# and documented the same WHERE dataset='...' requirement). Filtering to
# dataset='FLUXMET' (matching what this script reads independently) gives
# EXACT agreement (diffs ~1e-12, floating-point noise) for P_ERA, P_F, and
# NEE_VUT_REF at all 5 sites -- see table_stage3_independent_rederivation_fixed.csv.
# ============================================================================
