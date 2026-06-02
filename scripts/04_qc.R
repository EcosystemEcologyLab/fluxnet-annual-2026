## 04_qc.R — Apply QC filtering per resolution
## See CLAUDE.md QC Flag Reference for flag definitions.
## 
## Creates a copy of each resolution's table with rows filtered out by QC 
## variable(s).
##
## Two-stage QC strategy:
##   Stage 1: at DD/MM/WW/YY resolution, _QC values are fractions 0–1.
##     `qc_flagged` (TRUE if p_gapfilled > 1-threshold) and `p_gapfilled` columns
##     are added; flagged rows are dropped and logged as exclusions. Rows with
##     qc_flagged == NA are kept. Stage 1 is skipped for HH/HR data.
##
##     Row exclusion uses VUT QC for sites that have NEE_VUT_REF (the majority),
##     and CUT QC for the ~36 sites where NEE_VUT_REF is absent (CUT-only sites).
##     When both are present, VUT is the reference and VUT QC is the gate.
##     Secondary variable QC columns (GPP, RECO, LE, H) are retained in the
##     output for per-variable filtering downstream but do not drive row exclusion.
##     See docs/decisions_pending.md and docs/known_issues.md Section 8.
##
##   Stage 2: at HH/HR resolution, _QC values are integers
##     0–3. Rows with any _QC > max_qc are dropped. At DD/MM/WW/YY this is a
##     no-op (all fractions ≤ 1 pass max_qc = 1L) but the call is retained so
##     the script is resolution-agnostic.

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
# library(fluxnet)
library(dplyr)
library(duckdb)

source("R/pipeline_config.R")
check_pipeline_config()

# source("R/qc.R")
source("R/utils.R")

db_path <- file.path(FLUXNET_DATA_ROOT, "duckdb/fluxnet.duckdb")
con <- dbConnect(duckdb(), db_path)

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


tables <- dbListTables(con)
stage_1_tables <- tables[tables %in% c("annual", "monthly", "weekly", "daily")]

for (table in stage_1_tables) {
  data <- tbl(con, table)

  has_vut_col <- "NEE_VUT_REF_QC" %in% colnames(data)
  has_cut_col <- "NEE_CUT_REF_QC" %in% colnames(data)

  if (!has_vut_col && !has_cut_col) {
    cli::cli_warn(c(
      "!" = "Neither NEE_VUT_REF_QC nor NEE_CUT_REF_QC found in {table} data",
      i = "Stage 1 skipped for this resolution."
    ))
    next
  }

  threshold <- switch(
    table,
    "annual"  = QC_THRESHOLD_YY,
    "monthly" = QC_THRESHOLD_MM,
    "weekly"  = QC_THRESHOLD_WW,
    "daily"   = QC_THRESHOLD_DD
  )

  suffix <- switch(
    table,
    "annual"  = "YY",
    "monthly" = "MM",
    "weekly"  = "WW",
    "daily"   = "DD"
  )

  # Precompute per-site QC column assignment.
  #
  # DuckDB ingest uses union_by_name, so CUT-only sites have NEE_VUT_REF_QC
  # present as a column but entirely NA — column absence is not the right
  # signal. Instead, count non-NA values per site to determine which QC
  # column to use. This is a per-site decision, not per-row: all rows for a
  # site use the same QC reference to avoid mixing VUT and CUT within a site.
  #
  # Priority: VUT (any non-NA) → CUT (any non-NA, VUT all-NA) → no gate.
  site_vut_counts <- data |>
    group_by(site_id) |>
    summarise(n_vut = sum(!is.na(NEE_VUT_REF_QC))) |>
    collect()

  if (has_cut_col) {
    site_cut_counts <- data |>
      group_by(site_id) |>
      summarise(n_cut = sum(!is.na(NEE_CUT_REF_QC))) |>
      collect()
    site_counts <- dplyr::left_join(site_vut_counts, site_cut_counts,
                                    by = "site_id")
  } else {
    site_counts <- dplyr::mutate(site_vut_counts, n_cut = 0L)
  }

  site_qc_flag <- site_counts |>
    dplyr::mutate(qc_col_used = dplyr::case_when(
      n_vut > 0L ~ "NEE_VUT_REF_QC",
      n_cut > 0L ~ "NEE_CUT_REF_QC",
      TRUE       ~ NA_character_
    )) |>
    dplyr::select(site_id, qc_col_used)

  n_vut <- sum(site_qc_flag$qc_col_used == "NEE_VUT_REF_QC", na.rm = TRUE)
  n_cut <- sum(site_qc_flag$qc_col_used == "NEE_CUT_REF_QC", na.rm = TRUE)
  n_none <- sum(is.na(site_qc_flag$qc_col_used))
  message(sprintf("  %s QC: %d VUT-gated, %d CUT-gated, %d no-QC sites",
                  suffix, n_vut, n_cut, n_none))

  # Join the per-site flag back to the full lazy table (copy = TRUE writes the
  # small flag frame to a DuckDB temp table for the join). Then apply the
  # per-site-appropriate QC column via CASE WHEN — not a row-by-row coalesce.
  data_qc <- data |>
    dplyr::left_join(site_qc_flag, by = "site_id", copy = TRUE) |>
    dplyr::mutate(
      p_gapfilled = dplyr::case_when(
        qc_col_used == "NEE_VUT_REF_QC" ~ 1 - NEE_VUT_REF_QC,
        qc_col_used == "NEE_CUT_REF_QC" ~ 1 - NEE_CUT_REF_QC,
        TRUE                            ~ NA_real_
      )
    ) |>
    dplyr::mutate(qc_flagged = p_gapfilled > threshold)

  # If the _qc table already exists, drop it before recomputing.
  qc_name <- glue::glue("{table}_qc")
  current_tables <- dbListTables(con)
  if (qc_name %in% current_tables) {
    dbExecute(con, glue::glue("DROP TABLE {qc_name}"))
  }
  data_qc |>
    dplyr::filter(is.na(qc_flagged) | !qc_flagged) |>
    dplyr::compute(name = qc_name, temporary = FALSE)

  # Exclusion log — group by qc_col_used so the audit trail records whether
  # each excluded row was gated on VUT QC or CUT QC.
  excluded <- data_qc |>
    dplyr::filter(qc_flagged) |>
    dplyr::select(site_id, qc_col_used, dplyr::starts_with("TIMESTAMP")) |>
    collect()

  if (nrow(excluded) > 0L) {
    ts_col <- grep("^TIMESTAMP", names(excluded), value = TRUE)[1]
    for (qc_col in unique(excluded$qc_col_used[!is.na(excluded$qc_col_used)])) {
      excl_sub <- excluded[!is.na(excluded$qc_col_used) &
                             excluded$qc_col_used == qc_col, ]
      log_exclusion(
        site_id     = excl_sub$site_id,
        variable    = qc_col,
        timestamp   = excl_sub[[ts_col]],
        reason      = glue::glue(
          "{qc_col} p_gapfilled > {round(1 - threshold, 2)} ",
          "(QC_THRESHOLD_{suffix}={threshold})"
        ),
        threshold   = glue::glue("QC_THRESHOLD_{suffix}={threshold}"),
        excluded_by = "04_qc.R"
      )
    }
  }
}

# Do hourly QC differently as sub-daily data uses integer _QC flags (Stage 2
# only; skip Stage 1).
if ("hourly" %in% tables) {
  hourly <- tbl(con, "hourly")
  hourly_qc <- hourly |>
    mutate(qc_flagged = if_any(ends_with("_QC"), \(x) as.integer(x) > 1L))

  # If QC table already exists, remove it
  if ("hourly_qc" %in% tables) {
    dbExecute(con, "DROP TABLE hourly_qc")
  }
  hourly_qc |>
    filter(!qc_flagged | is.na(qc_flagged)) |>
    compute(name = "hourly_qc", temporary = FALSE)

  excluded <- hourly_qc |>
    filter(qc_flagged) |>
    select(site_id, starts_with("TIMESTAMP")) |>
    collect()

  log_exclusion(
    site_id = excluded[[1]],
    variable = "ALL",
    timestamp = excluded[[2]],
    reason = "*_QC column(s) > 1",
    threshold = "max QC flag = 1",
    excluded_by = "04_qc.R"
  )
}

dbDisconnect(con)

cat("\n--- 04_qc.R: log file summary ---\n")
cat("exclusion_log.csv:", excl_log_path,
    if (file.exists(excl_log_path))
      paste0("(written, ", nrow(read.csv(excl_log_path)), " row(s))")
    else "(not written)", "\n")
cat("unknown_log.csv:  ", unknown_log_path,
    if (file.exists(unknown_log_path))
      paste0("(written, ", nrow(read.csv(unknown_log_path)), " row(s))")
    else "(not written)", "\n")
