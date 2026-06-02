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
##     Row exclusion gated on NEE_VUT_REF_QC only — primary variable for the
##     FLUXNET Annual Paper 2026. Secondary variable QC columns (GPP, RECO,
##     LE, H) are retained in the output for per-variable filtering downstream
##     but do not drive row exclusion. See docs/decisions_pending.md.
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

  # TODO: if no NEE_VUT_REF_QC for a particular site, fall back on NEE_CUT_REF_QC

  qc_col <- "NEE_VUT_REF_QC"

  threshold <- switch(
    table,
    "annual" = QC_THRESHOLD_YY,
    "monthly" = QC_THRESHOLD_MM,
    "weekly" = QC_THRESHOLD_WW,
    "daily" = QC_THRESHOLD_DD
  )

  suffix <- switch(
    table,
    "annual" = "YY",
    "monthly" = "MM",
    "weekly" = "WW",
    "daily" = "DD"
  )

  if (!qc_col %in% colnames(data)) {
    cli::cli_warn(c(
      "!" = "QC column {.var {qc_col}} not found in {table} data",
      i = "Stage 1 skipped for this resolution."
    ))
    data_qc <- data
    data_qc$p_gapfilled <- NA_real_
    data_qc$qc_flagged <- NA
    return(data)
  } else {
    data_qc <- data |>
      mutate(
        p_gapfilled = 1 - .data[[qc_col]],
        qc_flagged = p_gapfilled > threshold
      )
  }

  # This copies the entire QC-filtered table into the database.  Maybe not the
  # best way to do things?  Probably better to just use SQL to create the
  # `qc_flagged` column in each table and then downstream scripts can just start
  # with `filter(!qc_flagged)`.

  # If qc table already exists, delete it
  qc_name <- glue::glue("{table}_qc")
  if (qc_name %in% tables) {
    dbExecute(con, glue::glue("DROP TABLE {qc_name}"))
  }
  data_qc |>
    filter(is.na(qc_flagged) | !qc_flagged) |>
    dplyr::compute(name = glue::glue("{table}_qc"), temporary = FALSE)

  excluded <- data_qc |>
    filter(qc_flagged) |>
    select(site_id, starts_with("TIMESTAMP")) |>
    collect()

  log_exclusion(
    site_id = excluded[[1]],
    variable = "ALL",
    timestamp = excluded[[2]],
    reason = glue::glue(
      "{qc_col} p_gapfilled > {round(1 - threshold, 2)} (QC_THRESHOLD_{suffix}={threshold})"
    ),
    threshold = glue::glue("QC_THRESHOLD_{suffix}={threshold}"),
    excluded_by = "04_qc.R"
  )
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
