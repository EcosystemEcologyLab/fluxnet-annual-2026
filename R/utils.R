# Shared utility functions for the fluxnet-annual-2026 pipeline.

#' Write output metadata companion file
#'
#' Writes a `.meta.json` companion file alongside an output file, recording
#' provenance metadata required by CLAUDE.md. Also appends the current R
#' session info to `outputs/session_info.txt`.
#'
#' @param output_path Path to the output file (e.g. `"outputs/nee_annual.csv"`).
#'   The companion file will be written to the same directory with the same base
#'   name and a `.meta.json` extension.
#' @param input_sources Character vector of URLs, DOIs, or file paths of all
#'   primary inputs used to produce this output.
#' @param notes Free-text string recording any manual decisions, overrides, or
#'   deviations from defaults. Empty string if none — field must always be present.
#'
#' @return Invisibly returns the path to the companion `.meta.json` file.
#'
#' @examples
#' \dontrun{
#' write_output_metadata(
#'   "outputs/nee_annual.csv",
#'   input_sources = c("data/snapshots/fluxnet_shuttle_snapshot_20260327T182556.csv"),
#'   notes = ""
#' )
#' }
write_output_metadata <- function(output_path, input_sources, notes = "") {
  if (!is.character(notes) || length(notes) != 1) {
    stop("notes must be a single character string (use empty string if none)")
  }

  meta <- list(
    run_datetime_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    pipeline_version = system("git rev-parse --short HEAD", intern = TRUE),
    input_sources    = as.list(input_sources),
    notes            = notes
  )

  meta_path <- paste0(tools::file_path_sans_ext(output_path), ".meta.json")
  writeLines(jsonlite::toJSON(meta, pretty = TRUE, auto_unbox = TRUE), meta_path)

  # Append session info to outputs/session_info.txt
  session_info_path <- file.path("outputs", "session_info.txt")
  if (!dir.exists("outputs")) dir.create("outputs", recursive = TRUE)
  sink(session_info_path, append = TRUE)
  cat("\n---\n")
  cat(format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), "\n")
  print(sessionInfo())
  sink()

  invisible(meta_path)
}

#' Log an excluded record to outputs/exclusion_log.csv
#'
#' Appends one row to `outputs/exclusion_log.csv`, creating the file with
#' headers if it does not yet exist. Call this function for every record
#' dropped because it failed a QC threshold or inclusion rule.
#'
#' @param site_id FLUXNET site ID (e.g. `"US-Ha1"`).
#' @param variable Variable name, or `"ALL"` if the whole record is excluded.
#' @param timestamp Record timestamp, or `"ALL"` if the whole site-year is excluded.
#' @param reason Human-readable reason (e.g. `"QC_THRESHOLD_YY=0.75 not met"`).
#' @param threshold The threshold or rule applied (e.g. `"QC_THRESHOLD_YY=0.75"`).
#' @param excluded_by Script name that performed the exclusion (e.g. `"04_qc.R"`).
#'
#' @return Invisibly returns the path to the exclusion log.
log_exclusion <- function(site_id, variable, timestamp, reason, threshold, excluded_by) {
  log_path <- file.path("outputs", "exclusion_log.csv")
  if (!dir.exists("outputs")) dir.create("outputs", recursive = TRUE)

  row <- data.frame(
    site_id     = as.character(site_id),
    variable    = as.character(variable),
    timestamp   = as.character(timestamp),
    reason      = as.character(reason),
    threshold   = as.character(threshold),
    excluded_by = as.character(excluded_by),
    stringsAsFactors = FALSE
  )

  write_log_row(log_path, row)
  invisible(log_path)
}

#' Log an unknown record to outputs/unknown_log.csv
#'
#' Appends one row to `outputs/unknown_log.csv`, creating the file with
#' headers if it does not yet exist. Call this function for every record
#' that cannot be assessed — i.e. missing data, failed download, or
#' unassessable quality. Do not use this for records that fail QC thresholds
#' (use `log_exclusion()` for those).
#'
#' @param record_id Site ID or record identifier.
#' @param reason Why the record could not be assessed.
#' @param logged_by Script name (e.g. `"04_qc.R"`).
#'
#' @return Invisibly returns the path to the unknown log.
log_unknown <- function(record_id, reason, logged_by) {
  log_path <- file.path("outputs", "unknown_log.csv")
  if (!dir.exists("outputs")) dir.create("outputs", recursive = TRUE)

  row <- data.frame(
    record_id = as.character(record_id),
    reason    = as.character(reason),
    logged_by = as.character(logged_by),
    stringsAsFactors = FALSE
  )

  write_log_row(log_path, row)
  invisible(log_path)
}

# Internal helper: append a row to a CSV log, writing headers if file is new.
write_log_row <- function(path, row) {
  if (file.exists(path)) {
    utils::write.table(
      row, path,
      sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE, quote = TRUE
    )
  } else {
    utils::write.csv(row, path, row.names = FALSE, quote = TRUE)
  }
}
