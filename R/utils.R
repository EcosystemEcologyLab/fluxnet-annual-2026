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

# ---- Data-presence helpers ---------------------------------------------------

#' Compute NEE data presence per site per year
#'
#' Two-step lookup: (1) a site-year has data if annual (YY) `NEE_VUT_REF` is
#' non-NA; (2) for site-years absent or NA in the YY data, fall back to monthly
#' (MM) data and require at least `min_months` months with non-NA
#' `NEE_VUT_REF`. The result is saved to `out_path` for caching and used by
#' [is_functionally_active()] to assess data currency without relying solely on
#' snapshot `last_year` metadata.
#'
#' @param data_yy Data frame. Annual (YY resolution) flux data with columns
#'   `site_id`, `TIMESTAMP` (integer year), and `NEE_VUT_REF`.
#' @param data_mm Data frame. Monthly (MM resolution) flux data with columns
#'   `site_id`, `DATE` (Date class, first day of each month), and
#'   `NEE_VUT_REF`. Used only for site-years not resolved by `data_yy`.
#' @param min_months Integer. Minimum months with non-NA `NEE_VUT_REF` in the
#'   MM fallback for `has_data = TRUE` (default `3L`).
#' @param out_path Character. Path to write the output CSV
#'   (default `"data/snapshots/site_year_data_presence.csv"`).
#'
#' @return Data frame with columns `site_id` (character), `year` (integer),
#'   `has_data` (logical). Also writes this data frame to `out_path`.
#'
#' @examples
#' \dontrun{
#' yy       <- readRDS("data/processed/flux_data_converted_yy.rds")
#' mm       <- readRDS("data/processed/flux_data_converted_mm.rds")
#' presence <- compute_site_year_presence(yy, mm)
#' head(presence)
#' }
compute_site_year_presence <- function(data_yy,
                                        data_mm,
                                        min_months = 3L,
                                        out_path   = "data/snapshots/site_year_data_presence.csv") {
  for (pkg in c("dplyr", "readr")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required. Install with: install.packages('",
           pkg, "')", call. = FALSE)
    }
  }

  # Step 1 — YY primary: non-NA annual NEE means data is present
  # YY files use YEAR as the timestamp column (integer year)
  yy_ts_col <- if ("TIMESTAMP" %in% names(data_yy)) "TIMESTAMP" else "YEAR"
  yy_presence <- data_yy |>
    dplyr::transmute(
      site_id  = .data$site_id,
      year     = as.integer(.data[[yy_ts_col]]),
      has_data = !is.na(.data$NEE_VUT_REF)
    )

  # Step 2 — MM fallback: for site-years where YY NEE is NA or absent,
  # check whether >= min_months months have non-NA NEE_VUT_REF
  mm_presence <- data_mm |>
    dplyr::mutate(year = as.integer(format(.data$DATE, "%Y"))) |>
    dplyr::group_by(.data$site_id, .data$year) |>
    dplyr::summarise(
      n_months_valid = as.integer(sum(!is.na(.data$NEE_VUT_REF))),
      .groups        = "drop"
    ) |>
    dplyr::mutate(has_data = .data$n_months_valid >= as.integer(min_months)) |>
    dplyr::select("site_id", "year", "has_data")

  # Rows resolved by YY (has_data TRUE or FALSE where YY row exists)
  yy_true <- dplyr::filter(yy_presence, .data$has_data)

  # Site-years NOT already TRUE in YY: check MM fallback
  yy_false_or_absent <- dplyr::anti_join(
    dplyr::bind_rows(
      dplyr::filter(yy_presence, !.data$has_data),
      dplyr::anti_join(mm_presence, yy_presence, by = c("site_id", "year"))
    ),
    yy_true,
    by = c("site_id", "year")
  ) |>
    dplyr::select("site_id", "year") |>
    dplyr::left_join(mm_presence, by = c("site_id", "year")) |>
    dplyr::mutate(has_data = dplyr::coalesce(.data$has_data, FALSE))

  presence <- dplyr::bind_rows(yy_true, yy_false_or_absent) |>
    dplyr::arrange(.data$site_id, .data$year)

  readr::write_csv(presence, out_path)
  message(
    "compute_site_year_presence: ",
    sum(presence$has_data), " / ", nrow(presence),
    " site-years have data (YY primary, MM \u2265", min_months,
    " months fallback). Written to: ", out_path
  )

  presence
}

#' Test whether sites are functionally active at a reference year
#'
#' A site is *functionally active* in reference year `Y` if it has valid
#' NEE data (at least `min_months` non-NA `NEE_VUT_REF` months, as recorded
#' in `presence_df`) in at least one year within the window
#' `[Y − (active_threshold − 1), Y]`.
#'
#' When `presence_df` is `NULL` the function falls back to the simpler
#' metadata-based test: `last_year >= reference_year − active_threshold`.
#' Pass `last_year_vec` (parallel to `site_ids`) to enable the fallback.
#'
#' @param site_ids Character vector of FLUXNET site IDs to assess.
#' @param reference_year Integer. The reference year `Y`.
#' @param presence_df Data frame or `NULL`. Output of
#'   [compute_site_year_presence()], with columns `site_id`, `year`,
#'   and `has_data`. When `NULL`, the `last_year_vec` fallback is used.
#' @param active_threshold Integer. Window length: sites are assessed over
#'   `[reference_year − (active_threshold − 1), reference_year]`
#'   (default `4L`).
#' @param last_year_vec Integer vector (parallel to `site_ids`). Required
#'   when `presence_df` is `NULL`; ignored otherwise.
#'
#' @return Named logical vector (names = `site_ids`). `TRUE` if the site is
#'   functionally active at `reference_year`.
#'
#' @examples
#' \dontrun{
#' presence <- readr::read_csv("data/snapshots/site_year_data_presence.csv")
#' meta     <- readr::read_csv("data/snapshots/fluxnet_shuttle_snapshot_20260414T153648.csv")
#' flags    <- is_functionally_active(meta$site_id, 2025L, presence)
#' mean(flags)   # fraction of sites active
#' }
is_functionally_active <- function(site_ids,
                                    reference_year,
                                    presence_df      = NULL,
                                    active_threshold = 4L,
                                    last_year_vec    = NULL) {
  yr     <- as.integer(reference_year)
  thresh <- as.integer(active_threshold)

  # Fallback: metadata-only test (last_year >= reference_year - active_threshold)
  if (is.null(presence_df)) {
    if (is.null(last_year_vec)) {
      stop(
        "is_functionally_active: supply either presence_df or last_year_vec",
        call. = FALSE
      )
    }
    result        <- as.integer(last_year_vec) >= yr - thresh
    names(result) <- site_ids
    return(result)
  }

  # NEE presence-based test: window = [Y - (threshold - 1), Y]
  window <- seq.int(yr - (thresh - 1L), yr)

  # Sites with at least one window year flagged has_data == TRUE
  active_ids <- unique(
    presence_df$site_id[presence_df$year %in% window & presence_df$has_data]
  )

  result        <- site_ids %in% active_ids
  names(result) <- site_ids
  result
}
