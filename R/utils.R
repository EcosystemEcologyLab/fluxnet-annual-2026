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

#' Compute flux data presence per site per year from monthly data
#'
#' For each site-month, a month is considered "present" if at least one of the
#' specified flux variables has a non-NA value. `n_months_present` counts how
#' many months in each year meet this condition. The output includes one row per
#' site-year for every year in the range `[first_year, last_year]` for that
#' site, including gap years (where `n_months_present` = 0). Gap years are
#' included so the file shows operational gaps explicitly.
#'
#' `has_data` (`n_months_present > 0`) is included for backward compatibility
#' with [is_functionally_active()] and other callers that expect a logical flag.
#'
#' @param data_mm Data frame. Monthly (MM resolution) flux data with columns
#'   `site_id` and `DATE` (character or Date, first day of each month), plus
#'   the flux variable columns listed in `flux_vars`.
#' @param flux_vars Character vector. Column names to check for non-NA values.
#'   Columns absent from `data_mm` are silently ignored. Defaults to the five
#'   primary flux variables (NEE, GPP, RECO, LE, H) across VUT and CUT
#'   processing paths, as agreed 2026-05-07.
#' @param out_path Character. Path to write the output CSV
#'   (default `"data/snapshots/site_year_data_presence.csv"`).
#'
#' @return Data frame with columns `site_id` (character), `year` (integer),
#'   `n_months_present` (integer, 0–12), `has_data` (logical). Also writes
#'   this data frame to `out_path`.
#'
#' @examples
#' \dontrun{
#' mm       <- readRDS("data/processed/flux_data_raw_mm.rds")
#' presence <- compute_site_year_presence(mm)
#' head(presence)
#' }
compute_site_year_presence <- function(
    data_mm,
    flux_vars = c(
      "NEE_VUT_REF", "NEE_CUT_REF",
      "GPP_NT_VUT_REF", "GPP_DT_VUT_REF", "GPP_NT_CUT_REF", "GPP_DT_CUT_REF",
      "RECO_NT_VUT_REF", "RECO_DT_VUT_REF", "RECO_NT_CUT_REF", "RECO_DT_CUT_REF",
      "LE_F_MDS", "H_F_MDS"
    ),
    out_path = "data/snapshots/site_year_data_presence.csv") {

  for (pkg in c("dplyr", "tidyr", "readr")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required. Install with: install.packages('",
           pkg, "')", call. = FALSE)
    }
  }

  # Keep only flux_vars that are actually present in data_mm
  flux_vars_found <- intersect(flux_vars, names(data_mm))
  if (length(flux_vars_found) == 0L) {
    stop(
      "compute_site_year_presence: none of the requested flux_vars found in data_mm. ",
      "Columns available: ", paste(names(data_mm), collapse = ", "),
      call. = FALSE
    )
  }
  flux_vars_missing <- setdiff(flux_vars, flux_vars_found)
  if (length(flux_vars_missing) > 0L) {
    message(
      "compute_site_year_presence: ", length(flux_vars_missing),
      " flux_var(s) not found in data_mm and will be ignored: ",
      paste(flux_vars_missing, collapse = ", ")
    )
  }

  # Parse year from DATE (handles character "YYYY-MM-DD" or Date class)
  mm_parsed <- data_mm |>
    dplyr::mutate(
      year          = as.integer(format(as.Date(.data$DATE), "%Y")),
      month_present = rowSums(!is.na(dplyr::across(
        dplyr::all_of(flux_vars_found)
      ))) > 0L
    )

  # Count months present per site-year
  site_year_counts <- mm_parsed |>
    dplyr::group_by(.data$site_id, .data$year) |>
    dplyr::summarise(
      n_months_present = as.integer(sum(.data$month_present)),
      .groups          = "drop"
    )

  # Build a complete year grid: every year from first_year to last_year per site,
  # so gap years with 0 months of data appear explicitly.
  site_ranges <- site_year_counts |>
    dplyr::group_by(.data$site_id) |>
    dplyr::summarise(
      first_year = min(.data$year),
      last_year  = max(.data$year),
      .groups    = "drop"
    )

  complete_grid <- site_ranges |>
    dplyr::rowwise() |>
    dplyr::mutate(year = list(seq.int(.data$first_year, .data$last_year))) |>
    tidyr::unnest(cols = "year") |>
    dplyr::ungroup() |>
    dplyr::select("site_id", "year") |>
    dplyr::mutate(year = as.integer(.data$year))

  # Join counts; gap years get n_months_present = 0
  presence <- complete_grid |>
    dplyr::left_join(site_year_counts, by = c("site_id", "year")) |>
    dplyr::mutate(
      n_months_present = dplyr::coalesce(.data$n_months_present, 0L),
      has_data         = .data$n_months_present > 0L
    ) |>
    dplyr::arrange(.data$site_id, .data$year)

  readr::write_csv(presence, out_path)
  message(
    "compute_site_year_presence: ",
    sum(presence$has_data), " / ", nrow(presence),
    " site-years have data (", length(flux_vars_found), " flux vars, any non-NA). ",
    "Written to: ", out_path
  )

  presence
}

#' Test whether sites are functionally active at a reference year
#'
#' A site is *functionally active* in reference year `Y` if `presence_df`
#' records `has_data = TRUE` (any non-NA value across NEE VUT/CUT, GPP, RECO,
#' LE, or H — as computed by [compute_site_year_presence()]) for at least one
#' year within the window `[Y − (active_threshold − 1), Y]`. CUT-only sites
#' (with valid `NEE_CUT_REF` but no `NEE_VUT_REF`) are included when
#' `presence_df` was built with the default `flux_vars`.
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

  # Presence-based test: window = [Y - (threshold - 1), Y]
  window <- seq.int(yr - (thresh - 1L), yr)

  # Derive has_data from n_months_present if needed (new schema compat)
  if (!"has_data" %in% names(presence_df) && "n_months_present" %in% names(presence_df)) {
    presence_df <- dplyr::mutate(presence_df, has_data = .data$n_months_present > 0L)
  }

  # Sites with at least one window year flagged has_data == TRUE
  active_ids <- unique(
    presence_df$site_id[presence_df$year %in% window & presence_df$has_data]
  )

  result        <- site_ids %in% active_ids
  names(result) <- site_ids
  result
}
