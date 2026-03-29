# R/sync.R — Snapshot comparison and update detection for the FLUXNET pipeline.
#
# Detects new sites, extended data coverage, and reprocessed products between
# consecutive Shuttle snapshot CSVs by comparing fluxnet_product_name,
# first_year/last_year, and product_id fields.
#
# OPEN QUESTIONS — pending response from Gilberto's team (support@fluxnet.org):
#
#   Q1 (ICOS/TERN product_id stability): Do ICOS/TERN product_id hash values
#      change when a site is reprocessed, or are they stable across updates
#      within the same ONEFlux version? Until confirmed, ICOS/TERN reprocessing
#      cannot be detected via product_id comparison.
#
#   Q2 (release increment semantics): What does a release increment (_r2, _r3)
#      signify for AmeriFlux — new data years, corrected flux values, or
#      metadata-only changes? Affects whether reprocessed_ameriflux sites
#      require full re-extraction or metadata refresh only.
#
#   Q3 (ICOS/TERN product name convention): Do ICOS and TERN
#      fluxnet_product_name values follow the same _vX.X_rN convention as
#      AmeriFlux, making them comparable between snapshots?
#
#   Q4 (push notifications): Is there any push-based update notification
#      mechanism, or is polling listall() the only way to detect changes?


# Required columns for a valid snapshot CSV (shuttle schema v0.0.1).
.SNAPSHOT_REQUIRED_COLS <- c(
  "data_hub", "site_id", "first_year", "last_year",
  "fluxnet_product_name", "product_id", "oneflux_code_version",
  "product_source_network"
)


#' Load and validate a Shuttle snapshot CSV
#'
#' Reads a snapshot CSV produced by `flux_listall()` / `write_snapshot()` and
#' checks that all required columns are present. Stops with a clear message if
#' any required column is missing.
#'
#' @param path Path to the snapshot CSV file.
#' @return A data frame with one row per site-product.
#'
#' @note **Schema stability caveat:** column validation is against the shuttle
#'   v0.0.1 schema. If Gilberto's team renames or removes columns in a future
#'   release (open question Q4 — contact support@fluxnet.org), this function
#'   will stop with a missing-column error, which is the intended behaviour.
#'
#' @seealso [write_snapshot()], [compare_snapshots()]
#' @export
load_snapshot <- function(path) {
  if (!file.exists(path)) {
    stop("Snapshot file not found: ", path)
  }
  df <- readr::read_csv(path, show_col_types = FALSE)
  missing_cols <- setdiff(.SNAPSHOT_REQUIRED_COLS, names(df))
  if (length(missing_cols) > 0) {
    stop(
      "Snapshot CSV is missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      "\nFile: ", path
    )
  }
  df
}


#' Find the previous snapshot file
#'
#' Scans `snapshots_dir` for CSV files matching the shuttle snapshot filename
#' pattern and returns the path to the second-most-recent file (sorted
#' lexicographically by filename, which is equivalent to chronological order
#' because the timestamp is embedded as `YYYYMMDDTHHMMSS`). Returns `NULL` if
#' fewer than two snapshot files exist.
#'
#' @param snapshots_dir Directory to search. Defaults to
#'   `file.path(FLUXNET_DATA_ROOT, "snapshots")`.
#' @return Character path to the previous snapshot CSV, or `NULL`.
#'
#' @seealso [load_snapshot()], [compare_snapshots()]
#' @export
find_previous_snapshot <- function(
    snapshots_dir = file.path(FLUXNET_DATA_ROOT, "snapshots")) {
  candidates <- sort(
    fs::dir_ls(
      snapshots_dir,
      regexp = "fluxnet_shuttle_snapshot_\\d{8}T\\d{6}\\.csv$"
    )
  )
  if (length(candidates) < 2) {
    return(NULL)
  }
  candidates[[length(candidates) - 1L]]
}


#' Compare two Shuttle snapshots to detect new and updated sites
#'
#' Compares a current snapshot against a previous one and classifies sites into
#' four change categories. Each non-NULL category is a data frame of site rows
#' taken from the *current* snapshot.
#'
#' @section Change categories:
#' \describe{
#'   \item{`new_sites`}{Sites present in `current` but absent from `previous`
#'     (matched on `site_id`).}
#'   \item{`extended_data`}{Sites present in both snapshots where `first_year`
#'     decreased or `last_year` increased — i.e., the temporal coverage
#'     expanded.}
#'   \item{`reprocessed_ameriflux`}{AmeriFlux sites present in both snapshots
#'     where `fluxnet_product_name` changed. Catches release increments
#'     (_r1 → _r2) and ONEFlux version bumps. Sites already in `extended_data`
#'     are excluded to avoid double-counting.}
#'   \item{`reprocessed_icos_tern`}{Always `NULL`. See note below.}
#' }
#'
#' @param current A data frame from [load_snapshot()] — the newer snapshot.
#' @param previous A data frame from [load_snapshot()] — the older snapshot.
#' @return A named list with elements `new_sites`, `extended_data`,
#'   `reprocessed_ameriflux`, and `reprocessed_icos_tern`.
#'
#' @note **ICOS/TERN reprocessing detection is not yet implemented.**
#'   Open questions Q1 and Q3 (contact support@fluxnet.org) must be resolved
#'   before ICOS/TERN reprocessing can be detected reliably:
#'   \itemize{
#'     \item Q1: Are ICOS/TERN `product_id` hashes stable across updates?
#'     \item Q3: Do ICOS/TERN `fluxnet_product_name` values follow the
#'       same `_vX.X_rN` convention as AmeriFlux?
#'   }
#'   Until then, `reprocessed_icos_tern` is returned as `NULL` and ICOS/TERN
#'   reprocessing will not trigger a re-download.
#'
#' @seealso [load_snapshot()], [sites_to_download()]
#' @export
compare_snapshots <- function(current, previous) {
  # --- new sites (in current, not in previous) ---
  new_sites <- dplyr::filter(current, !.data$site_id %in% previous$site_id)

  # --- sites present in both snapshots ---
  both <- dplyr::inner_join(
    current,
    dplyr::select(
      previous,
      site_id,
      prev_first_year   = first_year,
      prev_last_year    = last_year,
      prev_product_name = fluxnet_product_name
    ),
    by = "site_id"
  )

  # --- extended data coverage ---
  extended_data <- dplyr::filter(
    both,
    .data$first_year < .data$prev_first_year |
      .data$last_year > .data$prev_last_year
  )

  # --- reprocessed AmeriFlux: product name changed, not already in extended ---
  # Sites with extended coverage already trigger a re-download, so excluding
  # them here avoids redundant entries in the log and download list.
  reprocessed_ameriflux <- dplyr::filter(
    both,
    .data$data_hub == "AmeriFlux",
    .data$fluxnet_product_name != .data$prev_product_name,
    !.data$site_id %in% extended_data$site_id
  )

  # --- reprocessed ICOS/TERN: not yet implemented ---
  # Requires answers to open questions Q1 and Q3 (support@fluxnet.org).
  reprocessed_icos_tern <- NULL  # pending Q1 / Q3

  list(
    new_sites             = new_sites,
    extended_data         = extended_data,
    reprocessed_ameriflux = reprocessed_ameriflux,
    reprocessed_icos_tern = reprocessed_icos_tern
  )
}


#' Return the union of site IDs that should be (re-)downloaded
#'
#' Collects `site_id` values from all actionable change categories returned by
#' [compare_snapshots()] and returns the deduplicated union. ICOS/TERN
#' reprocessing is excluded until detection logic is confirmed with Gilberto's
#' team.
#'
#' @param comparison A named list returned by [compare_snapshots()].
#' @return A character vector of unique `site_id` values to download, in no
#'   particular order. May be length zero if nothing has changed.
#'
#' @note **ICOS/TERN reprocessing not included in the download set.**
#'   `reprocessed_icos_tern` is `NULL` and is intentionally excluded until
#'   open questions Q1 and Q3 are resolved (support@fluxnet.org). Once
#'   resolved, add `comparison$reprocessed_icos_tern$site_id` to the `c()`
#'   call below.
#'
#' @seealso [compare_snapshots()]
#' @export
sites_to_download <- function(comparison) {
  unique(c(
    comparison$new_sites$site_id,
    comparison$extended_data$site_id,
    comparison$reprocessed_ameriflux$site_id
    # reprocessed_icos_tern excluded — pending Q1 / Q3 (support@fluxnet.org)
  ))
}
