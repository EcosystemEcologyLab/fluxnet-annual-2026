# R/sync.R — Snapshot comparison and update detection for the FLUXNET pipeline.
#
# Detects new sites, extended data coverage, and reprocessed products between
# consecutive Shuttle snapshot CSVs by comparing fluxnet_product_name,
# first_year/last_year, and product_id fields.
#
# Reprocessing detection confirmed for all hubs by Dario Papale (2026-03-30):
#   - product_id is a checksum-based PID that changes whenever a file is
#     updated — reliable reprocessing signal for ALL hubs.
#   - All hubs use the same _vX.X_rN convention in fluxnet_product_name,
#     making product_name comparison valid across hubs.
#   - Release increments (e.g. _r2) are not AmeriFlux-specific; the 5 _r2
#     sites in the 2026-03-28 snapshots are ICOS sites.


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
#' three change categories. Each category is a data frame of site rows taken
#' from the *current* snapshot.
#'
#' @section Change categories:
#' \describe{
#'   \item{`new_sites`}{Sites present in `current` but absent from `previous`
#'     (matched on `site_id`).}
#'   \item{`extended_data`}{Sites present in both snapshots where `first_year`
#'     decreased or `last_year` increased — i.e., the temporal coverage
#'     expanded.}
#'   \item{`reprocessed`}{Sites present in both snapshots where `product_id`
#'     changed or `fluxnet_product_name` changed (catches release increments
#'     such as _r1 → _r2 and ONEFlux version bumps). Applies to all hubs:
#'     `product_id` is a checksum-based PID confirmed to change on file update
#'     for all hubs; `fluxnet_product_name` follows the same _vX.X_rN
#'     convention across all hubs (confirmed Dario Papale 2026-03-30). Sites
#'     already in `extended_data` are excluded to avoid double-counting.}
#' }
#'
#' @param current A data frame from [load_snapshot()] — the newer snapshot.
#' @param previous A data frame from [load_snapshot()] — the older snapshot.
#' @return A named list with elements `new_sites`, `extended_data`, and
#'   `reprocessed`.
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
      prev_product_name = fluxnet_product_name,
      prev_product_id   = product_id
    ),
    by = "site_id"
  )

  # --- extended data coverage ---
  extended_data <- dplyr::filter(
    both,
    .data$first_year < .data$prev_first_year |
      .data$last_year > .data$prev_last_year
  )

  # --- reprocessed: product_id or product_name changed, not already in extended ---
  # product_id is a checksum-based PID — changes on file update for all hubs.
  # product_name change catches release increments and ONEFlux version bumps.
  # Sites with extended coverage already trigger a re-download, so excluding
  # them here avoids redundant entries in the log and download list.
  reprocessed <- dplyr::filter(
    both,
    .data$product_id != .data$prev_product_id |
      .data$fluxnet_product_name != .data$prev_product_name,
    !.data$site_id %in% extended_data$site_id
  )

  list(
    new_sites     = new_sites,
    extended_data = extended_data,
    reprocessed   = reprocessed
  )
}


#' Return the union of site IDs that should be (re-)downloaded
#'
#' Collects `site_id` values from all actionable change categories returned by
#' [compare_snapshots()] and returns the deduplicated union.
#'
#' @param comparison A named list returned by [compare_snapshots()].
#' @return A character vector of unique `site_id` values to download, in no
#'   particular order. May be length zero if nothing has changed.
#'
#' @seealso [compare_snapshots()]
#' @export
sites_to_download <- function(comparison) {
  unique(c(
    comparison$new_sites$site_id,
    comparison$extended_data$site_id,
    comparison$reprocessed$site_id
  ))
}
