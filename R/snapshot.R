#' Resolve the site manifest according to the current snapshot mode
#'
#' In `"development"` mode, returns the manifest passed in directly (typically
#' the live result of `flux_listall()`). In `"locked"` mode, reads the
#' snapshot CSV at `FLUXNET_SNAPSHOT_FILE` and returns that instead, ignoring
#' the `live_manifest` argument.
#'
#' This ensures that in locked mode the pipeline always operates on exactly the
#' same set of sites and versions that were recorded at lock time, regardless
#' of what the Shuttle currently reports.
#'
#' @param live_manifest A data frame returned by `flux_listall()`. Ignored in
#'   locked mode but required in development mode.
#' @return A data frame: either `live_manifest` (development) or the snapshot
#'   CSV (locked).
#' @export
resolve_snapshot <- function(live_manifest = NULL) {
  snapshot_mode <- Sys.getenv("FLUXNET_SNAPSHOT_MODE", unset = "development")

  if (snapshot_mode == "locked") {
    snapshot_file <- Sys.getenv("FLUXNET_SNAPSHOT_FILE", unset = NA_character_)
    if (is.na(snapshot_file) || nchar(trimws(snapshot_file)) == 0) {
      stop(
        "FLUXNET_SNAPSHOT_MODE is 'locked' but FLUXNET_SNAPSHOT_FILE is not set."
      )
    }
    if (!file.exists(snapshot_file)) {
      stop("Snapshot file not found: ", snapshot_file)
    }
    message("Locked mode: loading snapshot from ", snapshot_file)
    return(readr::read_csv(snapshot_file, show_col_types = FALSE))
  }

  # development mode
  if (is.null(live_manifest)) {
    stop("live_manifest must be provided when FLUXNET_SNAPSHOT_MODE is 'development'.")
  }
  live_manifest
}

#' Write the current manifest to a timestamped snapshot CSV
#'
#' Saves `manifest` to `data/snapshots/` with a filename of the form
#' `fluxnet_shuttle_snapshot_<YYYYMMDDTHHMMSS>.csv`. Used in step 1 of the
#' dataset locking workflow.
#'
#' @param manifest A data frame to save (typically the output of
#'   `flux_listall()`).
#' @param snapshot_dir Path to the snapshots directory. Default:
#'   `"data/snapshots"`.
#' @return The path to the written file (invisibly).
#' @export
write_snapshot <- function(manifest, snapshot_dir = "data/snapshots") {
  fs::dir_create(snapshot_dir)
  timestamp <- format(Sys.time(), "%Y%m%dT%H%M%S")
  filename  <- paste0("fluxnet_shuttle_snapshot_", timestamp, ".csv")
  path      <- fs::path(snapshot_dir, filename)
  readr::write_csv(manifest, path)
  message("Snapshot written to: ", path)
  invisible(path)
}
