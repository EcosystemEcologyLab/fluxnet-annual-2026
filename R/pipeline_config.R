# Site filter — when set, pipeline restricts downloads (and downstream steps)
# to only the listed site IDs. Space-separated. Unset = all sites.
FLUXNET_SITE_FILTER <- {
  raw <- Sys.getenv("FLUXNET_SITE_FILTER", unset = "")
  if (nchar(trimws(raw)) == 0) character(0) else strsplit(trimws(raw), "\\s+")[[1]]
}

# Data root directory — set FLUXNET_DATA_ROOT to relocate all pipeline data
# directories (raw, extracted, processed, snapshots). Useful for HPC scratch
# filesystems or machines where the repo checkout is read-only.
# Default: "data" (relative to the project root).
FLUXNET_DATA_ROOT <- Sys.getenv("FLUXNET_DATA_ROOT", unset = "data")

# Temporal resolutions to extract — space-separated flux_extract() codes.
# Valid values: y (yearly), m (monthly), w (weekly), d (daily),
#               h (hourly / half-hourly)
# Default: "y m d" — YY, MM, DD only. The Annual Paper does not use HH/HR
# sub-daily data, and excluding h reduces extracted volume by ~95%.
FLUXNET_EXTRACT_RESOLUTIONS <- strsplit(
  Sys.getenv("FLUXNET_EXTRACT_RESOLUTIONS", unset = "y m d"),
  "\\s+"
)[[1]]

# ZIP cleanup — when TRUE, each ZIP in data/raw/ is deleted immediately after
# at least one file has been successfully extracted from it. Reduces peak disk
# usage during batch downloads. Set to FALSE to retain raw ZIPs.
FLUXNET_DELETE_ZIPS <- isTRUE(as.logical(
  Sys.getenv("FLUXNET_DELETE_ZIPS", unset = "TRUE")
))

# QC threshold constants — change here to adjust pipeline-wide filtering.
# DD/WW/MM/YY thresholds: keep records where _QC > threshold.
# Lowered from 0.75 to 0.50 to match FLUXNET published convention — to be revisited with co-authors

#' @export
QC_THRESHOLD_DD <- 0.50  # daily

#' @export
QC_THRESHOLD_WW <- 0.50  # weekly

#' @export
QC_THRESHOLD_MM <- 0.50  # monthly

#' @export
QC_THRESHOLD_YY <- 0.50  # annual

#' Check pipeline configuration
#'
#' Validates that all required environment variables are set, that the
#' installed fluxnet-shuttle version matches `FLUXNET_SHUTTLE_VERSION`, and
#' that `FLUXNET_SNAPSHOT_MODE` is a recognised value. Must be called at the
#' top of every pipeline script.
#'
#' @return Invisibly returns `TRUE` if all checks pass. Issues warnings for
#'   non-fatal mismatches (e.g. shuttle version mismatch). Stops with an error
#'   for missing required credentials.
#' @export
check_pipeline_config <- function() {
  ok <- TRUE

  # --- Required credentials ---
  user_name  <- Sys.getenv("AMERIFLUX_USER_NAME",  unset = NA_character_)
  user_email <- Sys.getenv("AMERIFLUX_USER_EMAIL", unset = NA_character_)

  if (is.na(user_name) || nchar(trimws(user_name)) == 0) {
    stop("AMERIFLUX_USER_NAME is not set. Set it as a Codespace Secret or in .env.")
  }
  if (is.na(user_email) || nchar(trimws(user_email)) == 0) {
    stop("AMERIFLUX_USER_EMAIL is not set. Set it as a Codespace Secret or in .env.")
  }

  # --- Intended use code ---
  intended_use <- Sys.getenv("AMERIFLUX_INTENDED_USE", unset = NA_character_)
  if (is.na(intended_use) || nchar(trimws(intended_use)) == 0) {
    message(
      "AMERIFLUX_INTENDED_USE is not set. ",
      "Using default: 1 = network synthesis analysis. ",
      "Change this in .env or Codespace Secrets if appropriate."
    )
  }

  # --- Shuttle version check ---
  expected_version <- Sys.getenv("FLUXNET_SHUTTLE_VERSION", unset = "0.3.7.post1")
  # Use the CLI executable rather than importing the Python module: the
  # fluxnet_shuttle module does not expose a __version__ attribute, so the
  # import approach always returns NA.  fluxnet:::fluxnet_shuttle_executable()
  # returns the path to the binary installed in the 'fluxnet' virtualenv.
  installed_version <- tryCatch(
    {
      exe <- fluxnet:::fluxnet_shuttle_executable()
      if (!is.null(exe) && nzchar(exe) && file.exists(exe)) {
        raw <- system2(exe, "--version", stdout = TRUE, stderr = FALSE)
        # Output: "fluxnet-shuttle 0.3.7.post1" — extract the version token
        trimws(sub("^.*\\s+", "", raw[1]))
      } else {
        NA_character_
      }
    },
    error = function(e) NA_character_
  )

  if (is.na(installed_version)) {
    warning(
      "fluxnet-shuttle does not appear to be installed. ",
      "It will be installed automatically on the first call to flux_listall()."
    )
    ok <- FALSE
  } else if (!identical(installed_version, expected_version)) {
    warning(
      "Installed fluxnet-shuttle version (", installed_version, ") ",
      "does not match FLUXNET_SHUTTLE_VERSION (", expected_version, "). ",
      "Results may differ from the expected pipeline run."
    )
    ok <- FALSE
  } else {
    message("fluxnet-shuttle version OK: ", installed_version)
  }

  # --- Data root ---
  message("Data root: ", FLUXNET_DATA_ROOT)

  # --- Extract resolutions ---
  valid_resolutions <- c("y", "m", "w", "d", "h")
  bad_res <- setdiff(FLUXNET_EXTRACT_RESOLUTIONS, valid_resolutions)
  if (length(bad_res) > 0) {
    stop(
      "FLUXNET_EXTRACT_RESOLUTIONS contains invalid value(s): ",
      paste(bad_res, collapse = ", "),
      ". Valid values: ", paste(valid_resolutions, collapse = ", "), "."
    )
  }
  message("Extract resolutions: ", paste(FLUXNET_EXTRACT_RESOLUTIONS, collapse = " "))

  # --- ZIP cleanup ---
  message("Delete ZIPs after extraction: ", FLUXNET_DELETE_ZIPS)

  # --- Site filter ---
  if (length(FLUXNET_SITE_FILTER) > 0) {
    message(
      "Site filter active: ", length(FLUXNET_SITE_FILTER), " site(s) — ",
      paste(FLUXNET_SITE_FILTER, collapse = " ")
    )
  } else {
    message("Site filter: none (all sites)")
  }

  # --- Snapshot mode ---
  snapshot_mode <- Sys.getenv("FLUXNET_SNAPSHOT_MODE", unset = "development")
  valid_modes <- c("development", "locked")
  if (!snapshot_mode %in% valid_modes) {
    stop(
      "FLUXNET_SNAPSHOT_MODE must be one of: ",
      paste(valid_modes, collapse = ", "),
      ". Got: '", snapshot_mode, "'."
    )
  }
  message("Snapshot mode: ", snapshot_mode)

  if (snapshot_mode == "locked") {
    snapshot_file <- Sys.getenv("FLUXNET_SNAPSHOT_FILE", unset = NA_character_)
    if (is.na(snapshot_file) || nchar(trimws(snapshot_file)) == 0) {
      stop(
        "FLUXNET_SNAPSHOT_MODE is 'locked' but FLUXNET_SNAPSHOT_FILE is not set."
      )
    }
    if (!file.exists(snapshot_file)) {
      stop("FLUXNET_SNAPSHOT_FILE does not exist: ", snapshot_file)
    }
    message("Snapshot file: ", snapshot_file)
  }

  invisible(TRUE)
}
