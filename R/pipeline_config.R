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

# QC threshold constants — change here to adjust pipeline-wide filtering.
# DD/WW/MM/YY thresholds: keep records where _QC > threshold.
# The default of 0.75 is stricter than the FLUXNET2015 published convention
# (0.50). To reproduce FLUXNET2015 figures, set all thresholds to 0.5.

#' @export
QC_THRESHOLD_DD <- 0.75  # daily

#' @export
QC_THRESHOLD_WW <- 0.75  # weekly

#' @export
QC_THRESHOLD_MM <- 0.75  # monthly

#' @export
QC_THRESHOLD_YY <- 0.75  # annual

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
  expected_version <- Sys.getenv("FLUXNET_SHUTTLE_VERSION", unset = "0.2.0")
  installed_version <- tryCatch(
    {
      env <- reticulate::import("fluxnet_shuttle", convert = FALSE)
      reticulate::py_to_r(env$`__version__`)
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
