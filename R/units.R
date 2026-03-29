#' Convert FLUXNET variables from native units to analysis units
#'
#' Applies period-aware unit conversions to a data frame of FLUXNET data.
#' Accepts the `temporal_resolution` column (CLAUDE.md standard) or the
#' `time_resolution` column returned by the fluxnet package inventory.
#' Supports HH, HR, DD, WW, MM, and YY resolutions.
#'
#' When `manifest` contains multiple resolutions (e.g., a full file inventory
#' with DD/HH/MM/WW/YY rows), the function uses the `flux_resolution`
#' attribute set by `flux_read()` to determine which resolution to apply.
#' Pass a pre-filtered manifest if the attribute is not set.
#'
#' Conversion rules:
#' | Variable       | Native unit          | Analysis unit            |
#' |----------------|----------------------|--------------------------|
#' | NEE, GPP, RECO | µmol CO₂ m⁻² s⁻¹    | gC m⁻² per period        |
#' | LE             | W m⁻²                | mm H₂O per period        |
#' | H, SW_IN       | W m⁻²                | MJ m⁻² per period        |
#' | TA             | °C                   | K                        |
#' | P              | mm per timestep      | mm per period (sum step) |
#' | VPD            | hPa                  | kPa                      |
#'
#' @param data A data frame containing FLUXNET variables to convert. If read
#'   by [fluxnet::flux_read()], the `flux_resolution` attribute will be set
#'   and used automatically.
#' @param manifest A data frame with at least a `temporal_resolution` or
#'   `time_resolution` column for the site(s) represented in `data`.
#' @return `data` with converted columns in analysis units. Original values
#'   are retained with a `_native` suffix.
#' @export
fluxnet_convert_units <- function(data, manifest) {
  # Accept either column name; the fluxnet package inventory uses time_resolution,
  # CLAUDE.md standard is temporal_resolution.
  if ("temporal_resolution" %in% names(manifest)) {
    res_col <- "temporal_resolution"
  } else if ("time_resolution" %in% names(manifest)) {
    res_col <- "time_resolution"
  } else {
    stop(
      "manifest must contain a 'temporal_resolution' or 'time_resolution' column."
    )
  }

  resolutions <- unique(manifest[[res_col]])
  resolutions  <- resolutions[!is.na(resolutions)]

  if (length(resolutions) > 1) {
    # Use the flux_resolution attribute set by flux_read() to disambiguate
    data_res <- attr(data, "flux_resolution")
    if (!is.null(data_res) && data_res %in% resolutions) {
      message(
        "Multiple resolutions in manifest; using data resolution from flux_read: ",
        data_res
      )
      manifest     <- manifest[manifest[[res_col]] == data_res & !is.na(manifest[[res_col]]), ]
      resolutions  <- data_res
    } else {
      stop(
        "Cannot convert units for data with mixed temporal resolutions: ",
        paste(resolutions, collapse = ", "),
        ". Filter the manifest to a single resolution before converting units."
      )
    }
  }

  resolution <- resolutions[[1]]

  valid_resolutions <- c("HH", "HR", "DD", "WW", "MM", "YY")
  if (!resolution %in% valid_resolutions) {
    stop(
      "Unrecognised temporal_resolution: '", resolution, "'. ",
      "Expected one of: ", paste(valid_resolutions, collapse = ", "), "."
    )
  }

  # Compute seconds per period (vectorised for YY leap-year awareness)
  seconds_per_period <- switch(
    resolution,
    "HH" = 1800L,
    "HR" = 3600L,
    "DD" = 86400L,
    "WW" = 604800L,
    "MM" = {
      # 30.4375-day mean month approximation; per-month accuracy not needed for
      # annual synthesis. Pending: per-row implementation using lubridate.
      message("MM resolution: using 30.4375-day month approximation for unit conversion.")
      round(30.4375 * 86400)
    },
    "YY" = {
      # Per-year, leap-year-aware seconds count.
      year_col <- intersect(c("YEAR", "TIMESTAMP"), names(data))
      if (length(year_col) > 0) {
        yr <- suppressWarnings(as.integer(data[[year_col[[1]]]]))
        ifelse(lubridate::leap_year(yr), 366L, 365L) * 86400L
      } else {
        message("YY resolution: YEAR column not found; using 365.25-day approximation.")
        rep(round(365.25 * 86400), nrow(data))
      }
    }
  )

  # Helper: match candidate column names, excluding _QC flag columns
  flux_cols <- function(candidates) {
    intersect(names(data), grep("_QC$", candidates, value = TRUE, invert = TRUE))
  }

  # Carbon flux: µmol CO₂ m⁻² s⁻¹  →  gC m⁻² per period
  # Molar mass of C = 12 g/mol (NOT CO₂ = 44 g/mol)
  carbon_cols <- flux_cols(c("NEE", "GPP", "RECO",
                              grep("^NEE_|^GPP_|^RECO_", names(data), value = TRUE)))
  for (col in carbon_cols) {
    data[[paste0(col, "_native")]] <- data[[col]]
    data[[col]] <- data[[col]] * 12e-6 * seconds_per_period
  }

  # LE: W m⁻²  →  mm H₂O per period  (lambda ≈ 2.45e6 J/kg, rho_w = 1000 kg/m³)
  le_cols <- flux_cols(c("LE", grep("^LE_", names(data), value = TRUE)))
  for (col in le_cols) {
    data[[paste0(col, "_native")]] <- data[[col]]
    data[[col]] <- data[[col]] * seconds_per_period / 2.45e6
  }

  # H and SW_IN: W m⁻²  →  MJ m⁻² per period
  energy_cols <- flux_cols(c("H", "SW_IN",
                              grep("^H_|^SW_IN_", names(data), value = TRUE)))
  for (col in energy_cols) {
    data[[paste0(col, "_native")]] <- data[[col]]
    data[[col]] <- data[[col]] * seconds_per_period * 1e-6
  }

  # TA: °C  →  K
  ta_cols <- flux_cols(c("TA", grep("^TA_", names(data), value = TRUE)))
  for (col in ta_cols) {
    data[[paste0(col, "_native")]] <- data[[col]]
    data[[col]] <- data[[col]] + 273.15
  }

  # VPD: hPa  →  kPa
  vpd_cols <- flux_cols(c("VPD", grep("^VPD_", names(data), value = TRUE)))
  for (col in vpd_cols) {
    data[[paste0(col, "_native")]] <- data[[col]]
    data[[col]] <- data[[col]] / 10
  }

  # P: mm per timestep  →  kept as mm per period (sum handled at aggregation)
  # No numeric scaling needed; P must be summed not averaged when aggregating.

  data
}
