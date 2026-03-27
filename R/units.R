#' Convert FLUXNET variables from native units to analysis units
#'
#' Applies timestep-aware unit conversions to a data frame of FLUXNET data.
#' The `temporal_resolution` field must be present in `manifest` (values:
#' `"HH"` for half-hourly, `"HR"` for hourly). Stops if resolution is missing
#' or unrecognised.
#'
#' Conversion rules:
#' | Variable       | Native unit          | Analysis unit         |
#' |----------------|----------------------|-----------------------|
#' | NEE, GPP, RECO | µmol CO₂ m⁻² s⁻¹    | gC m⁻² per period    |
#' | LE             | W m⁻²                | mm H₂O per period    |
#' | H, SW_IN       | W m⁻²                | MJ m⁻² per period    |
#' | TA             | °C                   | K                    |
#' | P              | mm per timestep      | mm per period (sum)  |
#' | VPD            | hPa                  | kPa                  |
#'
#' @param data A data frame containing FLUXNET variables to convert.
#' @param manifest A data frame with at least a `temporal_resolution` column
#'   (`"HH"` or `"HR"`) for the site(s) represented in `data`.
#' @return `data` with converted columns added (original columns are retained
#'   with a `_native` suffix).
#' @export
fluxnet_convert_units <- function(data, manifest) {
  if (!"temporal_resolution" %in% names(manifest)) {
    stop("manifest must contain a 'temporal_resolution' column.")
  }

  resolutions <- unique(manifest$temporal_resolution)
  if (length(resolutions) > 1) {
    stop(
      "Cannot convert units for data with mixed temporal resolutions: ",
      paste(resolutions, collapse = ", "),
      ". Aggregate to a common resolution before converting units."
    )
  }

  resolution <- resolutions[[1]]
  if (!resolution %in% c("HH", "HR")) {
    stop(
      "Unrecognised temporal_resolution: '", resolution, "'. ",
      "Expected 'HH' (half-hourly) or 'HR' (hourly)."
    )
  }

  seconds_per_timestep <- if (resolution == "HH") 1800L else 3600L

  # Carbon flux: µmol CO₂ m⁻² s⁻¹  →  gC m⁻² per timestep
  # 12 g/mol C (molar mass of C, NOT CO₂)
  carbon_cols <- intersect(names(data), c("NEE", "GPP", "RECO",
                                           grep("^NEE_|^GPP_|^RECO_",
                                                names(data), value = TRUE)))
  for (col in carbon_cols) {
    data[[paste0(col, "_native")]] <- data[[col]]
    data[[col]] <- data[[col]] * 12e-6 * seconds_per_timestep  # gC m⁻² per timestep
  }

  # LE: W m⁻²  →  mm H₂O per timestep  (lambda ≈ 2.45e6 J/kg, rho_w = 1000 kg/m³)
  le_cols <- intersect(names(data), c("LE", grep("^LE_", names(data), value = TRUE)))
  for (col in le_cols) {
    data[[paste0(col, "_native")]] <- data[[col]]
    data[[col]] <- data[[col]] * seconds_per_timestep / 2.45e6  # mm per timestep
  }

  # H and SW_IN: W m⁻²  →  MJ m⁻² per timestep
  energy_cols <- intersect(names(data), c("H", "SW_IN",
                                           grep("^H_|^SW_IN_", names(data), value = TRUE)))
  for (col in energy_cols) {
    data[[paste0(col, "_native")]] <- data[[col]]
    data[[col]] <- data[[col]] * seconds_per_timestep * 1e-6  # MJ m⁻² per timestep
  }

  # TA: °C  →  K
  ta_cols <- intersect(names(data), c("TA", grep("^TA_", names(data), value = TRUE)))
  for (col in ta_cols) {
    data[[paste0(col, "_native")]] <- data[[col]]
    data[[col]] <- data[[col]] + 273.15
  }

  # VPD: hPa  →  kPa
  vpd_cols <- intersect(names(data), c("VPD", grep("^VPD_", names(data), value = TRUE)))
  for (col in vpd_cols) {
    data[[paste0(col, "_native")]] <- data[[col]]
    data[[col]] <- data[[col]] / 10
  }

  # P: mm per timestep  →  kept as mm (sum is handled at aggregation step)
  # No numeric transformation needed; document that P must be summed, not averaged.

  data
}
