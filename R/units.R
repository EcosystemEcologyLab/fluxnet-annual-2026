# Module-level cache so BIFVARINFO files are read at most once per session.
.bifvarinfo_cache <- new.env(parent = emptyenv())

#' Read variable units from a FLUXNET BIFVARINFO_YY metadata file
#'
#' Scans `extracted_dir` recursively for a file matching `BIFVARINFO_YY*.csv`,
#' reads the `GRP_VAR_INFO` group, and returns a named character vector mapping
#' each variable name to its unit string (e.g. `c(NEE_VUT_REF = "gC m-2 y-1",
#' LE_F_MDS = "W m-2", ...)`).
#'
#' Results are cached in a module-level environment so the file is read at most
#' once per R session. If no BIFVARINFO file is found, falls back to a hardcoded
#' lookup table with a warning.
#'
#' @param extracted_dir Path to the directory containing extracted FLUXNET files
#'   (default: `$FLUXNET_DATA_ROOT/extracted`). Files are searched recursively.
#'
#' @return A named character vector: variable name → unit string. The vector may
#'   come from a BIFVARINFO file or from the hardcoded fallback; callers should
#'   not assume completeness — use [.infer_source_unit()] to query it.
#'
#' @export
read_bifvarinfo_units <- function(extracted_dir = NULL) {
  if (exists("units", envir = .bifvarinfo_cache)) {
    return(.bifvarinfo_cache$units)
  }

  if (is.null(extracted_dir)) {
    extracted_dir <- file.path(
      Sys.getenv("FLUXNET_DATA_ROOT", unset = "data"),
      "extracted"
    )
  }

  bifvar_files <- list.files(
    extracted_dir,
    pattern   = "BIFVARINFO_YY.*\\.csv$",
    full.names = TRUE,
    recursive  = TRUE
  )

  if (length(bifvar_files) == 0) {
    warning(
      "No BIFVARINFO_YY file found in '", extracted_dir, "'. ",
      "Using hardcoded unit lookup table. ",
      "Run flux_extract() to populate data/extracted/ with BIFVARINFO files."
    )
    result <- .bifvarinfo_hardcoded_lookup()
    assign("units", result, envir = .bifvarinfo_cache)
    return(result)
  }

  bifvar_path <- bifvar_files[[1]]
  message("read_bifvarinfo_units(): reading '", basename(bifvar_path), "'")

  bifvar_raw <- utils::read.csv(
    bifvar_path,
    stringsAsFactors = FALSE,
    check.names      = FALSE
  )

  required_cols <- c("GROUP_ID", "VAR_INFO_VARNAME", "VAR_INFO_UNIT")
  missing_cols  <- setdiff(required_cols, names(bifvar_raw))
  if (length(missing_cols) > 0) {
    warning(
      "BIFVARINFO file '", basename(bifvar_path), "' is missing columns: ",
      paste(missing_cols, collapse = ", "), ". Using hardcoded fallback."
    )
    result <- .bifvarinfo_hardcoded_lookup()
    assign("units", result, envir = .bifvarinfo_cache)
    return(result)
  }

  # Keep only GRP_VAR_INFO rows with a non-empty variable name
  var_info <- bifvar_raw[
    !is.na(bifvar_raw$GROUP_ID) &
      bifvar_raw$GROUP_ID == "GRP_VAR_INFO" &
      !is.na(bifvar_raw$VAR_INFO_VARNAME) &
      nchar(trimws(bifvar_raw$VAR_INFO_VARNAME)) > 0,
  ]

  if (nrow(var_info) == 0) {
    warning(
      "BIFVARINFO file '", basename(bifvar_path), "' has no GRP_VAR_INFO rows. ",
      "Using hardcoded fallback."
    )
    result <- .bifvarinfo_hardcoded_lookup()
    assign("units", result, envir = .bifvarinfo_cache)
    return(result)
  }

  result <- stats::setNames(
    trimws(var_info$VAR_INFO_UNIT),
    trimws(var_info$VAR_INFO_VARNAME)
  )
  message("  ", length(result), " variable units read from BIFVARINFO.")

  assign("units", result, envir = .bifvarinfo_cache)
  result
}

#' Hardcoded unit fallback for known FLUXNET YY-resolution variables
#'
#' Used by [read_bifvarinfo_units()] when no BIFVARINFO file is available.
#' Units reflect the ONEFlux output convention: carbon fluxes at DD/MM/WW/YY
#' are pre-integrated totals (gC m-2 period-1), NOT instantaneous rates.
#' Energy fluxes are mean rates (W m-2) at all resolutions.
#'
#' @return Named character vector: variable name → unit string.
#' @keywords internal
.bifvarinfo_hardcoded_lookup <- function() {
  c(
    # ── Carbon fluxes ── pre-integrated at YY resolution (gC m-2 y-1)
    # The same convention applies at MM (gC m-2 m-1) and DD (gC m-2 d-1).
    # All of these pass through without conversion at coarse resolutions.
    NEE_VUT_REF          = "gC m-2 y-1",
    NEE_VUT_REF_NIGHT    = "gC m-2 y-1",
    NEE_VUT_REF_DAY      = "gC m-2 y-1",
    NEE_VUT_USTAR50      = "gC m-2 y-1",
    NEE_VUT_MEAN         = "gC m-2 y-1",
    NEE_CUT_REF          = "gC m-2 y-1",
    NEE_CUT_USTAR50      = "gC m-2 y-1",
    GPP_NT_VUT_REF       = "gC m-2 y-1",
    GPP_NT_VUT_USTAR50   = "gC m-2 y-1",
    GPP_NT_VUT_MEAN      = "gC m-2 y-1",
    GPP_DT_VUT_REF       = "gC m-2 y-1",
    GPP_DT_VUT_USTAR50   = "gC m-2 y-1",
    GPP_DT_VUT_MEAN      = "gC m-2 y-1",
    GPP_NT_CUT_REF       = "gC m-2 y-1",
    GPP_DT_CUT_REF       = "gC m-2 y-1",
    RECO_NT_VUT_REF      = "gC m-2 y-1",
    RECO_NT_VUT_USTAR50  = "gC m-2 y-1",
    RECO_NT_VUT_MEAN     = "gC m-2 y-1",
    RECO_DT_VUT_REF      = "gC m-2 y-1",
    RECO_DT_VUT_USTAR50  = "gC m-2 y-1",
    RECO_DT_VUT_MEAN     = "gC m-2 y-1",
    # ── Energy fluxes ── mean rates at all resolutions (W m-2)
    LE_F_MDS             = "W m-2",
    LE_CORR              = "W m-2",
    LE_RANDUNC           = "W m-2",
    H_F_MDS              = "W m-2",
    H_CORR               = "W m-2",
    H_RANDUNC            = "W m-2",
    SW_IN_F              = "W m-2",
    SW_IN_POT            = "W m-2",
    SW_OUT               = "W m-2",
    LW_IN_F              = "W m-2",
    LW_OUT               = "W m-2",
    NETRAD               = "W m-2",
    G_F_MDS              = "W m-2",
    # ── Meteorological ──
    TA_F                 = "degC",
    TA_F_MDS             = "degC",
    TA_ERA               = "degC",
    VPD_F                = "hPa",
    VPD_F_MDS            = "hPa",
    VPD_ERA              = "hPa",
    PA_F                 = "kPa",
    P_F                  = "mm",
    WS_F                 = "m s-1",
    WS_ERA               = "m s-1",
    CO2_F_MDS            = "umol mol-1",
    USTAR                = "m s-1",
    # ── QC flags ── adimensional at all resolutions
    NEE_VUT_REF_QC       = "adimensional",
    GPP_NT_VUT_REF_QC    = "adimensional",
    LE_F_MDS_QC          = "adimensional",
    H_F_MDS_QC           = "adimensional",
    TA_F_MDS_QC          = "adimensional",
    SW_IN_F_QC           = "adimensional",
    P_F_QC               = "adimensional"
  )
}

#' Infer source unit for one column
#'
#' For carbon flux variables (NEE, GPP, RECO) the resolution controls unit
#' selection entirely: HH/HR data is in µmol CO₂ m⁻² s⁻¹; coarse-resolution
#' data (DD/MM/WW/YY) is pre-integrated by ONEFlux and delivered in
#' gC m⁻² period⁻¹. The BIFVARINFO_YY lookup is consulted for YY carbon
#' variables as a validation check (expected: "gC m-2 y-1"). For all other
#' variable types, the BIFVARINFO named vector is queried first, then
#' prefix-based pattern matching.
#'
#' @param col_name Column name.
#' @param var_units Named character vector from [read_bifvarinfo_units()].
#' @param resolution Temporal resolution string ("HH", "HR", "DD", "MM", "YY", ...).
#'
#' @return A unit string, or `NA_character_` if the unit cannot be determined.
#' @keywords internal
.infer_source_unit <- function(col_name, var_units, resolution) {
  res <- toupper(resolution)

  # QC flags are adimensional at all resolutions.
  if (grepl("_QC$", col_name)) return("adimensional")

  # Carbon variables — resolution determines whether the value is a rate or a
  # pre-integrated total. BIFVARINFO_YY units are only valid for YY-resolution
  # data; at DD/MM/WW or HH/HR the unit must be inferred from the resolution.
  if (grepl("^(NEE|GPP|RECO)_?", col_name)) {
    if (res %in% c("HH", "HR")) {
      return("umol m-2 s-1")
    }
    # For YY: consult BIFVARINFO as a validation check.
    # For DD/MM/WW: always use the resolution-derived unit (BIFVARINFO_YY does
    # not reflect sub-annual integration periods).
    if (res == "YY" && col_name %in% names(var_units)) {
      return(var_units[[col_name]])
    }
    return(switch(res,
      "YY" = "gC m-2 y-1",
      "MM" = "gC m-2 m-1",
      "WW" = "gC m-2 w-1",
      "DD" = "gC m-2 d-1",
      "gC m-2 period-1"   # unknown coarse resolution
    ))
  }

  # Non-carbon variables: BIFVARINFO lookup first, then prefix patterns.
  if (col_name %in% names(var_units)) return(var_units[[col_name]])

  if (grepl("^(LE|H|SW_IN|SW_OUT|LW_IN|LW_OUT|NETRAD|G)_", col_name)) return("W m-2")
  if (grepl("^TA_",  col_name)) return("degC")
  if (grepl("^VPD_", col_name)) return("hPa")
  if (grepl("^P_",   col_name)) return("mm")

  NA_character_
}

#' Convert FLUXNET variables from native units to analysis units
#'
#' Reads native variable units from a BIFVARINFO_YY metadata file (via
#' [read_bifvarinfo_units()]) and applies period-aware unit conversions only
#' where the source unit differs from the target. Variables already in the
#' target unit are passed through unchanged.
#'
#' **Critical:** At DD/MM/WW/YY resolutions the FLUXNET Shuttle (ONEFlux
#' pipeline) delivers carbon fluxes (NEE, GPP, RECO) as pre-integrated totals
#' in gC m⁻² period⁻¹. These must NOT be multiplied by the µmol→gC conversion
#' factor. The conversion is only needed for HH/HR data, where values are still
#' in µmol CO₂ m⁻² s⁻¹. This function checks the source unit before applying
#' any carbon conversion.
#'
#' Accepts the `temporal_resolution` column (CLAUDE.md standard) or the
#' `time_resolution` column returned by the fluxnet package inventory.
#'
#' Conversion rules applied when source unit requires it:
#' | Variable       | Source unit          | Analysis unit            | Condition   |
#' |----------------|----------------------|--------------------------|-------------|
#' | NEE, GPP, RECO | µmol CO₂ m⁻² s⁻¹    | gC m⁻² per period        | HH/HR only  |
#' | NEE, GPP, RECO | gC m⁻² period⁻¹     | (passed through)         | DD/MM/WW/YY |
#' | LE             | W m⁻²                | mm H₂O per period        | all         |
#' | H, SW_IN       | W m⁻²                | MJ m⁻² per period        | all         |
#' | TA             | °C                   | K                        | all         |
#' | VPD            | hPa                  | kPa                      | all         |
#'
#' @param data A data frame containing FLUXNET variables. If read by
#'   [fluxnet::flux_read()], the `flux_resolution` attribute will be present
#'   and used automatically when the manifest contains multiple resolutions.
#' @param manifest A data frame with a `temporal_resolution` or
#'   `time_resolution` column for the site(s) in `data`.
#'
#' @return `data` with converted columns in analysis units. Converted columns
#'   retain their original values in a `<col>_native` companion column.
#'   Pass-through columns (already in the target unit) are not duplicated.
#'   A `conversion_log` attribute (data frame) is attached to the result,
#'   recording the variable, source unit, target unit, and whether conversion
#'   was applied.
#' @export
fluxnet_convert_units <- function(data, manifest) {
  # ── Resolve temporal resolution ──────────────────────────────────────────
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
  resolutions <- resolutions[!is.na(resolutions)]

  if (length(resolutions) > 1) {
    data_res <- attr(data, "flux_resolution")
    if (!is.null(data_res) && data_res %in% resolutions) {
      message(
        "Multiple resolutions in manifest; using data resolution from flux_read: ",
        data_res
      )
      manifest    <- manifest[manifest[[res_col]] == data_res & !is.na(manifest[[res_col]]), ]
      resolutions <- data_res
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

  # ── Seconds per period (vectorised for YY leap-year awareness) ────────────
  seconds_per_period <- switch(
    resolution,
    "HH" = 1800L,
    "HR" = 3600L,
    "DD" = 86400L,
    "WW" = 604800L,
    "MM" = {
      message(
        "MM resolution: using 30.4375-day month approximation for unit conversion."
      )
      round(30.4375 * 86400)
    },
    "YY" = {
      year_col <- intersect(c("YEAR", "TIMESTAMP"), names(data))
      if (length(year_col) > 0) {
        yr <- suppressWarnings(as.integer(data[[year_col[[1]]]]))
        ifelse(lubridate::leap_year(yr), 366L, 365L) * 86400L
      } else {
        message(
          "YY resolution: YEAR column not found; using 365.25-day approximation."
        )
        rep(round(365.25 * 86400), nrow(data))
      }
    }
  )

  # ── Load BIFVARINFO source units ──────────────────────────────────────────
  extracted_dir <- file.path(
    Sys.getenv("FLUXNET_DATA_ROOT", unset = "data"),
    "extracted"
  )
  var_units <- read_bifvarinfo_units(extracted_dir)

  # ── Conversion log ────────────────────────────────────────────────────────
  conv_rows <- list()

  log_decision <- function(variable, source_unit, target_unit, converted) {
    conv_rows[[length(conv_rows) + 1L]] <<- data.frame(
      variable    = variable,
      source_unit = if (is.na(source_unit)) "unknown" else source_unit,
      target_unit = target_unit,
      converted   = converted,
      stringsAsFactors = FALSE
    )
  }

  # ── Helper: select relevant data columns, excluding _QC flag columns ──────
  flux_cols <- function(candidates) {
    intersect(names(data), grep("_QC$", candidates, value = TRUE, invert = TRUE))
  }

  # ── Carbon fluxes ─────────────────────────────────────────────────────────
  # At HH/HR: source is µmol CO₂ m⁻² s⁻¹ → convert to gC m⁻² per period.
  # At DD/MM/WW/YY: ONEFlux delivers pre-integrated gC m⁻² totals → pass through.
  # Per-variable source unit from BIFVARINFO overrides the resolution default.
  carbon_cols <- flux_cols(c(
    "NEE", "GPP", "RECO",
    grep("^NEE_|^GPP_|^RECO_", names(data), value = TRUE)
  ))

  carbon_target_unit <- switch(toupper(resolution),
    "YY" = "gC m-2 y-1",
    "MM" = "gC m-2 m-1",
    "WW" = "gC m-2 w-1",
    "DD" = "gC m-2 d-1",
    "gC m-2 per period"   # HH, HR (post-conversion)
  )

  for (col in carbon_cols) {
    src_unit <- .infer_source_unit(col, var_units, resolution)

    # Determine if the source is in µmol (needs conversion) or gC (pass through)
    src_is_umol <- if (is.na(src_unit)) {
      # Unit unknown: convert for HH/HR, pass through for coarser resolutions
      resolution %in% c("HH", "HR")
    } else {
      grepl("umol|µmol", src_unit, ignore.case = TRUE)
    }

    if (is.na(src_unit) && !src_is_umol) {
      warning(
        "Variable '", col, "' unit unknown; assuming pre-integrated at ",
        resolution, " resolution. Passing through without conversion."
      )
    }

    if (src_is_umol) {
      # µmol CO₂ m⁻² s⁻¹ → gC m⁻² per period (molar mass of C = 12 g/mol)
      data[[paste0(col, "_native")]] <- data[[col]]
      data[[col]] <- data[[col]] * 12e-6 * seconds_per_period
      log_decision(col, src_unit, carbon_target_unit, converted = TRUE)
    } else {
      # Already in gC m⁻² per period — no transformation needed
      log_decision(col, src_unit, src_unit, converted = FALSE)
    }
  }

  # ── LE: W m⁻² → mm H₂O per period (λ ≈ 2.45×10⁶ J kg⁻¹, ρ_w = 1000 kg m⁻³)
  le_target_unit <- switch(toupper(resolution),
    "YY" = "mm H2O y-1", "MM" = "mm H2O m-1",
    "WW" = "mm H2O w-1", "DD" = "mm H2O d-1",
    "mm H2O per period"
  )
  le_cols <- flux_cols(c("LE", grep("^LE_", names(data), value = TRUE)))
  for (col in le_cols) {
    src_unit <- .infer_source_unit(col, var_units, resolution)
    data[[paste0(col, "_native")]] <- data[[col]]
    data[[col]] <- data[[col]] * seconds_per_period / 2.45e6
    log_decision(col, src_unit, le_target_unit, converted = TRUE)
  }

  # ── H and SW_IN: W m⁻² → MJ m⁻² per period
  energy_target_unit <- switch(toupper(resolution),
    "YY" = "MJ m-2 y-1", "MM" = "MJ m-2 m-1",
    "WW" = "MJ m-2 w-1", "DD" = "MJ m-2 d-1",
    "MJ m-2 per period"
  )
  energy_cols <- flux_cols(c(
    "H", "SW_IN",
    grep("^H_|^SW_IN_", names(data), value = TRUE)
  ))
  for (col in energy_cols) {
    src_unit <- .infer_source_unit(col, var_units, resolution)
    data[[paste0(col, "_native")]] <- data[[col]]
    data[[col]] <- data[[col]] * seconds_per_period * 1e-6
    log_decision(col, src_unit, energy_target_unit, converted = TRUE)
  }

  # ── TA: °C → K
  ta_cols <- flux_cols(c("TA", grep("^TA_", names(data), value = TRUE)))
  for (col in ta_cols) {
    src_unit <- .infer_source_unit(col, var_units, resolution)
    data[[paste0(col, "_native")]] <- data[[col]]
    data[[col]] <- data[[col]] + 273.15
    log_decision(col, src_unit, "K", converted = TRUE)
  }

  # ── VPD: hPa → kPa
  vpd_cols <- flux_cols(c("VPD", grep("^VPD_", names(data), value = TRUE)))
  for (col in vpd_cols) {
    src_unit <- .infer_source_unit(col, var_units, resolution)
    data[[paste0(col, "_native")]] <- data[[col]]
    data[[col]] <- data[[col]] / 10
    log_decision(col, src_unit, "kPa", converted = TRUE)
  }

  # P: mm per timestep — kept as-is; must be summed (not averaged) on aggregation.

  # ── Attach conversion log as attribute ───────────────────────────────────
  if (length(conv_rows) > 0) {
    attr(data, "conversion_log") <- do.call(rbind, conv_rows)
  } else {
    attr(data, "conversion_log") <- data.frame(
      variable    = character(0),
      source_unit = character(0),
      target_unit = character(0),
      converted   = logical(0),
      stringsAsFactors = FALSE
    )
  }

  data
}
