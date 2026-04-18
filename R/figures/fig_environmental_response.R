# R/figures/fig_environmental_response.R
# Binned flux vs environment response curves for the FLUXNET Annual Paper 2026.
#
# Functions:
#   fig_environmental_response()           — response curves of flux_vars across binned
#                                            env_vars (median + IQR ribbon or IGBP lines)
#   fig_environmental_response_era5()      — 3×4 patchwork using ERA5 climate predictors
#                                            and site-year observations (not site means)
#   fig_environmental_response_worldclim() — 3×3 patchwork using WorldClim bio1/bio12
#                                            site-level constants (MAT and MAP)

library(ggplot2)
library(ggtext)
library(dplyr)

# ---- Internal helpers -------------------------------------------------------

# WorldClim bioclimatic variable names (bio1–bio19) and common aliases.
# Any env_var matching this set requires worldclim_data to be supplied.
.WORLDCLIM_VARS <- c(paste0("bio", 1:19), paste0("bio0", 1:9), "MAT", "MAP")

#' Map environment variable name to axis label
#'
#' @param var Character. Column name.
#' @return Character label (HTML safe for element_markdown).
#' @noRd
.env_response_label <- function(var) {
  switch(var,
    TA_F          = lab_temp_annual,
    P_F           = lab_precip_annual,
    aridity_index = "Aridity Index (CGIAR)",
    bio1          = "Mean Annual Temperature (\u00b0C)",
    bio12         = "Mean Annual Precipitation (mm yr<sup>-1</sup>)",
    MAT           = "Mean Annual Temperature (\u00b0C)",
    MAP           = "Mean Annual Precipitation (mm yr<sup>-1</sup>)",
    var
  )
}

#' Map flux variable name to axis label
#'
#' @param var Character. Column name.
#' @return Character label (HTML safe for element_markdown).
#' @noRd
.flux_response_label <- function(var) {
  switch(
    sub("_.*", "", var),
    NEE  = lab_nee_annual,
    GPP  = lab_gpp_annual,
    RECO = lab_reco_annual,
    LE   = "LE (MJ m<sup>-2</sup> yr<sup>-1</sup>)",
    H    = "H (MJ m<sup>-2</sup> yr<sup>-1</sup>)",
    var
  )
}

# ---- Exported function -------------------------------------------------------

#' Binned flux vs environment response curves
#'
#' For each combination of environmental variable and flux variable, bins the
#' environmental variable into `n_bins` equal-frequency quantile bins, then
#' computes the median and IQR of the flux variable within each bin.  Results
#' are returned as a named list of ggplot objects, one per `env_var × flux_var`
#' combination.
#'
#' When `IGBP` (or `igbp`) is present in `data_yy`, separate median lines are
#' drawn per IGBP class and coloured with [scale_color_igbp()].  Otherwise a
#' single median line with an IQR ribbon is drawn.
#'
#' WorldClim bioclimatic variables (`bio1`–`bio19`, `MAT`, `MAP`) require
#' `worldclim_data`.  `"aridity_index"` requires `aridity_data`.  Both are
#' joined to `data_yy` by `site_id` before binning.  When a \pkg{terra}
#' `SpatRaster` is supplied as `worldclim_data`, site-level values are
#' extracted using the same `terra::vect()` / `terra::extract()` pattern as
#' [fig_whittaker_hexbin()].
#'
#' @param data_yy Annual FLUXNET data frame.  Must contain `site_id` and the
#'   flux/env columns named in `flux_vars` and `env_vars` (unless those columns
#'   are provided via `worldclim_data` or `aridity_data`).  An `IGBP` or
#'   `igbp` column is used for IGBP-coloured grouping when present.
#' @param worldclim_data `NULL`, a \pkg{terra} `SpatRaster`, or a data frame
#'   with `site_id` and WorldClim bioclimatic variable columns.  Required when
#'   any WorldClim-derived variable (`bio1`–`bio19`, `MAT`, `MAP`) appears in
#'   `env_vars`.
#' @param aridity_data `NULL` or a data frame with `site_id` and
#'   `aridity_index` columns.  Required when `"aridity_index"` appears in
#'   `env_vars`.
#' @param flux_vars Character vector of flux variable column names for the
#'   y-axis (default: NEE, GPP, RECO, LE, H).
#' @param env_vars Character vector of environmental variable column names to
#'   bin on the x-axis (default: `TA_F`, `P_F`, `aridity_index`).
#' @param n_bins Integer.  Number of equal-frequency quantile bins (default 15).
#'
#' @return A named list of ggplot objects.  Each element is named
#'   `"<env_var>.<flux_var>"` and contains one response-curve plot.
#'   Combinations where data are insufficient are skipped with a warning.
#'
#' @examples
#' \dontrun{
#' # Default env_vars include aridity_index, which requires aridity_data.
#' # To use only flux-tower climate columns:
#' plots <- fig_environmental_response(
#'   data_yy,
#'   env_vars = c("TA_F", "P_F")
#' )
#' plots[["TA_F.NEE_VUT_REF"]]
#' }
#'
#' @export
fig_environmental_response <- function(
    data_yy,
    worldclim_data = NULL,
    aridity_data   = NULL,
    flux_vars      = c("NEE_VUT_REF", "GPP_NT_VUT_REF", "RECO_NT_VUT_REF",
                       "LE_F_MDS", "H_F_MDS"),
    env_vars       = c("TA_F", "P_F", "aridity_index"),
    n_bins         = 15L
) {
  # ---- Guards ---------------------------------------------------------------
  wc_requested <- intersect(env_vars, .WORLDCLIM_VARS)
  if (length(wc_requested) > 0L && is.null(worldclim_data)) {
    stop(
      "WorldClim data not available. See R/external_data.R for download instructions.",
      call. = FALSE
    )
  }
  if ("aridity_index" %in% env_vars && is.null(aridity_data)) {
    stop(
      "CGIAR aridity index not available. See R/external_data.R for download instructions.",
      call. = FALSE
    )
  }
  if (!"site_id" %in% names(data_yy)) {
    stop("data_yy must contain a 'site_id' column.", call. = FALSE)
  }
  n_bins <- as.integer(n_bins)

  # ---- WorldClim raster extraction (Session 3 pattern) --------------------
  # worldclim_data may arrive as a terra SpatRaster (extract at site locations)
  # or a pre-extracted data frame (use directly).
  if (!is.null(worldclim_data) && length(wc_requested) > 0L) {
    if (inherits(worldclim_data, "SpatRaster")) {
      if (!all(c("location_lat", "location_long") %in% names(data_yy))) {
        stop(
          "WorldClim SpatRaster extraction requires 'location_lat' and ",
          "'location_long' columns in data_yy.",
          call. = FALSE
        )
      }
      site_coords <- data_yy |>
        dplyr::group_by(.data$site_id) |>
        dplyr::summarise(
          location_lat  = dplyr::first(stats::na.omit(.data$location_lat)),
          location_long = dplyr::first(stats::na.omit(.data$location_long)),
          .groups = "drop"
        ) |>
        dplyr::filter(!is.na(.data$location_lat), !is.na(.data$location_long))

      pts <- terra::vect(
        data.frame(x = site_coords$location_long, y = site_coords$location_lat),
        geom = c("x", "y"),
        crs  = "EPSG:4326"
      )
      wc_extracted <- as.data.frame(terra::extract(worldclim_data, pts, ID = FALSE))

      # WorldClim 1.x encodes MAT as °C × 10; values > 70 in absolute magnitude
      # are implausible temperatures — divide by 10 to recover °C.
      bio1_col <- grep(
        "bio[_.]?0?1([^0-9]|$)", names(wc_extracted),
        value = TRUE, ignore.case = TRUE, perl = TRUE
      )[1]
      if (!is.na(bio1_col)) {
        raw_vals <- wc_extracted[[bio1_col]]
        if (max(abs(raw_vals), na.rm = TRUE) > 70) {
          wc_extracted[[bio1_col]] <- raw_vals / 10
        }
      }

      worldclim_data <- dplyr::bind_cols(
        dplyr::select(site_coords, "site_id"),
        wc_extracted
      )
    }

    if (!inherits(worldclim_data, "data.frame")) {
      stop(
        "'worldclim_data' must be a terra SpatRaster or a data frame with ",
        "'site_id' and WorldClim variable columns.",
        call. = FALSE
      )
    }
    if (!"site_id" %in% names(worldclim_data)) {
      stop("worldclim_data must contain a 'site_id' column.", call. = FALSE)
    }
  }

  # ---- Build combined dataset: data_yy + site-level external columns ------
  combined <- data_yy

  if (!is.null(worldclim_data) && length(wc_requested) > 0L) {
    avail_wc  <- intersect(wc_requested, names(worldclim_data))
    missing_wc <- setdiff(wc_requested, names(worldclim_data))
    if (length(missing_wc) > 0L) {
      warning(
        "Requested WorldClim variable(s) not found in worldclim_data: ",
        paste(missing_wc, collapse = ", "),
        call. = FALSE
      )
    }
    if (length(avail_wc) > 0L) {
      combined <- dplyr::left_join(
        combined,
        dplyr::select(worldclim_data, "site_id", dplyr::all_of(avail_wc)),
        by = "site_id"
      )
    }
  }

  if (!is.null(aridity_data) && "aridity_index" %in% env_vars) {
    if (!all(c("site_id", "aridity_index") %in% names(aridity_data))) {
      stop(
        "aridity_data must contain 'site_id' and 'aridity_index' columns.",
        call. = FALSE
      )
    }
    combined <- dplyr::left_join(
      combined,
      dplyr::select(aridity_data, "site_id", "aridity_index"),
      by = "site_id"
    )
  }

  # ---- IGBP normalisation and detection ------------------------------------
  # Accept both 'IGBP' (upper) and 'igbp' (lower); standardise to 'IGBP'.
  if (!"IGBP" %in% names(combined) && "igbp" %in% names(combined)) {
    combined <- dplyr::rename(combined, IGBP = "igbp")
  }
  igbp_present <- "IGBP" %in% names(combined) && any(!is.na(combined$IGBP))

  if (igbp_present) {
    combined <- dplyr::mutate(
      combined,
      IGBP = factor(.data$IGBP, levels = IGBP_order)
    )
  }

  # ---- Build one plot per (env_var × flux_var) ----------------------------
  out <- list()

  for (ev in env_vars) {
    if (!ev %in% names(combined)) {
      warning("env_var '", ev, "' not found in combined data — skipping.",
              call. = FALSE)
      next
    }
    x_label <- .env_response_label(ev)

    for (fv in flux_vars) {
      if (!fv %in% names(combined)) {
        warning("flux_var '", fv, "' not found in data — skipping.",
                call. = FALSE)
        next
      }
      y_label <- .flux_response_label(fv)
      key     <- paste(ev, fv, sep = ".")

      # Select and filter complete cases
      keep_cols <- c("site_id", ev, fv, if (igbp_present) "IGBP")
      df_pair <- combined |>
        dplyr::select(dplyr::all_of(keep_cols)) |>
        dplyr::filter(!is.na(.data[[ev]]), !is.na(.data[[fv]]))

      if (nrow(df_pair) < n_bins * 2L) {
        warning(
          "Insufficient data for '", ev, "' vs '", fv,
          "' (n = ", nrow(df_pair), ", need >= ", n_bins * 2L, ") — skipping.",
          call. = FALSE
        )
        next
      }

      # Equal-frequency binning and bin midpoints
      df_pair <- df_pair |>
        dplyr::mutate(env_bin = dplyr::ntile(.data[[ev]], n_bins))

      bin_mids <- df_pair |>
        dplyr::group_by(.data$env_bin) |>
        dplyr::summarise(
          bin_mid = stats::median(.data[[ev]], na.rm = TRUE),
          .groups = "drop"
        )
      df_pair <- dplyr::left_join(df_pair, bin_mids, by = "env_bin")

      # Summarise and build plot
      if (igbp_present) {
        # One median line per IGBP class, coloured with scale_color_igbp()
        summary_df <- df_pair |>
          dplyr::group_by(.data$env_bin, .data$bin_mid, .data$IGBP) |>
          dplyr::summarise(
            flux_med = stats::median(.data[[fv]], na.rm = TRUE),
            .groups  = "drop"
          ) |>
          dplyr::filter(!is.na(.data$IGBP))

        p <- ggplot2::ggplot(
          summary_df,
          ggplot2::aes(
            x     = .data$bin_mid,
            y     = .data$flux_med,
            color = .data$IGBP,
            group = .data$IGBP
          )
        ) +
          ggplot2::geom_line(linewidth = poster_linewidth, alpha = 0.85) +
          ggplot2::geom_point(size = 2.0, alpha = 0.75) +
          scale_color_igbp(
            guide = ggplot2::guide_legend(ncol = 3, title = "IGBP")
          )
      } else {
        # Single median line with IQR ribbon
        summary_df <- df_pair |>
          dplyr::group_by(.data$env_bin, .data$bin_mid) |>
          dplyr::summarise(
            flux_med = stats::median(.data[[fv]], na.rm = TRUE),
            flux_q25 = stats::quantile(.data[[fv]], 0.25, na.rm = TRUE),
            flux_q75 = stats::quantile(.data[[fv]], 0.75, na.rm = TRUE),
            .groups  = "drop"
          )

        p <- ggplot2::ggplot(
          summary_df,
          ggplot2::aes(x = .data$bin_mid)
        ) +
          ggplot2::geom_ribbon(
            ggplot2::aes(ymin = .data$flux_q25, ymax = .data$flux_q75),
            fill  = "steelblue",
            alpha = 0.25
          ) +
          ggplot2::geom_line(
            ggplot2::aes(y = .data$flux_med),
            color     = "steelblue",
            linewidth = poster_linewidth
          ) +
          ggplot2::geom_point(
            ggplot2::aes(y = .data$flux_med),
            color = "steelblue",
            size  = 2.0,
            alpha = 0.75
          )
      }

      out[[key]] <- p +
        ggplot2::labs(x = x_label, y = y_label) +
        fluxnet_theme() +
        ggplot2::theme(
          axis.title.x = ggtext::element_markdown(),
          axis.title.y = ggtext::element_markdown()
        )
    }
  }

  if (length(out) == 0L) {
    warning(
      "fig_environmental_response(): no valid env_var \u00d7 flux_var ",
      "combinations found in data.",
      call. = FALSE
    )
  }

  out
}

# ---- ERA5 variant -----------------------------------------------------------

#' Map ERA5 column name to axis label
#'
#' @param var Character. ERA5 column name.
#' @return Character label (HTML safe for element_markdown).
#' @noRd
.era5_response_label <- function(var) {
  switch(var,
    TA_ERA_C      = "Mean Annual Temperature (\u00b0C)",
    P_ERA         = "Precipitation \u2014 ERA5 (mm yr<sup>-1</sup>)",
    VPD_ERA       = "VPD \u2014 ERA5 (kPa)",
    aridity_index = "Aridity Index (CGIAR)",
    var
  )
}

#' Binned flux vs ERA5 climate response curves — site-year observations
#'
#' Produces a 3\u00d74 \pkg{patchwork} response figure. Rows are flux variables
#' (NEE, LE, H by default); columns are ERA5 climate predictors (temperature
#' in \u00b0C, precipitation, VPD, and aridity index). Each observation is
#' **one site in one year** — no site-level means are computed.
#'
#' \code{TA_ERA} (stored in Kelvin after unit conversion) is converted to
#' Celsius internally before plotting.  Physically implausible ERA5 values are
#' filtered and reported to the console before binning:
#' P_ERA > 5000 mm, VPD_ERA > 5 kPa, TA_ERA_C outside \[-30, 40\] \u00b0C.
#'
#' Binning is performed on site-year ERA5 values directly.  Each panel shows:
#' \itemize{
#'   \item Light grey points — individual site-year observations
#'   \item Grey IQR ribbon — 25th\u201375th percentile across all site-years per bin
#'   \item Dark grey median line — overall median per bin
#'   \item IGBP-coloured lines — per-IGBP bin median (requires \code{metadata})
#' }
#'
#' FLUXMET rows from \code{data_yy} are used because they carry both ERA5
#' climate values and measured fluxes in the same row.
#'
#' @param data_yy Annual FLUXNET data frame (e.g.
#'   \code{flux_data_converted_yy.rds}).  Must contain a \code{dataset} column;
#'   FLUXMET rows are selected automatically.
#' @param metadata Data frame with \code{site_id} and \code{igbp} (or
#'   \code{IGBP}) for IGBP colour coding.  If \code{NULL}, IGBP lines are
#'   omitted and only the overall ribbon and median are drawn.
#' @param aridity_data \code{NULL} or a data frame with \code{site_id} and
#'   \code{aridity_index} columns (e.g. from
#'   \code{data/snapshots/site_aridity.csv}).  Required when
#'   \code{"aridity_index"} appears in \code{env_vars}.
#' @param flux_vars Character vector of flux column names (y-axis).
#' @param env_vars Character vector of ERA5 climate column names (x-axis).
#'   Use \code{"TA_ERA_C"} for temperature in \u00b0C (computed internally from
#'   \code{TA_ERA}).  Include \code{"aridity_index"} to add a fourth column
#'   (requires \code{aridity_data}).
#' @param n_bins Integer.  Number of equal-frequency quantile bins (default 15).
#'
#' @return A \pkg{patchwork} ggplot object (\code{length(flux_vars)} rows \u00d7
#'   \code{length(env_vars)} columns) with the IGBP legend collected at the
#'   bottom.
#'
#' @examples
#' \dontrun{
#' data_yy <- readRDS("data/processed/flux_data_converted_yy.rds")
#' snap    <- readr::read_csv("data/snapshots/fluxnet_shuttle_snapshot_*.csv")
#' p <- fig_environmental_response_era5(data_yy, metadata = snap)
#' ggplot2::ggsave("review/figures/climate/fig_environmental_response_era5.png",
#'                 plot = p, width = 14, height = 12, units = "in",
#'                 dpi = 150, bg = "white")
#' }
#'
#' @export
fig_environmental_response_era5 <- function(
    data_yy,
    metadata     = NULL,
    aridity_data = NULL,
    flux_vars    = c("NEE_VUT_REF", "LE_F_MDS", "H_F_MDS"),
    env_vars     = c("TA_ERA_C", "P_ERA", "VPD_ERA", "aridity_index"),
    n_bins       = 15L
) {
  n_bins <- as.integer(n_bins)

  # ---- FLUXMET rows carry both ERA5 climate values and measured fluxes ------
  if ("dataset" %in% names(data_yy)) {
    df <- dplyr::filter(data_yy, .data$dataset == "FLUXMET")
  } else {
    df <- data_yy
  }

  # ---- Kelvin to Celsius conversion -----------------------------------------
  if ("TA_ERA" %in% names(df)) {
    df <- dplyr::mutate(df, TA_ERA_C = .data$TA_ERA - 273.15)
  }

  # ---- Filter ERA5 outliers — report removals to console --------------------
  n_before   <- nrow(df)
  out_precip <- !is.na(df$P_ERA)    & df$P_ERA    > 5000
  out_vpd    <- !is.na(df$VPD_ERA)  & df$VPD_ERA  > 5
  out_temp   <- !is.na(df$TA_ERA_C) &
                  (df$TA_ERA_C < -30 | df$TA_ERA_C > 40)
  out_any    <- out_precip | out_vpd | out_temp

  message(sprintf(
    paste0("fig_environmental_response_era5(): ERA5 outlier filter\n",
           "  P_ERA > 5000 mm:           %d site-years\n",
           "  VPD_ERA > 5 kPa:           %d site-years\n",
           "  TA_ERA_C outside [-30,40]: %d site-years\n",
           "  Total removed: %d of %d FLUXMET site-years"),
    sum(out_precip), sum(out_vpd), sum(out_temp),
    sum(out_any), n_before
  ))

  df <- dplyr::filter(df,
    is.na(.data$P_ERA)    | .data$P_ERA    <= 5000,
    is.na(.data$VPD_ERA)  | .data$VPD_ERA  <= 5,
    is.na(.data$TA_ERA_C) | (.data$TA_ERA_C >= -30 & .data$TA_ERA_C <= 40)
  )

  # ---- Join IGBP from metadata -----------------------------------------------
  if (!is.null(metadata)) {
    igbp_src <- if ("IGBP" %in% names(metadata)) "IGBP" else
                if ("igbp" %in% names(metadata)) "igbp" else NULL
    if (!is.null(igbp_src)) {
      df <- dplyr::left_join(
        df,
        dplyr::distinct(metadata, .data$site_id,
                        IGBP = .data[[igbp_src]]),
        by = "site_id"
      )
    }
  }

  if (!"IGBP" %in% names(df) && "igbp" %in% names(df)) {
    df <- dplyr::rename(df, IGBP = "igbp")
  }
  igbp_present <- "IGBP" %in% names(df) && any(!is.na(df$IGBP))

  if (igbp_present) {
    df <- dplyr::mutate(df, IGBP = factor(.data$IGBP, levels = IGBP_order))
  }

  # ---- Join aridity index (site-level constant) ------------------------------
  if (!is.null(aridity_data) && "aridity_index" %in% env_vars) {
    if (!all(c("site_id", "aridity_index") %in% names(aridity_data))) {
      stop("aridity_data must contain 'site_id' and 'aridity_index' columns.",
           call. = FALSE)
    }
    df <- dplyr::left_join(
      df,
      dplyr::select(aridity_data, "site_id", "aridity_index"),
      by = "site_id"
    )
    n_ai <- sum(!is.na(df$aridity_index))
    message(sprintf(
      "fig_environmental_response_era5(): %d of %d FLUXMET rows matched aridity index",
      n_ai, nrow(df)
    ))
  }

  # ---- Confirm TA_ERA_C is annual mean in °C ----------------------------------
  if ("TA_ERA_C" %in% env_vars && "TA_ERA_C" %in% names(df)) {
    ta_range <- range(df$TA_ERA_C, na.rm = TRUE)
    message(sprintf(
      "fig_environmental_response_era5(): TA_ERA_C range = [%.1f, %.1f] °C (annual mean)",
      ta_range[1], ta_range[2]
    ))
  }

  # ---- x-axis clipping limits per env_var ------------------------------------
  .clip_xlim <- function(ev) {
    switch(ev,
      TA_ERA_C      = c(-5,  NA),
      P_ERA         = c(NA,  4000),
      VPD_ERA       = c(NA,  2),
      aridity_index = c(NA,  2.5),
      c(NA, NA)
    )
  }

  # ---- Build one panel per (env_var \u00d7 flux_var) --------------------------------
  # show_legend = TRUE: IGBP legend inside plot (bottom-right panel only).
  # show_legend = FALSE: legend suppressed on all other panels.
  make_panel <- function(ev, fv, show_legend = FALSE) {
    if (!ev %in% names(df) || !fv %in% names(df)) return(NULL)

    keep_cols <- c(ev, fv, if (igbp_present) "IGBP")
    df_pair <- df |>
      dplyr::select(dplyr::all_of(keep_cols)) |>
      dplyr::filter(!is.na(.data[[ev]]), !is.na(.data[[fv]]))

    if (nrow(df_pair) < n_bins * 2L) {
      warning("Insufficient data for '", ev, "' vs '", fv, "' \u2014 skipping.",
              call. = FALSE)
      return(NULL)
    }

    # Equal-frequency binning on the ERA5 predictor
    df_pair <- df_pair |>
      dplyr::mutate(env_bin = dplyr::ntile(.data[[ev]], n_bins))

    bin_mids <- df_pair |>
      dplyr::group_by(.data$env_bin) |>
      dplyr::summarise(
        bin_mid = stats::median(.data[[ev]], na.rm = TRUE),
        .groups = "drop"
      )
    df_pair <- dplyr::left_join(df_pair, bin_mids, by = "env_bin")

    # Overall IQR ribbon + median (all site-years pooled)
    overall_summ <- df_pair |>
      dplyr::group_by(.data$env_bin, .data$bin_mid) |>
      dplyr::summarise(
        flux_med = stats::median(.data[[fv]], na.rm = TRUE),
        flux_q25 = stats::quantile(.data[[fv]], 0.25, na.rm = TRUE),
        flux_q75 = stats::quantile(.data[[fv]], 0.75, na.rm = TRUE),
        .groups  = "drop"
      )

    p <- ggplot2::ggplot() +
      # Individual site-year observations — small, light grey
      ggplot2::geom_point(
        data = df_pair,
        ggplot2::aes(x = .data[[ev]], y = .data[[fv]]),
        color = "#cccccc",
        size  = 0.6,
        alpha = 0.25
      ) +
      # Overall IQR ribbon
      ggplot2::geom_ribbon(
        data = overall_summ,
        ggplot2::aes(x    = .data$bin_mid,
                     ymin = .data$flux_q25,
                     ymax = .data$flux_q75),
        fill  = "#999999",
        alpha = 0.30
      ) +
      # Overall median line
      ggplot2::geom_line(
        data = overall_summ,
        ggplot2::aes(x = .data$bin_mid, y = .data$flux_med),
        color     = "#444444",
        linewidth = 0.7
      )

    # Per-IGBP median lines
    if (igbp_present) {
      igbp_summ <- df_pair |>
        dplyr::filter(!is.na(.data$IGBP)) |>
        dplyr::group_by(.data$env_bin, .data$bin_mid, .data$IGBP) |>
        dplyr::summarise(
          flux_med = stats::median(.data[[fv]], na.rm = TRUE),
          .groups  = "drop"
        )

      p <- p +
        ggplot2::geom_line(
          data = igbp_summ,
          ggplot2::aes(x     = .data$bin_mid,
                       y     = .data$flux_med,
                       color = .data$IGBP,
                       group = .data$IGBP),
          linewidth = poster_linewidth,
          alpha     = 0.85
        ) +
        scale_color_igbp(
          guide = if (show_legend) {
            ggplot2::guide_legend(
              ncol         = 3,
              title        = "IGBP",
              override.aes = list(linewidth = 2)
            )
          } else {
            "none"
          }
        )
    }

    # Build final panel with axis clipping and enlarged text
    xlim <- .clip_xlim(ev)
    p_final <- p +
      ggplot2::labs(
        x = .era5_response_label(ev),
        y = .flux_response_label(fv)
      ) +
      fluxnet_theme(base_size = 11) +
      ggplot2::theme(
        axis.title.x = ggtext::element_markdown(),
        axis.title.y = ggtext::element_markdown(),
        axis.text    = ggplot2::element_text(size = 20),
        axis.title   = ggplot2::element_text(size = 22)
      )

    if (!all(is.na(xlim))) {
      p_final <- p_final +
        ggplot2::coord_cartesian(xlim = xlim)
    }

    if (show_legend) {
      p_final + ggplot2::theme(
        legend.position      = c(0.98, 0.98),
        legend.justification = c(1, 1),
        legend.background    = ggplot2::element_rect(fill = "white", colour = NA)
      )
    } else {
      p_final + ggplot2::theme(legend.position = "none")
    }
  }

  # ---- Assemble panels in row-major order (flux_var rows, env_var cols) -----
  # IGBP legend appears only in the bottom-right panel (last flux_var × last env_var).
  panels <- vector("list", length(flux_vars) * length(env_vars))
  k <- 0L
  for (fv in flux_vars) {
    for (ev in env_vars) {
      k <- k + 1L
      is_br <- (fv == flux_vars[length(flux_vars)]) &&
               (ev == env_vars[length(env_vars)])
      panels[[k]] <- make_panel(ev, fv, show_legend = is_br)
    }
  }
  panels <- Filter(Negate(is.null), panels)

  if (length(panels) == 0L) {
    stop("fig_environmental_response_era5(): no valid panels could be built.",
         call. = FALSE)
  }

  # plot_layout(axes = "collect") suppresses redundant axis labels.
  # guides are NOT collected — each panel manages its own legend.
  col_labels  <- vapply(env_vars, .era5_response_label, character(1L))
  caption_str <- paste(col_labels, collapse = "  \u2022  ")

  patchwork::wrap_plots(panels,
                        nrow = length(flux_vars),
                        ncol = length(env_vars)) +
    patchwork::plot_layout(axes = "collect") +
    patchwork::plot_annotation(caption = caption_str)
}

# ---- WorldClim variant ------------------------------------------------------

#' Map WorldClim predictor column to axis label
#'
#' @param var Character. Predictor column name.
#' @return Character label (HTML safe for element_markdown).
#' @noRd
.worldclim_response_label <- function(var) {
  switch(var,
    mat_worldclim = "Mean Annual Temperature \u2014 WorldClim (\u00b0C)",
    map_worldclim = "Annual Precipitation \u2014 WorldClim (mm yr<sup>-1</sup>)",
    aridity_index = "Aridity Index (CGIAR)",
    var
  )
}

#' Binned flux vs WorldClim climate response curves — site-year observations
#'
#' Produces a 3\u00d73 \pkg{patchwork} response figure. Rows are flux variables
#' (NEE, LE, H by default); columns are WorldClim site-level climate normals
#' (bio1 = MAT in \u00b0C; bio12 = MAP in mm yr\u207b\u00b9) plus aridity index when
#' \code{aridity_data} is supplied. Each y-axis observation is **one site in
#' one year**; the x-axis value is the site-level constant (identical for all
#' years at a site).
#'
#' \code{worldclim_data} must be a data frame (or path to a CSV) with columns
#' \code{site_id}, \code{mat_worldclim}, and \code{map_worldclim}, as produced
#' by \code{scripts/step1_extract_worldclim.R} and saved to
#' \code{data/snapshots/site_worldclim.csv}.
#'
#' Binning is performed on the WorldClim values.  Each panel shows:
#' \itemize{
#'   \item Light grey points — individual site-year flux observations
#'   \item Grey IQR ribbon — 25th\u201375th percentile across all site-years per bin
#'   \item Dark grey median line — overall bin median
#'   \item IGBP-coloured lines — per-IGBP bin median (requires \code{metadata})
#' }
#'
#' @param data_yy Annual FLUXNET data frame (e.g.
#'   \code{flux_data_converted_yy.rds}).  FLUXMET rows are selected
#'   automatically via the \code{dataset} column.
#' @param worldclim_data Data frame with \code{site_id},
#'   \code{mat_worldclim} (bio1, \u00b0C), and \code{map_worldclim} (bio12, mm).
#'   Alternatively, a character path to a CSV file with those columns.
#' @param aridity_data \code{NULL} or a data frame with \code{site_id} and
#'   \code{aridity_index} columns (e.g. from
#'   \code{data/snapshots/site_aridity.csv}).  When supplied, a third column
#'   (aridity index) is added to the patchwork.
#' @param flux_vars Character vector of flux column names (y-axis).
#' @param metadata Data frame with \code{site_id} and \code{igbp} (or
#'   \code{IGBP}) for IGBP colour coding.  If \code{NULL}, IGBP lines are
#'   omitted.
#' @param n_bins Integer.  Number of equal-frequency quantile bins (default 15).
#'
#' @return A \pkg{patchwork} ggplot object (3 rows \u00d7 2\u20133 columns, depending
#'   on whether \code{aridity_data} is supplied) with the IGBP legend
#'   collected at the bottom.
#'
#' @examples
#' \dontrun{
#' wc <- readr::read_csv("data/snapshots/site_worldclim.csv")
#' p  <- fig_environmental_response_worldclim(data_yy, worldclim_data = wc,
#'                                            metadata = snapshot_meta)
#' ggplot2::ggsave("review/figures/climate/fig_environmental_response_worldclim.png",
#'                 plot = p, width = 10, height = 12, units = "in",
#'                 dpi = 150, bg = "white")
#' }
#'
#' @export
fig_environmental_response_worldclim <- function(
    data_yy,
    worldclim_data,
    aridity_data  = NULL,
    flux_vars     = c("NEE_VUT_REF", "LE_F_MDS", "H_F_MDS"),
    metadata      = NULL,
    n_bins        = 15L
) {
  n_bins <- as.integer(n_bins)

  # ---- Accept path or data frame for worldclim_data -------------------------
  if (is.character(worldclim_data)) {
    worldclim_data <- readr::read_csv(worldclim_data, show_col_types = FALSE)
  }
  if (!is.data.frame(worldclim_data) ||
      !all(c("site_id", "mat_worldclim", "map_worldclim") %in%
           names(worldclim_data))) {
    stop(
      "worldclim_data must be a data frame (or CSV path) with columns: ",
      "site_id, mat_worldclim, map_worldclim.",
      call. = FALSE
    )
  }

  # ---- FLUXMET rows carry both ERA5 climate values and measured fluxes ------
  if ("dataset" %in% names(data_yy)) {
    df <- dplyr::filter(data_yy, .data$dataset == "FLUXMET")
  } else {
    df <- data_yy
  }

  # ---- Join WorldClim site-level constants ----------------------------------
  df <- dplyr::left_join(
    df,
    dplyr::select(worldclim_data, "site_id", "mat_worldclim", "map_worldclim"),
    by = "site_id"
  )

  n_matched <- sum(!is.na(df$mat_worldclim))
  message(sprintf(
    "fig_environmental_response_worldclim(): %d of %d FLUXMET rows matched WorldClim",
    n_matched, nrow(df)
  ))

  # ---- Join aridity index (site-level constant) ------------------------------
  if (!is.null(aridity_data)) {
    if (!all(c("site_id", "aridity_index") %in% names(aridity_data))) {
      stop("aridity_data must contain 'site_id' and 'aridity_index' columns.",
           call. = FALSE)
    }
    df <- dplyr::left_join(
      df,
      dplyr::select(aridity_data, "site_id", "aridity_index"),
      by = "site_id"
    )
    n_ai <- sum(!is.na(df$aridity_index))
    message(sprintf(
      "fig_environmental_response_worldclim(): %d of %d FLUXMET rows matched aridity index",
      n_ai, nrow(df)
    ))
  }

  # ---- Join IGBP from metadata -----------------------------------------------
  if (!is.null(metadata)) {
    igbp_src <- if ("IGBP" %in% names(metadata)) "IGBP" else
                if ("igbp" %in% names(metadata)) "igbp" else NULL
    if (!is.null(igbp_src)) {
      df <- dplyr::left_join(
        df,
        dplyr::distinct(metadata, .data$site_id,
                        IGBP = .data[[igbp_src]]),
        by = "site_id"
      )
    }
  }

  if (!"IGBP" %in% names(df) && "igbp" %in% names(df)) {
    df <- dplyr::rename(df, IGBP = "igbp")
  }
  igbp_present <- "IGBP" %in% names(df) && any(!is.na(df$IGBP))

  if (igbp_present) {
    df <- dplyr::mutate(df, IGBP = factor(.data$IGBP, levels = IGBP_order))
  }

  env_vars <- c("mat_worldclim", "map_worldclim",
                if (!is.null(aridity_data)) "aridity_index")

  # ---- Build one panel per (env_var x flux_var) -----------------------------
  make_panel <- function(ev, fv) {
    if (!ev %in% names(df) || !fv %in% names(df)) return(NULL)

    keep_cols <- c(ev, fv, if (igbp_present) "IGBP")
    df_pair <- df |>
      dplyr::select(dplyr::all_of(keep_cols)) |>
      dplyr::filter(!is.na(.data[[ev]]), !is.na(.data[[fv]]))

    if (nrow(df_pair) < n_bins * 2L) {
      warning("Insufficient data for '", ev, "' vs '", fv, "' \u2014 skipping.",
              call. = FALSE)
      return(NULL)
    }

    # Equal-frequency binning on the WorldClim predictor
    df_pair <- df_pair |>
      dplyr::mutate(env_bin = dplyr::ntile(.data[[ev]], n_bins))

    bin_mids <- df_pair |>
      dplyr::group_by(.data$env_bin) |>
      dplyr::summarise(
        bin_mid = stats::median(.data[[ev]], na.rm = TRUE),
        .groups = "drop"
      )
    df_pair <- dplyr::left_join(df_pair, bin_mids, by = "env_bin")

    # Overall IQR ribbon + median (all site-years pooled)
    overall_summ <- df_pair |>
      dplyr::group_by(.data$env_bin, .data$bin_mid) |>
      dplyr::summarise(
        flux_med = stats::median(.data[[fv]], na.rm = TRUE),
        flux_q25 = stats::quantile(.data[[fv]], 0.25, na.rm = TRUE),
        flux_q75 = stats::quantile(.data[[fv]], 0.75, na.rm = TRUE),
        .groups  = "drop"
      )

    p <- ggplot2::ggplot() +
      # Individual site-year observations — small, light grey
      ggplot2::geom_point(
        data = df_pair,
        ggplot2::aes(x = .data[[ev]], y = .data[[fv]]),
        color = "#cccccc",
        size  = 0.6,
        alpha = 0.25
      ) +
      # Overall IQR ribbon
      ggplot2::geom_ribbon(
        data = overall_summ,
        ggplot2::aes(x    = .data$bin_mid,
                     ymin = .data$flux_q25,
                     ymax = .data$flux_q75),
        fill  = "#999999",
        alpha = 0.30
      ) +
      # Overall median line
      ggplot2::geom_line(
        data = overall_summ,
        ggplot2::aes(x = .data$bin_mid, y = .data$flux_med),
        color     = "#444444",
        linewidth = 0.7
      )

    # Per-IGBP median lines
    if (igbp_present) {
      igbp_summ <- df_pair |>
        dplyr::filter(!is.na(.data$IGBP)) |>
        dplyr::group_by(.data$env_bin, .data$bin_mid, .data$IGBP) |>
        dplyr::summarise(
          flux_med = stats::median(.data[[fv]], na.rm = TRUE),
          .groups  = "drop"
        )

      p <- p +
        ggplot2::geom_line(
          data = igbp_summ,
          ggplot2::aes(x     = .data$bin_mid,
                       y     = .data$flux_med,
                       color = .data$IGBP,
                       group = .data$IGBP),
          linewidth = poster_linewidth,
          alpha     = 0.85
        ) +
        scale_color_igbp(
          guide = ggplot2::guide_legend(
            ncol         = 5,
            title        = "IGBP",
            override.aes = list(linewidth = 2)
          )
        )
    }

    p +
      ggplot2::labs(
        x = .worldclim_response_label(ev),
        y = .flux_response_label(fv)
      ) +
      fluxnet_theme(base_size = 11) +
      ggplot2::theme(
        axis.title.x = ggtext::element_markdown(),
        axis.title.y = ggtext::element_markdown()
      )
  }

  # ---- Assemble panels in row-major order (flux_var rows, env_var cols) -----
  panels <- vector("list", length(flux_vars) * length(env_vars))
  k <- 0L
  for (fv in flux_vars) {
    for (ev in env_vars) {
      k <- k + 1L
      panels[[k]] <- make_panel(ev, fv)
    }
  }
  panels <- Filter(Negate(is.null), panels)

  if (length(panels) == 0L) {
    stop("fig_environmental_response_worldclim(): no valid panels.",
         call. = FALSE)
  }

  col_labels <- vapply(env_vars, .worldclim_response_label, character(1L))
  caption_str <- paste(col_labels, collapse = "  \u2022  ")

  (patchwork::wrap_plots(panels,
                         nrow = length(flux_vars),
                         ncol = length(env_vars)) +
    patchwork::plot_layout(axes = "collect", guides = "collect") +
    patchwork::plot_annotation(caption = caption_str)) &
    ggplot2::theme(legend.position = "bottom")
}
