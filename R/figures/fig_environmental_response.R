# R/figures/fig_environmental_response.R
# Binned flux vs environment response curves for the FLUXNET Annual Paper 2026.
#
# Functions:
#   fig_environmental_response()      — response curves of flux_vars across binned
#                                       env_vars (median + IQR ribbon or IGBP lines)
#   fig_environmental_response_era5() — 3×3 patchwork using ERA5 climate predictors
#                                       and site-year observations (not site means)

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
    TA_ERA  = "Temperature \u2014 ERA5 (K)",
    P_ERA   = "Precipitation \u2014 ERA5 (mm yr<sup>-1</sup>)",
    VPD_ERA = "VPD \u2014 ERA5 (kPa)",
    var
  )
}

#' Binned flux vs ERA5 climate response curves — site-year observations
#'
#' Produces a 3\u00d73 \pkg{patchwork} response figure. Rows are flux variables
#' (NEE, GPP, RECO by default); columns are ERA5 climate predictors
#' (temperature, precipitation, VPD). Each observation is **one site in one
#' year** — no site-level means are computed.
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
#' @param flux_vars Character vector of flux column names (y-axis).
#' @param env_vars Character vector of ERA5 climate column names (x-axis).
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
    metadata  = NULL,
    flux_vars = c("NEE_VUT_REF", "GPP_NT_VUT_REF", "RECO_NT_VUT_REF"),
    env_vars  = c("TA_ERA", "P_ERA", "VPD_ERA"),
    n_bins    = 15L
) {
  n_bins <- as.integer(n_bins)

  # ---- FLUXMET rows carry both ERA5 climate values and measured fluxes ------
  if ("dataset" %in% names(data_yy)) {
    df <- dplyr::filter(data_yy, .data$dataset == "FLUXMET")
  } else {
    df <- data_yy
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

  # ---- Build one panel per (env_var \u00d7 flux_var) --------------------------------
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
          guide = ggplot2::guide_legend(
            ncol         = 5,
            title        = "IGBP",
            override.aes = list(linewidth = 2)
          )
        )
    }

    p +
      ggplot2::labs(
        x = .era5_response_label(ev),
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
    stop("fig_environmental_response_era5(): no valid panels could be built.",
         call. = FALSE)
  }

  patchwork::wrap_plots(panels,
                        nrow   = length(flux_vars),
                        ncol   = length(env_vars),
                        guides = "collect") &
    ggplot2::theme(legend.position = "bottom")
}
