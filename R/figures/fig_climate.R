# R/figures/fig_climate.R
# Climate-flux scatter figures for the FLUXNET Annual Paper 2026.
#
# Ported from legacy/demo_fluxnet_plots.R and legacy/fcn_plot_FLUXNET.R.
# Reference only — do not edit legacy/ originals.
#
# Functions:
#   fig_whittaker_hexbin()  — Whittaker biome hexbin (requires WorldClim data)
#   fig_climate_scatter()   — Precipitation vs NEE + Temperature vs GPP
#   fig_xy_annual()         — General XY scatter with IGBP shapes

library(ggplot2)
library(ggtext)
library(dplyr)
library(colorspace)

# ---- Internal helpers -------------------------------------------------------

#' Check that required columns exist in a data frame
#'
#' @param data A data frame.
#' @param cols Character vector of required column names.
#' @param data_name Character. Label used in the error message.
#' @return Invisibly TRUE; stops with an informative message if any col missing.
#' @noRd
.check_cols_climate <- function(data, cols, data_name = "data") {
  missing <- setdiff(cols, names(data))
  if (length(missing) > 0L) {
    stop(
      data_name, " is missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Map flux variable name to axis label
#'
#' @param flux_var Character. Variable name.
#' @return Character label string (HTML safe for element_markdown).
#' @noRd
.flux_climate_label <- function(flux_var) {
  switch(
    sub("_.*", "", flux_var),
    NEE  = lab_nee_annual,
    GPP  = lab_gpp_annual,
    RECO = lab_reco_annual,
    flux_var
  )
}

# ---- Exported functions -----------------------------------------------------

#' Whittaker biome hexbin of temperature × precipitation coloured by flux
#'
#' Plots tower sites on a Whittaker biome diagram using hexagonal binning, with
#' each hex coloured by the median site-mean of `flux_var`. Climate normals
#' (MAT and MAP) are extracted from WorldClim raster data at tower locations —
#' see `R/external_data.R` for download instructions.
#'
#' WorldClim data must be present at one of two locations:
#' \itemize{
#'   \item `data/wc_worldclim_30s.rds` — a saved RDS of a \pkg{terra} SpatRaster;
#'   \item `wc_data/bio/*.tif` — WorldClim GeoTIFF bio-variable files.
#' }
#' Layers bio1 (Annual Mean Temperature) and bio12 (Annual Precipitation) are
#' required. **Designed for local Mac execution where WorldClim is available —
#' do not run in the Codespace.**
#'
#' @param data_yy Annual FLUXNET data frame. Must contain `site_id` and
#'   `flux_var`. Columns `location_lat` and `location_long` are joined from
#'   `metadata` when absent.
#' @param metadata Optional data frame with `site_id`, `location_lat`,
#'   `location_long`, `first_year`, and `last_year` columns. Required when
#'   `year_cutoff` is set or coordinates are absent from `data_yy`.
#' @param flux_var Character. Flux variable to summarise per hex
#'   (default `"NEE_VUT_REF"`).
#' @param year_cutoff Integer or `NULL`. When set, two filters are applied:
#'   \enumerate{
#'     \item Only sites where `first_year <= year_cutoff` AND
#'           `last_year >= year_cutoff` are kept (site was actively measuring).
#'     \item Flux records are restricted to years `<= year_cutoff`.
#'   }
#'   Requires `first_year` / `last_year` columns in `data_yy` or joinable from
#'   `metadata`.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' fig_whittaker_hexbin(data_yy, metadata = snapshot_meta, year_cutoff = 2020)
#' }
#'
#' @export
fig_whittaker_hexbin <- function(data_yy,
                                 metadata    = NULL,
                                 flux_var    = "NEE_VUT_REF",
                                 year_cutoff = NULL) {

  # --- WorldClim loading (exact pattern for local Mac execution) ---------------
  if (file.exists("data/wc_worldclim_30s.rds")) {
    wc <- terra::rast(readRDS("data/wc_worldclim_30s.rds"))
  } else {
    tifs <- list.files("wc_data/bio", pattern = "\\.tif$", full.names = TRUE)
    if (length(tifs) == 0) stop(
      "WorldClim data not found. See R/external_data.R for download instructions."
    )
    wc <- terra::rast(tifs)
  }

  if (!requireNamespace("hexbin", quietly = TRUE)) {
    stop(
      "Package 'hexbin' is required for hexagonal binning. ",
      "Install with: install.packages('hexbin')",
      call. = FALSE
    )
  }

  # --- column check -----------------------------------------------------------
  .check_cols_climate(data_yy, c("site_id", flux_var))

  # --- join metadata (coords, first/last year) only for missing columns -------
  if (!is.null(metadata) && "site_id" %in% names(metadata)) {
    need             <- c("location_lat", "location_long",
                          "first_year", "last_year", "igbp")
    missing_from_data <- setdiff(
      intersect(need, names(metadata)),
      names(data_yy)
    )
    if (length(missing_from_data) > 0L) {
      data_yy <- dplyr::left_join(
        data_yy,
        dplyr::select(metadata, "site_id",
                      dplyr::all_of(missing_from_data)),
        by = "site_id"
      )
    }
  }

  # --- year_cutoff filtering --------------------------------------------------
  if (!is.null(year_cutoff)) {
    year_cutoff <- as.integer(year_cutoff)

    if (!all(c("first_year", "last_year") %in% names(data_yy))) {
      stop(
        "year_cutoff requires 'first_year' and 'last_year' columns. ",
        "Supply a metadata argument that contains these columns.",
        call. = FALSE
      )
    }

    # (1) Keep sites actively measuring at year_cutoff
    data_yy <- dplyr::filter(
      data_yy,
      as.integer(.data$first_year) <= year_cutoff,
      as.integer(.data$last_year)  >= year_cutoff
    )

    # (2) Restrict flux records to years <= year_cutoff
    if ("YEAR" %in% names(data_yy)) {
      data_yy <- dplyr::filter(data_yy,
                               as.integer(.data$YEAR) <= year_cutoff)
    } else if ("TIMESTAMP" %in% names(data_yy)) {
      data_yy <- dplyr::filter(
        data_yy,
        as.integer(substr(as.character(.data$TIMESTAMP), 1L, 4L)) <= year_cutoff
      )
    }
  }

  # --- per-site summary: mean flux, retain first coord pair -------------------
  if (!all(c("location_lat", "location_long") %in% names(data_yy))) {
    stop(
      "Columns 'location_lat' and 'location_long' are required. ",
      "Supply a metadata argument with site coordinates.",
      call. = FALSE
    )
  }

  site_summary <- data_yy |>
    dplyr::filter(!is.na(.data[[flux_var]])) |>
    dplyr::group_by(site_id) |>
    dplyr::summarise(
      mean_flux     = mean(.data[[flux_var]], na.rm = TRUE),
      location_lat  = dplyr::first(.data$location_lat),
      location_long = dplyr::first(.data$location_long),
      .groups = "drop"
    ) |>
    dplyr::filter(!is.na(.data$location_lat), !is.na(.data$location_long))

  if (nrow(site_summary) == 0L) {
    warning("No sites with valid flux and coordinates after filtering.",
            call. = FALSE)
    return(ggplot2::ggplot() + ggplot2::labs(title = "No data") + fluxnet_theme())
  }

  # --- extract WorldClim bio1 (MAT) and bio12 (MAP) at site locations ---------
  pts <- terra::vect(
    data.frame(x = site_summary$location_long,
               y = site_summary$location_lat),
    geom = c("x", "y"),
    crs  = "EPSG:4326"
  )
  wc_vals <- as.data.frame(terra::extract(wc, pts, ID = FALSE))

  # Identify layers — robust to WorldClim 1.x and 2.x naming conventions
  bio1_col <- grep(
    "bio[_.]?0?1([^0-9]|$)",
    names(wc_vals), value = TRUE, ignore.case = TRUE, perl = TRUE
  )[1]
  bio12_col <- grep(
    "bio[_.]?12([^0-9]|$)",
    names(wc_vals), value = TRUE, ignore.case = TRUE, perl = TRUE
  )[1]

  if (is.na(bio1_col) || is.na(bio12_col)) {
    stop(
      "Cannot identify bio1 (MAT) and/or bio12 (MAP) in WorldClim layers.\n",
      "  Layers present: ", paste(head(names(wc_vals), 25), collapse = ", "),
      call. = FALSE
    )
  }

  mat_raw <- wc_vals[[bio1_col]]
  # WorldClim 1.x encodes MAT as °C × 10; 2.x stores °C directly.
  # Values > 70 in absolute magnitude are implausible temperatures — assume ×10.
  mat_vals <- if (max(abs(mat_raw), na.rm = TRUE) > 70) mat_raw / 10 else mat_raw

  site_clim <- site_summary |>
    dplyr::mutate(
      MAT = mat_vals,
      MAP = wc_vals[[bio12_col]]
    ) |>
    dplyr::filter(!is.na(.data$MAT), !is.na(.data$MAP))

  if (nrow(site_clim) == 0L) {
    warning("No valid WorldClim values extracted. Returning empty plot.",
            call. = FALSE)
    return(ggplot2::ggplot() + ggplot2::labs(title = "No data") + fluxnet_theme())
  }

  # --- labels and title -------------------------------------------------------
  flux_label  <- .flux_climate_label(flux_var)
  cutoff_text <- if (!is.null(year_cutoff))
    paste0(" \u2014 through ", year_cutoff) else ""
  title_text  <- paste0("Whittaker biome diagram", cutoff_text)

  # --- plot -------------------------------------------------------------------
  ggplot2::ggplot(
    site_clim,
    ggplot2::aes(x = .data$MAT, y = .data$MAP, z = .data$mean_flux)
  ) +
    ggplot2::stat_summary_hex(
      fun   = median,
      bins  = 15,
      alpha = 0.85
    ) +
    colorspace::scale_fill_continuous_diverging(
      palette = "Blue-Red 3",
      mid     = 0,
      name    = flux_label,
      guide   = ggplot2::guide_colorbar(
        barwidth       = 8,
        barheight      = 0.7,
        title.position = "top",
        title.hjust    = 0.5
      )
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = .data$MAT, y = .data$MAP),
      size        = 1.6,
      colour      = "grey30",
      alpha       = 0.55,
      inherit.aes = FALSE
    ) +
    ggplot2::labs(
      x        = "Mean annual temperature (\u00b0C)",
      y        = "Mean annual precipitation (mm yr<sup>-1</sup>)",
      title    = title_text,
      subtitle = paste0("n\u2009=\u2009", nrow(site_clim), " sites")
    ) +
    fluxnet_theme() +
    ggplot2::theme(
      axis.title.y    = ggtext::element_markdown(),
      legend.position = "bottom"
    )
}

#' Climate scatter plots: precipitation vs NEE and temperature vs GPP
#'
#' Produces two scatter plots coloured by IGBP: (a) annual precipitation vs a
#' user-specified flux, and (b) annual temperature vs a second flux.  Both
#' plots use `fluxnet_theme()` and `ggtext::element_markdown()` axis labels.
#'
#' The column name convention for climate variables in the converted annual
#' data is `P_F` (precipitation, mm yr⁻¹) and `TA_F` (temperature, °C).
#' The function checks for these columns and stops with an informative error if
#' they are absent.
#'
#' @param data_yy Annual FLUXNET data frame. Must contain `site_id`, `IGBP`,
#'   `P_F`, `TA_F`, and the flux columns named in `flux_var_y` and
#'   `flux_var_y2`.
#' @param flux_var_y Character. Flux variable for the precipitation plot
#'   y-axis (default `"NEE_VUT_REF"`).
#' @param flux_var_y2 Character. Flux variable for the temperature plot
#'   y-axis (default `"GPP_NT_VUT_REF"`).
#' @param metadata Optional data frame with `site_id` and `igbp` columns used
#'   to supply IGBP when absent from `data_yy`.
#'
#' @return A named list:
#'   \describe{
#'     \item{`precip_vs_flux`}{ggplot of P_F vs `flux_var_y`.}
#'     \item{`temp_vs_flux`}{ggplot of TA_F vs `flux_var_y2`.}
#'   }
#'
#' @examples
#' \dontrun{
#' plots <- fig_climate_scatter(data_yy, metadata = snapshot_meta)
#' plots$precip_vs_flux
#' plots$temp_vs_flux
#' }
#'
#' @export
fig_climate_scatter <- function(data_yy,
                                flux_var_y  = "NEE_VUT_REF",
                                flux_var_y2 = "GPP_NT_VUT_REF",
                                metadata    = NULL) {

  # --- column checks ----------------------------------------------------------
  .check_cols_climate(data_yy, c("site_id", "P_F", "TA_F", flux_var_y, flux_var_y2))

  # --- IGBP -------------------------------------------------------------------
  if (!"IGBP" %in% names(data_yy) && !is.null(metadata)) {
    if (all(c("site_id", "igbp") %in% names(metadata))) {
      data_yy <- dplyr::left_join(
        data_yy,
        dplyr::select(metadata, site_id, IGBP = igbp),
        by = "site_id"
      )
    }
  }

  igbp_present <- "IGBP" %in% names(data_yy) && any(!is.na(data_yy$IGBP))

  if (!igbp_present) {
    warning("IGBP not available — plots will not be colour-coded by IGBP.",
            call. = FALSE)
    color_scale <- ggplot2::scale_color_discrete()
    aes_col <- NULL
  } else {
    data_yy     <- dplyr::mutate(data_yy,
                                 IGBP = factor(IGBP, levels = IGBP_order))
    color_scale <- scale_color_igbp(
      guide = ggplot2::guide_legend(ncol = 3, title = "IGBP")
    )
    aes_col <- ggplot2::aes(color = IGBP)
  }

  # --- labels -----------------------------------------------------------------
  y_lab1 <- .flux_climate_label(flux_var_y)
  y_lab2 <- .flux_climate_label(flux_var_y2)

  base_aes1 <- ggplot2::aes(x = P_F, y = .data[[flux_var_y]])
  base_aes2 <- ggplot2::aes(x = TA_F, y = .data[[flux_var_y2]])

  if (igbp_present) {
    base_aes1 <- utils::modifyList(base_aes1, aes_col)
    base_aes2 <- utils::modifyList(base_aes2, aes_col)
  }

  # --- precipitation vs flux --------------------------------------------------
  p_precip <- ggplot2::ggplot(
    dplyr::filter(data_yy, !is.na(P_F), !is.na(.data[[flux_var_y]])),
    base_aes1
  ) +
    ggplot2::geom_point(alpha = 0.55, size = 2.0) +
    color_scale +
    ggplot2::labs(
      x = lab_precip_annual,
      y = y_lab1
    ) +
    fluxnet_theme() +
    ggplot2::theme(
      axis.title.x = ggtext::element_markdown(),
      axis.title.y = ggtext::element_markdown()
    )

  # --- temperature vs flux ----------------------------------------------------
  p_temp <- ggplot2::ggplot(
    dplyr::filter(data_yy, !is.na(TA_F), !is.na(.data[[flux_var_y2]])),
    base_aes2
  ) +
    ggplot2::geom_point(alpha = 0.55, size = 2.0) +
    color_scale +
    ggplot2::labs(
      x = lab_temp_annual,
      y = y_lab2
    ) +
    fluxnet_theme() +
    ggplot2::theme(
      axis.title.y = ggtext::element_markdown()
    )

  list(
    precip_vs_flux = p_precip,
    temp_vs_flux   = p_temp
  )
}

#' General XY scatter of any two annual variables, shaped by IGBP
#'
#' A flexible scatter plot for annual FLUXNET data with IGBP encoded as shape.
#' Axis labels default to the column names but can be overridden.
#'
#' @param data_yy Annual FLUXNET data frame. Must contain `site_id`, `x_var`,
#'   and `y_var`.
#' @param x_var Character. Column name for the x-axis variable.
#' @param y_var Character. Column name for the y-axis variable.
#' @param x_lab Character or `NULL`. Custom x-axis label (default: `x_var`).
#'   Supports HTML via `ggtext::element_markdown()`.
#' @param y_lab Character or `NULL`. Custom y-axis label (default: `y_var`).
#'   Supports HTML via `ggtext::element_markdown()`.
#' @param metadata Optional data frame with `site_id` and `igbp` columns for
#'   IGBP join when absent from `data_yy`.
#'
#' @return A single ggplot object.
#'
#' @examples
#' \dontrun{
#' fig_xy_annual(data_yy, x_var = "GPP_NT_VUT_REF", y_var = "LE_F_MDS",
#'               y_lab = "LE (W m<sup>-2</sup>)", metadata = snapshot_meta)
#' }
#'
#' @export
fig_xy_annual <- function(data_yy,
                          x_var,
                          y_var,
                          x_lab    = NULL,
                          y_lab    = NULL,
                          metadata = NULL) {

  .check_cols_climate(data_yy, c("site_id", x_var, y_var))

  # --- IGBP -------------------------------------------------------------------
  if (!"IGBP" %in% names(data_yy) && !is.null(metadata)) {
    if (all(c("site_id", "igbp") %in% names(metadata))) {
      data_yy <- dplyr::left_join(
        data_yy,
        dplyr::select(metadata, site_id, IGBP = igbp),
        by = "site_id"
      )
    }
  }

  igbp_present <- "IGBP" %in% names(data_yy) && any(!is.na(data_yy$IGBP))

  if (igbp_present) {
    igbp_lvls <- intersect(IGBP_order, unique(na.omit(data_yy$IGBP)))
    shp       <- stats::setNames(
      shape_igbp[seq_along(igbp_lvls)],
      igbp_lvls
    )
    data_yy <- dplyr::mutate(data_yy,
                             IGBP = factor(IGBP, levels = IGBP_order))
  } else {
    warning("IGBP not available — shapes not differentiated.", call. = FALSE)
  }

  plot_data <- dplyr::filter(
    data_yy,
    !is.na(.data[[x_var]]),
    !is.na(.data[[y_var]])
  )

  x_label <- if (!is.null(x_lab)) x_lab else x_var
  y_label <- if (!is.null(y_lab)) y_lab else y_var

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]])
  ) +
    ggplot2::geom_point(size = 2.5, stroke = 0.7, color = "black",
                        alpha = 0.75) +
    ggplot2::labs(x = x_label, y = y_label, shape = "IGBP") +
    fluxnet_theme() +
    ggplot2::theme(
      axis.title.x = ggtext::element_markdown(),
      axis.title.y = ggtext::element_markdown(),
      legend.position = "right"
    )

  if (igbp_present) {
    p <- p +
      ggplot2::aes(shape = IGBP) +
      ggplot2::scale_shape_manual(values = shp)
  }

  p
}
