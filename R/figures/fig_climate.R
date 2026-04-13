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
#' Plots tower site-years on a Whittaker biome diagram using hexagonal binning,
#' with each hex coloured by the median of `flux_var`. Requires WorldClim
#' climate data at tower locations — see `R/external_data.R` for download
#' instructions.
#'
#' @param data_yy Annual FLUXNET data frame. Must contain `site_id`,
#'   `flux_var`, and climate columns `TA_F` (°C) and `P_F` (mm yr⁻¹).
#' @param worldclim_data Unused placeholder. If `NULL` (default), the function
#'   stops with instructions to load WorldClim data first.
#' @param flux_var Character. Flux variable to summarise per hex
#'   (default `"NEE_VUT_REF"`).
#'
#' @return Never returns normally when `worldclim_data = NULL`; stops with an
#'   informative error message.
#'
#' @examples
#' \dontrun{
#' # This will stop with an informative message:
#' fig_whittaker_hexbin(data_yy)
#' }
#'
#' @export
fig_whittaker_hexbin <- function(data_yy,
                                 worldclim_data = NULL,
                                 flux_var = "NEE_VUT_REF") {
  stop(
    "WorldClim data not available. ",
    "Run load_worldclim() first. ",
    "See R/external_data.R for download instructions.",
    call. = FALSE
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
