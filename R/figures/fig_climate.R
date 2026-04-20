# R/figures/fig_climate.R
# Climate-flux scatter figures for the FLUXNET Annual Paper 2026.
#
# Ported from legacy/demo_fluxnet_plots.R and legacy/fcn_plot_FLUXNET.R.
# Reference only — do not edit legacy/ originals.
#
# Functions:
#   fig_whittaker_worldclim()  — Whittaker hexbin (WorldClim climate, all site lists)
#   fig_climate_scatter()      — Precipitation vs NEE + Temperature vs GPP
#   fig_xy_annual()            — General XY scatter with IGBP shapes
#
# Deprecated (moved to R/figures/fig_climate_legacy.R):
#   fig_whittaker_hexbin()           — use fig_whittaker_worldclim() instead
#   fig_whittaker_hexbin_era5()      — use fig_whittaker_worldclim() instead
#   fig_whittaker_hexbin_worldclim() — use fig_whittaker_worldclim() instead

library(ggplot2)
library(dplyr)
library(colorspace)

# ---- Shared Whittaker style constants ----------------------------------------

#' Shared visual parameters for all Whittaker biome figures
#'
#' A named list used as the default \code{style} argument to
#' \code{\link{fig_whittaker_worldclim}}.  Override individual elements by
#' passing a modified copy to the function.
#'
#' @format A named list with elements:
#' \describe{
#'   \item{xlim}{MAT axis limits in °C.}
#'   \item{ylim}{MAP axis limits in mm yr⁻¹.}
#'   \item{width_in, height_in}{Default ggsave dimensions in inches.}
#'   \item{legend_pos, legend_just}{Legend position and justification (NDC).}
#'   \item{detail_x, detail_y}{Inset detail-text anchor (NDC fractions).}
#'   \item{nee_lims}{Colour-scale limits; computed at runtime when \code{NULL}.}
#' }
#' @note Axis and legend labels are \code{expression()} objects constructed at
#'   plot-build time in \code{\link{fig_whittaker_worldclim}} — they are not
#'   stored here, because R expressions cannot be stored in a plain list and
#'   retrieved as expressions.
#' @export
WHITTAKER_STYLE <- list(
  xlim        = c(-15, 35),
  ylim        = c(0, 4000),
  width_in    = 14,
  height_in   = 7,
  legend_pos  = c(0.02, 0.88),
  legend_just = c(0, 1),
  detail_x    = 0.02,
  detail_y    = 0.98,
  nee_lims    = NULL
)

# ---- fig_whittaker_worldclim ------------------------------------------------

#' Whittaker biome hexbin — WorldClim climate, any site list
#'
#' Plots sites on a MAT × MAP climate space using hexagonal binning with each
#' hex coloured by the median site-mean NEE from \code{data_yy}.  Sites present
#' in \code{site_meta} but absent from the Shuttle flux data render as grey
#' hexbins (NA bins are not drawn).
#'
#' Climate source priority per site:
#' \enumerate{
#'   \item \code{worldclim_csv} — pre-computed table (fast, Codespace-safe).
#'   \item \code{terra::extract()} from \code{worldclim_dir} GeoTIFFs — used
#'     only for sites not already in the CSV.
#' }
#'
#' NEE colour-scale limits are derived from the 5th–95th percentile of the
#' \emph{full} \code{data_yy} distribution (before any site or year filtering),
#' ensuring a shared scale across all panels in a composite figure.  Pass a
#' pre-computed \code{style$nee_lims} to override.
#'
#' @param data_yy Annual FLUXNET data frame (Shuttle \code{flux_data_converted_yy.rds}).
#'   Must contain \code{site_id}, \code{NEE_VUT_REF}, and \code{YEAR}.
#' @param site_meta Data frame of sites to display.  Must contain \code{site_id},
#'   \code{location_lat}, and \code{location_long}.  For \code{year_cutoff}
#'   filtering, also needs \code{first_year}.
#' @param worldclim_dir Character. Directory of WorldClim 2.1 2.5m bio GeoTIFFs
#'   (default \code{"data/external/worldclim/climate/wc2.1_2.5m/"}).
#' @param worldclim_csv Character. Path to pre-computed per-site WorldClim table
#'   (default \code{"data/snapshots/site_worldclim.csv"}).
#' @param year_cutoff Integer or \code{NULL}.  When set, \code{site_meta} is
#'   filtered to \code{first_year <= year_cutoff} and \code{data_yy} records to
#'   \code{YEAR <= year_cutoff}.
#' @param detail_label Character or \code{NULL}.  Dataset label shown in the
#'   inset text (e.g. \code{"FLUXNET Shuttle 2025"}).
#' @param style Named list of visual parameters.  Defaults to
#'   \code{\link{WHITTAKER_STYLE}}.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' snapshot_meta <- readr::read_csv("data/snapshots/fluxnet_shuttle_snapshot_latest.csv")
#' data_yy       <- readRDS("data/processed/flux_data_converted_yy.rds")
#' p <- fig_whittaker_worldclim(data_yy, snapshot_meta,
#'                              detail_label = "FLUXNET Shuttle 2025")
#' }
#'
#' @export
fig_whittaker_worldclim <- function(
  data_yy,
  site_meta,
  worldclim_dir = "data/external/worldclim/climate/wc2.1_2.5m/",
  worldclim_csv = "data/snapshots/site_worldclim.csv",
  year_cutoff   = NULL,
  detail_label  = NULL,
  style         = WHITTAKER_STYLE
) {

  for (pkg in c("hexbin", "colorspace")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required for fig_whittaker_worldclim().",
           call. = FALSE)
    }
  }

  .check_cols_climate(data_yy,   c("site_id", "NEE_VUT_REF", "YEAR"))
  .check_cols_climate(site_meta, c("site_id", "location_lat", "location_long"))

  # --- year_cutoff: filter site_meta to sites established by cutoff year ------
  if (!is.null(year_cutoff)) {
    year_cutoff <- as.integer(year_cutoff)
    if ("first_year" %in% names(site_meta)) {
      site_meta <- dplyr::filter(site_meta,
                                 as.integer(.data$first_year) <= year_cutoff)
    }
  }

  # --- WorldClim climate lookup -----------------------------------------------
  wc_known <- if (file.exists(worldclim_csv)) {
    readr::read_csv(worldclim_csv, show_col_types = FALSE)
  } else {
    data.frame(site_id       = character(0),
               mat_worldclim = numeric(0),
               map_worldclim = numeric(0))
  }

  need_extract <- site_meta |>
    dplyr::filter(
      !.data$site_id %in% wc_known$site_id,
      !is.na(.data$location_lat),
      !is.na(.data$location_long)
    ) |>
    dplyr::distinct(.data$site_id, .keep_all = TRUE)

  wc_extracted <- NULL
  if (nrow(need_extract) > 0L) {
    if (requireNamespace("terra", quietly = TRUE) && dir.exists(worldclim_dir)) {
      bio_files  <- list.files(worldclim_dir, pattern = "\\.tif$",
                               full.names = TRUE)
      bio1_file  <- bio_files[grepl("_bio_1\\.tif$",  bio_files,
                                    ignore.case = TRUE)]
      bio12_file <- bio_files[grepl("_bio_12\\.tif$", bio_files,
                                    ignore.case = TRUE)]

      if (length(bio1_file) == 1L && length(bio12_file) == 1L) {
        wc_rast <- terra::rast(c(bio1_file, bio12_file))
        pts     <- terra::vect(
          data.frame(x = need_extract$location_long,
                     y = need_extract$location_lat),
          geom = c("x", "y"), crs = "EPSG:4326"
        )
        vals    <- as.data.frame(terra::extract(wc_rast, pts, ID = FALSE))
        mat_raw <- vals[[1L]]
        mat_c   <- if (max(abs(mat_raw), na.rm = TRUE) > 70) mat_raw / 10
                   else mat_raw
        wc_extracted <- need_extract |>
          dplyr::mutate(mat_worldclim = mat_c,
                        map_worldclim = vals[[2L]])
        message("fig_whittaker_worldclim: extracted WorldClim for ",
                nrow(wc_extracted), " sites not in worldclim_csv.")
      } else {
        message("fig_whittaker_worldclim: bio_1.tif or bio_12.tif not found in ",
                worldclim_dir, ". Those sites will lack climate data.")
      }
    } else {
      message("fig_whittaker_worldclim: terra unavailable or worldclim_dir ",
              "missing. ", nrow(need_extract), " sites will lack climate data.")
    }
  }

  wc_all <- dplyr::bind_rows(
    wc_known,
    if (!is.null(wc_extracted))
      dplyr::select(wc_extracted, "site_id", "mat_worldclim", "map_worldclim")
    else NULL
  )

  # --- NEE colour limits from FULL data_yy (not filtered) ---------------------
  if (is.null(style$nee_lims)) {
    nee_q   <- quantile(data_yy$NEE_VUT_REF, probs = c(0.05, 0.95),
                        na.rm = TRUE)
    nee_max <- max(abs(nee_q))
    style$nee_lims <- c(-nee_max, nee_max)
  }

  # --- filter data_yy to sites and year_cutoff --------------------------------
  site_ids  <- unique(site_meta$site_id)
  data_filt <- dplyr::filter(data_yy, .data$site_id %in% site_ids)
  if (!is.null(year_cutoff)) {
    data_filt <- dplyr::filter(data_filt,
                               as.integer(.data$YEAR) <= year_cutoff)
  }

  # --- per-site NEE median and site-year count --------------------------------
  site_nee <- data_filt |>
    dplyr::filter(!is.na(.data$NEE_VUT_REF)) |>
    dplyr::group_by(.data$site_id) |>
    dplyr::summarise(
      median_nee  = median(.data$NEE_VUT_REF, na.rm = TRUE),
      n_nee_years = dplyr::n_distinct(.data$YEAR),
      .groups     = "drop"
    )

  n_sites      <- length(site_ids)
  n_site_years <- sum(site_nee$n_nee_years)

  # --- assemble plot data (climate + NEE) -------------------------------------
  plot_data <- site_meta |>
    dplyr::select("site_id", "location_lat", "location_long") |>
    dplyr::distinct(.data$site_id, .keep_all = TRUE) |>
    dplyr::left_join(
      dplyr::select(wc_all, "site_id", "mat_worldclim", "map_worldclim"),
      by = "site_id"
    ) |>
    dplyr::left_join(
      dplyr::select(site_nee, "site_id", "median_nee"),
      by = "site_id"
    ) |>
    dplyr::filter(!is.na(.data$mat_worldclim), !is.na(.data$map_worldclim))

  if (nrow(plot_data) == 0L) {
    warning("fig_whittaker_worldclim: no sites with climate data after filtering.",
            call. = FALSE)
    return(ggplot2::ggplot() +
             ggplot2::labs(title = "No data") +
             fluxnet_theme())
  }

  # --- inset detail text (top-left) -------------------------------------------
  detail_str <- paste0(
    if (!is.null(detail_label)) paste0(detail_label, "\n") else "",
    "N = ", n_sites, " sites | ", n_site_years, " site-years"
  )

  # --- build plot -------------------------------------------------------------
  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$mat_worldclim, y = .data$map_worldclim,
                 z = .data$median_nee)
  ) +
    ggplot2::stat_summary_hex(
      fun   = function(x) if (all(is.na(x))) NA_real_
                          else median(x, na.rm = TRUE),
      bins  = 15,
      alpha = 0.85
    ) +
    colorspace::scale_fill_continuous_diverging(
      palette  = "Blue-Red 3",
      mid      = 0,
      limits   = style$nee_lims,
      oob      = scales::squish,
      na.value = NA
      # guide set separately below via guides() so expression() title works
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(
        title          = expression("NEE (g C m"^{-2}*" yr"^{-1}*")"),
        title.position = "top",
        barwidth       = 15,
        barheight      = 0.8,
        direction      = "horizontal"
      )
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = .data$mat_worldclim, y = .data$map_worldclim),
      size        = 1.4,
      colour      = "grey30",
      alpha       = 0.50,
      inherit.aes = FALSE
    ) +
    ggplot2::annotate(
      "text",
      x     = -Inf, y = Inf,
      label = detail_str,
      hjust = -0.07, vjust = 1.3,
      size  = 6.5                          # ≈ 18 pt (size * 2.835)
    ) +
    ggplot2::coord_cartesian(
      xlim = style$xlim,
      ylim = style$ylim
    ) +
    ggplot2::labs(
      x = expression("Mean Annual Temperature (" * degree * "C)"),
      y = expression(atop("Mean Annual Precipitation", "(mm yr"^{-1}*")"))
    ) +
    .whittaker_theme(style)

  p
}


# ---- Internal helpers -------------------------------------------------------

#' Build the ggplot2 theme for Whittaker figures
#'
#' Uses \code{element_text()} (NOT \code{element_markdown()}) for axis titles so
#' that \code{expression()} plotmath labels render correctly.  \code{fluxnet_theme()}
#' sets \code{element_markdown()} globally, which coerces expressions to raw
#' strings; this function bypasses that by building from \code{theme_classic()}.
#'
#' @param style Named list — the \code{WHITTAKER_STYLE} passed to the parent call.
#' @return A ggplot2 theme object.
#' @noRd
.whittaker_theme <- function(style) {
  ggplot2::theme_classic(base_size = 16) +
    ggplot2::theme(
      panel.border           = ggplot2::element_rect(color = "black", fill = NA,
                                                     linewidth = 0.8),
      panel.background       = ggplot2::element_blank(),
      axis.text              = ggplot2::element_text(color = "black", size = 22),
      axis.ticks             = ggplot2::element_line(color = "black"),
      axis.ticks.length      = grid::unit(-4, "pt"),
      axis.ticks.length.x    = grid::unit(-4, "pt"),
      axis.ticks.length.y    = grid::unit(-4, "pt"),
      # element_text (NOT element_markdown) — required for plotmath expressions
      axis.title             = ggplot2::element_text(size = 24),
      legend.text            = ggplot2::element_text(size = 20),
      legend.title           = ggplot2::element_text(size = 22),
      legend.position        = "inside",
      legend.position.inside = style$legend_pos,
      legend.justification   = style$legend_just,
      legend.background      = ggplot2::element_rect(fill = "white", color = NA)
    )
}

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
