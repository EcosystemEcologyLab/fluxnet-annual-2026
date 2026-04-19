# R/figures/fig_climate.R
# Climate-flux scatter figures for the FLUXNET Annual Paper 2026.
#
# Ported from legacy/demo_fluxnet_plots.R and legacy/fcn_plot_FLUXNET.R.
# Reference only — do not edit legacy/ originals.
#
# Functions:
#   fig_whittaker_hexbin()           — Wrapper (ERA5 by default; source arg to switch)
#   fig_whittaker_hexbin_era5()      — Whittaker hexbin using TA_ERA/P_ERA (Codespace-safe)
#   fig_whittaker_hexbin_worldclim() — Whittaker hexbin using WorldClim bio1/bio12 raster
#   fig_climate_scatter()            — Precipitation vs NEE + Temperature vs GPP
#   fig_xy_annual()                  — General XY scatter with IGBP shapes

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

#' Hexbin of temperature × precipitation coloured by flux (ERA5 climate)
#'
#' Plots tower sites on a temperature × precipitation scatter using hexagonal
#' binning, with each hex coloured by the median site-mean of \code{flux_var}.
#' Climate normals (MAT and MAP) are derived entirely from ERA5 variables
#' already present in the annual FLUXNET data (\code{TA_ERA} in K;
#' \code{P_ERA} in mm yr⁻¹), so no external raster data are needed.
#' **This version runs in the Codespace.**
#'
#' Site-level MAT and MAP are computed by averaging \code{TA_ERA - 273.15} and
#' \code{P_ERA} across all years for each site before plotting.  Sites with
#' implausibly high precipitation (> 8 000 mm yr⁻¹, indicating ERA5 unit
#' conversion artifacts) are dropped with a \code{message()} before plotting.
#'
#' @param data_yy Annual FLUXNET data frame.  Must contain \code{site_id},
#'   \code{TA_ERA} (K), \code{P_ERA} (mm yr⁻¹), and \code{flux_var}.
#' @param flux_var Character. Flux variable to summarise per hex
#'   (default \code{"NEE_VUT_REF"}).
#' @param year_cutoff Integer or \code{NULL}. When set, only sites with
#'   \code{first_year <= year_cutoff} are included (i.e. sites established by
#'   that year), and flux records are restricted to years
#'   \code{<= year_cutoff}.  This shows the network as it had grown by each
#'   cutoff, regardless of whether recent data has been submitted yet.
#'   Requires \code{YEAR} and \code{first_year} columns in \code{data_yy}.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' fig_whittaker_hexbin_era5(data_yy, flux_var = "NEE_VUT_REF")
#' fig_whittaker_hexbin_era5(data_yy, year_cutoff = 2020)
#' }
#'
#' @export
fig_whittaker_hexbin_era5 <- function(data_yy,
                                      flux_var    = "NEE_VUT_REF",
                                      year_cutoff = NULL,
                                      metadata    = NULL) {

  if (!requireNamespace("hexbin", quietly = TRUE)) {
    stop(
      "Package 'hexbin' is required for hexagonal binning. ",
      "Install with: install.packages('hexbin')",
      call. = FALSE
    )
  }

  # --- column check -----------------------------------------------------------
  .check_cols_climate(data_yy, c("site_id", "TA_ERA", "P_ERA", flux_var))

  # --- year_cutoff filtering --------------------------------------------------
  # Filter: sites established by year_cutoff (first_year <= year_cutoff) AND
  # flux records up to year_cutoff.  last_year is intentionally not used — we
  # show all sites that had started by the cutoff year, regardless of whether
  # recent data has been submitted yet.
  if (!is.null(year_cutoff)) {
    year_cutoff <- as.integer(year_cutoff)
    if (!"YEAR" %in% names(data_yy)) {
      stop(
        "year_cutoff requires a 'YEAR' column in data_yy.",
        call. = FALSE
      )
    }
    if ("first_year" %in% names(data_yy)) {
      data_yy <- dplyr::filter(data_yy,
                                as.integer(.data$first_year) <= year_cutoff)
    }
    data_yy <- dplyr::filter(data_yy,
                              as.integer(.data$YEAR) <= year_cutoff)
  }

  # --- per-site summary: mean ERA5 climate + mean flux ------------------------
  # TA_ERA is in Kelvin (unit conversion step adds 273.15); subtract to get °C.
  site_clim <- data_yy |>
    dplyr::filter(!is.na(.data$TA_ERA),
                  !is.na(.data$P_ERA),
                  !is.na(.data[[flux_var]])) |>
    dplyr::group_by(.data$site_id) |>
    dplyr::summarise(
      MAT       = mean(.data$TA_ERA - 273.15, na.rm = TRUE),
      MAP       = mean(.data$P_ERA,            na.rm = TRUE),
      mean_flux = mean(.data[[flux_var]],       na.rm = TRUE),
      .groups   = "drop"
    )

  # --- filter extreme MAP values (ERA5 unit-conversion artifacts) -------------
  map_limit <- 8000
  n_extreme  <- sum(site_clim$MAP > map_limit, na.rm = TRUE)
  if (n_extreme > 0L) {
    extreme_ids <- site_clim$site_id[site_clim$MAP > map_limit]
    message(
      n_extreme, " site(s) with mean annual P_ERA > ", map_limit,
      " mm yr\u207b\u00b9 excluded (likely ERA5 unit-conversion artifacts):\n  ",
      paste(sort(extreme_ids), collapse = ", ")
    )
    site_clim <- dplyr::filter(site_clim, .data$MAP <= map_limit)
  }

  if (nrow(site_clim) == 0L) {
    warning("No sites with valid ERA5 climate and flux data after filtering.",
            call. = FALSE)
    return(ggplot2::ggplot() + ggplot2::labs(title = "No data") + fluxnet_theme())
  }

  # --- symmetric colour limits: 5th–95th percentile, zero-centred ------------
  nee_lims <- quantile(site_clim$mean_flux, probs = c(0.05, 0.95), na.rm = TRUE)
  nee_max  <- max(abs(nee_lims))

  # --- labels -----------------------------------------------------------------
  flux_label <- .flux_climate_label(flux_var)
  n_sites    <- nrow(site_clim)

  # --- plot -------------------------------------------------------------------
  p <- ggplot2::ggplot(
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
      limits  = c(-nee_max, nee_max),
      oob     = scales::squish,
      name    = flux_label,
      guide   = ggplot2::guide_colorbar(
        title.position = "top",
        barwidth       = 15,
        barheight      = 0.8,
        direction      = "horizontal"
      )
    ) +
    # Individual site locations
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
      subtitle = paste0("n\u2009=\u2009", n_sites, " sites",
                        " \u2014 Climate from ERA5 (site-extracted)")
    ) +
    fluxnet_theme() +
    ggplot2::theme(legend.position = "bottom")

  # --- year annotation (inset top-left) and N/site-years (top-right) ----------
  if (!is.null(year_cutoff)) {
    # Compute site-years from metadata when available
    n_site_years <- NA_integer_
    if (!is.null(metadata) &&
        all(c("site_id", "first_year", "last_year") %in% names(metadata))) {
      meta_q <- metadata |>
        dplyr::filter(
          !is.na(.data$first_year), !is.na(.data$last_year),
          as.integer(.data$first_year) <= year_cutoff
        )
      n_meta       <- nrow(meta_q)
      n_site_years <- as.integer(sum(
        pmin(as.integer(meta_q$last_year), year_cutoff) -
          as.integer(meta_q$first_year) + 1L,
        na.rm = TRUE
      ))
    } else {
      n_meta <- n_sites
    }

    p <- p +
      ggplot2::annotate(
        "text", x = -Inf, y = Inf,
        label = as.character(year_cutoff),
        hjust = -0.1, vjust = 1.5,
        size  = 4, fontface = "bold"
      ) +
      ggplot2::annotate(
        "text", x = Inf, y = Inf,
        label = if (!is.na(n_site_years))
          paste0("n = ", n_meta, "\nsite-years = ", n_site_years)
        else
          paste0("n = ", n_meta),
        hjust = 1.1, vjust = 1.5,
        size  = 3.5
      )
  }

  p
}


#' Hexbin of temperature × precipitation coloured by flux (WorldClim climate)
#'
#' Plots tower sites on a temperature × precipitation scatter using hexagonal
#' binning, with each hex coloured by the median site-mean of \code{flux_var}.
#' Climate normals are extracted from a WorldClim \pkg{terra} SpatRaster
#' supplied via \code{worldclim_data}.  Layers bio1 (Annual Mean Temperature,
#' °C or °C × 10) and bio12 (Annual Precipitation, mm) are required.
#'
#' This version requires WorldClim raster data and the \pkg{terra} package.
#' **Designed for local Mac execution where WorldClim is available — do not run
#' in the Codespace.**  For a Codespace-compatible alternative, see
#' \code{\link{fig_whittaker_hexbin_era5}}.
#'
#' @param data_yy Annual FLUXNET data frame. Must contain \code{site_id} and
#'   \code{flux_var}. Columns \code{location_lat} and \code{location_long} are
#'   joined from \code{metadata} when absent.
#' @param worldclim_data A \pkg{terra} SpatRaster containing WorldClim bio-variable
#'   layers, including bio1 (MAT) and bio12 (MAP).  Pass the result of
#'   \code{terra::rast()} here.  Function stops with an informative message when
#'   \code{NULL}.
#' @param metadata Optional data frame with \code{site_id}, \code{location_lat},
#'   \code{location_long}, \code{first_year}, and \code{last_year} columns.
#'   Required when \code{year_cutoff} is set or coordinates are absent from
#'   \code{data_yy}.
#' @param flux_var Character. Flux variable to summarise per hex
#'   (default \code{"NEE_VUT_REF"}).
#' @param year_cutoff Integer or \code{NULL}. When set, only sites with
#'   \code{first_year <= year_cutoff} are included (i.e. sites established by
#'   that year), and flux records are restricted to years
#'   \code{<= year_cutoff}.  \code{last_year} is not used — this shows the
#'   network as it had grown by each cutoff, regardless of whether recent data
#'   has been submitted yet.  Requires \code{first_year} in \code{data_yy} or
#'   joinable from \code{metadata}.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' wc <- terra::rast("data/wc_worldclim_30s.rds")
#' fig_whittaker_hexbin_worldclim(data_yy, worldclim_data = wc,
#'                                metadata = snapshot_meta, year_cutoff = 2020)
#' }
#'
#' @export
fig_whittaker_hexbin_worldclim <- function(data_yy,
                                           worldclim_data = NULL,
                                           metadata       = NULL,
                                           flux_var       = "NEE_VUT_REF",
                                           year_cutoff    = NULL) {

  # --- WorldClim data check ---------------------------------------------------
  if (is.null(worldclim_data)) {
    stop(
      "worldclim_data is NULL. Supply a terra SpatRaster with WorldClim bio ",
      "layers (bio1 = MAT, bio12 = MAP).\n",
      "  Example: terra::rast('data/wc_worldclim_30s.rds')\n",
      "  See R/external_data.R for download instructions.\n",
      "  For a Codespace-compatible Whittaker figure, use ",
      "fig_whittaker_hexbin_era5() instead.",
      call. = FALSE
    )
  }

  if (!requireNamespace("terra", quietly = TRUE)) {
    stop(
      "Package 'terra' is required for WorldClim raster extraction. ",
      "Install with: install.packages('terra')",
      call. = FALSE
    )
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
    need              <- c("location_lat", "location_long",
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
  # Filter: sites established by year_cutoff (first_year <= year_cutoff) AND
  # flux records up to year_cutoff.  last_year is intentionally not used — we
  # show all sites that had started by the cutoff year, regardless of whether
  # recent data has been submitted yet.
  if (!is.null(year_cutoff)) {
    year_cutoff <- as.integer(year_cutoff)

    if (!"first_year" %in% names(data_yy)) {
      stop(
        "year_cutoff requires a 'first_year' column. ",
        "Supply a metadata argument that contains this column.",
        call. = FALSE
      )
    }

    # (1) Keep sites established by year_cutoff
    data_yy <- dplyr::filter(
      data_yy,
      as.integer(.data$first_year) <= year_cutoff
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
    dplyr::group_by(.data$site_id) |>
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
  wc_vals <- as.data.frame(terra::extract(worldclim_data, pts, ID = FALSE))

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

  # --- symmetric colour limits: 5th–95th percentile, zero-centred ------------
  nee_lims <- quantile(site_clim$mean_flux, probs = c(0.05, 0.95), na.rm = TRUE)
  nee_max  <- max(abs(nee_lims))

  # --- labels -----------------------------------------------------------------
  flux_label <- .flux_climate_label(flux_var)
  n_sites    <- nrow(site_clim)

  # --- plot -------------------------------------------------------------------
  p <- ggplot2::ggplot(
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
      limits  = c(-nee_max, nee_max),
      oob     = scales::squish,
      name    = flux_label,
      guide   = ggplot2::guide_colorbar(
        title.position = "top",
        barwidth       = 15,
        barheight      = 0.8,
        direction      = "horizontal",
        title.theme    = ggtext::element_markdown()
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
      subtitle = paste0("n\u2009=\u2009", n_sites, " sites",
                        " \u2014 Climate from WorldClim 2.1")
    ) +
    fluxnet_theme()

  # --- year annotation (inset top-left) and N/site-years (top-right) ----------
  if (!is.null(year_cutoff)) {
    n_site_years <- NA_integer_
    n_meta       <- n_sites
    if (!is.null(metadata) &&
        all(c("site_id", "first_year", "last_year") %in% names(metadata))) {
      meta_q <- metadata |>
        dplyr::filter(
          !is.na(.data$first_year), !is.na(.data$last_year),
          as.integer(.data$first_year) <= year_cutoff
        )
      n_meta       <- nrow(meta_q)
      n_site_years <- as.integer(sum(
        pmin(as.integer(meta_q$last_year), year_cutoff) -
          as.integer(meta_q$first_year) + 1L,
        na.rm = TRUE
      ))
    }

    p <- p +
      ggplot2::annotate(
        "text", x = -Inf, y = Inf,
        label = as.character(year_cutoff),
        hjust = -0.1, vjust = 1.5,
        size  = 4, fontface = "bold"
      ) +
      ggplot2::annotate(
        "text", x = Inf, y = Inf,
        label = if (!is.na(n_site_years))
          paste0("n = ", n_meta, "\nsite-years = ", n_site_years)
        else
          paste0("n = ", n_meta),
        hjust = 1.1, vjust = 1.5,
        size  = 3.5
      )
  }

  p
}


#' Whittaker biome hexbin — wrapper selecting ERA5 or WorldClim climate source
#'
#' Convenience wrapper around \code{\link{fig_whittaker_hexbin_era5}} and
#' \code{\link{fig_whittaker_hexbin_worldclim}}.  Defaults to the ERA5 version
#' because it requires no external data and runs in the Codespace.
#'
#' @param data_yy Annual FLUXNET data frame.
#' @param flux_var Character. Flux variable to summarise per hex
#'   (default \code{"NEE_VUT_REF"}).
#' @param year_cutoff Integer or \code{NULL}. Passed to the selected function.
#' @param source Character. \code{"era5"} (default) or \code{"worldclim"}.
#' @param worldclim_data A \pkg{terra} SpatRaster. Required when
#'   \code{source = "worldclim"}; ignored for ERA5.
#' @param metadata Optional data frame with site coordinates / first/last year.
#'   Passed to \code{fig_whittaker_hexbin_worldclim()} when
#'   \code{source = "worldclim"}.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' # Codespace-safe ERA5 version (default)
#' fig_whittaker_hexbin(data_yy)
#' fig_whittaker_hexbin(data_yy, source = "era5", year_cutoff = 2020)
#'
#' # WorldClim version (local Mac only)
#' wc <- terra::rast("data/wc_worldclim_30s.rds")
#' fig_whittaker_hexbin(data_yy, source = "worldclim",
#'                      worldclim_data = wc, metadata = snapshot_meta)
#' }
#'
#' @export
fig_whittaker_hexbin <- function(data_yy,
                                 flux_var       = "NEE_VUT_REF",
                                 year_cutoff    = NULL,
                                 source         = "era5",
                                 worldclim_data = NULL,
                                 metadata       = NULL) {

  source <- match.arg(source, c("era5", "worldclim"))

  if (source == "era5") {
    fig_whittaker_hexbin_era5(
      data_yy     = data_yy,
      flux_var    = flux_var,
      year_cutoff = year_cutoff,
      metadata    = metadata
    )
  } else {
    fig_whittaker_hexbin_worldclim(
      data_yy        = data_yy,
      worldclim_data = worldclim_data,
      metadata       = metadata,
      flux_var       = flux_var,
      year_cutoff    = year_cutoff
    )
  }
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
