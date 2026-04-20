# R/figures/fig_climate_legacy.R
#
# LEGACY — these functions are deprecated as of 2026-04-20.
# Use fig_whittaker_worldclim() in R/figures/fig_climate.R instead.
#
# Retained for reference only. Do not call these in new code.

library(ggplot2)
library(ggtext)
library(dplyr)
library(colorspace)

#' @rdname fig_whittaker_hexbin_era5-deprecated
#' @title Deprecated: Whittaker hexbin using ERA5 climate
#' @description Use \code{\link{fig_whittaker_worldclim}} instead.
#' @keywords internal
fig_whittaker_hexbin_era5 <- function(data_yy,
                                      flux_var    = "NEE_VUT_REF",
                                      year_cutoff = NULL,
                                      metadata    = NULL) {

  .Deprecated("fig_whittaker_worldclim",
              msg = paste0(
                "fig_whittaker_hexbin_era5() is deprecated as of 2026-04-20.\n",
                "Use fig_whittaker_worldclim() in R/figures/fig_climate.R instead."
              ))

  if (!requireNamespace("hexbin", quietly = TRUE)) {
    stop(
      "Package 'hexbin' is required for hexagonal binning. ",
      "Install with: install.packages('hexbin')",
      call. = FALSE
    )
  }

  .check_cols_climate(data_yy, c("site_id", "TA_ERA", "P_ERA", flux_var))

  if (!is.null(year_cutoff)) {
    year_cutoff <- as.integer(year_cutoff)
    if (!"YEAR" %in% names(data_yy)) {
      stop("year_cutoff requires a 'YEAR' column in data_yy.", call. = FALSE)
    }
    if ("first_year" %in% names(data_yy)) {
      data_yy <- dplyr::filter(data_yy,
                                as.integer(.data$first_year) <= year_cutoff)
    }
    data_yy <- dplyr::filter(data_yy, as.integer(.data$YEAR) <= year_cutoff)
  }

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

  map_limit <- 8000
  n_extreme <- sum(site_clim$MAP > map_limit, na.rm = TRUE)
  if (n_extreme > 0L) {
    extreme_ids <- site_clim$site_id[site_clim$MAP > map_limit]
    message(n_extreme, " site(s) with mean annual P_ERA > ", map_limit,
            " mm yr\u207b\u00b9 excluded (likely ERA5 unit-conversion artifacts):\n  ",
            paste(sort(extreme_ids), collapse = ", "))
    site_clim <- dplyr::filter(site_clim, .data$MAP <= map_limit)
  }

  if (nrow(site_clim) == 0L) {
    warning("No sites with valid ERA5 climate and flux data after filtering.",
            call. = FALSE)
    return(ggplot2::ggplot() + ggplot2::labs(title = "No data") + fluxnet_theme())
  }

  nee_lims   <- quantile(site_clim$mean_flux, probs = c(0.05, 0.95), na.rm = TRUE)
  nee_max    <- max(abs(nee_lims))
  flux_label <- .flux_climate_label(flux_var)
  n_sites    <- nrow(site_clim)

  p <- ggplot2::ggplot(
    site_clim,
    ggplot2::aes(x = .data$MAT, y = .data$MAP, z = .data$mean_flux)
  ) +
    ggplot2::stat_summary_hex(fun = median, bins = 15, alpha = 0.85) +
    colorspace::scale_fill_continuous_diverging(
      palette = "Blue-Red 3", mid = 0,
      limits  = c(-nee_max, nee_max), oob = scales::squish,
      name    = flux_label,
      guide   = ggplot2::guide_colorbar(
        title.position = "top", barwidth = 15, barheight = 0.8,
        direction = "horizontal"
      )
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = .data$MAT, y = .data$MAP),
      size = 1.6, colour = "grey30", alpha = 0.55, inherit.aes = FALSE
    ) +
    ggplot2::labs(
      x        = "Mean annual temperature (\u00b0C)",
      y        = "Mean annual precipitation (mm yr\u207b\u00b9)",
      subtitle = paste0("n\u2009=\u2009", n_sites,
                        " sites \u2014 Climate from ERA5 (site-extracted)")
    ) +
    fluxnet_theme() +
    ggplot2::theme(legend.position = "bottom")

  if (!is.null(year_cutoff)) {
    n_site_years <- NA_integer_
    if (!is.null(metadata) &&
        all(c("site_id", "first_year", "last_year") %in% names(metadata))) {
      meta_q <- metadata |>
        dplyr::filter(!is.na(.data$first_year), !is.na(.data$last_year),
                      as.integer(.data$first_year) <= year_cutoff)
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
      ggplot2::annotate("text", x = -Inf, y = Inf,
                        label = as.character(year_cutoff),
                        hjust = -0.1, vjust = 1.5, size = 4, fontface = "bold") +
      ggplot2::annotate("text", x = Inf, y = Inf,
                        label = if (!is.na(n_site_years))
                          paste0("n = ", n_meta, "\nsite-years = ", n_site_years)
                        else paste0("n = ", n_meta),
                        hjust = 1.1, vjust = 1.5, size = 3.5)
  }
  p
}


#' @rdname fig_whittaker_hexbin_worldclim-deprecated
#' @title Deprecated: Whittaker hexbin using WorldClim SpatRaster
#' @description Use \code{\link{fig_whittaker_worldclim}} instead.
#' @keywords internal
fig_whittaker_hexbin_worldclim <- function(data_yy,
                                           worldclim_data = NULL,
                                           metadata       = NULL,
                                           flux_var       = "NEE_VUT_REF",
                                           year_cutoff    = NULL) {

  .Deprecated("fig_whittaker_worldclim",
              msg = paste0(
                "fig_whittaker_hexbin_worldclim() is deprecated as of 2026-04-20.\n",
                "Use fig_whittaker_worldclim() in R/figures/fig_climate.R instead."
              ))

  if (is.null(worldclim_data)) {
    stop(
      "worldclim_data is NULL. Supply a terra SpatRaster with WorldClim bio ",
      "layers (bio1 = MAT, bio12 = MAP).\n",
      "  For the new interface, use fig_whittaker_worldclim() instead.",
      call. = FALSE
    )
  }
  for (pkg in c("terra", "hexbin")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required.", call. = FALSE)
    }
  }
  .check_cols_climate(data_yy, c("site_id", flux_var))

  if (!is.null(metadata) && "site_id" %in% names(metadata)) {
    need              <- c("location_lat", "location_long",
                           "first_year", "last_year", "igbp")
    missing_from_data <- setdiff(intersect(need, names(metadata)), names(data_yy))
    if (length(missing_from_data) > 0L) {
      data_yy <- dplyr::left_join(
        data_yy,
        dplyr::select(metadata, "site_id", dplyr::all_of(missing_from_data)),
        by = "site_id"
      )
    }
  }

  if (!is.null(year_cutoff)) {
    year_cutoff <- as.integer(year_cutoff)
    if (!"first_year" %in% names(data_yy)) {
      stop("year_cutoff requires a 'first_year' column.", call. = FALSE)
    }
    data_yy <- dplyr::filter(data_yy,
                              as.integer(.data$first_year) <= year_cutoff)
    if ("YEAR" %in% names(data_yy)) {
      data_yy <- dplyr::filter(data_yy, as.integer(.data$YEAR) <= year_cutoff)
    } else if ("TIMESTAMP" %in% names(data_yy)) {
      data_yy <- dplyr::filter(
        data_yy,
        as.integer(substr(as.character(.data$TIMESTAMP), 1L, 4L)) <= year_cutoff
      )
    }
  }

  if (!all(c("location_lat", "location_long") %in% names(data_yy))) {
    stop("Columns 'location_lat' and 'location_long' are required.", call. = FALSE)
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

  pts     <- terra::vect(
    data.frame(x = site_summary$location_long, y = site_summary$location_lat),
    geom = c("x", "y"), crs = "EPSG:4326"
  )
  wc_vals <- as.data.frame(terra::extract(worldclim_data, pts, ID = FALSE))

  bio1_col <- grep("bio[_.]?0?1([^0-9]|$)", names(wc_vals),
                   value = TRUE, ignore.case = TRUE, perl = TRUE)[1]
  bio12_col <- grep("bio[_.]?12([^0-9]|$)", names(wc_vals),
                    value = TRUE, ignore.case = TRUE, perl = TRUE)[1]

  if (is.na(bio1_col) || is.na(bio12_col)) {
    stop("Cannot identify bio1 (MAT) and/or bio12 (MAP) in WorldClim layers.\n",
         "  Layers present: ", paste(head(names(wc_vals), 25), collapse = ", "),
         call. = FALSE)
  }

  mat_raw  <- wc_vals[[bio1_col]]
  mat_vals <- if (max(abs(mat_raw), na.rm = TRUE) > 70) mat_raw / 10 else mat_raw

  site_clim <- site_summary |>
    dplyr::mutate(MAT = mat_vals, MAP = wc_vals[[bio12_col]]) |>
    dplyr::filter(!is.na(.data$MAT), !is.na(.data$MAP))

  if (nrow(site_clim) == 0L) {
    warning("No valid WorldClim values extracted. Returning empty plot.",
            call. = FALSE)
    return(ggplot2::ggplot() + ggplot2::labs(title = "No data") + fluxnet_theme())
  }

  nee_lims   <- quantile(site_clim$mean_flux, probs = c(0.05, 0.95), na.rm = TRUE)
  nee_max    <- max(abs(nee_lims))
  flux_label <- .flux_climate_label(flux_var)
  n_sites    <- nrow(site_clim)

  p <- ggplot2::ggplot(
    site_clim,
    ggplot2::aes(x = .data$MAT, y = .data$MAP, z = .data$mean_flux)
  ) +
    ggplot2::stat_summary_hex(fun = median, bins = 15, alpha = 0.85) +
    colorspace::scale_fill_continuous_diverging(
      palette = "Blue-Red 3", mid = 0,
      limits  = c(-nee_max, nee_max), oob = scales::squish,
      name    = flux_label,
      guide   = ggplot2::guide_colorbar(
        title.position = "top", barwidth = 15, barheight = 0.8,
        direction = "horizontal",
        title.theme = ggtext::element_markdown()
      )
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = .data$MAT, y = .data$MAP),
      size = 1.6, colour = "grey30", alpha = 0.55, inherit.aes = FALSE
    ) +
    ggplot2::labs(
      x        = "Mean annual temperature (\u00b0C)",
      y        = "Mean annual precipitation (mm yr\u207b\u00b9)",
      subtitle = paste0("n\u2009=\u2009", n_sites,
                        " sites \u2014 Climate from WorldClim 2.1")
    ) +
    fluxnet_theme()

  if (!is.null(year_cutoff)) {
    n_site_years <- NA_integer_
    n_meta       <- n_sites
    if (!is.null(metadata) &&
        all(c("site_id", "first_year", "last_year") %in% names(metadata))) {
      meta_q <- metadata |>
        dplyr::filter(!is.na(.data$first_year), !is.na(.data$last_year),
                      as.integer(.data$first_year) <= year_cutoff)
      n_meta       <- nrow(meta_q)
      n_site_years <- as.integer(sum(
        pmin(as.integer(meta_q$last_year), year_cutoff) -
          as.integer(meta_q$first_year) + 1L,
        na.rm = TRUE
      ))
    }
    p <- p +
      ggplot2::annotate("text", x = -Inf, y = Inf,
                        label = as.character(year_cutoff),
                        hjust = -0.1, vjust = 1.5, size = 4, fontface = "bold") +
      ggplot2::annotate("text", x = Inf, y = Inf,
                        label = if (!is.na(n_site_years))
                          paste0("n = ", n_meta, "\nsite-years = ", n_site_years)
                        else paste0("n = ", n_meta),
                        hjust = 1.1, vjust = 1.5, size = 3.5)
  }
  p
}


#' @rdname fig_whittaker_hexbin-deprecated
#' @title Deprecated: Whittaker hexbin wrapper (ERA5 or WorldClim)
#' @description Use \code{\link{fig_whittaker_worldclim}} instead.
#' @keywords internal
fig_whittaker_hexbin <- function(data_yy,
                                 flux_var       = "NEE_VUT_REF",
                                 year_cutoff    = NULL,
                                 source         = "era5",
                                 worldclim_data = NULL,
                                 metadata       = NULL) {

  .Deprecated("fig_whittaker_worldclim",
              msg = paste0(
                "fig_whittaker_hexbin() is deprecated as of 2026-04-20.\n",
                "Use fig_whittaker_worldclim() in R/figures/fig_climate.R instead."
              ))

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
