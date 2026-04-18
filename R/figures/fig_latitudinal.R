# R/figures/fig_latitudinal.R
# Latitudinal ribbon plot for the FLUXNET Annual Paper 2026.
#
# Ported from legacy/fcn_plot_FLUXNET.R (plot_latitudinal_flux).
# Reference only — do not edit legacy/ originals.
#
# Functions:
#   fig_latitudinal_flux()  — ribbon of flux range by latitude band + site points
#   fig_latitudinal_multi() — multi-variable wrapper with vertical patchwork

library(ggplot2)
library(ggtext)
library(dplyr)

# ---- Internal helpers -------------------------------------------------------

#' Map flux variable name to axis label (latitudinal context)
#'
#' @param flux_var Character. Variable name.
#' @return Character label string (HTML safe for element_markdown).
#' @noRd
.flux_lat_label <- function(flux_var) {
  switch(
    sub("_.*", "", flux_var),
    NEE  = lab_nee_annual,
    GPP  = lab_gpp_annual,
    RECO = lab_reco_annual,
    LE   = "Latent Heat Flux (W m<sup>-2</sup>)",
    H    = "Sensible Heat Flux (W m<sup>-2</sup>)",
    flux_var
  )
}

# ---- Exported function ------------------------------------------------------

#' Latitudinal ribbon plot of flux range
#'
#' Computes per-site mean, min, and max of `flux_var` over all available years,
#' then bins sites into latitude bands.  Produces a `coord_flip()` ribbon of
#' the min–max range per band with individual site-mean points shaped by IGBP.
#'
#' Latitude is taken from the `location_lat` column of `metadata` (lowercase
#' snake_case, as delivered by the FLUXNET Shuttle snapshot CSV).
#'
#' @param data_yy Annual FLUXNET data frame. Must contain `site_id` and
#'   `flux_var`.
#' @param metadata Site metadata data frame. Must contain `site_id` and
#'   `location_lat`.  `igbp` is used for point shapes when present.
#' @param flux_var Character. Flux variable to summarise (default
#'   `"NEE_VUT_REF"`).
#' @param bin_width Numeric. Latitude band width in degrees (default `5`).
#'
#' @return A single ggplot object with latitude on the y-axis (via
#'   `coord_flip()`) and flux on the x-axis.
#'
#' @examples
#' \dontrun{
#' p <- fig_latitudinal_flux(data_yy, metadata = snapshot_meta,
#'                           flux_var = "GPP_NT_VUT_REF")
#' print(p)
#' }
#'
#' @export
fig_latitudinal_flux <- function(data_yy,
                                 metadata,
                                 flux_var  = "NEE_VUT_REF",
                                 bin_width = 5) {

  # --- column checks ----------------------------------------------------------
  if (!flux_var %in% names(data_yy)) {
    stop("data_yy is missing required column: ", flux_var, call. = FALSE)
  }
  if (!"site_id" %in% names(data_yy)) {
    stop("data_yy must contain column 'site_id'.", call. = FALSE)
  }
  if (!all(c("site_id", "location_lat") %in% names(metadata))) {
    stop("metadata must contain columns 'site_id' and 'location_lat'.",
         call. = FALSE)
  }

  # --- filter GPP to non-negative (matching legacy behaviour) ----------------
  if (grepl("^GPP", flux_var)) {
    data_yy <- dplyr::filter(data_yy, .data[[flux_var]] >= 0)
  }

  # --- per-site summary -------------------------------------------------------
  site_summary <- data_yy |>
    dplyr::filter(!is.na(.data[[flux_var]])) |>
    dplyr::group_by(site_id) |>
    dplyr::summarise(
      min_flux  = min(.data[[flux_var]], na.rm = TRUE),
      max_flux  = max(.data[[flux_var]], na.rm = TRUE),
      mean_flux = mean(.data[[flux_var]], na.rm = TRUE),
      .groups   = "drop"
    )

  # --- join latitude (and optionally IGBP) from metadata ---------------------
  meta_cols <- intersect(c("site_id", "location_lat", "igbp"), names(metadata))
  site_summary <- dplyr::left_join(
    site_summary,
    dplyr::select(metadata, dplyr::all_of(meta_cols)),
    by = "site_id"
  )

  igbp_present <- "igbp" %in% names(site_summary) &&
    any(!is.na(site_summary$igbp))

  if (igbp_present) {
    igbp_lvls <- intersect(IGBP_order, unique(na.omit(site_summary$igbp)))
    shp       <- stats::setNames(shape_igbp[seq_along(igbp_lvls)], igbp_lvls)
    site_summary <- dplyr::mutate(
      site_summary,
      IGBP = factor(igbp, levels = IGBP_order)
    )
  } else {
    warning("IGBP not found in metadata — all site points will share one shape.",
            call. = FALSE)
  }

  # Drop rows with no latitude
  site_summary <- dplyr::filter(site_summary, !is.na(location_lat))

  if (nrow(site_summary) == 0L) {
    warning("No sites with valid latitude — returning empty plot.", call. = FALSE)
    return(
      ggplot2::ggplot() +
        ggplot2::labs(title = "No data") +
        fluxnet_theme()
    )
  }

  # --- latitude bins ----------------------------------------------------------
  breaks_vec <- seq(-90, 90, by = bin_width)

  binned <- site_summary |>
    dplyr::mutate(
      lat_bin  = cut(location_lat, breaks = breaks_vec,
                     include.lowest = TRUE, right = FALSE),
      lat_idx  = as.integer(lat_bin),
      lat_mid  = breaks_vec[lat_idx] + bin_width / 2
    ) |>
    dplyr::filter(!is.na(lat_bin)) |>
    dplyr::group_by(lat_mid) |>
    dplyr::summarise(
      min_flux = min(min_flux, na.rm = TRUE),
      max_flux = max(max_flux, na.rm = TRUE),
      .groups  = "drop"
    )

  # --- axis label -------------------------------------------------------------
  y_label <- .flux_lat_label(flux_var)
  title_text <- paste0("Flux range in ", bin_width, "\u00b0 lat bands")

  # --- plot -------------------------------------------------------------------
  p <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = binned,
      ggplot2::aes(x = lat_mid, ymin = min_flux, ymax = max_flux),
      fill  = "steelblue",
      alpha = 0.40
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x     = "Latitude (\u00b0)",
      y     = y_label,
      title = title_text,
      shape = "IGBP"
    ) +
    fluxnet_theme() +
    ggplot2::theme(
      axis.title.y = ggtext::element_markdown(),
      legend.position = "right"
    )

  if (igbp_present) {
    p <- p +
      ggplot2::geom_point(
        data = site_summary,
        ggplot2::aes(x = location_lat, y = mean_flux, color = IGBP, shape = IGBP),
        size  = 2.8,
        stroke = 0.7,
        inherit.aes = FALSE
      ) +
      scale_color_igbp() +
      ggplot2::scale_shape_manual(values = shp)
  } else {
    p <- p +
      ggplot2::geom_point(
        data = site_summary,
        ggplot2::aes(x = location_lat, y = mean_flux),
        size  = 2.8,
        color = "black",
        stroke = 0.7,
        inherit.aes = FALSE
      )
  }

  p
}

#' Multi-variable latitudinal ribbon wrapper
#'
#' Calls [fig_latitudinal_flux()] for each variable in `flux_vars` and
#' assembles the results into a vertical patchwork with a shared latitude axis.
#' Panel titles use axis labels from `R/plot_constants.R` where available,
#' falling back to the variable name for variables not covered there (e.g.
#' `LE_F_MDS`, `H_F_MDS`).  Variables absent from `data_yy` are skipped with
#' a warning rather than stopping.
#'
#' @param data_yy Annual FLUXNET data frame passed to [fig_latitudinal_flux()].
#' @param metadata Site metadata data frame passed to [fig_latitudinal_flux()].
#'   Must contain `site_id` and `location_lat`; `igbp` is used for point
#'   shapes when present.
#' @param flux_vars Character vector. Flux variable names to plot, in panel
#'   order (default: `c("NEE_VUT_REF", "LE_F_MDS", "H_F_MDS")`). Variables
#'   absent from `data_yy` are skipped with a warning.
#' @param bin_width Numeric. Latitude band width in degrees, passed to
#'   [fig_latitudinal_flux()] (default `5`).
#'
#' @return A single patchwork object with one panel per available variable.
#'
#' @examples
#' \dontrun{
#' p <- fig_latitudinal_multi(data_yy, metadata = snapshot_meta)
#' print(p)
#' }
#'
#' @export
fig_latitudinal_multi <- function(data_yy,
                                   metadata,
                                   flux_vars = c("NEE_VUT_REF",
                                                  "LE_F_MDS",
                                                  "H_F_MDS"),
                                   bin_width = 5) {

  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop(
      "Package 'patchwork' is required. ",
      "Install with: install.packages('patchwork')",
      call. = FALSE
    )
  }

  avail_vars <- intersect(flux_vars, names(data_yy))
  skipped    <- setdiff(flux_vars, names(data_yy))

  if (length(skipped) > 0L) {
    warning(
      "Variable(s) not found in data_yy \u2014 skipped: ",
      paste(skipped, collapse = ", "),
      call. = FALSE
    )
  }
  if (length(avail_vars) == 0L) {
    stop("None of the requested flux_vars are present in data_yy.", call. = FALSE)
  }

  panels <- lapply(avail_vars, function(var) {
    p <- fig_latitudinal_flux(
      data_yy   = data_yy,
      metadata  = metadata,
      flux_var  = var,
      bin_width = bin_width
    )
    # Remove panel title; y-axis label (from .flux_lat_label) carries the units.
    p + ggplot2::labs(title = NULL)
  })

  patchwork::wrap_plots(panels, ncol = 1) +
    patchwork::plot_layout(guides = "collect") &
    ggplot2::theme(legend.position = "right")
}
