# R/figures/fig_igbp.R
# IGBP-stratified annual flux figures for the FLUXNET Annual Paper 2026.
#
# Ported from:
#   legacy/fcn_plot_FLUXNET.R  — plot_flux_by_igbp(), plot_flux_by_igbp_timeslice_grouped(),
#                                 plot_flux_box_by_group(), plot_flux_timeseries_by_igbp()
#   legacy/AMFOct25_poster.R   — style conventions
#
# All functions accept pre-converted annual data (flux_data_converted_yy.rds)
# and an optional metadata data frame for IGBP / coordinate joins.
# No data loading occurs inside these functions.

source("R/plot_constants.R")

# ---- Internal helpers ------------------------------------------------------

# Map a flux variable name to its axis label (ggtext HTML superscripts).
# Labels come from plot_constants.R and reflect post-conversion units.
.flux_y_label <- function(flux_var) {
  if (startsWith(flux_var, "NEE"))  return(lab_nee_annual)
  if (startsWith(flux_var, "GPP"))  return(lab_gpp_annual)
  if (startsWith(flux_var, "RECO")) return(lab_reco_annual)
  if (startsWith(flux_var, "LE"))   return("LE (mm H<sub>2</sub>O yr<sup>-1</sup>)")
  flux_var
}

# Join IGBP (and optionally lat/lon) from metadata onto data if the column is
# absent. Metadata must contain site_id and IGBP. Warns if IGBP is still
# missing after the join.
.ensure_igbp <- function(data, metadata = NULL) {
  if (!"IGBP" %in% names(data)) {
    if (is.null(metadata)) {
      warning(
        "IGBP column not found in data and no metadata supplied. ",
        "Pass a metadata data frame with columns site_id and IGBP.",
        call. = FALSE
      )
      data$IGBP <- NA_character_
    } else {
      meta_cols <- intersect(c("site_id", "IGBP", "LOCATION_LAT", "LOCATION_LONG"),
                             names(metadata))
      data <- dplyr::left_join(data, metadata[, meta_cols, drop = FALSE],
                               by = "site_id")
    }
  }
  if (all(is.na(data$IGBP))) {
    warning("All IGBP values are NA — figures will be empty.", call. = FALSE)
  }
  data
}

# Validate that flux_var exists in the data frame.
.check_flux_var <- function(data, flux_var) {
  if (!flux_var %in% names(data)) {
    stop(sprintf("flux_var '%s' not found in data. Available: %s",
                 flux_var, paste(names(data), collapse = ", ")),
         call. = FALSE)
  }
}

# ---- fig_flux_by_igbp ------------------------------------------------------

#' IGBP flux distribution: boxplot + jitter composite
#'
#' Produces a three-panel composite (flux boxplot / median bar / site-year
#' count) assembled with patchwork. Adapted from `plot_flux_by_igbp()` in
#' `legacy/fcn_plot_FLUXNET.R`.
#'
#' @param data_yy Data frame. Converted annual flux data
#'   (`flux_data_converted_yy.rds`) with column `site_id`, `TIMESTAMP`, and
#'   the variable named by `flux_var`. May already contain `IGBP`; if not,
#'   supply `metadata`.
#' @param flux_var Character. Name of the flux variable to plot
#'   (default `"NEE_VUT_REF"`).
#' @param metadata Optional data frame with columns `site_id` and `IGBP` used
#'   to join vegetation class information when absent from `data_yy`.
#' @param igbp_subset Optional character vector of IGBP codes to include. If
#'   `NULL` (default), uses [IGBP_order] from `R/plot_constants.R`.
#'
#' @return A named list with elements:
#'   \describe{
#'     \item{flux_plot}{Boxplot + jitter ggplot.}
#'     \item{median_plot}{Median bar ggplot.}
#'     \item{count_plot}{Site-year count bar ggplot.}
#'     \item{composite}{Patchwork composite of all three panels.}
#'     \item{summary_table}{Data frame of per-IGBP summaries.}
#'   }
#'
#' @examples
#' \dontrun{
#' data_yy <- readRDS("data/processed/flux_data_converted_yy.rds")
#' out <- fig_flux_by_igbp(data_yy, flux_var = "GPP_NT_VUT_REF")
#' print(out$composite)
#' }
fig_flux_by_igbp <- function(data_yy,
                              flux_var    = "NEE_VUT_REF",
                              metadata    = NULL,
                              igbp_subset = NULL) {
  .check_flux_var(data_yy, flux_var)
  data_yy <- .ensure_igbp(data_yy, metadata)

  order_use <- if (!is.null(igbp_subset)) igbp_subset else IGBP_order
  y_label   <- .flux_y_label(flux_var)

  plot_data <- data_yy |>
    dplyr::filter(IGBP %in% order_use) |>
    dplyr::mutate(
      IGBP = factor(IGBP, levels = order_use),
      FLUX = .data[[flux_var]]
    ) |>
    dplyr::filter(!is.na(FLUX))

  # Summary table
  summary_tbl <- plot_data |>
    dplyr::group_by(IGBP) |>
    dplyr::summarise(
      median_flux = stats::median(FLUX, na.rm = TRUE),
      n_siteyears = dplyr::n(),
      n_sites     = dplyr::n_distinct(site_id),
      p25_flux    = stats::quantile(FLUX, 0.25, na.rm = TRUE),
      p75_flux    = stats::quantile(FLUX, 0.75, na.rm = TRUE),
      .groups     = "drop"
    ) |>
    dplyr::mutate(IGBP = factor(as.character(IGBP), levels = order_use)) |>
    dplyr::arrange(IGBP)

  base_style <- fluxnet_theme(base_size = 14) +
    ggplot2::theme(
      axis.ticks        = ggplot2::element_line(color = "black", linewidth = 0.6),
      axis.ticks.length = grid::unit(-3, "pt")
    )

  p_flux <- ggplot2::ggplot(plot_data, ggplot2::aes(x = IGBP, y = FLUX)) +
    ggplot2::geom_jitter(width = 0.25, alpha = 0.35, size = 2.6,
                         color = "grey40", show.legend = FALSE) +
    ggplot2::geom_boxplot(color = "black", fill = NA, outlier.shape = NA,
                          linewidth = 0.9) +
    ggplot2::labs(x = NULL, y = y_label) +
    base_style +
    ggplot2::theme(
      axis.title.y    = ggtext::element_markdown(),
      axis.text.x     = ggplot2::element_blank(),
      axis.ticks.x    = ggplot2::element_blank(),
      legend.position = "none"
    ) +
    ggplot2::coord_cartesian(clip = "off")

  p_median <- ggplot2::ggplot(summary_tbl,
                               ggplot2::aes(x = IGBP, y = median_flux)) +
    ggplot2::geom_col(fill = "black", alpha = 0.6) +
    ggplot2::labs(x = NULL, y = "Median") +
    base_style +
    ggplot2::theme(
      axis.text.x  = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )

  p_count <- ggplot2::ggplot(summary_tbl,
                              ggplot2::aes(x = IGBP, y = n_siteyears)) +
    ggplot2::geom_col(fill = "gray40", alpha = 0.7) +
    ggplot2::labs(x = "IGBP", y = "Site-years") +
    base_style +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  composite <- p_flux / p_median / p_count +
    patchwork::plot_layout(heights = c(0.7, 0.2, 0.1))

  list(
    flux_plot     = p_flux,
    median_plot   = p_median,
    count_plot    = p_count,
    composite     = composite,
    summary_table = summary_tbl
  )
}

# ---- fig_flux_by_igbp_timeslice --------------------------------------------

#' IGBP flux distribution grouped by time bins
#'
#' Boxplot of annual fluxes stratified by IGBP class and binned into equal-
#' width time slices. Adapted from `plot_flux_by_igbp_timeslice_grouped()` in
#' `legacy/fcn_plot_FLUXNET.R`.
#'
#' @param data_yy Data frame. Converted annual flux data with `site_id`,
#'   `TIMESTAMP` (integer year), and the variable named by `flux_var`.
#' @param flux_var Character. Flux variable to plot (default `"NEE_VUT_REF"`).
#' @param bin_width Integer. Width of time bins in years (default `5`).
#' @param metadata Optional data frame with `site_id` and `IGBP`.
#'
#' @return A named list with elements `flux_plot`, `median_plot`, and
#'   `count_plot` (each a ggplot object).
#'
#' @examples
#' \dontrun{
#' data_yy <- readRDS("data/processed/flux_data_converted_yy.rds")
#' out <- fig_flux_by_igbp_timeslice(data_yy, flux_var = "NEE_VUT_REF")
#' print(out$flux_plot)
#' }
fig_flux_by_igbp_timeslice <- function(data_yy,
                                        flux_var  = "NEE_VUT_REF",
                                        bin_width = 5,
                                        metadata  = NULL) {
  .check_flux_var(data_yy, flux_var)
  data_yy <- .ensure_igbp(data_yy, metadata)

  y_label <- .flux_y_label(flux_var)

  # Build dynamic time-bin breaks from the data
  year_min <- min(as.integer(data_yy$TIMESTAMP), na.rm = TRUE)
  year_max <- max(as.integer(data_yy$TIMESTAMP), na.rm = TRUE)
  bin_start <- floor(year_min / bin_width) * bin_width
  bin_end   <- ceiling(year_max / bin_width) * bin_width
  breaks    <- seq(bin_start, bin_end, by = bin_width)

  labels <- paste0(head(breaks, -1) + 1, "\u2013", tail(breaks, -1))

  plot_data <- data_yy |>
    dplyr::filter(IGBP %in% IGBP_order) |>
    dplyr::mutate(
      FLUX      = .data[[flux_var]],
      year      = as.integer(TIMESTAMP),
      TimeSlice = cut(year, breaks = breaks, labels = labels, right = TRUE),
      IGBP      = factor(IGBP, levels = IGBP_order)
    ) |>
    dplyr::filter(!is.na(TimeSlice), !is.na(IGBP), !is.na(FLUX))

  base_style <- fluxnet_theme(base_size = 14) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  p_flux <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = IGBP, y = FLUX, fill = TimeSlice)
  ) +
    ggplot2::geom_boxplot(outlier.shape = NA,
                          position = ggplot2::position_dodge(width = 0.75)) +
    ggplot2::labs(x = "IGBP", y = y_label, fill = "Period") +
    base_style +
    ggplot2::theme(axis.title.y = ggtext::element_markdown())

  summary_data <- plot_data |>
    dplyr::group_by(IGBP, TimeSlice) |>
    dplyr::summarise(median_flux = stats::median(FLUX, na.rm = TRUE),
                     .groups = "drop")

  p_median <- ggplot2::ggplot(
    summary_data,
    ggplot2::aes(x = IGBP, y = median_flux, fill = TimeSlice)
  ) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75)) +
    ggplot2::labs(x = "IGBP", y = "Median", fill = "Period") +
    base_style

  site_counts <- plot_data |>
    dplyr::group_by(IGBP, TimeSlice) |>
    dplyr::summarise(n_siteyears = dplyr::n(), .groups = "drop")

  p_count <- ggplot2::ggplot(
    site_counts,
    ggplot2::aes(x = IGBP, y = n_siteyears, fill = TimeSlice)
  ) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75)) +
    ggplot2::labs(x = "IGBP", y = "Site-years", fill = "Period") +
    base_style

  list(flux_plot = p_flux, median_plot = p_median, count_plot = p_count)
}

# ---- fig_flux_by_biome_group -----------------------------------------------

#' Annual flux boxplots by broad biome group
#'
#' Returns a named list of ggplot boxplots — one per broad biome group
#' (Forest, Shrub/Opens, Grass/Crops/Wet, Other) — showing interannual
#' variation within each IGBP class. Adapted from `plot_flux_box_by_group()`
#' in `legacy/fcn_plot_FLUXNET.R`.
#'
#' @param data_yy Data frame. Converted annual flux data with `site_id`,
#'   `TIMESTAMP`, and the variable named by `flux_var`.
#' @param flux_var Character. Flux variable to plot (default `"NEE_VUT_REF"`).
#' @param metadata Optional data frame with `site_id` and `IGBP`.
#'
#' @return A named list with ggplot objects: `Forest`, `ShrubOpens`,
#'   `GrassCropsWet`, `Other`.
#'
#' @examples
#' \dontrun{
#' data_yy <- readRDS("data/processed/flux_data_converted_yy.rds")
#' out <- fig_flux_by_biome_group(data_yy)
#' print(out$Forest)
#' }
fig_flux_by_biome_group <- function(data_yy,
                                     flux_var = "NEE_VUT_REF",
                                     metadata = NULL) {
  .check_flux_var(data_yy, flux_var)
  data_yy <- .ensure_igbp(data_yy, metadata)

  y_label <- .flux_y_label(flux_var)

  group_map <- list(
    Forest          = c("DBF", "ENF", "MF", "EBF", "DNF"),
    ShrubOpens      = c("OSH", "CSH", "WSA", "SAV"),
    GrassCropsWet   = c("GRA", "CRO", "WET"),
    Other           = c("URB", "NV", "BSV", "WAT")
  )

  plot_data <- data_yy |>
    dplyr::mutate(
      FLUX  = .data[[flux_var]],
      year  = as.integer(TIMESTAMP),
      Group = dplyr::case_when(
        IGBP %in% group_map$Forest        ~ "Forest",
        IGBP %in% group_map$ShrubOpens    ~ "ShrubOpens",
        IGBP %in% group_map$GrassCropsWet ~ "GrassCropsWet",
        !is.na(IGBP)                      ~ "Other",
        TRUE                              ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(FLUX), !is.na(Group))

  site_counts <- plot_data |>
    dplyr::group_by(year, IGBP) |>
    dplyr::summarise(n_sites = dplyr::n_distinct(site_id), .groups = "drop")

  build_panel <- function(group_label) {
    grp_data   <- dplyr::filter(plot_data, Group == group_label)
    grp_counts <- dplyr::filter(site_counts, IGBP %in% unique(grp_data$IGBP))

    if (nrow(grp_data) == 0L) {
      return(
        ggplot2::ggplot() +
          ggplot2::labs(title = group_label, subtitle = "No data") +
          fluxnet_theme()
      )
    }

    y_max <- max(grp_data$FLUX, na.rm = TRUE)

    ggplot2::ggplot(grp_data, ggplot2::aes(x = factor(year), y = FLUX)) +
      ggplot2::geom_boxplot(outlier.shape = NA, fill = NA, color = "black",
                            alpha = 0.4) +
      ggplot2::geom_jitter(ggplot2::aes(color = IGBP), size = 0.8,
                           alpha = 0.3, width = 0.25) +
      ggplot2::geom_text(
        data = grp_counts,
        ggplot2::aes(x = factor(year), y = y_max * 1.05,
                     label = paste0("n=", n_sites)),
        inherit.aes = FALSE, size = 2.5, vjust = 0
      ) +
      ggplot2::facet_wrap(ggplot2::vars(IGBP), ncol = 1,
                          strip.position = "right") +
      ggplot2::labs(title = group_label, x = "Year", y = y_label) +
      fluxnet_theme(base_size = 12) +
      ggplot2::theme(
        axis.title.y    = ggtext::element_markdown(),
        axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1),
        legend.position = "none"
      )
  }

  list(
    Forest        = build_panel("Forest"),
    ShrubOpens    = build_panel("ShrubOpens"),
    GrassCropsWet = build_panel("GrassCropsWet"),
    Other         = build_panel("Other")
  )
}

# ---- fig_flux_timeseries_by_igbp -------------------------------------------

#' Median annual flux time series faceted by IGBP
#'
#' Plots median annual flux with a 95% CI ribbon over time, with one facet per
#' IGBP class. Adapted from `plot_flux_timeseries_by_igbp()` in
#' `legacy/fcn_plot_FLUXNET.R`.
#'
#' @param data_yy Data frame. Converted annual flux data with `site_id`,
#'   `TIMESTAMP`, and the variable named by `flux_var`.
#' @param flux_var Character. Flux variable to plot (default `"NEE_VUT_REF"`).
#' @param metadata Optional data frame with `site_id` and `IGBP`.
#'
#' @return A ggplot object (faceted time series).
#'
#' @examples
#' \dontrun{
#' data_yy <- readRDS("data/processed/flux_data_converted_yy.rds")
#' p <- fig_flux_timeseries_by_igbp(data_yy, flux_var = "GPP_NT_VUT_REF")
#' print(p)
#' }
fig_flux_timeseries_by_igbp <- function(data_yy,
                                         flux_var = "NEE_VUT_REF",
                                         metadata = NULL) {
  .check_flux_var(data_yy, flux_var)
  data_yy <- .ensure_igbp(data_yy, metadata)

  y_label <- .flux_y_label(flux_var)

  plot_data <- data_yy |>
    dplyr::filter(IGBP %in% IGBP_order) |>
    dplyr::mutate(
      FLUX = .data[[flux_var]],
      year = as.integer(TIMESTAMP),
      IGBP = factor(IGBP, levels = IGBP_order)
    ) |>
    dplyr::filter(!is.na(FLUX), !is.na(year))

  if (nrow(plot_data) == 0L) {
    warning("No data after IGBP filtering — returning empty plot.", call. = FALSE)
    return(ggplot2::ggplot() + ggplot2::labs(title = "No data") + fluxnet_theme())
  }

  ts_summary <- plot_data |>
    dplyr::group_by(IGBP, year) |>
    dplyr::summarise(
      median_flux = stats::median(FLUX, na.rm = TRUE),
      lower_CI    = stats::quantile(FLUX, 0.025, na.rm = TRUE),
      upper_CI    = stats::quantile(FLUX, 0.975, na.rm = TRUE),
      .groups     = "drop"
    )

  ggplot2::ggplot(ts_summary, ggplot2::aes(x = year, y = median_flux)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_CI, ymax = upper_CI),
                         fill = "steelblue", alpha = 0.3) +
    ggplot2::geom_line(color = "steelblue", linewidth = 1) +
    ggplot2::geom_point(color = "steelblue", size = 1.5) +
    ggplot2::facet_wrap(ggplot2::vars(IGBP), scales = "free_y", ncol = 4) +
    ggplot2::labs(x = "Year", y = y_label) +
    fluxnet_theme(base_size = 12) +
    ggplot2::theme(axis.title.y = ggtext::element_markdown())
}
