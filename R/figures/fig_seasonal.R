# R/figures/fig_seasonal.R
# Seasonal cycle figures for the FLUXNET Annual Paper 2026.
#
# Ported from:
#   legacy/fcn_plot_FLUXNET.R  — plot_seasonal_cycle()
#   legacy/AGUSlop.R           — weekly seasonal cycles by IGBP, site triplet
#   legacy/AMFOct25_poster.R   — style conventions, add_top_right_axes()
#
# All functions accept pre-converted daily data (flux_data_converted_dd.rds)
# and an optional metadata data frame for IGBP / site joins.
# No data loading occurs inside these functions.

source("R/plot_constants.R")

# ---- Internal helpers ------------------------------------------------------

# Parse YYYYMMDD integer TIMESTAMP to Date.
.parse_date <- function(x) lubridate::ymd(as.character(x))

# Map flux variable name to a daily axis label.
.flux_daily_label <- function(flux_var) {
  if (startsWith(flux_var, "NEE"))  return(lab_nee_daily)
  if (startsWith(flux_var, "GPP"))  return(lab_gpp_daily)
  if (startsWith(flux_var, "RECO")) return(lab_reco_daily)
  if (startsWith(flux_var, "LE"))   return("LE (mm H<sub>2</sub>O d<sup>-1</sup>)")
  flux_var
}

# Add mirrored top / right axes with inward ticks (for date x-axis).
# Adapted from add_top_right_axes() in legacy/fcn_plot_FLUXNET.R.
.add_top_right_axes <- function(p,
                                 x_breaks         = scales::breaks_width("2 months"),
                                 show_top_labels  = FALSE,
                                 show_right_labels = FALSE) {
  p +
    ggplot2::scale_x_date(
      breaks      = x_breaks,
      date_labels = "%b",
      sec.axis    = ggplot2::dup_axis(
        labels = if (show_top_labels) ggplot2::waiver() else NULL,
        name   = NULL
      )
    ) +
    ggplot2::scale_y_continuous(
      sec.axis = ggplot2::dup_axis(
        labels = if (show_right_labels) ggplot2::waiver() else NULL,
        name   = NULL
      )
    ) +
    ggplot2::theme(
      axis.ticks.length  = grid::unit(-3, "pt"),
      axis.line.x.bottom = ggplot2::element_line(),
      axis.line.x.top    = ggplot2::element_line(),
      axis.line.y.left   = ggplot2::element_line(),
      axis.line.y.right  = ggplot2::element_line(),
      axis.text.x.top    = if (show_top_labels)
        ggplot2::element_text() else ggplot2::element_blank(),
      axis.text.y.right  = if (show_right_labels)
        ggplot2::element_text() else ggplot2::element_blank(),
      axis.title.x       = ggplot2::element_blank()
    )
}

# Join IGBP and optionally lat/lon from metadata when absent from data.
.ensure_igbp_dd <- function(data, metadata = NULL) {
  if (!"IGBP" %in% names(data)) {
    if (is.null(metadata)) {
      warning(
        "IGBP column not found in data and no metadata supplied. ",
        "Pass a metadata data frame with columns site_id and IGBP.",
        call. = FALSE
      )
      data$IGBP <- NA_character_
    } else {
      meta_cols <- intersect(
        c("site_id", "IGBP", "LOCATION_LAT", "LOCATION_LONG"),
        names(metadata)
      )
      data <- dplyr::left_join(
        data, metadata[, meta_cols, drop = FALSE], by = "site_id"
      )
    }
  }
  if (all(is.na(data$IGBP))) {
    warning("All IGBP values are NA — figures will be empty.", call. = FALSE)
  }
  data
}

# ---- fig_seasonal_cycle ----------------------------------------------------

#' DOY seasonal cycle by broad biome group
#'
#' Plots day-of-year mean flux with 95% CI ribbon for each IGBP class, faceted
#' into four broad biome groups (Forest, Shrub/Opens, Grass/Crops/Wet, Other).
#' Adapted from `plot_seasonal_cycle()` in `legacy/fcn_plot_FLUXNET.R`.
#'
#' @param data_dd Data frame. Converted daily flux data
#'   (`flux_data_converted_dd.rds`) with columns `site_id`, `TIMESTAMP`
#'   (YYYYMMDD integer), `DOY`, and the variable named by `flux_var`.
#' @param flux_var Character. Flux variable to plot (default `"GPP_NT_VUT_REF"`).
#' @param metadata Optional data frame with `site_id` and `IGBP`.
#'
#' @return A named list with ggplot objects: `Forest`, `ShrubOpens`,
#'   `GrassCropsWet`, `Other`.
#'
#' @examples
#' \dontrun{
#' data_dd <- readRDS("data/processed/flux_data_converted_dd.rds")
#' out <- fig_seasonal_cycle(data_dd, flux_var = "GPP_NT_VUT_REF")
#' print(out$Forest)
#' }
fig_seasonal_cycle <- function(data_dd,
                                flux_var = "GPP_NT_VUT_REF",
                                metadata = NULL) {
  if (!flux_var %in% names(data_dd)) {
    stop(sprintf("flux_var '%s' not found in data.", flux_var), call. = FALSE)
  }
  data_dd <- .ensure_igbp_dd(data_dd, metadata)

  y_label <- .flux_daily_label(flux_var)

  group_map <- list(
    Forest        = c("DBF", "ENF", "MF", "EBF", "DNF"),
    ShrubOpens    = c("OSH", "CSH", "WSA", "SAV"),
    GrassCropsWet = c("GRA", "CRO", "WET"),
    Other         = c("URB", "NV", "BSV", "WAT")
  )

  plot_data <- data_dd |>
    dplyr::mutate(
      FLUX  = .data[[flux_var]],
      Group = dplyr::case_when(
        IGBP %in% group_map$Forest        ~ "Forest",
        IGBP %in% group_map$ShrubOpens    ~ "ShrubOpens",
        IGBP %in% group_map$GrassCropsWet ~ "GrassCropsWet",
        !is.na(IGBP)                      ~ "Other",
        TRUE                              ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(FLUX), !is.na(DOY), !is.na(Group))

  build_panel <- function(group_label) {
    grp_data <- dplyr::filter(plot_data, Group == group_label)

    if (nrow(grp_data) == 0L || all(is.na(grp_data$IGBP))) {
      return(
        ggplot2::ggplot() +
          ggplot2::labs(title = group_label, subtitle = "No data") +
          fluxnet_theme()
      )
    }

    seasonal_summary <- grp_data |>
      dplyr::group_by(IGBP, DOY) |>
      dplyr::summarise(
        mean_flux = mean(FLUX, na.rm = TRUE),
        se_flux   = stats::sd(FLUX, na.rm = TRUE) / sqrt(dplyr::n()),
        n         = dplyr::n(),
        .groups   = "drop"
      ) |>
      dplyr::mutate(
        ci_lower = mean_flux - stats::qt(0.975, df = pmax(n - 1, 1)) * se_flux,
        ci_upper = mean_flux + stats::qt(0.975, df = pmax(n - 1, 1)) * se_flux
      )

    ggplot2::ggplot(
      seasonal_summary,
      ggplot2::aes(x = DOY, y = mean_flux, color = IGBP, fill = IGBP)
    ) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
                           alpha = 0.2, color = NA) +
      ggplot2::geom_line(linewidth = 1.1) +
      ggplot2::labs(
        title  = group_label,
        x      = "Day of year",
        y      = y_label,
        color  = "IGBP",
        fill   = "IGBP"
      ) +
      fluxnet_theme(base_size = 12) +
      ggplot2::theme(
        axis.title.y    = ggtext::element_markdown(),
        legend.position = "right"
      )
  }

  list(
    Forest        = build_panel("Forest"),
    ShrubOpens    = build_panel("ShrubOpens"),
    GrassCropsWet = build_panel("GrassCropsWet"),
    Other         = build_panel("Other")
  )
}

# ---- fig_seasonal_weekly ---------------------------------------------------

#' Weekly median seasonal cycle by IGBP class
#'
#' Aggregates daily flux to ISO-week medians and plots seasonal trajectories by
#' IGBP class using stable colours and shapes. Only IGBP classes with at least
#' `min_sites` sites are included. Adapted from the weekly cycle section of
#' `legacy/AGUSlop.R` and `legacy/AMFOct25_poster.R`.
#'
#' @param data_dd Data frame. Converted daily flux data with `site_id`,
#'   `TIMESTAMP` (YYYYMMDD integer), and the variable named by `flux_var`.
#' @param flux_var Character. Flux variable to plot (default `"GPP_NT_VUT_REF"`).
#' @param metadata Optional data frame with `site_id` and `IGBP`.
#' @param min_sites Integer. Minimum number of sites an IGBP class must have to
#'   be included (default `3`; legacy default was `10` for full datasets).
#'
#' @return A ggplot object with top/right mirror axes.
#'
#' @examples
#' \dontrun{
#' data_dd <- readRDS("data/processed/flux_data_converted_dd.rds")
#' p <- fig_seasonal_weekly(data_dd, flux_var = "NEE_VUT_REF")
#' print(p)
#' }
fig_seasonal_weekly <- function(data_dd,
                                 flux_var  = "GPP_NT_VUT_REF",
                                 metadata  = NULL,
                                 min_sites = 3L) {
  if (!flux_var %in% names(data_dd)) {
    stop(sprintf("flux_var '%s' not found in data.", flux_var), call. = FALSE)
  }
  data_dd <- .ensure_igbp_dd(data_dd, metadata)

  y_label <- .flux_daily_label(flux_var)

  # Filter to IGBPs with enough sites
  igbp_counts <- data_dd |>
    dplyr::filter(!is.na(IGBP)) |>
    dplyr::distinct(site_id, IGBP) |>
    dplyr::count(IGBP, name = "n_sites")

  eligible <- igbp_counts |>
    dplyr::filter(n_sites >= min_sites) |>
    dplyr::pull(IGBP) |>
    sort()

  if (length(eligible) == 0L) {
    warning(
      sprintf("No IGBP classes have >= %d sites. Returning empty plot.", min_sites),
      call. = FALSE
    )
    return(ggplot2::ggplot() +
             ggplot2::labs(title = "No eligible IGBP classes") + fluxnet_theme())
  }

  flux_weekly <- data_dd |>
    dplyr::filter(IGBP %in% eligible, !is.na(.data[[flux_var]])) |>
    dplyr::mutate(
      date = .parse_date(TIMESTAMP),
      week = lubridate::isoweek(date),
      FLUX = .data[[flux_var]]
    ) |>
    dplyr::group_by(IGBP, week) |>
    dplyr::summarise(FLUX = stats::median(FLUX, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      IGBP      = factor(IGBP, levels = eligible),
      fake_date = lubridate::ymd("2020-01-01") + lubridate::weeks(week)
    )

  shape_vec  <- c(16, 17, 15, 3, 7, 8, 18, 0, 1, 2, 4, 5, 6, 9, 10, 11)
  shp        <- stats::setNames(shape_vec[seq_along(eligible)], eligible)

  p <- ggplot2::ggplot(
    flux_weekly,
    ggplot2::aes(x = fake_date, y = FLUX, color = IGBP, shape = IGBP)
  ) +
    poster_geom_point() +
    poster_geom_line() +
    scale_color_igbp(guide = ggplot2::guide_legend(ncol = 2)) +
    ggplot2::scale_shape_manual(values = shp) +
    ggplot2::labs(x = "", y = y_label, color = "IGBP", shape = "IGBP") +
    fluxnet_theme() +
    ggplot2::theme(
      axis.title.y = ggtext::element_markdown(angle = 90, vjust = 0.5,
                                              hjust = 0.5)
    )

  .add_top_right_axes(p)
}

# ---- fig_seasonal_triplet --------------------------------------------------

#' Weekly NEE seasonal cycle for a named set of sites
#'
#' Plots IQR ribbon + median line for each of a small set of named sites,
#' allowing direct site-to-site seasonal comparison. Adapted from the Harvard
#' triplet section of `legacy/AGUSlop.R`.
#'
#' @param data_dd Data frame. Converted daily flux data with `site_id`,
#'   `TIMESTAMP` (YYYYMMDD integer), and `flux_var`.
#' @param flux_var Character. Flux variable to plot (default `"NEE_VUT_REF"`).
#' @param site_ids Character vector. Site IDs to include
#'   (default: first three sites found in data).
#' @param ribbon_colors Optional named character vector of hex colours, one per
#'   site. If `NULL`, colours are assigned automatically.
#' @param site_shapes Optional named integer vector of point shapes, one per
#'   site. If `NULL`, shapes are assigned automatically.
#' @param y_limits Optional numeric vector of length 2 for y-axis limits.
#'
#' @return A ggplot object with top/right mirror axes.
#'
#' @examples
#' \dontrun{
#' data_dd <- readRDS("data/processed/flux_data_converted_dd.rds")
#' p <- fig_seasonal_triplet(data_dd, site_ids = c("US-Ha1", "US-Me2", "FI-Hyy"))
#' print(p)
#' }
fig_seasonal_triplet <- function(data_dd,
                                  flux_var      = "NEE_VUT_REF",
                                  site_ids      = NULL,
                                  ribbon_colors = NULL,
                                  site_shapes   = NULL,
                                  y_limits      = NULL) {
  if (!flux_var %in% names(data_dd)) {
    stop(sprintf("flux_var '%s' not found in data.", flux_var), call. = FALSE)
  }

  available_sites <- unique(data_dd$site_id)
  if (is.null(site_ids)) {
    site_ids <- head(available_sites, 3L)
    message("site_ids not specified — using: ", paste(site_ids, collapse = ", "))
  }

  missing_sites <- setdiff(site_ids, available_sites)
  if (length(missing_sites) > 0L) {
    warning(
      "These site_ids are not in the data and will be skipped: ",
      paste(missing_sites, collapse = ", "),
      call. = FALSE
    )
    site_ids <- intersect(site_ids, available_sites)
  }

  if (length(site_ids) == 0L) {
    warning("No valid sites found. Returning empty plot.", call. = FALSE)
    return(ggplot2::ggplot() +
             ggplot2::labs(title = "No valid sites") + fluxnet_theme())
  }

  y_label <- .flux_daily_label(flux_var)

  # Default colours and shapes (one per site)
  default_cols   <- c("#F58518", "#4C78A8", "#54A24B",
                       "#E45756", "#72B7B2", "#FF9DA6")
  default_shapes <- c(15L, 16L, 25L, 17L, 18L, 3L)

  if (is.null(ribbon_colors)) {
    ribbon_colors <- stats::setNames(
      default_cols[seq_along(site_ids)], site_ids
    )
  }
  if (is.null(site_shapes)) {
    site_shapes <- stats::setNames(
      default_shapes[seq_along(site_ids)], site_ids
    )
  }

  weekly <- data_dd |>
    dplyr::filter(site_id %in% site_ids, !is.na(.data[[flux_var]])) |>
    dplyr::mutate(
      date = .parse_date(TIMESTAMP),
      week = lubridate::isoweek(date),
      FLUX = .data[[flux_var]]
    ) |>
    dplyr::group_by(site_id, week) |>
    dplyr::summarise(
      FLUX_med = stats::median(FLUX, na.rm = TRUE),
      FLUX_lo  = stats::quantile(FLUX, 0.25, na.rm = TRUE, names = FALSE),
      FLUX_hi  = stats::quantile(FLUX, 0.75, na.rm = TRUE, names = FALSE),
      .groups  = "drop"
    ) |>
    dplyr::mutate(
      site_id   = factor(site_id, levels = site_ids),
      fake_date = lubridate::ymd("2020-01-01") + lubridate::weeks(week)
    )

  p <- ggplot2::ggplot(
    weekly,
    ggplot2::aes(x = fake_date, y = FLUX_med, group = site_id)
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = FLUX_lo, ymax = FLUX_hi, fill = site_id),
      alpha = 0.18, color = NA
    ) +
    ggplot2::scale_fill_manual(values = ribbon_colors, guide = "none") +
    poster_geom_line(color = "black") +
    poster_geom_point(ggplot2::aes(shape = site_id), color = "black") +
    ggplot2::scale_shape_manual(values = site_shapes) +
    ggplot2::labs(x = "", y = y_label, shape = "Site") +
    fluxnet_theme() +
    ggplot2::theme(
      axis.title.y = ggtext::element_markdown(angle = 90, vjust = 0.5,
                                              hjust = 0.5)
    )

  if (!is.null(y_limits)) {
    p <- p + ggplot2::coord_cartesian(ylim = y_limits)
  }

  .add_top_right_axes(p)
}
