# R/figures/fig_network_growth.R
# FLUXNET network growth figures for the Annual Paper 2026.
#
# Requires metadata only — no flux data. Intended for use early in the
# paper to show the expansion of the global eddy covariance network.
#
# Functions:
#   fig_network_growth()               — cumulative stacked area chart
#   fig_network_growth_annual()        — new sites per year stacked bar chart
#   fig_network_duration_profile()     — deployment duration histograms at snapshot years
#   fig_network_active_proportion()    — cumulative count + % functionally active over time

source("R/plot_constants.R")

# ---- Internal helper --------------------------------------------------------

#' Prepare and filter site metadata for network growth figures
#'
#' @param metadata Data frame. Snapshot CSV (one row per site).
#' @return Filtered data frame with `site_id`, `igbp` (factor), `first_year`,
#'   `last_year` (integer). Stops if required columns are missing.
#' @noRd
.prep_growth_sites <- function(metadata) {
  required_cols <- c("site_id", "igbp", "first_year", "last_year")
  missing_cols  <- setdiff(required_cols, names(metadata))
  if (length(missing_cols) > 0L) {
    stop(
      "fig_network_growth: metadata is missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  metadata |>
    dplyr::distinct(.data$site_id, .keep_all = TRUE) |>
    dplyr::select("site_id", "igbp", "first_year", "last_year") |>
    dplyr::filter(
      !is.na(.data$first_year),
      !is.na(.data$igbp),
      .data$igbp %in% IGBP_order
    ) |>
    dplyr::mutate(
      igbp       = factor(.data$igbp, levels = IGBP_order),
      first_year = as.integer(.data$first_year),
      last_year  = as.integer(.data$last_year)
    )
}

# ---- fig_network_growth -----------------------------------------------------

#' Cumulative site growth stacked area chart
#'
#' Calculates the cumulative count of active sites by year, stratified by IGBP
#' class, using `first_year` from the snapshot metadata. Each site is counted
#' from its `first_year` onward through the final year in the dataset.
#' Forests stack at the bottom (darkest greens); barren classes at the top
#' (greys), following [IGBP_order].
#'
#' @param metadata Data frame. Snapshot CSV data (one row per site) with columns
#'   `site_id`, `igbp`, `first_year`, and `last_year`. Typically read from
#'   `data/snapshots/`.
#' @param geo_level Character. Geographic scope. Currently only `"global"` is
#'   supported. Continental breakdown is planned as a future extension.
#'
#' @return A ggplot stacked area chart: x = year, y = cumulative site count,
#'   fill = IGBP class using [scale_fill_igbp()].
#'
#' @examples
#' \dontrun{
#' meta <- readr::read_csv("data/snapshots/fluxnet_shuttle_snapshot_20260413T141220.csv")
#' p <- fig_network_growth(meta)
#' print(p)
#' }
fig_network_growth <- function(metadata, geo_level = "global") {
  if (!geo_level %in% "global") {
    message(
      "fig_network_growth: geo_level = '", geo_level, "' is not yet supported. ",
      "Continental breakdown is planned as a future extension. ",
      "Falling back to geo_level = 'global'."
    )
    geo_level <- "global"
  }

  sites <- .prep_growth_sites(metadata)

  if (nrow(sites) == 0L) {
    warning("fig_network_growth: no sites remain after filtering — returning empty plot.",
            call. = FALSE)
    return(ggplot2::ggplot() +
             ggplot2::labs(title = "Network growth — no data") +
             fluxnet_theme())
  }

  year_min  <- min(sites$first_year, na.rm = TRUE)
  year_max  <- max(sites$last_year,  na.rm = TRUE)
  all_years <- seq(year_min, year_max)

  entries <- sites |>
    dplyr::count(.data$igbp, .data$first_year, name = "new_sites")

  cumulative <- tidyr::expand_grid(
    year = all_years,
    igbp = factor(levels(sites$igbp), levels = IGBP_order)
  ) |>
    dplyr::left_join(entries, by = c("year" = "first_year", "igbp" = "igbp")) |>
    dplyr::mutate(new_sites = tidyr::replace_na(.data$new_sites, 0L)) |>
    dplyr::arrange(.data$igbp, .data$year) |>
    dplyr::group_by(.data$igbp) |>
    dplyr::mutate(cumulative_sites = cumsum(.data$new_sites)) |>
    dplyr::ungroup()

  ggplot2::ggplot(
    cumulative,
    ggplot2::aes(x = .data$year, y = .data$cumulative_sites,
                 fill = .data$igbp)
  ) +
    ggplot2::geom_area(position = "stack", alpha = 0.85, colour = NA) +
    scale_fill_igbp(name = "IGBP") +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 8),
                                expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 6),
                                expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(
      title    = "Cumulative FLUXNET network growth by IGBP class",
      subtitle = paste0("n = ", nrow(sites), " sites; ",
                        year_min, "\u2013", year_max),
      x        = "Year",
      y        = "Cumulative sites"
    ) +
    fluxnet_theme() +
    ggplot2::theme(legend.position = "right")
}

# ---- fig_network_growth_annual ----------------------------------------------

#' Annual new-site additions stacked bar chart
#'
#' Shows the number of new sites added to the FLUXNET network each year,
#' stratified by IGBP class, using `first_year` from the snapshot metadata.
#' This complements [fig_network_growth()] by showing the *rate* of network
#' expansion rather than the cumulative total.
#'
#' @param metadata Data frame. Snapshot CSV data (one row per site) with columns
#'   `site_id`, `igbp`, `first_year`, and `last_year`. Typically read from
#'   `data/snapshots/`.
#' @param geo_level Character. Geographic scope. Currently only `"global"` is
#'   supported.
#'
#' @return A ggplot stacked bar chart: x = year, y = new sites added,
#'   fill = IGBP class using [scale_fill_igbp()].
#'
#' @examples
#' \dontrun{
#' meta <- readr::read_csv("data/snapshots/fluxnet_shuttle_snapshot_20260413T141220.csv")
#' p <- fig_network_growth_annual(meta)
#' print(p)
#' }
fig_network_growth_annual <- function(metadata, geo_level = "global") {
  if (!geo_level %in% "global") {
    message(
      "fig_network_growth_annual: geo_level = '", geo_level,
      "' is not yet supported. Falling back to geo_level = 'global'."
    )
    geo_level <- "global"
  }

  sites <- .prep_growth_sites(metadata)

  if (nrow(sites) == 0L) {
    warning(
      "fig_network_growth_annual: no sites remain after filtering — returning empty plot.",
      call. = FALSE
    )
    return(ggplot2::ggplot() +
             ggplot2::labs(title = "Annual new sites — no data") +
             fluxnet_theme())
  }

  year_min <- min(sites$first_year, na.rm = TRUE)
  year_max <- max(sites$last_year,  na.rm = TRUE)

  annual <- sites |>
    dplyr::count(.data$igbp, .data$first_year, name = "new_sites") |>
    dplyr::rename(year = "first_year") |>
    dplyr::mutate(igbp = factor(.data$igbp, levels = IGBP_order))

  ggplot2::ggplot(
    annual,
    ggplot2::aes(x = .data$year, y = .data$new_sites, fill = .data$igbp)
  ) +
    ggplot2::geom_col(position = "stack", width = 0.85, colour = NA) +
    scale_fill_igbp(name = "IGBP") +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 8),
                                expand = ggplot2::expansion(mult = c(0.01, 0.01))) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 6),
                                expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(
      title    = "New FLUXNET sites added per year by IGBP class",
      subtitle = paste0("n = ", nrow(sites), " sites; ",
                        year_min, "\u2013", year_max),
      x        = "Year",
      y        = "New sites"
    ) +
    fluxnet_theme() +
    ggplot2::theme(legend.position = "right")
}

# ---- fig_network_duration_profile -------------------------------------------

#' Deployment duration profile histograms at snapshot years
#'
#' For each year in `years`, calculates per-site record length as
#' `n_years_with_valid_NEE` — the count of calendar years in `data_yy` where
#' `NEE_VUT_REF` is not `NA` for that site — then classifies each site as
#' "Functionally active" or "Inactive / high latency" based on whether
#' `last_year` from the snapshot metadata falls within `active_threshold` years
#' of the snapshot year.
#'
#' **Why `n_years_with_valid_NEE` rather than nominal record length:**
#' Nominal record length (last_year - first_year + 1) overstates the
#' analysis-relevant record for sites with extended gaps or delayed processing.
#' Counting years with at least one valid NEE observation in the processed YY
#' data reflects actual data availability for flux synthesis.
#'
#' @param metadata Data frame. Snapshot CSV (one row per site) with columns
#'   `site_id` and `last_year`.
#' @param data_yy Data frame. Processed annual (YY) flux data with columns
#'   `site_id`, `NEE_VUT_REF`, and `YEAR` (integer). Typically
#'   `flux_data_converted_yy.rds`.
#' @param years Integer vector of length 4. Snapshot years at which to assess
#'   the network (default `c(2010, 2015, 2020, 2025)`). Assembled as a 2x2
#'   patchwork via [patchwork::wrap_plots()].
#' @param active_threshold Integer. A site is "Functionally active" at a given
#'   snapshot year if `last_year >= snapshot_year - active_threshold`
#'   (default `4`).
#'
#' @return A patchwork object: 2x2 grid of histograms, one per snapshot year.
#'   x-axis = record length (years with valid `NEE_VUT_REF`);
#'   y-axis = number of sites; fill = active/inactive status.
#'
#' @examples
#' \dontrun{
#' meta    <- readr::read_csv("data/snapshots/fluxnet_shuttle_snapshot_20260414T153648.csv")
#' data_yy <- readRDS("data/processed/flux_data_converted_yy.rds")
#' p <- fig_network_duration_profile(meta, data_yy)
#' print(p)
#' }
fig_network_duration_profile <- function(metadata, data_yy,
                                         years = c(2010, 2015, 2020, 2025),
                                         active_threshold = 4L) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop(
      "Package 'patchwork' is required for fig_network_duration_profile(). ",
      "Install with: install.packages('patchwork')",
      call. = FALSE
    )
  }

  required_meta <- c("site_id", "last_year")
  missing_meta  <- setdiff(required_meta, names(metadata))
  if (length(missing_meta) > 0L) {
    stop(
      "fig_network_duration_profile: metadata missing required column(s): ",
      paste(missing_meta, collapse = ", "),
      call. = FALSE
    )
  }
  if (!"NEE_VUT_REF" %in% names(data_yy)) {
    stop(
      "fig_network_duration_profile: data_yy missing required column NEE_VUT_REF",
      call. = FALSE
    )
  }
  if (!"YEAR" %in% names(data_yy)) {
    stop(
      "fig_network_duration_profile: data_yy missing required column YEAR",
      call. = FALSE
    )
  }

  # Count calendar years per site with at least one valid NEE_VUT_REF
  nee_record <- data_yy |>
    dplyr::filter(!is.na(.data$NEE_VUT_REF)) |>
    dplyr::distinct(.data$site_id, .data$YEAR) |>
    dplyr::group_by(.data$site_id) |>
    dplyr::summarise(n_years_with_valid_NEE = dplyr::n(), .groups = "drop")

  site_profile <- metadata |>
    dplyr::distinct(.data$site_id, .keep_all = TRUE) |>
    dplyr::select("site_id", "last_year") |>
    dplyr::filter(!is.na(.data$last_year)) |>
    dplyr::mutate(last_year = as.integer(.data$last_year)) |>
    dplyr::left_join(nee_record, by = "site_id") |>
    dplyr::mutate(
      n_years_with_valid_NEE = tidyr::replace_na(.data$n_years_with_valid_NEE, 0L)
    )

  if (nrow(site_profile) == 0L) {
    warning(
      "fig_network_duration_profile: no sites with valid last_year — returning empty plot.",
      call. = FALSE
    )
    empty <- replicate(
      length(years),
      ggplot2::ggplot() + fluxnet_theme() + ggplot2::labs(title = "No data"),
      simplify = FALSE
    )
    return(patchwork::wrap_plots(empty, ncol = 2L))
  }

  active_colours <- c(
    "Functionally active"     = "#2196F3",
    "Inactive / high latency" = "#B0BEC5"
  )

  panels <- lapply(years, function(yr) {
    panel_data <- site_profile |>
      dplyr::mutate(
        activity = dplyr::if_else(
          .data$last_year >= yr - active_threshold,
          "Functionally active",
          "Inactive / high latency"
        ),
        activity = factor(
          .data$activity,
          levels = c("Functionally active", "Inactive / high latency")
        )
      )

    ggplot2::ggplot(
      panel_data,
      ggplot2::aes(x = .data$n_years_with_valid_NEE, fill = .data$activity)
    ) +
      ggplot2::geom_histogram(binwidth = 1L, colour = "white", linewidth = 0.25,
                              position = "stack") +
      ggplot2::scale_fill_manual(values = active_colours, name = NULL,
                                 drop = FALSE) +
      ggplot2::scale_x_continuous(
        breaks = scales::pretty_breaks(n = 6),
        expand = ggplot2::expansion(mult = c(0, 0.02))
      ) +
      ggplot2::scale_y_continuous(
        breaks = scales::pretty_breaks(n = 5),
        expand = ggplot2::expansion(mult = c(0, 0.05))
      ) +
      ggplot2::labs(
        title = paste0("Snapshot: ", yr),
        x     = "Record length (years with valid NEE)",
        y     = "Sites"
      ) +
      fluxnet_theme(base_size = 13) +
      ggplot2::theme(legend.position = "bottom")
  })

  patchwork::wrap_plots(panels, ncol = 2L) +
    patchwork::plot_annotation(
      title    = "Deployment duration profile of the FLUXNET network",
      subtitle = paste0(
        "Record length = years with valid NEE_VUT_REF. ",
        "Functionally active = data submitted within ",
        active_threshold, " years of snapshot year."
      )
    )
}

# ---- fig_network_active_proportion ------------------------------------------

#' Cumulative site count and proportion functionally active over time
#'
#' Two-panel figure showing (top) the cumulative count of sites with records
#' starting by each year, and (bottom) the percentage of those sites that are
#' "functionally active" — i.e., have submitted data within `active_threshold`
#' years. Both panels share the x-axis (1990–2025).
#'
#' This is a draft candidate for integration into the network growth figure.
#' Generate as a standalone figure first for co-author review before combining
#' with [fig_network_growth()].
#'
#' @param metadata Data frame. Snapshot CSV (one row per site) with columns
#'   `site_id`, `first_year`, and `last_year`.
#' @param data_yy Data frame. Annual (YY) flux data. Currently unused in this
#'   function; included for API consistency with
#'   [fig_network_duration_profile()].
#' @param active_threshold Integer. A site is "functionally active" in year `y`
#'   if `last_year >= y - active_threshold` (default `4`).
#'
#' @return A patchwork object with two vertically stacked panels sharing the
#'   x-axis. Top panel: cumulative site count as a filled area line. Bottom
#'   panel: percentage of cumulative sites that are functionally active.
#'
#' @examples
#' \dontrun{
#' meta    <- readr::read_csv("data/snapshots/fluxnet_shuttle_snapshot_20260414T153648.csv")
#' data_yy <- readRDS("data/processed/flux_data_converted_yy.rds")
#' p <- fig_network_active_proportion(meta, data_yy)
#' print(p)
#' }
fig_network_active_proportion <- function(metadata, data_yy,
                                          active_threshold = 4L) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop(
      "Package 'patchwork' is required for fig_network_active_proportion(). ",
      "Install with: install.packages('patchwork')",
      call. = FALSE
    )
  }

  required_meta <- c("site_id", "first_year", "last_year")
  missing_meta  <- setdiff(required_meta, names(metadata))
  if (length(missing_meta) > 0L) {
    stop(
      "fig_network_active_proportion: metadata missing required column(s): ",
      paste(missing_meta, collapse = ", "),
      call. = FALSE
    )
  }

  sites <- metadata |>
    dplyr::distinct(.data$site_id, .keep_all = TRUE) |>
    dplyr::select("site_id", "first_year", "last_year") |>
    dplyr::filter(!is.na(.data$first_year), !is.na(.data$last_year)) |>
    dplyr::mutate(
      first_year = as.integer(.data$first_year),
      last_year  = as.integer(.data$last_year)
    )

  if (nrow(sites) == 0L) {
    warning(
      "fig_network_active_proportion: no sites with valid first/last year — returning empty plot.",
      call. = FALSE
    )
    empty <- ggplot2::ggplot() + fluxnet_theme() + ggplot2::labs(title = "No data")
    return(empty / empty)
  }

  year_range <- 1990:2025

  annual_stats <- do.call(rbind, lapply(year_range, function(yr) {
    cum_sites    <- sum(sites$first_year <= yr)
    active_sites <- sum(sites$first_year <= yr &
                          sites$last_year  >= yr - active_threshold)
    data.frame(
      year             = yr,
      cumulative_sites = cum_sites,
      pct_active       = if (cum_sites > 0L) 100 * active_sites / cum_sites
                         else NA_real_
    )
  }))

  # Top panel: cumulative site count
  p_top <- ggplot2::ggplot(
    annual_stats,
    ggplot2::aes(x = .data$year, y = .data$cumulative_sites)
  ) +
    ggplot2::geom_area(fill = "#1565C0", alpha = 0.2, colour = NA) +
    ggplot2::geom_line(colour = "#1565C0", linewidth = 1.0) +
    ggplot2::scale_x_continuous(
      limits = range(year_range),
      breaks = scales::pretty_breaks(n = 8),
      expand = ggplot2::expansion(mult = c(0, 0.01))
    ) +
    ggplot2::scale_y_continuous(
      breaks = scales::pretty_breaks(n = 6),
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::labs(y = "Cumulative sites") +
    fluxnet_theme() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x  = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )

  # Bottom panel: % functionally active
  p_bottom <- ggplot2::ggplot(
    annual_stats,
    ggplot2::aes(x = .data$year, y = .data$pct_active)
  ) +
    ggplot2::geom_line(colour = "#2196F3", linewidth = 1.0) +
    ggplot2::geom_point(colour = "#2196F3", size = 1.8) +
    ggplot2::scale_x_continuous(
      limits = range(year_range),
      breaks = scales::pretty_breaks(n = 8),
      expand = ggplot2::expansion(mult = c(0, 0.01))
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, by = 20),
      expand = ggplot2::expansion(mult = c(0, 0.02))
    ) +
    ggplot2::labs(x = "Year", y = "% functionally active") +
    fluxnet_theme()

  (p_top / p_bottom) +
    patchwork::plot_layout(heights = c(1.6, 1)) +
    patchwork::plot_annotation(
      title    = "FLUXNET network size and data currency over time",
      subtitle = paste0(
        "Top: cumulative sites with first_year \u2264 year. ",
        "Bottom: % with last_year \u2265 year \u2212 ", active_threshold,
        " (functionally active). ",
        "Draft for integration into network growth figure."
      )
    )
}
