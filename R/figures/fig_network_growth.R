# R/figures/fig_network_growth.R
# Cumulative FLUXNET network growth figure for the Annual Paper 2026.
#
# Requires metadata only — no flux data. Intended for use early in the
# paper to show the expansion of the global eddy covariance network.

source("R/plot_constants.R")

#' Cumulative site growth stacked area chart
#'
#' Calculates the cumulative count of active sites by year, stratified by IGBP
#' class, using `first_year` from the snapshot metadata. Each site is counted
#' from its `first_year` onward through the final year in the dataset.
#'
#' @param metadata Data frame. Snapshot CSV data (one row per site) with columns
#'   `site_id`, `igbp`, `first_year`, `last_year`, `location_lat`, and
#'   `location_long`. Typically read from `data/snapshots/`.
#' @param geo_level Character. Geographic scope. Currently only `"global"` is
#'   supported. Continental breakdown is planned as a future extension.
#'
#' @return A ggplot stacked area chart: x = year, y = cumulative site count,
#'   fill = IGBP class. IGBP classes follow [IGBP_order] and are coloured with
#'   [scale_fill_igbp()].
#'
#' @examples
#' \dontrun{
#' meta <- readr::read_csv("data/snapshots/fluxnet_shuttle_snapshot_20260402T231017.csv")
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

  required_cols <- c("site_id", "igbp", "first_year", "last_year")
  missing_cols  <- setdiff(required_cols, names(metadata))
  if (length(missing_cols) > 0) {
    stop(
      "fig_network_growth: metadata is missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # One row per site, keeping only the columns we need
  sites <- metadata |>
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

  if (nrow(sites) == 0L) {
    warning("fig_network_growth: no sites remain after filtering — returning empty plot.",
            call. = FALSE)
    return(ggplot2::ggplot() +
             ggplot2::labs(title = "Network growth — no data") +
             fluxnet_theme())
  }

  year_min <- min(sites$first_year, na.rm = TRUE)
  year_max <- max(sites$last_year,  na.rm = TRUE)
  all_years <- seq(year_min, year_max)

  igbp_levels <- levels(sites$igbp)

  # Count new sites entering each year per IGBP, then cumsum over time
  entries <- sites |>
    dplyr::count(.data$igbp, .data$first_year, name = "new_sites")

  cumulative <- tidyr::expand_grid(
    year = all_years,
    igbp = factor(igbp_levels, levels = IGBP_order)
  ) |>
    dplyr::left_join(entries, by = c("year" = "first_year", "igbp" = "igbp")) |>
    dplyr::mutate(new_sites = tidyr::replace_na(.data$new_sites, 0L)) |>
    dplyr::arrange(.data$igbp, .data$year) |>
    dplyr::group_by(.data$igbp) |>
    dplyr::mutate(cumulative_sites = cumsum(.data$new_sites)) |>
    dplyr::ungroup()

  total_sites <- max(
    dplyr::summarise(
      dplyr::group_by(cumulative, .data$year),
      total = sum(.data$cumulative_sites)
    )$total
  )

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
