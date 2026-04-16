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
#' `snapshot_year - first_year` — how long the site had been running by that
#' snapshot year. Sites with `first_year > snapshot_year` are excluded from
#' that panel. Activity classification uses `last_year` only:
#' "Functionally active" if `last_year >= snapshot_year - active_threshold`.
#'
#' For example: US-Ha1 (first_year = 1991) shows 19 years in the 2010 panel
#' and 34 years in the 2025 panel.
#'
#' All panels share the same x and y limits, derived from the final (largest)
#' snapshot year so that scales are comparable across panels.
#'
#' Returns a named list with two layout variants:
#' - **`vA`**: four panels stacked in a single column (oldest on top). X axis
#'   on the bottom panel only; y axis title "Sites" on the vertically centred
#'   panel only; snapshot year as a right-aligned strip title per panel.
#' - **`vB`**: four panels in a single row (oldest on left). Y axis on the
#'   leftmost panel only; x axis title on the horizontally centred panel only;
#'   snapshot year as a centred title per panel.
#'
#' Both variants use [patchwork::plot_annotation()] for the outer figure title
#' and subtitle, and collect the shared legend via
#' [patchwork::plot_layout(guides = "collect")].
#'
#' @param metadata Data frame. Snapshot CSV (one row per site) with columns
#'   `site_id`, `first_year`, and `last_year`.
#' @param years Integer vector of length 4. Snapshot years (default
#'   `c(2010, 2015, 2020, 2025)`).
#' @param active_threshold Integer. "Functionally active" if
#'   `last_year >= snapshot_year - active_threshold` (default `4`).
#'
#' @return Named list with elements `vA` (single-column patchwork) and `vB`
#'   (single-row patchwork).
#'
#' @examples
#' \dontrun{
#' meta <- readr::read_csv("data/snapshots/fluxnet_shuttle_snapshot_20260414T153648.csv")
#' figs <- fig_network_duration_profile(meta)
#' print(figs$vA)
#' print(figs$vB)
#' }
fig_network_duration_profile <- function(metadata,
                                         years = c(2010, 2015, 2020, 2025),
                                         active_threshold = 4L) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop(
      "Package 'patchwork' is required for fig_network_duration_profile(). ",
      "Install with: install.packages('patchwork')",
      call. = FALSE
    )
  }

  required_meta <- c("site_id", "first_year", "last_year")
  missing_meta  <- setdiff(required_meta, names(metadata))
  if (length(missing_meta) > 0L) {
    stop(
      "fig_network_duration_profile: metadata missing required column(s): ",
      paste(missing_meta, collapse = ", "),
      call. = FALSE
    )
  }

  site_profile <- metadata |>
    dplyr::distinct(.data$site_id, .keep_all = TRUE) |>
    dplyr::select("site_id", "first_year", "last_year") |>
    dplyr::filter(!is.na(.data$first_year), !is.na(.data$last_year)) |>
    dplyr::mutate(
      first_year = as.integer(.data$first_year),
      last_year  = as.integer(.data$last_year)
    )

  if (nrow(site_profile) == 0L) {
    warning(
      "fig_network_duration_profile: no sites with valid first/last year — returning empty plots.",
      call. = FALSE
    )
    empty <- ggplot2::ggplot() + fluxnet_theme() + ggplot2::labs(title = "No data")
    empties <- replicate(length(years), empty, simplify = FALSE)
    return(list(
      vA = patchwork::wrap_plots(empties, ncol = 1L),
      vB = patchwork::wrap_plots(empties, nrow = 1L)
    ))
  }

  # --- shared axis limits from the final (largest) snapshot year --------------
  ref_yr   <- max(years)
  ref_data <- site_profile |>
    dplyr::filter(.data$first_year <= ref_yr) |>
    dplyr::mutate(record_length = ref_yr - .data$first_year)

  x_max <- max(ref_data$record_length)
  y_max <- ref_data |>
    dplyr::count(.data$record_length) |>
    dplyr::pull(.data$n) |>
    max()
  x_lim <- c(0, x_max + 1L)   # +1 ensures the rightmost histogram bar renders fully
  y_lim <- c(0, ceiling(y_max * 1.08))

  # --- colours and annotation text --------------------------------------------
  active_colours <- c(
    "Functionally active"     = "#2196F3",
    "Inactive / high latency" = "#B0BEC5"
  )
  outer_subtitle <- paste0(
    "Record length = snapshot_year \u2212 first_year. ",
    "Functionally active = last_year \u2265 snapshot_year \u2212 ",
    active_threshold, "."
  )

  n_years      <- length(years)
  mid_idx      <- ceiling(n_years / 2)   # panel index for centred axis title

  # --- per-panel data ---------------------------------------------------------
  panel_datasets <- lapply(years, function(yr) {
    site_profile |>
      dplyr::filter(.data$first_year <= yr) |>
      dplyr::mutate(
        record_length = yr - .data$first_year,
        activity = factor(
          dplyr::if_else(
            .data$last_year >= yr - active_threshold,
            "Functionally active",
            "Inactive / high latency"
          ),
          levels = c("Functionally active", "Inactive / high latency")
        )
      )
  })

  # --- panel builder ----------------------------------------------------------
  # show_x_text:  show x axis tick labels and ticks
  # show_x_title: show x axis title
  # show_y_text:  show y axis tick labels and ticks
  # show_y_title: show y axis title
  # title_hjust:  hjust for the panel title (year label)
  # mar:          plot.margin (t, r, b, l) in points
  make_panel <- function(pd, yr,
                         show_x_text, show_x_title,
                         show_y_text, show_y_title,
                         title_hjust, mar) {
    n_sites <- nrow(pd)
    ggplot2::ggplot(
      pd,
      ggplot2::aes(x = .data$record_length, fill = .data$activity)
    ) +
      ggplot2::geom_histogram(binwidth = 1L, colour = "white", linewidth = 0.2,
                              position = "stack") +
      ggplot2::scale_fill_manual(values = active_colours, name = NULL,
                                 drop = FALSE) +
      ggplot2::scale_x_continuous(
        breaks = scales::pretty_breaks(n = 6),
        expand = ggplot2::expansion(mult = c(0, 0))
      ) +
      ggplot2::scale_y_continuous(
        breaks = scales::pretty_breaks(n = 4),
        expand = ggplot2::expansion(mult = c(0, 0))
      ) +
      ggplot2::coord_cartesian(xlim = x_lim, ylim = y_lim) +
      ggplot2::labs(
        title = paste0(yr, "  (n\u2009=\u2009", n_sites, ")"),
        x     = if (show_x_title) "Record length (years)" else NULL,
        y     = if (show_y_title) "Sites" else NULL
      ) +
      fluxnet_theme(base_size = 12) +
      ggplot2::theme(
        plot.title      = ggplot2::element_text(size = 11, hjust = title_hjust,
                                                margin = ggplot2::margin(b = 2)),
        axis.text.x     = if (show_x_text)  ggplot2::element_text()
                          else               ggplot2::element_blank(),
        axis.ticks.x    = if (show_x_text)  ggplot2::element_line()
                          else               ggplot2::element_blank(),
        axis.text.y     = if (show_y_text)  ggplot2::element_text()
                          else               ggplot2::element_blank(),
        axis.ticks.y    = if (show_y_text)  ggplot2::element_line()
                          else               ggplot2::element_blank(),
        legend.position = "bottom",
        plot.margin     = ggplot2::margin(mar[1], mar[2], mar[3], mar[4],
                                          unit = "pt")
      )
  }

  # --- Version A: single column, oldest on top --------------------------------
  #   x axis text+title: bottom panel only
  #   y axis text: all panels (all are leftmost in ncol=1)
  #   y axis title "Sites": mid_idx panel only (approximately centred)
  #   year label: right-aligned title

  vA_panels <- lapply(seq_along(years), function(i) {
    is_top    <- i == 1L
    is_bottom <- i == n_years
    is_mid    <- i == mid_idx

    # top margin: 4pt for first panel, 0 for others (panels touch)
    # bottom margin: 4pt for last panel, 0 for others
    mar <- c(
      if (is_top)    4L else 0L,   # top
      6L,                           # right (room for axis text)
      if (is_bottom) 4L else 0L,   # bottom
      4L                            # left
    )

    make_panel(
      pd          = panel_datasets[[i]],
      yr          = years[[i]],
      show_x_text  = is_bottom,
      show_x_title = is_bottom,
      show_y_text  = TRUE,
      show_y_title = is_mid,
      title_hjust  = 1,            # right-aligned strip
      mar          = mar
    )
  })

  vA <- patchwork::wrap_plots(vA_panels, ncol = 1L) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(
      title    = "Deployment duration profile of the FLUXNET network",
      subtitle = outer_subtitle
    ) &
    ggplot2::theme(legend.position = "bottom")

  # --- Version B: single row, oldest on left ----------------------------------
  #   y axis text+title: leftmost panel only
  #   x axis text: all panels (all are bottom in nrow=1)
  #   x axis title "Record length (years)": mid_idx panel only (approximately centred)
  #   year label: centred title

  vB_panels <- lapply(seq_along(years), function(i) {
    is_left <- i == 1L
    is_mid  <- i == mid_idx

    # left margin: 4pt for first panel, 0 for others (panels touch)
    # right margin: 4pt for last panel, 0 for others
    mar <- c(
      4L,                                    # top
      if (i == n_years) 4L else 0L,         # right
      6L,                                    # bottom (room for axis text)
      if (is_left)      4L else 0L           # left
    )

    make_panel(
      pd           = panel_datasets[[i]],
      yr           = years[[i]],
      show_x_text  = TRUE,
      show_x_title = is_mid,
      show_y_text  = is_left,
      show_y_title = is_left,
      title_hjust  = 0.5,          # centred strip
      mar          = mar
    )
  })

  vB <- patchwork::wrap_plots(vB_panels, nrow = 1L) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(
      title    = "Deployment duration profile of the FLUXNET network",
      subtitle = outer_subtitle
    ) &
    ggplot2::theme(legend.position = "bottom")

  list(vA = vA, vB = vB)
}

# ---- fig_network_subregion_overview -----------------------------------------

#' Three-panel subregion overview: map, total sites, and data latency
#'
#' Combines three panels into a single patchwork figure summarising the
#' FLUXNET network by UN M.49 subregion at a snapshot year:
#'
#' - **Left** — 2025 choropleth map produced by
#'   [fig_map_subregion_sites()] (must be sourced from
#'   `R/figures/fig_maps.R` before calling this function).
#' - **Middle** — Horizontal stacked bar showing *all* sites in `metadata`,
#'   classified as "Functionally active" (`current_year - last_year <=
#'   active_threshold`) or "Inactive / high latency".
#' - **Right** — Horizontal stacked bar of functionally active sites only,
#'   using the traffic-light latency palette from [fig_latency_by_subregion()].
#'
#' Both bar panels share the same subregion factor order (total sites,
#' descending from top). Y-axis labels (subregion names) appear on the middle
#' panel only; the right panel suppresses them so the two bars appear joined.
#' Subregion assignment uses `countrycode::countrycode(iso2, "iso2c",
#' "un.regionsub.name")` on the first two characters of `site_id`. FLUXNET
#' "UK-*" sites are silently recoded from "UK" to "GB" before lookup.
#'
#' @param metadata Data frame. Snapshot CSV (one row per site) with columns
#'   `site_id`, `last_year`, `location_lat`, `location_long`, and `first_year`
#'   (the latter two are required by the map panel).
#' @param data_yy Data frame. Annual (YY) flux data. Currently unused;
#'   included for API consistency. Pass `NULL` if not available.
#' @param current_year Integer. Reference year for latency calculation
#'   (default `2026L`).
#' @param active_threshold Integer. Maximum latency (inclusive) for a site to
#'   be considered functionally active (default `4L`).
#'
#' @return A patchwork object with three panels (map | total-sites bar |
#'   active-latency bar) at approximate width ratio 2:1:1.
#'
#' @examples
#' \dontrun{
#' source("R/figures/fig_maps.R")
#' meta <- readr::read_csv("data/snapshots/fluxnet_shuttle_snapshot_20260414T153648.csv")
#' p <- fig_network_subregion_overview(meta, data_yy = NULL)
#' ggplot2::ggsave("review/figures/fig_network_subregion_overview.png",
#'                 p, width = 16, height = 10, units = "in", dpi = 150)
#' }
fig_network_subregion_overview <- function(metadata,
                                           data_yy          = NULL,
                                           current_year     = 2026L,
                                           active_threshold = 4L) {
  for (pkg in c("patchwork", "countrycode")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required. Install with: install.packages('",
           pkg, "')", call. = FALSE)
    }
  }
  if (!exists("fig_map_subregion_sites", mode = "function")) {
    stop(
      "fig_map_subregion_sites() is not loaded. ",
      "Source R/figures/fig_maps.R before calling fig_network_subregion_overview().",
      call. = FALSE
    )
  }

  required_meta <- c("site_id", "last_year")
  missing_meta  <- setdiff(required_meta, names(metadata))
  if (length(missing_meta) > 0L) {
    stop(
      "fig_network_subregion_overview: metadata missing required column(s): ",
      paste(missing_meta, collapse = ", "),
      call. = FALSE
    )
  }

  # --- shared constants -------------------------------------------------------
  latency_colours <- c(
    "0\u20131 years" = "#2ECC71",
    "1\u20132 years" = "#F39C12",
    "2\u20133 years" = "#E67E22",
    "3\u20134 years" = "#E74C3C"
  )
  latency_levels  <- names(latency_colours)
  active_colours  <- c(
    "Functionally active"     = "#2C7FB8",
    "Inactive / high latency" = "#BDBDBD"
  )
  subregion_labels <- c(
    "Latin America and the Caribbean" = "Latin America &\nthe Caribbean",
    "Australia and New Zealand"        = "Australia &\nNew Zealand"
  )

  # --- subregion assignment for all sites (shared between both bars) ----------
  sites <- metadata |>
    dplyr::distinct(.data$site_id, .keep_all = TRUE) |>
    dplyr::select("site_id", "last_year") |>
    dplyr::filter(!is.na(.data$last_year)) |>
    dplyr::mutate(
      last_year = as.integer(.data$last_year),
      latency   = as.integer(current_year) - .data$last_year,
      iso2      = dplyr::if_else(
        substr(.data$site_id, 1L, 2L) == "UK", "GB",
        substr(.data$site_id, 1L, 2L)
      ),
      subregion = countrycode::countrycode(
        .data$iso2, "iso2c", "un.regionsub.name", warn = FALSE
      )
    )

  n_na <- sum(is.na(sites$subregion))
  if (n_na > 0L) {
    message("fig_network_subregion_overview: ", n_na,
            " site(s) with unresolvable subregion labelled 'Unknown'.")
    sites <- dplyr::mutate(
      sites,
      subregion = dplyr::if_else(is.na(.data$subregion), "Unknown",
                                 .data$subregion)
    )
  }

  # Subregion order: ascending total count so highest is at top after y-axis
  # (ggplot2 factor levels: level 1 = bottom, level n = top)
  subregion_order <- sites |>
    dplyr::count(.data$subregion, name = "total") |>
    dplyr::arrange(.data$total) |>
    dplyr::pull(.data$subregion)

  sites <- dplyr::mutate(
    sites,
    subregion = factor(.data$subregion, levels = subregion_order)
  )

  # --- Left panel: 2025 choropleth map ----------------------------------------
  p_map <- tryCatch(
    fig_map_subregion_sites(metadata, year_cutoffs = 2025L,
                            metric = "count", add_dots = TRUE),
    error = function(e) {
      warning("fig_network_subregion_overview: map panel failed: ",
              conditionMessage(e), call. = FALSE)
      NULL
    }
  )

  # --- Middle panel: all sites, active vs inactive ----------------------------
  middle_data <- dplyr::mutate(
    sites,
    status = factor(
      dplyr::if_else(.data$latency <= active_threshold,
                     "Functionally active", "Inactive / high latency"),
      levels = c("Functionally active", "Inactive / high latency")
    )
  )

  p_middle <- ggplot2::ggplot(
    middle_data,
    ggplot2::aes(y = .data$subregion, fill = .data$status)
  ) +
    ggplot2::geom_bar(position = "stack", width = 0.7,
                      colour = "white", linewidth = 0.2) +
    ggplot2::scale_fill_manual(values = active_colours, name = "Status",
                               drop = FALSE) +
    ggplot2::scale_x_continuous(
      breaks = scales::pretty_breaks(n = 5),
      expand = ggplot2::expansion(mult = c(0, 0.04))
    ) +
    ggplot2::scale_y_discrete(
      labels = function(x) {
        x <- dplyr::recode(x, !!!subregion_labels)
        scales::label_wrap(22)(x)
      }
    ) +
    ggplot2::labs(x = "Total sites", y = NULL) +
    fluxnet_theme(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text.y     = ggplot2::element_text(size = 10),
      plot.margin     = ggplot2::margin(t = 4, r = 0, b = 4, l = 4, unit = "pt")
    )

  # --- Right panel: active sites by latency bin --------------------------------
  right_data <- sites |>
    dplyr::filter(.data$latency >= 0L, .data$latency <= active_threshold) |>
    dplyr::mutate(
      latency_bin = factor(
        dplyr::case_when(
          .data$latency <= 1L ~ "0\u20131 years",
          .data$latency <= 2L ~ "1\u20132 years",
          .data$latency <= 3L ~ "2\u20133 years",
          .data$latency <= 4L ~ "3\u20134 years"
        ),
        levels = latency_levels
      )
    )

  p_right <- ggplot2::ggplot(
    right_data,
    ggplot2::aes(y = .data$subregion, fill = .data$latency_bin)
  ) +
    ggplot2::geom_bar(position = "stack", width = 0.7,
                      colour = "white", linewidth = 0.2) +
    ggplot2::scale_fill_manual(values = latency_colours, name = "Latency",
                               drop = FALSE) +
    ggplot2::scale_x_continuous(
      breaks = scales::pretty_breaks(n = 5),
      expand = ggplot2::expansion(mult = c(0, 0.04))
    ) +
    ggplot2::labs(x = "Functionally active sites", y = NULL) +
    fluxnet_theme(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text.y     = ggplot2::element_blank(),
      axis.ticks.y    = ggplot2::element_blank(),
      plot.margin     = ggplot2::margin(t = 4, r = 4, b = 4, l = 0, unit = "pt")
    )

  # --- Assemble ---------------------------------------------------------------
  bars <- (p_middle | p_right) +
    patchwork::plot_layout(widths = c(1, 1), guides = "collect") &
    ggplot2::theme(legend.position = "bottom")

  if (!is.null(p_map)) {
    pw <- patchwork::wrap_elements(full = p_map) /
          patchwork::wrap_elements(full = bars)
    pw <- pw + patchwork::plot_layout(heights = c(1, 2.5))
  } else {
    pw <- bars
  }

  pw +
    patchwork::plot_annotation(
      title = paste0(
        "FLUXNET network coverage and data latency by UN subregion (",
        current_year - 1L, ")"
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

# ---- fig_latency_by_subregion -----------------------------------------------

#' Data latency by UN subregion — functionally active sites only
#'
#' Calculates per-site latency as `current_year - last_year` from the snapshot
#' metadata, retains only functionally active sites
#' (`latency <= active_threshold`), assigns each site to a UN M.49 subregion
#' via [countrycode::countrycode()], and plots a horizontal stacked bar chart
#' with a traffic-light fill scheme showing how current the data is within
#' each subregion.
#'
#' **Subregion assignment:** uses the first two characters of `site_id` as an
#' ISO 3166-1 alpha-2 country code. FLUXNET UK-* sites use the informal prefix
#' "UK" rather than the ISO standard "GB"; these are silently recoded to "GB"
#' before the lookup. Any remaining unmatched prefixes are placed in an
#' "Unknown" subregion and flagged with a message.
#'
#' **Latency bins (traffic light):**
#' - `0-1 years` (green `#2ECC71`): `latency <= 1`
#' - `1-2 years` (amber `#F39C12`): `latency == 2`
#' - `2-3 years` (dark orange `#E67E22`): `latency == 3`
#' - `3-4 years` (red-orange `#E74C3C`): `latency == 4`
#'
#' **Potential integration options (for methods / layout discussion):**
#' 1. *Third panel in a 3-panel network growth stack* — append below
#'    [fig_network_active_proportion()] as the bottom panel of a
#'    cumulative-count / active-proportion / latency-by-subregion triptych.
#' 2. *Standalone supplementary figure* — include as a supplementary figure
#'    describing the data currency of the network at submission.
#' 3. *Inset map* — pair with a geographic dot map coloured by latency bin
#'    using the same traffic-light palette, shown as an inset alongside the
#'    bar chart.
#'
#' @param metadata Data frame. Snapshot CSV (one row per site) with columns
#'   `site_id` and `last_year`.
#' @param current_year Integer. The reference year for latency calculation
#'   (default `2026L`).
#' @param active_threshold Integer. Maximum latency (inclusive) for a site to
#'   be considered functionally active (default `4L`).
#'
#' @return A ggplot object: horizontal stacked bar chart, x-axis = site count,
#'   y-axis = UN subregion ordered by total active sites (descending from top),
#'   fill = latency bin (traffic-light palette).
#'
#' @examples
#' \dontrun{
#' meta <- readr::read_csv("data/snapshots/fluxnet_shuttle_snapshot_20260414T153648.csv")
#' p <- fig_latency_by_subregion(meta)
#' print(p)
#' }
fig_latency_by_subregion <- function(metadata,
                                     current_year     = 2026L,
                                     active_threshold = 4L) {
  if (!requireNamespace("countrycode", quietly = TRUE)) {
    stop(
      "Package 'countrycode' is required for fig_latency_by_subregion(). ",
      "Install with: install.packages('countrycode')",
      call. = FALSE
    )
  }

  required_meta <- c("site_id", "last_year")
  missing_meta  <- setdiff(required_meta, names(metadata))
  if (length(missing_meta) > 0L) {
    stop(
      "fig_latency_by_subregion: metadata missing required column(s): ",
      paste(missing_meta, collapse = ", "),
      call. = FALSE
    )
  }

  latency_colours <- c(
    "0\u20131 years" = "#2ECC71",
    "1\u20132 years" = "#F39C12",
    "2\u20133 years" = "#E67E22",
    "3\u20134 years" = "#E74C3C"
  )
  latency_levels <- names(latency_colours)

  subregion_labels <- c(
    "Latin America and the Caribbean" = "Latin America &\nthe Caribbean",
    "Australia and New Zealand"        = "Australia &\nNew Zealand"
  )

  # --- compute latency, recode UK → GB, map to subregion ---------------------
  df <- metadata |>
    dplyr::distinct(.data$site_id, .keep_all = TRUE) |>
    dplyr::select("site_id", "last_year") |>
    dplyr::filter(!is.na(.data$last_year)) |>
    dplyr::mutate(
      last_year = as.integer(.data$last_year),
      latency   = as.integer(current_year) - .data$last_year,
      iso2      = dplyr::if_else(
        substr(.data$site_id, 1L, 2L) == "UK", "GB",
        substr(.data$site_id, 1L, 2L)
      ),
      subregion = countrycode::countrycode(
        .data$iso2, "iso2c", "un.regionsub.name", warn = FALSE
      )
    )

  # Report sites with unresolved subregion
  n_na <- sum(is.na(df$subregion))
  if (n_na > 0L) {
    message(
      "fig_latency_by_subregion: ", n_na, " site(s) could not be assigned to a ",
      "UN subregion and are labelled 'Unknown': ",
      paste(df$site_id[is.na(df$subregion)], collapse = ", ")
    )
    df <- dplyr::mutate(
      df, subregion = dplyr::if_else(is.na(.data$subregion), "Unknown",
                                     .data$subregion)
    )
  }

  # --- filter to functionally active sites and bin latency -------------------
  active <- df |>
    dplyr::filter(.data$latency >= 0L, .data$latency <= active_threshold) |>
    dplyr::mutate(
      latency_bin = dplyr::case_when(
        .data$latency <= 1L ~ "0\u20131 years",
        .data$latency <= 2L ~ "1\u20132 years",
        .data$latency <= 3L ~ "2\u20133 years",
        .data$latency <= 4L ~ "3\u20134 years"
      ),
      latency_bin = factor(.data$latency_bin, levels = latency_levels)
    )

  if (nrow(active) == 0L) {
    warning("fig_latency_by_subregion: no functionally active sites — returning empty plot.",
            call. = FALSE)
    return(ggplot2::ggplot() + fluxnet_theme() +
             ggplot2::labs(title = "Latency by subregion — no data"))
  }

  # --- order subregions by total active sites, ascending so highest is top ---
  # (coord_flip() reverses y-axis so ascending factor order → highest at top)
  subregion_order <- active |>
    dplyr::count(.data$subregion, name = "total") |>
    dplyr::arrange(.data$total) |>
    dplyr::pull(.data$subregion)

  active <- dplyr::mutate(
    active,
    subregion = factor(.data$subregion, levels = subregion_order)
  )

  n_active   <- nrow(active)
  n_subregion <- dplyr::n_distinct(active$subregion)

  caption_text <- paste0(
    "Functionally active sites only (data submitted within ",
    active_threshold, " years). ",
    "Latency = ", current_year, " \u2212 last data year."
  )

  ggplot2::ggplot(
    active,
    ggplot2::aes(y = .data$subregion, fill = .data$latency_bin)
  ) +
    ggplot2::geom_bar(position = "stack", width = 0.7, colour = "white",
                      linewidth = 0.2) +
    ggplot2::scale_fill_manual(values = latency_colours, name = "Latency",
                               drop = FALSE) +
    ggplot2::scale_x_continuous(
      breaks = scales::pretty_breaks(n = 6),
      expand = ggplot2::expansion(mult = c(0, 0.04))
    ) +
    ggplot2::scale_y_discrete(
      labels = function(x) {
        x <- dplyr::recode(x, !!!subregion_labels)
        scales::label_wrap(22)(x)
      }
    ) +
    ggplot2::labs(
      title   = "Data latency by UN subregion — functionally active sites",
      subtitle = paste0("n\u2009=\u2009", n_active, " sites across ",
                        n_subregion, " subregions"),
      x       = "Number of sites",
      y       = NULL,
      caption = caption_text
    ) +
    fluxnet_theme(base_size = 12) +
    ggplot2::theme(
      legend.position  = "right",
      axis.text.y      = ggplot2::element_text(size = 11),
      plot.caption     = ggplot2::element_text(size = 9, colour = "grey40",
                                               hjust = 0)
    )
}
