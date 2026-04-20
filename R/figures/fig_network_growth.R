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

# ---- DUR_STYLE --------------------------------------------------------------

#' Shared visual parameters for all duration histogram figures
#'
#' A named list used as the default \code{style} argument to
#' \code{\link{fig_duration_historical}}.  Override individual elements by
#' passing a modified copy.
#'
#' @format A named list:
#' \describe{
#'   \item{width_in, height_in}{Default ggsave dimensions in inches.}
#'   \item{base_size}{Base font size passed to \code{\link{fluxnet_theme}}.}
#'   \item{bin_width}{Histogram bin width in years.  Bins are anchored at zero
#'     (\code{boundary = 0}) so edges fall at 0, 1, 2, \ldots}
#'   \item{xlim, ylim}{\code{NULL} — set at runtime from the maximum record
#'     length and maximum bin count across all 9 datasets so that every panel
#'     shares identical axes.}
#'   \item{detail_x, detail_y}{Inset detail-text NDC anchor (for reference;
#'     text is placed top-right via \code{annotate()}).}
#' }
#' @export
DUR_STYLE <- list(
  width_in  = 14,
  height_in = 7,
  base_size = 24,
  bin_width = 1,          # years; bins anchored at 0 → edges at 0, 1, 2, …
  xlim      = NULL,       # set at runtime: 0 to max record length across all datasets
  ylim      = NULL,       # set at runtime: 0 to max bin count across all datasets
  detail_x  = 0.02,
  detail_y  = 0.98
)

# ---- fig_duration_historical ------------------------------------------------

#' Single-panel deployment duration histogram for any FLUXNET site list
#'
#' Computes per-site record length as \code{snapshot_year - first_year} and
#' plots a uniform white-bar histogram using a shared bin width and axis limits.
#' Designed to be called nine times from
#' \code{scripts/generate_duration_histograms.R} with identical
#' \code{style$xlim} and \code{style$ylim} so all panels are directly
#' comparable.
#'
#' All bars use a fixed white fill with black outline — no active/inactive
#' classification or legend.  X-axis breaks are aligned to bin edges (multiples
#' of \code{style$bin_width}) and anchored at zero via \code{boundary = 0}.
#'
#' @param site_meta Data frame.  Must contain \code{site_id},
#'   \code{first_year}, and \code{last_year}.
#' @param snapshot_year Integer.  Year against which record length is computed:
#'   \code{record_length = snapshot_year - first_year}.  Sites with
#'   \code{first_year > snapshot_year} are excluded.
#' @param detail_label Character scalar or \code{NULL}.  Dataset label shown in
#'   the top-right inset text.
#' @param is_shuttle Logical.  If \code{TRUE}, site-years are counted from
#'   \code{presence_df} (rows where \code{has_data = TRUE} and
#'   \code{year <= snapshot_year}).  If \code{FALSE} (default), site-years are
#'   computed as \code{sum(last_year - first_year + 1)} across all sites in the
#'   filtered dataset — the appropriate formula for historical site lists whose
#'   records are fully contained within their snapshot window.
#' @param presence_df Data frame or \code{NULL}.  Required when
#'   \code{is_shuttle = TRUE}.  Must contain columns \code{site_id},
#'   \code{year} (integer), and \code{has_data} (logical).  Typically
#'   \code{data/snapshots/site_year_data_presence.csv}.
#' @param style Named list.  Visual parameters; defaults to
#'   \code{\link{DUR_STYLE}}.  Set \code{style$xlim} and \code{style$ylim}
#'   before calling — compute them once from all 9 datasets in the generate
#'   script.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' meta       <- readr::read_csv(
#'   "data/snapshots/fluxnet_shuttle_snapshot_20260414T154430.csv"
#' )
#' style      <- DUR_STYLE
#' style$xlim <- c(0, 39)
#' style$ylim <- c(0, 200)
#' p <- fig_duration_historical(meta, 2025L, "FLUXNET Shuttle 2025",
#'                               style = style)
#' print(p)
#' }
#' @export
fig_duration_historical <- function(site_meta,
                                     snapshot_year,
                                     detail_label = NULL,
                                     is_shuttle   = FALSE,
                                     presence_df  = NULL,
                                     style        = DUR_STYLE) {

  required_cols <- c("site_id", "first_year", "last_year")
  missing_cols  <- setdiff(required_cols, names(site_meta))
  if (length(missing_cols) > 0L) {
    stop(
      "fig_duration_historical: site_meta is missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  sites <- site_meta |>
    dplyr::distinct(.data$site_id, .keep_all = TRUE) |>
    dplyr::select("site_id", "first_year", "last_year") |>
    dplyr::filter(
      !is.na(.data$first_year), !is.na(.data$last_year),
      as.integer(.data$first_year) <= as.integer(snapshot_year)
    ) |>
    dplyr::mutate(
      first_year    = as.integer(.data$first_year),
      last_year     = as.integer(.data$last_year),
      record_length = as.integer(snapshot_year) - .data$first_year
    )

  n_sites <- nrow(sites)

  if (n_sites == 0L) {
    warning(
      "fig_duration_historical: no sites with first_year <= snapshot_year (",
      snapshot_year, ") — returning empty plot.",
      call. = FALSE
    )
    return(ggplot2::ggplot() +
             fluxnet_theme(style$base_size) +
             ggplot2::labs(title = "No data"))
  }

  # Compute site-years: Shuttle uses presence_df (observed valid data);
  # historical datasets use span formula (last_year - first_year + 1).
  n_site_years <- if (is_shuttle) {
    if (is.null(presence_df)) {
      stop(
        "fig_duration_historical: is_shuttle = TRUE requires presence_df.",
        call. = FALSE
      )
    }
    sum(
      presence_df$site_id %in% sites$site_id &
        presence_df$year   <= as.integer(snapshot_year) &
        presence_df$has_data
    )
  } else {
    sum(sites$last_year - sites$first_year + 1L, na.rm = TRUE)
  }

  detail_text <- paste0(
    if (!is.null(detail_label)) paste0(detail_label, "\n") else "",
    "N = ", n_sites, " sites | ", n_site_years, " site-years"
  )
  annot_size <- style$base_size * 0.25   # mm; ≈ base_size pt for base_size = 24

  # X-axis breaks aligned to bin edges (multiples of bin_width from 0)
  xlim_max <- if (!is.null(style$xlim)) style$xlim[2] else
    max(sites$record_length) + style$bin_width
  x_breaks <- seq(0, xlim_max, by = style$bin_width)

  p <- ggplot2::ggplot(sites, ggplot2::aes(x = .data$record_length)) +
    ggplot2::geom_histogram(
      fill     = "white",
      color    = "black",
      binwidth = style$bin_width,
      boundary = 0
    ) +
    ggplot2::scale_x_continuous(
      breaks = x_breaks,
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::scale_y_continuous(
      breaks = scales::pretty_breaks(n = 5),
      expand = ggplot2::expansion(mult = c(0, 0.02))
    ) +
    ggplot2::labs(
      x = "Record length (years)",
      y = "Sites"
    ) +
    fluxnet_theme(style$base_size) +
    ggplot2::theme(legend.position = "none") +
    # Detail text: top-right
    ggplot2::annotate(
      "text",
      x = Inf, y = Inf,
      label    = detail_text,
      hjust    = 1.05, vjust = 1.4,
      size     = annot_size,
      fontface = "bold"
    )

  # Apply shared axis limits when set
  if (!is.null(style$xlim) || !is.null(style$ylim)) {
    p <- p + ggplot2::coord_cartesian(
      xlim = style$xlim,
      ylim = style$ylim
    )
  }

  p
}

# ---- fig_duration_overlay ---------------------------------------------------

#' Three-panel overlay: Shuttle snapshots vs historical datasets
#'
#' Produces a patchwork figure with three vertically stacked panels, one per
#' historical reference year (2000, 2007, 2015). Each panel overlays two
#' histograms:
#' \itemize{
#'   \item \strong{Shuttle snapshot} (grey filled bars): sites from the Shuttle
#'     dataset with \code{first_year <= snapshot_year}; record length =
#'     \code{snapshot_year - first_year}.
#'   \item \strong{Historical dataset} (red outline bars): Marconi (2000),
#'     La Thuile (2007), or FLUXNET2015 (2015); same record-length definition.
#' }
#'
#' All panels share a common Y axis limit derived from the maximum 1-year bin
#' count across all six datasets within \code{xlim = c(0, 20)}. The X axis is
#' clipped to 20 years because none of the historical datasets or pre-2015
#' Shuttle snapshots contain longer records.
#'
#' NOTE: historical site lists are comparison/illustration data only —
#' non-Shuttle primary data (see CLAUDE.md §1).
#'
#' @param shuttle_meta Data frame. Shuttle snapshot CSV (one row per site)
#'   with columns \code{site_id} and \code{first_year}.
#' @param sites_marconi Data frame. Marconi site list with \code{site_id} and
#'   \code{first_year} (joined from \code{data/snapshots/years_marconi.csv}).
#' @param sites_la_thuile Data frame. La Thuile site list with \code{site_id}
#'   and \code{first_year}.
#' @param sites_fluxnet2015 Data frame. FLUXNET2015 site list with
#'   \code{site_id} and \code{first_year}.
#' @param base_size Integer. Base font size passed to \code{\link{fluxnet_theme}}
#'   (default \code{36L}).
#'
#' @return A patchwork object: three vertically stacked histogram panels with a
#'   shared legend at the bottom and single X/Y axis labels.
#'
#' @examples
#' \dontrun{
#' shuttle <- readr::read_csv(
#'   "data/snapshots/fluxnet_shuttle_snapshot_20260414T154430.csv"
#' )
#' marconi <- readr::read_csv("data/snapshots/sites_marconi_clean.csv") |>
#'   dplyr::left_join(readr::read_csv("data/snapshots/years_marconi.csv"),
#'                    by = "site_id")
#' # ... load la_thuile and fluxnet2015 similarly ...
#' p <- fig_duration_overlay(shuttle, marconi, la_thuile, fluxnet2015)
#' print(p)
#' }
#' @export
fig_duration_overlay <- function(shuttle_meta,
                                 sites_marconi,
                                 sites_la_thuile,
                                 sites_fluxnet2015,
                                 base_size = 36L) {

  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop(
      "Package 'patchwork' is required for fig_duration_overlay(). ",
      "Install with: install.packages('patchwork')",
      call. = FALSE
    )
  }

  for (arg in list(shuttle_meta, sites_marconi, sites_la_thuile, sites_fluxnet2015)) {
    missing_cols <- setdiff(c("site_id", "first_year"), names(arg))
    if (length(missing_cols) > 0L) {
      stop(
        "fig_duration_overlay: input data frame missing column(s): ",
        paste(missing_cols, collapse = ", "),
        call. = FALSE
      )
    }
  }

  # Helper: filter to snapshot year, deduplicate, compute record length
  .mk <- function(df, snap_yr) {
    df |>
      dplyr::distinct(.data$site_id, .keep_all = TRUE) |>
      dplyr::filter(!is.na(.data$first_year),
                    as.integer(.data$first_year) <= snap_yr) |>
      dplyr::mutate(record_length = snap_yr - as.integer(.data$first_year))
  }

  s2000  <- .mk(shuttle_meta,      2000L)
  s2007  <- .mk(shuttle_meta,      2007L)
  s2015  <- .mk(shuttle_meta,      2015L)
  d_marc <- .mk(sites_marconi,     2000L)
  d_lath <- .mk(sites_la_thuile,   2007L)
  d_f15  <- .mk(sites_fluxnet2015, 2015L)

  # Shared Y limit: max 1-year bin count within [0, 20] across all 6 datasets
  .max_in_range <- function(df) {
    r <- df$record_length[df$record_length >= 0L & df$record_length <= 20L]
    if (length(r) == 0L) return(0L)
    max(tabulate(r + 1L))   # tabulate is 1-indexed; +1 maps 0-year records to bin 1
  }
  ylim_max <- ceiling(max(
    .max_in_range(s2000), .max_in_range(s2007), .max_in_range(s2015),
    .max_in_range(d_marc), .max_in_range(d_lath), .max_in_range(d_f15)
  ) * 1.08)

  annot_size <- base_size * 0.28

  # Legend key ordering: ggplot2 sorts factor levels alphabetically.
  # "Historical dataset" (H) sorts before "Shuttle snapshot" (S), so
  # override.aes index 1 = Historical, index 2 = Shuttle.
  fill_guide <- ggplot2::guide_legend(
    override.aes = list(
      fill      = c(NA,        "grey70"),  # 1=Historical: no fill; 2=Shuttle: grey
      colour    = c("#E74C3C", "grey50"),  # 1=Historical: red;    2=Shuttle: grey50
      linewidth = c(1.2,        0.3)       # 1=Historical: thick;  2=Shuttle: thin
    )
  )

  # Internal panel builder — returns a ggplot; axis suppression applied after
  .make_panel <- function(shuttle_df, hist_df,
                          inset_line1, inset_line2,
                          show_x, show_y_title,
                          top_mar, bot_mar) {

    p <- ggplot2::ggplot() +
      # Background: Shuttle snapshot — grey filled bars
      ggplot2::geom_histogram(
        data = shuttle_df,
        ggplot2::aes(x      = .data$record_length,
                     fill   = "Shuttle snapshot",
                     colour = "Shuttle snapshot"),
        binwidth = 1, boundary = 0, linewidth = 0.3
      ) +
      # Foreground: historical dataset — red outline, no fill
      ggplot2::geom_histogram(
        data = hist_df,
        ggplot2::aes(x      = .data$record_length,
                     fill   = "Historical dataset",
                     colour = "Historical dataset"),
        binwidth = 1, boundary = 0, linewidth = 1.2
      ) +
      ggplot2::scale_fill_manual(
        values = c("Shuttle snapshot"   = "grey70",
                   "Historical dataset" = NA),
        name  = NULL,
        guide = fill_guide
      ) +
      ggplot2::scale_colour_manual(
        values = c("Shuttle snapshot"   = "grey50",
                   "Historical dataset" = "#E74C3C"),
        name  = NULL,
        guide = "none"   # merged into fill legend via override.aes above
      ) +
      ggplot2::scale_x_continuous(
        breaks = seq(0, 20, by = 5),
        expand = ggplot2::expansion(mult = c(0, 0.01))
      ) +
      ggplot2::scale_y_continuous(
        breaks = scales::pretty_breaks(n = 4),
        expand = ggplot2::expansion(mult = c(0, 0.02))
      ) +
      ggplot2::coord_cartesian(xlim = c(0, 20), ylim = c(0, ylim_max)) +
      ggplot2::labs(x = "Record length (years)", y = "Sites") +
      fluxnet_theme(base_size = base_size) +
      ggplot2::annotate(
        "text", x = Inf, y = Inf,
        label    = paste0(inset_line1, "\n", inset_line2),
        hjust = 1.05, vjust = 1.35,
        size  = annot_size, fontface = "bold"
      ) +
      ggplot2::theme(
        legend.position = "none",
        plot.margin     = ggplot2::margin(top_mar, 5, bot_mar, 5)
      )

    # Suppress axis elements only via element_blank() to avoid theme merge
    # conflicts with the element_markdown() axis titles in fluxnet_theme().
    if (!show_x) {
      p <- p + ggplot2::theme(
        axis.text.x  = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank()
      )
    }
    if (!show_y_title) {
      p <- p + ggplot2::theme(axis.title.y = ggplot2::element_blank())
    }

    p
  }

  n_fmt <- function(n) paste0("n\u2009=\u2009", n)   # thin-space before =

  p1 <- .make_panel(
    s2000, d_marc,
    "2000",
    paste0("Shuttle 2000 (", n_fmt(nrow(s2000)), ")  \u2502  ",
           "Marconi (", n_fmt(nrow(d_marc)), ")"),
    show_x = FALSE, show_y_title = FALSE, top_mar = 5, bot_mar = 0
  )
  p2 <- .make_panel(
    s2007, d_lath,
    "2007",
    paste0("Shuttle 2007 (", n_fmt(nrow(s2007)), ")  \u2502  ",
           "La Thuile (", n_fmt(nrow(d_lath)), ")"),
    show_x = FALSE, show_y_title = TRUE, top_mar = 0, bot_mar = 0
  )
  p3 <- .make_panel(
    s2015, d_f15,
    "2015",
    paste0("Shuttle 2015 (", n_fmt(nrow(s2015)), ")  \u2502  ",
           "FLUXNET2015 (", n_fmt(nrow(d_f15)), ")"),
    show_x = TRUE, show_y_title = FALSE, top_mar = 0, bot_mar = 5
  )

  (p1 / p2 / p3) +
    patchwork::plot_layout(guides = "collect") &
    ggplot2::theme(
      legend.position  = "bottom",
      legend.direction = "horizontal",
      legend.text      = ggplot2::element_text(size = as.integer(base_size) - 4L),
      legend.key.size  = grid::unit(18, "pt")
    )
}

# ---- fig_siteyears_by_year --------------------------------------------------

#' Site-years per calendar year: Shuttle vs historical datasets
#'
#' Plots the number of sites contributing data in each calendar year as a
#' single-panel figure. The FLUXNET Shuttle network is shown as a grey filled
#' area (observed site-years from \code{presence_df}). Three historical
#' datasets are overlaid as coloured lines derived from their \code{first_year}
#' to \code{last_year} ranges:
#'
#' \itemize{
#'   \item Marconi (green \code{#2ECC71})
#'   \item La Thuile (red \code{#E74C3C})
#'   \item FLUXNET2015 (blue \code{#3498DB})
#' }
#'
#' Vertical dashed lines mark the historical release years (2000, 2007, 2015).
#' Each historical line is labelled directly at its peak year via
#' \code{geom_label()} — no separate legend.
#'
#' NOTE: historical site lists are comparison/illustration data only —
#' non-Shuttle primary data (see CLAUDE.md §1).
#'
#' @param presence_df Data frame. \code{data/snapshots/site_year_data_presence.csv}
#'   with columns \code{site_id}, \code{year} (integer), and \code{has_data}
#'   (logical). Only rows where \code{has_data == TRUE} are counted.
#' @param sites_marconi Data frame. Marconi site list joined with
#'   \code{years_marconi.csv}; must contain \code{site_id}, \code{first_year},
#'   and \code{last_year}.
#' @param sites_la_thuile Data frame. La Thuile site list joined with
#'   \code{years_la_thuile.csv}; same columns required.
#' @param sites_fluxnet2015 Data frame. FLUXNET2015 site list joined with
#'   \code{years_fluxnet2015.csv}; same columns required.
#' @param year_range Integer vector. Calendar years to display on the X axis
#'   (default \code{1991:2024}).
#' @param base_size Integer. Base font size passed to \code{\link{fluxnet_theme}}
#'   (default \code{36L}).
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' presence <- readr::read_csv("data/snapshots/site_year_data_presence.csv")
#' marconi  <- readr::read_csv("data/snapshots/sites_marconi_clean.csv") |>
#'   dplyr::left_join(readr::read_csv("data/snapshots/years_marconi.csv"),
#'                    by = "site_id")
#' # ... load la_thuile and fluxnet2015 similarly ...
#' p <- fig_siteyears_by_year(presence, marconi, la_thuile, fluxnet2015)
#' print(p)
#' }
#' @export
fig_siteyears_by_year <- function(presence_df,
                                   sites_marconi,
                                   sites_la_thuile,
                                   sites_fluxnet2015,
                                   year_range = 1991:2024,
                                   base_size  = 36L) {

  yr_min <- min(year_range)
  yr_max <- max(year_range)

  # ---- Shuttle: observed site-years from presence_df -----------------------
  shuttle_annual <- presence_df |>
    dplyr::filter(.data$has_data,
                  .data$year >= yr_min,
                  .data$year <= yr_max) |>
    dplyr::count(.data$year, name = "n")

  # ---- Historical: expand site records to one row per active year ----------
  # Uses first_year / last_year from the years lookup CSVs. For each site,
  # every year in [first_year, last_year] is counted as one contributing site.
  .expand_years <- function(df) {
    valid <- df |>
      dplyr::distinct(.data$site_id, .keep_all = TRUE) |>
      dplyr::filter(!is.na(.data$first_year), !is.na(.data$last_year)) |>
      dplyr::mutate(first_year = as.integer(.data$first_year),
                    last_year  = as.integer(.data$last_year))
    if (nrow(valid) == 0L) {
      return(data.frame(year = integer(0L), n = integer(0L)))
    }
    # Expand each site's range to individual year rows (base R, no purrr dep)
    do.call(rbind, Map(
      function(fy, ly) data.frame(year = seq.int(fy, ly)),
      valid$first_year, valid$last_year
    )) |>
      dplyr::filter(.data$year >= yr_min, .data$year <= yr_max) |>
      dplyr::count(.data$year, name = "n")
  }

  hist_colours <- c(
    "Marconi"     = "#2ECC71",
    "La Thuile"   = "#E74C3C",
    "FLUXNET2015" = "#3498DB"
  )

  hist_all <- dplyr::bind_rows(
    dplyr::mutate(.expand_years(sites_marconi),     dataset = "Marconi"),
    dplyr::mutate(.expand_years(sites_la_thuile),   dataset = "La Thuile"),
    dplyr::mutate(.expand_years(sites_fluxnet2015), dataset = "FLUXNET2015")
  ) |>
    dplyr::mutate(dataset = factor(.data$dataset,
                                   levels = names(hist_colours)))

  # Peak year per dataset — label anchor for geom_label()
  peak_labels <- hist_all |>
    dplyr::group_by(.data$dataset) |>
    dplyr::slice_max(.data$n, n = 1L, with_ties = FALSE) |>
    dplyr::ungroup()

  ggplot2::ggplot() +
    # Vertical release-year reference lines (drawn behind all data)
    ggplot2::geom_vline(
      xintercept = c(2000L, 2007L, 2015L),
      linetype   = "dashed", colour = "grey55", linewidth = 0.7
    ) +
    # Shuttle background: grey filled area
    ggplot2::geom_area(
      data = shuttle_annual,
      ggplot2::aes(x = .data$year, y = .data$n),
      fill      = "grey70",
      colour    = "grey50",
      linewidth = 0.4,
      alpha     = 0.85
    ) +
    # Historical dataset lines
    ggplot2::geom_line(
      data = hist_all,
      ggplot2::aes(x      = .data$year,
                   y      = .data$n,
                   colour = .data$dataset),
      linewidth = 1.5
    ) +
    # Direct labels at peak year
    ggplot2::geom_label(
      data = peak_labels,
      ggplot2::aes(x      = .data$year,
                   y      = .data$n,
                   label  = .data$dataset,
                   colour = .data$dataset),
      fill        = "white",
      size        = base_size * 0.22,
      linewidth   = 0.5,
      fontface    = "bold",
      vjust       = -0.4,
      show.legend = FALSE
    ) +
    ggplot2::scale_colour_manual(values = hist_colours, guide = "none") +
    ggplot2::scale_x_continuous(
      limits = c(yr_min, yr_max),
      breaks = seq(1990L, 2025L, by = 5L),
      expand = ggplot2::expansion(mult = c(0.01, 0.01))
    ) +
    ggplot2::scale_y_continuous(
      breaks = scales::pretty_breaks(n = 6),
      expand = ggplot2::expansion(mult = c(0, 0.18))  # headroom for labels
    ) +
    ggplot2::labs(
      x = "Year",
      y = "Site-years per year"
    ) +
    fluxnet_theme(base_size = base_size)
}

# ---- fig_siteyears_by_year_igbp ---------------------------------------------

#' Site-years per calendar year: IGBP-stacked Shuttle area with historical overlays
#'
#' Variant of \code{\link{fig_siteyears_by_year}} in which the Shuttle
#' background is a stacked area chart coloured by IGBP class rather than a
#' plain grey fill. Historical dataset lines (Marconi, La Thuile, FLUXNET2015)
#' and all other visual elements are identical to the base function.
#'
#' The IGBP class for each site is taken from \code{shuttle_meta} (the
#' \code{igbp} column of the Shuttle snapshot CSV). Sites in \code{presence_df}
#' that cannot be matched to an IGBP class are silently dropped.
#'
#' NOTE: historical site lists are comparison/illustration data only —
#' non-Shuttle primary data (see CLAUDE.md §1).
#'
#' @param presence_df Data frame. \code{data/snapshots/site_year_data_presence.csv}
#'   with columns \code{site_id}, \code{year} (integer), and \code{has_data}
#'   (logical).
#' @param shuttle_meta Data frame. Shuttle snapshot CSV (one row per site) with
#'   columns \code{site_id} and \code{igbp}.
#' @param sites_marconi Data frame. Marconi site list joined with
#'   \code{years_marconi.csv}; must contain \code{site_id}, \code{first_year},
#'   and \code{last_year}.
#' @param sites_la_thuile Data frame. La Thuile site list; same columns.
#' @param sites_fluxnet2015 Data frame. FLUXNET2015 site list; same columns.
#' @param year_range Integer vector. Calendar years to display (default
#'   \code{1991:2024}).
#' @param base_size Integer. Base font size for \code{\link{fluxnet_theme}}
#'   (default \code{36L}).
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' presence <- readr::read_csv("data/snapshots/site_year_data_presence.csv")
#' shuttle  <- readr::read_csv(
#'   "data/snapshots/fluxnet_shuttle_snapshot_20260414T154430.csv"
#' )
#' # ... load marconi, la_thuile, fluxnet2015 ...
#' p <- fig_siteyears_by_year_igbp(presence, shuttle, marconi, la_thuile, f15)
#' print(p)
#' }
#' @export
fig_siteyears_by_year_igbp <- function(presence_df,
                                        shuttle_meta,
                                        sites_marconi,
                                        sites_la_thuile,
                                        sites_fluxnet2015,
                                        year_range = 1991:2024,
                                        base_size  = 36L) {

  yr_min <- min(year_range)
  yr_max <- max(year_range)

  # ---- Shuttle: site-years per year per IGBP class -------------------------
  igbp_lookup <- shuttle_meta |>
    dplyr::distinct(.data$site_id, .keep_all = TRUE) |>
    dplyr::select("site_id", "igbp") |>
    dplyr::filter(!is.na(.data$igbp), .data$igbp %in% IGBP_order)

  shuttle_igbp_annual <- presence_df |>
    dplyr::filter(.data$has_data,
                  .data$year >= yr_min,
                  .data$year <= yr_max) |>
    dplyr::left_join(igbp_lookup, by = "site_id") |>
    dplyr::filter(!is.na(.data$igbp)) |>
    dplyr::mutate(igbp = factor(.data$igbp, levels = IGBP_order)) |>
    dplyr::count(.data$year, .data$igbp, name = "n") |>
    # Ensure all year × IGBP combinations are present so stacking is complete
    tidyr::complete(year, igbp, fill = list(n = 0L))

  # ---- Historical: expand site records to one row per active year ----------
  # Identical logic to fig_siteyears_by_year() — base R, no purrr dependency.
  .expand_years <- function(df) {
    valid <- df |>
      dplyr::distinct(.data$site_id, .keep_all = TRUE) |>
      dplyr::filter(!is.na(.data$first_year), !is.na(.data$last_year)) |>
      dplyr::mutate(first_year = as.integer(.data$first_year),
                    last_year  = as.integer(.data$last_year))
    if (nrow(valid) == 0L) {
      return(data.frame(year = integer(0L), n = integer(0L)))
    }
    do.call(rbind, Map(
      function(fy, ly) data.frame(year = seq.int(fy, ly)),
      valid$first_year, valid$last_year
    )) |>
      dplyr::filter(.data$year >= yr_min, .data$year <= yr_max) |>
      dplyr::count(.data$year, name = "n")
  }

  hist_colours <- c(
    "Marconi"     = "#2ECC71",
    "La Thuile"   = "#E74C3C",
    "FLUXNET2015" = "#3498DB"
  )

  hist_all <- dplyr::bind_rows(
    dplyr::mutate(.expand_years(sites_marconi),     dataset = "Marconi"),
    dplyr::mutate(.expand_years(sites_la_thuile),   dataset = "La Thuile"),
    dplyr::mutate(.expand_years(sites_fluxnet2015), dataset = "FLUXNET2015")
  ) |>
    dplyr::mutate(dataset = factor(.data$dataset, levels = names(hist_colours)))

  peak_labels <- hist_all |>
    dplyr::group_by(.data$dataset) |>
    dplyr::slice_max(.data$n, n = 1L, with_ties = FALSE) |>
    dplyr::ungroup()

  ggplot2::ggplot() +
    # Vertical release-year reference lines (behind all data)
    ggplot2::geom_vline(
      xintercept = c(2000L, 2007L, 2015L),
      linetype   = "dashed", colour = "grey55", linewidth = 0.7
    ) +
    # Shuttle background: IGBP-stacked area
    ggplot2::geom_area(
      data = shuttle_igbp_annual,
      ggplot2::aes(x = .data$year, y = .data$n, fill = .data$igbp),
      position  = "stack",
      alpha     = 0.8,
      colour    = NA       # no bar outlines — too busy with 15 classes
    ) +
    scale_fill_igbp(name = "IGBP") +
    # Historical dataset lines
    ggplot2::geom_line(
      data = hist_all,
      ggplot2::aes(x      = .data$year,
                   y      = .data$n,
                   colour = .data$dataset),
      linewidth = 1.5
    ) +
    # Direct labels at peak year
    ggplot2::geom_label(
      data = peak_labels,
      ggplot2::aes(x      = .data$year,
                   y      = .data$n,
                   label  = .data$dataset,
                   colour = .data$dataset),
      fill        = "white",
      size        = base_size * 0.22,
      linewidth   = 0.5,
      fontface    = "bold",
      vjust       = -0.4,
      show.legend = FALSE
    ) +
    ggplot2::scale_colour_manual(values = hist_colours, guide = "none") +
    ggplot2::scale_x_continuous(
      limits = c(yr_min, yr_max),
      breaks = seq(1990L, 2025L, by = 5L),
      expand = ggplot2::expansion(mult = c(0.01, 0.01))
    ) +
    ggplot2::scale_y_continuous(
      breaks = scales::pretty_breaks(n = 6),
      expand = ggplot2::expansion(mult = c(0, 0.18))
    ) +
    ggplot2::labs(x = "Year", y = "Site-years per year") +
    fluxnet_theme(base_size = base_size) +
    ggplot2::theme(
      legend.position  = "right",
      legend.text      = ggplot2::element_text(size = as.integer(base_size) - 10L),
      legend.title     = ggplot2::element_text(size = as.integer(base_size) - 8L),
      legend.key.size  = grid::unit(14, "pt")
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
fig_network_growth <- function(metadata, geo_level = "global", presence_df = NULL) {
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

  # ---- % functionally active: NEE presence or last_year fallback -------------
  # Denominator: all sites established by each year (first_year <= year).
  # Numerator:   those functionally active per is_functionally_active().
  active_df <- data.frame(
    year       = all_years,
    pct_active = vapply(all_years, function(yr) {
      established <- sites$first_year <= yr
      n_total     <- sum(established, na.rm = TRUE)
      if (n_total == 0L) return(NA_real_)
      n_active <- sum(
        is_functionally_active(
          sites$site_id[established],
          reference_year   = yr,
          presence_df      = presence_df,
          active_threshold = 4L,
          last_year_vec    = sites$last_year[established]
        ),
        na.rm = TRUE
      )
      100 * n_active / n_total
    }, numeric(1L))
  )
  # Map 0–100 % onto primary-axis range 480–680 so the active line sits in the
  # lower portion of the stacked area chart without obscuring the IGBP classes.
  ai_lo <- 480; ai_hi <- 680
  active_df$y_primary <- active_df$pct_active * (ai_hi - ai_lo) / 100 + ai_lo

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
    # Functionally active % — dashed black line on secondary axis
    ggplot2::geom_line(
      data        = active_df,
      ggplot2::aes(x = .data$year, y = .data$y_primary),
      colour      = "black",
      linetype    = "dashed",
      linewidth   = 0.9,
      inherit.aes = FALSE
    ) +
    scale_fill_igbp(name = "IGBP") +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 8),
                                expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::scale_y_continuous(
      breaks   = scales::pretty_breaks(n = 6),
      expand   = ggplot2::expansion(mult = c(0, 0.05)),
      sec.axis = ggplot2::sec_axis(
        transform = ~ (. - 480) * 100 / (680 - 480),
        name      = "% Functionally active sites",
        breaks    = seq(0, 100, 25)
      )
    ) +
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
#'   panel only; snapshot year as a bold inset annotation per panel.
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
#'   `c(2000, 2007, 2015, 2025)`).
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
                                         years = c(2000, 2007, 2015, 2025), # Snapshot years: ~Marconi (2000), La Thuile (2007), FLUXNET2015 (2015), Shuttle/modern (2025)
                                         active_threshold = 4L,
                                         presence_df      = NULL) {
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
    if (!is.null(presence_df))
      paste0("Functionally active = \u22653 months valid NEE in any year within ",
             "[y\u2212", active_threshold - 1L, ", y].")
    else
      paste0("Functionally active = last_year \u2265 snapshot_year \u2212 ",
             active_threshold, ".")
  )

  n_years      <- length(years)
  mid_idx      <- ceiling(n_years / 2)   # panel index for centred axis title

  # --- per-panel data ---------------------------------------------------------
  panel_datasets <- lapply(years, function(yr) {
    pd <- site_profile |> dplyr::filter(.data$first_year <= yr)
    active_flags <- is_functionally_active(
      pd$site_id,
      reference_year   = yr,
      presence_df      = presence_df,
      active_threshold = active_threshold,
      last_year_vec    = pd$last_year
    )
    pd |>
      dplyr::mutate(
        record_length = yr - .data$first_year,
        activity = factor(
          dplyr::if_else(
            active_flags[.data$site_id],
            "Functionally active",
            "Inactive / high latency"
          ),
          levels = c("Functionally active", "Inactive / high latency")
        )
      )
  })

  # --- panel builder ----------------------------------------------------------
  # show_x_text:       show x axis tick labels and ticks
  # show_x_title:      show x axis title
  # show_y_text:       show y axis tick labels and ticks
  # show_y_title:      show y axis title (text)
  # title_hjust:       hjust for the panel title (year label, vB only)
  # mar:               plot.margin (t, r, b, l) in points
  # inset_label:       if non-NULL, suppress labs(title) and add annotate()
  #                    at top-left instead
  # base_size:         passed to fluxnet_theme()
  # legend_pos:        legend.position value for this panel
  # legend_text_size:  if non-NULL, override legend text/title size
  make_panel <- function(pd, yr,
                         show_x_text, show_x_title,
                         show_y_text, show_y_title,
                         title_hjust, mar,
                         inset_label      = NULL,
                         base_size        = 12,
                         legend_pos       = "bottom",
                         legend_text_size = NULL) {
    n_sites      <- nrow(pd)
    n_site_years <- sum(
      pmin(pd$last_year, yr) - pd$first_year + 1L,
      na.rm = TRUE
    )
    stats_lbl <- paste0("n = ", n_sites, "\nsite-years = ", n_site_years)

    p <- ggplot2::ggplot(
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
        title = if (is.null(inset_label))
                  paste0(yr, "  (n\u2009=\u2009", n_sites, ")")
                else
                  NULL,
        x = if (show_x_title) "Record length (years)" else NULL,
        y = if (show_y_title) "Sites"                  else NULL
      ) +
      fluxnet_theme(base_size = base_size) +
      ggplot2::theme(
        plot.title   = if (is.null(inset_label))
                         ggplot2::element_text(size = 11, hjust = title_hjust,
                                               margin = ggplot2::margin(b = 2))
                       else
                         ggplot2::element_blank(),
        axis.text.x  = if (show_x_text)  ggplot2::element_text()
                       else               ggplot2::element_blank(),
        axis.ticks.x = if (show_x_text)  ggplot2::element_line()
                       else               ggplot2::element_blank(),
        axis.text.y  = if (show_y_text)  ggplot2::element_text()
                       else               ggplot2::element_blank(),
        axis.ticks.y = if (show_y_text)  ggplot2::element_line()
                       else               ggplot2::element_blank(),
        legend.position = legend_pos,
        plot.margin  = ggplot2::margin(mar[1], mar[2], mar[3], mar[4],
                                       unit = "pt")
      )

    if (!is.null(legend_text_size)) {
      p <- p + ggplot2::theme(
        legend.text  = ggplot2::element_text(size = legend_text_size),
        legend.title = ggplot2::element_text(size = legend_text_size)
      )
    }

    if (!is.null(inset_label)) {
      p <- p + ggplot2::annotate(
        "text", x = -Inf, y = Inf,
        label    = inset_label,
        hjust    = -0.1, vjust = 1.5,
        size     = 5, fontface = "bold"
      )
    }

    # Top-right annotation: n and site-years for this snapshot year
    p <- p + ggplot2::annotate(
      "text", x = Inf, y = Inf,
      label = stats_lbl,
      hjust = 1.1, vjust = 1.5,
      size  = 4
    )

    p
  }

  # --- Version A: single column, oldest on top --------------------------------
  #   x axis text+title: bottom panel only
  #   y axis text: all panels; y axis title: shared label outside the stack
  #   snapshot year + n: bold inset annotation, top-left of each panel
  #   legend: inside top panel only (top-right), hidden on all others

  vA_base_size <- 18L   # 50% larger than the standard 12pt base

  vA_panels <- lapply(seq_along(years), function(i) {
    is_top    <- i == 1L
    is_bottom <- i == n_years
    n_sites   <- nrow(panel_datasets[[i]])

    make_panel(
      pd               = panel_datasets[[i]],
      yr               = years[[i]],
      show_x_text      = is_bottom,
      show_x_title     = is_bottom,
      show_y_text      = TRUE,
      show_y_title     = FALSE,          # shared Y label added outside stack
      title_hjust      = 1,
      mar              = c(0L, 5L, 0L, 5L),   # zero top/bottom; panels touch
      inset_label      = as.character(years[[i]]),
      base_size        = vA_base_size,
      legend_pos       = if (is_top) c(0.75, 0.85) else "none",
      legend_text_size = if (is_top) 14L else NULL
    )
  })

  # Shared Y axis label: narrow ggplot column centred across the full stack
  vA_y_label <- ggplot2::ggplot() +
    ggplot2::annotate(
      "text", x = 0.5, y = 0.5, label = "Sites",
      angle = 90, size = 7, fontface = "plain"
    ) +
    ggplot2::theme_void()

  vA_stack <- patchwork::wrap_plots(vA_panels, ncol = 1L) +
    patchwork::plot_layout(guides = "keep") &
    ggplot2::theme(plot.margin = ggplot2::margin(0, 5, 0, 5))

  vA <- (vA_y_label | vA_stack) +
    patchwork::plot_layout(widths = c(0.04, 1)) +
    patchwork::plot_annotation(
      title    = "Deployment duration profile of the FLUXNET network",
      subtitle = outer_subtitle
    )

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

#' Two-panel subregion bar overview: total sites and data latency
#'
#' Combines two horizontal stacked bar panels side by side, sharing a common
#' Y axis (UN M.49 subregion), to summarise the FLUXNET network at a snapshot
#' year.  The companion geographic figure is [fig_map_subregion_sites()].
#'
#' - **Left** — All sites in `metadata`, classified as "Functionally active"
#'   (`current_year - last_year <= active_threshold`) or
#'   "Inactive / high latency". Y-axis labels shown here only.
#' - **Right** — Functionally active sites only, filled by traffic-light
#'   latency bins (same palette as [fig_latency_by_subregion()]).
#'   Y-axis labels suppressed so the two panels appear joined.
#'
#' Subregion order is descending total-site count (highest at top).
#' Subregion assignment uses `countrycode::countrycode(iso2, "iso2c",
#' "un.regionsub.name")` on the first two characters of `site_id`. FLUXNET
#' "UK-*" sites are silently recoded from "UK" to "GB" before lookup.
#'
#' @param metadata Data frame. Snapshot CSV (one row per site) with columns
#'   `site_id` and `last_year`.
#' @param current_year Integer. Reference year for latency calculation
#'   (default `2026L`).
#' @param active_threshold Integer. Maximum latency (inclusive) for a site to
#'   be considered functionally active (default `4L`).
#'
#' @return A patchwork object: two bar panels side by side with shared legends
#'   at the bottom.
#'
#' @examples
#' \dontrun{
#' meta <- readr::read_csv("data/snapshots/fluxnet_shuttle_snapshot_20260414T153648.csv")
#' p <- fig_network_subregion_overview(meta)
#' ggplot2::ggsave("review/figures/fig_network_subregion_overview.png",
#'                 p, width = 12, height = 10, units = "in", dpi = 150)
#' }
fig_network_subregion_overview <- function(metadata,
                                           current_year     = 2026L,
                                           active_threshold = 4L,
                                           presence_df      = NULL) {
  for (pkg in c("patchwork", "countrycode")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required. Install with: install.packages('",
           pkg, "')", call. = FALSE)
    }
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

  # --- subregion assignment for all sites (shared between both panels) --------
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

  # Subregion order: ascending total count so highest is at top
  subregion_order <- sites |>
    dplyr::count(.data$subregion, name = "total") |>
    dplyr::arrange(.data$total) |>
    dplyr::pull(.data$subregion)

  sites <- dplyr::mutate(
    sites,
    subregion = factor(.data$subregion, levels = subregion_order)
  )

  # --- compute activity flags (NEE presence or last_year fallback) ------------
  active_flags <- is_functionally_active(
    sites$site_id,
    reference_year   = as.integer(current_year) - 1L,
    presence_df      = presence_df,
    active_threshold = active_threshold,
    last_year_vec    = sites$last_year
  )
  sites <- dplyr::mutate(sites, is_active = active_flags[.data$site_id])

  # --- Left panel: all sites, active vs inactive ------------------------------
  middle_data <- dplyr::mutate(
    sites,
    status = factor(
      dplyr::if_else(.data$is_active,
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
    fluxnet_theme() +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text.y     = ggplot2::element_text(size = 10),
      plot.margin     = ggplot2::margin(t = 4, r = 0, b = 4, l = 4, unit = "pt")
    )

  # --- Right panel: active sites by latency bin --------------------------------
  right_data <- sites |>
    dplyr::filter(.data$latency >= 0L, .data$is_active) |>
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
    fluxnet_theme() +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text.y     = ggplot2::element_blank(),
      axis.ticks.y    = ggplot2::element_blank(),
      plot.margin     = ggplot2::margin(t = 4, r = 4, b = 4, l = 0, unit = "pt")
    )

  # --- Assemble: two bars side by side, shared legends -----------------------
  (p_middle | p_right) +
    patchwork::plot_layout(widths = c(1, 1), guides = "collect") +
    patchwork::plot_annotation(
      title = paste0(
        "FLUXNET network by UN subregion \u2014 site counts and data latency (",
        current_year - 1L, ")"
      )
    ) &
    ggplot2::theme(legend.position = "bottom")
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
                                          active_threshold = 4L,
                                          presence_df      = NULL) {
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
    established  <- sites$first_year <= yr
    cum_sites    <- sum(established)
    if (cum_sites == 0L) {
      return(data.frame(year = yr, cumulative_sites = 0L, pct_active = NA_real_))
    }
    active_sites <- sum(
      is_functionally_active(
        sites$site_id[established],
        reference_year   = yr,
        presence_df      = presence_df,
        active_threshold = active_threshold,
        last_year_vec    = sites$last_year[established]
      )
    )
    data.frame(
      year             = yr,
      cumulative_sites = cum_sites,
      pct_active       = 100 * active_sites / cum_sites
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
        "Bottom: % functionally active",
        if (!is.null(presence_df))
          paste0(" (\u22653 months valid NEE in any year within [y\u2212",
                 active_threshold - 1L, ", y]).")
        else
          paste0(" (last_year \u2265 year \u2212 ", active_threshold, ")."),
        " Draft for integration into network growth figure."
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
                                     active_threshold = 4L,
                                     presence_df      = NULL) {
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
  active_flags <- is_functionally_active(
    df$site_id,
    reference_year   = as.integer(current_year) - 1L,
    presence_df      = presence_df,
    active_threshold = active_threshold,
    last_year_vec    = df$last_year
  )
  active <- df |>
    dplyr::filter(.data$latency >= 0L, active_flags[.data$site_id]) |>
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
    "Latency = ", current_year, " \u2212 last data year. ",
    "X axis compressed 4\u00d7 above 75 (dotted line)."
  )

  # --- x-axis break at 75: linear 0–75, 4× compressed above 75 ---------------
  x_brk  <- 75L
  x_comp <- 4L
  x_break_trans <- scales::trans_new(
    name      = "count_break_75",
    transform = function(x) ifelse(x <= x_brk, x,
                                   x_brk + (x - x_brk) / x_comp),
    inverse   = function(x) ifelse(x <= x_brk, x,
                                   x_brk + (x - x_brk) * x_comp)
  )

  ggplot2::ggplot(
    active,
    ggplot2::aes(y = .data$subregion, fill = .data$latency_bin)
  ) +
    ggplot2::geom_bar(position = "stack", width = 0.7, colour = "white",
                      linewidth = 0.2) +
    ggplot2::geom_vline(xintercept = 75, colour = "grey30",
                        linetype = "dotted", linewidth = 0.6) +
    ggplot2::scale_fill_manual(values = latency_colours, name = "Latency",
                               drop = FALSE) +
    ggplot2::scale_x_continuous(
      trans  = x_break_trans,
      breaks = c(0L, 25L, 50L, 75L, 150L, 200L),
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
    fluxnet_theme() +
    ggplot2::theme(
      legend.position  = "right",
      axis.text.y      = ggplot2::element_text(size = 11),
      plot.caption     = ggplot2::element_text(size = 9, colour = "grey40",
                                               hjust = 0)
    )
}

# ---- fig_latency_by_subregion_pct -------------------------------------------

#' Data latency by UN subregion — percentage version with N= annotations
#'
#' Like [fig_latency_by_subregion()] but shows all sites in each subregion
#' as percentages. Sites with latency 0–4 years use the traffic-light palette;
#' sites with latency > 4 years are shown in grey (`"4+ years"` bin). Each bar
#' is annotated at the right edge with the total site count `(N=...)`.
#'
#' @inheritParams fig_latency_by_subregion
#'
#' @return A ggplot object: horizontal stacked bar chart, x-axis = percentage
#'   of all sites (0–100%), y-axis = UN subregion ordered by total sites
#'   (descending from top), fill = latency bin (traffic-light palette plus
#'   grey for 4+ years), right-edge `(N=...)` annotations.
#'
#' @examples
#' \dontrun{
#' meta <- readr::read_csv("data/snapshots/fluxnet_shuttle_snapshot_20260414T153648.csv")
#' p <- fig_latency_by_subregion_pct(meta)
#' print(p)
#' }
fig_latency_by_subregion_pct <- function(metadata,
                                          current_year     = 2026L,
                                          active_threshold = 4L,
                                          presence_df      = NULL) {
  if (!requireNamespace("countrycode", quietly = TRUE)) {
    stop(
      "Package 'countrycode' is required for fig_latency_by_subregion_pct(). ",
      "Install with: install.packages('countrycode')",
      call. = FALSE
    )
  }

  required_meta <- c("site_id", "last_year")
  missing_meta  <- setdiff(required_meta, names(metadata))
  if (length(missing_meta) > 0L) {
    stop(
      "fig_latency_by_subregion_pct: metadata missing required column(s): ",
      paste(missing_meta, collapse = ", "),
      call. = FALSE
    )
  }

  latency_colours <- c(
    "0\u20131 years" = "#2ECC71",
    "1\u20132 years" = "#F39C12",
    "2\u20133 years" = "#E67E22",
    "3\u20134 years" = "#E74C3C",
    "4+ years"       = "#95A5A6"
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
      "fig_latency_by_subregion_pct: ", n_na, " site(s) could not be assigned to a ",
      "UN subregion and are labelled 'Unknown': ",
      paste(df$site_id[is.na(df$subregion)], collapse = ", ")
    )
    df <- dplyr::mutate(
      df, subregion = dplyr::if_else(is.na(.data$subregion), "Unknown",
                                     .data$subregion)
    )
  }

  # --- bin latency for all sites with non-negative latency -------------------
  # Sites with latency > 4 years are shown in grey ("4+ years").
  all_sites <- df |>
    dplyr::filter(.data$latency >= 0L) |>
    dplyr::mutate(
      latency_bin = dplyr::case_when(
        .data$latency <= 1L ~ "0\u20131 years",
        .data$latency <= 2L ~ "1\u20132 years",
        .data$latency <= 3L ~ "2\u20133 years",
        .data$latency <= 4L ~ "3\u20134 years",
        TRUE                ~ "4+ years"
      ),
      latency_bin = factor(.data$latency_bin, levels = latency_levels)
    )

  if (nrow(all_sites) == 0L) {
    warning("fig_latency_by_subregion_pct: no sites with valid latency — returning empty plot.",
            call. = FALSE)
    return(ggplot2::ggplot() + fluxnet_theme() +
             ggplot2::labs(title = "Latency by subregion (%) — no data"))
  }

  # --- order subregions by total sites, ascending so highest is top ----------
  subregion_order <- all_sites |>
    dplyr::count(.data$subregion, name = "total") |>
    dplyr::arrange(.data$total) |>
    dplyr::pull(.data$subregion)

  all_sites <- dplyr::mutate(
    all_sites,
    subregion = factor(.data$subregion, levels = subregion_order)
  )

  n_total     <- nrow(all_sites)
  n_subregion <- dplyr::n_distinct(all_sites$subregion)

  caption_text <- paste0(
    "All sites. Latency = ", current_year, " \u2212 last data year. ",
    "Grey = \u22654 years latency (data not submitted within 4 years)."
  )

  # --- compute per-subregion percentages and totals --------------------------
  counts <- all_sites |>
    dplyr::count(.data$subregion, .data$latency_bin, .drop = FALSE) |>
    dplyr::group_by(.data$subregion) |>
    dplyr::mutate(pct = .data$n / sum(.data$n) * 100) |>
    dplyr::ungroup()

  subregion_totals <- all_sites |>
    dplyr::count(.data$subregion, name = "n_sites") |>
    dplyr::mutate(subregion = factor(.data$subregion, levels = subregion_order))

  ggplot2::ggplot(
    counts,
    ggplot2::aes(x = .data$pct, y = .data$subregion, fill = .data$latency_bin)
  ) +
    ggplot2::geom_col(position = "stack", width = 0.7, colour = "white",
                      linewidth = 0.2) +
    ggplot2::geom_text(
      data = subregion_totals,
      ggplot2::aes(x = 102, y = .data$subregion,
                   label = paste0("(N=", .data$n_sites, ")")),
      hjust = 0, size = 3.5, color = "grey30", inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_manual(values = latency_colours, name = "Latency",
                               drop = FALSE) +
    ggplot2::scale_x_continuous(
      limits = c(0, 115),
      breaks = c(0L, 25L, 50L, 75L, 100L),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::scale_y_discrete(
      labels = function(x) {
        x <- dplyr::recode(x, !!!subregion_labels)
        scales::label_wrap(22)(x)
      }
    ) +
    ggplot2::labs(
      title    = "Data latency by UN subregion — all sites",
      subtitle = paste0("n\u2009=\u2009", n_total, " sites across ",
                        n_subregion, " subregions"),
      x        = "% of sites",
      y        = NULL,
      caption  = caption_text
    ) +
    fluxnet_theme() +
    ggplot2::theme(
      legend.position  = "right",
      axis.text.y      = ggplot2::element_text(size = 11),
      plot.caption     = ggplot2::element_text(size = 9, colour = "grey40",
                                               hjust = 0)
    )
}
