# R/figures/fig_anomaly_context.R
# Anomaly context figure for the FLUXNET Annual Paper 2026.
#
# Shows annual flux values for a selected IGBP / subregion / GEZ stratum,
# comparing a long-term historical distribution (left zone, boxplot + jitter)
# against recent years (right zone, per-year median + 95% CI with long-term
# envelope shaded).
#
# Functions:
#   fig_anomaly_context()   — three-panel anomaly context figure (NEE / LE / H)
#
# Data contract:
#   data_yy    — converted annual flux data (flux_data_converted_yy.rds,
#                FLUXMET rows only), columns: site_id, TIMESTAMP or YEAR,
#                flux variable columns
#   metadata   — snapshot metadata data frame, columns include: site_id,
#                igbp, data_hub, location_lat, location_long
#   gez_lookup — site-level GEZ lookup, columns: site_id, gez_name, gez_code
#                (generated from data/snapshots/site_gez_lookup.csv)

library(ggplot2)
library(ggtext)
library(dplyr)
library(tidyr)
library(patchwork)

source("R/plot_constants.R")

# ---- Internal helpers -------------------------------------------------------

#' Map flux variable name to annual axis label
#'
#' @param flux_var Character. Column name.
#' @return HTML-safe label string for element_markdown.
#' @noRd
.anom_flux_label <- function(flux_var) {
  if (startsWith(flux_var, "NEE"))  return(lab_nee_annual)
  if (startsWith(flux_var, "GPP"))  return(lab_gpp_annual)
  if (startsWith(flux_var, "RECO")) return(lab_reco_annual)
  if (startsWith(flux_var, "LE"))   return("LE (MJ m<sup>-2</sup> yr<sup>-1</sup>)")
  if (startsWith(flux_var, "H"))    return("H (MJ m<sup>-2</sup> yr<sup>-1</sup>)")
  flux_var
}

#' Map site ID two-letter prefix to UN subregion name
#'
#' Wraps countrycode with FLUXNET convention (UK \u2192 GB).
#'
#' @param iso2 Character vector of two-letter prefixes.
#' @return Character vector of UN subregion names.
#' @noRd
.anom_iso2_to_subregion <- function(iso2) {
  if (!requireNamespace("countrycode", quietly = TRUE)) {
    stop(
      "Package 'countrycode' is required for subregion filtering. ",
      "Install with: install.packages('countrycode')",
      call. = FALSE
    )
  }
  iso2_norm <- dplyr::case_when(iso2 == "UK" ~ "GB", TRUE ~ iso2)
  countrycode::countrycode(
    iso2_norm,
    origin      = "iso2c",
    destination = "un.regionsub.name",
    warn        = FALSE
  )
}

#' Build one anomaly context panel (single flux variable)
#'
#' Internal function called once per flux variable by [fig_anomaly_context()].
#' Returns a ggplot object or NULL if insufficient data.
#'
#' @param df_sites Data frame. Annual data filtered to selected sites. Must
#'   contain `site_id`, the column named by `year_col`, and `flux_var`.
#' @param flux_var Character. Column name of the flux variable to plot.
#' @param recent_years Integer vector. Years for the right (recent) zone.
#' @param year_col Character. Name of the year column in `df_sites`
#'   ("TIMESTAMP" or "YEAR").
#' @param is_bottom_panel Logical. If TRUE, show x-axis labels and ticks.
#' @param is_nee Logical. If TRUE, add horizontal dashed line at y = 0.
#'
#' @return A ggplot2 object, or NULL if historical data are absent.
#' @noRd
.build_anom_panel <- function(df_sites, flux_var, recent_years, year_col,
                               is_bottom_panel, is_nee) {

  # Filter to non-NA observations for this flux variable
  df_all <- df_sites |>
    dplyr::mutate(.year = as.integer(.data[[year_col]])) |>
    dplyr::filter(!is.na(.data[[flux_var]]))

  df_hist   <- dplyr::filter(df_all, .data$.year <  min(recent_years))
  df_recent <- dplyr::filter(df_all, .data$.year %in% recent_years)

  if (nrow(df_hist) == 0L) {
    warning(
      "No historical data found for flux variable '", flux_var, "'. ",
      "Skipping panel.",
      call. = FALSE
    )
    return(NULL)
  }

  # ---- Long-term pooled statistics ------------------------------------------
  hist_vals <- df_hist[[flux_var]]
  lt_p05    <- unname(quantile(hist_vals, 0.05, na.rm = TRUE))
  lt_p25    <- unname(quantile(hist_vals, 0.25, na.rm = TRUE))
  lt_med    <- unname(quantile(hist_vals, 0.50, na.rm = TRUE))
  lt_p75    <- unname(quantile(hist_vals, 0.75, na.rm = TRUE))
  lt_p95    <- unname(quantile(hist_vals, 0.95, na.rm = TRUE))

  hist_min_yr <- min(df_hist$.year, na.rm = TRUE)
  hist_max_yr <- max(df_hist$.year, na.rm = TRUE)
  hist_label  <- paste0(hist_min_yr, "\u2013", hist_max_yr)  # e.g. "1991\u20132016"

  # ---- X-axis positional mapping --------------------------------------------
  # x = 1  : historical summary (all years pooled to one position)
  # x = 2..n+1: recent years in chronological order
  x_hist     <- 1L
  n_recent   <- length(recent_years)
  x_vec_rec  <- seq(2L, 1L + n_recent)
  yr_to_x    <- stats::setNames(x_vec_rec, as.character(sort(recent_years)))

  x_min    <- 0.5
  x_max    <- 1L + n_recent + 0.5
  x_breaks <- c(x_hist, x_vec_rec)
  x_labels <- c(hist_label, as.character(sort(recent_years)))
  vline_x  <- 1.5  # separator between historical and recent zones

  # ---- Historical jitter data -----------------------------------------------
  df_hist_mapped <- dplyr::mutate(df_hist, .x = x_hist)

  # ---- Historical boxplot (custom 5th\u201395th whiskers) -----------------------
  box_df <- data.frame(
    .x     = x_hist,
    ymin   = lt_p05,
    lower  = lt_p25,
    middle = lt_med,
    upper  = lt_p75,
    ymax   = lt_p95
  )

  # ---- Recent year statistics — with percentile position --------------------
  # Percentile: where does each annual median sit within the long-term
  # site-year distribution?  ecdf() gives P(X <= x) on [0,1]; multiply by 100.
  lt_ecdf <- stats::ecdf(hist_vals)

  if (nrow(df_recent) > 0L) {
    recent_stats <- df_recent |>
      dplyr::group_by(.year) |>
      dplyr::summarise(
        yr_med = median(.data[[flux_var]], na.rm = TRUE),
        yr_lo  = unname(quantile(.data[[flux_var]], 0.025, na.rm = TRUE)),
        yr_hi  = unname(quantile(.data[[flux_var]], 0.975, na.rm = TRUE)),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        .x          = unname(yr_to_x[as.character(.data$.year)]),
        percentile  = lt_ecdf(.data$yr_med) * 100
      )
  } else {
    recent_stats <- NULL
  }

  # ---- Ribbon data (long-term 5th–95th envelope, spans both zones) ----------
  ribbon_hist_df <- data.frame(
    x    = c(x_min, vline_x),
    ymin = lt_p05,
    ymax = lt_p95
  )
  ribbon_rec_df <- data.frame(
    x    = c(vline_x, x_max),
    ymin = lt_p05,
    ymax = lt_p95
  )

  # ---- Build ggplot ----------------------------------------------------------
  p <- ggplot2::ggplot() +
    # Left zone — historical ribbon (behind points and box)
    ggplot2::geom_ribbon(
      data = ribbon_hist_df,
      ggplot2::aes(x = x, ymin = ymin, ymax = ymax),
      fill = "steelblue", alpha = 0.2, inherit.aes = FALSE
    ) +
    # Historical jitter: individual site-year values (plotted first, behind box)
    ggplot2::geom_jitter(
      data  = df_hist_mapped,
      ggplot2::aes(x = .x, y = .data[[flux_var]]),
      color = "grey50", alpha = 0.3, width = 0.25, height = 0,
      show.legend = FALSE
    ) +
    # Historical boxplot: whiskers to 5th\u201395th percentile (on top of points)
    ggplot2::geom_errorbar(
      data = box_df,
      ggplot2::aes(x = .x, ymin = ymin, ymax = ymax),
      width = 0.3, linewidth = 0.8, color = "black"
    ) +
    # Historical boxplot: IQR box with transparent fill so points show through
    ggplot2::geom_crossbar(
      data = box_df,
      ggplot2::aes(x = .x, y = middle, ymin = lower, ymax = upper),
      width = 0.5, linewidth = 0.7, fill = NA, color = "black"
    ) +
    # Zone separator
    ggplot2::geom_vline(
      xintercept = vline_x, linetype = "dashed",
      color = "grey40", linewidth = 0.5
    )

  # y = 0 reference line (NEE panel only)
  if (is_nee) {
    p <- p + ggplot2::geom_hline(
      yintercept = 0L, linetype = "dashed", color = "grey60", linewidth = 0.4
    )
  }

  # Recent zone elements (ribbon + percentile-coloured points + CIs)
  if (!is.null(recent_stats) && nrow(recent_stats) > 0L) {
    p <- p +
      ggplot2::geom_ribbon(
        data = ribbon_rec_df,
        ggplot2::aes(x = x, ymin = ymin, ymax = ymax),
        fill = "steelblue", alpha = 0.2, inherit.aes = FALSE
      ) +
      ggplot2::geom_errorbar(
        data = recent_stats,
        ggplot2::aes(x = .x, ymin = yr_lo, ymax = yr_hi, color = percentile),
        width = 0.25, linewidth = 0.7, show.legend = FALSE
      ) +
      ggplot2::geom_point(
        data = recent_stats,
        ggplot2::aes(x = .x, y = yr_med, color = percentile),
        size = 3
      ) +
      ggplot2::scale_color_gradient2(
        low      = "#2166AC",
        mid      = "white",
        high     = "#B2182B",
        midpoint = 50,
        limits   = c(0, 100),
        name     = "Percentile\nwithin\nlong-term range"
      )
  }

  p <- p +
    ggplot2::scale_x_continuous(
      breaks = x_breaks,
      labels = x_labels,
      limits = c(x_min, x_max),
      expand = ggplot2::expansion(0)
    ) +
    ggplot2::labs(y = .anom_flux_label(flux_var), x = NULL) +
    fluxnet_theme() +
    ggplot2::theme(axis.title.y = ggtext::element_markdown())

  if (!is_bottom_panel) {
    p <- p + ggplot2::theme(
      axis.text.x  = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      plot.margin  = ggplot2::margin(t = 5, r = 5, b = 0, l = 5, unit = "pt")
    )
  } else {
    p <- p + ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
      plot.margin = ggplot2::margin(t = 0, r = 5, b = 5, l = 5, unit = "pt")
    )
  }

  p
}

# ---- fig_anomaly_context ----------------------------------------------------

#' Anomaly context figure — recent flux vs long-term distribution
#'
#' Produces a three-panel figure (NEE / LE / H by default) comparing recent
#' annual flux values against the long-term historical distribution for sites
#' matching a specified IGBP class, UN subregion, and FAO GEZ zone.
#'
#' Each panel is divided into two zones by a vertical dashed line:
#' \describe{
#'   \item{Left zone — long-term context}{
#'     Individual site-year values plotted first as jittered grey points
#'     (alpha = 0.3), then a steelblue ribbon from the 5th to 95th percentile
#'     (alpha = 0.2), then a boxplot overlaid on top with a transparent fill so
#'     points remain visible.  Whiskers span the 5th\u201395th percentile; box spans
#'     the IQR; median bar is visible.  The x-axis label shows the historical
#'     year range.
#'   }
#'   \item{Right zone — recent years}{
#'     The long-term 5th\u201395th percentile envelope is drawn as a shaded ribbon
#'     (steelblue, alpha = 0.2).  Per-year medians are plotted as points (size 3)
#'     with 95\% CI error bars (2.5th\u201397.5th percentile across sites), coloured
#'     by their percentile position within the long-term distribution:
#'     blue (\u2248 0th) \u2192 white (50th) \u2192 red (\u2248 100th), using a diverging scale
#'     with midpoint = 50.
#'   }
#' }
#'
#' @param data_yy Data frame. Converted annual flux data (FLUXMET rows only).
#'   Must contain \code{site_id}, a year column (\code{TIMESTAMP} or
#'   \code{YEAR}), \code{NEE_VUT_REF} (used for site selection), and the
#'   columns named in \code{flux_vars}.
#' @param metadata Data frame. Site metadata from the FLUXNET Shuttle snapshot.
#'   Must contain \code{site_id}, \code{igbp}, \code{data_hub}, and
#'   \code{location_lat}.
#' @param gez_lookup Data frame. Site-level GEZ lookup, columns \code{site_id},
#'   \code{gez_name}, \code{gez_code}.  See \code{data/snapshots/site_gez_lookup.csv}.
#' @param igbp Character scalar. IGBP class code to filter to (e.g. \code{"ENF"}).
#' @param gez_filter Character vector. One or more FAO GEZ zone names to include
#'   (matched case-insensitively against \code{gez_lookup$gez_name}).
#' @param subregion Character scalar. UN subregion name to filter to
#'   (default \code{"Northern America"}).  Matched against
#'   \code{countrycode::countrycode(iso2, "iso2c", "un.regionsub.name")}.
#' @param recent_years Integer vector. Years shown in the right (recent) zone
#'   (default \code{2019:2024}).
#' @param min_sites Integer. Minimum number of sites required to proceed
#'   (default \code{5}).
#' @param min_nee_years Integer. Minimum number of valid \code{NEE_VUT_REF}
#'   years a site must have to be included (default \code{8L}).
#' @param flux_vars Character vector. Flux variable column names to plot as
#'   stacked panels in top-to-bottom order
#'   (default: \code{c("NEE_VUT_REF", "LE_F_MDS", "H_F_MDS")}).
#'
#' @return A \pkg{patchwork} object with one panel per available flux variable
#'   stacked vertically with a shared x-axis.  Returns \code{invisible(NULL)}
#'   if no panels could be built.
#'
#' @note Exploratory figure \u2014 not yet a candidate for the main paper.
#'   GEZ stratification planned as future enhancement.
#'
#' @examples
#' \dontrun{
#' gez_lookup <- readr::read_csv("data/snapshots/site_gez_lookup.csv")
#' p <- fig_anomaly_context(
#'   data_yy    = data_yy_flux,
#'   metadata   = snapshot_meta,
#'   gez_lookup = gez_lookup,
#'   igbp       = "ENF",
#'   gez_filter = c("Temperate Continental Forest", "Temperate Oceanic Forest"),
#'   subregion  = "Northern America"
#' )
#' }
#'
#' @export
fig_anomaly_context <- function(
    data_yy,
    metadata,
    gez_lookup,
    igbp,
    gez_filter,
    subregion     = "Northern America",
    recent_years  = 2019:2024,
    min_sites     = 5L,
    min_nee_years = 8L,
    flux_vars     = c("NEE_VUT_REF", "LE_F_MDS", "H_F_MDS")
) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop(
      "Package 'patchwork' is required. ",
      "Install with: install.packages('patchwork')",
      call. = FALSE
    )
  }
  if (!requireNamespace("countrycode", quietly = TRUE)) {
    stop(
      "Package 'countrycode' is required. ",
      "Install with: install.packages('countrycode')",
      call. = FALSE
    )
  }

  # ---- Input validation -------------------------------------------------------
  if (!is.character(igbp) || length(igbp) != 1L) {
    stop("igbp must be a single character string.", call. = FALSE)
  }
  if (!is.character(gez_filter) || length(gez_filter) == 0L) {
    stop("gez_filter must be a non-empty character vector.", call. = FALSE)
  }
  required_meta <- c("site_id", "igbp", "data_hub", "location_lat")
  miss_meta <- setdiff(required_meta, names(metadata))
  if (length(miss_meta) > 0L) {
    stop(
      "metadata is missing required column(s): ",
      paste(miss_meta, collapse = ", "),
      call. = FALSE
    )
  }
  required_gez <- c("site_id", "gez_name")
  miss_gez <- setdiff(required_gez, names(gez_lookup))
  if (length(miss_gez) > 0L) {
    stop(
      "gez_lookup is missing required column(s): ",
      paste(miss_gez, collapse = ", "),
      call. = FALSE
    )
  }
  if (!"site_id" %in% names(data_yy)) {
    stop("data_yy must contain a 'site_id' column.", call. = FALSE)
  }

  year_col <- if ("TIMESTAMP" %in% names(data_yy)) {
    "TIMESTAMP"
  } else if ("YEAR" %in% names(data_yy)) {
    "YEAR"
  } else {
    stop(
      "data_yy must contain a 'TIMESTAMP' or 'YEAR' column.",
      call. = FALSE
    )
  }

  # ---- Site selection ---------------------------------------------------------

  # Normalise metadata to one row per site
  meta_work <- metadata |>
    dplyr::select(dplyr::all_of(c("site_id", "igbp", "data_hub", "location_lat"))) |>
    dplyr::distinct()

  # Filter by IGBP (case-insensitive)
  # Assign to local variable first to avoid NSE masking by the column of the same name
  igbp_lower <- tolower(igbp)
  meta_igbp <- meta_work |>
    dplyr::filter(tolower(.data$igbp) == igbp_lower)

  if (nrow(meta_igbp) == 0L) {
    stop("No sites found for IGBP = '", igbp, "'.", call. = FALSE)
  }

  # Filter by UN subregion
  subregion_target <- subregion
  meta_igbp <- meta_igbp |>
    dplyr::mutate(
      .iso2      = substr(.data$site_id, 1L, 2L),
      .subregion = .anom_iso2_to_subregion(.data$.iso2)
    ) |>
    dplyr::filter(.data$.subregion == subregion_target)

  if (nrow(meta_igbp) == 0L) {
    stop(
      "No ", igbp, " sites found in subregion '", subregion, "'.",
      call. = FALSE
    )
  }

  # Join GEZ and filter by gez_filter (case-insensitive)
  gez_filter_lower <- tolower(gez_filter)

  meta_gez <- meta_igbp |>
    dplyr::left_join(
      dplyr::select(gez_lookup, "site_id", "gez_name"),
      by = "site_id"
    ) |>
    dplyr::filter(tolower(.data$gez_name) %in% gez_filter_lower)

  if (nrow(meta_gez) == 0L) {
    stop(
      "No ", igbp, " sites in '", subregion, "' match GEZ filter: ",
      paste(gez_filter, collapse = ", "),
      ". Available GEZ zones for this stratum: ",
      paste(
        sort(unique(meta_igbp |>
          dplyr::left_join(dplyr::select(gez_lookup, "site_id", "gez_name"),
                           by = "site_id") |>
          dplyr::pull(.data$gez_name))),
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  # Count valid NEE years per site
  if ("NEE_VUT_REF" %in% names(data_yy)) {
    nee_counts <- data_yy |>
      dplyr::filter(.data$site_id %in% meta_gez$site_id) |>
      dplyr::group_by(.data$site_id) |>
      dplyr::summarise(
        n_years_valid_nee = sum(!is.na(.data$NEE_VUT_REF)),
        .groups = "drop"
      )
  } else {
    nee_counts <- dplyr::tibble(
      site_id           = meta_gez$site_id,
      n_years_valid_nee = 0L
    )
  }

  meta_final <- meta_gez |>
    dplyr::left_join(nee_counts, by = "site_id") |>
    dplyr::mutate(
      n_years_valid_nee = dplyr::coalesce(as.integer(.data$n_years_valid_nee), 0L)
    ) |>
    dplyr::filter(.data$n_years_valid_nee >= min_nee_years) |>
    dplyr::select("site_id", "data_hub", "igbp", "gez_name",
                  "location_lat", "n_years_valid_nee") |>
    dplyr::arrange(dplyr::desc(.data$n_years_valid_nee))

  n_sites <- nrow(meta_final)

  if (n_sites < min_sites) {
    stop(
      "Only ", n_sites, " site(s) with >= ", min_nee_years,
      " valid NEE years found for IGBP=", igbp,
      ", subregion='", subregion, "', GEZ=[",
      paste(gez_filter, collapse = ", "), "]",
      " (minimum required: ", min_sites, " sites).\n",
      "Sites found: ",
      if (n_sites > 0L)
        paste(meta_final$site_id, collapse = ", ")
      else
        "(none)",
      call. = FALSE
    )
  }

  # Print selected sites
  cat("\n--- fig_anomaly_context: selected sites ---\n")
  cat(sprintf(
    "IGBP: %s | subregion: %s | GEZ: %s | n_sites: %d\n",
    igbp, subregion, paste(gez_filter, collapse = " & "), n_sites
  ))
  print(
    as.data.frame(meta_final[, c("site_id", "data_hub", "igbp",
                                  "gez_name", "location_lat",
                                  "n_years_valid_nee")]),
    row.names = FALSE
  )
  cat("\n")

  # ---- Filter data_yy to selected sites --------------------------------------
  df_sites <- data_yy |>
    dplyr::filter(.data$site_id %in% meta_final$site_id)

  # ---- Build flux variable panels --------------------------------------------
  avail_vars <- intersect(flux_vars, names(df_sites))
  skipped    <- setdiff(flux_vars, names(df_sites))
  if (length(skipped) > 0L) {
    warning(
      "flux_var(s) not found in data_yy \u2014 skipped: ",
      paste(skipped, collapse = ", "),
      call. = FALSE
    )
  }
  if (length(avail_vars) == 0L) {
    warning("None of the requested flux_vars are present in data_yy.", call. = FALSE)
    return(invisible(NULL))
  }

  panels     <- list()
  n_panels   <- length(avail_vars)
  nee_vars   <- c("NEE_VUT_REF", "NEE_CUT_REF", "NEE_VUT_MEAN", "NEE_CUT_MEAN")

  for (i in seq_along(avail_vars)) {
    fv     <- avail_vars[[i]]
    is_bot <- (i == n_panels)
    is_nee <- fv %in% nee_vars

    p <- .build_anom_panel(
      df_sites        = df_sites,
      flux_var        = fv,
      recent_years    = recent_years,
      year_col        = year_col,
      is_bottom_panel = is_bot,
      is_nee          = is_nee
    )
    if (!is.null(p)) panels[[fv]] <- p
  }

  if (length(panels) == 0L) {
    warning(
      "fig_anomaly_context(): no panels built. Check data availability.",
      call. = FALSE
    )
    return(invisible(NULL))
  }

  # ---- Assemble patchwork ----------------------------------------------------
  fig_title <- sprintf(
    "%s \u2014 %s \u2014 %s (n=%d sites)",
    igbp, subregion, paste(gez_filter, collapse = " & "), n_sites
  )

  pw <- patchwork::wrap_plots(panels, ncol = 1) +
    patchwork::plot_annotation(
      title = fig_title,
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 13,
                                           hjust = 0.5)
      )
    )

  pw
}
