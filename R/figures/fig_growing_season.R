# R/figures/fig_growing_season.R
# Growing season length vs annual NEE for the FLUXNET Annual Paper 2026.
#
# Ported from legacy/AGUSlop.R.
# Reference only — do not edit legacy/ originals.
#
# Functions:
#   fig_growing_season_nee() — scatter of growing season length vs annual NEE;
#                              returns list($count, $span)

library(ggplot2)
library(ggtext)
library(dplyr)
library(lubridate)

# ---- Internal helpers -------------------------------------------------------

#' Parse YYYYMMDD integer timestamps to Date
#'
#' @param x Integer or character vector of YYYYMMDD dates.
#' @return A Date vector.
#' @noRd
.parse_date_gs <- function(x) {
  lubridate::ymd(as.character(x))
}

#' Build the growing-season scatter plot for one x-variable
#'
#' @param df Data frame with columns: `gsl_x` (the x variable), `NEE_annual`,
#'   `IGBP` (factor), and any aesthetics needed.
#' @param x_col Character. Column name to use on x-axis.
#' @param x_limits Numeric length-2. x-axis limits.
#' @param x_breaks Numeric vector. x-axis break positions.
#' @param x_label Character. x-axis label string.
#' @param shp Named integer vector of shapes by IGBP.
#' @return A ggplot object.
#' @noRd
.plot_gsl <- function(df, x_col, x_limits, x_breaks, x_label, shp) {

  df <- dplyr::filter(
    df,
    !is.na(.data[[x_col]]),
    !is.na(NEE_annual),
    !is.na(IGBP)
  )

  if (nrow(df) < 3L) {
    warning(
      "Fewer than 3 complete rows for '", x_col, "' — returning empty plot.",
      call. = FALSE
    )
    return(
      ggplot2::ggplot() +
        ggplot2::labs(title = paste("No data for", x_col)) +
        fluxnet_theme()
    )
  }

  # Regression + 95% prediction SE bounds
  fit  <- stats::lm(stats::as.formula(paste0("NEE_annual ~ ", x_col)), data = df)
  xgrid <- seq(x_limits[1], x_limits[2], length.out = 200)
  pred  <- stats::predict(
    fit,
    newdata = stats::setNames(data.frame(xgrid), x_col),
    se.fit  = TRUE
  )
  tval <- stats::qt(0.975, df = stats::df.residual(fit))

  pred_df <- data.frame(
    x    = xgrid,
    yhat = pred$fit,
    ylo  = pred$fit - tval * pred$se.fit,
    yhi  = pred$fit + tval * pred$se.fit
  )

  y_limits <- c(-800, 100)
  y_breaks <- seq(100, -800, by = -100)

  ggplot2::ggplot(
    df,
    ggplot2::aes(
      x     = .data[[x_col]],
      y     = NEE_annual,
      color = IGBP,
      shape = IGBP
    )
  ) +
    # IGBP points
    ggplot2::geom_point(size = poster_point_size, stroke = 0.7, alpha = 0.75) +
    # horizontal reference lines
    ggplot2::geom_hline(
      yintercept  = 100,
      color       = "black",
      linewidth   = 0.6,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_hline(
      yintercept  = -800,
      color       = "black",
      linewidth   = 0.6,
      inherit.aes = FALSE
    ) +
    # regression line
    ggplot2::geom_line(
      data        = pred_df,
      ggplot2::aes(x = x, y = yhat),
      color       = "black",
      linewidth   = 0.9,
      inherit.aes = FALSE
    ) +
    # 95% SE dashed bounds
    ggplot2::geom_line(
      data        = pred_df,
      ggplot2::aes(x = x, y = ylo),
      color       = "black",
      linetype    = "dashed",
      linewidth   = 0.7,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_line(
      data        = pred_df,
      ggplot2::aes(x = x, y = yhi),
      color       = "black",
      linetype    = "dashed",
      linewidth   = 0.7,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_x_continuous(limits = x_limits, breaks = x_breaks) +
    ggplot2::scale_y_continuous(limits = y_limits, breaks = y_breaks) +
    scale_color_igbp(guide = ggplot2::guide_legend(ncol = 2)) +
    ggplot2::scale_shape_manual(values = shp) +
    ggplot2::labs(
      x     = x_label,
      y     = lab_nee_annual,
      color = "IGBP",
      shape = "IGBP"
    ) +
    fluxnet_theme() +
    ggplot2::theme(
      axis.title.y    = ggtext::element_markdown(),
      # mirror top / right ticks without labels
      axis.ticks.x.top   = ggplot2::element_line(),
      axis.ticks.y.right = ggplot2::element_line(),
      axis.text.x.top    = ggplot2::element_blank(),
      axis.text.y.right  = ggplot2::element_blank()
    )
}

# ---- Exported function ------------------------------------------------------

#' Growing season length vs annual NEE
#'
#' Computes two growing season length metrics per site-year from daily data,
#' then scatters each against annual NEE.  Only sites with at least
#' `min_years` complete years are included.
#'
#' **Metric definitions** (from legacy/AGUSlop.R):
#' - `gsl_count`: number of days in a site-year where NEE < 0 (uptake days).
#' - `gsl_span`: calendar span from the first to last uptake day + 1 day.
#'
#' @param data_yy Annual FLUXNET data frame.  Must contain `site_id` and
#'   `NEE_VUT_REF` (or whatever annual NEE column you specify via
#'   `flux_var_yy`).  A `year` column (integer) is required; if absent, it is
#'   derived from `TIMESTAMP` (integer year format used by FLUXNET YY files).
#' @param data_dd Daily FLUXNET data frame.  Must contain `site_id`,
#'   `TIMESTAMP` (YYYYMMDD integer), and `NEE_VUT_REF`.
#' @param flux_var_yy Character. Annual NEE column name in `data_yy`
#'   (default `"NEE_VUT_REF"`).
#' @param flux_var_dd Character. Daily NEE column name in `data_dd` used to
#'   compute growing season length (default `"NEE_VUT_REF"`).
#' @param igbp_filter Character vector or `NULL`. If supplied, restrict to
#'   these IGBP classes (default `NULL` = all classes).
#' @param min_years Integer. Minimum number of annual data years per site to
#'   include (default `10L`).
#' @param metadata Optional data frame with `site_id` and `igbp` columns used
#'   to supply IGBP when absent from `data_yy`.
#' @param min_days_per_year Integer. Minimum valid daily records required in a
#'   site-year for the growing season calculation to be retained (default
#'   `330L`).
#'
#' @return A named list:
#'   \describe{
#'     \item{`count`}{ggplot — uptake days (gsl_count) vs annual NEE.}
#'     \item{`span`}{ggplot — first-to-last uptake span (gsl_span) vs annual
#'       NEE.}
#'   }
#'
#' @examples
#' \dontrun{
#' plots <- fig_growing_season_nee(data_yy, data_dd, min_years = 1L,
#'                                  metadata = snapshot_meta)
#' plots$count
#' plots$span
#' }
#'
#' @export
fig_growing_season_nee <- function(data_yy,
                                   data_dd,
                                   flux_var_yy       = "NEE_VUT_REF",
                                   flux_var_dd       = "NEE_VUT_REF",
                                   igbp_filter       = NULL,
                                   min_years         = 10L,
                                   metadata          = NULL,
                                   min_days_per_year = 330L) {

  # --- column checks ----------------------------------------------------------
  if (!"site_id" %in% names(data_yy))
    stop("data_yy must contain column 'site_id'.", call. = FALSE)
  if (!flux_var_yy %in% names(data_yy))
    stop("data_yy is missing column: ", flux_var_yy, call. = FALSE)
  if (!"site_id" %in% names(data_dd))
    stop("data_dd must contain column 'site_id'.", call. = FALSE)
  if (!flux_var_dd %in% names(data_dd))
    stop("data_dd is missing column: ", flux_var_dd, call. = FALSE)
  if (!"TIMESTAMP" %in% names(data_dd))
    stop("data_dd must contain column 'TIMESTAMP' (YYYYMMDD integer).",
         call. = FALSE)

  # --- derive year in annual data if absent ----------------------------------
  if (!"year" %in% names(data_yy)) {
    if ("TIMESTAMP" %in% names(data_yy)) {
      data_yy <- dplyr::mutate(data_yy, year = as.integer(TIMESTAMP))
    } else {
      stop(
        "data_yy must contain either 'year' or 'TIMESTAMP' (integer year).",
        call. = FALSE
      )
    }
  }

  # --- IGBP: join from metadata if needed ------------------------------------
  if (!"IGBP" %in% names(data_yy) && !is.null(metadata)) {
    if (all(c("site_id", "igbp") %in% names(metadata))) {
      data_yy <- dplyr::left_join(
        data_yy,
        dplyr::select(metadata, site_id, IGBP = igbp),
        by = "site_id"
      )
    }
  }

  if (!"IGBP" %in% names(data_dd) && !is.null(metadata)) {
    if (all(c("site_id", "igbp") %in% names(metadata))) {
      data_dd <- dplyr::left_join(
        data_dd,
        dplyr::select(metadata, site_id, IGBP = igbp),
        by = "site_id"
      )
    }
  }

  # --- parse daily date -------------------------------------------------------
  data_dd <- dplyr::mutate(
    data_dd,
    .date = .parse_date_gs(TIMESTAMP),
    .year = lubridate::year(.date)
  )

  # --- growing season length per site-year ------------------------------------
  gsl_data <- data_dd |>
    dplyr::filter(!is.na(.date), !is.na(.data[[flux_var_dd]])) |>
    dplyr::group_by(site_id, .year) |>
    dplyr::summarise(
      n_days    = dplyr::n(),
      IGBP      = dplyr::first(
        if ("IGBP" %in% names(data_dd)) .data[["IGBP"]] else NA_character_
      ),
      gsl_count = sum(.data[[flux_var_dd]] < 0, na.rm = TRUE),
      gsl_span  = if (any(.data[[flux_var_dd]] < 0, na.rm = TRUE)) {
        as.integer(
          max(.data[[".date"]][.data[[flux_var_dd]] < 0], na.rm = TRUE) -
          min(.data[[".date"]][.data[[flux_var_dd]] < 0], na.rm = TRUE) + 1L
        )
      } else {
        NA_integer_
      },
      .groups = "drop"
    ) |>
    dplyr::filter(n_days >= min_days_per_year) |>
    dplyr::rename(year = .year)

  # --- merge with annual NEE --------------------------------------------------
  annual_nee <- data_yy |>
    dplyr::select(site_id, year, NEE_annual = dplyr::all_of(flux_var_yy)) |>
    dplyr::filter(!is.na(site_id), !is.na(year), !is.na(NEE_annual))

  merged <- dplyr::inner_join(annual_nee, gsl_data, by = c("site_id", "year"))

  # Bring IGBP from data_yy if still missing
  if (!"IGBP" %in% names(merged) || all(is.na(merged$IGBP))) {
    if ("IGBP" %in% names(data_yy)) {
      igbp_lookup <- dplyr::distinct(
        dplyr::select(data_yy, site_id, IGBP),
        site_id, .keep_all = TRUE
      )
      merged <- merged |>
        dplyr::select(-dplyr::any_of("IGBP")) |>
        dplyr::left_join(igbp_lookup, by = "site_id")
    }
  }

  # --- IGBP filter ------------------------------------------------------------
  if (!is.null(igbp_filter)) {
    merged <- dplyr::filter(merged, IGBP %in% igbp_filter)
  }

  # --- minimum years filter ---------------------------------------------------
  sites_ok <- merged |>
    dplyr::distinct(site_id, year) |>
    dplyr::count(site_id, name = "n_years") |>
    dplyr::filter(n_years >= min_years) |>
    dplyr::pull(site_id)

  merged <- dplyr::filter(merged, site_id %in% sites_ok)

  if (nrow(merged) == 0L) {
    warning(
      "No data remaining after filtering (min_years = ", min_years, "). ",
      "Returning empty plots.",
      call. = FALSE
    )
    empty <- ggplot2::ggplot() +
      ggplot2::labs(title = "No data") +
      fluxnet_theme()
    return(list(count = empty, span = empty))
  }

  # --- stable IGBP palette and shapes ----------------------------------------
  igbp_lvls <- intersect(IGBP_order, unique(na.omit(merged$IGBP)))

  if (length(igbp_lvls) == 0L) {
    warning("IGBP not available — plots will not be colour/shape coded.",
            call. = FALSE)
    shp    <- stats::setNames(16L, "Unknown")
    merged <- dplyr::mutate(merged, IGBP = factor("Unknown"))
  } else {
    shp    <- stats::setNames(shape_igbp[seq_along(igbp_lvls)], igbp_lvls)
    merged <- dplyr::mutate(merged, IGBP = factor(IGBP, levels = IGBP_order))
  }

  # --- axis specs (shared) ---------------------------------------------------
  x_limits <- c(100, 365)
  x_breaks <- seq(100, 365, by = 50)

  x_lab_count <- "Length of growing season (uptake days)"
  x_lab_span  <- "Growing season span (days, first to last uptake day)"

  # --- build the two plots ---------------------------------------------------
  p_count <- .plot_gsl(
    df       = merged,
    x_col    = "gsl_count",
    x_limits = x_limits,
    x_breaks = x_breaks,
    x_label  = x_lab_count,
    shp      = shp
  )

  p_span <- .plot_gsl(
    df       = merged,
    x_col    = "gsl_span",
    x_limits = x_limits,
    x_breaks = x_breaks,
    x_label  = x_lab_span,
    shp      = shp
  )

  list(count = p_count, span = p_span)
}
