# R/figures/fig_maps.R
# Global and regional site maps for the FLUXNET Annual Paper 2026.
#
# Ported from:
#   legacy/MappingFluxnet.R    — global site maps, network overlay maps
#   legacy/AMFOct25_poster.R   — historical NEE map, ΔNEE map
#
# All functions take metadata as a data frame (e.g. read from the snapshot CSV
# via readr::read_csv("data/snapshots/...csv")) with columns:
#   site_id, location_lat, location_long, igbp, data_hub, network
#
# NEE map functions additionally take data_yy (flux_data_converted_yy.rds).
# No file I/O occurs inside these functions.

source("R/plot_constants.R")

# ---- Internal helpers ------------------------------------------------------

# Suppress s2 spherical geometry (avoids outline artefacts on some systems).
# Called at the top of each exported function.
.disable_s2 <- function() {
  old <- sf::sf_use_s2(FALSE)
  invisible(old)
}

# Return a land-outline sf object using the bundled rnaturalearthdata.
# Scale: "medium" (1:50m). Cached in the calling environment after first call.
.land_sf <- function(scale = "medium") {
  rnaturalearth::ne_countries(scale = scale, returnclass = "sf") |>
    sf::st_make_valid()
}

# Shared void basemap with land outline and consistent legend styling.
# Applies theme_void() + fluxnet_theme() legend settings.
.map_base <- function(land, title = NULL, base_size = 11) {
  ggplot2::ggplot() +
    ggplot2::theme_void(base_size = base_size) +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(hjust = 0.5, face = "bold",
                                              size  = base_size * 1.1),
      legend.position = "bottom",
      legend.title    = ggtext::element_markdown(size = base_size * 0.9),
      legend.text     = ggtext::element_markdown(size = base_size * 0.85)
    ) +
    ggplot2::geom_sf(data = land, fill = "gray95", color = "black",
                     linewidth = 0.25) +
    ggplot2::labs(title = title)
}

# Apply region-specific coordinate limits.
# region: "global" (default) | "north_america" | "europe" | numeric vector
# c(xmin, xmax, ymin, ymax).
.apply_region <- function(p, region) {
  limits <- switch(
    region,
    global        = NULL,
    north_america = c(-170, -50, 5, 83),
    europe        = c(-25, 45, 34, 72),
    {
      if (is.numeric(region) && length(region) == 4L) region
      else {
        warning("Unknown region '", region, "' — using global extent.",
                call. = FALSE)
        NULL
      }
    }
  )
  if (is.null(limits)) {
    return(p + ggplot2::coord_sf(expand = FALSE))
  }
  p + ggplot2::coord_sf(
    xlim   = limits[1:2],
    ylim   = limits[3:4],
    expand = FALSE
  )
}

# Symmetric colour limits: take the central `probs` quantile range of x,
# then enforce symmetry around 0.
.robust_limits <- function(x, probs = c(0.01, 0.99), mult = 1.2) {
  q   <- stats::quantile(x[is.finite(x)], probs = probs, na.rm = TRUE)
  lim <- max(abs(q)) * mult
  c(-lim, lim)
}

# Fixed hub colour map (pattern-based, robust to label variants).
.hub_colors <- function(hubs) {
  hub_palette <- c(
    AmeriFlux  = "#D0104C",
    ICOS       = "#E69F00",
    OzFlux     = "#7FC97F",
    AsiaFlux   = "#7570B3",
    JapanFlux  = "#A6761D",
    ChinaFlux  = "#999999",
    FluxnetLSA = "#F0E442",
    Other      = "#2C7FB8"
  )
  result <- stats::setNames(rep(hub_palette[["Other"]], length(hubs)), hubs)
  for (pattern in names(hub_palette)) {
    idx <- grepl(pattern, hubs, ignore.case = TRUE)
    result[idx] <- hub_palette[[pattern]]
  }
  result
}

# Validate that required columns exist in metadata.
.check_meta_cols <- function(metadata, required) {
  missing <- setdiff(required, names(metadata))
  if (length(missing) > 0L) {
    stop(
      "metadata is missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
}

# ---- fig_map_global --------------------------------------------------------

#' Global site map coloured by hub or IGBP
#'
#' Plots all sites in `metadata` as points on an outline world map, coloured by
#' `data_hub` (default) or `igbp`. Adapted from the outline-only map section of
#' `legacy/MappingFluxnet.R`.
#'
#' @param metadata Data frame with columns `site_id`, `location_lat`,
#'   `location_long`, `data_hub`, and `igbp`. Typically read from the snapshot
#'   CSV: `readr::read_csv("data/snapshots/<file>.csv")`.
#' @param color_by Character. Variable to colour points by: `"data_hub"`
#'   (default) or `"igbp"`.
#' @param pt_size Numeric. Point size (default `2.4`).
#' @param pt_alpha Numeric. Point transparency (default `0.75`).
#' @param region Character or numeric. Map extent: `"global"` (default),
#'   `"north_america"`, `"europe"`, or a numeric vector
#'   `c(xmin, xmax, ymin, ymax)`.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' meta <- readr::read_csv("data/snapshots/fluxnet_shuttle_snapshot_20260328T153608.csv")
#' p <- fig_map_global(meta, color_by = "data_hub")
#' print(p)
#' }
fig_map_global <- function(metadata,
                            color_by = "data_hub",
                            pt_size  = 2.4,
                            pt_alpha = 0.75,
                            region   = "global") {
  .disable_s2()
  .check_meta_cols(metadata, c("site_id", "location_lat", "location_long",
                                "data_hub", "igbp"))
  if (!color_by %in% names(metadata)) {
    stop(sprintf("color_by column '%s' not found in metadata.", color_by),
         call. = FALSE)
  }

  land <- .land_sf()

  sites_clean <- metadata |>
    dplyr::filter(
      !is.na(location_lat), !is.na(location_long),
      dplyr::between(location_lat,  -90,  90),
      dplyr::between(location_long, -180, 180)
    ) |>
    dplyr::distinct(site_id, .keep_all = TRUE)

  p <- .map_base(land, title = NULL)

  if (color_by == "data_hub") {
    hubs      <- sort(unique(stats::na.omit(sites_clean$data_hub)))
    hub_cols  <- .hub_colors(hubs)
    p <- p +
      ggplot2::geom_point(
        data = sites_clean,
        ggplot2::aes(x = location_long, y = location_lat,
                     fill = data_hub),
        shape = 21, color = "black", size = pt_size,
        alpha = pt_alpha, stroke = 0.3
      ) +
      ggplot2::scale_fill_manual(
        values = hub_cols,
        name   = "Hub",
        na.value = "#2C7FB8"
      )
  } else if (color_by == "igbp") {
    p <- p +
      ggplot2::geom_point(
        data = sites_clean,
        ggplot2::aes(x = location_long, y = location_lat,
                     fill = igbp),
        shape = 21, color = "black", size = pt_size,
        alpha = pt_alpha, stroke = 0.3
      ) +
      scale_fill_igbp(name = "IGBP", na.value = "grey60")
  } else {
    # Generic continuous or character column
    p <- p +
      ggplot2::geom_point(
        data = sites_clean,
        ggplot2::aes(x = location_long, y = location_lat,
                     color = .data[[color_by]]),
        size = pt_size, alpha = pt_alpha
      )
  }

  .apply_region(p, region)
}

# ---- fig_map_nee_mean ------------------------------------------------------

#' Global site map coloured by long-term mean NEE
#'
#' Computes a per-site long-term mean NEE from `data_yy`, joins site
#' coordinates from `metadata`, and maps sites using a blue–grey–red diverging
#' colour scale centred on zero. Only sites with at least `min_years` of valid
#' annual data are shown. Adapted from the historical NEE map section of
#' `legacy/AMFOct25_poster.R`.
#'
#' @param data_yy Data frame. Converted annual flux data
#'   (`flux_data_converted_yy.rds`) with columns `site_id`, `TIMESTAMP`
#'   (integer year), and `NEE_VUT_REF`.
#' @param metadata Data frame with `site_id`, `location_lat`, `location_long`.
#' @param min_years Integer. Minimum years of valid NEE data required to plot a
#'   site (default `10`).
#' @param region Character or numeric. Map extent (default `"global"`).
#'   See [fig_map_global()] for accepted values.
#' @param exclude_sites Optional character vector of site IDs to drop.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' data_yy <- readRDS("data/processed/flux_data_converted_yy.rds")
#' meta    <- readr::read_csv("data/snapshots/fluxnet_shuttle_snapshot_20260328T153608.csv")
#' p <- fig_map_nee_mean(data_yy, meta, min_years = 5)
#' print(p)
#' }
fig_map_nee_mean <- function(data_yy,
                              metadata,
                              min_years     = 10L,
                              region        = "global",
                              exclude_sites = NULL) {
  .disable_s2()
  .check_meta_cols(metadata, c("site_id", "location_lat", "location_long"))

  if (!"NEE_VUT_REF" %in% names(data_yy)) {
    stop("NEE_VUT_REF column not found in data_yy.", call. = FALSE)
  }

  flux <- data_yy
  if (!is.null(exclude_sites)) {
    flux <- dplyr::filter(flux, !site_id %in% exclude_sites)
  }

  site_stats <- flux |>
    dplyr::filter(is.finite(NEE_VUT_REF)) |>
    dplyr::group_by(site_id) |>
    dplyr::summarise(
      n_years  = dplyr::n_distinct(TIMESTAMP),
      mean_nee = mean(NEE_VUT_REF, na.rm = TRUE),
      .groups  = "drop"
    ) |>
    dplyr::filter(n_years >= min_years)

  if (nrow(site_stats) == 0L) {
    warning(
      sprintf("No sites have >= %d years of valid NEE. ", min_years),
      "Try reducing min_years. Returning empty plot.",
      call. = FALSE
    )
    return(ggplot2::ggplot() +
             ggplot2::labs(title = sprintf("No sites with >= %d years", min_years)) +
             fluxnet_theme())
  }

  plot_data <- site_stats |>
    dplyr::left_join(
      dplyr::select(metadata, site_id, location_lat, location_long),
      by = "site_id"
    ) |>
    dplyr::filter(is.finite(location_lat), is.finite(location_long))

  lims <- .robust_limits(plot_data$mean_nee)
  land <- .land_sf()

  p <- .map_base(land) +
    ggplot2::geom_point(
      data  = plot_data,
      ggplot2::aes(x = location_long, y = location_lat, color = mean_nee),
      size  = 2.2, alpha = 0.85
    ) +
    ggplot2::scale_color_gradient2(
      low      = "#2C7BB6",
      mid      = "gray85",
      high     = "#D7191C",
      midpoint = 0,
      limits   = lims,
      oob      = scales::squish,
      name     = "NEE (g C m<sup>-2</sup> yr<sup>-1</sup>)"
    ) +
    ggplot2::labs(
      title    = sprintf("Long-term mean NEE (sites with \u2265%d years)", min_years),
      subtitle = sprintf("n = %d sites", nrow(plot_data))
    )

  .apply_region(p, region)
}

# ---- fig_map_nee_delta -----------------------------------------------------

#' Global site map coloured by ΔNEE (recent minus historical)
#'
#' Computes per-site ΔNEE = mean(recent_years) − mean(pre-recent years) from
#' `data_yy`, joins coordinates from `metadata`, and maps sites using a
#' blue–grey–red diverging scale. Only sites with at least `min_years` of
#' total valid data AND at least one value in `recent_years` are shown.
#' Adapted from the ΔNEE map section of `legacy/AMFOct25_poster.R`.
#'
#' @param data_yy Data frame. Converted annual flux data with `site_id`,
#'   `TIMESTAMP` (integer year), and `NEE_VUT_REF`.
#' @param metadata Data frame with `site_id`, `location_lat`, `location_long`.
#' @param recent_years Integer vector. Years that define the "recent" period
#'   (default `2020:2023`).
#' @param min_years Integer. Minimum total years of valid NEE required
#'   (default `10`).
#' @param region Character or numeric. Map extent (default `"global"`).
#' @param exclude_sites Optional character vector of site IDs to drop.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' data_yy <- readRDS("data/processed/flux_data_converted_yy.rds")
#' meta    <- readr::read_csv("data/snapshots/fluxnet_shuttle_snapshot_20260328T153608.csv")
#' p <- fig_map_nee_delta(data_yy, meta, recent_years = 2020:2023, min_years = 5)
#' print(p)
#' }
fig_map_nee_delta <- function(data_yy,
                               metadata,
                               recent_years  = 2020:2023,
                               min_years     = 10L,
                               region        = "global",
                               exclude_sites = NULL) {
  .disable_s2()
  .check_meta_cols(metadata, c("site_id", "location_lat", "location_long"))

  if (!"NEE_VUT_REF" %in% names(data_yy)) {
    stop("NEE_VUT_REF column not found in data_yy.", call. = FALSE)
  }

  flux <- data_yy
  if (!is.null(exclude_sites)) {
    flux <- dplyr::filter(flux, !site_id %in% exclude_sites)
  }

  site_stats <- flux |>
    dplyr::filter(is.finite(NEE_VUT_REF)) |>
    dplyr::mutate(is_recent = TIMESTAMP %in% recent_years) |>
    dplyr::group_by(site_id) |>
    dplyr::summarise(
      n_years      = dplyr::n_distinct(TIMESTAMP),
      n_recent     = sum(is_recent),
      mean_recent  = if (sum(is_recent) > 0L)
        mean(NEE_VUT_REF[is_recent], na.rm = TRUE) else NA_real_,
      mean_hist    = if (sum(!is_recent) > 0L)
        mean(NEE_VUT_REF[!is_recent], na.rm = TRUE) else NA_real_,
      .groups      = "drop"
    ) |>
    dplyr::mutate(delta_nee = mean_recent - mean_hist) |>
    dplyr::filter(
      n_years >= min_years,
      n_recent > 0L,
      is.finite(mean_recent),
      is.finite(mean_hist)
    )

  if (nrow(site_stats) == 0L) {
    warning(
      sprintf(
        "No sites meet criteria (>= %d years AND data in recent_years %s\u2013%s). ",
        min_years, min(recent_years), max(recent_years)
      ),
      "Try reducing min_years. Returning empty plot.",
      call. = FALSE
    )
    return(ggplot2::ggplot() +
             ggplot2::labs(
               title = sprintf(
                 "No sites: \u2265%d yrs + data in %s\u2013%s",
                 min_years, min(recent_years), max(recent_years)
               )
             ) + fluxnet_theme())
  }

  plot_data <- site_stats |>
    dplyr::left_join(
      dplyr::select(metadata, site_id, location_lat, location_long),
      by = "site_id"
    ) |>
    dplyr::filter(is.finite(location_lat), is.finite(location_long))

  lims <- .robust_limits(plot_data$delta_nee)
  land <- .land_sf()

  recent_label <- if (length(recent_years) == 1L)
    as.character(recent_years)
  else
    sprintf("%d\u2013%d", min(recent_years), max(recent_years))

  p <- .map_base(land) +
    ggplot2::geom_point(
      data  = plot_data,
      ggplot2::aes(x = location_long, y = location_lat, color = delta_nee),
      size  = 2.2, alpha = 0.85
    ) +
    ggplot2::scale_color_gradient2(
      low      = "#2C7BB6",
      mid      = "gray85",
      high     = "#D7191C",
      midpoint = 0,
      limits   = lims,
      oob      = scales::squish,
      name     = "\u0394NEE (g C m<sup>-2</sup> yr<sup>-1</sup>)"
    ) +
    ggplot2::labs(
      title    = sprintf(
        "\u0394NEE: mean(%s) \u2212 long-term mean", recent_label
      ),
      subtitle = sprintf("n = %d sites", nrow(plot_data))
    )

  .apply_region(p, region)
}
