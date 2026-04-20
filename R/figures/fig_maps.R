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
                            pt_size  = 1.8,
                            pt_alpha = 0.9,
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
        alpha = pt_alpha, stroke = 0.5
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
        alpha = pt_alpha, stroke = 0.5
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

  year_col <- if ("YEAR" %in% names(flux)) "YEAR" else "TIMESTAMP"
  site_stats <- flux |>
    dplyr::filter(is.finite(NEE_VUT_REF)) |>
    dplyr::group_by(site_id) |>
    dplyr::summarise(
      n_years  = dplyr::n_distinct(.data[[year_col]]),
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
      ggplot2::aes(x = location_long, y = location_lat, fill = mean_nee),
      shape = 21, color = "black", size = 1.8, stroke = 0.5, alpha = 0.9
    ) +
    ggplot2::scale_fill_gradient2(
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

  year_col <- if ("YEAR" %in% names(flux)) "YEAR" else "TIMESTAMP"
  site_stats <- flux |>
    dplyr::filter(is.finite(NEE_VUT_REF)) |>
    dplyr::mutate(is_recent = .data[[year_col]] %in% recent_years) |>
    dplyr::group_by(site_id) |>
    dplyr::summarise(
      n_years      = dplyr::n_distinct(.data[[year_col]]),
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
      ggplot2::aes(x = location_long, y = location_lat, fill = delta_nee),
      shape = 21, color = "black", size = 1.8, stroke = 0.5, alpha = 0.9
    ) +
    ggplot2::scale_fill_gradient2(
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

# ---- fig_map_subregion_sites ------------------------------------------------

#' UN subregion choropleth of FLUXNET site counts at multiple year cutoffs
#'
#' Counts FLUXNET sites active (\code{first_year <= cutoff <= last_year}) per
#' UN M.49 subregion at each year in \code{year_cutoffs}, then fills subregion
#' polygons by that count or site density.  Panels (one per cutoff) are
#' assembled into a patchwork with a shared sequential colour scale.
#' Requires metadata only — no flux data.
#'
#' Country polygons are fetched from \pkg{rnaturalearth}, validated with
#' \code{sf::st_make_valid()}, mapped to UN subregions via
#' \code{countrycode::countrycode(..., "un.regionsub.name")}, then dissolved
#' with \code{sf::st_union()}.  Site-to-subregion assignment uses a spatial
#' join (\code{sf::st_join()}), with \code{sf::st_nearest_feature()} as
#' fallback for the small number of sites that fall on coastlines outside
#' polygon boundaries.
#'
#' @param metadata Data frame. Snapshot CSV (one row per site) with columns
#'   \code{site_id}, \code{location_lat}, \code{location_long},
#'   \code{first_year}, and \code{last_year}.
#' @param year_cutoffs Integer vector. Years at which to count sites
#'   (default \code{c(2007L, 2015L, 2025L)}).  The filter is
#'   \code{first_year <= cutoff} — i.e. all sites established by each cutoff
#'   year, regardless of whether their most recent data has been submitted yet.
#'   This avoids representing data-submission latency as apparent network
#'   shrinkage in the most recent panel.
#' @param metric Character. \code{"count"} (default) — raw number of sites per
#'   subregion established by the cutoff; or \code{"density"} — sites per
#'   million km\eqn{^2} of subregion land area (computed with
#'   \code{sf::st_area()}).
#' @param add_dots Logical. If \code{TRUE} (default), overlay site locations
#'   as small points on top of the choropleth fill for each panel.
#'
#' @return A \pkg{patchwork} object with one panel per element of
#'   \code{year_cutoffs}. The colour scale is shared across panels.
#'
#' @examples
#' \dontrun{
#' meta <- readr::read_csv("data/snapshots/fluxnet_shuttle_snapshot_20260412T024606.csv")
#' fig_map_subregion_sites(meta)
#' fig_map_subregion_sites(meta, metric = "density")
#' }
#'
#' @export
fig_map_subregion_sites <- function(metadata,
                                     # Snapshot years: ~Marconi (2000), La Thuile (2007), FLUXNET2015 (2015), Shuttle/modern (2025)
                                     year_cutoffs = c(2000L, 2007L, 2015L, 2025L),
                                     metric       = c("count", "density"),
                                     add_dots     = TRUE) {

  for (pkg in c("patchwork", "countrycode", "rnaturalearth")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required. Install with: install.packages('",
           pkg, "')", call. = FALSE)
    }
  }

  metric <- match.arg(metric)
  .disable_s2()
  .check_meta_cols(metadata,
                   c("site_id", "location_lat", "location_long",
                     "first_year", "last_year"))

  year_cutoffs <- as.integer(year_cutoffs)

  # --- Build subregion polygons -----------------------------------------------
  countries <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
    sf::st_make_valid()

  countries$subregion <- countrycode::countrycode(
    countries$name_long, "country.name", "un.regionsub.name", warn = FALSE
  )

  # Dissolve country polygons into subregion polygons; validate result
  subregions <- countries |>
    dplyr::filter(!is.na(.data$subregion)) |>
    dplyr::group_by(.data$subregion) |>
    dplyr::summarise(
      geometry = suppressWarnings(sf::st_union(.data$geometry)),
      .groups  = "drop"
    ) |>
    sf::st_make_valid()

  if (metric == "density") {
    # st_area() requires s2 or lwgeom for lon/lat data; re-enable s2 temporarily.
    old_s2 <- sf::sf_use_s2(TRUE)
    subregions$area_mkm2 <- as.numeric(sf::st_area(subregions)) / 1e12
    sf::sf_use_s2(old_s2)
  }

  # --- Assign each site to a subregion via spatial join -----------------------
  sites_clean <- metadata |>
    dplyr::filter(
      !is.na(.data$location_lat), !is.na(.data$location_long),
      dplyr::between(.data$location_lat,  -90,  90),
      dplyr::between(.data$location_long, -180, 180)
    ) |>
    dplyr::distinct(.data$site_id, .keep_all = TRUE) |>
    dplyr::mutate(
      first_year = as.integer(.data$first_year),
      last_year  = as.integer(.data$last_year)
    )

  sites_sf <- sf::st_as_sf(
    sites_clean,
    coords = c("location_long", "location_lat"),
    crs    = 4326,
    remove = FALSE
  )

  # Primary join: point-in-polygon
  sites_joined <- sf::st_join(sites_sf, subregions["subregion"], left = TRUE)

  # Fallback for coastal/border points that miss all polygons
  na_idx <- which(is.na(sites_joined$subregion))
  if (length(na_idx) > 0L) {
    nearest <- sf::st_nearest_feature(sites_joined[na_idx, ], subregions)
    sites_joined$subregion[na_idx] <- subregions$subregion[nearest]
  }

  sites_tbl <- sf::st_drop_geometry(sites_joined)

  # Grey land background — all ne_countries polygons (validated above)
  bg_land <- countries

  # --- Shared colour scale: compute breaks from the maximum year cutoff -------
  # All panels use the same limits/breaks so visual comparison is valid across
  # the three snapshot years.
  fill_max_yr <- max(year_cutoffs)
  counts_max  <- sites_tbl |>
    dplyr::filter(.data$first_year <= fill_max_yr) |>
    dplyr::count(.data$subregion, name = "n_sites")

  if (metric == "density") {
    sub_max      <- dplyr::left_join(subregions, counts_max, by = "subregion")
    fill_max_val <- max(sub_max$n_sites / sub_max$area_mkm2, na.rm = TRUE)
  } else {
    fill_max_val <- max(counts_max$n_sites, na.rm = TRUE)
  }
  fill_max_val <- max(fill_max_val, 1)   # guard against degenerate case

  # Pretty breaks spanning [0, fill_max_val]
  shared_breaks <- pretty(c(0, fill_max_val), n = 5L)
  shared_breaks <- shared_breaks[shared_breaks >= 0 & shared_breaks <= fill_max_val]

  # --- One panel per year cutoff ----------------------------------------------
  panels <- lapply(year_cutoffs, function(yr) {
    active_sites <- sites_tbl |> dplyr::filter(.data$first_year <= yr)
    counts <- dplyr::count(active_sites, .data$subregion, name = "n_sites")

    n_active     <- nrow(active_sites)
    n_site_years <- sum(
      pmin(as.integer(active_sites$last_year), yr) -
        as.integer(active_sites$first_year) + 1L,
      na.rm = TRUE
    )
    plot_sub  <- dplyr::left_join(subregions, counts, by = "subregion")

    plot_sub$fill_val <- if (metric == "density") {
      plot_sub$n_sites / plot_sub$area_mkm2
    } else {
      plot_sub$n_sites
    }

    p <- ggplot2::ggplot() +
      ggplot2::theme_void(base_size = 10L) +
      ggplot2::theme(
        plot.title      = ggplot2::element_text(hjust = 0.5, face = "bold",
                                                size  = 10L),
        plot.subtitle   = ggplot2::element_text(hjust = 0.5, size = 8L,
                                                color = "grey40"),
        legend.position = "bottom",
        legend.title    = ggplot2::element_text(size = 9L, hjust = 0.5)
      ) +
      # Grey background for all land (validated)
      ggplot2::geom_sf(data = bg_land, fill = "grey90", colour = "white",
                       linewidth = 0.1) +
      # Subregion choropleth fill
      ggplot2::geom_sf(
        data  = plot_sub,
        ggplot2::aes(fill = .data$fill_val),
        colour    = "white",
        linewidth = 0.25
      ) +
      # Shared scale across all panels — breaks and limits from max year cutoff
      (if (metric == "count") {
        ggplot2::scale_fill_viridis_b(
          breaks   = shared_breaks,
          limits   = c(0, fill_max_val),
          na.value = "grey90",
          name     = "Number of sites",
          option   = "viridis"
        )
      } else {
        ggplot2::scale_fill_viridis_b(
          breaks   = shared_breaks,
          limits   = c(0, fill_max_val),
          na.value = "grey90",
          name     = "Sites per million km\u00b2",
          option   = "viridis"
        )
      }) +
      ggplot2::labs(
        title    = as.character(yr),
        subtitle = paste0("n\u2009=\u2009", n_active, " sites established by ", yr),
        caption  = if (yr >= 2025L)
          "2025 panel reflects sites established by 2025 \u2014 recent data may not yet be available."
        else
          NULL
      ) +
      ggplot2::coord_sf(expand = FALSE) +
      ggplot2::annotate(
        "text", x = Inf, y = Inf,
        label = paste0("n = ", n_active, "\nsite-years = ", n_site_years),
        hjust = 1.1, vjust = 1.5,
        size  = 3.5
      )

    if (add_dots) {
      active_sites <- sites_tbl |>
        dplyr::filter(.data$first_year <= yr)
      p <- p +
        ggplot2::geom_point(
          data  = active_sites,
          ggplot2::aes(x = .data$location_long, y = .data$location_lat),
          shape  = 21,
          fill   = "white",
          color  = "black",
          size   = 1.8,
          stroke = 0.5,
          alpha  = 0.9
        )
    }

    p
  })

  patchwork::wrap_plots(panels, ncol = 1L) +
    patchwork::plot_layout(guides = "collect") &
    ggplot2::theme(legend.position = "bottom")
}

# ---- MAP_STYLE --------------------------------------------------------------

#' Shared visual parameters for all historical choropleth map figures
#'
#' A named list used as the default \code{style} argument to
#' \code{\link{fig_map_historical}}.  Override individual elements by
#' passing a modified copy.
#'
#' @format A named list:
#' \describe{
#'   \item{width_in, height_in}{Default ggsave dimensions in inches.}
#'   \item{base_size}{Base font size for \code{theme_void} (points).}
#'   \item{legend_pos, legend_just}{Legend position and justification (NDC).}
#'   \item{detail_x, detail_y}{Inset detail-text anchor (NDC fractions).}
#'   \item{breaks}{Colour scale breaks; \code{NULL} — set at runtime via
#'     \code{scale_breaks} argument.  Computed from 95th percentile of
#'     per-subregion site counts across all datasets before first call.}
#'   \item{na_fill}{Fill colour for subregions with no sites.}
#' }
#' @export
MAP_STYLE <- list(
  width_in    = 14,
  height_in   = 7,
  base_size   = 24,
  legend_pos  = c(0.02, 0.88),
  legend_just = c(0, 1),
  detail_x    = 0.02,
  detail_y    = 0.98,
  # Colour scale breaks clipped to avoid US dominance.
  # Compute from 95th percentile of per-subregion site counts across all datasets.
  breaks      = NULL,
  na_fill     = "grey90"
)

# ---- fig_map_historical -------------------------------------------------------

#' Single-panel UN subregion choropleth for any FLUXNET site list
#'
#' Counts sites per UN M.49 subregion, fills subregion polygons by that count,
#' and overlays individual site locations as dots.  Designed to be called nine
#' times from \code{scripts/generate_maps.R} with a shared \code{scale_breaks}
#' vector so all panels are directly comparable across datasets and snapshot
#' years.
#'
#' Country polygons are fetched from \pkg{rnaturalearth}, mapped to UN
#' subregions via \code{countrycode::countrycode()}, and dissolved with
#' \code{sf::st_union()}.  Site-to-subregion assignment uses a spatial join
#' with \code{sf::st_nearest_feature()} fallback for coastal sites.
#'
#' @param site_meta Data frame.  Must contain \code{site_id},
#'   \code{location_lat}, and \code{location_long}.  If \code{year_cutoff}
#'   is non-\code{NULL}, also requires \code{first_year}.
#' @param detail_label Character scalar or \code{NULL}.  Dataset label shown
#'   in the top-left inset text (e.g. \code{"FLUXNET Shuttle 2025"}).
#' @param year_cutoff Integer or \code{NULL}.  If set, filters
#'   \code{site_meta} to rows where \code{first_year <= year_cutoff}.
#' @param scale_breaks Numeric vector or \code{NULL}.  Shared colour scale
#'   breaks passed to \code{\link[ggplot2]{scale_fill_viridis_b}}.  Compute
#'   once in the calling script from the 95th percentile of per-subregion
#'   site counts across all datasets and pass the same vector to every call.
#'   Falls back to \code{pretty()} breaks when \code{NULL}.
#' @param style Named list.  Visual parameters; defaults to
#'   \code{\link{MAP_STYLE}}.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' meta   <- readr::read_csv(
#'   "data/snapshots/fluxnet_shuttle_snapshot_20260414T154430.csv"
#' )
#' breaks <- c(0, 5, 15, 30, 50, 100)
#' p <- fig_map_historical(meta, "FLUXNET Shuttle 2025", scale_breaks = breaks)
#' print(p)
#' }
#' @export
fig_map_historical <- function(site_meta,
                                detail_label = NULL,
                                year_cutoff  = NULL,
                                scale_breaks = NULL,
                                style        = MAP_STYLE) {

  for (pkg in c("countrycode", "rnaturalearth")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required. Install with: install.packages('",
           pkg, "')", call. = FALSE)
    }
  }

  .disable_s2()
  .check_meta_cols(site_meta, c("site_id", "location_lat", "location_long"))

  if (!is.null(year_cutoff)) {
    .check_meta_cols(site_meta, "first_year")
    site_meta <- dplyr::filter(
      site_meta,
      as.integer(.data$first_year) <= as.integer(year_cutoff)
    )
  }

  sites_clean <- site_meta |>
    dplyr::filter(
      !is.na(.data$location_lat), !is.na(.data$location_long),
      dplyr::between(.data$location_lat,   -90,  90),
      dplyr::between(.data$location_long, -180, 180)
    ) |>
    dplyr::distinct(.data$site_id, .keep_all = TRUE)

  n_sites <- nrow(sites_clean)

  # Build subregion polygons (country polygons dissolved to UN M.49 subregions)
  countries <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
    sf::st_make_valid()

  countries$subregion <- countrycode::countrycode(
    countries$name_long, "country.name", "un.regionsub.name", warn = FALSE
  )

  subregions <- countries |>
    dplyr::filter(!is.na(.data$subregion)) |>
    dplyr::group_by(.data$subregion) |>
    dplyr::summarise(
      geometry = suppressWarnings(sf::st_union(.data$geometry)),
      .groups  = "drop"
    ) |>
    sf::st_make_valid()

  # Assign each site to a subregion via spatial join + nearest-feature fallback
  sites_sf <- sf::st_as_sf(
    sites_clean,
    coords = c("location_long", "location_lat"),
    crs    = 4326,
    remove = FALSE
  )

  sites_joined <- sf::st_join(sites_sf, subregions["subregion"], left = TRUE)

  na_idx <- which(is.na(sites_joined$subregion))
  if (length(na_idx) > 0L) {
    nearest <- sf::st_nearest_feature(sites_joined[na_idx, ], subregions)
    sites_joined$subregion[na_idx] <- subregions$subregion[nearest]
  }

  sites_tbl <- sf::st_drop_geometry(sites_joined)
  counts    <- dplyr::count(sites_tbl, .data$subregion, name = "n_sites")
  plot_sub  <- dplyr::left_join(subregions, counts, by = "subregion")

  # Colour scale breaks — fall back to pretty() if not supplied
  if (is.null(scale_breaks)) {
    fill_max     <- max(counts$n_sites, 1L)
    scale_breaks <- pretty(c(0, fill_max), n = 5L)
    scale_breaks <- scale_breaks[scale_breaks >= 0 & scale_breaks <= fill_max]
  }
  fill_limits <- c(0, max(scale_breaks))

  # Inset detail text (top-left)
  detail_text <- if (!is.null(detail_label)) {
    paste0(detail_label, "\nN = ", n_sites, " sites")
  } else {
    paste0("N = ", n_sites, " sites")
  }
  annot_size <- style$base_size * 0.25   # mm; ≈ base_size pt for base_size = 24

  ggplot2::ggplot() +
    ggplot2::theme_void(base_size = style$base_size) +
    ggplot2::theme(
      legend.position      = style$legend_pos,
      legend.justification = style$legend_just,
      legend.direction     = "horizontal",
      legend.title         = ggplot2::element_text(size  = style$base_size * 0.7,
                                                   hjust = 0.5),
      legend.text          = ggplot2::element_text(size  = style$base_size * 0.65)
    ) +
    # Grey background for all land
    ggplot2::geom_sf(data = countries, fill = "grey90", colour = "white",
                     linewidth = 0.1) +
    # UN subregion choropleth fill
    ggplot2::geom_sf(
      data  = plot_sub,
      ggplot2::aes(fill = .data$n_sites),
      colour = "white", linewidth = 0.25
    ) +
    ggplot2::scale_fill_viridis_b(
      breaks   = scale_breaks,
      limits   = fill_limits,
      oob      = scales::squish,
      na.value = style$na_fill,
      name     = "Sites",
      option   = "viridis",
      guide    = ggplot2::guide_colorsteps(
        title.position = "top",
        barwidth       = ggplot2::unit(10, "lines"),
        barheight      = ggplot2::unit(0.8, "lines")
      )
    ) +
    # Site location dots
    ggplot2::geom_point(
      data  = sites_clean,
      ggplot2::aes(x = .data$location_long, y = .data$location_lat),
      shape  = 21,
      fill   = "white",
      color  = "black",
      size   = 1.5,
      stroke = 0.4
    ) +
    # Inset detail text: top-left corner
    ggplot2::annotate(
      "text",
      x        = -Inf, y = Inf,
      label    = detail_text,
      hjust    = -0.05, vjust = 1.3,
      size     = annot_size,
      fontface = "bold"
    ) +
    ggplot2::coord_sf(expand = FALSE)
}

# ---- fig_map_country_sites (deprecated alias) --------------------------------

#' Deprecated: country choropleth of FLUXNET site counts
#'
#' This function is deprecated. Use \code{\link{fig_map_subregion_sites}}
#' instead, which aggregates to UN M.49 subregions rather than individual
#' countries and supports \code{metric = "count"} or \code{"density"}.
#'
#' @inheritParams fig_map_subregion_sites
#' @return A \pkg{patchwork} object. See \code{\link{fig_map_subregion_sites}}.
#' @export
fig_map_country_sites <- function(metadata,
                                   # Snapshot years: ~Marconi (2000), La Thuile (2007), FLUXNET2015 (2015), Shuttle/modern (2025)
                                   year_cutoffs = c(2000L, 2007L, 2015L, 2025L)) {
  .Deprecated(
    new = "fig_map_subregion_sites",
    msg = paste0(
      "'fig_map_country_sites()' is deprecated. ",
      "Use 'fig_map_subregion_sites()' instead."
    )
  )
  fig_map_subregion_sites(metadata, year_cutoffs = year_cutoffs)
}
