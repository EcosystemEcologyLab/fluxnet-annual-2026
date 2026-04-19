# R/figures/fig_historical_comparison.R
# Historical FLUXNET dataset comparison figures for the Annual Paper 2026.
#
# Three figure families, each available in per-dataset and combined-panel forms:
#   fig_choropleth_datasets()    — UN subregion choropleth (site counts)
#   fig_duration_datasets()      — deployment duration histograms
#   fig_whittaker_datasets()     — Whittaker biome hexbin (WorldClim + NEE)
#
# Master entry point:
#   fig_historical_dataset_comparison() — generates all three families and saves
#     outputs to review/figures/historical/
#
# Requires:
#   R/historical_datasets.R   — load_historical_site_lists(), DATASET_PUB_YEARS
#   R/plot_constants.R        — fluxnet_theme(), lab_nee_annual, etc.

source("R/plot_constants.R")
source("R/historical_datasets.R")

# ---- Internal: subregion polygon builder ------------------------------------

#' Build UN M.49 subregion polygons and assign each site to a subregion
#'
#' Returns a list used by the choropleth functions. Handles s2 suppression,
#' country dissolve, and nearest-feature fallback for coastal sites.
#'
#' @param sites_list Named list of site data frames (each with site_id,
#'   location_lat, location_long).
#' @return Named list: `$subregions` (sf), `$sites_joined` (named list of
#'   data frames, one per dataset), `$bg_land` (sf).
#' @noRd
.build_subregion_data <- function(sites_list) {
  for (pkg in c("sf", "rnaturalearth", "countrycode")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required.", call. = FALSE)
    }
  }
  suppressMessages(sf::sf_use_s2(FALSE))

  countries  <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
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
  sites_joined <- lapply(sites_list, function(df) {
    clean <- df |>
      dplyr::filter(
        !is.na(.data$location_lat), !is.na(.data$location_long),
        dplyr::between(.data$location_lat,  -90,  90),
        dplyr::between(.data$location_long, -180, 180)
      ) |>
      dplyr::distinct(.data$site_id, .keep_all = TRUE)

    pts    <- sf::st_as_sf(clean, coords = c("location_long", "location_lat"),
                           crs = 4326, remove = FALSE)
    joined <- sf::st_join(pts, subregions["subregion"], left = TRUE)
    na_idx <- which(is.na(joined$subregion))
    if (length(na_idx) > 0L) {
      nearest <- sf::st_nearest_feature(joined[na_idx, ], subregions)
      joined$subregion[na_idx] <- subregions$subregion[nearest]
    }
    sf::st_drop_geometry(joined)
  })

  list(subregions = subregions, sites_joined = sites_joined, bg_land = countries)
}

#' Build a single choropleth map panel
#'
#' @param sites_tbl Data frame of sites with location_lat, location_long,
#'   subregion columns.
#' @param subregions sf. Subregion polygon sf object.
#' @param bg_land sf. Land background polygon.
#' @param panel_title Character. Title for this panel.
#' @param shared_breaks Numeric. Fill scale break positions.
#' @param fill_max_val Numeric. Maximum fill value (upper colour limit).
#' @param add_dots Logical. Overlay site dots.
#' @param base_size Integer. Theme base font size.
#' @return ggplot object.
#' @noRd
.choropleth_panel <- function(sites_tbl, subregions, bg_land,
                               panel_title, shared_breaks, fill_max_val,
                               add_dots = TRUE, base_size = 10L) {
  counts   <- dplyr::count(sites_tbl, .data$subregion, name = "n_sites")
  plot_sub <- dplyr::left_join(subregions, counts, by = "subregion")

  p <- ggplot2::ggplot() +
    ggplot2::theme_void(base_size = base_size) +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(hjust = 0.5, face = "bold",
                                              size  = base_size),
      plot.subtitle   = ggplot2::element_text(hjust = 0.5,
                                              size  = base_size * 0.85,
                                              color = "grey40"),
      legend.position = "bottom",
      legend.title    = ggplot2::element_text(size = base_size * 0.9,
                                              hjust = 0.5)
    ) +
    ggplot2::geom_sf(data = bg_land, fill = "grey90", colour = "white",
                     linewidth = 0.1) +
    ggplot2::geom_sf(
      data  = plot_sub,
      ggplot2::aes(fill = .data$n_sites),
      colour = "white", linewidth = 0.25
    ) +
    ggplot2::scale_fill_viridis_b(
      breaks   = shared_breaks,
      limits   = c(0, fill_max_val),
      na.value = "grey90",
      name     = "Number of sites",
      option   = "viridis"
    ) +
    ggplot2::labs(
      title    = panel_title,
      subtitle = paste0("n\u2009=\u2009", nrow(sites_tbl), " sites")
    ) +
    ggplot2::coord_sf(expand = FALSE)

  if (add_dots) {
    p <- p +
      ggplot2::geom_point(
        data  = sites_tbl,
        ggplot2::aes(x = .data$location_long, y = .data$location_lat),
        shape = 21, fill = "white", color = "black",
        size  = 1.2, stroke = 0.4, alpha = 0.85
      )
  }
  p
}

# ---- fig_choropleth_datasets ------------------------------------------------

#' UN subregion choropleth for all four historical FLUXNET datasets
#'
#' Generates one choropleth map per dataset showing site counts per UN M.49
#' subregion. All four panels use a shared colour scale whose breaks are
#' computed from the Shuttle dataset (the largest). Individual panels are
#' returned for standalone saving; a four-panel stacked combined figure is
#' also returned.
#'
#' @param site_lists Named list from [load_historical_site_lists()].
#' @param add_dots Logical. Overlay site location dots (default `TRUE`).
#'
#' @return Named list with elements:
#'   - `$individual`: named list of four ggplots (names match `site_lists`).
#'   - `$combined`: patchwork of all four panels stacked vertically.
#'
#' @examples
#' \dontrun{
#' sl   <- load_historical_site_lists()
#' figs <- fig_choropleth_datasets(sl)
#' ggplot2::ggsave("fig_choropleth_all.png", figs$combined,
#'                 width = 10, height = 20, units = "in", dpi = 150, bg = "white")
#' }
fig_choropleth_datasets <- function(site_lists, add_dots = TRUE) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package 'patchwork' is required.", call. = FALSE)
  }

  spatial   <- .build_subregion_data(site_lists)
  sub_poly  <- spatial$subregions
  bg_land   <- spatial$bg_land
  joined    <- spatial$sites_joined

  # Shared colour scale: breaks from Shuttle (the dataset with most sites)
  shuttle_counts <- dplyr::count(joined[["shuttle"]], .data$subregion,
                                 name = "n_sites")
  fill_max_val   <- max(c(shuttle_counts$n_sites, 1L), na.rm = TRUE)
  shared_breaks  <- pretty(c(0, fill_max_val), n = 5L)
  shared_breaks  <- shared_breaks[shared_breaks >= 0 &
                                    shared_breaks <= fill_max_val]

  # Panel title: "Marconi 2000 (N=35, Site Years=96)"
  site_years_vec <- attr(site_lists, "site_years")
  panel_titles <- vapply(names(site_lists), function(nm) {
    lbl <- unique(site_lists[[nm]]$dataset)[[1L]]
    sy  <- site_years_vec[[nm]]
    sy_str <- if (!is.null(sy) && !is.na(sy)) paste0(", Site Years=", sy) else ""
    paste0(lbl, " (N=", nrow(site_lists[[nm]]), sy_str, ")")
  }, character(1L))

  panels <- lapply(names(site_lists), function(nm) {
    .choropleth_panel(
      sites_tbl     = joined[[nm]],
      subregions    = sub_poly,
      bg_land       = bg_land,
      panel_title   = panel_titles[[nm]],
      shared_breaks = shared_breaks,
      fill_max_val  = fill_max_val,
      add_dots      = add_dots
    )
  })
  names(panels) <- names(site_lists)

  combined <- patchwork::wrap_plots(panels, ncol = 1L) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(
      title    = "FLUXNET site coverage by UN subregion \u2014 historical datasets",
      subtitle = "Colour scale fixed to Shuttle 2025 maximum. Dots = individual site locations."
    ) &
    ggplot2::theme(legend.position = "bottom")

  list(individual = panels, combined = combined)
}

# ---- fig_duration_datasets --------------------------------------------------

#' Deployment duration histograms for all four historical FLUXNET datasets
#'
#' Four panels stacked vertically with shared x and y axis limits. Record
#' length is taken from `record_length` in the site list (n_years_valid_nee
#' for Shuttle sites where available; last_year - first_year otherwise).
#'
#' Fill colours encode current Shuttle activity status (assessed relative to
#' the 2025 Shuttle snapshot regardless of panel):
#' - **Functionally active** (blue): site is in the Shuttle AND functionally
#'   active (\u22653 months valid NEE in any year within 2022\u20132025 when
#'   `presence_df` is supplied; `last_year >= 2021` otherwise).
#' - **Inactive / high latency** (grey): site is in the Shuttle but not
#'   functionally active.
#' - **Historical only** (amber): site is not in the Shuttle.
#'
#' Sites without a determinable record length (non-Shuttle historical sites
#' with no first/last year data) are excluded from the histogram but counted
#' in the inset label.
#'
#' @param site_lists Named list from [load_historical_site_lists()].
#' @param presence_df Data frame or `NULL`. Output of
#'   [compute_site_year_presence()]. When supplied, activity status is
#'   determined by NEE data presence (\u22653 months in the 4-year window
#'   2022\u20132025); when `NULL`, falls back to `last_year >= 2021`.
#'
#' @return A patchwork of four vertically stacked panels sharing the x-axis.
#'
#' @examples
#' \dontrun{
#' sl <- load_historical_site_lists()
#' p  <- fig_duration_datasets(sl)
#' ggplot2::ggsave("fig_duration_all_datasets.png", p,
#'                 width = 8, height = 14, units = "in", dpi = 150, bg = "white")
#' }
fig_duration_datasets <- function(site_lists, presence_df = NULL) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package 'patchwork' is required.", call. = FALSE)
  }

  activity_colours <- c(
    "Functionally active"     = "#2196F3",
    "Inactive / high latency" = "#B0BEC5",
    "Historical only"         = "#C9A227"
  )

  # Build per-dataset plot data (exclude NA record_length)
  plot_data <- lapply(names(site_lists), function(nm) {
    df           <- site_lists[[nm]]
    active_flags <- is_functionally_active(
      df$site_id,
      reference_year   = 2025L,
      presence_df      = presence_df,
      active_threshold = 4L,
      last_year_vec    = df$last_year
    )
    df |>
      dplyr::filter(!is.na(.data$record_length)) |>
      dplyr::mutate(
        activity = factor(
          dplyr::case_when(
            !.data$in_shuttle                               ~ "Historical only",
            .data$in_shuttle & active_flags[.data$site_id] ~ "Functionally active",
            TRUE                                            ~ "Inactive / high latency"
          ),
          levels = c("Functionally active", "Inactive / high latency",
                     "Historical only")
        )
      )
  })
  names(plot_data) <- names(site_lists)

  # Shared axis limits derived from the Shuttle panel
  x_max <- max(plot_data[["shuttle"]]$record_length, na.rm = TRUE)
  x_lim <- c(0, x_max + 1L)

  bin_max <- max(
    vapply(plot_data, function(df) {
      if (nrow(df) == 0L) return(0L)
      dplyr::count(df, rl = round(.data$record_length)) |>
        dplyr::pull(.data$n) |>
        max(0L)
    }, numeric(1L)),
    na.rm = TRUE
  )
  y_lim <- c(0, ceiling(bin_max * 1.08))

  n_panels       <- length(site_lists)
  base_sz        <- 14L
  site_years_vec <- attr(site_lists, "site_years")

  panels <- lapply(seq_along(site_lists), function(i) {
    nm        <- names(site_lists)[[i]]
    df        <- plot_data[[nm]]
    is_bottom <- i == n_panels
    n_total   <- nrow(site_lists[[nm]])
    lbl       <- unique(site_lists[[nm]]$dataset)[[1L]]
    sy        <- site_years_vec[[nm]]
    sy_str    <- if (!is.null(sy) && !is.na(sy)) paste0(", Site Years=", sy) else ""

    ggplot2::ggplot(
      df,
      ggplot2::aes(x = .data$record_length, fill = .data$activity)
    ) +
      ggplot2::geom_histogram(binwidth = 1L, colour = "white", linewidth = 0.2,
                              position = "stack") +
      ggplot2::scale_fill_manual(values = activity_colours, name = NULL,
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
      ggplot2::annotate(
        "text", x = -Inf, y = Inf,
        label    = paste0(lbl, " (N=", n_total, sy_str, ")"),
        hjust    = -0.07, vjust = 1.5,
        size     = 5.5, fontface = "bold"
      ) +
      ggplot2::labs(
        x = if (is_bottom) "Record length (years)" else NULL,
        y = NULL
      ) +
      fluxnet_theme(base_size = base_sz) +
      ggplot2::theme(
        axis.text.x  = if (is_bottom) ggplot2::element_text()
                       else           ggplot2::element_blank(),
        axis.ticks.x = if (is_bottom) ggplot2::element_line()
                       else           ggplot2::element_blank(),
        legend.position = if (i == 1L) c(0.78, 0.82) else "none",
        plot.margin  = ggplot2::margin(0, 5, 0, 5, unit = "pt")
      )
  })

  # Shared Y axis label as narrow ggplot column
  y_label <- ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0.5, y = 0.5, label = "Sites",
                      angle = 90, size = 5.5) +
    ggplot2::theme_void()

  stack <- patchwork::wrap_plots(panels, ncol = 1L) +
    patchwork::plot_layout(guides = "keep") &
    ggplot2::theme(plot.margin = ggplot2::margin(0, 5, 0, 5))

  (y_label | stack) +
    patchwork::plot_layout(widths = c(0.04, 1)) +
    patchwork::plot_annotation(
      title    = "Deployment duration profile \u2014 historical FLUXNET datasets",
      subtitle = paste0(
        "Record length from n_years_valid_nee (Shuttle sites) or last_year \u2212 first_year. ",
        "Blue = currently active in Shuttle (last_year \u2265 2021); ",
        "Amber = site not in Shuttle."
      )
    )
}

# ---- fig_whittaker_datasets -------------------------------------------------

#' Whittaker biome hexbin for all four historical FLUXNET datasets
#'
#' Each panel plots sites on a MAT × MAP climate space (WorldClim 2.1 bio1 and
#' bio12) with hexagonal binning coloured by median NEE from Shuttle flux data
#' where available. Sites not in the Shuttle have `mean_flux = NA`; hexes with
#' no Shuttle flux data are not rendered (NA bins are dropped by
#' `stat_summary_hex`).
#'
#' Climate source priority:
#' 1. `site_worldclim.csv` (pre-computed for all 672 Shuttle sites)
#' 2. `terra::extract()` from WorldClim 2.1 2.5m GeoTIFFs at site coordinates
#'    (for the ~266 historical sites not in the Shuttle)
#'
#' NEE colour scale limits are computed from the 5th–95th percentile of the
#' full Shuttle flux distribution and shared across all four panels.
#'
#' @param site_lists Named list from [load_historical_site_lists()].
#' @param data_yy Annual flux data frame. Must contain `site_id` and
#'   `NEE_VUT_REF` (gC m⁻² yr⁻¹ post-conversion).
#' @param worldclim_path Character. Directory containing WorldClim 2.1 2.5m
#'   bio GeoTIFF files (default `"data/external/worldclim/climate/wc2.1_2.5m/"`).
#' @param worldclim_csv Character. Path to pre-computed WorldClim site table
#'   (default `"data/snapshots/site_worldclim.csv"`).
#'
#' @return Named list:
#'   - `$individual`: named list of four ggplots.
#'   - `$combined`: patchwork of all four panels stacked vertically.
#'
#' @examples
#' \dontrun{
#' sl      <- load_historical_site_lists()
#' data_yy <- readRDS("data/processed/flux_data_converted_yy.rds")
#' figs    <- fig_whittaker_datasets(sl, data_yy)
#' print(figs$combined)
#' }
fig_whittaker_datasets <- function(site_lists,
                                    data_yy,
                                    worldclim_path = "data/external/worldclim/climate/wc2.1_2.5m/",
                                    worldclim_csv  = "data/snapshots/site_worldclim.csv") {
  for (pkg in c("hexbin", "patchwork", "colorspace")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required.", call. = FALSE)
    }
  }

  # --- Per-site mean NEE from Shuttle data ------------------------------------
  site_nee <- data_yy |>
    dplyr::filter(!is.na(.data$NEE_VUT_REF)) |>
    dplyr::group_by(.data$site_id) |>
    dplyr::summarise(mean_flux = mean(.data$NEE_VUT_REF, na.rm = TRUE),
                     .groups   = "drop")

  # Shared NEE limits: 5th–95th percentile of Shuttle distribution
  nee_lims <- quantile(site_nee$mean_flux, probs = c(0.05, 0.95), na.rm = TRUE)
  nee_max  <- max(abs(nee_lims))

  # --- WorldClim climate lookup -----------------------------------------------
  wc_known <- readr::read_csv(worldclim_csv, show_col_types = FALSE)

  # All unique sites across all datasets (need climate for each)
  all_sites <- purrr::map_dfr(site_lists, function(df) {
    dplyr::select(df, "site_id", "location_lat", "location_long")
  }) |>
    dplyr::distinct(.data$site_id, .keep_all = TRUE)

  # Sites without pre-computed WorldClim values
  need_extract <- all_sites |>
    dplyr::filter(
      !.data$site_id %in% wc_known$site_id,
      !is.na(.data$location_lat), !is.na(.data$location_long)
    )

  wc_extracted <- NULL
  if (nrow(need_extract) > 0L) {
    if (requireNamespace("terra", quietly = TRUE) && dir.exists(worldclim_path)) {
      bio_files  <- list.files(worldclim_path, pattern = "\\.tif$",
                               full.names = TRUE)
      # Match wc2.1_2.5m_bio_1.tif and wc2.1_2.5m_bio_12.tif specifically
      bio1_file  <- bio_files[grepl("_bio_1\\.tif$",  bio_files, ignore.case = TRUE)]
      bio12_file <- bio_files[grepl("_bio_12\\.tif$", bio_files, ignore.case = TRUE)]

      if (length(bio1_file) == 1L && length(bio12_file) == 1L) {
        wc_rast <- terra::rast(c(bio1_file, bio12_file))
        pts     <- terra::vect(
          data.frame(x = need_extract$location_long,
                     y = need_extract$location_lat),
          geom = c("x", "y"), crs = "EPSG:4326"
        )
        vals <- as.data.frame(terra::extract(wc_rast, pts, ID = FALSE))
        # WorldClim 2.x stores MAT in °C; values > 70 °C absolute indicate
        # legacy ×10 encoding — divide to recover °C.
        mat_raw <- vals[[1L]]
        mat_c   <- if (max(abs(mat_raw), na.rm = TRUE) > 70) mat_raw / 10 else mat_raw
        wc_extracted <- need_extract |>
          dplyr::mutate(mat_worldclim = mat_c, map_worldclim = vals[[2L]])
        message("fig_whittaker_datasets: extracted WorldClim for ",
                nrow(wc_extracted), " non-Shuttle sites.")
      } else {
        message("fig_whittaker_datasets: bio_1.tif or bio_12.tif not found in ",
                worldclim_path, ". Non-Shuttle sites will lack climate data.")
      }
    } else {
      message("fig_whittaker_datasets: terra unavailable or worldclim_path missing. ",
              nrow(need_extract), " non-Shuttle sites will lack climate data.")
    }
  }

  # Combine pre-computed and extracted WorldClim values
  wc_all <- dplyr::bind_rows(
    wc_known,
    if (!is.null(wc_extracted))
      dplyr::select(wc_extracted, "site_id", "mat_worldclim", "map_worldclim")
    else
      NULL
  )

  # --- Per-panel builder ------------------------------------------------------
  site_years_vec <- attr(site_lists, "site_years")

  .whittaker_panel <- function(nm, dataset_df) {
    pd <- dataset_df |>
      dplyr::left_join(
        dplyr::select(wc_all, "site_id", "mat_worldclim", "map_worldclim"),
        by = "site_id"
      ) |>
      dplyr::left_join(site_nee, by = "site_id") |>
      dplyr::filter(!is.na(.data$mat_worldclim), !is.na(.data$map_worldclim))

    if (nrow(pd) == 0L) {
      return(ggplot2::ggplot() +
               ggplot2::labs(title = nm, subtitle = "No climate data") +
               fluxnet_theme())
    }

    n_total    <- nrow(dataset_df)
    n_climate  <- nrow(pd)
    n_nee      <- sum(!is.na(pd$mean_flux))
    lbl        <- unique(dataset_df$dataset)[[1L]]
    sy         <- site_years_vec[[nm]]
    sy_str     <- if (!is.null(sy) && !is.na(sy)) paste0(", Site Years=", sy) else ""

    ggplot2::ggplot(
      pd,
      ggplot2::aes(x = .data$mat_worldclim, y = .data$map_worldclim,
                   z = .data$mean_flux)
    ) +
      ggplot2::stat_summary_hex(
        fun   = function(x) if (all(is.na(x))) NA_real_
                            else median(x, na.rm = TRUE),
        bins  = 15,
        alpha = 0.85
      ) +
      colorspace::scale_fill_continuous_diverging(
        palette  = "Blue-Red 3",
        mid      = 0,
        limits   = c(-nee_max, nee_max),
        oob      = scales::squish,
        na.value = NA,          # NA bins not rendered (no hex plotted)
        name     = lab_nee_annual,
        guide    = ggplot2::guide_colorbar(
          title.position = "top",
          barwidth       = 15,
          barheight      = 0.8,
          direction      = "horizontal",
          title.theme    = ggtext::element_markdown()
        )
      ) +
      ggplot2::geom_point(
        ggplot2::aes(x = .data$mat_worldclim, y = .data$map_worldclim),
        size        = 1.4,
        colour      = "grey30",
        alpha       = 0.50,
        inherit.aes = FALSE
      ) +
      ggplot2::annotate(
        "text", x = -Inf, y = Inf,
        label    = paste0(lbl, " (N=", n_total, sy_str, ")"),
        hjust    = -0.07, vjust = 1.5,
        size     = 4.5, fontface = "bold"
      ) +
      ggplot2::labs(
        x        = "Mean annual temperature (\u00b0C)",
        y        = "Mean annual precipitation (mm yr<sup>-1</sup>)",
        subtitle = paste0(
          n_climate, " sites with climate data; ",
          n_nee, " with Shuttle NEE"
        )
      ) +
      fluxnet_theme() +
      ggplot2::theme(legend.position = "bottom")
  }

  panels <- lapply(names(site_lists), function(nm) {
    .whittaker_panel(nm, site_lists[[nm]])
  })
  names(panels) <- names(site_lists)

  combined <- patchwork::wrap_plots(panels, ncol = 1L) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(
      title    = "FLUXNET sites in Whittaker biome space \u2014 historical datasets",
      subtitle = paste0(
        "Climate from WorldClim 2.1 (bio1 MAT, bio12 MAP). ",
        "NEE fill from Shuttle flux data (Shuttle sites only). ",
        "Grey hexes = climate space with no Shuttle NEE data."
      )
    ) &
    ggplot2::theme(legend.position = "bottom")

  list(individual = panels, combined = combined)
}

# ---- fig_historical_dataset_comparison (master) -----------------------------

#' Generate all historical FLUXNET dataset comparison figures
#'
#' Master function calling [fig_choropleth_datasets()],
#' [fig_duration_datasets()], and [fig_whittaker_datasets()]. Saves all outputs
#' to `out_dir` and returns the figure objects invisibly.
#'
#' Output files:
#' - `fig_choropleth_{dataset}.png`   × 4 individual datasets
#' - `fig_choropleth_all_datasets.png`
#' - `fig_duration_all_datasets.png`
#' - `fig_whittaker_{dataset}.png`    × 4 individual datasets
#' - `fig_whittaker_all_datasets.png`
#'
#' @param site_lists Named list from [load_historical_site_lists()].
#' @param data_yy Annual flux data frame (required for Whittaker; if `NULL`
#'   Whittaker figures are skipped with a message).
#' @param worldclim_path Character. WorldClim 2.1 2.5m GeoTIFF directory.
#' @param worldclim_csv Character. Pre-computed WorldClim site-level table.
#' @param out_dir Character. Output directory
#'   (default `"review/figures/historical"`).
#' @param dpi Integer. PNG resolution in dots per inch (default `150L`).
#'
#' @return Invisibly, a named list: `$choropleth`, `$duration`, `$whittaker`,
#'   each containing the ggplot / patchwork objects generated.
#'
#' @examples
#' \dontrun{
#' sl      <- load_historical_site_lists()
#' data_yy <- readRDS("data/processed/flux_data_converted_yy.rds")
#' fig_historical_dataset_comparison(sl, data_yy)
#' }
fig_historical_dataset_comparison <- function(
  site_lists,
  data_yy        = NULL,
  worldclim_path = "data/external/worldclim/climate/wc2.1_2.5m/",
  worldclim_csv  = "data/snapshots/site_worldclim.csv",
  out_dir        = "review/figures/historical",
  dpi            = 150L
) {
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
    message("Created output directory: ", out_dir)
  }

  ds_slugs <- c("marconi", "la_thuile", "fluxnet2015", "shuttle")

  # ---- Choropleth ----
  message("\n--- Generating choropleth figures ---")
  choro <- fig_choropleth_datasets(site_lists)

  for (i in seq_along(ds_slugs)) {
    nm   <- ds_slugs[[i]]
    path <- file.path(out_dir, paste0("fig_choropleth_", nm, ".png"))
    ggplot2::ggsave(path, plot = choro$individual[[nm]],
                    width = 10, height = 5, units = "in",
                    dpi = dpi, bg = "white")
    message("  Saved: ", path)
  }
  path_choro_all <- file.path(out_dir, "fig_choropleth_all_datasets.png")
  ggplot2::ggsave(path_choro_all, plot = choro$combined,
                  width = 10, height = 20, units = "in",
                  dpi = dpi, bg = "white")
  message("  Saved: ", path_choro_all)

  # ---- Duration histogram ----
  message("\n--- Generating duration histogram ---")
  dur      <- fig_duration_datasets(site_lists)
  path_dur <- file.path(out_dir, "fig_duration_all_datasets.png")
  ggplot2::ggsave(path_dur, plot = dur,
                  width = 8, height = 14, units = "in",
                  dpi = dpi, bg = "white")
  message("  Saved: ", path_dur)

  # ---- Whittaker ----
  wh <- NULL
  if (!is.null(data_yy)) {
    message("\n--- Generating Whittaker figures ---")
    wh <- fig_whittaker_datasets(
      site_lists,
      data_yy        = data_yy,
      worldclim_path = worldclim_path,
      worldclim_csv  = worldclim_csv
    )

    for (i in seq_along(ds_slugs)) {
      nm   <- ds_slugs[[i]]
      path <- file.path(out_dir, paste0("fig_whittaker_", nm, ".png"))
      ggplot2::ggsave(path, plot = wh$individual[[nm]],
                      width = 8, height = 6, units = "in",
                      dpi = dpi, bg = "white")
      message("  Saved: ", path)
    }
    path_wh_all <- file.path(out_dir, "fig_whittaker_all_datasets.png")
    ggplot2::ggsave(path_wh_all, plot = wh$combined,
                    width = 8, height = 24, units = "in",
                    dpi = dpi, bg = "white")
    message("  Saved: ", path_wh_all)
  } else {
    message("data_yy not supplied \u2014 skipping Whittaker figures.")
  }

  message("\nAll figures saved to: ", out_dir)
  invisible(list(choropleth = choro, duration = dur, whittaker = wh))
}
