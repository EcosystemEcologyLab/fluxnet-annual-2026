## scripts/generate_historical_comparison_figures.R
##
## Produces 6 paired historical vs Shuttle comparison figures:
##   Whittaker:  review/figures/historical/fig_compare_whittaker_{2000,2007,2015}.png
##   Duration:   review/figures/historical/fig_compare_duration_{2000,2007,2015}.png
##
## LEFT panel  = historical dataset (Marconi 2000 | La Thuile 2007 | FLUXNET2015)
## RIGHT panel = Shuttle snapshot at same year (first_year <= snapshot_year)
##
## Whittaker logic:  adapted from fig_whittaker_hexbin_worldclim() in R/figures/fig_climate.R
## Duration logic:   adapted from fig_network_duration_profile() in R/figures/fig_network_growth.R
## WorldClim source: data/snapshots/site_worldclim.csv (Shuttle cache) +
##                   terra::extract() for historical sites not in cache

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
source("R/plot_constants.R")
source("R/utils.R")
source("R/external_data.R")
source("R/figures/fig_climate.R")
source("R/figures/fig_network_growth.R")
source("R/historical_datasets.R")

library(dplyr)
library(ggplot2)
library(patchwork)
library(readr)
library(colorspace)
library(scales)
library(terra)
library(ggtext)

# ── Output directory ──────────────────────────────────────────────────────────
out_dir <- file.path("review", "figures", "historical")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ── Load data ─────────────────────────────────────────────────────────────────
message("Loading data ...")

processed_dir <- file.path(FLUXNET_DATA_ROOT, "processed")
snapshots_dir <- file.path(FLUXNET_DATA_ROOT, "snapshots")

## Shuttle snapshot metadata (most recent)
snap_file <- sort(list.files(snapshots_dir,
  pattern = "fluxnet_shuttle_snapshot_.*\\.csv$", full.names = TRUE),
  decreasing = TRUE)[[1]]
snapshot_meta <- readr::read_csv(snap_file, show_col_types = FALSE) |>
  dplyr::distinct(site_id, .keep_all = TRUE) |>
  dplyr::mutate(first_year = as.integer(first_year),
                last_year  = as.integer(last_year))
message("  Shuttle snapshot: ", nrow(snapshot_meta), " sites (", basename(snap_file), ")")

## YY annual flux data — FLUXMET only, for Shuttle NEE per site-year
data_yy <- readRDS(file.path(processed_dir, "flux_data_converted_yy.rds")) |>
  dplyr::filter(dataset == "FLUXMET") |>
  dplyr::mutate(YEAR = as.integer(YEAR))
message("  YY data: ", nrow(data_yy), " rows")

## Site-year presence data for Shuttle active/inactive classification
presence_df <- readr::read_csv(
  file.path(snapshots_dir, "site_year_data_presence.csv"),
  show_col_types = FALSE
)

## Historical site lists via load_historical_site_lists()
## Enriches each list with Shuttle first/last year and coordinates where available.
message("Loading historical site lists ...")
site_lists <- load_historical_site_lists(
  la_thuile_path   = "data/snapshots/sites_la_thuile_clean.csv",
  fluxnet2015_path = "data/snapshots/sites_fluxnet2015_clean.csv"
)
marconi_sites     <- site_lists$marconi
la_thuile_sites   <- site_lists$la_thuile
fluxnet2015_sites <- site_lists$fluxnet2015

message("  Marconi: ",     nrow(marconi_sites),
        "  La Thuile: ",   nrow(la_thuile_sites),
        "  FLUXNET2015: ", nrow(fluxnet2015_sites))

## WorldClim cache — one row per Shuttle site
wc_cache <- readr::read_csv(
  file.path(snapshots_dir, "site_worldclim.csv"),
  show_col_types = FALSE
)  # columns: site_id, mat_worldclim, map_worldclim

## WorldClim raster for terra::extract() on historical sites not in cache
message("Loading WorldClim raster ...")
wc_rast <- load_worldclim()

# ── Extract WorldClim for historical sites not in cache ───────────────────────
all_hist_sites <- dplyr::bind_rows(
  marconi_sites     |> dplyr::select(site_id, location_lat, location_long),
  la_thuile_sites   |> dplyr::select(site_id, location_lat, location_long),
  fluxnet2015_sites |> dplyr::select(site_id, location_lat, location_long)
) |>
  dplyr::distinct(site_id, .keep_all = TRUE) |>
  dplyr::filter(!site_id %in% wc_cache$site_id,
                !is.na(location_lat), !is.na(location_long))

message("Extracting WorldClim for ", nrow(all_hist_sites),
        " historical sites not in cache ...")

if (nrow(all_hist_sites) > 0L) {
  pts <- terra::vect(
    data.frame(x = all_hist_sites$location_long,
               y = all_hist_sites$location_lat),
    geom = c("x", "y"), crs = "EPSG:4326"
  )
  wc_vals  <- as.data.frame(terra::extract(wc_rast, pts, ID = FALSE))

  bio1_col  <- grep("bio[_.]?0?1([^0-9]|$)", names(wc_vals),
                    value = TRUE, ignore.case = TRUE, perl = TRUE)[1L]
  bio12_col <- grep("bio[_.]?12([^0-9]|$)", names(wc_vals),
                    value = TRUE, ignore.case = TRUE, perl = TRUE)[1L]

  mat_raw   <- wc_vals[[bio1_col]]
  mat_vals  <- if (max(abs(mat_raw), na.rm = TRUE) > 70) mat_raw / 10 else mat_raw

  wc_extracted <- dplyr::tibble(
    site_id       = all_hist_sites$site_id,
    mat_worldclim = mat_vals,
    map_worldclim = wc_vals[[bio12_col]]
  )
} else {
  wc_extracted <- dplyr::tibble(
    site_id = character(0L), mat_worldclim = numeric(0L), map_worldclim = numeric(0L)
  )
}

wc_all <- dplyr::bind_rows(wc_cache, wc_extracted)
message("  Combined WorldClim lookup: ", nrow(wc_all), " sites")

# ── Attach WorldClim to each site list ────────────────────────────────────────
attach_wc <- function(sites) {
  dplyr::left_join(sites, wc_all, by = "site_id")
}
marconi_sites     <- attach_wc(marconi_sites)
la_thuile_sites   <- attach_wc(la_thuile_sites)
fluxnet2015_sites <- attach_wc(fluxnet2015_sites)

# Also attach WorldClim to Shuttle metadata
snapshot_meta_wc <- snapshot_meta |>
  dplyr::left_join(wc_cache, by = "site_id")

# ── Shuttle NEE per site, filtered to <= snapshot year ────────────────────────
## Returns a tibble: site_id, mat_worldclim, map_worldclim, mean_flux
shuttle_clim_nee_at <- function(snap_yr) {
  nee_by_site <- data_yy |>
    dplyr::filter(as.integer(YEAR) <= as.integer(snap_yr)) |>
    dplyr::group_by(site_id) |>
    dplyr::summarise(mean_flux = mean(NEE_VUT_REF, na.rm = TRUE),
                     .groups = "drop")

  snapshot_meta_wc |>
    dplyr::filter(as.integer(first_year) <= as.integer(snap_yr)) |>
    dplyr::left_join(nee_by_site, by = "site_id") |>
    dplyr::filter(!is.na(mat_worldclim), !is.na(map_worldclim))
}

## Shuttle sites at snapshot year (for duration histograms)
shuttle_at <- function(snap_yr) {
  snapshot_meta |>
    dplyr::filter(as.integer(first_year) <= as.integer(snap_yr))
}

## Shuttle site-years sum at snapshot year
shuttle_site_years_at <- function(snap_yr) {
  d <- shuttle_at(snap_yr)
  sum(pmin(as.integer(d$last_year), as.integer(snap_yr)) -
        as.integer(d$first_year) + 1L,
      na.rm = TRUE)
}

# ── Shared Whittaker axis limits ──────────────────────────────────────────────
## Collect MAT/MAP from ALL datasets (historical + Shuttle) for global range
all_wc_vals <- dplyr::bind_rows(
  marconi_sites     |> dplyr::select(mat_worldclim, map_worldclim),
  la_thuile_sites   |> dplyr::select(mat_worldclim, map_worldclim),
  fluxnet2015_sites |> dplyr::select(mat_worldclim, map_worldclim),
  snapshot_meta_wc  |> dplyr::select(mat_worldclim, map_worldclim)
)
mat_range <- range(all_wc_vals$mat_worldclim, na.rm = TRUE)
map_range <- range(all_wc_vals$map_worldclim, na.rm = TRUE)
mat_lim   <- mat_range + c(-1.5,  1.5)
map_lim   <- c(0, map_range[2] * 1.06)

## NEE fill limits: 5th–95th percentile of full Shuttle distribution
nee_vals <- data_yy |>
  dplyr::group_by(site_id) |>
  dplyr::summarise(m = mean(NEE_VUT_REF, na.rm = TRUE), .groups = "drop") |>
  dplyr::pull(m)
nee_lims <- quantile(nee_vals, probs = c(0.05, 0.95), na.rm = TRUE)
nee_max  <- max(abs(nee_lims))
message("NEE fill limits: ±", round(nee_max, 1), " gC m⁻² yr⁻¹")

# ── Shared duration axis limits ───────────────────────────────────────────────
BW <- 5L   # bin width (years)

## Record length helper
record_lengths <- function(sites, snap_yr) {
  sites |>
    dplyr::filter(!is.na(first_year)) |>
    dplyr::mutate(record_length = as.integer(snap_yr) - as.integer(first_year)) |>
    dplyr::filter(record_length >= 0L)
}

## Compute max bin count for a record-length vector with BW-year bins
max_bin_count <- function(rl_vec) {
  if (length(rl_vec) == 0L) return(0L)
  n_max <- max(rl_vec, na.rm = TRUE)
  if (is.na(n_max) || n_max < BW) return(length(rl_vec))
  breaks <- seq(0, n_max + BW, by = BW)
  max(graphics::hist(rl_vec, breaks = breaks, plot = FALSE)$counts)
}

x_dur_max <- max(c(
  record_lengths(marconi_sites,     2000L)$record_length,
  record_lengths(la_thuile_sites,   2007L)$record_length,
  record_lengths(fluxnet2015_sites, 2015L)$record_length,
  record_lengths(shuttle_at(2000L), 2000L)$record_length,
  record_lengths(shuttle_at(2007L), 2007L)$record_length,
  record_lengths(shuttle_at(2015L), 2015L)$record_length
), na.rm = TRUE)

y_dur_max <- max(c(
  max_bin_count(record_lengths(marconi_sites,     2000L)$record_length),
  max_bin_count(record_lengths(la_thuile_sites,   2007L)$record_length),
  max_bin_count(record_lengths(fluxnet2015_sites, 2015L)$record_length),
  max_bin_count(record_lengths(shuttle_at(2000L), 2000L)$record_length),
  max_bin_count(record_lengths(shuttle_at(2007L), 2007L)$record_length),
  max_bin_count(record_lengths(shuttle_at(2015L), 2015L)$record_length)
), na.rm = TRUE)

x_lim_dur <- c(0, x_dur_max + BW)
y_lim_dur <- c(0, ceiling(y_dur_max * 1.18))

message("Duration x limit: 0 – ", x_dur_max + BW,
        "  y limit: 0 – ", ceiling(y_dur_max * 1.18))

# ── Shared constants and scales ───────────────────────────────────────────────
HIST_COLOUR    <- "#7F8C8D"   # neutral grey for historical bars / hexbins
ACTIVE_COLOURS <- c("Functionally active"     = "#2196F3",
                    "Inactive / high latency"  = "#B0BEC5")

## NEE diverging fill scale — shared limits across all 6 Whittaker panels
nee_fill_scale <- function(show_guide = TRUE) {
  colorspace::scale_fill_continuous_diverging(
    palette  = "Blue-Red 3",
    mid      = 0,
    limits   = c(-nee_max, nee_max),
    oob      = scales::squish,
    na.value = HIST_COLOUR,
    name     = "NEE (g C m\u207b\u00b2 yr\u207b\u00b9)",
    guide    = if (show_guide) {
      ggplot2::guide_colorbar(
        title.position = "top",
        barwidth       = 7,
        barheight      = 0.65,
        direction      = "horizontal"
      )
    } else {
      ggplot2::guide_none()
    }
  )
}

## Base Whittaker axis + coord settings (returned as a list of layers)
wh_base <- function() {
  list(
    ggplot2::labs(
      x = "MAT (\u00b0C)",
      y = "MAP (mm yr\u207b\u00b9)"
    ),
    ggplot2::coord_cartesian(xlim = mat_lim, ylim = map_lim),
    fluxnet_theme(base_size = 13)
  )
}

# ── Whittaker panel builders ──────────────────────────────────────────────────

#' Left Whittaker panel — historical dataset as grey hexbins
#'
#' All hexbins are rendered grey (na.value) because mean_flux = NA for
#' historical sites. The NEE colorbar legend IS shown here so it appears
#' in the upper-left of the paired figure.
make_wh_left <- function(sites_wc, panel_label) {
  df <- sites_wc |>
    dplyr::filter(!is.na(mat_worldclim), !is.na(map_worldclim)) |>
    dplyr::mutate(mean_flux = NA_real_)

  n_shown <- nrow(df)
  n_total <- nrow(sites_wc)
  if (n_shown < n_total) {
    message("  Whittaker left: ", n_shown, "/", n_total,
            " sites have WorldClim data")
  }

  ggplot2::ggplot(
    df,
    ggplot2::aes(x = mat_worldclim, y = map_worldclim, z = mean_flux)
  ) +
    ggplot2::stat_summary_hex(
      fun   = function(x) NA_real_,   # all hexbins → NA → na.value grey
      bins  = 15,
      alpha = 0.85
    ) +
    nee_fill_scale(show_guide = TRUE) +
    ggplot2::geom_point(
      ggplot2::aes(x = mat_worldclim, y = map_worldclim),
      size = 1.5, colour = "grey30", alpha = 0.55, inherit.aes = FALSE
    ) +
    wh_base() +
    ggplot2::theme(
      legend.position      = c(0.03, 0.97),
      legend.justification = c(0, 1),
      legend.background    = ggplot2::element_rect(fill = "white", color = NA),
      legend.title         = ggtext::element_markdown(size = 9),
      legend.text          = ggplot2::element_text(size = 8),
      plot.margin          = ggplot2::margin(4, 0, 4, 4)
    ) +
    ggplot2::annotate(
      "text", x = -Inf, y = Inf,
      label = panel_label, hjust = -0.06, vjust = 1.35,
      size = 3.8, fontface = "bold"
    )
}

#' Right Whittaker panel — Shuttle snapshot coloured by median NEE
make_wh_right <- function(site_clim_df, panel_label) {
  df <- site_clim_df |>
    dplyr::filter(!is.na(mat_worldclim), !is.na(map_worldclim),
                  !is.na(mean_flux))

  ggplot2::ggplot(
    df,
    ggplot2::aes(x = mat_worldclim, y = map_worldclim, z = mean_flux)
  ) +
    ggplot2::stat_summary_hex(
      fun   = median,
      bins  = 15,
      alpha = 0.85
    ) +
    nee_fill_scale(show_guide = FALSE) +
    ggplot2::geom_point(
      ggplot2::aes(x = mat_worldclim, y = map_worldclim),
      size = 1.5, colour = "grey30", alpha = 0.55, inherit.aes = FALSE
    ) +
    wh_base() +
    ggplot2::theme(
      legend.position  = "none",
      axis.title.y     = ggplot2::element_blank(),
      axis.text.y      = ggplot2::element_blank(),
      axis.ticks.y     = ggplot2::element_blank(),
      plot.margin      = ggplot2::margin(4, 4, 4, 0)
    ) +
    ggplot2::annotate(
      "text", x = -Inf, y = Inf,
      label = panel_label, hjust = -0.06, vjust = 1.35,
      size = 3.8, fontface = "bold"
    )
}

# ── Duration panel builders ───────────────────────────────────────────────────

## Shared x/y scales for duration panels
dur_scales <- function() {
  list(
    ggplot2::scale_x_continuous(
      breaks = scales::pretty_breaks(n = 6),
      expand = ggplot2::expansion(mult = c(0, 0))
    ),
    ggplot2::scale_y_continuous(
      breaks = scales::pretty_breaks(n = 4),
      expand = ggplot2::expansion(mult = c(0, 0))
    ),
    ggplot2::coord_cartesian(xlim = x_lim_dur, ylim = y_lim_dur)
  )
}

#' Left duration panel — historical dataset, uniform grey bars
make_dur_left <- function(rl_df, panel_label) {
  ggplot2::ggplot(rl_df, ggplot2::aes(x = record_length)) +
    ggplot2::geom_histogram(
      binwidth  = BW,
      fill      = HIST_COLOUR,
      colour    = "white",
      linewidth = 0.2,
      boundary  = 0
    ) +
    dur_scales() +
    ggplot2::labs(
      x = "Record length (years)",
      y = "Number of sites"
    ) +
    fluxnet_theme(base_size = 13) +
    ggplot2::theme(
      legend.position = "none",
      plot.margin     = ggplot2::margin(4, 0, 4, 4)
    ) +
    ggplot2::annotate(
      "text", x = -Inf, y = Inf,
      label = panel_label, hjust = -0.06, vjust = 1.35,
      size = 3.8, fontface = "bold"
    )
}

#' Right duration panel — Shuttle snapshot, coloured by functionally active status
make_dur_right <- function(sites_df, snap_yr, panel_label) {
  rl_df <- record_lengths(sites_df, snap_yr)

  active_flags <- is_functionally_active(
    rl_df$site_id,
    reference_year   = as.integer(snap_yr),
    presence_df      = presence_df,
    active_threshold = 4L,
    last_year_vec    = rl_df$last_year
  )

  rl_df <- rl_df |>
    dplyr::mutate(
      activity = factor(
        dplyr::if_else(
          active_flags[site_id],
          "Functionally active",
          "Inactive / high latency"
        ),
        levels = c("Functionally active", "Inactive / high latency")
      )
    )

  ggplot2::ggplot(rl_df, ggplot2::aes(x = record_length, fill = activity)) +
    ggplot2::geom_histogram(
      binwidth  = BW,
      colour    = "white",
      linewidth = 0.2,
      position  = "stack",
      boundary  = 0
    ) +
    ggplot2::scale_fill_manual(
      values = ACTIVE_COLOURS, name = NULL, drop = FALSE
    ) +
    dur_scales() +
    ggplot2::labs(
      x = "Record length (years)",
      y = NULL
    ) +
    fluxnet_theme(base_size = 13) +
    ggplot2::theme(
      legend.position      = c(0.97, 0.97),
      legend.justification = c(1, 1),
      legend.background    = ggplot2::element_rect(fill = "white", color = NA),
      legend.text          = ggplot2::element_text(size = 9),
      axis.title.y         = ggplot2::element_blank(),
      axis.text.y          = ggplot2::element_blank(),
      axis.ticks.y         = ggplot2::element_blank(),
      plot.margin          = ggplot2::margin(4, 4, 4, 0)
    ) +
    ggplot2::annotate(
      "text", x = -Inf, y = Inf,
      label = panel_label, hjust = -0.06, vjust = 1.35,
      size = 3.8, fontface = "bold"
    )
}

# ── Period definitions ────────────────────────────────────────────────────────
## Published site-years (historical datasets): Marconi=97, LaThuile=965, FN2015=1532
periods <- list(
  list(
    year       = 2000L,
    hist_label = "Marconi",
    hist_name  = "Marconi 2000",
    hist_sites = marconi_sites,
    hist_n     = 35L,
    hist_sy    = 97L
  ),
  list(
    year       = 2007L,
    hist_label = "LaThuile",
    hist_name  = "La Thuile 2007",
    hist_sites = la_thuile_sites,
    hist_n     = 252L,
    hist_sy    = 965L
  ),
  list(
    year       = 2015L,
    hist_label = "FN2015",
    hist_name  = "FLUXNET2015",
    hist_sites = fluxnet2015_sites,
    hist_n     = 212L,
    hist_sy    = 1532L
  )
)

# ── Build and save all 6 figures ──────────────────────────────────────────────
for (pd in periods) {
  yr      <- pd$year
  yr_str  <- as.character(yr)
  message("\n── ", pd$hist_name, " vs Shuttle ", yr_str, " ──")

  sh_sites <- shuttle_at(yr)
  sh_n     <- nrow(sh_sites)
  sh_sy    <- shuttle_site_years_at(yr)

  lbl_hist    <- paste0(pd$hist_name, "\n(n=", pd$hist_n,
                         ", ", format(pd$hist_sy, big.mark = ","), " site-years)")
  lbl_shuttle <- paste0("Shuttle snapshot ", yr_str, "\n(n=", sh_n,
                         ", ", format(sh_sy, big.mark = ","), " site-years)")

  # ---- Whittaker paired figure ----------------------------------------------
  message("  Building Whittaker ...")
  wh_shuttle <- shuttle_clim_nee_at(yr)

  p_wh_l <- make_wh_left(pd$hist_sites, lbl_hist)
  p_wh_r <- make_wh_right(wh_shuttle, lbl_shuttle)

  pw_wh <- (p_wh_l | p_wh_r) +
    patchwork::plot_layout(guides = "keep")

  out_wh <- file.path(out_dir,
                       paste0("fig_compare_whittaker_", yr_str, ".png"))
  ggplot2::ggsave(out_wh, plot = pw_wh, width = 14, height = 7,
                  units = "in", dpi = 150, bg = "white")
  message("  Saved: ", basename(out_wh))

  # ---- Duration histogram paired figure -------------------------------------
  message("  Building duration histogram ...")
  hist_rl <- record_lengths(pd$hist_sites, yr)
  message("    Historical sites with first_year: ", nrow(hist_rl),
          " / ", pd$hist_n)

  p_dur_l <- make_dur_left(hist_rl, lbl_hist)
  p_dur_r <- make_dur_right(sh_sites, yr, lbl_shuttle)

  pw_dur <- (p_dur_l | p_dur_r) +
    patchwork::plot_layout(guides = "keep")

  out_dur <- file.path(out_dir,
                        paste0("fig_compare_duration_", yr_str, ".png"))
  ggplot2::ggsave(out_dur, plot = pw_dur, width = 14, height = 7,
                  units = "in", dpi = 150, bg = "white")
  message("  Saved: ", basename(out_dur))
}

message("\nAll historical comparison figures done.")
