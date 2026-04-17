# scripts/generate_availability_heatmap.R
#
# Generates two data-availability heatmaps (UN subregion × GEZ, faceted by
# IGBP) showing the number of sites with ≥5 and ≥10 valid NEE years.
#
# Qualifying IGBP classes are determined from the full 672-site shuttle
# snapshot (threshold: >10 sites). Fill values come from
# long_record_site_candidates_gez.csv, which carries n_years_valid_nee.
#
# Outputs:
#   review/figures/anomalies/fig_availability_5yr.png   (width=18, height=12)
#   review/figures/anomalies/fig_availability_10yr.png  (width=18, height=12)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(fs)
})

source("R/pipeline_config.R")

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}

check_pipeline_config()

# ---- Paths -------------------------------------------------------------------

snapshots_dir <- file.path(FLUXNET_DATA_ROOT, "snapshots")
out_dir       <- file.path("review", "figures", "anomalies")
fs::dir_create(out_dir)

# ---- Load data ---------------------------------------------------------------

# Full 672-site snapshot — used only to determine qualifying IGBPs
snap_csv <- tail(sort(fs::dir_ls(snapshots_dir,
                                  glob = "*fluxnet_shuttle_snapshot*.csv")), 1)
message("Snapshot: ", snap_csv)
snapshot <- readr::read_csv(snap_csv, show_col_types = FALSE)
cat(sprintf("Full snapshot: %d sites\n\n", nrow(snapshot)))

# GEZ-enriched candidates — carries un_subregion, gez_name, n_years_valid_nee
candidates_path <- file.path(snapshots_dir, "long_record_site_candidates_gez.csv")
candidates <- readr::read_csv(candidates_path, show_col_types = FALSE)
cat(sprintf("Candidates with valid NEE: %d sites\n\n", nrow(candidates)))

# ---- Determine qualifying IGBP classes from snapshot -------------------------

igbp_counts <- snapshot |>
  dplyr::count(.data$igbp, name = "n_sites_snapshot") |>
  dplyr::arrange(dplyr::desc(.data$n_sites_snapshot))

qualifying_igbps <- igbp_counts |>
  dplyr::filter(.data$n_sites_snapshot > 10L) |>
  dplyr::pull(.data$igbp)

excluded_igbps <- igbp_counts |>
  dplyr::filter(.data$n_sites_snapshot <= 10L)

cat("=============================================================\n")
cat("  IGBP CLASS QUALIFICATION (threshold: >10 sites in snapshot)\n")
cat("=============================================================\n\n")

cat("  Qualifying IGBPs (included in heatmap):\n")
igbp_counts |>
  dplyr::filter(.data$igbp %in% qualifying_igbps) |>
  dplyr::mutate(status = "INCLUDED") |>
  dplyr::select(.data$igbp, .data$n_sites_snapshot, .data$status) |>
  as.data.frame() |>
  print(row.names = FALSE)

cat("\n  Excluded IGBPs (<=10 sites in snapshot):\n")
excluded_igbps |>
  dplyr::mutate(status = "EXCLUDED") |>
  dplyr::select(.data$igbp, .data$n_sites_snapshot, .data$status) |>
  as.data.frame() |>
  print(row.names = FALSE)
cat("\n")

# ---- Build heatmap for a given NEE-year threshold ----------------------------

build_availability_heatmap <- function(candidates, qualifying_igbps,
                                        nee_threshold, figure_title) {
  # Filter to qualifying IGBPs and sites meeting the NEE-year threshold
  df_thresh <- candidates |>
    dplyr::filter(
      .data$igbp %in% qualifying_igbps,
      !is.na(.data$un_subregion),
      !is.na(.data$gez_name),
      .data$n_years_valid_nee >= nee_threshold
    )

  # Count sites per igbp × un_subregion × gez_name
  counts <- df_thresh |>
    dplyr::group_by(.data$igbp, .data$un_subregion, .data$gez_name) |>
    dplyr::summarise(n_sites = dplyr::n(), .groups = "drop")

  # Complete the grid across all qualifying IGBPs, subregions, and GEZ zones
  # so every facet has the same axes
  all_subregions <- sort(unique(counts$un_subregion))
  all_gez        <- sort(unique(counts$gez_name))

  grid <- tidyr::expand_grid(
    igbp         = qualifying_igbps,
    un_subregion = all_subregions,
    gez_name     = all_gez
  ) |>
    dplyr::left_join(counts, by = c("igbp", "un_subregion", "gez_name")) |>
    dplyr::mutate(
      n_sites  = dplyr::coalesce(.data$n_sites, 0L),
      label    = dplyr::if_else(.data$n_sites > 0L,
                                as.character(.data$n_sites),
                                "")
    )

  # Order subregions and GEZ by total sites across all IGBPs (descending)
  subregion_order <- counts |>
    dplyr::group_by(.data$un_subregion) |>
    dplyr::summarise(total = sum(.data$n_sites), .groups = "drop") |>
    dplyr::arrange(.data$total) |>  # bottom = most sites (ggplot flips)
    dplyr::pull(.data$un_subregion)

  gez_order <- counts |>
    dplyr::group_by(.data$gez_name) |>
    dplyr::summarise(total = sum(.data$n_sites), .groups = "drop") |>
    dplyr::arrange(.data$total) |>
    dplyr::pull(.data$gez_name)

  # Order qualifying IGBPs by total site count in snapshot (most abundant first)
  igbp_factor_order <- igbp_counts |>
    dplyr::filter(.data$igbp %in% qualifying_igbps) |>
    dplyr::arrange(dplyr::desc(.data$n_sites_snapshot)) |>
    dplyr::pull(.data$igbp)

  grid <- grid |>
    dplyr::mutate(
      un_subregion = factor(.data$un_subregion, levels = subregion_order),
      gez_name     = factor(.data$gez_name,     levels = gez_order),
      igbp         = factor(.data$igbp,          levels = igbp_factor_order)
    )

  max_n <- max(grid$n_sites, na.rm = TRUE)

  ggplot2::ggplot(grid,
                  ggplot2::aes(x = .data$gez_name,
                               y = .data$un_subregion,
                               fill = .data$n_sites)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.4) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$label),
      size  = 2.5,
      color = "white",
      fontface = "bold"
    ) +
    ggplot2::scale_fill_gradientn(
      colours = viridisLite::viridis(9),
      breaks  = pretty(c(0, max_n), n = 5),
      limits  = c(0, max_n),
      name    = "n sites"
    ) +
    ggplot2::facet_wrap(ggplot2::vars(.data$igbp), ncol = 4) +
    ggplot2::labs(
      title    = figure_title,
      subtitle = sprintf(
        "%d qualifying IGBP classes (>10 sites in 672-site snapshot) | rows: UN subregion | cols: FAO GEZ",
        length(qualifying_igbps)
      ),
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 9) +
    ggplot2::theme(
      axis.text.x      = ggplot2::element_text(angle = 45, hjust = 1,
                                                size = 7),
      axis.text.y      = ggplot2::element_text(size = 7),
      strip.text       = ggplot2::element_text(face = "bold", size = 9),
      panel.grid       = ggplot2::element_blank(),
      legend.position  = "right",
      plot.title       = ggplot2::element_text(face = "bold", size = 12),
      plot.subtitle    = ggplot2::element_text(size = 8, color = "grey40"),
      panel.border     = ggplot2::element_rect(fill = NA, color = "grey80",
                                               linewidth = 0.4)
    )
}

# ---- Generate and save both figures ------------------------------------------

cat("Generating >=5 NEE-year availability heatmap...\n")
p5 <- build_availability_heatmap(
  candidates      = candidates,
  qualifying_igbps = qualifying_igbps,
  nee_threshold   = 5L,
  figure_title    = "Data availability: sites with \u22655 valid NEE years by UN subregion \u00d7 GEZ"
)
path5 <- file.path(out_dir, "fig_availability_5yr.png")
ggplot2::ggsave(path5, plot = p5, width = 18, height = 12,
                units = "in", dpi = 150, bg = "white")
message("Saved: ", path5)

cat("Generating >=10 NEE-year availability heatmap...\n")
p10 <- build_availability_heatmap(
  candidates       = candidates,
  qualifying_igbps = qualifying_igbps,
  nee_threshold    = 10L,
  figure_title     = "Data availability: sites with \u226510 valid NEE years by UN subregion \u00d7 GEZ"
)
path10 <- file.path(out_dir, "fig_availability_10yr.png")
ggplot2::ggsave(path10, plot = p10, width = 18, height = 12,
                units = "in", dpi = 150, bg = "white")
message("Saved: ", path10)

cat("\nDone.\n")
