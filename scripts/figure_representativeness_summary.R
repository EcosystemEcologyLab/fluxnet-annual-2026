## figure_representativeness_summary.R
## Multi-network representativeness summary figures (Rep001-Rep007).
##
## Rep001-Rep006: Dot-plot summaries of weighted Jaccard across 11 axes.
##   make_summary_figure(network, mode, overlay_network, output_path)
##   mode = "single" | "overlay" | "delta"
##
## Rep007: Trajectory figure — weighted Jaccard across 4 network generations
##   for 6 selected axes.
##
## Rep007 six-axis qualitative palette (ColorBrewer Paired):
##   KG present-day (5-class)   = "#e31a1c"  # red
##   LULC (high-level)          = "#6a3d9a"  # purple
##   Aridity (5-class)          = "#ff7f00"  # orange
##   Biomass (7-bin hybrid)     = "#33a02c"  # dark green
##   TRENDY NEE-IAV (7-bin)     = "#1f78b4"  # dark blue
##   TRENDY ET-median (7-bin)   = "#b15928"  # brown

if (file.exists(".env")) dotenv::load_dot_env()
source("R/pipeline_config.R")
source("R/utils.R")
check_pipeline_config()

library(ggplot2)
library(patchwork)
library(dplyr)
library(readr)
library(tidyr)

# ---- Output directory --------------------------------------------------------
out_dir <- file.path("review", "figures", "representativeness")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Data source -------------------------------------------------------------
METRICS_CSV <- file.path(FLUXNET_DATA_ROOT, "snapshots",
                          "representativeness_metrics.csv")
if (!file.exists(METRICS_CSV))
  stop("representativeness_metrics.csv not found — run all axis scripts first.",
       call. = FALSE)

metrics_all <- readr::read_csv(METRICS_CSV, show_col_types = FALSE)

# ---- Rep007 six-axis qualitative palette (ColorBrewer Paired) ----------------
# Used as single representative color per axis in the trajectory figure.
# Colors chosen for perceptual distinctness on white background.
TRAJ_PAL <- c(
  "KG (present-day)"  = "#e31a1c",   # red
  "LULC"              = "#6a3d9a",   # purple
  "Aridity"           = "#ff7f00",   # orange
  "Biomass"           = "#33a02c",   # dark green
  "TRENDY NEE-IAV"    = "#1f78b4",   # dark blue
  "TRENDY ET-median"  = "#b15928"    # brown
)

# ---- Axis selection for summary figures (11 representative rows) --------------
# One aggregation level per major axis group; includes both aridity scales.
SUMMARY_AXES <- c(
  "KG present-day (5-class)",
  "KG SSP2-4.5 2041-70 (5-class)",
  "KG SSP5-8.5 2071-99 (5-class)",
  "Aridity (5-class)",
  "Aridity (7-class)",
  "Biomass AGB (7-bin)",
  "Land cover (10-class)",
  "TRENDY NEE-IAV (7-bin)",
  "TRENDY NEE-median (7-bin)",
  "TRENDY ET-IAV (7-bin)",
  "TRENDY ET-median (7-bin)"
)

SUMMARY_SEL <- data.frame(
  axis = c(
    "koppen_beck2023",
    "koppen_beck2023_future_ssp245_2041_2070",
    "koppen_beck2023_future_ssp585_2071_2099",
    "aridity_unep5",
    "aridity_unep7",
    "biomass_cci_v7",
    "landcover_cci",
    "trendy_nee_iav",
    "trendy_nee_median",
    "trendy_et_iav",
    "trendy_et_median"
  ),
  aggregation_level = c(
    "5class", "5class", "5class",
    "unep5", "unep7",
    "7bin_hybrid",
    "high_level",
    "7bin_hybrid", "7bin_hybrid", "7bin_hybrid", "7bin_hybrid"
  ),
  axis_label = SUMMARY_AXES,
  stringsAsFactors = FALSE
)

# ---- Network ordering, labels, and colors ------------------------------------
NET_LEVELS  <- c("marconi", "la_thuile", "fluxnet2015", "current_767")
NET_LABELS  <- c(
  marconi     = "Marconi (n=35)",
  la_thuile   = "La Thuile (n=252)",
  fluxnet2015 = "FLUXNET2015 (n=212)",
  current_767 = "Current (n=767)"
)
NET_COLORS  <- c(
  marconi     = "#999999",
  la_thuile   = "#fdae61",
  fluxnet2015 = "#2c7bb6",
  current_767 = "#1a9641"
)

# ---- Shared theme ------------------------------------------------------------
base_theme <- theme_minimal(base_size = 10, base_family = "sans") +
  theme(
    panel.grid.minor  = element_blank(),
    plot.background   = element_rect(fill = "white", colour = NA),
    panel.background  = element_rect(fill = "white", colour = NA),
    plot.title        = element_text(size = 10, face = "bold"),
    legend.background = element_rect(fill = "white", colour = NA)
  )

# ---- Helper: filter metrics to selected axes for one network -----------------
get_summary_data <- function(network_val) {
  metrics_all |>
    dplyr::filter(network == network_val) |>
    dplyr::inner_join(SUMMARY_SEL, by = c("axis", "aggregation_level")) |>
    dplyr::mutate(
      axis_label = factor(axis_label, levels = rev(SUMMARY_AXES))
    )
}

# ---- make_summary_figure() ---------------------------------------------------
#' @param network      character; one of NET_LEVELS
#' @param mode         "single" | "overlay" | "delta"
#' @param overlay_network character; required for overlay/delta modes
#' @param output_path  character or NULL; if supplied, saves PNG + meta.json
make_summary_figure <- function(network,
                                mode            = c("single", "overlay", "delta"),
                                overlay_network = NULL,
                                output_path     = NULL) {
  mode <- match.arg(mode)

  if (mode %in% c("overlay", "delta") && is.null(overlay_network))
    stop("`overlay_network` must be supplied for mode '", mode, "'.",
         call. = FALSE)

  df_a    <- get_summary_data(network)
  lbl_a   <- NET_LABELS[[network]]
  col_a   <- NET_COLORS[[network]]

  if (mode == "single") {

    p <- ggplot(df_a, aes(x = weighted_jaccard, y = axis_label)) +
      geom_vline(xintercept = 1, linetype = "dashed",
                 colour = "grey60", linewidth = 0.4) +
      geom_segment(aes(x = 0, xend = weighted_jaccard,
                       y = axis_label, yend = axis_label),
                   colour = "grey78", linewidth = 0.5) +
      geom_point(colour = col_a, size = 3.5) +
      geom_text(aes(label = sprintf("%.3f", weighted_jaccard)),
                hjust = -0.3, size = 2.6, family = "sans",
                colour = "grey25") +
      scale_x_continuous(
        name   = "Weighted Jaccard (J)",
        limits = c(0, 1.12),
        breaks = seq(0, 1, by = 0.2),
        expand = expansion(mult = 0)
      ) +
      scale_y_discrete(name = NULL) +
      labs(title = paste0("Representativeness — ", lbl_a)) +
      base_theme +
      theme(
        panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3),
        panel.grid.major.x = element_blank()
      )

    dims <- c(7.5, 6.0)

  } else if (mode == "overlay") {

    df_b  <- get_summary_data(overlay_network)
    lbl_b <- NET_LABELS[[overlay_network]]
    col_b <- NET_COLORS[[overlay_network]]

    df_both <- dplyr::bind_rows(
      dplyr::mutate(df_a, net_lbl = lbl_a),
      dplyr::mutate(df_b, net_lbl = lbl_b)
    ) |>
      dplyr::mutate(net_lbl = factor(net_lbl, levels = c(lbl_a, lbl_b)))

    p <- ggplot(df_both,
                aes(x = weighted_jaccard, y = axis_label,
                    colour = net_lbl, shape = net_lbl)) +
      geom_vline(xintercept = 1, linetype = "dashed",
                 colour = "grey60", linewidth = 0.4) +
      geom_point(size = 3, position = position_dodge(width = 0.55)) +
      scale_colour_manual(
        values = setNames(c(col_a, col_b), c(lbl_a, lbl_b)),
        name   = NULL
      ) +
      scale_shape_manual(
        values = setNames(c(16L, 17L), c(lbl_a, lbl_b)),
        name   = NULL
      ) +
      scale_x_continuous(
        name   = "Weighted Jaccard (J)",
        limits = c(0, 1.05),
        breaks = seq(0, 1, by = 0.2),
        expand = expansion(mult = 0)
      ) +
      scale_y_discrete(name = NULL) +
      labs(title = paste0("Representativeness — ", lbl_a, " vs. ", lbl_b)) +
      base_theme +
      theme(
        panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3),
        panel.grid.major.x = element_blank(),
        legend.position    = "bottom"
      )

    dims <- c(8.5, 6.5)

  } else {  # delta

    df_b  <- get_summary_data(overlay_network)
    lbl_b <- NET_LABELS[[overlay_network]]

    df_delta <- dplyr::inner_join(
      dplyr::select(df_a, axis_label, j_a = weighted_jaccard),
      dplyr::select(df_b, axis_label, j_b = weighted_jaccard),
      by = "axis_label"
    ) |>
      dplyr::mutate(
        delta     = j_a - j_b,
        direction = ifelse(delta >= 0, "gain", "loss")
      )

    p <- ggplot(df_delta, aes(x = delta, y = axis_label, fill = direction)) +
      geom_vline(xintercept = 0, colour = "grey50", linewidth = 0.5) +
      geom_col(width = 0.6, colour = NA) +
      geom_text(
        aes(label   = sprintf("%+.3f", delta),
            hjust   = ifelse(delta >= 0, -0.15, 1.15)),
        size = 2.6, family = "sans", colour = "grey25"
      ) +
      scale_fill_manual(
        values = c(gain = "#1a9641", loss = "#d73027"),
        guide  = "none"
      ) +
      scale_x_continuous(
        name   = paste0("Δ Weighted Jaccard (", lbl_a, " − ", lbl_b, ")"),
        limits = c(-0.4, 0.4),
        breaks = seq(-0.4, 0.4, by = 0.1)
      ) +
      scale_y_discrete(name = NULL) +
      labs(title = paste0("Representativeness change: ", lbl_a, " vs. ", lbl_b)) +
      base_theme +
      theme(
        panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3),
        panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3)
      )

    dims <- c(7.5, 6.0)
  }

  if (!is.null(output_path)) {
    ggsave(output_path, plot = p, width = dims[1], height = dims[2],
           dpi = 200, bg = "white")
    message("Saved: ", output_path)
    write_output_metadata(
      output_path,
      input_sources = METRICS_CSV,
      notes = paste0(
        "make_summary_figure(network='", network, "', mode='", mode, "'",
        if (!is.null(overlay_network))
          paste0(", overlay_network='", overlay_network, "'")
        else "",
        ")"
      )
    )
  }

  invisible(p)
}

# ---- make_trajectory_figure() ------------------------------------------------
#' @param output_path character or NULL; if supplied, saves PNG + meta.json
make_trajectory_figure <- function(output_path = NULL) {

  # 6 axes shown in trajectory — matches TRAJ_PAL order
  traj_axes <- data.frame(
    axis = c(
      "koppen_beck2023",
      "landcover_cci",
      "aridity_unep5",
      "biomass_cci_v7",
      "trendy_nee_iav",
      "trendy_et_median"
    ),
    aggregation_level = c(
      "5class",
      "high_level",
      "unep5",
      "7bin_hybrid",
      "7bin_hybrid",
      "7bin_hybrid"
    ),
    traj_label = c(
      "KG (present-day)",
      "LULC",
      "Aridity",
      "Biomass",
      "TRENDY NEE-IAV",
      "TRENDY ET-median"
    ),
    stringsAsFactors = FALSE
  )

  traj_df <- metrics_all |>
    dplyr::inner_join(traj_axes, by = c("axis", "aggregation_level")) |>
    dplyr::mutate(
      network    = factor(network, levels = NET_LEVELS),
      traj_label = factor(traj_label, levels = names(TRAJ_PAL))
    )

  net_x_labels <- c(
    marconi     = "Marconi\n(n=35)",
    la_thuile   = "La Thuile\n(n=252)",
    fluxnet2015 = "FLUXNET2015\n(n=212)",
    current_767 = "Current\n(n=767)"
  )

  p <- ggplot(traj_df,
              aes(x = network, y = weighted_jaccard,
                  colour = traj_label, group = traj_label)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.8) +
    scale_colour_manual(
      values = TRAJ_PAL,
      name   = "Representativeness axis"
    ) +
    scale_x_discrete(
      name   = "FLUXNET network generation",
      labels = net_x_labels
    ) +
    scale_y_continuous(
      name   = "Weighted Jaccard (J)",
      limits = c(0.1, 0.82),
      breaks = seq(0.1, 0.8, by = 0.1)
    ) +
    labs(
      title    = "Representativeness trajectory across FLUXNET network generations",
      subtitle = "Weighted Jaccard (J): 0 = no overlap with global land distribution, 1 = perfect match"
    ) +
    base_theme +
    theme(
      panel.grid.major = element_line(colour = "grey92", linewidth = 0.3),
      legend.position  = "right",
      plot.subtitle    = element_text(size = 8, colour = "grey45")
    )

  if (!is.null(output_path)) {
    ggsave(output_path, plot = p, width = 9, height = 5.5, dpi = 200,
           bg = "white")
    message("Saved: ", output_path)
    write_output_metadata(
      output_path,
      input_sources = METRICS_CSV,
      notes = paste0(
        "Trajectory figure: weighted Jaccard for 6 selected axes across ",
        "4 FLUXNET network generations (Marconi, La Thuile, FLUXNET2015, current_767). ",
        "Axes: KG 5-class, LULC high-level, Aridity 5-class, Biomass 7-bin, ",
        "TRENDY NEE-IAV 7-bin, TRENDY ET-median 7-bin."
      )
    )
  }

  invisible(p)
}

# ---- Produce Rep001-Rep007 ---------------------------------------------------
message("\n--- Rep001: current_767 (single) ---")
make_summary_figure(
  network     = "current_767",
  mode        = "single",
  output_path = file.path(out_dir, "fig_repr_summary_Rep001_current767.png")
)

message("\n--- Rep002: fluxnet2015 (single) ---")
make_summary_figure(
  network     = "fluxnet2015",
  mode        = "single",
  output_path = file.path(out_dir, "fig_repr_summary_Rep002_fluxnet2015.png")
)

message("\n--- Rep003: la_thuile (single) ---")
make_summary_figure(
  network     = "la_thuile",
  mode        = "single",
  output_path = file.path(out_dir, "fig_repr_summary_Rep003_la_thuile.png")
)

message("\n--- Rep004: marconi (single) ---")
make_summary_figure(
  network     = "marconi",
  mode        = "single",
  output_path = file.path(out_dir, "fig_repr_summary_Rep004_marconi.png")
)

message("\n--- Rep005: current_767 vs fluxnet2015 (overlay) ---")
make_summary_figure(
  network         = "current_767",
  mode            = "overlay",
  overlay_network = "fluxnet2015",
  output_path     = file.path(out_dir,
                              "fig_repr_summary_Rep005_overlay_cur_f2015.png")
)

message("\n--- Rep006: current_767 minus fluxnet2015 (delta) ---")
make_summary_figure(
  network         = "current_767",
  mode            = "delta",
  overlay_network = "fluxnet2015",
  output_path     = file.path(out_dir,
                              "fig_repr_summary_Rep006_delta_cur_f2015.png")
)

message("\n--- Rep007: trajectory across all 4 generations ---")
make_trajectory_figure(
  output_path = file.path(out_dir, "fig_repr_summary_Rep007_trajectory.png")
)

message("\nAll 7 representativeness summary figures complete.")
message("Output directory: ", out_dir)
