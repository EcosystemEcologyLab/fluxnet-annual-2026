## figure_representativeness_summary.R
##
## Rep001–007: per-category sampling-ratio panels (Rep001–006)
## and Jaccard trajectory across four network generations (Rep007).
##
## Rep001: current_767  (single)
## Rep002: marconi      (single)
## Rep003: la_thuile    (single)
## Rep004: fluxnet2015  (single)
## Rep005: current_767 vs fluxnet2015 (overlay)
## Rep006: current_767 minus fluxnet2015 (delta)
## Rep007: Jaccard trajectory, 6 axes across 4 network generations
##
## Six axes (fixed order, top to bottom in each figure):
##   1. Beck 2023 KG present-day, 13-class (koppen_twoletter)
##   2. ESA CCI Land Cover, 10-class high-level
##   3. CGIAR Aridity UNEP, 7-class
##   4. ESA CCI Biomass v7, 7-bin hybrid
##   5. TRENDY NEE-IAV, 7-bin hybrid
##   6. TRENDY ET-median, 7-bin hybrid
##
## Rep007 six-axis qualitative palette: Okabe-Ito (colorblind-safe).

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(patchwork)
  library(fs)
})

SNAP <- "data/snapshots"
EXT  <- "data/external"
OUTD <- "review/figures/representativeness"
fs::dir_create(OUTD)

# ---- 1. KG 13-class (twoletter) colors ----------------------------------------
# Mean RGB of 30-class Beck 2023 legend members per two-letter group.
kg_leg_lines <- readLines(file.path(EXT, "koppen_beck2023", "legend.txt"))
kg_leg_lines <- kg_leg_lines[grepl("^\\s+\\d+:", kg_leg_lines)]
kg_leg_df <- data.frame(
  koppen_class = sub("^\\s*\\d+:\\s+(\\S+)\\s+.*",  "\\1", kg_leg_lines),
  r = as.integer(sub(".*\\[(\\d+)\\s+\\d+\\s+\\d+\\].*", "\\1", kg_leg_lines)),
  g = as.integer(sub(".*\\[\\d+\\s+(\\d+)\\s+\\d+\\].*", "\\1", kg_leg_lines)),
  b = as.integer(sub(".*\\[\\d+\\s+\\d+\\s+(\\d+)\\].*", "\\1", kg_leg_lines)),
  stringsAsFactors = FALSE
)

TL_ORDER <- c("Af","Am","Aw","BS","BW","Cf","Cs","Cw","Df","Ds","Dw","EF","ET")
KG_COLORS <- setNames(
  vapply(TL_ORDER, function(tl) {
    members <- kg_leg_df[substr(kg_leg_df$koppen_class, 1L, 2L) == tl, ]
    grDevices::rgb(mean(members$r), mean(members$g), mean(members$b), maxColorValue = 255)
  }, character(1L)),
  TL_ORDER
)

# ---- 2. LULC high-level (10-class) colors ------------------------------------
# Mean RGB of constituent native LCCS classes per high-level group.
lc_lut <- readr::read_csv(
  file.path(SNAP, "cci_landcover_aggregation_lookup.csv"),
  show_col_types = FALSE
)
lc_hl_colors <- lc_lut |>
  dplyr::group_by(lulc_highlevel) |>
  dplyr::summarise(R = mean(R), G = mean(G), B = mean(B), .groups = "drop") |>
  dplyr::mutate(
    hex = grDevices::rgb(R, G, B, maxColorValue = 255),
    hex = dplyr::if_else(lulc_highlevel == 8L, "#e0f3f8", hex)
  )
LULC_COLORS <- setNames(lc_hl_colors$hex, as.character(lc_hl_colors$lulc_highlevel))

# ---- 3. Aridity 7-class palette (from aridity figure script) ----------------
ARIDITY_ORDER <- c("Hyper-Arid","Arid","Semi-Arid","Dry Sub-Humid",
                    "Humid (low)","Humid (moderate)","Hyper-Humid")
ARIDITY_COLORS <- c(
  "Hyper-Arid"       = "#d73027",
  "Arid"             = "#fc8d59",
  "Semi-Arid"        = "#ffff33",
  "Dry Sub-Humid"    = "#66bd63",
  "Humid (low)"      = "#74add1",
  "Humid (moderate)" = "#4575b4",
  "Hyper-Humid"      = "#313695"
)

# ---- 4. Biomass 7-bin palette (from biomass figure script) ------------------
BIO_COLORS <- c(
  "1" = "#f7f4f9",
  "2" = "#f0e1c4",
  "3" = "#d4d491",
  "4" = "#a3c585",
  "5" = "#6cb375",
  "6" = "#2e8b57",
  "7" = "#14532d"
)

# ---- 5. TRENDY NEE-IAV 7-bin palette (from trendy wrap script) --------------
NEE_COLORS <- c(
  "1" = "#f4faf0",
  "2" = "#c8e8a4",
  "3" = "#9acb72",
  "4" = "#67ae42",
  "5" = "#3d8c27",
  "6" = "#1f6415",
  "7" = "#0b3e09"
)

# ---- 6. TRENDY ET-median 7-bin palette (from trendy wrap script) ------------
ET_COLORS <- c(
  "1" = "#f0f8ff",
  "2" = "#bcd8f4",
  "3" = "#82bce8",
  "4" = "#4498d5",
  "5" = "#1d74b3",
  "6" = "#0c4f84",
  "7" = "#06305a"
)

# ---- 7. Rep007 six-axis qualitative palette (Okabe-Ito, colorblind-safe) ----
REP007_AXES   <- c("KG (13-class)","LULC (10-class)","Aridity (7-class)",
                   "Biomass (7-bin)","TRENDY NEE-IAV","TRENDY ET-median")
REP007_COLORS <- c(
  "KG (13-class)"     = "#D55E00",  # vermilion
  "LULC (10-class)"   = "#CC79A7",  # reddish pink
  "Aridity (7-class)" = "#E69F00",  # amber
  "Biomass (7-bin)"   = "#009E73",  # green
  "TRENDY NEE-IAV"    = "#0072B2",  # blue
  "TRENDY ET-median"  = "#56B4E9"   # sky blue
)

# ---- Load global distributions -----------------------------------------------
kg_global <- readr::read_csv(
  file.path(SNAP, "koppen_beck2023_global_distribution.csv"),
  show_col_types = FALSE
) |>
  dplyr::group_by(koppen_twoletter) |>
  dplyr::summarise(global_land_fraction = sum(global_land_fraction), .groups = "drop") |>
  dplyr::rename(class = koppen_twoletter)

arid_global <- readr::read_csv(
  file.path(SNAP, "aridity_unep7_global_distribution.csv"),
  show_col_types = FALSE
) |>
  dplyr::rename(class = unep_class)

bio_global <- readr::read_csv(
  file.path(SNAP, "biomass_cci_v7_global_distribution.csv"),
  show_col_types = FALSE
) |>
  dplyr::mutate(class = as.character(biomass_bin))

lulc_global <- readr::read_csv(
  file.path(SNAP, "landcover_cci_highlevel_global_distribution.csv"),
  show_col_types = FALSE
) |>
  dplyr::mutate(class = as.character(cci_high_level_class))

nee_iav_global <- readr::read_csv(
  file.path(SNAP, "trendy_nee_iav_global_distribution.csv"),
  show_col_types = FALSE
) |>
  dplyr::mutate(class = as.character(bin))

et_med_global <- readr::read_csv(
  file.path(SNAP, "trendy_et_median_global_distribution.csv"),
  show_col_types = FALSE
) |>
  dplyr::mutate(class = as.character(bin))

# ---- Per-site CSV loader -----------------------------------------------------
site_file <- function(axis_tag, network) {
  suffix <- if (network == "current_767") "" else paste0("_", network)
  file.path(SNAP, paste0("site_", axis_tag, suffix, ".csv"))
}

count_sites <- function(df, class_col) {
  n_total <- nrow(df)
  df |>
    dplyr::filter(!is.na(.data[[class_col]])) |>
    dplyr::count(.data[[class_col]], name = "n") |>
    dplyr::rename(class = 1) |>
    dplyr::mutate(
      class        = as.character(class),
      network_frac = n / n_total
    )
}

load_kg_counts    <- function(net) {
  readr::read_csv(site_file("koppen_beck2023", net), show_col_types = FALSE) |>
    count_sites("koppen_twoletter")
}
load_arid_counts  <- function(net) {
  readr::read_csv(site_file("aridity", net), show_col_types = FALSE) |>
    count_sites("unep_class_7")
}
load_bio_counts   <- function(net) {
  readr::read_csv(site_file("biomass_cci_v7", net), show_col_types = FALSE) |>
    count_sites("biomass_bin")
}
load_lulc_counts  <- function(net) {
  readr::read_csv(site_file("landcover_cci", net), show_col_types = FALSE) |>
    count_sites("lulc_highlevel")
}
load_trendy_counts <- function(axis_tag, bin_col, net) {
  readr::read_csv(site_file(axis_tag, net), show_col_types = FALSE) |>
    count_sites(bin_col)
}

# ---- Merge site counts with global distribution to compute sampling ratio ----
merge_sr <- function(site_counts, global_df) {
  global_df |>
    dplyr::left_join(site_counts |> dplyr::select(class, n, network_frac),
                     by = "class") |>
    dplyr::mutate(
      n            = dplyr::coalesce(n, 0L),
      network_frac = dplyr::coalesce(network_frac, 0.0),
      sampling_ratio = dplyr::if_else(
        global_land_fraction > 0 & network_frac > 0,
        network_frac / global_land_fraction,
        NA_real_
      ),
      log2_sr = dplyr::if_else(!is.na(sampling_ratio), log2(sampling_ratio), NA_real_)
    )
}

# ---- Load all six axes for one network, returning a named list of data frames --
load_all_axes <- function(network) {
  kg <- merge_sr(load_kg_counts(network),
    kg_global |> dplyr::select(class, global_land_fraction)
  ) |>
    dplyr::mutate(
      class_label = class,
      class_order = match(class, TL_ORDER),
      color_hex   = KG_COLORS[class]
    )

  lulc <- merge_sr(load_lulc_counts(network),
    lulc_global |> dplyr::select(class, global_land_fraction, cci_high_level_class_name)
  ) |>
    dplyr::mutate(
      class_label = cci_high_level_class_name,
      class_order = as.integer(class),
      color_hex   = LULC_COLORS[class]
    )

  aridity <- merge_sr(load_arid_counts(network),
    arid_global |> dplyr::select(class, global_land_fraction)
  ) |>
    dplyr::mutate(
      class_label = class,
      class_order = match(class, ARIDITY_ORDER),
      color_hex   = ARIDITY_COLORS[class]
    )

  biomass <- merge_sr(load_bio_counts(network),
    bio_global |> dplyr::select(class, global_land_fraction, biomass_bin_label)
  ) |>
    dplyr::mutate(
      class_label = biomass_bin_label,
      class_order = as.integer(class),
      color_hex   = BIO_COLORS[class]
    )

  nee_iav <- merge_sr(
    load_trendy_counts("trendy_nee_iav", "trendy_nee_iav_bin", network),
    nee_iav_global |> dplyr::select(class, global_land_fraction, bin_label)
  ) |>
    dplyr::mutate(
      class_label = bin_label,
      class_order = as.integer(class),
      color_hex   = NEE_COLORS[class]
    )

  et_median <- merge_sr(
    load_trendy_counts("trendy_et_median", "trendy_et_median_bin", network),
    et_med_global |> dplyr::select(class, global_land_fraction, bin_label)
  ) |>
    dplyr::mutate(
      class_label = bin_label,
      class_order = as.integer(class),
      color_hex   = ET_COLORS[class]
    )

  list(kg = kg, lulc = lulc, aridity = aridity,
       biomass = biomass, nee_iav = nee_iav, et_median = et_median)
}

# ---- Shared theme and axis metadata ------------------------------------------
AXIS_KEYS   <- c("kg","lulc","aridity","biomass","nee_iav","et_median")
AXIS_TITLES <- c(
  kg        = "Beck 2023 KG (13-class)",
  lulc      = "ESA CCI Land Cover (10-class)",
  aridity   = "CGIAR Aridity UNEP (7-class)",
  biomass   = "ESA CCI Biomass v7 (7-bin)",
  nee_iav   = "TRENDY NEE-IAV (7-bin)",
  et_median = "TRENDY ET-median (7-bin)"
)

LOG2_BREAKS <- c(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5)
LOG2_LABELS <- c("1/32×","1/16×","1/8×","1/4×","1/2×",
                 "1×","2×","4×","8×","16×","32×")
LOG2_LIMITS <- c(-5.2, 5.2)

base_theme <- theme_minimal(base_size = 9) +
  theme(
    plot.background   = element_rect(fill = "white", colour = NA),
    panel.background  = element_rect(fill = "white", colour = NA),
    panel.grid.minor  = element_blank(),
    legend.background = element_rect(fill = "white", colour = NA)
  )

ordered_df <- function(df) {
  df |>
    dplyr::arrange(class_order) |>
    dplyr::mutate(class_label = factor(class_label, levels = unique(class_label)))
}

# ---- Panel builders ----------------------------------------------------------

make_panel_single <- function(df, axis_key, show_xlab = FALSE) {
  df     <- ordered_df(df)
  colors <- setNames(df$color_hex, as.character(df$class_label))

  ggplot(df, aes(x = log2_sr, y = class_label, fill = class_label)) +
    geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.5) +
    geom_col(width = 0.72, na.rm = TRUE, show.legend = FALSE) +
    scale_fill_manual(values = colors) +
    scale_x_continuous(
      limits = LOG2_LIMITS,
      breaks = LOG2_BREAKS,
      labels = LOG2_LABELS,
      name   = if (show_xlab) "Sampling ratio (network / global)" else NULL,
      expand = expansion(mult = 0)
    ) +
    scale_y_discrete(name = NULL) +
    labs(subtitle = AXIS_TITLES[[axis_key]]) +
    base_theme +
    theme(
      plot.subtitle      = element_text(size = 7.5, face = "plain", colour = "grey30"),
      axis.text.y        = element_text(size = 7),
      axis.text.x        = if (show_xlab) element_text(size = 7) else element_blank(),
      axis.ticks.x       = if (show_xlab) element_line() else element_blank(),
      axis.title.x       = element_text(size = 8),
      panel.grid.major.x = element_line(colour = "grey88", linewidth = 0.3),
      panel.grid.major.y = element_blank()
    )
}

make_panel_overlay <- function(df_a, df_b, axis_key, lbl_a, lbl_b, show_xlab = FALSE) {
  df_a <- ordered_df(df_a) |> dplyr::mutate(net_grp = lbl_a)
  df_b <- df_b |>
    dplyr::arrange(class_order) |>
    dplyr::mutate(
      class_label = factor(class_label, levels = levels(df_a$class_label)),
      net_grp = lbl_b
    )
  df_both <- dplyr::bind_rows(df_a, df_b) |>
    dplyr::mutate(net_grp = factor(net_grp, levels = c(lbl_b, lbl_a)))

  colors <- setNames(df_a$color_hex, as.character(df_a$class_label))

  ggplot(df_both,
         aes(x = log2_sr, y = class_label, fill = class_label, alpha = net_grp)) +
    geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.5) +
    geom_col(position = position_dodge(width = 0.85), width = 0.8,
             na.rm = TRUE, show.legend = FALSE) +
    scale_fill_manual(values = colors) +
    scale_alpha_manual(values = setNames(c(0.42, 1.0), c(lbl_b, lbl_a)),
                       guide = "none") +
    scale_x_continuous(
      limits = LOG2_LIMITS,
      breaks = LOG2_BREAKS,
      labels = LOG2_LABELS,
      name   = if (show_xlab) "Sampling ratio (network / global)" else NULL,
      expand = expansion(mult = 0)
    ) +
    scale_y_discrete(name = NULL) +
    labs(subtitle = AXIS_TITLES[[axis_key]]) +
    base_theme +
    theme(
      plot.subtitle      = element_text(size = 7.5, face = "plain", colour = "grey30"),
      axis.text.y        = element_text(size = 7),
      axis.text.x        = if (show_xlab) element_text(size = 7) else element_blank(),
      axis.ticks.x       = if (show_xlab) element_line() else element_blank(),
      axis.title.x       = element_text(size = 8),
      panel.grid.major.x = element_line(colour = "grey88", linewidth = 0.3),
      panel.grid.major.y = element_blank()
    )
}

make_panel_delta <- function(df_a, df_b, axis_key, show_xlab = FALSE) {
  delta_df <- df_a |>
    dplyr::select(class, class_label, class_order, sampling_ratio) |>
    dplyr::rename(sr_a = sampling_ratio) |>
    dplyr::left_join(
      df_b |> dplyr::select(class, sampling_ratio) |> dplyr::rename(sr_b = sampling_ratio),
      by = "class"
    ) |>
    dplyr::mutate(
      sr_a       = dplyr::coalesce(sr_a, 0.0),
      sr_b       = dplyr::coalesce(sr_b, 0.0),
      delta      = sr_a - sr_b,
      fill_color = dplyr::if_else(delta >= 0, "#2166ac", "#d6604d")
    ) |>
    ordered_df()

  ggplot(delta_df, aes(x = delta, y = class_label, fill = fill_color)) +
    geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.5) +
    geom_col(width = 0.72, na.rm = TRUE, show.legend = FALSE) +
    scale_fill_identity() +
    scale_x_continuous(
      name   = if (show_xlab) "Delta sampling ratio (current − FLUXNET2015)" else NULL,
      expand = expansion(mult = 0.05)
    ) +
    scale_y_discrete(name = NULL) +
    labs(subtitle = AXIS_TITLES[[axis_key]]) +
    base_theme +
    theme(
      plot.subtitle      = element_text(size = 7.5, face = "plain", colour = "grey30"),
      axis.text.y        = element_text(size = 7),
      axis.text.x        = if (show_xlab) element_text(size = 7) else element_blank(),
      axis.ticks.x       = if (show_xlab) element_line() else element_blank(),
      axis.title.x       = element_text(size = 8),
      panel.grid.major.x = element_line(colour = "grey88", linewidth = 0.3),
      panel.grid.major.y = element_blank()
    )
}

# ---- Network labels ----------------------------------------------------------
NET_LABELS <- c(
  current_767 = "Current FLUXNET (n=767)",
  marconi     = "Marconi (n=35)",
  la_thuile   = "La Thuile (n=252)",
  fluxnet2015 = "FLUXNET2015 (n=212)"
)

# ---- make_summary_figure() ---------------------------------------------------
#' @param network         one of: "current_767", "marconi", "la_thuile", "fluxnet2015"
#' @param mode            "single" | "overlay" | "delta"
#' @param overlay_network second network for overlay/delta (e.g., "fluxnet2015")
#' @param output_path     if supplied, saves PNG at that path
make_summary_figure <- function(network,
                                mode            = c("single", "overlay", "delta"),
                                overlay_network = NULL,
                                output_path     = NULL) {
  mode  <- match.arg(mode)
  if (mode %in% c("overlay", "delta") && is.null(overlay_network))
    stop("`overlay_network` required for mode '", mode, "'.", call. = FALSE)

  axes_a <- load_all_axes(network)
  lbl_a  <- NET_LABELS[[network]]

  if (mode == "single") {
    panels <- lapply(seq_along(AXIS_KEYS), function(i) {
      make_panel_single(axes_a[[AXIS_KEYS[[i]]]], AXIS_KEYS[[i]],
                        show_xlab = (i == length(AXIS_KEYS)))
    })
    fig_title <- paste0("Representativeness — ", lbl_a)

  } else {
    axes_b <- load_all_axes(overlay_network)
    lbl_b  <- NET_LABELS[[overlay_network]]

    if (mode == "overlay") {
      panels <- lapply(seq_along(AXIS_KEYS), function(i) {
        make_panel_overlay(axes_a[[AXIS_KEYS[[i]]]], axes_b[[AXIS_KEYS[[i]]]],
                           AXIS_KEYS[[i]], lbl_a, lbl_b,
                           show_xlab = (i == length(AXIS_KEYS)))
      })
      fig_title <- paste0("Representativeness — ", lbl_a, " vs. ", lbl_b)

    } else {
      panels <- lapply(seq_along(AXIS_KEYS), function(i) {
        make_panel_delta(axes_a[[AXIS_KEYS[[i]]]], axes_b[[AXIS_KEYS[[i]]]],
                         AXIS_KEYS[[i]],
                         show_xlab = (i == length(AXIS_KEYS)))
      })
      fig_title <- paste0("Representativeness change — ", lbl_a, " minus ", lbl_b)
    }
  }

  fig <- patchwork::wrap_plots(panels, ncol = 1) +
    patchwork::plot_annotation(
      title = fig_title,
      theme = theme(
        plot.title      = element_text(size = 11, face = "bold"),
        plot.background = element_rect(fill = "white", colour = NA)
      )
    )

  if (!is.null(output_path)) {
    ggplot2::ggsave(output_path, fig,
                    width = 7, height = 14, dpi = 300, bg = "white")
    message("Saved: ", output_path)
  }

  invisible(fig)
}

# ---- Rep007: Jaccard trajectory across four network generations --------------
make_rep007 <- function(output_path = NULL) {
  metrics <- readr::read_csv(
    file.path(SNAP, "representativeness_metrics.csv"),
    show_col_types = FALSE
  )

  NET_ORDER   <- c("marconi", "la_thuile", "fluxnet2015", "current_767")
  NET_XLABELS <- c(
    marconi     = "Marconi\n(n=35)",
    la_thuile   = "La Thuile\n(n=252)",
    fluxnet2015 = "FLUXNET2015\n(n=212)",
    current_767 = "Current\n(n=767)"
  )

  traj_sel <- tibble::tribble(
    ~axis,                ~aggregation_level,  ~axis_label,
    "koppen_beck2023",    "13class_twoletter", "KG (13-class)",
    "landcover_cci",      "high_level",        "LULC (10-class)",
    "aridity_unep7",      "unep7",             "Aridity (7-class)",
    "biomass_cci_v7",     "7bin_hybrid",       "Biomass (7-bin)",
    "trendy_nee_iav",     "7bin_hybrid",       "TRENDY NEE-IAV",
    "trendy_et_median",   "7bin_hybrid",       "TRENDY ET-median"
  )

  traj_df <- metrics |>
    dplyr::inner_join(traj_sel, by = c("axis", "aggregation_level")) |>
    dplyr::mutate(
      network    = factor(network, levels = NET_ORDER),
      axis_label = factor(axis_label, levels = REP007_AXES)
    )

  p <- ggplot(traj_df, aes(x = network, y = weighted_jaccard,
                            colour = axis_label, group = axis_label)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.6) +
    scale_colour_manual(values = REP007_COLORS, name = NULL) +
    scale_x_discrete(labels = NET_XLABELS) +
    scale_y_continuous(
      name   = "Weighted Jaccard (J)",
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.2),
      expand = expansion(mult = 0.02)
    ) +
    labs(title = "Representativeness trajectory across FLUXNET network generations") +
    base_theme +
    theme(
      legend.position  = "right",
      legend.key.size  = unit(0.6, "lines"),
      legend.text      = element_text(size = 8),
      axis.text.x      = element_text(size = 9),
      axis.title.y     = element_text(size = 9),
      panel.grid.major = element_line(colour = "grey88", linewidth = 0.3)
    )

  if (!is.null(output_path)) {
    ggplot2::ggsave(output_path, p,
                    width = 7, height = 4.5, dpi = 300, bg = "white")
    message("Saved: ", output_path)
  }

  invisible(p)
}

# ---- Produce all seven figures -----------------------------------------------

message("Rep001: current_767 (single)")
make_summary_figure("current_767", "single",
  output_path = file.path(OUTD, "fig_rep001_current.png"))

message("Rep002: marconi (single)")
make_summary_figure("marconi", "single",
  output_path = file.path(OUTD, "fig_rep002_marconi.png"))

message("Rep003: la_thuile (single)")
make_summary_figure("la_thuile", "single",
  output_path = file.path(OUTD, "fig_rep003_la_thuile.png"))

message("Rep004: fluxnet2015 (single)")
make_summary_figure("fluxnet2015", "single",
  output_path = file.path(OUTD, "fig_rep004_fluxnet2015.png"))

message("Rep005: current_767 vs fluxnet2015 (overlay)")
make_summary_figure("current_767", "overlay", overlay_network = "fluxnet2015",
  output_path = file.path(OUTD, "fig_rep005_fluxnet2015_vs_current.png"))

message("Rep006: current_767 minus fluxnet2015 (delta)")
make_summary_figure("current_767", "delta", overlay_network = "fluxnet2015",
  output_path = file.path(OUTD, "fig_rep006_delta_2015_to_current.png"))

message("Rep007: Jaccard trajectory")
make_rep007(output_path = file.path(OUTD, "fig_rep007_jaccard_trajectory.png"))

message("Done!")
