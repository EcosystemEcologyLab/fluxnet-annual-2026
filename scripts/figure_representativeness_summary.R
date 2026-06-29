## figure_representativeness_summary.R
##
## Rep001–Rep010: FLUXNET network representativeness figures.
##
## Figs 001–004  single-network 2×3 bar grids (sampling ratio, log2 scale)
## Fig  005      overlay 2×3 grid (current_767 vs FLUXNET2015)
## Fig  006      count-difference 2×3 grid (current_767 minus FLUXNET2015)
## Figs 007–008  Jaccard trajectory, 6 default axes (no bars / with count bars)
## Fig  009      Jaccard trajectory, 3 native-resolution categorical axes
## Fig  010      Jaccard trajectory, 5 continuous axes at 30-bin hybrid
##
## Default axis set (Figs 001–006):
##   KG 13-class | LULC 10-class HL | Aridity 7-class
##   Biomass 7-bin | TRENDY NEE-IAV 7-bin | TRENDY ET-median 7-bin
##
## Fig 009 axis set: KG 30-class | LULC 37-class native | Aridity 7-class
## Fig 010 axis set: Biomass 30-bin | NEE-IAV | NEE-median | ET-IAV | ET-median

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(patchwork)
  library(fs)
})

source("R/pipeline_config.R")
check_pipeline_config()

SNAP <- "data/snapshots"
EXT  <- "data/external"
OUTD <- "review/figures/representativeness"
fs::dir_create(OUTD)

# ============================================================================
# 1. COLOR PALETTES
# ============================================================================

# ---- 1a. KG legend (Beck 2023 legend.txt) -----------------------------------
kg_leg_raw <- readLines(file.path(EXT, "koppen_beck2023", "legend.txt"))
kg_leg_raw <- kg_leg_raw[grepl("^\\s+\\d+:", kg_leg_raw)]
kg_leg_df  <- data.frame(
  koppen_class = sub("^\\s*\\d+:\\s+(\\S+)\\s+.*",       "\\1", kg_leg_raw),
  r = as.integer(sub(".*\\[(\\d+)\\s+\\d+\\s+\\d+\\].*", "\\1", kg_leg_raw)),
  g = as.integer(sub(".*\\[\\d+\\s+(\\d+)\\s+\\d+\\].*", "\\1", kg_leg_raw)),
  b = as.integer(sub(".*\\[\\d+\\s+\\d+\\s+(\\d+)\\].*", "\\1", kg_leg_raw)),
  stringsAsFactors = FALSE
) |>
  dplyr::mutate(
    koppen_twoletter = substr(koppen_class, 1, 2),
    color_hex        = grDevices::rgb(r, g, b, maxColorValue = 255)
  )

# 13-class (two-letter) colors: mean RGB of all 30-class members per group
TL_ORDER <- c("Af","Am","Aw","BS","BW","Cf","Cs","Cw","Df","Ds","Dw","EF","ET")
KG13_COLORS <- setNames(
  vapply(TL_ORDER, function(tl) {
    members <- kg_leg_df[kg_leg_df$koppen_twoletter == tl, ]
    grDevices::rgb(mean(members$r), mean(members$g), mean(members$b), maxColorValue = 255)
  }, character(1L)),
  TL_ORDER
)

# 30-class colors: individual RGB per class (legend order = code order)
KG30_ORDER  <- kg_leg_df$koppen_class           # Af, Am, ..., EF (code order)
KG30_COLORS <- setNames(kg_leg_df$color_hex, KG30_ORDER)

# ---- 1b. LULC high-level (10-class) colors ----------------------------------
lc_lut <- readr::read_csv(
  file.path(SNAP, "cci_landcover_aggregation_lookup.csv"),
  show_col_types = FALSE
)
lc_hl_colors_df <- lc_lut |>
  dplyr::group_by(lulc_highlevel) |>
  dplyr::summarise(R = mean(R), G = mean(G), B = mean(B), .groups = "drop") |>
  dplyr::mutate(
    hex = grDevices::rgb(R, G, B, maxColorValue = 255),
    hex = dplyr::if_else(lulc_highlevel == 8L, "#e0f3f8", hex)  # Snow/Ice override
  )
LULC_HL_COLORS <- setNames(lc_hl_colors_df$hex, as.character(lc_hl_colors_df$lulc_highlevel))

# 37-class (native) colors: individual RGB per native code
lc_native_colors_df <- lc_lut |>
  dplyr::mutate(
    hex = grDevices::rgb(R, G, B, maxColorValue = 255)
  ) |>
  dplyr::distinct(lulc_native, .keep_all = TRUE)
LULC_NATIVE_COLORS <- setNames(lc_native_colors_df$hex,
                                as.character(lc_native_colors_df$lulc_native))

# ---- 1c. Aridity 7-class (hardcoded from aridity figure script) ------------
ARIDITY_ORDER <- c("Hyper-Arid","Arid","Semi-Arid","Dry Sub-Humid",
                   "Humid (low)","Humid (moderate)","Hyper-Humid")
ARIDITY_COLORS <- c(
  "Hyper-Arid"       = "#d73027", "Arid"             = "#fc8d59",
  "Semi-Arid"        = "#ffff33", "Dry Sub-Humid"    = "#66bd63",
  "Humid (low)"      = "#74add1", "Humid (moderate)" = "#4575b4",
  "Hyper-Humid"      = "#313695"
)

# ---- 1d. Biomass 7-bin sequential green palette ----------------------------
BIO7_COLORS <- c("1"="#f7f4f9","2"="#f0e1c4","3"="#d4d491",
                 "4"="#a3c585","5"="#6cb375","6"="#2e8b57","7"="#14532d")

# ---- 1e. TRENDY NEE 7-bin and ET 7-bin palettes ----------------------------
NEE7_COLORS <- c("1"="#f4faf0","2"="#c8e8a4","3"="#9acb72",
                 "4"="#67ae42","5"="#3d8c27","6"="#1f6415","7"="#0b3e09")
ET7_COLORS  <- c("1"="#f0f8ff","2"="#bcd8f4","3"="#82bce8",
                 "4"="#4498d5","5"="#1d74b3","6"="#0c4f84","7"="#06305a")

# ---- 1f. Trajectory qualitative palettes (Okabe-Ito) -----------------------

# 6-axis palette for Fig 007–008 (continuous axes at 18-bin from this version onward)
TRAJ6_COLORS <- c(
  "KG (13-class)"     = "#D55E00",   # vermilion
  "LULC (10-class)"   = "#CC79A7",   # reddish pink
  "Aridity (7-class)" = "#E69F00",   # amber
  "Biomass (18-bin)"  = "#009E73",   # green
  "TRENDY NEE-IAV"    = "#0072B2",   # blue
  "TRENDY ET-median"  = "#56B4E9"    # sky blue
)

# 3-axis palette for Fig 009 (first three Okabe-Ito: vermilion, reddish pink, amber)
TRAJ3_COLORS <- c(
  "KG (30-class)"      = "#D55E00",
  "LULC (37-class)"    = "#CC79A7",
  "Aridity (7-class)"  = "#E69F00"
)

# 5-axis palette for Fig 010
# Palette rationale: biomass green (Okabe-Ito #009E73), NEE-IAV Okabe-Ito blue (#0072B2),
# NEE-median darkened navy (#1a5276 — same blue family, clearly darker), ET-IAV sky
# blue (#56B4E9), ET-median teal (#00898e — between green and sky blue). All five
# distinguishable at print size; tonally related within each variable pair (NEE, ET).
TRAJ5_COLORS <- c(
  "Biomass (30-bin)"      = "#009E73",
  "TRENDY NEE-IAV"        = "#0072B2",
  "TRENDY NEE-median"     = "#1a5276",
  "TRENDY ET-IAV"         = "#56B4E9",
  "TRENDY ET-median"      = "#00898e"
)

# ---- 1g. Sequential single-hue palettes for Rep011-018 ----------------------
#
# shade_palette(base_hex, factors): returns length(factors) hex colors.
#   f <= 1: blend base toward white  (f=1 = pure base, f=0 = white)
#   f > 1:  darken below base        (f=1.3 → multiply RGB by 0.7)
# No external packages — uses only base grDevices.
shade_palette <- function(base_hex, factors) {
  base_rgb <- as.numeric(grDevices::col2rgb(base_hex) / 255)  # plain vector [R, G, B]
  vapply(factors, function(f) {
    if (f <= 1) {
      rgb_new <- base_rgb * f + (1 - f)
    } else {
      rgb_new <- base_rgb * (2 - f)
    }
    rgb_new <- pmax(0, pmin(1, rgb_new))
    grDevices::rgb(rgb_new[1L], rgb_new[2L], rgb_new[3L])
  }, character(1L))
}

# Documented shading factors (lightest → darkest within each family):
#   3-shade: f = 0.30, 0.65, 1.00  (light, medium, base)
#   5-shade: f = 0.25, 0.50, 0.75, 1.00, 1.30  (very light → dark)
#   2-shade: f = 0.45, 1.00  (light, base)
#
# Legend ordering: finest-first (darkest shade at top of legend).
# setNames(rev(shades), c(finest, ..., coarsest)) achieves this.

# Rep011: KG — vermilion #D55E00
KG_AGG_COLORS <- setNames(
  rev(shade_palette("#D55E00", c(0.30, 0.65, 1.00))),
  c("30-class", "13-class", "5-class")
)

# Rep012: LULC — reddish pink #CC79A7
LULC_AGG_COLORS <- setNames(
  rev(shade_palette("#CC79A7", c(0.30, 0.65, 1.00))),
  c("37-class (native)", "22-class (level 2)", "10-class (high level)")
)

# Rep013: Aridity — amber #E69F00, 2 shades
ARID_AGG_COLORS <- setNames(
  rev(shade_palette("#E69F00", c(0.45, 1.00))),
  c("7-class", "5-class")
)

# Rep014: Biomass — green #009E73
BIO_AGG_COLORS <- setNames(
  rev(shade_palette("#009E73", c(0.25, 0.50, 0.75, 1.00, 1.30))),
  c("30-bin", "20-bin", "18-bin", "12-bin", "7-bin")
)

# Rep015: NEE-IAV — blue #0072B2
NEEIAV_AGG_COLORS <- setNames(
  rev(shade_palette("#0072B2", c(0.25, 0.50, 0.75, 1.00, 1.30))),
  c("30-bin", "20-bin", "18-bin", "12-bin", "7-bin")
)

# Rep016: NEE-median — dark navy #1a5276
NEEMED_AGG_COLORS <- setNames(
  rev(shade_palette("#1a5276", c(0.25, 0.50, 0.75, 1.00, 1.30))),
  c("30-bin", "20-bin", "18-bin", "12-bin", "7-bin")
)

# Rep017: ET-IAV — sky blue #56B4E9
ETIAV_AGG_COLORS <- setNames(
  rev(shade_palette("#56B4E9", c(0.25, 0.50, 0.75, 1.00, 1.30))),
  c("30-bin", "20-bin", "18-bin", "12-bin", "7-bin")
)

# Rep018: ET-median — teal #00898e
ETMED_AGG_COLORS <- setNames(
  rev(shade_palette("#00898e", c(0.25, 0.50, 0.75, 1.00, 1.30))),
  c("30-bin", "20-bin", "18-bin", "12-bin", "7-bin")
)

# ============================================================================
# 2. GLOBAL DISTRIBUTIONS (loaded once, normalized to class key = character)
# ============================================================================

kg13_global <- readr::read_csv(
  file.path(SNAP, "koppen_beck2023_global_distribution.csv"), show_col_types = FALSE
) |>
  dplyr::group_by(koppen_twoletter) |>
  dplyr::summarise(global_land_fraction = sum(global_land_fraction), .groups = "drop") |>
  dplyr::rename(class = koppen_twoletter)

kg30_global <- readr::read_csv(
  file.path(SNAP, "koppen_beck2023_global_distribution.csv"), show_col_types = FALSE
) |>
  dplyr::select(class = koppen_class, global_land_fraction)

aridity_global <- readr::read_csv(
  file.path(SNAP, "aridity_unep7_global_distribution.csv"), show_col_types = FALSE
) |>
  dplyr::rename(class = unep_class)

lulc_hl_global <- readr::read_csv(
  file.path(SNAP, "landcover_cci_highlevel_global_distribution.csv"), show_col_types = FALSE
) |>
  dplyr::mutate(class = as.character(cci_high_level_class))

lulc_native_global <- readr::read_csv(
  file.path(SNAP, "landcover_cci_native_global_distribution.csv"), show_col_types = FALSE
) |>
  dplyr::mutate(class = as.character(lulc_native))

bio7_global <- readr::read_csv(
  file.path(SNAP, "biomass_cci_v7_global_distribution.csv"), show_col_types = FALSE
) |>
  dplyr::mutate(class = as.character(biomass_bin))

nee_iav7_global <- readr::read_csv(
  file.path(SNAP, "trendy_nee_iav_global_distribution.csv"), show_col_types = FALSE
) |>
  dplyr::mutate(class = as.character(bin))

et_med7_global <- readr::read_csv(
  file.path(SNAP, "trendy_et_median_global_distribution.csv"), show_col_types = FALSE
) |>
  dplyr::mutate(class = as.character(bin))

# ---- Load Jaccard metrics ---------------------------------------------------
metrics_df <- readr::read_csv(
  file.path(SNAP, "representativeness_metrics.csv"), show_col_types = FALSE
)

# ============================================================================
# 3. SITE LOADER HELPERS
# ============================================================================

site_csv <- function(base, network) {
  suffix <- if (network == "current_767") "" else paste0("_", network)
  file.path(SNAP, paste0("site_", base, suffix, ".csv"))
}

count_sites <- function(df, class_col) {
  n_total <- nrow(df)
  df |>
    dplyr::filter(!is.na(.data[[class_col]])) |>
    dplyr::count(.data[[class_col]], name = "n") |>
    dplyr::rename(class = 1) |>
    dplyr::mutate(class = as.character(class), network_frac = n / n_total)
}

# merge site counts with global dist -> sampling_ratio + log2_sr
merge_sr <- function(site_counts, global_df) {
  global_df |>
    dplyr::select(class, global_land_fraction) |>
    dplyr::left_join(site_counts |> dplyr::select(class, n, network_frac), by = "class") |>
    dplyr::mutate(
      n            = dplyr::coalesce(n, 0L),
      network_frac = dplyr::coalesce(network_frac, 0.0),
      sampling_ratio = dplyr::if_else(
        global_land_fraction > 0 & network_frac > 0,
        network_frac / global_land_fraction, NA_real_
      ),
      log2_sr = dplyr::if_else(!is.na(sampling_ratio), log2(sampling_ratio), NA_real_)
    )
}

# retrieve J for a given axis/aggregation/network combination
get_j <- function(axis, agg, net) {
  v <- metrics_df |>
    dplyr::filter(.data$axis == !!axis, aggregation_level == agg, network == net) |>
    dplyr::pull(weighted_jaccard)
  if (length(v) == 0L) NA_real_ else v[[1L]]
}

# ============================================================================
# 4. DEFAULT 6-AXIS CONFIGURATION (Figs 001–006)
# ============================================================================

# Each entry: load site counts for a given network; add class_label, class_order,
# color_hex; specify the global_df, metrics lookup keys, and display title.
AXES6 <- list(
  kg = list(
    title   = "Köppen-Geiger (Beck 2023, 13-class)",
    m_axis  = "koppen_beck2023", m_agg = "13class_twoletter",
    load_fn = function(net) {
      readr::read_csv(site_csv("koppen_beck2023", net), show_col_types = FALSE) |>
        count_sites("koppen_twoletter")
    },
    global_df = kg13_global,
    augment_fn = function(df) {
      df |> dplyr::mutate(
        class_label = class,
        class_order = match(class, TL_ORDER),
        color_hex   = KG13_COLORS[class]
      )
    }
  ),
  lulc = list(
    title   = "ESA CCI Land Cover v2.1.1 (10-class, high-level)",
    m_axis  = "landcover_cci", m_agg = "high_level",
    load_fn = function(net) {
      readr::read_csv(site_csv("landcover_cci", net), show_col_types = FALSE) |>
        count_sites("cci_high_level_class")
    },
    global_df = lulc_hl_global,
    augment_fn = function(df) {
      lulc_hl_global |>
        dplyr::select(class, cci_high_level_class_name) |>
        dplyr::right_join(df, by = "class") |>
        dplyr::mutate(
          class_label = cci_high_level_class_name,
          class_order = as.integer(class),
          color_hex   = LULC_HL_COLORS[class]
        )
    }
  ),
  aridity = list(
    title   = "CGIAR Aridity Index v3.1 (7-class, UNEP scheme)",
    m_axis  = "aridity_unep7", m_agg = "unep7",
    load_fn = function(net) {
      readr::read_csv(site_csv("aridity", net), show_col_types = FALSE) |>
        count_sites("unep_class_7")
    },
    global_df = aridity_global,
    augment_fn = function(df) {
      df |> dplyr::mutate(
        class_label = class,
        class_order = match(class, ARIDITY_ORDER),
        color_hex   = ARIDITY_COLORS[class]
      )
    }
  ),
  biomass = list(
    title   = "ESA CCI Biomass v7, 2024 estimate (7-bin hybrid)",
    m_axis  = "biomass_cci_v7", m_agg = "7bin_hybrid",
    load_fn = function(net) {
      readr::read_csv(site_csv("biomass_cci_v7", net), show_col_types = FALSE) |>
        count_sites("biomass_bin")
    },
    global_df = bio7_global,
    augment_fn = function(df) {
      bio7_global |>
        dplyr::select(class, bin_label = biomass_bin_label) |>
        dplyr::right_join(df, by = "class") |>
        dplyr::mutate(
          class_label = bin_label,
          class_order = as.integer(class),
          color_hex   = BIO7_COLORS[class]
        )
    }
  ),
  nee_iav = list(
    title   = "TRENDY v14 NEE-IAV (7-bin hybrid)",
    m_axis  = "trendy_nee_iav", m_agg = "7bin_hybrid",
    load_fn = function(net) {
      readr::read_csv(site_csv("trendy_nee_iav", net), show_col_types = FALSE) |>
        count_sites("trendy_nee_iav_bin")
    },
    global_df = nee_iav7_global,
    augment_fn = function(df) {
      nee_iav7_global |>
        dplyr::select(class, bin_label) |>
        dplyr::right_join(df, by = "class") |>
        dplyr::mutate(
          class_label = bin_label,
          class_order = as.integer(class),
          color_hex   = NEE7_COLORS[class]
        )
    }
  ),
  et_median = list(
    title   = "TRENDY v14 ET-median (7-bin hybrid)",
    m_axis  = "trendy_et_median", m_agg = "7bin_hybrid",
    load_fn = function(net) {
      readr::read_csv(site_csv("trendy_et_median", net), show_col_types = FALSE) |>
        count_sites("trendy_et_median_bin")
    },
    global_df = et_med7_global,
    augment_fn = function(df) {
      et_med7_global |>
        dplyr::select(class, bin_label) |>
        dplyr::right_join(df, by = "class") |>
        dplyr::mutate(
          class_label = bin_label,
          class_order = as.integer(class),
          color_hex   = ET7_COLORS[class]
        )
    }
  )
)

# layout: 2x3 grid positions
AXES6_KEYS <- c("kg","lulc","aridity","biomass","nee_iav","et_median")

# ============================================================================
# 5. SHARED THEME + CONSTANTS
# ============================================================================

LOG2_MAX    <- log2(5)                           # 2.322
LOG2_BREAKS <- c(-LOG2_MAX, -1, 0, 1, LOG2_MAX)
LOG2_LABELS <- c("1/5×","1/2×","1×","2×","5×")
LOG2_XLIM   <- c(-LOG2_MAX - 0.25, LOG2_MAX + 0.25)

# Fixed symmetric range for Rep006 count-difference panels.
# Max observed delta (current minus FLUXNET2015) = 199 sites; 210 gives ~5% headroom.
COUNT_DIFF_XLIM <- c(-210, 210)

base_theme <- theme_minimal(base_size = 9) +
  theme(
    plot.background   = element_rect(fill = "white", colour = NA),
    panel.background  = element_rect(fill = "white", colour = NA),
    panel.border      = element_rect(colour = "black", fill = NA, linewidth = 0.4),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    axis.ticks        = element_line(colour = "black"),
    axis.ticks.length = unit(-0.15, "cm"),
    legend.background = element_rect(fill = "white", colour = NA)
  )

# Sort df by class_order, set class_label as ordered factor (bottom=first in coord_flip)
prep_ordered <- function(df) {
  df |>
    dplyr::arrange(class_order) |>
    dplyr::mutate(class_label = factor(class_label, levels = unique(class_label)))
}

# Clip log2_sr to ±LOG2_MAX; flag truncated bars; build annotation text
prep_clip <- function(df) {
  df |>
    dplyr::mutate(
      log2_sr_clip = pmax(pmin(dplyr::coalesce(log2_sr, 0), LOG2_MAX), -LOG2_MAX),
      truncated    = !is.na(log2_sr) & abs(log2_sr) > LOG2_MAX,
      absent       = is.na(log2_sr) & n == 0L,
      annot_label  = dplyr::case_when(
        truncated & log2_sr > 0 ~ paste0(sprintf("%.1f", sampling_ratio), "×"),
        truncated & log2_sr < 0 ~ paste0("1/", sprintf("%.1f", 1 / sampling_ratio), "×"),
        TRUE ~ NA_character_
      ),
      annot_x = dplyr::case_when(
        truncated & log2_sr > 0 ~  LOG2_MAX - 0.08,
        truncated & log2_sr < 0 ~ -LOG2_MAX + 0.08,
        TRUE ~ NA_real_
      ),
      annot_hjust = dplyr::case_when(
        truncated & log2_sr > 0 ~ 1,
        truncated & log2_sr < 0 ~ 0,
        TRUE ~ 0.5
      )
    )
}

# ============================================================================
# 6. SINGLE-NETWORK PANEL (Figs 001–004)
# ============================================================================

make_panel_single <- function(ax, network, metrics_df, show_xlab = FALSE,
                               panel_label = NULL) {
  site_counts <- ax$load_fn(network)
  df <- merge_sr(site_counts, ax$global_df) |>
    ax$augment_fn() |>
    prep_ordered() |>
    prep_clip()

  j_val    <- get_j(ax$m_axis, ax$m_agg, network)
  col_vals  <- setNames(df$color_hex, as.character(df$class_label))
  n_lev     <- nlevels(df$class_label)
  y_top     <- n_lev + 0.45   # inside expanded margin above top bar

  p <- ggplot(df, aes(x = log2_sr_clip, y = class_label, fill = class_label)) +
    geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.5) +
    geom_col(width = 0.72, na.rm = TRUE, show.legend = FALSE,
             colour = "black", linewidth = 0.25) +
    scale_fill_manual(values = col_vals) +
    scale_x_continuous(limits = LOG2_XLIM, breaks = LOG2_BREAKS, labels = LOG2_LABELS,
                       expand = expansion(mult = 0),
                       name   = if (show_xlab) "Sampling ratio (network / global)" else NULL) +
    scale_y_discrete(name = NULL) +
    labs(subtitle = ax$title) +
    base_theme +
    theme(
      plot.subtitle = element_text(size = 7.5, face = "plain", colour = "grey30"),
      axis.text.y   = element_text(size = 6.5, margin = margin(r = 5)),
      axis.text.x   = if (show_xlab) element_text(size = 7, margin = margin(t = 5))
                      else element_blank(),
      axis.ticks.x  = if (show_xlab) element_line() else element_blank(),
      axis.title.x  = element_text(size = 8)
    )

  # Truncated-bar annotations
  ann_df <- dplyr::filter(df, !is.na(annot_label))
  if (nrow(ann_df) > 0) {
    p <- p + geom_text(
      data = ann_df,
      aes(x = annot_x, y = class_label, label = annot_label, hjust = annot_hjust),
      inherit.aes = FALSE, size = 2.2, colour = "grey15", fontface = "plain"
    )
  }

  # Panel label A–F (top-left)
  if (!is.null(panel_label)) {
    p <- p + annotate("text", x = LOG2_XLIM[1] + 0.08, y = y_top,
                      label = panel_label, hjust = 0, vjust = 0,
                      size = 3.5, fontface = "bold", colour = "grey10")
  }

  # J value (top-right)
  if (!is.na(j_val)) {
    p <- p + annotate("text", x = LOG2_XLIM[2] - 0.08, y = y_top,
                      label = sprintf("J = %.3f", j_val), hjust = 1, vjust = 0,
                      size = 2.5, colour = "grey20")
  }

  p
}

# ============================================================================
# 7. OVERLAY PANEL (Fig 005: current_767 vs fluxnet2015)
# ============================================================================

make_panel_overlay <- function(ax, net_a, net_b, metrics_df, show_xlab = FALSE,
                               panel_label = NULL) {
  counts_a <- ax$load_fn(net_a)
  counts_b <- ax$load_fn(net_b)
  df_a <- merge_sr(counts_a, ax$global_df) |> ax$augment_fn() |> prep_ordered() |> prep_clip()
  df_b <- merge_sr(counts_b, ax$global_df) |> ax$augment_fn() |>
    dplyr::mutate(class_label = factor(class_label, levels = levels(df_a$class_label))) |>
    prep_clip()

  j_a <- get_j(ax$m_axis, ax$m_agg, net_a)
  j_b <- get_j(ax$m_axis, ax$m_agg, net_b)

  lbl_a <- "Current (n=767)"
  lbl_b <- "FLUXNET2015 (n=212)"

  df_both <- dplyr::bind_rows(
    dplyr::mutate(df_a, net_grp = lbl_a),
    dplyr::mutate(df_b, net_grp = lbl_b)
  ) |>
    dplyr::mutate(net_grp = factor(net_grp, levels = c(lbl_a, lbl_b)))

  col_vals <- setNames(df_a$color_hex, as.character(df_a$class_label))
  n_lev    <- nlevels(df_a$class_label)
  y_top    <- n_lev + 0.45

  p <- ggplot(df_both, aes(x = log2_sr_clip, y = class_label,
                            fill = class_label, alpha = net_grp)) +
    geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.5) +
    geom_col(position = position_dodge(width = 0.85), width = 0.8,
             na.rm = TRUE, colour = "black", linewidth = 0.2,
             show.legend = c(fill = FALSE, alpha = TRUE)) +
    scale_fill_manual(values = col_vals) +
    scale_alpha_manual(
      name   = "Network",
      values = c("Current (n=767)" = 1.0, "FLUXNET2015 (n=212)" = 0.42),
      guide  = guide_legend(override.aes = list(fill = "grey50",
                                                colour = "black", linewidth = 0.25))
    ) +
    scale_x_continuous(limits = LOG2_XLIM, breaks = LOG2_BREAKS, labels = LOG2_LABELS,
                       expand = expansion(mult = 0),
                       name   = if (show_xlab) "Sampling ratio (network / global)" else NULL) +
    scale_y_discrete(name = NULL) +
    labs(subtitle = ax$title) +
    base_theme +
    theme(
      plot.subtitle = element_text(size = 7.5, face = "plain", colour = "grey30"),
      axis.text.y   = element_text(size = 6.5, margin = margin(r = 5)),
      axis.text.x   = if (show_xlab) element_text(size = 7, margin = margin(t = 5))
                      else element_blank(),
      axis.ticks.x  = if (show_xlab) element_line() else element_blank(),
      axis.title.x  = element_text(size = 8),
      legend.position = "top",
      legend.title    = element_text(size = 7.5),
      legend.text     = element_text(size = 7)
    )

  # Panel label (top-left)
  if (!is.null(panel_label)) {
    p <- p + annotate("text", x = LOG2_XLIM[1] + 0.08, y = y_top,
                      label = panel_label, hjust = 0, vjust = 0,
                      size = 3.5, fontface = "bold", colour = "grey10")
  }

  # J values stacked (top-right) — current above, 2015 below
  if (!is.na(j_a)) {
    p <- p + annotate("text", x = LOG2_XLIM[2] - 0.08, y = y_top,
                      label = sprintf("J = %.3f (current)", j_a),
                      hjust = 1, vjust = 0, size = 2.2, colour = "grey20")
  }
  if (!is.na(j_b)) {
    p <- p + annotate("text", x = LOG2_XLIM[2] - 0.08, y = y_top - 0.4,
                      label = sprintf("J = %.3f (2015)", j_b),
                      hjust = 1, vjust = 0, size = 2.2, colour = "grey40")
  }

  p
}

# ============================================================================
# 8. COUNT-DIFFERENCE PANEL (Fig 006)
# ============================================================================

make_panel_count_diff <- function(ax, net_a, net_b, show_xlab = FALSE,
                                   panel_label = NULL) {
  counts_a <- ax$load_fn(net_a)
  counts_b <- ax$load_fn(net_b)

  # Get full class list from global dist to anchor zeros
  all_classes <- ax$global_df |> dplyr::select(class) |>
    dplyr::mutate(class = as.character(class))

  make_count_row <- function(counts) {
    all_classes |>
      dplyr::left_join(counts |> dplyr::select(class, n), by = "class") |>
      dplyr::mutate(n = dplyr::coalesce(n, 0L))
  }
  cnt_a <- make_count_row(counts_a)
  cnt_b <- make_count_row(counts_b)

  delta_df <- cnt_a |>
    dplyr::rename(n_a = n) |>
    dplyr::left_join(cnt_b |> dplyr::rename(n_b = n), by = "class") |>
    dplyr::mutate(delta = n_a - n_b) |>
    ax$augment_fn() |>
    prep_ordered() |>
    dplyr::mutate(
      bar_color = dplyr::if_else(delta >= 0L, "#2c7bb6", "#d7191c"),
      ann_x     = dplyr::if_else(delta >= 0L, delta + 0.3, delta - 0.3),
      ann_hjust = dplyr::if_else(delta >= 0L, 0, 1)
    )

  j_val  <- get_j(ax$m_axis, ax$m_agg, net_a)  # J for current network
  n_lev  <- nlevels(delta_df$class_label)
  y_top  <- n_lev + 0.45

  p <- ggplot(delta_df, aes(x = delta, y = class_label, fill = bar_color)) +
    geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.5) +
    geom_col(width = 0.72, show.legend = FALSE,
             colour = "black", linewidth = 0.25) +
    geom_text(aes(x = ann_x, label = delta, hjust = ann_hjust),
              size = 2.2, colour = "grey15") +
    scale_fill_identity() +
    scale_x_continuous(
      name   = if (show_xlab) "Site count change (current − FLUXNET2015)" else NULL,
      limits = COUNT_DIFF_XLIM,
      expand = expansion(mult = 0)
    ) +
    scale_y_discrete(name = NULL) +
    labs(subtitle = ax$title) +
    base_theme +
    theme(
      plot.subtitle = element_text(size = 7.5, face = "plain", colour = "grey30"),
      axis.text.y   = element_text(size = 6.5, margin = margin(r = 5)),
      axis.text.x   = if (show_xlab) element_text(size = 7, margin = margin(t = 5))
                      else element_blank(),
      axis.ticks.x  = if (show_xlab) element_line() else element_blank(),
      axis.title.x  = element_text(size = 8)
    )

  # Panel label (top-left)
  if (!is.null(panel_label)) {
    p <- p + annotate("text", x = COUNT_DIFF_XLIM[1] + 4, y = y_top,
                      label = panel_label, hjust = 0, vjust = 0,
                      size = 3.5, fontface = "bold", colour = "grey10")
  }

  # J value for current network (top-right)
  if (!is.na(j_val)) {
    p <- p + annotate("text", x = COUNT_DIFF_XLIM[2] - 4, y = y_top,
                      label = sprintf("J = %.3f", j_val), hjust = 1, vjust = 0,
                      size = 2.5, colour = "grey20")
  }

  p
}

# ============================================================================
# 9. BUILD 2×3 GRID FIGURES (Figs 001–006)
# ============================================================================

NET_TITLES <- c(
  current_767 = "Current FLUXNET network (n=767)",
  marconi     = "Marconi Conference (n=35)",
  la_thuile   = "La Thuile (n=252)",
  fluxnet2015 = "FLUXNET2015 (n=212)"
)

make_grid_fig <- function(network, output_path,
                          mode = c("single", "overlay", "count_diff"),
                          overlay_network = NULL,
                          width_in = 10, height_in = 7, dpi = 300) {
  mode <- match.arg(mode)

  # Build 6 panels in 2×3 order: kg lulc aridity / biomass nee_iav et_median
  # Panel labels A–F in reading order
  panels <- lapply(seq_along(AXES6_KEYS), function(i) {
    k         <- AXES6_KEYS[i]
    ax        <- AXES6[[k]]
    show_xlab <- k %in% c("biomass","nee_iav","et_median")  # bottom row
    lbl       <- LETTERS[i]
    if (mode == "single") {
      make_panel_single(ax, network, metrics_df, show_xlab = show_xlab, panel_label = lbl)
    } else if (mode == "overlay") {
      make_panel_overlay(ax, network, overlay_network, metrics_df,
                         show_xlab = show_xlab, panel_label = lbl)
    } else {
      make_panel_count_diff(ax, network, overlay_network,
                            show_xlab = show_xlab, panel_label = lbl)
    }
  })

  title_str <- if (mode == "overlay") {
    paste0("Current FLUXNET (n=767) vs FLUXNET2015 (n=212) — sampling ratio per class")
  } else if (mode == "count_diff") {
    "Site count change per class: Current FLUXNET − FLUXNET2015"
  } else {
    paste0(NET_TITLES[[network]], " — sampling ratio per class")
  }

  combined <- (panels[[1]] | panels[[2]] | panels[[3]]) /
              (panels[[4]] | panels[[5]] | panels[[6]]) +
    plot_annotation(
      title   = title_str,
      caption = "X axis: sampling ratio = network fraction / global KG-land fraction, log₂ scale.\n1× = proportional representation. Bars clipped at ±5×; actual ratio annotated.",
      theme   = theme(
        plot.title    = element_text(size = 10, face = "bold"),
        plot.caption  = element_text(size = 6.5, colour = "grey40", hjust = 0),
        plot.background = element_rect(fill = "white", colour = NA)
      )
    )

  if (mode == "overlay") {
    combined <- combined + plot_layout(guides = "collect") &
      theme(legend.position = "top")
  }

  ggplot2::ggsave(output_path, combined, width = width_in, height = height_in,
                  dpi = dpi, bg = "white")
  message("Saved: ", output_path)

  # Count truncated bars for reporting
  n_trunc <- sum(vapply(AXES6_KEYS, function(k) {
    ax <- AXES6[[k]]
    counts <- ax$load_fn(network)
    df <- merge_sr(counts, ax$global_df) |> ax$augment_fn() |> prep_clip()
    sum(df$truncated, na.rm = TRUE)
  }, integer(1)))
  invisible(n_trunc)
}

# ============================================================================
# 10. TRAJECTORY FIGURES (Figs 007–010)
# ============================================================================

NET_ORDER <- c("marconi", "la_thuile", "fluxnet2015", "current_767")
NET_X     <- setNames(1:4, NET_ORDER)
NET_NSITES <- c(marconi=35L, la_thuile=252L, fluxnet2015=212L, current_767=767L)
MAX_SITES  <- max(NET_NSITES)

# Labels with n for Fig 007; without n for Fig 008-010
NET_XLABELS_N    <- c("Marconi\n(n=35)","La Thuile\n(n=252)",
                       "FLUXNET2015\n(n=212)","Current\n(n=767)")
NET_XLABELS_BARE <- c("Marconi","La Thuile","FLUXNET2015","Current")

traj_theme <- theme_minimal(base_size = 9) +
  theme(
    plot.background   = element_rect(fill = "white", colour = NA),
    panel.background  = element_rect(fill = "white", colour = NA),
    panel.border      = element_rect(colour = "black", fill = NA, linewidth = 0.4),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    axis.ticks        = element_line(colour = "black"),
    axis.ticks.length = unit(-0.15, "cm"),
    axis.text.x       = element_text(margin = margin(t = 5)),
    axis.text.y       = element_text(margin = margin(r = 5)),
    legend.background = element_rect(fill = "white", colour = NA),
    legend.position   = "right"
  )

# Build trajectory data frame for one set of axis configurations
# axis_specs: named list with keys = display labels, values = list(axis, agg)
build_traj_df <- function(axis_specs) {
  do.call(dplyr::bind_rows, lapply(names(axis_specs), function(label) {
    spec <- axis_specs[[label]]
    do.call(dplyr::bind_rows, lapply(NET_ORDER, function(net) {
      j <- get_j(spec$axis, spec$agg, net)
      data.frame(
        axis_label = label,
        network    = net,
        net_x      = NET_X[[net]],
        n_sites    = NET_NSITES[[net]],
        jaccard    = j,
        stringsAsFactors = FALSE
      )
    }))
  }))
}

# Core trajectory plot (Fig 007 style, no background bars)
make_traj_no_bars <- function(traj_df, colors, xlabels, title_str, output_path,
                               width_in = 7, height_in = 5, dpi = 300) {
  traj_df$axis_label <- factor(traj_df$axis_label, levels = names(colors))

  p <- ggplot(traj_df, aes(x = net_x, y = jaccard, colour = axis_label, group = axis_label)) +
    geom_line(linewidth = 0.7, na.rm = TRUE) +
    geom_point(size = 2.5, na.rm = TRUE) +
    scale_colour_manual(name = NULL, values = colors) +
    scale_x_continuous(breaks = 1:4, labels = xlabels,
                       limits = c(0.6, 4.4), expand = expansion(mult = 0)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25),
                       name = "Weighted Jaccard") +
    labs(title = title_str, x = NULL) +
    traj_theme +
    theme(
      legend.key.size = unit(0.5, "cm"),
      legend.text     = element_text(size = 8)
    )
  ggplot2::ggsave(output_path, p, width = width_in, height = height_in, dpi = dpi, bg = "white")
  message("Saved: ", output_path)
}

# Trajectory plot WITH count bars + secondary axis (Figs 008–010)
make_traj_with_bars <- function(traj_df, colors, xlabels, title_str, output_path,
                                 caption_str = NULL,
                                 width_in = 7, height_in = 5, dpi = 300) {
  traj_df$axis_label <- factor(traj_df$axis_label, levels = names(colors))

  bars_df <- data.frame(
    net_x  = 1:4,
    y_scaled = as.numeric(NET_NSITES[NET_ORDER]) / MAX_SITES
  )

  p <- ggplot() +
    # Background bars (drawn first, behind lines)
    geom_col(data = bars_df, aes(x = net_x, y = y_scaled),
             fill = "#d9d9d9", colour = "black", linewidth = 0.3,
             alpha = 0.4, width = 0.38, inherit.aes = FALSE) +
    # Lines
    geom_line(data = traj_df,
              aes(x = net_x, y = jaccard, colour = axis_label, group = axis_label),
              linewidth = 0.7, na.rm = TRUE) +
    # Points
    geom_point(data = traj_df,
               aes(x = net_x, y = jaccard, colour = axis_label, group = axis_label),
               size = 2.5, na.rm = TRUE) +
    scale_colour_manual(name = NULL, values = colors) +
    scale_x_continuous(breaks = 1:4, labels = xlabels,
                       limits = c(0.6, 4.4), expand = expansion(mult = 0)) +
    scale_y_continuous(
      name   = "Weighted Jaccard",
      limits = c(0, 1),
      breaks = seq(0, 1, 0.25),
      sec.axis = sec_axis(
        ~ . * MAX_SITES,
        name   = "n sites",
        breaks = c(0, 200, 400, 600, 800),
        labels = c("0","200","400","600","800")
      )
    ) +
    labs(title = title_str, x = NULL, caption = caption_str) +
    traj_theme +
    theme(
      legend.key.size    = unit(0.5, "cm"),
      legend.text        = element_text(size = 8),
      axis.title.y.right = element_text(size = 8, colour = "grey50"),
      axis.text.y.right  = element_text(size = 7, colour = "grey50"),
      axis.ticks.y.right = element_line(colour = "grey70"),
      plot.caption       = element_text(size = 6.5, colour = "grey40", hjust = 0)
    )

  ggplot2::ggsave(output_path, p, width = width_in, height = height_in, dpi = dpi, bg = "white")
  message("Saved: ", output_path)
}

# ============================================================================
# 11. EXECUTE ALL 10 FIGURES
# ============================================================================

message("\n=== Fig 001: current_767 single-network grid ===")
n_trunc_001 <- make_grid_fig(
  "current_767",
  file.path(OUTD, "fig_rep001_current.png"),
  mode = "single"
)
message("  Truncated bars: ", n_trunc_001)

message("\n=== Fig 002: marconi single-network grid ===")
n_trunc_002 <- make_grid_fig(
  "marconi",
  file.path(OUTD, "fig_rep002_marconi.png"),
  mode = "single"
)
message("  Truncated bars: ", n_trunc_002)

message("\n=== Fig 003: la_thuile single-network grid ===")
n_trunc_003 <- make_grid_fig(
  "la_thuile",
  file.path(OUTD, "fig_rep003_la_thuile.png"),
  mode = "single"
)
message("  Truncated bars: ", n_trunc_003)

message("\n=== Fig 004: fluxnet2015 single-network grid ===")
n_trunc_004 <- make_grid_fig(
  "fluxnet2015",
  file.path(OUTD, "fig_rep004_fluxnet2015.png"),
  mode = "single"
)
message("  Truncated bars: ", n_trunc_004)

message("\n=== Fig 005: overlay current_767 vs fluxnet2015 ===")
make_grid_fig(
  "current_767",
  file.path(OUTD, "fig_rep005_fluxnet2015_vs_current.png"),
  mode = "overlay",
  overlay_network = "fluxnet2015"
)

message("\n=== Fig 006: count difference current_767 minus fluxnet2015 ===")
panels_006 <- lapply(AXES6_KEYS, function(k) {
  ax        <- AXES6[[k]]
  show_xlab <- k %in% c("biomass","nee_iav","et_median")
  make_panel_count_diff(ax, "current_767", "fluxnet2015", show_xlab = show_xlab)
})
combined_006 <- (panels_006[[1]] | panels_006[[2]] | panels_006[[3]]) /
                (panels_006[[4]] | panels_006[[5]] | panels_006[[6]]) +
  plot_annotation(
    title   = "Site count change per class: Current FLUXNET − FLUXNET2015",
    caption = "Blue: current network has more sites in this class. Red: fewer sites.\nCount label = current − FLUXNET2015.",
    theme   = theme(
      plot.title   = element_text(size = 10, face = "bold"),
      plot.caption = element_text(size = 6.5, colour = "grey40", hjust = 0),
      plot.background = element_rect(fill = "white", colour = NA)
    )
  )
ggplot2::ggsave(file.path(OUTD, "fig_rep006_delta_count_2015_to_current.png"),
                combined_006, width = 10, height = 7, dpi = 300, bg = "white")
message("Saved: fig_rep006_delta_count_2015_to_current.png")

# Count-diff summary for reporting
for (k in AXES6_KEYS) {
  ax <- AXES6[[k]]
  cnts_a <- ax$load_fn("current_767")
  cnts_b <- ax$load_fn("fluxnet2015")
  all_cl <- ax$global_df |> dplyr::select(class) |> dplyr::mutate(class = as.character(class))
  cnt_a_full <- all_cl |> dplyr::left_join(cnts_a |> dplyr::select(class, n), by="class") |>
    dplyr::mutate(n = dplyr::coalesce(n, 0L))
  cnt_b_full <- all_cl |> dplyr::left_join(cnts_b |> dplyr::select(class, n), by="class") |>
    dplyr::mutate(n = dplyr::coalesce(n, 0L))
  delta_vec <- cnt_a_full$n - cnt_b_full$n
  message("  ", k, ": n_positive=", sum(delta_vec > 0),
          " n_negative=", sum(delta_vec < 0),
          " max_abs=", max(abs(delta_vec)))
}

message("\n=== Fig 007: Jaccard trajectory (6 axes, no count bars) ===")
ax_specs_6 <- list(
  "KG (13-class)"     = list(axis="koppen_beck2023",   agg="13class_twoletter"),
  "LULC (10-class)"   = list(axis="landcover_cci",     agg="high_level"),
  "Aridity (7-class)" = list(axis="aridity_unep7",     agg="unep7"),
  "Biomass (18-bin)"  = list(axis="biomass_cci_v7",    agg="18bin_hybrid"),
  "TRENDY NEE-IAV"    = list(axis="trendy_nee_iav",    agg="18bin_hybrid"),
  "TRENDY ET-median"  = list(axis="trendy_et_median",  agg="18bin_hybrid")
)
traj_df_6 <- build_traj_df(ax_specs_6)
make_traj_no_bars(
  traj_df_6, TRAJ6_COLORS,
  NET_XLABELS_N,
  "Jaccard representativeness trajectory — 6 default axes",
  file.path(OUTD, "fig_rep007_jaccard_trajectory.png")
)

message("\n=== Fig 008: Jaccard trajectory (6 axes, with count bars) ===")
make_traj_with_bars(
  traj_df_6, TRAJ6_COLORS,
  NET_XLABELS_BARE,
  "Jaccard representativeness trajectory with network size — 6 default axes",
  file.path(OUTD, "fig_rep008_jaccard_trajectory_with_counts.png")
)

message("\n=== Fig 009: Jaccard trajectory (3 native-resolution axes, with count bars) ===")
ax_specs_3 <- list(
  "KG (30-class)"     = list(axis="koppen_beck2023", agg="30class"),
  "LULC (37-class)"   = list(axis="landcover_cci",   agg="native"),
  "Aridity (7-class)" = list(axis="aridity_unep7",   agg="unep7")
)
traj_df_3 <- build_traj_df(ax_specs_3)
make_traj_with_bars(
  traj_df_3, TRAJ3_COLORS,
  NET_XLABELS_BARE,
  "Jaccard trajectory at finer resolution — 3 categorical axes",
  file.path(OUTD, "fig_rep009_jaccard_trajectory_native.png")
)

message("\n=== Fig 010: Jaccard trajectory (5 continuous axes at 30-bin, with count bars) ===")
ax_specs_5 <- list(
  "Biomass (30-bin)"  = list(axis="biomass_cci_v7",    agg="30bin_hybrid"),
  "TRENDY NEE-IAV"    = list(axis="trendy_nee_iav",    agg="30bin_hybrid"),
  "TRENDY NEE-median" = list(axis="trendy_nee_median", agg="30bin_hybrid"),
  "TRENDY ET-IAV"     = list(axis="trendy_et_iav",     agg="30bin_hybrid"),
  "TRENDY ET-median"  = list(axis="trendy_et_median",  agg="30bin_hybrid")
)
traj_df_5 <- build_traj_df(ax_specs_5)
make_traj_with_bars(
  traj_df_5, TRAJ5_COLORS,
  NET_XLABELS_BARE,
  "Jaccard trajectory at 30-bin hybrid — 5 continuous axes",
  file.path(OUTD, "fig_rep010_jaccard_trajectory_30bin_continuous.png")
)

message("\n=== All 10 figures complete ===")

# ============================================================================
# 12. AGGREGATION SENSITIVITY FIGURES (Figs 011–018)
#
# One figure per axis; each shows weighted Jaccard across the four network
# generations at every aggregation resolution available for that axis.
# Single-hue sequential palette: lightest = coarsest, darkest = finest.
# Background count bars and secondary y-axis as in Figs 008-010.
# ============================================================================

message("\n=== Fig 011: KG aggregation sensitivity ===")
ax_specs_kg_agg <- list(
  "30-class" = list(axis = "koppen_beck2023", agg = "30class"),
  "13-class" = list(axis = "koppen_beck2023", agg = "13class_twoletter"),
  "5-class"  = list(axis = "koppen_beck2023", agg = "5class")
)
make_traj_with_bars(
  build_traj_df(ax_specs_kg_agg), KG_AGG_COLORS, NET_XLABELS_BARE,
  "Jaccard aggregation sensitivity — Beck 2023 KG climate zones",
  file.path(OUTD, "fig_rep011_jaccard_kg_aggregation.png")
)

message("\n=== Fig 012: LULC aggregation sensitivity ===")
ax_specs_lulc_agg <- list(
  "37-class (native)"     = list(axis = "landcover_cci", agg = "native"),
  "22-class (level 2)"    = list(axis = "landcover_cci", agg = "level2"),
  "10-class (high level)" = list(axis = "landcover_cci", agg = "high_level")
)
make_traj_with_bars(
  build_traj_df(ax_specs_lulc_agg), LULC_AGG_COLORS, NET_XLABELS_BARE,
  "Jaccard aggregation sensitivity — ESA CCI land cover",
  file.path(OUTD, "fig_rep012_jaccard_lulc_aggregation.png")
)

message("\n=== Fig 013: Aridity aggregation sensitivity ===")
ax_specs_arid_agg <- list(
  "7-class" = list(axis = "aridity_unep7", agg = "unep7"),
  "5-class" = list(axis = "aridity_unep5", agg = "unep5")
)
make_traj_with_bars(
  build_traj_df(ax_specs_arid_agg), ARID_AGG_COLORS, NET_XLABELS_BARE,
  "Jaccard aggregation sensitivity — CGIAR aridity (UNEP)",
  file.path(OUTD, "fig_rep013_jaccard_aridity_aggregation.png")
)

message("\n=== Fig 014: Biomass bin-count sensitivity ===")
ax_specs_bio_agg <- list(
  "30-bin" = list(axis = "biomass_cci_v7", agg = "30bin_hybrid"),
  "20-bin" = list(axis = "biomass_cci_v7", agg = "20bin_hybrid"),
  "18-bin" = list(axis = "biomass_cci_v7", agg = "18bin_hybrid"),
  "12-bin" = list(axis = "biomass_cci_v7", agg = "12bin_hybrid"),
  "7-bin"  = list(axis = "biomass_cci_v7", agg = "7bin_hybrid")
)
make_traj_with_bars(
  build_traj_df(ax_specs_bio_agg), BIO_AGG_COLORS, NET_XLABELS_BARE,
  "Jaccard bin-count sensitivity — ESA CCI Biomass (hybrid bins)",
  file.path(OUTD, "fig_rep014_jaccard_biomass_aggregation.png")
)

message("\n=== Fig 015: NEE-IAV bin-count sensitivity ===")
ax_specs_neeiav_agg <- list(
  "30-bin" = list(axis = "trendy_nee_iav", agg = "30bin_hybrid"),
  "20-bin" = list(axis = "trendy_nee_iav", agg = "20bin_hybrid"),
  "18-bin" = list(axis = "trendy_nee_iav", agg = "18bin_hybrid"),
  "12-bin" = list(axis = "trendy_nee_iav", agg = "12bin_hybrid"),
  "7-bin"  = list(axis = "trendy_nee_iav", agg = "7bin_hybrid")
)
make_traj_with_bars(
  build_traj_df(ax_specs_neeiav_agg), NEEIAV_AGG_COLORS, NET_XLABELS_BARE,
  "Jaccard bin-count sensitivity — TRENDY NEE interannual variability",
  file.path(OUTD, "fig_rep015_jaccard_nee_iav_aggregation.png")
)

message("\n=== Fig 016: NEE-median bin-count sensitivity ===")
ax_specs_neemed_agg <- list(
  "30-bin" = list(axis = "trendy_nee_median", agg = "30bin_hybrid"),
  "20-bin" = list(axis = "trendy_nee_median", agg = "20bin_hybrid"),
  "18-bin" = list(axis = "trendy_nee_median", agg = "18bin_hybrid"),
  "12-bin" = list(axis = "trendy_nee_median", agg = "12bin_hybrid"),
  "7-bin"  = list(axis = "trendy_nee_median", agg = "7bin_hybrid")
)
make_traj_with_bars(
  build_traj_df(ax_specs_neemed_agg), NEEMED_AGG_COLORS, NET_XLABELS_BARE,
  "Jaccard bin-count sensitivity — TRENDY NEE long-run median",
  file.path(OUTD, "fig_rep016_jaccard_nee_median_aggregation.png")
)

message("\n=== Fig 017: ET-IAV bin-count sensitivity ===")
ax_specs_etiav_agg <- list(
  "30-bin" = list(axis = "trendy_et_iav", agg = "30bin_hybrid"),
  "20-bin" = list(axis = "trendy_et_iav", agg = "20bin_hybrid"),
  "18-bin" = list(axis = "trendy_et_iav", agg = "18bin_hybrid"),
  "12-bin" = list(axis = "trendy_et_iav", agg = "12bin_hybrid"),
  "7-bin"  = list(axis = "trendy_et_iav", agg = "7bin_hybrid")
)
make_traj_with_bars(
  build_traj_df(ax_specs_etiav_agg), ETIAV_AGG_COLORS, NET_XLABELS_BARE,
  "Jaccard bin-count sensitivity — TRENDY ET interannual variability",
  file.path(OUTD, "fig_rep017_jaccard_et_iav_aggregation.png")
)

message("\n=== Fig 018: ET-median bin-count sensitivity ===")
ax_specs_etmed_agg <- list(
  "30-bin" = list(axis = "trendy_et_median", agg = "30bin_hybrid"),
  "20-bin" = list(axis = "trendy_et_median", agg = "20bin_hybrid"),
  "18-bin" = list(axis = "trendy_et_median", agg = "18bin_hybrid"),
  "12-bin" = list(axis = "trendy_et_median", agg = "12bin_hybrid"),
  "7-bin"  = list(axis = "trendy_et_median", agg = "7bin_hybrid")
)
# ET-median: at the current_767 point, 18-bin J (0.433) < 20-bin J (0.447) due to
# histogram ceiling collapse at 1000 mm yr⁻¹. Non-monotonicity is visible at
# figure scale (~0.014 gap); noted in caption.
make_traj_with_bars(
  build_traj_df(ax_specs_etmed_agg), ETMED_AGG_COLORS, NET_XLABELS_BARE,
  "Jaccard bin-count sensitivity — TRENDY ET long-run median",
  file.path(OUTD, "fig_rep018_jaccard_et_median_aggregation.png"),
  caption_str = paste0(
    "Note: at Current network, 18-bin J (0.433) < 20-bin J (0.447). ",
    "Artefact of histogram ceiling at 1000 mm yr⁻¹ causing one collapsed ",
    "breakpoint at 18-bin; does not affect other networks or axes."
  )
)

message("\n=== All 18 figures complete ===")
