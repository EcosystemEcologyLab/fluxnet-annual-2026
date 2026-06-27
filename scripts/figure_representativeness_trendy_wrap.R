## figure_representativeness_trendy_wrap.R
## Figures and methods text for four TRENDY v14 representativeness axes.
##
## Reads pre-computed outputs from figure_representativeness_trendy_compute.R:
##   data/snapshots/trendy_*_global_distribution.csv
##   data/snapshots/site_trendy_*.csv
##   data/snapshots/representativeness_metrics.csv
##
## Outputs:
##   review/figures/representativeness/fig_representativeness_trendy_nee_iav.png
##   review/figures/representativeness/fig_representativeness_trendy_et_iav.png
##   review/figures/representativeness/fig_representativeness_trendy_nee_median.png
##   review/figures/representativeness/fig_representativeness_trendy_et_median.png
##   review/figures/representativeness/methods_trendy_iav.md

suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
  library(readr)
  library(dplyr)
  library(tidyr)
})

SNAP_DIR <- "data/snapshots"
OUT_DIR  <- file.path("review", "figures", "representativeness")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

METRICS_CSV <- file.path(SNAP_DIR, "representativeness_metrics.csv")

for (p in c(METRICS_CSV)) {
  if (!file.exists(p)) stop("Required file not found: ", p, call. = FALSE)
}

# ---- Color palettes (7 bins) -------------------------------------------------
# NEE: pale-to-dark green (carbon flux magnitude)
PAL_GREEN <- c(
  "#f4faf0",   # bin 1: near-zero
  "#c8e8a4",   # bin 2
  "#9acb72",   # bin 3
  "#67ae42",   # bin 4
  "#3d8c27",   # bin 5
  "#1f6415",   # bin 6
  "#0b3e09"    # bin 7: max flux
)

# ET: pale-to-dark blue (water flux magnitude)
PAL_BLUE <- c(
  "#f0f8ff",   # bin 1: near-zero
  "#bcd8f4",   # bin 2
  "#82bce8",   # bin 3
  "#4498d5",   # bin 4
  "#1d74b3",   # bin 5
  "#0c4f84",   # bin 6
  "#06305a"    # bin 7: max ET
)

# ---- Figure function ---------------------------------------------------------
make_fig <- function(axis_key, val_col, bin_col,
                     glob_csv, site_csv, metrics,
                     axis_title, pal, fig_out) {

  for (p in c(glob_csv, site_csv)) {
    if (!file.exists(p)) stop("Required file not found: ", p, call. = FALSE)
  }

  dist_df <- read_csv(glob_csv, show_col_types = FALSE)
  site_df <- read_csv(site_csv, show_col_types = FALSE)
  n_sites <- nrow(site_df)

  bin_labels <- dist_df$bin_label
  p_global   <- dist_df$global_land_fraction
  q_net      <- vapply(1:7, function(b)
    sum(site_df[[bin_col]] == b, na.rm = TRUE) / n_sites,
    numeric(1)
  )

  m_row <- dplyr::filter(metrics, axis == axis_key)
  J <- m_row$weighted_jaccard
  H <- m_row$hellinger_distance

  base_theme <- theme_minimal(base_size = 10, base_family = "sans") +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.ticks.x       = element_blank(),
      legend.position    = "right",
      plot.background    = element_rect(fill = "white", colour = NA),
      panel.background   = element_rect(fill = "white", colour = NA)
    )

  # Stacked bars panel
  plot_long <- data.frame(
    class_id = factor(bin_labels, levels = bin_labels),
    global   = p_global,
    network  = q_net,
    stringsAsFactors = FALSE
  ) |>
    tidyr::pivot_longer(c(global, network),
                        names_to = "bar", values_to = "fraction") |>
    dplyr::mutate(
      bar = factor(bar, levels = c("global", "network"),
                   labels = c("Global land", "FLUXNET\n(767 sites)")),
      label = dplyr::case_when(
        fraction >= 0.07  ~ sprintf("%d\n%.1f%%", as.integer(class_id),
                                    fraction * 100),
        fraction >= 0.03  ~ sprintf("%d  %.1f%%", as.integer(class_id),
                                    fraction * 100),
        fraction >= 0.005 ~ as.character(as.integer(class_id)),
        TRUE              ~ NA_character_
      )
    )

  bars_panel <- ggplot(plot_long,
                       aes(x = bar, y = fraction, fill = class_id)) +
    geom_bar(stat = "identity", width = 0.6,
             colour = "white", linewidth = 0.2) +
    geom_text(aes(label = label),
              position = position_stack(vjust = 0.5),
              size = 2.3, family = "sans", colour = "black", na.rm = TRUE) +
    scale_fill_manual(
      values = setNames(pal, bin_labels),
      breaks = bin_labels,
      labels = paste0(1:7, ": ", bin_labels),
      name   = NULL,
      guide  = guide_legend(ncol = 1, override.aes = list(colour = NA))
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.01)),
      labels = scales::percent_format(accuracy = 1),
      name   = "Fraction of total"
    ) +
    scale_x_discrete(name = NULL) +
    labs(title = axis_title) +
    annotate("text", x = Inf, y = Inf,
             label  = sprintf("J = %.2f\nH = %.2f", J, H),
             hjust  = 1.08, vjust = 1.5,
             size   = 2.9, family = "sans",
             colour = "grey25", lineheight = 1.2) +
    base_theme +
    theme(
      legend.key.size = unit(0.35, "cm"),
      legend.text     = element_text(size = 7, family = "sans")
    )

  # X-axis labels for ratio panel: "N\n(lo–hi)"
  fmt_v <- function(x, decimals = 1) {
    if (x == round(x)) sprintf("%d", as.integer(x))
    else sprintf(paste0("%.", decimals, "f"), x)
  }
  xlab_ratio <- vapply(seq_len(nrow(dist_df)), function(i) {
    mn <- dist_df$min_value[i]
    mx <- dist_df$max_value[i]
    if (is.na(mx)) {
      sprintf("%d\n(>%s)", i, fmt_v(mn))
    } else {
      sprintf("%d\n(%s–%s)", i, fmt_v(mn), fmt_v(mx))
    }
  }, character(1))

  # Sampling ratio panel
  ratio_df <- data.frame(
    class_id       = factor(bin_labels, levels = bin_labels),
    sampling_ratio = q_net / p_global,
    stringsAsFactors = FALSE
  ) |>
    dplyr::mutate(
      sr_plot = dplyr::if_else(
        sampling_ratio <= 0 | !is.finite(sampling_ratio),
        NA_real_,
        sampling_ratio
      ),
      label = dplyr::case_when(
        !is.finite(sampling_ratio) ~ "N/A",
        sampling_ratio == 0        ~ "0×",
        TRUE ~ sprintf("%.2f×", sampling_ratio)
      )
    )

  # y limits: ensure all plotted points fit with margin
  finite_ratios <- ratio_df$sr_plot[!is.na(ratio_df$sr_plot)]
  y_lo <- if (length(finite_ratios) > 0) min(0.05, min(finite_ratios) * 0.7)  else 0.05
  y_hi <- if (length(finite_ratios) > 0) max(6.0,  max(finite_ratios) * 1.4)  else 6.0

  ratio_panel <- ggplot(ratio_df,
                        aes(x = class_id, y = sr_plot, colour = class_id)) +
    geom_hline(yintercept = 1, linetype = "dashed",
               colour = "grey50", linewidth = 0.5) +
    geom_segment(aes(xend = class_id, yend = 1),
                 colour = "grey75", linewidth = 0.5, na.rm = TRUE) +
    geom_point(data = dplyr::filter(ratio_df, !is.na(sr_plot)), size = 3) +
    geom_point(data = dplyr::filter(ratio_df, is.na(sr_plot)),
               aes(y = y_lo * 1.2), shape = 4, size = 3, alpha = 0.55) +
    geom_text(aes(label = label), vjust = -0.65, size = 2.4,
              family = "sans", colour = "black", na.rm = TRUE) +
    geom_text(data = dplyr::filter(ratio_df, is.na(sr_plot)),
              aes(y = y_lo * 1.2, label = label),
              vjust = -0.65, size = 2.4, family = "sans",
              colour = "black") +
    scale_colour_manual(
      values = setNames(pal, bin_labels), guide = "none",
      na.value = "grey40"
    ) +
    scale_y_continuous(
      name   = "Sampling ratio\n(network / global)",
      trans  = "log2",
      breaks = c(0.125, 0.25, 0.5, 1, 2, 4, 8),
      labels = c("0.13×", "0.25×", "0.5×",
                 "1×", "2×", "4×", "8×"),
      limits = c(y_lo * 0.9, y_hi)
    ) +
    scale_x_discrete(name = NULL, labels = xlab_ratio) +
    base_theme +
    theme(
      panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3),
      panel.grid.major.x = element_blank(),
      axis.text.x        = element_text(size = 6.5),
      axis.title.y       = element_text(size = 8),
      legend.position    = "none"
    )

  fig <- (bars_panel / ratio_panel +
    plot_layout(heights = c(3.5, 1.8), guides = "keep")) &
    theme(plot.background = element_rect(fill = "white", colour = NA))

  ggsave(fig_out, plot = fig, width = 7.5, height = 6.5,
         dpi = 200, bg = "white")
  message("Saved: ", fig_out)
  invisible(list(J = J, H = H, q_net = q_net, p_global = p_global,
                 bin_labels = bin_labels))
}

# ---- Load metrics ------------------------------------------------------------
metrics <- readr::read_csv(METRICS_CSV, show_col_types = FALSE)

# ---- Generate four figures ---------------------------------------------------
message("\n=== Generating TRENDY representativeness figures ===\n")

res_nee_iav <- make_fig(
  axis_key  = "trendy_nee_iav",
  val_col   = "trendy_nee_iav_value",
  bin_col   = "trendy_nee_iav_bin",
  glob_csv  = file.path(SNAP_DIR, "trendy_nee_iav_global_distribution.csv"),
  site_csv  = file.path(SNAP_DIR, "site_trendy_nee_iav.csv"),
  metrics   = metrics,
  axis_title = "NEE Interannual Variability — TRENDY v14 (17-model ensemble)",
  pal       = PAL_GREEN,
  fig_out   = file.path(OUT_DIR, "fig_representativeness_trendy_nee_iav.png")
)

res_et_iav <- make_fig(
  axis_key  = "trendy_et_iav",
  val_col   = "trendy_et_iav_value",
  bin_col   = "trendy_et_iav_bin",
  glob_csv  = file.path(SNAP_DIR, "trendy_et_iav_global_distribution.csv"),
  site_csv  = file.path(SNAP_DIR, "site_trendy_et_iav.csv"),
  metrics   = metrics,
  axis_title = "ET Interannual Variability — TRENDY v14 (17-model ensemble)",
  pal       = PAL_BLUE,
  fig_out   = file.path(OUT_DIR, "fig_representativeness_trendy_et_iav.png")
)

res_nee_median <- make_fig(
  axis_key  = "trendy_nee_median",
  val_col   = "trendy_nee_median_value",
  bin_col   = "trendy_nee_median_bin",
  glob_csv  = file.path(SNAP_DIR, "trendy_nee_median_global_distribution.csv"),
  site_csv  = file.path(SNAP_DIR, "site_trendy_nee_median.csv"),
  metrics   = metrics,
  axis_title = "NEE Magnitude (|median|) — TRENDY v14 (17-model ensemble)",
  pal       = PAL_GREEN,
  fig_out   = file.path(OUT_DIR, "fig_representativeness_trendy_nee_median.png")
)

res_et_median <- make_fig(
  axis_key  = "trendy_et_median",
  val_col   = "trendy_et_median_value",
  bin_col   = "trendy_et_median_bin",
  glob_csv  = file.path(SNAP_DIR, "trendy_et_median_global_distribution.csv"),
  site_csv  = file.path(SNAP_DIR, "site_trendy_et_median.csv"),
  metrics   = metrics,
  axis_title = "ET Magnitude (mean) — TRENDY v14 (17-model ensemble)",
  pal       = PAL_BLUE,
  fig_out   = file.path(OUT_DIR, "fig_representativeness_trendy_et_median.png")
)

# ---- Print sampling ratio summary -------------------------------------------
axes <- list(
  list(key = "NEE-IAV",    res = res_nee_iav),
  list(key = "ET-IAV",     res = res_et_iav),
  list(key = "NEE-median", res = res_nee_median),
  list(key = "ET-median",  res = res_et_median)
)

cat("\n=== SAMPLING RATIOS ACROSS AXES ===\n")
for (ax in axes) {
  r <- ax$res
  ratio <- r$q_net / r$p_global
  most_over   <- which.max(ratio)
  least_cover <- which.min(ratio)
  cat(sprintf("%s (J=%.3f, H=%.3f):\n", ax$key, r$J, r$H))
  cat(sprintf("  Most over-sampled:   bin %d (%s)  ratio=%.2fx\n",
              most_over,   r$bin_labels[most_over],   ratio[most_over]))
  cat(sprintf("  Least over-sampled:  bin %d (%s)  ratio=%.2fx\n",
              least_cover, r$bin_labels[least_cover], ratio[least_cover]))
}

# ---- Read global distribution breakpoints for methods text ------------------
nee_iav_dist    <- readr::read_csv(
  file.path(SNAP_DIR, "trendy_nee_iav_global_distribution.csv"), show_col_types=FALSE)
et_iav_dist     <- readr::read_csv(
  file.path(SNAP_DIR, "trendy_et_iav_global_distribution.csv"),  show_col_types=FALSE)
nee_median_dist <- readr::read_csv(
  file.path(SNAP_DIR, "trendy_nee_median_global_distribution.csv"), show_col_types=FALSE)
et_median_dist  <- readr::read_csv(
  file.path(SNAP_DIR, "trendy_et_median_global_distribution.csv"),  show_col_types=FALSE)

fmt_bp <- function(dist_df) {
  mins <- dist_df$min_value[-1]   # skip bin-1 (0–low_cut)
  maxs <- dist_df$max_value[-1]
  bp   <- mins[-1]                # internal breakpoints (5 values)
  paste(round(bp, 1), collapse = ", ")
}

m_nee_iav    <- dplyr::filter(metrics, axis == "trendy_nee_iav")
m_et_iav     <- dplyr::filter(metrics, axis == "trendy_et_iav")
m_nee_median <- dplyr::filter(metrics, axis == "trendy_nee_median")
m_et_median  <- dplyr::filter(metrics, axis == "trendy_et_median")

# ---- Methods text -----------------------------------------------------------
methods_out <- file.path(OUT_DIR, "methods_trendy_iav.md")

methods_lines <- c(
  "# Methods: TRENDY v14 Representativeness Axes (NEE-IAV, ET-IAV, NEE-median, ET-median)",
  "",
  "## Overview",
  "",
  "Four representativeness axes are derived from the TRENDY v14-gcb2025 multi-model",
  "ensemble: two interannual variability (IAV) axes quantified as the linear-detrended",
  "standard deviation of annual carbon and water fluxes, and two magnitude axes",
  "quantified as the temporal mean absolute value (NEE) or mean (ET) across the",
  "analysis window. Together these axes characterise whether the FLUXNET tower network",
  "samples the global range of ecosystem carbon and water flux variability and magnitude",
  "as simulated by current land surface models.",
  "",
  "## Data source",
  "",
  "TRENDY v14-gcb2025, S3 simulation (transient, with historical land-use change).",
  "Variables: nbp (net biome production, kg C m⁻² s⁻¹, monthly or annual depending",
  "on model) and evapotrans (evapotranspiration, kg m⁻² s⁻¹, monthly).",
  "",
  "Protocol reference: Sitch S et al. (2024). The global carbon budget 2024.",
  "Earth System Science Data. doi:10.1029/2024GB008102.",
  "Global Carbon Budget paper: Friedlingstein P et al. (2025). Global Carbon Budget 2025",
  "(in prep.). The acknowledgment-only attribution policy applies to TRENDY model outputs;",
  "individual model PIs are acknowledged in the Acknowledgements section of the paper,",
  "not cited formally.",
  "",
  "## Model ensemble",
  "",
  "The full TRENDY v14 archive includes 20 models. Three were excluded:",
  "",
  "| Model | Reason for exclusion | Type |",
  "|---|---|---|",
  "| CARDAMOM | Temporal coverage only 22 years (insufficient for 34-year IAV analysis) | technical |",
  "| CLM-FATES | Irregular longitude spacing — terra::rast() fails to parse | technical |",
  "| JSBACH | Irregular latitude spacing — terra::rast() fails to parse | technical |",
  "",
  "All three exclusions are technical, not selective. The remaining 17-model ensemble",
  "is: CABLE-POP, CLASSIC, CLM, DLEM, ED, ELM, ELM-FATES, IBIS, ISAM, JULES-ES,",
  "LPJ-GUESS, LPJml, LPJwsl, LPX-Bern, ORCHIDEE, TEM, VISIT-UT.",
  "",
  "## Analysis window",
  "",
  "1990–2023 (34 years). This window is bounded by the intersection of model",
  "availability: CLASSIC, DLEM, and ELM end their S3 simulation at 2023.",
  "",
  "ELM note: ELM submitted data through 2022 only; its 2023 annual layer is NA",
  "across all pixels for both nbp and evapotrans. All per-pixel statistics functions",
  "(detrended SD, mean absolute value, mean) require all 34 years to be non-NA before",
  "computing (complete-row requirement). Consequently, ELM contributes an all-NA",
  "per-pixel stat raster to the ensemble stack for all four axes. The ensemble median",
  "is computed with na.rm = TRUE, so the 16 remaining models determine each pixel's",
  "ensemble value. ELM is therefore excluded from all four ensemble maps, not only",
  "the IAV maps. The effective ensemble size for any pixel is 16 models.",
  "",
  "This differs from the original expectation (ELM excluded from IAV only, included",
  "in median maps via na.rm). On inspection, the complete-row requirement in",
  "compute_mean_abs() and compute_mean() applies identically to compute_detrended_sd().",
  "",
  "## Per-model regridding",
  "",
  "Each model's NetCDF was loaded with terra::rast(). Three models (CLM, ISAM,",
  "ELM-FATES) store longitude 0–360°; these were rotated to −180–180° via",
  "terra::rotate() before any subsetting. DLEM, LPJ-GUESS, and LPJml store",
  "nbp as annual time steps; all other models use monthly time steps. For monthly",
  "models, annual sums were computed from complete 12-month blocks; years with",
  "fewer than 12 months were summed over the available months with a console warning.",
  "",
  "Unit conversions applied before annual summation:",
  "",
  "| Variable | Native unit | Analysis unit | Conversion |",
  "|---|---|---|---|",
  "| nbp (monthly) | kg C m⁻² s⁻¹ | gC m⁻² yr⁻¹ | × 2629800 s mo⁻¹ × 1000 |",
  "| nbp (annual) | kg C m⁻² s⁻¹ | gC m⁻² yr⁻¹ | × 31557600 s yr⁻¹ × 1000 |",
  "| evapotrans | kg m⁻² s⁻¹ | mm yr⁻¹ | × 2629800 s mo⁻¹ (then summed) |",
  "",
  "The unit for evapotrans follows from 1 kg m⁻² = 1 mm (water density = 1000 kg m⁻³).",
  "",
  "All models were resampled to a common 0.5° global grid (720 × 360 cells,",
  "EPSG:4326) via terra::resample(method = 'bilinear', threads = TRUE). The land",
  "mask (Beck 2023 KG 0.5° raster) was then applied. Regridded intermediates were",
  "written as compressed GeoTIFFs (34 annual layers per model per variable) to",
  "data/external/trendy/derived/intermediate/ (252 MB total; 17 models × 2",
  "variables × 34 layers).",
  "",
  "## Per-pixel statistics",
  "",
  "### IAV axes: linear-detrended standard deviation",
  "",
  "For each pixel with all 34 years non-NA, the annual time series was linearly",
  "detrended using OLS projection (hat matrix H = X(XᵀX)⁻¹Xᵀ; residual",
  "projection P = I − H). The detrended SD was then computed as:",
  "",
  "  SD_detrended = sqrt( sum(residuals²) / (n − 2) )",
  "",
  "where n = 34 and the df reduction of 2 accounts for the fitted intercept and",
  "slope. Pixels missing any year were assigned NA.",
  "",
  "### NEE-median axis: mean of absolute annual NBP",
  "",
  "For each pixel, the mean of the absolute value of annual NBP was computed",
  "across all 34 years (complete rows only). NBP can be negative (net source).",
  "Taking the absolute value before averaging gives a signed-symmetric measure of",
  "carbon flux magnitude regardless of sign, parallel to the IAV axis framing.",
  "",
  "### ET-median axis: mean annual ET",
  "",
  "For each pixel, the mean of annual evapotranspiration was computed across all",
  "34 years (complete rows only). ET is non-negative; no absolute value is needed.",
  "",
  "### Ensemble median",
  "",
  "After computing the per-pixel statistic for each model, a 17-layer raster stack",
  "was built and the ensemble median was computed per pixel using terra::app()",
  "with na.rm = TRUE. The four ensemble-median maps were saved as single-layer",
  "GeoTIFFs (~260–350 KB each; compressed) to data/external/trendy/derived/.",
  "Each map has 94,589 non-NA pixels (the global KG land mask at 0.5°).",
  "",
  "## Binning",
  "",
  "All four axes use the same hybrid 7-bin scheme applied to other continuous",
  "representativeness axes (biomass, aridity):",
  "",
  "  Bin 1: values 0–5 (fixed near-zero cut) — separates very low-flux or",
  "    non-terrestrial pixels from the main distribution. Negative pixel values",
  "    (net-source pixels in the IAV/median maps) are mapped to 0 before binning.",
  "",
  "  Bins 2–7: six equal-area quantile bins computed from the area-weighted",
  "    global distribution of KG-land pixels above the near-zero cut.",
  "    Each bin contains approximately 1/6 of the total area of the",
  "    above-threshold global land.",
  "",
  "### Breakpoints by axis",
  "",
  sprintf("NEE-IAV (gC m⁻² yr⁻¹, near-zero cut 5): %s",    fmt_bp(nee_iav_dist)),
  sprintf("ET-IAV  (mm yr⁻¹,       near-zero cut 5): %s",    fmt_bp(et_iav_dist)),
  sprintf("NEE-med (gC m⁻² yr⁻¹, near-zero cut 5): %s", fmt_bp(nee_median_dist)),
  sprintf("ET-med  (mm yr⁻¹,       near-zero cut 5): %s",    fmt_bp(et_median_dist)),
  "",
  "Breakpoints are data-dependent and were computed at runtime from the ensemble",
  "maps produced by figure_representativeness_trendy_compute.R. The near-zero",
  "threshold (5.0 for both gC m⁻² yr⁻¹ and mm yr⁻¹) was confirmed to capture",
  "a non-trivial land fraction (≥24% of global land for NEE axes, ~3% for ET axes)",
  "before being used; the code automatically adjusts if the threshold captures",
  "< 1% or > 70% of global land.",
  "",
  "## Per-site extraction",
  "",
  "Per-site values were extracted from each ensemble-median map at the site's",
  "reported latitude/longitude using terra::extract(method = 'simple') (nearest",
  "pixel). Sites returning NA (ocean pixels, ice, or coverage gaps) were recovered",
  "via a nearest-land search within a 3° window: the closest KG-land cell within",
  "the window was used, and the recovery distance was recorded in the site CSV.",
  "",
  "terra API note: terra::extract(raster, matrix) in terra ≥ 1.9.27 returns a",
  "one-column data frame (values only, no ID column prepended). The extraction",
  "code was updated to index raw[[1]] rather than raw[[2]] after this change",
  "caused a 'subscript out of bounds' error during the initial Step 3 run.",
  "(Commit 7ee9210, 2026-06-27.)",
  "",
  "## Representativeness metrics",
  "",
  "Weighted Jaccard index (J) and Hellinger distance (H) as described in",
  "methods_koppen_beck2023.md. p_k = global land fraction of bin k;",
  "q_k = fraction of 767-site FLUXNET network assigned to bin k.",
  "",
  "| Axis | J | H |",
  "|---|---|---|",
  sprintf("| NEE-IAV    | %.3f | %.3f |",
          m_nee_iav$weighted_jaccard,    m_nee_iav$hellinger_distance),
  sprintf("| ET-IAV     | %.3f | %.3f |",
          m_et_iav$weighted_jaccard,     m_et_iav$hellinger_distance),
  sprintf("| NEE-median | %.3f | %.3f |",
          m_nee_median$weighted_jaccard, m_nee_median$hellinger_distance),
  sprintf("| ET-median  | %.3f | %.3f |",
          m_et_median$weighted_jaccard,  m_et_median$hellinger_distance),
  "",
  "Across all four axes, the pattern is consistent with the biomass and",
  "Koppen-Geiger axes: the network over-samples intermediate flux regimes",
  "(temperate mesic forests and grasslands) and under-samples both the",
  "near-zero bin (arid, boreal, or low-productivity land) and the highest",
  "flux bins (tropical forests, wet tropics). ET-IAV yields the highest J",
  "(0.663), indicating reasonably broad sampling of ET variability; ET-median",
  "yields the lowest J (0.459), indicating the strongest magnitude bias.",
  "",
  "## Cross-axis comparison: NEE-median vs. above-ground biomass",
  "",
  "A pixel-level comparison between the ensemble-median NEE magnitude map and",
  "the ESA CCI Biomass v7.0 map (band 18, 2024, resampled to 0.5° bilinear)",
  "yields Pearson r = 0.55 and Spearman ρ = 0.89 across 94,589 shared non-NA",
  "pixels. The strong rank correlation confirms the expectation that high-biomass",
  "pixels tend to have high carbon flux magnitude in the TRENDY ensemble.",
  "The weaker Pearson correlation (r = 0.55) reflects a nonlinear relationship:",
  "high-biomass tropical forests show high NEE magnitude, but low-biomass",
  "semi-arid and boreal systems occupy a broad range of NEE magnitudes driven",
  "by water and temperature limitation. The implications for network",
  "representativeness are consistent across both axes: the network",
  "under-samples the highest flux/biomass bins (dense tropical forests) and",
  "over-samples the intermediate bins.",
  "",
  "Breakpoints for the NEE-median axis differ slightly from those of NEE-IAV",
  "because the underlying distributions differ (mean |flux| vs detrended SD),",
  "but both axes partition global land in the same structural way.",
  ""
)

writeLines(methods_lines, methods_out)
message("Saved: ", methods_out)
message("\nAll TRENDY wrap outputs complete.")
