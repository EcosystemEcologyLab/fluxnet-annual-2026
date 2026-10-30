## 06_fig2.R — GSL figure rebuild, Figure 2: where sensitivity sits across
## all 62 sites. Main run only. Reads only Stage 1/2 tables and data.

source("GSL_definition/code/00_config.R")
source("GSL_definition/code/fig_helpers.R")
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(ggnewscale)
})

derived_site <- read_csv(file.path(GSL_ROOT, "tables", "derived_site.csv"), show_col_types = FALSE) |>
  filter(gpp_variant == "main", neg_gpp_variant == "main") |>
  left_join(GSL_SITES |> select(site_id, group), by = "site_id")

fine_grid <- readRDS(file.path(GSL_ROOT, "data", "fine_grid.rds")) |>
  filter(gpp_variant == "main", neg_gpp_variant == "main", pct_of_P99 >= 0.5, pct_of_P99 <= 35)

## Row order: ascending skewness, top of plot = highest skewness (so the
## heatmap reads low-to-high skewness bottom-to-top, matching how sites are
## listed low/middle/high third in the text).
site_order <- derived_site |> arrange(skewness) |> pull(site_id)
derived_site$site_id <- factor(derived_site$site_id, levels = site_order)
fine_grid$site_id <- factor(fine_grid$site_id, levels = site_order)
stopifnot(nlevels(derived_site$site_id) == 62L)

## ---- Log-uniform binning, shared by the heatmap (panel b) and the median
## curve (panel d) -- fixes the vertical-stripe artifact at low thresholds
## (the fine grid is uniform in LINEAR %P99, 0.01 steps; on a log-x axis
## that packs far too many tiles into very few screen pixels at the low
## end, aliasing into stripes) and doubles as the "smooth the bottom panel"
## revision (a running view over ~0.1 log10 units, via binning rather than a
## rolling window, so both panels share the exact same bin edges). ----
N_BINS <- 35
## x-axis now starts at 0.5% of P99 (PeerJ restyle), not 0.1% -- low enough
## to confirm the low-threshold sensitivity structure is still visible once
## the colour scale below is capped near its 95th percentile.
log_edges <- 10^seq(log10(0.5), log10(35), length.out = N_BINS + 1)
bin_of <- function(pct) pmin(findInterval(pct, log_edges, all.inside = TRUE), N_BINS)

fine_grid <- fine_grid |>
  mutate(bin = bin_of(pct_of_P99), bin_lo = log_edges[bin], bin_hi = log_edges[bin + 1],
         bin_mid = sqrt(bin_lo * bin_hi), bin_width = bin_hi - bin_lo)

heat_data <- fine_grid |>
  group_by(site_id, bin, bin_mid, bin_width) |>
  summarise(abs_sensitivity = mean(abs(local_sensitivity), na.rm = TRUE), .groups = "drop")

## Colour-scale cap (95th percentile of the plotted values), computed here
## so the exact number can also be reported in .legend.txt -- see
## fig_helpers.R::scale_fill_gsl_sensitivity(), which recomputes the same
## quantity internally when building the scale itself.
sensitivity_cap <- unname(quantile(heat_data$abs_sensitivity, 0.95, na.rm = TRUE))

## ---- Main heatmap: rows = 62 sites (skewness order), columns = log-binned
## thresholds, fill = |local sensitivity| (days of GSL lost per 1% of P99;
## darker = more sensitive). geom_tile(width=) in untransformed data units
## renders correctly under the log coordinate transform (geom_raster() does
## not -- see the Stage 3 methods memo). ----
p_heat <- ggplot(heat_data, aes(x = bin_mid, y = site_id, fill = abs_sensitivity)) +
  geom_tile(aes(width = bin_width), height = 1) +
  ## Capped near the 95th percentile of the values actually plotted here
  ## (not a fixed number) -- see fig_helpers.R.
  scale_fill_gsl_sensitivity(heat_data$abs_sensitivity) +
  geom_vline(xintercept = 10, colour = "grey30", linewidth = nature_lwd(PEERJ_LWD_REF)) +
  geom_point(data = derived_site, aes(x = 10, y = site_id, shape = "Z17"), inherit.aes = FALSE,
             colour = GSL_MARKER_COLORS[["Z17"]], size = 1) +
  geom_point(data = derived_site, aes(x = t95_pct, y = site_id, shape = "T95"), inherit.aes = FALSE,
             colour = GSL_MARKER_COLORS[["T95"]], size = 1) +
  scale_shape_gsl_marker() +
  scale_x_log10(name = "Threshold (% of P99)", expand = c(0, 0)) +
  ## coord_cartesian() clips the view without discarding data (unlike
  ## scale_x_log10(limits=...), which drops any row outside the limits --
  ## including whole tiles whose center was in range but rendered edge
  ## wasn't, which silently removed ~2/3 of the low-threshold columns).
  coord_cartesian(xlim = c(0.5, 35)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme_gsl() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank(),
        legend.key.height = unit(4, "mm"), legend.key.width = unit(3, "mm")) +
  ## panel_letter()'s default x=-Inf is undefined under a log scale -- give
  ## it an explicit position within the panel's defined range instead.
  gsl_panel_letter("b", x = 0.55, hjust = 0)

## ---- Left strip: skewness third + functional group, same row order ------
strip_data <- derived_site |> select(site_id, skewness, skewness_third, group)
p_strip <- ggplot(strip_data, aes(y = site_id)) +
  geom_tile(aes(x = 1, fill = skewness_third), width = 1) +
  scale_fill_gsl_skew_third() +
  new_scale_fill() +
  geom_tile(aes(x = 2, fill = group), width = 1) +
  scale_fill_gsl_group(name = "Group") +
  scale_x_continuous(breaks = c(1, 2), labels = c("Skew", "Group"), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = NULL, y = NULL) +
  theme_gsl() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.key.height = unit(4, "mm"), legend.key.width = unit(3, "mm")) +
  gsl_panel_letter("a")

## ---- Right panel: |sensitivity| at Z17's T and at T95, joined by a
## segment (absolute value, matching panel b -- PeerJ restyle). One marker
## convention (GSL_MARKER_SHAPES/COLORS, fig_helpers.R): Z17 = black
## circle, T95 = red triangle, same as panels b/c/4c. ----
right_data <- derived_site |> select(site_id, z17_sensitivity, t95_sensitivity) |>
  mutate(z17_sensitivity = abs(z17_sensitivity), t95_sensitivity = abs(t95_sensitivity))
p_right <- ggplot(right_data, aes(y = site_id)) +
  geom_segment(aes(x = z17_sensitivity, xend = t95_sensitivity, yend = site_id),
               colour = "grey60", linewidth = nature_lwd(PEERJ_LWD_REF)) +
  geom_point(aes(x = z17_sensitivity, shape = "Z17"), size = 1.4, colour = GSL_MARKER_COLORS[["Z17"]]) +
  geom_point(aes(x = t95_sensitivity, shape = "T95"), size = 1.4, colour = GSL_MARKER_COLORS[["T95"]]) +
  scale_shape_gsl_marker() +
  labs(x = "Sensitivity\n(days/%P99,\nabs. value)", y = NULL) +
  ## Panel is narrow (1 of 4.5 layout-width units) -- with PeerJ's larger
  ## tick text, the default 4-5 breaks overlap; 3 keeps them legible.
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) +
  theme_gsl() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position = c(0.68, 0.9), legend.key.width = unit(2, "mm"),
        legend.text = element_text(size = PEERJ_BASE_PT),
        legend.background = element_rect(fill = "white", colour = NA), legend.key = element_blank()) +
  gsl_panel_letter("c")

## ---- Bottom panel: smoothed (log-binned) median |sensitivity| by
## skewness third (absolute value, matching panels b/c -- PeerJ restyle),
## same bins as panel b, shared skewness-third palette. ----
bottom_data <- fine_grid |>
  left_join(derived_site |> select(site_id, skewness_third), by = "site_id") |>
  group_by(skewness_third, bin, bin_mid) |>
  summarise(median_sensitivity = median(abs(local_sensitivity), na.rm = TRUE), .groups = "drop") |>
  mutate(skewness_third = factor(skewness_third, levels = GSL_SKEW_THIRD_LEVELS))

p_bottom <- ggplot(bottom_data, aes(x = bin_mid, y = median_sensitivity, colour = skewness_third)) +
  geom_line(linewidth = nature_lwd(PEERJ_LWD_DATA)) +
  geom_vline(xintercept = 10, colour = "grey50", linewidth = nature_lwd(PEERJ_LWD_REF), linetype = "dashed") +
  scale_color_gsl_skew_third() +
  scale_x_log10(name = "Threshold (% of P99)", expand = c(0, 0)) +
  coord_cartesian(xlim = c(0.5, 35)) +
  labs(y = "Median local sensitivity,\nabs. value (days/%P99)") +
  theme_gsl() +
  ## panel_letter()'s default x=-Inf is undefined (NaN) under scale_x_log10()
  ## -- silently dropped the letter until caught by direct reproduction
  ## (Stage 3 revisions review). Explicit x, as panel "b" above already uses.
  gsl_panel_letter("d", x = 0.55, hjust = 0)

## ---- Assemble -----------------------------------------------------------
top_row <- p_strip + p_heat + p_right + plot_layout(widths = c(0.5, 3, 1), guides = "keep")
fig2 <- top_row / p_bottom + plot_layout(heights = c(3, 1), guides = "keep")

n_low <- sum(derived_site$skewness_third == "low")
n_mid <- sum(derived_site$skewness_third == "middle")
n_high <- sum(derived_site$skewness_third == "high")

legend_lines <- c(
  "Figure 2. Where threshold sensitivity sits across all 62 sites (main run: GPP_NT_VUT_REF,",
  "negative GPP set to 0, year screening d80, GSL counts GPP > T).",
  paste0(
    "Panel a: skewness third (low/middle/high, boundaries 0.63 and 0.88 per docs/framework.md; ",
    "colourblind-safe discrete palette, fig_helpers.R::GSL_SKEW_THIRD_COLORS, used in every figure ",
    "that colours by skewness) and functional group (categorical) per site, same row order as ",
    "panels b/c (ascending skewness bottom to top). Exact per-site skewness values are in ",
    "tables/derived_site.csv (this figure's input source), not repeated here."
  ),
  sprintf(
    paste0(
      "Panel b: heat map of |local sensitivity| (days of GSL lost per 1%% of P99, absolute value; ",
      "darker = more sensitive), 62 sites x log-binned thresholds (0.5-35%% of P99, %d bins). Colour ",
      "scale capped at the 95th percentile of the plotted values (%.1f days/%%P99); top colour ",
      "labelled '>= %.1f'. Z17 (black circle) and T95 (red triangle) marked per row -- same ",
      "convention as panels c and 4c."
    ), N_BINS, sensitivity_cap, sensitivity_cap
  ),
  "Panel c: |sensitivity| at Z17 (black circle) and at T95 (red triangle), joined by a segment.",
  sprintf(
    paste0(
      "Panel d: median |local sensitivity| per threshold bin (same %d log-spaced bins as panel b), ",
      "by skewness third (low/middle/high, same palette as panel a). Dashed line at 10%% of P99 (Z17)."
    ), N_BINS
  ),
  sprintf("Sites per skewness third: low = %d, middle = %d, high = %d (total 62).", n_low, n_mid, n_high)
)

save_gsl_figure(
  fig2, "Figure 2", width_mm = PEERJ_FIG_WIDTH_MM, height_mm = 220,
  legend_lines = legend_lines,
  input_sources = c(file.path(GSL_ROOT, "tables", "derived_site.csv"),
                     file.path(GSL_ROOT, "data", "fine_grid.rds")),
  notes = sprintf("62 sites, skewness order. Terciles: low=%d, middle=%d, high=%d. Sensitivity shown as |value|, log-binned (%d bins, 0.5-35%% of P99), colour scale capped at the 95th percentile (%.2f days/%%P99).", n_low, n_mid, n_high, N_BINS, sensitivity_cap)
)

message("--- 06_fig2.R: done --- n_low=", n_low, " n_mid=", n_mid, " n_high=", n_high)
