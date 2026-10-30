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
  filter(gpp_variant == "main", neg_gpp_variant == "main", pct_of_P99 >= 0.1, pct_of_P99 <= 35)

## Row order: ascending skewness, top of plot = highest skewness (so the
## heatmap reads low-to-high skewness bottom-to-top, matching how sites are
## listed low/middle/high third in the text).
site_order <- derived_site |> arrange(skewness) |> pull(site_id)
derived_site$site_id <- factor(derived_site$site_id, levels = site_order)
fine_grid$site_id <- factor(fine_grid$site_id, levels = site_order)
stopifnot(nlevels(derived_site$site_id) == 62L)

## ---- Main heatmap: rows = 62 sites (skewness order), columns = fine-grid
## thresholds (log axis), fill = local sensitivity (single-hue scale). -----
## geom_raster() assumes a grid that is uniform AFTER the scale transform;
## under scale_x_log10() that is false (the data is uniform in untransformed
## %P99, 0.01 steps, not in log space), which silently rendered as near-blank
## with thin stripes. geom_tile(width=) operates in untransformed data units
## (the tile's own x +/- width/2 is what gets log-transformed, same as the
## point itself), so an explicit width = the grid step size renders correctly.
p_heat <- ggplot(fine_grid, aes(x = pct_of_P99, y = site_id, fill = local_sensitivity)) +
  geom_tile(width = 0.01, height = 1) +
  scale_fill_distiller(palette = "Blues", direction = 1, name = "Local\nsensitivity\n(days/%P99)") +
  geom_vline(xintercept = 10, colour = "black", linewidth = nature_lwd(0.4)) +
  geom_point(data = derived_site, aes(x = t95_pct, y = site_id), inherit.aes = FALSE,
             colour = "black", size = 0.5) +
  scale_x_log10(name = "Threshold (% of P99)", expand = c(0, 0)) +
  ## coord_cartesian() clips the view without discarding data (unlike
  ## scale_x_log10(limits=...), which drops any row outside the limits --
  ## including whole tiles whose center was in range but rendered edge
  ## wasn't, which silently removed ~2/3 of the low-threshold columns).
  coord_cartesian(xlim = c(0.1, 35)) +
  scale_y_discrete(expand = c(0, 0)) +
  theme_gsl() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank(),
        legend.key.height = unit(3, "mm"), legend.key.width = unit(2.5, "mm")) +
  ## panel_letter()'s default x=-Inf is undefined under a log scale -- give
  ## it an explicit position within the panel's defined range instead.
  panel_letter("b", x = 0.15, hjust = 0)

## ---- Left strip: skewness value + functional group, same row order ------
strip_data <- derived_site |> select(site_id, skewness, group)
p_strip <- ggplot(strip_data, aes(y = site_id)) +
  geom_tile(aes(x = 1, fill = skewness), width = 1) +
  scale_fill_gsl_skew() +
  new_scale_fill() +
  geom_tile(aes(x = 2, fill = group), width = 1) +
  scale_fill_gsl_group(name = "Group") +
  scale_x_continuous(breaks = c(1, 2), labels = c("Skew", "Group"), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = NULL, y = NULL) +
  theme_gsl() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.key.height = unit(3, "mm"), legend.key.width = unit(2.5, "mm")) +
  panel_letter("a")

## ---- Right panel: sensitivity at Z17's T and at T95, joined by a segment --
right_data <- derived_site |> select(site_id, z17_sensitivity, t95_sensitivity)
p_right <- ggplot(right_data, aes(y = site_id)) +
  geom_segment(aes(x = z17_sensitivity, xend = t95_sensitivity, yend = site_id),
               colour = "grey60", linewidth = nature_lwd(0.3)) +
  geom_point(aes(x = z17_sensitivity, shape = "Z17 (10% of P99)"), size = 0.7, colour = "black") +
  geom_point(aes(x = t95_sensitivity, shape = "T95"), size = 0.7, colour = "firebrick") +
  scale_shape_manual(values = c("Z17 (10% of P99)" = 16, "T95" = 17), name = NULL) +
  labs(x = "Sensitivity\n(days/%P99)", y = NULL) +
  theme_gsl() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position = c(0.7, 0.88), legend.key.width = unit(1, "mm"),
        legend.text = element_text(size = NATURE_SMALL_PT),
        legend.background = element_blank(), legend.key = element_blank()) +
  panel_letter("c")

## ---- Bottom panel: median sensitivity per column, by skewness third -----
bottom_data <- fine_grid |>
  left_join(derived_site |> select(site_id, skewness_third), by = "site_id") |>
  group_by(skewness_third, pct_of_P99) |>
  summarise(median_sensitivity = median(local_sensitivity, na.rm = TRUE), .groups = "drop") |>
  mutate(skewness_third = factor(skewness_third, levels = c("low", "middle", "high")))

p_bottom <- ggplot(bottom_data, aes(x = pct_of_P99, y = median_sensitivity, colour = skewness_third)) +
  geom_line(linewidth = nature_lwd(0.5)) +
  geom_vline(xintercept = 10, colour = "grey50", linewidth = nature_lwd(0.25), linetype = "dashed") +
  scale_colour_manual(values = c(low = "#1b7837", middle = "#b2abd2", high = "#762a83"),
                       name = "Skewness third") +
  scale_x_log10(name = "Threshold (% of P99)", expand = c(0, 0)) +
  coord_cartesian(xlim = c(0.1, 35)) +
  labs(y = "Median local\nsensitivity (days/%P99)") +
  theme_gsl() +
  panel_letter("d")

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
    "Panel a: skewness (continuous, viridis) and functional group (categorical) per site, same ",
    "row order as panels b/c (ascending skewness bottom to top)."
  ),
  paste0(
    "Panel b: heat map of local sensitivity (change in mean GSL per 1% of P99, centered ",
    "difference over +/-0.05% of P99), 62 sites x fine-grid thresholds (0.1-35% of P99, log axis). ",
    "Vertical black line at 10% of P99 (Z17); black dot per row at that site's own T95."
  ),
  "Panel c: sensitivity at Z17's threshold (circle) and at T95 (triangle), joined by a segment.",
  paste0(
    "Panel d: median local sensitivity per threshold, by skewness third (low/middle/high, ",
    "boundaries 0.63 and 0.88 per docs/framework.md). Dashed line at 10% of P99 (Z17)."
  ),
  sprintf("Sites per skewness third: low = %d, middle = %d, high = %d (total 62).", n_low, n_mid, n_high)
)

save_gsl_figure(
  fig2, "fig_02_sensitivity_heatmap", width_mm = NATURE_WIDTH_DOUBLE_MM, height_mm = 220,
  legend_lines = legend_lines,
  input_sources = c(file.path(GSL_ROOT, "tables", "derived_site.csv"),
                     file.path(GSL_ROOT, "data", "fine_grid.rds")),
  notes = sprintf("62 sites, skewness order. Terciles: low=%d, middle=%d, high=%d.", n_low, n_mid, n_high)
)

message("--- 06_fig2.R: done --- n_low=", n_low, " n_mid=", n_mid, " n_high=", n_high)
