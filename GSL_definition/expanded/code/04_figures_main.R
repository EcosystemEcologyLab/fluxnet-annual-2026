## 04_figures_main.R — GSL expanded analysis, Figures 1-6 (PeerJ style,
## reusing code/fig_helpers.R by sourcing). Z17 and T95 shown as two series
## in every relevant panel; at most 3 panels per figure.

source("GSL_definition/expanded/code/00_config.R")
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(lubridate); library(purrr)
  library(ggplot2); library(patchwork); library(scales)
})

exp_sites    <- read_csv(file.path(EXP_TABLES, "sites.csv"), show_col_types = FALSE)
derived_site <- read_csv(file.path(EXP_TABLES, "derived_site.csv"), show_col_types = FALSE) |> filter(variant == "main")
season_boundaries <- read_csv(file.path(EXP_TABLES, "season_boundaries.csv"), show_col_types = FALSE)
cg_data      <- read_csv(file.path(EXP_TABLES, "carbon_gain_data.csv"), show_col_types = FALSE)
cg_site      <- read_csv(file.path(EXP_TABLES, "carbon_gain_per_site.csv"), show_col_types = FALSE)
cg_pooled    <- read_csv(file.path(EXP_TABLES, "carbon_gain_pooled.csv"), show_col_types = FALSE)
ts_site      <- read_csv(file.path(EXP_TABLES, "temperature_sensitivity_per_site.csv"), show_col_types = FALSE)
ts_pooled    <- read_csv(file.path(EXP_TABLES, "temperature_sensitivity_pooled.csv"), show_col_types = FALSE)
trend_pooled <- read_csv(file.path(EXP_TABLES, "trend_pooled.csv"), show_col_types = FALSE)
fine_grid_main <- readRDS(file.path(EXP_DATA, "fine_grid_main.rds"))
daily_extract  <- readRDS(file.path(EXP_DATA, "daily_extract.rds"))
site_years_list <- split(read_csv(file.path(EXP_TABLES, "site_years.csv"), show_col_types = FALSE)$year,
                          read_csv(file.path(EXP_TABLES, "site_years.csv"), show_col_types = FALSE)$site_id)

aridity <- read_csv("data/snapshots/site_aridity.csv", show_col_types = FALSE) |> select(site_id, ai_value)
derived_site <- derived_site |> left_join(aridity, by = "site_id")

SUBSET_LABELS <- c(third_low = "Low", third_middle = "Middle", third_high = "High",
                    group_grassland = "Grassland", group_evergreen_forest = "Evergreen forest",
                    group_deciduous_forest = "Deciduous forest",
                    `group_woody-grass_mosaic` = "Woody-grass mosaic", group_mixed_forest = "Mixed forest")

## ============================================================================
## Figure 1 -- US-Ha1 and US-SRM, illustrations (not rule-selected)
## ============================================================================
FIG1_SITES <- c("US-Ha1", "US-SRM")

day_class_one <- function(sid) {
  P99 <- derived_site$P99[derived_site$site_id == sid]
  t95pct <- derived_site$t95_pct[derived_site$site_id == sid]
  d <- daily_extract |> filter(site_id == sid, year %in% site_years_list[[sid]]) |>
    mutate(gpp_use = pmax(GPP_NT_REF, 0),
           above_z17 = !is.na(gpp_use) & gpp_use > 0.10 * P99,
           above_t95 = !is.na(gpp_use) & gpp_use > (t95pct / 100) * P99,
           category = case_when(
             above_z17 & above_t95 ~ "both", above_z17 & !above_t95 ~ "Z17 only",
             !above_z17 & above_t95 ~ "T95 only", TRUE ~ "neither"
           ), doy = lubridate::yday(TIMESTAMP))
  d |> select(site_id, year, doy, category)
}
fig1a_data <- bind_rows(lapply(FIG1_SITES, day_class_one)) |>
  mutate(category = factor(category, levels = c("both", "Z17 only", "T95 only", "neither")))

FIG1_CAT_COLORS <- c(both = "#1B7837", `Z17 only` = GSL_MARKER_COLORS[["Z17"]],
                      `T95 only` = GSL_MARKER_COLORS[["T95"]], neither = "grey85")

p1a <- ggplot(fig1a_data, aes(x = doy, y = factor(year), fill = category)) +
  geom_tile() +
  facet_wrap(~ site_id, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = FIG1_CAT_COLORS, name = "Included by", drop = FALSE) +
  scale_x_continuous(breaks = c(1, 91, 182, 274, 366), labels = c("Jan", "Apr", "Jul", "Oct", "Jan")) +
  labs(x = "Day of year", y = NULL) +
  theme_gsl() +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold")) +
  gsl_panel_letter("a") # appears once per facet panel (annotate() default); both instances sit
                        # in empty corner space, not over data or the legend -- not a QA-check issue

fig1b_data <- fine_grid_main |> filter(site_id %in% FIG1_SITES, pct_of_P99 <= 50)
fig1b_marks <- derived_site |> filter(site_id %in% FIG1_SITES) |>
  select(site_id, z17_pct, z17_mean_gsl, t95_pct, t95_mean_gsl) |>
  pivot_longer(c(z17_pct, t95_pct), names_to = "which", values_to = "pct") |>
  mutate(gsl = ifelse(which == "z17_pct", z17_mean_gsl, t95_mean_gsl),
         marker = ifelse(which == "z17_pct", "Z17", "T95")) |>
  select(site_id, pct, gsl, marker)

p1b <- ggplot(fig1b_data, aes(x = pct_of_P99, y = mean_gsl, colour = site_id)) +
  geom_line(linewidth = nature_lwd(PEERJ_LWD_DATA)) +
  geom_point(data = fig1b_marks, aes(x = pct, y = gsl, shape = marker), colour = "black", size = 2,
             inherit.aes = FALSE) +
  scale_shape_gsl_marker() +
  scale_color_manual(values = c(`US-Ha1` = "#56B4E9", `US-SRM` = "#D55E00"), name = "Site") +
  labs(x = "Threshold (% of P99)", y = "Mean GSL (days)") +
  theme_gsl() + theme(legend.position = "right") +
  gsl_panel_letter("b")

fig1 <- p1a / p1b + plot_layout(heights = c(2.2, 1))
save_gsl_figure(
  fig1, "Figure 1", height_mm = 200,
  legend_lines = c(
    "Figure 1. Mechanism at two illustrative sites, US-Ha1 and US-SRM -- chosen to illustrate",
    "contrasting phenology, NOT selected by any rule (one deciduous forest, one woody-grass",
    "mosaic; see docs/report_back_2026-10-30.md for why these two).",
    "Panel a: each day of each passing year, coloured by whether it is included in the growing",
    "season under both Z17 and T95, one only, or neither.",
    "Panel b: mean GSL against threshold (% of P99, 0-50%) for both sites; black circle/red",
    "triangle mark each site's own Z17/T95 point on its curve."
  ),
  input_sources = c(file.path(EXP_DATA, "daily_extract.rds"), file.path(EXP_DATA, "fine_grid_main.rds"),
                     file.path(EXP_TABLES, "derived_site.csv")),
  notes = "Illustrative sites chosen for contrast, not by a selection rule."
)

## ============================================================================
## Figure 2 -- share of annual GPP outside the Z17 season vs skewness, aridity
## ============================================================================
p2a <- ggplot(derived_site, aes(x = skewness, y = share_outside_season_z17, colour = group)) +
  geom_point(size = 1.8, alpha = 0.85) +
  scale_color_gsl_group(name = "Group") +
  labs(x = "Skewness", y = "Share of annual GPP\noutside Z17 season") +
  theme_gsl() + theme(legend.position = "right") + gsl_panel_letter("a")

p2b <- ggplot(derived_site, aes(x = ai_value, y = share_outside_season_z17, colour = group)) +
  geom_point(size = 1.8, alpha = 0.85) +
  scale_color_gsl_group(name = "Group") +
  labs(x = "Aridity index (CGIAR; higher = more humid)", y = "Share of annual GPP\noutside Z17 season") +
  theme_gsl() + theme(legend.position = "right") + gsl_panel_letter("b")

fig2 <- p2a / p2b
n_no_aridity <- sum(is.na(derived_site$ai_value))
save_gsl_figure(
  fig2, "Figure 2", height_mm = 170,
  legend_lines = c(
    "Figure 2. Share of annual GPP outside the Z17 growing season (1 - F at 10% of P99), against",
    "skewness (a) and the CGIAR aridity index (b; higher = more humid, lower = more arid),",
    "coloured by functional group.",
    sprintf("%d of %d sites have no aridity index value and are omitted from panel b.",
            n_no_aridity, nrow(derived_site))
  ),
  input_sources = c(file.path(EXP_TABLES, "derived_site.csv"), "data/snapshots/site_aridity.csv"),
  notes = ""
)

message("--- Figures 1-2 done ---")

## ============================================================================
## Figure 3 -- sensitivity in two units
## ============================================================================
fine_grid_summary <- read_csv(file.path(EXP_TABLES, "fine_grid_summary.csv"), show_col_types = FALSE) |>
  filter(variant == "main") |> left_join(derived_site |> select(site_id, skewness_third, skewness), by = "site_id")

fg_third <- fine_grid_main |>
  filter(pct_of_P99 >= 0.1, pct_of_P99 <= 35) |>
  left_join(derived_site |> select(site_id, skewness_third), by = "site_id") |>
  filter(!is.na(skewness_third)) |>
  group_by(skewness_third, pct_of_P99) |>
  summarise(median_sensitivity = median(local_sensitivity, na.rm = TRUE), .groups = "drop")

p3a <- ggplot(fg_third, aes(x = pct_of_P99, y = median_sensitivity, colour = skewness_third)) +
  geom_line(linewidth = nature_lwd(PEERJ_LWD_DATA)) +
  scale_x_log10() +
  scale_color_gsl_skew_third() +
  labs(x = "Threshold (% of P99, log scale)", y = "Median sensitivity\n(days / 1% of P99)") +
  theme_gsl() + theme(legend.position = "right") +
  gsl_panel_letter("a", x = 0.11, hjust = 0)

p3b <- ggplot(fine_grid_summary, aes(x = skewness, y = sensitivity_at_10pct, colour = skewness_third)) +
  geom_point(size = 1.8, alpha = 0.85) +
  scale_color_gsl_skew_third() +
  labs(x = "Skewness", y = "Sensitivity at Z17\n(days / 1% of P99)") +
  theme_gsl() + theme(legend.position = "right") + gsl_panel_letter("b")

cus_data <- derived_site |> select(site_id, skewness, z17_common_unit_sensitivity, t95_common_unit_sensitivity) |>
  pivot_longer(c(z17_common_unit_sensitivity, t95_common_unit_sensitivity), names_to = "which", values_to = "cus") |>
  mutate(marker = ifelse(which == "z17_common_unit_sensitivity", "Z17", "T95"))

p3c <- ggplot(cus_data, aes(x = skewness, y = cus, colour = marker, shape = marker)) +
  geom_point(size = 1.8, alpha = 0.85) +
  scale_color_manual(values = GSL_MARKER_COLORS, name = NULL, breaks = c("Z17", "T95")) +
  scale_shape_gsl_marker() +
  labs(x = "Skewness", y = "Days / 1% of\nincluded GPP") +
  theme_gsl() + theme(legend.position = "right") + gsl_panel_letter("c")

fig3 <- p3a / p3b / p3c
cor_b <- cor(fine_grid_summary$skewness, fine_grid_summary$sensitivity_at_10pct, use = "complete.obs")
cor_c_z17 <- cor(derived_site$skewness, derived_site$z17_common_unit_sensitivity, use = "complete.obs")
save_gsl_figure(
  fig3, "Figure 3", height_mm = 230,
  legend_lines = c(
    "Figure 3. Threshold sensitivity in two units (expanded set, main run).",
    "Panel a: median sensitivity (days lost per 1% of P99), 0.1-35% of P99 (log axis), by skewness third.",
    "Panel b: sensitivity at Z17 specifically, against skewness (one point per site; r = ",
    sprintf("%.2f).", cor_b),
    "Panel c: 'days per 1% of included GPP' (the common-unit sensitivity dGSL/dF x 0.01), Z17 and",
    "T95, against skewness.",
    sprintf(
      "Panels b and c run in opposite directions (b: r = %.2f; c, Z17: r = %.2f) because panel c ",
      cor_b, cor_c_z17
    ),
    "normalises by how much the included-GPP fraction F changes per % of P99 -- at high-skewness",
    "sites a small threshold change corresponds to a LARGE change in F (GPP is concentrated near",
    "zero for much of the year), so the same day-count sensitivity becomes a SMALLER per-unit-F",
    "sensitivity; panel b has no such normalisation."
  ),
  input_sources = c(file.path(EXP_DATA, "fine_grid_main.rds"), file.path(EXP_TABLES, "fine_grid_summary.csv"),
                     file.path(EXP_TABLES, "derived_site.csv")),
  notes = sprintf("cor(skewness, sensitivity_at_Z17) = %.3f; cor(skewness, common_unit_sensitivity_Z17) = %.3f", cor_b, cor_c_z17)
)

message("--- Figure 3 done ---")

## ============================================================================
## Figure 4 -- season timing
## ============================================================================
sb_wide <- season_boundaries |>
  filter(threshold_name %in% c("5pct", "20pct", "z17", "t95")) |>
  group_by(site_id, threshold_name) |>
  summarise(mean_start_doy = mean(start_doy, na.rm = TRUE), mean_end_doy = mean(end_doy, na.rm = TRUE),
            mean_gap_days = mean(gap_days, na.rm = TRUE), .groups = "drop")

shift_data <- sb_wide |> filter(threshold_name %in% c("5pct", "20pct")) |>
  select(site_id, threshold_name, mean_start_doy, mean_end_doy) |> # mean_gap_days dropped first --
  ## pivot_wider() would otherwise fold it into the implicit id_cols (it
  ## differs between 5pct and 20pct rows for the same site), splitting each
  ## site across two non-matching rows and leaving start/end mostly NA.
  pivot_wider(names_from = threshold_name, values_from = c(mean_start_doy, mean_end_doy)) |>
  mutate(start_shift = mean_start_doy_20pct - mean_start_doy_5pct,
         end_shift = mean_end_doy_20pct - mean_end_doy_5pct) |>
  select(site_id, start_shift, end_shift) |>
  pivot_longer(c(start_shift, end_shift), names_to = "which", values_to = "shift_days") |>
  mutate(which = ifelse(which == "start_shift", "Start date", "End date")) |>
  left_join(derived_site |> select(site_id, skewness), by = "site_id")

p4a <- ggplot(shift_data, aes(x = skewness, y = shift_days, colour = which)) +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = nature_lwd(PEERJ_LWD_REF)) +
  geom_point(size = 1.8, alpha = 0.85) +
  scale_color_manual(values = c("Start date" = "#2C7BB6", "End date" = "#D7191C"), name = NULL) +
  labs(x = "Skewness", y = "Shift, 5% to 20% of P99\n(days; end - start convention)") +
  theme_gsl() + theme(legend.position = "right") + gsl_panel_letter("a")

gap_data <- sb_wide |> filter(threshold_name %in% c("z17", "t95")) |>
  mutate(marker = ifelse(threshold_name == "z17", "Z17", "T95")) |>
  left_join(derived_site |> select(site_id, skewness), by = "site_id")

p4b <- ggplot(gap_data, aes(x = skewness, y = mean_gap_days, colour = marker, shape = marker)) +
  geom_point(size = 1.8, alpha = 0.85) +
  scale_color_manual(values = GSL_MARKER_COLORS, name = NULL, breaks = c("Z17", "T95")) +
  scale_shape_gsl_marker() +
  labs(x = "Skewness", y = "Mean gap days\nwithin season") +
  theme_gsl() + theme(legend.position = "right") + gsl_panel_letter("b")

fig4 <- p4a / p4b
save_gsl_figure(
  fig4, "Figure 4", height_mm = 170,
  legend_lines = c(
    "Figure 4. Season timing (expanded set, main run).",
    "Panel a: shift in mean start and end date between the 5% and 20% of P99 thresholds, against",
    "skewness (positive = date moves later in the year as the threshold loosens from 5% to 20%).",
    "Panel b: mean within-season gap days (days below threshold between season start and end),",
    "against skewness, at Z17 and T95."
  ),
  input_sources = file.path(EXP_TABLES, "season_boundaries.csv"), notes = ""
)

message("--- Figure 4 done ---")

## ============================================================================
## Figure 5 -- carbon gain
## ============================================================================
site_meta_cg <- derived_site |> select(site_id, skewness_third)

#' Build a forest-plot panel: x = skewness third, colour = Z17/T95,
#' facet_grid(response_lab ~ date_lab) -- date_lab is a single level ("GSL")
#' for panel a, or "Start date"/"End date" for panel b, so both panels share
#' one function with a clear facet structure instead of merging predictors
#' into one undifferentiated axis.
pred_to_date_lab <- function(predictor) {
  dplyr::case_when(
    grepl("^gsl_", predictor) ~ "GSL",
    grepl("^start_", predictor) ~ "Start date",
    grepl("^end_", predictor) ~ "End date"
  )
}
make_fig5_panel <- function(predictors, ylab, panel_letter, date_levels) {
  tag <- function(df) df |> mutate(
    threshold = ifelse(grepl("z17$", predictor), "Z17", "T95"),
    date_lab = factor(pred_to_date_lab(predictor), levels = date_levels),
    response_lab = recode(response, annual_gpp = "GPP", annual_nep = "NEP")
  )
  pooled <- cg_pooled |> filter(predictor %in% predictors, subset %in% paste0("third_", GSL_SKEW_THIRD_LEVELS)) |>
    mutate(third = factor(sub("third_", "", subset), levels = GSL_SKEW_THIRD_LEVELS)) |> tag()
  site <- cg_site |> filter(predictor %in% predictors) |>
    left_join(site_meta_cg, by = "site_id") |>
    mutate(third = factor(skewness_third, levels = GSL_SKEW_THIRD_LEVELS)) |> tag()

  ggplot(pooled, aes(x = third, y = slope, colour = threshold)) +
    geom_jitter(data = site, width = 0.12, size = 0.8, alpha = 0.3, shape = 16) +
    geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), position = position_dodge(width = 0.5), width = 0.15,
                  linewidth = nature_lwd(PEERJ_LWD_REF)) +
    geom_point(position = position_dodge(width = 0.5), size = 2) +
    facet_grid(response_lab ~ date_lab, scales = "free_y") +
    scale_color_manual(values = GSL_MARKER_COLORS, name = NULL, breaks = c("Z17", "T95")) +
    labs(x = "Skewness third", y = ylab) +
    theme_gsl() + theme(legend.position = "right", strip.text = element_text(face = "bold")) +
    gsl_panel_letter(panel_letter)
}

p5a <- make_fig5_panel(c("gsl_z17", "gsl_t95"), "Pooled slope (gC/m2 per day of GSL);\nfaint points = per-site slopes", "a", "GSL")
p5b <- make_fig5_panel(c("start_z17", "start_t95", "end_z17", "end_t95"),
                        "Pooled slope (gC/m2 per day shift\nin start/end date)", "b", c("Start date", "End date"))

fig5 <- p5a / p5b + plot_layout(heights = c(1, 1))
save_gsl_figure(
  fig5, "Figure 5", height_mm = 230,
  legend_lines = c(
    "Figure 5. Carbon gain (expanded set, main run).",
    "Panel a: pooled slope of annual GPP and NEP on GSL (gC/m2 per day of GSL), by skewness third,",
    "for Z17 and T95; faint points behind are individual trend-site (>=10yr) slopes.",
    "Panel b: the same, on season start date and end date (gC/m2 per day of date shift), faceted",
    "by GPP/NEP (rows) and start/end (columns)."
  ),
  input_sources = c(file.path(EXP_TABLES, "carbon_gain_pooled.csv"), file.path(EXP_TABLES, "carbon_gain_per_site.csv")),
  notes = ""
)

message("--- Figure 5 done ---")

## ============================================================================
## Figure 6 -- climate fingerprints
## ============================================================================
ts_pooled_third <- ts_pooled |>
  filter(subset %in% paste0("third_", GSL_SKEW_THIRD_LEVELS)) |>
  mutate(third = factor(sub("third_", "", subset), levels = GSL_SKEW_THIRD_LEVELS),
         threshold = ifelse(grepl("_z17$", combo), "Z17", "T95"),
         var = factor(case_when(
           grepl("^gsl_", combo) ~ "GSL", grepl("^start_", combo) ~ "Start date", TRUE ~ "End date"
         ), levels = c("GSL", "Start date", "End date")))

p6a <- ggplot(ts_pooled_third, aes(x = third, y = slope, colour = threshold)) +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), position = position_dodge(width = 0.5), width = 0.15,
                linewidth = nature_lwd(PEERJ_LWD_REF)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = nature_lwd(PEERJ_LWD_REF)) +
  facet_wrap(~ var, nrow = 1, scales = "free_y") +
  scale_color_manual(values = GSL_MARKER_COLORS, name = NULL, breaks = c("Z17", "T95")) +
  labs(x = "Skewness third", y = "Temperature sensitivity\n(days / degC)") +
  theme_gsl() + theme(legend.position = "right", strip.text = element_text(face = "bold")) +
  gsl_panel_letter("a")

## ---- Panel b: pooled trend vs threshold (5-20% of P99), by third and by
## group, Z17/T95 marked, each pooled estimate labelled with years-to-detect.
trend_numeric <- trend_pooled |> filter(threshold_name != "t95pct") |>
  mutate(threshold_pct = as.numeric(sub("pct", "", threshold_name)),
         subset_type = ifelse(startsWith(subset, "third_"), "By skewness third", "By functional group"),
         subset_lab = recode(subset, !!!SUBSET_LABELS))
trend_t95 <- trend_pooled |> filter(threshold_name == "t95pct") |>
  mutate(subset_type = ifelse(startsWith(subset, "third_"), "By skewness third", "By functional group"),
         subset_lab = recode(subset, !!!SUBSET_LABELS))
trend_z17_pts <- trend_numeric |> filter(threshold_pct == 10)

subset_colors_third <- GSL_SKEW_THIRD_COLORS
subset_colors_group <- GSL_GROUP_COLORS
names(subset_colors_third) <- SUBSET_LABELS[paste0("third_", names(subset_colors_third))]
names(subset_colors_group) <- SUBSET_LABELS[paste0("group_", gsub(" ", "_", names(subset_colors_group)))]
subset_colors_all <- c(subset_colors_third, subset_colors_group)

trend_t95_lab <- trend_t95 |>
  mutate(label = ifelse(is.na(years_to_detect_median) | !converged, NA_character_,
                         sprintf("%.0f yr", years_to_detect_median)))

p6b <- ggplot(trend_numeric, aes(x = threshold_pct, y = slope, colour = subset_lab, group = subset_lab)) +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = nature_lwd(PEERJ_LWD_REF)) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi, fill = subset_lab), alpha = 0.12, colour = NA) +
  geom_line(linewidth = nature_lwd(PEERJ_LWD_DATA)) +
  geom_point(data = trend_z17_pts, aes(x = threshold_pct, y = slope), shape = 16, size = 1.6) +
  geom_point(data = trend_t95, aes(x = 21, y = slope, fill = subset_lab), shape = 24, size = 2.4,
             colour = "black", stroke = 0.4) +
  ggrepel::geom_text_repel(
    data = trend_t95_lab, aes(x = 21, y = slope, label = label), inherit.aes = TRUE,
    size = PEERJ_ANNOTATION_PT / .pt, show.legend = FALSE, seed = 1, min.segment.length = 0,
    segment.linewidth = 0.3, direction = "y", nudge_x = 4, hjust = 0, xlim = c(21, 28), na.rm = TRUE
  ) +
  facet_wrap(~ subset_type, ncol = 1, scales = "free_y") +
  scale_color_manual(values = subset_colors_all, name = NULL) +
  scale_fill_manual(values = subset_colors_all, guide = "none") +
  scale_x_continuous(limits = c(5, 30), breaks = seq(5, 20, 5)) +
  labs(x = "Threshold (% of P99; T95 plotted at right, filled triangle)", y = "Pooled GSL trend (days/yr)") +
  theme_gsl() + theme(legend.position = "right") +
  gsl_panel_letter("b")

fig6 <- p6a / p6b + plot_layout(heights = c(1, 1.6))
n_na_ytd <- sum(is.na(trend_pooled$years_to_detect_median) & trend_pooled$converged)
save_gsl_figure(
  fig6, "Figure 6", height_mm = 235,
  legend_lines = c(
    "Figure 6. Climate fingerprints (expanded set, main run).",
    "Panel a: temperature sensitivity (days/degC) of GSL, season start and end, by skewness third,",
    "Z17 and T95 (pooled mixed model; error bars = 95% CI).",
    "Panel b: pooled GSL trend (days/yr) across 5-20% of P99 (line + 95% CI ribbon, circle marks",
    "Z17 at 10%), by skewness third (top) and functional group (bottom); T95's own pooled trend is",
    "plotted at the right edge (triangle), labelled with the Weatherhead et al. (1998) years needed",
    "to detect that pooled trend at 95% confidence / 80% power (median across that subset's sites,",
    "each using its own residual SD and lag-1 autocorrelation with the pooled slope).",
    "Mixed-forest's trend is not shown (only 2 of its 7 sites have >=10 years, below the >=3-site",
    "floor for a pooled model); any other NA years-to-detect reflects a near-zero pooled slope."
  ),
  input_sources = file.path(EXP_TABLES, "trend_pooled.csv"),
  notes = sprintf("%d pooled estimate(s) converged but had an undefined (NA) years-to-detect (zero slope).", n_na_ytd)
)

message("--- Figure 6 done ---")

message("--- 04_figures_main.R: Figures 1-6 done ---")
