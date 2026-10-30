## 10_supplementary.R — GSL figure rebuild, supplementary figures.
##
## Figures 2 and 3 repeated (i) with GPP_DT_VUT_REF and (ii) with negative
## GPP as-delivered (not zero-floored) -- per the Stage 3 prompt, replacing
## the framework's original "with Tash's screening" variant, since Tash's
## screening selects the same 505 site-years as the main run (Stage 1/2) so
## there is no separate version to build. The current manuscript's own
## Figures 1-4 are explicitly DEFERRED (not built here): the framework does
## not describe them in enough detail to rebuild without the manuscript
## itself -- see docs/report_back_*.md.

source("GSL_definition/code/00_config.R")
source("GSL_definition/code/fig_helpers.R")
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(lubridate); library(ggnewscale)
})

derived_site_all <- read_csv(file.path(GSL_ROOT, "tables", "derived_site.csv"), show_col_types = FALSE) |>
  left_join(GSL_SITES |> select(site_id, group), by = "site_id")
fine_grid_all <- readRDS(file.path(GSL_ROOT, "data", "fine_grid.rds"))
daily_extract <- readRDS(file.path(GSL_ROOT, "data", "daily_extract.rds"))
daily_extract$TIMESTAMP <- as.Date(as.character(daily_extract$TIMESTAMP))
daily_extract$year <- lubridate::year(daily_extract$TIMESTAMP)

trend_sites <- GSL_SITES_GE10YR

## ============================================================================
## Figure 2 builder (identical structure/fixes to 06_fig2.R), parameterized
## by variant.
## ============================================================================
build_fig2 <- function(gpp_variant, neg_variant, stem, variant_label) {
  derived_site <- derived_site_all |> filter(gpp_variant == !!gpp_variant, neg_gpp_variant == !!neg_variant)
  fine_grid <- fine_grid_all |> filter(gpp_variant == !!gpp_variant, neg_gpp_variant == !!neg_variant,
                                         pct_of_P99 >= 0.1, pct_of_P99 <= 35)
  site_order <- derived_site |> arrange(skewness) |> pull(site_id)
  derived_site$site_id <- factor(derived_site$site_id, levels = site_order)
  fine_grid$site_id <- factor(fine_grid$site_id, levels = site_order)

  p_heat <- ggplot(fine_grid, aes(x = pct_of_P99, y = site_id, fill = local_sensitivity)) +
    geom_tile(width = 0.01, height = 1) +
    scale_fill_distiller(palette = "Blues", direction = 1, name = "Local\nsensitivity\n(days/%P99)") +
    geom_vline(xintercept = 10, colour = "black", linewidth = nature_lwd(0.4)) +
    geom_point(data = derived_site, aes(x = t95_pct, y = site_id), inherit.aes = FALSE,
               colour = "black", size = 0.5) +
    scale_x_log10(name = "Threshold (% of P99)", expand = c(0, 0)) +
    coord_cartesian(xlim = c(0.1, 35)) +
    scale_y_discrete(expand = c(0, 0)) +
    theme_gsl() +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank(),
          legend.key.height = unit(3, "mm"), legend.key.width = unit(2.5, "mm")) +
    panel_letter("b", x = 0.15, hjust = 0)

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

  bottom_data <- fine_grid |>
    left_join(derived_site |> select(site_id, skewness_third), by = "site_id") |>
    group_by(skewness_third, pct_of_P99) |>
    summarise(median_sensitivity = median(local_sensitivity, na.rm = TRUE), .groups = "drop") |>
    mutate(skewness_third = factor(skewness_third, levels = c("low", "middle", "high")))
  p_bottom <- ggplot(bottom_data, aes(x = pct_of_P99, y = median_sensitivity, colour = skewness_third)) +
    geom_line(linewidth = nature_lwd(0.5)) +
    geom_vline(xintercept = 10, colour = "grey50", linewidth = nature_lwd(0.25), linetype = "dashed") +
    scale_colour_manual(values = c(low = "#1b7837", middle = "#b2abd2", high = "#762a83"), name = "Skewness third") +
    scale_x_log10(name = "Threshold (% of P99)", expand = c(0, 0)) +
    coord_cartesian(xlim = c(0.1, 35)) +
    labs(y = "Median local\nsensitivity (days/%P99)") +
    theme_gsl() +
    panel_letter("d")

  top_row <- p_strip + p_heat + p_right + plot_layout(widths = c(0.5, 3, 1), guides = "keep")
  fig <- top_row / p_bottom + plot_layout(heights = c(3, 1), guides = "keep")

  n_low <- sum(derived_site$skewness_third == "low")
  n_mid <- sum(derived_site$skewness_third == "middle")
  n_high <- sum(derived_site$skewness_third == "high")

  save_gsl_figure(
    fig, stem, width_mm = NATURE_WIDTH_DOUBLE_MM, height_mm = 220,
    legend_lines = c(
      sprintf("Figure 2 (supplementary: %s). Same specification as the main Figure 2, this variant only.", variant_label),
      sprintf("Sites per skewness third: low = %d, middle = %d, high = %d (total %d).", n_low, n_mid, n_high, nrow(derived_site))
    ),
    input_sources = c(file.path(GSL_ROOT, "tables", "derived_site.csv"), file.path(GSL_ROOT, "data", "fine_grid.rds")),
    notes = sprintf("Supplementary Figure 2, %s. Terciles: low=%d, middle=%d, high=%d.", variant_label, n_low, n_mid, n_high)
  )
  invisible(NULL)
}

## ============================================================================
## Figure 3 builder (identical structure to 07_fig3.R), parameterized.
## ============================================================================
build_fig3 <- function(gpp_variant, neg_variant, stem, variant_label) {
  derived_site <- derived_site_all |> filter(gpp_variant == !!gpp_variant, neg_gpp_variant == !!neg_variant)
  gpp_col <- GSL_GPP_VARIANTS[[gpp_variant]]
  neg_to_zero <- GSL_NEG_GPP_VARIANTS[[neg_variant]]
  sweep_grid <- seq(5, 20, by = 0.1)

  site_threshold_trend <- function(sid) {
    years <- GSL_SITES$years[GSL_SITES$site_id == sid][[1]]
    P99 <- derived_site$P99[derived_site$site_id == sid]
    t95_pct <- derived_site$t95_pct[derived_site$site_id == sid]
    thresholds_pct <- sort(unique(c(sweep_grid, t95_pct)))
    d <- daily_extract |> filter(site_id == sid, year %in% years)
    d$gpp <- if (neg_to_zero) pmax(d[[gpp_col]], 0) else d[[gpp_col]]
    gpp_by_year <- split(d$gpp, as.character(d$year))
    out <- lapply(thresholds_pct, function(pct) {
      T_abs <- pct / 100 * P99
      gsl_by_year <- vapply(gpp_by_year, function(gy) sum(gy > T_abs, na.rm = TRUE), numeric(1))
      yrs_num <- as.integer(names(gsl_by_year))
      fit <- lm(gsl_by_year ~ yrs_num)
      co <- summary(fit)$coefficients
      tibble(pct_of_P99 = pct, slope = unname(co[2, 1]), p_value = unname(co[2, 4]))
    })
    bind_rows(out) |> mutate(site_id = sid)
  }
  trend_sweep <- bind_rows(lapply(trend_sites, site_threshold_trend))

  panel_a_data <- derived_site |>
    filter(site_id %in% trend_sites) |>
    select(site_id, skewness, group, t95_pct, trend_gsl_z17_days_per_yr, trend_ci_lo, trend_ci_hi) |>
    left_join(
      trend_sweep |> filter(pct_of_P99 >= 5, pct_of_P99 <= 20) |>
        group_by(site_id) |> summarise(range_lo = min(slope), range_hi = max(slope), .groups = "drop"),
      by = "site_id"
    ) |>
    left_join(
      trend_sweep |> left_join(derived_site |> select(site_id, t95_pct), by = "site_id") |>
        group_by(site_id) |> filter(abs(pct_of_P99 - t95_pct) == min(abs(pct_of_P99 - t95_pct))) |>
        slice(1) |> ungroup() |> select(site_id, slope_t95 = slope),
      by = "site_id"
    ) |>
    arrange(skewness) |> mutate(site_id = factor(site_id, levels = site_id))

  p3a <- ggplot(panel_a_data, aes(y = site_id)) +
    geom_vline(xintercept = 0, colour = "grey50", linewidth = nature_lwd(0.25)) +
    geom_segment(aes(x = trend_ci_lo, xend = trend_ci_hi, yend = site_id), colour = "grey75", linewidth = nature_lwd(1)) +
    geom_segment(aes(x = range_lo, xend = range_hi, yend = site_id, colour = group), linewidth = nature_lwd(0.5)) +
    geom_point(aes(x = trend_gsl_z17_days_per_yr, shape = "Z17"), size = 1, colour = "black") +
    geom_point(aes(x = slope_t95, shape = "T95"), size = 1, colour = "black") +
    scale_shape_manual(values = c("Z17" = 16, "T95" = 18), name = NULL) +
    scale_color_gsl_group(name = "Group") +
    labs(x = "GSL trend (days/yr)", y = NULL) +
    theme_gsl() +
    theme(legend.position = "right", legend.box = "vertical", legend.key.height = unit(3, "mm")) +
    panel_letter("a")

  panel_b_data <- panel_a_data |> mutate(
    threshold_range = range_hi - range_lo, ci_width = trend_ci_hi - trend_ci_lo,
    ratio = threshold_range / ci_width
  )
  sign_sig_changes <- trend_sweep |> filter(pct_of_P99 >= 5, pct_of_P99 <= 20) |>
    group_by(site_id) |>
    summarise(sign_changes = n_distinct(sign(slope)) > 1, sig_changes = n_distinct(p_value < 0.05) > 1, .groups = "drop")
  n_sign_or_sig <- sum(sign_sig_changes$sign_changes | sign_sig_changes$sig_changes)
  n_sign <- sum(sign_sig_changes$sign_changes)
  n_sig <- sum(sign_sig_changes$sig_changes)

  p3b <- ggplot(panel_b_data, aes(x = skewness, y = ratio, colour = group)) +
    geom_point(size = 1) +
    scale_color_gsl_group(name = "Group") +
    labs(x = "Skewness", y = "Threshold range / CI width") +
    theme_gsl() +
    theme(legend.position = "right", legend.key.height = unit(3, "mm")) +
    panel_letter("b")

  fig <- p3a / p3b + plot_layout(heights = c(2, 1), guides = "keep")

  save_gsl_figure(
    fig, stem, width_mm = NATURE_WIDTH_DOUBLE_MM, height_mm = 200,
    legend_lines = c(
      sprintf("Figure 3 (supplementary: %s). Same specification as the main Figure 3, this variant only.", variant_label),
      sprintf("Of the %d trend sites: %d sign changes, %d significance changes, %d either, across 5-20%% of P99.",
              length(trend_sites), n_sign, n_sig, n_sign_or_sig)
    ),
    input_sources = c(file.path(GSL_ROOT, "tables", "derived_site.csv"), file.path(GSL_ROOT, "data", "daily_extract.rds")),
    notes = sprintf("Supplementary Figure 3, %s. Sign changes: %d; significance changes: %d; either: %d.",
                     variant_label, n_sign, n_sig, n_sign_or_sig)
  )
  invisible(list(n_sign = n_sign, n_sig = n_sig, n_either = n_sign_or_sig))
}

## ============================================================================
## Build the 4 supplementary figures.
## ============================================================================
build_fig2("supplementary", "main", "fig_02_sensitivity_heatmap_supp_DT", "GPP_DT_VUT_REF, negative GPP zero-floored")
build_fig2("main", "as_delivered", "fig_02_sensitivity_heatmap_supp_asDelivered", "GPP_NT_VUT_REF, negative GPP as-delivered")
r3_dt <- build_fig3("supplementary", "main", "fig_03_trends_supp_DT", "GPP_DT_VUT_REF, negative GPP zero-floored")
r3_asdel <- build_fig3("main", "as_delivered", "fig_03_trends_supp_asDelivered", "GPP_NT_VUT_REF, negative GPP as-delivered")

message("--- 10_supplementary.R: done ---")
message("Fig3 supp DT: sign=", r3_dt$n_sign, " sig=", r3_dt$n_sig, " either=", r3_dt$n_either)
message("Fig3 supp as-delivered: sign=", r3_asdel$n_sign, " sig=", r3_asdel$n_sig, " either=", r3_asdel$n_either)
