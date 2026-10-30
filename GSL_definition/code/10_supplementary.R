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
## Figure 2 builder (identical structure/fixes to 06_fig2.R -- log-uniform
## binning to remove low-threshold striping, |local_sensitivity| with
## darker = more sensitive, shared Z17/T95 marker convention via
## fig_helpers.R), parameterized by variant.
## ============================================================================
build_fig2 <- function(gpp_variant, neg_variant, stem, variant_label) {
  derived_site <- derived_site_all |> filter(gpp_variant == !!gpp_variant, neg_gpp_variant == !!neg_variant)
  fine_grid <- fine_grid_all |> filter(gpp_variant == !!gpp_variant, neg_gpp_variant == !!neg_variant,
                                         pct_of_P99 >= 0.5, pct_of_P99 <= 35)
  site_order <- derived_site |> arrange(skewness) |> pull(site_id)
  derived_site$site_id <- factor(derived_site$site_id, levels = site_order)
  fine_grid$site_id <- factor(fine_grid$site_id, levels = site_order)
  stopifnot(nlevels(derived_site$site_id) == nrow(derived_site))

  ## Same log-uniform binning as 06_fig2.R, shared by the heatmap and the
  ## bottom panel -- see that script for why (fixes low-threshold striping,
  ## doubles as the smoothed bottom-panel view). x-axis starts at 0.5% of
  ## P99, not 0.1% (PeerJ restyle).
  N_BINS <- 35
  log_edges <- 10^seq(log10(0.5), log10(35), length.out = N_BINS + 1)
  bin_of <- function(pct) pmin(findInterval(pct, log_edges, all.inside = TRUE), N_BINS)
  fine_grid <- fine_grid |>
    mutate(bin = bin_of(pct_of_P99), bin_lo = log_edges[bin], bin_hi = log_edges[bin + 1],
           bin_mid = sqrt(bin_lo * bin_hi), bin_width = bin_hi - bin_lo)

  heat_data <- fine_grid |>
    group_by(site_id, bin, bin_mid, bin_width) |>
    summarise(abs_sensitivity = mean(abs(local_sensitivity), na.rm = TRUE), .groups = "drop")
  sensitivity_cap <- unname(quantile(heat_data$abs_sensitivity, 0.95, na.rm = TRUE))

  p_heat <- ggplot(heat_data, aes(x = bin_mid, y = site_id, fill = abs_sensitivity)) +
    geom_tile(aes(width = bin_width), height = 1) +
    scale_fill_gsl_sensitivity(heat_data$abs_sensitivity) +
    geom_vline(xintercept = 10, colour = "grey30", linewidth = nature_lwd(PEERJ_LWD_REF)) +
    geom_point(data = derived_site, aes(x = 10, y = site_id, shape = "Z17"), inherit.aes = FALSE,
               colour = GSL_MARKER_COLORS[["Z17"]], size = 1) +
    geom_point(data = derived_site, aes(x = t95_pct, y = site_id, shape = "T95"), inherit.aes = FALSE,
               colour = GSL_MARKER_COLORS[["T95"]], size = 1) +
    scale_shape_gsl_marker() +
    scale_x_log10(name = "Threshold (% of P99)", expand = c(0, 0)) +
    coord_cartesian(xlim = c(0.5, 35)) +
    scale_y_discrete(expand = c(0, 0)) +
    theme_gsl() +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank(),
          legend.key.height = unit(4, "mm"), legend.key.width = unit(3, "mm")) +
    gsl_panel_letter("b", x = 0.55, hjust = 0)

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

  right_data <- derived_site |> select(site_id, z17_sensitivity, t95_sensitivity) |>
    mutate(z17_sensitivity = abs(z17_sensitivity), t95_sensitivity = abs(t95_sensitivity))
  p_right <- ggplot(right_data, aes(y = site_id)) +
    geom_segment(aes(x = z17_sensitivity, xend = t95_sensitivity, yend = site_id),
                 colour = "grey60", linewidth = nature_lwd(PEERJ_LWD_REF)) +
    geom_point(aes(x = z17_sensitivity, shape = "Z17"), size = 1.4, colour = GSL_MARKER_COLORS[["Z17"]]) +
    geom_point(aes(x = t95_sensitivity, shape = "T95"), size = 1.4, colour = GSL_MARKER_COLORS[["T95"]]) +
    scale_shape_gsl_marker() +
    labs(x = "Sensitivity\n(days/%P99,\nabs. value)", y = NULL) +
    ## Panel is narrow -- with PeerJ's larger tick text, the default 4-5
    ## breaks overlap; 3 keeps them legible (see 06_fig2.R).
    scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) +
    theme_gsl() +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          legend.position = c(0.68, 0.9), legend.key.width = unit(2, "mm"),
          legend.text = element_text(size = PEERJ_BASE_PT),
          legend.background = element_rect(fill = "white", colour = NA), legend.key = element_blank()) +
    gsl_panel_letter("c")

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
    ## Same panel_letter() x=-Inf/log-scale fix as 06_fig2.R.
    gsl_panel_letter("d", x = 0.55, hjust = 0)

  top_row <- p_strip + p_heat + p_right + plot_layout(widths = c(0.5, 3, 1), guides = "keep")
  fig <- top_row / p_bottom + plot_layout(heights = c(3, 1), guides = "keep")

  n_low <- sum(derived_site$skewness_third == "low")
  n_mid <- sum(derived_site$skewness_third == "middle")
  n_high <- sum(derived_site$skewness_third == "high")

  save_gsl_figure(
    fig, stem, width_mm = PEERJ_FIG_WIDTH_MM, height_mm = 220,
    legend_lines = c(
      sprintf("%s. Same specification as the main Figure 2, this variant only --", stem),
      sprintf("GPP variant: %s.", variant_label),
      sprintf(
        "|local sensitivity| (absolute value, darker = more sensitive), log-binned thresholds (%d bins, 0.5-35%% of P99),",
        N_BINS
      ),
      sprintf("colour scale capped at the 95th percentile of the plotted values (%.1f days/%%P99; top colour '>= %.1f').", sensitivity_cap, sensitivity_cap),
      "Z17 (black circle) and T95 (red triangle) marker convention, same as the main Figure 2.",
      "Panel a skewness third / panel d line colours: same discrete palette as the main Figure 2.",
      sprintf("Sites per skewness third: low = %d, middle = %d, high = %d (total %d).", n_low, n_mid, n_high, nrow(derived_site))
    ),
    input_sources = c(file.path(GSL_ROOT, "tables", "derived_site.csv"), file.path(GSL_ROOT, "data", "fine_grid.rds")),
    notes = sprintf("%s. %s. Terciles: low=%d, middle=%d, high=%d. Sensitivity shown as |value|, log-binned (%d bins, 0.5-35%% of P99), colour scale capped at the 95th percentile (%.2f days/%%P99).", stem, variant_label, n_low, n_mid, n_high, N_BINS, sensitivity_cap)
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
      ## See 07_fig3.R: a year with zero non-NA daily GPP must contribute NA,
      ## not a fabricated GSL=0 (sum(..., na.rm=TRUE) over an all-NA vector
      ## silently returns 0) -- fixed here identically (CH-Dav 2008 bug).
      gsl_by_year <- vapply(gpp_by_year, function(gy) {
        gy <- gy[!is.na(gy)]
        if (length(gy) == 0) NA_real_ else sum(gy > T_abs)
      }, numeric(1))
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

  ## Same consistency check as 07_fig3.R: Z17's trend must lie within the
  ## sweep's own [range_lo, range_hi] for every trend site.
  eps <- 1e-6
  consistency_check <- panel_a_data |>
    mutate(within_range = trend_gsl_z17_days_per_yr >= (range_lo - eps) &
             trend_gsl_z17_days_per_yr <= (range_hi + eps))
  if (!all(consistency_check$within_range)) {
    message("Z17 trend outside its own range bar (", variant_label, "): ",
            paste(consistency_check$site_id[!consistency_check$within_range], collapse = ", "))
  }
  stopifnot(all(consistency_check$within_range))

  p3a <- ggplot(panel_a_data, aes(y = site_id)) +
    geom_vline(xintercept = 0, colour = "grey50", linewidth = nature_lwd(PEERJ_LWD_REF)) +
    geom_segment(aes(x = trend_ci_lo, xend = trend_ci_hi, yend = site_id), colour = "grey75", linewidth = nature_lwd(PEERJ_LWD_DATA)) +
    geom_segment(aes(x = range_lo, xend = range_hi, yend = site_id, colour = group), linewidth = nature_lwd(PEERJ_LWD_REF)) +
    geom_point(aes(x = trend_gsl_z17_days_per_yr, shape = "Z17"), size = 2, colour = GSL_MARKER_COLORS[["Z17"]]) +
    geom_point(aes(x = slope_t95, shape = "T95"), size = 2, colour = GSL_MARKER_COLORS[["T95"]]) +
    scale_shape_gsl_marker() +
    scale_color_gsl_group(name = "Group") +
    labs(x = "GSL trend (days/yr)", y = NULL) +
    theme_gsl() +
    theme(legend.position = "right", legend.box = "vertical", legend.key.height = unit(4, "mm")) +
    gsl_panel_letter("a")

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
    geom_point(size = 2) +
    scale_color_gsl_group(name = "Group") +
    labs(x = "Skewness", y = "Threshold range / CI width") +
    theme_gsl() +
    theme(legend.position = "right", legend.key.height = unit(4, "mm")) +
    gsl_panel_letter("b")

  fig <- p3a / p3b + plot_layout(heights = c(2, 1), guides = "keep")

  save_gsl_figure(
    fig, stem, width_mm = PEERJ_FIG_WIDTH_MM, height_mm = 220,
    legend_lines = c(
      sprintf("%s. Same specification as the main Figure 3, this variant only.", stem),
      sprintf("GPP variant: %s.", variant_label),
      sprintf("Of the %d trend sites: %d sign changes, %d significance changes, %d either, across 5-20%% of P99.",
              length(trend_sites), n_sign, n_sig, n_sign_or_sig)
    ),
    input_sources = c(file.path(GSL_ROOT, "tables", "derived_site.csv"), file.path(GSL_ROOT, "data", "daily_extract.rds")),
    notes = sprintf("%s. %s. Sign changes: %d; significance changes: %d; either: %d.",
                     stem, variant_label, n_sign, n_sig, n_sign_or_sig)
  )
  invisible(list(n_sign = n_sign, n_sig = n_sig, n_either = n_sign_or_sig))
}

## ============================================================================
## Build the 4 supplementary figures. S-numbers assigned in citation order
## (the order built here, matching docs/data_and_code_availability.md and
## the GSL_definition/README) -- PeerJ naming: "Supplemental Figure SN".
## ============================================================================
build_fig2("supplementary", "main", "Supplemental Figure S1", "GPP_DT_VUT_REF, negative GPP zero-floored")
build_fig2("main", "as_delivered", "Supplemental Figure S2", "GPP_NT_VUT_REF, negative GPP as-delivered")
r3_dt <- build_fig3("supplementary", "main", "Supplemental Figure S3", "GPP_DT_VUT_REF, negative GPP zero-floored")
r3_asdel <- build_fig3("main", "as_delivered", "Supplemental Figure S4", "GPP_NT_VUT_REF, negative GPP as-delivered")

message("--- 10_supplementary.R: done ---")
message("Fig3 supp DT: sign=", r3_dt$n_sign, " sig=", r3_dt$n_sig, " either=", r3_dt$n_either)
message("Fig3 supp as-delivered: sign=", r3_asdel$n_sign, " sig=", r3_asdel$n_sig, " either=", r3_asdel$n_either)
