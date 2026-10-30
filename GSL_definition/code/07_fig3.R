## 07_fig3.R — GSL figure rebuild, Figure 3: consequences for GSL trends.
## Main run only, 21 trend sites (>=10yr after d80 screening -- unchanged
## from the fixed 62-site table, since Stage 1 showed d80 keeps all 505
## site-years). Threshold range: default 5-20% of P99.
##
## 03_derive.R only persisted POOLED mean GSL/F per threshold (fine_grid.rds),
## not per-year GSL across a threshold sweep -- the trend-vs-threshold curve
## this figure needs is a new quantity, computed here directly from
## daily_extract.rds. P99 and skewness are REUSED from derived_site.csv, not
## recomputed.

source("GSL_definition/code/00_config.R")
source("GSL_definition/code/fig_helpers.R")
suppressPackageStartupMessages({ library(dplyr); library(readr); library(tidyr); library(lubridate) })

derived_site <- read_csv(file.path(GSL_ROOT, "tables", "derived_site.csv"), show_col_types = FALSE) |>
  filter(gpp_variant == "main", neg_gpp_variant == "main") |>
  left_join(GSL_SITES |> select(site_id, group), by = "site_id")

daily_extract <- readRDS(file.path(GSL_ROOT, "data", "daily_extract.rds"))
daily_extract$TIMESTAMP <- as.Date(as.character(daily_extract$TIMESTAMP))
daily_extract$year <- lubridate::year(daily_extract$TIMESTAMP)

trend_sites <- GSL_SITES_GE10YR
stopifnot(length(trend_sites) == 21L)
message("Trend sites (>=10yr after screening): ", length(trend_sites))

gpp_col <- GSL_GPP_VARIANTS[["main"]]

## ---- Per-threshold trend slope + p-value, 5-20% of P99 (+ each site's own
## T95 if outside that range), per trend site. ----
sweep_grid <- seq(5, 20, by = 0.1) # 151 points; fine enough for a trend range, coarser than the
                                    # 0.01% fine grid since this requires an lm() fit per point

site_threshold_trend <- function(sid) {
  years <- GSL_SITES$years[GSL_SITES$site_id == sid][[1]]
  P99 <- derived_site$P99[derived_site$site_id == sid]
  t95_pct <- derived_site$t95_pct[derived_site$site_id == sid]
  thresholds_pct <- sort(unique(c(sweep_grid, t95_pct)))

  d <- daily_extract |> filter(site_id == sid, year %in% years)
  d$gpp <- pmax(d[[gpp_col]], 0)
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

## ---- Panel a data: per-site trend range (5-20%), Z17 CI, Z17 and T95 dots -
panel_a_data <- derived_site |>
  filter(site_id %in% trend_sites) |>
  select(site_id, skewness, group, t95_pct, mean_gsl_z17, trend_gsl_z17_days_per_yr,
         trend_ci_lo, trend_ci_hi) |>
  left_join(
    trend_sweep |> filter(pct_of_P99 >= 5, pct_of_P99 <= 20) |>
      group_by(site_id) |> summarise(range_lo = min(slope), range_hi = max(slope), .groups = "drop"),
    by = "site_id"
  ) |>
  left_join(
    trend_sweep |>
      left_join(derived_site |> select(site_id, t95_pct), by = "site_id") |>
      group_by(site_id) |>
      filter(abs(pct_of_P99 - t95_pct) == min(abs(pct_of_P99 - t95_pct))) |>
      slice(1) |> ungroup() |> select(site_id, slope_t95 = slope),
    by = "site_id"
  ) |>
  arrange(skewness) |>
  mutate(site_id = factor(site_id, levels = site_id))

p3a <- ggplot(panel_a_data, aes(y = site_id)) +
  geom_vline(xintercept = 0, colour = "grey50", linewidth = nature_lwd(0.25)) +
  geom_segment(aes(x = trend_ci_lo, xend = trend_ci_hi, yend = site_id),
               colour = "grey75", linewidth = nature_lwd(1)) +
  geom_segment(aes(x = range_lo, xend = range_hi, yend = site_id, colour = group),
               linewidth = nature_lwd(0.5)) +
  geom_point(aes(x = trend_gsl_z17_days_per_yr, shape = "Z17"), size = 1, colour = "black") +
  geom_point(aes(x = slope_t95, shape = "T95"), size = 1, colour = "black") +
  scale_shape_manual(values = c("Z17" = 16, "T95" = 18), name = NULL) +
  scale_color_gsl_group(name = "Group") +
  labs(x = "GSL trend (days/yr)", y = NULL) +
  theme_gsl() +
  theme(legend.position = "right", legend.box = "vertical", legend.key.height = unit(3, "mm")) +
  panel_letter("a")

## ---- Panel b: ratio of threshold range to CI width vs. skewness ----------
panel_b_data <- panel_a_data |>
  mutate(
    threshold_range = range_hi - range_lo,
    ci_width = trend_ci_hi - trend_ci_lo,
    ratio = threshold_range / ci_width
  )

## Sign/significance changes across the 5-20% sweep, per site.
sign_sig_changes <- trend_sweep |>
  filter(pct_of_P99 >= 5, pct_of_P99 <= 20) |>
  group_by(site_id) |>
  summarise(
    sign_changes = n_distinct(sign(slope)) > 1,
    sig_changes = n_distinct(p_value < 0.05) > 1,
    .groups = "drop"
  )
n_sign_or_sig_change <- sign_sig_changes |>
  filter(sign_changes | sig_changes) |> nrow()
n_sign_change <- sum(sign_sig_changes$sign_changes)
n_sig_change <- sum(sign_sig_changes$sig_changes)

p3b <- ggplot(panel_b_data, aes(x = skewness, y = ratio, colour = group)) +
  geom_point(size = 1) +
  scale_color_gsl_group(name = "Group") +
  labs(x = "Skewness", y = "Threshold range / CI width") +
  theme_gsl() +
  theme(legend.position = "right", legend.key.height = unit(3, "mm")) +
  panel_letter("b")

## ---- Assemble -------------------------------------------------------------
fig3 <- p3a / p3b + plot_layout(heights = c(2, 1), guides = "keep")

legend_lines <- c(
  "Figure 3. Consequences for GSL trends vs. sampling uncertainty (main run: GPP_NT_VUT_REF,",
  "negative GPP set to 0, year screening d80, GSL counts GPP > T).",
  sprintf("Trend sites: %d sites with >= 10 years after screening (unchanged from the ", length(trend_sites)),
  "framework's 21, since d80 screening keeps all 505 site-years -- see docs/methods_memo.md).",
  "Threshold range: 5-20% of P99 (framework default).",
  paste0(
    "Panel a: one row per site, sorted by skewness. Pale grey bar = 95% CI of the trend at Z17's ",
    "threshold (10% of P99); colored bar (by functional group) = range of trend estimates across ",
    "5-20% of P99; circle = trend at Z17; diamond = trend at T95; vertical line at zero."
  ),
  "Panel b: ratio of the threshold-range bar's width to the Z17 CI's width, vs. skewness, by group.",
  sprintf(
    "Of the %d trend sites: %d have a sign change in the trend across 5-20%% of P99, %d have a ",
    length(trend_sites), n_sign_change, n_sig_change
  ),
  sprintf("significance change (p<0.05 threshold), %d have either.", n_sign_or_sig_change)
)

save_gsl_figure(
  fig3, "fig_03_trends", width_mm = NATURE_WIDTH_DOUBLE_MM, height_mm = 200,
  legend_lines = legend_lines,
  input_sources = c(file.path(GSL_ROOT, "tables", "derived_site.csv"),
                     file.path(GSL_ROOT, "data", "daily_extract.rds")),
  notes = sprintf(
    "21 trend sites, 5-20%% of P99 threshold range. Sign changes: %d; significance changes: %d; either: %d.",
    n_sign_change, n_sig_change, n_sign_or_sig_change
  )
)

message("--- 07_fig3.R: done --- n_sign_change=", n_sign_change, " n_sig_change=", n_sig_change,
        " n_either=", n_sign_or_sig_change)
