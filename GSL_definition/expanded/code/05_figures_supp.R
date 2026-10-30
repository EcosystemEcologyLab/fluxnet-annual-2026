## 05_figures_supp.R — GSL expanded analysis, Supplemental Figures S1-S7.

source("GSL_definition/expanded/code/00_config.R")
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(lubridate); library(purrr)
  library(ggplot2); library(patchwork); library(scales)
})

exp_sites    <- read_csv(file.path(EXP_TABLES, "sites.csv"), show_col_types = FALSE)
exp_site_years <- read_csv(file.path(EXP_TABLES, "site_years.csv"), show_col_types = FALSE)
derived_site <- read_csv(file.path(EXP_TABLES, "derived_site.csv"), show_col_types = FALSE)
derived_site_main <- derived_site |> filter(variant == "main")
derived_site_dt   <- derived_site |> filter(variant == "dt")
derived_site_neg  <- derived_site |> filter(variant == "negdelivered")
bounds_check <- read_csv(file.path(EXP_TABLES, "bounds_check.csv"), show_col_types = FALSE)
fine_grid_summary <- read_csv(file.path(EXP_TABLES, "fine_grid_summary.csv"), show_col_types = FALSE)
cg_pooled    <- read_csv(file.path(EXP_TABLES, "carbon_gain_pooled.csv"), show_col_types = FALSE)
trend_per_site <- read_csv(file.path(EXP_TABLES, "trend_per_site.csv"), show_col_types = FALSE)
fraction_sweep_main <- read_csv(file.path(EXP_TABLES, "fraction_sweep_main.csv"), show_col_types = FALSE)
site_years_list <- split(exp_site_years$year, exp_site_years$site_id)
trend_site_ids  <- names(Filter(function(y) length(y) >= 10L, site_years_list))

## ============================================================================
## S1 -- site/site-year counts by group, Tash's 62 vs the expanded set
## ============================================================================
counts_sites <- bind_rows(
  exp_sites |> filter(in_gsl62) |> count(group, name = "n") |> mutate(set = "GSL 62"),
  exp_sites |> count(group, name = "n") |> mutate(set = "Expanded (151)")
)
counts_site_years <- bind_rows(
  exp_site_years |> inner_join(exp_sites |> filter(in_gsl62) |> select(site_id, group), by = "site_id") |>
    count(group, name = "n") |> mutate(set = "GSL 62"),
  exp_site_years |> left_join(exp_sites |> select(site_id, group), by = "site_id") |>
    count(group, name = "n") |> mutate(set = "Expanded (151)")
)

ps1a <- ggplot(counts_sites, aes(x = group, y = n, fill = set)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  scale_fill_manual(values = c("GSL 62" = "grey40", "Expanded (151)" = "#2C7BB6"), name = NULL) +
  labs(x = NULL, y = "N sites") +
  theme_gsl() + theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "top") +
  gsl_panel_letter("a")

ps1b <- ggplot(counts_site_years, aes(x = group, y = n, fill = set)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  scale_fill_manual(values = c("GSL 62" = "grey40", "Expanded (151)" = "#2C7BB6"), name = NULL) +
  labs(x = NULL, y = "N site-years") +
  theme_gsl() + theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "top") +
  gsl_panel_letter("b")

figS1 <- ps1a | ps1b
cut_sites <- exp_sites$site_id[exp_sites$carbon_src == "CUT"]
recovered_lr <- exp_sites$site_id[exp_sites$is_long_record & !exp_sites$in_gsl62]
save_gsl_figure(
  figS1, "Supplemental Figure S1", height_mm = 140,
  legend_lines = c(
    "Supplemental Figure S1. Site (a) and site-year (b) counts by functional group, GSL 62 vs",
    "the expanded set (151 sites).",
    sprintf("%d of 151 expanded sites are gated on the CUT QC/GPP fallback (never VUT): %s.",
            length(cut_sites), paste(sort(cut_sites), collapse = ", ")),
    sprintf("%d long-record sites (>=15yr, 12 months/yr) not in the GSL 62 are recovered: %s.",
            length(recovered_lr), paste(sort(recovered_lr), collapse = ", "))
  ),
  input_sources = c(file.path(EXP_TABLES, "sites.csv"), file.path(EXP_TABLES, "site_years.csv")),
  notes = ""
)
message("--- S1 done ---")

## ============================================================================
## S2 -- bounds-check spread per site-year at Z17 and T95
## ============================================================================
## geom_histogram(position="identity") with two heavily overlapping series
## blends their fill colours into an indistinguishable third colour --
## geom_freqpoly (step outlines, no fill) keeps Z17 and T95 visually distinct.
ps2a_data <- bounds_check |> mutate(threshold_lab = ifelse(threshold_name == "z17", "Z17", "T95"))
ps2a <- ggplot(ps2a_data, aes(x = spread, colour = threshold_lab)) +
  geom_freqpoly(binwidth = 1, boundary = -0.5, linewidth = nature_lwd(PEERJ_LWD_DATA)) +
  scale_color_manual(values = GSL_MARKER_COLORS, name = NULL, breaks = c("Z17", "T95")) +
  labs(x = "Bounds-check spread (days; gsl_upper - gsl_lower = n_fail_days)", y = "N site-years") +
  theme_gsl() + theme(legend.position = "top") + gsl_panel_letter("a")

site_spread <- bounds_check |> group_by(site_id, threshold_name) |>
  summarise(mean_spread = mean(spread), max_spread = max(spread), .groups = "drop")
large_spread_sites <- site_spread |> filter(threshold_name == "z17") |>
  arrange(desc(mean_spread)) |> slice_head(n = 15) |>
  left_join(exp_sites |> select(site_id, group), by = "site_id") |>
  mutate(site_id = factor(site_id, levels = rev(site_id)))

ps2b <- ggplot(large_spread_sites, aes(x = mean_spread, y = site_id, fill = group)) +
  geom_col() +
  scale_fill_gsl_group(name = "Group") +
  labs(x = "Mean bounds-check spread at Z17 (days)", y = NULL) +
  theme_gsl() + theme(legend.position = "right") + gsl_panel_letter("b")

figS2 <- ps2a / ps2b + plot_layout(heights = c(1, 1.3))
save_gsl_figure(
  figS2, "Supplemental Figure S2", height_mm = 190,
  legend_lines = c(
    "Supplemental Figure S2. Bounds-check spread (GSL if every QC-failing day is forced above",
    "threshold, minus GSL if every such day is forced below -- algebraically exactly n_fail_days,",
    "0-14 by rule 4's R2 N=14 construction), at Z17 and T95, across all expanded-set site-years.",
    "Panel a: frequency polygon (Z17 black, T95 red) -- the two lines coincide exactly, because",
    "n_fail_days (QC pass/fail) does not depend on which GPP threshold is being tested, so the",
    "spread distribution is identical at Z17 and T95 by construction, not a plotting error.",
    "Panel b: the 15 sites with the largest MEAN spread at Z17, by group (flagged as most",
    "sensitive to the QC-failing-day treatment)."
  ),
  input_sources = file.path(EXP_TABLES, "bounds_check.csv"), notes = ""
)
message("--- S2 done ---")

## ============================================================================
## S3 -- nested comparison: GSL 62 overlaid on the expanded set
## ============================================================================
ds3 <- derived_site_main |> mutate(set = ifelse(in_gsl62, "GSL 62", "Expanded (non-62)"))

ps3a <- ggplot(ds3, aes(x = skewness, fill = set)) +
  geom_density(alpha = 0.5, colour = NA) +
  scale_fill_manual(values = c("GSL 62" = "grey40", "Expanded (non-62)" = "#2C7BB6"), name = NULL) +
  labs(x = "Skewness", y = "Density") + theme_gsl() + theme(legend.position = "top") + gsl_panel_letter("a")

ps3b <- ggplot(ds3, aes(x = t95_pct, fill = set)) +
  geom_density(alpha = 0.5, colour = NA) +
  scale_fill_manual(values = c("GSL 62" = "grey40", "Expanded (non-62)" = "#2C7BB6"), name = NULL) +
  labs(x = "T95 (% of P99)", y = "Density") + theme_gsl() + theme(legend.position = "top") + gsl_panel_letter("b")

ds3c <- ds3 |> left_join(fine_grid_summary |> filter(variant == "main") |> select(site_id, sensitivity_at_10pct), by = "site_id")
ps3c <- ggplot(ds3c, aes(x = sensitivity_at_10pct, fill = set)) +
  geom_density(alpha = 0.5, colour = NA) +
  scale_fill_manual(values = c("GSL 62" = "grey40", "Expanded (non-62)" = "#2C7BB6"), name = NULL) +
  labs(x = "Sensitivity at Z17 (days / 1% of P99)", y = "Density") + theme_gsl() + theme(legend.position = "top") + gsl_panel_letter("c")

figS3 <- ps3a / ps3b / ps3c
save_gsl_figure(
  figS3, "Supplemental Figure S3", height_mm = 230,
  legend_lines = c(
    "Supplemental Figure S3. The nested comparison: skewness (a), T95 (b), and sensitivity at Z17",
    "(c) for the GSL 62 (grey) overlaid on the rest of the expanded set (blue, non-62 sites only).",
    "Headline numbers for both sets are in tables/S3_headline.csv."
  ),
  input_sources = c(file.path(EXP_TABLES, "derived_site.csv"), file.path(EXP_TABLES, "fine_grid_summary.csv")),
  notes = ""
)

s3_headline <- ds3c |> group_by(set) |>
  summarise(
    n = n(), mean_skewness = mean(skewness, na.rm = TRUE), median_skewness = median(skewness, na.rm = TRUE),
    mean_t95_pct = mean(t95_pct, na.rm = TRUE), mean_sensitivity_z17 = mean(sensitivity_at_10pct, na.rm = TRUE),
    mean_z17_gsl = mean(z17_mean_gsl, na.rm = TRUE), .groups = "drop"
  )
write_csv(s3_headline, file.path(EXP_TABLES, "S3_headline.csv"))
write_meta(
  file.path(EXP_TABLES, "S3_headline.csv"),
  input_sources = c(file.path(EXP_TABLES, "derived_site.csv"), file.path(EXP_TABLES, "fine_grid_summary.csv")),
  notes = "Headline numbers for Supplemental Figure S3: GSL 62 vs the rest of the expanded set (non-62 sites)."
)
message("--- S3 done ---")

## ============================================================================
## S4 -- per-site trends (current Figure 3a style), expanded trend sites
## ============================================================================
panel_a_data <- derived_site_main |>
  filter(site_id %in% trend_site_ids) |>
  select(site_id, skewness, group) |>
  left_join(
    trend_per_site |> filter(threshold_name == "10pct") |> # "Z17" = the 10% point in trend_sweep's labels
      select(site_id, trend_z17 = slope, ci_lo, ci_hi),
    by = "site_id"
  ) |>
  left_join(
    trend_per_site |> filter(!is.na(threshold_pct)) |> group_by(site_id) |>
      summarise(range_lo = min(slope, na.rm = TRUE), range_hi = max(slope, na.rm = TRUE), .groups = "drop"),
    by = "site_id"
  ) |>
  left_join(trend_per_site |> filter(threshold_name == "t95pct") |> select(site_id, slope_t95 = slope), by = "site_id") |>
  arrange(skewness) |> mutate(site_id = factor(site_id, levels = site_id))

figS4 <- ggplot(panel_a_data, aes(y = site_id)) +
  geom_vline(xintercept = 0, colour = "grey50", linewidth = nature_lwd(PEERJ_LWD_REF)) +
  geom_segment(aes(x = ci_lo, xend = ci_hi, yend = site_id), colour = "grey75", linewidth = nature_lwd(PEERJ_LWD_DATA)) +
  geom_segment(aes(x = range_lo, xend = range_hi, yend = site_id, colour = group), linewidth = nature_lwd(PEERJ_LWD_REF)) +
  geom_point(aes(x = trend_z17, shape = "Z17"), size = 1.6, colour = GSL_MARKER_COLORS[["Z17"]]) +
  geom_point(aes(x = slope_t95, shape = "T95"), size = 1.6, colour = GSL_MARKER_COLORS[["T95"]]) +
  scale_shape_gsl_marker() + scale_color_gsl_group(name = "Group") +
  labs(x = "GSL trend (days/yr)", y = NULL) +
  theme_gsl() + theme(axis.text.y = element_text(size = 6), legend.position = "right") +
  gsl_panel_letter("")

save_gsl_figure(
  figS4, "Supplemental Figure S4", height_mm = 230,
  legend_lines = c(
    "Supplemental Figure S4. Per-site GSL trends, expanded trend sites (>=10 years, n = 67), sorted",
    "by skewness (the expanded-set analogue of the main-paper Figure 3a). Grey bar = 95% CI of the",
    "trend at Z17; coloured bar (by group) = range of trend estimates across 5-20% of P99; black",
    "circle = trend at Z17; red triangle = trend at T95."
  ),
  input_sources = file.path(EXP_TABLES, "trend_per_site.csv"), notes = ""
)
message("--- S4 done ---")

## ============================================================================
## S5 -- Figures 3 and 6 with GPP_DT, thirds under GPP_DT, + as-delivered line
## ============================================================================
fgs_dt <- fine_grid_summary |> filter(variant == "dt") |>
  left_join(derived_site_dt |> select(site_id, skewness, skewness_third), by = "site_id")

ps5a <- ggplot(fgs_dt, aes(x = skewness, y = sensitivity_at_10pct, colour = skewness_third)) +
  geom_point(size = 1.8, alpha = 0.85) +
  scale_color_gsl_skew_third() +
  labs(x = "Skewness (GPP_DT_REF)", y = "Sensitivity at Z17\n(days / 1% of P99, GPP_DT_REF)") +
  theme_gsl() + theme(legend.position = "right") + gsl_panel_letter("a")

trend_z17_compare <- trend_per_site |> filter(threshold_name == "10pct") |>
  select(site_id, trend_main = slope) |>
  inner_join(derived_site_dt |> select(site_id, skewness_third), by = "site_id") |>
  filter(!is.na(skewness_third)) |>
  mutate(skewness_third = factor(skewness_third, levels = GSL_SKEW_THIRD_LEVELS))
## trend_per_site is main-variant only (02_derive.R/03_models.R scope); a full DT-variant
## AR1 trend refit (another ~136 pooled model fits) was out of scope for this check -- shown
## instead: each trend site's own main-variant Z17 trend, coloured by its DT-variant third,
## to see whether the thirds assignment itself is sensitive to the GPP variant.
ps5b <- ggplot(trend_z17_compare, aes(x = skewness_third, y = trend_main, colour = skewness_third)) +
  geom_jitter(width = 0.15, size = 1.8, alpha = 0.7) +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = nature_lwd(PEERJ_LWD_REF)) +
  scale_color_gsl_skew_third() +
  labs(x = "Skewness third (under GPP_DT_REF)", y = "Main-variant (GPP_NT_REF)\nZ17 trend (days/yr)") +
  theme_gsl() + theme(legend.position = "right") + gsl_panel_letter("b")

figS5 <- ps5a / ps5b
dt_vs_nt_third_agree <- mean(
  derived_site_main$skewness_third[match(derived_site_dt$site_id, derived_site_main$site_id)] == derived_site_dt$skewness_third,
  na.rm = TRUE
)
save_gsl_figure(
  figS5, "Supplemental Figure S5", height_mm = 160,
  legend_lines = c(
    "Supplemental Figure S5. Robustness to the GPP variant (GPP_DT_REF in place of GPP_NT_REF).",
    "Panel a: sensitivity at Z17 vs skewness, both computed under GPP_DT_REF, with skewness thirds",
    "recomputed on GPP_DT_REF's own distribution (the full Figure 3 analogue under GPP_DT_REF).",
    "Panel b: each trend site's main-variant (GPP_NT_REF) Z17 trend, grouped by the skewness third",
    "it falls into under GPP_DT_REF -- a full GPP_DT_REF AR(1) pooled-trend refit (Figure 6's",
    "analogue) was out of scope for this check; this instead asks whether the GPP variant changes",
    "which third a site falls into.",
    sprintf("%.0f%% of sites keep the same skewness third under GPP_DT_REF as under GPP_NT_REF.",
            100 * dt_vs_nt_third_agree),
    sprintf(
      "As-delivered negative GPP (one line): mean skewness %.3f (main: %.3f), mean Z17 GSL %.1f d (main: %.1f d).",
      mean(derived_site_neg$skewness, na.rm = TRUE), mean(derived_site_main$skewness, na.rm = TRUE),
      mean(derived_site_neg$z17_mean_gsl, na.rm = TRUE), mean(derived_site_main$z17_mean_gsl, na.rm = TRUE)
    )
  ),
  input_sources = c(file.path(EXP_TABLES, "fine_grid_summary.csv"), file.path(EXP_TABLES, "derived_site.csv"),
                     file.path(EXP_TABLES, "trend_per_site.csv")),
  notes = "Scope reduction documented: no full GPP_DT_REF AR(1) pooled-trend refit (see docs/methods_memo.md)."
)
message("--- S5 done ---")

## ============================================================================
## S6 -- main numbers without the CUT-fallback sites
## ============================================================================
no_cut_sites <- exp_sites$site_id[exp_sites$carbon_src != "CUT"]
cg_pooled_nocut_src <- read_csv(file.path(EXP_TABLES, "carbon_gain_data.csv"), show_col_types = FALSE) |>
  filter(site_id %in% no_cut_sites)

## Refit the headline GPP~GSL (Z17) pooled-by-third slope without CUT sites, for direct comparison.
refit_nocut <- purrr::map_dfr(GSL_SKEW_THIRD_LEVELS, function(th) {
  d <- cg_pooled_nocut_src |> filter(skewness_third == th)
  d <- d[!is.na(d$gsl_z17) & !is.na(d$annual_gpp), ]
  d$site_id <- factor(d$site_id)
  if (n_distinct(d$site_id) < 3) return(tibble(third = th, slope = NA_real_, se = NA_real_))
  fit <- tryCatch(
    nlme::lme(annual_gpp ~ gsl_z17, random = ~ gsl_z17 | site_id, data = d, method = "REML",
              control = nlme::lmeControl(maxIter = 300, msMaxIter = 300, opt = "nlminb")),
    error = function(e) NULL
  )
  if (is.null(fit)) return(tibble(third = th, slope = NA_real_, se = NA_real_))
  co <- summary(fit)$tTable
  tibble(third = th, slope = unname(co["gsl_z17", "Value"]), se = unname(co["gsl_z17", "Std.Error"]))
})

compare_cut <- bind_rows(
  cg_pooled |> filter(predictor == "gsl_z17", response == "annual_gpp", subset %in% paste0("third_", GSL_SKEW_THIRD_LEVELS)) |>
    mutate(third = sub("third_", "", subset)) |> select(third, slope, se) |> mutate(set = "All 151 (with CUT)"),
  refit_nocut |> mutate(set = "Without CUT-fallback sites")
) |> mutate(third = factor(third, levels = GSL_SKEW_THIRD_LEVELS))

figS6 <- ggplot(compare_cut, aes(x = third, y = slope, colour = set)) +
  geom_errorbar(aes(ymin = slope - 1.96 * se, ymax = slope + 1.96 * se), position = position_dodge(width = 0.4), width = 0.15) +
  geom_point(position = position_dodge(width = 0.4), size = 2) +
  scale_color_manual(values = c("All 151 (with CUT)" = "#2C7BB6", "Without CUT-fallback sites" = "#D55E00"), name = NULL) +
  labs(x = "Skewness third", y = "Pooled GPP~GSL slope at Z17\n(gC/m2 per day)") +
  theme_gsl() + theme(legend.position = "top") + gsl_panel_letter("")

save_gsl_figure(
  figS6, "Supplemental Figure S6", height_mm = 130,
  legend_lines = c(
    "Supplemental Figure S6. The headline carbon-gain result (pooled GPP~GSL slope at Z17, by",
    "skewness third) with vs without the 12 CUT-fallback sites, to check the VUT/CUT fallback",
    "itself is not driving the main result.",
    sprintf("%d of 151 sites are CUT-fallback.", length(cut_sites))
  ),
  input_sources = c(file.path(EXP_TABLES, "carbon_gain_pooled.csv"), file.path(EXP_TABLES, "carbon_gain_data.csv")),
  notes = ""
)
message("--- S6 done ---")

## ============================================================================
## S7 -- GSL against target fraction, all sites
## ============================================================================
fsw <- fraction_sweep_main |> left_join(exp_sites |> select(site_id, group), by = "site_id")
figS7 <- ggplot(fsw, aes(x = target_fraction, y = gsl, group = site_id, colour = group)) +
  geom_line(alpha = 0.35, linewidth = nature_lwd(0.6)) +
  scale_color_gsl_group(name = "Group") +
  labs(x = "Target fraction of annual GPP included", y = "GSL (days)") +
  theme_gsl() + theme(legend.position = "right") + gsl_panel_letter("")

save_gsl_figure(
  figS7, "Supplemental Figure S7", height_mm = 140,
  legend_lines = c(
    "Supplemental Figure S7. GSL against the target included-GPP fraction (0.80-0.99), one line",
    "per site (all 151, main variant), coloured by functional group."
  ),
  input_sources = file.path(EXP_TABLES, "fraction_sweep_main.csv"), notes = ""
)
message("--- S7 done ---")

message("--- 05_figures_supp.R: S1-S7 done ---")
