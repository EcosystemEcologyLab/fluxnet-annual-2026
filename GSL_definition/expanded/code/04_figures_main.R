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

## Item 2: classify days using the SCREENED GPP_NT_REF (winter-screened days
## forced to 0, same as the 'main' variant) -- join against
## winter_screen_days.csv rather than recomputing the screen's own
## criterion here. daily_extract (loaded above) is the UNSCREENED extract.
winter_screen_days <- read_csv(file.path(EXP_TABLES, "winter_screen_days.csv"), show_col_types = FALSE) |>
  mutate(is_screened = TRUE) |> select(site_id, TIMESTAMP, is_screened)

day_class_one <- function(sid) {
  P99 <- derived_site$P99[derived_site$site_id == sid]
  t95pct <- derived_site$t95_pct[derived_site$site_id == sid]
  d <- daily_extract |> filter(site_id == sid, year %in% site_years_list[[sid]]) |>
    left_join(winter_screen_days, by = c("site_id", "TIMESTAMP")) |>
    mutate(gpp_use = ifelse(coalesce(is_screened, FALSE), 0, pmax(GPP_NT_REF, 0)),
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

## Panel-letter fix: restrict the "a" annotation to a single facet (the
## first site) instead of annotate()'s default of repeating it in every
## facet panel -- a one-row data frame with an explicit site_id, drawn via
## geom_text (facet-aware), not annotate() (facet-naive).
fig1a_letter <- tibble(site_id = FIG1_SITES[1], doy = 10, year = factor(max(fig1a_data$year[fig1a_data$site_id == FIG1_SITES[1]])))

p1a <- ggplot(fig1a_data, aes(x = doy, y = factor(year), fill = category)) +
  geom_tile() +
  facet_wrap(~ site_id, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = FIG1_CAT_COLORS, name = "Included by") + # drop=TRUE (default): "Z17 only" is
  ## structurally empty at both illustrative sites (T95's % <= Z17's here, so above_z17 implies
  ## above_t95) and no longer shows an empty legend entry.
  scale_x_continuous(breaks = c(1, 91, 182, 274, 366), labels = c("Jan", "Apr", "Jul", "Oct", "Jan")) +
  labs(x = "Day of year", y = NULL) +
  theme_gsl() +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold")) +
  geom_text(data = fig1a_letter, aes(x = 5, y = year, label = "a"), inherit.aes = FALSE,
            fontface = "bold", size = PEERJ_PANEL_LETTER_PT / .pt, hjust = 0, vjust = 1)

fig1b_data <- fine_grid_main |> filter(site_id %in% FIG1_SITES, pct_of_P99 <= 50)
fig1b_marks <- derived_site |> filter(site_id %in% FIG1_SITES) |>
  select(site_id, z17_pct, z17_mean_gsl, t95_pct, t95_mean_gsl) |>
  pivot_longer(c(z17_pct, t95_pct), names_to = "which", values_to = "pct") |>
  mutate(gsl = ifelse(which == "z17_pct", z17_mean_gsl, t95_mean_gsl),
         marker = ifelse(which == "z17_pct", "Z17", "T95")) |>
  select(site_id, pct, gsl, marker)

p1b <- ggplot(fig1b_data, aes(x = pct_of_P99, y = mean_gsl, colour = site_id)) +
  geom_line(linewidth = nature_lwd(PEERJ_LWD_DATA)) +
  ## T95/Z17 marks drawn as two literal-colour layers (not an aes(colour=)
  ## mapping, which would need a second colour scale via ggnewscale since
  ## `colour` already maps to site_id above) -- GSL_MARKER_COLORS[["Z17"]]
  ## is black, ["T95"] is red, matching scale_shape_gsl_marker()'s legend
  ## override.aes exactly (previously both marks were hard-coded black).
  geom_point(data = fig1b_marks |> filter(marker == "Z17"), aes(x = pct, y = gsl, shape = marker),
             colour = GSL_MARKER_COLORS[["Z17"]], size = 2, inherit.aes = FALSE) +
  geom_point(data = fig1b_marks |> filter(marker == "T95"), aes(x = pct, y = gsl, shape = marker),
             colour = GSL_MARKER_COLORS[["T95"]], size = 2, inherit.aes = FALSE) +
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
    "mosaic; see docs/report_back_2026-10-30.md for why these two). Winter-screened days (item 2)",
    "are shown as 'neither' (GPP_NT_REF forced to 0).",
    "Panel a: each day of each passing year, coloured by whether it is included in the growing",
    "season under both Z17 and T95, one only, or neither.",
    "Panel b: mean GSL against threshold (% of P99, 0-50%) for both sites; black circle/red",
    "triangle mark each site's own Z17/T95 point on its curve."
  ),
  input_sources = c(file.path(EXP_DATA, "daily_extract.rds"), file.path(EXP_DATA, "fine_grid_main.rds"),
                     file.path(EXP_TABLES, "derived_site.csv"), file.path(EXP_TABLES, "winter_screen_days.csv")),
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

## Item 6: plot absolute values throughout Figure 3 (local_sensitivity is
## already the +/-1% window from 02_derive.R; abs() applied here is purely
## a display choice -- the underlying signed values stay in the tables).
fg_third <- fine_grid_main |>
  filter(pct_of_P99 >= 0.1, pct_of_P99 <= 35) |>
  left_join(derived_site |> select(site_id, skewness_third), by = "site_id") |>
  filter(!is.na(skewness_third)) |>
  group_by(skewness_third, pct_of_P99) |>
  summarise(median_sensitivity = median(abs(local_sensitivity), na.rm = TRUE), .groups = "drop")

p3a <- ggplot(fg_third, aes(x = pct_of_P99, y = median_sensitivity, colour = skewness_third)) +
  geom_line(linewidth = nature_lwd(PEERJ_LWD_DATA)) +
  scale_x_log10() +
  scale_color_gsl_skew_third() +
  labs(x = "Threshold (% of P99, log scale)", y = "Median |sensitivity|\n(days / 1% of P99)") +
  theme_gsl() + theme(legend.position = "right") +
  gsl_panel_letter("a", x = 0.11, hjust = 0)

p3b <- ggplot(fine_grid_summary, aes(x = skewness, y = abs(sensitivity_at_10pct), colour = skewness_third)) +
  geom_point(size = 1.8, alpha = 0.85) +
  scale_color_gsl_skew_third() +
  labs(x = "Skewness", y = "|Sensitivity| at Z17\n(days / 1% of P99)") +
  theme_gsl() + theme(legend.position = "right") + gsl_panel_letter("b")

cus_data <- derived_site |> select(site_id, skewness, z17_common_unit_sensitivity, t95_common_unit_sensitivity) |>
  pivot_longer(c(z17_common_unit_sensitivity, t95_common_unit_sensitivity), names_to = "which", values_to = "cus") |>
  mutate(marker = ifelse(which == "z17_common_unit_sensitivity", "Z17", "T95"), cus = abs(cus))

p3c <- ggplot(cus_data, aes(x = skewness, y = cus, colour = marker, shape = marker)) +
  geom_point(size = 1.8, alpha = 0.85) +
  scale_color_manual(values = GSL_MARKER_COLORS, name = NULL, breaks = c("Z17", "T95")) +
  scale_shape_gsl_marker() +
  labs(x = "Skewness", y = "|Days / 1% of\nincluded GPP|") +
  theme_gsl() + theme(legend.position = "right") + gsl_panel_letter("c")

fig3 <- p3a / p3b / p3c
cor_b <- cor(fine_grid_summary$skewness, abs(fine_grid_summary$sensitivity_at_10pct), use = "complete.obs")
cor_c_z17 <- cor(derived_site$skewness, abs(derived_site$z17_common_unit_sensitivity), use = "complete.obs")
save_gsl_figure(
  fig3, "Figure 3", height_mm = 230,
  legend_lines = c(
    "Figure 3. Threshold sensitivity in two units (expanded set, main run). Item 6: local",
    "sensitivity is a +/-1% of P99 centred difference (widened from +/-0.05%); all three panels",
    "plot ABSOLUTE VALUES (signed values are in fine_grid_summary.csv/derived_site.csv).",
    "Panel a: median |sensitivity| (days lost per 1% of P99), 0.1-35% of P99 (log axis), by skewness third.",
    "Panel b: |sensitivity| at Z17 specifically, against skewness (one point per site; r = ",
    sprintf("%.2f).", cor_b),
    "Panel c: |days per 1% of included GPP| (the common-unit sensitivity |dGSL/dF x 0.01|), Z17 and",
    "T95, against skewness (r, Z17 = ", sprintf("%.2f).", cor_c_z17)
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

## Item: label the two sites with 140-160 gap days (either Z17 or T95).
gap_label_data <- gap_data |> filter(mean_gap_days >= 140, mean_gap_days <= 160)
message("Figure 4b: ", nrow(gap_label_data), " site x threshold point(s) in the 140-160 gap-day range: ",
        paste(sprintf("%s (%s, %.0f d)", gap_label_data$site_id, gap_label_data$marker, gap_label_data$mean_gap_days), collapse = "; "))

p4b <- ggplot(gap_data, aes(x = skewness, y = mean_gap_days, colour = marker, shape = marker)) +
  geom_point(size = 1.8, alpha = 0.85) +
  ggrepel::geom_text_repel(
    data = gap_label_data, aes(label = site_id), inherit.aes = TRUE, show.legend = FALSE,
    size = PEERJ_ANNOTATION_PT / .pt, seed = 1, min.segment.length = 0, colour = "grey20"
  ) +
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
    "against skewness, at Z17 and T95; the two site x threshold points with 140-160 gap days are",
    "labelled by site ID."
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
## Item: "no repeated panel letters across facets" -- geom_text with a
## single-row data frame pinned to one specific facet cell (not
## gsl_panel_letter()/annotate(), which repeats in every facet panel).
make_fig5_panel <- function(predictors, ylab, panel_letter, date_levels, clamp_outliers = FALSE) {
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

  if (clamp_outliers) {
    ## Item: panel b axes limited to the bulk of site slopes, outliers
    ## marked at the edge. "Bulk" = the Tukey (1.5x IQR) range of the
    ## PER-SITE slopes themselves, per facet (response_lab x date_lab) --
    ## the same outlier convention used elsewhere in this project (session
    ## 1 check 2). Outliers are CLAMPED to the Tukey boundary and drawn as
    ## an open triangle instead of a filled circle; free_y then auto-scales
    ## to the (now outlier-free) data range, so outliers sit pinned at the
    ## panel edge without stretching the axis to accommodate them.
    bulk <- site |> group_by(response_lab, date_lab) |>
      summarise(
        q1 = quantile(slope, 0.25, na.rm = TRUE), q3 = quantile(slope, 0.75, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(iqr = q3 - q1, lo_pad = q1 - 1.5 * iqr, hi_pad = q3 + 1.5 * iqr)
    site <- site |> left_join(bulk, by = c("response_lab", "date_lab")) |>
      mutate(is_outlier = !is.na(slope) & (slope < lo_pad | slope > hi_pad),
             slope_clamped = pmin(pmax(slope, lo_pad), hi_pad))
    n_outliers <- sum(site$is_outlier, na.rm = TRUE)
    message("Figure 5b: ", n_outliers, " of ", sum(!is.na(site$slope)),
            " per-site slope(s) clamped to the panel edge (beyond 1.5x IQR of that facet's site slopes).")
  } else {
    site <- site |> mutate(slope_clamped = slope, is_outlier = FALSE)
  }

  ## date_lab must stay a factor WITH THE SAME LEVELS as pooled$date_lab --
  ## a plain character here reordered Figure 5b's facet columns
  ## alphabetically (End date before Start date) across the two layers.
  letter_data <- tibble(third = factor(GSL_SKEW_THIRD_LEVELS[1], levels = GSL_SKEW_THIRD_LEVELS),
                         response_lab = unique(pooled$response_lab)[1],
                         date_lab = factor(date_levels[1], levels = date_levels))

  ggplot(pooled, aes(x = third, y = slope, colour = threshold)) +
    geom_jitter(data = site, aes(y = slope_clamped, shape = is_outlier), width = 0.12, size = 0.8, alpha = 0.4) +
    scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 2), guide = "none") +
    geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), position = position_dodge(width = 0.5), width = 0.15,
                  linewidth = nature_lwd(PEERJ_LWD_REF)) +
    geom_point(position = position_dodge(width = 0.5), size = 2) +
    facet_grid(response_lab ~ date_lab, scales = "free_y") +
    scale_color_manual(values = GSL_MARKER_COLORS, name = NULL, breaks = c("Z17", "T95")) +
    labs(x = "Skewness third", y = ylab) +
    theme_gsl() + theme(legend.position = "right", strip.text = element_text(face = "bold")) +
    geom_text(data = letter_data, aes(x = third, y = Inf, label = panel_letter), inherit.aes = FALSE,
              fontface = "bold", size = PEERJ_PANEL_LETTER_PT / .pt, hjust = -0.3, vjust = 1.3)
}

p5a <- make_fig5_panel(c("gsl_z17", "gsl_t95"), "Pooled slope (gC m-2 d-1 per day of GSL);\nfaint points = per-site slopes", "a", "GSL")
p5b <- make_fig5_panel(c("start_z17", "start_t95", "end_z17", "end_t95"),
                        "Pooled slope (gC m-2 d-1 per day\nshift in start/end date)", "b",
                        c("Start date", "End date"), clamp_outliers = TRUE)

fig5 <- p5a / p5b + plot_layout(heights = c(1, 1))
save_gsl_figure(
  fig5, "Figure 5", height_mm = 230,
  legend_lines = c(
    "Figure 5. Carbon gain (expanded set, main run). Units: gC m-2 d-1 per day of GSL/date shift.",
    "Panel a: pooled slope of annual GPP and NEP on GSL, by skewness third, for Z17 and T95; faint",
    "points behind are individual trend-site (>=10yr) slopes.",
    "Panel b: the same, on season start date and end date, faceted by GPP/NEP (rows) and start/end",
    "(columns); axes are limited to 1.5x the pooled 95% CI range per facet -- per-site slopes beyond",
    "that range are CLAMPED to the panel edge and drawn as an open triangle (not a filled circle),",
    "so one or two extreme per-site slopes cannot compress the pooled estimates into an unreadable",
    "sliver of the panel."
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

## var must stay a factor WITH THE SAME LEVELS as ts_pooled_third$var -- see
## the p6c_letter comment below for why a plain character here reorders the
## facets alphabetically across both layers.
p6a_letter <- tibble(third = factor(GSL_SKEW_THIRD_LEVELS[1], levels = GSL_SKEW_THIRD_LEVELS),
                      var = factor("GSL", levels = levels(ts_pooled_third$var)))
p6a <- ggplot(ts_pooled_third, aes(x = third, y = slope, colour = threshold)) +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), position = position_dodge(width = 0.5), width = 0.15,
                linewidth = nature_lwd(PEERJ_LWD_REF)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = nature_lwd(PEERJ_LWD_REF)) +
  facet_wrap(~ var, nrow = 1, scales = "free_y") +
  scale_color_manual(values = GSL_MARKER_COLORS, name = NULL, breaks = c("Z17", "T95")) +
  labs(x = "Skewness third", y = "Temperature sensitivity\n(days / degC)") +
  theme_gsl() + theme(legend.position = "right", strip.text = element_text(face = "bold")) +
  geom_text(data = p6a_letter, aes(x = third, y = Inf, label = "a"), inherit.aes = FALSE,
            fontface = "bold", size = PEERJ_PANEL_LETTER_PT / .pt, hjust = -0.3, vjust = 1.3)

## ---- Panel b (item 7): pooled trend vs threshold (5-20% of P99), by third
## and by group -- two SEPARATE ggplot objects (own colour scale/legend
## each), stacked via patchwork, instead of one facet_wrap() sharing a
## single merged legend. Mixed forest is already excluded upstream
## (TREND_SUBSET_DEFS, item 9) -- trend_pooled.csv has no such row, so no
## extra filtering is needed here. Years-to-detect-the-OBSERVED-trend
## labels are DROPPED (item 7) -- replaced by panel c's trend-size sweep.
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

make_fig6b_panel <- function(subtype, colours, panel_letter) {
  tn <- trend_numeric |> filter(subset_type == subtype)
  tz <- trend_z17_pts |> filter(subset_type == subtype)
  t95 <- trend_t95 |> filter(subset_type == subtype)
  ggplot(tn, aes(x = threshold_pct, y = slope, colour = subset_lab, group = subset_lab)) +
    geom_hline(yintercept = 0, colour = "grey70", linewidth = nature_lwd(PEERJ_LWD_REF)) +
    geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi, fill = subset_lab), alpha = 0.12, colour = NA) +
    geom_line(linewidth = nature_lwd(PEERJ_LWD_DATA)) +
    geom_point(data = tz, aes(x = threshold_pct, y = slope), shape = 16, size = 1.6) +
    geom_point(data = t95, aes(x = 21, y = slope, fill = subset_lab), shape = 24, size = 2.4,
               colour = "black", stroke = 0.4) +
    scale_color_manual(values = colours, name = subtype) +
    scale_fill_manual(values = colours, guide = "none") +
    scale_x_continuous(limits = c(5, 23), breaks = seq(5, 20, 5)) +
    labs(x = "Threshold (% of P99; T95 plotted at right, filled triangle)", y = "Pooled GSL trend (days/yr)") +
    theme_gsl() + theme(legend.position = "right") +
    { if (!is.null(panel_letter)) gsl_panel_letter(panel_letter) }
}
p6b_third <- make_fig6b_panel("By skewness third", subset_colors_third, "b")
p6b_group <- make_fig6b_panel("By functional group", subset_colors_group, NULL)

## ---- Panel c (item 7): years to detect a trend of a given SIZE
## (0.1-1.0 days/yr -- EXP_CFG_DETECT_TREND_GRID), Z17 and T95 (colour),
## one facet per subset (third or group) -- replaces the dropped observed-
## trend labels. Marks 0.25 (typical) and 0.5 (strong) with vertical lines.
ytd_sweep <- read_csv(file.path(EXP_TABLES, "years_to_detect_sweep.csv"), show_col_types = FALSE) |>
  mutate(threshold_lab = ifelse(threshold_name == "z17", "Z17", "T95"),
         subset_lab = factor(recode(subset, !!!SUBSET_LABELS),
                               levels = SUBSET_LABELS[names(SUBSET_LABELS) != "group_mixed_forest"]))

## subset_lab must stay a factor WITH THE SAME LEVELS here -- a plain
## character value in this second layer made ggplot re-derive the facet's
## level order as the alphabetical union across both layers (found by
## inspecting the first render: facets came out A-Z instead of thirds-then-
## groups).
p6c_letter <- tibble(subset_lab = factor(levels(ytd_sweep$subset_lab)[1], levels = levels(ytd_sweep$subset_lab)),
                      trend_size_days_per_yr = 0.1,
                      years_to_detect_median = max(ytd_sweep$years_to_detect_median, na.rm = TRUE))
p6c <- ggplot(ytd_sweep, aes(x = trend_size_days_per_yr, y = years_to_detect_median, colour = threshold_lab)) +
  geom_vline(xintercept = EXP_CFG_DETECT_MARK_TYPICAL, linetype = "dashed", colour = "grey60",
             linewidth = nature_lwd(PEERJ_LWD_REF)) +
  geom_vline(xintercept = EXP_CFG_DETECT_MARK_STRONG, linetype = "dotted", colour = "grey60",
             linewidth = nature_lwd(PEERJ_LWD_REF)) +
  geom_line(linewidth = nature_lwd(PEERJ_LWD_DATA)) +
  facet_wrap(~ subset_lab, ncol = 4) +
  scale_color_manual(values = GSL_MARKER_COLORS, name = NULL, breaks = c("Z17", "T95")) +
  scale_y_log10() +
  labs(x = "Trend size (days/yr; dashed = 0.25 typical, dotted = 0.5 strong)",
       y = "Years to detect\n(95% conf./80% power, log scale)") +
  theme_gsl() + theme(legend.position = "right", strip.text = element_text(size = PEERJ_BASE_PT)) +
  geom_text(data = p6c_letter, aes(x = trend_size_days_per_yr, y = Inf, label = "c"), inherit.aes = FALSE,
            fontface = "bold", size = PEERJ_PANEL_LETTER_PT / .pt, hjust = -0.3, vjust = 1.3)

fig6 <- p6a / p6b_third / p6b_group / p6c + plot_layout(heights = c(1, 1, 1, 1.3))
save_gsl_figure(
  fig6, "Figure 6", height_mm = 240,
  legend_lines = c(
    "Figure 6. Climate fingerprints (expanded set, main run).",
    "Panel a: temperature sensitivity (days/degC) of GSL, season start and end, by skewness third,",
    "Z17 and T95 (pooled mixed model; error bars = 95% CI). Item 5: GSL's predictor is the average",
    "of the start- and end-window 60-day pre-means (NOT the annual mean -- that version is in",
    "tables/temperature_sensitivity_pooled_annualta.csv only).",
    "Panel b: pooled GSL trend (days/yr) across 5-20% of P99 (line + 95% CI ribbon, circle marks",
    "Z17 at 10%), by skewness third (top) and functional group (bottom) -- SEPARATE legends for",
    "thirds and groups (item 7); T95's own pooled trend is plotted at the right edge (triangle).",
    "Mixed forest is excluded from both (only 2 of its 7 sites have >=10 years, below the >=3-site",
    "floor for a pooled model -- item 9).",
    "Panel c: years needed to detect a trend of a given SIZE (0.1-1.0 days/yr, Weatherhead et al.",
    "1998, 95% confidence/80% power), at Z17 and T95, one facet per subset (mixed forest excluded);",
    "dashed/dotted lines mark 0.25 (typical) and 0.5 (strong) days/yr. Replaces the dropped years-",
    "to-detect-the-OBSERVED-trend labels (kept in trend_pooled.csv for reference, not plotted).",
    "Pooled trends are also expressed as % of each subset's own mean GSL per decade in",
    "tables/trend_pooled.csv (slope_pct_per_decade), not shown as a separate axis here."
  ),
  input_sources = c(file.path(EXP_TABLES, "trend_pooled.csv"), file.path(EXP_TABLES, "years_to_detect_sweep.csv"),
                     file.path(EXP_TABLES, "temperature_sensitivity_pooled.csv")),
  notes = ""
)

message("--- Figure 6 done ---")

message("--- 04_figures_main.R: Figures 1-6 done ---")
