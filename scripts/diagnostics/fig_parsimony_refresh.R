## fig_parsimony_refresh.R
##
## Recreates review/diagnostics/it_mbo_parsimony/fig_mean_annual_by_resolution.png
## from the refreshed values already computed and written by
## it_mbo_parsimony_refresh.R -- plotting code only. Does not read the store,
## DuckDB, or any raw extracted file, and does not recompute any mean-annual
## precipitation value. Does not edit or overwrite the original figure.
##
## Two outputs:
##  1. fig_mean_annual_by_resolution_refreshed.png -- same spec as the
##     original, drawn from the refreshed table only, with two readability
##     fixes (P_ERA/P_F dodged apart where they nearly coincide; explicit y
##     headroom so US-HB4's P_ERA is not clipped against the panel top).
##  2. fig_mean_annual_by_resolution_before_after.png -- the same figure with
##     the previous report's values added as faint grey open points/dashed
##     lines behind the refreshed series, on the identical y range as (1) so
##     the two figures are directly comparable.

source("R/pipeline_config.R")
source("R/utils.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(ggplot2)
  library(scales)
})

OUTD <- "review/diagnostics/it_mbo_parsimony_refresh"
stopifnot(dir.exists(OUTD))

message("=== fig_parsimony_refresh.R ===")

in_refreshed <- file.path(OUTD, "table_1_mean_annual_by_resolution_refreshed.csv")
in_previous  <- file.path(OUTD, "table_1_mean_annual_by_resolution_previous_reproduced.csv")

d1_refreshed <- readr::read_csv(in_refreshed, show_col_types = FALSE)
d1_previous  <- readr::read_csv(in_previous, show_col_types = FALSE)

site_levels <- c("FI-Hyy", "IT-MBo", "US-HB4")  # same order as the original (alphabetical facet_wrap default)

to_long <- function(d1, period_label) {
  d1 |>
    tidyr::pivot_longer(cols = dplyr::starts_with("mean_") & dplyr::ends_with("_mm"),
                         names_to = "resolution", values_to = "mean_mm") |>
    dplyr::mutate(resolution = toupper(sub("mean_(.*)_mm", "\\1", resolution)),
                  resolution = factor(resolution, levels = c("HH", "DD", "MM", "YY")),
                  site_id = factor(site_id, levels = site_levels),
                  period = period_label)
}

long_refreshed <- to_long(d1_refreshed, "Refreshed")
long_previous  <- to_long(d1_previous, "Previous")

hlines_from <- function(d1) {
  d1 |> dplyr::distinct(site_id, bio12_mm, badm_map_mm) |>
    dplyr::mutate(site_id = factor(site_id, levels = site_levels)) |>
    tidyr::pivot_longer(cols = c(bio12_mm, badm_map_mm), names_to = "reference", values_to = "ref_mm") |>
    dplyr::mutate(reference = ifelse(reference == "bio12_mm", "WorldClim BIO12", "BADM PI-reported MAP"))
}
hlines <- hlines_from(d1_refreshed)  # bio12/badm are unchanged by the refresh -- identical in both tables

# ============================================================================
# Shared y range: computed from the union of refreshed + previous + reference
# values, so both figures use the identical y axis even though figure 1 only
# plots refreshed data. Headroom fixes the original's US-HB4 P_ERA clipping.
# ============================================================================
all_values <- c(long_refreshed$mean_mm, long_previous$mean_mm, hlines$ref_mm)
y_limits <- c(min(all_values) * 0.85, max(all_values) * 1.15)

DODGE <- ggplot2::position_dodge(width = 0.28)

base_theme <- ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", color = NA),
                 panel.background = ggplot2::element_rect(fill = "white", color = NA),
                 legend.position = "bottom")

y_scale <- ggplot2::scale_y_log10(
  labels = scales::label_number(big.mark = " "),
  limits = y_limits
)

# ============================================================================
# Figure 1: refreshed values only, same spec as the original, two fixes:
##  (1) P_ERA/P_F dodged apart horizontally so neither occludes the other
##      where they nearly coincide (FI-Hyy, IT-MBo panels).
##  (2) explicit y-axis headroom (15% above the max value) so US-HB4's P_ERA
##      is not clipped against the panel's top border.
# ============================================================================
message("\n================ Figure 1: refreshed ================")

p1 <- ggplot2::ggplot(long_refreshed, ggplot2::aes(x = resolution, y = mean_mm, color = variable, group = variable)) +
  ggplot2::geom_hline(data = hlines, ggplot2::aes(yintercept = ref_mm, linetype = reference), color = "grey40", linewidth = 0.5) +
  ggplot2::geom_line(linewidth = 0.9, position = DODGE) +
  ggplot2::geom_point(size = 2.5, position = DODGE) +
  y_scale +
  ggplot2::scale_color_manual(values = c(P_ERA = "#D55E00", P_F = "#0072B2"), name = "Variable") +
  ggplot2::scale_linetype_manual(values = c("WorldClim BIO12" = "dashed", "BADM PI-reported MAP" = "dotted"), name = "Reference") +
  ggplot2::facet_wrap(~ site_id, nrow = 1) +
  ggplot2::labs(x = "Resolution", y = "Mean annual precipitation (mm/yr, log scale)",
                title = "Mean annual precipitation by resolution: IT-MBo, US-HB4, FI-Hyy",
                subtitle = "Complete-coverage years only; BIO12/BADM shown for reference, not fitted") +
  base_theme

out_fig1 <- file.path(OUTD, "fig_mean_annual_by_resolution_refreshed.png")
ggplot2::ggsave(out_fig1, p1, width = 11, height = 4.5, dpi = 200, bg = "white")
write_output_metadata(out_fig1,
  input_sources = in_refreshed,
  notes = "Recreation of review/diagnostics/it_mbo_parsimony/fig_mean_annual_by_resolution.png from the refreshed values (table_1_mean_annual_by_resolution_refreshed.csv) only -- no value recomputed, no raw file or store read. Same spec as the original (3 facets FI-Hyy/IT-MBo/US-HB4, log10 y, BIO12 dashed/BADM dotted grey reference lines, P_ERA orange/P_F blue, two bottom legends). Two readability fixes vs. the original, applied here for the first time: (1) P_ERA and P_F are drawn with a small horizontal dodge (position_dodge, width 0.28) applied identically to points and lines, so neither series occludes the other where they nearly coincide (this was previously invisible for P_ERA in the FI-Hyy and IT-MBo panels); values themselves are unchanged, only their horizontal plotting position. (2) The y-axis uses explicit log10 limits with ~15% headroom above the maximum plotted value (previously default expansion clipped US-HB4's P_ERA against the panel's top border).")
message("Saved: ", out_fig1)

# ============================================================================
# Figure 2: before/after. Previous report's values (table_1_..._previous_
# reproduced.csv) added as faint grey open points and dashed lines behind the
# refreshed series; "Previous" vs. "Refreshed" distinguished via a merged
# shape+linetype "Period" legend. Same y range as figure 1 (see y_limits
# above) so the two figures are directly comparable panel-for-panel.
# ============================================================================
message("\n================ Figure 2: before/after ================")

p2 <- ggplot2::ggplot(mapping = ggplot2::aes(x = resolution, y = mean_mm, group = variable)) +
  ggplot2::geom_hline(data = hlines, ggplot2::aes(yintercept = ref_mm, linetype = reference), color = "grey40", linewidth = 0.5, inherit.aes = FALSE) +
  # previous: plain grey, open points, dashed lines, drawn first (behind)
  ggplot2::geom_line(data = long_previous, color = "grey55", linewidth = 0.7,
                      linetype = "dashed", alpha = 0.65, position = DODGE) +
  ggplot2::geom_point(data = long_previous, ggplot2::aes(shape = period),
                       color = "grey55", size = 2.8, alpha = 0.65, position = DODGE) +
  # refreshed: normal colored series, drawn on top
  ggplot2::geom_line(data = long_refreshed, ggplot2::aes(color = variable), linewidth = 0.9, position = DODGE) +
  ggplot2::geom_point(data = long_refreshed, ggplot2::aes(color = variable, shape = period), size = 2.5, position = DODGE) +
  y_scale +
  ggplot2::scale_color_manual(values = c(P_ERA = "#D55E00", P_F = "#0072B2"), name = "Variable") +
  ggplot2::scale_linetype_manual(values = c("WorldClim BIO12" = "dashed", "BADM PI-reported MAP" = "dotted"), name = "Reference") +
  ggplot2::scale_shape_manual(values = c(Previous = 1, Refreshed = 16), name = "Period") +
  ggplot2::facet_wrap(~ site_id, nrow = 1) +
  ggplot2::labs(x = "Resolution", y = "Mean annual precipitation (mm/yr, log scale)",
                title = "Mean annual precipitation by resolution: IT-MBo, US-HB4, FI-Hyy",
                subtitle = "Complete-coverage years only; BIO12/BADM shown for reference, not fitted") +
  base_theme

out_fig2 <- file.path(OUTD, "fig_mean_annual_by_resolution_before_after.png")
ggplot2::ggsave(out_fig2, p2, width = 11, height = 4.5, dpi = 200, bg = "white")
write_output_metadata(out_fig2,
  input_sources = c(in_refreshed, in_previous),
  notes = "Before/after version of fig_mean_annual_by_resolution_refreshed.png. Refreshed series (colored, filled points, solid lines) from table_1_mean_annual_by_resolution_refreshed.csv; previous-report series (grey, open points, dashed lines, alpha 0.65) from table_1_mean_annual_by_resolution_previous_reproduced.csv -- both read as already-computed tables, no value recomputed here. 'Previous' vs. 'Refreshed' distinguished by a merged shape/linetype 'Period' legend (open circle + dashed = previous, filled circle + solid = refreshed); the pre-existing 'Variable' (color) and 'Reference' (BIO12/BADM linetype) legends are unchanged from the single-series figure. Same explicit y-axis limits as fig_mean_annual_by_resolution_refreshed.png (computed from the union of both tables' values) so the two figures are directly comparable panel-for-panel.")
message("Saved: ", out_fig2)

message("\n=== fig_parsimony_refresh.R complete ===")
