## 04_figure2_g_diagnostic.R — Figure 2: ground heat flux (G) diagnostic,
## priority item: SN-Nkr.
##
## Built for the site team, not for interpretation in a manuscript. The
## question this figure exists to let them judge directly: does SN-Nkr's G
## amplitude approach or exceed its Rn amplitude in a way that points to a
## unit error, a sign-convention error, a plate that isn't in soil, or a
## mislabelled variable -- rather than a genuine sparse-canopy signal? See
## docs/report_back_20260908.md §5 for the closure-regression flag that
## motivated this figure (R² = 0.001 at SN-Nkr; 26% of qualifying
## half-hours have |G_F_MDS| > 200 W/m², up to ~860 W/m², consistent across
## all 7 years).
##
## Panels:
##   (a) SN-Nkr: mean diurnal composite of G and Rn, one line pair per year
##   (b) Other sites with usable G data: same composite, all years pooled
##   (c) Distribution of G by site (full record)
##   (d) SN-Nkr: date x hour-of-day heatmap of raw G, to see whether the
##       implausible values are continuous across the record or episodic
##
## Filtering (see methods_memo.md for the recorded decision): BJ-Nhu is
## excluded throughout -- G_F_MDS is present in its header but 100% NA (no
## soil heat flux measurement at all, not a QC/thinness issue). No QC
## filtering and no completeness threshold is applied to G_F_MDS or NETRAD
## anywhere in this figure -- this is a raw visual diagnostic of what the
## instrument reported, not a filtered analysis figure, so gap-filled
## half-hours are included and only NA rows are dropped.
##
## Outputs (git-tracked):
##   figures/figure2_g_diagnostic_snnkr.pdf
##   figures/figure2_g_diagnostic_snnkr.png

source("WAFNET/energy_partitioning/code/00_config.R")
source("WAFNET/energy_partitioning/code/fig_helpers.R")

library(dplyr)
library(tidyr)
library(patchwork)

combined_path <- file.path(WAFNET_ROOT, "data", "processed", "flux_hh_all_sites.rds")
if (!file.exists(combined_path)) {
  stop("[WAFNET] ", combined_path, " not found -- run 02_read_hh.R first.")
}
hh <- readRDS(combined_path)

# Sites with usable G data: G_F_MDS column present AND not entirely NA.
# BJ-Nhu fails this (present, 100% NA) -- excluded, not silently dropped.
g_available <- hh |>
  dplyr::group_by(site_id) |>
  dplyr::summarise(has_g = any(!is.na(.data$G_F_MDS)), .groups = "drop") |>
  dplyr::filter(.data$has_g) |>
  dplyr::pull(site_id)
sites_no_g <- setdiff(WAFNET_SITES, g_available)
message("[WAFNET] Figure 2: sites with usable G data: ", paste(g_available, collapse = ", "))
message("[WAFNET] Figure 2: sites excluded (no G data): ", paste(sites_no_g, collapse = ", "))
if (!"SN-Nkr" %in% g_available) {
  stop("[WAFNET] SN-Nkr has no G data at all -- Figure 2 cannot be produced as specified.")
}

hh <- hh |>
  dplyr::mutate(
    year      = lubridate::year(.data$DATETIME_START),
    hh_of_day = (lubridate::hour(.data$DATETIME_START) * 60 +
                   lubridate::minute(.data$DATETIME_START)) / 30,
    hour      = .data$hh_of_day / 2
  )

# ── (a) SN-Nkr: mean diurnal composite of G and Rn, per year ───────────────
nkr <- hh |> dplyr::filter(.data$site_id == "SN-Nkr")

nkr_diurnal <- nkr |>
  dplyr::filter(!is.na(.data$G_F_MDS) | !is.na(.data$NETRAD)) |>
  dplyr::group_by(.data$year, .data$hour) |>
  dplyr::summarise(
    G  = mean(.data$G_F_MDS, na.rm = TRUE),
    Rn = mean(.data$NETRAD, na.rm = TRUE),
    .groups = "drop"
  ) |>
  tidyr::pivot_longer(c("G", "Rn"), names_to = "variable", values_to = "value")

p_a <- ggplot(nkr_diurnal, aes(.data$hour, .data$value, color = factor(.data$year), linetype = .data$variable)) +
  geom_line(linewidth = 0.6) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.3) +
  scale_linetype_manual(values = c(G = "solid", Rn = "22"), name = NULL) +
  scale_color_viridis_d(name = "Year") +
  scale_x_continuous(breaks = seq(0, 24, 6)) +
  labs(
    title = "(a) SN-Nkr: mean diurnal composite of G and Rn, by year",
    x = "Hour of day (UTC)", y = expression(W~m^-2)
  ) +
  theme_wafnet()

# ── (b) Other sites with usable G: composite pooled across all years ───────
comparison_sites <- setdiff(g_available, "SN-Nkr")

comp_diurnal <- hh |>
  dplyr::filter(.data$site_id %in% comparison_sites) |>
  dplyr::filter(!is.na(.data$G_F_MDS) | !is.na(.data$NETRAD)) |>
  dplyr::group_by(.data$site_id, .data$hour) |>
  dplyr::summarise(
    G  = mean(.data$G_F_MDS, na.rm = TRUE),
    Rn = mean(.data$NETRAD, na.rm = TRUE),
    .groups = "drop"
  ) |>
  tidyr::pivot_longer(c("G", "Rn"), names_to = "variable", values_to = "value") |>
  dplyr::mutate(site_id = factor(.data$site_id, levels = WAFNET_SITE_ORDER))

p_b <- ggplot(comp_diurnal, aes(.data$hour, .data$value, color = .data$site_id, linetype = .data$variable)) +
  geom_line(linewidth = 0.6) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.3) +
  scale_linetype_manual(values = c(G = "solid", Rn = "22"), name = NULL) +
  scale_color_manual(values = WAFNET_SITE_COLORS, name = "Site") +
  facet_wrap(~.data$site_id, nrow = 1) +
  scale_x_continuous(breaks = seq(0, 24, 12)) +
  labs(
    title = "(b) Other sites with usable G data: composite pooled across all years (BJ-Nhu excluded)",
    x = "Hour of day (UTC)", y = expression(W~m^-2)
  ) +
  theme_wafnet() +
  theme(legend.position = "none")

# ── (c) Distribution of G by site (full record) ────────────────────────────
g_dist <- hh |>
  dplyr::filter(.data$site_id %in% g_available, !is.na(.data$G_F_MDS)) |>
  dplyr::mutate(site_id = factor(.data$site_id, levels = WAFNET_SITE_ORDER))

p_c <- ggplot(g_dist, aes(.data$site_id, .data$G_F_MDS, fill = .data$site_id)) +
  geom_boxplot(outlier.size = 0.4, outlier.alpha = 0.3) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.3) +
  scale_fill_manual(values = WAFNET_SITE_COLORS, guide = "none") +
  labs(
    title = "(c) Distribution of G by site, full record (BJ-Nhu excluded)",
    x = NULL, y = expression(G~(W~m^-2))
  ) +
  theme_wafnet()

# ── (d) SN-Nkr: date x hour-of-day heatmap of raw G ────────────────────────
# Color scale capped at +-300 W/m^2 (oob = squish) so the ~26% of
# half-hours exceeding that (up to ~860 W/m^2) don't wash out the rest of
# the palette; the caption states the cap explicitly.
nkr_heat <- nkr |>
  dplyr::filter(!is.na(.data$G_F_MDS)) |>
  dplyr::mutate(date = as.Date(.data$DATETIME_START))

p_d <- ggplot(nkr_heat, aes(.data$date, .data$hour, fill = .data$G_F_MDS)) +
  # geom_tile, not geom_raster: the date axis has gaps (missing days), and
  # geom_raster assumes a complete regular grid, which shifts pixels out of
  # place when it isn't one (see docs/report_back_20260908.md site-year
  # completeness notes -- SN-Nkr 2018 has 15,711 of a possible 17,568
  # half-hours). geom_tile draws only the cells present, at their correct
  # date/hour position.
  geom_tile(width = 1, height = 0.5) +
  scale_fill_gradient2(
    low = "#2166ac", mid = "white", high = "#b2182b", midpoint = 0,
    limits = c(-300, 300), oob = scales::squish,
    name = expression(G~(W~m^-2))
  ) +
  scale_y_continuous(breaks = seq(0, 24, 6)) +
  labs(
    title = "(d) SN-Nkr: G by date and hour of day (color capped at +/-300 W m^-2; values to +/-860 W m^-2 occur)",
    x = NULL, y = "Hour of day (UTC)"
  ) +
  theme_wafnet() +
  theme(legend.position = "right")

fig2 <- (p_a | p_c) / p_b / p_d +
  patchwork::plot_layout(heights = c(1.1, 0.85, 1)) +
  patchwork::plot_annotation(
    title = "Figure 2. Ground heat flux (G) diagnostic - priority: SN-Nkr",
    theme = theme(plot.title = element_text(face = "bold", size = 13))
  )

caption <- c(
  paste0(
    "Figure 2. Ground heat flux (G) diagnostic for SN-Nkr, the site flagged ",
    "by the pre-analysis energy-balance closure check (docs/report_back_20260908.md, ",
    "§5): closure R² = 0.001 (n = 69,224) with 26% of qualifying half-hours ",
    "showing |G_F_MDS| > 200 W m⁻², up to ~860 W m⁻², consistent across ",
    "all 7 years 2018–2024."
  ),
  paste0(
    "What is plotted: (a) mean diurnal composite (48 half-hourly bins) of ",
    "G_F_MDS (solid) and NETRAD (dashed) at SN-Nkr, one line pair per year; ",
    "(b) the same composite pooled across all years, for the other sites ",
    "with usable G data (GH-Ank, BJ-Db1, BJ-Bfg, SN-Dhr); (c) the full-",
    "record distribution of G_F_MDS by site; (d) a date × hour-of-day heat ",
    "map of raw (non-composited) G_F_MDS at SN-Nkr, with the color scale ",
    "capped at ±300 W m⁻² (oob values squished into the end colors, not ",
    "dropped) so the rest of the record stays visible against the extremes."
  ),
  paste0(
    "Filtering applied: BJ-Nhu is excluded from every panel — G_F_MDS is ",
    "present in its header but 100% NA across all half-hours (no soil heat ",
    "flux measurement at all for this site, not a QC or thinness issue; see ",
    "docs/report_back_20260908.md §5). No QC filtering and no completeness ",
    "threshold is applied to G_F_MDS or NETRAD anywhere in this figure — ",
    "this is a raw visual diagnostic of what the instrument reported, not a ",
    "filtered analysis figure; MDS gap-filled half-hours are included, and ",
    "only rows where the relevant variable(s) are NA are dropped."
  ),
  paste0(
    "What this figure supports: comparing SN-Nkr's G amplitude against its ",
    "own Rn and against the other sites (a, b, c), and reading from (d) ",
    "whether SN-Nkr's implausible G values are spread continuously across ",
    "the whole record or confined to particular dates/hours — which bears ",
    "on whether this looks like a persistent instrumentation or labelling ",
    "issue versus an episodic fault."
  ),
  paste0(
    "What this figure does not support: it does not diagnose the cause. ",
    "Distinguishing a unit error, a sign-convention error, a mis-sited ",
    "plate, or a genuine sparse-canopy soil heat flux signal requires site-",
    "team documentation (plate depth, calibration history, siting) that is ",
    "not available in the Shuttle BADM metadata for any of these six sites ",
    "(tables/g_measurement_notes.csv — no GRP_HEATFLUX/GRP_SOILHEATFLUX group ",
    "found for any of them). This figure is intended to prompt that ",
    "conversation with the site team, not settle it."
  )
)

save_figure(fig2, "figure2_g_diagnostic_snnkr", caption, width = 11, height = 13)

message("[WAFNET] 04_figure2_g_diagnostic.R complete.")
