## 13_handoff_figures.R — Stage 2 hand-off figures for the 11 WUE isotope
## pilot sites, in the paper's Nature/Scientific Data figure format (as the
## repo already applies it: R/nature_format.R + R/plot_constants.R's
## PAPER_IGBP_ORDER/PAPER_IGBP_COLOURS). Read-and-report: reads already-
## committed stage 2 and precip_compare tables plus two repo-root
## read-only snapshots; writes nothing outside WUE/isotope_pilot/.
##
## Sites: the 11 stage-2 sites (WUE_SITES_STAGE2 -- CH-Dav and NL-Loo
## already dropped). Figures 3-4 additionally drop NL-Loo by instruction
## (it was never part of the precip_compare input tables' site list at the
## time those tables were generated in a way requiring a second exclusion --
## recorded explicitly here since those tables predate NL-Loo's stage-2
## removal and may still carry its rows).
##
## No interpretation anywhere in this script or its outputs: no trend
## tests, no Sen's slopes, no statements about what a series means.
##
## Output (figures/handoff/, git-tracked, each a .png + .pdf + .legend.txt):
##   fig0_map_sites
##   fig1_by_site_ratio_to_mean          fig1_by_site_pct_change_guerrieri
##   fig2_by_pft_ratio_to_mean           fig2_by_pft_pct_change_guerrieri
##   fig3_rain_source_summary
##   fig4_rain_rule_by_year
## Output (tables/handoff/, git-tracked, each with a .meta.json companion):
##   site_meta.csv                 fig1_fig2_metrics.csv
##   fig3_wet_day_share.csv        fig3_days_removed_mean.csv
##   fig4_days_removed_by_year.csv
## Output (docs/): handoff_figure_legends_<date>.md

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(ggtext)
  library(ggrepel)
  library(patchwork)
})

options(useFancyQuotes = FALSE)

source("R/plot_constants.R")
source("R/nature_format.R")
source("R/figures/fig_maps.R")   # for .land_sf(), .map_base(), .apply_region() (internal, reused not re-derived)

msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)

WUE_ROOT  <- "WUE/isotope_pilot"
TABLE_DIR <- file.path(WUE_ROOT, "tables")
FIG_DIR   <- file.path(WUE_ROOT, "figures", "handoff")
OUT_TBL   <- file.path(WUE_ROOT, "tables", "handoff")
DOCS_DIR  <- file.path(WUE_ROOT, "docs")
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_TBL, recursive = TRUE, showWarnings = FALSE)

## 11 stage-2 sites (WUE_SITES_STAGE2 as of 2026-10-07: CH-Dav and NL-Loo
## already excluded). Hardcoded rather than sourcing 00_config.R, which
## would pull in the full FLUXNET Shuttle credential/venv check this
## read-only figure script has no need for.
SITES <- c("US-Ha1", "US-Ho2", "US-MMS", "US-SP1", "US-Bar", "US-Slt",
           "US-Dk2", "US-Fuf", "DE-Tha", "BE-Vie", "FI-Hyy")

write_meta <- function(output_path, notes = "") {
  meta <- list(
    run_datetime_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    pipeline_version = tryCatch(system("git rev-parse --short HEAD", intern = TRUE), error = function(e) NA_character_),
    input_sources    = list("WUE/isotope_pilot/tables/ (stage 2), WUE/isotope_pilot/tables/precip_compare/, data/snapshots/ (read-only)"),
    notes            = notes
  )
  meta_path <- paste0(tools::file_path_sans_ext(output_path), ".meta.json")
  writeLines(jsonlite::toJSON(meta, pretty = TRUE, auto_unbox = TRUE), meta_path)
  invisible(meta_path)
}
write_csv_meta <- function(df, path, notes = "") {
  write.csv(df, path, row.names = FALSE)
  write_meta(path, notes)
}

## ============================================================================
## 1. Load inputs
## ============================================================================
wue_annual <- read_csv(file.path(TABLE_DIR, "wue_annual.csv"), show_col_types = FALSE) |>
  filter(site %in% SITES) |>
  rename(site_id = site)

screen_attrition <- read_csv(file.path(TABLE_DIR, "screen_attrition.csv"), show_col_types = FALSE) |>
  filter(site_id %in% SITES, year_kept)

wet_freq <- read_csv(file.path(TABLE_DIR, "precip_compare", "wet_day_frequency.csv"), show_col_types = FALSE) |>
  filter(site_id %in% SITES, period == "all", threshold_mm == 0)

rain_rule <- read_csv(file.path(TABLE_DIR, "precip_compare", "rain_rule_consequence.csv"), show_col_types = FALSE) |>
  filter(site_id %in% SITES)

snapshot <- read_csv("data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv", show_col_types = FALSE) |>
  filter(site_id %in% SITES) |>
  distinct(site_id, .keep_all = TRUE) |>
  select(site_id, location_lat, location_long, igbp)

koppen <- read_csv("data/snapshots/site_koppen_era5_fig4.csv", show_col_types = FALSE) |>
  filter(site_id %in% SITES) |>
  select(site_id, koppen = panel_a_class_used)

site_meta <- snapshot |>
  left_join(koppen, by = "site_id") |>
  mutate(igbp_order = match(igbp, PAPER_IGBP_ORDER)) |>
  arrange(igbp_order, desc(location_lat))
site_order <- site_meta$site_id

write_csv_meta(site_meta, file.path(OUT_TBL, "site_meta.csv"),
  notes = "site_id, lat/lon, IGBP from data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv; two-letter Koppen class (panel_a_class_used) from data/snapshots/site_koppen_era5_fig4.csv.")

msg("Loaded inputs for ", length(SITES), " sites.")

## ============================================================================
## 2. Figures 1-2 data: full kept-year grid, both normalisations
## ============================================================================
k_at_limit <- function(x) !is.na(x) & (abs(x - 0) < 1e-6 | abs(x - 1.5) < 1e-6)

metric_grid <- screen_attrition |>
  select(site_id, year, valid_days, share_days_gauge_measured) |>
  left_join(
    wue_annual |> select(site_id, year, WUE_y, IWUE_y, uWUE_y, k_star_subdaily),
    by = c("site_id", "year")
  ) |>
  mutate(
    ## Sub-daily k*; drop (to NA) site-years where it sits on a grid limit --
    ## its own missingness, independent of the other three metrics.
    k_star_subdaily = ifelse(k_at_limit(k_star_subdaily), NA_real_, k_star_subdaily)
  )

metrics_long <- metric_grid |>
  pivot_longer(cols = c(WUE_y, IWUE_y, uWUE_y, k_star_subdaily), names_to = "metric", values_to = "value") |>
  mutate(metric = recode(metric, WUE_y = "WUE", IWUE_y = "IWUE", uWUE_y = "uWUE", k_star_subdaily = "k*"),
         metric = factor(metric, levels = c("WUE", "IWUE", "uWUE", "k*")),
         low_days = valid_days > 0 & valid_days < 10)

## Normalisation (i): ratio to the site's own mean over its plotted years.
metrics_long <- metrics_long |>
  group_by(site_id, metric) |>
  mutate(norm_ratio = value / mean(value, na.rm = TRUE)) |>
  ungroup()

## Normalisation (ii): Guerrieri et al. (2019) style -- percentage change
## relative to the site's first year with a value AND >=10 valid days (0%).
baseline <- metrics_long |>
  filter(!is.na(value), valid_days >= 10) |>
  group_by(site_id, metric) |>
  slice_min(year, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(site_id, metric, baseline_value = value, baseline_year = year)

metrics_long <- metrics_long |>
  left_join(baseline, by = c("site_id", "metric")) |>
  mutate(norm_pct = (value - baseline_value) / baseline_value * 100)

write_csv_meta(metrics_long, file.path(OUT_TBL, "fig1_fig2_metrics.csv"),
  notes = "One row per kept site-year x metric (WUE, IWUE, uWUE, sub-daily k*). k* is NA where it sat on a grid-search limit (0 or 1.5). norm_ratio = value / site's own mean over its plotted years; norm_pct = percent change from the site's first year with a value and >=10 valid days (Guerrieri et al. 2019 style). low_days = TRUE for valid_days in (0,10).")

n_no_baseline <- baseline |> count(metric) |> mutate(n_sites_with_baseline = n)
msg("metrics_long: ", nrow(metrics_long), " rows. Baseline years found for ",
    nrow(baseline), " of ", length(SITES) * 4, " site x metric combinations.")

## ============================================================================
## 3. Shared constants
## ============================================================================
METRIC_COLOURS <- c(WUE = "#1B9E77", IWUE = "#D95F02", uWUE = "#7570B3", "k*" = "#E7298A")

## Per-panel background bars (valid days) rescaled into the panel's own
## primary y-range so each panel's secondary axis is correctly calibrated --
## NOT a single global rescale, since facet-specific secondary axes are not
## supported by ggplot2 and panels here have very different y-ranges.
panel_y_range <- function(values) {
  rng <- range(c(0, values), na.rm = TRUE)
  if (!all(is.finite(rng))) rng <- c(0, 1)
  pad <- diff(rng) * 0.08
  if (pad == 0) pad <- max(abs(rng[1]), 1) * 0.1
  rng + c(-pad, pad)
}
bar_rescale <- function(d, valid_days_col, yrange) {
  max_days <- suppressWarnings(max(d[[valid_days_col]], na.rm = TRUE))
  if (!is.finite(max_days) || max_days <= 0) max_days <- 1
  span <- diff(yrange) * 0.9
  scale_factor <- span / max_days
  list(scale_factor = scale_factor, floor = yrange[1])
}

## panel_letter() (R/nature_format.R) positions its annotate("text", x=-Inf,
## y=Inf, ...) in DATA coordinates -- confirmed by direct reproduction to
## render nothing at all on a coord_flip() + discrete-axis plot (fig3's own
## panels), where -Inf/Inf are not meaningful positions on a discrete scale.
## plot.tag (ggplot2's own margin-relative panel-tag mechanism) is immune to
## this, so fig3 uses it instead; fig0's map (continuous lon/lat, no
## coord_flip()) keeps panel_letter() since that one is confirmed working.
panel_tag <- function(letter) {
  list(
    labs(tag = tolower(letter)),
    theme(plot.tag = element_text(face = "bold", size = NATURE_PANEL_LETTER_PT, family = NATURE_FONT),
          plot.tag.position = c(0.02, 0.98))
  )
}

site_panel_title_theme <- nature_theme() +
  theme(
    plot.title      = element_text(size = NATURE_BASE_PT, face = "bold", hjust = 0.5, family = NATURE_FONT, margin = margin(b = 1)),
    legend.position = "bottom",
    legend.key.size = unit(3, "mm"),
    axis.title.y.right = element_text(size = NATURE_SMALL_PT)
  )

## ============================================================================
## 4. Figure 1 — by site, one panel per site, two normalisations
## ============================================================================
build_site_panel <- function(sid, norm_col, ylab) {
  d <- metrics_long |> filter(site_id == sid) |> arrange(year)
  meta_row <- site_meta |> filter(site_id == sid)
  yrange <- panel_y_range(d[[norm_col]])
  bars <- bar_rescale(d, "valid_days", yrange)
  d$bar_y <- bars$floor + d$valid_days * bars$scale_factor
  ## One row per YEAR for the bar layer -- metrics_long has 4 rows per
  ## site-year (one per metric), and bar_y is identical across them; feeding
  ## all 4 into geom_col() drew 4 exactly-overlapping bars per year (harmless
  ## visually but wasteful, and inflated the "removed rows outside scale
  ## range" warning counts 4x). geom_rect() (not geom_col(), which always
  ## baselines at y=0) draws the bar from the computed floor to bar_y, so a
  ## zero-valid-day year's bar sits exactly at the floor instead of
  ## appearing as a small negative-going sliver below y=0.
  bar_df <- d |> dplyr::distinct(year, bar_y)

  bad_gauge_years <- d |> filter(share_days_gauge_measured < 0.8) |> pull(year) |> unique()
  all_years <- sort(unique(d$year))
  x_labels <- ifelse(all_years %in% bad_gauge_years,
                      paste0("<span style='color:#CC3311'>", all_years, "</span>"),
                      as.character(all_years))

  p <- ggplot(d, aes(x = year)) +
    geom_rect(data = bar_df, inherit.aes = FALSE,
              aes(xmin = year - 0.4, xmax = year + 0.4, ymin = bars$floor, ymax = bar_y),
              fill = "grey85", colour = NA) +
    geom_line(aes(y = .data[[norm_col]], colour = metric), linewidth = nature_lwd(0.5), na.rm = TRUE) +
    geom_point(data = d |> filter(!low_days), aes(y = .data[[norm_col]], colour = metric),
               shape = 19, size = 0.8, na.rm = TRUE) +
    geom_point(data = d |> filter(low_days), aes(y = .data[[norm_col]], colour = metric),
               shape = 1, size = 0.8, stroke = nature_lwd(0.4), na.rm = TRUE) +
    scale_colour_manual(values = METRIC_COLOURS, name = NULL, drop = FALSE) +
    scale_x_continuous(breaks = all_years, labels = x_labels) +
    scale_y_continuous(
      limits = yrange,
      labels = if (grepl("pct", norm_col)) nature_minus_labels() else waiver(),
      sec.axis = sec_axis(~ (. - bars$floor) / bars$scale_factor, name = "Valid days")
    ) +
    labs(x = NULL, y = ylab, title = sid) +
    site_panel_title_theme +
    theme(axis.text.x = ggtext::element_markdown(size = NATURE_SMALL_PT, angle = 90, vjust = 0.5, hjust = 1)) +
    annotate("text", x = Inf, y = Inf, label = paste0(meta_row$igbp, " / ", meta_row$koppen),
              hjust = 1.05, vjust = 1.4, size = NATURE_SMALL_PT / .pt, family = NATURE_FONT, colour = "grey30")
  p
}

make_fig1 <- function(norm_col, ylab) {
  panels <- lapply(site_order, build_site_panel, norm_col = norm_col, ylab = ylab)
  patchwork::wrap_plots(panels, ncol = 3) +
    patchwork::plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
}

fig1_ratio <- make_fig1("norm_ratio", "Ratio to site mean")
fig1_pct   <- make_fig1("norm_pct",   "% change from baseline")

msg("Figure 1 (both normalisations) built.")

## ============================================================================
## 5. Figure 2 — by PFT (DBF, ENF; BE-Vie/MF excluded)
## ============================================================================
pft_sites <- site_meta |> filter(igbp %in% c("DBF", "ENF"))
pft_metrics <- metrics_long |>
  inner_join(pft_sites |> select(site_id, igbp), by = "site_id")

build_pft_panel <- function(pft, norm_col, ylab) {
  d <- pft_metrics |> filter(igbp == pft) |> arrange(year)
  n_sites <- n_distinct(d$site_id)

  med <- d |>
    filter(!is.na(.data[[norm_col]])) |>
    group_by(year, metric) |>
    summarise(med_val = median(.data[[norm_col]]), .groups = "drop")

  days_mean <- screen_attrition |>
    inner_join(pft_sites |> filter(igbp == pft) |> select(site_id), by = "site_id") |>
    group_by(year) |>
    summarise(mean_valid_days = mean(valid_days), .groups = "drop")

  yrange <- panel_y_range(c(d[[norm_col]], med$med_val))
  bars <- bar_rescale(days_mean, "mean_valid_days", yrange)
  days_mean$bar_y <- bars$floor + days_mean$mean_valid_days * bars$scale_factor

  p <- ggplot() +
    geom_rect(data = days_mean, aes(xmin = year - 0.4, xmax = year + 0.4, ymin = bars$floor, ymax = bar_y),
              fill = "grey88", colour = NA) +
    geom_line(data = d, aes(x = year, y = .data[[norm_col]], group = interaction(site_id, metric), colour = metric),
              linewidth = nature_lwd(0.25), alpha = 0.25, na.rm = TRUE) +
    geom_line(data = med, aes(x = year, y = med_val, colour = metric), linewidth = nature_lwd(0.9), na.rm = TRUE) +
    scale_colour_manual(values = METRIC_COLOURS, name = NULL, drop = FALSE) +
    scale_y_continuous(
      limits = yrange,
      labels = if (grepl("pct", norm_col)) nature_minus_labels() else waiver(),
      sec.axis = sec_axis(~ (. - bars$floor) / bars$scale_factor, name = "Mean valid days")
    ) +
    labs(x = NULL, y = ylab, title = pft) +
    site_panel_title_theme +
    theme(axis.text.x = element_text(size = NATURE_SMALL_PT)) +
    annotate("text", x = Inf, y = Inf, label = paste0("n = ", n_sites, " sites"),
              hjust = 1.05, vjust = 1.4, size = NATURE_SMALL_PT / .pt, family = NATURE_FONT, colour = "grey30")
  p
}

make_fig2 <- function(norm_col, ylab) {
  panels <- lapply(c("DBF", "ENF"), build_pft_panel, norm_col = norm_col, ylab = ylab)
  patchwork::wrap_plots(panels, ncol = 2) +
    patchwork::plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
}

fig2_ratio <- make_fig2("norm_ratio", "Median ratio to site mean")
fig2_pct   <- make_fig2("norm_pct",   "Median % change from baseline")

msg("Figure 2 (both normalisations) built.")

## ============================================================================
## 6. Figure 0 — map
## ============================================================================
.disable_s2()
land <- .land_sf()
map_pts <- site_meta |> rename(x = location_long, y = location_lat)

## NOT via .map_base() -- that helper's own theme() sets legend.text/
## legend.title to ggtext::element_markdown(), which ggplot2 refuses to
## merge with nature_theme()'s later element_text() override ("Only
## elements of the same class can be merged" -- the same documented gotcha
## R/nature_format.R's header describes for axis.title, here hitting the
## legend instead). Built directly from .land_sf()'s output + a plain
## theme_void() base so nature_theme() has only ordinary element_text()
## slots to override.
##
## Region bounding boxes -- MUST match .apply_region()'s own internal
## switch() exactly (north_america/europe lon/lat limits), copied here
## (not re-derived from it) so points/labels for sites outside a panel's
## region can be filtered out BEFORE plotting. coord_sf(xlim=, ylim=) only
## crops the rendered viewport -- it does NOT stop ggrepel::geom_text_repel()
## from placing a label (with a leader line) for an off-panel point
## somewhere inside the visible frame, confirmed directly: the first render
## showed "BE-Vie"/"DE-Tha"/"FI-Hyy" labels bleeding into the North America
## panel and every US site's label bleeding into the Europe panel.
REGION_BBOX <- list(
  north_america = c(xmin = -170, xmax = -50, ymin = 5,  ymax = 83),
  europe        = c(xmin = -25,  xmax = 45,  ymin = 34, ymax = 72)
)
## Same IGBP domain (limits=) in both panels' fill scale, even though each
## panel's own sites only cover a subset of these classes -- so patchwork
## merges the two into one identical, complete legend rather than two
## different partial ones.
IGBP_USED <- unique(site_meta$igbp)

## include_full_legend = TRUE adds a layer carrying all of IGBP_USED's
## classes, positioned off-panel (x = y = -9999, well outside every
## REGION_BBOX so coord_sf's xlim/ylim clips it from the rendered map) so
## the legend has a real data row for every class. Two things that did NOT
## work, confirmed by direct reproduction: (1) scale_fill_manual(limits=,
## drop=FALSE) alone -- a legend key for a class with zero data rows renders
## its text label but an EMPTY swatch, a geom_point draw_key gap, not a
## scale/guide option; (2) an alpha=0 phantom layer -- draw_key_point()
## inherits the layer's own alpha for the KEY GLYPH too, so alpha=0 makes
## the swatch invisible exactly like the point itself. Off-panel positioning
## (ordinary alpha = 1) avoids both failure modes.
build_region_map <- function(region, include_full_legend = FALSE) {
  bb <- REGION_BBOX[[region]]
  pts <- map_pts |>
    filter(x >= bb["xmin"], x <= bb["xmax"], y >= bb["ymin"], y <= bb["ymax"])

  p <- ggplot()
  if (include_full_legend) {
    legend_dummy <- data.frame(igbp = IGBP_USED, x = -9999, y = -9999)
    p <- p + geom_point(data = legend_dummy, aes(x = x, y = y, fill = igbp), shape = 21,
                         colour = "black", size = 3, stroke = 0.4)
  }
  p <- p +
    geom_sf(data = land, fill = "gray95", colour = "black", linewidth = nature_lwd(0.25)) +
    geom_point(data = pts, aes(x = x, y = y, fill = igbp), shape = 21,
               colour = "black", size = 3, stroke = 0.4) +
    ggrepel::geom_text_repel(data = pts, aes(x = x, y = y, label = site_id),
                              size = NATURE_SMALL_PT / .pt, colour = "black", seed = 42,
                              min.segment.length = 0.1, segment.size = nature_lwd(0.25),
                              box.padding = 0.3, point.padding = 0.2) +
    scale_fill_paper_igbp(name = "IGBP", limits = IGBP_USED, drop = FALSE) +
    theme_void() +
    nature_theme()
  .apply_region(p, region)
}

## plot_layout(guides = "collect") did not merge the two panels' otherwise-
## identical IGBP legends into one (confirmed directly: two side-by-side
## "IGBP ENF DBF MF" legends rendered) -- simpler and robust fix: show the
## legend on one panel only. Both scales share limits = IGBP_USED, so the
## single legend still lists all classes present across BOTH panels, not
## just panel a's own subset.
fig0_na <- build_region_map("north_america", include_full_legend = TRUE) + panel_letter("a") + theme(legend.position = "bottom")
fig0_eu <- build_region_map("europe") + panel_letter("b") + theme(legend.position = "none")
fig0_map <- patchwork::wrap_plots(fig0_na, fig0_eu, ncol = 2)

msg("Figure 0 (map) built.")

## ============================================================================
## 7. Figure 3 — rain source summary (11 sites, NL-Loo already absent)
## ============================================================================
wet_share_wide <- wet_freq |>
  select(site_id, series, share_wet) |>
  pivot_wider(names_from = series, values_from = share_wet) |>
  left_join(site_meta |> select(site_id, igbp_order, location_lat), by = "site_id") |>
  arrange(igbp_order, desc(location_lat)) |>
  mutate(site_id = factor(site_id, levels = site_id))

write_csv_meta(wet_share_wide |> select(site_id, gauge, P_ERA), file.path(OUT_TBL, "fig3_wet_day_share.csv"),
  notes = "Share of fully measured days with daily total > 0 mm, gauge vs P_ERA (tables/precip_compare/wet_day_frequency.csv, period=all, threshold=0).")

fig3a <- ggplot(wet_share_wide) +
  geom_segment(aes(x = site_id, xend = site_id, y = gauge, yend = P_ERA), linewidth = nature_lwd(0.4), colour = "grey50") +
  geom_point(aes(x = site_id, y = gauge, colour = "Gauge"), size = 1.2) +
  geom_point(aes(x = site_id, y = P_ERA, colour = "P_ERA"), size = 1.2) +
  scale_colour_manual(values = c(Gauge = "#1F78B4", P_ERA = "#E66101"), name = NULL) +
  coord_flip() +
  labs(x = NULL, y = "Share of fully measured days wet (> 0 mm)") +
  nature_theme() + theme(legend.position = "bottom") +
  panel_tag("a")

days_removed_mean <- rain_rule |>
  group_by(site_id) |>
  summarise(mean_days_removed_gauge = mean(days_removed_gauge),
            mean_days_removed_era   = mean(days_removed_era), .groups = "drop") |>
  left_join(site_meta |> select(site_id, igbp_order, location_lat), by = "site_id") |>
  arrange(igbp_order, desc(location_lat)) |>
  mutate(site_id = factor(site_id, levels = site_id))

write_csv_meta(days_removed_mean |> select(site_id, mean_days_removed_gauge, mean_days_removed_era),
  file.path(OUT_TBL, "fig3_days_removed_mean.csv"),
  notes = "Mean days removed per year by the rain rule, gauge-driven vs P_ERA-driven, over site-years with >=350 fully measured days (tables/precip_compare/rain_rule_consequence.csv).")

days_removed_long <- days_removed_mean |>
  select(site_id, mean_days_removed_gauge, mean_days_removed_era) |>
  pivot_longer(-site_id, names_to = "source", values_to = "days") |>
  mutate(source = recode(source, mean_days_removed_gauge = "Gauge", mean_days_removed_era = "P_ERA"))

fig3b <- ggplot(days_removed_long, aes(x = site_id, y = days, fill = source)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = c(Gauge = "#1F78B4", P_ERA = "#E66101"), name = NULL) +
  coord_flip() +
  labs(x = NULL, y = "Mean days removed per year by the rain rule") +
  nature_theme() + theme(legend.position = "bottom", axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  panel_tag("b")

fig3 <- patchwork::wrap_plots(fig3a, fig3b, ncol = 2)

msg("Figure 3 (rain source summary) built.")

## ============================================================================
## 8. Figure 4 — rain rule by year, redrawn (11 sites)
## ============================================================================
rrc_long <- rain_rule |>
  select(site_id, year, days_removed_gauge, days_removed_era) |>
  pivot_longer(c(days_removed_gauge, days_removed_era), names_to = "source", values_to = "days") |>
  mutate(source = recode(source, days_removed_gauge = "Gauge", days_removed_era = "P_ERA"),
         site_id = factor(site_id, levels = site_order))

write_csv_meta(rrc_long, file.path(OUT_TBL, "fig4_days_removed_by_year.csv"),
  notes = "Days removed per site-year by the rain rule, gauge-driven vs P_ERA-driven (tables/precip_compare/rain_rule_consequence.csv, site-years with >=350 fully measured days only).")

## Whole-year breaks explicitly, adaptive step by panel range -- ggplot2's
## default continuous breaks on a sparse single/two-year panel (e.g. US-Fuf,
## only 1 qualifying year) otherwise produce fractional labels like
## "2005.6", confirmed directly. Under facet_wrap(scales = "free_x") this
## breaks function is called once per panel, against that panel's own range.
integer_year_breaks <- function(x) {
  rng <- range(x, na.rm = TRUE)
  width <- diff(rng)
  if (!is.finite(width) || width <= 0) return(round(rng[1]))
  step <- if (width <= 5) 1 else if (width <= 10) 2 else if (width <= 20) 5 else 10
  seq(ceiling(rng[1] / step) * step, floor(rng[2] / step) * step, by = step)
}

fig4 <- ggplot(rrc_long, aes(x = year, y = days, fill = source)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c(Gauge = "#1F78B4", P_ERA = "#E66101"), name = NULL) +
  scale_x_continuous(breaks = integer_year_breaks, labels = function(x) round(x)) +
  facet_wrap(~site_id, ncol = 3, scales = "free_x") +
  labs(x = NULL, y = "Days removed by the rain rule") +
  nature_theme() +
  theme(legend.position = "bottom", strip.text = element_text(size = NATURE_BASE_PT, face = "bold"),
        axis.text.x = element_text(size = NATURE_SMALL_PT, angle = 90, vjust = 0.5, hjust = 1))

msg("Figure 4 (rain rule by year) built.")

## ============================================================================
## 9. Save all figures (PNG + PDF via save_nature_figure(), plus a
## .meta.json companion per figure -- CLAUDE.md "Output Metadata" applies to
## every output file, not just tables).
## ============================================================================
save_fig_with_meta <- function(plot, stem, width_mm, height_mm, notes = "") {
  out <- save_nature_figure(plot, stem, width_mm = width_mm, height_mm = height_mm)
  write_meta(out$png, notes = notes)
  invisible(out)
}

save_fig_with_meta(fig0_map,   file.path(FIG_DIR, "fig0_map_sites"),                   width_mm = NATURE_WIDTH_DOUBLE_MM, height_mm = 110)
save_fig_with_meta(fig1_ratio, file.path(FIG_DIR, "fig1_by_site_ratio_to_mean"),        width_mm = NATURE_WIDTH_DOUBLE_MM, height_mm = 240)
save_fig_with_meta(fig1_pct,   file.path(FIG_DIR, "fig1_by_site_pct_change_guerrieri"), width_mm = NATURE_WIDTH_DOUBLE_MM, height_mm = 240)
save_fig_with_meta(fig2_ratio, file.path(FIG_DIR, "fig2_by_pft_ratio_to_mean"),         width_mm = NATURE_WIDTH_DOUBLE_MM, height_mm = 100)
save_fig_with_meta(fig2_pct,   file.path(FIG_DIR, "fig2_by_pft_pct_change_guerrieri"),  width_mm = NATURE_WIDTH_DOUBLE_MM, height_mm = 100)
save_fig_with_meta(fig3,       file.path(FIG_DIR, "fig3_rain_source_summary"),          width_mm = NATURE_WIDTH_DOUBLE_MM, height_mm = 110)
save_fig_with_meta(fig4,       file.path(FIG_DIR, "fig4_rain_rule_by_year"),            width_mm = NATURE_WIDTH_DOUBLE_MM, height_mm = 240)

msg("All figures saved to ", FIG_DIR)

## ============================================================================
## 10. Per-figure .legend.txt + consolidated docs/handoff_figure_legends_<date>.md
## ============================================================================
legends <- list(
  fig0_map_sites = c(
    "FIGURE LEGEND -- fig0_map_sites",
    "TITLE: Map of the 11 WUE isotope pilot sites",
    "DESCRIPTION: Panel a, North America; panel b, Europe. Each site is a large dot coloured by",
    "IGBP class (PAPER_IGBP_COLOURS/PAPER_IGBP_ORDER), labelled with its site code. No values are",
    "plotted here, only site location and vegetation class."
  ),
  fig1_by_site_ratio_to_mean = c(
    "FIGURE LEGEND -- fig1_by_site_ratio_to_mean",
    "TITLE: WUE, IWUE, uWUE and k* by site, normalised to the site's own mean",
    "DESCRIPTION: One panel per site (11), ordered by IGBP then latitude, each titled with the",
    "site code and the site's IGBP/Koppen class in the top-right corner. Four coloured lines",
    "(WUE, IWUE, uWUE, sub-daily k*) show each metric's annual value divided by that site's own",
    "mean over its plotted years. k* excludes site-years where it sat on a grid-search limit (0",
    "or 1.5). Site-years with fewer than 10 valid days are open points; kept years with zero",
    "valid days (US-Ho2 2012, 2013) show no points. Light grey bars (secondary axis, right) show",
    "valid days per year. Year labels on the x-axis are red where fewer than 80% of that year's",
    "days had a fully gauge-measured precipitation record. No trend tests or slopes are shown."
  ),
  fig1_by_site_pct_change_guerrieri = c(
    "FIGURE LEGEND -- fig1_by_site_pct_change_guerrieri",
    "TITLE: WUE, IWUE, uWUE and k* by site, percent change from each site's first qualifying year",
    "DESCRIPTION: Same layout and panels as fig1_by_site_ratio_to_mean, but each metric is shown",
    "as percent change from the site's first year with a value and at least 10 valid days (0%",
    "baseline), following Guerrieri et al. (2019)'s normalisation. Open points, zero-valid-day",
    "years, background valid-day bars, and red gauge-coverage year labels as in the ratio",
    "version. No trend tests or slopes are shown."
  ),
  fig2_by_pft_ratio_to_mean = c(
    "FIGURE LEGEND -- fig2_by_pft_ratio_to_mean",
    "TITLE: WUE, IWUE, uWUE and k* by plant functional type, normalised to each site's own mean",
    "DESCRIPTION: Two panels, DBF and ENF (BE-Vie, the only MF site among the 11, is left out of",
    "this figure). Thin faint lines show each contributing site's own ratio-to-mean series; the",
    "bold line is the across-site median per calendar year, per metric. Grey background bars",
    "(secondary axis) show the mean valid days per year across contributing sites. The number of",
    "sites in the group is noted in the top-right corner. No trend tests or slopes are shown."
  ),
  fig2_by_pft_pct_change_guerrieri = c(
    "FIGURE LEGEND -- fig2_by_pft_pct_change_guerrieri",
    "TITLE: WUE, IWUE, uWUE and k* by plant functional type, percent change from baseline",
    "DESCRIPTION: Same layout as fig2_by_pft_ratio_to_mean, but each site's series is percent",
    "change from its own first year with a value and at least 10 valid days before the",
    "across-site median is taken, per calendar year and metric. No trend tests or slopes are",
    "shown."
  ),
  fig3_rain_source_summary = c(
    "FIGURE LEGEND -- fig3_rain_source_summary",
    "TITLE: Rain source summary, 11 sites",
    "DESCRIPTION: Panel a: share of each site's fully measured days with a daily total above 0 mm,",
    "by the tower gauge and by P_ERA (one pair of points per site, connected by a grey segment).",
    "Panel b: mean days removed per year by the stage 2 rain rule when driven by the gauge versus",
    "by P_ERA, over site-years with at least 350 fully measured days. Sites in both panels are",
    "ordered by IGBP then latitude. No cause is asserted."
  ),
  fig4_rain_rule_by_year = c(
    "FIGURE LEGEND -- fig4_rain_rule_by_year",
    "TITLE: Days removed by the rain rule, by site-year and source",
    "DESCRIPTION: One panel per site (11), days removed per year by the stage 2 rain rule, gauge-",
    "driven versus P_ERA-driven, for site-years with at least 350 fully measured days. Redraws",
    "figures/precip_compare/fig_days_removed_by_source.png in this hand-off's Nature format. No",
    "trend tests or slopes are shown."
  )
)

for (nm in names(legends)) {
  txt <- c(legends[[nm]], "", strrep("=", 60))
  writeLines(txt, file.path(FIG_DIR, paste0(nm, ".legend.txt")))
}

legend_doc_path <- file.path(DOCS_DIR, paste0("handoff_figure_legends_", format(Sys.Date(), "%Y%m%d"), ".md"))
doc_lines <- c(
  paste0("# WUE isotope pilot — hand-off figure legends (", Sys.Date(), ")"),
  "",
  "Side analysis, not the FLUXNET Annual Paper 2026. Draft legends only, at most 200 words each.",
  "No interpretation: no trend tests, no Sen's slopes, no statements about what a series means.",
  ""
)
word_counts <- character(0)
for (nm in names(legends)) {
  title <- sub("^TITLE: ", "", legends[[nm]][2])
  body  <- paste(legends[[nm]][-c(1, 2)], collapse = " ")
  body  <- sub("^DESCRIPTION: ", "", body)
  wc <- lengths(strsplit(body, "\\s+"))
  word_counts <- c(word_counts, paste0(nm, ": ", wc, " words"))
  doc_lines <- c(doc_lines,
    paste0("## ", title, " (`", nm, "`)"),
    "",
    body,
    ""
  )
}
writeLines(doc_lines, legend_doc_path)
msg("Legend doc written: ", legend_doc_path)
msg(paste(word_counts, collapse = " | "))

msg("13_handoff_figures.R complete.")
