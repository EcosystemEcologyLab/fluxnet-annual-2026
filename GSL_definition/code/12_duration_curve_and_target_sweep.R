## 12_duration_curve_and_target_sweep.R — three proposed analyses on Tash's
## 62-site reproduction run, for discussion with her (proposed by the context
## agent, approved by Dave). See logs/prompts/2026-11-03_gsl_duration_curve_target_sweep.md.
##
## Does NOT modify any existing script, table, or figure. Main run only
## (gpp_variant = "main" [GPP_NT_VUT_REF], neg_gpp_variant = "main"
## [negative daily GPP set to 0], calendar years, GSL_SITES' own year list,
## no winter screen) -- the same run 04_reproduce.R checks against the
## manuscript. Reuses 03_derive.R's existing fine_grid.rds/fraction_sweep.rds/
## derived_site.csv wherever they already have what's needed; recomputes only
## what's missing (the 0.85 crossing; the selected threshold at each of 19
## target shares, not just 0.95; near-zero-day shares and partial skewness at
## three cuts; and per-threshold across-year GSL sd/trend on the 22-point
## Tash grid). All recomputation draws on the already-cached
## data/daily_extract.rds (62 sites, FLUXMET-equivalent daily GPP) -- no
## DuckDB connection is opened by this script, since nothing it needs is
## missing from that cache.
##
## Three analyses:
##   1. GPP-duration curve: mean GSL(T) vs. mean F(T) across the fine grid,
##      per site. Z16 and the 0.85/0.95 share-crossings are points on this
##      curve.
##   2. Tash's Figure 4 relationship (selected threshold vs. skewness) swept
##      across target shares 0.80-0.98, with and without AU-ASM, checked
##      against her published r = -0.68 / adjusted R2 = 0.57 at 0.95.
##   3. Two kinds of skew: near-zero-day share vs. above-the-cut partial
##      skewness, as competing predictors of threshold sensitivity.
##
## Output (git-tracked):
##   tables/duration_curve_crossings.csv
##   tables/target_sweep_thresholds.csv
##   tables/target_sweep_fits.csv
##   tables/skew_predictors.csv
##   tables/skew_predictor_correlations.csv
##   tables/skew_predictor_two_predictor_regression.csv
##   figures/drafts/proposal/Figure P1a (duration curve examples).{png,pdf,legend.txt,meta.json}
##   figures/drafts/proposal/Figure P1b (duration curve all sites).{png,pdf,legend.txt,meta.json}
##   figures/drafts/proposal/Figure P1c (duration curve exemplars).{png,pdf,legend.txt,meta.json}
##   figures/drafts/proposal/Figure P1d (sensitivity and Z16 capture vs skewness).{png,pdf,legend.txt,meta.json}
##   figures/drafts/proposal/Figure P2 (target share sweep).{png,pdf,legend.txt,meta.json}
##
## 2026-11-03 redraw/fixes (logs/prompts/2026-11-03_gsl_duration_curve_redraw_fixes.md):
## added Figures P1c/P1d (P1b's all-62-site continuous-skewness colour scale
## does not order these curves, since the two kinds of skew pull curves in
## opposite directions -- see P1c's caption); fixed Figure P2's reversed
## solid/dashed caption wording, switched panel (a) to quadratic fit lines,
## and moved panel (b)'s legend off the data. P1a and P1b themselves are
## unchanged, per that prompt's instruction. No existing table recomputed;
## built from duration_curve_crossings.csv/skew_predictors.csv/fine_grid.rds,
## already loaded above.

source("GSL_definition/code/00_config.R")
options(gsl.skip_driver = TRUE)
source("GSL_definition/code/03_derive.R")  # FINE_GRID_PCT, TASH_GRID_PCT, FRACTION_TARGETS,
options(gsl.skip_driver = FALSE)           # g1_adjusted(), year_threshold_matrices() -- reused, not copied
source("GSL_definition/code/fig_helpers.R")

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(purrr); library(lubridate)
  library(ggplot2); library(patchwork)
})

## "Draft for discussion" title -- same helper as GSL_definition/expanded/code/12_methods_review_figures.R
## (not shared code; this is a self-contained copy, per the "do not change any existing script" rule).
draft_title <- function(p) {
  if (inherits(p, "patchwork")) {
    p + patchwork::plot_annotation(title = "Draft for discussion",
          theme = theme(plot.title = element_text(size = PEERJ_AXIS_TITLE_PT, face = "bold", colour = "grey10", hjust = 0)))
  } else {
    p + labs(title = "Draft for discussion") +
      theme(plot.title = element_text(size = PEERJ_AXIS_TITLE_PT, face = "bold", colour = "grey10", hjust = 0))
  }
}

## ---- Load the existing main-run outputs (reused, not recomputed) ---------
derived_site <- read_csv(file.path(GSL_ROOT, "tables", "derived_site.csv"), show_col_types = FALSE) |>
  filter(gpp_variant == "main", neg_gpp_variant == "main")
fine_grid <- readRDS(file.path(GSL_ROOT, "data", "fine_grid.rds")) |>
  filter(gpp_variant == "main", neg_gpp_variant == "main")
fraction_sweep <- readRDS(file.path(GSL_ROOT, "data", "fraction_sweep.rds")) |>
  filter(gpp_variant == "main", neg_gpp_variant == "main")

daily_extract <- readRDS(file.path(GSL_ROOT, "data", "daily_extract.rds"))
daily_extract$TIMESTAMP <- as.Date(as.character(daily_extract$TIMESTAMP))
daily_extract$year <- lubridate::year(daily_extract$TIMESTAMP)
gpp_col <- GSL_GPP_VARIANTS[["main"]]

## skew_predictors.csv (2026-11-03, commit cc1809b) read here, ahead of
## Section 3 below which recomputes and re-writes it, so Figure P1d (Section
## 1, redraw/fixes prompt) can reuse near_zero_share_1pct/partial_skew_ge_1pct
## without duplicating that computation -- per "build from the existing
## tables/skew_predictors.csv... no new data processing" in that prompt.
## Section 3's own recomputation is deterministic from the same main-run
## inputs and overwrites this object later with an identical result; nothing
## downstream of that point depends on which copy is used.
skew_predictors <- read_csv(file.path(GSL_ROOT, "tables", "skew_predictors.csv"), show_col_types = FALSE)

AU_ASM <- "AU-ASM"

message("=== Loaded main-run outputs: ", n_distinct(fine_grid$site_id), " sites, ",
        nrow(fine_grid), " fine-grid rows, ", nrow(fraction_sweep), " fraction-sweep rows ===")

## ============================================================================
## 1. GPP-duration curve
## ============================================================================

## Share-crossing: the first fine-grid point (ascending threshold) where mean
## F rounds to <= target -- identical rule to 03_derive.R's own T95
## (t95_idx), generalised to any target and computed here from the ALREADY-
## COMPUTED fine_grid (no raw-data recomputation).
share_crossing <- function(site_fine, target) {
  site_fine <- site_fine[order(site_fine$pct_of_P99), ]
  f_rounded <- round(site_fine$mean_f, 6)
  idx <- which(f_rounded <= target)[1]
  if (is.na(idx)) {
    return(tibble(pct = NA_real_, gsl = NA_real_, f = NA_real_))
  }
  tibble(pct = site_fine$pct_of_P99[idx], gsl = site_fine$mean_gsl[idx], f = site_fine$mean_f[idx])
}

crossings <- fine_grid |>
  group_by(site_id) |>
  group_modify(~ bind_cols(
    share_crossing(.x, 0.85) |> rename(t85_pct = pct, t85_gsl = gsl, t85_f = f),
    share_crossing(.x, 0.95) |> rename(t95_pct = pct, t95_gsl = gsl, t95_f = f)
  )) |>
  ungroup()

## Sanity check: the 0.95 crossing recomputed here from fine_grid.rds must
## match derived_site.csv's own t95_pct/t95_mean_gsl exactly (same rule, same
## source grid) -- if it doesn't, something about the "reuse, don't
## recompute" assumption is wrong and this must stop, not silently diverge.
t95_check <- crossings |> select(site_id, t85_pct, t95_pct_here = t95_pct) |>
  left_join(derived_site |> select(site_id, t95_pct), by = "site_id")
t95_mismatch <- t95_check |> filter(abs(t95_pct_here - t95_pct) > 1e-9 | (is.na(t95_pct_here) != is.na(t95_pct)))
if (nrow(t95_mismatch) > 0) {
  stop("0.95 crossing recomputed from fine_grid.rds does not match derived_site.csv's t95_pct for: ",
       paste(t95_mismatch$site_id, collapse = ", "))
}
message("Checked: 0.95 crossing recomputed from fine_grid.rds matches derived_site.csv's t95_pct for all ",
        nrow(t95_check), " sites.")

duration_curve_table <- derived_site |>
  select(site_id, skewness, skewness_third, z16_mean_gsl, z16_mean_f) |>
  left_join(crossings, by = "site_id") |>
  arrange(skewness)

write_csv(duration_curve_table, file.path(GSL_ROOT, "tables", "duration_curve_crossings.csv"))
write_meta(
  file.path(GSL_ROOT, "tables", "duration_curve_crossings.csv"),
  input_sources = c(file.path(GSL_ROOT, "data", "fine_grid.rds"), file.path(GSL_ROOT, "tables", "derived_site.csv")),
  notes = paste0(
    "Main run (GPP_NT_VUT_REF, negative GPP set to 0, calendar years, no winter screen). Per site: ",
    "Z16 = mean GSL/F at 10% of P99 (z16_mean_gsl/z16_mean_f, from derived_site.csv). t85/t95 = the ",
    "GPP-duration curve's crossing of mean F = 0.85/0.95 -- the first fine-grid threshold (ascending, ",
    "0.01-50% of P99) where mean F rounds (6 d.p.) to <= that share; t95 recomputed here from ",
    "fine_grid.rds and checked to match derived_site.csv's own t95_pct exactly. NA where a site's ",
    "curve never reaches that share (mean F's maximum stays above it even at 50% of P99, or drops ",
    "below it before the grid starts)."
  )
)

n_t85_na <- sum(is.na(duration_curve_table$t85_pct))
n_t95_na <- sum(is.na(duration_curve_table$t95_pct))
message("duration_curve_crossings: ", nrow(duration_curve_table), " sites; t85 NA for ", n_t85_na,
        "; t95 NA for ", n_t95_na, ".")

## ---- Figure P1a: AU-ASM and AU-How, one panel each ------------------------
pair_sites <- c("AU-ASM", "AU-How")
stopifnot(all(pair_sites %in% fine_grid$site_id))

one_duration_panel <- function(sid, letter) {
  d <- fine_grid |> filter(site_id == sid) |> arrange(pct_of_P99)
  tash_seg <- d |> filter(pct_of_P99 >= 0.1, pct_of_P99 <= 20)
  row <- duration_curve_table |> filter(site_id == sid)
  markers <- tibble(
    label = c("Z16", "0.85", "0.95"),
    gsl = c(row$z16_mean_gsl, row$t85_gsl, row$t95_gsl),
    f = c(row$z16_mean_f, row$t85_f, row$t95_f)
  )
  skew_lab <- sprintf("%s (skewness = %.2f)", sid, row$skewness)

  ggplot(d, aes(x = mean_gsl, y = mean_f)) +
    geom_path(colour = "grey55", linewidth = nature_lwd(PEERJ_LWD_REF)) +
    geom_path(data = tash_seg, aes(x = mean_gsl, y = mean_f), colour = "#0072B2",
              linewidth = nature_lwd(PEERJ_LWD_DATA)) +
    geom_point(data = markers, aes(x = gsl, y = f, shape = label), colour = "black",
               size = 2.1, stroke = nature_lwd(PEERJ_LWD_DATA), na.rm = TRUE) +
    scale_shape_manual(values = c("Z16" = 16, "0.85" = 17, "0.95" = 15), name = NULL) +
    labs(x = "Mean days above threshold (GSL)", y = "Mean included fraction of annual GPP",
         subtitle = skew_lab) +
    theme_gsl() +
    theme(plot.subtitle = element_text(size = PEERJ_BASE_PT, colour = "grey10"),
          legend.position = c(0.97, 0.03), legend.justification = c(1, 0),
          legend.background = element_rect(fill = "white", colour = NA),
          legend.key = element_blank(), legend.text = element_text(size = PEERJ_BASE_PT)) +
    gsl_panel_letter(letter)
}

p1a_1 <- one_duration_panel("AU-ASM", "a")
p1a_2 <- one_duration_panel("AU-How", "b")
fig_p1a <- draft_title((p1a_1 | p1a_2))

legend_p1a <- c(
  "Draft for discussion. How much of a site's annual photosynthesis falls inside the growing season",
  "depends on where the threshold is set. Each panel traces that trade-off for one site: as the",
  "threshold rises (fewer days counted, moving left), the share of the year's total photosynthesis",
  "those days still capture falls (moving down). The blue stretch of the curve is the range Tash's own",
  "22-value grid covers; the circle, triangle, and square mark three specific ways of choosing a",
  "threshold (a fixed small value, or a point chosen to capture 85% or 95% of the year's photosynthesis).",
  "AU-ASM (highly skewed photosynthesis, dominated by a short wet season) and AU-How (far less skewed)",
  "show how differently those three choices behave depending on the site."
)
save_gsl_figure(
  fig_p1a, "drafts/proposal/Figure P1a (duration curve examples)",
  width_mm = 170, height_mm = 95, legend_lines = legend_p1a,
  input_sources = c(file.path(GSL_ROOT, "data", "fine_grid.rds"), file.path(GSL_ROOT, "tables", "duration_curve_crossings.csv")),
  notes = "Draft for discussion (proposal). Panels a=AU-ASM, b=AU-How. Blue segment = 0.1-20% of P99 (Tash's 22-value grid range). Markers: circle=Z16 (10% of P99), triangle=0.85 crossing, square=0.95 crossing."
)

## ---- Figure P1b: all 62 sites, coloured by skewness (continuous) ---------
z16_pts <- derived_site |> select(site_id, skewness, z16_mean_gsl, z16_mean_f)

p1b <- ggplot(fine_grid |> left_join(derived_site |> select(site_id, skewness), by = "site_id"),
              aes(x = mean_gsl, y = mean_f, group = site_id, colour = skewness)) +
  geom_path(linewidth = nature_lwd(PEERJ_LWD_REF), alpha = 0.85) +
  geom_point(data = z16_pts, aes(x = z16_mean_gsl, y = z16_mean_f, colour = skewness),
             inherit.aes = FALSE, shape = 16, size = 1.4) +
  scale_colour_viridis_c(name = "Skewness", option = "viridis") +
  labs(x = "Mean days above threshold (GSL)", y = "Mean included fraction of annual GPP") +
  theme_gsl() +
  theme(legend.key.height = unit(10, "mm")) +
  gsl_panel_letter("a")

fig_p1b <- draft_title(p1b)

legend_p1b <- c(
  "Draft for discussion. The same trade-off as Figure P1a (days counted vs. share of annual",
  "photosynthesis captured), shown for all 62 sites at once, each as one curve coloured by how skewed",
  "its daily photosynthesis is (darker/purple = more evenly spread across the year, brighter/yellow =",
  "more concentrated in a short burst). The dots mark each site's Z16 point (a fixed 10%-of-P99",
  "threshold). Skewness alone does not order these curves cleanly -- a short burst of photosynthesis",
  "after many near-zero days and a long tail of low but nonzero days can both raise skewness while",
  "pulling a curve in different directions; see Figure P1c for named examples of both kinds."
)  ## wording fixed 2026-11-03 (redraw/fixes prompt): removed the incorrect "low-skew curves sit
   ## higher... high-skew sites drop away" monotone-ordering claim. The PNG/PDF image itself is
   ## unchanged (same plot object, same save_gsl_figure() call) -- only this caption text changed.
save_gsl_figure(
  fig_p1b, "drafts/proposal/Figure P1b (duration curve all sites)",
  width_mm = 150, height_mm = 130, legend_lines = legend_p1b,
  input_sources = c(file.path(GSL_ROOT, "data", "fine_grid.rds"), file.path(GSL_ROOT, "tables", "derived_site.csv")),
  notes = "Draft for discussion (proposal). All 62 sites' GPP-duration curves, continuous skewness colour scale (viridis), Z16 points marked."
)

## ---- Figure P1c: five exemplar duration curves, replacing P1b's continuous
## skewness colour scale for a readable view of individual curve SHAPES.
## Skewness alone does not order these curves -- a short burst of GPP after
## many near-zero days (US-Wkg) and a long tail of low-but-nonzero days
## (US-SRG) both raise skewness but pull the curve in opposite directions
## (steep-then-flat vs. a smooth decline), so a single colour scale cannot
## convey which kind of skew a site has. Five sites spanning that range are
## picked out in distinct (non-ordered) colours; all other 57 sites are thin
## grey background. Reuses fine_grid.rds/duration_curve_crossings.csv --
## P1a/P1b themselves are unchanged. ----
EXEMPLAR_SITES <- c("AU-How", "US-MMS", "US-Wkg", "US-SRG", "AU-ASM")
EXEMPLAR_COLORS <- c(
  "AU-How" = "#0072B2", "US-MMS" = "#009E73", "US-Wkg" = "#E69F00",
  "US-SRG" = "#CC79A7", "AU-ASM" = "#D55E00"
)
stopifnot(all(EXEMPLAR_SITES %in% fine_grid$site_id))

fine_grid_bg <- fine_grid |> filter(!site_id %in% EXEMPLAR_SITES)
fine_grid_ex <- fine_grid |> filter(site_id %in% EXEMPLAR_SITES) |> arrange(site_id, pct_of_P99)
tash_seg_ex <- fine_grid_ex |> filter(pct_of_P99 >= 0.1, pct_of_P99 <= 20)

markers_ex <- bind_rows(
  duration_curve_table |> filter(site_id %in% EXEMPLAR_SITES) |>
    transmute(site_id, label = "Z16", gsl = z16_mean_gsl, f = z16_mean_f),
  duration_curve_table |> filter(site_id %in% EXEMPLAR_SITES) |>
    transmute(site_id, label = "0.85", gsl = t85_gsl, f = t85_f),
  duration_curve_table |> filter(site_id %in% EXEMPLAR_SITES) |>
    transmute(site_id, label = "0.95", gsl = t95_gsl, f = t95_f)
)

## Direct end-of-line labels (no colour legend), placed at each curve's
## terminal (highest-threshold) point and repelled apart with ggrepel --
## already used elsewhere in the repo (WAFNET/fluxcom_comparison/code/
## 09_figures_draft.R) via the same namespaced call, not a new dependency.
## ggrepel's placement is stochastic without a fixed seed (the reason an
## earlier WAFNET figure's PNG drifted cosmetically on re-run) -- seeded here.
end_points_ex <- fine_grid_ex |> group_by(site_id) |> slice_max(pct_of_P99, n = 1) |> ungroup() |>
  left_join(duration_curve_table |> select(site_id, skewness), by = "site_id") |>
  mutate(end_label = sprintf("%s (%.2f)", site_id, skewness))

p1c <- ggplot() +
  geom_path(data = fine_grid_bg, aes(x = mean_gsl, y = mean_f, group = site_id),
            colour = "grey85", linewidth = nature_lwd(PEERJ_LWD_OTHERYEARS)) +
  geom_path(data = fine_grid_ex, aes(x = mean_gsl, y = mean_f, group = site_id, colour = site_id),
            linewidth = nature_lwd(PEERJ_LWD_REF)) +
  geom_path(data = tash_seg_ex, aes(x = mean_gsl, y = mean_f, group = site_id, colour = site_id),
            linewidth = nature_lwd(PEERJ_LWD_DATA)) +
  geom_point(data = markers_ex, aes(x = gsl, y = f, shape = label), colour = "black",
             size = 2.1, stroke = nature_lwd(PEERJ_LWD_DATA), na.rm = TRUE) +
  ggrepel::geom_text_repel(
    data = end_points_ex, aes(x = mean_gsl, y = mean_f, label = end_label, colour = site_id),
    size = PEERJ_ANNOTATION_PT / .pt, fontface = "bold", show.legend = FALSE,
    segment.size = nature_lwd(0.4), segment.colour = "grey40", min.segment.length = 0, seed = 42
  ) +
  scale_colour_manual(values = EXEMPLAR_COLORS, guide = "none") +
  scale_shape_manual(values = c("Z16" = 16, "0.85" = 17, "0.95" = 15), name = NULL) +
  labs(x = "Mean days above threshold (GSL)", y = "Mean included fraction of annual GPP") +
  theme_gsl() +
  theme(legend.position = c(0.97, 0.78), legend.justification = c(1, 1),
        legend.background = element_rect(fill = "white", colour = NA), legend.key = element_blank(),
        legend.text = element_text(size = PEERJ_BASE_PT))

fig_p1c <- draft_title(p1c)  ## single panel -- no panel letter (only multi-panel figures get one)

legend_p1c <- c(
  "Draft for discussion. Each site's days are ranked from highest to lowest photosynthesis (GPP);",
  "the x-axis is how many of those top-ranked days are counted per year, the y-axis is the share of",
  "the year's total photosynthesis found on those days, both averaged over years. A curve stops short",
  "of 365 days where the remaining, lowest-ranked days contribute zero. Five sites are shown in colour,",
  "spanning different shapes of skew -- from almost no near-zero days (AU-How) to many near-zero days",
  "followed by a short burst (US-Wkg) to a long tail of low but nonzero days (AU-ASM) -- with all other",
  "57 sites as thin grey background; the thicker segment of each coloured curve is the range covered by",
  "Tash's own 22-value threshold grid, and the three markers show where a fixed 10%-of-P99 threshold",
  "(circle) and two fixed shares of annual photosynthesis, 85% (triangle) and 95% (square), fall on that",
  "curve."
)
save_gsl_figure(
  fig_p1c, "drafts/proposal/Figure P1c (duration curve exemplars)",
  width_mm = 170, height_mm = 150, legend_lines = legend_p1c,
  input_sources = c(file.path(GSL_ROOT, "data", "fine_grid.rds"), file.path(GSL_ROOT, "tables", "duration_curve_crossings.csv")),
  notes = paste0(
    "Draft for discussion (proposal). Replaces Figure P1b's continuous skewness colour scale for a ",
    "readable view of curve shape (skewness does not order these curves -- see legend). Exemplar sites: ",
    "AU-How (skewness 0.36), US-MMS (0.58), US-Wkg (2.07), US-SRG (1.92), AU-ASM (3.88); all other 57 ",
    "sites as thin grey background. Thick segment = 0.1-20% of P99 (Tash's 22-value grid range). ",
    "Markers: circle=Z16 (10% of P99), triangle=0.85 crossing, square=0.95 crossing. End-of-line labels ",
    "placed with ggrepel, seed=42 for reproducibility."
  )
)

## ---- Figure P1d: one point per site, two kinds of skew shown separately --
## colour (near-zero-day share) vs. the x-axis skewness/sensitivity
## relationship itself -- so this figure and P1c connect through the same
## five exemplar sites. Panel (a): Tash's sensitivity measure (IQR of mean
## GSL across her 22-point grid) vs. skewness. Panel (b): the share of
## annual GPP inside the fixed Z16 season vs. skewness. Panel (c), added
## after checking it reads clearly (see docs/duration_curve_and_target_
## sweep_2026-11-03.md): panel (a) repeated against the skewness of only the
## above-1%-of-P99 days (partial skewness) instead of full skewness. ----
p1d_data <- duration_curve_table |> select(site_id, skewness, z16_mean_f) |>
  left_join(skew_predictors |> select(site_id, sensitivity_iqr_mean_gsl, near_zero_share_1pct, partial_skew_ge_1pct),
             by = "site_id") |>
  mutate(is_exemplar = site_id %in% EXEMPLAR_SITES)
exemplar_labels_p1d <- p1d_data |> filter(is_exemplar)

one_p1d_panel <- function(xvar, yvar, xlab, ylab, letter) {
  ggplot(p1d_data, aes(x = .data[[xvar]], y = .data[[yvar]])) +
    geom_point(aes(colour = near_zero_share_1pct, shape = is_exemplar, size = is_exemplar), na.rm = TRUE) +
    ggrepel::geom_text_repel(
      data = exemplar_labels_p1d, aes(x = .data[[xvar]], y = .data[[yvar]], label = site_id),
      colour = "grey20", size = PEERJ_ANNOTATION_PT / .pt, fontface = "bold",
      segment.size = nature_lwd(0.4), segment.colour = "grey40", min.segment.length = 0, seed = 42,
      force = 6, force_pull = 0.5, box.padding = 0.6, point.padding = 0.3, max.overlaps = Inf
      ## AU-How and US-MMS sit almost on top of each other in panel b (both
      ## near x=0.3-0.6, y=0.98-1.0) -- the default repel force left their
      ## labels overlapping there (caught on this review); a stronger force
      ## and larger padding separates them cleanly without affecting panels
      ## a/c, where the five points were already well spread out.
    ) +
    scale_colour_viridis_c(name = "Share of days with\nnear-zero GPP") +
    scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 17), guide = "none") +
    scale_size_manual(values = c("FALSE" = 1.3, "TRUE" = 2.3), guide = "none") +
    labs(x = xlab, y = ylab) +
    theme_gsl() +
    gsl_panel_letter(letter)
}

p1d_a <- one_p1d_panel("skewness", "sensitivity_iqr_mean_gsl",
                        "Skewness (full distribution)", "Sensitivity (IQR of mean GSL\nacross Tash's grid, days)", "a")
p1d_b <- one_p1d_panel("skewness", "z16_mean_f",
                        "Skewness (full distribution)", "Share of annual GPP\ninside the Z16 season", "b")
p1d_c <- one_p1d_panel("partial_skew_ge_1pct", "sensitivity_iqr_mean_gsl",
                        "Skewness of days >= 1% of P99 (partial)", "Sensitivity (IQR of mean GSL\nacross Tash's grid, days)", "c")

fig_p1d <- draft_title((p1d_a / p1d_b / p1d_c) + plot_layout(guides = "collect") &
                          theme(legend.position = "right"))

legend_p1d <- c(
  "Draft for discussion. One point per site. Panel a: how much a site's growing-season length moves",
  "across Tash's 22 threshold choices (interquartile range of the mean), against full-distribution",
  "skewness. Panel b: the share of annual photosynthesis already captured by a fixed 10%-of-P99",
  "threshold, against the same skewness. Panel c repeats panel a against the skewness of only the",
  "days with more than 1% of P99 worth of photosynthesis -- the kind of skew that actually crosses",
  "thresholds. Points are coloured by the share of days with near-zero photosynthesis; the five sites",
  "from the previous figure are labelled so the two figures connect."
)
save_gsl_figure(
  fig_p1d, "drafts/proposal/Figure P1d (sensitivity and Z16 capture vs skewness)",
  width_mm = 170, height_mm = 220, legend_lines = legend_p1d,
  input_sources = c(file.path(GSL_ROOT, "tables", "duration_curve_crossings.csv"), file.path(GSL_ROOT, "tables", "skew_predictors.csv")),
  notes = paste0(
    "Draft for discussion (proposal). Panels a/b share the x-axis (full-distribution skewness); panel c ",
    "uses partial skewness (days >= 1% of P99) instead, included after a visual check that it reads ",
    "clearly alongside a/b (see report). Colour = near_zero_share_1pct (share of days < 1% of P99), ",
    "collected into one legend via patchwork guides='collect'. Exemplar sites (same five as Figure P1c) ",
    "labelled with ggrepel, seed=42."
  )
)

## ============================================================================
## 2. Tash's Figure 4 relationship (selected threshold vs. skewness) across
##    target shares 0.80-0.98 (subset of the existing fraction_sweep.rds'
##    0.80-0.99 range; the fraction_sweep's own `gsl` column is reused for
##    nothing here -- it does not carry the threshold %, which is the one
##    thing missing and recomputed below from fine_grid.rds).
## ============================================================================

TARGET_SHARES <- round(seq(0.80, 0.98, by = 0.01), 2)  # rounded: seq()'s float accumulation otherwise
                                                        # makes e.g. 0.85 != 0.8500000000000001 downstream
stopifnot(all(round(TARGET_SHARES, 2) %in% round(FRACTION_TARGETS, 2))) # confirms these are a subset of the existing sweep's targets

threshold_at_target <- function(target) {
  fine_grid |>
    group_by(site_id) |>
    group_modify(~ tibble(threshold_pct = share_crossing(.x, target)$pct)) |>
    ungroup() |>
    mutate(target_fraction = target)
}

target_sweep_thresholds <- map_dfr(TARGET_SHARES, threshold_at_target) |>
  left_join(derived_site |> select(site_id, skewness), by = "site_id") |>
  relocate(target_fraction, site_id, skewness, threshold_pct)

write_csv(target_sweep_thresholds, file.path(GSL_ROOT, "tables", "target_sweep_thresholds.csv"))
write_meta(
  file.path(GSL_ROOT, "tables", "target_sweep_thresholds.csv"),
  input_sources = file.path(GSL_ROOT, "data", "fine_grid.rds"),
  notes = "Per site x target_fraction (0.80-0.98, step 0.01): the selected threshold (% of P99), same share-crossing rule as T95 (03_derive.R), recomputed from fine_grid.rds for every target in the sweep, not just 0.95."
)

## ---- Fits: Pearson r, linear and quadratic (raw, not orthogonal, so the
## quadratic term's coefficient is directly interpretable), with/without
## AU-ASM. cor.test()'s own Fisher-z CI is used for r's 95% CI (not a
## hand-rolled Fisher transform) -- same statistic, standard implementation. ----
fit_one <- function(df) {
  df <- df |> filter(!is.na(threshold_pct), !is.na(skewness))
  n <- nrow(df)
  if (n < 4) {
    return(tibble(n = n, r = NA_real_, r_ci_lo = NA_real_, r_ci_hi = NA_real_,
                  lin_adj_r2 = NA_real_, lin_aic = NA_real_,
                  quad_adj_r2 = NA_real_, quad_aic = NA_real_,
                  quad_b2 = NA_real_, quad_b2_t = NA_real_, quad_b2_p = NA_real_))
  }
  ct <- cor.test(df$threshold_pct, df$skewness)
  lin <- lm(threshold_pct ~ skewness, data = df)
  df$skew_sq <- df$skewness^2
  quad <- lm(threshold_pct ~ skewness + skew_sq, data = df)
  quad_s <- summary(quad)
  b2_row <- quad_s$coefficients["skew_sq", ]
  tibble(
    n = n, r = unname(ct$estimate), r_ci_lo = ct$conf.int[1], r_ci_hi = ct$conf.int[2],
    lin_adj_r2 = summary(lin)$adj.r.squared, lin_aic = AIC(lin),
    quad_adj_r2 = quad_s$adj.r.squared, quad_aic = AIC(quad),
    quad_b2 = unname(b2_row["Estimate"]), quad_b2_t = unname(b2_row["t value"]), quad_b2_p = unname(b2_row["Pr(>|t|)"])
  )
}

target_sweep_fits <- map_dfr(TARGET_SHARES, function(target) {
  df <- target_sweep_thresholds |> filter(target_fraction == target)
  with_asm <- fit_one(df) |> mutate(au_asm = "included")
  without_asm <- fit_one(df |> filter(site_id != AU_ASM)) |> mutate(au_asm = "excluded")
  bind_rows(with_asm, without_asm) |> mutate(target_fraction = target, .before = 1)
})

write_csv(target_sweep_fits, file.path(GSL_ROOT, "tables", "target_sweep_fits.csv"))
write_meta(
  file.path(GSL_ROOT, "tables", "target_sweep_fits.csv"),
  input_sources = file.path(GSL_ROOT, "tables", "target_sweep_thresholds.csv"),
  notes = paste0(
    "Per target_fraction x au_asm (included/excluded): Pearson r (threshold_pct vs. skewness) with ",
    "cor.test()'s Fisher-z 95% CI; linear fit lm(threshold_pct ~ skewness) adjusted R2 and AIC; ",
    "quadratic fit lm(threshold_pct ~ skewness + skewness^2), raw (not orthogonal) terms, adjusted R2, ",
    "AIC, and the quadratic term's own coefficient (quad_b2) with its t-statistic/p-value ",
    "(quad_b2_t/quad_b2_p). At target_fraction = 0.95, compare r and lin_adj_r2/quad_adj_r2 against ",
    "Tash's published r = -0.68, adjusted R2 = 0.57 (0.56 excluding AU-ASM)."
  )
)

check_095 <- target_sweep_fits |> filter(target_fraction == 0.95)
msg_check <- sprintf(
  paste0("Check at target=0.95 (published r=-0.68, adj.R2=0.57 incl. AU-ASM / 0.56 excl.):\n",
         "  included: r=%.4f (diff %.4f), linear adj.R2=%.4f (diff %.4f), quadratic adj.R2=%.4f (diff %.4f)\n",
         "  excluded: r=%.4f (diff %.4f), linear adj.R2=%.4f (diff %.4f), quadratic adj.R2=%.4f (diff %.4f)\n"),
  check_095$r[check_095$au_asm == "included"], check_095$r[check_095$au_asm == "included"] - (-0.68),
  check_095$lin_adj_r2[check_095$au_asm == "included"], check_095$lin_adj_r2[check_095$au_asm == "included"] - 0.57,
  check_095$quad_adj_r2[check_095$au_asm == "included"], check_095$quad_adj_r2[check_095$au_asm == "included"] - 0.57,
  check_095$r[check_095$au_asm == "excluded"], check_095$r[check_095$au_asm == "excluded"] - (-0.68),
  check_095$lin_adj_r2[check_095$au_asm == "excluded"], check_095$lin_adj_r2[check_095$au_asm == "excluded"] - 0.56,
  check_095$quad_adj_r2[check_095$au_asm == "excluded"], check_095$quad_adj_r2[check_095$au_asm == "excluded"] - 0.56
)
message(msg_check)

## ---- Figure P2: (a) small multiples at 5 target shares, (b) r vs. target --
multiples_targets <- c(0.80, 0.85, 0.90, 0.95, 0.98)
multiples_df <- target_sweep_thresholds |> filter(target_fraction %in% multiples_targets) |>
  mutate(is_au_asm = site_id == AU_ASM,
         panel_lab = sprintf("%.2f", target_fraction))

## Quadratic (not linear) fit lines -- Tash's own published adjusted R2 at
## 0.95 only matches a quadratic fit (section 2's fit_one()/check above), so
## the small-multiples panel should show the curve actually being fit, not a
## straight line. Raw (not orthogonal) squared term, same convention as
## fit_one() above.
fit_lines <- map_dfr(multiples_targets, function(target) {
  df <- multiples_df |> filter(target_fraction == target, !is.na(threshold_pct)) |>
    mutate(skew_sq = skewness^2)
  xr <- range(df$skewness, na.rm = TRUE)
  xs <- seq(xr[1], xr[2], length.out = 50)
  with_fit <- lm(threshold_pct ~ skewness + skew_sq, data = df)
  without_fit <- lm(threshold_pct ~ skewness + skew_sq, data = df |> filter(site_id != AU_ASM))
  bind_rows(
    tibble(target_fraction = target, skewness = xs,
           threshold_pct = predict(with_fit, newdata = tibble(skewness = xs, skew_sq = xs^2)), au_asm = "included"),
    tibble(target_fraction = target, skewness = xs,
           threshold_pct = predict(without_fit, newdata = tibble(skewness = xs, skew_sq = xs^2)), au_asm = "excluded")
  )
}) |> mutate(panel_lab = sprintf("%.2f", target_fraction))

p2a <- ggplot(multiples_df, aes(x = skewness, y = threshold_pct)) +
  geom_line(data = fit_lines, aes(linetype = au_asm), colour = "#0072B2", linewidth = nature_lwd(PEERJ_LWD_REF)) +
  geom_point(aes(shape = is_au_asm), size = 1.3, na.rm = TRUE) +
  scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 8), labels = c("other sites", "AU-ASM"), name = NULL) +
  scale_linetype_manual(values = c("included" = "solid", "excluded" = "dashed"), name = "Fit") +
  facet_wrap(~panel_lab, nrow = 2) +
  labs(x = "Skewness", y = "Selected threshold (% of P99)") +
  theme_gsl() +
  theme(legend.position = "bottom", legend.box = "vertical", legend.key.height = unit(3, "mm"))
  ## NOTE: gsl_panel_letter() is an in-panel annotate() -- on a faceted plot
  ## like this one it would be drawn once per facet (checked visually: all 5
  ## strips got their own "a"), not once for the figure. Tagged instead via
  ## patchwork's tag_levels on the combined figure below, which tags once per
  ## top-level patchwork element regardless of faceting.

r_by_target <- target_sweep_fits |> select(target_fraction, au_asm, r, r_ci_lo, r_ci_hi)

p2b <- ggplot(r_by_target, aes(x = target_fraction, y = r, colour = au_asm, fill = au_asm)) +
  geom_ribbon(aes(ymin = r_ci_lo, ymax = r_ci_hi), alpha = 0.18, colour = NA) +
  geom_line(linewidth = nature_lwd(PEERJ_LWD_DATA)) +
  geom_point(size = 1.3) +
  geom_vline(xintercept = 0.95, linetype = "dotted", colour = "grey40") +
  scale_colour_manual(values = c("included" = "#0072B2", "excluded" = "#D55E00"), name = "AU-ASM") +
  scale_fill_manual(values = c("included" = "#0072B2", "excluded" = "#D55E00"), name = "AU-ASM") +
  labs(x = "Target share of annual GPP", y = "Pearson r (threshold vs. skewness)") +
  theme_gsl() +
  theme(legend.position = "right")
  ## Moved from an inside bottom-left corner (legend.position = c(0.03, 0.03))
  ## -- that corner sat on top of the lines/CI bands for target shares
  ## 0.80-0.83, where both series' confidence bands are at their widest and
  ## most negative. "right" places the legend entirely in the plot margin,
  ## outside the panel, so it cannot cover any data regardless of where the
  ## curves sit.

fig_p2 <- (p2a / p2b) + plot_layout(heights = c(1.3, 1)) +
  plot_annotation(
    title = "Draft for discussion",
    tag_levels = "a",
    theme = theme(
      plot.title = element_text(size = PEERJ_AXIS_TITLE_PT, face = "bold", colour = "grey10", hjust = 0),
      plot.tag = element_text(size = PEERJ_PANEL_LETTER_PT, face = "bold", colour = "black")
    )
  )

legend_p2 <- c(
  "Draft for discussion. Panel a: at five different target shares of annual photosynthesis (shown in",
  "each strip), the threshold each site needs to capture that share, plotted against how skewed the",
  "site's daily photosynthesis is, with curved (quadratic) fits -- the shape that matches the original",
  "published statistic. Solid curves show the fit with AU-ASM (star), the most extreme site, included;",
  "dashed curves show the fit with it excluded; circles are the other sites. Panel b: the strength of",
  "that relationship (Pearson correlation, with its 95% confidence band) across every target share from",
  "80% to 98%, again with and without AU-ASM; the dotted line marks the 95% target used in the original",
  "analysis."
)
save_gsl_figure(
  fig_p2, "drafts/proposal/Figure P2 (target share sweep)",
  width_mm = 170, height_mm = 190, legend_lines = legend_p2,
  input_sources = c(file.path(GSL_ROOT, "tables", "target_sweep_thresholds.csv"), file.path(GSL_ROOT, "tables", "target_sweep_fits.csv")),
  notes = paste0(
    "Draft for discussion (proposal). Fixed 2026-11-03 (redraw/fixes prompt): panel a now draws ",
    "quadratic (not linear) fit lines, matching the fit Tash's published adjusted R2 actually comes ",
    "from; panel b's legend moved from an inside bottom-left corner (which covered the 0.80-0.83 ",
    "lines/bands) to outside the panel (legend.position='right'); caption corrected -- solid=with ",
    "AU-ASM included, dashed=with it excluded (previously ambiguous wording read as reversed). Panel ",
    "a: small multiples at target shares 0.80/0.85/0.90/0.95/0.98, quadratic fit lines with/without ",
    "AU-ASM, star=AU-ASM. Panel b: Pearson r (with 95% CI ribbon) vs. target share, with/without ",
    "AU-ASM; dotted vertical line at 0.95."
  )
)

## ============================================================================
## 3. Two kinds of skew: near-zero-day share vs. above-the-cut partial skewness
## ============================================================================

NEARZERO_CUTS_PCT <- c(0.5, 1, 2)  # % of P99

## ---- Per-site pooled daily GPP (main run convention: zero-floored,
## GSL_SITES' own year list, calendar years) -- reused across all three cuts. ----
pooled_gpp_for <- function(sid) {
  years <- GSL_SITES$years[GSL_SITES$site_id == sid][[1]]
  d <- daily_extract |> filter(site_id == sid, year %in% years)
  gpp <- pmax(d[[gpp_col]], 0)
  gpp[!is.na(gpp)]
}

skew_predictors <- map_dfr(GSL_SITES$site_id, function(sid) {
  pooled <- pooled_gpp_for(sid)
  P99 <- derived_site$P99[derived_site$site_id == sid]
  row <- list(site_id = sid)
  for (cut in NEARZERO_CUTS_PCT) {
    thresh <- cut / 100 * P99
    row[[paste0("near_zero_share_", cut, "pct")]] <- mean(pooled < thresh)
    row[[paste0("partial_skew_ge_", cut, "pct")]] <- g1_adjusted(pooled[pooled >= thresh])
  }
  as_tibble(row)
})

## ---- Sensitivity metrics: IQR across Tash's 22-point grid (0.1, 0.5,
## 1-20% of P99) of (a) mean GSL, (b) across-year GSL s.d., (c) across-year
## GSL trend slope, per site -- recomputed here (not persisted anywhere by
## 03_derive.R, which only keeps the pooled mean per grid point) by reusing
## year_threshold_matrices() directly on the already-cached daily_extract.rds. ----
sensitivity_metrics <- map_dfr(GSL_SITES$site_id, function(sid) {
  years <- GSL_SITES$years[GSL_SITES$site_id == sid][[1]]
  d <- daily_extract |> filter(site_id == sid, year %in% years)
  gpp <- pmax(d[[gpp_col]], 0)
  d$gpp_use <- gpp
  gpp_by_year <- split(d$gpp_use, as.character(d$year))
  P99 <- derived_site$P99[derived_site$site_id == sid]
  tash_abs <- TASH_GRID_PCT / 100 * P99
  mats <- year_threshold_matrices(gpp_by_year, tash_abs)

  mean_per_col <- colMeans(mats$gsl, na.rm = TRUE)
  sd_per_col <- apply(mats$gsl, 2, sd, na.rm = TRUE)
  yrs_num <- as.integer(rownames(mats$gsl))
  trend_per_col <- if (length(unique(yrs_num)) >= 2) {
    apply(mats$gsl, 2, function(col) {
      ok <- !is.na(col)
      if (sum(ok) < 3 || length(unique(yrs_num[ok])) < 2) return(NA_real_)
      unname(coef(lm(col[ok] ~ yrs_num[ok]))[2])
    })
  } else {
    rep(NA_real_, ncol(mats$gsl))
  }

  tibble(
    site_id = sid,
    sensitivity_iqr_mean_gsl = IQR(mean_per_col, na.rm = TRUE),
    sensitivity_iqr_sd_gsl = IQR(sd_per_col, na.rm = TRUE),
    sensitivity_iqr_trend_gsl = IQR(trend_per_col, na.rm = TRUE)
  )
})

skew_predictors <- skew_predictors |>
  left_join(derived_site |> select(site_id, skewness), by = "site_id") |>
  left_join(sensitivity_metrics, by = "site_id") |>
  left_join(duration_curve_table |> select(site_id, t85_pct, t95_pct), by = "site_id")

write_csv(skew_predictors, file.path(GSL_ROOT, "tables", "skew_predictors.csv"))
write_meta(
  file.path(GSL_ROOT, "tables", "skew_predictors.csv"),
  input_sources = c(file.path(GSL_ROOT, "data", "daily_extract.rds"), file.path(GSL_ROOT, "tables", "derived_site.csv"),
                     file.path(GSL_ROOT, "tables", "duration_curve_crossings.csv")),
  notes = paste0(
    "Per site, main run (zero-floored GPP_NT_VUT_REF, calendar years, GSL_SITES' own years, no ",
    "winter screen). near_zero_share_<c>pct = share of pooled daily GPP < c% of P99 (c = 0.5, 1, 2). ",
    "partial_skew_ge_<c>pct = adjusted Fisher-Pearson skewness (g1_adjusted(), 03_derive.R) of only ",
    "the days with GPP >= c% of P99. skewness = full-distribution skewness (derived_site.csv), for ",
    "comparison. sensitivity_iqr_{mean,sd,trend}_gsl = interquartile range, across Tash's 22-point ",
    "threshold grid (0.1%, 0.5%, 1-20% of P99), of that grid's per-threshold mean GSL / across-year ",
    "GSL standard deviation / across-year GSL trend slope (lm(gsl ~ year) at each threshold). t85_pct/",
    "t95_pct = selected threshold at target shares 0.85/0.95 (duration_curve_crossings.csv)."
  )
)

## ---- Correlations: each predictor (near-zero share, partial skew, full
## skewness) x each outcome (3 sensitivity IQRs, t85, t95), at each near-zero
## cut (0.5/1/2%). ----
outcomes <- c("sensitivity_iqr_mean_gsl", "sensitivity_iqr_sd_gsl", "sensitivity_iqr_trend_gsl", "t85_pct", "t95_pct")

cor_one <- function(x, y) {
  ok <- complete.cases(x, y)
  if (sum(ok) < 4) return(c(r = NA_real_, p = NA_real_, n = sum(ok)))
  ct <- cor.test(x[ok], y[ok])
  c(r = unname(ct$estimate), p = ct$p.value, n = sum(ok))
}

skew_predictor_correlations <- map_dfr(NEARZERO_CUTS_PCT, function(cut) {
  nz_col <- paste0("near_zero_share_", cut, "pct")
  pk_col <- paste0("partial_skew_ge_", cut, "pct")
  map_dfr(outcomes, function(outcome) {
    r_nz <- cor_one(skew_predictors[[nz_col]], skew_predictors[[outcome]])
    r_pk <- cor_one(skew_predictors[[pk_col]], skew_predictors[[outcome]])
    r_full <- cor_one(skew_predictors$skewness, skew_predictors[[outcome]])
    bind_rows(
      tibble(near_zero_cut_pct = cut, outcome = outcome, predictor = "near_zero_share",
             r = r_nz["r"], p = r_nz["p"], n = r_nz["n"]),
      tibble(near_zero_cut_pct = cut, outcome = outcome, predictor = "partial_skewness",
             r = r_pk["r"], p = r_pk["p"], n = r_pk["n"]),
      tibble(near_zero_cut_pct = cut, outcome = outcome, predictor = "full_skewness",
             r = r_full["r"], p = r_full["p"], n = r_full["n"])
    )
  })
})

write_csv(skew_predictor_correlations, file.path(GSL_ROOT, "tables", "skew_predictor_correlations.csv"))
write_meta(
  file.path(GSL_ROOT, "tables", "skew_predictor_correlations.csv"),
  input_sources = file.path(GSL_ROOT, "tables", "skew_predictors.csv"),
  notes = "Pearson r (cor.test(), complete.obs) of each predictor (near_zero_share, partial_skewness at that same cut, full_skewness for comparison) against each outcome, at each near-zero cut (0.5/1/2% of P99). p = two-sided p-value; n = sites with non-NA values for both variables."
)

## ---- Two-predictor regression: outcome ~ near_zero_share + partial_skew
## (same cut), at each near-zero cut. ----
two_pred_one <- function(outcome, nz_col, pk_col, cut) {
  df <- skew_predictors |> select(all_of(c("site_id", outcome, nz_col, pk_col))) |>
    rename(y = !!outcome, x1 = !!nz_col, x2 = !!pk_col) |> filter(complete.cases(across(c(y, x1, x2))))
  n <- nrow(df)
  if (n < 5) {
    return(tibble(outcome = outcome, near_zero_cut_pct = cut, n = n, adj_r2 = NA_real_,
                  b_near_zero = NA_real_, b_near_zero_t = NA_real_, b_near_zero_p = NA_real_,
                  b_partial_skew = NA_real_, b_partial_skew_t = NA_real_, b_partial_skew_p = NA_real_))
  }
  fit <- lm(y ~ x1 + x2, data = df)
  s <- summary(fit)
  tibble(
    outcome = outcome, near_zero_cut_pct = cut, n = n, adj_r2 = s$adj.r.squared,
    b_near_zero = unname(s$coefficients["x1", "Estimate"]), b_near_zero_t = unname(s$coefficients["x1", "t value"]), b_near_zero_p = unname(s$coefficients["x1", "Pr(>|t|)"]),
    b_partial_skew = unname(s$coefficients["x2", "Estimate"]), b_partial_skew_t = unname(s$coefficients["x2", "t value"]), b_partial_skew_p = unname(s$coefficients["x2", "Pr(>|t|)"])
  )
}

skew_predictor_two_predictor_regression <- map_dfr(NEARZERO_CUTS_PCT, function(cut) {
  nz_col <- paste0("near_zero_share_", cut, "pct")
  pk_col <- paste0("partial_skew_ge_", cut, "pct")
  map_dfr(outcomes, two_pred_one, nz_col = nz_col, pk_col = pk_col, cut = cut)
})

write_csv(skew_predictor_two_predictor_regression, file.path(GSL_ROOT, "tables", "skew_predictor_two_predictor_regression.csv"))
write_meta(
  file.path(GSL_ROOT, "tables", "skew_predictor_two_predictor_regression.csv"),
  input_sources = file.path(GSL_ROOT, "tables", "skew_predictors.csv"),
  notes = "lm(outcome ~ near_zero_share + partial_skewness) at each near-zero cut (0.5/1/2% of P99): adjusted R2 and each predictor's own coefficient/t-statistic/p-value, holding the other predictor constant."
)

message("=== 12_duration_curve_and_target_sweep.R: done ===")
message("Tables: duration_curve_crossings.csv, target_sweep_thresholds.csv, target_sweep_fits.csv, ",
        "skew_predictors.csv, skew_predictor_correlations.csv, skew_predictor_two_predictor_regression.csv")
message("Figures: drafts/proposal/Figure P1a/P1b/P1c/P1d/P2")
