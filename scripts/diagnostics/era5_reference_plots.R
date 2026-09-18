## era5_reference_plots.R
##
## Plotting task only: no new verdict, no correction, no reclassification.
## Produces histograms and scatter plots comparing ERA5-derived MAP,
## WorldClim BIO12, and BADM PI-reported MAP for the 781 current-network
## sites, for visual inspection.
##
## Provenance: per-site map_era5 / bio12_mm / badm_map_mm / ratio_to_bio12 /
## ratio_to_badm_map / data_hub / product_source_network are unchanged values
## originally computed in review/diagnostics/era5_precip_units_v2/table_t2_ratios.csv
## and carried through unmodified into table_b1_factor_estimates.csv (v3),
## which is the single file this script reads (v3 was confirmed identical to
## v2 on every shared column and row order before writing this script).
## factor_estimate / n_refs / nearest_cluster are original to
## table_b1_factor_estimates.csv (v3). Nothing from either file is
## recalculated. This script itself computes only: per-panel Spearman rho and
## n (not previously computed anywhere), a human-readable cluster_membership
## label (a direct rename of nearest_cluster's existing values), and the
## per-panel exclusion sets needed because a small number of sites have a
## value of exactly zero or are missing a reference, which is undefined on a
## log axis.
##
## Read-only w.r.t. the pipeline and all prior diagnostics: does not modify
## R/climate_classification.R, any numbered pipeline script, any figure,
## legend, or snapshot CSV, or any v1-v4 era5_precip_units output. Writes
## only new files under review/diagnostics/era5_reference_plots/.

source("R/pipeline_config.R")
source("R/utils.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(gridExtra)
  library(grid)
  library(fs)
})

OUTD <- "review/diagnostics/era5_reference_plots"
V3D  <- "review/diagnostics/era5_precip_units_v3"
fs::dir_create(OUTD)

message("=== era5_reference_plots.R ===")

# ============================================================================
# 1. LOAD + TIDY (no recomputation of any base value or ratio)
# ============================================================================

b1_path <- file.path(V3D, "table_b1_factor_estimates.csv")
b1 <- readr::read_csv(b1_path, show_col_types = FALSE)
stopifnot(nrow(b1) == 781)

d <- b1 |>
  dplyr::transmute(
    site_id, data_hub, product_source_network,
    era5_map_mm = map_era5,
    bio12_mm,
    badm_map_mm,
    ratio_to_bio12,
    ratio_to_badm_map,
    nearest_cluster,
    cluster_membership = dplyr::case_when(
      nearest_cluster == "4" ~ "4x_cluster",
      nearest_cluster == "8" ~ "8x_cluster",
      TRUE ~ "not_in_4x_or_8x_cluster"
    ),
    era5_positive  = !is.na(era5_map_mm) & era5_map_mm > 0,
    bio12_positive = !is.na(bio12_mm) & bio12_mm > 0,
    badm_present   = !is.na(badm_map_mm),
    badm_positive  = badm_present & badm_map_mm > 0
  )

n_total         <- nrow(d)
n_missing_badm  <- sum(!d$badm_present)
n_missing_bio12 <- sum(!d$bio12_positive & is.na(d$bio12_mm))  # bio12 has no NAs in this network; kept explicit
n_missing_both  <- sum(!d$badm_present & is.na(d$bio12_mm))
n_cluster       <- sum(d$cluster_membership != "not_in_4x_or_8x_cluster")

message(sprintf(
  "Sites: %d total | missing BADM: %d | missing BIO12: %d | missing both: %d | in 4x/8x cluster: %d",
  n_total, n_missing_badm, n_missing_bio12, n_missing_both, n_cluster
))

# ============================================================================
# 2. PER-PANEL EXCLUSION SETS
#    A log axis is undefined at zero/negative/missing values. era5_map_mm is
#    exactly 0 at one site (CA-TP2); badm_map_mm is exactly 0 at five sites
#    (CZ-LnG, DE-Lnf, ES-Agu, ES-Amo, KE-Kpt) in addition to being missing
#    (NA) at 144 sites. bio12_mm has no zero, negative, or missing values in
#    this network.
# ============================================================================

excl_hist_bio12 <- d |> dplyr::filter(!era5_positive) |> dplyr::pull(site_id)
excl_hist_badm  <- d |> dplyr::filter(!era5_positive | !badm_present | !badm_positive) |> dplyr::pull(site_id)
excl_scatter_a  <- excl_hist_badm                                              # ERA5 vs BADM
excl_scatter_b  <- excl_hist_bio12                                             # ERA5 vs BIO12
excl_scatter_c  <- d |> dplyr::filter(!badm_present | !badm_positive) |> dplyr::pull(site_id)  # BADM vs BIO12

reasons <- dplyr::bind_rows(
  d |> dplyr::filter(!era5_positive) |>
    dplyr::transmute(site_id, panel = "hist:ratio_to_bio12, scatter:ERA5-vs-BIO12, scatter:ERA5-vs-BADM",
                      reason = "era5_map_mm == 0"),
  d |> dplyr::filter(!badm_present) |>
    dplyr::transmute(site_id, panel = "hist:ratio_to_badm, scatter:ERA5-vs-BADM, scatter:BADM-vs-BIO12",
                      reason = "badm_map_mm missing (NA)"),
  d |> dplyr::filter(badm_present & !badm_positive) |>
    dplyr::transmute(site_id, panel = "hist:ratio_to_badm, scatter:ERA5-vs-BADM, scatter:BADM-vs-BIO12",
                      reason = "badm_map_mm == 0")
)

message("\n-- Panel n --")
message(sprintf("hist ratio_to_bio12: n=%d (excludes %d)", n_total - length(excl_hist_bio12), length(excl_hist_bio12)))
message(sprintf("hist ratio_to_badm:  n=%d (excludes %d)", n_total - length(excl_hist_badm), length(excl_hist_badm)))
message(sprintf("scatter ERA5-vs-BADM:  n=%d", n_total - length(excl_scatter_a)))
message(sprintf("scatter ERA5-vs-BIO12: n=%d", n_total - length(excl_scatter_b)))
message(sprintf("scatter BADM-vs-BIO12: n=%d", n_total - length(excl_scatter_c)))

# ============================================================================
# 3. SPEARMAN RHO PER SCATTER PANEL (computed here; not present in inputs)
# ============================================================================

spearman <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  ct <- suppressWarnings(cor.test(x[ok], y[ok], method = "spearman"))
  list(rho = unname(ct$estimate), p = ct$p.value, n = sum(ok))
}

dat_a <- d |> dplyr::filter(!(site_id %in% excl_scatter_a))   # ERA5 vs BADM
dat_b <- d |> dplyr::filter(!(site_id %in% excl_scatter_b))   # ERA5 vs BIO12
dat_c <- d |> dplyr::filter(!(site_id %in% excl_scatter_c))   # BADM vs BIO12

rho_a <- spearman(dat_a$badm_map_mm,  dat_a$era5_map_mm)
rho_b <- spearman(dat_b$bio12_mm,     dat_b$era5_map_mm)
rho_c <- spearman(dat_c$bio12_mm,     dat_c$badm_map_mm)

fmt_p <- function(p) if (p < 0.001) "< 0.001" else sprintf("= %.3f", p)

message(sprintf("\nSpearman ERA5~BADM:  n=%d rho=%.3f p%s", rho_a$n, rho_a$rho, fmt_p(rho_a$p)))
message(sprintf("Spearman ERA5~BIO12: n=%d rho=%.3f p%s", rho_b$n, rho_b$rho, fmt_p(rho_b$p)))
message(sprintf("Spearman BADM~BIO12: n=%d rho=%.3f p%s", rho_c$n, rho_c$rho, fmt_p(rho_c$p)))

# ============================================================================
# 4. SHARED STYLE
# ============================================================================

COL_ALL   <- "#4C72B0"
COL_OTHER <- "#4C72B0"
COL_CLUST <- "#D55E00"
CLUSTER_LEGEND_TITLE <-
  paste0(
    "Membership rule (verbatim, table_b1_factor_estimates.csv):\n",
    "factor_estimate = median(ratio_to_bio12, ratio_to_badm_map);\n",
    "assigned to nearest of candidate factors {1, 4, 8, 24, 1000}\n",
    "if within 15% of it (tol = 0.15), else 'elsewhere'.\n",
    "Shown = sites with nearest_cluster %in% c('4','8')\n",
    sprintf("(n = %d: 113 near 4x + 10 near 8x)", n_cluster)
  )

theme_diag <- function() {
  theme_minimal(base_size = 9) +
    theme(plot.background = element_rect(fill = "white", colour = NA),
          legend.key.size = unit(0.3, "cm"),
          legend.text = element_text(size = 6.5),
          legend.title = element_text(size = 6.5))
}

# ============================================================================
# 5. FIGURE 1 -- HISTOGRAMS OF log10(ratio), TWO PANELS
# ============================================================================

BINWIDTH <- 0.1  # log10 units; 10^0.1 ~= 1.259, i.e. ~26% per bin
REF_LINES <- log10(c(1, 4, 8))

make_hist <- function(df_sub, ratio_col, panel_label, n_panel, highlight) {
  df_sub <- df_sub |> dplyr::mutate(log10_ratio = log10(.data[[ratio_col]]))
  subtitle <- sprintf("n = %d | bin width = %.2g log10 units (ratio x %.3f per bin)", n_panel, BINWIDTH, 10^BINWIDTH)
  if (!highlight) {
    p <- ggplot(df_sub, aes(x = log10_ratio)) +
      geom_histogram(binwidth = BINWIDTH, boundary = 0, fill = COL_ALL, colour = "white", linewidth = 0.15)
  } else {
    df_sub <- df_sub |>
      dplyr::mutate(cluster = factor(cluster_membership != "not_in_4x_or_8x_cluster",
                                      levels = c(FALSE, TRUE),
                                      labels = c("not in 4x/8x cluster", "in 4x/8x cluster")))
    p <- ggplot(df_sub, aes(x = log10_ratio, fill = cluster)) +
      geom_histogram(binwidth = BINWIDTH, boundary = 0, colour = "white", linewidth = 0.15, position = "stack") +
      scale_fill_manual(values = c("not in 4x/8x cluster" = COL_OTHER, "in 4x/8x cluster" = COL_CLUST),
                         name = CLUSTER_LEGEND_TITLE)
  }
  brks <- log10(c(0.001, 0.01, 0.1, 1, 4, 8, 100, 1000))
  lbls <- c("0.001x", "0.01x", "0.1x", "1x", "4x", "8x", "100x", "1000x")
  p +
    geom_vline(xintercept = REF_LINES, linetype = "dashed", colour = "grey30", linewidth = 0.35) +
    scale_x_continuous(name = paste0(panel_label, "  [log10(ratio) scale]"), breaks = brks, labels = lbls) +
    labs(y = "number of sites", subtitle = subtitle) +
    theme_diag()
}

build_fig1 <- function(highlight) {
  p1 <- make_hist(d |> dplyr::filter(!(site_id %in% excl_hist_bio12)),
                   "ratio_to_bio12", "ERA5 MAP / WorldClim BIO12", n_total - length(excl_hist_bio12), highlight)
  p2 <- make_hist(d |> dplyr::filter(!(site_id %in% excl_hist_badm)),
                   "ratio_to_badm_map", "ERA5 MAP / BADM PI-reported MAP", n_total - length(excl_hist_badm), highlight)
  cap <- paste0(
    "Figure 1. Histograms of the per-site ratio of ERA5-derived mean annual precipitation (MAP) to two ",
    "independent references, ", n_total, " current-network sites. Left panel: ERA5/BIO12 (n=",
    n_total - length(excl_hist_bio12), "; excludes 1 site with era5_map_mm == 0, undefined on a log axis: CA-TP2). ",
    "Right panel: ERA5/BADM (n=", n_total - length(excl_hist_badm), "; excludes ", length(excl_hist_badm),
    " sites: 144 with no BADM value, 5 with BADM MAP == 0, and the same 1 site with era5_map_mm == 0). ",
    "X-axis is log10(ratio); bin width 0.1 log10 units (~26% per bin); axis range is not clipped and extends ",
    "to the full observed range in each panel, so the single most extreme site in each panel appears in its ",
    "own bin. Dashed reference lines at ratio = 1x, 4x, 8x.",
    if (highlight) paste0(" Version B: the ", n_cluster, " sites in the 4x/8x cluster are shown in a second ",
                           "colour; see legend for the membership rule.") else " Version A: all sites, one colour, no grouping."
  )
  fig <- gridExtra::arrangeGrob(p1, p2, ncol = 2)
  list(fig = fig, caption = cap)
}

fig1A <- build_fig1(highlight = FALSE)
fig1B <- build_fig1(highlight = TRUE)

out_fig1A <- file.path(OUTD, "fig1_ratio_histograms_versionA.png")
out_fig1B <- file.path(OUTD, "fig1_ratio_histograms_versionB.png")
ggsave(out_fig1A, fig1A$fig, width = 12, height = 5, dpi = 300, bg = "white")
ggsave(out_fig1B, fig1B$fig, width = 12, height = 5, dpi = 300, bg = "white")
message("Saved: ", out_fig1A)
message("Saved: ", out_fig1B)

# ============================================================================
# 6. FIGURE 2 -- SCATTER PLOTS, THREE PANELS
#    (a) ERA5 vs BADM  (y = ERA5, x = BADM)
#    (b) ERA5 vs BIO12 (y = ERA5, x = BIO12)
#    (c) BADM vs BIO12 (y = BADM, x = BIO12)
# ============================================================================

make_scatter <- function(df_sub, xcol, ycol, xlab, ylab, rho_info, highlight, panel_letter) {
  rng <- range(c(df_sub[[xcol]], df_sub[[ycol]]), na.rm = TRUE)
  ann <- sprintf("(%s) n = %d\nSpearman rho = %.3f\np %s", panel_letter, rho_info$n, rho_info$rho, fmt_p(rho_info$p))
  if (!highlight) {
    p <- ggplot(df_sub, aes(x = .data[[xcol]], y = .data[[ycol]])) +
      geom_point(alpha = 0.55, size = 1.3, colour = COL_ALL)
  } else {
    df_sub <- df_sub |>
      dplyr::mutate(cluster = factor(cluster_membership != "not_in_4x_or_8x_cluster",
                                      levels = c(FALSE, TRUE),
                                      labels = c("not in 4x/8x cluster", "in 4x/8x cluster")))
    p <- ggplot(df_sub, aes(x = .data[[xcol]], y = .data[[ycol]], colour = cluster)) +
      geom_point(alpha = 0.6, size = 1.3) +
      scale_colour_manual(values = c("not in 4x/8x cluster" = COL_OTHER, "in 4x/8x cluster" = COL_CLUST),
                           name = CLUSTER_LEGEND_TITLE)
  }
  p +
    geom_abline(slope = 1, intercept = 0, linetype = "solid", colour = "grey35", linewidth = 0.4) +
    geom_abline(slope = 1, intercept = log10(4),  linetype = "dashed", colour = "grey50", linewidth = 0.3) +
    geom_abline(slope = 1, intercept = -log10(4), linetype = "dashed", colour = "grey50", linewidth = 0.3) +
    geom_abline(slope = 1, intercept = log10(8),  linetype = "dotted", colour = "grey50", linewidth = 0.3) +
    geom_abline(slope = 1, intercept = -log10(8), linetype = "dotted", colour = "grey50", linewidth = 0.3) +
    scale_x_log10(limits = rng, name = xlab) +
    scale_y_log10(limits = rng, name = ylab) +
    annotate("text", x = rng[1] * 1.3, y = rng[2] / 1.15, label = ann, hjust = 0, vjust = 1, size = 2.5, lineheight = 1.05) +
    theme_diag()
}

build_fig2 <- function(highlight) {
  rng_a <- round(range(c(dat_a$badm_map_mm, dat_a$era5_map_mm), na.rm = TRUE), 1)
  rng_b <- round(range(c(dat_b$bio12_mm, dat_b$era5_map_mm), na.rm = TRUE), 1)
  rng_c <- round(range(c(dat_c$bio12_mm, dat_c$badm_map_mm), na.rm = TRUE), 1)

  pa <- make_scatter(dat_a, "badm_map_mm", "era5_map_mm", "BADM MAP (mm/yr)", "ERA5 MAP (mm/yr)", rho_a, highlight, "a")
  pb <- make_scatter(dat_b, "bio12_mm", "era5_map_mm", "WorldClim BIO12 (mm/yr)", "ERA5 MAP (mm/yr)", rho_b, highlight, "b")
  pc <- make_scatter(dat_c, "bio12_mm", "badm_map_mm", "WorldClim BIO12 (mm/yr)", "BADM MAP (mm/yr)", rho_c, highlight, "c")

  cap <- paste0(
    "Figure 2. Pairwise comparison of the three mean annual precipitation (MAP) estimates, log-log axes, ",
    "identical x/y range within each panel (not clipped). Solid line = 1:1; dashed lines = 4x and 0.25x offset; ",
    "dotted lines = 8x and 0.125x offset. ",
    "(a) ERA5 vs BADM: n=", rho_a$n, ", Spearman rho=", sprintf("%.3f", rho_a$rho), ", axis range [",
    rng_a[1], ", ", rng_a[2], "] mm/yr; excludes ", length(excl_scatter_a), " sites (BADM missing or ==0, or era5_map_mm==0). ",
    "(b) ERA5 vs BIO12: n=", rho_b$n, ", Spearman rho=", sprintf("%.3f", rho_b$rho), ", axis range [",
    rng_b[1], ", ", rng_b[2], "] mm/yr; excludes ", length(excl_scatter_b), " site (era5_map_mm==0: CA-TP2). ",
    "(c) BADM vs BIO12: n=", rho_c$n, ", Spearman rho=", sprintf("%.3f", rho_c$rho), ", axis range [",
    rng_c[1], ", ", rng_c[2], "] mm/yr; excludes ", length(excl_scatter_c), " sites (BADM missing or ==0).",
    if (highlight) paste0(" Version B: the ", n_cluster, " sites in the 4x/8x cluster are shown in a second ",
                           "colour; see legend for the membership rule.") else " Version A: all sites, one colour, no grouping."
  )
  fig <- gridExtra::arrangeGrob(pa, pb, pc, ncol = 3)
  list(fig = fig, caption = cap, rng_a = rng_a, rng_b = rng_b, rng_c = rng_c)
}

fig2A <- build_fig2(highlight = FALSE)
fig2B <- build_fig2(highlight = TRUE)

out_fig2A <- file.path(OUTD, "fig2_scatter_versionA.png")
out_fig2B <- file.path(OUTD, "fig2_scatter_versionB.png")
ggsave(out_fig2A, fig2A$fig, width = 15, height = 5.5, dpi = 300, bg = "white")
ggsave(out_fig2B, fig2B$fig, width = 15, height = 5.5, dpi = 300, bg = "white")
message("Saved: ", out_fig2A)
message("Saved: ", out_fig2B)

# ============================================================================
# 7. COMPANION CSV
# ============================================================================

companion <- d |>
  dplyr::select(site_id, data_hub, product_source_network,
                 era5_map_mm, bio12_mm, badm_map_mm,
                 ratio_to_bio12, ratio_to_badm_map,
                 cluster_membership)

out_csv <- file.path(OUTD, "table_site_reference_comparison.csv")
readr::write_csv(companion, out_csv)
write_output_metadata(
  out_csv,
  input_sources = b1_path,
  notes = paste0(
    "One row per current-network site (n=781). era5_map_mm/bio12_mm/badm_map_mm/ratio_to_bio12/",
    "ratio_to_badm_map are unchanged values from table_b1_factor_estimates.csv (v3), which carried ",
    "them through unmodified from table_t2_ratios.csv (v2). cluster_membership is a rename of that ",
    "file's nearest_cluster column ('4' -> '4x_cluster', '8' -> '8x_cluster', else -> ",
    "'not_in_4x_or_8x_cluster'; rule: factor_estimate = median(ratio_to_bio12, ratio_to_badm_map), ",
    "nearest of {1,4,8,24,1000} within 15%). Missing BADM (144 sites) and BADM==0 (5 sites) are carried ",
    "as NA and 0 respectively, not dropped. Plotting-only diagnostic; no new verdict, correction, or ",
    "reclassification of any site."
  )
)
message("Saved: ", out_csv)

# ============================================================================
# 8. METADATA FOR FIGURES
# ============================================================================

write_output_metadata(out_fig1A, input_sources = b1_path,
  notes = paste0("Version A (no grouping) of Figure 1. ", fig1A$caption))
write_output_metadata(out_fig1B, input_sources = b1_path,
  notes = paste0("Version B (4x/8x cluster highlighted) of Figure 1. ", fig1B$caption))
write_output_metadata(out_fig2A, input_sources = b1_path,
  notes = paste0("Version A (no grouping) of Figure 2. ", fig2A$caption))
write_output_metadata(out_fig2B, input_sources = b1_path,
  notes = paste0("Version B (4x/8x cluster highlighted) of Figure 2. ", fig2B$caption))

# ============================================================================
# 9. EXCLUSION SUMMARY (for report)
# ============================================================================

out_excl <- file.path(OUTD, "table_panel_exclusions.csv")
readr::write_csv(reasons, out_excl)
write_output_metadata(out_excl, input_sources = b1_path,
  notes = "Sites excluded from one or more panels of Figure 1 or Figure 2 because a value used on a log axis is exactly zero or missing, and the specific reason. Not an exclusion from the companion CSV, which carries all 781 sites.")
message("Saved: ", out_excl)

message("\n=== era5_reference_plots.R complete ===")
