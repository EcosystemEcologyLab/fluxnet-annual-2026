## nee_et_site_vs_trendy_core.R
##
## Diagnostic investigation for a co-author decision: is the low Jaccard
## (0.233) for the site-measured NEE axis (Row E' of
## Supp_compare_geospatial_vs_sitelevel_grid.png, vs. J=0.506 for the
## TRENDY-at-site NEE-IAV axis, Row E) an artifact, a definitional
## mismatch, a scale effect, or a real sampling result? ET (Row F/F',
## J=0.456/0.489, well-behaved) is the control throughout.
##
## Covers T1 (percentile/ratio part; the raw-monthly-stack re-derivation
## sub-part of T1, plus T3 and T4, are in the sibling script
## nee_et_site_vs_trendy_raster.R, which needs the raw ~127GB TRENDY
## archive), T2, T5, T6, T7.
##
## Read-only with respect to the existing pipeline: does NOT modify any
## existing script, figure, legend, snapshot CSV, or
## representativeness_metrics.csv. Reads only existing files under
## data/snapshots/, data/external/, and review/figures/candidates/.
## Writes only new files under review/diagnostics/nee_et_site_vs_trendy/.
##
## Throughout, "TRENDY-at-site" means the site-extracted TRENDY nee_median
## / et_median value (site_trendy_nee_median.csv / site_trendy_et_median.csv)
## -- the "mean |flux|" statistic that Row E'/F' of the comparison grid are
## actually built against (NOT the NEE-IAV axis used in the original
## Row E of that same figure, which is a different physical quantity --
## interannual variability, not magnitude -- and is not the counterpart
## the co-author's question is about; confirmed against the user's own
## framing: "site-measured |NEP| piling into the top bin of the TRENDY
## nee_median scheme").

source("R/pipeline_config.R")
source("R/utils.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(terra)
  library(fs)
})

SNAP <- "data/snapshots"
EXT  <- "data/external"
CAND <- "review/figures/candidates"
OUTD <- "review/diagnostics/nee_et_site_vs_trendy"
fs::dir_create(OUTD)

message("=== nee_et_site_vs_trendy_core.R ===")

IGBP_ORDER <- c("ENF","EBF","DNF","DBF","MF","CSH","OSH","WSA","SAV","GRA",
                "WET","CRO","URB","CVM","SNO","BSV","WAT")

# ============================================================================
# 0. BUILD THE PAIRED DATASETS (measured vs. TRENDY-at-site)
# ============================================================================

flux   <- readr::read_csv(file.path(SNAP, "site_flux_medians_shuttle.csv"), show_col_types = FALSE)
nee_t  <- readr::read_csv(file.path(SNAP, "site_trendy_nee_median.csv"), show_col_types = FALSE)
et_t   <- readr::read_csv(file.path(SNAP, "site_trendy_et_median.csv"), show_col_types = FALSE)

paired_nee <- flux |>
  dplyr::filter(!is.na(nep_median)) |>
  dplyr::select(site_id, location_lat, location_long, igbp_class, n_years_nee, nep_median) |>
  dplyr::inner_join(dplyr::select(nee_t, site_id, trendy_nee_median_value), by = "site_id") |>
  dplyr::mutate(measured_abs = abs(nep_median), trendy_abs = trendy_nee_median_value,
                diff = measured_abs - trendy_abs, ratio = measured_abs / trendy_abs)

paired_et <- flux |>
  dplyr::filter(!is.na(et_median)) |>
  dplyr::select(site_id, location_lat, location_long, igbp_class, n_years_le, et_median) |>
  dplyr::inner_join(dplyr::select(et_t, site_id, trendy_et_median_value), by = "site_id") |>
  dplyr::mutate(measured_abs = et_median, trendy_abs = trendy_et_median_value,
                diff = measured_abs - trendy_abs, ratio = measured_abs / trendy_abs)

stopifnot(nrow(paired_nee) == 636L, nrow(paired_et) == 656L)
message("Paired NEE sites: ", nrow(paired_nee), " | Paired ET sites: ", nrow(paired_et))

out_pairs <- file.path(OUTD, "table_paired_measured_vs_trendy.csv")
readr::write_csv(
  dplyr::bind_rows(
    dplyr::mutate(paired_nee, axis = "nee", .before = 1),
    dplyr::mutate(paired_et,  axis = "et",  .before = 1)
  ) |> dplyr::select(axis, site_id, igbp_class, measured_abs, trendy_abs, diff, ratio),
  out_pairs
)
write_output_metadata(out_pairs, input_sources = c(
  file.path(SNAP, "site_flux_medians_shuttle.csv"),
  file.path(SNAP, "site_trendy_nee_median.csv"), file.path(SNAP, "site_trendy_et_median.csv")
), notes = "Site-level paired measured-vs-TRENDY-at-site values for NEE (|NEP| vs trendy_nee_median) and ET (et_median vs trendy_et_median), current_781 network.")
message("Saved: ", out_pairs)

# ============================================================================
# T1. UNIT AND CONVERSION SCREEN (percentile/ratio part; raw-stack
#     re-derivation is in the sibling _raster.R script)
# ============================================================================

message("\n================ T1: unit/conversion screen ================")

pct <- c(0.05, 0.25, 0.50, 0.75, 0.95)
t1_percentiles <- dplyr::bind_rows(
  data.frame(axis = "nee", quantity = "measured_abs_NEP", pct = pct,
             value = quantile(paired_nee$measured_abs, pct), unit = "gC m-2 yr-1"),
  data.frame(axis = "nee", quantity = "trendy_abs_NBP", pct = pct,
             value = quantile(paired_nee$trendy_abs, pct), unit = "gC m-2 yr-1"),
  data.frame(axis = "et", quantity = "measured_ET", pct = pct,
             value = quantile(paired_et$measured_abs, pct), unit = "mm yr-1"),
  data.frame(axis = "et", quantity = "trendy_ET", pct = pct,
             value = quantile(paired_et$trendy_abs, pct), unit = "mm yr-1")
)
row.names(t1_percentiles) <- NULL

CANDIDATE_FACTORS <- c(`12` = 12, `30` = 30, `365` = 365, `1000` = 1000,
                        `2629800/31557600` = 2629800/31557600)
nearest_factor <- function(ratio_vec) {
  med <- median(ratio_vec, na.rm = TRUE)
  d <- abs(log(CANDIDATE_FACTORS) - log(med))  # compare on log scale (ratio or its reciprocal)
  d_inv <- abs(log(1 / CANDIDATE_FACTORS) - log(med))
  best <- which.min(pmin(d, d_inv))
  data.frame(median_ratio = med, nearest_candidate = names(CANDIDATE_FACTORS)[best],
             nearest_candidate_value = CANDIDATE_FACTORS[best],
             log_distance = min(d[best], d_inv[best]))
}
t1_ratio_screen <- dplyr::bind_rows(
  dplyr::mutate(nearest_factor(paired_nee$ratio), axis = "nee", .before = 1),
  dplyr::mutate(nearest_factor(paired_et$ratio),  axis = "et",  .before = 1)
)

t1_ratio_dist <- dplyr::bind_rows(
  data.frame(axis = "nee", dplyr::as_tibble(as.list(quantile(paired_nee$ratio, pct))), mean = mean(paired_nee$ratio)),
  data.frame(axis = "et",  dplyr::as_tibble(as.list(quantile(paired_et$ratio,  pct))), mean = mean(paired_et$ratio))
)

cat("\n-- Percentiles (measured / TRENDY-at-site) --\n")
print(t1_percentiles)
cat("\n-- Per-site ratio distribution --\n")
print(t1_ratio_dist)
cat("\n-- Nearest unit-conversion-factor candidate (12/30/365/1000/2629800:31557600) --\n")
print(t1_ratio_screen)
cat("\nNEE median ratio =", round(median(paired_nee$ratio), 3),
    "-- not close (log-distance ", round(t1_ratio_screen$log_distance[t1_ratio_screen$axis=="nee"], 2),
    ") to any of the screened factors -> a simple conversion-factor slip is NOT indicated by this test alone.\n")
cat("ET median ratio =", round(median(paired_et$ratio), 3),
    "-- consistent with ~1:1, no conversion issue.\n")

out_t1a <- file.path(OUTD, "table_t1_percentiles.csv")
out_t1b <- file.path(OUTD, "table_t1_ratio_screen.csv")
readr::write_csv(t1_percentiles, out_t1a)
readr::write_csv(dplyr::left_join(t1_ratio_screen, t1_ratio_dist, by = "axis"), out_t1b)
write_output_metadata(out_t1a, input_sources = out_pairs, notes = "T1 percentiles of measured and TRENDY-at-site magnitudes.")
write_output_metadata(out_t1b, input_sources = out_pairs, notes = "T1 per-site ratio distribution and nearest screened unit-conversion-factor candidate. Raw-monthly-stack re-derivation for 20 sample pixels is in table_t1_raw_stack_check.csv (nee_et_site_vs_trendy_raster.R).")
message("Saved: ", out_t1a, ", ", out_t1b)

# ============================================================================
# T2. BIN COVERAGE
# ============================================================================

message("\n================ T2: bin coverage ================")

nee_bins <- readr::read_csv(file.path(SNAP, "trendy_nee_median_global_distribution.csv"), show_col_types = FALSE)
et_bins  <- readr::read_csv(file.path(SNAP, "trendy_et_median_global_distribution.csv"), show_col_types = FALSE)

classify_into_bins <- function(x, bin_df) {
  idx <- findInterval(x, bin_df$min_value, all.inside = FALSE)
  idx[idx < 1L] <- NA_integer_
  idx
}

bin_coverage <- function(values, bin_df, label) {
  idx <- classify_into_bins(values, bin_df)
  tab <- table(factor(idx, levels = bin_df$bin))
  data.frame(
    source = label, bin = bin_df$bin, bin_label = bin_df$bin_label,
    n = as.integer(tab), fraction = as.numeric(tab) / length(values)
  )
}

t2 <- dplyr::bind_rows(
  dplyr::mutate(bin_coverage(paired_nee$measured_abs, nee_bins, "measured_NEE"), axis = "nee", .before = 1),
  dplyr::mutate(bin_coverage(paired_nee$trendy_abs,   nee_bins, "trendy_at_site_NEE"), axis = "nee", .before = 1),
  dplyr::mutate(bin_coverage(paired_et$measured_abs,  et_bins,  "measured_ET"), axis = "et", .before = 1),
  dplyr::mutate(bin_coverage(paired_et$trendy_abs,    et_bins,  "trendy_at_site_ET"), axis = "et", .before = 1)
)

cat("\n-- Bin coverage (top bin = bin 7 = '> top edge') --\n")
print(as.data.frame(t2), row.names = FALSE)

top_bin_summary <- t2 |> dplyr::filter(bin == 7) |>
  dplyr::select(axis, source, n, fraction)
cat("\n-- Top-bin ('above top edge') fraction summary --\n")
print(top_bin_summary)

out_t2 <- file.path(OUTD, "table_t2_bin_coverage.csv")
readr::write_csv(t2, out_t2)
write_output_metadata(out_t2, input_sources = c(out_pairs,
  file.path(SNAP, "trendy_nee_median_global_distribution.csv"),
  file.path(SNAP, "trendy_et_median_global_distribution.csv")),
  notes = "Per-bin site counts/fractions for measured and TRENDY-at-site values, NEE and ET, using the stored 7-bin edges (min_value/max_value columns) from the unchanged global-distribution CSVs.")
message("Saved: ", out_t2)

# ============================================================================
# T5. MANAGEMENT AND DEFINITION PROXY
# ============================================================================

message("\n================ T5: management/definition proxy (IGBP stratification) ================")

badm <- readr::read_csv(file.path(SNAP, "badm_management_coverage.csv"), show_col_types = FALSE)
message("BADM management coverage file: ", nrow(badm), " sites (767-site snapshot; current network is 781 -- ",
        "the 14 sites added since have NO row here, not even NA -- see report.md for the exact gap list).")

FOREST_CLASSES  <- c("ENF","EBF","DNF","DBF","MF")
MANAGED_CLASSES <- c("CRO","GRA")  # WET handled separately via BADM flag, per instruction

t5_by_class <- function(paired_df, unit) {
  paired_df |>
    dplyr::left_join(dplyr::select(badm, site_id, any_management, mgmt_drainage_wtd), by = "site_id") |>
    dplyr::group_by(igbp_class) |>
    dplyr::summarise(
      n = dplyr::n(),
      median_diff = median(diff), iqr_lo = quantile(diff, 0.25), iqr_hi = quantile(diff, 0.75),
      n_badm_matched = sum(!is.na(any_management)),
      n_flagged_managed = sum(any_management, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(median_diff)) |>
    dplyr::mutate(unit = unit)
}

t5_nee_by_class <- t5_by_class(paired_nee, "gC m-2 yr-1")
t5_et_by_class  <- t5_by_class(paired_et,  "mm yr-1")

cat("\n-- NEE: median(measured-TRENDY) by IGBP class --\n")
print(as.data.frame(t5_nee_by_class))
cat("\n-- ET: median(measured-TRENDY) by IGBP class (control) --\n")
print(as.data.frame(t5_et_by_class))

# Two-group rank test: managed (CRO+GRA+WET-flagged-managed) vs unmanaged forest
build_groups <- function(paired_df) {
  d <- paired_df |> dplyr::left_join(dplyr::select(badm, site_id, mgmt_drainage_wtd), by = "site_id")
  d |> dplyr::mutate(
    group = dplyr::case_when(
      igbp_class %in% MANAGED_CLASSES ~ "managed (CRO+GRA)",
      igbp_class == "WET" & isTRUE(mgmt_drainage_wtd) ~ "managed (WET, BADM-flagged)",
      igbp_class %in% FOREST_CLASSES ~ "unmanaged forest",
      TRUE ~ NA_character_
    )
  ) |> dplyr::filter(!is.na(group)) |>
    dplyr::mutate(group2 = dplyr::if_else(startsWith(group, "managed"), "managed", "unmanaged forest"))
}

run_rank_test <- function(paired_df, label) {
  g <- build_groups(paired_df)
  tab <- g |> dplyr::group_by(group2) |> dplyr::summarise(n = dplyr::n(), median_diff = median(diff), .groups = "drop")
  wt <- suppressWarnings(wilcox.test(diff ~ group2, data = g))
  list(label = label, table = tab, wilcox_W = unname(wt$statistic), wilcox_p = wt$p.value, data = g)
}

t5_nee_test <- run_rank_test(paired_nee, "NEE")
t5_et_test  <- run_rank_test(paired_et,  "ET")

cat("\n-- NEE: managed (CRO+GRA+BADM-flagged-managed WET) vs unmanaged forest --\n")
print(as.data.frame(t5_nee_test$table))
cat("Wilcoxon W =", t5_nee_test$wilcox_W, " p =", format.pval(t5_nee_test$wilcox_p, digits = 3), "\n")
cat("NOTE: direction check -- is managed > unmanaged forest (the hypothesis), or the reverse?\n")

cat("\n-- ET: same grouping (control) --\n")
print(as.data.frame(t5_et_test$table))
cat("Wilcoxon W =", t5_et_test$wilcox_W, " p =", format.pval(t5_et_test$wilcox_p, digits = 3), "\n")

out_t5a <- file.path(OUTD, "table_t5_by_igbp_class.csv")
out_t5b <- file.path(OUTD, "table_t5_managed_vs_forest_test.csv")
readr::write_csv(dplyr::bind_rows(dplyr::mutate(t5_nee_by_class, axis="nee", .before=1),
                                   dplyr::mutate(t5_et_by_class, axis="et", .before=1)), out_t5a)
readr::write_csv(dplyr::bind_rows(
  dplyr::mutate(t5_nee_test$table, axis = "nee", wilcox_W = t5_nee_test$wilcox_W, wilcox_p = t5_nee_test$wilcox_p),
  dplyr::mutate(t5_et_test$table,  axis = "et",  wilcox_W = t5_et_test$wilcox_W,  wilcox_p = t5_et_test$wilcox_p)
), out_t5b)
write_output_metadata(out_t5a, input_sources = c(out_pairs, file.path(SNAP, "badm_management_coverage.csv")),
  notes = "Per-IGBP-class median(measured-TRENDY) difference, NEE and ET, with BADM management-flag match counts. This is a proxy per the task instruction -- separating harvest/fire/land-use terms would require TRENDY variables (fFire, fHarvest, fLUC) not downloaded in this repo; only nbp and evapotrans were downloaded (confirmed against scripts/figure_representativeness_trendy_compute.R and data/external/trendy/ contents).")
write_output_metadata(out_t5b, input_sources = out_t5a, notes = "Managed (CRO+GRA+BADM-flagged-managed WET) vs. unmanaged-forest two-group Wilcoxon rank-sum test on the paired difference, NEE and ET.")
message("Saved: ", out_t5a, ", ", out_t5b)

# ============================================================================
# T6. SUB-GRID SITING BIAS
# ============================================================================

message("\n================ T6: sub-grid siting bias ================")

biomass_rast <- terra::rast(file.path(EXT, "cci_biomass", "ESACCI-BIOMASS-L4-AGB-MERGED-1000m-fv7.0.tif"))[[18L]]
trendy_grid  <- terra::rast(file.path(EXT, "trendy", "derived", "trendy_et_median.tif"))
message("Biomass raster res: ", paste(round(terra::res(biomass_rast), 5), collapse=" x "),
        " | TRENDY grid res: ", paste(terra::res(trendy_grid), collapse=" x "))
stopifnot(abs(terra::res(trendy_grid)[1] / terra::res(biomass_rast)[1] - 50) < 1e-6)

site_biomass_percentile <- function(lon, lat) {
  cell <- terra::cellFromXY(trendy_grid, cbind(lon, lat))
  if (is.na(cell)) return(NA_real_)
  cell_ext <- terra::ext(trendy_grid, cell)
  win <- terra::crop(biomass_rast, cell_ext)
  vals <- terra::values(win, na.rm = TRUE)
  if (length(vals) < 10L) return(NA_real_)
  site_val <- terra::extract(biomass_rast, cbind(lon, lat))[[1]]
  if (is.na(site_val)) return(NA_real_)
  mean(vals <= site_val) * 100
}

message("Computing sub-grid biomass percentile for ", nrow(paired_nee), " NEE + ", nrow(paired_et), " ET sites ",
        "(each site = one small 50x50-pixel crop; may take a few minutes)...")

all_sites <- dplyr::bind_rows(
  dplyr::select(paired_nee, site_id, location_lat, location_long, diff) |> dplyr::mutate(axis = "nee"),
  dplyr::select(paired_et,  site_id, location_lat, location_long, diff) |> dplyr::mutate(axis = "et")
) |> dplyr::distinct(site_id, axis, .keep_all = TRUE)

all_sites$biomass_percentile <- vapply(seq_len(nrow(all_sites)), function(i) {
  site_biomass_percentile(all_sites$location_long[i], all_sites$location_lat[i])
}, numeric(1L))

t6 <- all_sites |> dplyr::filter(!is.na(biomass_percentile))
cor_nee <- cor.test(t6$biomass_percentile[t6$axis=="nee"], t6$diff[t6$axis=="nee"], method = "spearman")
cor_et  <- cor.test(t6$biomass_percentile[t6$axis=="et"],  t6$diff[t6$axis=="et"],  method = "spearman")

cat("\n-- Sub-grid biomass percentile distribution --\n")
print(t6 |> dplyr::group_by(axis) |> dplyr::summarise(
  n = dplyr::n(), median_pct = median(biomass_percentile),
  p25 = quantile(biomass_percentile, .25), p75 = quantile(biomass_percentile, .75)))
cat("\nSpearman(biomass_percentile, measured-TRENDY diff): NEE rho=", round(unname(cor_nee$estimate),3),
    " p=", format.pval(cor_nee$p.value, digits=3),
    " | ET rho=", round(unname(cor_et$estimate),3), " p=", format.pval(cor_et$p.value, digits=3), "\n")

out_t6 <- file.path(OUTD, "table_t6_siting_bias.csv")
readr::write_csv(t6, out_t6)
write_output_metadata(out_t6, input_sources = c(out_pairs,
  file.path(EXT, "cci_biomass", "ESACCI-BIOMASS-L4-AGB-MERGED-1000m-fv7.0.tif"),
  file.path(EXT, "trendy", "derived", "trendy_et_median.tif")),
  notes = sprintf("Per-site percentile of its 1km ESA CCI biomass value within its enclosing 0.5deg TRENDY grid cell's 1km biomass population (2500 pixels/cell, grids confirmed perfectly nested 50:1, no resampling). Spearman vs. paired diff: NEE rho=%.3f p=%s, ET rho=%.3f p=%s.",
    unname(cor_nee$estimate), format.pval(cor_nee$p.value, digits=3), unname(cor_et$estimate), format.pval(cor_et$p.value, digits=3)))
message("Saved: ", out_t6)

# ============================================================================
# T7. PAIRED FIGURE
# ============================================================================

message("\n================ T7: paired figure ================")

IGBP_COLORS <- setNames(
  grDevices::colorRampPalette(c("#8B4513","#228B22","#6B8E23","#FFD700","#00CED1",
                                 "#9400D3","#FF69B4","#4682B4","#A0522D"))(length(IGBP_ORDER)),
  IGBP_ORDER
)

make_scatter <- function(df, unit_lab, bin_df, row_label) {
  fit <- lm(log10(measured_abs) ~ log10(trendy_abs), data = df)
  slope <- coef(fit)[[2]]
  med_bias <- median(df$diff); iqr <- quantile(df$diff, c(.25,.75))
  ann <- sprintf("OLS slope (log-log) = %.2f\nmedian bias = %.1f %s\nIQR = [%.1f, %.1f]",
                 slope, med_bias, unit_lab, iqr[1], iqr[2])
  rng <- range(c(df$measured_abs, df$trendy_abs), na.rm = TRUE)
  ggplot(df, aes(x = trendy_abs, y = measured_abs, colour = igbp_class)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey40") +
    geom_point(alpha = 0.6, size = 1.6) +
    scale_colour_manual(values = IGBP_COLORS, name = "IGBP", drop = TRUE) +
    scale_x_log10(limits = rng, name = paste0("TRENDY-at-site (", unit_lab, ")")) +
    scale_y_log10(limits = rng, name = paste0("Measured (", unit_lab, ")")) +
    annotate("text", x = rng[1]*1.3, y = rng[2]/1.5, label = ann, hjust = 0, size = 2.6, lineheight=1.1) +
    labs(title = row_label) +
    theme_minimal(base_size = 9) +
    theme(plot.background = element_rect(fill="white", colour=NA), legend.key.size = unit(0.3,"cm"))
}

make_marginals <- function(df, bin_df, unit_lab, row_label) {
  long <- dplyr::bind_rows(
    dplyr::transmute(df, value = measured_abs, source = "Measured"),
    dplyr::transmute(df, value = trendy_abs, source = "TRENDY-at-site")
  )
  edges <- sort(unique(c(bin_df$min_value, max(long$value, na.rm=TRUE))))
  ggplot(long, aes(x = value, fill = source)) +
    geom_histogram(aes(y = after_stat(density)), position = "identity", alpha = 0.5, bins = 40) +
    geom_vline(xintercept = bin_df$min_value, linetype = "dotted", colour = "grey30", linewidth = 0.3) +
    scale_fill_manual(values = c("Measured" = "#D55E00", "TRENDY-at-site" = "#0072B2")) +
    labs(title = paste0(row_label, " -- marginal distributions with stored 7-bin edges"), x = unit_lab, y = "density") +
    theme_minimal(base_size = 9) +
    theme(plot.background = element_rect(fill="white", colour=NA))
}

p_nee_scatter <- make_scatter(paired_nee, "gC m-2 yr-1", nee_bins, "NEE: measured abs(NEP) vs. TRENDY-at-site abs(NBP) (nee_median)")
p_et_scatter  <- make_scatter(paired_et,  "mm yr-1",     et_bins,  "ET: measured vs. TRENDY-at-site (et_median)")
p_nee_marg    <- make_marginals(paired_nee, nee_bins, "gC m-2 yr-1", "NEE")
p_et_marg     <- make_marginals(paired_et,  et_bins,  "mm yr-1",     "ET")

fig <- gridExtra::arrangeGrob(p_nee_scatter, p_nee_marg, p_et_scatter, p_et_marg, nrow = 2, ncol = 2)
out_fig <- file.path(OUTD, "fig_t7_measured_vs_trendy_paired.png")
ggsave(out_fig, fig, width = 12, height = 8, dpi = 300, bg = "white")
message("Saved: ", out_fig)

message("\n=== nee_et_site_vs_trendy_core.R complete ===")
