## era5_cumulative_test.R
##
## Read-only diagnostic test. Run on the mini, where data/extracted is
## complete (781 sites) and the raw *_FLUXNET_ERA5_MM_*.csv files are all
## present. Reads those raw files directly -- NOT DuckDB -- so ingestion
## (03_read.R / duckdb_setup.R / duckdb_update.R) plays no part in this
## test. No edits to anything in the pipeline, any figure, legend,
## snapshot CSV, or any prior diagnostic output. Writes only new files
## under review/diagnostics/era5_cumulative_test/.
##
## HYPOTHESIS under test: the monthly P_ERA series at the affected sites
## may be a within-year cumulative total rather than a monthly value.
## This script tests that hypothesis; it does not assume it.

source("R/pipeline_config.R")
source("R/utils.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(ggplot2)
  library(fs)
  library(lubridate)
  library(purrr)
})

OUTD <- "review/diagnostics/era5_cumulative_test"
fs::dir_create(OUTD)

message("=== era5_cumulative_test.R ===")
message("Host: ", Sys.info()[["nodename"]])

AFFECTED <- c("JP-Tak", "JP-Mse", "JP-Yms", "KH-Kmp", "PE-QFR", "BR-Ji3")
CONTROLS <- c("US-Ha1", "DE-Tha", "AU-Wom")
NAMED_SITES <- c(AFFECTED, CONTROLS)

# ============================================================================
# 0. SITE-DIRECTORY LOOKUP AND RAW-FILE READER (read-only, raw CSVs only)
# ============================================================================

extracted_dirs <- list.dirs("data/extracted", recursive = FALSE, full.names = TRUE)
dir_site_ids <- sub("^[A-Za-z0-9]+_([A-Za-z]{2}-[A-Za-z0-9]+)_FLUXNET_.*$", "\\1", basename(extracted_dirs))
site_dir_lookup <- setNames(extracted_dirs, dir_site_ids)
message("data/extracted/: ", length(extracted_dirs), " site directories found")

read_era5_mm <- function(site_id) {
  d <- site_dir_lookup[[site_id]]
  if (is.null(d) || is.na(d)) return(NULL)
  f <- list.files(d, pattern = "_ERA5_MM_.*\\.csv$", full.names = TRUE)
  if (length(f) != 1) return(NULL)
  raw <- readr::read_csv(f, show_col_types = FALSE, progress = FALSE)
  if (!all(c("TIMESTAMP", "P_ERA", "TA_ERA") %in% names(raw))) return(NULL)
  raw |>
    dplyr::transmute(
      site_id = site_id,
      year = TIMESTAMP %/% 100L,
      month = TIMESTAMP %% 100L,
      P_ERA, TA_ERA,
      days = lubridate::days_in_month(as.Date(sprintf("%04d-%02d-01", year, month)))
    )
}

complete_years <- function(df, value_col) {
  df |>
    dplyr::filter(!is.na(.data[[value_col]])) |>
    dplyr::group_by(site_id, year) |>
    dplyr::filter(dplyr::n_distinct(month) == 12L) |>
    dplyr::ungroup()
}

## Most recent run of 3 *consecutive* complete years for a site.
last_three_consecutive <- function(years) {
  years <- sort(unique(years), decreasing = TRUE)
  for (y in years) {
    if ((y - 1) %in% years && (y - 2) %in% years) return(c(y - 2, y - 1, y))
  }
  NA_integer_
}

# ============================================================================
# 1. RAW MONTHLY P_ERA, 3 CONSECUTIVE COMPLETE YEARS, 9 NAMED SITES
# ============================================================================

message("\n================ Step 1: raw monthly P_ERA, 9 named sites ================")

named_raw <- purrr::map(NAMED_SITES, read_era5_mm) |> purrr::compact() |> dplyr::bind_rows()
stopifnot(all(NAMED_SITES %in% unique(named_raw$site_id)))

step1_years <- complete_years(named_raw, "P_ERA") |>
  dplyr::group_by(site_id) |>
  dplyr::summarise(years3 = list(last_three_consecutive(year)), .groups = "drop")

step1_table <- named_raw |>
  dplyr::inner_join(step1_years, by = "site_id") |>
  dplyr::rowwise() |>
  dplyr::filter(year %in% years3) |>
  dplyr::ungroup() |>
  dplyr::mutate(role = ifelse(site_id %in% AFFECTED, "affected", "control")) |>
  dplyr::select(role, site_id, year, month, P_ERA) |>
  dplyr::arrange(match(site_id, NAMED_SITES), year, month)

cat("\n-- Step 1: raw monthly P_ERA (mm/day as stored), 3 consecutive complete years per site --\n")
for (sid in NAMED_SITES) {
  sub <- step1_table |> dplyr::filter(site_id == sid)
  cat(sprintf("\n%s (%s), years %s:\n", sid, unique(sub$role), paste(unique(sub$year), collapse = ", ")))
  wide <- sub |> tidyr::pivot_wider(id_cols = year, names_from = month, values_from = P_ERA)
  print(as.data.frame(wide), row.names = FALSE)
}

out_step1 <- file.path(OUTD, "table_step1_raw_monthly.csv")
readr::write_csv(step1_table, out_step1)
write_output_metadata(out_step1, input_sources = "data/extracted/*/*_ERA5_MM_*.csv (raw, read directly)",
  notes = "Raw monthly P_ERA (mm/day as stored in the raw file) for the 6 affected + 3 control sites, most recent run of 3 consecutive complete calendar years per site. Read-only test; no DuckDB, no pipeline edits.")
message("Saved: ", out_step1)

# ============================================================================
# 2. DEC/JAN RATIO AND SPEARMAN RHO vs MONTH, EVERY SITE, GROUPED BY CLUSTER
# ============================================================================

message("\n================ Step 2: Dec/Jan ratio and rho(value, month), network-wide ================")

comparison <- readr::read_csv("review/diagnostics/era5_reference_plots/table_site_reference_comparison.csv", show_col_types = FALSE)
network_sites <- comparison$site_id
stopifnot(length(network_sites) == 781)

siteyear_stat <- function(df, value_col) {
  cy <- complete_years(df, value_col)
  if (nrow(cy) == 0) return(dplyr::tibble())
  cy |>
    dplyr::group_by(site_id, year) |>
    dplyr::summarise(
      dec_jan_ratio = .data[[value_col]][month == 12] / .data[[value_col]][month == 1],
      rho = suppressWarnings(cor(.data[[value_col]], month, method = "spearman")),
      .groups = "drop"
    )
}

all_era5 <- vector("list", length(network_sites))
n_missing_file <- 0L
for (i in seq_along(network_sites)) {
  sid <- network_sites[i]
  d <- read_era5_mm(sid)
  if (is.null(d)) { n_missing_file <- n_missing_file + 1L; next }
  all_era5[[i]] <- d
}
all_era5 <- dplyr::bind_rows(all_era5)
message(sprintf("Sites with a readable ERA5 MM file: %d / %d (%d missing/unreadable)",
                 length(unique(all_era5$site_id)), length(network_sites), n_missing_file))

p_stats <- siteyear_stat(all_era5, "P_ERA") |>
  dplyr::left_join(comparison |> dplyr::select(site_id, cluster_membership), by = "site_id") |>
  dplyr::mutate(in_cluster = cluster_membership != "not_in_4x_or_8x_cluster")

cat("\n-- Step 2: P_ERA Dec/Jan ratio and Spearman rho(value, month), per site-year, by 4x/8x cluster --\n")
p_stats_summary <- p_stats |>
  dplyr::filter(is.finite(dec_jan_ratio), is.finite(rho)) |>
  dplyr::group_by(in_cluster) |>
  dplyr::summarise(
    n_site_years = dplyr::n(), n_sites = dplyr::n_distinct(site_id),
    dec_jan_ratio_median = median(dec_jan_ratio), dec_jan_ratio_q25 = quantile(dec_jan_ratio, .25),
    dec_jan_ratio_q75 = quantile(dec_jan_ratio, .75),
    rho_median = median(rho), rho_q25 = quantile(rho, .25), rho_q75 = quantile(rho, .75),
    .groups = "drop"
  )
print(as.data.frame(p_stats_summary))

out_step2 <- file.path(OUTD, "table_step2_siteyear_stats_P_ERA.csv")
readr::write_csv(p_stats, out_step2)
out_step2_summary <- file.path(OUTD, "table_step2_summary_P_ERA.csv")
readr::write_csv(p_stats_summary, out_step2_summary)
write_output_metadata(out_step2, input_sources = c("data/extracted/*/*_ERA5_MM_*.csv (raw, read directly)",
  "review/diagnostics/era5_reference_plots/table_site_reference_comparison.csv"),
  notes = "Per site-year: P_ERA December/January ratio and Spearman rho(P_ERA, month index), all complete calendar years, all sites with a readable raw ERA5 MM file, tagged with 4x/8x cluster membership.")
write_output_metadata(out_step2_summary, input_sources = out_step2,
  notes = "Step 2 summary: median/IQR of Dec/Jan ratio and Spearman rho, grouped by 4x/8x cluster membership.")
message("Saved: ", out_step2, " and ", out_step2_summary)

# ============================================================================
# 3. QUANTITATIVE PREDICTION: IMPLIED TRUE MONTHLY VALUES, AFFECTED SITES
# ============================================================================

message("\n================ Step 3: implied true monthly values (differencing), affected sites ================")

affected_era5 <- all_era5 |> dplyr::filter(site_id %in% AFFECTED)
affected_cy <- complete_years(affected_era5, "P_ERA") |> dplyr::arrange(site_id, year, month)

step3_siteyear <- affected_cy |>
  dplyr::mutate(raw_mm = P_ERA * days) |>          # pipeline's own day-weighting
  dplyr::arrange(site_id, year, month) |>
  dplyr::group_by(site_id, year) |>
  dplyr::mutate(diff_mm = raw_mm - dplyr::lag(raw_mm),
                implied_mm = dplyr::if_else(month == 1, raw_mm, pmax(diff_mm, 0))) |>
  dplyr::summarise(
    implied_annual_mm = sum(implied_mm),
    raw_sum_mm = sum(raw_mm),
    predicted_factor = raw_sum_mm / implied_annual_mm,
    n_negative_diffs = sum(diff_mm < 0, na.rm = TRUE),
    .groups = "drop"
  )

step3_site <- step3_siteyear |>
  dplyr::group_by(site_id) |>
  dplyr::summarise(
    n_years = dplyr::n(),
    implied_annual_mm_mean = mean(implied_annual_mm),
    predicted_factor_mean = mean(predicted_factor),
    predicted_factor_median = median(predicted_factor),
    .groups = "drop"
  ) |>
  dplyr::left_join(comparison |> dplyr::select(site_id, ratio_to_bio12, bio12_mm, badm_map_mm, era5_map_mm), by = "site_id") |>
  dplyr::mutate(factor_vs_ratio_diff = predicted_factor_mean - ratio_to_bio12)

cat("\n-- Step 3: predicted factor (sum(cumulative)/implied true total) vs observed ratio_to_bio12 --\n")
print(as.data.frame(step3_site |>
  dplyr::select(site_id, n_years, predicted_factor_mean, ratio_to_bio12, factor_vs_ratio_diff, implied_annual_mm_mean, bio12_mm, badm_map_mm)))

out_step3_siteyear <- file.path(OUTD, "table_step3_siteyear.csv")
out_step3_site <- file.path(OUTD, "table_step3_site_summary.csv")
readr::write_csv(step3_siteyear, out_step3_siteyear)
readr::write_csv(step3_site, out_step3_site)
write_output_metadata(out_step3_siteyear, input_sources = "data/extracted/*/*_ERA5_MM_*.csv (raw, read directly)",
  notes = "Per site-year, affected sites only: raw_mm = P_ERA*days_in_month (pipeline's own day-weighting); diff_mm = raw_mm - previous month's raw_mm (Jan uses its own raw_mm, no prior month); implied_mm = diff_mm clamped to >=0 (per instruction, 'treat the positive differences as the implied true monthly values'); implied_annual_mm = sum(implied_mm); predicted_factor = sum(raw_mm)/implied_annual_mm.")
write_output_metadata(out_step3_site, input_sources = c(out_step3_siteyear, "review/diagnostics/era5_reference_plots/table_site_reference_comparison.csv"),
  notes = "Per-site mean of predicted_factor across that site's complete years, compared against the pipeline's own observed ratio_to_bio12 (era5_map_mm/bio12_mm, table_site_reference_comparison.csv). Quantitative test of the cumulative-total hypothesis.")
message("Saved: ", out_step3_siteyear, " and ", out_step3_site)

# ============================================================================
# 4. IMPLIED TRUE ANNUAL TOTAL vs. BIO12/BADM, NORMAL-SCATTER BAND FROM NEAR-1x
# ============================================================================

message("\n================ Step 4: implied true annual total vs BIO12/BADM, near-1x normal scatter ================")

b1 <- readr::read_csv("review/diagnostics/era5_precip_units_v3/table_b1_factor_estimates.csv", show_col_types = FALSE)
near1 <- b1 |> dplyr::filter(nearest_cluster == "1")
message(sprintf("Near-1x group (table_b1_factor_estimates.csv, nearest_cluster=='1'): %d sites", nrow(near1)))

band_bio12 <- quantile(near1$ratio_to_bio12, c(.05, .95), na.rm = TRUE)
band_badm  <- quantile(near1$ratio_to_badm_map, c(.05, .95), na.rm = TRUE)
message(sprintf("Near-1x normal-scatter band, ratio_to_bio12: [%.3f, %.3f]", band_bio12[1], band_bio12[2]))
message(sprintf("Near-1x normal-scatter band, ratio_to_badm_map: [%.3f, %.3f]", band_badm[1], band_badm[2]))

step4 <- step3_site |>
  dplyr::mutate(
    implied_ratio_to_bio12 = implied_annual_mm_mean / bio12_mm,
    implied_ratio_to_badm  = implied_annual_mm_mean / badm_map_mm,
    within_band_bio12 = implied_ratio_to_bio12 >= band_bio12[1] & implied_ratio_to_bio12 <= band_bio12[2],
    within_band_badm  = !is.na(badm_map_mm) & implied_ratio_to_badm >= band_badm[1] & implied_ratio_to_badm <= band_badm[2]
  )

cat("\n-- Step 4: implied true annual total vs BIO12/BADM, and whether within the near-1x normal-scatter band --\n")
print(as.data.frame(step4 |>
  dplyr::select(site_id, implied_annual_mm_mean, bio12_mm, implied_ratio_to_bio12, within_band_bio12,
                 badm_map_mm, implied_ratio_to_badm, within_band_badm)))

out_step4 <- file.path(OUTD, "table_step4_implied_vs_references.csv")
readr::write_csv(step4, out_step4)
write_output_metadata(out_step4, input_sources = c(out_step3_site, "review/diagnostics/era5_precip_units_v3/table_b1_factor_estimates.csv"),
  notes = sprintf("Implied true annual total (step 3) divided by BIO12 and BADM MAP, checked against the near-1x group's (nearest_cluster=='1', n=%d) 5th-95th percentile band of ratio_to_bio12 [%.3f, %.3f] and ratio_to_badm_map [%.3f, %.3f].",
                   nrow(near1), band_bio12[1], band_bio12[2], band_badm[1], band_badm[2]))
message("Saved: ", out_step4)

# ============================================================================
# 5. SAME TEST ON TA_ERA (AVERAGED VARIABLE), 9 NAMED SITES
# ============================================================================

message("\n================ Step 5: Dec/Jan ratio and rho(value, month) for TA_ERA, 9 named sites ================")

ta_stats <- siteyear_stat(named_raw, "TA_ERA") |>
  dplyr::mutate(role = ifelse(site_id %in% AFFECTED, "affected", "control"))

cat("\n-- Step 5: TA_ERA Dec/Jan ratio and Spearman rho(value, month), per site-year, 9 named sites --\n")
ta_stats_summary <- ta_stats |>
  dplyr::filter(is.finite(dec_jan_ratio), is.finite(rho)) |>
  dplyr::group_by(role) |>
  dplyr::summarise(n_site_years = dplyr::n(), n_sites = dplyr::n_distinct(site_id),
                    dec_jan_ratio_median = median(dec_jan_ratio), rho_median = median(rho), .groups = "drop")
print(as.data.frame(ta_stats_summary))
cat("\nPer site-year detail:\n")
print(as.data.frame(ta_stats |> dplyr::arrange(match(site_id, NAMED_SITES), year)))

out_step5 <- file.path(OUTD, "table_step5_siteyear_stats_TA_ERA.csv")
readr::write_csv(ta_stats, out_step5)
write_output_metadata(out_step5, input_sources = "data/extracted/*/*_ERA5_MM_*.csv (raw, read directly)",
  notes = "Same statistics as step 2 (Dec/Jan ratio, Spearman rho(value, month)) computed on TA_ERA instead of P_ERA, for the same 9 named sites (6 affected + 3 control), to check whether the effect is precipitation-specific.")
message("Saved: ", out_step5)

# ============================================================================
# FIGURE: monthly P_ERA vs month, 3 years, small multiples
# ============================================================================

message("\n================ Figure: monthly P_ERA vs month, small multiples ================")

fig_df <- step1_table |>
  dplyr::mutate(site_label = paste0(site_id, " (", role, ")"),
                site_label = factor(site_label, levels = unique(site_label[match(NAMED_SITES, site_id)])),
                year = factor(year))

fig <- ggplot(fig_df, aes(x = month, y = P_ERA, colour = year)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1.2) +
  facet_wrap(~site_label, ncol = 3) +
  scale_x_continuous(breaks = c(1, 4, 7, 10, 12)) +
  labs(x = "month", y = "raw monthly P_ERA (as stored, mm/day)", colour = "year",
       title = "Raw monthly P_ERA, 3 consecutive complete years -- affected sites and controls") +
  theme_minimal(base_size = 11) +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        strip.text = element_text(size = 9))

out_fig <- file.path(OUTD, "fig_monthly_p_era_small_multiples.png")
ggsave(out_fig, fig, width = 10, height = 8, dpi = 300, bg = "white")
write_output_metadata(out_fig, input_sources = out_step1,
  notes = "Raw monthly P_ERA (as stored in the raw ERA5 MM file, mm/day) vs. month, one panel per site, 3 lines (one per consecutive complete year), 6 affected + 3 control sites.")
message("Saved: ", out_fig)

message("\n=== era5_cumulative_test.R complete ===")
