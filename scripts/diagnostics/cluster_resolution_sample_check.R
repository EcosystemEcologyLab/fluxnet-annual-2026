## cluster_resolution_sample_check.R
##
## Follow-up to cluster_resolution_sample_download.R (which selected a
## stratified sample from the 123-site 4x/8x cluster identified in
## era5_precip_units_v4 -- up to 3 flagged sites per product_source_network,
## plus 1 unflagged control per network, fixed seed 20260920 -- and
## downloaded/extracted HH resolution for the sample). This script runs the
## actual check: does IT-MBo's DD/MM/YY-vs-HH resolution mismatch
## (it_mbo_bug_hunt.R: ~21.25x) generalise across the sampled cluster sites,
## or is it unusual?
##
## Read-only with respect to every existing diagnostic output and the
## pipeline itself. All values in mm/yr. No ratio statistics as headline
## numbers, no factor fitting -- the one agreement/disagreement criterion
## used in the verdict is a fixed, stated, round-number threshold (differs
## by more than 2x either direction), not fit to this sample.

source("R/pipeline_config.R")
source("R/utils.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(lubridate)
  library(fs)
  library(ggplot2)
})

OUTD <- "review/diagnostics/cluster_resolution_sample"
fs::dir_create(OUTD)

message("=== cluster_resolution_sample_check.R ===")

sample_df <- readr::read_csv(file.path(OUTD, "table_0_sample_selection.csv"), show_col_types = FALSE)
SITES <- sample_df$site_id
message(length(SITES), " sampled sites: ", paste(SITES, collapse = ", "))

find_dirs <- function(site_id) {
  d <- list.files("data/extracted", pattern = paste0("_", site_id, "_FLUXNET_"), full.names = TRUE)
  d[dir.exists(d)]
}
find_file <- function(dirs, pattern) {
  for (dir in dirs) {
    f <- list.files(dir, pattern = pattern, full.names = TRUE)
    if (length(f) > 0L) return(f[[1L]])
  }
  NA_character_
}

MISSING_MAX <- -9998  # FLUXNET missing sentinel is -9999; treat <= -9998 as missing

# ============================================================================
# Resolution helpers (same logic as it_mbo_parsimony.R)
# ============================================================================
expected_hh_per_day <- 48L

mean_annual_hh <- function(df, value_col) {
  if (is.null(df) || nrow(df) == 0L) return(list(mean_mm = NA_real_, n_years = 0L))
  per_day <- df |> dplyr::count(date, year, name = "n_hh")
  complete_days <- per_day |> dplyr::filter(n_hh == expected_hh_per_day)
  if (nrow(complete_days) == 0L) return(list(mean_mm = NA_real_, n_years = 0L))
  daily_sum <- df |> dplyr::inner_join(dplyr::select(complete_days, date), by = "date") |>
    dplyr::group_by(date, year) |> dplyr::summarise(day_total = sum(.data[[value_col]]), .groups = "drop")
  days_per_year <- daily_sum |> dplyr::count(year, name = "n_days")
  expected_days <- tibble::tibble(year = days_per_year$year,
                                    expected = ifelse(lubridate::leap_year(days_per_year$year), 366L, 365L))
  complete_years <- dplyr::inner_join(days_per_year, expected_days, by = "year") |>
    dplyr::filter(n_days == expected)
  if (nrow(complete_years) == 0L) return(list(mean_mm = NA_real_, n_years = 0L))
  annual <- daily_sum |> dplyr::inner_join(dplyr::select(complete_years, year), by = "year") |>
    dplyr::group_by(year) |> dplyr::summarise(annual_total = sum(day_total), .groups = "drop")
  list(mean_mm = mean(annual$annual_total), n_years = nrow(annual))
}

mean_annual_dd <- function(df, value_col) {
  if (is.null(df) || nrow(df) == 0L) return(list(mean_mm = NA_real_, n_years = 0L))
  per_year <- df |> dplyr::group_by(year) |>
    dplyr::summarise(n_days = dplyr::n(), annual_total = sum(.data[[value_col]]), .groups = "drop") |>
    dplyr::mutate(expected = ifelse(lubridate::leap_year(year), 366L, 365L)) |>
    dplyr::filter(n_days == expected)
  if (nrow(per_year) == 0L) return(list(mean_mm = NA_real_, n_years = 0L))
  list(mean_mm = mean(per_year$annual_total), n_years = nrow(per_year))
}

mean_annual_mm <- function(df, value_col) {
  if (is.null(df) || nrow(df) == 0L) return(list(mean_mm = NA_real_, n_years = 0L))
  per_year <- df |> dplyr::mutate(days = lubridate::days_in_month(as.Date(sprintf("%04d-%02d-01", year, month)))) |>
    dplyr::group_by(year) |>
    dplyr::summarise(n_months = dplyr::n(), annual_total = sum(.data[[value_col]] * days), .groups = "drop") |>
    dplyr::filter(n_months == 12L)
  if (nrow(per_year) == 0L) return(list(mean_mm = NA_real_, n_years = 0L))
  list(mean_mm = mean(per_year$annual_total), n_years = nrow(per_year))
}

mean_annual_yy <- function(df, value_col) {
  if (is.null(df) || nrow(df) == 0L) return(list(mean_mm = NA_real_, n_years = 0L))
  list(mean_mm = mean(df[[value_col]], na.rm = TRUE), n_years = nrow(df))
}

# ============================================================================
# Load raw files and compute D1-style table for every sampled site
# ============================================================================
message("\n================ Loading raw files and computing mean annual precipitation ================")

d1_rows <- list()
load_failures <- character(0)

for (sid in SITES) {
  d <- find_dirs(sid)
  if (length(d) == 0L) {
    message(sid, ": no extracted directory found -- skipping (download may have failed for this site)")
    load_failures <- c(load_failures, sid)
    next
  }
  paths <- list(
    era_hh = find_file(d, "FLUXNET_ERA5_HH_.*\\.csv$"),
    era_dd = find_file(d, "FLUXNET_ERA5_DD_.*\\.csv$"),
    era_mm = find_file(d, "FLUXNET_ERA5_MM_.*\\.csv$"),
    era_yy = find_file(d, "FLUXNET_ERA5_YY_.*\\.csv$"),
    fm_hh  = find_file(d, "FLUXNET_FLUXMET_HH_.*\\.csv$"),
    fm_dd  = find_file(d, "FLUXNET_FLUXMET_DD_.*\\.csv$"),
    fm_mm  = find_file(d, "FLUXNET_FLUXMET_MM_.*\\.csv$"),
    fm_yy  = find_file(d, "FLUXNET_FLUXMET_YY_.*\\.csv$")
  )
  missing <- names(paths)[vapply(paths, is.na, logical(1))]
  if (length(missing) > 0L) {
    message(sid, ": missing file(s) for ", paste(missing, collapse = ", "), " -- skipping")
    load_failures <- c(load_failures, sid)
    next
  }

  raw <- list(
    era_hh = readr::read_csv(paths$era_hh, show_col_types = FALSE) |>
      dplyr::mutate(date = as.Date(substr(as.character(TIMESTAMP_START), 1, 8), format = "%Y%m%d"),
                     year = lubridate::year(date)) |>
      dplyr::filter(P_ERA > MISSING_MAX),
    era_dd = readr::read_csv(paths$era_dd, show_col_types = FALSE) |>
      dplyr::mutate(date = as.Date(as.character(TIMESTAMP), format = "%Y%m%d"), year = lubridate::year(date)) |>
      dplyr::filter(P_ERA > MISSING_MAX),
    era_mm = readr::read_csv(paths$era_mm, show_col_types = FALSE) |>
      dplyr::mutate(year = TIMESTAMP %/% 100L, month = TIMESTAMP %% 100L) |>
      dplyr::filter(P_ERA > MISSING_MAX),
    era_yy = readr::read_csv(paths$era_yy, show_col_types = FALSE) |>
      dplyr::rename(year = TIMESTAMP) |> dplyr::filter(P_ERA > MISSING_MAX),
    fm_hh = readr::read_csv(paths$fm_hh, show_col_types = FALSE) |>
      dplyr::mutate(date = as.Date(substr(as.character(TIMESTAMP_START), 1, 8), format = "%Y%m%d"),
                     year = lubridate::year(date)) |>
      dplyr::filter(P_F > MISSING_MAX),
    fm_dd = readr::read_csv(paths$fm_dd, show_col_types = FALSE) |>
      dplyr::mutate(date = as.Date(as.character(TIMESTAMP), format = "%Y%m%d"), year = lubridate::year(date)) |>
      dplyr::filter(P_F > MISSING_MAX),
    fm_mm = readr::read_csv(paths$fm_mm, show_col_types = FALSE) |>
      dplyr::mutate(year = TIMESTAMP %/% 100L, month = TIMESTAMP %% 100L) |>
      dplyr::filter(P_F > MISSING_MAX),
    fm_yy = readr::read_csv(paths$fm_yy, show_col_types = FALSE) |>
      dplyr::rename(year = TIMESTAMP) |> dplyr::filter(P_F > MISSING_MAX)
  )

  for (var_pair in list(c("era", "P_ERA"), c("fm", "P_F"))) {
    src <- var_pair[1]; col <- var_pair[2]
    hh <- mean_annual_hh(raw[[paste0(src, "_hh")]], col)
    dd <- mean_annual_dd(raw[[paste0(src, "_dd")]], col)
    mm <- mean_annual_mm(raw[[paste0(src, "_mm")]], col)
    yy <- mean_annual_yy(raw[[paste0(src, "_yy")]], col)
    d1_rows[[paste(sid, col)]] <- tibble::tibble(
      site_id = sid, variable = col,
      mean_hh_mm = hh$mean_mm, n_years_hh = hh$n_years,
      mean_dd_mm = dd$mean_mm, n_years_dd = dd$n_years,
      mean_mm_mm = mm$mean_mm, n_years_mm = mm$n_years,
      mean_yy_mm = yy$mean_mm, n_years_yy = yy$n_years
    )
  }
  message(sid, ": OK")
}

if (length(load_failures) > 0L) {
  message("\nSites skipped (no usable extracted data): ", paste(load_failures, collapse = ", "))
}

d1 <- dplyr::bind_rows(d1_rows) |>
  dplyr::left_join(dplyr::select(sample_df, site_id, product_source_network, role, bio12_mm, badm_map_mm),
                    by = "site_id") |>
  dplyr::relocate(product_source_network, role, .after = site_id)

print(as.data.frame(d1))

out_table <- file.path(OUTD, "table_1_mean_annual_by_resolution.csv")
readr::write_csv(d1, out_table)
write_output_metadata(out_table,
  input_sources = "data/extracted/*/*_{ERA5,FLUXMET}_{HH,DD,MM,YY}_*.csv (raw, read directly) and table_0_sample_selection.csv (BIO12/BADM columns, read-only)",
  notes = sprintf(
    "Mean annual precipitation (mm/yr) by resolution (HH/DD/MM/YY) and variable (P_ERA/P_F), for the %d-site stratified sample from the 123-site 4x/8x cluster (seed 20260920). Complete-coverage years only; n_years_* gives the year count per cell. %d site(s) skipped for missing extracted data: %s.",
    length(unique(d1$site_id)), length(load_failures), if (length(load_failures) > 0L) paste(load_failures, collapse = ", ") else "none"))
message("Saved: ", out_table)

# ============================================================================
# Figure: MM branch (x) vs HH branch (y), P_ERA only, log-log, 1:1 line
# ============================================================================
message("\n================ Figure ================")

fig_df <- d1 |> dplyr::filter(variable == "P_ERA", !is.na(mean_hh_mm), !is.na(mean_mm_mm))

p <- ggplot2::ggplot(fig_df, ggplot2::aes(x = mean_mm_mm, y = mean_hh_mm, color = role, shape = role)) +
  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
  ggplot2::geom_point(size = 3, alpha = 0.85) +
  ggplot2::scale_x_log10(labels = scales::label_number()) +
  ggplot2::scale_y_log10(labels = scales::label_number()) +
  ggplot2::scale_color_manual(values = c(flagged = "#D55E00", control = "#0072B2"), name = NULL) +
  ggplot2::scale_shape_manual(values = c(flagged = 17, control = 16), name = NULL) +
  ggplot2::labs(x = "Mean annual P_ERA, monthly branch (mm/yr, log scale)",
                y = "Mean annual P_ERA, half-hourly branch (mm/yr, log scale)",
                title = "Monthly vs. half-hourly P_ERA, sampled 4x/8x-cluster and control sites",
                subtitle = sprintf("n=%d sites, seed 20260920; dashed line is 1:1, not fitted", nrow(fig_df))) +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", color = NA),
                 panel.background = ggplot2::element_rect(fill = "white", color = NA),
                 legend.position = "bottom")

out_fig <- file.path(OUTD, "fig_mm_vs_hh_p_era.png")
ggplot2::ggsave(out_fig, p, width = 8, height = 7.5, dpi = 200, bg = "white")
write_output_metadata(out_fig,
  input_sources = out_table,
  notes = "Mean annual P_ERA from the monthly branch (x) against the half-hourly branch (y), one point per sampled site, log-log axes, 1:1 reference line (not fitted). Flagged (4x/8x cluster) vs. control sites distinguished by color and shape. White background.")
message("Saved: ", out_fig)

# ============================================================================
# Verdict components (printed for report.md to quote verbatim)
# ============================================================================
message("\n================ Verdict components ================")

DISAGREEMENT_FACTOR <- 2.0  # fixed, stated threshold -- not fit to this sample

verdict_df <- fig_df |>
  dplyr::mutate(ratio_mm_to_hh = mean_mm_mm / mean_hh_mm,
                disagrees = ratio_mm_to_hh > DISAGREEMENT_FACTOR | ratio_mm_to_hh < (1 / DISAGREEMENT_FACTOR))

flagged_verdict <- verdict_df |> dplyr::filter(role == "flagged")
n_flagged_disagree <- sum(flagged_verdict$disagrees, na.rm = TRUE)
n_flagged_agree <- sum(!flagged_verdict$disagrees, na.rm = TRUE)
n_flagged_na <- sum(is.na(flagged_verdict$disagrees))

cat(sprintf("\nFixed disagreement criterion: MM-branch mean annual P_ERA differs from HH-branch by more than %gx in either direction.\n", DISAGREEMENT_FACTOR))
cat(sprintf("Sampled flagged sites: %d disagree, %d agree, %d could not be evaluated (missing HH or MM data).\n",
            n_flagged_disagree, n_flagged_agree, n_flagged_na))

cat("\nDisagreement by network (flagged sites only):\n")
print(as.data.frame(flagged_verdict |> dplyr::group_by(product_source_network) |>
  dplyr::summarise(n = dplyr::n(), n_disagree = sum(disagrees, na.rm = TRUE), .groups = "drop")))

cat("\nFull per-site detail (flagged sites):\n")
print(as.data.frame(flagged_verdict |> dplyr::select(site_id, product_source_network, mean_mm_mm, mean_hh_mm, ratio_mm_to_hh, disagrees)))

cat("\nControl sites, same criterion (sanity check -- expect near-1x, few/no disagreements):\n")
control_verdict <- verdict_df |> dplyr::filter(role == "control")
print(as.data.frame(control_verdict |> dplyr::select(site_id, product_source_network, mean_mm_mm, mean_hh_mm, ratio_mm_to_hh, disagrees)))

# verdict_df is not written to disk -- the brief specifies exactly one table
# (table_1) and one figure as OUTPUT ("Nothing else"). The ratio/disagreement
# numbers above are derived from table_1's own mean_mm_mm/mean_hh_mm columns
# and are reproducible from it directly; report.md quotes this console output
# verbatim rather than adding a third file.

message("\n=== cluster_resolution_sample_check.R complete ===")
message("See report.md for the full verdict.")
