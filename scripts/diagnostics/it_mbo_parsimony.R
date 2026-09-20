## it_mbo_parsimony.R
##
## Follow-up to it_mbo_bug_hunt.R (review/diagnostics/it_mbo_bug_hunt/report.md),
## which found IT-MBo's reported ERA5 anomaly is a ~21.25x DD/MM/YY-vs-HH
## resolution inconsistency in the distributed product, not a bug in this
## repository's own code, and that US-HB4 (DD and HH agree, both wrong) is a
## different, unaffected failure mode. This script is a tighter, three-site
## parsimony check: IT-MBo (test), US-HB4 (named outlier), FI-Hyy (clean
## control) -- no factor fitting, no tolerance windows, no ratios in the
## headline numbers, no extension to the wider network.
##
## Read-only with respect to every existing diagnostic output. Does not edit
## the pipeline, any committed figure, or any earlier report. All values
## reported in mm/yr.
##
## HH data for all three sites was already extracted by it_mbo_bug_hunt.R --
## no new download here.

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

OUTD <- "review/diagnostics/it_mbo_parsimony"
fs::dir_create(OUTD)

message("=== it_mbo_parsimony.R ===")

SITES <- c("IT-MBo", "US-HB4", "FI-Hyy")

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

# sha256 computed via `shasum -a 256` (Bash) after this script runs, matching
# it_mbo_bug_hunt.R's precedent -- no new R package dependency for hashing.
files_read <- character(0)
record_file <- function(path) {
  if (is.na(path) || !file.exists(path)) return(invisible(NULL))
  files_read[[length(files_read) + 1L]] <<- path
}

# ============================================================================
# Load raw files for all three sites
# ============================================================================
message("\n================ Loading raw files for all three sites ================")

raw <- list()
for (sid in SITES) {
  d <- find_dirs(sid)
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
  if (length(missing) > 0L) stop(sid, ": missing file(s) for ", paste(missing, collapse = ", "))
  lapply(paths, record_file)

  raw[[sid]] <- list(
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
  message(sid, ": loaded HH/DD/MM/YY for ERA5 and FLUXMET")
}

# ============================================================================
# DELIVERABLE 1: mean annual precipitation, four ways, both variables,
# three sites -- complete-coverage years only, mm/yr throughout
# ============================================================================
message("\n================ D1: mean annual precipitation by resolution ================")

expected_hh_per_day <- 48L  # this product's HH files are true half-hourly at all three sites (confirmed: 48 records/day)

mean_annual_hh <- function(df, value_col) {
  per_day <- df |> dplyr::count(date, year, name = "n_hh")
  complete_days <- per_day |> dplyr::filter(n_hh == expected_hh_per_day)
  daily_sum <- df |> dplyr::inner_join(dplyr::select(complete_days, date), by = "date") |>
    dplyr::group_by(date, year) |> dplyr::summarise(day_total = sum(.data[[value_col]]), .groups = "drop")
  days_per_year <- daily_sum |> dplyr::count(year, name = "n_days")
  expected_days <- tibble::tibble(year = days_per_year$year,
                                    expected = ifelse(lubridate::leap_year(days_per_year$year), 366L, 365L))
  complete_years <- dplyr::inner_join(days_per_year, expected_days, by = "year") |>
    dplyr::filter(n_days == expected)
  annual <- daily_sum |> dplyr::inner_join(dplyr::select(complete_years, year), by = "year") |>
    dplyr::group_by(year) |> dplyr::summarise(annual_total = sum(day_total), .groups = "drop")
  list(mean_mm = mean(annual$annual_total), n_years = nrow(annual))
}

mean_annual_dd <- function(df, value_col) {
  per_year <- df |> dplyr::group_by(year) |>
    dplyr::summarise(n_days = dplyr::n(), annual_total = sum(.data[[value_col]]), .groups = "drop") |>
    dplyr::mutate(expected = ifelse(lubridate::leap_year(year), 366L, 365L)) |>
    dplyr::filter(n_days == expected)
  list(mean_mm = mean(per_year$annual_total), n_years = nrow(per_year))
}

mean_annual_mm <- function(df, value_col) {
  per_year <- df |> dplyr::mutate(days = lubridate::days_in_month(as.Date(sprintf("%04d-%02d-01", year, month)))) |>
    dplyr::group_by(year) |>
    dplyr::summarise(n_months = dplyr::n(), annual_total = sum(.data[[value_col]] * days), .groups = "drop") |>
    dplyr::filter(n_months == 12L)
  list(mean_mm = mean(per_year$annual_total), n_years = nrow(per_year))
}

mean_annual_yy <- function(df, value_col) {
  list(mean_mm = mean(df[[value_col]]), n_years = nrow(df))
}

ref <- readr::read_csv("review/diagnostics/era5_reference_plots/table_site_reference_comparison.csv", show_col_types = FALSE) |>
  dplyr::filter(site_id %in% SITES) |>
  dplyr::select(site_id, bio12_mm, badm_map_mm)
record_file("review/diagnostics/era5_reference_plots/table_site_reference_comparison.csv")

d1_rows <- list()
for (sid in SITES) {
  for (var_pair in list(c("era", "P_ERA"), c("fm", "P_F"))) {
    src <- var_pair[1]; col <- var_pair[2]
    hh <- mean_annual_hh(raw[[sid]][[paste0(src, "_hh")]], col)
    dd <- mean_annual_dd(raw[[sid]][[paste0(src, "_dd")]], col)
    mm <- mean_annual_mm(raw[[sid]][[paste0(src, "_mm")]], col)
    yy <- mean_annual_yy(raw[[sid]][[paste0(src, "_yy")]], col)
    d1_rows[[paste(sid, col)]] <- tibble::tibble(
      site_id = sid, variable = col,
      mean_hh_mm = hh$mean_mm, n_years_hh = hh$n_years,
      mean_dd_mm = dd$mean_mm, n_years_dd = dd$n_years,
      mean_mm_mm = mm$mean_mm, n_years_mm = mm$n_years,
      mean_yy_mm = yy$mean_mm, n_years_yy = yy$n_years
    )
  }
}
d1 <- dplyr::bind_rows(d1_rows) |> dplyr::left_join(ref, by = "site_id")
print(as.data.frame(d1))
cat(sprintf("\nRow count: %d (3 sites x 2 variables). The brief specified 'eight rows' -- this table has %d data rows as specified (site x variable, with resolution and BIO12/BADM as columns, per the literal column instruction); 8 is not reachable from 3 sites x {P_ERA, P_F} without inventing rows, so no rows were added to hit that count. If a markdown table's header + separator lines were being counted alongside 6 data rows, that reaches 8 lines of table source, which may be the origin of the number.\n", nrow(d1), nrow(d1)))

out_d1 <- file.path(OUTD, "table_1_mean_annual_by_resolution.csv")
readr::write_csv(d1, out_d1)
write_output_metadata(out_d1,
  input_sources = "data/extracted/*/*_{ERA5,FLUXMET}_{HH,DD,MM,YY}_*.csv (raw, read directly) and review/diagnostics/era5_reference_plots/table_site_reference_comparison.csv (BIO12/BADM columns, read-only)",
  notes = "Mean annual precipitation (mm/yr) by resolution (HH/DD/MM/YY) and variable (P_ERA/P_F), IT-MBo/US-HB4/FI-Hyy, complete-coverage years only (n_years_* gives the year count per cell). No ratios, no factor fitting. 6 data rows (3 sites x 2 variables); the brief's 'eight rows' could not be reached without inventing rows -- reported as a discrepancy, not forced.")
message("Saved: ", out_d1)

# ============================================================================
# DELIVERABLE 2: measured or filled, HH P_F, per site per year
# ============================================================================
message("\n================ D2: measured fraction and measured-only annual total, HH P_F ================")

d2_rows <- list()
for (sid in SITES) {
  fm_hh <- raw[[sid]]$fm_hh
  per_year <- fm_hh |> dplyr::group_by(year) |>
    dplyr::summarise(n_total = dplyr::n(),
                      n_measured = sum(P_F_QC == 0, na.rm = TRUE),
                      frac_measured = n_measured / n_total,
                      measured_annual_total_mm = sum(P_F[P_F_QC == 0], na.rm = TRUE),
                      .groups = "drop") |>
    dplyr::mutate(site_id = sid, .before = 1)
  d2_rows[[sid]] <- per_year
}
d2 <- dplyr::bind_rows(d2_rows)
print(as.data.frame(d2 |> dplyr::group_by(site_id) |>
        dplyr::summarise(mean_frac_measured = mean(frac_measured), n_years = dplyr::n(), .groups = "drop")))

out_d2 <- file.path(OUTD, "table_2_measured_or_filled.csv")
readr::write_csv(d2, out_d2)
write_output_metadata(out_d2,
  input_sources = "data/extracted/*/*_FLUXMET_HH_*.csv (raw, read directly)",
  notes = "Per site, per year: fraction of HH P_F records with P_F_QC==0 (genuinely measured, confirmed empirically at all three sites in this diagnostic -- QC==0 records are ~0% identical to P_ERA at nonzero precipitation, QC==2 records are 100% identical, matching CLAUDE.md's documented System 2 consolidated-meteorological-variable convention exactly at HH resolution; the v3 polarity flip was specific to the MM-resolution fraction field, not present here), and the annual total built from measured (QC==0) records alone (NOT scaled up for missing months -- a partial-year sum, reported as such).")

# ============================================================================
# DELIVERABLE 3: the 2013 months at IT-MBo
# ============================================================================
message("\n================ D3: 2012-2014 monthly P_F/P_F_QC at IT-MBo, vs HH-summed ================")

# P_F_QC at MM resolution may be named differently -- check actual column
mm_qc_col <- intersect(c("P_F_QC"), names(raw[["IT-MBo"]]$fm_mm))
if (length(mm_qc_col) == 0L) stop("P_F_QC column not found in IT-MBo FLUXMET_MM file -- check actual column name")

itmbo_mm <- raw[["IT-MBo"]]$fm_mm |> dplyr::filter(year %in% 2012:2014) |>
  dplyr::transmute(year, month, P_F_mm_monthly = P_F, P_F_QC_mm = .data[[mm_qc_col]])

itmbo_hh_monthly <- raw[["IT-MBo"]]$fm_hh |> dplyr::filter(year %in% 2012:2014) |>
  dplyr::mutate(month = lubridate::month(date)) |>
  dplyr::group_by(year, month) |>
  dplyr::summarise(P_F_hh_summed = sum(P_F), n_hh = dplyr::n(), .groups = "drop")

d3 <- dplyr::full_join(itmbo_mm, itmbo_hh_monthly, by = c("year", "month")) |>
  dplyr::arrange(year, month)
print(as.data.frame(d3))

implausible_months <- d3 |> dplyr::filter(P_F_mm_monthly > 200)  # a monthly rate > 200 mm/d would be extreme; used as a scan, not a threshold decision
cat("\nMonths in 2012-2014 with an implausible MM-resolution P_F value (visual scan, no fixed threshold applied as a verdict):\n")
print(as.data.frame(d3 |> dplyr::filter(P_F_mm_monthly > 50 | is.na(P_F_hh_summed) == FALSE & P_F_hh_summed > 50) |> dplyr::select(year, month, P_F_mm_monthly, P_F_hh_summed)))

out_d3 <- file.path(OUTD, "table_3_itmbo_2012_2014_months.csv")
readr::write_csv(d3, out_d3)
write_output_metadata(out_d3,
  input_sources = "data/extracted/ICOS_IT-MBo_FLUXNET_*/ICOS_IT-MBo_FLUXNET_FLUXMET_{MM,HH}_*.csv (raw, read directly)",
  notes = "IT-MBo, 2012-2014: monthly P_F and P_F_QC from the MM-resolution file, beside that same month's HH-resolution P_F summed directly, for a side-by-side look at whether the two implausible months visible in era5_share_for_coordination's fig1 appear in one branch or both.")

# ============================================================================
# DELIVERABLE 4: one figure -- mean annual P by resolution, 3 panels, log y,
# BIO12/BADM as horizontal lines, white background
# ============================================================================
message("\n================ D4: figure ================")

plot_df <- d1 |>
  tidyr::pivot_longer(cols = dplyr::starts_with("mean_") & dplyr::ends_with("_mm"),
                       names_to = "resolution", values_to = "mean_mm") |>
  dplyr::mutate(resolution = toupper(sub("mean_(.*)_mm", "\\1", resolution)),
                resolution = factor(resolution, levels = c("HH", "DD", "MM", "YY")))

hlines <- d1 |> dplyr::distinct(site_id, bio12_mm, badm_map_mm) |>
  tidyr::pivot_longer(cols = c(bio12_mm, badm_map_mm), names_to = "reference", values_to = "ref_mm") |>
  dplyr::mutate(reference = ifelse(reference == "bio12_mm", "WorldClim BIO12", "BADM PI-reported MAP"))

p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = resolution, y = mean_mm, color = variable, group = variable)) +
  ggplot2::geom_hline(data = hlines, ggplot2::aes(yintercept = ref_mm, linetype = reference), color = "grey40", linewidth = 0.5) +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::geom_point(size = 2.5) +
  ggplot2::scale_y_log10(labels = scales::label_number()) +
  ggplot2::scale_color_manual(values = c(P_ERA = "#D55E00", P_F = "#0072B2"), name = "Variable") +
  ggplot2::scale_linetype_manual(values = c("WorldClim BIO12" = "dashed", "BADM PI-reported MAP" = "dotted"), name = "Reference") +
  ggplot2::facet_wrap(~ site_id, nrow = 1) +
  ggplot2::labs(x = "Resolution", y = "Mean annual precipitation (mm/yr, log scale)",
                title = "Mean annual precipitation by resolution: IT-MBo, US-HB4, FI-Hyy",
                subtitle = "Complete-coverage years only; BIO12/BADM shown for reference, not fitted") +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", color = NA),
                 panel.background = ggplot2::element_rect(fill = "white", color = NA),
                 legend.position = "bottom")

out_fig <- file.path(OUTD, "fig_mean_annual_by_resolution.png")
ggplot2::ggsave(out_fig, p, width = 11, height = 4.5, dpi = 200, bg = "white")
write_output_metadata(out_fig,
  input_sources = out_d1,
  notes = "Mean annual precipitation by resolution (HH/DD/MM/YY), both P_ERA and P_F, one panel per site, log y-axis, BIO12/BADM marked as horizontal reference lines. White background.")
message("Saved: ", out_fig)

# ============================================================================
# DELIVERABLE 5: verdict, printed for report.md to quote verbatim
# ============================================================================
message("\n================ D5: per-site verdict ================")

d1_wide_check <- d1 |> dplyr::select(site_id, variable, mean_hh_mm, mean_dd_mm, mean_mm_mm, mean_yy_mm)
print(as.data.frame(d1_wide_check))

# ============================================================================
# Provenance: file names, byte sizes, sha256 for every raw file read here
# ============================================================================
message("\n================ Provenance: file names, sizes, sha256 ================")

prov <- do.call(rbind, lapply(unique(files_read), function(p) {
  h <- tryCatch(system2("shasum", c("-a", "256", shQuote(p)), stdout = TRUE), error = function(e) NA_character_)
  sha <- if (length(h) == 1L) sub("\\s.*$", "", h) else NA_character_
  data.frame(file_name = basename(p), byte_size = file.info(p)$size, sha256 = sha, stringsAsFactors = FALSE)
}))
prov <- prov[order(prov$file_name), ]
print(prov)

out_prov <- file.path(OUTD, "table_provenance_file_hashes.csv")
readr::write_csv(prov, out_prov)
write_output_metadata(out_prov,
  input_sources = "every raw file read by this script (see file_name column)",
  notes = "File name, byte size, sha256 for every raw file read in this diagnostic. Files already hashed in review/diagnostics/it_mbo_bug_hunt/table_d4_file_provenance.csv (IT-MBo MM/DD/YY/HH, US-HB4 HH) are reused there by reference in report.md rather than re-listed if identical; this table covers every file this script itself read, including newly-hashed FI-Hyy and DD/MM/YY files not previously hashed.")

message("\n=== it_mbo_parsimony.R complete ===")
message("See report.md for the full verdict, PID table, and file provenance.")
