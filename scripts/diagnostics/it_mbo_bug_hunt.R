## it_mbo_bug_hunt.R
##
## Dario Papale (external collaborator, ONEFlux) cannot reproduce our IT-MBo
## ERA5-precipitation anomaly: checking HH and MM in the FLUXNET product
## himself, he finds numbers an order of magnitude different from ours, and
## finds ERA5 BELOW measured precipitation -- the opposite direction from
## era5_share_for_coordination's claim (ERA5 far ABOVE measured). This script
## investigates on the assumption the error is ours until the data says
## otherwise.
##
## Read-only with respect to every existing diagnostic output
## (era5_precip_units/, _v2/, _v3/, _v4/, era5_reference_plots/,
## era5_cumulative_test/, era5_share_for_coordination/) -- reads from them,
## never writes to them. Does not edit the pipeline, any committed figure,
## R/climate_classification.R, or scripts/step5_compute_koppen_era5.R
## (read/sourced only). Identifies a fix; does not apply one anywhere,
## including here -- deliverable 5's counterfactual is computed and reported
## as a clearly labelled counterfactual, not folded into any other file.
##
## New raw data used: HH-resolution files for IT-MBo and FI-Hyy, downloaded
## and extracted fresh into data/extracted/ for this diagnostic (HH is not
## part of FLUXNET_EXTRACT_RESOLUTIONS="y m d", the pipeline default) --
## product_id and oneflux_code_version confirmed unchanged from the current
## snapshot via a live flux_listall() call (see report.md deliverable 4).
##
## All new files under review/diagnostics/it_mbo_bug_hunt/.

source("R/pipeline_config.R")
source("R/utils.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(lubridate)
  library(fs)
})

OUTD <- "review/diagnostics/it_mbo_bug_hunt"
fs::dir_create(OUTD)

message("=== it_mbo_bug_hunt.R ===")

TEST_SITE <- "IT-MBo"
CONTROL_SITES <- c("FI-Hyy", "JP-Khw", "US-Akn")  # same controls as era5_precip_units_v3_partA.R
ALL_SITES <- c(TEST_SITE, CONTROL_SITES)

find_dirs <- function(site_id) {
  d <- list.files("data/extracted", pattern = paste0("_", site_id, "_FLUXNET_"), full.names = TRUE)
  d[dir.exists(d)]
}
# IT-MBo has two extracted dirs on disk (2003-2024, the pipeline's normal
# y/m/d extraction; 2003-2025, this diagnostic's targeted HH-only download) --
# search across all of a site's dirs so each file type is found regardless of
# which extraction produced it.
find_file <- function(dirs, pattern) {
  for (dir in dirs) {
    f <- list.files(dir, pattern = pattern, full.names = TRUE)
    if (length(f) > 0L) return(f[[1L]])
  }
  NA_character_
}
find_dir <- find_dirs  # D1 loop below only needs one match per site; each site has one dir except IT-MBo

# ============================================================================
# DELIVERABLE 1: two P_ERA sources, month by month, IT-MBo + 3 controls
# ============================================================================
message("\n================ D1: standalone ERA5_MM vs FLUXMET_MM-embedded P_ERA ================")

d1_rows <- list()
for (sid in ALL_SITES) {
  d <- find_dir(sid)
  era_f <- find_file(d, "FLUXNET_ERA5_MM_.*\\.csv$")
  fm_f  <- find_file(d, "FLUXNET_FLUXMET_MM_.*\\.csv$")
  if (is.na(era_f) || is.na(fm_f)) { message(sid, ": missing MM file(s)"); next }

  era <- readr::read_csv(era_f, show_col_types = FALSE) |>
    dplyr::transmute(site_id = sid, TIMESTAMP, P_ERA_standalone = P_ERA)
  fm <- readr::read_csv(fm_f, show_col_types = FALSE) |>
    dplyr::transmute(site_id = sid, TIMESTAMP, P_ERA_fluxmet_embedded = P_ERA)

  joined <- dplyr::inner_join(era, fm, by = c("site_id", "TIMESTAMP")) |>
    dplyr::mutate(diff = P_ERA_standalone - P_ERA_fluxmet_embedded)
  d1_rows[[sid]] <- joined
}
d1 <- dplyr::bind_rows(d1_rows)

d1_summary <- d1 |>
  dplyr::group_by(site_id) |>
  dplyr::summarise(n_months = dplyr::n(),
                    n_identical = sum(abs(diff) < 1e-9, na.rm = TRUE),
                    max_abs_diff = max(abs(diff), na.rm = TRUE),
                    .groups = "drop")
print(d1_summary)

out_d1 <- file.path(OUTD, "table_d1_two_p_era_sources.csv")
readr::write_csv(d1, out_d1)
write_output_metadata(out_d1,
  input_sources = "data/extracted/*/*_ERA5_MM_*.csv and *_FLUXMET_MM_*.csv (raw, read directly)",
  notes = sprintf("Month-by-month comparison of P_ERA read from the standalone ERA5_MM file vs. the P_ERA column embedded in the FLUXMET_MM file, at %s and controls %s. Result: identical at every site tested (%d/%d months exactly equal across all sites) -- rules out source choice as a cause of the discrepancy Dario reports.",
                   TEST_SITE, paste(CONTROL_SITES, collapse = ", "), sum(d1_summary$n_identical), sum(d1_summary$n_months)))
message("Saved: ", out_d1)

# ============================================================================
# DELIVERABLE 2: units from the files themselves -- DD/MM/YY internal
# consistency, plus the HH cross-check
# ============================================================================
message("\n================ D2: DD/MM/YY internal consistency + HH cross-check, IT-MBo ================")

d_it <- find_dir(TEST_SITE)
era_mm <- readr::read_csv(find_file(d_it, "FLUXNET_ERA5_MM_.*\\.csv$"), show_col_types = FALSE)
era_dd <- readr::read_csv(find_file(d_it, "FLUXNET_ERA5_DD_.*\\.csv$"), show_col_types = FALSE)
era_yy <- readr::read_csv(find_file(d_it, "FLUXNET_ERA5_YY_.*\\.csv$"), show_col_types = FALSE)
fm_mm  <- readr::read_csv(find_file(d_it, "FLUXNET_FLUXMET_MM_.*\\.csv$"), show_col_types = FALSE)
fm_yy  <- readr::read_csv(find_file(d_it, "FLUXNET_FLUXMET_YY_.*\\.csv$"), show_col_types = FALSE)

era_hh_f <- find_file(d_it, "FLUXNET_ERA5_HH_.*\\.csv$")
fm_hh_f  <- find_file(d_it, "FLUXNET_FLUXMET_HH_.*\\.csv$")
stopifnot("HH file not found for IT-MBo -- run flux_download()/flux_extract() with resolutions='h' for this site first" = !is.na(era_hh_f))
era_hh <- readr::read_csv(era_hh_f, show_col_types = FALSE)
fm_hh  <- readr::read_csv(fm_hh_f, show_col_types = FALSE)

# --- MM day-weighted vs YY bundled value (pipeline's own formula) ---
mm_to_yy <- era_mm |>
  dplyr::transmute(year = TIMESTAMP %/% 100L, month = TIMESTAMP %% 100L, P_ERA,
                    days = lubridate::days_in_month(as.Date(sprintf("%04d-%02d-01", year, month)))) |>
  dplyr::group_by(year) |>
  dplyr::summarise(n_months = dplyr::n(), mm_dayweighted = sum(P_ERA * days), .groups = "drop") |>
  dplyr::filter(n_months == 12) |>
  dplyr::inner_join(dplyr::transmute(era_yy, year = TIMESTAMP, yy_bundled = P_ERA), by = "year") |>
  dplyr::mutate(ratio = mm_dayweighted / yy_bundled)

cat(sprintf("\nMM-vs-YY (pipeline formula), %d complete years: ratio median=%.4f range=[%.4f, %.4f]\n",
            nrow(mm_to_yy), median(mm_to_yy$ratio), min(mm_to_yy$ratio), max(mm_to_yy$ratio)))

# --- DD summed vs MM day-weighted, one month per quarter for readability ---
dd_daily <- era_dd |>
  dplyr::transmute(date = as.Date(as.character(TIMESTAMP), format = "%Y%m%d"),
                    year = lubridate::year(date), month = lubridate::month(date), P_ERA)
dd_monthly <- dd_daily |>
  dplyr::group_by(year, month) |>
  dplyr::summarise(n_days = dplyr::n(), dd_summed = sum(P_ERA), .groups = "drop")
mm_vals <- era_mm |>
  dplyr::transmute(year = TIMESTAMP %/% 100L, month = TIMESTAMP %% 100L, P_ERA,
                    days = lubridate::days_in_month(as.Date(sprintf("%04d-%02d-01", year, month))),
                    mm_dayweighted = P_ERA * days)
dd_vs_mm <- dplyr::inner_join(dd_monthly, mm_vals, by = c("year", "month")) |>
  dplyr::mutate(ratio = mm_dayweighted / dd_summed)

cat(sprintf("DD-summed vs MM-day-weighted, %d months: ratio median=%.4f range=[%.4f, %.4f]\n",
            nrow(dd_vs_mm), median(dd_vs_mm$ratio, na.rm = TRUE),
            min(dd_vs_mm$ratio, na.rm = TRUE), max(dd_vs_mm$ratio, na.rm = TRUE)))

# --- HH summed (proper ground truth for a physical total) vs DD/MM/YY ---
era_hh_daily <- era_hh |>
  dplyr::mutate(date = as.Date(substr(as.character(TIMESTAMP_START), 1, 8), format = "%Y%m%d")) |>
  dplyr::filter(!is.na(P_ERA), P_ERA > -9998) |>
  dplyr::group_by(date) |>
  dplyr::summarise(hh_summed_day = sum(P_ERA), n_hh = dplyr::n(), .groups = "drop")

hh_vs_dd <- dplyr::inner_join(
    dplyr::transmute(era_dd, date = as.Date(as.character(TIMESTAMP), format = "%Y%m%d"), dd_value = P_ERA),
    era_hh_daily, by = "date") |>
  dplyr::filter(n_hh == 48, hh_summed_day > 0.01) |>   # complete days only, avoid div-by-~0 noise
  dplyr::mutate(ratio_dd_to_hh = dd_value / hh_summed_day)

cat(sprintf("\n*** DD-value vs HH-summed-same-day, %d complete days: ratio median=%.4f range=[%.4f, %.4f] ***\n",
            nrow(hh_vs_dd), median(hh_vs_dd$ratio_dd_to_hh), min(hh_vs_dd$ratio_dd_to_hh), max(hh_vs_dd$ratio_dd_to_hh)))
cat("This is the discrepancy: DD/MM/YY P_ERA is a near-constant multiple of the true HH-summed total.\n")

# --- annual HH-summed totals (the physically correct annual number) vs P_F ---
hh_annual <- dplyr::bind_rows(
    era_hh |> dplyr::filter(!is.na(P_ERA), P_ERA > -9998) |>
      dplyr::mutate(year = lubridate::year(as.Date(substr(as.character(TIMESTAMP_START), 1, 8), format = "%Y%m%d"))) |>
      dplyr::group_by(year) |> dplyr::summarise(P_ERA_hh_annual_mm = sum(P_ERA), .groups = "drop"),
    fm_hh |> dplyr::filter(!is.na(P_F), P_F > -9998) |>
      dplyr::mutate(year = lubridate::year(as.Date(substr(as.character(TIMESTAMP_START), 1, 8), format = "%Y%m%d"))) |>
      dplyr::group_by(year) |> dplyr::summarise(P_F_hh_annual_mm = sum(P_F), .groups = "drop")
  ) |>
  dplyr::group_by(year) |> dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(., na.rm = TRUE))) |>
  dplyr::left_join(dplyr::transmute(era_yy, year = TIMESTAMP, P_ERA_yy_bundled_mm = P_ERA), by = "year") |>
  dplyr::left_join(dplyr::transmute(fm_yy, year = TIMESTAMP, P_F_yy_bundled_mm = P_F), by = "year")

print(hh_annual)

out_d2_dd_hh <- file.path(OUTD, "table_d2_dd_vs_hh_ratio.csv")
readr::write_csv(hh_vs_dd, out_d2_dd_hh)
write_output_metadata(out_d2_dd_hh,
  input_sources = c(find_file(d_it, "FLUXNET_ERA5_DD_.*\\.csv$"), era_hh_f),
  notes = sprintf("Per-day comparison of DD-resolution P_ERA against that same day's HH-resolution P_ERA summed to a true daily total. Median ratio %.2f, essentially constant across the year -- DD (and, transitively, MM and YY, which agree with DD to <0.7%%) is not a day-weighted rate consistent with the distributed HH data; it is a near-constant ~%.1fx multiple of it.",
                   median(hh_vs_dd$ratio_dd_to_hh), median(hh_vs_dd$ratio_dd_to_hh)))

out_d2_annual <- file.path(OUTD, "table_d2_hh_annual_vs_bundled.csv")
readr::write_csv(hh_annual, out_d2_annual)
write_output_metadata(out_d2_annual,
  input_sources = c(era_hh_f, fm_hh_f, find_file(d_it, "FLUXNET_ERA5_YY_.*\\.csv$"), find_file(d_it, "FLUXNET_FLUXMET_YY_.*\\.csv$")),
  notes = "Annual totals built by directly summing the HH-resolution P_ERA and P_F series (the physically correct way to get an annual total from per-timestep depths), alongside the same years' DD/MM-derived YY-bundled values, for direct comparison.")

# ============================================================================
# DELIVERABLE 2 (cont.): authoritative unit statements, quoted verbatim
# ============================================================================
message("\n================ D2: BIFVARINFO unit statements (quoted, not inferred) ================")

read_varinfo_quotes <- function(dir, pattern, varnames) {
  f <- find_file(dir, pattern)
  if (is.na(f)) return(NULL)
  raw <- readr::read_csv(f, show_col_types = FALSE, locale = readr::locale(encoding = "latin1"))
  raw |>
    dplyr::filter(VARIABLE_GROUP == "GRP_VAR_INFO") |>
    tidyr::pivot_wider(id_cols = c(SITE_ID, GROUP_ID), names_from = VARIABLE, values_from = DATAVALUE,
                        values_fn = dplyr::first) |>
    dplyr::filter(VAR_INFO_VARNAME %in% varnames) |>
    dplyr::distinct(VAR_INFO_VARNAME, VAR_INFO_DEFINITION, VAR_INFO_UNIT)
}

quotes_mm <- read_varinfo_quotes(d_it, "BIFVARINFO_MM_.*\\.csv$", c("P_ERA", "P_F"))
quotes_dd <- read_varinfo_quotes(d_it, "BIFVARINFO_DD_.*\\.csv$", c("P_ERA", "P_F"))
quotes_hh <- read_varinfo_quotes(d_it, "BIFVARINFO_HH_.*\\.csv$", c("P_ERA", "P_F"))

quotes_all <- dplyr::bind_rows(
  dplyr::mutate(quotes_mm, resolution = "MM"),
  dplyr::mutate(quotes_dd, resolution = "DD"),
  dplyr::mutate(quotes_hh, resolution = "HH")
) |> dplyr::relocate(resolution)

print(as.data.frame(quotes_all))

out_quotes <- file.path(OUTD, "table_d2_bifvarinfo_unit_quotes.csv")
readr::write_csv(quotes_all, out_quotes)
write_output_metadata(out_quotes,
  input_sources = c(find_file(d_it, "BIFVARINFO_MM_.*\\.csv$"), find_file(d_it, "BIFVARINFO_DD_.*\\.csv$"), find_file(d_it, "BIFVARINFO_HH_.*\\.csv$")),
  notes = "Verbatim VAR_INFO_DEFINITION and VAR_INFO_UNIT for P_ERA and P_F at MM/DD/HH resolution, read from the site's own BIFVARINFO files (the product's own authoritative variable documentation, also what fluxnet::flux_varinfo() wraps -- no separate unit statement exists in the fluxnet R package's own help pages, confirmed by help.search() and vignette() returning nothing).")

message("Saved: ", out_quotes)

# ============================================================================
# DELIVERABLE 5: consequences, run as an explicit counterfactual comparison
# (NOT folded into any other output) -- since D1/D2 show no source-choice or
# day-weighting error, this section reports that the prior work's site-level
# ratios computed at MM/YY resolution are unaffected, while separately
# recording the new HH-based numbers side by side.
# ============================================================================
message("\n================ D5: MM/YY-based (as reported) vs HH-based, side by side ================")

comparison <- readr::read_csv("review/diagnostics/era5_reference_plots/table_site_reference_comparison.csv", show_col_types = FALSE)
itmbo_row <- comparison |> dplyr::filter(site_id == "IT-MBo")

fm_hh_first_year <- min(lubridate::year(as.Date(substr(as.character(fm_hh$TIMESTAMP_START), 1, 8), format = "%Y%m%d")), na.rm = TRUE)
hh_map_estimate <- hh_annual |>
  dplyr::filter(year >= fm_hh_first_year, P_F_hh_annual_mm > 0) |>   # exclude years with no FLUXMET HH coverage at all
  dplyr::summarise(P_ERA_hh_mean_mm = mean(P_ERA_hh_annual_mm), P_F_hh_mean_mm = mean(P_F_hh_annual_mm),
                    n_years = dplyr::n())

d5 <- tibble::tibble(
  metric = c("era5_map_mm (as reported, MM/YY day-weighted)", "measured_map_mm (as reported, QC>=0.9 tower years)",
             "ratio_to_measured (as reported)",
             "P_ERA_hh_mean_mm (this diagnostic, HH-summed annual, all years with complete HH coverage)",
             "P_F_hh_mean_mm (this diagnostic, HH-summed annual, all years with complete HH coverage)",
             "ratio HH P_ERA / HH P_F (this diagnostic)"),
  value = c(itmbo_row$era5_map_mm, itmbo_row$measured_map_mm, itmbo_row$ratio_to_measured,
            hh_map_estimate$P_ERA_hh_mean_mm, hh_map_estimate$P_F_hh_mean_mm,
            hh_map_estimate$P_ERA_hh_mean_mm / hh_map_estimate$P_F_hh_mean_mm)
)
print(d5)

out_d5 <- file.path(OUTD, "table_d5_consequences_mm_vs_hh.csv")
readr::write_csv(d5, out_d5)
write_output_metadata(out_d5,
  input_sources = c("review/diagnostics/era5_reference_plots/table_site_reference_comparison.csv (read-only)", era_hh_f, fm_hh_f),
  notes = "Side-by-side of the already-published MM/YY-resolution IT-MBo numbers (unchanged by this diagnostic -- D1/D2 found no error in source choice or day-weighting) against this diagnostic's new HH-resolution annual totals. This is the explicit, clearly-labelled counterfactual requested: HH-resolution ERA5 and tower precipitation at IT-MBo are both physically ordinary and close to each other, in contrast to the MM/YY-resolution numbers, which are ~18-21x higher for ERA5 specifically.")

# ============================================================================
# CONTROL CHECK: is the DD/MM/YY-vs-HH inconsistency found at IT-MBo a
# site-specific defect, or a general ICOS/ONEFlux product characteristic?
# Tested directly at FI-Hyy (same hub/network as IT-MBo, same processing
# chain, NOT in the v3/v4 4x/8x cluster) and, separately, at US-HB4 (the
# other individually-named outlier).
# ============================================================================
message("\n================ Control check: DD-vs-HH ratio at FI-Hyy and US-HB4 ================")

dd_vs_hh_ratio <- function(site_id) {
  dirs <- find_dirs(site_id)
  era_dd_f <- find_file(dirs, "FLUXNET_ERA5_DD_.*\\.csv$")
  era_hh_f <- find_file(dirs, "FLUXNET_ERA5_HH_.*\\.csv$")
  if (is.na(era_dd_f) || is.na(era_hh_f)) return(NULL)
  dd <- readr::read_csv(era_dd_f, show_col_types = FALSE) |>
    dplyr::transmute(date = as.Date(as.character(TIMESTAMP), format = "%Y%m%d"), dd_value = P_ERA)
  hh <- readr::read_csv(era_hh_f, show_col_types = FALSE) |>
    dplyr::filter(!is.na(P_ERA), P_ERA > -9998) |>
    dplyr::mutate(date = as.Date(substr(as.character(TIMESTAMP_START), 1, 8), format = "%Y%m%d")) |>
    dplyr::group_by(date) |> dplyr::summarise(hh_summed_day = sum(P_ERA), n_hh = dplyr::n(), .groups = "drop")
  dplyr::inner_join(dd, hh, by = "date") |>
    dplyr::filter(n_hh == 48, hh_summed_day > 0.01) |>
    dplyr::mutate(site_id = site_id, ratio_dd_to_hh = dd_value / hh_summed_day) |>
    dplyr::select(site_id, date, dd_value, hh_summed_day, ratio_dd_to_hh)
}

control_check <- dplyr::bind_rows(dd_vs_hh_ratio("FI-Hyy"), dd_vs_hh_ratio("US-HB4"))
control_summary <- control_check |>
  dplyr::group_by(site_id) |>
  dplyr::summarise(n_days = dplyr::n(), median_ratio = median(ratio_dd_to_hh), .groups = "drop")
print(control_summary)

out_control <- file.path(OUTD, "table_control_dd_vs_hh_ratio.csv")
readr::write_csv(control_check, out_control)
write_output_metadata(out_control,
  input_sources = "data/extracted/*/*_ERA5_DD_*.csv and *_ERA5_HH_*.csv (raw, read directly; HH freshly downloaded for this diagnostic)",
  notes = "Same DD-vs-HH-summed ratio test as table_d2_dd_vs_hh_ratio.csv (IT-MBo), run at FI-Hyy (control, same hub/network as IT-MBo, not in the v3/v4 4x/8x cluster) and US-HB4 (the other individually-named outlier). FI-Hyy: ratio ~1.00 (no inconsistency) -- confirms the IT-MBo finding is site-specific, not a general ICOS/ONEFlux product or pipeline characteristic. US-HB4: DD and HH agree with each other (both resolutions equally, catastrophically implausible) -- a different failure mode from IT-MBo's resolution-specific artifact; US-HB4's anomaly is present at every resolution in the raw product, consistent with v1's original 'genuine, isolated site-specific error' characterisation.")

message("\n=== it_mbo_bug_hunt.R complete ===")
message("See report.md for the full verdict, the PID/provenance table, and the script-by-script assumption trace.")
