## 12_screen_variants.R — Stage 2 screen-variant check, read-and-report, on
## the three stage 2 test sites (US-Fuf, US-Ho2, US-MMS). Stage 2 itself
## stays on hold -- this script does not rerun 07_apply_screens.R and does
## not overwrite any stage 2 table.
##
## Question: how many valid days per kept site-year survive the Zhou et al.
## (2015) screens when (i) the rain source and (ii) the radiation condition
## in screen c change.
##
## GATE (item 2 of the brief): before anything else, run_zhou_screens()
## (code/zhou_screens.R, the function 07_apply_screens.R itself now calls)
## with rain from P_ERA, screen c's radiation test from NETRAD_filled, and
## PET pressure FIXED at 101.3 kPa must reproduce the already-committed
## tables/screen_attrition.csv EXACTLY for these three sites -- confirming
## the 07 refactor changed nothing. Stops if it does not. The fixed-101.3
## pressure (not the daily PA_F now in rain_rule.R) is deliberate: it
## matches the PET formula screen_attrition.csv was actually generated
## under (07 has not been re-run since the PA_F change -- "do not rerun 07").
##
## The four variants below (item 3) instead use PET with the daily PA_F --
## "PET using the daily PA_F now in rain_rule.R" -- crossed with:
##   rain source:    P_ERA > 0           | P_F > 0 (as distributed: gauge
##                                          where measured, P_ERA fill where not)
##   screen c input: NETRAD_filled >= 0  | SW_IN_F >= 0
## Everything else (quality screen, daylight window, the 10% GPP day test)
## stays exactly as coded. PET itself always uses NETRAD_filled regardless
## of which radiation column screen c is tested against (see zhou_screens.R).
##
## Output (tables/screen_variants/, git-tracked, each with .meta.json):
##   attrition_by_variant.csv        site, kept year, variant: days removed
##                                   by rain, lost to quality, lost to screen
##                                   c, lost to record count, lost to GPP
##                                   test, valid days
##   valid_days_by_month.csv         valid days per site, variant, month
##                                   (kept years pooled)
##   records_in_window_by_month.csv  per site/month: median records per day
##                                   in [05:00,21:00] with NETRAD_filled>=0,
##                                   and with SW_IN_F>=0, before any other
##                                   screen
##   gauge_share.csv                 per site: share of days in kept years
##                                   where P_F is fully gauge-measured
## Output (docs/): report_screen_variants_<date>.md

source("WUE/isotope_pilot/code/00_config.R")
source("WUE/isotope_pilot/code/rain_rule.R")
source("WUE/isotope_pilot/code/zhou_screens.R")

TEST_SITES <- c("US-Fuf", "US-Ho2", "US-MMS")

processed_dir <- file.path(WUE_ROOT, "data", "processed")
augmented_dir <- file.path(processed_dir, "wue_augmented")
tables_dir    <- file.path(WUE_ROOT, "tables")
out_tables    <- file.path(tables_dir, "screen_variants")
docs_dir      <- file.path(WUE_ROOT, "docs")
dir.create(out_tables, recursive = TRUE, showWarnings = FALSE)

years_dropped <- readr::read_csv(file.path(tables_dir, "years_dropped.csv"), show_col_types = FALSE)

load_site <- function(site) {
  p <- file.path(augmented_dir, paste0(site, ".rds"))
  if (!file.exists(p)) stop("[WUE] ", site, ": ", p, " not found -- run 06_build_site_years.R for this site first.")
  d <- readRDS(p)
  d$site_id <- site
  d
}
raw_data <- setNames(lapply(TEST_SITES, load_site), TEST_SITES)

write_meta <- function(output_path, notes = "") {
  meta <- list(
    run_datetime_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    pipeline_version = tryCatch(system("git rev-parse --short HEAD", intern = TRUE), error = function(e) NA_character_),
    input_sources    = list("WUE/isotope_pilot/data/processed/wue_augmented/ (06_build_site_years.R output)"),
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
## GATE: reproduce the committed screen_attrition.csv exactly (fixed 101.3 kPa)
## ============================================================================
committed <- readr::read_csv(file.path(tables_dir, "screen_attrition.csv"), show_col_types = FALSE)
committed <- committed[committed$site_id %in% TEST_SITES, ]
committed <- committed[order(committed$site_id, committed$year), ]

gate_cols <- c("site_id", "year", "days_in_year", "days_p_era_above_zero",
               "days_removed_by_rain_rule", "days_lost_quality", "days_lost_daylight",
               "days_lost_day_level", "valid_days", "year_kept")

gate_rows <- lapply(TEST_SITES, function(site) {
  res <- run_zhou_screens(raw_data[[site]], years_dropped,
                           rain_col = "P_ERA", radiation_col = "NETRAD_filled",
                           pressure_kpa = 101.3)
  res$attrition[, gate_cols]
})
gate_check <- do.call(rbind, gate_rows)
gate_check <- gate_check[order(gate_check$site_id, gate_check$year), ]

gate_match <- isTRUE(all.equal(
  as.data.frame(gate_check), as.data.frame(committed[, gate_cols]),
  check.attributes = FALSE
))
if (!gate_match) {
  diffs <- which(!mapply(function(a, b) isTRUE(all.equal(a, b)), gate_check, committed[, gate_cols]))
  stop("[WUE] GATE FAILED: run_zhou_screens() (rain=P_ERA, rad=NETRAD_filled, pressure=101.3 kPa) ",
       "does not reproduce the committed tables/screen_attrition.csv for the 3 test sites. ",
       "Differing column(s): ", paste(names(diffs), collapse = ", "), ". Stopping per instructions.")
}
message("[WUE] GATE PASSED: run_zhou_screens() reproduces the committed screen_attrition.csv exactly ",
        "for ", length(TEST_SITES), " site(s) (", nrow(gate_check), " site-year rows).")

## ============================================================================
## Four variants, PET using the daily PA_F (rain_rule.R default)
## ============================================================================
VARIANTS <- list(
  list(key = "rain_P_ERA_rad_NETRAD", rain_col = "P_ERA", radiation_col = "NETRAD_filled"),
  list(key = "rain_P_ERA_rad_SW_IN",  rain_col = "P_ERA", radiation_col = "SW_IN_F"),
  list(key = "rain_P_F_rad_NETRAD",   rain_col = "P_F",   radiation_col = "NETRAD_filled"),
  list(key = "rain_P_F_rad_SW_IN",    rain_col = "P_F",   radiation_col = "SW_IN_F")
)

attrition_rows <- list()
valid_days_rows <- list()

for (site in TEST_SITES) {
  for (v in VARIANTS) {
    res <- run_zhou_screens(raw_data[[site]], years_dropped,
                             rain_col = v$rain_col, radiation_col = v$radiation_col,
                             pressure_kpa = NULL)  ## NULL -> daily mean PA_F

    att <- res$attrition
    att$variant <- v$key
    att$rain_source <- v$rain_col
    att$radiation_col <- v$radiation_col
    attrition_rows[[length(attrition_rows) + 1L]] <- att[att$year_kept, c(
      "site_id", "year", "variant", "rain_source", "radiation_col",
      "days_removed_by_rain_rule", "days_lost_quality",
      "days_lost_daylight", "days_lost_record_count", "days_lost_gpp_test", "valid_days"
    )]

    if (!is.null(res$daily_valid)) {
      dv <- res$daily_valid
      dv$month <- lubridate::month(dv$date)
      dv$variant <- v$key
      valid_days_rows[[length(valid_days_rows) + 1L]] <- dv[, c("site_id", "variant", "month")]
    }
  }
}

attrition_by_variant <- do.call(rbind, attrition_rows)
names(attrition_by_variant)[names(attrition_by_variant) == "days_lost_daylight"] <- "days_lost_screen_c"
write_csv_meta(attrition_by_variant, file.path(out_tables, "attrition_by_variant.csv"),
  notes = "Kept years only (year_kept==TRUE in years_dropped.csv's complement). days_lost_screen_c is the former combined days_lost_daylight column (daylight window + the radiation_col/GPP/ET/VPD non-negative tests); days_lost_record_count + days_lost_gpp_test is the former combined days_lost_day_level.")

valid_days_long <- do.call(rbind, valid_days_rows)
valid_days_by_month <- valid_days_long |>
  dplyr::group_by(site_id, variant, month) |>
  dplyr::summarise(valid_days = dplyr::n(), .groups = "drop")
## Ensure every site x variant x month combination is present (0 where no valid days)
full_grid <- expand.grid(site_id = TEST_SITES, variant = vapply(VARIANTS, `[[`, character(1), "key"),
                          month = 1:12, stringsAsFactors = FALSE)
valid_days_by_month <- dplyr::left_join(full_grid, valid_days_by_month, by = c("site_id", "variant", "month"))
valid_days_by_month$valid_days[is.na(valid_days_by_month$valid_days)] <- 0L
write_csv_meta(valid_days_by_month, file.path(out_tables, "valid_days_by_month.csv"),
  notes = "Kept years pooled (sum across all kept years of that site).")

## ============================================================================
## records_in_window_by_month.csv -- median records/day in [05:00,21:00]
## with NETRAD_filled>=0, and with SW_IN_F>=0, before any other screen.
## All days, all years (not restricted to kept years or non-rain days).
## ============================================================================
window_rows <- lapply(TEST_SITES, function(site) {
  d <- raw_data[[site]]
  d$date <- as.Date(d$TIMESTAMP_START)
  d$month <- lubridate::month(d$TIMESTAMP_START)
  d$hour_decimal <- lubridate::hour(d$TIMESTAMP_START) + lubridate::minute(d$TIMESTAMP_START) / 60
  in_window <- !is.na(d$hour_decimal) & d$hour_decimal >= 5 & d$hour_decimal <= 21
  d$in_window_netrad <- in_window & !is.na(d$NETRAD_filled) & d$NETRAD_filled >= 0
  d$in_window_swin   <- in_window & !is.na(d$SW_IN_F) & d$SW_IN_F >= 0

  per_day <- d |>
    dplyr::group_by(date, month) |>
    dplyr::summarise(n_netrad = sum(in_window_netrad), n_swin = sum(in_window_swin), .groups = "drop")
  per_day |>
    dplyr::group_by(month) |>
    dplyr::summarise(
      median_records_netrad_filled = stats::median(n_netrad),
      median_records_sw_in_f = stats::median(n_swin),
      n_days = dplyr::n(), .groups = "drop"
    ) |>
    dplyr::mutate(site_id = site)
})
records_in_window_by_month <- do.call(rbind, window_rows)[, c(
  "site_id", "month", "median_records_netrad_filled", "median_records_sw_in_f", "n_days"
)]
write_csv_meta(records_in_window_by_month, file.path(out_tables, "records_in_window_by_month.csv"),
  notes = "All days, all years -- no rain/quality screen applied, just the 05:00-21:00 window and the named radiation column's own non-negativity.")

## ============================================================================
## gauge_share.csv -- share of days in kept years where P_F is fully
## gauge-measured (P_F_QC == 0 at every expected timestep that day).
## ============================================================================
gauge_share_rows <- lapply(TEST_SITES, function(site) {
  d <- raw_data[[site]]
  res <- d$resolution[[1]]
  tpd <- if (identical(res, "HR")) 24L else if (identical(res, "HH")) 48L else
    stop("[WUE] ", site, ": unrecognised resolution '", res, "'.")
  d$date <- as.Date(d$TIMESTAMP_START)
  d$year <- lubridate::year(d$TIMESTAMP_START)
  kept_years <- unique(d$year)[!vapply(unique(d$year), function(y) any(years_dropped$site_id == site & years_dropped$year == y), logical(1))]

  d_kept <- d[d$year %in% kept_years, ]
  daily <- d_kept |>
    dplyr::group_by(date) |>
    dplyr::summarise(n = dplyr::n(), n_qc0 = sum(!is.na(P_F_QC) & P_F_QC == 0), .groups = "drop")
  daily$fully_gauge_measured <- daily$n == tpd & daily$n_qc0 == tpd

  data.frame(site_id = site, n_days_kept_years = nrow(daily),
             n_fully_gauge_measured = sum(daily$fully_gauge_measured),
             share_fully_gauge_measured = round(mean(daily$fully_gauge_measured), 4))
})
gauge_share <- do.call(rbind, gauge_share_rows)
write_csv_meta(gauge_share, file.path(out_tables, "gauge_share.csv"),
  notes = "Kept years only (years_dropped.csv's complement). Fully gauge-measured = P_F_QC==0 at every expected sub-daily timestep that day.")

message("[WUE] Tables written to ", out_tables)

## ============================================================================
## REPORT
## ============================================================================
knitr_like_table <- function(df, n_max = 60) {
  if (is.null(df) || nrow(df) == 0) return("_(no rows)_")
  if (nrow(df) > n_max) df <- df[seq_len(n_max), , drop = FALSE]
  fmt_cell <- function(x) if (is.numeric(x)) format(round(x, 4), trim = TRUE) else as.character(x)
  body <- vapply(seq_len(nrow(df)), function(i) {
    paste0("| ", paste(vapply(df[i, , drop = FALSE], fmt_cell, character(1)), collapse = " | "), " |")
  }, character(1))
  header <- paste0("| ", paste(names(df), collapse = " | "), " |")
  sep    <- paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|")
  paste(c(header, sep, body), collapse = "\n")
}

summary_wide <- attrition_by_variant |>
  dplyr::group_by(site_id, variant) |>
  dplyr::summarise(
    median_valid_days = stats::median(valid_days),
    min_valid_days = min(valid_days), max_valid_days = max(valid_days),
    .groups = "drop"
  ) |>
  dplyr::mutate(cell = sprintf("%s (%d-%d)", format(median_valid_days, trim = TRUE), min_valid_days, max_valid_days)) |>
  dplyr::select(site_id, variant, cell) |>
  tidyr::pivot_wider(names_from = variant, values_from = cell)
summary_wide <- as.data.frame(summary_wide)

report_path <- file.path(docs_dir, paste0("report_screen_variants_", format(Sys.Date(), "%Y%m%d"), ".md"))
report_lines <- c(
  paste0("# Stage 2 screen-variant check (", Sys.Date(), ")"),
  "",
  "Side analysis, read-and-report, on the three stage 2 test sites (US-Fuf, US-Ho2, US-MMS).",
  "Stage 2 itself stays on hold -- 07_apply_screens.R was not re-run and no stage 2 table was",
  "overwritten.",
  "",
  "**Question:** how many valid days per kept site-year survive the Zhou et al. (2015) screens",
  "when (i) the rain source and (ii) the radiation condition in screen c change.",
  "",
  "## Gate",
  "",
  paste0("`run_zhou_screens()` (code/zhou_screens.R) with rain from `P_ERA`, screen c's radiation",
         " test from `NETRAD_filled`, and PET pressure fixed at 101.3 kPa reproduces the",
         " already-committed `tables/screen_attrition.csv` **exactly** for all ", nrow(gate_check),
         " site-year rows across the 3 test sites -- confirming the 07_apply_screens.R refactor",
         " into zhou_screens.R changed nothing."),
  "",
  "## Four variants (PET using the daily PA_F)",
  "",
  "- `rain_P_ERA_rad_NETRAD`: rain from `P_ERA > 0`, screen c from `NETRAD_filled >= 0` (closest",
  "  to 07_apply_screens.R's own current code, but with PA_F-based PET rather than the gate's",
  "  fixed 101.3 kPa).",
  "- `rain_P_ERA_rad_SW_IN`: rain from `P_ERA > 0`, screen c from `SW_IN_F >= 0`.",
  "- `rain_P_F_rad_NETRAD`: rain from `P_F > 0` (as distributed: gauge where measured, `P_ERA`",
  "  fill where not), screen c from `NETRAD_filled >= 0`.",
  "- `rain_P_F_rad_SW_IN`: rain from `P_F > 0`, screen c from `SW_IN_F >= 0`.",
  "",
  "Everything else (quality screen, daylight window, the 10% GPP day test) is unchanged across",
  "variants; PET always uses `NETRAD_filled` for net radiation regardless of the screen c column.",
  "",
  "## Valid days per kept year, median (range), by site and variant",
  "",
  knitr_like_table(summary_wide),
  "",
  "## Supporting tables",
  "",
  "`tables/screen_variants/attrition_by_variant.csv`, `valid_days_by_month.csv`,",
  "`records_in_window_by_month.csv`, `gauge_share.csv` (each with a `.meta.json` companion).",
  "",
  "## What I could not do",
  "",
  "Nothing -- the gate passed and all four tables were produced for all 3 sites."
)
writeLines(unlist(report_lines), report_path)
message("[WUE] Report written: ", report_path)
message("[WUE] 12_screen_variants.R complete.")
