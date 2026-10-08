## 12_screen_variants.R — Stage 2 screen-variant check, read-and-report, on
## the three stage 2 test sites (US-Fuf, US-Ho2, US-MMS). Stage 2 itself
## stays on hold -- this script does not rerun 07_apply_screens.R and does
## not overwrite any stage 2 table.
##
## Question: how many valid days per kept site-year survive the Zhou et al.
## (2015) screens when (i) the rain source, (ii) the radiation condition in
## screen c, and (iii, added 2026-10-09) the GPP day test's reference
## maximum (gpp_test = "daymean" vs "halfhour") all change.
##
## GATE 1: run_zhou_screens() (code/zhou_screens.R, the function
## 07_apply_screens.R itself now calls) with rain from P_ERA, screen c's
## radiation test from NETRAD_filled, PET pressure FIXED at 101.3 kPa, and
## gpp_test = "daymean" (default) must reproduce the already-committed
## tables/screen_attrition.csv EXACTLY -- confirming the 07 refactor changed
## nothing. The fixed-101.3 pressure (not the daily PA_F now in rain_rule.R)
## is deliberate: it matches the PET formula screen_attrition.csv was
## actually generated under (07 has not been re-run since the PA_F change).
##
## GATE 2 (added 2026-10-09): with gpp_test = "daymean" and PET using the
## daily PA_F, the four rain x radiation variants below must reproduce the
## already-committed tables/screen_variants/attrition_by_variant.csv EXACTLY
## -- confirming the new gpp_test argument changed nothing when left at its
## default. Both gates stop() on failure.
##
## Eight variants (PET using the daily PA_F throughout) cross:
##   rain source:    P_ERA > 0           | P_F > 0 (as distributed: gauge
##                                          where measured, P_ERA fill where not)
##   screen c input: NETRAD_filled >= 0  | SW_IN_F >= 0
##   gpp_test:       "daymean" (10% of the largest daily mean GPP among
##                     candidate days) | "halfhour" (10% of the maximum
##                     single-record GPP over screen a-c survivors, Zhou et
##                     al. 2015's own wording)
## Everything else (quality screen, daylight window) stays exactly as coded;
## PET always uses NETRAD_filled regardless of the screen c column.
##
## Output (tables/screen_variants/, git-tracked, each with .meta.json;
## attrition_by_variant.csv and valid_days_by_month.csv extended with a
## gpp_test column rather than replaced -- the gate above confirms their
## "daymean" rows are unchanged):
##   attrition_by_variant.csv        site, kept year, variant, gpp_test:
##                                   days removed by rain, lost to quality,
##                                   lost to screen c, lost to record count,
##                                   lost to the GPP test, valid days
##   valid_days_by_month.csv         valid days per site, variant, gpp_test,
##                                   month (kept years pooled)
##   gpp_thresholds.csv              per site/kept year (reference variant:
##                                   rain=P_ERA, rad=NETRAD_filled): the two
##                                   thresholds (10% of the largest daily
##                                   mean; 10% of the maximum record), plus
##                                   the maximum record GPP over all records
##                                   with NEE QC 0 or 1, for comparison
##   records_in_window_by_month.csv  (unchanged from 2026-10-08 -- gpp_test
##                                   does not affect it, not recomputed)
##   gauge_share.csv                 (unchanged from 2026-10-08, same reason)
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
## Eight variants, PET using the daily PA_F (rain_rule.R default)
## ============================================================================
VARIANTS <- list(
  list(key = "rain_P_ERA_rad_NETRAD", rain_col = "P_ERA", radiation_col = "NETRAD_filled"),
  list(key = "rain_P_ERA_rad_SW_IN",  rain_col = "P_ERA", radiation_col = "SW_IN_F"),
  list(key = "rain_P_F_rad_NETRAD",   rain_col = "P_F",   radiation_col = "NETRAD_filled"),
  list(key = "rain_P_F_rad_SW_IN",    rain_col = "P_F",   radiation_col = "SW_IN_F")
)
GPP_TESTS <- c("daymean", "halfhour")

attrition_rows <- list()
valid_days_rows <- list()

for (site in TEST_SITES) {
  for (v in VARIANTS) {
    for (gt in GPP_TESTS) {
      res <- run_zhou_screens(raw_data[[site]], years_dropped,
                               rain_col = v$rain_col, radiation_col = v$radiation_col,
                               pressure_kpa = NULL,  ## NULL -> daily mean PA_F
                               gpp_test = gt)

      att <- res$attrition
      att$variant <- v$key
      att$rain_source <- v$rain_col
      att$radiation_col <- v$radiation_col
      att$gpp_test <- gt
      attrition_rows[[length(attrition_rows) + 1L]] <- att[att$year_kept, c(
        "site_id", "year", "variant", "rain_source", "radiation_col", "gpp_test",
        "days_removed_by_rain_rule", "days_lost_quality",
        "days_lost_daylight", "days_lost_record_count", "days_lost_gpp_test", "valid_days"
      )]

      if (!is.null(res$daily_valid)) {
        dv <- res$daily_valid
        dv$month <- lubridate::month(dv$date)
        dv$variant <- v$key
        dv$gpp_test <- gt
        valid_days_rows[[length(valid_days_rows) + 1L]] <- dv[, c("site_id", "variant", "gpp_test", "month")]
      }
    }
  }
}

attrition_by_variant <- do.call(rbind, attrition_rows)
names(attrition_by_variant)[names(attrition_by_variant) == "days_lost_daylight"] <- "days_lost_screen_c"

## ---- GATE 2: the gpp_test = "daymean" rows must reproduce the already-
## committed attrition_by_variant.csv exactly (confirms the new gpp_test
## argument changed nothing at its default). ---------------------------------
committed_avar <- readr::read_csv(file.path(out_tables, "attrition_by_variant.csv"), show_col_types = FALSE)
avar_cols <- c("site_id", "year", "variant", "rain_source", "radiation_col",
               "days_removed_by_rain_rule", "days_lost_quality", "days_lost_screen_c",
               "days_lost_record_count", "days_lost_gpp_test", "valid_days")
## committed_avar may ALREADY be this script's own extended (gpp_test-column)
## output from a prior run -- filter to "daymean" first so the gate stays
## idempotent across re-runs, not just correct on the very first run against
## the original (no gpp_test column) 2026-10-08 file.
if ("gpp_test" %in% names(committed_avar)) {
  committed_avar <- committed_avar[committed_avar$gpp_test == "daymean", ]
}
daymean_rows <- attrition_by_variant[attrition_by_variant$gpp_test == "daymean", avar_cols]
daymean_rows <- daymean_rows[order(daymean_rows$site_id, daymean_rows$year, daymean_rows$variant), ]
committed_avar <- committed_avar[order(committed_avar$site_id, committed_avar$year, committed_avar$variant), ]
gate2_match <- isTRUE(all.equal(as.data.frame(daymean_rows), as.data.frame(committed_avar[, avar_cols]),
                                 check.attributes = FALSE))
if (!gate2_match) {
  stop("[WUE] GATE 2 FAILED: run_zhou_screens(gpp_test = \"daymean\") does not reproduce the ",
       "already-committed tables/screen_variants/attrition_by_variant.csv exactly. Stopping per instructions.")
}
message("[WUE] GATE 2 PASSED: gpp_test = \"daymean\" reproduces the committed attrition_by_variant.csv ",
        "exactly (", nrow(daymean_rows), " rows).")

write_csv_meta(attrition_by_variant, file.path(out_tables, "attrition_by_variant.csv"),
  notes = "Extended 2026-10-09 with a gpp_test column (daymean/halfhour) -- Gate 2 confirms the daymean rows are unchanged from the 2026-10-08 version. Kept years only. days_lost_screen_c is the former combined days_lost_daylight column; days_lost_record_count + days_lost_gpp_test is the former combined days_lost_day_level.")

valid_days_long <- do.call(rbind, valid_days_rows)
valid_days_by_month <- valid_days_long |>
  dplyr::group_by(site_id, variant, gpp_test, month) |>
  dplyr::summarise(valid_days = dplyr::n(), .groups = "drop")
## Ensure every site x variant x gpp_test x month combination is present (0 where no valid days)
full_grid <- expand.grid(site_id = TEST_SITES, variant = vapply(VARIANTS, `[[`, character(1), "key"),
                          gpp_test = GPP_TESTS, month = 1:12, stringsAsFactors = FALSE)
valid_days_by_month <- dplyr::left_join(full_grid, valid_days_by_month, by = c("site_id", "variant", "gpp_test", "month"))
valid_days_by_month$valid_days[is.na(valid_days_by_month$valid_days)] <- 0L
write_csv_meta(valid_days_by_month, file.path(out_tables, "valid_days_by_month.csv"),
  notes = "Extended 2026-10-09 with a gpp_test column (daymean/halfhour). Kept years pooled (sum across all kept years of that site).")

## ============================================================================
## gpp_thresholds.csv -- per site/kept year, the two GPP-test thresholds and
## the max record GPP over NEE-QC-0-or-1 records, from the reference variant
## (rain=P_ERA, radiation=NETRAD_filled; both thresholds are always computed
## by run_zhou_screens() regardless of which gpp_test was requested).
## ============================================================================
gpp_threshold_rows <- lapply(TEST_SITES, function(site) {
  res <- run_zhou_screens(raw_data[[site]], years_dropped,
                           rain_col = "P_ERA", radiation_col = "NETRAD_filled", pressure_kpa = NULL)
  att <- res$attrition
  att[att$year_kept, c("site_id", "year", "threshold_daymean", "threshold_halfhour", "max_record_gpp_qc01")]
})
gpp_thresholds <- do.call(rbind, gpp_threshold_rows)
write_csv_meta(gpp_thresholds, file.path(out_tables, "gpp_thresholds.csv"),
  notes = "Reference variant: rain=P_ERA, radiation=NETRAD_filled, PET pressure from daily PA_F. threshold_daymean/threshold_halfhour are 10% of the respective year_max_gpp; max_record_gpp_qc01 is the max GPP_gC_sel over all records that year with NEE QC 0 or 1 (no other screen), for comparison.")

message("[WUE] Tables written to ", out_tables, " (records_in_window_by_month.csv and gauge_share.csv unchanged from 2026-10-08 -- gpp_test does not affect them, not recomputed).")

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
  dplyr::mutate(variant_gpp = paste0(variant, "_", gpp_test)) |>
  dplyr::group_by(site_id, variant_gpp) |>
  dplyr::summarise(
    median_valid_days = stats::median(valid_days),
    min_valid_days = min(valid_days), max_valid_days = max(valid_days),
    .groups = "drop"
  ) |>
  dplyr::mutate(cell = sprintf("%s (%d-%d)", format(median_valid_days, trim = TRUE), min_valid_days, max_valid_days)) |>
  dplyr::select(site_id, variant_gpp, cell) |>
  tidyr::pivot_wider(names_from = variant_gpp, values_from = cell)
summary_wide <- as.data.frame(summary_wide)

## Month-by-site table for the P_F + SW_IN_F variant, under each GPP test.
month_table <- valid_days_by_month[valid_days_by_month$variant == "rain_P_F_rad_SW_IN", ] |>
  dplyr::mutate(col = paste0("gpp_", gpp_test)) |>
  dplyr::select(site_id, month, col, valid_days) |>
  tidyr::pivot_wider(names_from = col, values_from = valid_days)
month_table <- as.data.frame(month_table[order(month_table$site_id, month_table$month), ])

report_path <- file.path(docs_dir, paste0("report_screen_variants_", format(Sys.Date(), "%Y%m%d"), ".md"))
report_lines <- c(
  paste0("# Stage 2 screen-variant check (", Sys.Date(), ")"),
  "",
  "Side analysis, read-and-report, on the three stage 2 test sites (US-Fuf, US-Ho2, US-MMS).",
  "Stage 2 itself stays on hold -- 07_apply_screens.R was not re-run and no stage 2 table was",
  "overwritten.",
  "",
  "**Question:** how many valid days per kept site-year survive the Zhou et al. (2015) screens",
  "when (i) the rain source, (ii) the radiation condition in screen c, and (iii, added",
  "2026-10-09) the GPP day test's reference maximum all change.",
  "",
  "## Gate 1",
  "",
  paste0("`run_zhou_screens()` (code/zhou_screens.R) with rain from `P_ERA`, screen c's radiation",
         " test from `NETRAD_filled`, PET pressure fixed at 101.3 kPa, and `gpp_test = \"daymean\"`",
         " (default) reproduces the already-committed `tables/screen_attrition.csv` **exactly** for",
         " all ", nrow(gate_check), " site-year rows across the 3 test sites -- confirming the",
         " 07_apply_screens.R refactor into zhou_screens.R changed nothing."),
  "",
  "## Gate 2 (added 2026-10-09)",
  "",
  paste0("With `gpp_test = \"daymean\"` and PET using the daily `PA_F`, the four rain x radiation",
         " variants reproduce the already-committed",
         " `tables/screen_variants/attrition_by_variant.csv` **exactly** for all ", nrow(daymean_rows),
         " rows -- confirming the new `gpp_test` argument changed nothing at its default."),
  "",
  "## Eight variants (PET using the daily PA_F throughout)",
  "",
  "Rain source x screen c radiation (as before, 2026-10-08):",
  "",
  "- `rain_P_ERA_rad_NETRAD`: rain from `P_ERA > 0`, screen c from `NETRAD_filled >= 0`.",
  "- `rain_P_ERA_rad_SW_IN`: rain from `P_ERA > 0`, screen c from `SW_IN_F >= 0`.",
  "- `rain_P_F_rad_NETRAD`: rain from `P_F > 0` (as distributed: gauge where measured, `P_ERA`",
  "  fill where not), screen c from `NETRAD_filled >= 0`.",
  "- `rain_P_F_rad_SW_IN`: rain from `P_F > 0`, screen c from `SW_IN_F >= 0`.",
  "",
  "Crossed with the GPP day test (added 2026-10-09):",
  "",
  "- `daymean` (default, 07's current code): a day's mean GPP must be >= 10% of the LARGEST such",
  "  daily mean among the site-year's candidate days.",
  "- `halfhour` (Zhou et al. 2015's own wording): a day's mean GPP must instead be >= 10% of the",
  "  maximum SINGLE-RECORD GPP in the site-year, over every record passing screens a-c.",
  "",
  "Quality screen and daylight window are unchanged across all eight; PET always uses",
  "`NETRAD_filled` for net radiation regardless of the screen c column.",
  "",
  "## Valid days per kept year, median (range), by site, for all 8 variants",
  "",
  knitr_like_table(summary_wide),
  "",
  "## Valid days by month, P_F + SW_IN_F variant, by GPP test",
  "",
  "`rain_P_F_rad_SW_IN`, kept years pooled, one row per site/month:",
  "",
  knitr_like_table(month_table, n_max = 40),
  "",
  "## Supporting tables",
  "",
  "`tables/screen_variants/attrition_by_variant.csv`, `valid_days_by_month.csv` (both extended",
  "2026-10-09 with a `gpp_test` column), `gpp_thresholds.csv` (new 2026-10-09),",
  "`records_in_window_by_month.csv`, `gauge_share.csv` (both unchanged from 2026-10-08 -- `gpp_test`",
  "does not affect them) -- each with a `.meta.json` companion.",
  "",
  "## What I could not do",
  "",
  "Nothing -- both gates passed and all tables were produced for all 3 sites."
)
writeLines(unlist(report_lines), report_path)
message("[WUE] Report written: ", report_path)
message("[WUE] 12_screen_variants.R complete.")
