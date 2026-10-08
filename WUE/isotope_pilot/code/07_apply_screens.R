## 07_apply_screens.R — Stage 2. The Zhou et al. (2015, JGR Biogeosciences,
## section 2.1) screens, applied in order a -> b -> c -> d, record-level for
## a/b/c and day-level for d. Operates only on the site-years
## 06_build_site_years.R did NOT drop (years_dropped.csv).
##
## a. Rain: daily P = midnight-to-midnight sum of P_ERA. Daily PET =
##    Priestley-Taylor (alpha 1.26) from daily mean NETRAD_filled and TA_F,
##    G = 0. Exclude every day with P > 0 (MY DECISION 4 -- no threshold).
##    Also exclude the two following days when P > 2*PET, or the one
##    following day when P > PET. Propagated across the site's FULL
##    continuous date range (not reset at calendar-year boundaries), so a
##    rain event on Dec 31 can still exclude Jan 1-2 of the next year.
##
##    Priestley-Taylor inputs: daily mean net radiation, air temperature, and
##    (revised 2026-10-08) daily mean PA_F for the psychrometric constant --
##    falling back to a fixed standard sea-level pressure (101.3 kPa) only
##    where PA_F is missing that day. See rain_rule.R and
##    docs/methods_memo.md "Stage 2 -- Priestley-Taylor PET".
##
## b. Quality: keep records where NEE_QC_sel, LE_F_MDS_QC, VPD_F_QC are each
##    0 or 1 (NA fails).
##
## c. Daylight: keep records with TIMESTAMP_START's local-standard-time
##    hour-of-day in [05:00, 21:00]. Exclude records with negative
##    NETRAD_filled, GPP_gC_sel, ET_mm, or VPD_F (NA fails).
##
## d. Day level: a day is valid only if >=24 records survive a+b+c (HH
##    sites) or >=12 (HR sites: US-Ha1, US-MMS), AND its mean GPP (over
##    surviving records) is >=10% of the max such mean among this
##    site-year's day candidates that already passed the record-count test.
##
## Output (data/processed/, gitignored):
##   wue_daily_valid/<site_id>.rds    one row per valid day: date, year,
##                                    GPP_d, ET_d, VPD_d, n_records,
##                                    day_netrad_estimated
##   wue_subdaily_valid/<site_id>.rds one row per surviving sub-daily record
##                                    within a valid day: date, year,
##                                    GPP_gC_sel, ET_mm, VPD_F -- consumed
##                                    only by 08_compute_metrics.R's
##                                    sub-daily-scale k* grid search
## Output (tables/, git-tracked):
##   screen_attrition.csv   site_id, year, days_in_year,
##                          days_p_era_above_zero, days_removed_by_rain_rule,
##                          days_lost_quality, days_lost_daylight,
##                          days_lost_day_level, valid_days
##
## The per-site screen logic itself (screens a-d) lives in
## run_zhou_screens() (code/zhou_screens.R, revised 2026-10-08) -- shared
## with 12_screen_variants.R's rain-source/radiation-column comparison, so
## the two can never silently diverge. This script calls it with every
## argument at its default (rain from P_ERA, screen c's radiation test from
## NETRAD_filled, PET pressure from daily mean PA_F) -- i.e. identical
## behaviour to the inline version this replaced.

source("WUE/isotope_pilot/code/00_config.R")
source("WUE/isotope_pilot/code/rain_rule.R")
source("WUE/isotope_pilot/code/zhou_screens.R")

processed_dir <- file.path(WUE_ROOT, "data", "processed")
augmented_dir <- file.path(processed_dir, "wue_augmented")
valid_dir     <- file.path(processed_dir, "wue_daily_valid")
subdaily_valid_dir <- file.path(processed_dir, "wue_subdaily_valid")
tables_dir    <- file.path(WUE_ROOT, "tables")
dir.create(valid_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(subdaily_valid_dir, recursive = TRUE, showWarnings = FALSE)

years_dropped <- readr::read_csv(file.path(tables_dir, "years_dropped.csv"), show_col_types = FALSE)

attrition_rows <- list()

process_one_site <- function(site) {
  p <- file.path(augmented_dir, paste0(site, ".rds"))
  if (!file.exists(p)) {
    warning("[WUE] ", site, ": no augmented data (06 did not produce it) -- skipping.")
    return(invisible(NULL))
  }
  d <- readRDS(p)
  if (nrow(d) == 0) return(invisible(NULL))
  d$site_id <- site

  res <- run_zhou_screens(d, years_dropped)

  ## screen_attrition.csv keeps its original single combined
  ## days_lost_day_level column (days_lost_record_count + days_lost_gpp_test
  ## -- run_zhou_screens() returns the split too, used only by
  ## 12_screen_variants.R).
  attrition_rows[[length(attrition_rows) + 1L]] <<- res$attrition[, c(
    "site_id", "year", "days_in_year", "days_p_era_above_zero",
    "days_removed_by_rain_rule", "days_lost_quality", "days_lost_daylight",
    "days_lost_day_level", "valid_days", "year_kept"
  )]

  if (!is.null(res$daily_valid)) {
    saveRDS(res$daily_valid, file.path(valid_dir, paste0(site, ".rds")))
    saveRDS(res$subdaily_valid, file.path(subdaily_valid_dir, paste0(site, ".rds")))
    message("[WUE] ", site, ": ", nrow(res$daily_valid), " valid day(s) across kept years.")
  } else {
    message("[WUE] ", site, ": 0 valid days across kept years.")
  }
  invisible(NULL)
}

for (site in WUE_SITES_STAGE2) process_one_site(site)

screen_attrition <- do.call(rbind, attrition_rows)
write.csv(screen_attrition, file.path(tables_dir, "screen_attrition.csv"), row.names = FALSE)
message("[WUE] screen_attrition.csv written (", nrow(screen_attrition), " site-year rows, ",
        "including dropped years for context -- see year_kept column).")

message("[WUE] 07_apply_screens.R complete.")
