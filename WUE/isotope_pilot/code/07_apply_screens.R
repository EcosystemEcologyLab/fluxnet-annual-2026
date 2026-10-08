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
##    Priestley-Taylor inputs: the user's instructions name only daily mean
##    net radiation and air temperature (G = 0) as inputs -- no atmospheric
##    pressure. The psychrometric constant therefore uses a FIXED standard
##    sea-level pressure (101.3 kPa), not site-specific PA_F. This is my
##    reading, not stated in Zhou et al. 2015 section 2.1 itself (same
##    caveat as the k* method below).
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

source("WUE/isotope_pilot/code/00_config.R")

processed_dir <- file.path(WUE_ROOT, "data", "processed")
augmented_dir <- file.path(processed_dir, "wue_augmented")
valid_dir     <- file.path(processed_dir, "wue_daily_valid")
subdaily_valid_dir <- file.path(processed_dir, "wue_subdaily_valid")
tables_dir    <- file.path(WUE_ROOT, "tables")
dir.create(valid_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(subdaily_valid_dir, recursive = TRUE, showWarnings = FALSE)

years_dropped <- readr::read_csv(file.path(tables_dir, "years_dropped.csv"), show_col_types = FALSE)
is_year_kept <- function(site, year) {
  !any(years_dropped$site_id == site & years_dropped$year == year)
}

## ---- Priestley-Taylor PET (alpha = 1.26), G = 0, fixed P = 101.3 kPa ------
pt_pet_mm_day <- function(rn_wm2_mean, ta_degc, alpha = 1.26) {
  rn_mj_day <- rn_wm2_mean * 86400 * 1e-6          # G = 0, so (Rn - G) = Rn
  lambda    <- 2.501 - 0.002361 * ta_degc          # MJ/kg (FAO-56)
  es        <- 0.6108 * exp(17.27 * ta_degc / (ta_degc + 237.3))  # kPa
  delta     <- 4098 * es / (ta_degc + 237.3)^2     # kPa/degC
  gamma     <- 1.013e-3 * 101.3 / (0.622 * lambda) # kPa/degC, fixed sea-level P
  alpha * (delta / (delta + gamma)) * rn_mj_day / lambda
}

attrition_rows <- list()

process_one_site <- function(site) {
  p <- file.path(augmented_dir, paste0(site, ".rds"))
  if (!file.exists(p)) {
    warning("[WUE] ", site, ": no augmented data (06 did not produce it) -- skipping.")
    return(invisible(NULL))
  }
  d <- readRDS(p)
  if (nrow(d) == 0) return(invisible(NULL))

  resolution  <- d$resolution[[1]]
  day_thresh  <- if (identical(resolution, "HR")) 12L else 24L

  d$date <- as.Date(d$TIMESTAMP_START)
  d$year <- lubridate::year(d$TIMESTAMP_START)
  d$hour_decimal <- lubridate::hour(d$TIMESTAMP_START) + lubridate::minute(d$TIMESTAMP_START) / 60

  ## ---- Daily table over the FULL continuous date range ----------------
  full_dates <- data.frame(date = seq(min(d$date), max(d$date), by = "day"))
  daily_obs <- d |>
    dplyr::group_by(date) |>
    dplyr::summarise(
      P_day = sum(P_ERA, na.rm = TRUE),
      TA_day = mean(TA_F, na.rm = TRUE),
      NETRAD_day = mean(NETRAD_filled, na.rm = TRUE),
      day_netrad_estimated = any(netrad_estimated),
      .groups = "drop"
    )
  daily <- dplyr::left_join(full_dates, daily_obs, by = "date")
  daily <- daily[order(daily$date), ]
  daily$PET_day <- pt_pet_mm_day(daily$NETRAD_day, daily$TA_day)

  rainy <- !is.na(daily$P_day) & daily$P_day > 0
  sev2  <- !is.na(daily$P_day) & !is.na(daily$PET_day) & daily$P_day > 2 * daily$PET_day
  sev1  <- !is.na(daily$P_day) & !is.na(daily$PET_day) & daily$P_day > daily$PET_day & !sev2

  n <- nrow(daily)
  excluded_following <- rep(FALSE, n)
  idx2 <- which(sev2)
  for (i in idx2) {
    for (off in 1:2) if (i + off <= n) excluded_following[i + off] <- TRUE
  }
  idx1 <- which(sev1)
  for (i in idx1) {
    if (i + 1 <= n) excluded_following[i + 1] <- TRUE
  }
  daily$excluded_rain_rule <- rainy | excluded_following

  rain_excluded_by_date <- stats::setNames(daily$excluded_rain_rule, as.character(daily$date))
  d$excluded_by_rain <- rain_excluded_by_date[as.character(d$date)]

  ## ---- a: rain -----------------------------------------------------------
  d$survives_a <- !d$excluded_by_rain

  ## ---- b: quality ---------------------------------------------------------
  qc_ok <- function(x) !is.na(x) & x %in% c(0, 1)
  d$survives_b <- d$survives_a &
    qc_ok(d$NEE_QC_sel) & qc_ok(d$LE_F_MDS_QC) & qc_ok(d$VPD_F_QC)

  ## ---- c: daylight + non-negative -----------------------------------------
  nonneg <- function(x) !is.na(x) & x >= 0
  d$survives_c <- d$survives_b &
    !is.na(d$hour_decimal) & d$hour_decimal >= 5 & d$hour_decimal <= 21 &
    nonneg(d$NETRAD_filled) & nonneg(d$GPP_gC_sel) & nonneg(d$ET_mm) & nonneg(d$VPD_F)

  ## ---- per-day record counts at each stage, + day-mean GPP among
  ## survives_c records -------------------------------------------------------
  per_day_counts <- d |>
    dplyr::group_by(date) |>
    dplyr::summarise(
      n_after_a = sum(survives_a),
      n_after_b = sum(survives_b),
      n_after_c = sum(survives_c),
      day_mean_gpp = if (any(survives_c)) mean(GPP_gC_sel[survives_c]) else NA_real_,
      .groups = "drop"
    )
  daily <- dplyr::left_join(daily, per_day_counts, by = "date")
  daily$n_after_a[is.na(daily$n_after_a)] <- 0L
  daily$n_after_b[is.na(daily$n_after_b)] <- 0L
  daily$n_after_c[is.na(daily$n_after_c)] <- 0L

  daily$year <- lubridate::year(daily$date)
  daily$candidate_valid <- daily$n_after_c >= day_thresh

  ## ---- d: day level (10% of this site-year's max day-mean GPP) ------------
  valid_rows <- list()
  subdaily_valid_rows <- list()
  attrition_this_site <- list()

  for (yr in sort(unique(daily$year))) {
    d_yr <- daily[daily$year == yr, , drop = FALSE]
    kept <- is_year_kept(site, yr)

    candidates <- d_yr[d_yr$candidate_valid, , drop = FALSE]
    year_max_gpp <- if (nrow(candidates) > 0) max(candidates$day_mean_gpp, na.rm = TRUE) else NA_real_
    d_yr$final_valid <- d_yr$candidate_valid &
      !is.na(d_yr$day_mean_gpp) & !is.na(year_max_gpp) &
      d_yr$day_mean_gpp >= 0.10 * year_max_gpp

    if (kept) {
      valid_dates <- d_yr$date[d_yr$final_valid]
      if (length(valid_dates) > 0) {
        day_tab <- d |>
          dplyr::filter(survives_c, date %in% valid_dates) |>
          dplyr::group_by(date) |>
          dplyr::summarise(
            GPP_d = sum(GPP_gC_sel), ET_d = sum(ET_mm), VPD_d = mean(VPD_F),
            n_records = dplyr::n(), .groups = "drop"
          ) |>
          dplyr::left_join(d_yr[, c("date", "day_netrad_estimated")], by = "date") |>
          dplyr::mutate(site_id = site, year = yr)
        valid_rows[[length(valid_rows) + 1L]] <- as.data.frame(day_tab)

        ## Sub-daily records contributing to this site-year's valid days --
        ## kept for 08_compute_metrics.R's sub-daily-scale k* grid search.
        rec_tab <- d[d$survives_c & d$date %in% valid_dates,
                     c("date", "GPP_gC_sel", "ET_mm", "VPD_F"), drop = FALSE]
        rec_tab$site_id <- site
        rec_tab$year    <- yr
        subdaily_valid_rows[[length(subdaily_valid_rows) + 1L]] <- rec_tab
      }
    }

    days_in_year      <- nrow(d_yr)
    days_rainy        <- sum(rainy[daily$year == yr], na.rm = TRUE)
    days_rain_removed <- sum(d_yr$n_after_a == 0 & days_in_year > 0)
    days_lost_quality <- sum(d_yr$n_after_a > 0 & d_yr$n_after_b == 0)
    days_lost_daylight <- sum(d_yr$n_after_b > 0 & d_yr$n_after_c == 0)
    days_lost_day_level <- sum(d_yr$n_after_c > 0 & !d_yr$final_valid)
    valid_days_n <- sum(d_yr$final_valid)

    attrition_this_site[[length(attrition_this_site) + 1L]] <- data.frame(
      site_id = site, year = yr, days_in_year = days_in_year,
      days_p_era_above_zero = days_rainy,
      days_removed_by_rain_rule = days_rain_removed,
      days_lost_quality = days_lost_quality,
      days_lost_daylight = days_lost_daylight,
      days_lost_day_level = days_lost_day_level,
      valid_days = valid_days_n,
      year_kept = kept,
      stringsAsFactors = FALSE
    )
  }

  attrition_rows[[length(attrition_rows) + 1L]] <<- do.call(rbind, attrition_this_site)

  if (length(valid_rows) > 0) {
    out <- do.call(rbind, valid_rows)
    saveRDS(out, file.path(valid_dir, paste0(site, ".rds")))
    rec_out <- do.call(rbind, subdaily_valid_rows)
    saveRDS(rec_out, file.path(subdaily_valid_dir, paste0(site, ".rds")))
    message("[WUE] ", site, ": ", nrow(out), " valid day(s) across kept years.")
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
