## zhou_screens.R — the Zhou et al. (2015, JGR Biogeosciences, section 2.1)
## screens (a: rain, b: quality, c: daylight + non-negative, d: day level),
## factored out of 07_apply_screens.R into run_zhou_screens() so 07 and the
## screen-variant side analysis (12_screen_variants.R) call the SAME
## function rather than keeping two copies that can drift apart.
##
## Defaults reproduce 07_apply_screens.R's current code (rain from P_ERA,
## screen c's non-negative-radiation test from NETRAD_filled, PET pressure
## from the daily mean PA_F -- rain_rule.R's pt_pet_mm_day() falls back to
## 101.3 kPa only where PA_F is itself missing that day -- and the day-level
## GPP test, gpp_test = "daymean", against the largest daily mean GPP among
## candidate days, added 2026-10-07 alongside "halfhour").
##
## Priestley-Taylor PET ALWAYS uses NETRAD_filled for Rn, regardless of
## `radiation_col` -- `radiation_col` only changes screen c's own
## non-negative-radiation test, never the PET input.

#' Run the Zhou et al. (2015) screens for one site's augmented sub-daily data
#'
#' @param d The augmented per-site data frame (as read from
#'   `data/processed/wue_augmented/<site>.rds` -- 06_build_site_years.R's
#'   output), one row per sub-daily record, for a single site.
#' @param years_dropped The `years_dropped.csv` data frame (site_id, year),
#'   listing site-years 06 dropped for incomplete nighttime GPP or year 2026.
#' @param rain_col Column in `d` summed to a daily total for the rain screen
#'   (screen a). Default `"P_ERA"` -- 07's current behaviour.
#' @param radiation_col Column in `d` tested for non-negativity in screen c.
#'   Default `"NETRAD_filled"` -- 07's current behaviour. Does NOT affect the
#'   Priestley-Taylor PET input, which always uses `NETRAD_filled`.
#' @param pressure_kpa Pressure (kPa) passed to `pt_pet_mm_day()` for the
#'   psychrometric constant. `NULL` (default) uses the site's own daily mean
#'   `PA_F` -- 07's current code. A single fixed value (e.g. `101.3`) forces
#'   that pressure for every day, for reproducing the pre-2026-10-07 PET
#'   formula (see 12_screen_variants.R's gate check against the already-
#'   committed `screen_attrition.csv`).
#' @param gpp_test Which maximum the day-level 10% GPP test (screen d) is
#'   taken against. `"daymean"` (default, 07's current code): a day's mean
#'   GPP (over its screen a-c survivors) must be >= 10% of the LARGEST such
#'   daily mean among the site-year's candidate days (days that already
#'   passed the record-count test). `"halfhour"` (Zhou et al. 2015's own
#'   wording): a day's mean GPP must instead be >= 10% of the maximum
#'   SINGLE-RECORD GPP in the site-year, taken over every record passing
#'   screens a-c (not just candidate days). Added 2026-10-07.
#'
#' @return A list: `daily_valid` (one row per valid day: date, year, GPP_d,
#'   ET_d, VPD_d, n_records, day_netrad_estimated, site_id), `subdaily_valid`
#'   (one row per surviving sub-daily record within a valid day: date, year,
#'   GPP_gC_sel, ET_mm, VPD_F, site_id), and `attrition` (one row per
#'   site-year: days_in_year, days_rainy (`rain_col` > 0),
#'   days_removed_by_rain_rule, days_lost_quality, days_lost_daylight,
#'   days_lost_record_count, days_lost_gpp_test, days_lost_day_level (=
#'   days_lost_record_count + days_lost_gpp_test, matching
#'   screen_attrition.csv's single combined column), valid_days, year_kept).
run_zhou_screens <- function(d, years_dropped, rain_col = "P_ERA",
                              radiation_col = "NETRAD_filled", pressure_kpa = NULL,
                              gpp_test = c("daymean", "halfhour")) {
  gpp_test <- match.arg(gpp_test)
  site <- d$site_id[[1]]
  is_year_kept <- function(yr) !any(years_dropped$site_id == site & years_dropped$year == yr)

  resolution  <- d$resolution[[1]]
  day_thresh  <- if (identical(resolution, "HR")) 12L else 24L

  d$date <- as.Date(d$TIMESTAMP_START)
  d$year <- lubridate::year(d$TIMESTAMP_START)
  d$hour_decimal <- lubridate::hour(d$TIMESTAMP_START) + lubridate::minute(d$TIMESTAMP_START) / 60

  ## ---- Daily table over the FULL continuous date range ----------------
  full_dates <- data.frame(date = seq(min(d$date), max(d$date), by = "day"))
  rain_series <- d[[rain_col]]
  daily_obs <- d |>
    dplyr::mutate(.rain_series = rain_series) |>
    dplyr::group_by(date) |>
    dplyr::summarise(
      P_day = sum(.rain_series, na.rm = TRUE),
      TA_day = mean(TA_F, na.rm = TRUE),
      NETRAD_day = mean(NETRAD_filled, na.rm = TRUE),   ## PET input -- fixed, never radiation_col
      PA_day = mean(PA_F, na.rm = TRUE),
      day_netrad_estimated = any(netrad_estimated),
      .groups = "drop"
    )
  daily <- dplyr::left_join(full_dates, daily_obs, by = "date")
  daily <- daily[order(daily$date), ]
  pressure_for_pet <- if (is.null(pressure_kpa)) daily$PA_day else pressure_kpa
  daily$PET_day <- pt_pet_mm_day(daily$NETRAD_day, daily$TA_day, pressure_for_pet)

  rainy <- !is.na(daily$P_day) & daily$P_day > 0
  daily$excluded_rain_rule <- rain_rule_excluded(daily)

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
  radiation_series <- d[[radiation_col]]
  d$survives_c <- d$survives_b &
    !is.na(d$hour_decimal) & d$hour_decimal >= 5 & d$hour_decimal <= 21 &
    nonneg(radiation_series) & nonneg(d$GPP_gC_sel) & nonneg(d$ET_mm) & nonneg(d$VPD_F)

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

  ## ---- d: day level (10% of a site-year maximum -- gpp_test chooses which) -
  valid_rows <- list()
  subdaily_valid_rows <- list()
  attrition_rows <- list()

  for (yr in sort(unique(daily$year))) {
    d_yr <- daily[daily$year == yr, , drop = FALSE]
    kept <- is_year_kept(yr)

    candidates <- d_yr[d_yr$candidate_valid, , drop = FALSE]
    ## Both maxima are always computed (regardless of which gpp_test is
    ## active) and returned in `attrition` as threshold_daymean/
    ## threshold_halfhour -- so a single call can report both thresholds
    ## for comparison (12_screen_variants.R's gpp_thresholds.csv), without
    ## a second, independent reimplementation of either.
    year_max_gpp_daymean <- if (nrow(candidates) > 0) max(candidates$day_mean_gpp, na.rm = TRUE) else NA_real_
    recs_year <- d[d$survives_c & d$year == yr, , drop = FALSE]
    year_max_gpp_halfhour <- if (nrow(recs_year) > 0) max(recs_year$GPP_gC_sel, na.rm = TRUE) else NA_real_
    year_max_gpp <- if (gpp_test == "daymean") year_max_gpp_daymean else year_max_gpp_halfhour

    qc01_recs <- d[d$year == yr & !is.na(d$NEE_QC_sel) & d$NEE_QC_sel %in% c(0, 1), , drop = FALSE]
    max_record_gpp_qc01 <- if (nrow(qc01_recs) > 0 && any(!is.na(qc01_recs$GPP_gC_sel))) {
      max(qc01_recs$GPP_gC_sel, na.rm = TRUE)
    } else {
      NA_real_
    }

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

        rec_tab <- d[d$survives_c & d$date %in% valid_dates,
                     c("date", "GPP_gC_sel", "ET_mm", "VPD_F"), drop = FALSE]
        rec_tab$site_id <- site
        rec_tab$year    <- yr
        subdaily_valid_rows[[length(subdaily_valid_rows) + 1L]] <- rec_tab
      }
    }

    days_in_year        <- nrow(d_yr)
    days_rainy          <- sum(rainy[daily$year == yr], na.rm = TRUE)
    days_rain_removed   <- sum(d_yr$n_after_a == 0 & days_in_year > 0)
    days_lost_quality   <- sum(d_yr$n_after_a > 0 & d_yr$n_after_b == 0)
    days_lost_daylight  <- sum(d_yr$n_after_b > 0 & d_yr$n_after_c == 0)
    ## Split of the original combined "day level" loss:
    days_lost_record_count <- sum(d_yr$n_after_c > 0 & !d_yr$candidate_valid)
    days_lost_gpp_test     <- sum(d_yr$candidate_valid & !d_yr$final_valid)
    days_lost_day_level    <- days_lost_record_count + days_lost_gpp_test  ## == sum(n_after_c>0 & !final_valid)
    valid_days_n <- sum(d_yr$final_valid)

    attrition_rows[[length(attrition_rows) + 1L]] <- data.frame(
      site_id = site, year = yr, days_in_year = days_in_year,
      days_p_era_above_zero = days_rainy,
      days_removed_by_rain_rule = days_rain_removed,
      days_lost_quality = days_lost_quality,
      days_lost_daylight = days_lost_daylight,
      days_lost_record_count = days_lost_record_count,
      days_lost_gpp_test = days_lost_gpp_test,
      days_lost_day_level = days_lost_day_level,
      valid_days = valid_days_n,
      year_kept = kept,
      threshold_daymean = 0.10 * year_max_gpp_daymean,
      threshold_halfhour = 0.10 * year_max_gpp_halfhour,
      max_record_gpp_qc01 = max_record_gpp_qc01,
      stringsAsFactors = FALSE
    )
  }

  list(
    daily_valid = if (length(valid_rows) > 0) do.call(rbind, valid_rows) else NULL,
    subdaily_valid = if (length(subdaily_valid_rows) > 0) do.call(rbind, subdaily_valid_rows) else NULL,
    attrition = do.call(rbind, attrition_rows)
  )
}
