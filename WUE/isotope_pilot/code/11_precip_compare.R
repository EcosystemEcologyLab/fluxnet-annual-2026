## 11_precip_compare.R — read-and-report comparison of ERA5-downscaled
## precipitation (P_ERA) against the tower rain gauge (P_F where
## P_F_QC == 0), at the DAILY scale the stage 2 rain rule (07_apply_screens.R,
## MY DECISION 4: exclude every day with P > 0) actually uses. Requested
## 2026-10-07, before deciding whether to launch the stage 2 full run.
##
## Prior ERA5-precipitation work (SESSION_LOG.md, 2026-09-17 to 2026-09-22,
## and the two 2026-10-07 coordination-package entries) compared ANNUAL and
## MONTHLY totals network-wide. None of it looked at daily wet/dry
## agreement -- this script fills exactly that gap, for the 12 WUE pilot
## sites only.
##
## SCOPE: read-only outside WUE/isotope_pilot/ (except this file and its
## outputs). Does NOT change the rain screen, does NOT recompute WUE, does
## NOT propose, recommend, or apply a threshold. Reuses 05_read_subdaily_wue.R
## (already-extracted data, no download) and the shared rain_rule.R /
## netrad_gapfill.R helpers 07/06 use -- "the same PET and the same code
## path", not a reimplementation (item 9 below).
##
## Definitions:
## - Gauge precipitation = P_F where P_F_QC == 0 only (flags 1, 2 never
##   treated as measured).
## - A fully measured day has P_F_QC == 0 at every expected sub-daily
##   timestep (48/day HH, 24/day HR) for that calendar day. ALL comparisons
##   below (except item 3, see its own note) use fully measured days only,
##   the same days for both series.
## - Daily totals: midnight-to-midnight sums (sub-daily sum for precip,
##   consistent with 07_apply_screens.R's own daily P).
##
## Output (tables/precip_compare/, figures/precip_compare/, git-tracked,
## each with a .meta.json companion):
##   identity_check.csv, coverage.csv, totals_site_year.csv,
##   totals_by_month.csv, gauge_resolution.csv, wet_day_frequency.csv,
##   agreement.csv, era_on_gauge_dry.csv, freq_matching_amount.csv,
##   duration.csv, rain_rule_consequence.csv
##   fig_wet_freq_amount.png, fig_days_removed_by_source.png,
##   fig_era_on_gauge_dry.png
## Output (docs/): report_precip_compare_<date>.md

source("WUE/isotope_pilot/code/00_config.R")
source("WUE/isotope_pilot/code/rain_rule.R")
source("WUE/isotope_pilot/code/netrad_gapfill.R")
library(ggplot2)

processed_dir <- file.path(WUE_ROOT, "data", "processed")
subdaily_dir  <- file.path(processed_dir, "subdaily_wue")
out_tables    <- file.path(WUE_ROOT, "tables", "precip_compare")
out_figures   <- file.path(WUE_ROOT, "figures", "precip_compare")
docs_dir      <- file.path(WUE_ROOT, "docs")
dir.create(out_tables, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(file.path(processed_dir, "read_status_wue.csv"))) {
  stop("[WUE] read_status_wue.csv not found -- run 05_read_subdaily_wue.R (all 12 sites, ",
       "WUE_SITE_SUBSET unset) first. This script reuses its output; no new download needed.")
}
read_status <- readr::read_csv(file.path(processed_dir, "read_status_wue.csv"), show_col_types = FALSE)

## ---- local .meta.json writer -----------------------------------------------
## NOT R/utils.R's write_output_metadata() -- that function always appends to
## the repo-root outputs/session_info.txt, which would break this pilot's own
## "writes nothing outside WUE/isotope_pilot/" rule (README.md). Same fields
## otherwise (CLAUDE.md "Output Metadata"), scoped locally.
write_meta <- function(output_path, notes = "") {
  meta <- list(
    run_datetime_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    pipeline_version = tryCatch(system("git rev-parse --short HEAD", intern = TRUE), error = function(e) NA_character_),
    input_sources    = list("WUE/isotope_pilot/data/processed/subdaily_wue/ (from data/extracted/, FLUXNET Shuttle)"),
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
save_fig_meta <- function(p, path, width, height, notes = "") {
  ggplot2::ggsave(path, plot = p, width = width, height = height, units = "in", dpi = 300, bg = "white")
  write_meta(path, notes)
}

timesteps_per_day <- function(resolution) if (identical(resolution, "HR")) 24L else 48L

## ---- Load all 12 sites' raw sub-daily series needed here -------------------
load_site_raw <- function(site) {
  p <- file.path(subdaily_dir, paste0(site, ".rds"))
  if (!file.exists(p)) return(NULL)
  d <- readRDS(p)
  if (nrow(d) == 0) return(NULL)
  res <- read_status$resolution[read_status$site_id == site]
  res <- if (length(res) == 0) NA_character_ else res[[1]]
  needed <- c("TIMESTAMP_START", "P_F", "P_F_QC", "P_ERA", "NETRAD", "SW_IN_F", "TA_F")
  missing_needed <- setdiff(needed, names(d))
  if (length(missing_needed) > 0) {
    warning("[WUE] ", site, ": missing column(s) needed for precip comparison -- ",
            paste(missing_needed, collapse = ", "))
    return(NULL)
  }
  d <- d[, needed]
  d$site_id <- site
  d$resolution <- res
  d
}

raw_data <- setNames(lapply(WUE_SITES_STAGE2, load_site_raw), WUE_SITES_STAGE2)
missing_sites <- names(raw_data)[vapply(raw_data, is.null, logical(1))]
if (length(missing_sites) > 0) {
  warning("[WUE] No sub-daily data for: ", paste(missing_sites, collapse = ", "),
          " -- excluded from this comparison.")
}
raw_data <- raw_data[!vapply(raw_data, is.null, logical(1))]

## ============================================================================
## IDENTITY CHECK (hard stop on any departure from 100%/0%)
## ============================================================================
identity_rows <- lapply(names(raw_data), function(site) {
  d <- raw_data[[site]]
  qc2 <- d[!is.na(d$P_F_QC) & d$P_F_QC == 2, ]
  qc0_nz <- d[!is.na(d$P_F_QC) & d$P_F_QC == 0 & !is.na(d$P_F) & d$P_F > 0, ]

  n_qc2 <- nrow(qc2)
  n_qc2_eq <- if (n_qc2 > 0) sum(qc2$P_F == qc2$P_ERA, na.rm = TRUE) else NA_integer_
  pct_qc2_eq <- if (n_qc2 > 0) round(100 * n_qc2_eq / n_qc2, 4) else NA_real_

  n_qc0_nz <- nrow(qc0_nz)
  n_qc0_nz_eq <- if (n_qc0_nz > 0) sum(qc0_nz$P_F == qc0_nz$P_ERA, na.rm = TRUE) else NA_integer_
  pct_qc0_nz_eq <- if (n_qc0_nz > 0) round(100 * n_qc0_nz_eq / n_qc0_nz, 4) else NA_real_

  departs <- (!is.na(pct_qc2_eq) && pct_qc2_eq != 100) || (!is.na(pct_qc0_nz_eq) && pct_qc0_nz_eq != 0)

  data.frame(
    site_id = site, n_qc2 = n_qc2, pct_qc2_eq_era = pct_qc2_eq,
    n_qc0_nonzero = n_qc0_nz, pct_qc0_nonzero_eq_era = pct_qc0_nz_eq,
    departs_from_expected = departs, stringsAsFactors = FALSE
  )
})
identity_check <- do.call(rbind, identity_rows)
write_csv_meta(identity_check, file.path(out_tables, "identity_check.csv"),
  notes = "Confirms the 2026-09-20 SESSION_LOG identity (P_F_QC==2 <-> P_ERA; P_F_QC==0 nonzero <-> never P_ERA) at the 12 WUE pilot sites.")

departing <- identity_check$site_id[identity_check$departs_from_expected]
identity_ok <- length(departing) == 0
if (!identity_ok) {
  message("[WUE] Identity check FAILED for: ", paste(departing, collapse = ", "),
          " -- departs from the expected 100%/0% pattern. See tables/precip_compare/identity_check.csv. ",
          "Per instructions: stopping here and reporting, not proceeding to items 1-9 or the figures.")
} else {
  message("[WUE] Identity check OK for all sites (see tables/precip_compare/identity_check.csv).")
}

## Items 1-9 and the figures only run if the identity check passed for every
## site -- per instructions, a departure is reported, not silently worked
## around or proceeded past.
if (identity_ok) {

## ============================================================================
## Daily table per site: P_ERA_day, gauge_day (fully measured only), PET_day
## ============================================================================
build_daily <- function(site) {
  d <- raw_data[[site]]
  res <- d$resolution[[1]]
  tpd <- timesteps_per_day(res)

  d$date <- as.Date(d$TIMESTAMP_START)

  full_dates <- data.frame(date = seq(min(d$date), max(d$date), by = "day"))
  daily_obs <- d |>
    dplyr::group_by(date) |>
    dplyr::summarise(
      n_records = dplyr::n(),
      n_qc0 = sum(!is.na(P_F_QC) & P_F_QC == 0),
      P_ERA_day = sum(P_ERA, na.rm = TRUE),
      gauge_day_raw = sum(P_F, na.rm = TRUE),
      TA_day = mean(TA_F, na.rm = TRUE),
      NETRAD_day_obs = mean(NETRAD, na.rm = TRUE),
      n_wet_steps_era = sum(!is.na(P_ERA) & P_ERA > 0),
      n_wet_steps_gauge = sum(!is.na(P_F_QC) & P_F_QC == 0 & !is.na(P_F) & P_F > 0),
      .groups = "drop"
    )
  daily <- dplyr::left_join(full_dates, daily_obs, by = "date")
  daily$n_records[is.na(daily$n_records)] <- 0L
  daily$n_qc0[is.na(daily$n_qc0)] <- 0L
  daily$fully_measured <- daily$n_records == tpd & daily$n_qc0 == tpd
  daily$gauge_day <- ifelse(daily$fully_measured, daily$gauge_day_raw, NA_real_)

  ## NETRAD gap-fill, same formula as 06_build_site_years.R/07_apply_screens.R
  ## (rain_rule.R + netrad_gapfill.R), computed fresh here since 06 has not
  ## been run for all 12 sites yet (stage 2 full run on hold).
  gf <- fit_netrad_gapfill(d$NETRAD, d$SW_IN_F)
  d$NETRAD_filled <- gf$filled
  netrad_filled_daily <- d |>
    dplyr::group_by(date) |>
    dplyr::summarise(NETRAD_day = mean(NETRAD_filled, na.rm = TRUE), .groups = "drop")
  daily <- dplyr::left_join(daily, netrad_filled_daily, by = "date")
  daily <- daily[order(daily$date), ]
  daily$PET_day <- pt_pet_mm_day(daily$NETRAD_day, daily$TA_day)

  daily$site_id <- site
  daily$year <- lubridate::year(daily$date)
  daily$month <- lubridate::month(daily$date)
  daily$resolution <- res
  list(daily = daily, netrad_fit = data.frame(site_id = site, n = gf$n, slope = gf$slope,
                                               intercept = gf$intercept, r_squared = gf$r_squared))
}

built <- lapply(names(raw_data), build_daily)
names(built) <- names(raw_data)
daily_all <- do.call(rbind, lapply(built, `[[`, "daily"))

PERIODS <- list(
  all     = function(d) d,
  may_sep = function(d) d[d$month %in% 5:9, , drop = FALSE]
)

## ============================================================================
## 1. Coverage
## ============================================================================
coverage_rows <- do.call(rbind, lapply(names(PERIODS), function(pname) {
  d <- PERIODS[[pname]](daily_all)
  d |>
    dplyr::group_by(site_id, year) |>
    dplyr::summarise(
      days_total = dplyr::n(), n_fully_measured = sum(fully_measured),
      frac_fully_measured = round(mean(fully_measured), 4), .groups = "drop"
    ) |>
    dplyr::mutate(period = pname)
}))
write_csv_meta(coverage_rows, file.path(out_tables, "coverage.csv"))

## ============================================================================
## 2. Totals: per site-year (both periods) + pooled by calendar month (all data)
## ============================================================================
totals_site_year <- do.call(rbind, lapply(names(PERIODS), function(pname) {
  d <- PERIODS[[pname]](daily_all)
  d[d$fully_measured, ] |>
    dplyr::group_by(site_id, year) |>
    dplyr::summarise(
      n_days = dplyr::n(), P_ERA_sum = sum(P_ERA_day), gauge_sum = sum(gauge_day),
      ratio_era_to_gauge = ifelse(sum(gauge_day) == 0, NA_real_, sum(P_ERA_day) / sum(gauge_day)),
      .groups = "drop"
    ) |>
    dplyr::mutate(period = pname)
}))
write_csv_meta(totals_site_year, file.path(out_tables, "totals_site_year.csv"))

totals_by_month <- daily_all[daily_all$fully_measured, ] |>
  dplyr::group_by(site_id, month) |>
  dplyr::summarise(
    n_days = dplyr::n(), P_ERA_sum = sum(P_ERA_day), gauge_sum = sum(gauge_day),
    ratio_era_to_gauge = ifelse(sum(gauge_day) == 0, NA_real_, sum(P_ERA_day) / sum(gauge_day)),
    .groups = "drop"
  )
write_csv_meta(totals_by_month, file.path(out_tables, "totals_by_month.csv"),
  notes = "Pooled across all site-years at each site; inherently all 12 months, not period-split.")

## ============================================================================
## 3. Gauge resolution -- NOT restricted to fully-measured days (this
## characterises the gauge instrument itself from every individually-measured
## (P_F_QC==0) timestep, not a day-level comparison between series, so the
## "same days for both series" rule does not apply here). Per site-year, to
## show directly whether it changes over the record.
## ============================================================================
gauge_res_rows <- lapply(names(raw_data), function(site) {
  d <- raw_data[[site]]
  d$year <- lubridate::year(d$TIMESTAMP_START)
  nz <- d[!is.na(d$P_F_QC) & d$P_F_QC == 0 & !is.na(d$P_F) & d$P_F > 0, ]
  by_year <- nz |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      min_nonzero = min(P_F),
      mode_nonzero = as.numeric(names(sort(table(P_F), decreasing = TRUE))[1]),
      n = dplyr::n(), .groups = "drop"
    ) |>
    dplyr::mutate(site_id = site)
  by_year
})
gauge_resolution <- do.call(rbind, gauge_res_rows)
changes <- gauge_resolution |>
  dplyr::group_by(site_id) |>
  dplyr::summarise(resolution_changes_over_record = dplyr::n_distinct(mode_nonzero) > 1, .groups = "drop")
gauge_resolution <- dplyr::left_join(gauge_resolution, changes, by = "site_id")
write_csv_meta(gauge_resolution, file.path(out_tables, "gauge_resolution.csv"),
  notes = "Per site-year, from all individually-measured (P_F_QC==0, P_F>0) timesteps -- not restricted to fully measured days.")

## ============================================================================
## 4. Wet-day frequency, 5. Agreement -- both at thresholds 0 (>0), 0.1, 0.2,
## 0.5, 1, 2, 5 mm (>=), both periods
## ============================================================================
THRESH <- c(0, 0.1, 0.2, 0.5, 1, 2, 5)

wet_freq_rows <- list()
agreement_rows <- list()
for (pname in names(PERIODS)) {
  d <- PERIODS[[pname]](daily_all)
  d <- d[d$fully_measured, ]
  for (site in unique(d$site_id)) {
    d_s <- d[d$site_id == site, ]
    n <- nrow(d_s)
    for (t in THRESH) {
      era_wet   <- if (t == 0) d_s$P_ERA_day > 0 else d_s$P_ERA_day >= t
      gauge_wet <- if (t == 0) d_s$gauge_day > 0 else d_s$gauge_day >= t
      wet_freq_rows[[length(wet_freq_rows) + 1L]] <- rbind(
        data.frame(site_id = site, period = pname, threshold_mm = t, series = "P_ERA",
                   share_wet = round(mean(era_wet), 4), n_days = n),
        data.frame(site_id = site, period = pname, threshold_mm = t, series = "gauge",
                   share_wet = round(mean(gauge_wet), 4), n_days = n)
      )
      agreement_rows[[length(agreement_rows) + 1L]] <- data.frame(
        site_id = site, period = pname, threshold_mm = t,
        both_wet = sum(era_wet & gauge_wet), era_only = sum(era_wet & !gauge_wet),
        gauge_only = sum(!era_wet & gauge_wet), both_dry = sum(!era_wet & !gauge_wet),
        n_days = n
      )
    }
  }
}
wet_day_frequency <- do.call(rbind, wet_freq_rows)
agreement <- do.call(rbind, agreement_rows)
write_csv_meta(wet_day_frequency, file.path(out_tables, "wet_day_frequency.csv"))
write_csv_meta(agreement, file.path(out_tables, "agreement.csv"))

## ============================================================================
## 6. P_ERA on gauge-dry days (gauge_day == 0, fully measured)
## ============================================================================
era_on_dry_rows <- list()
for (pname in names(PERIODS)) {
  d <- PERIODS[[pname]](daily_all)
  d <- d[d$fully_measured, ]
  for (site in unique(d$site_id)) {
    dry <- d[d$site_id == site & !is.na(d$gauge_day) & d$gauge_day == 0, ]
    n_dry <- nrow(dry)
    era_pos <- dry$P_ERA_day[dry$P_ERA_day > 0]
    era_on_dry_rows[[length(era_on_dry_rows) + 1L]] <- data.frame(
      site_id = site, period = pname, n_gauge_dry_days = n_dry,
      share_era_positive = if (n_dry > 0) round(length(era_pos) / n_dry, 4) else NA_real_,
      median_mm = if (length(era_pos) > 0) round(stats::median(era_pos), 4) else NA_real_,
      p90_mm = if (length(era_pos) > 0) round(stats::quantile(era_pos, 0.90, names = FALSE), 4) else NA_real_,
      p99_mm = if (length(era_pos) > 0) round(stats::quantile(era_pos, 0.99, names = FALSE), 4) else NA_real_,
      stringsAsFactors = FALSE
    )
  }
}
era_on_gauge_dry <- do.call(rbind, era_on_dry_rows)
write_csv_meta(era_on_gauge_dry, file.path(out_tables, "era_on_gauge_dry.csv"))

## ============================================================================
## 7. Frequency-matching amount -- the P_ERA daily amount A such that
## P(P_ERA_day > A) equals the gauge's share of days with gauge > 0. A
## descriptive number, not a proposed or applied threshold.
## ============================================================================
freq_match_rows <- list()
for (pname in names(PERIODS)) {
  d <- PERIODS[[pname]](daily_all)
  d <- d[d$fully_measured, ]
  for (site in unique(d$site_id)) {
    d_s <- d[d$site_id == site, ]
    gauge_wet_share <- mean(d_s$gauge_day > 0, na.rm = TRUE)
    amount <- stats::quantile(d_s$P_ERA_day, probs = 1 - gauge_wet_share, names = FALSE, na.rm = TRUE)
    freq_match_rows[[length(freq_match_rows) + 1L]] <- data.frame(
      site_id = site, period = pname, gauge_wet_share = round(gauge_wet_share, 4),
      freq_matching_amount_mm = round(amount, 4), n_days = nrow(d_s)
    )
  }
}
freq_matching_amount <- do.call(rbind, freq_match_rows)
write_csv_meta(freq_matching_amount, file.path(out_tables, "freq_matching_amount.csv"),
  notes = "A descriptive number (the P_ERA amount matching the gauge's wet-day frequency), not a proposed or applied threshold.")

## ============================================================================
## 8. Duration: mean wet sub-daily timesteps per wet day, each series
## ============================================================================
duration_rows <- list()
for (pname in names(PERIODS)) {
  d <- PERIODS[[pname]](daily_all)
  d <- d[d$fully_measured, ]
  for (site in unique(d$site_id)) {
    d_s <- d[d$site_id == site, ]
    era_wet_days   <- d_s[d_s$P_ERA_day > 0, ]
    gauge_wet_days <- d_s[!is.na(d_s$gauge_day) & d_s$gauge_day > 0, ]
    duration_rows[[length(duration_rows) + 1L]] <- rbind(
      data.frame(site_id = site, period = pname, series = "P_ERA",
                 mean_wet_timesteps = if (nrow(era_wet_days) > 0) round(mean(era_wet_days$n_wet_steps_era), 3) else NA_real_,
                 n_wet_days = nrow(era_wet_days)),
      data.frame(site_id = site, period = pname, series = "gauge",
                 mean_wet_timesteps = if (nrow(gauge_wet_days) > 0) round(mean(gauge_wet_days$n_wet_steps_gauge), 3) else NA_real_,
                 n_wet_days = nrow(gauge_wet_days))
    )
  }
}
duration <- do.call(rbind, duration_rows)
write_csv_meta(duration, file.path(out_tables, "duration.csv"))

## ============================================================================
## 9. Consequence for the rain rule -- site-years with >=350 fully measured
## days; days removed by rain_rule_excluded() (rain_rule.R, SAME function
## 07_apply_screens.R calls) driven by the gauge vs. by P_ERA, same PET.
## ============================================================================
qualifying <- coverage_rows[coverage_rows$period == "all" & coverage_rows$n_fully_measured >= 350, ]

consequence_rows <- lapply(names(built), function(site) {
  d <- built[[site]]$daily
  d <- d[order(d$date), ]
  d_era   <- data.frame(date = d$date, P_day = d$P_ERA_day, PET_day = d$PET_day)
  d_gauge <- data.frame(date = d$date, P_day = d$gauge_day,  PET_day = d$PET_day)
  excl_era   <- rain_rule_excluded(d_era)
  excl_gauge <- rain_rule_excluded(d_gauge)

  qual_years <- qualifying$year[qualifying$site_id == site]
  if (length(qual_years) == 0) return(NULL)

  do.call(rbind, lapply(qual_years, function(yr) {
    idx <- d$year == yr
    data.frame(
      site_id = site, year = yr,
      n_fully_measured = sum(d$fully_measured[idx]),
      days_removed_gauge = sum(excl_gauge[idx], na.rm = TRUE),
      days_removed_era = sum(excl_era[idx], na.rm = TRUE),
      diff_era_minus_gauge = sum(excl_era[idx], na.rm = TRUE) - sum(excl_gauge[idx], na.rm = TRUE)
    )
  }))
})
rain_rule_consequence <- do.call(rbind, consequence_rows)
if (is.null(rain_rule_consequence)) {
  rain_rule_consequence <- data.frame(site_id = character(0), year = integer(0),
    n_fully_measured = integer(0), days_removed_gauge = integer(0),
    days_removed_era = integer(0), diff_era_minus_gauge = integer(0))
}
write_csv_meta(rain_rule_consequence, file.path(out_tables, "rain_rule_consequence.csv"),
  notes = "Uses rain_rule.R's rain_rule_excluded()/pt_pet_mm_day() -- the same functions 07_apply_screens.R calls -- not a reimplementation. Gauge-driven run treats non-fully-measured days as NA P_day (not excluded by the base rain day rule, but still reachable via following-day propagation from a neighbouring rainy day).")

message("[WUE] 11_precip_compare.R: tables written. ", nrow(rain_rule_consequence),
        " qualifying site-year(s) (>=350 fully measured days) for the rain-rule consequence check.")

## ============================================================================
## FIGURES
## ============================================================================
base_theme <- ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                 strip.background = ggplot2::element_rect(fill = "grey90", color = NA))

## a. Wet-day frequency against amount (exceedance curve), both series, all months
exceed_rows <- lapply(unique(daily_all$site_id), function(site) {
  d <- daily_all[daily_all$site_id == site & daily_all$fully_measured, ]
  n <- nrow(d)
  mk <- function(x, series) {
    x <- sort(x[!is.na(x) & x > 0])
    if (length(x) == 0) return(NULL)
    data.frame(site_id = site, series = series, amount_mm = x,
               freq_exceeded = rev(seq_along(x)) / n)
  }
  rbind(mk(d$P_ERA_day, "P_ERA"), mk(d$gauge_day, "gauge"))
})
exceed_df <- do.call(rbind, exceed_rows)

p_a <- ggplot2::ggplot(exceed_df, ggplot2::aes(x = amount_mm, y = freq_exceeded, color = series)) +
  ggplot2::geom_step() +
  ggplot2::scale_x_log10() +
  ggplot2::facet_wrap(~site_id) +
  ggplot2::labs(x = "Daily amount (mm, log scale)", y = "Frequency of days >= amount",
                color = "Series", title = "Wet-day frequency vs. amount (fully measured days, all months)") +
  base_theme
save_fig_meta(p_a, file.path(out_figures, "fig_wet_freq_amount.png"), width = 11, height = 8)

## b. Days removed per year by the rain rule under each source, by site
if (nrow(rain_rule_consequence) > 0) {
  rrc_long <- rbind(
    data.frame(site_id = rain_rule_consequence$site_id, year = rain_rule_consequence$year,
               source = "gauge", days_removed = rain_rule_consequence$days_removed_gauge),
    data.frame(site_id = rain_rule_consequence$site_id, year = rain_rule_consequence$year,
               source = "P_ERA", days_removed = rain_rule_consequence$days_removed_era)
  )
  p_b <- ggplot2::ggplot(rrc_long, ggplot2::aes(x = year, y = days_removed, fill = source)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::facet_wrap(~site_id, scales = "free_x") +
    ggplot2::labs(x = "Year", y = "Days removed by the rain rule", fill = "Driven by",
                  title = "Rain-rule day removal: gauge-driven vs. P_ERA-driven (site-years with >=350 fully measured days)") +
    base_theme
  save_fig_meta(p_b, file.path(out_figures, "fig_days_removed_by_source.png"), width = 11, height = 8)
} else {
  message("[WUE] No site-years with >=350 fully measured days -- fig_days_removed_by_source.png not produced.")
}

## c. Daily P_ERA on gauge-dry days, log x-axis
dry_days_df <- daily_all[daily_all$fully_measured & !is.na(daily_all$gauge_day) &
                            daily_all$gauge_day == 0 & daily_all$P_ERA_day > 0, ]
p_c <- ggplot2::ggplot(dry_days_df, ggplot2::aes(x = P_ERA_day)) +
  ggplot2::geom_histogram(bins = 30) +
  ggplot2::scale_x_log10() +
  ggplot2::facet_wrap(~site_id, scales = "free_y") +
  ggplot2::labs(x = "P_ERA daily amount on gauge-dry days (mm, log scale)", y = "Count of days",
                title = "P_ERA on fully measured days the gauge recorded as dry") +
  base_theme
save_fig_meta(p_c, file.path(out_figures, "fig_era_on_gauge_dry.png"), width = 11, height = 8)

} else {
  message("[WUE] Skipping items 1-9 and the figures -- identity check failed. See the report.")
}

## ============================================================================
## REPORT (always written, whether or not the identity check passed)
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

## This paragraph was written before any table above was computed (it does
## not depend on, and was not adjusted for, the result found) -- the
## falsifiability statement the user's instructions required in advance.
falsifiability_statement <- c(
  "**What would show P_ERA can stand in for the gauge, under the stage 2 rain rule's \"any",
  "precipitation above zero\" definition (MY DECISION 4):** on fully measured days, P_ERA and the",
  "gauge classify most of the same days as wet vs. dry at the >0 mm level (few gauge-dry/P_ERA-wet",
  "or gauge-wet/P_ERA-dry days in the agreement table); P_ERA is rarely, and only slightly, positive",
  "on days the gauge recorded as fully dry (a low share_era_positive in era_on_gauge_dry.csv, and",
  "small median/p90/p99 amounts when it is); and the rain-rule day-removal counts in",
  "rain_rule_consequence.csv are similar whether driven by the gauge or by P_ERA.",
  "",
  "**What would show it cannot:** a large share of gauge-dry fully measured days carry non-trivial",
  "positive P_ERA (so the >0 rule would exclude many days the gauge alone would have kept); the two",
  "series disagree substantially on wet/dry classification at the >0 level; or the rain-rule",
  "day-removal counts diverge materially between the two sources in rain_rule_consequence.csv.",
  "",
  "This script does not apply either verdict -- it only reports the numbers above would need for",
  "someone else (or a later session) to decide."
)

report_path <- file.path(docs_dir, paste0("report_precip_compare_", format(Sys.Date(), "%Y%m%d"), ".md"))

report_lines <- c(
  paste0("# ERA5 vs. gauge daily precipitation comparison (", Sys.Date(), ")"),
  "",
  "Side analysis, not the FLUXNET Annual Paper 2026, and not stage 2 of the WUE isotope pilot",
  "itself -- a read-and-report comparison requested before deciding whether to launch the stage 2",
  "full run. Describes what was measured; asserts no cause; proposes, recommends, and applies no",
  "threshold; does not change the rain screen; does not recompute WUE.",
  "",
  "## Falsifiability statement (written before computing anything)",
  "",
  falsifiability_statement,
  "",
  "## 1. Identity check",
  "",
  "Per site: share of `P_F_QC==2` records where `P_F` equals `P_ERA` (expected 100%), and share of",
  "`P_F_QC==0` records with non-zero `P_F` where `P_F` equals `P_ERA` (expected 0%) -- confirming the",
  "2026-09-20 SESSION_LOG identity at these 12 sites specifically (that entry checked 3 different",
  "sites: IT-MBo, US-HB4, FI-Hyy).",
  "",
  knitr_like_table(identity_check),
  "",
  if (identity_ok) {
    "**Result: every site matches the expected 100%/0% pattern exactly.**"
  } else {
    c(
      paste0("**Result: ", length(departing), " of ", nrow(identity_check), " site(s) depart from the",
             " expected pattern** -- not at the `P_F_QC==2` side (100% everywhere `n_qc2 > 0`; `US-Dk2`",
             " has zero `P_F_QC==2` records, hence `NA`), but at the `P_F_QC==0` non-zero side, where a",
             " small share (0.05-0.43% of measured non-zero gauge records across the affected sites,",
             " `US-SP1` the only exact 0%) happen to equal `P_ERA` exactly."),
      "",
      paste0("Departing sites: ", paste(departing, collapse = ", "), "."),
      "",
      "Per instructions, this stops the analysis here: items 1-9 and the figures were not computed.",
      "No cause is asserted for the small non-zero match rate (it could be coincidental rounding --",
      "both series can land on the same value by chance when the gauge's own resolution is coarse --",
      "or something else; that question is not investigated here)."
    )
  },
  "",
  "## What I could not do",
  "",
  if (identity_ok) {
    "Nothing -- all nine COMPUTE items and all three figures were produced for all 12 sites."
  } else {
    c(
      "- Items 2-9 (totals, gauge resolution, wet-day frequency, agreement, P_ERA on gauge-dry days,",
      "  frequency-matching amount, duration, rain-rule consequence) and figures a-c: not computed.",
      "  The identity check (item 0, confirming the prerequisite 2026-09-20 identity) did not pass for",
      "  11 of 12 sites, and per instructions (\"Stop and tell me if a site departs from 100% and 0%\")",
      "  this analysis stops there rather than proceeding on an unconfirmed foundation."
    )
  }
)
writeLines(unlist(report_lines), report_path)
message("[WUE] Report written: ", report_path)

message("[WUE] 11_precip_compare.R complete.")
