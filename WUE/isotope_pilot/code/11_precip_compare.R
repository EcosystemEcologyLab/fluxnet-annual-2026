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

timesteps_per_day <- function(resolution) {
  if (identical(resolution, "HH")) return(48L)
  if (identical(resolution, "HR")) return(24L)
  stop("[WUE] timesteps_per_day(): unrecognised resolution '", resolution,
       "' -- expected exactly 'HH' or 'HR', no default.")
}

## ---- Load all 12 sites' raw sub-daily series needed here -------------------
load_site_raw <- function(site) {
  p <- file.path(subdaily_dir, paste0(site, ".rds"))
  if (!file.exists(p)) return(NULL)
  d <- readRDS(p)
  if (nrow(d) == 0) return(NULL)
  res <- read_status$resolution[read_status$site_id == site]
  res <- if (length(res) == 0) NA_character_ else res[[1]]
  needed <- c("TIMESTAMP_START", "P_F", "P_F_QC", "P_ERA", "NETRAD", "SW_IN_F", "TA_F", "PA_F")
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
## IDENTITY CHECK -- revised 2026-10-08: the exact-match gate (100%/0%) was
## too strict (see docs/report_precip_compare_20261007.md). A site now PASSES
## when the P_F_QC==2 side is 100% (exact -- that side is a QC-convention
## identity, not a coincidence question) AND the P_F_QC==0 non-zero side is
## <= 1% (not exactly 0% -- small coincidental matches, e.g. at a coarse
## gauge resolution, are expected and tolerated up to this bound). A site
## that fails either condition is EXCLUDED from items 1-9 and the figures
## below and named in the report; passing sites continue -- no longer an
## all-or-nothing gate.
## ============================================================================
top_n_values <- function(x, n = 5) {
  if (length(x) == 0) return("")
  tab <- sort(table(x), decreasing = TRUE)
  tab <- tab[seq_len(min(n, length(tab)))]
  paste(sprintf("%s:%d", names(tab), as.integer(tab)), collapse = ", ")
}

identity_rows <- lapply(names(raw_data), function(site) {
  d <- raw_data[[site]]
  qc2 <- d[!is.na(d$P_F_QC) & d$P_F_QC == 2, ]
  qc0_nz <- d[!is.na(d$P_F_QC) & d$P_F_QC == 0 & !is.na(d$P_F) & d$P_F > 0, ]

  n_qc2 <- nrow(qc2)
  n_qc2_eq <- if (n_qc2 > 0) sum(qc2$P_F == qc2$P_ERA, na.rm = TRUE) else NA_integer_
  pct_qc2_eq <- if (n_qc2 > 0) round(100 * n_qc2_eq / n_qc2, 4) else NA_real_

  n_qc0_nz <- nrow(qc0_nz)
  matching <- qc0_nz[qc0_nz$P_F == qc0_nz$P_ERA, ]
  n_qc0_nz_eq <- if (n_qc0_nz > 0) nrow(matching) else NA_integer_
  pct_qc0_nz_eq <- if (n_qc0_nz > 0) round(100 * n_qc0_nz_eq / n_qc0_nz, 4) else NA_real_

  pass_qc2 <- is.na(pct_qc2_eq) || pct_qc2_eq == 100
  pass_qc0 <- is.na(pct_qc0_nz_eq) || pct_qc0_nz_eq <= 1
  site_pass <- pass_qc2 && pass_qc0

  data.frame(
    site_id = site, n_qc2 = n_qc2, pct_qc2_eq_era = pct_qc2_eq,
    n_qc0_nonzero = n_qc0_nz, pct_qc0_nonzero_eq_era = pct_qc0_nz_eq,
    top5_matching_values = top_n_values(matching$P_F, 5),
    passes_identity_gate = site_pass, stringsAsFactors = FALSE
  )
})
identity_check <- do.call(rbind, identity_rows)
write_csv_meta(identity_check, file.path(out_tables, "identity_check.csv"),
  notes = "Revised 2026-10-08: pass = P_F_QC==2 side exactly 100% AND P_F_QC==0 nonzero side <= 1% (was an exact 100%/0% gate). A failing site is excluded from items 1-9 and the figures, not a global stop.")

passing_sites <- identity_check$site_id[identity_check$passes_identity_gate]
failing_sites <- identity_check$site_id[!identity_check$passes_identity_gate]
if (length(failing_sites) > 0) {
  message("[WUE] Identity gate: excluded ", paste(failing_sites, collapse = ", "),
          " (see tables/precip_compare/identity_check.csv). Continuing with: ",
          paste(passing_sites, collapse = ", "), ".")
} else {
  message("[WUE] Identity gate: all sites pass (see tables/precip_compare/identity_check.csv).")
}
raw_data <- raw_data[passing_sites]

## Pre-initialised so the always-runs report section below can check these
## with is.null() rather than exists(), whether or not any site passed.
era_missing_by_site_year <- NULL
era_missing_vs_years_dropped <- NULL
rain_rule_consequence <- NULL

## Items 1-9 and the figures run on whichever sites passed the identity gate.
if (length(passing_sites) > 0) {

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
      n_era_present = sum(!is.na(P_ERA)),
      P_ERA_day_raw = sum(P_ERA, na.rm = TRUE),
      gauge_day_raw = sum(P_F, na.rm = TRUE),
      TA_day = mean(TA_F, na.rm = TRUE),
      NETRAD_day_obs = mean(NETRAD, na.rm = TRUE),
      PA_day = mean(PA_F, na.rm = TRUE),
      n_wet_steps_era = sum(!is.na(P_ERA) & P_ERA > 0),
      n_wet_steps_gauge = sum(!is.na(P_F_QC) & P_F_QC == 0 & !is.na(P_F) & P_F > 0),
      .groups = "drop"
    )
  daily <- dplyr::left_join(full_dates, daily_obs, by = "date")
  daily$n_records[is.na(daily$n_records)] <- 0L
  daily$n_qc0[is.na(daily$n_qc0)] <- 0L
  daily$n_era_present[is.na(daily$n_era_present)] <- 0L
  ## Daily P_ERA is missing (NA) unless P_ERA is present at EVERY expected
  ## timestep that day -- previously summed with na.rm = TRUE, so a day with
  ## no P_ERA at all silently read as a 0 mm (dry) day. Revised 2026-10-08.
  daily$P_ERA_day <- ifelse(daily$n_era_present == tpd, daily$P_ERA_day_raw, NA_real_)
  ## "fully measured" now also requires P_ERA present at every timestep, not
  ## just P_F_QC == 0 at every timestep -- both series must be fully defined
  ## on the same days.
  daily$fully_measured <- daily$n_records == tpd & daily$n_qc0 == tpd & daily$n_era_present == tpd
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
  ## Same PET call shape as 07_apply_screens.R: daily mean PA_F for the
  ## psychrometric constant, falling back to 101.3 kPa only where PA_F is
  ## missing (rain_rule.R's pt_pet_mm_day(), revised 2026-10-08).
  daily$PET_day <- pt_pet_mm_day(daily$NETRAD_day, daily$TA_day, daily$PA_day)

  daily$site_id <- site
  daily$year <- lubridate::year(daily$date)
  daily$month <- lubridate::month(daily$date)
  daily$resolution <- res

  era_missing <- d |>
    dplyr::mutate(year = lubridate::year(date)) |>
    dplyr::group_by(year) |>
    dplyr::summarise(n_timesteps = dplyr::n(), n_era_missing = sum(is.na(P_ERA)), .groups = "drop") |>
    dplyr::mutate(site_id = site, frac_era_missing = round(n_era_missing / n_timesteps, 5))

  list(daily = daily, netrad_fit = data.frame(site_id = site, n = gf$n, slope = gf$slope,
                                               intercept = gf$intercept, r_squared = gf$r_squared),
       era_missing = era_missing)
}

built <- lapply(names(raw_data), build_daily)
names(built) <- names(raw_data)
daily_all <- do.call(rbind, lapply(built, `[[`, "daily"))

## ---- Missing-P_ERA timesteps by site and year (MY DECISION: leave 2026 out
## -- it is an incomplete current year regardless of this question) ---------
era_missing_by_site_year <- do.call(rbind, lapply(built, `[[`, "era_missing"))
era_missing_by_site_year <- era_missing_by_site_year[era_missing_by_site_year$year != 2026, ]
era_missing_by_site_year <- era_missing_by_site_year[, c("site_id", "year", "n_timesteps", "n_era_missing", "frac_era_missing")]
write_csv_meta(era_missing_by_site_year, file.path(out_tables, "era_missing_timesteps.csv"),
  notes = "Per site-year count of sub-daily timesteps with P_ERA == NA. 2026 excluded (incomplete current year regardless).")

## Cross-check: any of these site-years already KEPT by years_dropped.csv
## (i.e. not listed there as dropped)? years_dropped.csv currently only
## reflects the 3 stage-2 smoke-test sites (06 has not run for the other 9 --
## stage 2 full run on hold) -- reported honestly as "not yet assessed" for
## those, not silently treated as "kept".
years_dropped_path <- file.path(WUE_ROOT, "tables", "years_dropped.csv")
era_missing_vs_years_dropped <- NULL
if (file.exists(years_dropped_path)) {
  years_dropped <- readr::read_csv(years_dropped_path, show_col_types = FALSE)
  assessed_sites <- unique(years_dropped$site_id)
  has_missing <- era_missing_by_site_year[era_missing_by_site_year$n_era_missing > 0, ]
  ## vapply (not mapply) so a zero-row has_missing still yields a correctly-
  ## typed logical(0), not an empty list that breaks case_when() below.
  stage2_assessed <- has_missing$site_id %in% assessed_sites
  dropped_by_years_dropped <- vapply(seq_len(nrow(has_missing)), function(i) {
    any(years_dropped$site_id == has_missing$site_id[i] & years_dropped$year == has_missing$year[i])
  }, logical(1))
  era_missing_vs_years_dropped <- has_missing
  era_missing_vs_years_dropped$stage2_assessed <- stage2_assessed
  era_missing_vs_years_dropped$dropped_by_years_dropped <- dropped_by_years_dropped
  era_missing_vs_years_dropped$status <- dplyr::case_when(
    !stage2_assessed ~ "not yet assessed (06 not run for this site)",
    dropped_by_years_dropped ~ "dropped by years_dropped.csv",
    TRUE ~ "KEPT by years_dropped.csv -- has missing P_ERA timesteps"
  )
  write_csv_meta(era_missing_vs_years_dropped, file.path(out_tables, "era_missing_vs_years_dropped.csv"),
    notes = "Site-years with >=1 missing P_ERA timestep, cross-checked against tables/years_dropped.csv (2026 excluded).")
}

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
##
## Revised 2026-10-08 (same days for both sources): gauge_day is already NA
## wherever a day is not fully measured (by construction above); P_ERA is
## now ALSO set to NA on exactly those same days before either series is
## passed to rain_rule_excluded(), so the two runs see an identical
## missingness pattern -- a day can still be excluded by propagation from a
## neighbouring rainy day even where its own P_day is NA (same behaviour
## rain_rule_excluded() already has for any NA day). Days removed are then
## counted only among fully measured days, so the comparison itself is also
## restricted to days where both series are actually defined.
## ============================================================================
qualifying <- coverage_rows[coverage_rows$period == "all" & coverage_rows$n_fully_measured >= 350, ]

consequence_rows <- lapply(names(built), function(site) {
  d <- built[[site]]$daily
  d <- d[order(d$date), ]
  era_P_day_shared <- ifelse(is.na(d$gauge_day), NA_real_, d$P_ERA_day)
  d_era   <- data.frame(date = d$date, P_day = era_P_day_shared, PET_day = d$PET_day)
  d_gauge <- data.frame(date = d$date, P_day = d$gauge_day,      PET_day = d$PET_day)
  excl_era   <- rain_rule_excluded(d_era)
  excl_gauge <- rain_rule_excluded(d_gauge)

  qual_years <- qualifying$year[qualifying$site_id == site]
  if (length(qual_years) == 0) return(NULL)

  do.call(rbind, lapply(qual_years, function(yr) {
    idx <- d$year == yr & d$fully_measured
    data.frame(
      site_id = site, year = yr,
      n_fully_measured = sum(d$fully_measured[d$year == yr]),
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
  notes = "Revised 2026-10-08: both sources share the same missing-day pattern (P_ERA set to NA wherever the gauge day is not fully measured) and days_removed_* count only fully measured days. Uses rain_rule.R's rain_rule_excluded()/pt_pet_mm_day() -- the same functions 07_apply_screens.R calls -- not a reimplementation.")

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
  message("[WUE] No site passed the identity gate -- items 1-9 and the figures not produced. See the report.")
}

## ============================================================================
## Per-period, per-site summary table for the report (not a new tables/
## output -- the instruction was to keep the existing tables as they are;
## this is assembled from them for the report text only).
## ============================================================================
build_period_summary <- function(pname) {
  sites <- names(built)
  do.call(rbind, lapply(sites, function(site) {
    cov <- sum(coverage_rows$n_fully_measured[coverage_rows$period == pname & coverage_rows$site_id == site])
    sgw <- wet_day_frequency$share_wet[wet_day_frequency$period == pname & wet_day_frequency$site_id == site &
                                          wet_day_frequency$threshold_mm == 0 & wet_day_frequency$series == "gauge"]
    sew <- wet_day_frequency$share_wet[wet_day_frequency$period == pname & wet_day_frequency$site_id == site &
                                          wet_day_frequency$threshold_mm == 0 & wet_day_frequency$series == "P_ERA"]
    dry <- era_on_gauge_dry[era_on_gauge_dry$period == pname & era_on_gauge_dry$site_id == site, ]
    fm  <- freq_matching_amount$freq_matching_amount_mm[freq_matching_amount$period == pname &
                                                            freq_matching_amount$site_id == site]
    tot <- totals_site_year[totals_site_year$period == pname & totals_site_year$site_id == site, ]
    ratio <- if (nrow(tot) > 0 && sum(tot$gauge_sum) != 0) round(sum(tot$P_ERA_sum) / sum(tot$gauge_sum), 4) else NA_real_

    mean_removed_gauge <- NA_real_; mean_removed_era <- NA_real_
    if (pname == "all") {
      rrc_s <- rain_rule_consequence[rain_rule_consequence$site_id == site, ]
      if (nrow(rrc_s) > 0) {
        mean_removed_gauge <- round(mean(rrc_s$days_removed_gauge), 2)
        mean_removed_era   <- round(mean(rrc_s$days_removed_era), 2)
      }
    }

    data.frame(
      site_id = site, fully_measured_days = cov,
      share_gauge_wet = if (length(sgw)) sgw else NA_real_,
      share_era_wet   = if (length(sew)) sew else NA_real_,
      share_era_positive_on_gauge_dry = if (nrow(dry)) dry$share_era_positive else NA_real_,
      median_era_on_gauge_dry_mm = if (nrow(dry)) dry$median_mm else NA_real_,
      p90_era_on_gauge_dry_mm = if (nrow(dry)) dry$p90_mm else NA_real_,
      freq_matching_amount_mm = if (length(fm)) fm else NA_real_,
      ratio_era_to_gauge = ratio,
      mean_days_removed_gauge = mean_removed_gauge,
      mean_days_removed_era = mean_removed_era,
      stringsAsFactors = FALSE
    )
  }))
}
if (length(passing_sites) > 0) {
  summary_all     <- build_period_summary("all")
  summary_may_sep <- build_period_summary("may_sep")
} else {
  summary_all <- summary_may_sep <- NULL
}

## ============================================================================
## REPORT (always written, whether or not every site passed the identity gate)
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
## Unchanged from the 2026-10-07 run (today's revision changes the identity
## gate's strictness and a few counting details, not what would count as
## support or refutation).
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

n_missing_total <- if (!is.null(era_missing_by_site_year)) sum(era_missing_by_site_year$n_era_missing) else NA_integer_
kept_with_missing <- if (!is.null(era_missing_vs_years_dropped)) {
  era_missing_vs_years_dropped[era_missing_vs_years_dropped$status ==
    "KEPT by years_dropped.csv -- has missing P_ERA timesteps", ]
} else NULL
not_assessed_with_missing <- if (!is.null(era_missing_vs_years_dropped)) {
  era_missing_vs_years_dropped[era_missing_vs_years_dropped$status ==
    "not yet assessed (06 not run for this site)", ]
} else NULL

report_lines <- c(
  paste0("# ERA5 vs. gauge daily precipitation comparison (", Sys.Date(), ")"),
  "",
  "Side analysis, not the FLUXNET Annual Paper 2026, and not stage 2 of the WUE isotope pilot",
  "itself -- a read-and-report comparison requested before deciding whether to launch the stage 2",
  "full run. Describes what was measured; asserts no cause; proposes, recommends, and applies no",
  "threshold; does not change the rain screen (MY DECISION 4, any P_ERA above zero, is unchanged);",
  "does not recompute WUE. Revises the 2026-10-07 run of this same script",
  "(`docs/report_precip_compare_20261007.md`), which stopped entirely at an exact-match identity",
  "gate -- the user's own instruction, found too strict, now relaxed as described below.",
  "",
  "## Falsifiability statement (written before computing anything)",
  "",
  falsifiability_statement,
  "",
  "## 1. Identity check",
  "",
  "Per site: share of `P_F_QC==2` records where `P_F` equals `P_ERA` (expected 100%), share of",
  "`P_F_QC==0` records with non-zero `P_F` where `P_F` equals `P_ERA` (expected <= 1%, not exactly",
  "0% -- the 2026-10-07 run's exact-0% requirement was too strict), and the 5 most frequent matching",
  "values where `P_F_QC==0` non-zero matches occur. **A site passes when the `P_F_QC==2` side is",
  "exactly 100% AND the `P_F_QC==0` non-zero side is <= 1%; a failing site is excluded from items 1-9",
  "and the figures below (not a global stop) and named here.**",
  "",
  knitr_like_table(identity_check),
  "",
  if (length(failing_sites) == 0) {
    "**Result: every site passes.**"
  } else {
    paste0("**Result: ", length(failing_sites), " of ", nrow(identity_check), " site(s) fail and are",
           " excluded from everything below: ", paste(failing_sites, collapse = ", "), ".** Passing: ",
           paste(passing_sites, collapse = ", "), ".")
  },
  "",
  "## Missing P_ERA timesteps",
  "",
  "Daily `P_ERA` was previously summed with `na.rm = TRUE`, so a day with no `P_ERA` at all silently",
  "read as a 0 mm (dry) day. Revised: a day's `P_ERA` is now `NA` unless `P_ERA` is present at every",
  "expected timestep, and that is now also required for \"fully measured\" (in addition to the",
  "existing `P_F_QC == 0` requirement). Full counts by site and year (2026 excluded):",
  paste0("`tables/precip_compare/era_missing_timesteps.csv` (",
         if (is.na(n_missing_total)) "not computed (no site passed the identity gate)" else
           paste0(n_missing_total, " missing timestep(s) total across passing sites"), ")."),
  "",
  if (!is.null(kept_with_missing) && nrow(kept_with_missing) > 0) {
    c(
      paste0("**", nrow(kept_with_missing), " site-year(s) kept by `years_dropped.csv` have >=1",
             " missing P_ERA timestep:**"),
      "",
      knitr_like_table(kept_with_missing[, c("site_id", "year", "n_timesteps", "n_era_missing", "frac_era_missing")])
    )
  } else if (!is.null(era_missing_vs_years_dropped)) {
    "No site-year kept by `years_dropped.csv` has a missing P_ERA timestep."
  } else {
    "Not checked (no site passed the identity gate)."
  },
  "",
  if (!is.null(not_assessed_with_missing) && nrow(not_assessed_with_missing) > 0) {
    paste0("(", nrow(not_assessed_with_missing), " further site-year(s) with missing P_ERA belong to",
           " sites `years_dropped.csv` has not yet assessed -- stage 2's `06_build_site_years.R` has",
           " only run for the 3 smoke-test sites; the stage 2 full run remains on hold.)")
  } else "",
  "",
  "## Per-site summary, all months",
  "",
  "One row per site passing the identity gate. `fully_measured_days` pools all years;",
  "`mean_days_removed_*` covers only site-years with >=350 fully measured days (`rain_rule_consequence.csv`).",
  "",
  knitr_like_table(summary_all),
  "",
  "## Per-site summary, May to September",
  "",
  "Same columns, restricted to fully measured days with month in 5:9. `mean_days_removed_*` is an",
  "all-year quantity (item 9 was not computed by period) and is omitted here.",
  "",
  knitr_like_table(if (!is.null(summary_may_sep)) summary_may_sep[, setdiff(names(summary_may_sep),
    c("mean_days_removed_gauge", "mean_days_removed_era"))] else NULL),
  "",
  "## Figures",
  "",
  "- `figures/precip_compare/fig_wet_freq_amount.png` -- (a) wet-day frequency vs. amount, both",
  "  series, one panel per site (fully measured days, all months).",
  "- `figures/precip_compare/fig_days_removed_by_source.png` -- (b) days removed per year by the",
  "  rain rule under each source, by site (site-years with >=350 fully measured days).",
  "- `figures/precip_compare/fig_era_on_gauge_dry.png` -- (c) daily P_ERA on gauge-dry days, log",
  "  x-axis, one panel per site.",
  if (length(passing_sites) == 0) "  (None of the three were produced -- no site passed the identity gate.)" else NULL,
  "",
  "## What I could not do",
  "",
  if (length(failing_sites) == 0) {
    "Nothing -- all nine COMPUTE items, both per-period summaries, and all three figures were produced for all 12 sites."
  } else {
    c(
      paste0("- Items 2-9 and the figures were not computed for: ", paste(failing_sites, collapse = ", "),
             " -- excluded by the revised identity gate (`P_F_QC==0` non-zero match rate > 1%)."),
      if (length(passing_sites) == 0)
        "- No site passed the identity gate, so nothing beyond the identity check itself was produced."
      else NULL
    )
  }
)
writeLines(unlist(report_lines), report_path)
message("[WUE] Report written: ", report_path)

message("[WUE] 11_precip_compare.R complete.")
