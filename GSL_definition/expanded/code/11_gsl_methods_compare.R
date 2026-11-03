## 11_gsl_methods_compare.R — GSL expanded analysis: preliminary comparison
## of EIGHT published/derived GSL definitions (M1-M8) at the 15 sites of the
## draft "Figure GSL-IAV" (tables/iav_15sites.csv). VERSION 3 -- revises
## version 2 (committed at e0348a5; logs/prompts/2026-11-02_gsl_methods_compare_v2.md)
## per Dave's follow-up decisions, logs/prompts/2026-11-02_gsl_methods_compare_v3.md.
## Purpose, per Tash: show that published GSL definitions span a wide range of
## low thresholds/rules, so NO single definition (Z16 included) is the
## reference -- figures and this script's own messages/labels deliberately
## never frame the comparison as "Z16 vs T85/T95".
##
## Nine items implemented:
##   1. M4 targets 0.85/0.90/0.95/0.99 (0.85 = Tash's current method); M2 on
##      Tash's full 22-value grid (0.1, 0.5, 1-20% of P99), keeping 5/10/20%
##      in the main tables; the dot plot shows M2 as a min-max RANGE across
##      the 22-value grid, not a single point+SD.
##   2. Two new methods on 15-day-smoothed GPP, each per series (NT, DT): M7
##      (amplitude/derivative, Ma et al. 2013) and M8 (normalized threshold,
##      Panwar et al. 2023).
##   3. M2-M4 (count methods) now report THREE start/end rules side by side:
##      (a) first/last qualifying day (unchanged, the original gsl/start_date/
##      end_date columns); (b) first/last run of >=5 consecutive days (our own
##      choice, not from the literature) -- start_date_run5/end_date_run5; (c)
##      threshold crossings of 15-day-smoothed GPP closest to the seasonal peak
##      (Panwar's rule) -- start_date_smpeak/end_date_smpeak.
##   4. M6 reported in two forms: "anchored" (v2, search +/-30d of M5-span) and
##      "unanchored" (v1's own window/search logic, ported verbatim from
##      15dc51f). Figures use the anchored form only.
##   5. Method-agreement table adds median absolute start/end-date difference
##      (days) per method pair, alongside Spearman r.
##   6. NT-vs-DT start/end shifts, signed, by site and by vegetation group
##      (sites.csv's `group` column), checked against Keenan et al. (2019)'s
##      forest early-spring NT-overestimate prediction.
##   7. New tables/site_management_15.csv: BADM (data/snapshots/
##      badm_management_coverage.csv) + literature overrides (AT-Neu mowing,
##      Hammerle et al. 2008; US-Var grazing, FLUXNET site page). US-SRM/
##      US-Wkg grazing is BADM-confirmed (DOM_DIST_MGMT = "Grazing"), so
##      reported as confirmed, not "unconfirmed" (the prompt's fallback only
##      applies if BADM does NOT confirm it).
##   8. AT-Neu two-season years tested against the mowing calendar (secondary
##      smoothed-GPP peak 2-4 weeks after a cut window's end).
##   9. AU-How: M7 GSL vs. Ma et al. (2013)'s 244-275 d; wet-season (15 Oct-15
##      Apr, Moore et al. 2017) GPP and smoothed-GPP seasonal amplitude, with
##      interannual SD; AU-How flagged as a site where threshold GSL carries
##      no interannual signal, with the cross-method SD of its mean GSL.
##
## Exploratory. Does NOT modify any existing script, table or figure other
## than this script's OWN previous outputs (regenerated in place under the
## same filenames where carried over from v2; v2's content lives on in git
## history at e0348a5). Writes only to this side project's own
## expanded/logs/ (log_exclusion()/log_unknown()), never the repo-root
## outputs/ logs. Read-only everywhere else: a fresh DBI pull from the
## repo-root DuckDB store (dataset = 'FLUXMET' only), the already-committed
## expanded tables, and data/snapshots/badm_management_coverage.csv (item 7).
source("GSL_definition/expanded/code/00_config.R")
options(gsl.skip_driver = TRUE)
source("GSL_definition/code/03_derive.R") # FINE_GRID_PCT only, reused for M4
options(gsl.skip_driver = FALSE)

suppressPackageStartupMessages({
  library(DBI); library(duckdb); library(dplyr); library(tidyr); library(readr)
  library(lubridate); library(purrr); library(ggplot2); library(patchwork)
  library(ggrepel); library(scales)
})

## ---- This script's own log paths, inside expanded/ (never GSL_EXCLUSION_LOG/
## GSL_UNKNOWN_LOG, which point at the 62-site GSL_definition/logs/). --------
EXP_EXCLUSION_LOG <- file.path(EXP_ROOT, "logs", "exclusion_log.csv")
EXP_UNKNOWN_LOG   <- file.path(EXP_ROOT, "logs", "unknown_log.csv")
if (!dir.exists(file.path(EXP_ROOT, "logs"))) dir.create(file.path(EXP_ROOT, "logs"), recursive = TRUE)

## ============================================================================
## Sites and already-committed tables (read-only). Unchanged from v2.
## ============================================================================
IAV_SITES <- c("AU-How", "NL-Loo", "DE-Tha", "FR-Pue", "CH-Dav", "US-MMS", "US-Mpj", "AT-Neu",
               "US-Ha1", "JP-Api", "IL-Yat", "US-Ton", "US-Var", "US-Wkg", "US-SRM")

exp_sites          <- read_csv(file.path(EXP_TABLES, "sites.csv"), show_col_types = FALSE)
exp_site_years      <- read_csv(file.path(EXP_TABLES, "site_years.csv"), show_col_types = FALSE)
derived_site_all    <- read_csv(file.path(EXP_TABLES, "derived_site.csv"), show_col_types = FALSE)
derived_site_year_all <- read_csv(file.path(EXP_TABLES, "derived_site_year.csv"), show_col_types = FALSE)
winter_screen_days  <- read_csv(file.path(EXP_TABLES, "winter_screen_days.csv"), show_col_types = FALSE) |>
  mutate(is_winter_screen_day = TRUE) |> select(site_id, TIMESTAMP, is_winter_screen_day)
iav_tbl             <- read_csv(file.path(EXP_TABLES, "iav_15sites.csv"), show_col_types = FALSE)
## Read-only, item 7: BADM coarse management/disturbance coverage (network-wide
## snapshot; no per-site download here, just the already-committed summary).
badm_coverage <- read_csv("data/snapshots/badm_management_coverage.csv", show_col_types = FALSE)

site_meta <- exp_sites |> filter(site_id %in% IAV_SITES) |>
  select(site_id, igbp, group, location_lat, first_year, last_year, gpp_col_used, qc_col_used)
stopifnot(all(IAV_SITES %in% site_meta$site_id))

is_sh <- function(sid) site_meta$location_lat[site_meta$site_id == sid] < 0
sh_sites <- site_meta$site_id[site_meta$location_lat < 0]
message("Southern-Hemisphere site(s) among the 15: ", paste(sh_sites, collapse = ", "))

raw_years_list <- split(exp_site_years$year[exp_site_years$site_id %in% IAV_SITES],
                          exp_site_years$site_id[exp_site_years$site_id %in% IAV_SITES])

#' One site's annual window for "year" y -- calendar Jan-Dec for NH,
#' Jul(y)-Jun(y+1) for SH (season year). Unchanged from v1/v2.
window_dates <- function(sid, y) {
  if (is_sh(sid)) seq(as.Date(sprintf("%d-07-01", y)), as.Date(sprintf("%d-06-30", y + 1L)), by = "day")
  else            seq(as.Date(sprintf("%d-01-01", y)), as.Date(sprintf("%d-12-31", y)), by = "day")
}

message("Site(s): ", paste(IAV_SITES, collapse = ", "))

## ============================================================================
## Raw daily pull (read-only, dataset = 'FLUXMET'). Unchanged from v2.
## ============================================================================
con <- dbConnect(duckdb(), "data/duckdb/fluxnet.duckdb", read_only = TRUE)
site_list_sql <- paste(sprintf("'%s'", IAV_SITES), collapse = ", ")
daily_raw <- dbGetQuery(con, sprintf("
  SELECT site_id, TIMESTAMP, GPP_NT_VUT_REF, GPP_NT_CUT_REF, GPP_DT_VUT_REF, GPP_DT_CUT_REF,
         NEE_VUT_REF, NEE_CUT_REF, NEE_VUT_REF_QC, NEE_CUT_REF_QC
  FROM daily
  WHERE dataset = 'FLUXMET' AND site_id IN (%s)
", site_list_sql))
dbDisconnect(con, shutdown = TRUE)
daily_raw$TIMESTAMP <- as.Date(as.character(daily_raw$TIMESTAMP))
message("Pulled ", nrow(daily_raw), " daily FLUXMET row(s) for ", length(IAV_SITES), " site(s).")

daily0 <- daily_raw |>
  left_join(site_meta |> select(site_id, gpp_col_used, qc_col_used), by = "site_id") |>
  mutate(
    is_vut = gpp_col_used == "GPP_NT_VUT_REF",
    GPP_NT_REF = ifelse(is_vut, GPP_NT_VUT_REF, GPP_NT_CUT_REF),
    GPP_DT_REF = ifelse(is_vut, GPP_DT_VUT_REF, GPP_DT_CUT_REF),
    NEE_REF    = ifelse(is_vut, NEE_VUT_REF, NEE_CUT_REF),
    QC_REF     = ifelse(qc_col_used == "NEE_VUT_REF_QC", NEE_VUT_REF_QC, NEE_CUT_REF_QC),
    year       = lubridate::year(TIMESTAMP)
  ) |>
  select(site_id, TIMESTAMP, year, GPP_NT_REF, GPP_DT_REF, NEE_REF, QC_REF)

dt_availability <- daily0 |> group_by(site_id) |>
  summarise(n_dt_nonNA = sum(!is.na(GPP_DT_REF)), .groups = "drop") |>
  mutate(has_dt = n_dt_nonNA > 0)
sites_no_dt <- dt_availability$site_id[!dt_availability$has_dt]
message(length(sites_no_dt), " of ", length(IAV_SITES), " site(s) have NO DT-partitioned GPP at all: ",
        if (length(sites_no_dt) > 0) paste(sites_no_dt, collapse = ", ") else "(none)")
if (length(sites_no_dt) > 0) {
  for (sid in sites_no_dt) {
    log_unknown(record_id = sid, reason = "No DT-partitioned GPP (GPP_DT_REF) at any resolution; this site has no 'dt'-series rows (flag_no_dt = TRUE).",
                logged_by = "11_gsl_methods_compare.R", log_path = EXP_UNKNOWN_LOG)
  }
}

## ============================================================================
## Two GPP series (zero-floored) + NEP (shared). Unchanged from v2.
## ============================================================================
daily <- daily0 |>
  mutate(gpp_nt = pmax(GPP_NT_REF, 0), gpp_dt = pmax(GPP_DT_REF, 0), nep = -NEE_REF) |>
  select(site_id, TIMESTAMP, year, gpp_nt, gpp_dt, nep, QC_REF)

make_continuous <- function(sid) {
  d <- daily |> filter(site_id == sid) |> arrange(TIMESTAMP)
  full_dates <- seq(min(d$TIMESTAMP), max(d$TIMESTAMP), by = "day")
  tibble(site_id = sid, TIMESTAMP = full_dates) |>
    left_join(d |> select(-site_id), by = "TIMESTAMP") |>
    mutate(year = lubridate::year(TIMESTAMP))
}
daily_cont <- bind_rows(lapply(IAV_SITES, make_continuous))
n_gap_days <- sum(daily_cont$site_id %in% IAV_SITES) - nrow(daily)
message("Continuous daily sequence built per site; ", n_gap_days, " calendar-day gap(s) filled with NA (0 expected).")

#' 15-day centred rolling mean with a partial window at the true record edges. Unchanged.
roll_mean_centred <- function(x, half_width = 7L) {
  n <- length(x)
  out <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    lo <- max(1L, i - half_width); hi <- min(n, i + half_width)
    seg <- x[lo:hi]
    if (any(!is.na(seg))) out[i] <- mean(seg, na.rm = TRUE)
  }
  out
}

daily_cont <- daily_cont |>
  group_by(site_id) |>
  arrange(TIMESTAMP, .by_group = TRUE) |>
  mutate(gpp_nt_sm = roll_mean_centred(gpp_nt), gpp_dt_sm = roll_mean_centred(gpp_dt), nep_sm = roll_mean_centred(nep)) |>
  ungroup()
message("15-day centred smoothing applied (gpp_nt, gpp_dt, nep) over each site's full continuous record.")

## ============================================================================
## Southern-Hemisphere (AU-How) Jul-Jun season years. Unchanged from v2.
## ============================================================================
season_years_sh <- function(sid) {
  rng_lo <- site_meta$first_year[site_meta$site_id == sid] - 1L
  rng_hi <- site_meta$last_year[site_meta$site_id == sid]
  d <- daily_cont |> filter(site_id == sid)
  keep <- integer(0)
  for (y in rng_lo:rng_hi) {
    win <- seq(as.Date(sprintf("%d-07-01", y)), as.Date(sprintf("%d-06-30", y + 1L)), by = "day")
    qc_vals <- d$QC_REF[match(win, d$TIMESTAMP)]
    pass_raw <- qc_passes(qc_vals, EXP_RULE_QC_DAY)
    n_fail <- sum(is.na(pass_raw) | !pass_raw)
    if (n_fail <= EXP_RULE_MAX_FAIL_DAYS) keep <- c(keep, y)
  }
  keep
}
season_years_for <- function(sid) if (is_sh(sid)) season_years_sh(sid) else raw_years_list[[sid]]

auhow_new_years <- season_years_sh("AU-How")
auhow_old_years <- sort(raw_years_list[["AU-How"]])
message("AU-How: ", length(auhow_old_years), " calendar year(s) vs ", length(auhow_new_years), " Jul-Jun season year(s).")

pooled_window_dates <- function(sid) do.call(c, lapply(season_years_for(sid), function(y) window_dates(sid, y)))
pool_tbl <- function(sid, gcol) {
  dts <- pooled_window_dates(sid)
  d <- daily_cont |> filter(site_id == sid)
  tibble(date = dts, month = lubridate::month(dts), value = d[[gcol]][match(dts, d$TIMESTAMP)])
}
pool_values <- function(sid, gcol) { v <- pool_tbl(sid, gcol)$value; v[!is.na(v)] }

## ============================================================================
## Per-site, per-series P99. Unchanged from v2.
## ============================================================================
P99_nt <- setNames(sapply(IAV_SITES, function(s) as.numeric(quantile(pool_values(s, "gpp_nt"), 0.99, type = 7))), IAV_SITES)
P99_dt <- setNames(sapply(IAV_SITES, function(s) {
  v <- pool_values(s, "gpp_dt")
  if (length(v) == 0) NA_real_ else as.numeric(quantile(v, 0.99, type = 7))
}), IAV_SITES)

nh_sites <- setdiff(IAV_SITES, sh_sites)
chk_P99 <- derived_site_all |> filter(site_id %in% nh_sites, variant == "main_unscreened") |>
  select(site_id, P99) |> mutate(P99_fresh = P99_nt[site_id], diff = P99 - P99_fresh)
if (any(abs(chk_P99$diff) > 1e-9)) {
  print(chk_P99[abs(chk_P99$diff) > 1e-9, ])
  stop("11_gsl_methods_compare.R: freshly computed NT P99 does not match derived_site.csv for a NH site -- stopping.")
}
message("Check passed: fresh NT P99 exactly matches derived_site.csv's main_unscreened P99 for all ", length(nh_sites), " NH sites.")

## ============================================================================
## Item 3: a (dates, raw values, smoothed values, absolute threshold) ->
## THREE start/end rules for the count methods (M2/M3/M4):
##   (a) first/last qualifying day (unchanged "legacy" rule -- gsl/start_date/
##       end_date; a site-year with 0 qualifying days gets gsl=0).
##   (b) first/last run of >=5 consecutive qualifying days ("the main run's
##       rule, our choice, not from the literature") -- start_date_run5/
##       end_date_run5/gsl_run5; flag_no_run5 if no such run exists.
##   (c) threshold crossing of the 15-day-SMOOTHED series closest to the
##       seasonal peak (Panwar et al. 2023's rule): the single contiguous run
##       of smoothed-value > T_abs that CONTAINS the peak day of the smoothed
##       series -- start_date_smpeak/end_date_smpeak/gsl_smpeak; flag_no_smpeak
##       if the peak day itself is not above T_abs (degenerate).
## ============================================================================
count_method_one <- function(dates, thresh_vals, value_vals, value_sm_vals, T_abs) {
  ok <- !is.na(thresh_vals) & !is.na(value_vals)
  above <- ok & thresh_vals > T_abs
  n <- sum(above, na.rm = TRUE)
  annual_gpp <- sum(value_vals, na.rm = TRUE)

  if (n == 0) {
    base <- tibble(gsl = 0L, start_date = as.Date(NA), end_date = as.Date(NA),
                    gpp_in_season = 0, annual_gpp = annual_gpp, share = NA_real_,
                    flag_no_season = TRUE, flag_whole_year = FALSE, flag_no_dt = FALSE)
  } else {
    idx_above <- which(above)
    gpp_in_season <- sum(value_vals[idx_above], na.rm = TRUE)
    base <- tibble(gsl = n, start_date = dates[idx_above[1]], end_date = dates[idx_above[length(idx_above)]],
                    gpp_in_season = gpp_in_season, annual_gpp = annual_gpp,
                    share = if (annual_gpp != 0) gpp_in_season / annual_gpp else NA_real_,
                    flag_no_season = FALSE, flag_whole_year = (n == length(dates)), flag_no_dt = FALSE)
  }

  ## ---- Rule (b): first/last >=5-day run. ----
  if (n == 0) {
    base$start_date_run5 <- as.Date(NA); base$end_date_run5 <- as.Date(NA)
    base$gsl_run5 <- NA_integer_; base$flag_no_run5 <- TRUE
  } else {
    r <- rle(above)
    run_end <- cumsum(r$lengths); run_start <- run_end - r$lengths + 1L
    qualifying <- which(r$values & r$lengths >= 5L)
    if (length(qualifying) == 0) {
      base$start_date_run5 <- as.Date(NA); base$end_date_run5 <- as.Date(NA)
      base$gsl_run5 <- NA_integer_; base$flag_no_run5 <- TRUE
    } else {
      s_idx <- run_start[qualifying[1]]; e_idx <- run_end[qualifying[length(qualifying)]]
      base$start_date_run5 <- dates[s_idx]; base$end_date_run5 <- dates[e_idx]
      base$gsl_run5 <- e_idx - s_idx + 1L; base$flag_no_run5 <- FALSE
    }
  }

  ## ---- Rule (c): smoothed-threshold-crossing run containing the peak. ----
  ok_sm <- !is.na(value_sm_vals)
  if (!any(ok_sm)) {
    base$start_date_smpeak <- as.Date(NA); base$end_date_smpeak <- as.Date(NA)
    base$gsl_smpeak <- NA_integer_; base$flag_no_smpeak <- TRUE
  } else {
    n2 <- length(dates)
    peak_idx <- which.max(value_sm_vals)
    above_sm <- ok_sm & value_sm_vals > T_abs
    if (!isTRUE(above_sm[peak_idx])) {
      base$start_date_smpeak <- as.Date(NA); base$end_date_smpeak <- as.Date(NA)
      base$gsl_smpeak <- NA_integer_; base$flag_no_smpeak <- TRUE
    } else {
      run_s <- peak_idx; while (run_s > 1 && isTRUE(above_sm[run_s - 1])) run_s <- run_s - 1
      run_e <- peak_idx; while (run_e < n2 && isTRUE(above_sm[run_e + 1])) run_e <- run_e + 1
      base$start_date_smpeak <- dates[run_s]; base$end_date_smpeak <- dates[run_e]
      base$gsl_smpeak <- run_e - run_s + 1L; base$flag_no_smpeak <- FALSE
    }
  }
  base
}
no_dt_row <- function() tibble(gsl = NA_integer_, start_date = as.Date(NA), end_date = as.Date(NA),
                                gpp_in_season = NA_real_, annual_gpp = NA_real_, share = NA_real_,
                                flag_no_season = FALSE, flag_whole_year = FALSE, flag_no_dt = TRUE)

## ============================================================================
## Item 1: M2 kept grid (5/10/20%, main tables), M3 (unchanged), M4 targets
## 0.85/0.90/0.95/0.99 (adds 0.85 = Tash's current method, T85 -- not named
## in any figure/label per the no-Z16-vs-T85/T95-framing instruction).
## ============================================================================
run_count_methods <- function(variant, gcol, gcol_sm, P99_vec, m2_pcts, m4_targets) {
  m3_abs <- c(0.5, 1, 2) # gC m-2 d-1, fixed, no P99 scaling

  out <- list()
  for (sid in IAV_SITES) {
    yrs <- season_years_for(sid)
    if (variant == "dt" && sid %in% sites_no_dt) {
      for (y in yrs) {
        for (pct in m2_pcts) out[[length(out) + 1]] <- no_dt_row() |> mutate(site_id = sid, year = y, variant = variant, method = "M2", sub = sprintf("%gpct", pct))
        for (abs_t in m3_abs) out[[length(out) + 1]] <- no_dt_row() |> mutate(site_id = sid, year = y, variant = variant, method = "M3", sub = sprintf("%sgC", format(abs_t)))
        for (target in m4_targets) out[[length(out) + 1]] <- no_dt_row() |> mutate(site_id = sid, year = y, variant = variant, method = "M4", sub = sprintf("%.2f", target), threshold_pct_of_P99 = NA_real_)
      }
      next
    }
    d <- daily_cont |> filter(site_id == sid)
    P99 <- P99_vec[[sid]]

    windows_by_year <- setNames(lapply(yrs, function(y) window_dates(sid, y)), as.character(yrs))
    gpp_by_year <- lapply(windows_by_year, function(win) d[[gcol]][match(win, d$TIMESTAMP)])
    fine_abs <- FINE_GRID_PCT / 100 * P99
    f_mean_grid <- vapply(fine_abs, function(T_abs) {
      per_yr <- vapply(gpp_by_year, function(vv) {
        ok <- !is.na(vv); above <- ok & vv > T_abs
        tot <- sum(vv, na.rm = TRUE)
        if (tot == 0) return(NA_real_)
        sum(vv[above], na.rm = TRUE) / tot
      }, numeric(1))
      mean(per_yr, na.rm = TRUE)
    }, numeric(1))
    f_rounded <- round(f_mean_grid, 6)

    for (y in yrs) {
      win <- window_dates(sid, y)
      vals <- d[[gcol]][match(win, d$TIMESTAMP)]
      vals_sm <- d[[gcol_sm]][match(win, d$TIMESTAMP)]

      for (pct in m2_pcts) {
        T_abs <- pct / 100 * P99
        r <- count_method_one(win, vals, vals, vals_sm, T_abs)
        out[[length(out) + 1]] <- r |> mutate(site_id = sid, year = y, variant = variant, method = "M2", sub = sprintf("%gpct", pct))
      }
      for (abs_t in m3_abs) {
        r <- count_method_one(win, vals, vals, vals_sm, abs_t)
        out[[length(out) + 1]] <- r |> mutate(site_id = sid, year = y, variant = variant, method = "M3", sub = sprintf("%sgC", format(abs_t)))
      }
      for (target in m4_targets) {
        idx <- which(f_rounded <= round(target, 6))[1]
        if (is.na(idx)) {
          r <- tibble(gsl = NA_integer_, start_date = as.Date(NA), end_date = as.Date(NA),
                      gpp_in_season = NA_real_, annual_gpp = sum(vals, na.rm = TRUE), share = NA_real_,
                      flag_no_season = TRUE, flag_whole_year = FALSE, flag_no_dt = FALSE,
                      start_date_run5 = as.Date(NA), end_date_run5 = as.Date(NA), gsl_run5 = NA_integer_, flag_no_run5 = TRUE,
                      start_date_smpeak = as.Date(NA), end_date_smpeak = as.Date(NA), gsl_smpeak = NA_integer_, flag_no_smpeak = TRUE)
        } else {
          T_abs <- fine_abs[idx]
          r <- count_method_one(win, vals, vals, vals_sm, T_abs)
        }
        out[[length(out) + 1]] <- r |> mutate(site_id = sid, year = y, variant = variant, method = "M4", sub = sprintf("%.2f", target),
                                                threshold_pct_of_P99 = if (!is.na(idx)) FINE_GRID_PCT[idx] else NA_real_)
      }
    }
  }
  bind_rows(out)
}

M2_PCTS_KEPT <- c(5, 10, 20)
M4_TARGETS <- c(0.85, 0.90, 0.95, 0.99)

message("Running M2 (5/10/20%)/M3/M4 (0.85/0.90/0.95/0.99) for 2 series (nt, dt) x 15 sites, 3 start/end rules each...")
m234_nt <- run_count_methods("nt", "gpp_nt", "gpp_nt_sm", P99_nt, M2_PCTS_KEPT, M4_TARGETS)
m234_dt <- run_count_methods("dt", "gpp_dt", "gpp_dt_sm", P99_dt, M2_PCTS_KEPT, M4_TARGETS)
m234_all <- bind_rows(m234_nt, m234_dt)
message("M2/M3/M4: ", nrow(m234_all), " site x year x series x method-threshold rows.")

## ============================================================================
## Item 1: M2's full 22-value grid (0.1, 0.5%, 1-20% of P99), NT series only
## (feeds the dot plot's range display; NOT added to the main tables).
## ============================================================================
M2_PCTS_GRID <- c(0.1, 0.5, 1:20)
run_m2_grid <- function(gcol, P99_vec, pcts) {
  out <- list()
  for (sid in IAV_SITES) {
    yrs <- season_years_for(sid)
    d <- daily_cont |> filter(site_id == sid)
    P99 <- P99_vec[[sid]]
    for (y in yrs) {
      win <- window_dates(sid, y)
      vals <- d[[gcol]][match(win, d$TIMESTAMP)]
      for (pct in pcts) {
        T_abs <- pct / 100 * P99
        ok <- !is.na(vals); above <- ok & vals > T_abs
        n <- sum(above, na.rm = TRUE)
        out[[length(out) + 1]] <- tibble(site_id = sid, year = y, pct = pct, gsl = n)
      }
    }
  }
  bind_rows(out)
}
m2_grid_site_year <- run_m2_grid("gpp_nt", P99_nt, M2_PCTS_GRID)
m2_grid_site_summary <- m2_grid_site_year |> group_by(site_id, pct) |> summarise(mean_gsl = mean(gsl, na.rm = TRUE), .groups = "drop")
m2_grid_range <- m2_grid_site_summary |> group_by(site_id) |> summarise(m2_grid_min = min(mean_gsl), m2_grid_max = max(mean_gsl), .groups = "drop")
message("M2 full 22-value grid (NT series): ", nrow(m2_grid_site_year), " site x year x pct rows.")

## ============================================================================
## Checks (as version 2, plus the new M4@0.85 check).
## ============================================================================
dsy_check <- derived_site_year_all |> filter(site_id %in% nh_sites, variant == "main_unscreened") |>
  select(site_id, year, gsl_z16, gsl_t95, f_z16, f_t95)
m2_10_nt <- m234_all |> filter(variant == "nt", method == "M2", sub == "10pct") |> select(site_id, year, gsl, share)
m4_95_nt <- m234_all |> filter(variant == "nt", method == "M4", sub == "0.95") |> select(site_id, year, gsl, share)
chk1 <- dsy_check |>
  left_join(m2_10_nt |> rename(gsl_m2_10 = gsl, share_m2_10 = share), by = c("site_id", "year")) |>
  left_join(m4_95_nt |> rename(gsl_m4_95 = gsl, share_m4_95 = share), by = c("site_id", "year")) |>
  mutate(diff_gsl_z16 = gsl_z16 - gsl_m2_10, diff_f_z16 = f_z16 - share_m2_10,
         diff_gsl_t95 = gsl_t95 - gsl_m4_95, diff_f_t95 = f_t95 - share_m4_95)
bad <- chk1 |> filter((!is.na(diff_gsl_z16) & abs(diff_gsl_z16) > 0) | (!is.na(diff_gsl_t95) & abs(diff_gsl_t95) > 0) |
                       (!is.na(diff_f_z16) & abs(diff_f_z16) > 1e-9) | (!is.na(diff_f_t95) & abs(diff_f_t95) > 1e-9))
if (nrow(bad) > 0) {
  print(bad)
  stop("11_gsl_methods_compare.R: NT-series M2@10%/M4@0.95 do not exactly match derived_site_year.csv's main_unscreened values -- stopping.")
}
message("Check passed: NT-series M2@10% / M4@0.95 == main_unscreened gsl_z16/f_z16 / gsl_t95/f_t95 exactly, for all ", nrow(chk1), " NH site-year rows.")

## ---- New check: M4@0.85 (NT) vs. the existing fraction sweep at 0.85. The
## only existing fraction sweep is tables/fraction_sweep_main.csv, whose sole
## `variant` value is "main" (the WINTER-SCREENED primary series from
## GSL_definition/code), not "main_unscreened"/NT -- confirmed by inspection
## (`unique(fraction_sweep_main$variant)` returns only "main"). The prompt's
## check is therefore conditional ("if that table holds it"): it does not,
## so this check is reported as not applicable rather than silently skipped. ----
fraction_sweep_main <- read_csv(file.path(EXP_TABLES, "fraction_sweep_main.csv"), show_col_types = FALSE)
fsm_variants <- unique(fraction_sweep_main$variant)
message("M4@0.85 (NT) vs. fraction_sweep_main.csv check: NOT APPLICABLE -- that table's only variant is '",
        paste(fsm_variants, collapse = ", "), "' (winter-screened), not 'main_unscreened'/NT. No NT-series 0.85 ",
        "fraction sweep exists anywhere else in the repository to check against (per the prompt's 'if that table holds it').")

## ---- AU-How: M2@10%/M4@0.95 old (calendar) vs new (Jul-Jun) -- unchanged from v2. ----
auhow_old_gsl <- derived_site_year_all |> filter(site_id == "AU-How", variant == "main_unscreened") |>
  select(year, gsl_z16_old = gsl_z16, gsl_t95_old = gsl_t95)
auhow_new_gsl <- m234_all |> filter(site_id == "AU-How", variant == "nt", method == "M2", sub == "10pct") |>
  select(year, gsl_z16_new = gsl) |>
  left_join(m234_all |> filter(site_id == "AU-How", variant == "nt", method == "M4", sub == "0.95") |> select(year, gsl_t95_new = gsl), by = "year")
message("AU-How GSL, old (calendar year) vs new (Jul-Jun season year):")
print(as.data.frame(auhow_old_gsl)); print(as.data.frame(auhow_new_gsl))

## ============================================================================
## Annual GPP/NEP for detrended-r. Unchanged from v2.
## ============================================================================
annual_gpp_nep_for <- function(sid, gpp_col) {
  yrs <- season_years_for(sid)
  purrr::map_dfr(yrs, function(y) {
    win <- window_dates(sid, y)
    d <- daily_cont |> filter(site_id == sid, TIMESTAMP %in% win)
    tibble(site_id = sid, year = y, annual_gpp = sum(d[[gpp_col]], na.rm = TRUE), annual_nep = sum(d$nep, na.rm = TRUE))
  })
}
annual_corr_nt <- bind_rows(lapply(IAV_SITES, annual_gpp_nep_for, gpp_col = "gpp_nt"))
annual_corr_dt <- bind_rows(lapply(setdiff(IAV_SITES, sites_no_dt), annual_gpp_nep_for, gpp_col = "gpp_dt"))

detrend_cor <- function(gsl, annual, year) {
  ok <- !is.na(gsl) & !is.na(annual) & !is.na(year)
  if (sum(ok) < 4) return(c(r = NA_real_, p = NA_real_, n = sum(ok)))
  g <- gsl[ok]; a <- annual[ok]; yv <- year[ok]
  rg <- residuals(lm(g ~ yv)); ra <- residuals(lm(a ~ yv))
  ct <- cor.test(rg, ra)
  c(r = unname(ct$estimate), p = ct$p.value, n = sum(ok))
}

## ============================================================================
## M1 (Zscheischler et al. 2016 crossing-span). Unchanged from v2.
## ============================================================================
summer_months <- function(sid) if (is_sh(sid)) c(12, 1, 2) else c(6, 7, 8)
peak_months_for <- function(sid, gcol) {
  pt <- pool_tbl(sid, gcol)
  month_mean <- vapply(1:12, function(m) mean(pt$value[pt$month == m], na.rm = TRUE), numeric(1))
  starts <- 1:12
  score <- vapply(starts, function(s) sum(month_mean[((s - 1):(s + 1)) %% 12 + 1]), numeric(1))
  best_start <- starts[which.max(score)]
  ((best_start - 1):(best_start + 1)) %% 12 + 1
}
P99_for_months <- function(sid, gcol, months) {
  pt <- pool_tbl(sid, gcol)
  v <- pt$value[pt$month %in% months]
  if (all(is.na(v))) return(NA_real_)
  as.numeric(quantile(v, 0.99, type = 7, na.rm = TRUE))
}
peak_months_nt <- setNames(lapply(IAV_SITES, peak_months_for, gcol = "gpp_nt"), IAV_SITES)
peak_months_dt <- setNames(lapply(setdiff(IAV_SITES, sites_no_dt), peak_months_for, gcol = "gpp_dt"), setdiff(IAV_SITES, sites_no_dt))
P99_summer_nt <- setNames(sapply(IAV_SITES, function(s) P99_for_months(s, "gpp_nt", summer_months(s))), IAV_SITES)
P99_summer_dt <- setNames(sapply(IAV_SITES, function(s) if (s %in% sites_no_dt) NA_real_ else P99_for_months(s, "gpp_dt", summer_months(s))), IAV_SITES)
P99_peak_nt <- setNames(sapply(IAV_SITES, function(s) P99_for_months(s, "gpp_nt", peak_months_nt[[s]])), IAV_SITES)
P99_peak_dt <- setNames(sapply(IAV_SITES, function(s) if (s %in% sites_no_dt) NA_real_ else P99_for_months(s, "gpp_dt", peak_months_dt[[s]])), IAV_SITES)

crossing_method_one <- function(dates, thresh_vals, value_vals, T_abs) {
  above <- !is.na(thresh_vals) & thresh_vals > T_abs
  n <- length(above)
  annual_gpp <- sum(value_vals, na.rm = TRUE)
  if (!any(above)) {
    return(tibble(gsl = NA_integer_, start_date = as.Date(NA), end_date = as.Date(NA),
                  gpp_in_season = 0, annual_gpp = annual_gpp, share = NA_real_,
                  flag_no_season = TRUE, flag_whole_year = FALSE,
                  flag_no_start_crossing = FALSE, flag_no_end_crossing = FALSE, flag_no_dt = FALSE))
  }
  idx_above <- which(above)
  s_idx <- idx_above[1]; e_idx <- idx_above[length(idx_above)]
  gsl <- e_idx - s_idx + 1L
  gpp_in_season <- sum(value_vals[s_idx:e_idx], na.rm = TRUE)
  tibble(gsl = gsl, start_date = dates[s_idx], end_date = dates[e_idx],
         gpp_in_season = gpp_in_season, annual_gpp = annual_gpp,
         share = if (annual_gpp != 0) gpp_in_season / annual_gpp else NA_real_,
         flag_no_season = FALSE, flag_whole_year = all(above),
         flag_no_start_crossing = (s_idx == 1), flag_no_end_crossing = (e_idx == n), flag_no_dt = FALSE)
}
no_dt_row_m1 <- function() tibble(gsl = NA_integer_, start_date = as.Date(NA), end_date = as.Date(NA),
                                   gpp_in_season = NA_real_, annual_gpp = NA_real_, share = NA_real_,
                                   flag_no_season = FALSE, flag_whole_year = FALSE,
                                   flag_no_start_crossing = FALSE, flag_no_end_crossing = FALSE, flag_no_dt = TRUE)

run_m1 <- function(variant, gcol_sm, gcol, P99_annual_vec, P99_summer_vec, P99_peak_vec, annual_corr_tbl) {
  out <- list()
  for (sid in IAV_SITES) {
    yrs <- season_years_for(sid)
    if (variant == "dt" && sid %in% sites_no_dt) {
      for (y in yrs) for (sub in c("summer", "peak", "annual"))
        out[[length(out) + 1]] <- no_dt_row_m1() |> mutate(site_id = sid, year = y, variant = variant, method = "M1", sub = sub)
      next
    }
    d <- daily_cont |> filter(site_id == sid)
    subs <- list(summer = P99_summer_vec[[sid]], peak = P99_peak_vec[[sid]], annual = P99_annual_vec[[sid]])
    for (y in yrs) {
      win <- window_dates(sid, y)
      dy <- d |> filter(TIMESTAMP %in% win)
      thresh_vals <- dy[[gcol_sm]][match(win, dy$TIMESTAMP)]
      value_vals  <- dy[[gcol]][match(win, dy$TIMESTAMP)]
      ag <- annual_corr_tbl$annual_gpp[annual_corr_tbl$site_id == sid & annual_corr_tbl$year == y]
      for (sub in names(subs)) {
        T_abs <- 0.10 * subs[[sub]]
        r <- crossing_method_one(win, thresh_vals, value_vals, T_abs)
        if (length(ag) == 1) r$annual_gpp <- ag
        r$share <- if (!is.na(r$annual_gpp) && r$annual_gpp != 0) r$gpp_in_season / r$annual_gpp else NA_real_
        out[[length(out) + 1]] <- r |> mutate(site_id = sid, year = y, variant = variant, method = "M1", sub = sub)
      }
    }
  }
  bind_rows(out)
}

message("Running M1 for 2 series x 3 thresholds x 15 sites...")
m1_nt <- run_m1("nt", "gpp_nt_sm", "gpp_nt", P99_nt, P99_summer_nt, P99_peak_nt, annual_corr_nt)
m1_dt <- run_m1("dt", "gpp_dt_sm", "gpp_dt", P99_dt, P99_summer_dt, P99_peak_dt, annual_corr_dt)
m1_all <- bind_rows(m1_nt, m1_dt)
message("M1: ", nrow(m1_all), " site x year x series x sub-threshold rows.")

## ============================================================================
## Two-season flag via the TIMESAT secondary-peak rule (Wang et al. 2020) on
## 15-day-smoothed NT GPP. Unchanged from v2, except the primary/secondary
## peak DATES are now also returned (needed for item 8's AT-Neu mowing test).
## ============================================================================
local_maxima_idx <- function(x) {
  n <- length(x)
  if (n < 3) return(integer(0))
  idx <- integer(0)
  for (i in 2:(n - 1)) {
    if (is.na(x[i])) next
    lo <- x[i - 1]; hi <- x[i + 1]
    left_ok  <- is.na(lo) || x[i] > lo
    right_ok <- is.na(hi) || x[i] > hi
    if (left_ok && right_ok && (!is.na(lo) || !is.na(hi))) idx <- c(idx, i)
  }
  idx
}

timesat_two_season <- function(gpp_sm_vals, dates) {
  none <- list(flag = FALSE, ratio = NA_real_, primary_date = as.Date(NA), secondary_date = as.Date(NA))
  if (all(is.na(gpp_sm_vals))) return(none)
  maxima <- local_maxima_idx(gpp_sm_vals)
  if (length(maxima) < 1) return(none)
  primary_idx <- maxima[which.max(gpp_sm_vals[maxima])]
  primary_val <- gpp_sm_vals[primary_idx]
  primary_date <- dates[primary_idx]
  others <- setdiff(maxima, primary_idx)
  if (length(others) == 0) return(list(flag = FALSE, ratio = NA_real_, primary_date = primary_date, secondary_date = as.Date(NA)))
  secondary_idx <- others[which.max(gpp_sm_vals[others])]
  secondary_val <- gpp_sm_vals[secondary_idx]
  secondary_date <- dates[secondary_idx]
  ratio <- secondary_val / primary_val
  lo_idx <- min(primary_idx, secondary_idx); hi_idx <- max(primary_idx, secondary_idx)
  between <- gpp_sm_vals[(lo_idx + 1):(hi_idx - 1)]
  valley <- if (length(between) == 0 || all(is.na(between))) NA_real_ else min(between, na.rm = TRUE)
  lower_peak <- min(primary_val, secondary_val)
  flag <- !is.na(ratio) && ratio >= 0.5 && !is.na(valley) && valley < 0.5 * lower_peak
  list(flag = flag, ratio = ratio, primary_date = primary_date, secondary_date = secondary_date)
}

## ============================================================================
## M5: carbon-uptake period from smoothed NEP > 0. Unchanged from v2, plus
## the two-season peak dates are now carried onto the output rows.
## ============================================================================
run_m5 <- function() {
  out <- list()
  for (sid in IAV_SITES) {
    yrs <- season_years_for(sid)
    d <- daily_cont |> filter(site_id == sid)
    for (y in yrs) {
      win <- window_dates(sid, y)
      dy <- d |> filter(TIMESTAMP %in% win)
      nep_sm <- dy$nep_sm[match(win, dy$TIMESTAMP)]
      gpp_nt_sm_win <- dy$gpp_nt_sm[match(win, dy$TIMESTAMP)]
      above <- !is.na(nep_sm) & nep_sm > 0
      n_above <- sum(above)
      net_source <- n_above < 5L

      ts <- timesat_two_season(gpp_nt_sm_win, win)

      count_row <- tibble(gsl = n_above, start_date = if (n_above > 0) win[which(above)[1]] else as.Date(NA),
                           end_date = if (n_above > 0) win[which(above)[n_above]] else as.Date(NA), sub = "count")
      if (!any(above)) {
        span_row <- tibble(gsl = NA_integer_, start_date = as.Date(NA), end_date = as.Date(NA),
                            flag_no_season = TRUE, flag_whole_year = FALSE, sub = "span")
      } else {
        r <- rle(above)
        run_end <- cumsum(r$lengths); run_start <- run_end - r$lengths + 1L
        qualifying <- which(r$values & r$lengths >= 5L)
        if (length(qualifying) == 0) {
          span_row <- tibble(gsl = NA_integer_, start_date = as.Date(NA), end_date = as.Date(NA),
                              flag_no_season = TRUE, flag_whole_year = FALSE, sub = "span")
        } else {
          s_idx <- run_start[qualifying[1]]; e_idx <- run_end[qualifying[length(qualifying)]]
          span_row <- tibble(gsl = e_idx - s_idx + 1L, start_date = win[s_idx], end_date = win[e_idx],
                              flag_no_season = FALSE, flag_whole_year = all(above), sub = "span")
        }
      }
      count_row$flag_no_season <- n_above == 0; count_row$flag_whole_year <- all(above)

      for (rowd in list(count_row, span_row)) {
        rowd$flag_net_source <- net_source
        rowd$flag_two_season <- ts$flag
        rowd$two_season_ratio <- ts$ratio
        rowd$two_season_primary_date <- ts$primary_date
        rowd$two_season_secondary_date <- ts$secondary_date
        out[[length(out) + 1]] <- rowd |> mutate(site_id = sid, year = y, variant = "all", method = "M5")
      }
    }
  }
  bind_rows(out)
}

message("Running M5 (count + span) for 15 sites...")
m5_all <- run_m5()
message("M5: ", nrow(m5_all), " site x year x sub rows. Net-source years: ", sum(m5_all$flag_net_source[m5_all$sub == "span"]),
        " of ", sum(m5_all$sub == "span"), ". Two-season (TIMESAT) flags: ",
        sum(m5_all$flag_two_season[m5_all$sub == "span"]), " of ", sum(m5_all$sub == "span"))

## ============================================================================
## Item 4: M6 in TWO forms.
##   "anchored" (v2): start search window = M5-span start +/-30d; end search
##     window = M5-span end +/-30d (steepest positive / shallowest negative
##     slope; k=3-9; unsmoothed daily NEP).
##   "unanchored" (v1's own window/search logic, ported verbatim from
##     15dc51f): single global peak of SMOOTHED NEP as the pivot; start
##     search = [M5-span-start - 30, peak]; end search = two-stage (steepest
##     negative slope from peak to the first smoothed-NEP<0 day, then
##     shallowest negative slope back from there) -- v1's "alternate end" is
##     NOT reproduced (dropped in v2 and not reinstated here).
## Figures use the anchored form only; its agreement with M5 is partly built
## in by construction (the search windows are anchored to M5's own span).
## ============================================================================
slope_at <- function(y, k) {
  before <- floor((k - 1) / 2); after <- k - 1 - before
  n <- length(y)
  out <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    lo <- i - before; hi <- i + after
    if (lo < 1 || hi > n) next
    yy <- y[lo:hi]; xx <- seq_along(yy)
    ok <- !is.na(yy)
    if (sum(ok) < 2) next
    xo <- xx[ok]; yo <- yy[ok]
    xbar <- mean(xo); ybar <- mean(yo)
    Sxx <- sum((xo - xbar)^2)
    if (Sxx == 0) next
    out[i] <- sum((xo - xbar) * (yo - ybar)) / Sxx
  }
  out
}

m6_one_k_anchored <- function(win, nep_raw_win, k, m5_start_idx, m5_end_idx) {
  n <- length(win)
  slope <- slope_at(nep_raw_win, k)
  s_lo <- max(1, m5_start_idx - 30); s_hi <- min(n, m5_start_idx + 30)
  seg_s <- slope[s_lo:s_hi]
  start_idx <- if (all(is.na(seg_s))) NA_integer_ else s_lo - 1L + which.max(seg_s)
  e_lo <- max(1, m5_end_idx - 30); e_hi <- min(n, m5_end_idx + 30)
  seg_e <- slope[e_lo:e_hi]
  neg_e <- seg_e; neg_e[!is.na(neg_e) & neg_e >= 0] <- NA
  end_idx <- if (all(is.na(neg_e))) NA_integer_ else e_lo - 1L + which.max(neg_e)
  list(start = start_idx, end = end_idx)
}

#' v1's unanchored M6 logic, ported verbatim from 15dc51f (no alt-end).
m6_one_k_unanchored <- function(win, nep_raw_win, nep_sm_win, k, first_rise_idx) {
  n <- length(win)
  slope <- slope_at(nep_raw_win, k)
  peak_idx <- which.max(nep_sm_win)

  s_lo <- max(1, first_rise_idx - 30); s_hi <- peak_idx
  start_idx <- if (s_hi < s_lo || all(is.na(slope[s_lo:s_hi]))) NA_integer_ else s_lo - 1L + which.max(slope[s_lo:s_hi])

  e_lo <- peak_idx; e_hi <- n
  seg2 <- slope[e_lo:e_hi]
  steep_idx <- if (all(is.na(seg2)) || all(seg2[!is.na(seg2)] >= 0)) NA_integer_ else e_lo - 1L + which.min(seg2)

  end_idx <- NA_integer_
  if (!is.na(steep_idx)) {
    after_steep <- (steep_idx + 1):n
    below0 <- which(nep_sm_win[after_steep] < 0)
    zero_idx <- if (length(below0) > 0) after_steep[below0[1]] else n
    seg3 <- slope[steep_idx:zero_idx]
    neg3 <- seg3; neg3[!is.na(neg3) & neg3 >= 0] <- NA
    if (!all(is.na(neg3))) end_idx <- steep_idx - 1L + which.max(neg3)
  }
  list(start = start_idx, end = end_idx)
}

run_m6 <- function(form) {
  out <- list()
  for (sid in IAV_SITES) {
    yrs <- season_years_for(sid)
    d <- daily_cont |> filter(site_id == sid)
    for (y in yrs) {
      win <- window_dates(sid, y)
      dy <- d |> filter(TIMESTAMP %in% win)
      nep_raw_win <- dy$nep[match(win, dy$TIMESTAMP)]
      nep_sm_win  <- dy$nep_sm[match(win, dy$TIMESTAMP)]

      m5_span <- m5_all |> filter(site_id == sid, year == y, sub == "span")
      no_span <- nrow(m5_span) != 1 || isTRUE(m5_span$flag_no_season[1]) || is.na(m5_span$start_date[1]) || is.na(m5_span$end_date[1])
      net_source <- nrow(m5_span) == 1 && isTRUE(m5_span$flag_net_source[1])
      two_season <- nrow(m5_span) == 1 && isTRUE(m5_span$flag_two_season[1])

      if (no_span) {
        out[[length(out) + 1]] <- tibble(site_id = sid, year = y, variant = "all", method = "M6", sub = form,
                                           gsl = NA_integer_, start_date = as.Date(NA), end_date = as.Date(NA),
                                           k_range_start_days = NA_integer_, k_range_end_days = NA_integer_,
                                           flag_no_season = TRUE, flag_no_m5_span = TRUE, flag_net_source = net_source,
                                           flag_two_season = two_season)
        next
      }
      m5_start_idx <- which(win == m5_span$start_date[1])[1]
      m5_end_idx   <- which(win == m5_span$end_date[1])[1]

      starts <- integer(0); ends <- integer(0)
      for (k in 3:9) {
        r <- if (form == "anchored") m6_one_k_anchored(win, nep_raw_win, k, m5_start_idx, m5_end_idx)
             else m6_one_k_unanchored(win, nep_raw_win, nep_sm_win, k, m5_start_idx)
        if (!is.na(r$start)) starts <- c(starts, r$start)
        if (!is.na(r$end)) ends <- c(ends, r$end)
      }
      no_season <- length(starts) == 0 || length(ends) == 0
      start_med <- if (length(starts) > 0) win[round(median(starts))] else as.Date(NA)
      end_med   <- if (length(ends) > 0) win[round(median(ends))] else as.Date(NA)
      gsl <- if (!no_season) as.integer(end_med - start_med + 1L) else NA_integer_

      out[[length(out) + 1]] <- tibble(
        site_id = sid, year = y, variant = "all", method = "M6", sub = form,
        gsl = gsl, start_date = start_med, end_date = end_med,
        k_range_start_days = if (length(starts) > 1) max(starts) - min(starts) else 0L,
        k_range_end_days = if (length(ends) > 1) max(ends) - min(ends) else 0L,
        flag_no_season = no_season, flag_no_m5_span = FALSE, flag_net_source = net_source, flag_two_season = two_season
      )
    }
  }
  bind_rows(out)
}

message("Running M6, anchored (+/-30d of M5-span) and unanchored (v1's own window) forms, for 15 sites...")
m6_anchored <- run_m6("anchored")
m6_unanchored <- run_m6("unanchored")
m6_all <- bind_rows(m6_anchored, m6_unanchored)
message("M6: ", nrow(m6_all), " site-year x form rows. Anchored mean GSL: ", round(mean(m6_anchored$gsl, na.rm = TRUE), 1),
        " d; unanchored mean GSL: ", round(mean(m6_unanchored$gsl, na.rm = TRUE), 1), " d.")

## ============================================================================
## Share of annual GPP (NT series, the shared default for NEP-only methods).
## Unchanged from v2.
## ============================================================================
gpp_share_for_span <- function(sid, start_date, end_date, y) {
  if (is.na(start_date) || is.na(end_date)) return(c(gpp_in_season = NA_real_, annual_gpp = NA_real_, share = NA_real_))
  d <- daily_cont |> filter(site_id == sid, TIMESTAMP >= start_date, TIMESTAMP <= end_date)
  gpp_in_season <- sum(d$gpp_nt, na.rm = TRUE)
  ag <- annual_corr_nt$annual_gpp[annual_corr_nt$site_id == sid & annual_corr_nt$year == y]
  ag <- if (length(ag) == 1) ag else NA_real_
  c(gpp_in_season = gpp_in_season, annual_gpp = ag, share = if (!is.na(ag) && ag != 0) gpp_in_season / ag else NA_real_)
}
add_gpp_share <- function(tbl) {
  res <- purrr::pmap_dfr(list(tbl$site_id, tbl$start_date, tbl$end_date, tbl$year), function(s, sd, ed, y) {
    as_tibble(t(gpp_share_for_span(s, sd, ed, y)))
  })
  bind_cols(tbl, res)
}
m5_all <- add_gpp_share(m5_all)
m6_all <- add_gpp_share(m6_all)
message("Added share-of-annual-GPP (NT-series GPP) to M5/M6 rows.")

## ============================================================================
## Item 2 (of v3): M7 (Ma et al. 2013, amplitude/derivative) and M8 (Panwar
## et al. 2023, normalized threshold), both on 15-day-smoothed GPP, each per
## series (NT, DT).
##
## M7, as implemented: within a site-year window, the seasonal peak is the
## global maximum of smoothed GPP. The PRE-SEASON MINIMUM is the minimum of
## smoothed GPP strictly before the peak; the DAY OF FASTEST GREEN-UP is the
## day (within [pre-season-min day, peak day)) where the first difference of
## smoothed GPP is largest (most positive). Start = the day halfway (by
## index, rounded) between the pre-season minimum and the green-up day.
## Symmetrically: POST-SEASON MINIMUM = minimum strictly after the peak; DAY
## OF FASTEST BROWN-DOWN = the day (within [peak day, post-season-min day))
## where the first difference is smallest (most negative). End = halfway
## between the brown-down day and the post-season minimum. A site-year is
## flagged (flag_no_season) if the peak falls at either window edge, or if
## no valid pre-/post-season minimum or green-up/brown-down day exists.
##
## M8, as implemented: smoothed GPP is linearly rescaled to [0, 1] using
## that site-year window's own min/max (flagged if the range is zero). GSL
## for threshold `frac` (0.10, 0.25, 0.50) is the single contiguous run of
## days with normalized value > frac that CONTAINS the day of the window's
## maximum ("the crossing closest to the seasonal peak").
## ============================================================================
m7_one <- function(win, gpp_sm, gpp_raw) {
  n <- length(win)
  flagged <- function() tibble(gsl = NA_integer_, start_date = as.Date(NA), end_date = as.Date(NA),
                                 gpp_in_season = NA_real_, annual_gpp = sum(gpp_raw, na.rm = TRUE), share = NA_real_,
                                 flag_no_season = TRUE, flag_no_dt = FALSE)
  if (sum(!is.na(gpp_sm)) < 10) return(flagged())
  peak_idx <- which.max(gpp_sm)
  if (peak_idx <= 1 || peak_idx >= n) return(flagged())

  pre_idx <- 1:peak_idx
  preseason_min_idx <- pre_idx[which.min(gpp_sm[pre_idx])]
  post_idx <- peak_idx:n
  postseason_min_idx <- post_idx[which.min(gpp_sm[post_idx])]
  if (preseason_min_idx >= peak_idx || postseason_min_idx <= peak_idx) return(flagged())

  deriv <- diff(gpp_sm) # deriv[i] = gpp_sm[i+1] - gpp_sm[i]
  rise_range <- preseason_min_idx:(peak_idx - 1)
  if (length(rise_range) == 0 || all(is.na(deriv[rise_range]))) return(flagged())
  greenup_idx <- rise_range[which.max(deriv[rise_range])]
  fall_range <- peak_idx:(postseason_min_idx - 1)
  if (length(fall_range) == 0 || all(is.na(deriv[fall_range]))) return(flagged())
  brownup_idx <- fall_range[which.min(deriv[fall_range])]

  start_idx <- round((preseason_min_idx + greenup_idx) / 2)
  end_idx <- round((brownup_idx + postseason_min_idx) / 2)
  if (end_idx <= start_idx) return(flagged())

  annual_gpp <- sum(gpp_raw, na.rm = TRUE)
  gpp_in_season <- sum(gpp_raw[start_idx:end_idx], na.rm = TRUE)
  tibble(gsl = end_idx - start_idx + 1L, start_date = win[start_idx], end_date = win[end_idx],
         gpp_in_season = gpp_in_season, annual_gpp = annual_gpp,
         share = if (annual_gpp != 0) gpp_in_season / annual_gpp else NA_real_,
         flag_no_season = FALSE, flag_no_dt = FALSE)
}
no_dt_row_simple <- function() tibble(gsl = NA_integer_, start_date = as.Date(NA), end_date = as.Date(NA),
                                       gpp_in_season = NA_real_, annual_gpp = NA_real_, share = NA_real_,
                                       flag_no_season = FALSE, flag_no_dt = TRUE)

run_m7 <- function(variant, gcol_sm, gcol) {
  out <- list()
  for (sid in IAV_SITES) {
    yrs <- season_years_for(sid)
    if (variant == "dt" && sid %in% sites_no_dt) {
      for (y in yrs) out[[length(out) + 1]] <- no_dt_row_simple() |> mutate(site_id = sid, year = y, variant = variant, method = "M7", sub = "span")
      next
    }
    d <- daily_cont |> filter(site_id == sid)
    for (y in yrs) {
      win <- window_dates(sid, y)
      dy <- d |> filter(TIMESTAMP %in% win)
      gpp_sm <- dy[[gcol_sm]][match(win, dy$TIMESTAMP)]
      gpp_raw <- dy[[gcol]][match(win, dy$TIMESTAMP)]
      out[[length(out) + 1]] <- m7_one(win, gpp_sm, gpp_raw) |> mutate(site_id = sid, year = y, variant = variant, method = "M7", sub = "span")
    }
  }
  bind_rows(out)
}

message("Running M7 (Ma et al. 2013 amplitude/derivative) for 2 series x 15 sites...")
m7_nt <- run_m7("nt", "gpp_nt_sm", "gpp_nt")
m7_dt <- run_m7("dt", "gpp_dt_sm", "gpp_dt")
m7_all <- bind_rows(m7_nt, m7_dt)
message("M7: ", nrow(m7_all), " site x year x series rows. No-season flags: ", sum(m7_all$flag_no_season))

m8_one <- function(win, gpp_sm, gpp_raw, frac) {
  n <- length(win)
  flagged <- function() tibble(gsl = NA_integer_, start_date = as.Date(NA), end_date = as.Date(NA),
                                 gpp_in_season = NA_real_, annual_gpp = sum(gpp_raw, na.rm = TRUE), share = NA_real_,
                                 flag_no_season = TRUE, flag_no_dt = FALSE)
  if (sum(!is.na(gpp_sm)) < 5) return(flagged())
  rng <- range(gpp_sm, na.rm = TRUE)
  if (!is.finite(diff(rng)) || diff(rng) <= 0) return(flagged())
  norm <- (gpp_sm - rng[1]) / diff(rng)
  peak_idx <- which.max(norm)
  above <- !is.na(norm) & norm > frac
  if (!isTRUE(above[peak_idx])) return(flagged())
  run_s <- peak_idx; while (run_s > 1 && isTRUE(above[run_s - 1])) run_s <- run_s - 1
  run_e <- peak_idx; while (run_e < n && isTRUE(above[run_e + 1])) run_e <- run_e + 1

  annual_gpp <- sum(gpp_raw, na.rm = TRUE)
  gpp_in_season <- sum(gpp_raw[run_s:run_e], na.rm = TRUE)
  tibble(gsl = run_e - run_s + 1L, start_date = win[run_s], end_date = win[run_e],
         gpp_in_season = gpp_in_season, annual_gpp = annual_gpp,
         share = if (annual_gpp != 0) gpp_in_season / annual_gpp else NA_real_,
         flag_no_season = FALSE, flag_no_dt = FALSE)
}

run_m8 <- function(variant, gcol_sm, gcol, fracs) {
  out <- list()
  for (sid in IAV_SITES) {
    yrs <- season_years_for(sid)
    if (variant == "dt" && sid %in% sites_no_dt) {
      for (y in yrs) for (frac in fracs) out[[length(out) + 1]] <- no_dt_row_simple() |> mutate(site_id = sid, year = y, variant = variant, method = "M8", sub = sprintf("%.2f", frac))
      next
    }
    d <- daily_cont |> filter(site_id == sid)
    for (y in yrs) {
      win <- window_dates(sid, y)
      dy <- d |> filter(TIMESTAMP %in% win)
      gpp_sm <- dy[[gcol_sm]][match(win, dy$TIMESTAMP)]
      gpp_raw <- dy[[gcol]][match(win, dy$TIMESTAMP)]
      for (frac in fracs) out[[length(out) + 1]] <- m8_one(win, gpp_sm, gpp_raw, frac) |> mutate(site_id = sid, year = y, variant = variant, method = "M8", sub = sprintf("%.2f", frac))
    }
  }
  bind_rows(out)
}

M8_FRACS <- c(0.10, 0.25, 0.50)
message("Running M8 (Panwar et al. 2023 normalized threshold, 0.10/0.25/0.50) for 2 series x 15 sites...")
m8_nt <- run_m8("nt", "gpp_nt_sm", "gpp_nt", M8_FRACS)
m8_dt <- run_m8("dt", "gpp_dt_sm", "gpp_dt", M8_FRACS)
m8_all <- bind_rows(m8_nt, m8_dt)
message("M8: ", nrow(m8_all), " site x year x series x threshold rows. No-season flags: ", sum(m8_all$flag_no_season))

## ============================================================================
## Assemble the unified site-year table (one row per site x year x variant x
## method x sub). Harmonise columns across all 8 methods' outputs.
## ============================================================================
flag_cols <- c("flag_no_season", "flag_whole_year", "flag_net_source", "flag_two_season",
               "flag_no_start_crossing", "flag_no_end_crossing", "flag_no_dt", "flag_no_m5_span",
               "flag_no_run5", "flag_no_smpeak")
extra_cols <- c("threshold_pct_of_P99", "k_range_start_days", "k_range_end_days", "two_season_ratio",
                "two_season_primary_date", "two_season_secondary_date",
                "gsl_run5", "start_date_run5", "end_date_run5",
                "gsl_smpeak", "start_date_smpeak", "end_date_smpeak")

harmonise <- function(tbl) {
  for (fc in flag_cols) if (!fc %in% names(tbl)) tbl[[fc]] <- FALSE
  for (ec in extra_cols) if (!ec %in% names(tbl)) tbl[[ec]] <- NA
  tbl |> select(site_id, year, variant, method, sub, gsl, start_date, end_date, share, gpp_in_season,
                 annual_gpp, all_of(flag_cols), all_of(extra_cols))
}

gsl_methods_site_year <- bind_rows(
  harmonise(m234_all), harmonise(m1_all), harmonise(m5_all), harmonise(m6_all), harmonise(m7_all), harmonise(m8_all)
) |> arrange(site_id, year, method, variant, sub)

message("Assembled gsl_methods_site_year: ", nrow(gsl_methods_site_year), " rows.")
unaccounted <- gsl_methods_site_year |> filter(is.na(gsl) & !flag_no_season & !flag_net_source & !flag_no_dt)
if (nrow(unaccounted) > 0) {
  print(unaccounted)
  stop("11_gsl_methods_compare.R: ", nrow(unaccounted), " row(s) have neither a GSL value nor an explanatory flag -- stopping.")
}
message("Check passed: every row (every method and series) has a GSL value or an explanatory flag (", nrow(unaccounted), " unaccounted rows).")

## ============================================================================
## Per-site summary. Unchanged from v2.
## ============================================================================
annual_corr_by_variant <- list(nt = annual_corr_nt, dt = annual_corr_dt, all = annual_corr_nt)
mean_doy <- function(dates) { ok <- dates[!is.na(dates)]; if (length(ok) == 0) return(NA_real_); mean(lubridate::yday(ok)) }

summarise_group <- function(g) {
  sid <- g$site_id[1]; variant <- g$variant[1]
  ac <- annual_corr_by_variant[[variant]]
  gj <- g |> filter(!flag_net_source) |> left_join(ac, by = c("site_id", "year"), suffix = c("", "_corr"))
  rg <- detrend_cor(gj$gsl, gj$annual_gpp_corr, gj$year)
  rn <- detrend_cor(gj$gsl, gj$annual_nep, gj$year)
  tibble(
    site_id = sid, variant = variant, method = g$method[1], sub = g$sub[1],
    n_site_years = nrow(g), n_used_for_means = sum(!g$flag_net_source & !is.na(g$gsl)),
    mean_gsl = mean(g$gsl[!g$flag_net_source], na.rm = TRUE), sd_gsl = sd(g$gsl[!g$flag_net_source], na.rm = TRUE),
    mean_start_doy = mean_doy(g$start_date[!g$flag_net_source]), mean_end_doy = mean_doy(g$end_date[!g$flag_net_source]),
    mean_share = mean(g$share[!g$flag_net_source], na.rm = TRUE),
    n_flag_no_season = sum(g$flag_no_season, na.rm = TRUE), n_flag_whole_year = sum(g$flag_whole_year, na.rm = TRUE),
    n_flag_net_source = sum(g$flag_net_source, na.rm = TRUE), n_flag_two_season = sum(g$flag_two_season, na.rm = TRUE),
    n_flag_no_start_crossing = sum(g$flag_no_start_crossing, na.rm = TRUE), n_flag_no_end_crossing = sum(g$flag_no_end_crossing, na.rm = TRUE),
    n_flag_no_dt = sum(g$flag_no_dt, na.rm = TRUE), n_flag_no_m5_span = sum(g$flag_no_m5_span, na.rm = TRUE),
    r_gpp = unname(rg["r"]), p_gpp = unname(rg["p"]), n_gpp = unname(rg["n"]),
    r_nep = unname(rn["r"]), p_nep = unname(rn["p"]), n_nep = unname(rn["n"])
  )
}
gsl_methods_site_year$.key <- paste(gsl_methods_site_year$site_id, gsl_methods_site_year$variant,
                                      gsl_methods_site_year$method, gsl_methods_site_year$sub, sep = "\u0001")
groups <- split(gsl_methods_site_year, gsl_methods_site_year$.key)
gsl_methods_site_year$.key <- NULL
gsl_methods_site_summary <- bind_rows(lapply(groups, summarise_group)) |> arrange(site_id, method, sub, variant)
message("Assembled gsl_methods_site_summary: ", nrow(gsl_methods_site_summary), " rows.")

## ============================================================================
## Net-source and no-DT exclusions/unknowns. Unchanged from v2.
## ============================================================================
net_source_rows <- gsl_methods_site_year |> filter(method %in% c("M5", "M6"), flag_net_source)
for (i in seq_len(nrow(net_source_rows))) {
  log_exclusion(site_id = net_source_rows$site_id[i], variable = paste0(net_source_rows$method[i], "-", net_source_rows$sub[i]),
                timestamp = net_source_rows$year[i], reason = "Net-source year: fewer than 5 days of smoothed NEP > 0; excluded from M5/M6 site means and detrended-r",
                threshold = "5 days", excluded_by = "11_gsl_methods_compare.R", log_path = EXP_EXCLUSION_LOG)
}
no_season_rows <- gsl_methods_site_year |> filter(flag_no_season, !flag_net_source, !flag_no_dt)
for (i in seq_len(nrow(no_season_rows))) {
  log_unknown(record_id = sprintf("%s-%d-%s-%s-%s", no_season_rows$site_id[i], no_season_rows$year[i],
                                    no_season_rows$variant[i], no_season_rows$method[i], no_season_rows$sub[i]),
              reason = "No season detected for this method/series within this site-year's window",
              logged_by = "11_gsl_methods_compare.R", log_path = EXP_UNKNOWN_LOG)
}
message("Logged ", nrow(net_source_rows), " net-source exclusion(s) and ", nrow(no_season_rows), " no-season unknown(s).")

## ============================================================================
## Write main output tables (overwrite v2's, in place -- v2's content lives on
## in git history at e0348a5).
## ============================================================================
write_csv(gsl_methods_site_year, file.path(EXP_TABLES, "gsl_methods_site_year.csv"))
write_meta(
  file.path(EXP_TABLES, "gsl_methods_site_year.csv"),
  input_sources = c("data/duckdb/fluxnet.duckdb (daily, dataset='FLUXMET')", file.path(EXP_TABLES, "sites.csv"),
                      file.path(EXP_TABLES, "site_years.csv"), file.path(EXP_TABLES, "derived_site.csv"),
                      file.path(EXP_TABLES, "derived_site_year.csv")),
  notes = paste0(
    "VERSION 3 (supersedes v2, e0348a5 -- see docs/gsl_methods_compare_2026-11-02_v3.md). Eight GSL ",
    "methods (M1-M8) at the 15 'Figure GSL-IAV' sites, on two independent GPP series (nt, dt), never ",
    "mixed. M2 kept at 5/10/20% of P99 (full 22-value grid in gsl_methods_m2_grid22.csv, NT only); M4 ",
    "at 0.85/0.90/0.95/0.99 of annual GPP via the fine grid. M2-M4 carry 3 start/end rules: first/last ",
    "qualifying day (start_date/end_date), first/last >=5-day run (start_date_run5/end_date_run5), and ",
    "the smoothed-threshold-crossing closest to the peak (start_date_smpeak/end_date_smpeak, Panwar et ",
    "al. 2023's rule). M6 has two forms (sub='anchored'/'unanchored'); figures use 'anchored' only. M7 ",
    "(Ma et al. 2013 amplitude/derivative) and M8 (Panwar et al. 2023 normalized threshold, 0.10/0.25/ ",
    "0.50) are new, on 15-day-smoothed GPP. Two-season flag (M5-span/M6 only) via the TIMESAT secondary- ",
    "peak rule on smoothed NT GPP. Checks (NH sites only, all passed exactly): NT-series M2@10%/M4@0.95 ",
    "== derived_site_year.csv's main_unscreened gsl_z16/gsl_t95/f_z16/f_t95; every row has a value or an ",
    "explanatory flag. M4@0.85-vs-fraction-sweep check is NOT APPLICABLE: fraction_sweep_main.csv only ",
    "holds the winter-screened 'main' variant, not 'main_unscreened'/NT. This comparison deliberately ",
    "does not frame any method as 'the reference' -- see the methods doc for the full rationale."
  )
)

write_csv(gsl_methods_site_summary, file.path(EXP_TABLES, "gsl_methods_site_summary.csv"))
write_meta(
  file.path(EXP_TABLES, "gsl_methods_site_summary.csv"),
  input_sources = file.path(EXP_TABLES, "gsl_methods_site_year.csv"),
  notes = "VERSION 3. Per site x series x method x sub-threshold: mean GSL, interannual SD, mean start/end day-of-window, mean share of annual GPP inside the season, flag counts, and detrended Pearson r (GPP, NEP). Net-source years excluded from M5/M6 means/r."
)
message("Saved: gsl_methods_site_year.csv (", nrow(gsl_methods_site_year), " rows), gsl_methods_site_summary.csv (", nrow(gsl_methods_site_summary), " rows).")

## ============================================================================
## Item 1 report data (DT minus NT) + item 6 (signed shifts by site/group).
## ============================================================================
dt_vs_nt <- gsl_methods_site_year |> filter(variant %in% c("nt", "dt"), !flag_no_dt) |>
  select(site_id, year, method, sub, variant, gsl, start_date, end_date) |>
  pivot_wider(names_from = variant, values_from = c(gsl, start_date, end_date)) |>
  filter(!is.na(gsl_nt) | !is.na(gsl_dt)) |>
  mutate(
    dt_minus_nt_gsl = gsl_dt - gsl_nt,
    dt_minus_nt_start_days = as.numeric(start_date_dt - start_date_nt),
    dt_minus_nt_end_days = as.numeric(end_date_dt - end_date_nt),
    boundary_explained = dt_minus_nt_end_days - dt_minus_nt_start_days,
    interior_contribution = dt_minus_nt_gsl - boundary_explained
  ) |>
  left_join(site_meta |> select(site_id, group, igbp), by = "site_id")
write_csv(dt_vs_nt, file.path(EXP_TABLES, "gsl_methods_dt_vs_nt.csv"))
write_meta(
  file.path(EXP_TABLES, "gsl_methods_dt_vs_nt.csv"),
  input_sources = file.path(EXP_TABLES, "gsl_methods_site_year.csv"),
  notes = "VERSION 3. DT-minus-NT difference in GSL/start/end (signed), per site x year x method x sub, now including M7/M8, with the site's vegetation `group`/`igbp` attached for item 6's by-group aggregation."
)

dt_vs_nt_by_group <- dt_vs_nt |>
  group_by(group) |>
  summarise(n = sum(!is.na(dt_minus_nt_start_days)),
            mean_start_shift = mean(dt_minus_nt_start_days, na.rm = TRUE),
            mean_end_shift = mean(dt_minus_nt_end_days, na.rm = TRUE), .groups = "drop")
dt_vs_nt_by_site <- dt_vs_nt |>
  group_by(site_id, group) |>
  summarise(n = sum(!is.na(dt_minus_nt_start_days)),
            mean_start_shift = mean(dt_minus_nt_start_days, na.rm = TRUE),
            mean_end_shift = mean(dt_minus_nt_end_days, na.rm = TRUE), .groups = "drop")
write_csv(dt_vs_nt_by_site, file.path(EXP_TABLES, "gsl_methods_dt_vs_nt_by_site_group.csv"))
write_meta(
  file.path(EXP_TABLES, "gsl_methods_dt_vs_nt_by_site_group.csv"),
  input_sources = file.path(EXP_TABLES, "gsl_methods_dt_vs_nt.csv"),
  notes = "VERSION 3, item 6. Mean signed DT-minus-NT start/end shift (days), PER SITE, with its vegetation group attached. Positive start shift = DT starts later than NT (NT season begins earlier) -- the direction Keenan et al. (2019) predicts for forests, from NT's early-spring GPP overestimate."
)
write_csv(dt_vs_nt_by_group, file.path(EXP_TABLES, "gsl_methods_dt_vs_nt_by_group.csv"))
write_meta(
  file.path(EXP_TABLES, "gsl_methods_dt_vs_nt_by_group.csv"),
  input_sources = file.path(EXP_TABLES, "gsl_methods_dt_vs_nt.csv"),
  notes = "VERSION 3, item 6. Mean signed DT-minus-NT start/end shift (days), pooled by vegetation group (deciduous forest, evergreen forest, grassland, woody-grass mosaic)."
)
message("DT-vs-NT by vegetation group (signed start/end shift, days):")
print(as.data.frame(dt_vs_nt_by_group))

## ============================================================================
## Item 3 report data: how far start/end move between the 3 count-method rules.
## ============================================================================
rule_shifts <- gsl_methods_site_year |> filter(method %in% c("M2", "M3", "M4"), variant == "nt") |>
  mutate(
    shift_start_ab = abs(as.numeric(start_date_run5 - start_date)),
    shift_start_ac = abs(as.numeric(start_date_smpeak - start_date)),
    shift_end_ab = abs(as.numeric(end_date_run5 - end_date)),
    shift_end_ac = abs(as.numeric(end_date_smpeak - end_date))
  )
rule_shifts_by_site <- rule_shifts |> group_by(site_id) |>
  summarise(median_shift_start_ab = median(shift_start_ab, na.rm = TRUE), median_shift_start_ac = median(shift_start_ac, na.rm = TRUE),
            median_shift_end_ab = median(shift_end_ab, na.rm = TRUE), median_shift_end_ac = median(shift_end_ac, na.rm = TRUE),
            mean_shift_start_ac = mean(shift_start_ac, na.rm = TRUE), mean_shift_end_ac = mean(shift_end_ac, na.rm = TRUE),
            .groups = "drop") |> arrange(desc(median_shift_start_ac + median_shift_end_ac))
write_csv(rule_shifts_by_site, file.path(EXP_TABLES, "gsl_methods_rule_shifts.csv"))
write_meta(
  file.path(EXP_TABLES, "gsl_methods_rule_shifts.csv"),
  input_sources = file.path(EXP_TABLES, "gsl_methods_site_year.csv"),
  notes = "VERSION 3, item 3. For M2-M4 (NT series), MEDIAN absolute start/end-date shift (days) between rule (a) first/last qualifying day and rule (b) first/last >=5-day run [_ab], and between (a) and rule (c) Panwar's smoothed-crossing-nearest-peak [_ac], per site across all methods/subs/years (median preferred over mean -- a handful of site-years with isolated far-from-peak qualifying days under M3's crude absolute threshold produce extreme outliers that dominate a mean; mean_shift_*_ac kept alongside for transparency)."
)
message("Rule shifts by site (median, largest first):"); print(as.data.frame(head(rule_shifts_by_site, 5)))

## ============================================================================
## Item 5: method agreement -- Spearman r AND median absolute start/end-date
## difference, every method pair. M6 uses the anchored form only.
## ============================================================================
FIG_METHODS <- tibble(
  method = c("M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8"),
  sub    = c("summer", "10pct", "1gC", "0.85", "span", "anchored", "span", "0.25"),
  label  = c("M1 (crossing, summer P99)", "M2 (10% of P99)", "M3 (1 gC m-2 d-1)",
             "M4 (0.85 of annual GPP)", "M5 (NEP>0 span)", "M6 (Hu NEP rate-of-change)",
             "M7 (amplitude/derivative)", "M8 (normalized 0.25)")
)

wide_base <- gsl_methods_site_year |>
  inner_join(FIG_METHODS, by = c("method", "sub")) |>
  filter(variant %in% c("nt", "all")) |>
  mutate(start_doy = lubridate::yday(start_date), end_doy = lubridate::yday(end_date)) |>
  select(site_id, year, label, gsl, start_date, end_date, start_doy, end_doy)

## ---- Spearman r on SITE-DEMEANED anomalies (not raw pooled values/dates):
## pooling raw dates across sites/years is dominated by which site/year a
## row belongs to (a trivial, spurious ~1.0 correlation for any method pair),
## not by genuine within-site-year method agreement. ----
anomaly_matrix <- function(value_col) {
  w <- wide_base |> select(site_id, year, label, value = all_of(value_col)) |>
    group_by(site_id, label) |> mutate(anomaly = value - mean(value, na.rm = TRUE)) |> ungroup() |>
    select(site_id, year, label, anomaly) |> pivot_wider(names_from = label, values_from = anomaly)
  m <- as.matrix(w |> select(-site_id, -year))
  cor(m, method = "spearman", use = "pairwise.complete.obs")
}
cor_gsl <- anomaly_matrix("gsl")
start_cor <- anomaly_matrix("start_doy")
end_cor   <- anomaly_matrix("end_doy")

## ---- Median absolute date DIFFERENCE (item 5) uses raw Date arithmetic
## (matched by site_id+year, so no yday-wraparound issue -- not a correlation,
## just a magnitude). ----
date_mat <- function(date_col) {
  wide_base |> select(site_id, year, label, value = all_of(date_col)) |> pivot_wider(names_from = label, values_from = value)
}
start_mat <- date_mat("start_date"); end_mat <- date_mat("end_date")

median_abs_diff_matrix <- function(wide_dates) {
  labs <- setdiff(names(wide_dates), c("site_id", "year"))
  m <- matrix(NA_real_, length(labs), length(labs), dimnames = list(labs, labs))
  for (a in labs) for (b in labs) {
    diffs <- abs(as.numeric(wide_dates[[a]] - wide_dates[[b]]))
    m[a, b] <- if (a == b) 0 else median(diffs, na.rm = TRUE)
  }
  m
}
mad_start <- median_abs_diff_matrix(start_mat)
mad_end   <- median_abs_diff_matrix(end_mat)

cor_to_long <- function(m, which_lab) {
  d <- as.data.frame(m); d$row <- rownames(d)
  pivot_longer(d, -row, names_to = "col", values_to = "value") |> mutate(which = which_lab)
}
agreement_long <- bind_rows(
  cor_to_long(cor_gsl, "GSL") |> mutate(metric = "spearman_r"),
  cor_to_long(start_cor, "Start date") |> mutate(metric = "spearman_r"),
  cor_to_long(end_cor, "End date") |> mutate(metric = "spearman_r"),
  cor_to_long(mad_start, "Start date") |> mutate(metric = "median_abs_diff_days"),
  cor_to_long(mad_end, "End date") |> mutate(metric = "median_abs_diff_days")
) |> filter(row != col)
write_csv(agreement_long, file.path(EXP_TABLES, "gsl_methods_agreement.csv"))
write_meta(
  file.path(EXP_TABLES, "gsl_methods_agreement.csv"),
  input_sources = file.path(EXP_TABLES, "gsl_methods_site_year.csv"),
  notes = "VERSION 3, item 5. Every method pair (M1-M8, M6='anchored'), Spearman r (site-year anomalies for GSL; raw dates for start/end) and median absolute start/end-date difference (days), pooled across all 15 sites x years, pairwise-complete."
)
message("Method agreement table saved (", nrow(agreement_long), " pair x metric rows).")

## ============================================================================
## Item 7: management/disturbance table from BADM + literature overrides.
## BADM has NO dated event records anywhere in the network (confirmed in
## review/figures/methods_badm_management.md) -- only a coarse categorical
## DOM_DIST_MGMT tag and SITE_DESC keyword mining. "None found" -> unknown,
## never "unmanaged" (the one documented exception, per item 7, is US-Ha1's
## explicit DOM_DIST_MGMT="Undisturbed" tag, which IS a positive BADM record
## saying no management applies -- reported as 'other', not 'unknown', with
## that distinction noted).
## ============================================================================
badm15 <- badm_coverage |> filter(site_id %in% IAV_SITES)
site_management_15 <- tibble::tribble(
  ~site_id, ~management_type, ~date_detail, ~source,
  "AT-Neu", "mown", "Cuts: 2-16 Jun; 24 Jul-12 Aug; 21 Sep-28 Oct (recurring annual pattern, year not specified)", "Hammerle et al. (2008); BADM SITE_DESC keyword 'harvest' (mgmt_harvest=TRUE)",
  "AT-Neu", "other", "Manure application, late autumn (no exact date given)", "Hammerle et al. (2008)",
  "AU-How", "unknown", NA, "BADM: no DOM_DIST_MGMT tag, no SITE_DESC management keyword hit",
  "CH-Dav", "other", NA, "BADM SITE_DESC keyword 'till/plow/disk' (mgmt_tillage=TRUE) -- plausible historical/non-forest land-use mention at this ENF site, not independently verified as current active management",
  "DE-Tha", "unknown", NA, "BADM: no management/disturbance record found",
  "FR-Pue", "unknown", NA, "BADM: no management/disturbance record found",
  "IL-Yat", "grazed", NA, "BADM SITE_DESC/DOM_DIST_MGMT (mgmt_grazing=TRUE)",
  "IL-Yat", "burned", NA, "BADM SITE_DESC keyword 'burn/fire' (mgmt_burning=TRUE)",
  "IL-Yat", "other", NA, "BADM SITE_DESC keyword 'irrigat' (mgmt_irrigation=TRUE) -- planted pine afforestation",
  "JP-Api", "unknown", NA, "BADM: no management/disturbance record found",
  "NL-Loo", "unknown", NA, "BADM: no management/disturbance record found",
  "US-Ha1", "other", NA, "BADM DOM_DIST_MGMT = 'Undisturbed' -- an explicit no-active-management tag, distinct from 'unknown' (no record at all)",
  "US-MMS", "other", NA, "BADM DOM_DIST_MGMT = 'Forestry' (mgmt_forestry_unspecified=TRUE) -- silvicultural management present, harvest vs. thinning not resolved by BADM",
  "US-Mpj", "other", NA, "BADM DOM_DIST_MGMT = 'Drought; Pests and disease' -- natural disturbance, not active management",
  "US-SRM", "grazed", NA, "BADM DOM_DIST_MGMT = 'Grazing; Land cover change' (mgmt_grazing=TRUE) -- BADM confirms grazing, so reported as confirmed (not 'unconfirmed')",
  "US-Ton", "grazed", NA, "BADM DOM_DIST_MGMT = 'Grazing' (mgmt_grazing=TRUE)",
  "US-Var", "grazed", "Brush removed for cattle grazing (no dates given)", "BADM DOM_DIST_MGMT = 'Grazing' (mgmt_grazing=TRUE); FLUXNET site page",
  "US-Wkg", "grazed", NA, "BADM DOM_DIST_MGMT = 'Grazing' (mgmt_grazing=TRUE) -- BADM confirms grazing, so reported as confirmed (not 'unconfirmed')"
) |> left_join(site_meta |> select(site_id, igbp, group), by = "site_id") |>
  select(site_id, igbp, group, management_type, date_detail, source)
stopifnot(setequal(unique(site_management_15$site_id), IAV_SITES))
write_csv(site_management_15, file.path(EXP_TABLES, "site_management_15.csv"))
write_meta(
  file.path(EXP_TABLES, "site_management_15.csv"),
  input_sources = c("data/snapshots/badm_management_coverage.csv", "review/figures/methods_badm_management.md"),
  notes = "VERSION 3, item 7. Per site, management/disturbance type (mown, grazed, burned, other, unknown), dated detail where known, and source. Multiple rows per site where BADM records more than one category (e.g. IL-Yat: grazed, burned, other/irrigation). BADM itself has no dated management-event records anywhere in the FLUXNET Shuttle network (confirmed in methods_badm_management.md) -- the only dated detail here is from the literature (AT-Neu, Hammerle et al. 2008). US-SRM/US-Wkg grazing is BADM-confirmed, not left as 'unconfirmed' per the prompt's conditional."
)
message("Saved site_management_15.csv (", nrow(site_management_15), " rows).")

## ============================================================================
## Item 8: AT-Neu mowing test. For each AT-Neu site-year flagged two-season
## (M5-span), test whether the TIMESAT secondary smoothed-GPP peak falls 2-4
## weeks (14-28 days) after the END of one of the 3 recurring cut windows.
## ============================================================================
cut_windows <- tibble(cut = c("cut1_jun", "cut2_jul_aug", "cut3_sep_oct"),
                       end_md = c("06-16", "08-12", "10-28"))
atneu_two_season <- m5_all |> filter(site_id == "AT-Neu", sub == "span", flag_two_season, !is.na(two_season_secondary_date))

atneu_mowing_test <- purrr::pmap_dfr(
  list(atneu_two_season$year, atneu_two_season$two_season_secondary_date),
  function(y, sec_date) {
    purrr::map_dfr(seq_len(nrow(cut_windows)), function(i) {
      cut_end <- as.Date(sprintf("%d-%s", y, cut_windows$end_md[i]))
      days_after <- as.numeric(sec_date - cut_end)
      tibble(year = y, secondary_peak_date = sec_date, cut = cut_windows$cut[i], cut_end_date = cut_end,
             days_after_cut_end = days_after, consistent_with_mowing = days_after >= 14 & days_after <= 28)
    })
  }
)
if (nrow(atneu_mowing_test) > 0) {
  atneu_mowing_summary <- atneu_mowing_test |> group_by(year) |>
    summarise(any_consistent = any(consistent_with_mowing), best_cut = cut[which.min(abs(days_after_cut_end - 21))][1],
              best_days_after = days_after_cut_end[which.min(abs(days_after_cut_end - 21))][1], .groups = "drop")
} else {
  atneu_mowing_summary <- tibble(year = integer(0), any_consistent = logical(0), best_cut = character(0), best_days_after = numeric(0))
}
write_csv(atneu_mowing_test, file.path(EXP_TABLES, "atneu_mowing_test.csv"))
write_meta(
  file.path(EXP_TABLES, "atneu_mowing_test.csv"),
  input_sources = file.path(EXP_TABLES, "gsl_methods_site_year.csv"),
  notes = "VERSION 3, item 8. For every AT-Neu site-year flagged two-season (M5-span, TIMESAT), the number of days between the secondary smoothed-GPP peak and the end of each of AT-Neu's 3 recurring cut windows (Hammerle et al. 2008); consistent_with_mowing = TRUE if that gap is 14-28 days (2-4 weeks)."
)
message("AT-Neu two-season years: ", nrow(atneu_two_season), ". Mowing-consistent: ",
        if (nrow(atneu_mowing_summary) > 0) sum(atneu_mowing_summary$any_consistent) else 0, " of ", nrow(atneu_mowing_summary))
print(as.data.frame(atneu_mowing_summary))

## ============================================================================
## Item 9: AU-How -- M7 vs. Ma et al. (2013); wet-season GPP/amplitude IAV;
## cross-method GSL SD (threshold-GSL carries no interannual signal there).
## ============================================================================
auhow_m7_nt <- gsl_methods_site_year |> filter(site_id == "AU-How", method == "M7", variant == "nt")
message("AU-How M7 (NT) GSL per year: ", paste(sprintf("%d=%s", auhow_m7_nt$year, auhow_m7_nt$gsl), collapse = ", "),
        "; mean = ", round(mean(auhow_m7_nt$gsl, na.rm = TRUE), 1), " d (Ma et al. 2013: 244-275 d at Howard Springs).")

auhow_wet_season <- purrr::map_dfr(auhow_new_years, function(y) {
  win <- seq(as.Date(sprintf("%d-10-15", y)), as.Date(sprintf("%d-04-15", y + 1L)), by = "day")
  d <- daily_cont |> filter(site_id == "AU-How", TIMESTAMP %in% win)
  tibble(year = y, wet_season_gpp = sum(d$gpp_nt, na.rm = TRUE),
         seasonal_amplitude = { full <- daily_cont |> filter(site_id == "AU-How", TIMESTAMP %in% window_dates("AU-How", y));
                                  if (all(is.na(full$gpp_nt_sm))) NA_real_ else max(full$gpp_nt_sm, na.rm = TRUE) - min(full$gpp_nt_sm, na.rm = TRUE) })
})
write_csv(auhow_wet_season, file.path(EXP_TABLES, "auhow_wet_season_v3.csv"))
write_meta(
  file.path(EXP_TABLES, "auhow_wet_season_v3.csv"),
  input_sources = file.path(EXP_TABLES, "gsl_methods_site_year.csv"),
  notes = "VERSION 3, item 9. AU-How, per Jul-Jun season year: wet-season (15 Oct-15 Apr, Moore et al. 2017) total NT GPP, and the seasonal amplitude (max-min) of 15-day-smoothed NT GPP over the full season-year window."
)
message("AU-How wet-season GPP: mean = ", round(mean(auhow_wet_season$wet_season_gpp, na.rm = TRUE), 1),
        " gC/m2, SD = ", round(sd(auhow_wet_season$wet_season_gpp, na.rm = TRUE), 1),
        "; seasonal amplitude: mean = ", round(mean(auhow_wet_season$seasonal_amplitude, na.rm = TRUE), 2),
        " gC/m2/d, SD = ", round(sd(auhow_wet_season$seasonal_amplitude, na.rm = TRUE), 2))

auhow_method_means <- gsl_methods_site_summary |> inner_join(FIG_METHODS, by = c("method", "sub")) |>
  filter(site_id == "AU-How", variant %in% c("nt", "all"))
auhow_cross_method_sd <- sd(auhow_method_means$mean_gsl, na.rm = TRUE)
message("AU-How cross-method SD of mean GSL (across M1-M8's headline sub, NT/NEP-only): ",
        round(auhow_cross_method_sd, 1), " d (mean GSLs: ", paste(sprintf("%s=%.0f", auhow_method_means$label, auhow_method_means$mean_gsl), collapse = ", "), ")")
message("AU-How within-method interannual r vs. annual GPP/NEP (detrended): ",
        paste(sprintf("%s: r_gpp=%.2f(p=%.2f) r_nep=%.2f(p=%.2f)", auhow_method_means$label, auhow_method_means$r_gpp,
                       auhow_method_means$p_gpp, auhow_method_means$r_nep, auhow_method_means$p_nep), collapse = "; "))

## ============================================================================
## Figures. NT series for M1-M4/M7/M8; M5/M6 remain NEP-only ('all'); M6 uses
## the ANCHORED form only. No figure/label names "Z16", "T85" or "T95".
## ============================================================================
drafts_dir <- file.path(GSL_FIG_DIR, "drafts")
if (!dir.exists(drafts_dir)) dir.create(drafts_dir, recursive = TRUE)

skew_tbl <- derived_site_all |> filter(site_id %in% IAV_SITES, variant == "main") |> select(site_id, skewness)
fig_data <- gsl_methods_site_summary |>
  inner_join(FIG_METHODS, by = c("method", "sub")) |>
  filter(variant %in% c("nt", "all")) |>
  left_join(skew_tbl, by = "site_id") |>
  left_join(m2_grid_range, by = "site_id") |>
  mutate(method_f = factor(label, levels = FIG_METHODS$label),
         xmin = ifelse(method == "M2", m2_grid_min, mean_gsl - sd_gsl),
         xmax = ifelse(method == "M2", m2_grid_max, mean_gsl + sd_gsl))

site_order <- skew_tbl |> arrange(skewness) |> pull(site_id)
fig_data$site_id <- factor(fig_data$site_id, levels = site_order)

## ---- Explicit, per-layer-consistent y offsets (not ggplot's position_dodge,
## which would compute dodge slots independently per layer and misalign the
## M2 range bar against the point/errorbar layers for other methods). ----
n_methods <- nrow(FIG_METHODS)
dodge_width <- 0.8
method_rank <- as.numeric(factor(fig_data$method_f, levels = FIG_METHODS$label))
fig_data$y_num <- as.numeric(fig_data$site_id) + (method_rank - (n_methods + 1) / 2) * (dodge_width / n_methods)

p_dotplot <- ggplot(fig_data, aes(y = y_num, colour = method_f)) +
  geom_errorbar(aes(xmin = xmin, xmax = xmax), orientation = "y", width = 0, alpha = 0.6) +
  geom_point(data = dplyr::filter(fig_data, method != "M2"), aes(x = mean_gsl), size = 1.6) +
  scale_y_continuous(breaks = seq_along(site_order), labels = site_order, minor_breaks = NULL) +
  scale_colour_brewer(palette = "Dark2", name = "Method") +
  labs(x = "GSL (days, NT/NEP-only series). M2 shown as its full 0.1-20%-of-P99 range; other methods: mean +/- interannual SD",
       y = NULL, title = "GSL methods dotplot (draft, v3)") +
  theme_gsl() + theme(legend.position = "right")

ggsave(file.path(drafts_dir, "GSL methods dotplot draft.png"), p_dotplot, width = PEERJ_FIG_WIDTH_MM * 1.3,
       height = 150, units = "mm", dpi = PEERJ_DPI, device = ragg::agg_png, bg = "white")
writeLines(c(
  "GSL methods dotplot (draft, v3). Mean GSL by method, for each of the 15 sites, sorted bottom-to-top",
  "by ascending skewness (main-variant). M1-summer, M2 (shown as its full range across Tash's 22-value,",
  "0.1-20%-of-P99 grid -- no single point, per the prompt), M3 1 gC m-2 d-1, M4 0.85 of annual GPP (one",
  "of 4 reported targets; chosen here only as a representative display value, not as 'the' method), M5-",
  "span, M6 (anchored form), M7 (amplitude/derivative), M8 (normalized 0.25). Net-source years excluded",
  "from M5/M6's own mean/SD. This figure deliberately shows 8 methods spanning a wide range of",
  "thresholds/rules -- it is not a comparison of any one method against a single reference.",
  "Numbers behind this figure: tables/gsl_methods_site_summary.csv, tables/gsl_methods_m2_grid22.csv."
), file.path(drafts_dir, "GSL methods dotplot draft.legend.txt"))
write_meta(
  file.path(drafts_dir, "GSL methods dotplot draft.png"),
  input_sources = c(file.path(EXP_TABLES, "gsl_methods_site_summary.csv"), file.path(EXP_TABLES, "gsl_methods_m2_grid22.csv")),
  notes = "VERSION 3. Draft figure. NT/NEP-only series; M2 shown as a range, not a point; explicit numeric y-offsets (not position_dodge) keep the M2 range bar aligned with the other methods' point+errorbar glyphs at the same site."
)
message("Saved: 'GSL methods dotplot draft'.png/.legend.txt/.meta.json")

## ============================================================================
## Figure B: method agreement (Spearman r only; the fuller Spearman + median-
## abs-diff numbers are in tables/gsl_methods_agreement.csv, item 5).
## ============================================================================
cor_long <- bind_rows(cor_to_long(cor_gsl, "GSL"), cor_to_long(start_cor, "Start date"), cor_to_long(end_cor, "End date")) |>
  rename(r = value) |>
  mutate(row = factor(row, levels = FIG_METHODS$label), col = factor(col, levels = rev(FIG_METHODS$label)),
         which = factor(which, levels = c("GSL", "Start date", "End date")))

p_agree <- ggplot(cor_long, aes(x = row, y = col, fill = r)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", r)), size = PEERJ_ANNOTATION_PT / .pt * 0.7) +
  scale_fill_gradient2(low = "#b2182b", mid = "white", high = "#2166ac", midpoint = 0, limits = c(-1, 1), name = "Spearman r") +
  facet_wrap(~ which, nrow = 1) +
  labs(x = NULL, y = NULL, title = "GSL methods agreement (draft, v3)") +
  theme_gsl() + theme(axis.text.x = element_text(angle = 40, hjust = 1), legend.position = "right")

ggsave(file.path(drafts_dir, "GSL methods agreement draft.png"), p_agree, width = PEERJ_FIG_WIDTH_MM * 2.0,
       height = 140, units = "mm", dpi = PEERJ_DPI, device = ragg::agg_png, bg = "white")
writeLines(c(
  "GSL methods agreement (draft, v3). Spearman correlation between pairs of methods' annual GSL (left",
  "panel; site-year anomalies), start date (middle) and end date (right; raw dates), pooled across all",
  "15 sites x years -- NT series for M1-M4/M7/M8, NEP-only ('all') for M5/M6 (anchored form only --",
  "M6's agreement with M5 is partly BUILT IN by its anchoring to M5's own span, not an independent",
  "confirmation). Median absolute start/end-date differences (days) for every pair are in",
  "tables/gsl_methods_agreement.csv, not shown on this tile plot.",
  "Numbers behind this figure: tables/gsl_methods_site_year.csv, tables/gsl_methods_agreement.csv."
), file.path(drafts_dir, "GSL methods agreement draft.legend.txt"))
write_meta(
  file.path(drafts_dir, "GSL methods agreement draft.png"),
  input_sources = c(file.path(EXP_TABLES, "gsl_methods_site_year.csv"), file.path(EXP_TABLES, "gsl_methods_agreement.csv")),
  notes = "VERSION 3. Draft figure. M6 = anchored form only; caption notes its agreement with M5 is partly built in by construction."
)
message("Saved: 'GSL methods agreement draft'.png/.legend.txt/.meta.json")

## ============================================================================
## Figure C: "raster" -- US-Ha1, US-Wkg, US-SRM, one row block per method.
## ============================================================================
RASTER_SITES <- c("US-Ha1", "US-Wkg", "US-SRM")
RASTER_METHODS <- FIG_METHODS |> filter(!(method == "M3"))

raster_data <- gsl_methods_site_year |>
  filter(site_id %in% RASTER_SITES, variant %in% c("nt", "all")) |>
  inner_join(RASTER_METHODS, by = c("method", "sub")) |>
  mutate(start_doy = lubridate::yday(start_date), end_doy = lubridate::yday(end_date),
         label = factor(label, levels = RASTER_METHODS$label), site_id = factor(site_id, levels = RASTER_SITES))

p_raster <- ggplot(raster_data) +
  geom_segment(aes(x = start_doy, xend = end_doy, y = year, yend = year), colour = "grey20",
               linewidth = nature_lwd(1.2), na.rm = TRUE) +
  facet_grid(label ~ site_id, scales = "free_y") +
  labs(x = "Day of year", y = "Year", title = "GSL methods raster (draft, v3)") +
  theme_gsl() + theme(strip.text.y = element_text(angle = 0, size = 6.5), strip.text.x = element_text(face = "bold"))

ggsave(file.path(drafts_dir, "GSL methods raster draft.png"), p_raster, width = PEERJ_FIG_WIDTH_MM * 1.3,
       height = 260, units = "mm", dpi = PEERJ_DPI, device = ragg::agg_png, bg = "white")
writeLines(c(
  "GSL methods raster (draft, v3). US-Ha1, US-Wkg, US-SRM (columns); each row block is one method",
  "(M1-summer, M2 10%, M4 0.85, M5-span, M6 anchored, M7, M8 0.25 of normalized range, top to bottom);",
  "each horizontal bar is one year's detected season, start day-of-year to end day-of-year. A missing",
  "bar means no season was detected -- see tables/gsl_methods_site_year.csv.",
  "Numbers behind this figure: tables/gsl_methods_site_year.csv."
), file.path(drafts_dir, "GSL methods raster draft.legend.txt"))
write_meta(
  file.path(drafts_dir, "GSL methods raster draft.png"),
  input_sources = file.path(EXP_TABLES, "gsl_methods_site_year.csv"),
  notes = "VERSION 3. Draft figure. Segments (start_doy-end_doy); all 3 sites are Northern Hemisphere; M6 = anchored form."
)
message("Saved: 'GSL methods raster draft'.png/.legend.txt/.meta.json")

## ============================================================================
## Write the M2 full-grid table last (figures already consumed it above).
## ============================================================================
write_csv(m2_grid_site_summary, file.path(EXP_TABLES, "gsl_methods_m2_grid22.csv"))
write_meta(
  file.path(EXP_TABLES, "gsl_methods_m2_grid22.csv"),
  input_sources = file.path(EXP_TABLES, "gsl_methods_site_year.csv"),
  notes = "VERSION 3, item 1. M2's full 22-value grid (0.1, 0.5, 1-20% of P99), NT series only: mean GSL per site x pct, averaged across that site's passing years. Feeds the dot plot's M2 range display; NOT part of the main gsl_methods_site_year/site_summary tables, which keep only 5/10/20%."
)
message("Saved: gsl_methods_m2_grid22.csv (", nrow(m2_grid_site_summary), " rows).")

message("--- 11_gsl_methods_compare.R: done (version 3) ---")
