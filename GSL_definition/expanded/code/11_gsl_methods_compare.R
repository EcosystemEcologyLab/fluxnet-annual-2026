## 11_gsl_methods_compare.R — GSL expanded analysis: preliminary comparison
## of EIGHT published/derived GSL definitions (M1-M8) at the 15 sites of the
## draft "Figure GSL-IAV" (tables/iav_15sites.csv). VERSION 4 -- revises
## version 3 (committed at cd7e547; logs/prompts/2026-11-02_gsl_methods_compare_v3.md)
## per Dave's follow-up decisions, logs/prompts/2026-11-02_gsl_methods_compare_v4.md.
## Purpose, per Tash (unchanged since v3): show that published GSL definitions
## span a wide range of low thresholds/rules, so NO single definition (Z16
## included) is the reference.
##
## Five items implemented:
##   1. Every method now runs under TWO year definitions, tagged by a new
##      `year_rule` column: "calendar" (v1-v3's rule, unchanged: NH = Jan-Dec,
##      AU-How = Jul-Jun) and "min_gpp_month" (each site's year starts on the
##      1st of its own lowest-mean-NT-GPP calendar month, determined once from
##      the calendar rule's own pooled passing years). Both rules use the same
##      <=14-failing-day QC test for a year to pass. Figures/checks/the other
##      report tables (items 2-5, below) use the CALENDAR rule only, matching
##      v1-v3 and the prompt's "checks as version 3, run under the calendar
##      rule"; a new cross-rule comparison table covers item 1 itself.
##   2. M7 (Ma et al. 2013) resmoothed: GPP aggregated to 16-day composites,
##      then (since the `Rssa` package is NOT installed, and CLAUDE.md
##      requires discussion before adding a new dependency / touching
##      renv.lock -- not available in this autonomous run) a per-site-year
##      harmonic regression (2 or 3 harmonics, chosen by mean R^2 across all
##      site-years and reported) fitted to those composites and interpolated
##      to daily values, replacing the 15-day running mean. M7's own
##      amplitude/derivative algorithm (m7_one()) is UNCHANGED -- only its
##      input curve's smoothing method changed, per the prompt ("'Halfway' is
##      in time, as implemented").
##   3. AT-Neu: the 2010 mowing test is rerun with Hortnagl & Wohlfahrt
##      (2014)'s actual published cut dates; a new cut detector (>=50% drop
##      in unsmoothed daily NT GPP within 1-3 days, followed by >=30% regrowth
##      over the following 14-28 days) is built, validated against the 2010/
##      2011 published dates, and applied to AT-Neu's full record; every
##      AT-Neu two-season year's between-peaks trough is tested against a
##      detected cut. v3's "management is the main correlate of two-season
##      years" claim is removed; AT-Neu (cuts) is reported separately from the
##      US Southwest dryland two-season sites (US-SRM/US-Var/US-Wkg, where
##      winter/monsoon bimodal rainfall is a more likely driver than a single
##      event-like cut).
##   4. tables/site_management_15.csv is fully replaced with a literature-
##      sourced table (BADM keyword mining, which produced false positives in
##      v3, is dropped).
##   5. v3's NT-vs-DT item-6 conclusion is rewritten: Keenan et al. (2019) DID
##      include grasslands (1 of 11 ecosystem types; apparent daytime-
##      respiration inhibition highest in open shrublands/savannas/wetlands,
##      lowest in evergreen broadleaf forest, tracking each ecosystem's own
##      fAPAR seasonal cycle) -- predicting an earlier NT start wherever
##      green-up is fast, consistent with AT-Neu AND US-Wkg (not just
##      deciduous forest, v3's reading). See the methods doc for the full
##      rewritten text; no new computation was needed for this item, since
##      the underlying dt_vs_nt-by-group numbers are unchanged from v3.
##
## Exploratory. Does NOT modify any existing script, table or figure other
## than this script's OWN previous outputs (regenerated in place where
## carried over from v3; v3's content lives on in git history at cd7e547).
## Writes only to this side project's own expanded/logs/, never the repo-root
## outputs/ logs. Read-only everywhere else.
source("GSL_definition/expanded/code/00_config.R")
options(gsl.skip_driver = TRUE)
source("GSL_definition/code/03_derive.R") # FINE_GRID_PCT only, reused for M4
options(gsl.skip_driver = FALSE)

suppressPackageStartupMessages({
  library(DBI); library(duckdb); library(dplyr); library(tidyr); library(readr)
  library(lubridate); library(purrr); library(ggplot2); library(patchwork)
  library(ggrepel); library(scales)
})

## ---- This script's own log paths, inside expanded/. ----
EXP_EXCLUSION_LOG <- file.path(EXP_ROOT, "logs", "exclusion_log.csv")
EXP_UNKNOWN_LOG   <- file.path(EXP_ROOT, "logs", "unknown_log.csv")
if (!dir.exists(file.path(EXP_ROOT, "logs"))) dir.create(file.path(EXP_ROOT, "logs"), recursive = TRUE)

## ============================================================================
## Sites and already-committed tables (read-only). Unchanged from v3.
## ============================================================================
IAV_SITES <- c("AU-How", "NL-Loo", "DE-Tha", "FR-Pue", "CH-Dav", "US-MMS", "US-Mpj", "AT-Neu",
               "US-Ha1", "JP-Api", "IL-Yat", "US-Ton", "US-Var", "US-Wkg", "US-SRM")

exp_sites          <- read_csv(file.path(EXP_TABLES, "sites.csv"), show_col_types = FALSE)
exp_site_years      <- read_csv(file.path(EXP_TABLES, "site_years.csv"), show_col_types = FALSE)
derived_site_all    <- read_csv(file.path(EXP_TABLES, "derived_site.csv"), show_col_types = FALSE)
derived_site_year_all <- read_csv(file.path(EXP_TABLES, "derived_site_year.csv"), show_col_types = FALSE)

site_meta <- exp_sites |> filter(site_id %in% IAV_SITES) |>
  select(site_id, igbp, group, location_lat, first_year, last_year, gpp_col_used, qc_col_used)
stopifnot(all(IAV_SITES %in% site_meta$site_id))

is_sh <- function(sid) site_meta$location_lat[site_meta$site_id == sid] < 0
sh_sites <- site_meta$site_id[site_meta$location_lat < 0]
message("Southern-Hemisphere site(s) among the 15: ", paste(sh_sites, collapse = ", "))

raw_years_list <- split(exp_site_years$year[exp_site_years$site_id %in% IAV_SITES],
                          exp_site_years$site_id[exp_site_years$site_id %in% IAV_SITES])

## ============================================================================
## Item 1: year-rule dispatch. `.YEAR_RULE` is a module-level mutable switch
## (set by run_core_for_rule() below, around the whole per-rule computation);
## window_dates()/season_years_for() read it on every call via normal R
## lexical scoping, so every downstream function that already calls ONLY
## these two primitives (pool_tbl(), run_m1(), run_count_methods(), etc. --
## true throughout this script, verified by inspection) becomes rule-aware
## automatically, with no changes needed to their own bodies.
## ============================================================================
.YEAR_RULE <- "calendar"

window_dates <- function(sid, y) {
  if (.YEAR_RULE == "calendar") {
    if (is_sh(sid)) seq(as.Date(sprintf("%d-07-01", y)), as.Date(sprintf("%d-06-30", y + 1L)), by = "day")
    else            seq(as.Date(sprintf("%d-01-01", y)), as.Date(sprintf("%d-12-31", y)), by = "day")
  } else {
    m <- min_gpp_month_for[[sid]]
    start <- as.Date(sprintf("%d-%02d-01", y, m))
    seq(start, start + months(12) - days(1), by = "day")
  }
}

#' Generic day-count passing-year test for a window starting on `start_month`
#' of year y and running 12 months -- a direct generalisation of v1-v3's
#' AU-How-only Jul-Jun rule (season_years_sh(), now = season_years_generic(sid, 7L)).
season_years_generic <- function(sid, start_month) {
  d <- daily_cont |> filter(site_id == sid)
  rng_lo <- site_meta$first_year[site_meta$site_id == sid] - 1L
  rng_hi <- site_meta$last_year[site_meta$site_id == sid]
  keep <- integer(0)
  for (y in rng_lo:rng_hi) {
    start <- as.Date(sprintf("%d-%02d-01", y, start_month))
    win <- seq(start, start + months(12) - days(1), by = "day")
    qc_vals <- d$QC_REF[match(win, d$TIMESTAMP)]
    pass_raw <- qc_passes(qc_vals, EXP_RULE_QC_DAY)
    n_fail <- sum(is.na(pass_raw) | !pass_raw)
    if (n_fail <= EXP_RULE_MAX_FAIL_DAYS) keep <- c(keep, y)
  }
  keep
}
season_years_sh <- function(sid) season_years_generic(sid, 7L)

season_years_for <- function(sid) {
  if (.YEAR_RULE == "calendar") {
    if (is_sh(sid)) season_years_sh(sid) else raw_years_list[[sid]]
  } else {
    season_years_generic(sid, min_gpp_month_for[[sid]])
  }
}

message("Site(s): ", paste(IAV_SITES, collapse = ", "))

## ============================================================================
## Raw daily pull (read-only, dataset = 'FLUXMET'). Unchanged from v3.
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
## Two GPP series (zero-floored) + NEP (shared). Unchanged from v3.
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
message("15-day centred smoothing applied (gpp_nt, gpp_dt, nep) over each site's full continuous record (still used by M1/M2-4/M5/M8; M7 uses its own harmonic smoothing, item 2 below).")

pooled_window_dates <- function(sid) do.call(c, lapply(season_years_for(sid), function(y) window_dates(sid, y)))
pool_tbl <- function(sid, gcol) {
  dts <- pooled_window_dates(sid)
  d <- daily_cont |> filter(site_id == sid)
  tibble(date = dts, month = lubridate::month(dts), value = d[[gcol]][match(dts, d$TIMESTAMP)])
}
pool_values <- function(sid, gcol) { v <- pool_tbl(sid, gcol)$value; v[!is.na(v)] }

## ============================================================================
## Item 1: min-GPP month per site, determined ONCE from the CALENDAR rule's
## own pooled passing years (`.YEAR_RULE` is "calendar" at this point, its
## initial value -- no circularity: this computation does not depend on
## min_gpp_month_for itself).
## ============================================================================
stopifnot(.YEAR_RULE == "calendar")
month_mean_gpp <- function(sid) {
  pt <- pool_tbl(sid, "gpp_nt")
  vapply(1:12, function(m) mean(pt$value[pt$month == m], na.rm = TRUE), numeric(1))
}
min_gpp_month_for <- setNames(sapply(IAV_SITES, function(s) which.min(month_mean_gpp(s))), IAV_SITES)
message("Min-GPP-month per site (item 1, 'min_gpp_month' rule start month): ",
        paste(sprintf("%s=%d", IAV_SITES, min_gpp_month_for), collapse = ", "))

## ============================================================================
## Item 2: M7's harmonic resmoothing. `Rssa` is NOT installed
## (requireNamespace("Rssa") = FALSE); CLAUDE.md requires discussion before
## adding a new package dependency or touching renv.lock, not available in
## this autonomous run -- so, per the prompt's own fallback, a per-site-year
## harmonic regression on 16-day GPP composites is used instead. K (2 or 3
## harmonics) is chosen below by mean R^2 across all site-years (calendar
## rule, NT series) and reported; m7_one()'s amplitude/derivative algorithm
## itself is unchanged from v3.
## ============================================================================
compute_16day_composites <- function(win, raw_vals) {
  n <- length(win)
  bin <- (seq_len(n) - 1L) %/% 16L + 1L
  comp_val <- as.numeric(tapply(raw_vals, bin, function(v) mean(v, na.rm = TRUE)))
  comp_t   <- as.numeric(tapply(seq_len(n), bin, mean))
  list(t = comp_t, y = comp_val)
}

#' Fit a K-harmonic regression to 16-day composites of `raw_vals` over window
#' `win`, and return the fitted curve interpolated to every day of `win`
#' (plus the fit's R^2, for K-selection/diagnostics).
harmonic_predict_daily <- function(win, raw_vals, K) {
  n <- length(win)
  comp <- compute_16day_composites(win, raw_vals)
  t <- comp$t; y <- comp$y
  ok <- is.finite(y)
  if (sum(ok) < 2L * K + 2L) return(list(pred = rep(NA_real_, n), r2 = NA_real_))
  dfit <- data.frame(y = y[ok])
  for (k in seq_len(K)) { dfit[[paste0("c", k)]] <- cos(2 * pi * k * t[ok] / n); dfit[[paste0("s", k)]] <- sin(2 * pi * k * t[ok] / n) }
  fit <- lm(y ~ ., data = dfit)
  newt <- seq_len(n)
  dnew <- data.frame(row = newt)
  for (k in seq_len(K)) { dnew[[paste0("c", k)]] <- cos(2 * pi * k * newt / n); dnew[[paste0("s", k)]] <- sin(2 * pi * k * newt / n) }
  list(pred = as.numeric(predict(fit, newdata = dnew)), r2 = summary(fit)$r.squared)
}

## ---- Choose K (2 vs 3 harmonics) by mean R^2 across all site-years,
## calendar rule, NT series -- then use that K for BOTH series and BOTH rules. ----
.YEAR_RULE <- "calendar"
k_eval <- purrr::map_dfr(IAV_SITES, function(sid) {
  yrs <- season_years_for(sid)
  d <- daily_cont |> filter(site_id == sid)
  purrr::map_dfr(yrs, function(y) {
    win <- window_dates(sid, y)
    dy <- d |> filter(TIMESTAMP %in% win)
    raw <- dy$gpp_nt[match(win, dy$TIMESTAMP)]
    tibble(site_id = sid, year = y, r2_k2 = harmonic_predict_daily(win, raw, 2)$r2, r2_k3 = harmonic_predict_daily(win, raw, 3)$r2)
  })
})
mean_r2_k2 <- mean(k_eval$r2_k2, na.rm = TRUE)
mean_r2_k3 <- mean(k_eval$r2_k3, na.rm = TRUE)
M7_K <- if (mean_r2_k3 > mean_r2_k2) 3L else 2L
message("M7 harmonic-fit K selection: mean R^2 (K=2) = ", round(mean_r2_k2, 4), ", mean R^2 (K=3) = ",
        round(mean_r2_k3, 4), " -- using K = ", M7_K, " harmonics (chosen by higher mean R^2).")

## ============================================================================
## Item 3 (count-method 3-rule machinery, v3 item 3, unchanged) + M2/M3/M4.
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

## ============================================================================
## M1 (Zscheischler et al. 2016), M5, M6, M7, M8 building blocks -- unchanged
## from v3 except run_m7()/m7_one() (item 2, harmonic resmoothing).
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

#' TIMESAT secondary-peak rule (unchanged logic from v3), now also returning
#' the between-peaks TROUGH date (item 3: AT-Neu cut-coincidence test).
timesat_two_season <- function(gpp_sm_vals, dates) {
  none <- list(flag = FALSE, ratio = NA_real_, primary_date = as.Date(NA), secondary_date = as.Date(NA), trough_date = as.Date(NA))
  if (all(is.na(gpp_sm_vals))) return(none)
  maxima <- local_maxima_idx(gpp_sm_vals)
  if (length(maxima) < 1) return(none)
  primary_idx <- maxima[which.max(gpp_sm_vals[maxima])]
  primary_val <- gpp_sm_vals[primary_idx]
  primary_date <- dates[primary_idx]
  others <- setdiff(maxima, primary_idx)
  if (length(others) == 0) return(list(flag = FALSE, ratio = NA_real_, primary_date = primary_date, secondary_date = as.Date(NA), trough_date = as.Date(NA)))
  secondary_idx <- others[which.max(gpp_sm_vals[others])]
  secondary_val <- gpp_sm_vals[secondary_idx]
  secondary_date <- dates[secondary_idx]
  ratio <- secondary_val / primary_val
  lo_idx <- min(primary_idx, secondary_idx); hi_idx <- max(primary_idx, secondary_idx)
  between <- gpp_sm_vals[(lo_idx + 1):(hi_idx - 1)]
  valley <- if (length(between) == 0 || all(is.na(between))) NA_real_ else min(between, na.rm = TRUE)
  trough_date <- if (length(between) == 0 || all(is.na(between))) as.Date(NA) else dates[lo_idx + which.min(between)]
  lower_peak <- min(primary_val, secondary_val)
  flag <- !is.na(ratio) && ratio >= 0.5 && !is.na(valley) && valley < 0.5 * lower_peak
  list(flag = flag, ratio = ratio, primary_date = primary_date, secondary_date = secondary_date, trough_date = trough_date)
}

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
        rowd$two_season_trough_date <- ts$trough_date
        out[[length(out) + 1]] <- rowd |> mutate(site_id = sid, year = y, variant = "all", method = "M5")
      }
    }
  }
  bind_rows(out)
}

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
run_m6 <- function(form, m5_tbl) {
  out <- list()
  for (sid in IAV_SITES) {
    yrs <- season_years_for(sid)
    d <- daily_cont |> filter(site_id == sid)
    for (y in yrs) {
      win <- window_dates(sid, y)
      dy <- d |> filter(TIMESTAMP %in% win)
      nep_raw_win <- dy$nep[match(win, dy$TIMESTAMP)]
      nep_sm_win  <- dy$nep_sm[match(win, dy$TIMESTAMP)]

      m5_span <- m5_tbl |> filter(site_id == sid, year == y, sub == "span")
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

gpp_share_for_span <- function(sid, start_date, end_date, y, annual_corr_tbl) {
  if (is.na(start_date) || is.na(end_date)) return(c(gpp_in_season = NA_real_, annual_gpp = NA_real_, share = NA_real_))
  d <- daily_cont |> filter(site_id == sid, TIMESTAMP >= start_date, TIMESTAMP <= end_date)
  gpp_in_season <- sum(d$gpp_nt, na.rm = TRUE)
  ag <- annual_corr_tbl$annual_gpp[annual_corr_tbl$site_id == sid & annual_corr_tbl$year == y]
  ag <- if (length(ag) == 1) ag else NA_real_
  c(gpp_in_season = gpp_in_season, annual_gpp = ag, share = if (!is.na(ag) && ag != 0) gpp_in_season / ag else NA_real_)
}
add_gpp_share <- function(tbl, annual_corr_tbl) {
  res <- purrr::pmap_dfr(list(tbl$site_id, tbl$start_date, tbl$end_date, tbl$year), function(s, sd, ed, y) {
    as_tibble(t(gpp_share_for_span(s, sd, ed, y, annual_corr_tbl)))
  })
  bind_cols(tbl, res)
}

## ---- Item 2: M7, now fed the harmonic-fitted daily curve (M7_K harmonics,
## chosen above) instead of the 15-day rolling mean. m7_one() itself (the
## amplitude/derivative algorithm) is byte-for-byte unchanged from v3. ----
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

  deriv <- diff(gpp_sm)
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

run_m7 <- function(variant, gcol, K) {
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
      gpp_raw <- dy[[gcol]][match(win, dy$TIMESTAMP)]
      hf <- harmonic_predict_daily(win, gpp_raw, K)
      out[[length(out) + 1]] <- m7_one(win, hf$pred, gpp_raw) |> mutate(site_id = sid, year = y, variant = variant, method = "M7", sub = "span", m7_r2 = hf$r2)
    }
  }
  bind_rows(out)
}

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

flag_cols <- c("flag_no_season", "flag_whole_year", "flag_net_source", "flag_two_season",
               "flag_no_start_crossing", "flag_no_end_crossing", "flag_no_dt", "flag_no_m5_span",
               "flag_no_run5", "flag_no_smpeak")
extra_cols <- c("threshold_pct_of_P99", "k_range_start_days", "k_range_end_days", "two_season_ratio",
                "two_season_primary_date", "two_season_secondary_date", "two_season_trough_date",
                "gsl_run5", "start_date_run5", "end_date_run5",
                "gsl_smpeak", "start_date_smpeak", "end_date_smpeak")
harmonise <- function(tbl) {
  for (fc in flag_cols) if (!fc %in% names(tbl)) tbl[[fc]] <- FALSE
  for (ec in extra_cols) if (!ec %in% names(tbl)) tbl[[ec]] <- NA
  tbl |> select(site_id, year, variant, method, sub, gsl, start_date, end_date, share, gpp_in_season,
                 annual_gpp, all_of(flag_cols), all_of(extra_cols))
}

#' One site's annual-total GPP/NEP, rule-aware via season_years_for()/window_dates().
annual_gpp_nep_for <- function(sid, gpp_col) {
  yrs <- season_years_for(sid)
  purrr::map_dfr(yrs, function(y) {
    win <- window_dates(sid, y)
    d <- daily_cont |> filter(site_id == sid, TIMESTAMP %in% win)
    tibble(site_id = sid, year = y, annual_gpp = sum(d[[gpp_col]], na.rm = TRUE), annual_nep = sum(d$nep, na.rm = TRUE))
  })
}
detrend_cor <- function(gsl, annual, year) {
  ok <- !is.na(gsl) & !is.na(annual) & !is.na(year)
  if (sum(ok) < 4) return(c(r = NA_real_, p = NA_real_, n = sum(ok)))
  g <- gsl[ok]; a <- annual[ok]; yv <- year[ok]
  rg <- residuals(lm(g ~ yv)); ra <- residuals(lm(a ~ yv))
  ct <- cor.test(rg, ra)
  c(r = unname(ct$estimate), p = ct$p.value, n = sum(ok))
}
mean_doy <- function(dates) { ok <- dates[!is.na(dates)]; if (length(ok) == 0) return(NA_real_); mean(lubridate::yday(ok)) }

## ============================================================================
## Item 1: run the ENTIRE core computation once per year_rule. Everything
## from P99 through gsl_methods_site_year/site_summary is computed FRESH
## inside this function each call (nothing reused across rules), so every
## object (P99s, peak months, annual_corr, m234/m1/m5/m6/m7/m8) is correctly
## scoped to whichever rule is active in `.YEAR_RULE` for that call.
## ============================================================================
run_core_for_rule <- function(rule) {
  .YEAR_RULE <<- rule
  message("=== Running core computation for year_rule = '", rule, "' ===")

  P99_nt <- setNames(sapply(IAV_SITES, function(s) as.numeric(quantile(pool_values(s, "gpp_nt"), 0.99, type = 7))), IAV_SITES)
  P99_dt <- setNames(sapply(IAV_SITES, function(s) {
    v <- pool_values(s, "gpp_dt")
    if (length(v) == 0) NA_real_ else as.numeric(quantile(v, 0.99, type = 7))
  }), IAV_SITES)

  message("Running M2 (5/10/20%)/M3/M4 for 2 series x 15 sites, 3 start/end rules each...")
  m234_nt <- run_count_methods("nt", "gpp_nt", "gpp_nt_sm", P99_nt, M2_PCTS_KEPT, M4_TARGETS)
  m234_dt <- run_count_methods("dt", "gpp_dt", "gpp_dt_sm", P99_dt, M2_PCTS_KEPT, M4_TARGETS)
  m234_all <- bind_rows(m234_nt, m234_dt)

  m2_grid_site_year <- run_m2_grid("gpp_nt", P99_nt, M2_PCTS_GRID)
  m2_grid_site_summary <- m2_grid_site_year |> group_by(site_id, pct) |> summarise(mean_gsl = mean(gsl, na.rm = TRUE), .groups = "drop")
  m2_grid_range <- m2_grid_site_summary |> group_by(site_id) |> summarise(m2_grid_min = min(mean_gsl), m2_grid_max = max(mean_gsl), .groups = "drop")

  annual_corr_nt <- bind_rows(lapply(IAV_SITES, annual_gpp_nep_for, gpp_col = "gpp_nt"))
  annual_corr_dt <- bind_rows(lapply(setdiff(IAV_SITES, sites_no_dt), annual_gpp_nep_for, gpp_col = "gpp_dt"))

  peak_months_nt <- setNames(lapply(IAV_SITES, peak_months_for, gcol = "gpp_nt"), IAV_SITES)
  peak_months_dt <- setNames(lapply(setdiff(IAV_SITES, sites_no_dt), peak_months_for, gcol = "gpp_dt"), setdiff(IAV_SITES, sites_no_dt))
  P99_summer_nt <- setNames(sapply(IAV_SITES, function(s) P99_for_months(s, "gpp_nt", summer_months(s))), IAV_SITES)
  P99_summer_dt <- setNames(sapply(IAV_SITES, function(s) if (s %in% sites_no_dt) NA_real_ else P99_for_months(s, "gpp_dt", summer_months(s))), IAV_SITES)
  P99_peak_nt <- setNames(sapply(IAV_SITES, function(s) P99_for_months(s, "gpp_nt", peak_months_nt[[s]])), IAV_SITES)
  P99_peak_dt <- setNames(sapply(IAV_SITES, function(s) if (s %in% sites_no_dt) NA_real_ else P99_for_months(s, "gpp_dt", peak_months_dt[[s]])), IAV_SITES)

  message("Running M1 for 2 series x 3 thresholds x 15 sites...")
  m1_nt <- run_m1("nt", "gpp_nt_sm", "gpp_nt", P99_nt, P99_summer_nt, P99_peak_nt, annual_corr_nt)
  m1_dt <- run_m1("dt", "gpp_dt_sm", "gpp_dt", P99_dt, P99_summer_dt, P99_peak_dt, annual_corr_dt)
  m1_all <- bind_rows(m1_nt, m1_dt)

  message("Running M5 (count + span) for 15 sites...")
  m5_all <- run_m5()

  message("Running M6, anchored + unanchored, for 15 sites...")
  m6_anchored <- run_m6("anchored", m5_all)
  m6_unanchored <- run_m6("unanchored", m5_all)
  m6_all <- bind_rows(m6_anchored, m6_unanchored)

  m5_all <- add_gpp_share(m5_all, annual_corr_nt)
  m6_all <- add_gpp_share(m6_all, annual_corr_nt)

  message("Running M7 (harmonic-resmoothed amplitude/derivative, K=", M7_K, ") for 2 series x 15 sites...")
  m7_nt <- run_m7("nt", "gpp_nt", M7_K)
  m7_dt <- run_m7("dt", "gpp_dt", M7_K)
  m7_all <- bind_rows(m7_nt, m7_dt)

  message("Running M8 for 2 series x 15 sites...")
  m8_nt <- run_m8("nt", "gpp_nt_sm", "gpp_nt", M8_FRACS)
  m8_dt <- run_m8("dt", "gpp_dt_sm", "gpp_dt", M8_FRACS)
  m8_all <- bind_rows(m8_nt, m8_dt)

  gsl_methods_site_year <- bind_rows(
    harmonise(m234_all), harmonise(m1_all), harmonise(m5_all), harmonise(m6_all), harmonise(m7_all), harmonise(m8_all)
  ) |> arrange(site_id, year, method, variant, sub)

  unaccounted <- gsl_methods_site_year |> filter(is.na(gsl) & !flag_no_season & !flag_net_source & !flag_no_dt)
  if (nrow(unaccounted) > 0) {
    print(unaccounted)
    stop("11_gsl_methods_compare.R: ", nrow(unaccounted), " row(s) [rule=", rule, "] have neither a GSL value nor an explanatory flag -- stopping.")
  }

  annual_corr_by_variant <- list(nt = annual_corr_nt, dt = annual_corr_dt, all = annual_corr_nt)
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

  message("=== year_rule = '", rule, "' done: ", nrow(gsl_methods_site_year), " site_year rows, ", nrow(gsl_methods_site_summary), " summary rows ===")
  list(site_year = gsl_methods_site_year, site_summary = gsl_methods_site_summary,
       m234_all = m234_all, m5_all = m5_all, m7_all = m7_all,
       m2_grid_site_summary = m2_grid_site_summary, m2_grid_range = m2_grid_range,
       annual_corr_nt = annual_corr_nt, annual_corr_dt = annual_corr_dt)
}

YEAR_RULES <- c("calendar", "min_gpp_month")
core_results <- setNames(lapply(YEAR_RULES, run_core_for_rule), YEAR_RULES)
.YEAR_RULE <- "calendar" # restore the default after both passes, for any ad-hoc post-hoc code below

gsl_methods_site_year <- bind_rows(lapply(YEAR_RULES, function(r) core_results[[r]]$site_year |> mutate(year_rule = r)))
gsl_methods_site_summary <- bind_rows(lapply(YEAR_RULES, function(r) core_results[[r]]$site_summary |> mutate(year_rule = r)))
message("Combined gsl_methods_site_year: ", nrow(gsl_methods_site_year), " rows (both year_rules); gsl_methods_site_summary: ", nrow(gsl_methods_site_summary), " rows.")

## ---- Convenience aliases: the CALENDAR rule's own objects, used by every
## downstream report/figure/check that matches v1-v3 (the prompt: "checks as
## version 3, run under the calendar rule"). ----
m234_all <- core_results$calendar$m234_all
m5_all <- core_results$calendar$m5_all
m7_all_cal <- core_results$calendar$m7_all
m2_grid_site_summary <- core_results$calendar$m2_grid_site_summary
m2_grid_range <- core_results$calendar$m2_grid_range
annual_corr_nt <- core_results$calendar$annual_corr_nt
annual_corr_dt <- core_results$calendar$annual_corr_dt

## ============================================================================
## Checks (calendar rule only, as version 3).
## ============================================================================
.YEAR_RULE <- "calendar"
nh_sites <- setdiff(IAV_SITES, sh_sites)
P99_nt_cal <- setNames(sapply(IAV_SITES, function(s) as.numeric(quantile(pool_values(s, "gpp_nt"), 0.99, type = 7))), IAV_SITES)
chk_P99 <- derived_site_all |> filter(site_id %in% nh_sites, variant == "main_unscreened") |>
  select(site_id, P99) |> mutate(P99_fresh = P99_nt_cal[site_id], diff = P99 - P99_fresh)
if (any(abs(chk_P99$diff) > 1e-9)) {
  print(chk_P99[abs(chk_P99$diff) > 1e-9, ])
  stop("11_gsl_methods_compare.R: freshly computed NT P99 (calendar rule) does not match derived_site.csv for a NH site -- stopping.")
}
message("Check passed: fresh NT P99 (calendar rule) exactly matches derived_site.csv's main_unscreened P99 for all ", length(nh_sites), " NH sites.")

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
  stop("11_gsl_methods_compare.R: NT-series M2@10%/M4@0.95 (calendar rule) do not exactly match derived_site_year.csv's main_unscreened values -- stopping.")
}
message("Check passed: NT-series M2@10% / M4@0.95 (calendar rule) == main_unscreained gsl_z16/f_z16 / gsl_t95/f_t95 exactly, for all ", nrow(chk1), " NH site-year rows.")

fraction_sweep_main <- read_csv(file.path(EXP_TABLES, "fraction_sweep_main.csv"), show_col_types = FALSE)
fsm_variants <- unique(fraction_sweep_main$variant)
message("M4@0.85 (NT, calendar rule) vs. fraction_sweep_main.csv check: NOT APPLICABLE (as v3) -- that table's only variant is '",
        paste(fsm_variants, collapse = ", "), "' (winter-screened), not 'main_unscreened'/NT.")

auhow_old_gsl <- derived_site_year_all |> filter(site_id == "AU-How", variant == "main_unscreened") |>
  select(year, gsl_z16_old = gsl_z16, gsl_t95_old = gsl_t95)
auhow_new_gsl <- m234_all |> filter(site_id == "AU-How", variant == "nt", method == "M2", sub == "10pct") |>
  select(year, gsl_z16_new = gsl) |>
  left_join(m234_all |> filter(site_id == "AU-How", variant == "nt", method == "M4", sub == "0.95") |> select(year, gsl_t95_new = gsl), by = "year")
message("AU-How GSL, old (calendar year) vs new (Jul-Jun season year):")
print(as.data.frame(auhow_old_gsl)); print(as.data.frame(auhow_new_gsl))

unaccounted_all <- gsl_methods_site_year |> filter(is.na(gsl) & !flag_no_season & !flag_net_source & !flag_no_dt)
if (nrow(unaccounted_all) > 0) stop("11_gsl_methods_compare.R: ", nrow(unaccounted_all), " row(s) across both year_rules lack a value or flag -- stopping.")
message("Check passed: every one of ", nrow(gsl_methods_site_year), " rows (both year_rules) has a GSL value or an explanatory flag.")

## ============================================================================
## Net-source and no-DT exclusions/unknowns (calendar rule, as v1-v3 did).
## ============================================================================
gy_cal <- gsl_methods_site_year |> filter(year_rule == "calendar")
net_source_rows <- gy_cal |> filter(method %in% c("M5", "M6"), flag_net_source)
for (i in seq_len(nrow(net_source_rows))) {
  log_exclusion(site_id = net_source_rows$site_id[i], variable = paste0(net_source_rows$method[i], "-", net_source_rows$sub[i]),
                timestamp = net_source_rows$year[i], reason = "Net-source year: fewer than 5 days of smoothed NEP > 0; excluded from M5/M6 site means and detrended-r",
                threshold = "5 days", excluded_by = "11_gsl_methods_compare.R", log_path = EXP_EXCLUSION_LOG)
}
no_season_rows <- gy_cal |> filter(flag_no_season, !flag_net_source, !flag_no_dt)
for (i in seq_len(nrow(no_season_rows))) {
  log_unknown(record_id = sprintf("%s-%d-%s-%s-%s", no_season_rows$site_id[i], no_season_rows$year[i],
                                    no_season_rows$variant[i], no_season_rows$method[i], no_season_rows$sub[i]),
              reason = "No season detected for this method/series within this site-year's window (calendar rule)",
              logged_by = "11_gsl_methods_compare.R", log_path = EXP_UNKNOWN_LOG)
}
message("Logged ", nrow(net_source_rows), " net-source exclusion(s) and ", nrow(no_season_rows), " no-season unknown(s) (calendar rule).")

## ============================================================================
## Write main output tables (both year_rules, overwrite v3's in place).
## ============================================================================
write_csv(gsl_methods_site_year, file.path(EXP_TABLES, "gsl_methods_site_year.csv"))
write_meta(
  file.path(EXP_TABLES, "gsl_methods_site_year.csv"),
  input_sources = c("data/duckdb/fluxnet.duckdb (daily, dataset='FLUXMET')", file.path(EXP_TABLES, "sites.csv"),
                      file.path(EXP_TABLES, "site_years.csv"), file.path(EXP_TABLES, "derived_site.csv"),
                      file.path(EXP_TABLES, "derived_site_year.csv")),
  notes = paste0(
    "VERSION 4 (supersedes v3, cd7e547 -- see docs/gsl_methods_compare_2026-11-02_v4.md). Adds a ",
    "`year_rule` column: 'calendar' (v1-v3's rule, NH=Jan-Dec/AU-How=Jul-Jun) and 'min_gpp_month' ",
    "(each site's year starts on the 1st of its own lowest-mean-NT-GPP calendar month, determined ",
    "from the calendar rule's pooled years). Both rules use the same <=14-failing-day QC passing-year ",
    "test. Figures/checks use 'calendar' only, matching v1-v3. M7 now uses a ", M7_K, "-harmonic fit ",
    "to 16-day GPP composites per site-year (Rssa not installed; per-the-prompt fallback), replacing ",
    "the 15-day rolling mean used for M1-M5/M8. Checks (NH sites, calendar rule, all passed exactly): ",
    "NT-series M2@10%/M4@0.95 == derived_site_year.csv's main_unscreened values; every row (both year_",
    "rules) has a value or an explanatory flag. M4@0.85-vs-fraction-sweep check remains not applicable."
  )
)
write_csv(gsl_methods_site_summary, file.path(EXP_TABLES, "gsl_methods_site_summary.csv"))
write_meta(
  file.path(EXP_TABLES, "gsl_methods_site_summary.csv"),
  input_sources = file.path(EXP_TABLES, "gsl_methods_site_year.csv"),
  notes = "VERSION 4. Per site x year_rule x series x method x sub-threshold summary (mean GSL, SD, start/end day-of-window, share, flags, detrended r). See gsl_methods_site_year.csv's notes for year_rule/M7 changes."
)
message("Saved: gsl_methods_site_year.csv (", nrow(gsl_methods_site_year), " rows), gsl_methods_site_summary.csv (", nrow(gsl_methods_site_summary), " rows).")

## ============================================================================
## Item 1 report table: how years/GSL/start/end/M8/largest-differences change
## between the two year_rules, by site.
## ============================================================================
rule_compare_years <- bind_rows(lapply(YEAR_RULES, function(r) tibble(year_rule = r, site_id = IAV_SITES,
                                                                        n_years = sapply(IAV_SITES, function(s) { .YEAR_RULE <<- r; length(season_years_for(s)) }))))
.YEAR_RULE <- "calendar"

rule_compare_methods <- gsl_methods_site_summary |>
  filter(variant %in% c("nt", "all")) |>
  select(site_id, year_rule, method, sub, mean_gsl, mean_start_doy, mean_end_doy) |>
  pivot_wider(names_from = year_rule, values_from = c(mean_gsl, mean_start_doy, mean_end_doy)) |>
  mutate(
    gsl_shift = mean_gsl_min_gpp_month - mean_gsl_calendar,
    start_doy_shift = mean_start_doy_min_gpp_month - mean_start_doy_calendar,
    end_doy_shift = mean_end_doy_min_gpp_month - mean_end_doy_calendar
  )

rule_compare_m8 <- rule_compare_methods |> filter(method == "M8")
rule_compare_range <- gsl_methods_site_summary |>
  filter(variant %in% c("nt", "all"), method %in% c("M1","M2","M3","M4","M5","M6","M7","M8"),
         sub %in% c("summer","10pct","1gC","0.85","span","anchored","0.25")) |>
  group_by(site_id, year_rule) |>
  summarise(gsl_min = min(mean_gsl, na.rm = TRUE), gsl_max = max(mean_gsl, na.rm = TRUE), range = gsl_max - gsl_min, .groups = "drop") |>
  pivot_wider(names_from = year_rule, values_from = c(gsl_min, gsl_max, range))

write_csv(rule_compare_years, file.path(EXP_TABLES, "gsl_methods_year_rule_years.csv"))
write_meta(file.path(EXP_TABLES, "gsl_methods_year_rule_years.csv"), input_sources = file.path(EXP_TABLES, "gsl_methods_site_year.csv"),
           notes = "VERSION 4, item 1. Number of passing years per site, under each year_rule.")
write_csv(rule_compare_methods, file.path(EXP_TABLES, "gsl_methods_year_rule_compare.csv"))
write_meta(file.path(EXP_TABLES, "gsl_methods_year_rule_compare.csv"), input_sources = file.path(EXP_TABLES, "gsl_methods_site_summary.csv"),
           notes = "VERSION 4, item 1. Mean GSL/start-doy/end-doy per site x method x sub under each year_rule, plus min_gpp_month-minus-calendar shifts.")
write_csv(rule_compare_range, file.path(EXP_TABLES, "gsl_methods_year_rule_range.csv"))
write_meta(file.path(EXP_TABLES, "gsl_methods_year_rule_range.csv"), input_sources = file.path(EXP_TABLES, "gsl_methods_site_summary.csv"),
           notes = "VERSION 4, item 1. Largest-cross-method-difference-per-site (headline subs), under each year_rule.")
message("Year-rule comparison tables saved. Years per site:"); print(as.data.frame(pivot_wider(rule_compare_years, names_from = year_rule, values_from = n_years)))

## ============================================================================
## Item 3 (v3, rule shifts) -- calendar rule only, as v3.
## ============================================================================
rule_shifts <- gy_cal |> filter(method %in% c("M2", "M3", "M4"), variant == "nt") |>
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
write_meta(file.path(EXP_TABLES, "gsl_methods_rule_shifts.csv"), input_sources = file.path(EXP_TABLES, "gsl_methods_site_year.csv"),
           notes = "VERSION 4 (calendar rule only, as v3 item 3). MEDIAN absolute start/end-date shift (days) between rule (a) first/last qualifying day and rule (b) first/last >=5-day run [_ab], and between (a) and rule (c) Panwar's smoothed-crossing-nearest-peak [_ac], per site.")

## ============================================================================
## Item 1 cross-rule check on rule shifts: recompute under min_gpp_month too.
## ============================================================================
gy_mingpp <- gsl_methods_site_year |> filter(year_rule == "min_gpp_month")
rule_shifts_mingpp <- gy_mingpp |> filter(method %in% c("M2", "M3", "M4"), variant == "nt") |>
  mutate(shift_start_ac = abs(as.numeric(start_date_smpeak - start_date)), shift_end_ac = abs(as.numeric(end_date_smpeak - end_date))) |>
  group_by(site_id) |> summarise(median_shift_start_ac = median(shift_start_ac, na.rm = TRUE), median_shift_end_ac = median(shift_end_ac, na.rm = TRUE), .groups = "drop")
write_csv(rule_shifts_mingpp, file.path(EXP_TABLES, "gsl_methods_rule_shifts_min_gpp_month.csv"))
write_meta(file.path(EXP_TABLES, "gsl_methods_rule_shifts_min_gpp_month.csv"), input_sources = file.path(EXP_TABLES, "gsl_methods_site_year.csv"),
           notes = "VERSION 4, item 1. Same rule (a)-vs-(c) median shift as gsl_methods_rule_shifts.csv, recomputed under the min_gpp_month year_rule, for the cross-rule comparison.")

## ============================================================================
## Item 5 (v3 item 5): method agreement -- calendar rule only, as v3.
## ============================================================================
FIG_METHODS <- tibble(
  method = c("M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8"),
  sub    = c("summer", "10pct", "1gC", "0.85", "span", "anchored", "span", "0.25"),
  label  = c("M1 (crossing, summer P99)", "M2 (10% of P99)", "M3 (1 gC m-2 d-1)",
             "M4 (0.85 of annual GPP)", "M5 (NEP>0 span)", "M6 (Hu NEP rate-of-change)",
             "M7 (amplitude/derivative)", "M8 (normalized 0.25)")
)

wide_base <- gy_cal |>
  inner_join(FIG_METHODS, by = c("method", "sub")) |>
  filter(variant %in% c("nt", "all")) |>
  mutate(start_doy = lubridate::yday(start_date), end_doy = lubridate::yday(end_date)) |>
  select(site_id, year, label, gsl, start_date, end_date, start_doy, end_doy)

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
write_meta(file.path(EXP_TABLES, "gsl_methods_agreement.csv"), input_sources = file.path(EXP_TABLES, "gsl_methods_site_year.csv"),
           notes = "VERSION 4 (calendar rule only, as v3 item 5). Every method pair (M6='anchored'), Spearman r (site-demeaned anomalies) and median absolute start/end-date difference (days).")

## ============================================================================
## Items 1+6: NT-vs-DT (item 1 of v3; item 6 text rewritten in the doc only --
## no new computation needed, the underlying numbers are unchanged).
## ============================================================================
dt_vs_nt <- gy_cal |> filter(variant %in% c("nt", "dt"), !flag_no_dt) |>
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
write_meta(file.path(EXP_TABLES, "gsl_methods_dt_vs_nt.csv"), input_sources = file.path(EXP_TABLES, "gsl_methods_site_year.csv"),
           notes = "VERSION 4 (calendar rule only). Unchanged methodology from v3; see docs/gsl_methods_compare_2026-11-02_v4.md item 5 for the rewritten Keenan et al. (2019) interpretation.")

dt_vs_nt_by_group <- dt_vs_nt |> group_by(group) |>
  summarise(n = sum(!is.na(dt_minus_nt_start_days)), mean_start_shift = mean(dt_minus_nt_start_days, na.rm = TRUE),
            mean_end_shift = mean(dt_minus_nt_end_days, na.rm = TRUE), .groups = "drop")
dt_vs_nt_by_site <- dt_vs_nt |> group_by(site_id, group) |>
  summarise(n = sum(!is.na(dt_minus_nt_start_days)), mean_start_shift = mean(dt_minus_nt_start_days, na.rm = TRUE),
            mean_end_shift = mean(dt_minus_nt_end_days, na.rm = TRUE), .groups = "drop")
write_csv(dt_vs_nt_by_site, file.path(EXP_TABLES, "gsl_methods_dt_vs_nt_by_site_group.csv"))
write_meta(file.path(EXP_TABLES, "gsl_methods_dt_vs_nt_by_site_group.csv"), input_sources = file.path(EXP_TABLES, "gsl_methods_dt_vs_nt.csv"), notes = "VERSION 4 (calendar rule). Per-site signed DT-minus-NT shift with vegetation group.")
write_csv(dt_vs_nt_by_group, file.path(EXP_TABLES, "gsl_methods_dt_vs_nt_by_group.csv"))
write_meta(file.path(EXP_TABLES, "gsl_methods_dt_vs_nt_by_group.csv"), input_sources = file.path(EXP_TABLES, "gsl_methods_dt_vs_nt.csv"), notes = "VERSION 4 (calendar rule). Pooled by vegetation group; see the v4 doc for the rewritten Keenan et al. (2019) interpretation (item 5).")
message("DT-vs-NT by vegetation group (unchanged from v3; reinterpreted in the doc, item 5):"); print(as.data.frame(dt_vs_nt_by_group))

write_csv(m2_grid_site_summary, file.path(EXP_TABLES, "gsl_methods_m2_grid22.csv"))
write_meta(file.path(EXP_TABLES, "gsl_methods_m2_grid22.csv"), input_sources = file.path(EXP_TABLES, "gsl_methods_site_year.csv"),
           notes = "VERSION 4 (calendar rule). M2's full 22-value grid, NT series, site-level means.")

## ============================================================================
## Item 4: management/disturbance table -- FULL REPLACEMENT, literature-
## sourced (BADM keyword mining dropped; it produced false positives in v3).
## ============================================================================
site_management_15 <- tibble::tribble(
  ~site_id, ~management_type, ~date_detail, ~source,
  "AU-How", "burned", "Controlled burns, every 1-3 years, early in the dry season", "TERN Howard Springs site page",
  "NL-Loo", "other", "Scots pine planted c. 1909; very little management since", "ICOS Loobos station page",
  "DE-Tha", "other", "Spruce sown 1887; thinned April 2002 (~30% of trees); 2 ha windthrow 300 m from the tower, January 2007", "Bernhofer et al. (2011)",
  "FR-Pue", "other", "Holm oak coppice, last clear-cut 1942; throughfall-exclusion and thinning experiments since 2003", "DEIMS site page",
  "CH-Dav", "other", "Research plots never thinned; harvest occurs in the surrounding (non-footprint) forest; no tillage", "WSL Davos site page",
  "US-MMS", "other", "Selective logging, since ended (year not given)", "AmeriFlux/OSTI site page",
  "US-Mpj", "other", "Drought and bark-beetle pinon die-off, 2013-2015; no grazing reported", "AmeriFlux/OSTI site page",
  "AT-Neu", "mown", "Hay meadow, cut 3-4 times/year and manured; 2010 cuts 5 Jun/31 Jul/20 Sep, manure 18-22 Oct; 2011 cuts 6 Jun/1 Aug/26 Sep (no published dates 2018-2020)", "Hortnagl & Wohlfahrt (2014); FLUXNET site page",
  "US-Ha1", "other", "Cleared for farming in the 1600s-1700s; overstorey uprooted by the 1938 hurricane; no recent management", "AmeriFlux/OSTI site page",
  "JP-Api", "unknown", "Beech forest, c. 70 years old; no management reported", "AsiaFlux site page",
  "IL-Yat", "grazed", "Aleppo pine planted 1964; fire-protected (not burned); grazed by sheep/goats in late spring; irrigation experiment plot in the south of the site, footprint overlap unknown", "ICOS/FLUXNET Yatir station page",
  "US-Ton", "grazed", "Cattle grazing; brush periodically removed; season/intensity not found", "AmeriFlux/OSTI site page",
  "US-Var", "grazed", "Cattle grazing; brush periodically removed; season/intensity not found", "AmeriFlux/OSTI site page",
  "US-Wkg", "grazed", "Light to moderate grazing throughout the flux record", "Scott et al. (2010)",
  "US-SRM", "grazed", "Cattle grazing year-round 1957-2005; minimal grazing 2006; none from 2007", "Scott et al. (2009)"
) |> left_join(site_meta |> select(site_id, igbp, group), by = "site_id") |> select(site_id, igbp, group, management_type, date_detail, source)
stopifnot(setequal(unique(site_management_15$site_id), IAV_SITES))
write_csv(site_management_15, file.path(EXP_TABLES, "site_management_15.csv"))
write_meta(file.path(EXP_TABLES, "site_management_15.csv"), input_sources = character(0),
           notes = "VERSION 4, item 4. FULL REPLACEMENT of v3's table: literature-sourced (BADM DOM_DIST_MGMT/SITE_DESC keyword mining dropped -- it produced false positives, e.g. CH-Dav 'tillage'). One row per site; sources given per row.")
message("Saved site_management_15.csv (", nrow(site_management_15), " rows, full v4 replacement).")

## ============================================================================
## Item 3: AT-Neu cut detector, validation, and two-season/trough test.
## ============================================================================
atneu_daily <- daily_cont |> filter(site_id == "AT-Neu") |> arrange(TIMESTAMP)
atneu_gpp <- atneu_daily$gpp_nt
atneu_dates <- atneu_daily$TIMESTAMP

#' Cut detector on unsmoothed daily NT GPP: a short (3-day) pre-level drops by
#' more than `drop_thresh` within 1-3 days, followed by regrowth (value back
#' up by at least `regrow_factor` x the trough) 14-28 days after the trough.
#' Adjacent candidate days within 5 days of each other are clustered into one
#' event (keeping the deepest-drop candidate). As implemented; not from the
#' literature.
detect_cuts <- function(gpp, dates, pre_win = 3L, drop_thresh = 0.5, regrow_lag_lo = 14L, regrow_lag_hi = 28L, regrow_factor = 1.3) {
  n <- length(gpp)
  cand <- list()
  for (i in seq_len(n)) {
    if (i <= pre_win || i + regrow_lag_hi > n) next
    pre_level <- mean(gpp[(i - pre_win + 1):i], na.rm = TRUE)
    if (!is.finite(pre_level) || pre_level < 0.5) next
    win13 <- gpp[(i + 1):(i + 3)]
    if (all(is.na(win13))) next
    trough_val <- suppressWarnings(min(win13, na.rm = TRUE))
    if (!is.finite(trough_val)) next
    trough_lag <- which.min(win13)
    drop_frac <- (pre_level - trough_val) / pre_level
    if (!is.finite(drop_frac) || drop_frac <= drop_thresh) next
    trough_idx <- i + trough_lag
    if (trough_idx + regrow_lag_hi > n) next
    regrow_seg <- gpp[(trough_idx + regrow_lag_lo):(trough_idx + regrow_lag_hi)]
    if (all(is.na(regrow_seg))) next
    regrow_level <- mean(regrow_seg, na.rm = TRUE)
    if (!is.finite(regrow_level) || regrow_level <= trough_val * regrow_factor) next
    cand[[length(cand) + 1]] <- tibble(cand_idx = i, trough_idx = trough_idx, trough_date = dates[trough_idx],
                                         pre_level = pre_level, trough_val = trough_val, drop_frac = drop_frac, regrow_level = regrow_level)
  }
  if (length(cand) == 0) return(tibble())
  cand <- bind_rows(cand) |> arrange(trough_idx)
  cand$cluster <- cumsum(c(1L, diff(cand$trough_idx) > 5L))
  cand |> group_by(cluster) |> slice_max(drop_frac, n = 1, with_ties = FALSE) |> ungroup() |> select(-cluster)
}
atneu_cuts_detected <- detect_cuts(atneu_gpp, atneu_dates)
write_csv(atneu_cuts_detected, file.path(EXP_TABLES, "atneu_cut_detector_events.csv"))
write_meta(file.path(EXP_TABLES, "atneu_cut_detector_events.csv"), input_sources = "data/duckdb/fluxnet.duckdb (daily, dataset='FLUXMET')",
           notes = "VERSION 4, item 3. All cut-like events detected by detect_cuts() over AT-Neu's full record (unsmoothed daily NT GPP): >=50% drop within 1-3 days, followed by >=30% regrowth 14-28 days after the trough.")
message("AT-Neu cut detector: ", nrow(atneu_cuts_detected), " event(s) detected over the full record.")

## ---- Validate against the 2010/2011 published dates (Hortnagl & Wohlfahrt 2014). ----
atneu_published_cuts <- tibble(
  year = c(2010, 2010, 2010, 2011, 2011, 2011),
  cut_date = as.Date(c("2010-06-05", "2010-07-31", "2010-09-20", "2011-06-06", "2011-08-01", "2011-09-26"))
)
match_tol <- 10L
validation <- atneu_published_cuts |> rowwise() |>
  mutate(
    nearest_detected = { cand <- atneu_cuts_detected$trough_date[abs(as.numeric(atneu_cuts_detected$trough_date - cut_date)) == min(abs(as.numeric(atneu_cuts_detected$trough_date - cut_date)))]; if (length(cand) == 0) as.Date(NA) else cand[1] },
    timing_error_days = as.numeric(nearest_detected - cut_date),
    hit = !is.na(nearest_detected) && abs(timing_error_days) <= match_tol
  ) |> ungroup()
detected_2010_11 <- atneu_cuts_detected |> filter(lubridate::year(trough_date) %in% c(2010, 2011))
false_alarms <- detected_2010_11 |> filter(!sapply(trough_date, function(d) any(abs(as.numeric(d - atneu_published_cuts$cut_date)) <= match_tol)))
write_csv(validation, file.path(EXP_TABLES, "atneu_cut_detector_validation.csv"))
write_meta(file.path(EXP_TABLES, "atneu_cut_detector_validation.csv"), input_sources = file.path(EXP_TABLES, "atneu_cut_detector_events.csv"),
           notes = paste0("VERSION 4, item 3. Validation of detect_cuts() against Hortnagl & Wohlfahrt (2014)'s 6 published 2010/2011 cut dates, +/-", match_tol, "-day match tolerance. hit = TRUE if a detected event falls within tolerance; ", nrow(false_alarms), " detected 2010/2011 event(s) did not match any published cut (false alarms)."))
message("AT-Neu cut detector validation: ", sum(validation$hit), " of ", nrow(validation), " published 2010/2011 cuts matched (hits); ",
        sum(!validation$hit), " misses; ", nrow(false_alarms), " false alarm(s) in 2010/2011; mean |timing error| on hits = ",
        round(mean(abs(validation$timing_error_days[validation$hit]), na.rm = TRUE), 1), " d.")
print(as.data.frame(validation))

## ---- Rerun the 2010 mowing test with the ACTUAL published dates. ----
atneu_two_season_cal <- m5_all |> filter(site_id == "AT-Neu", sub == "span", flag_two_season, !is.na(two_season_secondary_date))
atneu_2010_secondary <- atneu_two_season_cal$two_season_secondary_date[atneu_two_season_cal$year == 2010]
if (length(atneu_2010_secondary) == 1) {
  atneu_2010_real_test <- atneu_published_cuts |> filter(year == 2010) |>
    mutate(days_after_cut = as.numeric(atneu_2010_secondary - cut_date), consistent_2_4wk = days_after_cut >= 14 & days_after_cut <= 28)
  write_csv(atneu_2010_real_test, file.path(EXP_TABLES, "atneu_2010_mowing_test_real_dates.csv"))
  write_meta(file.path(EXP_TABLES, "atneu_2010_mowing_test_real_dates.csv"), input_sources = file.path(EXP_TABLES, "gsl_methods_site_year.csv"),
             notes = "VERSION 4, item 3. v3's AT-Neu mowing test for 2010 rerun against Hortnagl & Wohlfahrt (2014)'s ACTUAL published cut dates (not v3's generic recurring window).")
  message("AT-Neu 2010 mowing test, real dates: secondary peak = ", atneu_2010_secondary); print(as.data.frame(atneu_2010_real_test))
} else {
  message("AT-Neu 2010 is not a two-season year under the calendar rule this run (or lacks a secondary-peak date) -- real-dates test not applicable.")
  atneu_2010_real_test <- tibble()
}

## ---- Apply detector network-wide at AT-Neu; test every two-season year's trough against a detected cut. ----
atneu_trough_test <- atneu_two_season_cal |>
  rowwise() |>
  mutate(
    nearest_cut = { if (nrow(atneu_cuts_detected) == 0 || is.na(two_season_trough_date)) as.Date(NA) else { d <- abs(as.numeric(atneu_cuts_detected$trough_date - two_season_trough_date)); atneu_cuts_detected$trough_date[which.min(d)] } },
    days_to_nearest_cut = if (is.na(nearest_cut)) NA_real_ else as.numeric(two_season_trough_date - nearest_cut),
    coincides_with_cut = !is.na(nearest_cut) && abs(days_to_nearest_cut) <= match_tol
  ) |> ungroup() |> select(year, two_season_secondary_date, two_season_trough_date, nearest_cut, days_to_nearest_cut, coincides_with_cut)
write_csv(atneu_trough_test, file.path(EXP_TABLES, "atneu_trough_vs_cut_test.csv"))
write_meta(file.path(EXP_TABLES, "atneu_trough_vs_cut_test.csv"), input_sources = c(file.path(EXP_TABLES, "gsl_methods_site_year.csv"), file.path(EXP_TABLES, "atneu_cut_detector_events.csv")),
           notes = paste0("VERSION 4, item 3. For every AT-Neu two-season year (calendar rule), the TIMESAT between-peaks trough date vs. the nearest detect_cuts() event, +/-", match_tol, "-day coincidence tolerance."))
message("AT-Neu trough-vs-detected-cut test: ", sum(atneu_trough_test$coincides_with_cut), " of ", nrow(atneu_trough_test), " two-season years coincide with a detected cut.")
print(as.data.frame(atneu_trough_test))

## ---- Two-season years, AT-Neu (cuts) reported separately from the US
## Southwest dryland sites (v3's "management is the main correlate" claim is
## REMOVED here; the rewritten text is in the doc). ----
two_season_cal <- gy_cal |> filter(method == "M5", sub == "span", flag_two_season)
us_dryland <- c("US-SRM", "US-Var", "US-Wkg", "US-Ton", "US-Mpj")
two_season_summary <- two_season_cal |> mutate(group_label = case_when(
  site_id == "AT-Neu" ~ "AT-Neu (mowing)", site_id %in% us_dryland ~ "US Southwest dryland (winter/monsoon)", TRUE ~ "other"
)) |> count(group_label, site_id)
write_csv(two_season_summary, file.path(EXP_TABLES, "two_season_years_by_group.csv"))
write_meta(file.path(EXP_TABLES, "two_season_years_by_group.csv"), input_sources = file.path(EXP_TABLES, "gsl_methods_site_year.csv"),
           notes = "VERSION 4, item 3. Two-season (M5-span, TIMESAT) site-years grouped as AT-Neu (mowing) vs. US Southwest dryland (winter/monsoon rainfall) vs. other -- replaces v3's single 'managed sites' grouping/claim.")
message("Two-season years by group:"); print(as.data.frame(two_season_summary))

## ============================================================================
## Item 2/9: AU-How recheck against Ma et al. (2013), with the new M7.
## ============================================================================
auhow_m7_nt <- gy_cal |> filter(site_id == "AU-How", method == "M7", variant == "nt")
message("AU-How M7 (NT, K=", M7_K, " harmonics) GSL per year: ", paste(sprintf("%d=%s", auhow_m7_nt$year, auhow_m7_nt$gsl), collapse = ", "),
        "; mean = ", round(mean(auhow_m7_nt$gsl, na.rm = TRUE), 1), " d (Ma et al. 2013: 244-275 d at Howard Springs).")
auhow_m7_gap <- round(244 - mean(auhow_m7_nt$gsl, na.rm = TRUE), 1)
message("AU-How M7 gap vs. Ma et al.'s lower bound (244 d): ", auhow_m7_gap, " d ", if (auhow_m7_gap > 20) "-- still well short; reported as a real difference." else "-- within range or close.")

auhow_new_years <- { .YEAR_RULE <<- "calendar"; season_years_sh("AU-How") }
auhow_wet_season <- purrr::map_dfr(auhow_new_years, function(y) {
  win <- seq(as.Date(sprintf("%d-10-15", y)), as.Date(sprintf("%d-04-15", y + 1L)), by = "day")
  d <- daily_cont |> filter(site_id == "AU-How", TIMESTAMP %in% win)
  full <- daily_cont |> filter(site_id == "AU-How", TIMESTAMP %in% { .YEAR_RULE <<- "calendar"; window_dates("AU-How", y) })
  tibble(year = y, wet_season_gpp = sum(d$gpp_nt, na.rm = TRUE),
         seasonal_amplitude = if (all(is.na(full$gpp_nt_sm))) NA_real_ else max(full$gpp_nt_sm, na.rm = TRUE) - min(full$gpp_nt_sm, na.rm = TRUE))
})
write_csv(auhow_wet_season, file.path(EXP_TABLES, "auhow_wet_season_v3.csv"))
write_meta(file.path(EXP_TABLES, "auhow_wet_season_v3.csv"), input_sources = file.path(EXP_TABLES, "gsl_methods_site_year.csv"),
           notes = "VERSION 4 (table name unchanged from v3; content recomputed with v4's calendar rule, numerically identical to v3). AU-How wet-season GPP (15 Oct-15 Apr) and smoothed-GPP seasonal amplitude, per Jul-Jun season-year.")
message("AU-How wet-season GPP: mean = ", round(mean(auhow_wet_season$wet_season_gpp, na.rm = TRUE), 1),
        " gC/m2, SD = ", round(sd(auhow_wet_season$wet_season_gpp, na.rm = TRUE), 1),
        "; seasonal amplitude: mean = ", round(mean(auhow_wet_season$seasonal_amplitude, na.rm = TRUE), 2),
        " gC/m2/d, SD = ", round(sd(auhow_wet_season$seasonal_amplitude, na.rm = TRUE), 2))

auhow_method_means <- gsl_methods_site_summary |> filter(year_rule == "calendar") |> inner_join(FIG_METHODS, by = c("method", "sub")) |>
  filter(site_id == "AU-How", variant %in% c("nt", "all"))
auhow_cross_method_sd <- sd(auhow_method_means$mean_gsl, na.rm = TRUE)
message("AU-How cross-method SD of mean GSL (M1-M8 headline subs, calendar rule): ", round(auhow_cross_method_sd, 1), " d")

## ============================================================================
## Figures. Calendar rule only, as v1-v3.
## ============================================================================
drafts_dir <- file.path(GSL_FIG_DIR, "drafts")
if (!dir.exists(drafts_dir)) dir.create(drafts_dir, recursive = TRUE)

skew_tbl <- derived_site_all |> filter(site_id %in% IAV_SITES, variant == "main") |> select(site_id, skewness)
fig_data <- gsl_methods_site_summary |> filter(year_rule == "calendar") |>
  inner_join(FIG_METHODS, by = c("method", "sub")) |>
  filter(variant %in% c("nt", "all")) |>
  left_join(skew_tbl, by = "site_id") |>
  left_join(m2_grid_range, by = "site_id") |>
  mutate(method_f = factor(label, levels = FIG_METHODS$label),
         xmin = ifelse(method == "M2", m2_grid_min, mean_gsl - sd_gsl),
         xmax = ifelse(method == "M2", m2_grid_max, mean_gsl + sd_gsl))

site_order <- skew_tbl |> arrange(skewness) |> pull(site_id)
fig_data$site_id <- factor(fig_data$site_id, levels = site_order)

n_methods <- nrow(FIG_METHODS)
dodge_width <- 0.8
method_rank <- as.numeric(factor(fig_data$method_f, levels = FIG_METHODS$label))
fig_data$y_num <- as.numeric(fig_data$site_id) + (method_rank - (n_methods + 1) / 2) * (dodge_width / n_methods)

## Retired 2026-11-05 (GSL cleanup step 1): these three draft figures are archived to
## archive/expanded/figures/drafts/; disabled rather than deleted -- see archive/README.md.
if (FALSE) {
p_dotplot <- ggplot(fig_data, aes(y = y_num, colour = method_f)) +
  geom_errorbar(aes(xmin = xmin, xmax = xmax), orientation = "y", width = 0, alpha = 0.6) +
  geom_point(data = dplyr::filter(fig_data, method != "M2"), aes(x = mean_gsl), size = 1.6) +
  scale_y_continuous(breaks = seq_along(site_order), labels = site_order, minor_breaks = NULL) +
  scale_colour_brewer(palette = "Dark2", name = "Method") +
  labs(x = "GSL (days, NT/NEP-only series, calendar year_rule). M2 shown as its full 0.1-20%-of-P99 range; other methods: mean +/- interannual SD",
       y = NULL, title = "GSL methods dotplot (draft, v4)") +
  theme_gsl() + theme(legend.position = "right")
ggsave(file.path(drafts_dir, "GSL methods dotplot draft.png"), p_dotplot, width = PEERJ_FIG_WIDTH_MM * 1.3,
       height = 150, units = "mm", dpi = PEERJ_DPI, device = ragg::agg_png, bg = "white")
writeLines(c(
  "GSL methods dotplot (draft, v4). Calendar year_rule only (see tables/gsl_methods_year_rule_compare.csv",
  "for the min_gpp_month comparison). Mean GSL by method, 15 sites, sorted by ascending skewness. M2",
  "shown as its full 22-value-grid range, not a point. M7 now uses a harmonic-fit (not 15-day rolling",
  "mean) smoothed curve, after Ma et al. (2013); Rssa was not available, so a per-site-year harmonic",
  "fit is used instead (see the methods doc for the K chosen and why).",
  "Numbers behind this figure: tables/gsl_methods_site_summary.csv, tables/gsl_methods_m2_grid22.csv."
), file.path(drafts_dir, "GSL methods dotplot draft.legend.txt"))
write_meta(file.path(drafts_dir, "GSL methods dotplot draft.png"), input_sources = c(file.path(EXP_TABLES, "gsl_methods_site_summary.csv"), file.path(EXP_TABLES, "gsl_methods_m2_grid22.csv")),
           notes = "VERSION 4. Draft figure. Calendar year_rule only; M7 now harmonic-smoothed.")
message("Saved: 'GSL methods dotplot draft'.png/.legend.txt/.meta.json")
}

cor_long <- bind_rows(cor_to_long(cor_gsl, "GSL"), cor_to_long(start_cor, "Start date"), cor_to_long(end_cor, "End date")) |>
  rename(r = value) |>
  mutate(row = factor(row, levels = FIG_METHODS$label), col = factor(col, levels = rev(FIG_METHODS$label)),
         which = factor(which, levels = c("GSL", "Start date", "End date")))
## Retired 2026-11-05 (GSL cleanup step 1): archived to archive/expanded/figures/drafts/.
if (FALSE) {
p_agree <- ggplot(cor_long, aes(x = row, y = col, fill = r)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", r)), size = PEERJ_ANNOTATION_PT / .pt * 0.7) +
  scale_fill_gradient2(low = "#b2182b", mid = "white", high = "#2166ac", midpoint = 0, limits = c(-1, 1), name = "Spearman r") +
  facet_wrap(~ which, nrow = 1) +
  labs(x = NULL, y = NULL, title = "GSL methods agreement (draft, v4)") +
  theme_gsl() + theme(axis.text.x = element_text(angle = 40, hjust = 1), legend.position = "right")
ggsave(file.path(drafts_dir, "GSL methods agreement draft.png"), p_agree, width = PEERJ_FIG_WIDTH_MM * 2.0,
       height = 140, units = "mm", dpi = PEERJ_DPI, device = ragg::agg_png, bg = "white")
writeLines(c(
  "GSL methods agreement (draft, v4). Calendar year_rule only. Spearman correlation between pairs of",
  "methods' annual GSL/start/end (site-demeaned anomalies), M6='anchored' only (its agreement with M5",
  "is partly built in by its own anchoring). Median absolute start/end-date differences are in",
  "tables/gsl_methods_agreement.csv, not shown on this tile plot.",
  "Numbers behind this figure: tables/gsl_methods_site_year.csv, tables/gsl_methods_agreement.csv."
), file.path(drafts_dir, "GSL methods agreement draft.legend.txt"))
write_meta(file.path(drafts_dir, "GSL methods agreement draft.png"), input_sources = c(file.path(EXP_TABLES, "gsl_methods_site_year.csv"), file.path(EXP_TABLES, "gsl_methods_agreement.csv")),
           notes = "VERSION 4. Draft figure. Calendar year_rule only.")
message("Saved: 'GSL methods agreement draft'.png/.legend.txt/.meta.json")
}

RASTER_SITES <- c("US-Ha1", "US-Wkg", "US-SRM")
RASTER_METHODS <- FIG_METHODS |> filter(!(method == "M3"))
raster_data <- gy_cal |>
  filter(site_id %in% RASTER_SITES, variant %in% c("nt", "all")) |>
  inner_join(RASTER_METHODS, by = c("method", "sub")) |>
  mutate(start_doy = lubridate::yday(start_date), end_doy = lubridate::yday(end_date),
         label = factor(label, levels = RASTER_METHODS$label), site_id = factor(site_id, levels = RASTER_SITES))
## Retired 2026-11-05 (GSL cleanup step 1): archived to archive/expanded/figures/drafts/.
if (FALSE) {
p_raster <- ggplot(raster_data) +
  geom_segment(aes(x = start_doy, xend = end_doy, y = year, yend = year), colour = "grey20",
               linewidth = nature_lwd(1.2), na.rm = TRUE) +
  facet_grid(label ~ site_id, scales = "free_y") +
  labs(x = "Day of year", y = "Year", title = "GSL methods raster (draft, v4)") +
  theme_gsl() + theme(strip.text.y = element_text(angle = 0, size = 6.5), strip.text.x = element_text(face = "bold"))
ggsave(file.path(drafts_dir, "GSL methods raster draft.png"), p_raster, width = PEERJ_FIG_WIDTH_MM * 1.3,
       height = 260, units = "mm", dpi = PEERJ_DPI, device = ragg::agg_png, bg = "white")
writeLines(c(
  "GSL methods raster (draft, v4). Calendar year_rule only. US-Ha1, US-Wkg, US-SRM (columns); each row",
  "block is one method; each horizontal bar is one year's detected season.",
  "Numbers behind this figure: tables/gsl_methods_site_year.csv."
), file.path(drafts_dir, "GSL methods raster draft.legend.txt"))
write_meta(file.path(drafts_dir, "GSL methods raster draft.png"), input_sources = file.path(EXP_TABLES, "gsl_methods_site_year.csv"),
           notes = "VERSION 4. Draft figure. Calendar year_rule only.")
message("Saved: 'GSL methods raster draft'.png/.legend.txt/.meta.json")
}

message("--- 11_gsl_methods_compare.R: done (version 4) ---")
