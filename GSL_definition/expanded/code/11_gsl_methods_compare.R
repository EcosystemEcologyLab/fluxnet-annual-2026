## 11_gsl_methods_compare.R — GSL expanded analysis: preliminary comparison
## of six published GSL definitions (M1-M6) at the 15 sites of the draft
## "Figure GSL-IAV" (tables/iav_15sites.csv). VERSION 2 -- revises version 1
## (committed at 15dc51f; see logs/prompts/2026-11-02_gsl_methods_compare.md)
## per Dave's follow-up decisions, logs/prompts/2026-11-02_gsl_methods_compare_v2.md:
##   1. Two GPP series (NT, DT), not three variants -- no winter screen, no
##      agreement/min-of-both series. Each method run on each series
##      separately, thresholds from that series' own P99/fine grid, never
##      mixed. DT-minus-NT reported as partitioning uncertainty.
##   2. Southern Hemisphere (AU-How) uses Jul-Jun season years in EVERY
##      method (including M2-M4, and for P99), with a fresh day-count
##      passing rule (<=14 failing days under the existing 0.25 daily-QC
##      rule) -- not the v1 "both bracketing calendar years pass" proxy.
##   3. M6 search windows = M5-span's own start/end +/-30 days; drops the
##      global-seasonal-peak device and the alternate-end column entirely.
##   4. Two-season flag uses the TIMESAT secondary-peak rule (Wang et al.
##      2020) on smoothed GPP, not the v1 slope-run rule (which saturated).
##
## Exploratory. Does NOT modify any existing script, table or figure other
## than this script's OWN previous outputs (tables/gsl_methods_site_year.csv,
## tables/gsl_methods_site_summary.csv, figures/drafts/"GSL methods *.png"),
## which this version regenerates in place under the same filenames -- v1's
## content lives on in git history at 15dc51f. The methods doc gets a new,
## `_v2`-suffixed filename per the prompt, alongside v1's doc (not replacing
## it). Writes only to this side project's own expanded/logs/
## (log_exclusion()/log_unknown()), never the repo-root outputs/ logs.
##
## Read-only: a fresh DBI pull from the repo-root DuckDB store (dataset =
## 'FLUXMET' only) for exactly these 15 sites, plus the already-committed
## expanded tables (sites.csv, site_years.csv, derived_site.csv,
## derived_site_year.csv, winter_screen_days.csv [read only for the
## DT-vs-NT "winter days" diagnostic below, not used to build any series],
## iav_15sites.csv) used read-only, for context and for the checks below --
## none are modified. Post-Z16-rename (commit d43ae22) column names used
## throughout.

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
## Sites and already-committed tables (read-only).
## ============================================================================
IAV_SITES <- c("AU-How", "NL-Loo", "DE-Tha", "FR-Pue", "CH-Dav", "US-MMS", "US-Mpj", "AT-Neu",
               "US-Ha1", "JP-Api", "IL-Yat", "US-Ton", "US-Var", "US-Wkg", "US-SRM")

exp_sites          <- read_csv(file.path(EXP_TABLES, "sites.csv"), show_col_types = FALSE)
exp_site_years      <- read_csv(file.path(EXP_TABLES, "site_years.csv"), show_col_types = FALSE)
derived_site_all    <- read_csv(file.path(EXP_TABLES, "derived_site.csv"), show_col_types = FALSE)
derived_site_year_all <- read_csv(file.path(EXP_TABLES, "derived_site_year.csv"), show_col_types = FALSE)
## Read-only diagnostic cross-reference for "where the DT-NT difference
## concentrates" (winter days); NOT used to build the NT/DT series (v2 has
## no winter screen at all).
winter_screen_days  <- read_csv(file.path(EXP_TABLES, "winter_screen_days.csv"), show_col_types = FALSE) |>
  mutate(is_winter_screen_day = TRUE) |> select(site_id, TIMESTAMP, is_winter_screen_day)
iav_tbl             <- read_csv(file.path(EXP_TABLES, "iav_15sites.csv"), show_col_types = FALSE)

site_meta <- exp_sites |> filter(site_id %in% IAV_SITES) |>
  select(site_id, igbp, group, location_lat, first_year, last_year, gpp_col_used, qc_col_used)
stopifnot(all(IAV_SITES %in% site_meta$site_id))

is_sh <- function(sid) site_meta$location_lat[site_meta$site_id == sid] < 0
sh_sites <- site_meta$site_id[site_meta$location_lat < 0]
message("Southern-Hemisphere site(s) among the 15: ", paste(sh_sites, collapse = ", "))

raw_years_list <- split(exp_site_years$year[exp_site_years$site_id %in% IAV_SITES],
                          exp_site_years$site_id[exp_site_years$site_id %in% IAV_SITES])

#' One site's annual window for "year" y -- calendar Jan-Dec for NH,
#' Jul(y)-Jun(y+1) for SH (season year). Unchanged from v1.
window_dates <- function(sid, y) {
  if (is_sh(sid)) seq(as.Date(sprintf("%d-07-01", y)), as.Date(sprintf("%d-06-30", y + 1L)), by = "day")
  else            seq(as.Date(sprintf("%d-01-01", y)), as.Date(sprintf("%d-12-31", y)), by = "day")
}

message("Site(s): ", paste(IAV_SITES, collapse = ", "))
message("Passing CALENDAR years per site (site_years.csv, used as-is for NH; superseded for AU-How below): ",
        paste(sprintf("%s=%d", names(raw_years_list), lengths(raw_years_list)), collapse = ", "))

## ============================================================================
## Raw daily pull (read-only, dataset = 'FLUXMET'), GPP_NT/DT, NEE and the
## matching NEE QC columns (needed for AU-How's new Jul-Jun day-count rule,
## item 2) -- VUT and CUT pulled, resolved per site via sites.csv's own
## gpp_col_used/qc_col_used (all 15 sites are VUT here, confirmed in v1; CUT
## branch kept for robustness/documentation, not exercised by this site set).
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

## ---- DT availability per site: a site "has no DT GPP" if GPP_DT_REF is
## NA for every row across its ENTIRE pulled record. Such a site contributes
## no "dt"-variant rows at all (flagged flag_no_dt, see below). ----
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
## Two GPP series (zero-floored, item 1) + NEP (shared, NOT zero-floored --
## NEP is legitimately negative, needed by M5/M6). No winter screen, no
## agreement/min-of-both series.
##   gpp_nt = zero-floored GPP_NT_REF (the main series, as-delivered, no
##            winter screen -- "Tash's analysis").
##   gpp_dt = zero-floored GPP_DT_REF (same VUT/CUT family as NT).
## ============================================================================
daily <- daily0 |>
  mutate(
    gpp_nt = pmax(GPP_NT_REF, 0),
    gpp_dt = pmax(GPP_DT_REF, 0),
    nep    = -NEE_REF
  ) |>
  select(site_id, TIMESTAMP, year, gpp_nt, gpp_dt, nep, QC_REF)

message("Variant series built: nt (zero-floored GPP_NT_REF, no winter screen), dt (zero-floored GPP_DT_REF); NEP shared.")

## ============================================================================
## Continuous per-site daily sequence (fill any calendar-day gaps with NA --
## confirmed none exist for these 15 sites, kept as a safety net) -- needed
## for 15-day smoothing across year boundaries, and carries QC_REF through
## for AU-How's Jul-Jun day-count rule below.
## ============================================================================
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

#' 15-day centred rolling mean with a PARTIAL window at the true start/end
#' of the vector `x` (no padding/reflection). NA values within a window are
#' dropped (na.rm). Unchanged from v1.
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
  mutate(
    gpp_nt_sm = roll_mean_centred(gpp_nt),
    gpp_dt_sm = roll_mean_centred(gpp_dt),
    nep_sm    = roll_mean_centred(nep)
  ) |>
  ungroup()
message("15-day centred smoothing applied (gpp_nt, gpp_dt, nep) over each site's full continuous record.")

## ============================================================================
## Item 2: Southern-Hemisphere (AU-How) Jul-Jun season years, EVERY method
## (M1-M6, including M2-M4, and P99), with a fresh day-count passing rule: a
## Jul(y)-Jun(y+1) window passes iff it has <=EXP_RULE_MAX_FAIL_DAYS failing
## days under the existing per-day QC rule (qc_passes(QC_REF, EXP_RULE_QC_DAY),
## NA counts as failing) -- NOT the v1 proxy ("both bracketing calendar years
## independently pass R2/N=14"), which is a coarser, different test. For NH
## sites `season_years_for()` is unchanged: the site's own calendar years
## from site_years.csv (already R2/N=14-passing). EXP_RULE_QC_DAY (0.25) and
## EXP_RULE_MAX_FAIL_DAYS (14) are both defined in 00_config.R (sourced
## above) and are the SAME constants scripts/04_qc.R and 01_select.R use for
## the calendar-year rule -- "the existing QC rule", per the prompt.
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
message("AU-How: ", length(auhow_old_years), " calendar (R2/N=14) year(s) [", paste(auhow_old_years, collapse = ", "),
        "] vs ", length(auhow_new_years), " new Jul-Jun (<=14-failing-day) season year(s) [",
        paste(auhow_new_years, collapse = ", "), "] under item 2's rule.")

#' All of a site's passing-year windows, concatenated as one Date vector
#' (season_years_for()-aware; never overlaps across years by construction).
pooled_window_dates <- function(sid) do.call(c, lapply(season_years_for(sid), function(y) window_dates(sid, y)))

#' A (date, month, value) table over a site's pooled passing-year windows,
#' for `gcol` -- the single building block for P99/summer-P99/peak-P99/
#' peak-month selection below (uniformly season-year-aware for AU-How, item 2).
pool_tbl <- function(sid, gcol) {
  dts <- pooled_window_dates(sid)
  d <- daily_cont |> filter(site_id == sid)
  tibble(date = dts, month = lubridate::month(dts), value = d[[gcol]][match(dts, d$TIMESTAMP)])
}
pool_values <- function(sid, gcol) { v <- pool_tbl(sid, gcol)$value; v[!is.na(v)] }

message("Site(s) x years to be processed (season_years_for(), post-item-2): ",
        paste(sprintf("%s=%d", IAV_SITES, sapply(IAV_SITES, function(s) length(season_years_for(s)))), collapse = ", "))

## ============================================================================
## Per-site, per-series P99 (item 1: NT and DT, each from its own series;
## item 2: AU-How pooled over Jul-Jun season years via pool_values()). A
## site with no DT gets P99_dt = NA (sites_no_dt).
## ============================================================================
P99_nt <- setNames(sapply(IAV_SITES, function(s) as.numeric(quantile(pool_values(s, "gpp_nt"), 0.99, type = 7))), IAV_SITES)
P99_dt <- setNames(sapply(IAV_SITES, function(s) {
  v <- pool_values(s, "gpp_dt")
  if (length(v) == 0) NA_real_ else as.numeric(quantile(v, 0.99, type = 7))
}), IAV_SITES)

## ---- Internal sanity check (NH sites only): fresh NT P99, pooled over
## calendar-year windows (unchanged for NH under item 2), must equal
## derived_site.csv's own P99 (variant main_unscreened) -- NT is exactly
## v1's "unscreened" series (zero-floored GPP_NT_REF, no winter screen), and
## derive_one() pools calendar-year windows, so for NH sites this MUST match
## exactly. AU-How is excluded (its P99 is now season-year-pooled, item 2,
## and will legitimately differ from derived_site.csv's calendar-year P99 --
## see the AU-How comparison below). ----
nh_sites <- setdiff(IAV_SITES, sh_sites)
chk_P99 <- derived_site_all |> filter(site_id %in% nh_sites, variant == "main_unscreened") |>
  select(site_id, P99) |> mutate(P99_fresh = P99_nt[site_id], diff = P99 - P99_fresh)
if (any(abs(chk_P99$diff) > 1e-9)) {
  print(chk_P99[abs(chk_P99$diff) > 1e-9, ])
  stop("11_gsl_methods_compare.R: freshly computed NT P99 does not match derived_site.csv for a NH site -- stopping.")
}
message("Check passed: fresh NT P99 exactly matches derived_site.csv's main_unscreened P99 for all ", length(nh_sites), " NH sites.")

auhow_P99_old <- derived_site_all$P99[derived_site_all$site_id == "AU-How" & derived_site_all$variant == "main_unscreened"]
message("AU-How P99 (zero-floored GPP_NT_REF): old calendar-year pooling = ", round(auhow_P99_old, 3),
        "; new Jul-Jun season-year pooling (item 2) = ", round(P99_nt["AU-How"], 3))

## ============================================================================
## M2/M3/M4 (count/fraction methods). Item 1: thresh_col == value_col ==
## gcol always (nt or dt), never mixed. Item 2: windows = window_dates(sid,y)
## for y in season_years_for(sid) -- calendar for NH, Jul-Jun for AU-How, in
## EVERY method now (no more calendar-only carve-out for M2-M4).
## ============================================================================

#' One (site, year, threshold) count-method result. Unchanged from v1;
#' thresh_vals and value_vals are always the SAME series now (item 1).
count_method_one <- function(dates, thresh_vals, value_vals, T_abs) {
  ok <- !is.na(thresh_vals) & !is.na(value_vals)
  above <- ok & thresh_vals > T_abs
  n <- sum(above, na.rm = TRUE)
  annual_gpp <- sum(value_vals, na.rm = TRUE)
  if (n == 0) {
    return(tibble(gsl = 0L, start_date = as.Date(NA), end_date = as.Date(NA),
                  gpp_in_season = 0, annual_gpp = annual_gpp, share = NA_real_,
                  flag_no_season = TRUE, flag_whole_year = FALSE, flag_no_dt = FALSE))
  }
  idx_above <- which(above)
  gpp_in_season <- sum(value_vals[idx_above], na.rm = TRUE)
  tibble(gsl = n, start_date = dates[idx_above[1]], end_date = dates[idx_above[length(idx_above)]],
         gpp_in_season = gpp_in_season, annual_gpp = annual_gpp,
         share = if (annual_gpp != 0) gpp_in_season / annual_gpp else NA_real_,
         flag_no_season = FALSE, flag_whole_year = (n == length(dates)), flag_no_dt = FALSE)
}
no_dt_row <- function() tibble(gsl = NA_integer_, start_date = as.Date(NA), end_date = as.Date(NA),
                                gpp_in_season = NA_real_, annual_gpp = NA_real_, share = NA_real_,
                                flag_no_season = FALSE, flag_whole_year = FALSE, flag_no_dt = TRUE)

#' Run M2 (pct-of-P99), M3 (fixed abs.) and M4 (fine-grid fraction target)
#' for ONE series ("nt" or "dt"), across all 15 sites x their own years.
run_count_methods <- function(variant, gcol, P99_vec) {
  m2_pcts <- c(5, 10, 20)
  m4_targets <- c(0.90, 0.95, 0.99)
  m3_abs <- c(0.5, 1, 2) # gC m-2 d-1, fixed, no P99 scaling

  out <- list()
  for (sid in IAV_SITES) {
    yrs <- season_years_for(sid)
    if (variant == "dt" && sid %in% sites_no_dt) {
      for (y in yrs) {
        for (pct in m2_pcts) out[[length(out) + 1]] <- no_dt_row() |> mutate(site_id = sid, year = y, variant = variant, method = "M2", sub = sprintf("%dpct", pct))
        for (abs_t in m3_abs) out[[length(out) + 1]] <- no_dt_row() |> mutate(site_id = sid, year = y, variant = variant, method = "M3", sub = sprintf("%sgC", format(abs_t)))
        for (target in m4_targets) out[[length(out) + 1]] <- no_dt_row() |> mutate(site_id = sid, year = y, variant = variant, method = "M4", sub = sprintf("%.2f", target), threshold_pct_of_P99 = NA_real_)
      }
      next
    }
    d <- daily_cont |> filter(site_id == sid)
    P99 <- P99_vec[[sid]]

    ## ---- M4's fine-grid rule: mean F(T) across ALL this site's passing
    ## years (season_years_for()-aware), at every fine-grid point. --------
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

      for (pct in m2_pcts) {
        T_abs <- pct / 100 * P99
        r <- count_method_one(win, vals, vals, T_abs)
        out[[length(out) + 1]] <- r |> mutate(site_id = sid, year = y, variant = variant, method = "M2", sub = sprintf("%dpct", pct))
      }
      for (abs_t in m3_abs) {
        r <- count_method_one(win, vals, vals, abs_t)
        out[[length(out) + 1]] <- r |> mutate(site_id = sid, year = y, variant = variant, method = "M3", sub = sprintf("%sgC", format(abs_t)))
      }
      for (target in m4_targets) {
        idx <- which(f_rounded <= round(target, 6))[1]
        if (is.na(idx)) {
          r <- tibble(gsl = NA_integer_, start_date = as.Date(NA), end_date = as.Date(NA),
                      gpp_in_season = NA_real_, annual_gpp = sum(vals, na.rm = TRUE), share = NA_real_,
                      flag_no_season = TRUE, flag_whole_year = FALSE, flag_no_dt = FALSE)
        } else {
          T_abs <- fine_abs[idx]
          r <- count_method_one(win, vals, vals, T_abs)
        }
        out[[length(out) + 1]] <- r |> mutate(site_id = sid, year = y, variant = variant, method = "M4", sub = sprintf("%.2f", target),
                                                threshold_pct_of_P99 = if (!is.na(idx)) FINE_GRID_PCT[idx] else NA_real_)
      }
    }
  }
  bind_rows(out)
}

message("Running M2/M3/M4 for 2 series (nt, dt) x 15 sites...")
m234_nt <- run_count_methods("nt", "gpp_nt", P99_nt)
m234_dt <- run_count_methods("dt", "gpp_dt", P99_dt)
m234_all <- bind_rows(m234_nt, m234_dt)
message("M2/M3/M4: ", nrow(m234_all), " site x year x series x method-threshold rows.")

## ============================================================================
## Check (NH sites only): M2 at 10% and M4 at 0.95 on the NT series must
## equal derived_site_year.csv's main_unscreened gsl_z16/gsl_t95 (and
## f_z16/f_t95). AU-How is excluded -- its windows are now Jul-Jun season
## years (item 2), which do not correspond to derived_site_year.csv's
## calendar-year rows at all (see the AU-How comparison section below for
## its own, separate before/after numbers).
## ============================================================================
dsy_check <- derived_site_year_all |> filter(site_id %in% nh_sites, variant == "main_unscreened") |>
  select(site_id, year, gsl_z16, gsl_t95, f_z16, f_t95)

m2_10_nt <- m234_all |> filter(variant == "nt", method == "M2", sub == "10pct") |> select(site_id, year, gsl, share)
m4_95_nt <- m234_all |> filter(variant == "nt", method == "M4", sub == "0.95") |> select(site_id, year, gsl, share)

chk1 <- dsy_check |>
  left_join(m2_10_nt |> rename(gsl_m2_10 = gsl, share_m2_10 = share), by = c("site_id", "year")) |>
  left_join(m4_95_nt |> rename(gsl_m4_95 = gsl, share_m4_95 = share), by = c("site_id", "year")) |>
  mutate(
    diff_gsl_z16 = gsl_z16 - gsl_m2_10, diff_f_z16 = f_z16 - share_m2_10,
    diff_gsl_t95 = gsl_t95 - gsl_m4_95, diff_f_t95 = f_t95 - share_m4_95
  )
bad_gsl_z16 <- chk1 |> filter(!is.na(diff_gsl_z16) & abs(diff_gsl_z16) > 0)
bad_gsl_t95 <- chk1 |> filter(!is.na(diff_gsl_t95) & abs(diff_gsl_t95) > 0)
bad_f_z16   <- chk1 |> filter(!is.na(diff_f_z16) & abs(diff_f_z16) > 1e-9)
bad_f_t95   <- chk1 |> filter(!is.na(diff_f_t95) & abs(diff_f_t95) > 1e-9)
if (nrow(bad_gsl_z16) + nrow(bad_gsl_t95) + nrow(bad_f_z16) + nrow(bad_f_t95) > 0) {
  cat("--- M2@10%/M4@0.95 (NT) CHECK FAILURES (NH sites) ---\n")
  print(bad_gsl_z16); print(bad_gsl_t95); print(bad_f_z16); print(bad_f_t95)
  stop("11_gsl_methods_compare.R: NT-series M2@10%/M4@0.95 do not exactly match derived_site_year.csv's main_unscreened gsl_z16/gsl_t95/f_z16/f_t95 for a NH site -- stopping.")
}
message("Check passed: NT-series M2@10% == gsl_z16/f_z16 and M4@0.95 == gsl_t95/f_t95 (main_unscreened), exactly, for all ",
        nrow(chk1), " NH site-year rows.")

## ---- AU-How: M2@10%/M4@0.95 on the NT series, new Jul-Jun season years,
## vs. the OLD calendar-year main_unscreened values -- "report how AU-How's
## years and GSL change against the calendar-year version" (item 2). -------
auhow_old_gsl <- derived_site_year_all |> filter(site_id == "AU-How", variant == "main_unscreened") |>
  select(year, gsl_z16_old = gsl_z16, gsl_t95_old = gsl_t95)
auhow_new_gsl <- m234_all |> filter(site_id == "AU-How", variant == "nt", method == "M2", sub == "10pct") |>
  select(year, gsl_z16_new = gsl) |>
  left_join(m234_all |> filter(site_id == "AU-How", variant == "nt", method == "M4", sub == "0.95") |> select(year, gsl_t95_new = gsl), by = "year")
message("AU-How GSL, old (calendar year) vs new (Jul-Jun season year, item 2):")
print(as.data.frame(auhow_old_gsl))
print(as.data.frame(auhow_new_gsl))

## ============================================================================
## Annual GPP/NEP for the detrended-r correlation used in the summary table
## (season_years_for()/window_dates()-aware -- uniformly handles AU-How's
## new Jul-Jun years now, item 2). This is NOT the same "annual_gpp" column
## used above as F(T)'s own denominator.
## ============================================================================
annual_gpp_nep_for <- function(sid, gpp_col) {
  yrs <- season_years_for(sid)
  purrr::map_dfr(yrs, function(y) {
    win <- window_dates(sid, y)
    d <- daily_cont |> filter(site_id == sid, TIMESTAMP %in% win)
    tibble(site_id = sid, year = y, annual_gpp = sum(d[[gpp_col]], na.rm = TRUE),
           annual_nep = sum(d$nep, na.rm = TRUE))
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
## M1 (Zscheischler et al. 2016 crossing-span). Item 1: thresholds (summer,
## peak, annual P99) computed from EACH series' own pooled values
## (pool_tbl()/pool_values(), already season_years_for()-aware for AU-How,
## item 2). M1's span detection itself also uses season_years_for()/
## window_dates() per site.
## ============================================================================
summer_months <- function(sid) if (is_sh(sid)) c(12, 1, 2) else c(6, 7, 8)

#' The 3 consecutive (wrapping) calendar months with the highest mean GPP,
#' pooled over this site's passing years, for series `gcol`.
peak_months_for <- function(sid, gcol) {
  pt <- pool_tbl(sid, gcol)
  month_mean <- vapply(1:12, function(m) mean(pt$value[pt$month == m], na.rm = TRUE), numeric(1))
  starts <- 1:12
  score <- vapply(starts, function(s) sum(month_mean[((s - 1):(s + 1)) %% 12 + 1]), numeric(1))
  best_start <- starts[which.max(score)]
  ((best_start - 1):(best_start + 1)) %% 12 + 1
}

#' P99 of zero-floored GPP (`gcol`), pooled over this site's passing years,
#' restricted to calendar months in `months`.
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

message("Peak 3-month window per site, NT series: ", paste(sprintf("%s=%s", IAV_SITES, sapply(peak_months_nt, paste, collapse = "-")), collapse = "; "))
message("Peak 3-month window per site, DT series: ", paste(sprintf("%s=%s", names(peak_months_dt), sapply(peak_months_dt, paste, collapse = "-")), collapse = "; "))

#' One (site, year, threshold) span-crossing result on a SMOOTHED series.
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
message("M1: ", nrow(m1_all), " site x year x series x sub-threshold rows. No-season flags: ", sum(m1_all$flag_no_season),
        "; whole-year flags: ", sum(m1_all$flag_whole_year), "; no-DT flags: ", sum(m1_all$flag_no_dt))

## ============================================================================
## Item 4: two-season flag via the TIMESAT secondary-peak rule (Wang et al.
## 2020), applied to 15-day SMOOTHED GPP -- replaces v1's slope-run rule on
## NEP (which saturated near-universally). M5/M6 have no GPP series of
## their own (NEP is shared, variant="all"), so this flag uses the NT
## series as a single default (same convention as the GPP-share default
## below) -- documented, not implied to differ by variant.
##
## As implemented: within one site-year window, find all STRICT local
## maxima of smoothed GPP (g[i] > both neighbours, NA-safe). The PRIMARY
## peak is the single global maximum. If there is no other (secondary)
## local maximum, there is one season (flag = FALSE). Otherwise the
## SECONDARY peak is the highest-valued other local maximum; flag TRUE iff
## (a) secondary >= 0.5 x primary, AND (b) the minimum of smoothed GPP
## strictly between the two peaks is < 0.5 x the LOWER of the two peaks
## (the "fall below half the lower peak" requirement -- this is what keeps
## a small wiggle immediately next to the main peak from counting: such a
## wiggle's between-peaks minimum stays close to the (high) primary peak,
## not down near half of itself). `two_season_ratio` = secondary/primary
## (NA if no secondary local max exists at all) is reported alongside the
## boolean flag as the "number" requested by the prompt.
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

timesat_two_season <- function(gpp_sm_vals) {
  if (all(is.na(gpp_sm_vals))) return(list(flag = FALSE, ratio = NA_real_))
  maxima <- local_maxima_idx(gpp_sm_vals)
  if (length(maxima) < 1) return(list(flag = FALSE, ratio = NA_real_))
  primary_idx <- maxima[which.max(gpp_sm_vals[maxima])]
  primary_val <- gpp_sm_vals[primary_idx]
  others <- setdiff(maxima, primary_idx)
  if (length(others) == 0) return(list(flag = FALSE, ratio = NA_real_))
  secondary_idx <- others[which.max(gpp_sm_vals[others])]
  secondary_val <- gpp_sm_vals[secondary_idx]
  ratio <- secondary_val / primary_val
  lo_idx <- min(primary_idx, secondary_idx); hi_idx <- max(primary_idx, secondary_idx)
  between <- gpp_sm_vals[(lo_idx + 1):(hi_idx - 1)]
  valley <- if (length(between) == 0 || all(is.na(between))) NA_real_ else min(between, na.rm = TRUE)
  lower_peak <- min(primary_val, secondary_val)
  flag <- !is.na(ratio) && ratio >= 0.5 && !is.na(valley) && valley < 0.5 * lower_peak
  list(flag = flag, ratio = ratio)
}

## ============================================================================
## M5: carbon-uptake period from smoothed NEP > 0 (unchanged from v1's
## definitions). count = n days > 0; span = first day of the first >=5-day
## run to the last day of the last such run. Net-source flag unchanged.
## Two-season flag now computed via timesat_two_season() on the NT series'
## smoothed GPP for this site-year window (item 4).
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

      ts <- timesat_two_season(gpp_nt_sm_win)

      count_row <- tibble(gsl = n_above, start_date = if (n_above > 0) win[which(above)[1]] else as.Date(NA),
                           end_date = if (n_above > 0) win[which(above)[n_above]] else as.Date(NA),
                           sub = "count")

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
        out[[length(out) + 1]] <- rowd |> mutate(site_id = sid, year = y, variant = "all", method = "M5")
      }
    }
  }
  bind_rows(out)
}

message("Running M5 (count + span) for 15 sites...")
m5_all <- run_m5()
message("M5: ", nrow(m5_all), " site x year x sub rows. Net-source years: ", sum(m5_all$flag_net_source[m5_all$sub == "span"]),
        " of ", sum(m5_all$sub == "span"), ". Two-season (TIMESAT, item 4) flags: ",
        sum(m5_all$flag_two_season[m5_all$sub == "span"]), " of ", sum(m5_all$sub == "span"))

## ============================================================================
## M6 (Hu et al. 2010 rate-of-change of NEP), on UNSMOOTHED daily NEP. Item
## 3: start search window = M5-span's own start +/-30 days; end search
## window = M5-span's own end +/-30 days (steepest positive / shallowest
## negative slope, as in v1; k = 3-9; drops the global-seasonal-peak device
## and the alternate-end column entirely). A site-year with no M5-span
## (flag_no_season on M5-span, which includes net-source years) gets NO M6
## value and is flagged (no_m5_span), per the prompt.
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

#' M6 for one site-year window, one k: returns start/end (date) indices
#' (within `win`), given the M5-span start/end indices.
m6_one_k <- function(win, nep_raw_win, k, m5_start_idx, m5_end_idx) {
  n <- length(win)
  slope <- slope_at(nep_raw_win, k)

  s_lo <- max(1, m5_start_idx - 30); s_hi <- min(n, m5_start_idx + 30)
  seg_s <- slope[s_lo:s_hi]
  start_idx <- if (all(is.na(seg_s))) NA_integer_ else s_lo - 1L + which.max(seg_s)

  e_lo <- max(1, m5_end_idx - 30); e_hi <- min(n, m5_end_idx + 30)
  seg_e <- slope[e_lo:e_hi]
  neg_e <- seg_e; neg_e[!is.na(neg_e) & neg_e >= 0] <- NA
  end_idx <- if (all(is.na(neg_e))) NA_integer_ else e_lo - 1L + which.max(neg_e) # shallowest (closest to 0) negative slope

  list(start = start_idx, end = end_idx)
}

run_m6 <- function() {
  out <- list()
  for (sid in IAV_SITES) {
    yrs <- season_years_for(sid)
    d <- daily_cont |> filter(site_id == sid)
    for (y in yrs) {
      win <- window_dates(sid, y)
      dy <- d |> filter(TIMESTAMP %in% win)
      nep_raw_win <- dy$nep[match(win, dy$TIMESTAMP)]

      m5_span <- m5_all |> filter(site_id == sid, year == y, sub == "span")
      no_span <- nrow(m5_span) != 1 || isTRUE(m5_span$flag_no_season[1]) || is.na(m5_span$start_date[1]) || is.na(m5_span$end_date[1])
      net_source <- nrow(m5_span) == 1 && isTRUE(m5_span$flag_net_source[1])
      two_season <- nrow(m5_span) == 1 && isTRUE(m5_span$flag_two_season[1])

      if (no_span) {
        out[[length(out) + 1]] <- tibble(site_id = sid, year = y, variant = "all", method = "M6", sub = "span",
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
        r <- m6_one_k(win, nep_raw_win, k, m5_start_idx, m5_end_idx)
        if (!is.na(r$start)) starts <- c(starts, r$start)
        if (!is.na(r$end)) ends <- c(ends, r$end)
      }
      no_season <- length(starts) == 0 || length(ends) == 0
      start_med <- if (length(starts) > 0) win[round(median(starts))] else as.Date(NA)
      end_med   <- if (length(ends) > 0) win[round(median(ends))] else as.Date(NA)
      gsl <- if (!no_season) as.integer(end_med - start_med + 1L) else NA_integer_

      out[[length(out) + 1]] <- tibble(
        site_id = sid, year = y, variant = "all", method = "M6", sub = "span",
        gsl = gsl, start_date = start_med, end_date = end_med,
        k_range_start_days = if (length(starts) > 1) max(starts) - min(starts) else 0L,
        k_range_end_days = if (length(ends) > 1) max(ends) - min(ends) else 0L,
        flag_no_season = no_season, flag_no_m5_span = FALSE, flag_net_source = net_source,
        flag_two_season = two_season
      )
    }
  }
  bind_rows(out)
}

message("Running M6 (k=3-9 slopes, M5-span +/-30-day windows, item 3) for 15 sites...")
m6_all <- run_m6()
message("M6: ", nrow(m6_all), " site-year rows. No-M5-span flags: ", sum(m6_all$flag_no_m5_span),
        "; other no-season flags: ", sum(m6_all$flag_no_season & !m6_all$flag_no_m5_span),
        ". Median k-range (start): ", median(m6_all$k_range_start_days, na.rm = TRUE), " days; (end): ",
        median(m6_all$k_range_end_days, na.rm = TRUE), " days. Max k-range (start): ", max(m6_all$k_range_start_days, na.rm = TRUE),
        "; (end): ", max(m6_all$k_range_end_days, na.rm = TRUE))

## ============================================================================
## M5/M6 have no GPP series of their own (NEP is shared) -- "share of annual
## GPP inside the season" uses the NT series as the single default
## (unchanged convention from v1, "screened" renamed to "nt").
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
## Assemble the unified site-year table (one row per site x year x variant
## x method x sub). Harmonise columns across all 6 methods' outputs into
## one superset schema; unused flags/fields are NA/FALSE for a given row.
## ============================================================================
flag_cols <- c("flag_no_season", "flag_whole_year", "flag_net_source", "flag_two_season",
               "flag_no_start_crossing", "flag_no_end_crossing", "flag_no_dt", "flag_no_m5_span")
extra_cols <- c("threshold_pct_of_P99", "k_range_start_days", "k_range_end_days", "two_season_ratio")

harmonise <- function(tbl) {
  for (fc in flag_cols) if (!fc %in% names(tbl)) tbl[[fc]] <- FALSE
  for (ec in extra_cols) if (!ec %in% names(tbl)) tbl[[ec]] <- NA
  tbl |> select(site_id, year, variant, method, sub, gsl, start_date, end_date, share, gpp_in_season,
                 annual_gpp, all_of(flag_cols), all_of(extra_cols))
}

gsl_methods_site_year <- bind_rows(
  harmonise(m234_all), harmonise(m1_all), harmonise(m5_all), harmonise(m6_all)
) |> arrange(site_id, year, method, variant, sub)

message("Assembled gsl_methods_site_year: ", nrow(gsl_methods_site_year), " rows.")
## Every row must have either a non-NA gsl OR a TRUE explanatory flag.
unaccounted <- gsl_methods_site_year |> filter(is.na(gsl) & !flag_no_season & !flag_net_source & !flag_no_dt)
if (nrow(unaccounted) > 0) {
  print(unaccounted)
  stop("11_gsl_methods_compare.R: ", nrow(unaccounted), " row(s) have neither a GSL value nor an explanatory flag -- stopping.")
}
message("Check passed: every row (every method and series) has a GSL value or an explanatory flag (", nrow(unaccounted), " unaccounted rows).")

## ============================================================================
## Per-site summary (mean GSL, interannual SD, mean start/end, flag counts,
## detrended r vs annual GPP/NEP). Net-source years are excluded from M5/M6
## site means and detrended-r (unchanged from v1); not excluded for M1-M4.
## ============================================================================
annual_corr_by_variant <- list(nt = annual_corr_nt, dt = annual_corr_dt, all = annual_corr_nt)

mean_doy <- function(dates) {
  ok <- dates[!is.na(dates)]
  if (length(ok) == 0) return(NA_real_)
  mean(lubridate::yday(ok))
}

summarise_group <- function(g) {
  sid <- g$site_id[1]; variant <- g$variant[1]
  ac <- annual_corr_by_variant[[variant]]
  gj <- g |> filter(!flag_net_source) |>
    left_join(ac, by = c("site_id", "year"), suffix = c("", "_corr"))

  rg <- detrend_cor(gj$gsl, gj$annual_gpp_corr, gj$year)
  rn <- detrend_cor(gj$gsl, gj$annual_nep, gj$year)

  tibble(
    site_id = sid, variant = variant, method = g$method[1], sub = g$sub[1],
    n_site_years = nrow(g), n_used_for_means = sum(!g$flag_net_source & !is.na(g$gsl)),
    mean_gsl = mean(g$gsl[!g$flag_net_source], na.rm = TRUE), sd_gsl = sd(g$gsl[!g$flag_net_source], na.rm = TRUE),
    mean_start_doy = mean_doy(g$start_date[!g$flag_net_source]), mean_end_doy = mean_doy(g$end_date[!g$flag_net_source]),
    mean_share = mean(g$share[!g$flag_net_source], na.rm = TRUE),
    n_flag_no_season = sum(g$flag_no_season, na.rm = TRUE), n_flag_whole_year = sum(g$flag_whole_year, na.rm = TRUE),
    n_flag_net_source = sum(g$flag_net_source, na.rm = TRUE),
    n_flag_two_season = sum(g$flag_two_season, na.rm = TRUE),
    n_flag_no_start_crossing = sum(g$flag_no_start_crossing, na.rm = TRUE),
    n_flag_no_end_crossing = sum(g$flag_no_end_crossing, na.rm = TRUE),
    n_flag_no_dt = sum(g$flag_no_dt, na.rm = TRUE), n_flag_no_m5_span = sum(g$flag_no_m5_span, na.rm = TRUE),
    r_gpp = unname(rg["r"]), p_gpp = unname(rg["p"]), n_gpp = unname(rg["n"]),
    r_nep = unname(rn["r"]), p_nep = unname(rn["p"]), n_nep = unname(rn["n"])
  )
}

gsl_methods_site_year$.key <- paste(gsl_methods_site_year$site_id, gsl_methods_site_year$variant,
                                      gsl_methods_site_year$method, gsl_methods_site_year$sub, sep = "\u0001")
groups <- split(gsl_methods_site_year, gsl_methods_site_year$.key)
gsl_methods_site_year$.key <- NULL
gsl_methods_site_summary <- bind_rows(lapply(groups, summarise_group)) |>
  arrange(site_id, method, sub, variant)

message("Assembled gsl_methods_site_summary: ", nrow(gsl_methods_site_summary), " rows.")

## ============================================================================
## Net-source and no-DT exclusions/unknowns -> this side project's own logs.
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
              reason = "No season detected (no day/run above threshold, or no M5-span to anchor M6's search windows, within this method's window)",
              logged_by = "11_gsl_methods_compare.R", log_path = EXP_UNKNOWN_LOG)
}
message("Logged ", nrow(net_source_rows), " net-source exclusion(s) and ", nrow(no_season_rows), " no-season unknown(s) to ",
        EXP_EXCLUSION_LOG, " / ", EXP_UNKNOWN_LOG, ".")

## ============================================================================
## Write output tables (overwrite v1's, in place, per the v2 prompt -- v1's
## content lives on in git history at 15dc51f).
## ============================================================================
write_csv(gsl_methods_site_year, file.path(EXP_TABLES, "gsl_methods_site_year.csv"))
write_meta(
  file.path(EXP_TABLES, "gsl_methods_site_year.csv"),
  input_sources = c("data/duckdb/fluxnet.duckdb (daily, dataset='FLUXMET')", file.path(EXP_TABLES, "sites.csv"),
                      file.path(EXP_TABLES, "site_years.csv"), file.path(EXP_TABLES, "derived_site.csv"),
                      file.path(EXP_TABLES, "derived_site_year.csv")),
  notes = paste0(
    "VERSION 2 (supersedes v1, 15dc51f -- see docs/gsl_methods_compare_2026-11-02_v2.md). Preliminary ",
    "comparison of 6 published GSL methods (M1-M6) at the 15 'Figure GSL-IAV' sites. Two independent ",
    "GPP series, never mixed within a method: nt = zero-floored GPP_NT_REF (no winter screen), dt = ",
    "zero-floored GPP_DT_REF, each with its own P99/fine grid. M5/M6 use NEP only (variant='all'). ",
    "Southern-Hemisphere (AU-How) uses Jul-Jun season years in EVERY method including M2-M4 and for ",
    "P99, with a fresh day-count passing rule (<=14 failing days under the existing 0.25 daily-QC ",
    "rule), not the v1 'both bracketing calendar years pass' proxy. M6 searches for start/end only ",
    "within +/-30 days of M5-span's own start/end (k=3-9 day OLS slope on UNSMOOTHED NEP, median ",
    "across k; no alternate-end). Two-season flag (M5-span/M6 only) uses the TIMESAT secondary-peak ",
    "rule (Wang et al. 2020) on 15-day-smoothed NT GPP, not a slope-run rule. Checks (NH sites only, ",
    "all passed exactly): NT-series M2@10%/M4@0.95 == derived_site_year.csv's main_unscreened ",
    "gsl_z16/gsl_t95/f_z16/f_t95; every row has a value or an explanatory flag. See SESSION_LOG.md ",
    "and the methods doc for AU-How's before/after comparison, DT-vs-NT partitioning uncertainty, ",
    "and M6-vs-v1."
  )
)

write_csv(gsl_methods_site_summary, file.path(EXP_TABLES, "gsl_methods_site_summary.csv"))
write_meta(
  file.path(EXP_TABLES, "gsl_methods_site_summary.csv"),
  input_sources = file.path(EXP_TABLES, "gsl_methods_site_year.csv"),
  notes = paste0(
    "VERSION 2. Per site x series x method x sub-threshold: mean GSL, interannual SD, mean start/end ",
    "day-of-window, mean share of annual GPP inside the season, flag counts, and detrended Pearson r ",
    "(with p, n) of annual GSL against annual GPP and against annual NEP -- each series detrended by ",
    "OLS on year first. Net-source years excluded from M5/M6 means/r; not excluded elsewhere. Annual ",
    "GPP/NEP reference for every r is the season_years_for()-aware total (Jul-Jun for AU-How, item 2)."
  )
)
message("Saved: gsl_methods_site_year.csv (", nrow(gsl_methods_site_year), " rows), gsl_methods_site_summary.csv (",
        nrow(gsl_methods_site_summary), " rows).")

## ============================================================================
## Item 1 report data: DT minus NT, per site-year x method x sub (the
## "partitioning uncertainty"), plus a per-site aggregate and a decomposition
## of whether the GSL difference is explained by the start/end shift (span
## methods, where it must be, by construction) or by interior/"winter" day
## disagreement (count methods, where it need not be).
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
  )
write_csv(dt_vs_nt, file.path(EXP_TABLES, "gsl_methods_dt_vs_nt.csv"))
write_meta(
  file.path(EXP_TABLES, "gsl_methods_dt_vs_nt.csv"),
  input_sources = file.path(EXP_TABLES, "gsl_methods_site_year.csv"),
  notes = paste0(
    "VERSION 2, item 1. DT-minus-NT difference in GSL/start/end, per site x year x method x sub, for ",
    "every row where both series have a value (flag_no_dt excluded). boundary_explained = the GSL ",
    "difference implied purely by the start/end shift (= dt_minus_nt_end_days - dt_minus_nt_start_days); ",
    "for span methods (M1) this equals dt_minus_nt_gsl exactly, by construction (GSL = end-start+1). ",
    "interior_contribution = dt_minus_nt_gsl - boundary_explained; non-zero only for count methods ",
    "(M2/M3/M4), where it measures GSL difference NOT explained by a start/end shift -- i.e. scattered ",
    "interior ('winter') day disagreement between the two partitioning methods. See the methods doc's ",
    "'NT vs DT' section for the per-site summary and interpretation."
  )
)
message("Saved: gsl_methods_dt_vs_nt.csv (", nrow(dt_vs_nt), " rows).")

## ============================================================================
## Figures (NT series throughout -- the main default series, item 1 -- for
## M1-M4; M5/M6 remain NEP-only ('all'). Same 3 drafts as v1, same filenames
## (regenerated in place).
## ============================================================================
FIG_METHODS <- tibble(
  method = c("M1", "M2", "M3", "M4", "M5", "M6"),
  sub    = c("summer", "10pct", "1gC", "0.95", "span", "span"),
  label  = c("M1 (crossing, summer P99)", "M2 (10% of P99)", "M3 (1 gC m-2 d-1)",
             "M4 (0.95 of annual GPP)", "M5 (NEP>0 span)", "M6 (Hu NEP rate-of-change)")
)
drafts_dir <- file.path(GSL_FIG_DIR, "drafts")
if (!dir.exists(drafts_dir)) dir.create(drafts_dir, recursive = TRUE)

skew_tbl <- derived_site_all |> filter(site_id %in% IAV_SITES, variant == "main") |> select(site_id, skewness)

fig_data <- gsl_methods_site_summary |>
  inner_join(FIG_METHODS, by = c("method", "sub")) |>
  filter(variant %in% c("nt", "all")) |>
  left_join(skew_tbl, by = "site_id") |>
  mutate(method_f = factor(label, levels = FIG_METHODS$label))

site_order <- skew_tbl |> arrange(skewness) |> pull(site_id)
fig_data$site_id <- factor(fig_data$site_id, levels = site_order)

## ---- Figure A: dotplot -- mean GSL +/- interannual SD, by method, sites
## sorted by skewness. ----
p_dotplot <- ggplot(fig_data, aes(x = mean_gsl, y = site_id, colour = method_f)) +
  geom_errorbar(aes(xmin = mean_gsl - sd_gsl, xmax = mean_gsl + sd_gsl), orientation = "y", width = 0, alpha = 0.5,
                position = position_dodge(width = 0.7)) +
  geom_point(size = 1.8, position = position_dodge(width = 0.7)) +
  scale_colour_brewer(palette = "Dark2", name = "Method") +
  labs(x = "Mean GSL (days, NT series; +/- interannual SD)", y = NULL,
       title = "GSL methods dotplot (draft, v2)") +
  theme_gsl() + theme(legend.position = "right")

ggsave(file.path(drafts_dir, "GSL methods dotplot draft.png"), p_dotplot, width = PEERJ_FIG_WIDTH_MM * 1.3,
       height = 150, units = "mm", dpi = PEERJ_DPI, device = ragg::agg_png, bg = "white")
writeLines(c(
  "GSL methods dotplot (draft, v2). Mean GSL (NT series) with interannual SD, by method, for each of",
  "the 15 sites, sorted bottom-to-top by ascending skewness (main-variant). One sub-threshold per",
  "method: M1-summer, M2 10% of P99 (= Z16), M3 1 gC m-2 d-1, M4 0.95 of annual GPP (= T95), M5-span",
  "(NEP>0, >=5-day runs), M6 (Hu et al. 2010 NEP rate-of-change, median across k=3-9, M5-span-anchored",
  "search windows). Net-source years excluded from M5/M6's own mean/SD.",
  "Numbers behind this figure: tables/gsl_methods_site_summary.csv."
), file.path(drafts_dir, "GSL methods dotplot draft.legend.txt"))
write_meta(
  file.path(drafts_dir, "GSL methods dotplot draft.png"),
  input_sources = file.path(EXP_TABLES, "gsl_methods_site_summary.csv"),
  notes = "VERSION 2. Draft figure, not a numbered manuscript figure. NT series; one sub-threshold per method."
)
message("Saved: 'GSL methods dotplot draft'.png/.legend.txt/.meta.json")

## ============================================================================
## Figure B: method agreement -- Spearman correlation between methods'
## annual GSL (and separately start, end), pooled across sites as site-year
## anomalies.
## ============================================================================
wide_base <- gsl_methods_site_year |>
  inner_join(FIG_METHODS, by = c("method", "sub")) |>
  filter(variant %in% c("nt", "all")) |>
  mutate(start_doy = lubridate::yday(start_date), end_doy = lubridate::yday(end_date)) |>
  select(site_id, year, label, gsl, start_doy, end_doy)

anomaly_matrix <- function(value_col) {
  w <- wide_base |> select(site_id, year, label, value = all_of(value_col)) |>
    group_by(site_id, label) |> mutate(anomaly = value - mean(value, na.rm = TRUE)) |> ungroup() |>
    select(site_id, year, label, anomaly) |>
    pivot_wider(names_from = label, values_from = anomaly)
  m <- as.matrix(w |> select(-site_id, -year))
  cor(m, method = "spearman", use = "pairwise.complete.obs")
}

cor_gsl   <- anomaly_matrix("gsl")
cor_start <- anomaly_matrix("start_doy")
cor_end   <- anomaly_matrix("end_doy")

cor_to_long <- function(m, which_lab) {
  d <- as.data.frame(m); d$row <- rownames(d)
  pivot_longer(d, -row, names_to = "col", values_to = "r") |> mutate(which = which_lab)
}
cor_long <- bind_rows(cor_to_long(cor_gsl, "GSL"), cor_to_long(cor_start, "Start date"), cor_to_long(cor_end, "End date")) |>
  mutate(row = factor(row, levels = FIG_METHODS$label), col = factor(col, levels = rev(FIG_METHODS$label)),
         which = factor(which, levels = c("GSL", "Start date", "End date")))

p_agree <- ggplot(cor_long, aes(x = row, y = col, fill = r)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", r)), size = PEERJ_ANNOTATION_PT / .pt * 0.8) +
  scale_fill_gradient2(low = "#b2182b", mid = "white", high = "#2166ac", midpoint = 0, limits = c(-1, 1), name = "Spearman r") +
  facet_wrap(~ which, nrow = 1) +
  labs(x = NULL, y = NULL, title = "GSL methods agreement (draft, v2)") +
  theme_gsl() + theme(axis.text.x = element_text(angle = 40, hjust = 1), legend.position = "right")

ggsave(file.path(drafts_dir, "GSL methods agreement draft.png"), p_agree, width = PEERJ_FIG_WIDTH_MM * 1.8,
       height = 130, units = "mm", dpi = PEERJ_DPI, device = ragg::agg_png, bg = "white")
writeLines(c(
  "GSL methods agreement (draft, v2). Spearman correlation between pairs of methods' annual GSL (left",
  "panel), start date (middle) and end date (right), pooled across all 15 sites x years as site-year",
  "anomalies -- NT series for M1-M4, NEP-only ('all') for M5/M6. One sub-threshold per method:",
  "M1-summer, M2 10%, M3 1 gC m-2 d-1, M4 0.95, M5-span, M6. Pairwise-complete Spearman correlation.",
  "Numbers behind this figure: tables/gsl_methods_site_year.csv."
), file.path(drafts_dir, "GSL methods agreement draft.legend.txt"))
write_meta(
  file.path(drafts_dir, "GSL methods agreement draft.png"),
  input_sources = file.path(EXP_TABLES, "gsl_methods_site_year.csv"),
  notes = "VERSION 2. Draft figure, not a numbered manuscript figure. Site-year anomalies, pairwise-complete Spearman correlation."
)
message("Saved: 'GSL methods agreement draft'.png/.legend.txt/.meta.json")

## ============================================================================
## Figure C: "raster" -- US-Ha1, US-Wkg, US-SRM, years (y) by day-of-year
## (x), the detected season (start-end) as one horizontal bar per site-year,
## one row BLOCK per method.
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
  labs(x = "Day of year", y = "Year", title = "GSL methods raster (draft, v2)") +
  theme_gsl() + theme(strip.text.y = element_text(angle = 0, size = 7), strip.text.x = element_text(face = "bold"))

ggsave(file.path(drafts_dir, "GSL methods raster draft.png"), p_raster, width = PEERJ_FIG_WIDTH_MM * 1.3,
       height = 230, units = "mm", dpi = PEERJ_DPI, device = ragg::agg_png, bg = "white")
writeLines(c(
  "GSL methods raster (draft, v2). US-Ha1, US-Wkg, US-SRM (columns); each row block is one method",
  "(M1-summer, M2 10%, M4 0.95, M5-span, M6 top to bottom); each horizontal bar is one year's",
  "detected season, start day-of-year to end day-of-year (NT series for M1/M2/M4, NEP-only for",
  "M5/M6). A missing bar means no season was detected -- see tables/gsl_methods_site_year.csv.",
  "Numbers behind this figure: tables/gsl_methods_site_year.csv."
), file.path(drafts_dir, "GSL methods raster draft.legend.txt"))
write_meta(
  file.path(drafts_dir, "GSL methods raster draft.png"),
  input_sources = file.path(EXP_TABLES, "gsl_methods_site_year.csv"),
  notes = "VERSION 2. Draft figure, not a numbered manuscript figure. Segments (start_doy-end_doy); all 3 sites are Northern Hemisphere."
)
message("Saved: 'GSL methods raster draft'.png/.legend.txt/.meta.json")

message("--- 11_gsl_methods_compare.R: done (version 2) ---")
