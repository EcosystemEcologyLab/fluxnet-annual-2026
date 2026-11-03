## 11_gsl_methods_compare.R — GSL expanded analysis: preliminary comparison
## of six published GSL definitions (M1-M6) at the 15 sites of the draft
## "Figure GSL-IAV" (tables/iav_15sites.csv). Dave's prompt, 2026-11-02 --
## see logs/prompts/2026-11-02_gsl_methods_compare.md.
##
## Exploratory. Does NOT modify any existing script, table or figure -- a
## brand-new, standalone script writing only to tables/gsl_methods_site_year.csv,
## tables/gsl_methods_site_summary.csv, figures/drafts/"GSL methods *.png",
## docs/gsl_methods_compare_2026-11-02.md, and this side project's own
## expanded/logs/ (log_exclusion()/log_unknown(), never the repo-root
## outputs/ logs, which scripts/04_qc.R clears on every run).
##
## Read-only: a fresh DBI pull from the repo-root DuckDB store (dataset =
## 'FLUXMET' only) for exactly these 15 sites, plus the already-committed
## expanded tables (sites.csv, site_years.csv, derived_site.csv,
## derived_site_year.csv, winter_screen_days.csv, iav_15sites.csv) used
## read-only, for context and for the correctness checks below -- none are
## modified. Post-Z16-rename (commit d43ae22) column names used throughout.

source("GSL_definition/expanded/code/00_config.R")
options(gsl.skip_driver = TRUE)
source("GSL_definition/code/03_derive.R") # g1_adjusted() only, reused for reference
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
winter_screen_days  <- read_csv(file.path(EXP_TABLES, "winter_screen_days.csv"), show_col_types = FALSE) |>
  mutate(is_screened = TRUE) |> select(site_id, TIMESTAMP, is_screened)
iav_tbl             <- read_csv(file.path(EXP_TABLES, "iav_15sites.csv"), show_col_types = FALSE)

site_meta <- exp_sites |> filter(site_id %in% IAV_SITES) |>
  select(site_id, igbp, group, location_lat, gpp_col_used)
stopifnot(all(IAV_SITES %in% site_meta$site_id))

is_sh <- function(sid) site_meta$location_lat[site_meta$site_id == sid] < 0
sh_sites <- site_meta$site_id[site_meta$location_lat < 0]
message("Southern-Hemisphere site(s) among the 15: ", paste(sh_sites, collapse = ", "))

raw_years_list <- split(exp_site_years$year[exp_site_years$site_id %in% IAV_SITES],
                          exp_site_years$site_id[exp_site_years$site_id %in% IAV_SITES])

#' A site's R2/N=14 passing years usable as a Jul-Jun SEASON year (SH only):
#' calendar label y is usable when BOTH y and y+1 are themselves passing
#' years (same rule as 02_derive.R's item 4 / 10_figure1_alts.R's
#' effective_years()). For a NH site this is simply all passing years.
season_years_for <- function(sid) {
  yrs <- sort(raw_years_list[[sid]])
  if (is_sh(sid)) yrs[(yrs + 1L) %in% yrs] else yrs
}

#' One site's annual window for "year" y -- calendar Jan-Dec for NH,
#' Jul(y)-Jun(y+1) for SH (season year), used for M1/M5/M6 (span/
#' continuous-NEP methods) and for annual GPP/NEP aggregates. NOT used for
#' M2/M3/M4, which use the literal calendar year (see the note below).
window_dates <- function(sid, y) {
  if (is_sh(sid)) seq(as.Date(sprintf("%d-07-01", y)), as.Date(sprintf("%d-06-30", y + 1L)), by = "day")
  else            seq(as.Date(sprintf("%d-01-01", y)), as.Date(sprintf("%d-12-31", y)), by = "day")
}

message("Site(s): ", paste(IAV_SITES, collapse = ", "))
message("Passing calendar years per site: ", paste(sprintf("%s=%d", names(raw_years_list), lengths(raw_years_list)), collapse = ", "))

## ============================================================================
## Raw daily pull (read-only, dataset = 'FLUXMET'), GPP_NT/DT and NEE, both
## VUT and CUT columns pulled and then resolved per site via sites.csv's own
## gpp_col_used (all 15 sites are VUT here, confirmed; CUT branch kept for
## robustness/documentation, not exercised by this site set). ---------------
## ============================================================================
con <- dbConnect(duckdb(), "data/duckdb/fluxnet.duckdb", read_only = TRUE)
site_list_sql <- paste(sprintf("'%s'", IAV_SITES), collapse = ", ")
daily_raw <- dbGetQuery(con, sprintf("
  SELECT site_id, TIMESTAMP, GPP_NT_VUT_REF, GPP_NT_CUT_REF, GPP_DT_VUT_REF, GPP_DT_CUT_REF,
         NEE_VUT_REF, NEE_CUT_REF
  FROM daily
  WHERE dataset = 'FLUXMET' AND site_id IN (%s)
", site_list_sql))
dbDisconnect(con, shutdown = TRUE)
daily_raw$TIMESTAMP <- as.Date(as.character(daily_raw$TIMESTAMP))
message("Pulled ", nrow(daily_raw), " daily FLUXMET row(s) for ", length(IAV_SITES), " site(s).")

daily0 <- daily_raw |>
  left_join(site_meta |> select(site_id, gpp_col_used), by = "site_id") |>
  mutate(
    is_vut = gpp_col_used == "GPP_NT_VUT_REF",
    GPP_NT_REF = ifelse(is_vut, GPP_NT_VUT_REF, GPP_NT_CUT_REF),
    GPP_DT_REF = ifelse(is_vut, GPP_DT_VUT_REF, GPP_DT_CUT_REF),
    NEE_REF    = ifelse(is_vut, NEE_VUT_REF, NEE_CUT_REF)
  ) |>
  select(site_id, TIMESTAMP, GPP_NT_REF, GPP_DT_REF, NEE_REF)

## ---- Winter screen (main run's rule, already computed and committed) --
## applied to GPP_NT_REF only, exactly as 02_derive.R's "main" variant. ----
daily0 <- daily0 |>
  left_join(winter_screen_days, by = c("site_id", "TIMESTAMP")) |>
  mutate(
    year = lubridate::year(TIMESTAMP),
    GPP_NT_REF_screened = ifelse(coalesce(is_screened, FALSE), 0, GPP_NT_REF)
  )

## ---- DT availability per site: a site "has no DT GPP" if GPP_DT_REF is
## NA for every row across its ENTIRE pulled record (not just passing
## years) -- the agreement variant then falls back to NT alone for that
## site, flagged. ----
dt_availability <- daily0 |> group_by(site_id) |>
  summarise(n_dt_nonNA = sum(!is.na(GPP_DT_REF)), .groups = "drop") |>
  mutate(has_dt = n_dt_nonNA > 0)
sites_no_dt <- dt_availability$site_id[!dt_availability$has_dt]
message(length(sites_no_dt), " of ", length(IAV_SITES), " site(s) have NO DT-partitioned GPP at all: ",
        if (length(sites_no_dt) > 0) paste(sites_no_dt, collapse = ", ") else "(none)")
if (length(sites_no_dt) > 0) {
  for (sid in sites_no_dt) {
    log_unknown(record_id = sid, reason = "No DT-partitioned GPP (GPP_DT_REF) at any resolution; agreement variant falls back to NT-only GPP for this site.",
                logged_by = "11_gsl_methods_compare.R", log_path = EXP_UNKNOWN_LOG)
  }
}

## ============================================================================
## Three GPP variants (zero-floored throughout) + NEP (same for all three,
## NOT zero-floored -- NEP is legitimately negative, needed by M5/M6).
##   screened   = winter-screened GPP_NT_REF, zero-floored (main run).
##   unscreened = GPP_NT_REF as-pulled (no winter screen), zero-floored.
##   agreement  = pmin(zero-floored screened-or-unscreened? ...) -- see
##     below: Dave's prompt defines it as pmin(zero-floored GPP_NT_REF,
##     zero-floored GPP_DT_REF) -- i.e. the UNSCREENED NT series (the winter
##     screen is explicitly a REPLACEMENT for this variant, so it uses the
##     as-delivered NT, not the already-screened NT) -- confirmed against
##     the prompt's own framing ("agreement... Dave's chosen REPLACEMENT for
##     the winter screen"): the two are alternative ways of handling the
##     same artifact, not stacked. Falls back to NT alone (no min taken)
##     where a site has no DT, per sites_no_dt above.
## ============================================================================
daily <- daily0 |>
  mutate(
    gpp_screened   = pmax(GPP_NT_REF_screened, 0),
    gpp_unscreened = pmax(GPP_NT_REF, 0),
    gpp_dt_floor   = pmax(GPP_DT_REF, 0),
    gpp_agreement  = ifelse(site_id %in% sites_no_dt, gpp_unscreened, pmin(gpp_unscreened, gpp_dt_floor)),
    nep            = -NEE_REF
  ) |>
  select(site_id, TIMESTAMP, year, gpp_screened, gpp_unscreened, gpp_agreement, nep)

message("Variant series built: screened, unscreened, agreement (= min(unscreened NT, DT), NT-only for ",
        length(sites_no_dt), " no-DT site(s)); NEP shared across all three.")

## ============================================================================
## Continuous per-site daily sequence (fill any calendar-day gaps with NA --
## confirmed none exist for these 15 sites, kept as a safety net) -- needed
## for 15-day smoothing across year boundaries using whatever neighbour-year
## data exists, even in a non-passing year (prompt's own instruction).
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
#' of the vector `x` (no padding/reflection) -- "use days from the
#' neighbouring year where data exist... otherwise use the partial window."
#' NA values within a window are dropped (na.rm), consistent with this
#' project's convention elsewhere of simple-mean-of-available-days.
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

## ---- Smooth gpp_screened/gpp_unscreened/gpp_agreement and nep, per site,
## over the FULL continuous record (so cross-year-boundary smoothing can
## see neighbour-year data regardless of that year's passing status). -----
daily_cont <- daily_cont |>
  group_by(site_id) |>
  arrange(TIMESTAMP, .by_group = TRUE) |>
  mutate(
    gpp_screened_sm   = roll_mean_centred(gpp_screened),
    gpp_unscreened_sm = roll_mean_centred(gpp_unscreened),
    gpp_agreement_sm  = roll_mean_centred(gpp_agreement),
    nep_sm            = roll_mean_centred(nep)
  ) |>
  ungroup()
message("15-day centred smoothing applied (gpp x3 variants, nep) over each site's full continuous record.")

## ============================================================================
## Per-site, per-variant P99 and threshold grids. "The main run's rule" for
## P99/GSL-counting (03_derive.R's derive_one()) pools CALENDAR-YEAR
## windows (year %in% passing years), NOT Jul-Jun season-year windows, for
## P99/skewness/GSL(T)/F(T) -- confirmed by reading derive_one() directly
## (it filters `year %in% years` on the literal calendar-year column).
## M2/M3/M4 therefore ALSO use calendar-year windows (needed to exactly
## reproduce derived_site_year.csv's gsl_z16/gsl_t95/f_z16/f_t95 for the
## correctness checks below); M1/M5/M6 (span/continuous-NEP methods) use
## Jul-Jun SEASON years for the one Southern-Hemisphere site (AU-How), per
## the prompt's explicit "Southern Hemisphere sites on Jul-Jun season
## years" instruction -- see docs/gsl_methods_compare_2026-11-02.md for the
## full reasoning; this is the single most consequential interpretation
## choice in this script, since the prompt does not resolve the tension
## between "same as the main run" (calendar-year, for P99/M2/M4) and "SH on
## season years" (for the span methods) explicitly itself.
## ============================================================================
calendar_pool <- function(sid, gcol) {
  yrs <- raw_years_list[[sid]]
  v <- daily_cont[[gcol]][daily_cont$site_id == sid & daily_cont$year %in% yrs]
  v[!is.na(v)]
}

P99_screened   <- setNames(sapply(IAV_SITES, function(s) as.numeric(quantile(calendar_pool(s, "gpp_screened"),   0.99, type = 7))), IAV_SITES)
P99_unscreened <- setNames(sapply(IAV_SITES, function(s) as.numeric(quantile(calendar_pool(s, "gpp_unscreened"), 0.99, type = 7))), IAV_SITES)
## Agreement variant reuses the UNSCREENED P99 (prompt: "thresholds ... stay
## as in the unscreened variant; only the series compared changes").
P99_agreement <- P99_unscreened

## ---- Sanity check: P99_screened/P99_unscreened must equal derived_site.csv's
## own P99 (variant main / main_unscreened) for these 15 sites, confirming
## the calendar-year-pooling interpretation above is correct. ---------------
chk_P99 <- derived_site_all |> filter(site_id %in% IAV_SITES, variant %in% c("main", "main_unscreened")) |>
  select(site_id, variant, P99) |>
  mutate(P99_fresh = ifelse(variant == "main", P99_screened[site_id], P99_unscreened[site_id]),
         diff = P99 - P99_fresh)
if (any(abs(chk_P99$diff) > 1e-9)) {
  print(chk_P99[abs(chk_P99$diff) > 1e-9, ])
  stop("11_gsl_methods_compare.R: freshly computed P99 does not match derived_site.csv -- stopping (per the prompt's own checks).")
}
message("Check passed: fresh P99 (screened & unscreened) exactly matches derived_site.csv for all 15 sites.")

## ============================================================================
## M2/M3/M4 (count/fraction methods): calendar-year windows throughout (see
## the note above). For "agreement", the series compared to the threshold
## is gpp_agreement, but the GPP summed for the numerator/denominator of
## "share of annual GPP" (and for the annual-GPP correlation check) is the
## UNSCREENED series -- consistent with "annual GPP for shares ... stay as
## in the unscreened variant" (documented interpretation: the agreement
## variant only changes WHICH days qualify, not which GPP values are
## totalled once they do).
## ============================================================================

#' One (site, calendar year, threshold) count-method result.
#' `thresh_vals` = the series compared against T (named by date);
#' `value_vals` = the series summed for GPP totals (same dates).
count_method_one <- function(dates, thresh_vals, value_vals, T_abs) {
  ok <- !is.na(thresh_vals) & !is.na(value_vals)
  above <- ok & thresh_vals > T_abs
  n <- sum(above, na.rm = TRUE)
  annual_gpp <- sum(value_vals, na.rm = TRUE)
  if (n == 0) {
    return(tibble(gsl = 0L, start_date = as.Date(NA), end_date = as.Date(NA),
                  gpp_in_season = 0, annual_gpp = annual_gpp, share = NA_real_,
                  flag_no_season = TRUE, flag_whole_year = FALSE))
  }
  idx_above <- which(above)
  gpp_in_season <- sum(value_vals[idx_above], na.rm = TRUE)
  tibble(gsl = n, start_date = dates[idx_above[1]], end_date = dates[idx_above[length(idx_above)]],
         gpp_in_season = gpp_in_season, annual_gpp = annual_gpp,
         share = if (annual_gpp != 0) gpp_in_season / annual_gpp else NA_real_,
         flag_no_season = FALSE, flag_whole_year = (n == length(dates)))
}

#' Run M2 (pct-of-P99 thresholds) and M4 (fine-grid fraction-target rule)
#' for one GPP variant, across all 15 sites x calendar years.
run_count_methods <- function(variant, thresh_col, value_col, P99_vec) {
  m2_pcts <- c(5, 10, 20)
  m4_targets <- c(0.90, 0.95, 0.99)
  m3_abs <- c(0.5, 1, 2) # gC m-2 d-1, fixed, no P99 scaling

  out <- list()
  for (sid in IAV_SITES) {
    d <- daily_cont |> filter(site_id == sid)
    P99 <- P99_vec[[sid]]
    yrs <- raw_years_list[[sid]]

    ## ---- M4's fine-grid rule: mean F(T) across ALL this site's passing
    ## calendar years, at every fine-grid point, exactly mirroring
    ## derive_one() (03_derive.R) but restricted to this script's own
    ## variant-specific gpp_by_year. -----------------------------------------
    gpp_by_year <- split(d[[value_col]][d$year %in% yrs], as.character(d$year[d$year %in% yrs]))
    thresh_by_year <- split(d[[thresh_col]][d$year %in% yrs], as.character(d$year[d$year %in% yrs]))
    ## year_threshold_matrices() needs ONE series for both GSL-counting and
    ## F-weighting; for screened/unscreened these are identical
    ## (thresh_col==value_col). For "agreement" they differ (thresh_col =
    ## gpp_agreement, value_col = gpp_unscreened) -- year_threshold_matrices()
    ## itself cannot take two series, so M4's fine-grid GSL/F for
    ## "agreement" is computed directly below (count_method_one(), looped
    ## over the fine grid) rather than via that shared helper.
    fine_abs <- FINE_GRID_PCT / 100 * P99
    f_mean_grid <- vapply(fine_abs, function(T_abs) {
      per_yr <- vapply(names(gpp_by_year), function(yr) {
        tv <- thresh_by_year[[yr]]; vv <- gpp_by_year[[yr]]
        ok <- !is.na(tv) & !is.na(vv); above <- ok & tv > T_abs
        tot <- sum(vv, na.rm = TRUE)
        if (tot == 0) return(NA_real_)
        sum(vv[above], na.rm = TRUE) / tot
      }, numeric(1))
      mean(per_yr, na.rm = TRUE)
    }, numeric(1))
    f_rounded <- round(f_mean_grid, 6)

    for (y in yrs) {
      win <- window_dates_calendar <- seq(as.Date(sprintf("%d-01-01", y)), as.Date(sprintf("%d-12-31", y)), by = "day")
      dy <- d |> filter(TIMESTAMP %in% win)
      thresh_vals <- dy[[thresh_col]][match(win, dy$TIMESTAMP)]
      value_vals  <- dy[[value_col]][match(win, dy$TIMESTAMP)]

      ## M2
      for (pct in m2_pcts) {
        T_abs <- pct / 100 * P99
        r <- count_method_one(win, thresh_vals, value_vals, T_abs)
        out[[length(out) + 1]] <- r |> mutate(site_id = sid, year = y, variant = variant,
                                                method = "M2", sub = sprintf("%dpct", pct))
      }
      ## M3 (fixed absolute thresholds, no P99 scaling)
      for (abs_t in m3_abs) {
        r <- count_method_one(win, thresh_vals, value_vals, abs_t)
        out[[length(out) + 1]] <- r |> mutate(site_id = sid, year = y, variant = variant,
                                                method = "M3", sub = sprintf("%sgC", format(abs_t)))
      }
      ## M4: pick the fine-grid threshold hitting each target fraction, then
      ## apply IT to this specific year.
      for (target in m4_targets) {
        idx <- which(f_rounded <= round(target, 6))[1]
        if (is.na(idx)) {
          r <- tibble(gsl = NA_integer_, start_date = as.Date(NA), end_date = as.Date(NA),
                      gpp_in_season = NA_real_, annual_gpp = sum(value_vals, na.rm = TRUE), share = NA_real_,
                      flag_no_season = TRUE, flag_whole_year = FALSE)
        } else {
          T_abs <- fine_abs[idx]
          r <- count_method_one(win, thresh_vals, value_vals, T_abs)
          attr(r, "threshold_pct") <- FINE_GRID_PCT[idx]
        }
        out[[length(out) + 1]] <- r |> mutate(site_id = sid, year = y, variant = variant,
                                                method = "M4", sub = sprintf("%.2f", target),
                                                threshold_pct_of_P99 = if (!is.na(idx)) FINE_GRID_PCT[idx] else NA_real_)
      }
    }
  }
  bind_rows(out)
}

message("Running M2/M3/M4 for 3 variants x 15 sites...")
m234_screened   <- run_count_methods("screened",   "gpp_screened",   "gpp_screened",   P99_screened)
m234_unscreened <- run_count_methods("unscreened", "gpp_unscreened", "gpp_unscreened", P99_unscreened)
m234_agreement  <- run_count_methods("agreement",  "gpp_agreement",  "gpp_unscreened", P99_agreement)
m234_all <- bind_rows(m234_screened, m234_unscreened, m234_agreement)
message("M2/M3/M4: ", nrow(m234_all), " site x year x variant x method-threshold rows.")

## ============================================================================
## Check 1: M2 at 10% and M4 at 0.95 must equal derived_site_year.csv's
## gsl_z16/gsl_t95 (and f_z16/f_t95), for the screened and unscreened
## variants.
## ============================================================================
dsy_check <- derived_site_year_all |> filter(site_id %in% IAV_SITES, variant %in% c("main", "main_unscreened")) |>
  select(site_id, year, variant, gsl_z16, gsl_t95, f_z16, f_t95) |>
  mutate(variant = ifelse(variant == "main", "screened", "unscreened"))

m2_10 <- m234_all |> filter(method == "M2", sub == "10pct") |> select(site_id, year, variant, gsl, share)
m4_95 <- m234_all |> filter(method == "M4", sub == "0.95") |> select(site_id, year, variant, gsl, share)

chk1 <- dsy_check |>
  left_join(m2_10 |> rename(gsl_m2_10 = gsl, share_m2_10 = share), by = c("site_id", "year", "variant")) |>
  left_join(m4_95 |> rename(gsl_m4_95 = gsl, share_m4_95 = share), by = c("site_id", "year", "variant")) |>
  mutate(
    diff_gsl_z16 = gsl_z16 - gsl_m2_10, diff_f_z16 = f_z16 - share_m2_10,
    diff_gsl_t95 = gsl_t95 - gsl_m4_95, diff_f_t95 = f_t95 - share_m4_95
  )
bad_gsl_z16 <- chk1 |> filter(!is.na(diff_gsl_z16) & abs(diff_gsl_z16) > 0)
bad_gsl_t95 <- chk1 |> filter(!is.na(diff_gsl_t95) & abs(diff_gsl_t95) > 0)
bad_f_z16   <- chk1 |> filter(!is.na(diff_f_z16) & abs(diff_f_z16) > 1e-9)
bad_f_t95   <- chk1 |> filter(!is.na(diff_f_t95) & abs(diff_f_t95) > 1e-9)
if (nrow(bad_gsl_z16) + nrow(bad_gsl_t95) + nrow(bad_f_z16) + nrow(bad_f_t95) > 0) {
  cat("--- M2@10%/M4@0.95 CHECK FAILURES ---\n")
  print(bad_gsl_z16); print(bad_gsl_t95); print(bad_f_z16); print(bad_f_t95)
  stop("11_gsl_methods_compare.R: M2@10%/M4@0.95 do not exactly match derived_site_year.csv's gsl_z16/gsl_t95/f_z16/f_t95 -- stopping.")
}
message("Check passed: M2@10% == gsl_z16/f_z16 and M4@0.95 == gsl_t95/f_t95, exactly, for screened & unscreened, all ",
        nrow(chk1), " site-year rows.")

## ============================================================================
## Annual GPP/NEP for the detrended-r CORRELATION checks and for the summary
## table's own r_gpp/r_nep -- NOT the same "annual_gpp" column used just
## above as F(T)'s denominator (that one is calendar-year, matching
## derived_site_year.csv's f_z16/f_t95 exactly, confirmed). This one follows
## carbon_gain_data.csv's / 09_figure_iav_15sites.R's own convention: NH =
## calendar-year sum; Southern Hemisphere (AU-How) = Jul(y)-Jun(y+1)
## SEASON-year sum, using window_dates() (already defined above) and only
## for AU-How's season_years_for() (Jul-Jun-valid) years -- needed to
## reproduce iav_15sites.csv's r_gpp_z16/r_gpp_t95 exactly for the check
## below, and used uniformly for every method's site-level correlation in
## the summary table (not just M2/M4).
## ============================================================================
annual_gpp_nep_corr_for <- function(sid, gpp_col) {
  yrs <- season_years_for(sid)
  purrr::map_dfr(yrs, function(y) {
    win <- window_dates(sid, y)
    d <- daily_cont |> filter(site_id == sid, TIMESTAMP %in% win)
    tibble(site_id = sid, year = y, annual_gpp = sum(d[[gpp_col]], na.rm = TRUE),
           annual_nep = sum(d$nep, na.rm = TRUE))
  })
}
annual_corr_screened   <- bind_rows(lapply(IAV_SITES, annual_gpp_nep_corr_for, gpp_col = "gpp_screened"))
annual_corr_unscreened <- bind_rows(lapply(IAV_SITES, annual_gpp_nep_corr_for, gpp_col = "gpp_unscreened"))
## Agreement's annual_gpp/nep for correlations = unscreened's (prompt rule).
annual_corr_agreement  <- annual_corr_unscreened

detrend_cor <- function(gsl, annual, year) {
  ok <- !is.na(gsl) & !is.na(annual) & !is.na(year)
  if (sum(ok) < 4) return(c(r = NA_real_, p = NA_real_, n = sum(ok)))
  g <- gsl[ok]; a <- annual[ok]; yv <- year[ok]
  rg <- residuals(lm(g ~ yv)); ra <- residuals(lm(a ~ yv))
  ct <- cor.test(rg, ra)
  c(r = unname(ct$estimate), p = ct$p.value, n = sum(ok))
}

## ---- Check 2: screened-variant M2@10%/M4@0.95 detrended r vs annual GPP
## must equal iav_15sites.csv's r_gpp_z16/r_gpp_t95, per site. --------------
chk2_data <- derived_site_year_all |> filter(site_id %in% IAV_SITES, variant == "main") |>
  select(site_id, year, gsl_z16, gsl_t95) |>
  inner_join(annual_corr_screened, by = c("site_id", "year"))

chk2 <- chk2_data |> group_by(site_id) |> summarise(
  r_m2_10 = detrend_cor(gsl_z16, annual_gpp, year)["r"],
  r_m4_95 = detrend_cor(gsl_t95, annual_gpp, year)["r"],
  .groups = "drop"
) |> left_join(iav_tbl |> select(site_id, r_gpp_z16, r_gpp_t95), by = "site_id") |>
  mutate(diff_z16 = r_m2_10 - r_gpp_z16, diff_t95 = r_m4_95 - r_gpp_t95)

bad2 <- chk2 |> filter(abs(diff_z16) > 1e-9 | abs(diff_t95) > 1e-9)
if (nrow(bad2) > 0) {
  print(chk2)
  stop("11_gsl_methods_compare.R: screened-variant detrended r (M2@10%/M4@0.95 vs annual GPP) does not match iav_15sites.csv -- stopping.")
}
message("Check passed: screened-variant detrended r (M2@10% and M4@0.95 vs annual GPP) exactly matches iav_15sites.csv's r_gpp_z16/r_gpp_t95 for all 15 sites.")

## ============================================================================
## M1 (Zscheischler et al. 2016 crossing-span). Three thresholds, all 10% of
## a P99: summer (Jun-Aug NH / Dec-Feb SH, pooled across passing CALENDAR
## years), peak (the 3 consecutive wrapping calendar months with the
## highest mean GPP, pooled the same way), annual (the same per-variant P99
## used throughout M2/M4 above). M1 ITSELF (the span detection) uses
## SEASON-YEAR windows (Jul-Jun for AU-How) -- the one place in this script
## where "Southern Hemisphere sites on Jul-Jun season years" governs a
## threshold-crossing computation rather than a simple calendar-year count.
## ============================================================================
summer_months <- function(sid) if (is_sh(sid)) c(12, 1, 2) else c(6, 7, 8)

#' The 3 consecutive (wrapping) calendar months with the highest mean daily
#' GPP, pooled over this site's passing CALENDAR years, for column `gcol`.
peak_months_for <- function(sid, gcol) {
  yrs <- raw_years_list[[sid]]
  d <- daily_cont |> filter(site_id == sid, year %in% yrs)
  d$month <- lubridate::month(d$TIMESTAMP)
  month_mean <- vapply(1:12, function(m) mean(d[[gcol]][d$month == m], na.rm = TRUE), numeric(1))
  starts <- 1:12
  score <- vapply(starts, function(s) sum(month_mean[((s - 1):(s + 1)) %% 12 + 1]), numeric(1))
  best_start <- starts[which.max(score)]
  ((best_start - 1):(best_start + 1)) %% 12 + 1
}

#' P99 of zero-floored GPP (`gcol`), pooled over this site's passing
#' calendar years, restricted to calendar months in `months`.
P99_for_months <- function(sid, gcol, months) {
  yrs <- raw_years_list[[sid]]
  d <- daily_cont |> filter(site_id == sid, year %in% yrs)
  d$month <- lubridate::month(d$TIMESTAMP)
  v <- d[[gcol]][d$month %in% months]
  as.numeric(quantile(v, 0.99, type = 7, na.rm = TRUE))
}

peak_months_screened   <- setNames(lapply(IAV_SITES, peak_months_for, gcol = "gpp_screened"), IAV_SITES)
peak_months_unscreened <- setNames(lapply(IAV_SITES, peak_months_for, gcol = "gpp_unscreened"), IAV_SITES)
peak_months_agreement  <- peak_months_unscreened # agreement reuses unscreened's threshold machinery

P99_summer_screened   <- setNames(sapply(IAV_SITES, function(s) P99_for_months(s, "gpp_screened",   summer_months(s))), IAV_SITES)
P99_summer_unscreened <- setNames(sapply(IAV_SITES, function(s) P99_for_months(s, "gpp_unscreened", summer_months(s))), IAV_SITES)
P99_summer_agreement  <- P99_summer_unscreened

P99_peak_screened   <- setNames(sapply(IAV_SITES, function(s) P99_for_months(s, "gpp_screened",   peak_months_screened[[s]])), IAV_SITES)
P99_peak_unscreened <- setNames(sapply(IAV_SITES, function(s) P99_for_months(s, "gpp_unscreened", peak_months_unscreened[[s]])), IAV_SITES)
P99_peak_agreement  <- P99_peak_unscreened

message("Peak 3-month window per site (unscreened GPP): ",
        paste(sprintf("%s=%s", IAV_SITES, sapply(peak_months_unscreened, paste, collapse = "-")), collapse = "; "))

#' One (site, season/calendar year, threshold) span-crossing result on a
#' SMOOTHED series. `thresh_vals`/`value_vals` aligned to `dates`.
crossing_method_one <- function(dates, thresh_vals, value_vals, T_abs) {
  above <- !is.na(thresh_vals) & thresh_vals > T_abs
  n <- length(above)
  annual_gpp <- sum(value_vals, na.rm = TRUE)
  if (!any(above)) {
    return(tibble(gsl = NA_integer_, start_date = as.Date(NA), end_date = as.Date(NA),
                  gpp_in_season = 0, annual_gpp = annual_gpp, share = NA_real_,
                  flag_no_season = TRUE, flag_whole_year = FALSE,
                  flag_no_start_crossing = FALSE, flag_no_end_crossing = FALSE))
  }
  idx_above <- which(above)
  s_idx <- idx_above[1]; e_idx <- idx_above[length(idx_above)]
  gsl <- e_idx - s_idx + 1L
  gpp_in_season <- sum(value_vals[s_idx:e_idx], na.rm = TRUE)
  tibble(gsl = gsl, start_date = dates[s_idx], end_date = dates[e_idx],
         gpp_in_season = gpp_in_season, annual_gpp = annual_gpp,
         share = if (annual_gpp != 0) gpp_in_season / annual_gpp else NA_real_,
         flag_no_season = FALSE, flag_whole_year = all(above),
         flag_no_start_crossing = (s_idx == 1), flag_no_end_crossing = (e_idx == n))
}

run_m1 <- function(variant, thresh_col_sm, value_col, P99_annual_vec, P99_summer_vec, P99_peak_vec, annual_corr_tbl) {
  out <- list()
  for (sid in IAV_SITES) {
    yrs <- season_years_for(sid)
    d <- daily_cont |> filter(site_id == sid)
    subs <- list(summer = P99_summer_vec[[sid]], peak = P99_peak_vec[[sid]], annual = P99_annual_vec[[sid]])
    for (y in yrs) {
      win <- window_dates(sid, y)
      dy <- d |> filter(TIMESTAMP %in% win)
      thresh_vals <- dy[[thresh_col_sm]][match(win, dy$TIMESTAMP)]
      value_vals  <- dy[[value_col]][match(win, dy$TIMESTAMP)]
      ## use the SAME window's annual GPP as the correlation table (season-
      ## year-aware for AU-How) rather than re-summing here, for consistency.
      ag <- annual_corr_tbl$annual_gpp[annual_corr_tbl$site_id == sid & annual_corr_tbl$year == y]
      for (sub in names(subs)) {
        T_abs <- 0.10 * subs[[sub]]
        r <- crossing_method_one(win, thresh_vals, value_vals, T_abs)
        if (length(ag) == 1) r$annual_gpp <- ag # override with the season-year-aware total
        r$share <- if (!is.na(r$annual_gpp) && r$annual_gpp != 0) r$gpp_in_season / r$annual_gpp else NA_real_
        out[[length(out) + 1]] <- r |> mutate(site_id = sid, year = y, variant = variant, method = "M1", sub = sub)
      }
    }
  }
  bind_rows(out)
}

message("Running M1 for 3 variants x 3 thresholds x 15 sites...")
m1_screened   <- run_m1("screened",   "gpp_screened_sm",   "gpp_screened",   P99_screened,   P99_summer_screened,   P99_peak_screened,   annual_corr_screened)
m1_unscreened <- run_m1("unscreened", "gpp_unscreened_sm", "gpp_unscreened", P99_unscreened, P99_summer_unscreened, P99_peak_unscreened, annual_corr_unscreened)
m1_agreement  <- run_m1("agreement",  "gpp_agreement_sm",  "gpp_unscreened", P99_agreement,  P99_summer_agreement,  P99_peak_agreement,  annual_corr_agreement)
m1_all <- bind_rows(m1_screened, m1_unscreened, m1_agreement)
message("M1: ", nrow(m1_all), " site x year x variant x sub-threshold rows. No-season flags: ", sum(m1_all$flag_no_season),
        "; whole-year flags: ", sum(m1_all$flag_whole_year))

## ============================================================================
## NEP is the SAME across all three GPP variants, so M5/M6 are computed
## ONCE per site-year (not x3) -- the output tables still carry a
## `variant` value for these rows (set to "all" below) so every row in
## gsl_methods_site_year.csv has a variant, per the output spec, without
## implying NEP actually differs by GPP variant.
## ============================================================================

#' Two-season check shared by M5-span and M6 (prompt: "for M5-span and M6,
## use the first rise and the last decline... flag every site-year with
## more than one distinct rise or decline (separate runs where the slope
## exceeds 50% of that year's maximum)"). Slope = first difference of
## SMOOTHED NEP (the simplest series available to both methods without
## picking one of M6's own k=3-9 windows) -- an explicit, documented
## interpretation choice (the prompt does not itself define "slope" for
## this shared check). Rise-runs: slope > 0.5*max(slope); decline-runs:
## slope < 0.5*min(slope) (most negative half), each within one year window.
two_season_check <- function(nep_sm_vals) {
  slope <- diff(nep_sm_vals) # length n-1, slope[i] = nep_sm[i+1]-nep_sm[i]
  if (all(is.na(slope))) return(list(n_rise = 0L, n_decline = 0L))
  mx <- suppressWarnings(max(slope, na.rm = TRUE)); mn <- suppressWarnings(min(slope, na.rm = TRUE))
  rise  <- !is.na(slope) & mx > 0 & slope > 0.5 * mx
  decl  <- !is.na(slope) & mn < 0 & slope < 0.5 * mn
  count_runs <- function(x) { if (!any(x)) return(0L); r <- rle(x); sum(r$values) }
  list(n_rise = count_runs(rise), n_decline = count_runs(decl))
}

#' M5: carbon-uptake period from smoothed NEP > 0. count = n days > 0;
## span = first day of the first >=5-day run to the last day of the last
## such run. Net-source flag: fewer than 5 days with smoothed NEP > 0
## (excluded from M5/M6 site means downstream, per the prompt).
run_m5 <- function() {
  out <- list()
  for (sid in IAV_SITES) {
    yrs <- season_years_for(sid)
    d <- daily_cont |> filter(site_id == sid)
    for (y in yrs) {
      win <- window_dates(sid, y)
      dy <- d |> filter(TIMESTAMP %in% win)
      nep_sm <- dy$nep_sm[match(win, dy$TIMESTAMP)]
      above <- !is.na(nep_sm) & nep_sm > 0
      n_above <- sum(above)
      net_source <- n_above < 5L

      ts <- two_season_check(nep_sm)

      ## M5-count
      count_row <- tibble(gsl = n_above, start_date = if (n_above > 0) win[which(above)[1]] else as.Date(NA),
                           end_date = if (n_above > 0) win[which(above)[n_above]] else as.Date(NA),
                           sub = "count")

      ## M5-span: runs of >=5 consecutive TRUE
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
        rowd$flag_two_season_rise <- ts$n_rise > 1
        rowd$flag_two_season_decline <- ts$n_decline > 1
        rowd$n_rise_runs <- ts$n_rise
        rowd$n_decline_runs <- ts$n_decline
        out[[length(out) + 1]] <- rowd |> mutate(site_id = sid, year = y, variant = "all", method = "M5")
      }
    }
  }
  bind_rows(out)
}

message("Running M5 (count + span) for 15 sites...")
m5_all <- run_m5()
message("M5: ", nrow(m5_all), " site x year x sub rows. Net-source years: ", sum(m5_all$flag_net_source[m5_all$sub == "span"]),
        " of ", sum(m5_all$sub == "span"), ". Two-season rise flags: ", sum(m5_all$flag_two_season_rise[m5_all$sub == "span"]),
        "; decline flags: ", sum(m5_all$flag_two_season_decline[m5_all$sub == "span"]))

## ============================================================================
## M6 (Hu et al. 2010 rate-of-change of NEP), on UNSMOOTHED daily NEP (an
## agreed exception to the shared 15-day smoothing). Interpretation choices
## made explicit here (see docs/gsl_methods_compare_2026-11-02.md for the
## full reasoning and the empirical finding that motivated them):
##
## 1. "First/last seasonal peak of smoothed NEP": tested several local-
##    maxima + prominence/valley definitions (0.5x-of-max threshold, with
##    and without a minimum-distance or saddle-depth requirement) and found
##    all of them flagged the large majority of site-years as having
##    "multiple peaks" -- smoothed NEP (unlike smoothed GPP) is not reliably
##    unimodal; short dry spells/cloudy spells create many local maxima
##    that satisfy any reasonably permissive prominence threshold. Rather
##    than report a "first vs last peak" distinction that is mostly noise,
##    this script uses the SINGLE GLOBAL MAXIMUM of smoothed NEP in the
##    window as both "first" and "last" seasonal peak -- a simplification,
##    not a claim that these sites are all strictly unimodal.
## 2. The k-day OLS slope is computed on UNSMOOTHED daily NEP, which has
##    substantial day-to-day weather-driven scatter (a cloudy day
##    immediately before/after a sunny one can swing NEP several gC/m2/d).
##    Because the Start search window spans from 30 days before the first
##    >=5-day NEP>0 run to the (single) seasonal peak -- often 2-3 months --
##    the steepest k-day slope within that window is sometimes a short
##    weather-driven swing well after the true spring green-up, not the
##    green-up itself. This is reported, not hidden -- see the k-range
##    ("does k matter") summary below and the methods doc.
## ============================================================================

#' Centred k-day OLS slope of `y` on day, at every index of a full
#' continuous vector; NA near either true record edge (not partial -- a
#' slope needs the full k points, unlike the smoothing mean above).
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
#' (within `win`), the alternate end, and the (single) seasonal peak.
m6_one_k <- function(win, nep_raw_win, nep_sm_win, k, first_rise_idx) {
  n <- length(win)
  slope <- slope_at(nep_raw_win, k)
  peak_idx <- which.max(nep_sm_win) # single global max -- see note above

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
    if (!all(is.na(neg3))) end_idx <- steep_idx - 1L + which.max(neg3) # shallowest (closest to 0) negative slope
  }

  below0b <- which(nep_sm_win[peak_idx:n] < 0)
  zero_idx_b <- if (length(below0b) > 0) peak_idx - 1L + below0b[1] else n
  segb <- slope[peak_idx:zero_idx_b]
  negb <- segb; negb[!is.na(negb) & negb >= 0] <- NA
  end_alt_idx <- if (all(is.na(negb))) NA_integer_ else peak_idx - 1L + which.max(negb)

  list(start = start_idx, end = end_idx, end_alt = end_alt_idx, peak = peak_idx)
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
      nep_sm_win  <- dy$nep_sm[match(win, dy$TIMESTAMP)]

      first_rise_date <- m5_all$start_date[m5_all$site_id == sid & m5_all$year == y & m5_all$sub == "span"]
      net_source <- isTRUE(m5_all$flag_net_source[m5_all$site_id == sid & m5_all$year == y & m5_all$sub == "span"][1])
      if (length(first_rise_date) != 1 || is.na(first_rise_date)) {
        out[[length(out) + 1]] <- tibble(site_id = sid, year = y, variant = "all", method = "M6", sub = "span",
                                           gsl = NA_integer_, start_date = as.Date(NA), end_date = as.Date(NA),
                                           start_date_alt_end = as.Date(NA), k_range_start_days = NA_integer_,
                                           k_range_end_days = NA_integer_, flag_no_season = TRUE, flag_net_source = net_source,
                                           flag_two_season_rise = isTRUE(m5_all$flag_two_season_rise[m5_all$site_id == sid & m5_all$year == y & m5_all$sub == "span"][1]),
                                           flag_two_season_decline = isTRUE(m5_all$flag_two_season_decline[m5_all$site_id == sid & m5_all$year == y & m5_all$sub == "span"][1]))
        next
      }
      first_rise_idx <- which(win == first_rise_date)[1]

      starts <- integer(0); ends <- integer(0); ends_alt <- integer(0)
      for (k in 3:9) {
        r <- m6_one_k(win, nep_raw_win, nep_sm_win, k, first_rise_idx)
        if (!is.na(r$start)) starts <- c(starts, r$start)
        if (!is.na(r$end)) ends <- c(ends, r$end)
        if (!is.na(r$end_alt)) ends_alt <- c(ends_alt, r$end_alt)
      }
      no_season <- length(starts) == 0 || length(ends) == 0
      start_med <- if (length(starts) > 0) win[round(median(starts))] else as.Date(NA)
      end_med   <- if (length(ends) > 0) win[round(median(ends))] else as.Date(NA)
      end_alt_med <- if (length(ends_alt) > 0) win[round(median(ends_alt))] else as.Date(NA)
      gsl <- if (!no_season) as.integer(end_med - start_med + 1L) else NA_integer_

      out[[length(out) + 1]] <- tibble(
        site_id = sid, year = y, variant = "all", method = "M6", sub = "span",
        gsl = gsl, start_date = start_med, end_date = end_med, start_date_alt_end = end_alt_med,
        k_range_start_days = if (length(starts) > 1) max(starts) - min(starts) else 0L,
        k_range_end_days = if (length(ends) > 1) max(ends) - min(ends) else 0L,
        flag_no_season = no_season, flag_net_source = net_source,
        flag_two_season_rise = isTRUE(m5_all$flag_two_season_rise[m5_all$site_id == sid & m5_all$year == y & m5_all$sub == "span"][1]),
        flag_two_season_decline = isTRUE(m5_all$flag_two_season_decline[m5_all$site_id == sid & m5_all$year == y & m5_all$sub == "span"][1])
      )
    }
  }
  bind_rows(out)
}

message("Running M6 (k=3-9 slopes) for 15 sites -- this is the slowest step...")
m6_all <- run_m6()
message("M6: ", nrow(m6_all), " site-year rows. No-season flags: ", sum(m6_all$flag_no_season),
        ". Median k-range (start): ", median(m6_all$k_range_start_days, na.rm = TRUE), " days; (end): ",
        median(m6_all$k_range_end_days, na.rm = TRUE), " days. Max k-range (start): ", max(m6_all$k_range_start_days, na.rm = TRUE),
        "; (end): ", max(m6_all$k_range_end_days, na.rm = TRUE))
message("Alternate-end (search from last peak directly) vs primary end, median |difference| (days): ",
        median(abs(as.numeric(m6_all$start_date_alt_end - m6_all$end_date)), na.rm = TRUE))

## ============================================================================
## M5/M6 have no GPP "variant" of their own (NEP is shared) -- "share of
## annual GPP inside the season" for these two methods uses the SCREENED
## (main-run) GPP series as the single default, consistent with
## annual_corr_screened's own convention (season-year-aware for AU-How).
## ============================================================================
gpp_share_for_span <- function(sid, start_date, end_date, y) {
  if (is.na(start_date) || is.na(end_date)) return(c(gpp_in_season = NA_real_, annual_gpp = NA_real_, share = NA_real_))
  d <- daily_cont |> filter(site_id == sid, TIMESTAMP >= start_date, TIMESTAMP <= end_date)
  gpp_in_season <- sum(d$gpp_screened, na.rm = TRUE)
  ag <- annual_corr_screened$annual_gpp[annual_corr_screened$site_id == sid & annual_corr_screened$year == y]
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
message("Added share-of-annual-GPP (screened-variant GPP, season-year-aware) to M5/M6 rows.")

## ============================================================================
## Assemble the unified site-year table (one row per site x year x variant
## x method x sub). Harmonise columns across all 6 methods' outputs into
## one superset schema; unused flags/fields are NA/FALSE for a given row.
## ============================================================================
flag_cols <- c("flag_no_season", "flag_whole_year", "flag_net_source", "flag_two_season_rise",
               "flag_two_season_decline", "flag_no_start_crossing", "flag_no_end_crossing")
extra_cols <- c("threshold_pct_of_P99", "k_range_start_days", "k_range_end_days", "start_date_alt_end",
                "n_rise_runs", "n_decline_runs")

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
## Every row must have either a non-NA gsl OR a TRUE no-season/net-source
## flag (prompt's own check: "every site-year has a value or a flag for
## every method").
unaccounted <- gsl_methods_site_year |> filter(is.na(gsl) & !flag_no_season & !flag_net_source)
if (nrow(unaccounted) > 0) {
  print(unaccounted)
  stop("11_gsl_methods_compare.R: ", nrow(unaccounted), " row(s) have neither a GSL value nor a no-season/net-source flag -- stopping.")
}
message("Check passed: every row has a GSL value or an explanatory flag (", nrow(unaccounted), " unaccounted rows).")

## ============================================================================
## Per-site summary (mean GSL, interannual SD, mean start/end, flag counts,
## detrended r vs annual GPP/NEP). The detrended-r reference annual GPP/NEP
## is ALWAYS the season-year-aware annual_corr_* table (never the calendar-
## year "annual_gpp" column carried in gsl_methods_site_year, which exists
## only as F(T)'s own denominator for M2/M4) -- screened for screened-
## variant rows AND for M5/M6 (which have no GPP variant of their own, see
## the share-of-GPP note above); unscreened for unscreened; unscreened
## (reused) for agreement.
## Net-source years (flag_net_source) are EXCLUDED from M5/M6 site means
## and from their own detrended-r (prompt's own instruction); they are
## NOT excluded from M1-M4 means (the flag does not apply to those rows).
## ============================================================================
annual_corr_by_variant <- list(screened = annual_corr_screened, unscreened = annual_corr_unscreened,
                                 agreement = annual_corr_agreement, all = annual_corr_screened)

mean_doy <- function(dates) {
  ok <- dates[!is.na(dates)]
  if (length(ok) == 0) return(NA_real_)
  mean(lubridate::yday(ok))
}

summarise_group <- function(g) {
  sid <- g$site_id[1]; variant <- g$variant[1]
  ac <- annual_corr_by_variant[[variant]]
  gj <- g |> filter(!flag_net_source) |> # no-op unless method is M5/M6, but harmless elsewhere (flag_net_source is always FALSE for M1-M4)
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
    n_flag_two_season_rise = sum(g$flag_two_season_rise, na.rm = TRUE),
    n_flag_two_season_decline = sum(g$flag_two_season_decline, na.rm = TRUE),
    n_flag_no_start_crossing = sum(g$flag_no_start_crossing, na.rm = TRUE),
    n_flag_no_end_crossing = sum(g$flag_no_end_crossing, na.rm = TRUE),
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
## Net-source and no-DT exclusions/unknowns -> this side project's own logs
## (never the repo-root outputs/ logs).
## ============================================================================
net_source_rows <- gsl_methods_site_year |> filter(method %in% c("M5", "M6"), flag_net_source)
for (i in seq_len(nrow(net_source_rows))) {
  log_exclusion(site_id = net_source_rows$site_id[i], variable = paste0(net_source_rows$method[i], "-", net_source_rows$sub[i]),
                timestamp = net_source_rows$year[i], reason = "Net-source year: fewer than 5 days of smoothed NEP > 0; excluded from M5/M6 site means and detrended-r",
                threshold = "5 days", excluded_by = "11_gsl_methods_compare.R", log_path = EXP_EXCLUSION_LOG)
}
no_season_rows <- gsl_methods_site_year |> filter(flag_no_season, !flag_net_source)
for (i in seq_len(nrow(no_season_rows))) {
  log_unknown(record_id = sprintf("%s-%d-%s-%s-%s", no_season_rows$site_id[i], no_season_rows$year[i],
                                    no_season_rows$variant[i], no_season_rows$method[i], no_season_rows$sub[i]),
              reason = "No season detected (no day/run above threshold within this method's window)",
              logged_by = "11_gsl_methods_compare.R", log_path = EXP_UNKNOWN_LOG)
}
message("Logged ", nrow(net_source_rows), " net-source exclusion(s) and ", nrow(no_season_rows), " no-season unknown(s) to ",
        EXP_EXCLUSION_LOG, " / ", EXP_UNKNOWN_LOG, ".")

## ============================================================================
## Write output tables.
## ============================================================================
write_csv(gsl_methods_site_year, file.path(EXP_TABLES, "gsl_methods_site_year.csv"))
write_meta(
  file.path(EXP_TABLES, "gsl_methods_site_year.csv"),
  input_sources = c("data/duckdb/fluxnet.duckdb (daily, dataset='FLUXMET')", file.path(EXP_TABLES, "sites.csv"),
                      file.path(EXP_TABLES, "site_years.csv"), file.path(EXP_TABLES, "winter_screen_days.csv"),
                      file.path(EXP_TABLES, "derived_site.csv"), file.path(EXP_TABLES, "derived_site_year.csv")),
  notes = paste0(
    "Preliminary comparison of 6 published GSL methods (M1-M6) at the 15 'Figure GSL-IAV' sites. ",
    "M1 = Zscheischler et al. (2016) crossing-span (summer/peak/annual P99 thresholds, all at 10%); ",
    "M2 = day count > X% of P99 (X=5/10/20 -- 10% is this project's Z16); M3 = day count > fixed GPP ",
    "(0.5/1/2 gC m-2 d-1); M4 = cumulative-GPP-fraction fine-grid rule (0.90/0.95/0.99 -- 0.95 is T95); ",
    "M5 = carbon-uptake period from smoothed NEP>0 (count/span); M6 = Hu et al. (2010) NEP rate-of-",
    "change (k=3-9 day OLS slopes on UNSMOOTHED NEP, start/end = median across k, see ",
    "docs/gsl_methods_compare_2026-11-02.md for k_range_start_days/k_range_end_days and the found ",
    "noise-sensitivity). 3 GPP variants (screened/unscreened/agreement = min of zero-floored NT and ",
    "DT GPP) for M1-M4; M5/M6 use NEP only (variant='all'), with GPP-share computed from the screened ",
    "variant as a single default. M2/M3/M4 use CALENDAR-year windows throughout (matches ",
    "derived_site_year.csv exactly, checked); M1/M5/M6 use Jul-Jun SEASON years for AU-How (the one ",
    "Southern-Hemisphere site). All checks in the prompt passed exactly (see SESSION_LOG.md and the ",
    "methods doc): M2@10%/M4@0.95 == derived_site_year.csv's gsl_z16/gsl_t95/f_z16/f_t95 for screened ",
    "and unscreened; screened-variant detrended r vs annual GPP == iav_15sites.csv's r_gpp_z16/r_gpp_t95."
  )
)

write_csv(gsl_methods_site_summary, file.path(EXP_TABLES, "gsl_methods_site_summary.csv"))
write_meta(
  file.path(EXP_TABLES, "gsl_methods_site_summary.csv"),
  input_sources = file.path(EXP_TABLES, "gsl_methods_site_year.csv"),
  notes = paste0(
    "Per site x variant x method x sub-threshold: mean GSL, interannual SD, mean start/end day-of-",
    "window, mean share of annual GPP inside the season, flag counts, and detrended Pearson r (with p, ",
    "n) of annual GSL against annual GPP and against annual NEP -- each series detrended by OLS on ",
    "year first, as in iav_15sites.csv. Net-source years (flag_net_source) are excluded from M5/M6 ",
    "means and their own r (prompt's instruction); not excluded elsewhere (the flag never applies to ",
    "M1-M4 rows). Annual GPP/NEP reference for every r is the season-year-aware total (Jul-Jun for ",
    "AU-How), matching carbon_gain_data.csv's / iav_15sites.csv's own convention -- NOT the calendar-",
    "year 'annual_gpp' column in gsl_methods_site_year.csv (that one is only F(T)'s own denominator)."
  )
)
message("Saved: gsl_methods_site_year.csv (", nrow(gsl_methods_site_year), " rows), gsl_methods_site_summary.csv (",
        nrow(gsl_methods_site_summary), " rows).")

## ============================================================================
## Figures (agreement variant throughout; one sub-variant per method, per
## the prompt: M1-summer, M2 10%, M3 1 gC, M4 0.95, M5-span, M6). -----------
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
  filter(variant %in% c("agreement", "all")) |>
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
  labs(x = "Mean GSL (days, agreement variant; +/- interannual SD)", y = NULL,
       title = "GSL methods dotplot (draft)") +
  theme_gsl() + theme(legend.position = "right")

ggsave(file.path(drafts_dir, "GSL methods dotplot draft.png"), p_dotplot, width = PEERJ_FIG_WIDTH_MM * 1.3,
       height = 150, units = "mm", dpi = PEERJ_DPI, device = ragg::agg_png, bg = "white")
writeLines(c(
  "GSL methods dotplot (draft). Mean GSL (agreement variant) with interannual SD, by method, for each",
  "of the 15 sites, sorted bottom-to-top by ascending skewness (main-variant). One sub-threshold per",
  "method: M1-summer, M2 10% of P99 (= Z16), M3 1 gC m-2 d-1, M4 0.95 of annual GPP (= T95), M5-span",
  "(NEP>0, >=5-day runs), M6 (Hu et al. 2010 NEP rate-of-change, median across k=3-9). Net-source",
  "years excluded from M5/M6's own mean/SD (see tables/gsl_methods_site_summary.csv).",
  "Numbers behind this figure: tables/gsl_methods_site_summary.csv."
), file.path(drafts_dir, "GSL methods dotplot draft.legend.txt"))
write_meta(
  file.path(drafts_dir, "GSL methods dotplot draft.png"),
  input_sources = file.path(EXP_TABLES, "gsl_methods_site_summary.csv"),
  notes = "Draft figure, not a numbered manuscript figure. Agreement variant; one sub-threshold per method."
)
message("Saved: 'GSL methods dotplot draft'.png/.legend.txt/.meta.json")

## ============================================================================
## Figure B: method agreement -- Spearman correlation between methods'
## annual GSL (and separately start, end), pooled across sites as SITE-YEAR
## ANOMALIES (value minus that site's own across-year mean), so sites with
## systematically different absolute GSL don't dominate/confound the
## cross-method agreement signal.
## ============================================================================
wide_base <- gsl_methods_site_year |>
  inner_join(FIG_METHODS, by = c("method", "sub")) |>
  filter(variant %in% c("agreement", "all")) |>
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
  labs(x = NULL, y = NULL, title = "GSL methods agreement (draft)") +
  theme_gsl() + theme(axis.text.x = element_text(angle = 40, hjust = 1), legend.position = "right")

ggsave(file.path(drafts_dir, "GSL methods agreement draft.png"), p_agree, width = PEERJ_FIG_WIDTH_MM * 1.8,
       height = 130, units = "mm", dpi = PEERJ_DPI, device = ragg::agg_png, bg = "white")
writeLines(c(
  "GSL methods agreement (draft). Spearman correlation between pairs of methods' annual GSL (left",
  "panel), start date (middle) and end date (right), pooled across all 15 sites x years as SITE-YEAR",
  "ANOMALIES (each site's own across-year mean subtracted first, per method, before pooling) --",
  "agreement variant for M1-M4, NEP-only ('all') for M5/M6. One sub-threshold per method: M1-summer,",
  "M2 10%, M3 1 gC m-2 d-1, M4 0.95, M5-span, M6. Pairwise-complete Spearman correlation (a site-year",
  "flagged for one method but not another drops from that pair only, not from every pair).",
  "Numbers behind this figure: tables/gsl_methods_site_year.csv."
), file.path(drafts_dir, "GSL methods agreement draft.legend.txt"))
write_meta(
  file.path(drafts_dir, "GSL methods agreement draft.png"),
  input_sources = file.path(EXP_TABLES, "gsl_methods_site_year.csv"),
  notes = "Draft figure, not a numbered manuscript figure. Site-year anomalies (site's own mean subtracted), pairwise-complete Spearman correlation."
)
message("Saved: 'GSL methods agreement draft'.png/.legend.txt/.meta.json")

## ============================================================================
## Figure C: "raster" -- US-Ha1, US-Wkg, US-SRM, years (y) by day-of-year
## (x), the detected season (start-end) as one horizontal bar per site-year,
## one row BLOCK per method (M1-summer, M2 10%, M4 0.95, M5-span, M6).
## Implemented as segments (start_doy to end_doy per year) rather than a
## filled day-by-day grid -- equivalent information, much simpler to get
## right, and all 3 sites are Northern-Hemisphere so day-of-year is
## unambiguous (no SH season-year axis issue here).
## ============================================================================
RASTER_SITES <- c("US-Ha1", "US-Wkg", "US-SRM")
RASTER_METHODS <- FIG_METHODS |> filter(!(method == "M3"))

raster_data <- gsl_methods_site_year |>
  filter(site_id %in% RASTER_SITES, variant %in% c("agreement", "all")) |>
  inner_join(RASTER_METHODS, by = c("method", "sub")) |>
  mutate(start_doy = lubridate::yday(start_date), end_doy = lubridate::yday(end_date),
         label = factor(label, levels = RASTER_METHODS$label), site_id = factor(site_id, levels = RASTER_SITES))

p_raster <- ggplot(raster_data) +
  geom_segment(aes(x = start_doy, xend = end_doy, y = year, yend = year), colour = "grey20",
               linewidth = nature_lwd(1.2), na.rm = TRUE) +
  facet_grid(label ~ site_id, scales = "free_y") +
  labs(x = "Day of year", y = "Year", title = "GSL methods raster (draft)") +
  theme_gsl() + theme(strip.text.y = element_text(angle = 0, size = 7), strip.text.x = element_text(face = "bold"))

ggsave(file.path(drafts_dir, "GSL methods raster draft.png"), p_raster, width = PEERJ_FIG_WIDTH_MM * 1.3,
       height = 230, units = "mm", dpi = PEERJ_DPI, device = ragg::agg_png, bg = "white")
writeLines(c(
  "GSL methods raster (draft). US-Ha1, US-Wkg, US-SRM (columns); each row block is one method",
  "(M1-summer, M2 10%, M4 0.95, M5-span, M6 top to bottom); each horizontal bar is one year's",
  "detected season, start day-of-year to end day-of-year (agreement variant for M1/M2/M4, NEP-only",
  "for M5/M6). A missing bar for a given year/method means no season was detected (flag_no_season) --",
  "see tables/gsl_methods_site_year.csv for exact dates and flags.",
  "Numbers behind this figure: tables/gsl_methods_site_year.csv."
), file.path(drafts_dir, "GSL methods raster draft.legend.txt"))
write_meta(
  file.path(drafts_dir, "GSL methods raster draft.png"),
  input_sources = file.path(EXP_TABLES, "gsl_methods_site_year.csv"),
  notes = "Draft figure, not a numbered manuscript figure. Segments (start_doy-end_doy), not a filled day-by-day grid; all 3 sites are Northern Hemisphere."
)
message("Saved: 'GSL methods raster draft'.png/.legend.txt/.meta.json")

message("--- 11_gsl_methods_compare.R: done ---")
