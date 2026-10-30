## 02_derive.R — GSL expanded analysis, step 2: every quantity in
## docs/framework.md's derived-quantities table (reusing derive_one() etc.
## from code/03_derive.R by sourcing, not copying -- see
## logs/prompts/2026-10-30_gsl_expanded.md), plus the new quantities this
## task asks for: 1-F at Z17/T95, season start/end/gap days, the bounds
## check, and inputs the modelling step (03_models.R) needs (annual GPP/NEP,
## annual mean TA_F). Three GPP/neg-GPP variants: main (GPP_NT_REF, negative
## GPP -> 0), dt (GPP_DT_REF, negative GPP -> 0, for Supplemental S5), and
## negdelivered (GPP_NT_REF, negative GPP as-delivered, one summary line for
## S5). Season start/end/gap/bounds-check use the main variant only.

source("GSL_definition/expanded/code/00_config.R")
options(gsl.skip_driver = TRUE)
source("GSL_definition/code/03_derive.R") # g1_adjusted, l_skewness, kelly_skewness,
                                            # year_threshold_matrices, derive_one,
                                            # FINE_GRID_PCT, TASH_GRID_PCT, WINDOW_STEPS, FRACTION_TARGETS
options(gsl.skip_driver = FALSE)

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(lubridate); library(purrr)
})

exp_sites      <- read_csv(file.path(EXP_TABLES, "sites.csv"), show_col_types = FALSE)
exp_site_years <- read_csv(file.path(EXP_TABLES, "site_years.csv"), show_col_types = FALSE)
daily_extract  <- readRDS(file.path(EXP_DATA, "daily_extract.rds"))

site_years_list <- split(exp_site_years$year, exp_site_years$site_id)
trend_site_ids  <- names(Filter(function(y) length(y) >= 10L, site_years_list))
message(length(trend_site_ids), " expanded trend site(s) (>= 10 passing years).")

## ============================================================================
## Step A: derive_one() per site, for 3 variants -------------------------------
## ============================================================================
variant_defs <- list(
  main        = list(gpp_col = "GPP_NT_REF", neg_to_zero = TRUE),
  dt          = list(gpp_col = "GPP_DT_REF", neg_to_zero = TRUE),
  negdelivered = list(gpp_col = "GPP_NT_REF", neg_to_zero = FALSE)
)

run_variant <- function(vname) {
  v <- variant_defs[[vname]]
  message("Deriving variant '", vname, "': ", v$gpp_col, ", neg_to_zero=", v$neg_to_zero)
  out <- purrr::map(exp_sites$site_id, function(sid) {
    derive_one(sid, v$gpp_col, v$neg_to_zero, site_years_list[[sid]], daily_extract,
               trend_site_ids = trend_site_ids)
  })
  list(
    site      = bind_rows(purrr::map(out, "site")) |> mutate(variant = vname, .before = 1),
    site_year = bind_rows(purrr::map(out, "site_year")) |> mutate(variant = vname, .before = 1),
    fine_grid = bind_rows(purrr::map(out, "fine_grid")) |> mutate(variant = vname, .before = 1),
    fraction_sweep = bind_rows(purrr::map(out, "fraction_sweep")) |> mutate(variant = vname, .before = 1)
  )
}

res_main <- run_variant("main")
res_dt   <- run_variant("dt")
res_neg  <- run_variant("negdelivered")

derived_site_year_all <- bind_rows(res_main$site_year, res_dt$site_year, res_neg$site_year)
fine_grid_main         <- res_main$fine_grid
fine_grid_dt           <- res_dt$fine_grid

## ---- Recompute skewness thirds on the EXPANDED set, per variant (not the
## 62-site project's fixed 0.63/0.88 cutoffs) -- terciles of this variant's
## own skewness distribution across the expanded sites. ----
recompute_thirds <- function(site_tbl) {
  brks <- quantile(site_tbl$skewness, c(1/3, 2/3), na.rm = TRUE, type = 7)
  site_tbl$skewness_third <- case_when(
    is.na(site_tbl$skewness) ~ NA_character_,
    site_tbl$skewness <= brks[1] ~ "low",
    site_tbl$skewness <= brks[2] ~ "middle",
    TRUE ~ "high"
  )
  attr(site_tbl, "tercile_breaks") <- as.numeric(brks)
  site_tbl
}
derived_site_main <- recompute_thirds(res_main$site)
derived_site_dt   <- recompute_thirds(res_dt$site)
derived_site_neg  <- recompute_thirds(res_neg$site)
tercile_breaks_main <- attr(derived_site_main, "tercile_breaks")
tercile_breaks_dt   <- attr(derived_site_dt, "tercile_breaks")
message("Expanded-set skewness tercile breaks (main variant): low<=", round(tercile_breaks_main[1], 3),
        ", middle<=", round(tercile_breaks_main[2], 3), " (n=", nrow(derived_site_main), ")")

derived_site_all <- bind_rows(derived_site_main, derived_site_dt, derived_site_neg)

## ---- Join group/IGBP/network/lat and add 1-F at Z17 and T95 --------------
derived_site_all <- derived_site_all |>
  left_join(exp_sites |> select(site_id, igbp, group, network, location_lat, location_long,
                                  carbon_src, in_gsl62, is_long_record), by = "site_id") |>
  mutate(
    share_outside_season_z17 = 1 - z17_mean_f,
    share_outside_season_t95 = 1 - t95_mean_f
  )

write_csv(derived_site_all, file.path(EXP_TABLES, "derived_site.csv"))
write_meta(
  file.path(EXP_TABLES, "derived_site.csv"),
  input_sources = c(file.path(EXP_DATA, "daily_extract.rds"), file.path(EXP_TABLES, "sites.csv")),
  notes = paste0(
    "One row per site x variant (main=GPP_NT_REF zero-floored, dt=GPP_DT_REF zero-floored, ",
    "negdelivered=GPP_NT_REF as-delivered negative GPP), ", n_distinct(derived_site_all$site_id),
    " sites. derive_one() reused by sourcing code/03_derive.R (gsl.skip_driver=TRUE). Skewness ",
    "thirds are TERCILES OF THIS EXPANDED SET'S OWN skewness distribution, recomputed per ",
    "variant (main breaks: low<=", round(tercile_breaks_main[1], 4), ", middle<=",
    round(tercile_breaks_main[2], 4), "; dt breaks: low<=", round(tercile_breaks_dt[1], 4),
    ", middle<=", round(tercile_breaks_dt[2], 4), ") -- NOT the 62-site project's fixed 0.63/0.88 ",
    "cutoffs. share_outside_season_* = 1 - mean F at that threshold (T95's is ~5% by construction)."
  )
)

write_csv(derived_site_year_all, file.path(EXP_TABLES, "derived_site_year.csv"))
write_meta(
  file.path(EXP_TABLES, "derived_site_year.csv"),
  input_sources = file.path(EXP_DATA, "daily_extract.rds"),
  notes = "Annual GSL(y) and F(y) at Z17 (10% of P99) and T95, per site x variant x year."
)

write_csv(res_main$fraction_sweep, file.path(EXP_TABLES, "fraction_sweep_main.csv"))
write_meta(
  file.path(EXP_TABLES, "fraction_sweep_main.csv"),
  input_sources = file.path(EXP_DATA, "daily_extract.rds"),
  notes = "Per-site mean GSL at each of 20 target included-GPP fractions (0.80-0.99), main variant -- the threshold needed to include that fraction of annual GPP, for Supplemental Figure S7."
)

saveRDS(fine_grid_main, file.path(EXP_DATA, "fine_grid_main.rds"))
saveRDS(fine_grid_dt, file.path(EXP_DATA, "fine_grid_dt.rds"))

fine_grid_summary <- bind_rows(fine_grid_main, fine_grid_dt) |>
  group_by(variant, site_id) |>
  summarise(
    sensitivity_at_5pct  = local_sensitivity[which.min(abs(pct_of_P99 - 5))],
    sensitivity_at_10pct = local_sensitivity[which.min(abs(pct_of_P99 - 10))],
    sensitivity_at_20pct = local_sensitivity[which.min(abs(pct_of_P99 - 20))],
    .groups = "drop"
  )
write_csv(fine_grid_summary, file.path(EXP_TABLES, "fine_grid_summary.csv"))
write_meta(
  file.path(EXP_TABLES, "fine_grid_summary.csv"),
  input_sources = c(file.path(EXP_DATA, "fine_grid_main.rds"), file.path(EXP_DATA, "fine_grid_dt.rds")),
  notes = "Per-site local sensitivity (d(mean GSL)/d(% of P99), centered difference over +/-0.05% of P99) at 5/10/20% of P99, main and dt variants. Full fine grid (5000 points, 0.01-50% of P99) in expanded/data/fine_grid_*.rds (gitignored)."
)

message("--- Step A done: derived_site (", nrow(derived_site_all), " rows), derived_site_year (",
        nrow(derived_site_year_all), " rows) ---")

## ============================================================================
## Step B: season start/end/gap days (main variant only), at 4 threshold
## points per site -- 5%, 10% (Z17), 20% of P99, and T95's own %.
## ============================================================================

daily_main <- daily_extract |>
  mutate(gpp_use = pmax(GPP_NT_REF, 0)) |>
  select(site_id, TIMESTAMP, year, gpp_use, QC_REF, NEE_REF, TA_F)

P99_by_site  <- setNames(derived_site_main$P99, derived_site_main$site_id)
T95_by_site  <- setNames(derived_site_main$t95_pct, derived_site_main$site_id)
sh_sites     <- exp_sites$site_id[exp_sites$location_lat < 0]

SEASON_PCTS <- c("5pct" = 5, "z17" = 10, "20pct" = 20, "t95" = NA) # t95's % filled in per site

#' Season start/end/gap days for one (site, season_year, threshold).
#' `d` is that site's full daily record (gpp_use, NA-safe); `window` is the
#' Date sequence for this season-year window (calendar year for NH; 1 Jul
#' (season_year) - 30 Jun (season_year+1) for SH, labelled by the starting
#' calendar year). A day with NA gpp_use counts as "below" (consistent with
#' this project's NA-counts-as-failing convention elsewhere).
season_one <- function(d, window_dates, T_abs) {
  g <- d$gpp_use[match(window_dates, d$TIMESTAMP)]
  above <- !is.na(g) & g > T_abs
  above[is.na(above)] <- FALSE
  n <- length(above)
  if (all(above)) {
    return(tibble(start_date = window_dates[1], end_date = window_dates[n],
                   gap_days = 0L, above_threshold_all_year = TRUE, n_days = n))
  }
  r <- rle(above)
  run_end <- cumsum(r$lengths)
  run_start <- run_end - r$lengths + 1L
  qualifying <- which(r$values & r$lengths >= 5L)
  if (length(qualifying) == 0) {
    return(tibble(start_date = as.Date(NA), end_date = as.Date(NA),
                   gap_days = NA_integer_, above_threshold_all_year = FALSE, n_days = n))
  }
  first_run <- qualifying[1]; last_run <- qualifying[length(qualifying)]
  s_idx <- run_start[first_run]; e_idx <- run_end[last_run]
  gap_days <- sum(!above[s_idx:e_idx])
  tibble(start_date = window_dates[s_idx], end_date = window_dates[e_idx],
         gap_days = gap_days, above_threshold_all_year = FALSE, n_days = n)
}

season_for_site <- function(sid) {
  d <- daily_main |> filter(site_id == sid)
  P99 <- P99_by_site[[sid]]
  t95pct <- T95_by_site[[sid]]
  pcts <- SEASON_PCTS; pcts["t95"] <- t95pct
  yrs <- site_years_list[[sid]]
  is_sh <- sid %in% sh_sites

  windows <- if (!is_sh) {
    lapply(yrs, function(y) list(season_year = y,
                                   dates = seq(as.Date(sprintf("%d-01-01", y)), as.Date(sprintf("%d-12-31", y)), by = "day")))
  } else {
    lapply(yrs, function(y) {
      ## SH season-year y = 1 Jul y - 30 Jun (y+1); requires BOTH calendar
      ## years y and y+1 to be in this site's own passing-year list, else
      ## skipped (insufficient/undefined coverage at the record's edges).
      if (!(y + 1L) %in% yrs) return(NULL)
      list(season_year = y, dates = seq(as.Date(sprintf("%d-07-01", y)), as.Date(sprintf("%d-06-30", y + 1L)), by = "day"))
    })
  }
  windows <- Filter(Negate(is.null), windows)

  out <- purrr::map_dfr(windows, function(w) {
    purrr::map_dfr(names(pcts), function(pn) {
      T_abs <- pcts[[pn]] / 100 * P99
      res <- season_one(d, w$dates, T_abs)
      res$threshold_name <- pn
      res$threshold_pct <- pcts[[pn]]
      res$season_year <- w$season_year
      res
    })
  })
  out$site_id <- sid
  out$is_southern <- is_sh
  out
}

message("Computing season start/end/gap days for ", nrow(exp_sites), " sites x 4 thresholds...")
season_boundaries <- purrr::map_dfr(exp_sites$site_id, season_for_site)
season_boundaries <- season_boundaries |>
  mutate(start_doy = lubridate::yday(start_date), end_doy = lubridate::yday(end_date)) |>
  select(site_id, season_year, is_southern, threshold_name, threshold_pct,
         start_date, end_date, start_doy, end_doy, gap_days, above_threshold_all_year, n_days)

n_all_year <- sum(season_boundaries$above_threshold_all_year)
message(n_all_year, " site-year x threshold combination(s) are above threshold all year.")

write_csv(season_boundaries, file.path(EXP_TABLES, "season_boundaries.csv"))
write_meta(
  file.path(EXP_TABLES, "season_boundaries.csv"),
  input_sources = file.path(EXP_DATA, "daily_extract.rds"),
  notes = paste0(
    "Season start = first day of the first run of >=5 consecutive days with gpp_use > threshold; ",
    "end = last day of the last such run; gap_days = days below threshold strictly between start ",
    "and end (inclusive range). NA gpp_use counts as 'below'. Northern-hemisphere sites use the ",
    "calendar year; Southern-hemisphere sites (location_lat<0) use 1 Jul (season_year) - 30 Jun ",
    "(season_year+1), included only when BOTH calendar years are in that site's own passing-year ",
    "list. threshold_name: 5pct/20pct = 5%/20% of P99 (Figure 4a); z17 = 10% of P99; t95 = the ",
    "site's own T95 %. above_threshold_all_year flags a site-year with no day below threshold at ",
    "all (start=day 1, end=last day, gap_days=0 trivially). ", n_all_year, " such combination(s)."
  )
)
message("Saved: season_boundaries.csv (", nrow(season_boundaries), " rows)")

## ============================================================================
## Step C: bounds check (main variant) -- GSL with every QC-failing day
## forced to "above", and again forced to "below", at Z17 and T95. ----------
## ============================================================================

daily_main_qc <- daily_main |>
  mutate(pass_raw = qc_passes(QC_REF, EXP_RULE_QC_DAY),
         is_fail = ifelse(is.na(pass_raw), TRUE, !pass_raw))

bounds_one <- function(sid) {
  d <- daily_main_qc |> filter(site_id == sid)
  P99 <- P99_by_site[[sid]]
  t95pct <- T95_by_site[[sid]]
  yrs <- site_years_list[[sid]]
  purrr::map_dfr(yrs, function(y) {
    dy <- d |> filter(year == y)
    purrr::map_dfr(list(z17 = 10, t95 = t95pct), function(pct) {
      T_abs <- pct / 100 * P99
      passing_above <- !is.na(dy$gpp_use) & dy$gpp_use > T_abs & !dy$is_fail
      n_fail <- sum(dy$is_fail)
      gsl_lower <- sum(passing_above)
      gsl_upper <- gsl_lower + n_fail
      gsl_actual <- sum(!is.na(dy$gpp_use) & dy$gpp_use > T_abs) # as reported elsewhere (uses every day's real value, incl. failing)
      tibble(gsl_lower = gsl_lower, gsl_upper = gsl_upper, gsl_actual = gsl_actual,
             n_fail_days = n_fail, spread = gsl_upper - gsl_lower)
    }, .id = "threshold_name") |> mutate(year = y)
  }) |> mutate(site_id = sid)
}

message("Computing the bounds check for ", nrow(exp_sites), " sites x 2 thresholds...")
bounds_check <- purrr::map_dfr(exp_sites$site_id, bounds_one) |>
  select(site_id, year, threshold_name, gsl_lower, gsl_actual, gsl_upper, n_fail_days, spread)

stopifnot(all(bounds_check$spread == bounds_check$n_fail_days)) # spread is exactly n_fail_days, by construction
stopifnot(all(bounds_check$gsl_actual >= bounds_check$gsl_lower - 0L & bounds_check$gsl_actual <= bounds_check$gsl_upper))

write_csv(bounds_check, file.path(EXP_TABLES, "bounds_check.csv"))
write_meta(
  file.path(EXP_TABLES, "bounds_check.csv"),
  input_sources = file.path(EXP_DATA, "daily_extract.rds"),
  notes = paste0(
    "Per site-year, at Z17 and T95: gsl_lower = GSL if every QC-failing day (fallback QC < 0.25, ",
    "or NA) is forced to count as BELOW threshold regardless of its own gap-filled GPP value; ",
    "gsl_upper = GSL if every such day is forced ABOVE threshold; gsl_actual = the GSL actually ",
    "reported elsewhere (uses each day's own real gap-filled value, including failing-QC days). ",
    "spread = gsl_upper - gsl_lower, which is algebraically exactly n_fail_days (0-14 by rule 4's ",
    "R2 N=14 construction) -- checked directly (stopifnot), not assumed. gsl_actual always lies in ",
    "[gsl_lower, gsl_upper] (checked)."
  )
)
message("Saved: bounds_check.csv (", nrow(bounds_check), " rows). Spread range: ",
        min(bounds_check$spread), "-", max(bounds_check$spread), ".")

## ============================================================================
## Step D: annual GPP/NEP and annual mean TA_F per site-year (main variant),
## inputs for 03_models.R's carbon-gain and temperature-sensitivity steps.
## NEP = -NEE (prompt instruction).
## ============================================================================

annual_inputs <- daily_main |>
  group_by(site_id, year) |>
  summarise(
    annual_gpp = sum(gpp_use, na.rm = TRUE),
    n_days_gpp = sum(!is.na(gpp_use)),
    annual_nep = sum(-NEE_REF, na.rm = TRUE),
    n_days_nee = sum(!is.na(NEE_REF)),
    annual_mean_ta = mean(TA_F, na.rm = TRUE),
    n_days_ta = sum(!is.na(TA_F)),
    .groups = "drop"
  ) |>
  semi_join(exp_site_years, by = c("site_id", "year"))

write_csv(annual_inputs, file.path(EXP_TABLES, "annual_inputs.csv"))
write_meta(
  file.path(EXP_TABLES, "annual_inputs.csv"),
  input_sources = file.path(EXP_DATA, "daily_extract.rds"),
  notes = "Annual GPP (gC/m2/yr, zero-floored, main variant), NEP = -NEE_REF, and mean TA_F per passing site-year. Sums/means use na.rm=TRUE over available days; n_days_* columns report coverage for transparency."
)
message("Saved: annual_inputs.csv (", nrow(annual_inputs), " rows)")

message("--- 02_derive.R: done ---")
