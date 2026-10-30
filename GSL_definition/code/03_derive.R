## 03_derive.R — GSL figure rebuild, framework run-order step 4.
##
## Every quantity in docs/framework.md's "Derived quantities" table, per
## site, for both negative-GPP treatments (as-delivered, zero-floored) and
## both GPP variants (GPP_NT_VUT_REF main, GPP_DT_VUT_REF supplementary).
## No figures (Stage 3). Draws only on the fixed Sites table (GSL_SITES)
## and the Stage 2 decision switches in 00_config.R -- not the Tash-rule
## network-wide reproduction from Stage 1.

source("GSL_definition/code/00_config.R")

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(lubridate); library(purrr)
})

## ---- Helper functions ------------------------------------------------------
##
## Reused directly by GSL_definition/expanded/ (sourced, not copied -- see
## logs/prompts/2026-10-30_gsl_expanded.md): derive_one() already takes
## gpp_col/neg_to_zero/years/daily_extract as arguments, so a caller with its
## own resolved GPP column and site/year list needs no changes to this
## function. Everything below the function definitions -- the 62-site-only
## daily_extract.rds load and the full variant_grid driver that writes
## GSL_definition/tables/derived_site*.csv -- is guarded by
## options(gsl.skip_driver = TRUE) so a caller can source this file for the
## functions alone without re-running or overwriting the 62-site outputs.

#' Adjusted Fisher-Pearson skewness (scipy.stats.skew, bias = FALSE).
g1_adjusted <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 3) return(NA_real_)
  m <- mean(x)
  m2 <- mean((x - m)^2)
  m3 <- mean((x - m)^3)
  if (m2 == 0) return(NA_real_)
  g1 <- m3 / m2^1.5
  g1 * sqrt(n * (n - 1)) / (n - 2)
}

#' Sample L-skewness (Hosking 1990 unbiased L-moment ratio, l3/l2).
l_skewness <- function(x) {
  x <- sort(x[!is.na(x)])
  n <- length(x)
  if (n < 3) return(NA_real_)
  i <- seq_len(n)
  b0 <- mean(x)
  b1 <- sum((i - 1) / (n - 1) * x) / n
  b2 <- sum((i - 1) * (i - 2) / ((n - 1) * (n - 2)) * x) / n
  l2 <- 2 * b1 - b0
  l3 <- 6 * b2 - 6 * b1 + b0
  if (l2 == 0) return(NA_real_)
  l3 / l2
}

#' Kelly's coefficient of skewness: (P90 + P10 - 2*P50) / (P90 - P10).
kelly_skewness <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_) # quantile() on an empty vector returns NA, not an error --
                                         # denom would then be NA and `if (denom == 0)` would abort
                                         # with "missing value where TRUE/FALSE needed". Found via
                                         # GSL_definition/expanded/ (CA-TP1 has GPP_NT_REF data but
                                         # zero non-NA GPP_DT_REF); none of the 62-site project's own
                                         # sites hit this, so it was previously unguarded.
  q <- quantile(x, c(0.10, 0.50, 0.90), type = 7, names = FALSE)
  denom <- q[3] - q[1]
  if (denom == 0) return(NA_real_)
  as.numeric((q[3] + q[1] - 2 * q[2]) / denom)
}

#' Per-year GSL(T) and F(T) across a vector of absolute thresholds, for one
#' site's screened daily GPP. Returns a list of two matrices (years x
#' thresholds): gsl (count of days with gpp > T, GSL_STRICT_GT) and f
#' (fraction of annual GPP sum on days with gpp > T).
year_threshold_matrices <- function(gpp_by_year, thresholds_abs) {
  n_grid <- length(thresholds_abs)
  years <- names(gpp_by_year)
  gsl_m <- matrix(NA_real_, nrow = length(years), ncol = n_grid, dimnames = list(years, NULL))
  f_m   <- matrix(NA_real_, nrow = length(years), ncol = n_grid, dimnames = list(years, NULL))
  for (yr in years) {
    gy <- gpp_by_year[[yr]]
    gy <- gy[!is.na(gy)]
    n <- length(gy)
    if (n == 0) next
    sorted <- sort(gy)
    csum <- c(0, cumsum(sorted))
    # findInterval(T, sorted) = count of values <= T (GSL_STRICT_GT=TRUE -> count(>T) = n - that)
    idx <- findInterval(thresholds_abs, sorted)
    gsl_m[yr, ] <- n - idx
    total_sum <- sum(gy)
    sum_le <- csum[idx + 1]
    sum_gt <- total_sum - sum_le
    f_m[yr, ] <- if (total_sum != 0) sum_gt / total_sum else NA_real_
  }
  list(gsl = gsl_m, f = f_m)
}

FINE_GRID_PCT <- seq(0.01, 50.00, by = 0.01) # 5000 points, % of P99
TASH_GRID_PCT <- c(0.1, 0.5, 1:20)           # 22 points, % of P99
WINDOW_STEPS <- 5L                           # +/-0.05% of P99 = 5 steps of 0.01%
FRACTION_TARGETS <- seq(0.80, 0.99, by = 0.01)

#' Full derivation for one site x one (gpp_variant, neg_gpp_variant) combo.
#'
#' @param trend_site_ids Site IDs for which the Z17 trend's 95% CI is
#'   computed (kept NA otherwise, since a CI on <10 years is not
#'   meaningful). Defaults to the 62-site project's own 21-site
#'   GSL_SITES_GE10YR for backward compatibility; a caller with a different
#'   population (e.g. GSL_definition/expanded/, sourced not copied) passes
#'   its own >=10-year site list here instead of relying on this default.
derive_one <- function(sid, gpp_col, neg_to_zero, years, daily_extract,
                        trend_site_ids = GSL_SITES_GE10YR) {
  d <- daily_extract |> filter(site_id == sid, year %in% years)
  gpp_raw <- d[[gpp_col]]
  gpp <- if (neg_to_zero) pmax(gpp_raw, 0) else gpp_raw
  d$gpp_use <- gpp

  pooled <- gpp[!is.na(gpp)]
  n_days <- length(pooled)
  P99 <- as.numeric(quantile(pooled, 0.99, type = 7, na.rm = TRUE))
  skew <- g1_adjusted(pooled)
  lskew <- l_skewness(pooled)
  kskew <- kelly_skewness(pooled)
  pct_below_5  <- 100 * mean(pooled < 0.05 * P99)
  pct_below_10 <- 100 * mean(pooled < 0.10 * P99)
  pct_below_20 <- 100 * mean(pooled < 0.20 * P99)

  gpp_by_year <- split(d$gpp_use, as.character(d$year))

  fine_abs <- FINE_GRID_PCT / 100 * P99
  fine <- year_threshold_matrices(gpp_by_year, fine_abs)
  mean_gsl_grid <- colMeans(fine$gsl, na.rm = TRUE)
  mean_f_grid   <- colMeans(fine$f,   na.rm = TRUE)

  tash_abs <- TASH_GRID_PCT / 100 * P99
  tash <- year_threshold_matrices(gpp_by_year, tash_abs)
  tash_mean_gsl <- colMeans(tash$gsl, na.rm = TRUE)
  tash_mean_f   <- colMeans(tash$f,   na.rm = TRUE)

  n_grid <- length(FINE_GRID_PCT)
  sensitivity <- rep(NA_real_, n_grid)
  lo <- (WINDOW_STEPS + 1):(n_grid - WINDOW_STEPS)
  sensitivity[lo] <- (mean_gsl_grid[lo + WINDOW_STEPS] - mean_gsl_grid[lo - WINDOW_STEPS]) / (2 * 0.05)

  z17_idx <- which.min(abs(FINE_GRID_PCT - 10))
  z17_mean_gsl <- mean_gsl_grid[z17_idx]
  z17_mean_f   <- mean_f_grid[z17_idx]
  z17_sensitivity <- sensitivity[z17_idx]

  f_rounded <- round(mean_f_grid, 6)
  t95_idx <- which(f_rounded <= 0.95)[1]
  t95_pct <- if (!is.na(t95_idx)) FINE_GRID_PCT[t95_idx] else NA_real_
  t95_mean_gsl <- if (!is.na(t95_idx)) mean_gsl_grid[t95_idx] else NA_real_
  t95_mean_f   <- if (!is.na(t95_idx)) mean_f_grid[t95_idx] else NA_real_
  t95_sensitivity <- if (!is.na(t95_idx)) sensitivity[t95_idx] else NA_real_

  common_unit_sens <- function(idx) {
    if (is.na(idx) || idx <= WINDOW_STEPS || idx > n_grid - WINDOW_STEPS) return(NA_real_)
    dGSL <- mean_gsl_grid[idx + WINDOW_STEPS] - mean_gsl_grid[idx - WINDOW_STEPS]
    dF   <- mean_f_grid[idx + WINDOW_STEPS]   - mean_f_grid[idx - WINDOW_STEPS]
    if (is.na(dF) || dF == 0) return(NA_real_)
    (dGSL / dF) * 0.01
  }
  cus_z17 <- common_unit_sens(z17_idx)
  cus_t95 <- common_unit_sens(t95_idx)

  gsl_z17_byyear <- fine$gsl[, z17_idx]
  gsl_t95_byyear <- if (!is.na(t95_idx)) fine$gsl[, t95_idx] else rep(NA_real_, length(years))
  f_z17_byyear   <- fine$f[, z17_idx]
  f_t95_byyear   <- if (!is.na(t95_idx)) fine$f[, t95_idx] else rep(NA_real_, length(years))

  yrs_num <- as.integer(names(gpp_by_year))
  mean_gsl_z17 <- mean(gsl_z17_byyear, na.rm = TRUE)
  sd_gsl_z17 <- sd(gsl_z17_byyear, na.rm = TRUE)
  trend_slope_z17 <- NA_real_; trend_ci_lo <- NA_real_; trend_ci_hi <- NA_real_
  ## sum(!is.na(...)) >= 3, not just length(yrs_num) >= 3: a site can have
  ## >=3 years on record but zero non-NA gsl_z17_byyear values (e.g. a GPP
  ## variant with no data at all for that site -- CA-TP1 has GPP_NT_REF but
  ## zero non-NA GPP_DT_REF), which previously reached lm.fit() with 0
  ## non-NA cases and errored ("0 (non-NA) cases"), found via
  ## GSL_definition/expanded/.
  if (length(yrs_num) >= 3 && length(unique(yrs_num)) >= 2 && sum(!is.na(gsl_z17_byyear)) >= 3) {
    fit <- lm(gsl_z17_byyear ~ yrs_num)
    trend_slope_z17 <- unname(coef(fit)[2])
    if (sid %in% trend_site_ids) {
      ci <- suppressWarnings(confint(fit, level = 0.95)[2, ])
      trend_ci_lo <- unname(ci[1]); trend_ci_hi <- unname(ci[2])
    }
  }

  fraction_sweep_gsl <- vapply(FRACTION_TARGETS, function(ft) {
    idx <- which(f_rounded <= round(ft, 6))[1]
    if (is.na(idx)) NA_real_ else mean_gsl_grid[idx]
  }, numeric(1))

  skew_third <- case_when(
    is.na(skew) ~ NA_character_,
    skew <= 0.63 ~ "low", skew <= 0.88 ~ "middle", TRUE ~ "high"
  )

  list(
    site = tibble(
      site_id = sid, n_years = length(years), n_days = n_days, P99 = P99,
      skewness = skew, skewness_third = skew_third, l_skewness = lskew, kelly_skewness = kskew,
      pct_days_below_5pctP99 = pct_below_5, pct_days_below_10pctP99 = pct_below_10,
      pct_days_below_20pctP99 = pct_below_20,
      z17_pct = 10, z17_mean_gsl = z17_mean_gsl, z17_mean_f = z17_mean_f,
      z17_sensitivity = z17_sensitivity, z17_common_unit_sensitivity = cus_z17,
      t95_pct = t95_pct, t95_mean_gsl = t95_mean_gsl, t95_mean_f = t95_mean_f, t95_sensitivity = t95_sensitivity,
      t95_common_unit_sensitivity = cus_t95,
      mean_gsl_z17 = mean_gsl_z17, sd_gsl_z17 = sd_gsl_z17,
      trend_gsl_z17_days_per_yr = trend_slope_z17,
      trend_ci_lo = trend_ci_lo, trend_ci_hi = trend_ci_hi
    ),
    site_year = tibble(
      site_id = sid, year = yrs_num, gsl_z17 = gsl_z17_byyear, gsl_t95 = gsl_t95_byyear,
      f_z17 = f_z17_byyear, f_t95 = f_t95_byyear
    ),
    fine_grid = tibble(
      site_id = sid, pct_of_P99 = FINE_GRID_PCT, mean_gsl = mean_gsl_grid,
      mean_f = mean_f_grid, local_sensitivity = sensitivity
    ),
    tash_grid = tibble(
      site_id = sid, pct_of_P99 = TASH_GRID_PCT, mean_gsl = tash_mean_gsl, mean_f = tash_mean_f
    ),
    fraction_sweep = tibble(
      site_id = sid, target_fraction = FRACTION_TARGETS, gsl = fraction_sweep_gsl
    )
  )
}

## ---- Run across all 4 (gpp_variant x neg_gpp_variant) combinations --------
if (!isTRUE(getOption("gsl.skip_driver"))) {

daily_extract <- readRDS(file.path(GSL_ROOT, "data", "daily_extract.rds"))
daily_extract$TIMESTAMP <- as.Date(as.character(daily_extract$TIMESTAMP))
daily_extract$year <- lubridate::year(daily_extract$TIMESTAMP)

variant_grid <- expand.grid(
  gpp_variant = names(GSL_GPP_VARIANTS),
  neg_variant = names(GSL_NEG_GPP_VARIANTS),
  stringsAsFactors = FALSE
)

site_results <- list(); site_year_results <- list()
fine_grid_results <- list(); tash_grid_results <- list(); fraction_sweep_results <- list()

for (vi in seq_len(nrow(variant_grid))) {
  gpp_variant <- variant_grid$gpp_variant[vi]
  neg_variant <- variant_grid$neg_variant[vi]
  gpp_col <- GSL_GPP_VARIANTS[[gpp_variant]]
  neg_to_zero <- GSL_NEG_GPP_VARIANTS[[neg_variant]]
  message("Deriving: gpp_variant=", gpp_variant, " (", gpp_col, "), neg_variant=", neg_variant,
          " (neg_to_zero=", neg_to_zero, ")")

  out <- purrr::map(GSL_SITES$site_id, function(sid) {
    years <- GSL_SITES$years[GSL_SITES$site_id == sid][[1]]
    derive_one(sid, gpp_col, neg_to_zero, years, daily_extract)
  })

  tag <- function(df) df |> mutate(gpp_variant = gpp_variant, neg_gpp_variant = neg_variant, .before = 1)
  site_results[[vi]]      <- tag(bind_rows(purrr::map(out, "site")))
  site_year_results[[vi]] <- tag(bind_rows(purrr::map(out, "site_year")))
  fine_grid_results[[vi]] <- tag(bind_rows(purrr::map(out, "fine_grid")))
  tash_grid_results[[vi]] <- tag(bind_rows(purrr::map(out, "tash_grid")))
  fraction_sweep_results[[vi]] <- tag(bind_rows(purrr::map(out, "fraction_sweep")))
}

derived_site <- bind_rows(site_results)
derived_site_year <- bind_rows(site_year_results)
fine_grid_all <- bind_rows(fine_grid_results)
tash_grid_all <- bind_rows(tash_grid_results)
fraction_sweep_all <- bind_rows(fraction_sweep_results)

## ---- Write outputs ---------------------------------------------------------

if (!dir.exists(file.path(GSL_ROOT, "tables"))) dir.create(file.path(GSL_ROOT, "tables"), recursive = TRUE)
if (!dir.exists(file.path(GSL_ROOT, "data"))) dir.create(file.path(GSL_ROOT, "data"), recursive = TRUE)

write_csv(derived_site, file.path(GSL_ROOT, "tables", "derived_site.csv"))
write_meta(
  file.path(GSL_ROOT, "tables", "derived_site.csv"),
  input_sources = file.path(GSL_ROOT, "data", "daily_extract.rds"),
  notes = paste0(
    "One row per site x gpp_variant x neg_gpp_variant (", nrow(derived_site), " total). ",
    "Main run = gpp_variant='main' (GPP_NT_VUT_REF), neg_gpp_variant='main' (negative GPP set ",
    "to 0). GSL_STRICT_GT=TRUE (GSL counts GPP > T). Skewness = adjusted Fisher-Pearson G1 ",
    "(scipy.stats.skew, bias=False), implemented directly (no package). L-skewness = Hosking ",
    "unbiased sample L-moment ratio l3/l2. Kelly's skewness = (P90+P10-2*P50)/(P90-P10), ",
    "quantile type 7. Z17 = mean F at T = 10% of P99 (fixed). T95 = first fine-grid point ",
    "(0.01-50% of P99, 0.01% steps) where mean F rounds to <= 0.95. Trend CI only computed for ",
    "the 21 sites with >= 10 years (GSL_SITES_GE10YR)."
  )
)

write_csv(derived_site_year, file.path(GSL_ROOT, "tables", "derived_site_year.csv"))
write_meta(
  file.path(GSL_ROOT, "tables", "derived_site_year.csv"),
  input_sources = file.path(GSL_ROOT, "data", "daily_extract.rds"),
  notes = "Annual GSL(y) and included fraction F(y) at Z17's T (10% of P99) and at T95, per site x gpp_variant x neg_gpp_variant."
)

saveRDS(fine_grid_all, file.path(GSL_ROOT, "data", "fine_grid.rds"))
saveRDS(fraction_sweep_all, file.path(GSL_ROOT, "data", "fraction_sweep.rds"))

## Summaries of the large fine-grid / fraction-sweep data, in tables/.
fine_grid_summary <- fine_grid_all |>
  group_by(gpp_variant, neg_gpp_variant, site_id) |>
  summarise(
    pct_of_max_sensitivity = pct_of_P99[which.max(local_sensitivity)],
    max_sensitivity = max(local_sensitivity, na.rm = TRUE),
    sensitivity_at_5pct  = local_sensitivity[which.min(abs(pct_of_P99 - 5))],
    sensitivity_at_10pct = local_sensitivity[which.min(abs(pct_of_P99 - 10))],
    sensitivity_at_20pct = local_sensitivity[which.min(abs(pct_of_P99 - 20))],
    .groups = "drop"
  )
write_csv(fine_grid_summary, file.path(GSL_ROOT, "tables", "fine_grid_summary.csv"))
write_meta(
  file.path(GSL_ROOT, "tables", "fine_grid_summary.csv"),
  input_sources = file.path(GSL_ROOT, "data", "fine_grid.rds"),
  notes = "Per-site summary of the full fine-grid local-sensitivity curve (5000 points, 0.01-50% of P99, in data/fine_grid.rds); local sensitivity = d(mean GSL)/d(% of P99), centered difference over +/-0.05% of P99."
)

fraction_sweep_summary <- fraction_sweep_all |>
  filter(target_fraction %in% c(0.80, 0.90, 0.95, 0.99)) |>
  pivot_wider(names_from = target_fraction, values_from = gsl, names_prefix = "gsl_at_frac_")
write_csv(fraction_sweep_summary, file.path(GSL_ROOT, "tables", "fraction_sweep_summary.csv"))
write_meta(
  file.path(GSL_ROOT, "tables", "fraction_sweep_summary.csv"),
  input_sources = file.path(GSL_ROOT, "data", "fraction_sweep.rds"),
  notes = "Per-site GSL at 4 representative target fractions (0.80/0.90/0.95/0.99) out of the full 0.80-0.99 sweep in data/fraction_sweep.rds."
)

write_csv(tash_grid_all, file.path(GSL_ROOT, "tables", "tash_grid.csv"))
write_meta(
  file.path(GSL_ROOT, "tables", "tash_grid.csv"),
  input_sources = file.path(GSL_ROOT, "data", "daily_extract.rds"),
  notes = "Tash's 22-value threshold grid (0.1%, 0.5%, 1-20% of P99), for direct comparison against the fine grid in the same units."
)

message("--- 03_derive.R: done ---")
message(
  "derived_site: ", nrow(derived_site), " rows (", n_distinct(derived_site$site_id), " sites x ",
  nrow(variant_grid), " variants)"
)

} # if (!isTRUE(getOption("gsl.skip_driver")))
