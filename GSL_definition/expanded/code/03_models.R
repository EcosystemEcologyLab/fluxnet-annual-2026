## 03_models.R — GSL expanded analysis, step 3: carbon-gain slopes,
## temperature sensitivity, and trends -- per-site OLS (sites with >=10
## years) and pooled mixed models (random site intercepts/slopes, nlme),
## by skewness third and by functional group, at Z17 and T95 (and, for
## trends, across the 5-20% of P99 sweep too). Years-to-detect each pooled
## trend: Weatherhead et al. (1998)'s formula at 95% confidence / 80% power.

source("GSL_definition/expanded/code/00_config.R")
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(lubridate); library(purrr); library(nlme)
})

exp_sites         <- read_csv(file.path(EXP_TABLES, "sites.csv"), show_col_types = FALSE)
exp_site_years    <- read_csv(file.path(EXP_TABLES, "site_years.csv"), show_col_types = FALSE)
derived_site       <- read_csv(file.path(EXP_TABLES, "derived_site.csv"), show_col_types = FALSE) |> filter(variant == "main")
derived_site_year  <- read_csv(file.path(EXP_TABLES, "derived_site_year.csv"), show_col_types = FALSE) |> filter(variant == "main")
season_boundaries  <- read_csv(file.path(EXP_TABLES, "season_boundaries.csv"), show_col_types = FALSE)
annual_inputs      <- read_csv(file.path(EXP_TABLES, "annual_inputs.csv"), show_col_types = FALSE)
daily_extract      <- readRDS(file.path(EXP_DATA, "daily_extract.rds"))

site_years_list <- split(exp_site_years$year, exp_site_years$site_id)
trend_site_ids  <- names(Filter(function(y) length(y) >= 10L, site_years_list))

site_meta <- exp_sites |> select(site_id, group, location_lat) |>
  left_join(derived_site |> select(site_id, skewness, skewness_third), by = "site_id")

## ============================================================================
## Generic helpers
## ============================================================================

#' Per-site OLS slope of y on x (>=3 paired obs required).
ols_site <- function(d, xcol, ycol) {
  d <- d[!is.na(d[[xcol]]) & !is.na(d[[ycol]]), ]
  if (nrow(d) < 3 || length(unique(d[[xcol]])) < 2) {
    return(tibble(slope = NA_real_, se = NA_real_, p_value = NA_real_, n = nrow(d)))
  }
  fit <- lm(d[[ycol]] ~ d[[xcol]])
  co <- summary(fit)$coefficients
  tibble(slope = unname(co[2, 1]), se = unname(co[2, 2]), p_value = unname(co[2, 4]), n = nrow(d))
}

#' Pooled mixed-model slope of y on x across sites, random intercept+slope
#' by site_id (falls back to random-intercept-only if the full model fails
#' to converge; reports which). Optionally AR(1) residuals (trends).
fit_pooled_slope <- function(d, xcol, ycol, use_ar1 = FALSE) {
  if (use_ar1) stopifnot(xcol == "year") # corAR1's time index below is xcol (renamed .x)
  d <- d[!is.na(d[[xcol]]) & !is.na(d[[ycol]]), ]
  d$site_id <- factor(d$site_id)
  n_sites <- n_distinct(d$site_id)
  if (nrow(d) < 10 || n_sites < 3) {
    return(tibble(slope = NA_real_, se = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
                   n_sites = n_sites, n_obs = nrow(d), converged = FALSE, random_slope = NA,
                   model_tier = NA_character_, note = "too few sites/observations"))
  }
  dd <- d |> rename(.y = all_of(ycol), .x = all_of(xcol))
  ## corAR1's form needs an explicit time index (not implicit row order) --
  ## every use_ar1=TRUE caller here uses xcol="year", so .x IS that index.
  ar1_corr <- nlme::corAR1(form = ~ .x | site_id)

  try_fit <- function(random_slope, use_ar1_this, opt) {
    rand <- if (random_slope) ~ .x | site_id else ~ 1 | site_id
    corr <- if (use_ar1_this) ar1_corr else NULL
    ctrl <- nlme::lmeControl(maxIter = 300, msMaxIter = 300, opt = opt, msVerbose = FALSE)
    tryCatch(
      suppressWarnings(nlme::lme(.y ~ .x, random = rand, correlation = corr, data = dd,
                                   control = ctrl, method = "REML")),
      error = function(e) NULL
    )
  }
  ## Fallback ladder, most- to least-faithful to the requested model (random
  ## site slopes + AR(1) residuals): try both optimizers at each tier before
  ## dropping a feature. Documented (not silently substituted) via `note`
  ## and `model_tier` -- the AR(1)+random-slope combination proved hard to
  ## converge for several subsets with the "optim" optimizer alone (see
  ## SESSION_LOG and docs/report_back_*.md).
  tiers <- if (use_ar1) {
    list(
      list(rs = TRUE,  ar1 = TRUE,  opt = "nlminb", label = "random slope + AR1"),
      list(rs = TRUE,  ar1 = TRUE,  opt = "optim",   label = "random slope + AR1"),
      list(rs = FALSE, ar1 = TRUE,  opt = "nlminb", label = "random intercept + AR1"),
      list(rs = FALSE, ar1 = TRUE,  opt = "optim",   label = "random intercept + AR1"),
      list(rs = TRUE,  ar1 = FALSE, opt = "nlminb", label = "random slope, AR1 dropped"),
      list(rs = FALSE, ar1 = FALSE, opt = "nlminb", label = "random intercept, AR1 dropped")
    )
  } else {
    list(
      list(rs = TRUE,  ar1 = FALSE, opt = "nlminb", label = "random slope"),
      list(rs = TRUE,  ar1 = FALSE, opt = "optim",   label = "random slope"),
      list(rs = FALSE, ar1 = FALSE, opt = "nlminb", label = "random intercept")
    )
  }
  fit <- NULL; tier_label <- NA_character_; random_slope <- NA; tier_idx <- NA_integer_
  for (i in seq_along(tiers)) {
    t <- tiers[[i]]
    fit <- try_fit(t$rs, t$ar1, t$opt)
    if (!is.null(fit)) { tier_label <- t$label; random_slope <- t$rs; tier_idx <- i; break }
  }
  if (is.null(fit)) {
    return(tibble(slope = NA_real_, se = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
                   n_sites = n_sites, n_obs = nrow(dd), converged = FALSE, random_slope = NA,
                   model_tier = NA_character_, note = "lme() failed to converge at every fallback tier"))
  }
  co <- summary(fit)$tTable
  slope <- unname(co[".x", "Value"]); se <- unname(co[".x", "Std.Error"])
  ci <- tryCatch(nlme::intervals(fit, which = "fixed")$fixed[".x", c("lower", "upper")],
                  error = function(e) c(lower = NA_real_, upper = NA_real_))
  tibble(slope = slope, se = se, ci_lo = unname(ci["lower"]), ci_hi = unname(ci["upper"]),
         n_sites = n_sites, n_obs = nrow(dd), converged = TRUE, random_slope = random_slope,
         model_tier = tier_label,
         note = if (tier_idx > 1) paste0("fell back to tier ", tier_idx, ": ", tier_label) else "")
}

#' Weatherhead et al. (1998): years to detect a trend of magnitude `slope`
#' at 95% confidence / 80% power, given a site's residual SD (sigma_n) and
#' lag-1 autocorrelation (phi) of its OLS year-residuals.
Z_SUM <- qnorm(0.975) + qnorm(0.80) # 2.801585; Weatherhead's own "3.3" is for 95%/90%
weatherhead_years <- function(sigma_n, phi, slope) {
  if (any(is.na(c(sigma_n, phi, slope))) || slope == 0 || phi <= -1 || phi >= 1) return(NA_real_)
  ((Z_SUM * sigma_n / abs(slope)) * sqrt((1 + phi) / (1 - phi)))^(2/3)
}

#' Per-site residual SD and lag-1 autocorrelation from an OLS fit of ycol
#' on year, for Weatherhead inputs.
site_noise_params <- function(d, ycol) {
  d <- d[!is.na(d[[ycol]]), ]
  d <- d[order(d$year), ]
  if (nrow(d) < 4) return(tibble(sigma_n = NA_real_, phi = NA_real_))
  fit <- lm(d[[ycol]] ~ d$year)
  resid_v <- residuals(fit)
  phi <- if (length(resid_v) >= 3) stats::acf(resid_v, lag.max = 1, plot = FALSE)$acf[2] else NA_real_
  tibble(sigma_n = sd(resid_v), phi = as.numeric(phi))
}

SUBSET_DEFS <- c(
  setNames(GSL_SKEW_THIRD_LEVELS, paste0("third_", GSL_SKEW_THIRD_LEVELS)),
  setNames(GSL_GROUP_ORDER, paste0("group_", gsub(" ", "_", GSL_GROUP_ORDER)))
)
subset_filter <- function(d, subset_name) {
  val <- SUBSET_DEFS[[subset_name]]
  if (startsWith(subset_name, "third_")) d[d$skewness_third == val & !is.na(d$skewness_third), ]
  else d[d$group == val & !is.na(d$group), ]
}

## ============================================================================
## Step A: carbon gain -- slopes of annual GPP/NEP on GSL, start, end --------
## ============================================================================

## Season start/end (Z17, T95), keyed by (site_id, season_year); joined to
## annual_inputs by treating season_year as the calendar year (documented
## simplification for the 14 Southern-hemisphere sites -- see methods_memo).
season_wide <- season_boundaries |>
  filter(threshold_name %in% c("z17", "t95")) |>
  select(site_id, year = season_year, threshold_name, start_doy, end_doy, gap_days) |>
  pivot_wider(names_from = threshold_name, values_from = c(start_doy, end_doy, gap_days))

cg_data <- derived_site_year |>
  select(site_id, year, gsl_z17, gsl_t95) |>
  left_join(annual_inputs |> select(site_id, year, annual_gpp, annual_nep), by = c("site_id", "year")) |>
  left_join(season_wide, by = c("site_id", "year")) |>
  left_join(site_meta, by = "site_id")

write_csv(cg_data, file.path(EXP_TABLES, "carbon_gain_data.csv"))
write_meta(
  file.path(EXP_TABLES, "carbon_gain_data.csv"),
  input_sources = c(file.path(EXP_TABLES, "derived_site_year.csv"), file.path(EXP_TABLES, "annual_inputs.csv"),
                     file.path(EXP_TABLES, "season_boundaries.csv")),
  notes = "Per site-year regression inputs for carbon-gain and temperature-sensitivity models (main variant). Southern-hemisphere season_year (Jul-Jun) is joined to the same-numbered calendar year's annual GPP/NEP/TA_F -- a documented simplification, not an exact calendar match."
)

CG_PREDICTORS <- list(gsl_z17 = "gsl_z17", gsl_t95 = "gsl_t95",
                       start_z17 = "start_doy_z17", start_t95 = "start_doy_t95",
                       end_z17 = "end_doy_z17", end_t95 = "end_doy_t95")
CG_RESPONSES <- c("annual_gpp", "annual_nep")

message("Carbon gain: per-site OLS (trend sites, >=10yr) ...")
cg_site <- purrr::map_dfr(names(CG_PREDICTORS), function(pn) {
  xcol <- CG_PREDICTORS[[pn]]
  purrr::map_dfr(CG_RESPONSES, function(ycol) {
    purrr::map_dfr(trend_site_ids, function(sid) {
      d <- cg_data |> filter(site_id == sid)
      bind_cols(tibble(site_id = sid, predictor = pn, response = ycol), ols_site(d, xcol, ycol))
    })
  })
})
write_csv(cg_site, file.path(EXP_TABLES, "carbon_gain_per_site.csv"))
write_meta(
  file.path(EXP_TABLES, "carbon_gain_per_site.csv"),
  input_sources = file.path(EXP_TABLES, "carbon_gain_data.csv"),
  notes = "Per-site OLS slope of annual GPP/NEP on GSL/start-day/end-day, trend sites only (>=10 passing years, n=67)."
)

message("Carbon gain: pooled mixed models, by third and by group ...")
cg_pooled <- purrr::map_dfr(names(CG_PREDICTORS), function(pn) {
  xcol <- CG_PREDICTORS[[pn]]
  purrr::map_dfr(CG_RESPONSES, function(ycol) {
    purrr::map_dfr(names(SUBSET_DEFS), function(sn) {
      d <- subset_filter(cg_data, sn)
      bind_cols(tibble(predictor = pn, response = ycol, subset = sn), fit_pooled_slope(d, xcol, ycol))
    })
  })
})
write_csv(cg_pooled, file.path(EXP_TABLES, "carbon_gain_pooled.csv"))
write_meta(
  file.path(EXP_TABLES, "carbon_gain_pooled.csv"),
  input_sources = file.path(EXP_TABLES, "carbon_gain_data.csv"),
  notes = paste0(
    "Pooled slope of annual GPP/NEP (gC/m2/yr) on GSL/start-day/end-day (days), by skewness third ",
    "and functional group, random site intercept+slope (nlme::lme), REML. ",
    sum(!cg_pooled$converged), " of ", nrow(cg_pooled), " did not converge (random-intercept-only ",
    "fallback also failed) -- see the note column; these are NA, not silently dropped."
  )
)
message("Saved: carbon_gain_per_site.csv, carbon_gain_pooled.csv. Non-converged: ",
        sum(!cg_pooled$converged), " of ", nrow(cg_pooled))

## ============================================================================
## Step B: temperature sensitivity -------------------------------------------
## ============================================================================

daily_ta <- daily_extract |> select(site_id, TIMESTAMP, TA_F)

## Fixed, site-level mean start/end DOY (Z17 and T95), from the per-site-year
## season boundaries, averaged across years -- the ANCHOR for each site's
## fixed 60-day pre-window (same window every year, not that year's own date).
mean_doy <- season_boundaries |>
  filter(threshold_name %in% c("z17", "t95")) |>
  group_by(site_id, threshold_name) |>
  summarise(mean_start_doy = mean(start_doy, na.rm = TRUE), mean_end_doy = mean(end_doy, na.rm = TRUE), .groups = "drop")

#' Mean TA_F over the 60 days ending the day before `anchor_doy` (a fixed,
#' site-level DOY), evaluated within calendar year `yr`'s own timeline --
#' Date arithmetic handles wraparound into the previous December correctly.
ta_pre_window <- function(sid, yr, anchor_doy, window = 60L) {
  if (is.na(anchor_doy)) return(NA_real_)
  end_date <- as.Date(sprintf("%d-01-01", yr)) + round(anchor_doy) - 2L
  start_date <- end_date - (window - 1L)
  d <- daily_ta |> filter(site_id == sid, TIMESTAMP >= start_date, TIMESTAMP <= end_date)
  if (nrow(d) == 0) return(NA_real_)
  mean(d$TA_F, na.rm = TRUE)
}

message("Computing 60-day pre-start/pre-end TA_F windows ...")
ta_windows <- purrr::map_dfr(exp_sites$site_id, function(sid) {
  yrs <- site_years_list[[sid]]
  md <- mean_doy |> filter(site_id == sid)
  purrr::map_dfr(c("z17", "t95"), function(th) {
    row <- md |> filter(threshold_name == th)
    if (nrow(row) == 0) return(tibble())
    purrr::map_dfr(yrs, function(y) tibble(
      site_id = sid, year = y, threshold_name = th,
      mean_ta_pre_start = ta_pre_window(sid, y, row$mean_start_doy),
      mean_ta_pre_end   = ta_pre_window(sid, y, row$mean_end_doy)
    ))
  })
})

ts_data <- cg_data |>
  select(site_id, year, group, skewness_third, gsl_z17, gsl_t95, start_doy_z17, start_doy_t95,
         end_doy_z17, end_doy_t95) |>
  left_join(annual_inputs |> select(site_id, year, annual_mean_ta), by = c("site_id", "year")) |>
  left_join(ta_windows |> filter(threshold_name == "z17") |>
              select(site_id, year, ta_pre_start_z17 = mean_ta_pre_start, ta_pre_end_z17 = mean_ta_pre_end),
            by = c("site_id", "year")) |>
  left_join(ta_windows |> filter(threshold_name == "t95") |>
              select(site_id, year, ta_pre_start_t95 = mean_ta_pre_start, ta_pre_end_t95 = mean_ta_pre_end),
            by = c("site_id", "year"))

write_csv(ts_data, file.path(EXP_TABLES, "temperature_sensitivity_data.csv"))
write_meta(
  file.path(EXP_TABLES, "temperature_sensitivity_data.csv"),
  input_sources = c(file.path(EXP_TABLES, "carbon_gain_data.csv"), file.path(EXP_TABLES, "annual_inputs.csv")),
  notes = "annual_mean_ta = calendar-year mean TA_F. ta_pre_start_*/ta_pre_end_* = mean TA_F over the fixed 60 days ending the day before the SITE'S OWN MEAN start/end date (not that year's own date), evaluated within each year's timeline."
)

TS_COMBOS <- list(
  list(y = "gsl_z17", x = "annual_mean_ta", label = "gsl_on_ta_z17"),
  list(y = "gsl_t95", x = "annual_mean_ta", label = "gsl_on_ta_t95"),
  list(y = "start_doy_z17", x = "ta_pre_start_z17", label = "start_on_ta_z17"),
  list(y = "start_doy_t95", x = "ta_pre_start_t95", label = "start_on_ta_t95"),
  list(y = "end_doy_z17", x = "ta_pre_end_z17", label = "end_on_ta_z17"),
  list(y = "end_doy_t95", x = "ta_pre_end_t95", label = "end_on_ta_t95")
)

message("Temperature sensitivity: per-site OLS (trend sites) ...")
ts_site <- purrr::map_dfr(TS_COMBOS, function(cb) {
  purrr::map_dfr(trend_site_ids, function(sid) {
    d <- ts_data |> filter(site_id == sid)
    bind_cols(tibble(site_id = sid, combo = cb$label), ols_site(d, cb$x, cb$y))
  })
})
write_csv(ts_site, file.path(EXP_TABLES, "temperature_sensitivity_per_site.csv"))
write_meta(
  file.path(EXP_TABLES, "temperature_sensitivity_per_site.csv"),
  input_sources = file.path(EXP_TABLES, "temperature_sensitivity_data.csv"),
  notes = "Per-site OLS temperature-sensitivity slopes, trend sites only (>=10 passing years, n=67)."
)

message("Temperature sensitivity: pooled mixed models, by third and by group ...")
ts_pooled <- purrr::map_dfr(TS_COMBOS, function(cb) {
  purrr::map_dfr(names(SUBSET_DEFS), function(sn) {
    d <- subset_filter(ts_data, sn)
    bind_cols(tibble(combo = cb$label, subset = sn), fit_pooled_slope(d, cb$x, cb$y))
  })
})
write_csv(ts_pooled, file.path(EXP_TABLES, "temperature_sensitivity_pooled.csv"))
write_meta(
  file.path(EXP_TABLES, "temperature_sensitivity_pooled.csv"),
  input_sources = file.path(EXP_TABLES, "temperature_sensitivity_data.csv"),
  notes = paste0(
    "Pooled slope (days/degC) of GSL/start-day/end-day on annual mean TA_F (GSL) or the 60-day ",
    "pre-window mean TA_F (start/end), by skewness third and functional group, random site ",
    "intercept+slope (nlme::lme), REML. ", sum(!ts_pooled$converged), " of ", nrow(ts_pooled),
    " did not converge."
  )
)
message("Saved: temperature_sensitivity_*.csv. Non-converged: ", sum(!ts_pooled$converged), " of ", nrow(ts_pooled))

message("--- 03_models.R Steps A-B done ---")

## ============================================================================
## Step C: trends -- per-site OLS (as before, 07_fig3.R pattern) and pooled
## mixed models (random site slopes + AR(1) residuals) by third and group,
## across the 5-20% of P99 sweep (1% steps) and at T95. Years needed to
## detect each pooled trend: Weatherhead et al. (1998), 95%/80% power.
## ============================================================================
options(gsl.skip_driver = TRUE)
source("GSL_definition/code/03_derive.R") # year_threshold_matrices(), reused directly
options(gsl.skip_driver = FALSE)

SWEEP_PCT <- seq(5, 20, by = 1) # 16 points; coarser than the 0.01%-step fine grid
                                 # (used for sensitivity, not trends) because pooled
                                 # lme(..., correlation = corAR1(...)) fits are far more
                                 # expensive than a per-site lm() -- documented in methods_memo.
P99_by_site <- setNames(derived_site$P99, derived_site$site_id)

trend_sweep_one <- function(sid) {
  d <- daily_extract |> filter(site_id == sid) |> mutate(gpp_use = pmax(GPP_NT_REF, 0))
  gpp_by_year <- split(d$gpp_use, as.character(d$year))
  gpp_by_year <- gpp_by_year[as.character(site_years_list[[sid]])]
  thresholds_abs <- SWEEP_PCT / 100 * P99_by_site[[sid]]
  m <- year_threshold_matrices(gpp_by_year, thresholds_abs)
  gsl <- m$gsl
  colnames(gsl) <- paste0("pct", SWEEP_PCT)
  as_tibble(gsl) |> mutate(site_id = sid, year = as.integer(rownames(gsl))) |>
    pivot_longer(starts_with("pct"), names_to = "threshold_name", values_to = "gsl",
                 names_transform = list(threshold_name = ~ sub("pct", "", .x))) |>
    mutate(threshold_pct = as.numeric(threshold_name), threshold_name = paste0(threshold_name, "pct"))
}

message("Trend sweep (5-20% of P99, 1% steps) for ", length(trend_site_ids), " trend sites ...")
trend_sweep <- purrr::map_dfr(trend_site_ids, trend_sweep_one)

t95_sweep <- derived_site_year |>
  filter(site_id %in% trend_site_ids) |>
  select(site_id, year, gsl = gsl_t95) |>
  mutate(threshold_name = "t95pct", threshold_pct = NA_real_)

trend_sweep_all <- bind_rows(trend_sweep, t95_sweep) |>
  left_join(site_meta, by = "site_id")

write_csv(trend_sweep_all, file.path(EXP_TABLES, "trend_sweep.csv"))
write_meta(
  file.path(EXP_TABLES, "trend_sweep.csv"),
  input_sources = file.path(EXP_DATA, "daily_extract.rds"),
  notes = "Per trend-site-year GSL at 16 common thresholds (5-20% of P99, 1% steps) and at each site's own T95, for the trend models below. Trend sites = expanded set's own >=10-year sites (n=67), not the 62-site project's fixed 21."
)

## ---- Per-site OLS trend + sign/significance changes (07_fig3.R pattern) --
message("Per-site OLS trends ...")
trend_per_site <- trend_sweep_all |>
  group_by(site_id, threshold_name, threshold_pct) |>
  group_modify(~ ols_site(.x, "year", "gsl")) |>
  ungroup() |>
  rename(p_value_trend = p_value)
write_csv(trend_per_site, file.path(EXP_TABLES, "trend_per_site.csv"))
write_meta(
  file.path(EXP_TABLES, "trend_per_site.csv"),
  input_sources = file.path(EXP_TABLES, "trend_sweep.csv"),
  notes = "Per-site OLS trend of GSL on year, at 16 common thresholds (5-20% of P99) and at T95, for the 67 expanded trend sites."
)

sign_sig <- trend_per_site |>
  filter(!is.na(threshold_pct)) |>
  group_by(site_id) |>
  summarise(sign_changes = n_distinct(sign(slope), na.rm = TRUE) > 1,
            sig_changes = n_distinct(p_value_trend < 0.05, na.rm = TRUE) > 1, .groups = "drop")
message(sum(sign_sig$sign_changes), " of ", nrow(sign_sig), " trend sites have a sign change across 5-20% of P99; ",
        sum(sign_sig$sig_changes), " have a significance change.")

## ---- Pooled mixed-model trends (random site slopes + AR(1)), by third
## and by group, at each threshold -- plus Weatherhead years-to-detect. ----
message("Pooled trend models (", length(unique(trend_sweep_all$threshold_name)), " thresholds x ",
        length(SUBSET_DEFS), " subsets) -- this takes a few minutes (AR1 residuals)...")

noise_by_site_threshold <- trend_sweep_all |>
  group_by(site_id, threshold_name) |>
  group_modify(~ site_noise_params(.x, "gsl")) |>
  ungroup()

trend_pooled <- purrr::map_dfr(unique(trend_sweep_all$threshold_name), function(th) {
  d_th <- trend_sweep_all |> filter(threshold_name == th)
  purrr::map_dfr(names(SUBSET_DEFS), function(sn) {
    d <- subset_filter(d_th, sn)
    fit <- fit_pooled_slope(d, "year", "gsl", use_ar1 = TRUE)
    noise <- noise_by_site_threshold |> filter(threshold_name == th, site_id %in% unique(d$site_id))
    yrs_needed <- if (isTRUE(fit$converged) && !is.na(fit$slope) && fit$slope != 0) {
      vapply(seq_len(nrow(noise)), function(i)
        weatherhead_years(noise$sigma_n[i], noise$phi[i], fit$slope), numeric(1))
    } else NA_real_
    bind_cols(tibble(threshold_name = th, subset = sn), fit,
              tibble(years_to_detect_median = suppressWarnings(median(yrs_needed, na.rm = TRUE)),
                     years_to_detect_n_sites = sum(!is.na(yrs_needed))))
  })
})

n_nonconv <- sum(!trend_pooled$converged)
write_csv(trend_pooled, file.path(EXP_TABLES, "trend_pooled.csv"))
write_meta(
  file.path(EXP_TABLES, "trend_pooled.csv"),
  input_sources = c(file.path(EXP_TABLES, "trend_sweep.csv")),
  notes = paste0(
    "Pooled trend of GSL on year (days/yr), random site intercept+slope + AR(1) residuals ",
    "(nlme::lme, corAR1), by skewness third and functional group, at 16 common thresholds ",
    "(5-20% of P99) and at each site's own T95. years_to_detect_median = median, across that ",
    "subset's sites, of the Weatherhead et al. (1998) years-to-detect-this-POOLED-trend-at-95%-",
    "confidence/80%-power, each using the pooled slope with that SITE'S OWN residual SD and ",
    "lag-1 autocorrelation (not the site's own trend estimate). ", n_nonconv, " of ", nrow(trend_pooled),
    " model(s) did not converge even with the random-intercept-only fallback -- NA, not dropped."
  )
)
message("Saved: trend_sweep.csv, trend_per_site.csv, trend_pooled.csv. Non-converged: ",
        n_nonconv, " of ", nrow(trend_pooled))
if (n_nonconv > 0) {
  print(trend_pooled |> filter(!converged) |> select(threshold_name, subset, n_sites, note))
}

message("--- 03_models.R: done ---")
