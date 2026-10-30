## 07_checks_session1.R — GSL expanded analysis, checks session 1.
##
## Four read-only checks on the expanded-site run (commit a22e893), per
## logs/prompts/2026-10-30_gsl_expanded_checks_session1.md. Checks only --
## does not modify any existing script, table or figure. All new outputs go
## to GSL_definition/expanded/tables/checks_*.csv (+ .meta.json via the
## local write_meta()) and GSL_definition/expanded/docs/checks_report_*.md.
## DuckDB is not queried directly here -- everything needed is already in
## expanded/data/daily_extract.rds (dataset='FLUXMET' only, pulled by
## 01_select.R) and GSL_definition/data/daily_extract.rds (the 62-site run,
## same source restriction, pulled by 01_extract_sites.R).
##
## fit_pooled_slope()/ols_site() (Check 2 and 3) are COPIED, not sourced,
## from expanded/code/03_models.R: that script has no gsl.skip_driver guard
## and rewrites expanded/tables/*.csv unconditionally when sourced, which
## would violate "do not change any existing table" above. Copying a
## function for this reason already has one precedent in this project
## (00_config.R's local write_meta(), not R/utils.R::write_output_metadata()).

source("GSL_definition/expanded/code/00_config.R")
options(gsl.skip_driver = TRUE)
source("GSL_definition/code/03_derive.R") # g1_adjusted, year_threshold_matrices, TASH_GRID_PCT
options(gsl.skip_driver = FALSE)

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(lubridate); library(purrr); library(nlme)
})

CHK_TABLES <- EXP_TABLES
CHK_DOCS   <- EXP_DOCS
TODAY <- format(Sys.Date(), "%Y-%m-%d")

message("=== 07_checks_session1.R starting ===")

## ============================================================================
## Shared loads
## ============================================================================
exp_sites         <- read_csv(file.path(EXP_TABLES, "sites.csv"), show_col_types = FALSE)
exp_site_years    <- read_csv(file.path(EXP_TABLES, "site_years.csv"), show_col_types = FALSE)
derived_site       <- read_csv(file.path(EXP_TABLES, "derived_site.csv"), show_col_types = FALSE) |> filter(variant == "main")
derived_site_year  <- read_csv(file.path(EXP_TABLES, "derived_site_year.csv"), show_col_types = FALSE) |> filter(variant == "main")
season_boundaries  <- read_csv(file.path(EXP_TABLES, "season_boundaries.csv"), show_col_types = FALSE)
annual_inputs      <- read_csv(file.path(EXP_TABLES, "annual_inputs.csv"), show_col_types = FALSE)
cg_data            <- read_csv(file.path(EXP_TABLES, "carbon_gain_data.csv"), show_col_types = FALSE)
cg_site            <- read_csv(file.path(EXP_TABLES, "carbon_gain_per_site.csv"), show_col_types = FALSE)
cg_pooled          <- read_csv(file.path(EXP_TABLES, "carbon_gain_pooled.csv"), show_col_types = FALSE)
ts_data            <- read_csv(file.path(EXP_TABLES, "temperature_sensitivity_data.csv"), show_col_types = FALSE)
ts_pooled          <- read_csv(file.path(EXP_TABLES, "temperature_sensitivity_pooled.csv"), show_col_types = FALSE)
daily_extract      <- readRDS(file.path(EXP_DATA, "daily_extract.rds"))

site_years_list <- split(exp_site_years$year, exp_site_years$site_id)
trend_site_ids  <- names(Filter(function(y) length(y) >= 10L, site_years_list))

site_meta <- exp_sites |> select(site_id, group, location_lat) |>
  left_join(derived_site |> select(site_id, skewness, skewness_third), by = "site_id")

## ---- Copied from 03_models.R (see header note) -----------------------------
ols_site <- function(d, xcol, ycol) {
  d <- d[!is.na(d[[xcol]]) & !is.na(d[[ycol]]), ]
  if (nrow(d) < 3 || length(unique(d[[xcol]])) < 2) {
    return(tibble(slope = NA_real_, se = NA_real_, p_value = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_, n = nrow(d)))
  }
  fit <- lm(d[[ycol]] ~ d[[xcol]])
  co <- summary(fit)$coefficients
  ci <- suppressWarnings(confint(fit, level = 0.95)[2, ])
  tibble(slope = unname(co[2, 1]), se = unname(co[2, 2]), p_value = unname(co[2, 4]),
         ci_lo = unname(ci[1]), ci_hi = unname(ci[2]), n = nrow(d))
}

fit_pooled_slope <- function(d, xcol, ycol) {
  d <- d[!is.na(d[[xcol]]) & !is.na(d[[ycol]]), ]
  d$site_id <- factor(d$site_id)
  n_sites <- n_distinct(d$site_id)
  if (nrow(d) < 10 || n_sites < 3) {
    return(tibble(slope = NA_real_, se = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
                   n_sites = n_sites, n_obs = nrow(d), converged = FALSE, random_slope = NA,
                   model_tier = NA_character_, note = "too few sites/observations"))
  }
  dd <- d |> rename(.y = all_of(ycol), .x = all_of(xcol))
  try_fit <- function(random_slope, opt) {
    rand <- if (random_slope) ~ .x | site_id else ~ 1 | site_id
    ctrl <- nlme::lmeControl(maxIter = 300, msMaxIter = 300, opt = opt, msVerbose = FALSE)
    tryCatch(
      suppressWarnings(nlme::lme(.y ~ .x, random = rand, data = dd, control = ctrl, method = "REML")),
      error = function(e) NULL
    )
  }
  tiers <- list(
    list(rs = TRUE,  opt = "nlminb", label = "random slope"),
    list(rs = TRUE,  opt = "optim",   label = "random slope"),
    list(rs = FALSE, opt = "nlminb", label = "random intercept")
  )
  fit <- NULL; tier_label <- NA_character_; random_slope <- NA; tier_idx <- NA_integer_
  for (i in seq_along(tiers)) {
    t <- tiers[[i]]
    fit <- try_fit(t$rs, t$opt)
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

SUBSET_DEFS <- c(
  setNames(GSL_SKEW_THIRD_LEVELS, paste0("third_", GSL_SKEW_THIRD_LEVELS)),
  setNames(GSL_GROUP_ORDER, paste0("group_", gsub(" ", "_", GSL_GROUP_ORDER)))
)
subset_filter <- function(d, subset_name) {
  val <- SUBSET_DEFS[[subset_name]]
  if (startsWith(subset_name, "third_")) d[d$skewness_third == val & !is.na(d$skewness_third), ]
  else d[d$group == val & !is.na(d$group), ]
}

message("--- Shared loads done. ", nrow(exp_sites), " sites, ", length(trend_site_ids), " trend sites. ---")

## ============================================================================
## CHECK 1: Harvard winter GPP + DBF/MF network-wide sweep
## ============================================================================
message("=== Check 1: US-Ha1 winter GPP ===")

ha1_P99 <- derived_site$P99[derived_site$site_id == "US-Ha1"]
ha1_t95pct <- derived_site$t95_pct[derived_site$site_id == "US-Ha1"]
ha1_years <- site_years_list[["US-Ha1"]]

ha1_daily <- daily_extract |>
  filter(site_id == "US-Ha1", year %in% ha1_years) |>
  mutate(
    gpp_use = pmax(GPP_NT_REF, 0),
    month = lubridate::month(TIMESTAMP),
    is_winter = month %in% c(12, 1, 2, 3),
    above_z17 = !is.na(gpp_use) & gpp_use > 0.10 * ha1_P99,
    above_t95 = !is.na(gpp_use) & gpp_use > (ha1_t95pct / 100) * ha1_P99,
    gap_filled = !is.na(QC_REF) & QC_REF < 1 # DD-resolution QC_REF is a FRACTION (System 1,
                                               # CLAUDE.md "QC Flag Reference") -- <1 means some
                                               # underlying HH/HR records within that day were
                                               # gap-filled, not a binary per-day flag
  )

## ---- Per-year winter counts (Dec-Mar of the SAME calendar year y, per the
## Fig 1 examples: "January to March 2015" and "December 2010" are each
## reported under their own calendar year -- Dec(y) + Jan/Feb/Mar(y), not a
## cross-year Dec(y)-Mar(y+1) window). ----
ha1_winter_counts <- ha1_daily |> filter(is_winter) |>
  group_by(year) |>
  summarise(
    n_winter_days_present = sum(!is.na(gpp_use)),
    n_above_z17 = sum(above_z17, na.rm = TRUE),
    n_above_t95 = sum(above_t95, na.rm = TRUE),
    .groups = "drop"
  ) |>
  right_join(tibble(year = ha1_years), by = "year") |>
  mutate(across(starts_with("n_"), ~ coalesce(., 0L))) |>
  arrange(year)

write_csv(ha1_winter_counts, file.path(CHK_TABLES, "checks_c1_ha1_winter_counts.csv"))
write_meta(file.path(CHK_TABLES, "checks_c1_ha1_winter_counts.csv"),
  input_sources = file.path(EXP_DATA, "daily_extract.rds"),
  notes = "US-Ha1, per year: days in Dec/Jan/Feb/Mar of that SAME calendar year with gap-filled daily GPP_NT_REF (zero-floored) above 10% of P99 (Z17) and above T95's own threshold. Matches Figure 1 panel a's winter tiles.")

## ---- Day-level detail for every winter day above EITHER threshold --------
ha1_winter_detail <- ha1_daily |> filter(is_winter, above_z17 | above_t95) |>
  mutate(
    doy = lubridate::yday(TIMESTAMP),
    is_artifact = GPP_NT_REF > 0 & NEE_REF >= 0 & TA_F < 0 & GPP_DT_REF < 0.10 * ha1_P99,
    artifact_label = ifelse(is_artifact, "likely partitioning artifact", "not flagged as artifact by this rule")
  ) |>
  select(year, TIMESTAMP, doy, above_z17, above_t95, GPP_NT_REF, GPP_DT_REF, NEE_REF, TA_F,
         QC_REF, gap_filled, artifact_label) |>
  arrange(TIMESTAMP)

write_csv(ha1_winter_detail, file.path(CHK_TABLES, "checks_c1_ha1_winter_days.csv"))
write_meta(file.path(CHK_TABLES, "checks_c1_ha1_winter_days.csv"),
  input_sources = file.path(EXP_DATA, "daily_extract.rds"),
  notes = paste0(
    "US-Ha1 winter (Dec/Jan/Feb/Mar) days above Z17 and/or T95's threshold, with GPP_NT_REF, ",
    "GPP_DT_REF, NEE_REF, TA_F, QC_REF (the per-site VUT/CUT-resolved NEE QC fraction at DD ",
    "resolution) and gap_filled (QC_REF < 1). artifact_label flags GPP_NT_REF > 0 AND NEE_REF >= 0 ",
    "AND TA_F < 0 AND GPP_DT_REF < 10% of P99 (NT shows uptake the ecosystem-scale carbon balance ",
    "and the DT partitioning method both disagree with, at a sub-freezing temperature) as a likely ",
    "partitioning artifact, per the check prompt's own example rule -- not a claim about every day."
  ))

n_artifact <- sum(ha1_winter_detail$artifact_label == "likely partitioning artifact")
message(nrow(ha1_winter_detail), " US-Ha1 winter day(s) above Z17 and/or T95; ", n_artifact, " flagged as likely artifacts.")

## ---- Recompute US-Ha1 annual GSL at Z17 and T95 excluding winter days ----
ha1_gsl_recompute <- derived_site_year |>
  filter(site_id == "US-Ha1") |>
  select(year, gsl_z17_orig = gsl_z17, gsl_t95_orig = gsl_t95) |>
  left_join(ha1_winter_counts |> select(year, n_above_z17, n_above_t95), by = "year") |>
  mutate(
    gsl_z17_excl_winter = gsl_z17_orig - n_above_z17,
    gsl_t95_excl_winter = gsl_t95_orig - n_above_t95,
    delta_z17 = gsl_z17_excl_winter - gsl_z17_orig,
    delta_t95 = gsl_t95_excl_winter - gsl_t95_orig
  ) |>
  arrange(year)

trend_lm <- function(y, x) {
  if (sum(!is.na(y)) < 3 || length(unique(x)) < 2) return(NA_real_)
  unname(coef(lm(y ~ x))[2])
}
ha1_gsl_summary <- tibble(
  metric = c("mean", "sd", "trend_days_per_yr"),
  z17_orig = c(mean(ha1_gsl_recompute$gsl_z17_orig), sd(ha1_gsl_recompute$gsl_z17_orig),
               trend_lm(ha1_gsl_recompute$gsl_z17_orig, ha1_gsl_recompute$year)),
  z17_excl_winter = c(mean(ha1_gsl_recompute$gsl_z17_excl_winter), sd(ha1_gsl_recompute$gsl_z17_excl_winter),
               trend_lm(ha1_gsl_recompute$gsl_z17_excl_winter, ha1_gsl_recompute$year)),
  t95_orig = c(mean(ha1_gsl_recompute$gsl_t95_orig), sd(ha1_gsl_recompute$gsl_t95_orig),
               trend_lm(ha1_gsl_recompute$gsl_t95_orig, ha1_gsl_recompute$year)),
  t95_excl_winter = c(mean(ha1_gsl_recompute$gsl_t95_excl_winter), sd(ha1_gsl_recompute$gsl_t95_excl_winter),
               trend_lm(ha1_gsl_recompute$gsl_t95_excl_winter, ha1_gsl_recompute$year))
) |> mutate(delta_z17 = z17_excl_winter - z17_orig, delta_t95 = t95_excl_winter - t95_orig)

write_csv(ha1_gsl_recompute, file.path(CHK_TABLES, "checks_c1_ha1_gsl_recompute.csv"))
write_meta(file.path(CHK_TABLES, "checks_c1_ha1_gsl_recompute.csv"),
  input_sources = c(file.path(EXP_TABLES, "derived_site_year.csv"), file.path(CHK_TABLES, "checks_c1_ha1_winter_counts.csv")),
  notes = "Per-year US-Ha1 annual GSL at Z17/T95, original vs. recomputed with Dec/Jan/Feb/Mar days above that threshold removed from the count (subtraction, not a full re-derivation -- exact because GSL is a simple day count).")
write_csv(ha1_gsl_summary, file.path(CHK_TABLES, "checks_c1_ha1_gsl_summary.csv"))
write_meta(file.path(CHK_TABLES, "checks_c1_ha1_gsl_summary.csv"),
  input_sources = file.path(CHK_TABLES, "checks_c1_ha1_gsl_recompute.csv"),
  notes = "Mean, s.d. and OLS trend (days/yr) of US-Ha1 annual GSL at Z17/T95, original vs. winter-days-excluded.")

## ---- DBF/MF network-wide sweep: Dec-Mar (NH) / Jun-Sep (SH), GPP > 10% of
## P99 AND TA_F < 0, same calendar-year attribution rule as above. ----------
message("Check 1: DBF/MF network-wide sweep ...")
dbf_mf_sites <- exp_sites |> filter(group %in% c("deciduous forest", "mixed forest"))
P99_by_site <- setNames(derived_site$P99, derived_site$site_id)
sh_sites_all <- exp_sites$site_id[exp_sites$location_lat < 0]

dbf_mf_daily <- daily_extract |>
  filter(site_id %in% dbf_mf_sites$site_id) |>
  semi_join(exp_site_years |> filter(site_id %in% dbf_mf_sites$site_id), by = c("site_id", "year")) |>
  mutate(
    gpp_use = pmax(GPP_NT_REF, 0),
    month = lubridate::month(TIMESTAMP),
    is_sh = site_id %in% sh_sites_all,
    is_winter = ifelse(is_sh, month %in% 6:9, month %in% c(12, 1, 2, 3)),
    P99 = P99_by_site[site_id],
    above_z17_cold = !is.na(gpp_use) & gpp_use > 0.10 * P99 & !is.na(TA_F) & TA_F < 0
  )

dbf_mf_winter <- dbf_mf_daily |> filter(is_winter) |>
  group_by(site_id, year) |>
  summarise(n_winter_cold_above_z17 = sum(above_z17_cold, na.rm = TRUE), .groups = "drop") |>
  left_join(derived_site_year |> filter(site_id %in% dbf_mf_sites$site_id) |>
              select(site_id, year, gsl_z17), by = c("site_id", "year")) |>
  left_join(exp_sites |> select(site_id, group), by = "site_id") |>
  mutate(gsl_z17_excl = gsl_z17 - n_winter_cold_above_z17, delta = gsl_z17_excl - gsl_z17) |>
  filter(n_winter_cold_above_z17 > 0) |>
  arrange(desc(abs(delta)))

dbf_mf_flagged <- dbf_mf_winter |> filter(abs(delta) > 5)

write_csv(dbf_mf_winter, file.path(CHK_TABLES, "checks_c1_dbf_mf_sweep.csv"))
write_meta(file.path(CHK_TABLES, "checks_c1_dbf_mf_sweep.csv"),
  input_sources = c(file.path(EXP_DATA, "daily_extract.rds"), file.path(EXP_TABLES, "derived_site_year.csv")),
  notes = paste0(
    "Every DBF/MF site-year with >=1 day in Dec/Jan/Feb/Mar (NH) or Jun/Jul/Aug/Sep (SH, ",
    "location_lat<0) with zero-floored daily GPP_NT_REF > 10% of P99 AND TA_F < 0 degC, same ",
    "calendar-year attribution as the US-Ha1 check. delta = GSL at Z17 with those days excluded ",
    "minus the original. ", nrow(dbf_mf_flagged), " site-year(s) across ",
    n_distinct(dbf_mf_flagged$site_id), " site(s) have |delta| > 5 days (see checks_c1_dbf_mf_sweep.csv, ",
    "filter abs(delta)>5)."
  ))
message(nrow(dbf_mf_winter), " DBF/MF site-year(s) have >=1 winter cold-GPP day; ",
        nrow(dbf_mf_flagged), " exceed 5 days of GSL change, across site(s): ",
        paste(sort(unique(dbf_mf_flagged$site_id)), collapse = ", "))

message("--- Check 1 done ---")

## ============================================================================
## CHECK 2: Carbon effect of start/end dates (Figure 5b) + DBF outliers
## ============================================================================
message("=== Check 2: carbon effect of start/end dates ===")

fig5b_predictors <- c("start_z17", "start_t95", "end_z17", "end_t95")
## carbon_gain_data.csv's actual column names behind each predictor LABEL
## (same mapping as 03_models.R's CG_PREDICTORS; the "predictor" values in
## carbon_gain_pooled.csv/carbon_gain_per_site.csv are the labels, not the
## column names).
CG_PREDICTOR_COLS <- c(start_z17 = "start_doy_z17", start_t95 = "start_doy_t95",
                        end_z17 = "end_doy_z17", end_t95 = "end_doy_t95")
fig5b <- cg_pooled |>
  filter(predictor %in% fig5b_predictors, response %in% c("annual_gpp", "annual_nep")) |>
  mutate(
    definition = ifelse(grepl("z17$", predictor), "Z17", "T95"),
    which_date = ifelse(grepl("^start", predictor), "start", "end"),
    subset_type = ifelse(startsWith(subset, "third_"), "skewness third", "functional group"),
    subset_label = sub("^(third_|group_)", "", subset)
  ) |>
  select(response, which_date, definition, subset_type, subset_label, slope, se, ci_lo, ci_hi,
         n_sites, n_obs, converged, model_tier, note) |>
  arrange(response, which_date, definition, subset_type, subset_label)

write_csv(fig5b, file.path(CHK_TABLES, "checks_c2_fig5b_summary.csv"))
write_meta(file.path(CHK_TABLES, "checks_c2_fig5b_summary.csv"),
  input_sources = file.path(EXP_TABLES, "carbon_gain_pooled.csv"),
  notes = paste0(
    "Figure 5b's own numbers (start_z17/start_t95/end_z17/end_t95 x annual_gpp/annual_nep, by ",
    "skewness third and functional group), pulled unchanged from carbon_gain_pooled.csv -- no ",
    "recomputation. n_obs = site-years. Season-boundary rule (02_derive.R season_one()): start = ",
    "first day of the first run of >=5 consecutive days with zero-floored daily GPP_NT_REF above ",
    "threshold; end = last day of the last such run; NA GPP counts as below threshold; NH sites ",
    "use the calendar year, SH sites (location_lat<0) use 1 Jul(y)-30 Jun(y+1), included only ",
    "when both calendar years are in that site's own passing-year list. Model (03_models.R ",
    "fit_pooled_slope()): nlme::lme(annual_gpp_or_nep ~ start_or_end_doy, random = ~ start_or_end_doy ",
    "| site_id, method='REML'), falling back to a random-intercept-only model if the random-slope ",
    "model fails to converge (see model_tier/note columns)."
  ))
message("Check 2: Figure 5b summary written (", nrow(fig5b), " rows).")

## ---- DBF-only per-site slopes + outliers ----------------------------------
dbf_site_ids <- exp_sites$site_id[exp_sites$group == "deciduous forest"]
dbf_trend_ids <- intersect(dbf_site_ids, trend_site_ids)
message(length(dbf_site_ids), " DBF site(s) total, ", length(dbf_trend_ids), " DBF trend site(s) (>=10 yr).")

dbf_per_site <- cg_site |>
  filter(site_id %in% dbf_trend_ids, predictor %in% fig5b_predictors, response == "annual_gpp") |>
  left_join(exp_sites |> select(site_id, record_span_years, n_passing_years_r2g14), by = "site_id")

outlier_flag <- function(x) {
  q <- quantile(x, c(0.25, 0.75), na.rm = TRUE, type = 7)
  iqr <- q[2] - q[1]
  x < q[1] - 1.5 * iqr | x > q[2] + 1.5 * iqr
}

dbf_per_site <- dbf_per_site |>
  group_by(predictor) |>
  mutate(is_outlier = outlier_flag(slope)) |>
  ungroup()

## Per-site diagnostics for "reason extreme, if visible": n years on record,
## whether the site's start/end date sits at the window edge in >=1 year
## (above_threshold_all_year, from season_boundaries.csv), any NA-boundary
## (missing-data) year.
site_diag <- season_boundaries |>
  filter(site_id %in% dbf_trend_ids, threshold_name %in% c("z17", "t95")) |>
  group_by(site_id, threshold_name) |>
  summarise(
    n_years_at_window_edge = sum(above_threshold_all_year, na.rm = TRUE),
    n_years_missing_boundary = sum(is.na(start_date)),
    .groups = "drop"
  ) |>
  mutate(definition = ifelse(threshold_name == "z17", "Z17", "T95"))

outlier_sites <- dbf_per_site |> filter(is_outlier) |>
  mutate(definition = as.character(ifelse(grepl("z17$", predictor), "Z17", "T95"))) |>
  left_join(site_diag, by = c("site_id", "definition")) |>
  mutate(reason = case_when(
    n < 7 ~ paste0("short record (n=", n, " site-years in the OLS)"),
    n_years_missing_boundary > 0 ~ paste0(n_years_missing_boundary, " year(s) with no qualifying season boundary (missing-data year)"),
    n_years_at_window_edge > 0 ~ paste0(n_years_at_window_edge, " year(s) above threshold all year (start/end pinned to window edge)"),
    TRUE ~ "not visible from n/window-edge/missing-boundary diagnostics alone"
  )) |>
  select(site_id, predictor, slope, se, p_value, n, record_span_years, n_passing_years_r2g14,
         n_years_at_window_edge, n_years_missing_boundary, reason)

write_csv(dbf_per_site, file.path(CHK_TABLES, "checks_c2_dbf_per_site.csv"))
write_meta(file.path(CHK_TABLES, "checks_c2_dbf_per_site.csv"),
  input_sources = file.path(EXP_TABLES, "carbon_gain_per_site.csv"),
  notes = "Per-site OLS slope of annual GPP on start/end DOY (Z17, T95), DBF trend sites (>=10 yr) only. is_outlier = beyond 1.5*IQR of this predictor's own DBF slope distribution (Tukey rule).")
write_csv(outlier_sites, file.path(CHK_TABLES, "checks_c2_dbf_outlier_sites.csv"))
write_meta(file.path(CHK_TABLES, "checks_c2_dbf_outlier_sites.csv"),
  input_sources = c(file.path(CHK_TABLES, "checks_c2_dbf_per_site.csv"), file.path(EXP_TABLES, "season_boundaries.csv")),
  notes = "DBF per-site slope outliers (Tukey 1.5*IQR), with record length and window-edge/missing-boundary diagnostics where visible.")

dbf_summary <- dbf_per_site |>
  group_by(predictor) |>
  summarise(
    median_slope_all = median(slope, na.rm = TRUE),
    iqr_slope_all = IQR(slope, na.rm = TRUE),
    n_all = n(),
    n_outliers = sum(is_outlier),
    median_slope_no_outliers = median(slope[!is_outlier], na.rm = TRUE),
    iqr_slope_no_outliers = IQR(slope[!is_outlier], na.rm = TRUE),
    n_no_outliers = sum(!is_outlier),
    .groups = "drop"
  )

message("Check 2: refitting DBF pooled models with and without outlier sites ...")
dbf_pooled_refit <- purrr::map_dfr(fig5b_predictors, function(pn) {
  xcol <- CG_PREDICTOR_COLS[[pn]]
  outlier_ids <- outlier_sites$site_id[outlier_sites$predictor == pn]
  d_all <- cg_data |> filter(site_id %in% dbf_site_ids)
  d_no_out <- d_all |> filter(!site_id %in% outlier_ids)
  bind_rows(
    bind_cols(tibble(predictor = pn, outliers = "with outliers"), fit_pooled_slope(d_all, xcol, "annual_gpp")),
    bind_cols(tibble(predictor = pn, outliers = "excluding outlier site(s)"), fit_pooled_slope(d_no_out, xcol, "annual_gpp"))
  )
})

write_csv(dbf_summary, file.path(CHK_TABLES, "checks_c2_dbf_summary.csv"))
write_meta(file.path(CHK_TABLES, "checks_c2_dbf_summary.csv"),
  input_sources = file.path(CHK_TABLES, "checks_c2_dbf_per_site.csv"),
  notes = "Median/IQR/n of DBF per-site OLS slopes (annual GPP on start/end DOY), with and without the Tukey-outlier sites for that predictor.")
write_csv(dbf_pooled_refit, file.path(CHK_TABLES, "checks_c2_dbf_pooled_refit.csv"))
write_meta(file.path(CHK_TABLES, "checks_c2_dbf_pooled_refit.csv"),
  input_sources = c(file.path(EXP_TABLES, "carbon_gain_data.csv"), file.path(CHK_TABLES, "checks_c2_dbf_outlier_sites.csv")),
  notes = "Pooled mixed-model slope of annual GPP on start/end DOY, all DBF site-years, with vs. without the outlier site(s) for that predictor (all of that site's years removed, not just the flagged year).")

message("--- Check 2 done ---")

## ============================================================================
## CHECK 3: Southern Hemisphere season-year vs. calendar-year mismatch
## ============================================================================
message("=== Check 3: Southern Hemisphere season-year mismatch ===")

sh_sites <- exp_sites |> filter(location_lat < 0) |>
  select(site_id, igbp, group, location_lat) |>
  left_join(derived_site |> select(site_id, skewness_third), by = "site_id") |>
  left_join(exp_site_years |> count(site_id, name = "n_site_years"), by = "site_id")

write_csv(sh_sites, file.path(CHK_TABLES, "checks_c3_sh_sites.csv"))
write_meta(file.path(CHK_TABLES, "checks_c3_sh_sites.csv"),
  input_sources = file.path(EXP_TABLES, "sites.csv"),
  notes = paste0(nrow(sh_sites), " Southern Hemisphere site(s) (location_lat<0) in the expanded set, with skewness third, functional group, and total passing site-years."))
message(nrow(sh_sites), " SH site(s): ", paste(sh_sites$site_id, collapse = ", "))

## ---- Jul-Jun season-year sums/means, same windowing rule as 02_derive.R's
## Step B (requires BOTH calendar years y, y+1 in that site's own passing-
## year list) ----
daily_sh <- daily_extract |> filter(site_id %in% sh_sites$site_id) |>
  mutate(gpp_use = pmax(GPP_NT_REF, 0))

season_year_annual_one <- function(sid) {
  yrs <- site_years_list[[sid]]
  purrr::map_dfr(yrs, function(y) {
    if (!(y + 1L) %in% yrs) return(NULL)
    win <- seq(as.Date(sprintf("%d-07-01", y)), as.Date(sprintf("%d-06-30", y + 1L)), by = "day")
    d <- daily_sh |> filter(site_id == sid, TIMESTAMP %in% win)
    tibble(site_id = sid, year = y,
           annual_gpp_seasonyr = sum(d$gpp_use, na.rm = TRUE),
           annual_nep_seasonyr = sum(-d$NEE_REF, na.rm = TRUE),
           annual_mean_ta_seasonyr = mean(d$TA_F, na.rm = TRUE),
           n_days_window = nrow(d))
  })
}
sh_seasonyr <- purrr::map_dfr(sh_sites$site_id, season_year_annual_one)

sh_compare <- sh_seasonyr |>
  left_join(annual_inputs |> select(site_id, year, annual_gpp, annual_nep, annual_mean_ta), by = c("site_id", "year")) |>
  mutate(
    diff_gpp = annual_gpp_seasonyr - annual_gpp, pct_diff_gpp = 100 * diff_gpp / annual_gpp,
    diff_nep = annual_nep_seasonyr - annual_nep, pct_diff_nep = 100 * diff_nep / annual_nep,
    diff_ta = annual_mean_ta_seasonyr - annual_mean_ta, pct_diff_ta = 100 * diff_ta / annual_mean_ta
  )

write_csv(sh_compare, file.path(CHK_TABLES, "checks_c3_sh_siteyear_compare.csv"))
write_meta(file.path(CHK_TABLES, "checks_c3_sh_siteyear_compare.csv"),
  input_sources = c(file.path(EXP_DATA, "daily_extract.rds"), file.path(EXP_TABLES, "annual_inputs.csv")),
  notes = paste0(
    "Per SH site-year: annual GPP/NEP (sum) and mean TA_F over the 1 Jul(y)-30 Jun(y+1) season ",
    "year vs. the calendar-year(y) values 03_models.R actually joins to that season year's ",
    "start/end dates. Only site-years where BOTH y and y+1 pass the selection rule are included ",
    "(same windowing as 02_derive.R Step B); ", n_distinct(sh_seasonyr$site_id), " of ",
    nrow(sh_sites), " SH sites have >=1 such site-year."
  ))

sh_compare_summary <- sh_compare |>
  summarise(
    n_siteyears = n(),
    cor_gpp = cor(annual_gpp_seasonyr, annual_gpp, use = "complete.obs"),
    median_absdiff_gpp = median(abs(diff_gpp), na.rm = TRUE),
    median_pctdiff_gpp = median(abs(pct_diff_gpp), na.rm = TRUE),
    cor_nep = cor(annual_nep_seasonyr, annual_nep, use = "complete.obs"),
    median_absdiff_nep = median(abs(diff_nep), na.rm = TRUE),
    median_pctdiff_nep = median(abs(pct_diff_nep), na.rm = TRUE),
    cor_ta = cor(annual_mean_ta_seasonyr, annual_mean_ta, use = "complete.obs"),
    median_absdiff_ta = median(abs(diff_ta), na.rm = TRUE),
    median_pctdiff_ta = median(abs(pct_diff_ta), na.rm = TRUE)
  )
write_csv(sh_compare_summary, file.path(CHK_TABLES, "checks_c3_sh_compare_summary.csv"))
write_meta(file.path(CHK_TABLES, "checks_c3_sh_compare_summary.csv"),
  input_sources = file.path(CHK_TABLES, "checks_c3_sh_siteyear_compare.csv"),
  notes = "Correlation, median |absolute difference| and median |percent difference| (season-year vs. calendar-year), across all compared SH site-years, for annual GPP, annual NEP and mean TA_F.")
message("Check 3: SH compare summary -- cor(GPP)=", round(sh_compare_summary$cor_gpp, 3),
        ", cor(NEP)=", round(sh_compare_summary$cor_nep, 3), ", cor(TA)=", round(sh_compare_summary$cor_ta, 3))

## ---- Refit pooled carbon-gain (start/end predictors) and temperature-
## sensitivity (gsl_on_ta) models with season-year values substituted for SH
## site-years, for every subset containing >=1 SH site. Non-SH rows and SH
## site-years with no valid season-year window keep their original
## (calendar-year) values -- the refit isolates the effect of the join
## mismatch, it does not drop SH sites from NH-only data elsewhere. --------
message("Check 3: refitting pooled models with SH season-year values ...")

cg_data_sy <- cg_data |>
  left_join(sh_seasonyr |> select(site_id, year, annual_gpp_seasonyr, annual_nep_seasonyr), by = c("site_id", "year")) |>
  mutate(
    annual_gpp = ifelse(!is.na(annual_gpp_seasonyr), annual_gpp_seasonyr, annual_gpp),
    annual_nep = ifelse(!is.na(annual_nep_seasonyr), annual_nep_seasonyr, annual_nep)
  )
ts_data_sy <- ts_data |>
  left_join(sh_seasonyr |> select(site_id, year, annual_mean_ta_seasonyr), by = c("site_id", "year")) |>
  mutate(annual_mean_ta = ifelse(!is.na(annual_mean_ta_seasonyr), annual_mean_ta_seasonyr, annual_mean_ta))

subsets_with_sh <- names(SUBSET_DEFS)[vapply(names(SUBSET_DEFS), function(sn) {
  any(subset_filter(site_meta, sn)$site_id %in% sh_sites$site_id)
}, logical(1))]
message(length(subsets_with_sh), " subset(s) contain >=1 SH site: ", paste(subsets_with_sh, collapse = ", "))

cg_refit <- purrr::map_dfr(fig5b_predictors, function(pn) {
  xcol <- CG_PREDICTOR_COLS[[pn]]
  purrr::map_dfr(c("annual_gpp", "annual_nep"), function(yc) {
    purrr::map_dfr(subsets_with_sh, function(sn) {
      d_orig <- subset_filter(cg_data, sn)
      d_sy   <- subset_filter(cg_data_sy, sn)
      bind_rows(
        bind_cols(tibble(predictor = pn, response = yc, subset = sn, source = "original (calendar-year join)"), fit_pooled_slope(d_orig, xcol, yc)),
        bind_cols(tibble(predictor = pn, response = yc, subset = sn, source = "season-year for SH sites"), fit_pooled_slope(d_sy, xcol, yc))
      )
    })
  })
})

ts_refit <- purrr::map_dfr(c("gsl_z17", "gsl_t95"), function(yc) {
  purrr::map_dfr(subsets_with_sh, function(sn) {
    d_orig <- subset_filter(ts_data, sn)
    d_sy   <- subset_filter(ts_data_sy, sn)
    bind_rows(
      bind_cols(tibble(combo = paste0(yc, "_on_ta"), subset = sn, source = "original (calendar-year join)"), fit_pooled_slope(d_orig, "annual_mean_ta", yc)),
      bind_cols(tibble(combo = paste0(yc, "_on_ta"), subset = sn, source = "season-year for SH sites"), fit_pooled_slope(d_sy, "annual_mean_ta", yc))
    )
  })
})

cg_refit_wide <- cg_refit |> select(predictor, response, subset, source, slope, se, n_sites, n_obs) |>
  pivot_wider(names_from = source, values_from = c(slope, se, n_sites, n_obs)) |>
  mutate(delta_slope = `slope_season-year for SH sites` - `slope_original (calendar-year join)`)
ts_refit_wide <- ts_refit |> select(combo, subset, source, slope, se, n_sites, n_obs) |>
  pivot_wider(names_from = source, values_from = c(slope, se, n_sites, n_obs)) |>
  mutate(delta_slope = `slope_season-year for SH sites` - `slope_original (calendar-year join)`)

write_csv(cg_refit_wide, file.path(CHK_TABLES, "checks_c3_cg_refit.csv"))
write_meta(file.path(CHK_TABLES, "checks_c3_cg_refit.csv"),
  input_sources = c(file.path(EXP_TABLES, "carbon_gain_data.csv"), file.path(CHK_TABLES, "checks_c3_sh_siteyear_compare.csv")),
  notes = "Pooled carbon-gain slopes (start/end DOY predictors only -- gsl_z17/gsl_t95 predictors are already calendar-year-consistent, see report), subsets containing >=1 SH site, original vs. SH season-year annual GPP/NEP substituted. Main outputs (carbon_gain_pooled.csv) are untouched.")
write_csv(ts_refit_wide, file.path(CHK_TABLES, "checks_c3_ts_refit.csv"))
write_meta(file.path(CHK_TABLES, "checks_c3_ts_refit.csv"),
  input_sources = c(file.path(EXP_TABLES, "temperature_sensitivity_data.csv"), file.path(CHK_TABLES, "checks_c3_sh_siteyear_compare.csv")),
  notes = "Pooled temperature-sensitivity slopes (gsl_on_ta combos only -- the only ones using annual_mean_ta; start_on_ta/end_on_ta use the fixed 60-day pre-window, not annual_mean_ta, so are unaffected by this mismatch), subsets containing >=1 SH site, original vs. SH season-year annual_mean_ta substituted. Main outputs (temperature_sensitivity_pooled.csv) are untouched.")

message("--- Check 3 done ---")

## ============================================================================
## CHECK 4: Tash's three Figure 1 correlations (r = 0.63, 0.49, 0.34)
## ============================================================================
message("=== Check 4: Tash's Figure 1 correlations (62-site reproduction run) ===")

GSL_FLAGGED_PRODUCT_SITES_CHK <- GSL_FLAGGED_PRODUCT_SITES # FR-Mej, IT-Tor, NL-Vkp (00_config.R)

daily62 <- readRDS(file.path(GSL_ROOT, "data", "daily_extract.rds"))
daily62$TIMESTAMP <- as.Date(as.character(daily62$TIMESTAMP))
daily62$year <- lubridate::year(daily62$TIMESTAMP)

derived_site62 <- read_csv(file.path(GSL_ROOT, "tables", "derived_site.csv"), show_col_types = FALSE)
main62_zf <- derived_site62 |> filter(gpp_variant == "main", neg_gpp_variant == "main")           # zero-floored
main62_ad <- derived_site62 |> filter(gpp_variant == "main", neg_gpp_variant == "as_delivered")    # as-delivered

PCT_GRID <- TASH_GRID_PCT # 0.1, 0.5, 1:20 -- 22 values, defined in code/03_derive.R
## Tash's 22-value FIXED (absolute, gC/m2/d) grid, 0.0001-2.5. The prompt
## gives only the two endpoints, not the interior spacing -- constructed
## here as the same two-small-points-then-linear-run shape as her OWN %
## grid (0.1, 0.5, then 1:20 linear by 1), scaled to the stated endpoints:
## two small points (0.0001, 0.0005) then 20 linear points from 2.5/20 to
## 2.5 in steps of 2.5/20. This is a documented ASSUMPTION, not a value
## recovered from any source -- flagged as such in the report.
FIXED_GRID <- c(0.0001, 0.0005, seq(2.5 / 20, 2.5, by = 2.5 / 20))
stopifnot(length(PCT_GRID) == 22, length(FIXED_GRID) == 22)

gsl_metrics_one_site <- function(sid, yrs, gpp_col = "GPP_NT_VUT_REF", neg_to_zero = TRUE, P99) {
  d <- daily62 |> filter(site_id == sid, year %in% yrs)
  gpp_raw <- d[[gpp_col]]
  gpp <- if (neg_to_zero) pmax(gpp_raw, 0) else gpp_raw
  gpp_by_year <- split(gpp, as.character(d$year))

  one_grid <- function(thresholds_abs) {
    m <- year_threshold_matrices(gpp_by_year, thresholds_abs)$gsl # years x 22
    yrs_num <- as.integer(rownames(m))
    mean_v <- colMeans(m, na.rm = TRUE)
    sd_v <- apply(m, 2, sd, na.rm = TRUE)
    trend_v <- apply(m, 2, function(col) trend_lm(col, yrs_num))
    list(mean = mean_v, sd = sd_v, trend = trend_v)
  }
  list(
    pct = one_grid(PCT_GRID / 100 * P99),
    fixed = one_grid(FIXED_GRID)
  )
}

spread_fns <- list(
  IQR = function(x) IQR(x, na.rm = TRUE),
  range = function(x) diff(range(x, na.rm = TRUE)),
  sd = function(x) sd(x, na.rm = TRUE)
)

## ---- Confirm the "positive threshold -> floor doesn't matter for GSL
## counts" claim used to justify computing GSL once (not twice) per site,
## varying only P99 (pct grid) and the skewness column (correlate-against). --
.chk_site <- main62_zf$site_id[1]
.chk_yrs <- GSL_SITES$years[GSL_SITES$site_id == .chk_site][[1]]
.g_zf <- gsl_metrics_one_site(.chk_site, .chk_yrs, neg_to_zero = TRUE, P99 = main62_zf$P99[main62_zf$site_id == .chk_site])
.g_ad <- gsl_metrics_one_site(.chk_site, .chk_yrs, neg_to_zero = FALSE, P99 = main62_zf$P99[main62_zf$site_id == .chk_site]) # same P99 on purpose: isolating the floor's effect on GSL alone
stopifnot(identical(.g_zf$fixed$mean, .g_ad$fixed$mean))
message("Confirmed: at the fixed (always-positive) grid, GSL counts are identical whether negative GPP is zero-floored or as-delivered.")

run_variant <- function(variant_label, neg_to_zero) {
  P99_col <- if (neg_to_zero) main62_zf else main62_ad
  purrr::map_dfr(GSL_SITES$site_id, function(sid) {
    yrs <- GSL_SITES$years[GSL_SITES$site_id == sid][[1]]
    P99 <- P99_col$P99[P99_col$site_id == sid]
    gm <- gsl_metrics_one_site(sid, yrs, neg_to_zero = neg_to_zero, P99 = P99)
    purrr::map_dfr(c("pct", "fixed"), function(grid_name) {
      g <- gm[[grid_name]]
      purrr::map_dfr(c("mean", "sd", "trend"), function(metric) {
        v <- g[[metric]]
        purrr::map_dfr(names(spread_fns), function(sm) {
          tibble(site_id = sid, variant = variant_label, grid = grid_name, metric = metric,
                 spread_measure = sm, spread_value = spread_fns[[sm]](v))
        })
      })
    })
  })
}
message("Check 4: computing GSL metrics across both grids, both negative-GPP treatments, 62 sites ...")
spread_zf <- run_variant("zero-floored", TRUE)
spread_ad <- run_variant("as-delivered", FALSE)
spread_all <- bind_rows(spread_zf, spread_ad)

skew_zf <- main62_zf |> select(site_id, skewness)
skew_ad <- main62_ad |> select(site_id, skewness)
spread_all <- spread_all |>
  left_join(bind_rows(skew_zf |> mutate(variant = "zero-floored"), skew_ad |> mutate(variant = "as-delivered")),
            by = c("site_id", "variant")) |>
  mutate(is_flagged = site_id %in% GSL_FLAGGED_PRODUCT_SITES_CHK)

## ---- Full table: one row per (variant, grid, metric, spread_measure,
## correlation_type, site_scope) ---------------------------------------------
TARGET_R <- c(mean = 0.63, sd = 0.49, trend = 0.34)
full_table <- purrr::map_dfr(c("zero-floored", "as-delivered"), function(vv) {
  purrr::map_dfr(c("pct", "fixed"), function(gg) {
    purrr::map_dfr(c("mean", "sd", "trend"), function(mm) {
      purrr::map_dfr(names(spread_fns), function(sm) {
        purrr::map_dfr(c("all 62", "excl. flagged (59)"), function(scope) {
          d <- spread_all |> filter(variant == vv, grid == gg, metric == mm, spread_measure == sm)
          if (scope == "excl. flagged (59)") d <- d |> filter(!is_flagged)
          r_pearson <- suppressWarnings(cor(d$spread_value, d$skewness, method = "pearson", use = "complete.obs"))
          r_spearman <- suppressWarnings(cor(d$spread_value, d$skewness, method = "spearman", use = "complete.obs"))
          tibble(variant = vv, grid = gg, metric = mm, spread_measure = sm, site_scope = scope,
                 n = sum(!is.na(d$spread_value) & !is.na(d$skewness)),
                 target_r = TARGET_R[[mm]], r_pearson = r_pearson, r_spearman = r_spearman)
        })
      })
    })
  })
})

write_csv(full_table, file.path(CHK_TABLES, "checks_c4_full_table.csv"))
write_meta(file.path(CHK_TABLES, "checks_c4_full_table.csv"),
  input_sources = c(file.path(GSL_ROOT, "data", "daily_extract.rds"), file.path(GSL_ROOT, "tables", "derived_site.csv")),
  notes = paste0(
    "62-site reproduction run (GSL_SITES, main GPP_NT_VUT_REF). Per site: annual GSL at each of ",
    "22 thresholds on Tash's % of P99 grid (0.1,0.5,1-20) and on a FIXED grid (0.0001-2.5 gC/m2/d, ",
    "ASSUMED spacing -- see report); then mean, s.d. and OLS trend of annual GSL across years, ",
    "at each threshold; then the IQR/range/s.d. of that 22-value vector = 'spread'. target_r ",
    "assumes the manuscript's r=0.63/0.49/0.34 map to mean/s.d./trend spread respectively, per ",
    "the check prompt. This is the FULL table -- every variant x grid x metric x spread_measure x ",
    "scope combination, not a filtered subset."
  ))

## ---- Combination search: one row per (variant, grid, spread_measure,
## correlation_type, site_scope), giving all three target comparisons -------
combo_search <- function(tbl) {
  purrr::map_dfr(unique(tbl$variant), function(vv) {
    purrr::map_dfr(unique(tbl$grid), function(gg) {
      purrr::map_dfr(unique(tbl$spread_measure), function(sm) {
        purrr::map_dfr(unique(tbl$site_scope), function(scope) {
          purrr::map_dfr(c("r_pearson", "r_spearman"), function(rcol) {
            sub <- tbl |> filter(variant == vv, grid == gg, spread_measure == sm, site_scope == scope)
            get_r <- function(mm) sub[[rcol]][sub$metric == mm]
            r_mean_v <- get_r("mean"); r_sd_v <- get_r("sd"); r_trend_v <- get_r("trend")
            diffs <- c(r_mean_v - TARGET_R[["mean"]], r_sd_v - TARGET_R[["sd"]], r_trend_v - TARGET_R[["trend"]])
            tibble(variant = vv, grid = gg, spread_measure = sm, site_scope = scope,
                   correlation_type = sub("^r_", "", rcol),
                   r_mean = r_mean_v, r_sd = r_sd_v, r_trend = r_trend_v,
                   max_abs_diff = max(abs(diffs)), all_within_0.02 = max(abs(diffs)) <= 0.02)
          })
        })
      })
    })
  })
}
combo_table <- combo_search(full_table) |> arrange(max_abs_diff)

write_csv(combo_table, file.path(CHK_TABLES, "checks_c4_combo_search.csv"))
write_meta(file.path(CHK_TABLES, "checks_c4_combo_search.csv"),
  input_sources = file.path(CHK_TABLES, "checks_c4_full_table.csv"),
  notes = "Every (variant x grid x spread_measure x site_scope x correlation_type) combination, applied uniformly to all three metrics (mean/sd/trend), sorted by max_abs_diff from the three target r values (0.63/0.49/0.34). all_within_0.02 = TRUE means that single combination reproduces all three simultaneously within 0.02.")

n_matching <- sum(combo_table$all_within_0.02)
message("Check 4: ", n_matching, " combination(s) reproduce all three target r values within 0.02.",
        if (n_matching > 0) paste0(" Best: ", combo_table$variant[1], "/", combo_table$grid[1], "/",
                                     combo_table$spread_measure[1], "/", combo_table$site_scope[1], "/",
                                     combo_table$correlation_type[1]) else "")

## ---- Capped pct-grid variants (only meaningful if nothing above matches) --
if (n_matching == 0) {
  message("Check 4: no combination matched within 0.02 -- adding spread-capped-at-5%/10%-of-P99 pct-grid variants.")
  PCT_GRID_CAP5 <- PCT_GRID[PCT_GRID <= 5]
  PCT_GRID_CAP10 <- PCT_GRID[PCT_GRID <= 10]

  run_capped <- function(variant_label, neg_to_zero, cap_grid, cap_label) {
    P99_col <- if (neg_to_zero) main62_zf else main62_ad
    purrr::map_dfr(GSL_SITES$site_id, function(sid) {
      yrs <- GSL_SITES$years[GSL_SITES$site_id == sid][[1]]
      P99 <- P99_col$P99[P99_col$site_id == sid]
      d <- daily62 |> filter(site_id == sid, year %in% yrs)
      gpp <- if (neg_to_zero) pmax(d$GPP_NT_VUT_REF, 0) else d$GPP_NT_VUT_REF
      gpp_by_year <- split(gpp, as.character(d$year))
      m <- year_threshold_matrices(gpp_by_year, cap_grid / 100 * P99)$gsl
      yrs_num <- as.integer(rownames(m))
      mean_v <- colMeans(m, na.rm = TRUE); sd_v <- apply(m, 2, sd, na.rm = TRUE)
      trend_v <- apply(m, 2, function(col) trend_lm(col, yrs_num))
      purrr::map_dfr(c("mean", "sd", "trend"), function(metric) {
        v <- switch(metric, mean = mean_v, sd = sd_v, trend = trend_v)
        purrr::map_dfr(names(spread_fns), function(sm) {
          tibble(site_id = sid, variant = variant_label, grid = cap_label, metric = metric,
                 spread_measure = sm, spread_value = spread_fns[[sm]](v))
        })
      })
    })
  }
  spread_cap <- bind_rows(
    run_capped("zero-floored", TRUE, PCT_GRID_CAP5, "pct_capped5"),
    run_capped("zero-floored", TRUE, PCT_GRID_CAP10, "pct_capped10"),
    run_capped("as-delivered", FALSE, PCT_GRID_CAP5, "pct_capped5"),
    run_capped("as-delivered", FALSE, PCT_GRID_CAP10, "pct_capped10")
  ) |>
    left_join(bind_rows(skew_zf |> mutate(variant = "zero-floored"), skew_ad |> mutate(variant = "as-delivered")),
              by = c("site_id", "variant")) |>
    mutate(is_flagged = site_id %in% GSL_FLAGGED_PRODUCT_SITES_CHK)

  full_table_cap <- purrr::map_dfr(c("zero-floored", "as-delivered"), function(vv) {
    purrr::map_dfr(c("pct_capped5", "pct_capped10"), function(gg) {
      purrr::map_dfr(c("mean", "sd", "trend"), function(mm) {
        purrr::map_dfr(names(spread_fns), function(sm) {
          purrr::map_dfr(c("all 62", "excl. flagged (59)"), function(scope) {
            d <- spread_cap |> filter(variant == vv, grid == gg, metric == mm, spread_measure == sm)
            if (scope == "excl. flagged (59)") d <- d |> filter(!is_flagged)
            r_pearson <- suppressWarnings(cor(d$spread_value, d$skewness, method = "pearson", use = "complete.obs"))
            r_spearman <- suppressWarnings(cor(d$spread_value, d$skewness, method = "spearman", use = "complete.obs"))
            tibble(variant = vv, grid = gg, metric = mm, spread_measure = sm, site_scope = scope,
                   n = sum(!is.na(d$spread_value) & !is.na(d$skewness)),
                   target_r = TARGET_R[[mm]], r_pearson = r_pearson, r_spearman = r_spearman)
          })
        })
      })
    })
  })
  combo_table_cap <- combo_search(full_table_cap) |> arrange(max_abs_diff)

  write_csv(full_table_cap, file.path(CHK_TABLES, "checks_c4_capped_full_table.csv"))
  write_meta(file.path(CHK_TABLES, "checks_c4_capped_full_table.csv"),
    input_sources = file.path(GSL_ROOT, "data", "daily_extract.rds"),
    notes = "Same as checks_c4_full_table.csv but restricted to the pct-of-P99 grid points <=5% (pct_capped5, 7 points: 0.1,0.5,1-5) or <=10% (pct_capped10, 12 points: 0.1,0.5,1-10) -- a short version added because no full-grid combination matched within 0.02.")
  write_csv(combo_table_cap, file.path(CHK_TABLES, "checks_c4_capped_combo_search.csv"))
  write_meta(file.path(CHK_TABLES, "checks_c4_capped_combo_search.csv"),
    input_sources = file.path(CHK_TABLES, "checks_c4_capped_full_table.csv"),
    notes = "Combination search (as checks_c4_combo_search.csv) over the capped pct-grid variants only.")
  n_matching_cap <- sum(combo_table_cap$all_within_0.02)
  message("Check 4 (capped): ", n_matching_cap, " combination(s) reproduce all three target r values within 0.02.")
} else {
  combo_table_cap <- NULL
  n_matching_cap <- NA_integer_
}

message("--- Check 4 done ---")
message("=== 07_checks_session1.R: all checks done ===")
