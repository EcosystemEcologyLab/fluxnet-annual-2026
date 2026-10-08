## 06_build_site_years.R — Stage 2. Per-site product choice, the P_ERA
## integrity check, the net-radiation gap-fill fit, and the completeness
## (years-dropped) screen. All run once per site BEFORE the Zhou et al. 2015
## screens in 07_apply_screens.R, because each later step depends on one of
## these: the rain screen needs a checked P_ERA and a filled NETRAD series,
## the daylight screen needs the filled NETRAD series, and the metrics need
## to know which NEE/GPP/RECO product a site is on and which of its years
## even qualify.
##
## MY DECISION 2 (VUT where available, CUT where not): the exact per-site
## rule in R/site_annual_fluxes.R / .compute_site_annual_fluxes_core() --
## "VUT if the site has ANY non-NA NEE_VUT_REF_QC, else CUT if it has ANY
## non-NA NEE_CUT_REF_QC, else ungated" -- applied here PER SITE (not per
## site-year) against the stage-2 sub-daily data, exactly as that function
## applies it per site against the annual data. That R file is read, not
## modified or called (it operates on annual DuckDB rows; this is sub-daily
## data read fresh in 05_read_subdaily_wue.R). NEE, its QC flag, and GPP all
## come from the chosen product for that site -- RECO_NT is carried along
## from the same product too, for completeness, though no WUE metric here
## uses RECO.
##
## MY DECISION 5 (drop incomplete site-years): a site-year is dropped if
## fewer than 80% of its calendar-year sub-daily timesteps have a non-NA
## nighttime GPP (from the chosen product), or if the year is 2026. This is
## a DATA-COMPLETENESS screen over the full record, independent of the Zhou
## rain/quality/daylight/day-level screens in 07 (which operate on days, not
## whole years, and run only on the years this script keeps).
##
## DATA: before using P_ERA, its per-site mean annual sum over the years in
## this pilot's own sub-daily record ("tower years") must match
## review/diagnostics/precip_site_filter/table_1_site_level_precip_estimates.csv's
## p_era_mean_mm_tower_years within 2%. A failure here is a hard stop() --
## unlike the resilience pattern in 01_download_extract.R, a P_ERA unit or
## join error silently propagating into every later screen and metric is
## worse than stopping the whole run.
##
## NETRAD gap-fill: where NETRAD is missing, estimate it from this site's own
## SW_IN_F via a per-site OLS fit (both present, any QC -- this is a
## magnitude fit, not a QC-gated analysis). The filled series (observed where
## present, fitted prediction where not) is used for daily PET, the daily
## mean "net radiation" feeding the rain screen, and the negative-NETRAD
## daylight screen in 07. netrad_estimated flags every record resting on the
## fitted value, carried through to the day level as "share of valid days
## resting on estimated net radiation" in wue_annual.csv.
##
## Output (data/processed/, gitignored):
##   wue_augmented/<site_id>.rds   one row per sub-daily record, chosen-
##                                 product NEE/GPP/RECO, filled NETRAD +
##                                 estimate flag, ET (LE_F_MDS / lambda(TA_F))
## Output (tables/, git-tracked):
##   site_product.csv        site, product, n_vut_qc, n_cut_qc, resolution
##   netrad_fits.csv         site, n, slope, intercept, r_squared
##   p_era_check.csv         site, n_years, mean_annual_mm_here,
##                           mean_annual_mm_reference, pct_diff, status
##   years_dropped.csv       site, year, completeness, reason

source("WUE/isotope_pilot/code/00_config.R")
source("R/units.R")

processed_dir  <- file.path(WUE_ROOT, "data", "processed")
subdaily_dir   <- file.path(processed_dir, "subdaily_wue")
augmented_dir  <- file.path(processed_dir, "wue_augmented")
tables_dir     <- file.path(WUE_ROOT, "tables")
dir.create(augmented_dir, recursive = TRUE, showWarnings = FALSE)

read_status <- readr::read_csv(file.path(processed_dir, "read_status_wue.csv"), show_col_types = FALSE)

load_site <- function(site) {
  p <- file.path(subdaily_dir, paste0(site, ".rds"))
  if (!file.exists(p)) return(NULL)
  readRDS(p)
}

seconds_per_timestep <- function(resolution) {
  if (identical(resolution, "HH")) 1800L
  else if (identical(resolution, "HR")) 3600L
  else NA_integer_
}

## ---- 1. Per-site product choice (NEE/GPP/RECO from the same product) -----

choose_product <- function(d) {
  n_vut <- if ("NEE_VUT_REF_QC" %in% names(d)) sum(!is.na(d$NEE_VUT_REF_QC)) else 0L
  n_cut <- if ("NEE_CUT_REF_QC" %in% names(d)) sum(!is.na(d$NEE_CUT_REF_QC)) else 0L
  product <- if (n_vut > 0L) "VUT" else if (n_cut > 0L) "CUT" else NA_character_
  list(product = product, n_vut = n_vut, n_cut = n_cut)
}

site_product_rows <- lapply(WUE_SITES_STAGE2, function(site) {
  d <- load_site(site)
  res <- read_status$resolution[read_status$site_id == site]
  res <- if (length(res) == 0) NA_character_ else res[[1]]
  if (is.null(d) || nrow(d) == 0) {
    return(data.frame(site_id = site, product = NA_character_, n_vut_qc = 0L,
                       n_cut_qc = 0L, resolution = res, stringsAsFactors = FALSE))
  }
  ch <- choose_product(d)
  data.frame(site_id = site, product = ch$product, n_vut_qc = ch$n_vut,
             n_cut_qc = ch$n_cut, resolution = res, stringsAsFactors = FALSE)
})
site_product <- do.call(rbind, site_product_rows)
write.csv(site_product, file.path(tables_dir, "site_product.csv"), row.names = FALSE)

no_product_sites <- site_product$site_id[is.na(site_product$product)]
if (length(no_product_sites) > 0) {
  warning("[WUE] Site(s) with neither NEE_VUT_REF_QC nor NEE_CUT_REF_QC present -- ",
          "cannot gate NEE/GPP at all: ", paste(no_product_sites, collapse = ", "))
}
message("[WUE] Product choice (per site, same rule as R/site_annual_fluxes.R):")
message(paste(capture.output(print(site_product, row.names = FALSE)), collapse = "\n"))

## ---- 2. P_ERA integrity check (hard stop on failure) ----------------------

precip_ref_path <- "review/diagnostics/precip_site_filter/table_1_site_level_precip_estimates.csv"
if (!file.exists(precip_ref_path)) {
  stop("[WUE] ", precip_ref_path, " not found -- cannot run the required P_ERA check.")
}
precip_ref <- readr::read_csv(precip_ref_path, show_col_types = FALSE)

p_era_check_rows <- lapply(WUE_SITES_STAGE2, function(site) {
  d <- load_site(site)
  if (is.null(d) || nrow(d) == 0 || !all(c("TIMESTAMP_START", "P_ERA") %in% names(d))) {
    return(data.frame(site_id = site, n_years = 0L, mean_annual_mm_here = NA_real_,
                       mean_annual_mm_reference = NA_real_, pct_diff = NA_real_,
                       status = "no_data", stringsAsFactors = FALSE))
  }
  yr <- lubridate::year(d$TIMESTAMP_START)
  annual_sum <- stats::aggregate(P_ERA ~ yr, data = data.frame(P_ERA = d$P_ERA, yr = yr),
                                  FUN = sum, na.rm = TRUE)
  mean_here <- mean(annual_sum$P_ERA)
  ref_row <- precip_ref[precip_ref$site_id == site, , drop = FALSE]
  ref_val <- if (nrow(ref_row) == 0) NA_real_ else ref_row$p_era_mean_mm_tower_years[[1]]
  pct_diff <- if (is.na(ref_val) || ref_val == 0) NA_real_ else 100 * (mean_here - ref_val) / ref_val
  status <- if (is.na(pct_diff)) "no_reference" else if (abs(pct_diff) <= 2) "ok" else "FAIL"
  data.frame(site_id = site, n_years = nrow(annual_sum), mean_annual_mm_here = round(mean_here, 2),
             mean_annual_mm_reference = ref_val, pct_diff = round(pct_diff, 3),
             status = status, stringsAsFactors = FALSE)
})
p_era_check <- do.call(rbind, p_era_check_rows)
write.csv(p_era_check, file.path(tables_dir, "p_era_check.csv"), row.names = FALSE)
message("[WUE] P_ERA check:")
message(paste(capture.output(print(p_era_check, row.names = FALSE)), collapse = "\n"))

failed_sites <- p_era_check$site_id[p_era_check$status == "FAIL"]
if (length(failed_sites) > 0) {
  stop("[WUE] P_ERA check FAILED for: ", paste(failed_sites, collapse = ", "),
       " -- mean annual sub-daily P_ERA sum differs from ",
       "review/diagnostics/precip_site_filter/table_1_site_level_precip_estimates.csv's ",
       "p_era_mean_mm_tower_years by more than 2%. See tables/p_era_check.csv. ",
       "Stopping per instructions rather than using unverified P_ERA.")
}

## ---- 3. Per-site NETRAD gap-fill fit + ET (LE_F_MDS / lambda(TA_F)) -------

## Latent heat of vaporisation as a function of air temperature (FAO-56 /
## Allen et al. 1998 Eq. for lambda(T), T in degC): lambda = 2.501 - 0.002361*T
## [MJ/kg]. This is a deliberate departure from R/units.R's fixed
## lambda = 2.45e6 J/kg (used for the Annual Paper's fluxnet_convert_units())
## -- the user's instruction for this pilot is explicitly temperature-
## dependent. GPP still goes through fluxnet_convert_units() (umol CO2 m-2
## s-1 -> gC m-2 per timestep needs no temperature dependence), but ET is
## computed directly here instead.
lambda_MJ_per_kg <- function(ta_degc) 2.501 - 0.002361 * ta_degc

netrad_fit_rows <- list()

augment_one_site <- function(site) {
  d <- load_site(site)
  if (is.null(d) || nrow(d) == 0) return(NULL)

  res <- read_status$resolution[read_status$site_id == site]
  res <- if (length(res) == 0) NA_character_ else res[[1]]
  spt <- seconds_per_timestep(res)
  if (is.na(spt)) stop("[WUE] ", site, ": could not determine HH/HR resolution -- cannot convert units.")

  prod <- site_product$product[site_product$site_id == site]
  if (length(prod) == 0 || is.na(prod)) {
    warning("[WUE] ", site, ": no usable NEE product -- excluded from stage 2 entirely.")
    return(NULL)
  }

  d$NEE_sel    <- if (prod == "VUT") d$NEE_VUT_REF    else d$NEE_CUT_REF
  d$NEE_QC_sel <- if (prod == "VUT") d$NEE_VUT_REF_QC else d$NEE_CUT_REF_QC
  d$GPP_NT_sel_umol <- if (prod == "VUT") d$GPP_NT_VUT_REF else d$GPP_NT_CUT_REF
  d$RECO_NT_sel_umol <- if (prod == "VUT") d$RECO_NT_VUT_REF else d$RECO_NT_CUT_REF
  d$product <- prod
  d$resolution <- res

  ## GPP: umol CO2 m-2 s-1 -> gC m-2 per timestep, via fluxnet_convert_units()
  ## (molar mass of C = 12 g/mol -- not CO2), same formula the Annual Paper
  ## uses at HH/HR. Built as a one-column data frame so VPD/TA/LE in `d`
  ## are untouched (this pilot keeps VPD in hPa, not fluxnet_convert_units()'s
  ## kPa -- see 00. units note in docs/methods_memo.md).
  manifest_hh <- data.frame(temporal_resolution = res)
  gpp_conv <- fluxnet_convert_units(
    data.frame(GPP_NT_sel = d$GPP_NT_sel_umol), manifest_hh
  )
  d$GPP_gC_sel <- gpp_conv$GPP_NT_sel

  ## ET: LE_F_MDS (W m-2) / lambda(TA_F) (MJ kg-1), times seconds per
  ## timestep, times 1e-6 to convert W (J/s) to MJ/s -- gives kg m-2 per
  ## timestep = mm per timestep (1 kg water over 1 m2 = 1 mm depth).
  lambda <- lambda_MJ_per_kg(d$TA_F)
  d$ET_mm <- d$LE_F_MDS * spt * 1e-6 / lambda

  ## NETRAD gap-fill: per-site OLS of NETRAD ~ SW_IN_F on paired non-NA
  ## records (any QC -- magnitude fit only).
  fit_data <- d[!is.na(d$NETRAD) & !is.na(d$SW_IN_F), c("NETRAD", "SW_IN_F")]
  n_fit <- nrow(fit_data)
  if (n_fit >= 30L) {
    fit <- stats::lm(NETRAD ~ SW_IN_F, data = fit_data)
    s <- summary(fit)
    slope <- unname(stats::coef(fit)[2]); intercept <- unname(stats::coef(fit)[1])
    r2 <- s$r.squared
    predicted <- intercept + slope * d$SW_IN_F
  } else {
    slope <- NA_real_; intercept <- NA_real_; r2 <- NA_real_
    predicted <- rep(NA_real_, nrow(d))
    warning("[WUE] ", site, ": only ", n_fit, " paired NETRAD/SW_IN_F records -- ",
            "too thin to fit; NETRAD_filled will be NA wherever NETRAD itself is NA.")
  }
  netrad_fit_rows[[length(netrad_fit_rows) + 1L]] <<- data.frame(
    site_id = site, n = n_fit, slope = slope, intercept = intercept, r_squared = r2,
    stringsAsFactors = FALSE
  )

  d$netrad_estimated <- is.na(d$NETRAD)
  d$NETRAD_filled <- ifelse(is.na(d$NETRAD), predicted, d$NETRAD)

  d[, c(
    "TIMESTAMP_START", "TIMESTAMP_END", "NIGHT",
    "SW_IN_F", "SW_IN_F_QC", "NETRAD", "NETRAD_filled", "netrad_estimated",
    "TA_F", "TA_F_QC", "VPD_F", "VPD_F_QC", "PA_F", "P_F", "P_F_QC", "P_ERA",
    "NEE_sel", "NEE_QC_sel", "GPP_NT_sel_umol", "GPP_gC_sel", "RECO_NT_sel_umol",
    "LE_F_MDS", "LE_F_MDS_QC", "ET_mm", "H_F_MDS", "H_F_MDS_QC",
    "product", "resolution"
  )]
}

for (site in WUE_SITES_STAGE2) {
  out_path <- file.path(augmented_dir, paste0(site, ".rds"))
  message("[WUE] Augmenting ", site, " ...")
  dat <- augment_one_site(site)
  if (!is.null(dat)) saveRDS(dat, out_path)
}

netrad_fits <- do.call(rbind, netrad_fit_rows)
write.csv(netrad_fits, file.path(tables_dir, "netrad_fits.csv"), row.names = FALSE)
message("[WUE] NETRAD ~ SW_IN_F fits:")
message(paste(capture.output(print(netrad_fits, row.names = FALSE)), collapse = "\n"))

## ---- 4. Completeness screen -> years_dropped.csv --------------------------
## MY DECISION 5: drop a site-year if <80% of its calendar-year sub-daily
## timesteps have non-NA nighttime GPP (chosen product), or if year == 2026.

timesteps_per_year <- function(resolution, year) {
  per_day <- if (identical(resolution, "HH")) 48L else if (identical(resolution, "HR")) 24L else NA_integer_
  days <- if (lubridate::leap_year(year)) 366L else 365L
  per_day * days
}

years_dropped_rows <- list()

for (site in WUE_SITES_STAGE2) {
  p <- file.path(augmented_dir, paste0(site, ".rds"))
  if (!file.exists(p)) next
  d <- readRDS(p)
  if (nrow(d) == 0) next
  d$year <- lubridate::year(d$TIMESTAMP_START)
  res <- d$resolution[[1]]

  by_year <- stats::aggregate(
    !is.na(GPP_NT_sel_umol) ~ year, data = d, FUN = sum
  )
  names(by_year) <- c("year", "n_gpp_present")

  for (i in seq_len(nrow(by_year))) {
    yr <- by_year$year[i]
    expected <- timesteps_per_year(res, yr)
    completeness <- round(by_year$n_gpp_present[i] / expected, 4)
    if (yr == 2026L) {
      years_dropped_rows[[length(years_dropped_rows) + 1L]] <- data.frame(
        site_id = site, year = yr, completeness = completeness,
        reason = "year 2026 (current, incomplete by instruction)", stringsAsFactors = FALSE
      )
    } else if (completeness < 0.80) {
      years_dropped_rows[[length(years_dropped_rows) + 1L]] <- data.frame(
        site_id = site, year = yr, completeness = completeness,
        reason = "completeness < 0.80 (nighttime GPP, chosen product)", stringsAsFactors = FALSE
      )
    }
  }
}
years_dropped <- if (length(years_dropped_rows) > 0) do.call(rbind, years_dropped_rows) else {
  data.frame(site_id = character(0), year = integer(0), completeness = numeric(0),
             reason = character(0))
}
write.csv(years_dropped, file.path(tables_dir, "years_dropped.csv"), row.names = FALSE)
message("[WUE] Years dropped (completeness < 0.80 or year 2026):")
message(paste(capture.output(print(years_dropped, row.names = FALSE)), collapse = "\n"))

message("[WUE] 06_build_site_years.R complete.")
