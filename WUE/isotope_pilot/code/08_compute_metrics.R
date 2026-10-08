## 08_compute_metrics.R — Stage 2. WUE, inherent WUE (IWUE), underlying WUE
## (uWUE), and the VPD exponent k*, from the valid days 07_apply_screens.R
## produced. Units: GPP in g C m-2, ET in kg H2O m-2 (numerically identical
## to the mm H2O 07 computed it in -- 1 kg water over 1 m2 = 1 mm depth),
## VPD in hPa (native FLUXNET unit, deliberately NOT fluxnet_convert_units()'s
## kPa -- see docs/methods_memo.md).
##
## Daily (from the surviving records of a valid day, already summed/averaged
## in 07): WUE_d = GPP_d/ET_d; IWUE_d = GPP_d*VPD_d/ET_d; uWUE_d =
## GPP_d*sqrt(VPD_d)/ET_d (Zhou et al. 2015 eq. 18).
##
## Yearly (sum over a site-year's valid days): WUE_y = sum(GPP_d)/sum(ET_d);
## IWUE_y = sum(GPP_d*VPD_d)/sum(ET_d) (Zhou eq. 20); uWUE_y =
## sum(GPP_d*sqrt(VPD_d))/sum(ET_d) (Zhou eq. 19). Plus mean/SD of the daily
## metrics.
##
## k*: per site-year, at the sub-daily scale (07's wue_subdaily_valid
## records) and the daily scale (07's wue_daily_valid rows) -- the exponent
## k in [0, 1.5] (step 0.01) maximising the Pearson correlation of
## GPP*VPD^k vs ET. Zhou et al. (2015) cites Zhou et al. (2014) for this
## method, which this analysis does not have access to -- the grid-search
## implementation here is this analysis's own reading of the method
## description, not a reproduction of that paper's code.
##
## Units check: Zhou et al. (2015)'s 123 site-years gave yearly uWUE
## 3.50-15.83 (mean 9.47) g C hPa^0.5 kg H2O-1 and yearly IWUE 5.32-62.31
## (mean 33.62) g C hPa kg H2O-1. This script compares against those ranges
## and flags (does not stop on) any site-year more than an order of
## magnitude outside them.
##
## Output (tables/, git-tracked):
##   wue_daily.csv.gz   site, date, GPP_d, ET_d, VPD_d, WUE_d, IWUE_d, uWUE_d,
##                      n_records, day_netrad_estimated
##   wue_annual.csv     site, year, product, resolution, valid_days,
##                      WUE_y, IWUE_y, uWUE_y, <metric>_d_mean/_sd,
##                      k_star_subdaily, cor_at_kstar_subdaily,
##                      k_star_daily, cor_at_kstar_daily,
##                      GPP_sum, ET_sum, VPD_mean, frac_estimated_netrad

source("WUE/isotope_pilot/code/00_config.R")

processed_dir      <- file.path(WUE_ROOT, "data", "processed")
valid_dir          <- file.path(processed_dir, "wue_daily_valid")
subdaily_valid_dir <- file.path(processed_dir, "wue_subdaily_valid")
tables_dir         <- file.path(WUE_ROOT, "tables")

site_product <- readr::read_csv(file.path(tables_dir, "site_product.csv"), show_col_types = FALSE)

## ---- wue_daily.csv.gz ------------------------------------------------------

load_rds_if_exists <- function(dir, site) {
  p <- file.path(dir, paste0(site, ".rds"))
  if (!file.exists(p)) return(NULL)
  readRDS(p)
}

daily_all <- do.call(rbind, lapply(WUE_SITES_STAGE2, function(site) load_rds_if_exists(valid_dir, site)))
if (is.null(daily_all) || nrow(daily_all) == 0) {
  stop("[WUE] No valid days found for any site -- 07_apply_screens.R produced nothing to compute metrics from.")
}

daily_all$WUE_d  <- daily_all$GPP_d / daily_all$ET_d
daily_all$IWUE_d <- daily_all$GPP_d * daily_all$VPD_d / daily_all$ET_d
daily_all$uWUE_d <- daily_all$GPP_d * sqrt(daily_all$VPD_d) / daily_all$ET_d

wue_daily_out <- daily_all[, c(
  "site_id", "date", "GPP_d", "ET_d", "VPD_d", "WUE_d", "IWUE_d", "uWUE_d",
  "n_records", "day_netrad_estimated"
)]
names(wue_daily_out)[names(wue_daily_out) == "site_id"] <- "site"
readr::write_csv(wue_daily_out, file.path(tables_dir, "wue_daily.csv.gz"))
message("[WUE] wue_daily.csv.gz written: ", nrow(wue_daily_out), " valid-day rows.")

## ---- k* grid search --------------------------------------------------------

K_GRID <- seq(0, 1.5, by = 0.01)

k_star <- function(gpp, vpd, et) {
  valid <- !is.na(gpp) & !is.na(vpd) & !is.na(et) & vpd >= 0
  gpp <- gpp[valid]; vpd <- vpd[valid]; et <- et[valid]
  if (length(gpp) < 3 || stats::sd(et) == 0) return(list(k = NA_real_, cor = NA_real_))
  cors <- vapply(K_GRID, function(k) {
    x <- gpp * vpd^k
    if (stats::sd(x) == 0) return(NA_real_)
    suppressWarnings(stats::cor(x, et, method = "pearson"))
  }, numeric(1))
  if (all(is.na(cors))) return(list(k = NA_real_, cor = NA_real_))
  best <- which.max(cors)
  list(k = K_GRID[best], cor = cors[best])
}

## ---- wue_annual.csv ---------------------------------------------------------

site_years <- unique(daily_all[, c("site_id", "year")])

annual_rows <- lapply(seq_len(nrow(site_years)), function(i) {
  site <- site_years$site_id[i]
  yr   <- site_years$year[i]
  d_yr <- daily_all[daily_all$site_id == site & daily_all$year == yr, , drop = FALSE]

  GPP_sum <- sum(d_yr$GPP_d)
  ET_sum  <- sum(d_yr$ET_d)
  WUE_y  <- GPP_sum / ET_sum
  IWUE_y <- sum(d_yr$GPP_d * d_yr$VPD_d) / ET_sum
  uWUE_y <- sum(d_yr$GPP_d * sqrt(d_yr$VPD_d)) / ET_sum

  rec <- load_rds_if_exists(subdaily_valid_dir, site)
  rec_yr <- if (!is.null(rec)) rec[rec$year == yr, , drop = FALSE] else NULL
  k_sub <- if (!is.null(rec_yr) && nrow(rec_yr) > 0) {
    k_star(rec_yr$GPP_gC_sel, rec_yr$VPD_F, rec_yr$ET_mm)
  } else list(k = NA_real_, cor = NA_real_)
  k_day <- k_star(d_yr$GPP_d, d_yr$VPD_d, d_yr$ET_d)

  prod <- site_product$product[site_product$site_id == site]
  res  <- site_product$resolution[site_product$site_id == site]

  data.frame(
    site = site, year = yr,
    product = if (length(prod)) prod else NA_character_,
    resolution = if (length(res)) res else NA_character_,
    valid_days = nrow(d_yr),
    WUE_y = WUE_y, IWUE_y = IWUE_y, uWUE_y = uWUE_y,
    WUE_d_mean = mean(d_yr$WUE_d), WUE_d_sd = stats::sd(d_yr$WUE_d),
    IWUE_d_mean = mean(d_yr$IWUE_d), IWUE_d_sd = stats::sd(d_yr$IWUE_d),
    uWUE_d_mean = mean(d_yr$uWUE_d), uWUE_d_sd = stats::sd(d_yr$uWUE_d),
    k_star_subdaily = k_sub$k, cor_at_kstar_subdaily = k_sub$cor,
    k_star_daily = k_day$k, cor_at_kstar_daily = k_day$cor,
    GPP_sum = GPP_sum, ET_sum = ET_sum, VPD_mean = mean(d_yr$VPD_d),
    frac_estimated_netrad = mean(d_yr$day_netrad_estimated),
    stringsAsFactors = FALSE
  )
})
wue_annual <- do.call(rbind, annual_rows)
wue_annual <- wue_annual[order(wue_annual$site, wue_annual$year), ]
write.csv(wue_annual, file.path(tables_dir, "wue_annual.csv"), row.names = FALSE)
message("[WUE] wue_annual.csv written: ", nrow(wue_annual), " site-year rows.")

## ---- Units sanity check against Zhou et al. (2015) ranges ------------------
## uWUE_y: 3.50-15.83 (mean 9.47) g C hPa^0.5 kg H2O-1
## IWUE_y: 5.32-62.31 (mean 33.62) g C hPa kg H2O-1

zhou_uwue_range  <- c(3.50, 15.83)
zhou_iwue_range  <- c(5.32, 62.31)

check_order_of_magnitude <- function(x, ref_range, label) {
  x <- x[!is.na(x) & is.finite(x)]
  if (length(x) == 0) return(invisible(NULL))
  lo <- ref_range[1]; hi <- ref_range[2]
  too_low  <- x[x > 0 & x < lo / 10]
  too_high <- x[x > hi * 10]
  if (length(too_low) + length(too_high) > 0) {
    warning(
      "[WUE] UNITS CHECK: ", length(too_low) + length(too_high), " site-year(s) of ",
      label, " are more than an order of magnitude outside Zhou et al. (2015)'s ",
      "reported range (", lo, "-", hi, ") -- possible unit error. Range found here: ",
      round(min(x), 3), "-", round(max(x), 3), "."
    )
  } else {
    message("[WUE] Units check OK for ", label, ": range here ",
            round(min(x), 3), "-", round(max(x), 3),
            " vs. Zhou et al. (2015) ", lo, "-", hi, ".")
  }
}
check_order_of_magnitude(wue_annual$uWUE_y,  zhou_uwue_range, "uWUE_y (g C hPa^0.5 kg H2O-1)")
check_order_of_magnitude(wue_annual$IWUE_y,  zhou_iwue_range, "IWUE_y (g C hPa kg H2O-1)")

message("[WUE] 08_compute_metrics.R complete.")
