## 08_extract_statistics.R — Stage 2 step 6: X-BASE extraction and statistics.
##
## FLUXCOM-X-BASE is non-Shuttle comparison data (CLAUDE.md Hard Rule 1).
##
## For each site (STAGE2_ALL_SITES: 5 agreed + 2 pending Ossenatou, see
## 00_config.R), extracts the FLUXCOM-X-BASE 0.5deg monthly grid cell
## containing the tower, for every year where the grid (2001-2021, confirmed
## from the downloaded files -- not the paper-text 2001-2020 headline; see
## docs/report_back_2026-11-03.md) overlaps the tower's QC-qualifying record
## (tables/site_variable_qc_overlap_mm.csv, from 06_tower_data_update.R).
##
## Variable handling (Stage 2 step 4):
##   - NEE: compared directly. Sign convention checked empirically against a
##     known strong-sink site (GH-Ank, tropical evergreen forest): X-BASE NEE
##     is negative there in every month of 2001, GPP is positive -- the same
##     atmospheric convention as the tower's NEE_VUT_REF/NEE_CUT_REF (negative
##     = net uptake by the ecosystem, positive = net release). No sign flip
##     needed.
##   - GPP: tower GPP_NT_VUT_REF/GPP_NT_CUT_REF (NOT DT) -- X-BASE's own ICOS
##     metadata states its GPP training targets are nighttime-partitioned
##     (see 07_training_site_check.R / docs/report_back_2026-11-03.md). GH-Ank
##     has NO NT-partitioned GPP/RECO in its FLUXMET extract at all (checked:
##     GPP_NT_VUT_REF is 100% NA there) -- GH-Ank is therefore skipped for
##     GPP/RECO only, not for NEE or LE.
##   - RECO: X-BASE has no respiration variable. Derived here as
##     GPP_xbase + NEE_xbase (both in the atmospheric sign convention above,
##     where NEE is negative during net uptake -- so this is GPP plus NEE,
##     equivalently GPP minus the uptake magnitude in months of net uptake)
##     and labelled "derived" in every output, per the Stage 2 prompt.
##   - LE: X-BASE distributes ET (mm/hr, a mean rate, not an integrated total,
##     even in the monthly file) and ET_T (transpiration, mm/hr) but no energy
##     variable. Converted to LE (W/m2) using the same latent heat of
##     vaporization as R/units.R's own LE conversion (lambda = 2.45e6 J/kg,
##     rho_water = 1000 kg/m3): LE[W/m2] = ET[mm/hr] * rho_water * lambda / 3600.
##     Compared against tower LE_F_MDS (uncorrected), per Stage 1 convention.
##   - H: X-BASE has no sensible heat variable at all. Not compared -- stated
##     in every output that touches H.
##
## Units already match without conversion at MM resolution: X-BASE carbon
## variables are gC m-2 d-1 monthly means (same as the tower's native MM
## FLUXMET unit, confirmed in both 01_observations_qc_overlap.R and by reading
## the downloaded netCDF units attribute directly) -- MM carbon values are
## compared as-is, NOT multiplied by days-in-month (that conversion, per
## R/units.R, is only for turning a MM mean-daily-rate into a period TOTAL,
## which is not what this comparison needs).
##
## Bias = product (X-BASE) minus tower, stated in every output (Stage 1/
## Ossenatou decision). Monthly statistics are the main result; annual
## statistics are a secondary look, computed only for site x flux
## combinations with >= 3 overlapping annual values (same decision).
##
## Output (git-tracked):
##   tables/xbase_monthly_values.csv   (site x year x month x flux, tower + X-BASE + bias)
##   tables/xbase_monthly_stats.csv    (site x flux: n, bias, RMSE, R2)
##   tables/xbase_annual_values.csv
##   tables/xbase_annual_stats.csv     (only site x flux with n >= 3)

source("WAFNET/fluxcom_comparison/code/00_config.R")
suppressPackageStartupMessages(library(terra))

LAMBDA_J_PER_KG <- 2.45e6   # same as R/units.R LE conversion; rho_water = 1000 kg/m3 is
                            # embedded in the "1mm over 1m2 = 1kg" identity used below

XBASE_YEARS <- 2001:2021

## Site coordinates: site_coords_for() is now shared in 00_config.R (used by
## 08/09/10) so every script reads the same BIF-derived lon/lat.

## ---- X-BASE monthly extraction at a point, one (variable, year) file -----
.xbase_cache <- new.env(parent = emptyenv())

xbase_extract_year <- function(variable, year, lon, lat) {
  key <- paste(variable, year)
  f <- file.path(FLUXCOM_XBASE_DIR, paste0(variable, "_", year, "_050_monthly.nc"))
  if (!file.exists(f)) return(rep(NA_real_, 12))
  r <- if (exists(key, envir = .xbase_cache)) get(key, envir = .xbase_cache) else {
    rr <- rast(f)[[1:12]]
    assign(key, rr, envir = .xbase_cache)
    rr
  }
  pt <- vect(data.frame(lon = lon, lat = lat), geom = c("lon", "lat"), crs = "EPSG:4326")
  as.numeric(extract(r, pt)[1, -1])
}

## ---- Tower monthly series per site, matched to X-BASE's NT partition -----
read_tower_mm <- function(site_id) {
  path <- wafnet_fluxmet_path(site_id, "MM")
  d <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  d[d == -9999] <- NA
  d
}

carbon_src_df <- utils::read.csv(file.path(FLUXCOM_ROOT, "tables", "site_carbon_source.csv"), stringsAsFactors = FALSE)
carbon_src_for <- function(site_id) carbon_src_df$carbon_src[carbon_src_df$site_id == site_id]

## ---- Build monthly comparison table ---------------------------------------
monthly_rows <- list()

for (site_id in STAGE2_ALL_SITES) {
  status <- if (site_id %in% STAGE2_PENDING_SITES) "pending Ossenatou" else "agreed"
  coords <- site_coords_for(site_id)
  src <- carbon_src_for(site_id)
  d <- read_tower_mm(site_id)
  thresh <- QC_THRESHOLD_MM

  nee_val <- if (src == "VUT") d$NEE_VUT_REF else d$NEE_CUT_REF
  nee_qc  <- if (src == "VUT") d$NEE_VUT_REF_QC else d$NEE_CUT_REF_QC
  nee_ok  <- !is.na(nee_val) & !is.na(nee_qc) & qc_passes(nee_qc, thresh)

  gpp_nt_val  <- if (src == "VUT") d$GPP_NT_VUT_REF else d$GPP_NT_CUT_REF
  reco_nt_val <- if (src == "VUT") d$RECO_NT_VUT_REF else d$RECO_NT_CUT_REF
  gpp_ok  <- nee_ok & !is.na(gpp_nt_val)
  reco_ok <- nee_ok & !is.na(reco_nt_val)

  le_ok <- !is.na(d$LE_F_MDS) & !is.na(d$LE_F_MDS_QC) & qc_passes(d$LE_F_MDS_QC, thresh)

  year  <- as.integer(substr(as.character(d$TIMESTAMP), 1, 4))
  month <- as.integer(substr(as.character(d$TIMESTAMP), 5, 6))

  for (yr in intersect(unique(year), XBASE_YEARS)) {
    xb_nee <- xbase_extract_year("NEE", yr, coords["lon"], coords["lat"])
    xb_gpp <- xbase_extract_year("GPP", yr, coords["lon"], coords["lat"])
    xb_et  <- xbase_extract_year("ET",  yr, coords["lon"], coords["lat"])
    xb_reco <- xb_gpp + xb_nee   # derived, same sign convention (see header)
    # ET[mm/hr] of water over 1 m2 is exactly 1 kg m-2 hr-1 at rho_water =
    # 1000 kg/m3 (1 mm * 1 m2 = 0.001 m3 = 1 kg) -- no separate mm->m or
    # RHO_WATER_KG_M3 factor is needed beyond that identity. /3600 converts to
    # kg m-2 s-1, then x lambda (J/kg) gives W/m2.
    xb_le  <- xb_et * LAMBDA_J_PER_KG / 3600

    for (m in 1:12) {
      i <- which(year == yr & month == m)
      if (length(i) != 1) next

      if (isTRUE(nee_ok[i]))  monthly_rows[[length(monthly_rows) + 1]] <- data.frame(
        site_id = site_id, site_status = status, year = yr, month = m, flux = "NEE",
        tower_value = nee_val[i], xbase_value = xb_nee[m], derived = FALSE)
      if (isTRUE(gpp_ok[i]))  monthly_rows[[length(monthly_rows) + 1]] <- data.frame(
        site_id = site_id, site_status = status, year = yr, month = m, flux = "GPP",
        tower_value = gpp_nt_val[i], xbase_value = xb_gpp[m], derived = FALSE)
      if (isTRUE(reco_ok[i])) monthly_rows[[length(monthly_rows) + 1]] <- data.frame(
        site_id = site_id, site_status = status, year = yr, month = m, flux = "RECO",
        tower_value = reco_nt_val[i], xbase_value = xb_reco[m], derived = TRUE)
      if (isTRUE(le_ok[i]))   monthly_rows[[length(monthly_rows) + 1]] <- data.frame(
        site_id = site_id, site_status = status, year = yr, month = m, flux = "LE",
        tower_value = d$LE_F_MDS[i], xbase_value = xb_le[m], derived = FALSE)
    }
  }
}

monthly_df <- do.call(rbind, monthly_rows)
monthly_df$bias_product_minus_tower <- monthly_df$xbase_value - monthly_df$tower_value
monthly_df$is_training_site <- monthly_df$site_id %in% c("GH-Ank", "SN-Dhr")

write.csv(monthly_df, file.path(FLUXCOM_ROOT, "tables", "xbase_monthly_values.csv"), row.names = FALSE)
write_meta(
  file.path(FLUXCOM_ROOT, "tables", "xbase_monthly_values.csv"),
  input_sources = c(
    file.path(ANNUAL_PAPER_EXTRACTED_DIR, WAFNET_SITE_DIRS[STAGE2_ALL_SITES], "*_FLUXMET_MM_*.csv (read-only)"),
    list.files(FLUXCOM_XBASE_DIR, pattern = "_050_monthly\\.nc$", full.names = TRUE)
  ),
  notes = "One row per QC-passing tower site-month (2001-2021) matched to the FLUXCOM-X-BASE 0.5deg grid cell at that site's coordinate. tower_value/xbase_value already in matching units (gC m-2 d-1 for NEE/GPP/RECO, W m-2 for LE -- no days-in-month conversion applied, X-BASE's own monthly files are already mean daily/hourly rates). RECO is derived (GPP + NEE, both X-BASE, same sign convention as NEE: negative = net uptake). GPP/RECO use the tower's NT partition (GPP_NT/RECO_NT), matching X-BASE's own nighttime-only GPP training; GH-Ank has no NT tower data and contributes no GPP/RECO rows. bias_product_minus_tower = xbase_value - tower_value."
)

## ---- Monthly statistics per site x flux -----------------------------------
r2_fn <- function(x, y) if (length(x) < 2 || stats::sd(x) == 0 || stats::sd(y) == 0) NA_real_ else stats::cor(x, y)^2
rmse_fn <- function(err) sqrt(mean(err^2))

stats_rows <- list()
for (site_id in unique(monthly_df$site_id)) {
  for (flux in unique(monthly_df$flux)) {
    sub <- monthly_df[monthly_df$site_id == site_id & monthly_df$flux == flux, ]
    if (nrow(sub) == 0) next
    stats_rows[[length(stats_rows) + 1]] <- data.frame(
      site_id = site_id,
      site_status = sub$site_status[1],
      flux = flux,
      derived = sub$derived[1],
      is_training_site = sub$is_training_site[1],
      n_months = nrow(sub),
      bias_product_minus_tower = mean(sub$bias_product_minus_tower),
      rmse = rmse_fn(sub$bias_product_minus_tower),
      r2 = r2_fn(sub$tower_value, sub$xbase_value)
    )
  }
}
monthly_stats <- do.call(rbind, stats_rows)
monthly_stats <- monthly_stats[order(monthly_stats$flux, monthly_stats$site_id), ]
write.csv(monthly_stats, file.path(FLUXCOM_ROOT, "tables", "xbase_monthly_stats.csv"), row.names = FALSE)
write_meta(
  file.path(FLUXCOM_ROOT, "tables", "xbase_monthly_stats.csv"),
  input_sources = file.path(FLUXCOM_ROOT, "tables", "xbase_monthly_values.csv"),
  notes = "Per site x flux: n_months, bias (mean of xbase_value - tower_value), RMSE (sqrt(mean(bias^2))), R2 (squared Pearson correlation between tower_value and xbase_value). This is the Stage 2 primary comparison result (monthly, not annual)."
)

## ---- Annual secondary look (>= 3 overlapping annual values only) ---------
monthly_df$days_in_month <- lubridate::days_in_month(as.Date(paste0(monthly_df$year, "-", monthly_df$month, "-01")))

annual_rows <- list()
for (site_id in unique(monthly_df$site_id)) {
  for (flux in unique(monthly_df$flux)) {
    sub <- monthly_df[monthly_df$site_id == site_id & monthly_df$flux == flux, ]
    if (nrow(sub) == 0) next
    for (yr in unique(sub$year)) {
      yr_sub <- sub[sub$year == yr, ]
      if (nrow(yr_sub) < 12) next   # require a complete calendar year
      if (flux == "LE") {
        tower_ann <- stats::weighted.mean(yr_sub$tower_value, yr_sub$days_in_month)
        xbase_ann <- stats::weighted.mean(yr_sub$xbase_value, yr_sub$days_in_month)
      } else {
        tower_ann <- sum(yr_sub$tower_value * yr_sub$days_in_month)
        xbase_ann <- sum(yr_sub$xbase_value * yr_sub$days_in_month)
      }
      annual_rows[[length(annual_rows) + 1]] <- data.frame(
        site_id = site_id, site_status = yr_sub$site_status[1], year = yr, flux = flux,
        derived = yr_sub$derived[1], is_training_site = yr_sub$is_training_site[1],
        tower_value = tower_ann, xbase_value = xbase_ann
      )
    }
  }
}
annual_df <- do.call(rbind, annual_rows)
if (!is.null(annual_df)) {
  annual_df$bias_product_minus_tower <- annual_df$xbase_value - annual_df$tower_value
  write.csv(annual_df, file.path(FLUXCOM_ROOT, "tables", "xbase_annual_values.csv"), row.names = FALSE)
  write_meta(
    file.path(FLUXCOM_ROOT, "tables", "xbase_annual_values.csv"),
    input_sources = file.path(FLUXCOM_ROOT, "tables", "xbase_monthly_values.csv"),
    notes = "One row per site x flux x calendar year with all 12 QC-passing months present (a stricter requirement than the Stage 2 monthly statistics, which use every qualifying month regardless of year completeness). Annual totals for NEE/GPP/RECO = sum(monthly_value * days_in_month); annual LE = days-in-month-weighted mean (a rate, not a total). Secondary look only -- see xbase_annual_stats.csv for which site x flux combinations reach the >=3-year threshold to be reported."
  )

  ann_stats_rows <- list()
  for (site_id in unique(annual_df$site_id)) {
    for (flux in unique(annual_df$flux)) {
      sub <- annual_df[annual_df$site_id == site_id & annual_df$flux == flux, ]
      if (nrow(sub) < 3) next   # secondary look only with >= 3 overlapping annual values
      ann_stats_rows[[length(ann_stats_rows) + 1]] <- data.frame(
        site_id = site_id, site_status = sub$site_status[1], flux = flux,
        derived = sub$derived[1], is_training_site = sub$is_training_site[1],
        n_years = nrow(sub),
        bias_product_minus_tower = mean(sub$bias_product_minus_tower),
        rmse = rmse_fn(sub$bias_product_minus_tower),
        r2 = r2_fn(sub$tower_value, sub$xbase_value)
      )
    }
  }
  annual_stats <- if (length(ann_stats_rows) > 0) do.call(rbind, ann_stats_rows) else data.frame(
    site_id = character(0), site_status = character(0), flux = character(0), derived = logical(0),
    is_training_site = logical(0), n_years = integer(0), bias_product_minus_tower = numeric(0),
    rmse = numeric(0), r2 = numeric(0)
  )
  write.csv(annual_stats, file.path(FLUXCOM_ROOT, "tables", "xbase_annual_stats.csv"), row.names = FALSE)
  write_meta(
    file.path(FLUXCOM_ROOT, "tables", "xbase_annual_stats.csv"),
    input_sources = file.path(FLUXCOM_ROOT, "tables", "xbase_annual_values.csv"),
    notes = "Secondary look: only site x flux combinations with >= 3 overlapping complete annual values (Stage 1/Ossenatou decision) are included -- other combinations are absent from this table, not reported with a thin n. Same bias/RMSE/R2 definitions as xbase_monthly_stats.csv, computed on annual instead of monthly values."
  )
} else {
  write.csv(data.frame(), file.path(FLUXCOM_ROOT, "tables", "xbase_annual_values.csv"), row.names = FALSE)
  write.csv(data.frame(), file.path(FLUXCOM_ROOT, "tables", "xbase_annual_stats.csv"), row.names = FALSE)
  write_meta(file.path(FLUXCOM_ROOT, "tables", "xbase_annual_values.csv"), input_sources = file.path(FLUXCOM_ROOT, "tables", "xbase_monthly_values.csv"), notes = "Empty: no site x flux x year had all 12 QC-passing months present.")
  write_meta(file.path(FLUXCOM_ROOT, "tables", "xbase_annual_stats.csv"), input_sources = file.path(FLUXCOM_ROOT, "tables", "xbase_annual_values.csv"), notes = "Empty: no site x flux combination reached the >=3-year threshold.")
}

cat("\n[WAFNET/fluxcom_comparison] Monthly stats (bias = X-BASE minus tower):\n")
print(monthly_stats)
cat("\n[WAFNET/fluxcom_comparison] Annual secondary stats (n_years >= 3 only):\n")
if (exists("annual_stats")) print(annual_stats) else cat("(none)\n")
cat("\nWrote tables/xbase_monthly_values.csv, tables/xbase_monthly_stats.csv, tables/xbase_annual_values.csv, tables/xbase_annual_stats.csv\n")
