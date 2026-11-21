## 12_package_ossenatou.R — Stage 3 step 3: package the 0.05deg FLUXCOM-X-BASE extraction
## and matching tower data for Ossenatou Mamadou's own analysis.
##
## FLUXCOM-X-BASE is non-Shuttle comparison data (CLAUDE.md Hard Rule 1). This is a data
## package deliverable, not a new analysis -- no new figures beyond the one check figure in
## 13_check_figure_005.R, no interpretation beyond simple per-site/flux statistics. Stage 2
## outputs (0.5deg, tables/figures/docs/report_back_2026-11-03.md) are untouched.
##
## Input: tables/xbase_005_site_extraction_raw.csv (11_download_extract_xbase_005.R's output --
## run that script to completion, all 84 files, before running this one).
##
## Output: WAFNET/fluxcom_comparison/package_ossenatou_<date>/
##   xbase_005_monthly.csv, tower_monthly.csv, sites.csv, summary.csv, README.md, and a zip of
##   the whole folder (written by the caller/shell, see docs/report_back_<date>.md).

source("WAFNET/fluxcom_comparison/code/00_config.R")
suppressPackageStartupMessages({ library(dplyr); library(tidyr) })

STAGE3_SITES <- c("GH-Ank", "BJ-Nhu", "BJ-Bfg", "SN-Dhr", "ML-Kem", "SN-Nkr")
XBASE005_YEARS <- 2001:2021
LAMBDA_J_PER_KG <- 2.45e6   # same as 08_extract_statistics.R / R/units.R

PKG_DATE <- format(Sys.Date(), "%Y-%m-%d")
PKG_DIR <- file.path(FLUXCOM_ROOT, paste0("package_ossenatou_", PKG_DATE))
dir.create(PKG_DIR, recursive = TRUE, showWarnings = FALSE)

RAW_IN <- file.path(FLUXCOM_ROOT, "tables", "xbase_005_site_extraction_raw.csv")
if (!file.exists(RAW_IN)) stop("[12_package_ossenatou] Missing ", RAW_IN, " -- run 11_download_extract_xbase_005.R to completion first.")
raw <- utils::read.csv(RAW_IN, stringsAsFactors = FALSE)
raw <- raw[raw$site_id %in% STAGE3_SITES, ]

n_files_expected <- length(STAGE3_SITES) * 0  # not used; just documenting expectation below
n_variable_years <- length(unique(paste(raw$variable, raw$year)))
message("[12_package_ossenatou] Raw extraction rows: ", nrow(raw), " (", n_variable_years,
        " variable-years present; 84 expected at completion: GPP/NEE/ET/ET_T x 2001-2021)")
if (n_variable_years < 84) {
  message("[12_package_ossenatou] WARNING: fewer than 84 variable-years present -- Stage 3 ",
          "download/extraction (11_download_extract_xbase_005.R) has not finished. Packaging ",
          "whatever is present; xbase_005_monthly.csv will have NA for missing variable-years.")
}

## ---- site metadata: snapshot (20 September 2026) + BIF, read-only -------------------------
SNAPSHOT_PATH <- "data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv"
snap <- utils::read.csv(SNAPSHOT_PATH, stringsAsFactors = FALSE)
snap_sites <- snap[snap$site_id %in% STAGE3_SITES, ]

# Country is a documented fact, not inferred from the site ID prefix (CLAUDE.md Hard Rule 2
# is about hub/region from a prefix specifically) -- same values already published in
# WAFNET/energy_partitioning/README.md's site table and confirmed by each site's own product
# citation text (e.g. "Fluxnet Archive Product from Kelma" -> Mali, per 00_config.R's comment).
SITE_COUNTRY <- c(
  "GH-Ank" = "Ghana", "BJ-Nhu" = "Benin", "BJ-Bfg" = "Benin",
  "SN-Dhr" = "Senegal", "ML-Kem" = "Mali", "SN-Nkr" = "Senegal"
)

training_df <- utils::read.csv(file.path(FLUXCOM_ROOT, "tables", "fluxcom_x_base_training_sites_stage2.csv"), stringsAsFactors = FALSE)

cell_info <- raw |>
  dplyr::distinct(site_id, tower_lon, tower_lat, cell_lon, cell_lat, cell_dist_km)

## ---- tower monthly data, same source/VUT-CUT/QC rule as Stage 2 ---------------------------
carbon_src_df <- utils::read.csv(file.path(FLUXCOM_ROOT, "tables", "site_carbon_source.csv"), stringsAsFactors = FALSE)
carbon_src_for <- function(site_id) carbon_src_df$carbon_src[carbon_src_df$site_id == site_id]

read_tower_mm <- function(site_id) {
  path <- wafnet_fluxmet_path(site_id, "MM")
  d <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  d[d == -9999] <- NA
  d
}

tower_rows <- lapply(STAGE3_SITES, function(site_id) {
  d <- read_tower_mm(site_id)
  src <- carbon_src_for(site_id)
  year <- as.integer(substr(as.character(d$TIMESTAMP), 1, 4))
  month <- as.integer(substr(as.character(d$TIMESTAMP), 5, 6))
  keep <- year %in% XBASE005_YEARS
  d <- d[keep, ]; year <- year[keep]; month <- month[keep]
  if (nrow(d) == 0) return(NULL)

  pick <- function(vut_col, cut_col) if (src == "VUT") d[[vut_col]] else d[[cut_col]]

  nee_val <- pick("NEE_VUT_REF", "NEE_CUT_REF")
  nee_qc  <- pick("NEE_VUT_REF_QC", "NEE_CUT_REF_QC")
  gpp_dt_val  <- pick("GPP_DT_VUT_REF", "GPP_DT_CUT_REF")
  reco_dt_val <- pick("RECO_DT_VUT_REF", "RECO_DT_CUT_REF")
  gpp_nt_val  <- pick("GPP_NT_VUT_REF", "GPP_NT_CUT_REF")
  reco_nt_val <- pick("RECO_NT_VUT_REF", "RECO_NT_CUT_REF")

  thresh <- QC_THRESHOLD_MM
  nee_pass <- !is.na(nee_val) & !is.na(nee_qc) & qc_passes(nee_qc, thresh)
  # GPP_DT/RECO_DT/GPP_NT/RECO_NT have no QC column of their own at this resolution; gated on
  # the site's NEE QC column, same rule as 06_tower_data_update.R::build_overlap_table().
  gpp_dt_pass  <- nee_pass & !is.na(gpp_dt_val)
  reco_dt_pass <- nee_pass & !is.na(reco_dt_val)
  gpp_nt_pass  <- nee_pass & !is.na(gpp_nt_val)
  reco_nt_pass <- nee_pass & !is.na(reco_nt_val)
  le_pass <- !is.na(d$LE_F_MDS) & !is.na(d$LE_F_MDS_QC) & qc_passes(d$LE_F_MDS_QC, thresh)
  h_pass  <- !is.na(d$H_F_MDS)  & !is.na(d$H_F_MDS_QC)  & qc_passes(d$H_F_MDS_QC, thresh)

  data.frame(
    site_id = site_id, year = year, month = month, carbon_src = src,
    NEE_VUT_REF = nee_val, NEE_VUT_REF_QC = nee_qc, NEE_pass_qc = nee_pass,
    GPP_DT_VUT_REF = gpp_dt_val, RECO_DT_VUT_REF = reco_dt_val,
    GPP_RECO_DT_QC = nee_qc, GPP_DT_pass_qc = gpp_dt_pass, RECO_DT_pass_qc = reco_dt_pass,
    GPP_NT_VUT_REF = gpp_nt_val, RECO_NT_VUT_REF = reco_nt_val,
    GPP_RECO_NT_QC = nee_qc, GPP_NT_pass_qc = gpp_nt_pass, RECO_NT_pass_qc = reco_nt_pass,
    LE_F_MDS = d$LE_F_MDS, LE_F_MDS_QC = d$LE_F_MDS_QC, LE_pass_qc = le_pass,
    H_F_MDS = d$H_F_MDS, H_F_MDS_QC = d$H_F_MDS_QC, H_pass_qc = h_pass
  )
})
tower_df <- do.call(rbind, tower_rows)
tower_df <- tower_df[order(tower_df$site_id, tower_df$year, tower_df$month), ]

utils::write.csv(tower_df, file.path(PKG_DIR, "tower_monthly.csv"), row.names = FALSE)

## ---- xbase_005_monthly.csv: wide, all months 2001-2021, no QC filtering (this is the raw
## product, not a comparison statistic) -------------------------------------------------------
xbase_wide <- raw |>
  dplyr::select(site_id, variable, year, month, value, cell_lon, cell_lat, cell_dist_km) |>
  tidyr::pivot_wider(names_from = variable, values_from = value)

all_grid <- tidyr::expand_grid(site_id = STAGE3_SITES, year = XBASE005_YEARS, month = 1:12)
xbase_005_monthly <- all_grid |>
  dplyr::left_join(xbase_wide, by = c("site_id", "year", "month")) |>
  dplyr::left_join(cell_info, by = "site_id", suffix = c("", ".site")) |>
  dplyr::mutate(
    cell_lon = dplyr::coalesce(cell_lon, cell_lon.site),
    cell_lat = dplyr::coalesce(cell_lat, cell_lat.site),
    cell_dist_km = dplyr::coalesce(cell_dist_km, cell_dist_km.site)
  ) |>
  dplyr::select(-dplyr::ends_with(".site")) |>
  dplyr::rename(GPP_gC_m2_d1 = GPP, NEE_gC_m2_d1 = NEE, ET_mm_hr = ET, ET_T_mm_hr = ET_T) |>
  dplyr::mutate(LE_W_m2 = ET_mm_hr * LAMBDA_J_PER_KG / 3600) |>
  dplyr::arrange(site_id, year, month) |>
  dplyr::select(site_id, year, month, GPP_gC_m2_d1, NEE_gC_m2_d1, ET_mm_hr, ET_T_mm_hr, LE_W_m2,
                cell_lon, cell_lat, cell_dist_km)

utils::write.csv(xbase_005_monthly, file.path(PKG_DIR, "xbase_005_monthly.csv"), row.names = FALSE)

## ---- sites.csv ------------------------------------------------------------------------------
months_overlap <- tower_df |>
  dplyr::group_by(site_id) |>
  dplyr::summarise(months_overlap_qc_passing_nee = sum(NEE_pass_qc, na.rm = TRUE), .groups = "drop")

sites_df <- data.frame(
  site_id = STAGE3_SITES,
  site_name = snap_sites$site_name[match(STAGE3_SITES, snap_sites$site_id)],
  country = SITE_COUNTRY[STAGE3_SITES],
  igbp = snap_sites$igbp[match(STAGE3_SITES, snap_sites$site_id)],
  tower_lon = cell_info$tower_lon[match(STAGE3_SITES, cell_info$site_id)],
  tower_lat = cell_info$tower_lat[match(STAGE3_SITES, cell_info$site_id)],
  cell005_lon = cell_info$cell_lon[match(STAGE3_SITES, cell_info$site_id)],
  cell005_lat = cell_info$cell_lat[match(STAGE3_SITES, cell_info$site_id)],
  cell005_dist_km = cell_info$cell_dist_km[match(STAGE3_SITES, cell_info$site_id)],
  months_overlap_qc_passing_nee = months_overlap$months_overlap_qc_passing_nee[match(STAGE3_SITES, months_overlap$site_id)],
  in_xbase_training_set = training_df$in_fluxcom_x_base_training_set[match(STAGE3_SITES, training_df$site_id)],
  site_citation = snap_sites$product_citation[match(STAGE3_SITES, snap_sites$site_id)],
  site_doi_or_handle = paste0("https://hdl.handle.net/11676/", snap_sites$product_id[match(STAGE3_SITES, snap_sites$site_id)])
)
utils::write.csv(sites_df, file.path(PKG_DIR, "sites.csv"), row.names = FALSE)

## ---- summary.csv: bias/RMSE/R2 at 0.05deg (NT tower partition, QC-passing months only),
## alongside the Stage 2 0.5deg numbers for the same site x flux --------------------------------
r2_fn <- function(x, y) if (length(x) < 2 || stats::sd(x) == 0 || stats::sd(y) == 0) NA_real_ else stats::cor(x, y)^2
rmse_fn <- function(err) sqrt(mean(err^2))

comparison_long <- list()
for (site_id in STAGE3_SITES) {
  td <- tower_df[tower_df$site_id == site_id, ]
  xd <- xbase_005_monthly[xbase_005_monthly$site_id == site_id, ]
  m <- merge(td, xd, by = c("site_id", "year", "month"))
  m$RECO_xbase_derived <- m$GPP_gC_m2_d1 + m$NEE_gC_m2_d1   # same derivation/sign convention as Stage 2

  if (isTRUE(any(m$NEE_pass_qc))) {
    sub <- m[m$NEE_pass_qc, ]
    comparison_long[[length(comparison_long) + 1]] <- data.frame(
      site_id = site_id, flux = "NEE", derived = FALSE,
      tower_value = sub$NEE_VUT_REF, xbase_value = sub$NEE_gC_m2_d1)
  }
  if (isTRUE(any(m$GPP_NT_pass_qc))) {
    sub <- m[m$GPP_NT_pass_qc, ]
    comparison_long[[length(comparison_long) + 1]] <- data.frame(
      site_id = site_id, flux = "GPP", derived = FALSE,
      tower_value = sub$GPP_NT_VUT_REF, xbase_value = sub$GPP_gC_m2_d1)
  }
  if (isTRUE(any(m$RECO_NT_pass_qc))) {
    sub <- m[m$RECO_NT_pass_qc, ]
    comparison_long[[length(comparison_long) + 1]] <- data.frame(
      site_id = site_id, flux = "RECO", derived = TRUE,
      tower_value = sub$RECO_NT_VUT_REF, xbase_value = sub$RECO_xbase_derived)
  }
  if (isTRUE(any(m$LE_pass_qc))) {
    sub <- m[m$LE_pass_qc, ]
    comparison_long[[length(comparison_long) + 1]] <- data.frame(
      site_id = site_id, flux = "LE", derived = FALSE,
      tower_value = sub$LE_F_MDS, xbase_value = sub$LE_W_m2)
  }
}
comparison_long <- do.call(rbind, comparison_long)
comparison_long <- comparison_long[stats::complete.cases(comparison_long[, c("tower_value", "xbase_value")]), ]

stats005_rows <- list()
for (site_id in unique(comparison_long$site_id)) {
  for (flux in unique(comparison_long$flux[comparison_long$site_id == site_id])) {
    sub <- comparison_long[comparison_long$site_id == site_id & comparison_long$flux == flux, ]
    if (nrow(sub) == 0) next
    bias <- mean(sub$xbase_value - sub$tower_value)
    stats005_rows[[length(stats005_rows) + 1]] <- data.frame(
      site_id = site_id, flux = flux, derived = sub$derived[1],
      n_months_005 = nrow(sub),
      bias_005 = bias,
      rmse_005 = rmse_fn(sub$xbase_value - sub$tower_value),
      r2_005 = r2_fn(sub$tower_value, sub$xbase_value)
    )
  }
}
stats005 <- do.call(rbind, stats005_rows)

stats050_path <- file.path(FLUXCOM_ROOT, "tables", "xbase_monthly_stats.csv")
stats050 <- utils::read.csv(stats050_path, stringsAsFactors = FALSE)
stats050_sub <- stats050[stats050$site_id %in% STAGE3_SITES, c("site_id", "flux", "n_months", "bias_product_minus_tower", "rmse", "r2")]
names(stats050_sub) <- c("site_id", "flux", "n_months_050", "bias_050", "rmse_050", "r2_050")

summary_df <- merge(stats005, stats050_sub, by = c("site_id", "flux"), all.x = TRUE)
summary_df$is_training_site <- training_df$in_fluxcom_x_base_training_set[match(summary_df$site_id, training_df$site_id)]
summary_df <- summary_df[order(summary_df$flux, summary_df$site_id), ]
utils::write.csv(summary_df, file.path(PKG_DIR, "summary.csv"), row.names = FALSE)

cat("\n[12_package_ossenatou] summary.csv (0.05deg vs. Stage 2 0.5deg):\n")
print(summary_df)

## ---- README.md for Ossenatou -----------------------------------------------------------------
readme <- c(
sprintf("# FLUXCOM-X-BASE 0.05 degree data package for Ossenatou Mamadou (%s)", PKG_DATE),
"",
"This package was prepared for the WAF-Net paper by David Moore's group, extracting the ",
"FLUXCOM-X-BASE product at its native 0.05 degree monthly grid resolution for the six WAF-Net ",
"sites, alongside the matching tower (eddy-covariance) monthly data. It is a data package for ",
"your own analysis and plotting -- it does not contain new figures or interpretation beyond the ",
"simple statistics in `summary.csv` and one labelled DRAFT check figure.",
"",
"**FLUXCOM-X-BASE is not FLUXNET/Shuttle data.** It is an independent, machine-learning-based ",
"gridded product used here only as a comparison against your tower observations.",
"",
"## Files",
"",
"- **`xbase_005_monthly.csv`** -- FLUXCOM-X-BASE values at the 0.05 degree grid cell containing ",
"  each tower, every month 2001-2021 (not filtered by tower QC -- this is the product as ",
"  distributed):",
"  - `site_id` -- FLUXNET site code.",
"  - `year`, `month`.",
"  - `GPP_gC_m2_d1` -- gross primary productivity, grams carbon per m2 per day (a mean daily ",
"    rate for the month, not a monthly total).",
"  - `NEE_gC_m2_d1` -- net ecosystem exchange, same units. **Negative = net uptake by the ",
"    ecosystem (carbon sink), positive = net release.** This is the same sign convention as the ",
"    tower's own NEE.",
"  - `ET_mm_hr`, `ET_T_mm_hr` -- evapotranspiration and its transpiration component, mean mm of ",
"    water per hour, as distributed by X-BASE.",
"  - `LE_W_m2` -- latent heat flux, derived from `ET_mm_hr` using the latent heat of ",
"    vaporization (2.45e6 J/kg): `LE = ET * 2.45e6 / 3600`. This is the unit used for comparison ",
"    against the tower's own LE.",
"  - `cell_lon`, `cell_lat` -- the centre of the 0.05 degree grid cell used (see `sites.csv` for ",
"    the distance from the tower).",
"",
"- **`tower_monthly.csv`** -- the matching monthly tower (eddy-covariance) data for the same six ",
"  sites and months, from the FLUXNET Shuttle, same QC rule and VUT/CUT choice as our Stage 2 ",
"  0.5 degree comparison:",
"  - `carbon_src` -- `VUT` or `CUT`, which partitioning the site's carbon fluxes use (all six ",
"    sites here are `VUT`).",
"  - `NEE_VUT_REF`, `GPP_DT_VUT_REF`/`RECO_DT_VUT_REF` (daytime partition), ",
"    `GPP_NT_VUT_REF`/`RECO_NT_VUT_REF` (nighttime partition), `LE_F_MDS`, `H_F_MDS` -- all in ",
"    their native FLUXNET units (NEE/GPP/RECO: gC m-2 d-1; LE/H: W m-2).",
"  - Each flux has a matching `..._QC` column: **the fraction (0-1) of the underlying ",
"    half-hourly/hourly records that were measured or good-quality gap-filled** -- not the 0/1/2/3 ",
"    integer flag used at half-hourly resolution. `GPP`/`RECO` (both partitions) have no QC ",
"    column of their own at monthly resolution and are gated on the site's own NEE QC instead ",
"    (`GPP_RECO_DT_QC`/`GPP_RECO_NT_QC` -- both equal `NEE_VUT_REF_QC`).",
"  - Each flux also has a `..._pass_qc` TRUE/FALSE column: whether that QC value meets the ",
"    threshold used throughout this analysis, QC >= 0.50 -- the same rule used for the Stage 2 ",
"    0.5 degree comparison and the FLUXNET2015 published convention.",
"  - We used **nighttime-partitioned** GPP/RECO (`GPP_NT`/`RECO_NT`) in `summary.csv`, because ",
"    X-BASE's own documentation states its GPP training targets are nighttime-partitioned; the ",
"    daytime columns (`GPP_DT`/`RECO_DT`) are included here too in case you want them.",
"",
"- **`sites.csv`** -- one row per site: name, country, IGBP vegetation class, tower coordinates, ",
"  the 0.05 degree grid cell centre used and its distance from the tower (`cell005_dist_km`), ",
"  the number of tower months (2001-2021) that pass the QC rule above, whether the site is in ",
"  FLUXCOM-X-BASE's own 294-site training set, and the tower dataset's own citation and ",
"  DOI/handle (read from the FLUXNET Shuttle snapshot of 20 September 2026) -- **please cite ",
"  each tower site's own dataset**, in addition to X-BASE, in any publication using this data.",
"",
"- **`summary.csv`** -- one row per site and flux (NEE, GPP, RECO [derived], LE): number of ",
"  QC-passing overlapping months, bias (X-BASE minus tower), RMSE, and R-squared, computed at ",
"  the 0.05 degree resolution in this package (`_005` columns) and, alongside, the same numbers ",
"  from our earlier 0.5 degree comparison (`_050` columns) for reference. `derived = TRUE` for ",
"  RECO; see below.",
"",
"- **`check_figure_xbase_005_vs_tower_DRAFT.png`** -- one check figure, monthly X-BASE (0.05 ",
"  degree) against tower, one panel each for NEE/GPP/LE, sites by colour, training sites marked, ",
"  1:1 reference line. Labelled DRAFT -- a sanity check, not a paper figure.",
"",
"## Important things to know before analysing this data",
"",
"- **Sign convention:** NEE is negative for net ecosystem carbon uptake, positive for release, ",
"  in both the tower and X-BASE series. **Bias is always X-BASE minus tower** throughout.",
"- **X-BASE has no ecosystem respiration variable of its own.** If you need RECO from X-BASE, ",
"  derive it as `GPP + NEE` (both from X-BASE, same sign convention) -- this is what `summary.csv` ",
"  does, and it is marked `derived = TRUE` wherever shown. It is not a measured product.",
"- **X-BASE has no sensible heat (H) variable at all.** H is not compared anywhere in this ",
"  package; `tower_monthly.csv`'s `H_F_MDS` has no X-BASE counterpart.",
"- **X-BASE's own GPP is trained on nighttime-partitioned eddy-covariance data only** -- that is ",
"  why we used your sites' `GPP_NT`/`RECO_NT` columns, not `GPP_DT`/`RECO_DT`, when computing the ",
"  comparison statistics in `summary.csv`.",
"- **QC rule:** a tower month is treated as usable only if its QC fraction is >= 0.50 (the ",
"  FLUXNET2015 published convention, and our default throughout this analysis).",
"- **Training-site caveat:** `GH-Ank` and `SN-Dhr` are confirmed to be in FLUXCOM-X-BASE's own ",
"  294-site training set (Nelson et al. 2024, Table 1) -- the comparison at those two sites is ",
"  partly within-sample, not fully independent. Marked in `sites.csv` and the check figure.",
"- **Grid-cell caveat:** the 0.05 degree cell containing each tower is not centred exactly on ",
"  the tower -- see `cell005_dist_km` in `sites.csv` for how far off it is. The product's spatial ",
"  resolution (roughly 5 km at the equator) also means the cell may not represent the tower's ",
"  specific footprint, especially at sites with heterogeneous land cover nearby.",
"",
"## Citations",
"",
"**FLUXCOM-X-BASE:** Nelson, J.A., Walther, S., Jung, M., Gans, F., Kraft, B., Weber, U., Hamdi, ",
"Z., Duveiller, G., Zhang, W. (2023). FLUXCOM-X-BASE. ICOS ERIC - Carbon Portal. ",
"https://doi.org/10.18160/5NZG-JMJE. Method/product paper: Nelson et al. (2024), ",
"*Biogeosciences* 21, 5079-5115, doi:10.18160/5NZG-JMJE. Licence: CC BY 4.0.",
"",
"**Each tower site's own dataset** must also be cited -- see `sites.csv`'s `site_citation` and ",
"`site_doi_or_handle` columns, one per site.",
"",
"Questions about how this package was built: see `docs/methods_memo.md` and ",
sprintf("`docs/report_back_%s.md` in the WAFNET/fluxcom_comparison repository.", PKG_DATE)
)
writeLines(readme, file.path(PKG_DIR, "README.md"))

for (f in c("xbase_005_monthly.csv", "tower_monthly.csv", "sites.csv", "summary.csv")) {
  write_meta(
    file.path(PKG_DIR, f),
    input_sources = c(RAW_IN, file.path(FLUXCOM_ROOT, "tables", "site_carbon_source.csv"),
                       SNAPSHOT_PATH, file.path(ANNUAL_PAPER_EXTRACTED_DIR, WAFNET_SITE_DIRS[STAGE3_SITES], "*_FLUXMET_MM_*.csv (read-only)")),
    notes = paste0("Stage 3 data package for Ossenatou Mamadou (", PKG_DATE, "). FLUXCOM-X-BASE is non-Shuttle ",
                    "comparison data (CLAUDE.md Hard Rule 1). See README.md in this same folder for full column ",
                    "documentation.")
  )
}

pkg_size_bytes <- sum(file.size(list.files(PKG_DIR, full.names = TRUE, recursive = TRUE)))
message("\n[12_package_ossenatou] Package written to ", PKG_DIR, " (", round(pkg_size_bytes / 1e6, 2), " MB before the check figure/zip)")
if (pkg_size_bytes > 5e6) {
  message("[12_package_ossenatou] NOTE: package already exceeds 5 MB before the check figure/zip -- ",
          "per the prompt, stop and report before committing if the finished package is more than a few MB.")
}
