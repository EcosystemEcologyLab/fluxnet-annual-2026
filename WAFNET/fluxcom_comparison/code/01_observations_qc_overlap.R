## 01_observations_qc_overlap.R — Stage 1 pre-analysis report, part 3-4.
##
## READ-ONLY. Reads the repo-root data/extracted/ YY and MM FLUXMET CSVs for
## the six WAFNET sites (already downloaded for the Annual Paper) and reports,
## per site x variable x resolution, which months/years pass the Annual
## Paper's QC rule. Does NOT touch any FLUXCOM product and does NOT compute
## any comparison statistic (bias/RMSE/R2/Taylor) -- this is Stage 1 only,
## per the user's request.
##
## GPP/RECO use the DAYTIME partition (GPP_DT/RECO_DT), not the Annual
## Paper's NT-preferred/DT-fallback default in R/site_annual_fluxes.R -- the
## WAF-Net paper specifically wants the daytime partition for comparison
## against FLUXCOM's daytime-partitioned members. VUT/CUT selection per site
## follows the identical rule as R/site_annual_fluxes.R (VUT if the site has
## any non-NA NEE_VUT_REF_QC, else CUT) -- same source, just not reused as a
## function call because that function hardcodes the NT/DT fallback and only
## operates on the DuckDB `annual` table, not a directly-read MM extract.
##
## GPP_DT/RECO_DT have no QC column of their own at MM/YY resolution (checked
## against the FLUXMET header, 2026-10-14) -- gated on the site's NEE QC
## column, like NT GPP/RECO are in R/site_annual_fluxes.R.
##
## Output (git-tracked):
##   tables/site_variable_qc_overlap_yy.csv
##   tables/site_variable_qc_overlap_mm.csv
##   tables/site_carbon_source.csv

source("WAFNET/fluxcom_comparison/code/00_config.R")

VARS <- c("NEE", "GPP_DT", "RECO_DT", "LE", "H")

read_fluxmet <- function(site_id, resolution) {
  path <- wafnet_fluxmet_path(site_id, resolution)
  d <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  d[d == -9999] <- NA
  d
}

native_unit <- function(varname, extracted_dir, resolution) {
  f <- list.files(extracted_dir, pattern = paste0("BIFVARINFO_", resolution, ".*\\.csv$"), full.names = TRUE)
  if (length(f) != 1) return(NA_character_)
  bv <- utils::read.csv(f[1], stringsAsFactors = FALSE, check.names = FALSE)
  gid <- bv$GROUP_ID[bv$VARIABLE == "VAR_INFO_VARNAME" & bv$DATAVALUE == varname]
  if (length(gid) == 0) return(NA_character_)
  unit_row <- bv$DATAVALUE[bv$GROUP_ID == gid[1] & bv$VARIABLE == "VAR_INFO_UNIT"]
  if (length(unit_row) == 0) NA_character_ else unit_row[1]
}

## ---- Per-site carbon_src decision (from YY data, same rule as
## R/site_annual_fluxes.R::.compute_site_annual_fluxes_core) ----------------
carbon_src_rows <- lapply(WAFNET_SITES, function(site_id) {
  yy <- read_fluxmet(site_id, "YY")
  n_vut <- sum(!is.na(yy$NEE_VUT_REF_QC))
  n_cut <- sum(!is.na(yy$NEE_CUT_REF_QC))
  carbon_src <- if (n_vut > 0) "VUT" else if (n_cut > 0) "CUT" else NA_character_
  data.frame(site_id = site_id, n_vut_years_nonNA_qc = n_vut, n_cut_years_nonNA_qc = n_cut, carbon_src = carbon_src)
})
carbon_src_df <- do.call(rbind, carbon_src_rows)
write.csv(carbon_src_df, file.path(FLUXCOM_ROOT, "tables", "site_carbon_source.csv"), row.names = FALSE)
write_meta(
  file.path(FLUXCOM_ROOT, "tables", "site_carbon_source.csv"),
  input_sources = file.path(ANNUAL_PAPER_EXTRACTED_DIR, WAFNET_SITE_DIRS[WAFNET_SITES], "*_FLUXMET_YY_*.csv (read-only)"),
  notes = "Stage 1, six original WAFNET sites only. carbon_src = VUT if the site has any non-NA NEE_VUT_REF_QC in its YY extract, else CUT (same rule as R/site_annual_fluxes.R). Superseded for the current Stage 2 site set (five agreed + two pending Ossenatou) by 06_tower_data_update.R, which overwrites this same file with a site_status column added."
)

carbon_src_for <- function(site_id) carbon_src_df$carbon_src[carbon_src_df$site_id == site_id]

## ---- Per-site x variable x resolution qualifying-period table -----------
qc_threshold_for <- function(resolution) if (resolution == "YY") QC_THRESHOLD_YY else QC_THRESHOLD_MM

build_overlap_table <- function(resolution) {
  rows <- list()
  for (site_id in WAFNET_SITES) {
    d <- read_fluxmet(site_id, resolution)
    src <- carbon_src_for(site_id)
    thresh <- qc_threshold_for(resolution)
    site_dir <- file.path(ANNUAL_PAPER_EXTRACTED_DIR, WAFNET_SITE_DIRS[[site_id]])

    nee_val <- if (!is.na(src) && src == "VUT") d$NEE_VUT_REF else if (!is.na(src) && src == "CUT") d$NEE_CUT_REF else rep(NA_real_, nrow(d))
    nee_qc  <- if (!is.na(src) && src == "VUT") d$NEE_VUT_REF_QC else if (!is.na(src) && src == "CUT") d$NEE_CUT_REF_QC else rep(NA_real_, nrow(d))
    nee_qualifies <- !is.na(nee_val) & !is.na(nee_qc) & qc_passes(nee_qc, thresh)

    gpp_dt_val  <- if (!is.na(src) && src == "VUT") d$GPP_DT_VUT_REF else if (!is.na(src) && src == "CUT") d$GPP_DT_CUT_REF else rep(NA_real_, nrow(d))
    reco_dt_val <- if (!is.na(src) && src == "VUT") d$RECO_DT_VUT_REF else if (!is.na(src) && src == "CUT") d$RECO_DT_CUT_REF else rep(NA_real_, nrow(d))

    le_qualifies <- !is.na(d$LE_F_MDS) & !is.na(d$LE_F_MDS_QC) & qc_passes(d$LE_F_MDS_QC, thresh)
    h_qualifies  <- !is.na(d$H_F_MDS)  & !is.na(d$H_F_MDS_QC)  & qc_passes(d$H_F_MDS_QC, thresh)

    qualifying <- list(
      NEE     = nee_qualifies,
      GPP_DT  = nee_qualifies & !is.na(gpp_dt_val),
      RECO_DT = nee_qualifies & !is.na(reco_dt_val),
      LE      = le_qualifies,
      H       = h_qualifies
    )
    colname <- list(NEE = "NEE_VUT_REF", GPP_DT = "GPP_DT_VUT_REF", RECO_DT = "RECO_DT_VUT_REF", LE = "LE_F_MDS", H = "H_F_MDS")

    for (v in VARS) {
      q <- qualifying[[v]]
      periods <- d$TIMESTAMP[q]
      n <- length(periods)
      rows[[length(rows) + 1]] <- data.frame(
        site_id = site_id,
        variable = v,
        resolution = resolution,
        carbon_src = if (v %in% c("NEE", "GPP_DT", "RECO_DT")) src else NA_character_,
        n_qualifying = n,
        first_period = if (n > 0) min(periods) else NA,
        last_period = if (n > 0) max(periods) else NA,
        native_unit = native_unit(colname[[v]], site_dir, resolution),
        qualifying_periods = paste(periods, collapse = ";")
      )
    }
  }
  do.call(rbind, rows)
}

overlap_yy <- build_overlap_table("YY")
overlap_mm <- build_overlap_table("MM")

write.csv(overlap_yy, file.path(FLUXCOM_ROOT, "tables", "site_variable_qc_overlap_yy.csv"), row.names = FALSE)
write.csv(overlap_mm, file.path(FLUXCOM_ROOT, "tables", "site_variable_qc_overlap_mm.csv"), row.names = FALSE)
for (res in c("yy", "mm")) {
  write_meta(
    file.path(FLUXCOM_ROOT, "tables", paste0("site_variable_qc_overlap_", res, ".csv")),
    input_sources = file.path(ANNUAL_PAPER_EXTRACTED_DIR, WAFNET_SITE_DIRS[WAFNET_SITES], paste0("*_FLUXMET_", toupper(res), "_*.csv (read-only)")),
    notes = paste0(
      "Stage 1, six original WAFNET sites, NEE/GPP_DT/RECO_DT/LE/H only. qc_passes() at QC_THRESHOLD_",
      toupper(res), " (0.50); GPP_DT/RECO_DT gated on the site's NEE QC column (neither has a QC column ",
      "of its own at this resolution). Superseded for the current Stage 2 site set (five agreed + two ",
      "pending Ossenatou, plus GPP_NT/RECO_NT rows) by 06_tower_data_update.R, which overwrites this ",
      "same file as a superset, not a replacement -- these Stage 1 DT rows are unchanged."
    )
  )
}

cat("\n[WAFNET/fluxcom_comparison] carbon_src per site:\n")
print(carbon_src_df)
cat("\n[WAFNET/fluxcom_comparison] YY qualifying-year counts:\n")
print(overlap_yy[, c("site_id", "variable", "n_qualifying", "first_period", "last_period", "native_unit")])
cat("\n[WAFNET/fluxcom_comparison] MM qualifying-month counts:\n")
print(overlap_mm[, c("site_id", "variable", "n_qualifying", "first_period", "last_period", "native_unit")])
cat("\nWrote tables/site_carbon_source.csv, tables/site_variable_qc_overlap_yy.csv, tables/site_variable_qc_overlap_mm.csv\n")
