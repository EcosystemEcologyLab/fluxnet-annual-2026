## 06_tower_data_update.R — Stage 2 step 5: extend the tower QC/overlap tables.
##
## READ-ONLY against the repo-root data/extracted/ YY and MM FLUXMET CSVs
## (same convention as 01_observations_qc_overlap.R). Extends Stage 1's
## carbon-source and QC-overlap tables to:
##   (a) STAGE2_SITES (00_config.R): the five agreed sites, including ML-Kem
##       (added at Ossenatou Mamadou's request, Mali, WSA, 2007-2009);
##   (b) STAGE2_PENDING_SITES: BJ-Db1 and SN-Nkr, re-checked here because
##       FLUXCOM-X-BASE's period (2001-2021) is longer than the original
##       FLUXCOM product that excluded them in Stage 1. Flagged
##       site_status = "pending Ossenatou" throughout — NOT folded into the
##       agreed five in any table or figure.
##
## Also extends VARS to add GPP_NT/RECO_NT alongside the existing GPP_DT/
## RECO_DT rows: FLUXCOM-X-BASE's own ICOS Carbon Portal metadata states "The
## GPP estimates from the eddy covariance data was based on the Nighttime
## Partitioning method" (direct product-description text, high confidence —
## supersedes Stage 1's "NOT FOUND" for X-BASE's partitioning, which only had
## the paper's image-rendered Table 1 to go on). Per the Stage 2 prompt step 4,
## the tower comparison against X-BASE therefore uses the NT partition
## (GPP_NT_VUT_REF/RECO_NT_VUT_REF), not DT — the DT rows are kept (not
## removed) since they remain the Annual Paper's WAF-Net default per
## WAFNET/fluxcom_comparison/README.md and may be wanted for a future
## comparison against a DT-partitioned product.
##
## Output (git-tracked, OVERWRITTEN with the extended superset — same schema,
## existing GH-Ank/BJ-Db1/BJ-Nhu/BJ-Bfg/SN-Nkr/SN-Dhr DT-partition rows are
## numerically unchanged from Stage 1):
##   tables/site_carbon_source.csv       (+ site_status column, + ML-Kem row)
##   tables/site_variable_qc_overlap_yy.csv  (+ site_status, + GPP_NT/RECO_NT, + ML-Kem)
##   tables/site_variable_qc_overlap_mm.csv  (+ site_status, + GPP_NT/RECO_NT, + ML-Kem)

source("WAFNET/fluxcom_comparison/code/00_config.R")

VARS <- c("NEE", "GPP_DT", "RECO_DT", "GPP_NT", "RECO_NT", "LE", "H")

site_status_for <- function(site_id) if (site_id %in% STAGE2_PENDING_SITES) "pending Ossenatou" else "agreed"

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

## ---- Per-site carbon_src decision (identical rule to Stage 1 / R/site_annual_fluxes.R) ----
carbon_src_rows <- lapply(STAGE2_ALL_SITES, function(site_id) {
  yy <- read_fluxmet(site_id, "YY")
  n_vut <- sum(!is.na(yy$NEE_VUT_REF_QC))
  n_cut <- sum(!is.na(yy$NEE_CUT_REF_QC))
  carbon_src <- if (n_vut > 0) "VUT" else if (n_cut > 0) "CUT" else NA_character_
  data.frame(
    site_id = site_id,
    site_status = site_status_for(site_id),
    n_vut_years_nonNA_qc = n_vut,
    n_cut_years_nonNA_qc = n_cut,
    carbon_src = carbon_src
  )
})
carbon_src_df <- do.call(rbind, carbon_src_rows)
write.csv(carbon_src_df, file.path(FLUXCOM_ROOT, "tables", "site_carbon_source.csv"), row.names = FALSE)
write_meta(
  file.path(FLUXCOM_ROOT, "tables", "site_carbon_source.csv"),
  input_sources = file.path(ANNUAL_PAPER_EXTRACTED_DIR, WAFNET_SITE_DIRS[STAGE2_ALL_SITES], "*_FLUXMET_YY_*.csv (read-only)"),
  notes = "Stage 2: five agreed sites + two pending Ossenatou (site_status column), superseding the Stage 1 six-site version 01_observations_qc_overlap.R wrote to this same file. carbon_src = VUT if the site has any non-NA NEE_VUT_REF_QC in its YY extract, else CUT (same rule as R/site_annual_fluxes.R). All seven sites resolve to VUT."
)

carbon_src_for <- function(site_id) carbon_src_df$carbon_src[carbon_src_df$site_id == site_id]

## ---- Per-site x variable x resolution qualifying-period table ------------
qc_threshold_for <- function(resolution) if (resolution == "YY") QC_THRESHOLD_YY else QC_THRESHOLD_MM

build_overlap_table <- function(resolution) {
  rows <- list()
  for (site_id in STAGE2_ALL_SITES) {
    d <- read_fluxmet(site_id, resolution)
    src <- carbon_src_for(site_id)
    thresh <- qc_threshold_for(resolution)
    site_dir <- file.path(ANNUAL_PAPER_EXTRACTED_DIR, WAFNET_SITE_DIRS[[site_id]])

    nee_val <- if (!is.na(src) && src == "VUT") d$NEE_VUT_REF else if (!is.na(src) && src == "CUT") d$NEE_CUT_REF else rep(NA_real_, nrow(d))
    nee_qc  <- if (!is.na(src) && src == "VUT") d$NEE_VUT_REF_QC else if (!is.na(src) && src == "CUT") d$NEE_CUT_REF_QC else rep(NA_real_, nrow(d))
    nee_qualifies <- !is.na(nee_val) & !is.na(nee_qc) & qc_passes(nee_qc, thresh)

    gpp_dt_val  <- if (!is.na(src) && src == "VUT") d$GPP_DT_VUT_REF else if (!is.na(src) && src == "CUT") d$GPP_DT_CUT_REF else rep(NA_real_, nrow(d))
    reco_dt_val <- if (!is.na(src) && src == "VUT") d$RECO_DT_VUT_REF else if (!is.na(src) && src == "CUT") d$RECO_DT_CUT_REF else rep(NA_real_, nrow(d))
    gpp_nt_val  <- if (!is.na(src) && src == "VUT") d$GPP_NT_VUT_REF else if (!is.na(src) && src == "CUT") d$GPP_NT_CUT_REF else rep(NA_real_, nrow(d))
    reco_nt_val <- if (!is.na(src) && src == "VUT") d$RECO_NT_VUT_REF else if (!is.na(src) && src == "CUT") d$RECO_NT_CUT_REF else rep(NA_real_, nrow(d))

    le_qualifies <- !is.na(d$LE_F_MDS) & !is.na(d$LE_F_MDS_QC) & qc_passes(d$LE_F_MDS_QC, thresh)
    h_qualifies  <- !is.na(d$H_F_MDS)  & !is.na(d$H_F_MDS_QC)  & qc_passes(d$H_F_MDS_QC, thresh)

    qualifying <- list(
      NEE     = nee_qualifies,
      GPP_DT  = nee_qualifies & !is.na(gpp_dt_val),
      RECO_DT = nee_qualifies & !is.na(reco_dt_val),
      GPP_NT  = nee_qualifies & !is.na(gpp_nt_val),
      RECO_NT = nee_qualifies & !is.na(reco_nt_val),
      LE      = le_qualifies,
      H       = h_qualifies
    )
    colname <- list(
      NEE = "NEE_VUT_REF", GPP_DT = "GPP_DT_VUT_REF", RECO_DT = "RECO_DT_VUT_REF",
      GPP_NT = "GPP_NT_VUT_REF", RECO_NT = "RECO_NT_VUT_REF", LE = "LE_F_MDS", H = "H_F_MDS"
    )

    for (v in VARS) {
      q <- qualifying[[v]]
      periods <- d$TIMESTAMP[q]
      n <- length(periods)
      rows[[length(rows) + 1]] <- data.frame(
        site_id = site_id,
        site_status = site_status_for(site_id),
        variable = v,
        resolution = resolution,
        carbon_src = if (v %in% c("NEE", "GPP_DT", "RECO_DT", "GPP_NT", "RECO_NT")) src else NA_character_,
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
    input_sources = file.path(ANNUAL_PAPER_EXTRACTED_DIR, WAFNET_SITE_DIRS[STAGE2_ALL_SITES], paste0("*_FLUXMET_", toupper(res), "_*.csv (read-only)")),
    notes = paste0(
      "Stage 2: extends the Stage 1 (01_observations_qc_overlap.R) version of this same file to a ",
      "superset -- adds ML-Kem and the two pending-Ossenatou sites (site_status column), and adds ",
      "GPP_NT/RECO_NT variable rows alongside the existing GPP_DT/RECO_DT rows (X-BASE's own GPP ",
      "training uses the nighttime partition only; the DT rows are kept, not removed, for any future ",
      "DT-partitioned product comparison). qc_passes() at QC_THRESHOLD_", toupper(res), " (0.50); ",
      "GPP_NT/RECO_NT and GPP_DT/RECO_DT gated on the site's NEE QC column (neither partition has a ",
      "QC column of its own at this resolution)."
    )
  )
}

cat("\n[WAFNET/fluxcom_comparison] Stage 2 carbon_src per site (STAGE2_ALL_SITES):\n")
print(carbon_src_df)
cat("\n[WAFNET/fluxcom_comparison] YY qualifying-year counts (NEE/GPP_NT/RECO_NT/LE/H):\n")
print(overlap_yy[overlap_yy$variable %in% c("NEE", "GPP_NT", "RECO_NT", "LE", "H"), c("site_id", "site_status", "variable", "n_qualifying", "first_period", "last_period")])
cat("\n[WAFNET/fluxcom_comparison] MM qualifying-month counts (NEE/GPP_NT/RECO_NT/LE/H):\n")
print(overlap_mm[overlap_mm$variable %in% c("NEE", "GPP_NT", "RECO_NT", "LE", "H"), c("site_id", "site_status", "variable", "n_qualifying", "first_period", "last_period")])
cat("\nWrote tables/site_carbon_source.csv, tables/site_variable_qc_overlap_yy.csv, tables/site_variable_qc_overlap_mm.csv\n")
