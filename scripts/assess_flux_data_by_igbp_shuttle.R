## assess_flux_data_by_igbp_shuttle.R
## Per-site annual flux data availability and IGBP-class distribution assessment
## using FLUXNET Shuttle YY (yearly) product.
##
## Steps:
##   1. Per-site annual medians: NEP, GPP, TER, ET, H with VUT/CUT fallback
##   2. IGBP-class site count table per flux variable
##   3. IGBP-class distribution shape (median, IQR, CV across sites)
##
## Outputs:
##   data/snapshots/site_flux_medians_shuttle.csv  + .meta.json
##   data/snapshots/igbp_class_flux_distributions_shuttle.csv + .meta.json
##   SESSION_LOG entry (written separately)
##
## Revised 2026-10-02: Step 1 (per-site flux medians) now reads the DuckDB
## `annual` table via R/site_annual_fluxes.R::compute_site_annual_fluxes()
## instead of looping over loose FLUXMET_YY CSVs in data/extracted/ with a
## hardcoded QC_THRESH=0.80. Same shared function and QC_THRESHOLD_YY gate
## (R/pipeline_config.R) as scripts/figure4_representativeness.R and
## scripts/generate_whittaker_alt_fig02_update.R (Figures 4 and 2). See
## docs/known_issues.md Sec 10 and SESSION_LOG.md 2026-10-02.
##
## Unit conventions:
##   NEP, GPP, TER : gC m-2 yr-1 (YY product is pre-integrated; sign-flip NEE->NEP)
##   ET             : mm yr-1, via compute_site_annual_fluxes()'s fluxnet_convert_units()
##   H              : W m-2, annual mean (compute_site_annual_fluxes(h_unit="W_m2") --
##                    the shared function's default converts H to a pre-integrated
##                    MJ m-2 yr-1 total; this script instead keeps H's native W m-2
##                    mean rate, matching this file's own historical convention and
##                    Figure 3 panel C, which plots H in W m-2)
##
## VUT/CUT fallback (per site, applied jointly to NEE/GPP/RECO, not per-year):
##   compute_site_annual_fluxes() picks ONE QC column per site -- VUT if the site
##   has any non-NA NEE_VUT_REF_QC, else CUT -- the same rule scripts/04_qc.R
##   applies, never mixed within a site across years (the retired loose-file
##   version decided VUT/CUT per ROW, which could mix the two within one site).
##   A row qualifies when (1 - QC) <= QC_THRESHOLD_YY. LE and H are gated on
##   their own QC columns independently of the NEE gate.
##
## NT/DT partitioning fallback (per site, GPP and TER only):
##   First preference: NT (nighttime partitioning) — GPP_NT_{VUT/CUT}_REF.
##   Fallback: DT (daytime partitioning) — GPP_DT_{VUT/CUT}_REF — only when
##   NT yields 0 qualifying years for that site.
##   Decision is per-site (not per-year) to avoid mixing methods within a site median.
##   Note: GPP_DT/RECO_DT at YY resolution carry no dedicated QC column; quality
##   is gated by the same NEE QC threshold used for the VUT/CUT decision.
##   Tracked in gpp_partition and ter_partition output columns ("NT" or "DT").

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(purrr)
  library(jsonlite)
  library(duckdb)
  library(DBI)
})

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
source("R/units.R")
source("R/site_annual_fluxes.R")
check_pipeline_config()

# ---- Constants ---------------------------------------------------------------
SNAP_CSV    <- "data/snapshots/fluxnet_shuttle_snapshot_20260901T094522.csv"  # pinned 2026-09-01, see SESSION_LOG.md
OUT_MEDIANS <- "data/snapshots/site_flux_medians_shuttle.csv"
OUT_IGBP    <- "data/snapshots/igbp_class_flux_distributions_shuttle.csv"
DB_PATH     <- file.path(FLUXNET_DATA_ROOT, "duckdb/fluxnet.duckdb")

STANDARD_IGBP <- c("EBF","MF","DBF","ENF","CSH","OSH","WSA","SAV",
                    "GRA","WET","CRO","CVM")

msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)

msg("=== IGBP flux data availability assessment ===")

# ---- Helper: write companion meta.json --------------------------------------
write_meta <- function(output_path, notes = "") {
  meta <- list(
    run_datetime_utc  = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    pipeline_version  = system("git rev-parse --short HEAD", intern = TRUE),
    snapshot_csv      = SNAP_CSV,
    yy_product        = "FLUXMET_YY v1.3_r1 (DuckDB `annual` table, dataset='FLUXMET')",
    qc_threshold      = QC_THRESHOLD_YY,
    partitioning_policy = paste0(
      "NT preferred (GPP_NT_VUT/CUT_REF, RECO_NT_VUT/CUT_REF). ",
      "DT fallback (GPP_DT_VUT/CUT_REF, RECO_DT_VUT/CUT_REF) used only ",
      "when NT yields 0 qualifying years for a site. Decision is per-site, ",
      "not per-year, to avoid mixing partitioning methods within a site median. ",
      "DT columns carry no dedicated QC at YY resolution; NEE QC gates quality. ",
      "Computed by R/site_annual_fluxes.R::compute_site_annual_fluxes()."),
    vut_cut_policy    = paste0(
      "Per-site (not per-row) VUT/CUT choice via compute_site_annual_fluxes(): VUT if the site ",
      "has any non-NA NEE_VUT_REF_QC, else CUT. A year qualifies when (1 - QC) <= ",
      "QC_THRESHOLD_YY=", QC_THRESHOLD_YY, " on the chosen column (scripts/04_qc.R's rule)."),
    unit_nep_gpp_ter  = "gC m-2 yr-1 (pre-integrated YY product; NEP = -NEE)",
    unit_et           = "mm yr-1, via compute_site_annual_fluxes()'s fluxnet_convert_units() (R/units.R)",
    unit_h            = "W m-2 (annual mean LE_F_MDS equivalent; compute_site_annual_fluxes(h_unit='W_m2'), not converted)",
    le_column         = "LE_F_MDS with LE_F_MDS_QC",
    h_column          = "H_F_MDS with H_F_MDS_QC",
    notes             = notes
  )
  jsonlite::write_json(meta, paste0(output_path, ".meta.json"),
                       pretty = TRUE, auto_unbox = TRUE)
}

# ---- Step 0: load snapshot site list ----------------------------------------
msg("Loading snapshot: ", basename(SNAP_CSV))
snap <- read_csv(SNAP_CSV, show_col_types = FALSE) |>
  select(site_id, igbp, location_lat, location_long) |>
  distinct(site_id, .keep_all = TRUE)

msg("  Snapshot sites: ", nrow(snap))

# ---- Step 1: per-site flux medians (compute_site_annual_fluxes()) -----------
msg("\n=== STEP 1: Per-site flux medians ===")
msg("Querying DuckDB `annual` table: ", DB_PATH)
con <- dbConnect(duckdb(), DB_PATH, read_only = TRUE)
site_fluxes <- compute_site_annual_fluxes(con, site_ids = snap$site_id, h_unit = "W_m2")
dbDisconnect(con, shutdown = TRUE)
msg("  compute_site_annual_fluxes(): ", nrow(site_fluxes$site_year), " site-year rows, ",
    sum(!is.na(site_fluxes$site_year$NEE)), " with a qualifying NEE value")

site_results <- site_fluxes$site_summary |>
  transmute(
    site_id,
    n_years_nee  = n_years_nee,
    n_years_gpp  = n_years_gpp,
    n_years_ter  = n_years_reco,
    n_years_le   = n_years_et,
    n_years_h    = n_years_h,
    nep_median   = dplyr::if_else(is.na(nee_median), NA_real_, -nee_median),
    gpp_median   = gpp_median,
    ter_median   = reco_median,
    et_median    = et_median,
    h_median     = h_median,
    ## NEE/GPP/RECO always share one per-site VUT/CUT source and one set of
    ## qualifying years by construction (compute_site_annual_fluxes() gates
    ## all three on the same NEE QC column/threshold) -- unlike the retired
    ## loose-file version, these three source columns can never disagree.
    nep_source   = nee_source,
    gpp_source   = nee_source,
    ter_source   = nee_source,
    gpp_partition = gpp_partition,
    ter_partition = reco_partition,
    ## Binary by construction (one column per site, never mixed within a
    ## site) -- kept as a fraction for schema compatibility with the retired
    ## per-row version's vut_frac_nee.
    vut_frac_nee = dplyr::case_when(
      nee_source == "VUT" ~ 1, nee_source == "CUT" ~ 0, TRUE ~ NA_real_
    )
  )

# Join with snapshot metadata
medians_out <- snap |>
  left_join(site_results, by = "site_id") |>
  # Replace NaN medians (all-NA medians) with NA
  mutate(across(c(nep_median, gpp_median, ter_median, et_median, h_median),
                ~ ifelse(is.nan(.x), NA_real_, .x)))

msg("  Sites processed: ", nrow(site_results))
msg("  Sites in snapshot with YY data: ",
    sum(!is.na(medians_out$n_years_nee) & medians_out$n_years_nee > 0))

# Select final columns in specified order
medians_final <- medians_out |>
  select(site_id, igbp_class = igbp, location_lat, location_long,
         n_years_nee, n_years_gpp, n_years_ter, n_years_le, n_years_h,
         nep_median, gpp_median, ter_median, et_median, h_median,
         nep_source, gpp_source, ter_source,
         gpp_partition, ter_partition, vut_frac_nee)

# ---- NT/DT partition summary ----
n_nt_gpp <- sum(medians_final$gpp_partition == "NT", na.rm = TRUE)
n_dt_gpp <- sum(medians_final$gpp_partition == "DT", na.rm = TRUE)
n_na_gpp <- sum(is.na(medians_final$gpp_partition))
n_nt_ter <- sum(medians_final$ter_partition == "NT", na.rm = TRUE)
n_dt_ter <- sum(medians_final$ter_partition == "DT", na.rm = TRUE)
n_na_ter <- sum(is.na(medians_final$ter_partition))

msg("\nPartitioning method summary:")
msg(sprintf("  GPP: NT=%d  DT=%d  NA=%d", n_nt_gpp, n_dt_gpp, n_na_gpp))
msg(sprintf("  TER: NT=%d  DT=%d  NA=%d", n_nt_ter, n_dt_ter, n_na_ter))

# NT-only vs NT+DT GPP/TER medians (sites that gained data via DT fallback)
if (n_dt_gpp > 0L || n_dt_ter > 0L) {
  msg("\nSites gaining GPP via DT fallback:")
  dt_gpp_sites <- medians_final |>
    filter(gpp_partition == "DT") |>
    select(site_id, igbp_class, n_years_gpp, gpp_median)
  print(as.data.frame(dt_gpp_sites), row.names = FALSE)
}

write_csv(medians_final, OUT_MEDIANS)
msg("Saved: ", OUT_MEDIANS)
write_meta(OUT_MEDIANS, notes = paste0(
  "QC_THRESHOLD_YY=", QC_THRESHOLD_YY, " (R/pipeline_config.R), via ",
  "R/site_annual_fluxes.R::compute_site_annual_fluxes(). ",
  "NT-preferred partitioning with DT fallback when NT has 0 qualifying years. ",
  "gpp_partition/ter_partition: 'NT' or 'DT' or NA. ",
  "LE_F_MDS used for ET; H_F_MDS used for H (h_unit='W_m2', native annual mean, not ",
  "pre-integrated). vut_frac_nee = 1/0/NA (VUT/CUT/neither) -- a per-site decision, ",
  "never mixed within a site across years."))

# ---- Step 2: IGBP class distribution ----------------------------------------
msg("\n=== STEP 2: IGBP class site counts per flux ===")

flux_vars <- c("nep","gpp","ter","et","h")
n_col     <- paste0("n_years_", c("nee","gpp","ter","le","h"))
med_col   <- paste0(flux_vars, "_median")

igbp_counts <- medians_final |>
  mutate(igbp_type = case_when(
    igbp_class %in% STANDARD_IGBP ~ igbp_class,
    is.na(igbp_class) | igbp_class == "" ~ "MISSING",
    TRUE ~ "OTHER"
  )) |>
  group_by(igbp_type) |>
  summarise(
    n_sites_total = n(),
    n_nee  = sum(n_years_nee  > 0, na.rm = TRUE),
    n_gpp  = sum(n_years_gpp  > 0, na.rm = TRUE),
    n_ter  = sum(n_years_ter  > 0, na.rm = TRUE),
    n_et   = sum(n_years_le   > 0, na.rm = TRUE),
    n_h    = sum(n_years_h    > 0, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(flag_low = if_else(
    pmin(n_nee, n_gpp, n_ter, n_et, n_h) < 5L, "n<5", ""
  )) |>
  arrange(match(igbp_type, c(STANDARD_IGBP, "OTHER", "MISSING")))

msg("\nIGBP class counts (n sites with usable data per flux):")
print(as.data.frame(igbp_counts), row.names = FALSE)

absent <- setdiff(STANDARD_IGBP, igbp_counts$igbp_type)
if (length(absent) > 0L) {
  msg("  IGBP classes ABSENT from shuttle network: ",
      paste(absent, collapse = ", "))
} else {
  msg("  All 12 standard IGBP classes present")
}

low_n <- igbp_counts |> filter(igbp_type %in% STANDARD_IGBP, flag_low == "n<5")
if (nrow(low_n) > 0L) {
  msg("  Classes with n<5 for at least one flux: ",
      paste(low_n$igbp_type, collapse = ", "))
}

# NT-only vs NT+DT comparison per IGBP class (for SESSION_LOG)
msg("\nNT-only vs NT+DT GPP/TER availability by IGBP class:")
nt_dt_cmp <- medians_final |>
  filter(igbp_class %in% STANDARD_IGBP) |>
  group_by(igbp_class) |>
  summarise(
    n_sites       = n(),
    n_gpp_nt      = sum(gpp_partition == "NT", na.rm = TRUE),
    n_gpp_dt_gain = sum(gpp_partition == "DT", na.rm = TRUE),
    n_gpp_total   = sum(!is.na(gpp_partition)),
    n_ter_nt      = sum(ter_partition == "NT", na.rm = TRUE),
    n_ter_dt_gain = sum(ter_partition == "DT", na.rm = TRUE),
    n_ter_total   = sum(!is.na(ter_partition)),
    gpp_med_nt    = median(gpp_median[gpp_partition == "NT"], na.rm = TRUE),
    gpp_med_all   = median(gpp_median[!is.na(gpp_partition)], na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(match(igbp_class, STANDARD_IGBP))

print(as.data.frame(nt_dt_cmp), row.names = FALSE)

# ---- Step 3: distribution shape per IGBP class ------------------------------
msg("\n=== STEP 3: Distribution shape per IGBP class ===")

cv_safe <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 2L || mean(x) == 0) return(NA_real_)
  sd(x) / abs(mean(x))
}

igbp_shape_rows <- list()
flux_label <- c(nep = "NEP (gC m-2 yr-1)", gpp = "GPP (gC m-2 yr-1)",
                ter = "TER (gC m-2 yr-1)", et  = "ET (mm yr-1)",
                h   = "H (W m-2)")

for (cls in STANDARD_IGBP) {
  sub <- medians_final |>
    filter(igbp_class == cls)

  for (fv in flux_vars) {
    med_c <- paste0(fv, "_median")
    vals  <- sub[[med_c]][!is.na(sub[[med_c]])]
    n     <- length(vals)

    igbp_shape_rows[[length(igbp_shape_rows) + 1L]] <- data.frame(
      igbp_class   = cls,
      flux_variable= fv,
      flux_label   = flux_label[[fv]],
      n_sites      = n,
      median_val   = if (n >= 1L) median(vals) else NA_real_,
      q25          = if (n >= 4L) quantile(vals, 0.25) else NA_real_,
      q75          = if (n >= 4L) quantile(vals, 0.75) else NA_real_,
      min_val      = if (n >= 1L) min(vals)    else NA_real_,
      max_val      = if (n >= 1L) max(vals)    else NA_real_,
      cv           = if (n >= 2L) cv_safe(vals) else NA_real_,
      spread       = if (n >= 2L) {
        cv <- cv_safe(vals)
        if (is.na(cv)) "unknown"
        else if (cv < 0.25) "tight" else if (cv < 0.60) "moderate" else "wide"
      } else "insufficient",
      stringsAsFactors = FALSE
    )
  }
}

igbp_shape <- bind_rows(igbp_shape_rows) |>
  mutate(across(c(median_val, q25, q75, min_val, max_val, cv),
                ~ round(.x, 3)))

write_csv(igbp_shape, OUT_IGBP)
msg("Saved: ", OUT_IGBP)
write_meta(OUT_IGBP, notes = paste0(
  "Distribution of site-median values per IGBP class × flux. ",
  "cv = sd/|mean| across site medians within each class. ",
  "spread: tight cv<0.25, moderate cv<0.60, wide cv>=0.60."))

# ---- Step 4: Summary report -------------------------------------------------
msg("\n=== STEP 4: Summary report ===")

n_total_sites    <- nrow(medians_final)
n_any_flux       <- sum(rowSums(!is.na(medians_final[, med_col])) > 0)
n_all_five       <- sum(rowSums(!is.na(medians_final[, med_col])) == 5L)

msg("Total sites in snapshot: ", n_total_sites)
msg("Sites with usable YY data for >= 1 flux: ", n_any_flux)
msg("Sites with usable data for all 5 fluxes: ", n_all_five)

# VUT/CUT fallback usage
vut_frac <- mean(medians_final$vut_frac_nee, na.rm = TRUE)
n_all_cut <- sum(medians_final$vut_frac_nee == 0 &
                   !is.na(medians_final$vut_frac_nee) &
                   medians_final$n_years_nee > 0, na.rm = TRUE)
msg("\nVUT usage:")
msg("  Mean fraction of site-years using VUT: ", round(vut_frac, 3))
msg("  Sites using CUT exclusively: ", n_all_cut)

# Cross-flux data availability pattern
msg("\nCross-flux availability (% of sites with usable data):")
for (j in seq_along(flux_vars)) {
  n_usable <- sum(medians_final[[n_col[j]]] > 0, na.rm = TRUE)
  msg("  ", toupper(flux_vars[j]), ": ", n_usable, " / ", n_total_sites,
      " (", round(100 * n_usable / n_total_sites, 1), "%)")
}

# Non-standard IGBP
n_other   <- sum(medians_final$igbp_class %in%
                   setdiff(unique(medians_final$igbp_class), c(STANDARD_IGBP, NA)), na.rm = TRUE)
n_missing <- sum(is.na(medians_final$igbp_class))
other_cls <- sort(unique(medians_final$igbp_class[
  !medians_final$igbp_class %in% STANDARD_IGBP & !is.na(medians_final$igbp_class)]))
msg("\nNon-standard IGBP labels: ", n_other, " sites — ",
    paste(other_cls, collapse = ", "))
msg("Missing/blank IGBP: ", n_missing, " sites")

msg("\nIGBP class distribution shape (median of site medians):")
for (cls in STANDARD_IGBP) {
  sub <- igbp_shape |> filter(igbp_class == cls)
  if (nrow(sub) == 0L) next
  vals_str <- paste(sprintf("%s=%.1f(n=%d)", sub$flux_variable,
                            ifelse(is.na(sub$median_val), 0, sub$median_val),
                            sub$n_sites), collapse = "  ")
  msg("  ", cls, ": ", vals_str)
}

msg("\n=== Assessment complete ===")
