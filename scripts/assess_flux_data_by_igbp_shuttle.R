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
## Unit conventions:
##   NEP, GPP, TER : gC m-2 yr-1 (YY product is pre-integrated; sign-flip NEE->NEP)
##   ET             : mm yr-1 (LE_F_MDS W m-2 * 365.25*86400 / (lambda*1000))
##   H              : W m-2 (annual mean; document only, not converted)
##
## VUT/CUT fallback (per site-year, applied jointly to NEE/GPP/RECO):
##   Use VUT if NEE_VUT_REF != -9999 and NEE_VUT_REF_QC >= QC_THRESH.
##   Fall back to CUT if VUT unavailable but CUT meets QC_THRESH.
##   LE and H are gated on their own QC columns independently.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(purrr)
  library(jsonlite)
})

# ---- Constants ---------------------------------------------------------------
QC_THRESH   <- 0.80          # minimum QC fraction to retain a site-year
LAMBDA      <- 2.45e6        # latent heat of vaporisation, J kg-1
SECS_YR     <- 365.25 * 86400
NA_FLAG     <- -9999
SNAP_CSV    <- "data/snapshots/fluxnet_shuttle_snapshot_20260624T095651.csv"
OUT_MEDIANS <- "data/snapshots/site_flux_medians_shuttle.csv"
OUT_IGBP    <- "data/snapshots/igbp_class_flux_distributions_shuttle.csv"

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
    yy_product        = "FLUXMET_YY v1.3_r1",
    qc_threshold      = QC_THRESH,
    partitioning      = "NT (nighttime)",
    vut_cut_policy    = paste0(
      "VUT used when NEE_VUT_REF_QC >= ", QC_THRESH,
      "; CUT fallback when VUT QC fails or value is -9999"),
    unit_nep_gpp_ter  = "gC m-2 yr-1 (pre-integrated YY product; NEP = -NEE)",
    unit_et           = paste0("mm yr-1 via LE_F_MDS [W m-2] * ", SECS_YR,
                               " [s yr-1] / lambda [J kg-1]; lambda = ", LAMBDA,
                               " J/kg. Derivation: 1 kg m-2 = 1 mm depth, so ",
                               "ET [mm yr-1] = LE * secs / lambda."),
    unit_h            = "W m-2 (annual mean LE_F_MDS equivalent; not converted)",
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

# ---- Step 0b: find all FLUXMET YY files -------------------------------------
msg("Scanning for FLUXMET YY files ...")
yy_files <- list.files("data/extracted", pattern = "FLUXMET_YY.*\\.csv$",
                        recursive = TRUE, full.names = TRUE)
msg("  Found: ", length(yy_files), " YY files")

# Extract site_id from filename: e.g. AMF_CA-TP3_FLUXNET_FLUXMET_YY_...csv
# Pattern: {HUB}_{SITE_ID}_FLUXNET_FLUXMET_YY
extract_site_id <- function(path) {
  bn <- basename(path)
  # Match two-letter country code + hyphen + alphanumeric site code
  m <- regmatches(bn, regexpr("[A-Z]{2,4}-[A-Za-z0-9]+(?=_FLUXNET)", bn, perl = TRUE))
  if (length(m) == 0L) NA_character_ else m[[1L]]
}

yy_df <- data.frame(
  path    = yy_files,
  site_id = vapply(yy_files, extract_site_id, character(1L)),
  stringsAsFactors = FALSE
)

# Deduplicate: if multiple YY files per site, keep longest (most years)
yy_df <- yy_df |>
  filter(!is.na(site_id)) |>
  mutate(fsize = file.size(path)) |>
  group_by(site_id) |>
  slice_max(fsize, n = 1L, with_ties = FALSE) |>
  ungroup()

msg("  Unique sites with YY file: ", nrow(yy_df))

# ---- Step 1: per-site flux medians ------------------------------------------
msg("\n=== STEP 1: Per-site flux medians ===")

na_to_na <- function(x) ifelse(is.na(x) | x == NA_FLAG, NA_real_, as.numeric(x))

process_site <- function(path, site_id) {
  yy <- tryCatch(
    read_csv(path, show_col_types = FALSE, na = as.character(NA_FLAG)),
    error = function(e) NULL
  )
  if (is.null(yy) || nrow(yy) == 0L) return(NULL)

  # Rename TIMESTAMP if needed
  if (!"TIMESTAMP" %in% names(yy)) return(NULL)

  # Required columns (check presence; some older files may be missing some)
  needed <- c("NEE_VUT_REF","NEE_VUT_REF_QC","NEE_CUT_REF","NEE_CUT_REF_QC",
              "GPP_NT_VUT_REF","GPP_NT_CUT_REF",
              "RECO_NT_VUT_REF","RECO_NT_CUT_REF",
              "LE_F_MDS","LE_F_MDS_QC","H_F_MDS","H_F_MDS_QC")
  missing_cols <- setdiff(needed, names(yy))
  if (length(missing_cols) > 0L) {
    # Add NA columns for missing ones so downstream code is uniform
    for (col in missing_cols) yy[[col]] <- NA_real_
  }

  # Coerce to numeric and replace -9999 with NA
  for (col in needed) yy[[col]] <- na_to_na(yy[[col]])

  rows <- list()
  for (i in seq_len(nrow(yy))) {
    r <- yy[i, ]
    yr <- r$TIMESTAMP

    # ---- VUT/CUT decision for carbon (NEE/GPP/RECO) ----
    vut_ok <- !is.na(r$NEE_VUT_REF) & !is.na(r$NEE_VUT_REF_QC) &
              r$NEE_VUT_REF_QC >= QC_THRESH
    cut_ok <- !is.na(r$NEE_CUT_REF) & !is.na(r$NEE_CUT_REF_QC) &
              r$NEE_CUT_REF_QC >= QC_THRESH

    if (vut_ok) {
      nee_val <- r$NEE_VUT_REF; gpp_val <- r$GPP_NT_VUT_REF
      ter_val <- r$RECO_NT_VUT_REF; carbon_src <- "VUT"
    } else if (cut_ok) {
      nee_val <- r$NEE_CUT_REF; gpp_val <- r$GPP_NT_CUT_REF
      ter_val <- r$RECO_NT_CUT_REF; carbon_src <- "CUT"
    } else {
      nee_val <- NA_real_; gpp_val <- NA_real_
      ter_val <- NA_real_; carbon_src <- NA_character_
    }

    # NEP = -NEE (sign flip)
    nep_val <- if (is.na(nee_val)) NA_real_ else -nee_val

    # ---- LE → ET ----
    le_ok  <- !is.na(r$LE_F_MDS) & !is.na(r$LE_F_MDS_QC) &
               r$LE_F_MDS_QC >= QC_THRESH
    le_val <- if (le_ok) r$LE_F_MDS else NA_real_
    et_val <- if (!is.na(le_val)) le_val * SECS_YR / LAMBDA else NA_real_

    # ---- H ----
    h_ok  <- !is.na(r$H_F_MDS) & !is.na(r$H_F_MDS_QC) &
              r$H_F_MDS_QC >= QC_THRESH
    h_val <- if (h_ok) r$H_F_MDS else NA_real_

    rows[[i]] <- data.frame(
      year = yr, nep = nep_val, gpp = gpp_val, ter = ter_val,
      et = et_val, h = h_val, carbon_src = carbon_src,
      stringsAsFactors = FALSE
    )
  }

  site_yy <- bind_rows(rows)

  data.frame(
    site_id      = site_id,
    n_years_nee  = sum(!is.na(site_yy$nep)),
    n_years_gpp  = sum(!is.na(site_yy$gpp)),
    n_years_ter  = sum(!is.na(site_yy$ter)),
    n_years_le   = sum(!is.na(site_yy$et)),
    n_years_h    = sum(!is.na(site_yy$h)),
    nep_median   = median(site_yy$nep, na.rm = TRUE),
    gpp_median   = median(site_yy$gpp, na.rm = TRUE),
    ter_median   = median(site_yy$ter, na.rm = TRUE),
    et_median    = median(site_yy$et,  na.rm = TRUE),
    h_median     = median(site_yy$h,   na.rm = TRUE),
    # Source: most common variant used (or "BOTH" if mixed)
    nep_source   = {
      srcs <- site_yy$carbon_src[!is.na(site_yy$nep)]
      if (length(srcs) == 0L) NA_character_
      else { u <- unique(srcs); if (length(u) == 1L) u else "BOTH" }
    },
    gpp_source   = {
      srcs <- site_yy$carbon_src[!is.na(site_yy$gpp)]
      if (length(srcs) == 0L) NA_character_
      else { u <- unique(srcs); if (length(u) == 1L) u else "BOTH" }
    },
    ter_source   = {
      srcs <- site_yy$carbon_src[!is.na(site_yy$ter)]
      if (length(srcs) == 0L) NA_character_
      else { u <- unique(srcs); if (length(u) == 1L) u else "BOTH" }
    },
    # VUT fraction (for reporting)
    vut_frac_nee = mean(site_yy$carbon_src[!is.na(site_yy$nep)] == "VUT",
                        na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

# Process all sites
site_results <- vector("list", nrow(yy_df))
for (i in seq_len(nrow(yy_df))) {
  if (i %% 50 == 0L) msg("  Processing site ", i, " / ", nrow(yy_df))
  site_results[[i]] <- process_site(yy_df$path[i], yy_df$site_id[i])
}
site_results <- bind_rows(Filter(Negate(is.null), site_results))

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
         nep_source, gpp_source, ter_source, vut_frac_nee)

write_csv(medians_final, OUT_MEDIANS)
msg("Saved: ", OUT_MEDIANS)
write_meta(OUT_MEDIANS, notes = paste0(
  "QC threshold ", QC_THRESH,
  ". LE_F_MDS used for ET; H_F_MDS used for H. ",
  "vut_frac_nee = fraction of qualifying site-years using VUT (vs CUT) for NEE."))

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
