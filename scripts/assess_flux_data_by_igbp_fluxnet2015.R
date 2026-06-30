## assess_flux_data_by_igbp_fluxnet2015.R
## Extraction + per-site annual flux medians + IGBP-class distribution
## assessment for the original FLUXNET2015 release (Pastorello et al. 2020,
## doi:10.1038/s41597-020-0534-3) — the reference-comparison counterpart to
## scripts/assess_flux_data_by_igbp_shuttle.R (current FLUXNET Shuttle
## reprocessing of the same sites). These are DISTINCT data products even
## where they cover the same site-years (see SESSION_LOG.md 2026-06-30,
## "FLUXNET2015 release YY data inventory").
##
## Steps:
##   0. Extract the FULLSET YY CSV (only) from each site's downloaded ZIP —
##      data/raw/fluxnet2015/<site_id>/*.zip -> data/extracted/fluxnet2015/<site_id>/.
##      Only the YY file is extracted, not the full archive: the FULLSET ZIP
##      also bundles HR/DD/WW/MM resolutions and AUX files (the HR file alone
##      is ~100-270 MB per site), none of which this annual-resolution
##      assessment needs. Re-extracting other resolutions later is possible
##      from the retained ZIPs in data/raw/fluxnet2015/ without re-downloading.
##   1. Per-site annual medians: identical VUT/CUT, NT/DT, QC>=0.80 logic to
##      assess_flux_data_by_igbp_shuttle.R, for direct comparability.
##   2. IGBP-class site count table per flux variable.
##   3. IGBP-class distribution shape (median, IQR, CV across sites).
##   4. Comparison vs the current shuttle medians
##      (data/snapshots/site_flux_medians_shuttle.csv): site counts,
##      median shifts, VUT/CUT and NT/DT distribution.
##
## Site scope: 206 of 212 sites in data/snapshots/sites_fluxnet2015_clean.csv.
## Six sites (RU-Sam, RU-SkP, RU-Tks, RU-Vrk, SE-St1, ZA-Kru) are Tier-2-only
## under the FLUXNET2015 Data Policy and were not downloaded (per user
## decision 2026-06-30, scripts/download_fluxnet2015.sh); they appear here as
## all-NA rows (no ZIP directory), not as extraction failures.
##
## data/extracted/ is already covered by .gitignore (line 6: "data/extracted/"),
## so data/extracted/fluxnet2015/ needs no separate gitignore entry.
##
## Outputs:
##   data/snapshots/site_flux_medians_fluxnet2015.csv + .meta.json
##   data/snapshots/igbp_class_flux_distributions_fluxnet2015.csv + .meta.json
##   SESSION_LOG entry (written separately, sourced from this script's output)
##
## Unit conventions (identical to the shuttle assessment):
##   NEP, GPP, TER : gC m-2 yr-1 (YY product is pre-integrated; sign-flip NEE->NEP)
##   ET             : mm yr-1 (LE_F_MDS W m-2 * 365.25*86400 / (lambda*1000))
##   H              : W m-2 (annual mean; document only, not converted)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(purrr)
  library(jsonlite)
})

# ---- Constants ---------------------------------------------------------------
QC_THRESH   <- 0.80
LAMBDA      <- 2.45e6
SECS_YR     <- 365.25 * 86400
NA_FLAG     <- -9999
RAW_DIR     <- "data/raw/fluxnet2015"
EXTRACT_DIR <- "data/extracted/fluxnet2015"
SITE_CSV    <- "data/snapshots/sites_fluxnet2015_clean.csv"
SHUTTLE_MEDIANS_CSV <- "data/snapshots/site_flux_medians_shuttle.csv"
OUT_MEDIANS <- "data/snapshots/site_flux_medians_fluxnet2015.csv"
OUT_IGBP    <- "data/snapshots/igbp_class_flux_distributions_fluxnet2015.csv"

STANDARD_IGBP <- c("EBF","MF","DBF","ENF","CSH","OSH","WSA","SAV",
                    "GRA","WET","CRO","CVM")
TIER2_ONLY_SITES <- c("RU-Sam","RU-SkP","RU-Tks","RU-Vrk","SE-St1","ZA-Kru")

msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)

msg("=== FLUXNET2015 release: IGBP flux data availability assessment ===")

dir.create(EXTRACT_DIR, recursive = TRUE, showWarnings = FALSE)

# ---- Helper: write companion meta.json --------------------------------------
write_meta <- function(output_path, notes = "") {
  meta <- list(
    run_datetime_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    pipeline_version = system("git rev-parse --short HEAD", intern = TRUE),
    source = paste0(
      "FLUXNET2015 release (Pastorello et al. 2020, Sci Data 7:225, ",
      "doi:10.1038/s41597-020-0534-3), FULLSET product, CC-BY-4.0 tier. ",
      "Downloaded via the AmeriFlux Cyberinfrastructure data API ",
      "(see SESSION_LOG.md 2026-06-30, \"FLUXNET2015 release portal ",
      "investigation and download launch\")."),
    source_zips   = RAW_DIR,
    site_csv      = SITE_CSV,
    site_coverage = paste0(
      "206 of 212 sites in ", SITE_CSV, ". Six Tier-2-only sites were not ",
      "downloaded per the CC-BY-4.0 scope decision (2026-06-30): ",
      paste(TIER2_ONLY_SITES, collapse = ", "),
      ". These appear here as all-NA rows (no ZIP directory found), not as ",
      "extraction failures."),
    yy_product    = "FLUXNET2015 FULLSET YY (annual resolution)",
    qc_threshold  = QC_THRESH,
    partitioning_policy = paste0(
      "NT preferred (GPP_NT_VUT/CUT_REF, RECO_NT_VUT/CUT_REF). ",
      "DT fallback (GPP_DT_VUT/CUT_REF, RECO_DT_VUT/CUT_REF) used only ",
      "when NT yields 0 qualifying years for a site. Decision is per-site, ",
      "not per-year. Identical policy to site_flux_medians_shuttle.csv for ",
      "direct comparability."),
    vut_cut_policy = paste0(
      "VUT used when NEE_VUT_REF_QC >= ", QC_THRESH,
      "; CUT fallback when VUT QC fails or value is -9999"),
    unit_nep_gpp_ter = "gC m-2 yr-1 (pre-integrated YY product; NEP = -NEE)",
    unit_et = paste0("mm yr-1 via LE_F_MDS [W m-2] * ", SECS_YR,
                      " [s yr-1] / lambda [J kg-1]; lambda = ", LAMBDA,
                      " J/kg. Derivation: 1 kg m-2 = 1 mm depth, so ",
                      "ET [mm yr-1] = LE * secs / lambda."),
    unit_h = "W m-2 (annual mean LE_F_MDS equivalent; not converted)",
    le_column = "LE_F_MDS with LE_F_MDS_QC",
    h_column  = "H_F_MDS with H_F_MDS_QC",
    network   = "fluxnet2015",
    notes     = notes
  )
  jsonlite::write_json(meta, paste0(output_path, ".meta.json"),
                       pretty = TRUE, auto_unbox = TRUE)
}

# ---- Step 0: site list and ZIP inventory -------------------------------------
msg("Loading site list: ", basename(SITE_CSV))
site_list <- read_csv(SITE_CSV, show_col_types = FALSE) |>
  select(site_id, igbp, location_lat, location_long) |>
  distinct(site_id, .keep_all = TRUE)
msg("  Sites in list: ", nrow(site_list))

site_dirs <- list.dirs(RAW_DIR, recursive = FALSE, full.names = TRUE)
msg("  Site ZIP directories found: ", length(site_dirs))

undownloaded <- setdiff(site_list$site_id, basename(site_dirs))
msg("  Sites in list with no downloaded ZIP: ", length(undownloaded),
    " (", paste(undownloaded, collapse = ", "), ")")
msg("  Tier-2-only sites match expectation: ",
    setequal(undownloaded, TIER2_ONLY_SITES))

# ---- Step 0b: extraction (FULLSET YY only) -----------------------------------
msg("\n=== STEP 0: Extraction (FULLSET YY only) ===")

needed_cols <- c("NEE_VUT_REF","NEE_VUT_REF_QC","NEE_CUT_REF","NEE_CUT_REF_QC",
                  "GPP_NT_VUT_REF","GPP_NT_CUT_REF",
                  "RECO_NT_VUT_REF","RECO_NT_CUT_REF",
                  "GPP_DT_VUT_REF","GPP_DT_CUT_REF",
                  "RECO_DT_VUT_REF","RECO_DT_CUT_REF",
                  "LE_F_MDS","LE_F_MDS_QC","H_F_MDS","H_F_MDS_QC")

extract_one <- function(site_dir) {
  site_id  <- basename(site_dir)
  zip_path <- list.files(site_dir, pattern = "_FLUXNET2015_FULLSET_.*\\.zip$",
                          full.names = TRUE)

  fail <- function(reason, yy_path = NA_character_) {
    data.frame(site_id = site_id, status = "FAIL", reason = reason,
               yy_path = yy_path, n_missing_cols = NA_integer_,
               missing_cols = NA_character_, stringsAsFactors = FALSE)
  }

  if (length(zip_path) != 1L) {
    return(fail(sprintf("%d ZIP file(s) found in %s (expected 1)",
                         length(zip_path), site_dir)))
  }

  entries <- tryCatch(utils::unzip(zip_path, list = TRUE)$Name,
                       error = function(e) NULL)
  if (is.null(entries)) return(fail("ZIP unreadable/corrupt"))

  yy_entry <- grep("_FLUXNET2015_FULLSET_YY_.*\\.csv$", entries, value = TRUE)
  if (length(yy_entry) != 1L) {
    return(fail(sprintf("%d YY entries found in ZIP (expected 1)",
                         length(yy_entry))))
  }

  out_dir <- file.path(EXTRACT_DIR, site_id)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  ok <- tryCatch({
    utils::unzip(zip_path, files = yy_entry, exdir = out_dir, junkpaths = TRUE)
    TRUE
  }, error = function(e) FALSE)
  if (!ok) return(fail("extraction error"))

  yy_path <- file.path(out_dir, basename(yy_entry))
  if (!file.exists(yy_path) || file.size(yy_path) == 0L) {
    return(fail("extracted YY file missing or empty"))
  }

  hdr <- tryCatch(names(read_csv(yy_path, n_max = 0L, show_col_types = FALSE)),
                   error = function(e) NULL)
  if (is.null(hdr) || !"TIMESTAMP" %in% hdr) {
    return(fail("unreadable or missing TIMESTAMP column", yy_path))
  }

  missing <- setdiff(needed_cols, hdr)
  data.frame(site_id = site_id, status = "OK", reason = "",
             yy_path = yy_path, n_missing_cols = length(missing),
             missing_cols = paste(missing, collapse = ";"),
             stringsAsFactors = FALSE)
}

extraction_log <- bind_rows(lapply(site_dirs, extract_one))
n_ok   <- sum(extraction_log$status == "OK")
n_fail <- sum(extraction_log$status == "FAIL")
msg("  Extraction: ", n_ok, " OK, ", n_fail, " FAILED (of ",
    nrow(extraction_log), " ZIP dirs)")

if (n_fail > 0L) {
  msg("  FAILURES:")
  fail_rows <- extraction_log |> filter(status == "FAIL")
  for (i in seq_len(nrow(fail_rows))) {
    msg("    ", fail_rows$site_id[i], ": ", fail_rows$reason[i])
  }
} else {
  msg("  No extraction failures.")
}

missing_col_sites <- extraction_log |> filter(status == "OK", n_missing_cols > 0L)
if (nrow(missing_col_sites) > 0L) {
  msg("  Sites with missing expected columns (NA-filled downstream):")
  for (i in seq_len(nrow(missing_col_sites))) {
    msg("    ", missing_col_sites$site_id[i], ": ", missing_col_sites$missing_cols[i])
  }
} else {
  msg("  All extracted YY files contain the full expected column set.")
}

# ---- Step 1: per-site flux medians -------------------------------------------
msg("\n=== STEP 1: Per-site flux medians ===")

na_to_na <- function(x) ifelse(is.na(x) | x == NA_FLAG, NA_real_, as.numeric(x))

process_site <- function(path, site_id) {
  yy <- tryCatch(
    read_csv(path, show_col_types = FALSE, na = as.character(NA_FLAG)),
    error = function(e) NULL
  )
  if (is.null(yy) || nrow(yy) == 0L) return(NULL)
  if (!"TIMESTAMP" %in% names(yy)) return(NULL)

  needed <- needed_cols
  missing_cols <- setdiff(needed, names(yy))
  if (length(missing_cols) > 0L) {
    for (col in missing_cols) yy[[col]] <- NA_real_
  }
  for (col in needed) yy[[col]] <- na_to_na(yy[[col]])

  rows <- list()
  for (i in seq_len(nrow(yy))) {
    r <- yy[i, ]
    yr <- r$TIMESTAMP

    vut_ok <- !is.na(r$NEE_VUT_REF) & !is.na(r$NEE_VUT_REF_QC) &
              r$NEE_VUT_REF_QC >= QC_THRESH
    cut_ok <- !is.na(r$NEE_CUT_REF) & !is.na(r$NEE_CUT_REF_QC) &
              r$NEE_CUT_REF_QC >= QC_THRESH

    if (vut_ok) {
      nee_val    <- r$NEE_VUT_REF
      gpp_nt_val <- r$GPP_NT_VUT_REF; ter_nt_val <- r$RECO_NT_VUT_REF
      gpp_dt_val <- r$GPP_DT_VUT_REF; ter_dt_val <- r$RECO_DT_VUT_REF
      carbon_src <- "VUT"
    } else if (cut_ok) {
      nee_val    <- r$NEE_CUT_REF
      gpp_nt_val <- r$GPP_NT_CUT_REF; ter_nt_val <- r$RECO_NT_CUT_REF
      gpp_dt_val <- r$GPP_DT_CUT_REF; ter_dt_val <- r$RECO_DT_CUT_REF
      carbon_src <- "CUT"
    } else {
      nee_val    <- NA_real_
      gpp_nt_val <- NA_real_; ter_nt_val <- NA_real_
      gpp_dt_val <- NA_real_; ter_dt_val <- NA_real_
      carbon_src <- NA_character_
    }

    nep_val <- if (is.na(nee_val)) NA_real_ else -nee_val

    le_ok  <- !is.na(r$LE_F_MDS) & !is.na(r$LE_F_MDS_QC) &
               r$LE_F_MDS_QC >= QC_THRESH
    le_val <- if (le_ok) r$LE_F_MDS else NA_real_
    et_val <- if (!is.na(le_val)) le_val * SECS_YR / LAMBDA else NA_real_

    h_ok  <- !is.na(r$H_F_MDS) & !is.na(r$H_F_MDS_QC) &
              r$H_F_MDS_QC >= QC_THRESH
    h_val <- if (h_ok) r$H_F_MDS else NA_real_

    rows[[i]] <- data.frame(
      year = yr, nep = nep_val,
      gpp_nt = gpp_nt_val, ter_nt = ter_nt_val,
      gpp_dt = gpp_dt_val, ter_dt = ter_dt_val,
      et = et_val, h = h_val, carbon_src = carbon_src,
      stringsAsFactors = FALSE
    )
  }

  site_yy <- bind_rows(rows)

  n_gpp_nt <- sum(!is.na(site_yy$gpp_nt))
  n_ter_nt <- sum(!is.na(site_yy$ter_nt))

  if (n_gpp_nt > 0L) {
    gpp_vals <- site_yy$gpp_nt; gpp_partition <- "NT"
  } else if (sum(!is.na(site_yy$gpp_dt)) > 0L) {
    gpp_vals <- site_yy$gpp_dt; gpp_partition <- "DT"
  } else {
    gpp_vals <- rep(NA_real_, nrow(site_yy)); gpp_partition <- NA_character_
  }

  if (n_ter_nt > 0L) {
    ter_vals <- site_yy$ter_nt; ter_partition <- "NT"
  } else if (sum(!is.na(site_yy$ter_dt)) > 0L) {
    ter_vals <- site_yy$ter_dt; ter_partition <- "DT"
  } else {
    ter_vals <- rep(NA_real_, nrow(site_yy)); ter_partition <- NA_character_
  }

  src_of <- function(vals, srcs) {
    s <- srcs[!is.na(vals)]
    if (length(s) == 0L) NA_character_
    else { u <- unique(s); if (length(u) == 1L) u else "BOTH" }
  }

  data.frame(
    site_id       = site_id,
    n_years_nee   = sum(!is.na(site_yy$nep)),
    n_years_gpp   = sum(!is.na(gpp_vals)),
    n_years_ter   = sum(!is.na(ter_vals)),
    n_years_le    = sum(!is.na(site_yy$et)),
    n_years_h     = sum(!is.na(site_yy$h)),
    nep_median    = median(site_yy$nep, na.rm = TRUE),
    gpp_median    = median(gpp_vals,    na.rm = TRUE),
    ter_median    = median(ter_vals,    na.rm = TRUE),
    et_median     = median(site_yy$et,  na.rm = TRUE),
    h_median      = median(site_yy$h,   na.rm = TRUE),
    nep_source    = src_of(site_yy$nep, site_yy$carbon_src),
    gpp_source    = src_of(gpp_vals,    site_yy$carbon_src),
    ter_source    = src_of(ter_vals,    site_yy$carbon_src),
    gpp_partition = gpp_partition,
    ter_partition = ter_partition,
    vut_frac_nee  = mean(site_yy$carbon_src[!is.na(site_yy$nep)] == "VUT",
                         na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

ok_sites <- extraction_log |> filter(status == "OK")
site_results <- vector("list", nrow(ok_sites))
for (i in seq_len(nrow(ok_sites))) {
  if (i %% 50 == 0L) msg("  Processing site ", i, " / ", nrow(ok_sites))
  site_results[[i]] <- process_site(ok_sites$yy_path[i], ok_sites$site_id[i])
}
site_results <- bind_rows(Filter(Negate(is.null), site_results))

medians_out <- site_list |>
  left_join(site_results, by = "site_id") |>
  mutate(across(c(nep_median, gpp_median, ter_median, et_median, h_median),
                ~ ifelse(is.nan(.x), NA_real_, .x)))

msg("  Sites processed: ", nrow(site_results))
msg("  Sites in list with usable YY data: ",
    sum(!is.na(medians_out$n_years_nee) & medians_out$n_years_nee > 0))

medians_final <- medians_out |>
  select(site_id, igbp_class = igbp, location_lat, location_long,
         n_years_nee, n_years_gpp, n_years_ter, n_years_le, n_years_h,
         nep_median, gpp_median, ter_median, et_median, h_median,
         nep_source, gpp_source, ter_source,
         gpp_partition, ter_partition, vut_frac_nee) |>
  mutate(network = "fluxnet2015")

n_nt_gpp <- sum(medians_final$gpp_partition == "NT", na.rm = TRUE)
n_dt_gpp <- sum(medians_final$gpp_partition == "DT", na.rm = TRUE)
n_na_gpp <- sum(is.na(medians_final$gpp_partition))
n_nt_ter <- sum(medians_final$ter_partition == "NT", na.rm = TRUE)
n_dt_ter <- sum(medians_final$ter_partition == "DT", na.rm = TRUE)
n_na_ter <- sum(is.na(medians_final$ter_partition))

msg("\nPartitioning method summary (FLUXNET2015):")
msg(sprintf("  GPP: NT=%d  DT=%d  NA=%d", n_nt_gpp, n_dt_gpp, n_na_gpp))
msg(sprintf("  TER: NT=%d  DT=%d  NA=%d", n_nt_ter, n_dt_ter, n_na_ter))

write_csv(medians_final, OUT_MEDIANS)
msg("Saved: ", OUT_MEDIANS)
write_meta(OUT_MEDIANS, notes = paste0(
  "QC threshold ", QC_THRESH,
  ". NT-preferred partitioning with DT fallback when NT has 0 qualifying years. ",
  "gpp_partition/ter_partition: 'NT' or 'DT' or NA. ",
  "LE_F_MDS used for ET; H_F_MDS used for H. ",
  "vut_frac_nee = fraction of qualifying site-years using VUT (vs CUT) for NEE. ",
  "network = 'fluxnet2015' for all rows (added for comparability when combined ",
  "with site_flux_medians_shuttle.csv, which has no such column)."))

# ---- Step 2: IGBP class site counts ------------------------------------------
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
    n_nee = sum(n_years_nee > 0, na.rm = TRUE),
    n_gpp = sum(n_years_gpp > 0, na.rm = TRUE),
    n_ter = sum(n_years_ter > 0, na.rm = TRUE),
    n_et  = sum(n_years_le  > 0, na.rm = TRUE),
    n_h   = sum(n_years_h   > 0, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(flag_low = if_else(
    pmin(n_nee, n_gpp, n_ter, n_et, n_h) < 5L, "n<5", ""
  )) |>
  arrange(match(igbp_type, c(STANDARD_IGBP, "OTHER", "MISSING")))

msg("\nIGBP class counts (n sites with usable data per flux) - FLUXNET2015:")
print(as.data.frame(igbp_counts), row.names = FALSE)

absent <- setdiff(STANDARD_IGBP, igbp_counts$igbp_type)
if (length(absent) > 0L) {
  msg("  IGBP classes ABSENT from FLUXNET2015 network: ", paste(absent, collapse = ", "))
} else {
  msg("  All 12 standard IGBP classes present in FLUXNET2015 site list")
}

low_n <- igbp_counts |> filter(igbp_type %in% STANDARD_IGBP, flag_low == "n<5")
if (nrow(low_n) > 0L) {
  msg("  Classes with n<5 for at least one flux: ", paste(low_n$igbp_type, collapse = ", "))
}

# ---- Step 3: distribution shape per IGBP class -------------------------------
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
  sub <- medians_final |> filter(igbp_class == cls)
  for (fv in flux_vars) {
    med_c <- paste0(fv, "_median")
    vals  <- sub[[med_c]][!is.na(sub[[med_c]])]
    n     <- length(vals)
    igbp_shape_rows[[length(igbp_shape_rows) + 1L]] <- data.frame(
      igbp_class    = cls,
      flux_variable = fv,
      flux_label    = flux_label[[fv]],
      n_sites       = n,
      median_val    = if (n >= 1L) median(vals) else NA_real_,
      q25           = if (n >= 4L) quantile(vals, 0.25) else NA_real_,
      q75           = if (n >= 4L) quantile(vals, 0.75) else NA_real_,
      min_val       = if (n >= 1L) min(vals) else NA_real_,
      max_val       = if (n >= 1L) max(vals) else NA_real_,
      cv            = if (n >= 2L) cv_safe(vals) else NA_real_,
      spread        = if (n >= 2L) {
        cv <- cv_safe(vals)
        if (is.na(cv)) "unknown"
        else if (cv < 0.25) "tight" else if (cv < 0.60) "moderate" else "wide"
      } else "insufficient",
      stringsAsFactors = FALSE
    )
  }
}

igbp_shape <- bind_rows(igbp_shape_rows) |>
  mutate(across(c(median_val, q25, q75, min_val, max_val, cv), ~ round(.x, 3)))

write_csv(igbp_shape, OUT_IGBP)
msg("Saved: ", OUT_IGBP)
write_meta(OUT_IGBP, notes = paste0(
  "Distribution of site-median values per IGBP class x flux, FLUXNET2015 release. ",
  "cv = sd/|mean| across site medians within each class. ",
  "spread: tight cv<0.25, moderate cv<0.60, wide cv>=0.60."))

# ---- Step 4: Comparison vs current FLUXNET Shuttle ---------------------------
msg("\n=== STEP 4: Comparison vs current FLUXNET Shuttle ===")

if (!file.exists(SHUTTLE_MEDIANS_CSV)) {
  msg("  WARNING: ", SHUTTLE_MEDIANS_CSV, " not found - skipping comparison.")
} else {
  shuttle_medians <- read_csv(SHUTTLE_MEDIANS_CSV, show_col_types = FALSE)

  # --- per-flux site count comparison (overall) ---
  msg("\nPer-flux site count comparison (FLUXNET2015 vs Shuttle):")
  for (j in seq_along(flux_vars)) {
    f15_n <- sum(medians_final[[n_col[j]]] > 0, na.rm = TRUE)
    sh_n  <- sum(shuttle_medians[[n_col[j]]] > 0, na.rm = TRUE)
    msg(sprintf("  %-4s  FLUXNET2015: %3d / %3d   Shuttle: %3d / %3d",
                toupper(flux_vars[j]), f15_n, nrow(medians_final),
                sh_n, nrow(shuttle_medians)))
  }

  # --- IGBP-class site counts side-by-side ---
  count_by_class <- function(df) {
    df |>
      filter(igbp_class %in% STANDARD_IGBP) |>
      group_by(igbp_class) |>
      summarise(
        n_sites_total = n(),
        n_nee = sum(n_years_nee > 0, na.rm = TRUE),
        n_gpp = sum(n_years_gpp > 0, na.rm = TRUE),
        n_ter = sum(n_years_ter > 0, na.rm = TRUE),
        n_et  = sum(n_years_le  > 0, na.rm = TRUE),
        n_h   = sum(n_years_h   > 0, na.rm = TRUE),
        .groups = "drop"
      ) |>
      arrange(match(igbp_class, STANDARD_IGBP))
  }

  f15_class_counts <- count_by_class(medians_final)
  sh_class_counts  <- count_by_class(shuttle_medians)

  cmp_counts <- f15_class_counts |>
    rename_with(~ paste0("f15_", .x), -igbp_class) |>
    full_join(sh_class_counts |> rename_with(~ paste0("sh_", .x), -igbp_class),
              by = "igbp_class") |>
    arrange(match(igbp_class, STANDARD_IGBP))

  msg("\nIGBP-class site counts, FLUXNET2015 vs Shuttle:")
  print(as.data.frame(cmp_counts), row.names = FALSE)

  classes_in_f15 <- f15_class_counts$igbp_class
  classes_in_sh  <- sh_class_counts$igbp_class
  absent_in_f15  <- setdiff(classes_in_sh, classes_in_f15)
  absent_in_sh   <- setdiff(classes_in_f15, classes_in_sh)
  if (length(absent_in_f15) > 0L) {
    msg("  Classes present in Shuttle but ABSENT in FLUXNET2015: ",
        paste(absent_in_f15, collapse = ", "))
  }
  if (length(absent_in_sh) > 0L) {
    msg("  Classes present in FLUXNET2015 but ABSENT in Shuttle: ",
        paste(absent_in_sh, collapse = ", "))
  }
  if (length(absent_in_f15) == 0L && length(absent_in_sh) == 0L) {
    msg("  Same set of IGBP classes present in both networks.")
  }

  # --- median comparison: largest shifts per flux ---
  msg("\nMedian comparison (FLUXNET2015 vs Shuttle), per IGBP class per flux:")
  class_median <- function(df) {
    df |>
      filter(igbp_class %in% STANDARD_IGBP) |>
      group_by(igbp_class) |>
      summarise(across(all_of(med_col), ~ median(.x, na.rm = TRUE)), .groups = "drop")
  }
  f15_cmed <- class_median(medians_final)
  sh_cmed  <- class_median(shuttle_medians)

  med_cmp <- f15_cmed |>
    rename_with(~ paste0("f15_", .x), -igbp_class) |>
    full_join(sh_cmed |> rename_with(~ paste0("sh_", .x), -igbp_class),
              by = "igbp_class") |>
    arrange(match(igbp_class, STANDARD_IGBP))

  for (fv in flux_vars) {
    f15c <- paste0("f15_", fv, "_median")
    shc  <- paste0("sh_", fv, "_median")
    med_cmp[[paste0(fv, "_diff")]] <- med_cmp[[f15c]] - med_cmp[[shc]]
    med_cmp[[paste0(fv, "_pct_diff")]] <- 100 * (med_cmp[[f15c]] - med_cmp[[shc]]) /
      abs(med_cmp[[shc]])
  }
  print(as.data.frame(med_cmp), row.names = FALSE)

  msg("\nLargest absolute median shifts per flux (FLUXNET2015 - Shuttle):")
  for (fv in flux_vars) {
    dcol <- paste0(fv, "_diff")
    sub  <- med_cmp |> filter(!is.na(.data[[dcol]])) |>
      arrange(desc(abs(.data[[dcol]])))
    if (nrow(sub) == 0L) next
    top <- head(sub, 3L)
    for (i in seq_len(nrow(top))) {
      msg(sprintf("  %-4s %-4s: F15=%.2f  Shuttle=%.2f  diff=%+.2f (%+.1f%%)",
                  toupper(fv), top$igbp_class[i],
                  top[[paste0("f15_", fv, "_median")]][i],
                  top[[paste0("sh_", fv, "_median")]][i],
                  top[[dcol]][i], top[[paste0(fv, "_pct_diff")]][i]))
    }
  }

  # --- VUT/CUT and NT/DT distribution comparison ---
  msg("\nVUT/CUT and NT/DT distribution comparison:")
  f15_vut_frac <- mean(medians_final$vut_frac_nee, na.rm = TRUE)
  sh_vut_frac  <- mean(shuttle_medians$vut_frac_nee, na.rm = TRUE)
  msg(sprintf("  Mean VUT fraction of site-years: FLUXNET2015=%.3f  Shuttle=%.3f",
              f15_vut_frac, sh_vut_frac))

  f15_gpp_tbl <- table(medians_final$gpp_partition, useNA = "ifany")
  sh_gpp_tbl  <- table(shuttle_medians$gpp_partition, useNA = "ifany")
  f15_ter_tbl <- table(medians_final$ter_partition, useNA = "ifany")
  sh_ter_tbl  <- table(shuttle_medians$ter_partition, useNA = "ifany")
  msg("  GPP partition - FLUXNET2015: ", paste(names(f15_gpp_tbl), f15_gpp_tbl, sep = "=", collapse = "  "))
  msg("  GPP partition - Shuttle:     ", paste(names(sh_gpp_tbl), sh_gpp_tbl, sep = "=", collapse = "  "))
  msg("  TER partition - FLUXNET2015: ", paste(names(f15_ter_tbl), f15_ter_tbl, sep = "=", collapse = "  "))
  msg("  TER partition - Shuttle:     ", paste(names(sh_ter_tbl), sh_ter_tbl, sep = "=", collapse = "  "))
}

msg("\n=== Assessment complete ===")
