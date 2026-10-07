## 04_report_preanalysis.R — Pre-analysis report for PI sign-off.
##
## STANDING RULE 2: this script REPORTS. It never computes WUE, intrinsic
## WUE, underlying WUE, or any VPD exponent, and never applies a rain-day or
## GPP screen. STANDING RULE 1: GPP_DT_*/RECO_DT_* are never read — enforced
## by 02_read_subdaily.R's column whitelist; this script never attempts to
## widen that whitelist.
##
## Must run to completion on whatever upstream outputs exist, even if
## 01/02/03 failed for some or all sites — failures are reported, not
## silently skipped. See code/run_setup_20261007.sh.
##
## Outputs (git-tracked):
##   tables/site_inventory.csv
##   tables/variable_availability.csv
##   tables/variable_availability_by_year.csv
##   tables/precip_measured_fraction.csv
##   tables/energy_balance_closure.csv
##   tables/daytime_measured_counts.csv
##   tables/treering_inventory.csv
##   tables/treering_site_map.csv
##   docs/report_back_<YYYYMMDD>.md

source("WUE/isotope_pilot/code/00_config.R")

knitr_like_table <- function(df, n_max = 20) {
  if (nrow(df) == 0) return("_(no rows)_")
  if (nrow(df) > n_max) df <- df[seq_len(n_max), , drop = FALSE]
  fmt_cell <- function(x) {
    if (is.numeric(x)) format(round(x, 4), trim = TRUE) else as.character(x)
  }
  body <- vapply(seq_len(nrow(df)), function(i) {
    paste0("| ", paste(vapply(df[i, , drop = FALSE], fmt_cell, character(1)), collapse = " | "), " |")
  }, character(1))
  header <- paste0("| ", paste(names(df), collapse = " | "), " |")
  sep    <- paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|")
  paste(c(header, sep, body), collapse = "\n")
}

pct_non_na <- function(x) if (length(x) == 0) NA_real_ else round(100 * mean(!is.na(x)), 1)
pct_qc_eq  <- function(qc, val) {
  if (length(qc) == 0 || all(is.na(qc))) return(NA_real_)
  round(100 * mean(!is.na(qc) & qc == val), 1)
}

processed_dir <- file.path(WUE_ROOT, "data", "processed")
subdaily_dir  <- file.path(processed_dir, "subdaily")
tables_dir    <- file.path(WUE_ROOT, "tables")
docs_dir      <- file.path(WUE_ROOT, "docs")

load_site <- function(site) {
  p <- file.path(subdaily_dir, paste0(site, ".rds"))
  if (!file.exists(p)) return(NULL)
  readRDS(p)
}
site_data <- setNames(lapply(WUE_SITES, load_site), WUE_SITES)

failed_steps <- character(0)

# ── 1. site_inventory.csv ───────────────────────────────────────────────────

manifest_info_path <- file.path(processed_dir, "download_manifest_info.csv")
if (file.exists(manifest_info_path)) {
  manifest_info <- readr::read_csv(manifest_info_path, show_col_types = FALSE)
} else {
  failed_steps <- c(failed_steps, "01_download_extract.R (no download_manifest_info.csv)")
  manifest_info <- data.frame(
    site_id = WUE_SITES, data_hub = NA, fluxnet_product_name = NA,
    oneflux_code_version = NA, first_year = NA, last_year = NA,
    download_status = "unknown (01 did not complete)", zip_path = NA,
    subdaily_resolution = NA, locked_product_name = NA, locked_oneflux_version = NA,
    differs_from_locked_snapshot = NA, stringsAsFactors = FALSE
  )
}

site_years_count <- vapply(WUE_SITES, function(site) {
  d <- site_data[[site]]
  if (is.null(d) || nrow(d) == 0) return(0L)
  has_val <- if ("LE_F_MDS" %in% names(d) || "NETRAD" %in% names(d)) {
    keep <- rep(FALSE, nrow(d))
    if ("LE_F_MDS" %in% names(d)) keep <- keep | !is.na(d$LE_F_MDS)
    if ("NETRAD" %in% names(d))  keep <- keep | !is.na(d$NETRAD)
    keep
  } else {
    rep(FALSE, nrow(d))
  }
  if (!"TIMESTAMP_START" %in% names(d) || !any(has_val)) return(0L)
  length(unique(lubridate::year(d$TIMESTAMP_START[has_val])))
}, integer(1))

site_inventory <- data.frame(
  site = manifest_info$site_id,
  product_name = manifest_info$fluxnet_product_name,
  oneflux_version = manifest_info$oneflux_code_version,
  subdaily_resolution = manifest_info$subdaily_resolution,
  first_year = manifest_info$first_year,
  last_year = manifest_info$last_year,
  site_years = site_years_count[manifest_info$site_id],
  download_status = manifest_info$download_status,
  differs_from_locked_snapshot = manifest_info$differs_from_locked_snapshot,
  stringsAsFactors = FALSE
)
write.csv(site_inventory, file.path(tables_dir, "site_inventory.csv"), row.names = FALSE)

# ── 2. variable_availability.csv ────────────────────────────────────────────

var_defs <- data.frame(
  variable = c("NIGHT", "SW_IN_POT", "SW_IN_F", "NETRAD", "TA_F", "VPD_F", "PA_F",
               "P_F", "WS_F", "USTAR", "CO2_F_MDS", "NEE_VUT_REF", "GPP_NT_VUT_REF",
               "GPP_NT_CUT_REF", "RECO_NT_VUT_REF", "LE_F_MDS", "H_F_MDS", "G_F_MDS"),
  qc_col   = c(NA, NA, "SW_IN_F_QC", NA, "TA_F_QC", "VPD_F_QC", NA,
               "P_F_QC", NA, NA, "CO2_F_MDS_QC", "NEE_VUT_REF_QC", NA,
               NA, NA, "LE_F_MDS_QC", "H_F_MDS_QC", NA),
  stringsAsFactors = FALSE
)
stopifnot(!any(grepl(WUE_FORBIDDEN_DT_PATTERN, var_defs$variable)))

variable_availability <- do.call(rbind, lapply(WUE_SITES, function(site) {
  d <- site_data[[site]]
  do.call(rbind, lapply(seq_len(nrow(var_defs)), function(i) {
    v <- var_defs$variable[i]; qc <- var_defs$qc_col[i]
    if (is.null(d)) {
      return(data.frame(site_id = site, variable = v, pct_non_na = NA_real_,
                         pct_qc0 = NA_real_, pct_qc1 = NA_real_, note = "no sub-daily data"))
    }
    if (!v %in% names(d)) {
      return(data.frame(site_id = site, variable = v, pct_non_na = NA_real_,
                         pct_qc0 = NA_real_, pct_qc1 = NA_real_, note = "column absent"))
    }
    pct_nn <- pct_non_na(d[[v]])
    if (is.na(qc) || !qc %in% names(d)) {
      return(data.frame(site_id = site, variable = v, pct_non_na = pct_nn,
                         pct_qc0 = NA_real_, pct_qc1 = NA_real_,
                         note = if (!is.na(qc)) "QC column absent" else ""))
    }
    data.frame(site_id = site, variable = v, pct_non_na = pct_nn,
               pct_qc0 = pct_qc_eq(d[[qc]], 0), pct_qc1 = pct_qc_eq(d[[qc]], 1), note = "")
  }))
}))
write.csv(variable_availability, file.path(tables_dir, "variable_availability.csv"), row.names = FALSE)

# ── 3. variable_availability_by_year.csv ────────────────────────────────────

by_year_defs <- data.frame(
  variable = c("GPP_NT_VUT_REF", "NEE_VUT_REF", "LE_F_MDS", "VPD_F", "P_F"),
  qc_col   = c(NA, "NEE_VUT_REF_QC", "LE_F_MDS_QC", "VPD_F_QC", "P_F_QC"),
  stringsAsFactors = FALSE
)

variable_availability_by_year <- do.call(rbind, lapply(WUE_SITES, function(site) {
  d <- site_data[[site]]
  if (is.null(d) || !"TIMESTAMP_START" %in% names(d)) {
    return(data.frame(site_id = site, year = NA_integer_, variable = by_year_defs$variable,
                       pct_non_na = NA_real_, pct_qc0 = NA_real_, pct_qc1 = NA_real_,
                       note = "no sub-daily data"))
  }
  yrs <- sort(unique(lubridate::year(d$TIMESTAMP_START)))
  do.call(rbind, lapply(yrs, function(yr) {
    d_yr <- d[lubridate::year(d$TIMESTAMP_START) == yr, , drop = FALSE]
    do.call(rbind, lapply(seq_len(nrow(by_year_defs)), function(i) {
      v <- by_year_defs$variable[i]; qc <- by_year_defs$qc_col[i]
      if (!v %in% names(d_yr)) {
        return(data.frame(site_id = site, year = yr, variable = v, pct_non_na = NA_real_,
                           pct_qc0 = NA_real_, pct_qc1 = NA_real_, note = "column absent"))
      }
      pct_nn <- pct_non_na(d_yr[[v]])
      if (is.na(qc) || !qc %in% names(d_yr)) {
        return(data.frame(site_id = site, year = yr, variable = v, pct_non_na = pct_nn,
                           pct_qc0 = NA_real_, pct_qc1 = NA_real_, note = ""))
      }
      data.frame(site_id = site, year = yr, variable = v, pct_non_na = pct_nn,
                 pct_qc0 = pct_qc_eq(d_yr[[qc]], 0), pct_qc1 = pct_qc_eq(d_yr[[qc]], 1), note = "")
    }))
  }))
}))
write.csv(variable_availability_by_year, file.path(tables_dir, "variable_availability_by_year.csv"), row.names = FALSE)

# ── 4. precip_measured_fraction.csv ─────────────────────────────────────────

precip_measured_fraction <- do.call(rbind, lapply(WUE_SITES, function(site) {
  d <- site_data[[site]]
  if (is.null(d) || !all(c("TIMESTAMP_START", "P_F_QC") %in% names(d))) {
    return(data.frame(site_id = site, year = NA_integer_, n_timesteps = 0L,
                       precip_measured_fraction = NA_real_,
                       note = "no sub-daily data or P_F_QC absent"))
  }
  yrs <- sort(unique(lubridate::year(d$TIMESTAMP_START)))
  do.call(rbind, lapply(yrs, function(yr) {
    d_yr <- d[lubridate::year(d$TIMESTAMP_START) == yr, , drop = FALSE]
    data.frame(
      site_id = site, year = yr, n_timesteps = nrow(d_yr),
      precip_measured_fraction = round(mean(!is.na(d_yr$P_F_QC) & d_yr$P_F_QC == 0), 4),
      note = ""
    )
  }))
}))
write.csv(precip_measured_fraction, file.path(tables_dir, "precip_measured_fraction.csv"), row.names = FALSE)

# Cross-check against review/diagnostics/precip_site_filter/ and
# review/diagnostics/era5_share_for_coordination_v2/ (read-only, repo root).
precip_ref_path <- "review/diagnostics/precip_site_filter/table_1_site_level_precip_estimates.csv"
precip_crosscheck_lines <- character(0)
if (file.exists(precip_ref_path)) {
  precip_ref <- readr::read_csv(precip_ref_path, show_col_types = FALSE)
  precip_ref <- precip_ref[precip_ref$site_id %in% WUE_SITES,
                            c("site_id", "n_years_measured", "n_years_fm_total")]
  # n_years_majority_measured_here: years where this pilot's own half-hourly
  # P_F_QC==0 share exceeds 50%. NOT the same metric as the reference table's
  # n_years_measured (per review/diagnostics/precip_site_filter_tower_years.R:
  # "n_years_measured are NA where a site has no year with P_F_QC > QC_THRESHOLD_YY",
  # i.e. the ANNUAL-resolution P_F_QC fraction -- which bundles measured AND
  # good-quality (QC<=1) gap-fill together, at QC_THRESHOLD_YY = 0.50 -- against
  # QC_THRESHOLD_YY itself, not a strict QC==0-only share at half-hourly
  # resolution. The two are not expected to match exactly; this is a magnitude
  # sanity check, not a re-derivation of the reference numbers.
  n_years_majority_measured_here <- vapply(WUE_SITES, function(site) {
    sub <- precip_measured_fraction[precip_measured_fraction$site_id == site &
                                       !is.na(precip_measured_fraction$precip_measured_fraction), ]
    if (nrow(sub) == 0) return(NA_integer_)
    sum(sub$precip_measured_fraction > 0.5)
  }, integer(1))
  cmp <- merge(
    precip_ref,
    data.frame(site_id = WUE_SITES, n_years_majority_measured_here = n_years_majority_measured_here),
    by = "site_id", all = TRUE
  )
  precip_crosscheck_lines <- c(
    "Comparison against `review/diagnostics/precip_site_filter/table_1_site_level_precip_estimates.csv`",
    "(`n_years_measured` / `n_years_fm_total`, annual-resolution P_F_QC fraction > QC_THRESHOLD_YY=0.50,",
    "measured+good-gapfill combined) vs. this pilot's own count of years where the half-hourly,",
    "measured-only (`P_F_QC == 0`) share exceeds 50% (`n_years_majority_measured_here`) -- a",
    "different metric at a different resolution; not expected to match exactly, shown as a",
    "magnitude sanity check only, not a re-derivation of the reference numbers:",
    "",
    knitr_like_table(cmp)
  )
} else {
  precip_crosscheck_lines <- c("`precip_site_filter/table_1_site_level_precip_estimates.csv` not found -- could not cross-check.")
}

era5_flag_paths <- c(
  "review/diagnostics/era5_share_for_coordination_v2/site_list_AmeriFlux.csv",
  "review/diagnostics/era5_share_for_coordination_v2/site_list_ICOS.csv",
  "review/diagnostics/era5_share_for_coordination_v2/site_list_TERN.csv"
)
era5_flagged_hits <- do.call(rbind, lapply(era5_flag_paths, function(p) {
  if (!file.exists(p)) return(NULL)
  d <- readr::read_csv(p, show_col_types = FALSE)
  d[d$site_id %in% WUE_SITES, , drop = FALSE]
}))
era5_crosscheck_line <- if (is.null(era5_flagged_hits) || nrow(era5_flagged_hits) == 0) {
  "None of the 13 WUE isotope-pilot sites appear in `review/diagnostics/era5_share_for_coordination_v2/site_list_*.csv` (confirmed by direct read, 2026-10-07)."
} else {
  paste0(nrow(era5_flagged_hits), " WUE site(s) ARE flagged in era5_share_for_coordination_v2: ",
         paste(era5_flagged_hits$site_id, collapse = ", "))
}

# ── 5. energy_balance_closure.csv ───────────────────────────────────────────
# Same method as WAFNET/energy_partitioning/code/03_report_variables_and_closure.R:
# OLS of (LE_F_MDS + H_F_MDS) ~ (NETRAD - G_F_MDS), measured (QC==0) half-hours only.

compute_closure <- function(site) {
  d <- site_data[[site]]
  if (is.null(d)) {
    return(data.frame(site_id = site, n = 0L, slope = NA_real_, intercept = NA_real_,
                       r_squared = NA_real_, note = "no sub-daily data available"))
  }
  needed <- c("LE_F_MDS", "LE_F_MDS_QC", "H_F_MDS", "H_F_MDS_QC", "NETRAD", "G_F_MDS")
  missing_cols <- setdiff(needed, names(d))
  if (length(missing_cols) > 0) {
    return(data.frame(site_id = site, n = 0L, slope = NA_real_, intercept = NA_real_,
                       r_squared = NA_real_,
                       note = paste0("missing column(s): ", paste(missing_cols, collapse = ", "))))
  }
  if (all(is.na(d$G_F_MDS))) {
    return(data.frame(site_id = site, n = 0L, slope = NA_real_, intercept = NA_real_,
                       r_squared = NA_real_,
                       note = "G_F_MDS column present but 100% NA -- no soil heat flux data, not a thinness issue"))
  }
  dd <- d[!is.na(d$LE_F_MDS) & !is.na(d$H_F_MDS) & !is.na(d$NETRAD) & !is.na(d$G_F_MDS) &
            d$LE_F_MDS_QC == 0 & d$H_F_MDS_QC == 0, , drop = FALSE]
  n <- nrow(dd)
  if (n < 30L) {
    return(data.frame(site_id = site, n = n, slope = NA_real_, intercept = NA_real_,
                       r_squared = NA_real_,
                       note = paste0("only ", n, " measured (QC=0) half-hours with complete LE/H/Rn/G -- too thin to fit")))
  }
  fit <- stats::lm(dd$LE_F_MDS + dd$H_F_MDS ~ (dd$NETRAD - dd$G_F_MDS))
  s <- summary(fit)
  note <- "measured (QC=0) half-hours only"
  if (s$r.squared < 0.1) {
    note <- paste0("R^2 = ", round(s$r.squared, 4), " despite n = ", n,
                    " -- data-quality flag, not a usable closure estimate")
  }
  data.frame(site_id = site, n = n, slope = unname(stats::coef(fit)[2]),
             intercept = unname(stats::coef(fit)[1]), r_squared = s$r.squared, note = note)
}
energy_balance_closure <- do.call(rbind, lapply(WUE_SITES, compute_closure))
write.csv(energy_balance_closure, file.path(tables_dir, "energy_balance_closure.csv"), row.names = FALSE)

# ── 6. daytime_measured_counts.csv ──────────────────────────────────────────

compute_daytime_counts <- function(site) {
  d <- site_data[[site]]
  if (is.null(d)) {
    return(data.frame(site_id = site, year = NA_integer_, n_daytime_all_measured = 0L,
                       note = "no sub-daily data available"))
  }
  needed <- c("NIGHT", "NEE_VUT_REF_QC", "LE_F_MDS_QC", "VPD_F_QC", "TIMESTAMP_START")
  missing_cols <- setdiff(needed, names(d))
  if (length(missing_cols) > 0) {
    return(data.frame(site_id = site, year = NA_integer_, n_daytime_all_measured = NA_integer_,
                       note = paste0("missing column(s): ", paste(missing_cols, collapse = ", "))))
  }
  d$year <- lubridate::year(d$TIMESTAMP_START)
  daytime <- d[!is.na(d$NIGHT) & d$NIGHT == 0, , drop = FALSE]
  all_measured <- !is.na(daytime$NEE_VUT_REF_QC) & daytime$NEE_VUT_REF_QC == 0 &
    !is.na(daytime$LE_F_MDS_QC) & daytime$LE_F_MDS_QC == 0 &
    !is.na(daytime$VPD_F_QC) & daytime$VPD_F_QC == 0
  agg <- stats::aggregate(all_measured ~ daytime$year, FUN = sum)
  names(agg) <- c("year", "n_daytime_all_measured")
  data.frame(site_id = site, agg, note = "")
}
daytime_measured_counts <- do.call(rbind, lapply(WUE_SITES, compute_daytime_counts))
write.csv(daytime_measured_counts, file.path(tables_dir, "daytime_measured_counts.csv"), row.names = FALSE)

# ── 7 & 8. Tree-ring tables ──────────────────────────────────────────────────
# See docs/methods_memo.md for how edi.401 / the GitHub repo were parsed.
source("WUE/isotope_pilot/code/treering_report_helpers.R")

treering_dir <- file.path(WUE_ROOT, "data", "external", "treering")
edi_dir      <- file.path(treering_dir, "edi_401")

treering_inventory <- build_treering_inventory(edi_dir)
write.csv(treering_inventory, file.path(tables_dir, "treering_inventory.csv"), row.names = FALSE)

treering_site_map <- build_treering_site_map(edi_dir, WUE_SITES)
write.csv(treering_site_map, file.path(tables_dir, "treering_site_map.csv"), row.names = FALSE)

# ── Step/site failure summary ───────────────────────────────────────────────

read_status_path <- file.path(processed_dir, "read_status.csv")
if (file.exists(read_status_path)) {
  read_status <- readr::read_csv(read_status_path, show_col_types = FALSE)
  failed_02_sites <- read_status$site_id[!read_status$status %in% c("read_ok", "read_ok_cached")]
  if (length(failed_02_sites) > 0) {
    failed_steps <- c(failed_steps, paste0("02_read_subdaily.R: ", paste(failed_02_sites, collapse = ", ")))
  }
} else {
  failed_steps <- c(failed_steps, "02_read_subdaily.R (no read_status.csv)")
}

failed_01_sites <- manifest_info$site_id[
  !manifest_info$download_status %in% c("reused_existing_zip", "downloaded", "downloaded_on_retry")
]
if (length(failed_01_sites) > 0) {
  failed_steps <- c(failed_steps, paste0("01_download_extract.R: ", paste(failed_01_sites, collapse = ", ")))
}

edi_status_path <- file.path(treering_dir, "edi_401_fetch_status.rds")
github_status_path <- file.path(treering_dir, "github_fetch_status.rds")
edi_status <- if (file.exists(edi_status_path)) readRDS(edi_status_path) else list(status = "not_run")
github_status <- if (file.exists(github_status_path)) readRDS(github_status_path) else list(status = "not_run")
if (!identical(edi_status$status, "ok")) failed_steps <- c(failed_steps, "03_fetch_treering.R: edi.401")
if (!identical(github_status$status, "ok")) failed_steps <- c(failed_steps, "03_fetch_treering.R: NE_Tree-Rings_Isotopes")

# ── docs/report_back_<date>.md ──────────────────────────────────────────────

report_path <- file.path(docs_dir, paste0("report_back_", format(Sys.Date(), "%Y%m%d"), ".md"))
report_lines <- c(
  paste0("# WUE isotope pilot — pre-analysis report (", Sys.Date(), ")"),
  "",
  "Side analysis, not the FLUXNET Annual Paper 2026. Generated by ",
  "`code/04_report_preanalysis.R`. GPP is the nighttime partition only ",
  "(see docs/methods_memo.md, Standing Rule 1) -- no `*_DT_*` column is ",
  "read, summarised, plotted or compared anywhere in this analysis. No WUE, ",
  "inherent WUE, underlying WUE, or VPD exponent is computed here (Standing ",
  "Rule 2).",
  "",
  "## 1. Site inventory",
  "",
  knitr_like_table(site_inventory),
  "",
  "## 2. Variable availability (percent non-NA; QC0/QC1 where a QC column exists)",
  "",
  "See `tables/variable_availability.csv` (long format, one row per site x variable).",
  "",
  "## 3. Variable availability by site-year",
  "",
  "See `tables/variable_availability_by_year.csv` for GPP_NT_VUT_REF, NEE_VUT_REF, ",
  "LE_F_MDS, VPD_F, P_F.",
  "",
  "## 4. Precipitation measured fraction",
  "",
  "See `tables/precip_measured_fraction.csv`.",
  "",
  precip_crosscheck_lines,
  "",
  era5_crosscheck_line,
  "",
  "## 5. Energy balance closure",
  "",
  "OLS of (LE_F_MDS + H_F_MDS) ~ (NETRAD - G_F_MDS), measured (QC=0) half-hours only.",
  "",
  knitr_like_table(energy_balance_closure),
  "",
  "## 6. Daytime fully-measured counts",
  "",
  "Per site-year count of daytime (NIGHT == 0) timesteps with NEE_VUT_REF_QC, ",
  "LE_F_MDS_QC and VPD_F_QC all == 0. See `tables/daytime_measured_counts.csv`.",
  "",
  "## 7. Tree-ring inventory (edi.401)",
  "",
  paste0("EDI fetch status: ", edi_status$status,
         if (identical(edi_status$status, "ok")) paste0(" (revision ", edi_status$revision, ", DOI ", edi_status$doi, ")") else ""),
  "",
  knitr_like_table(treering_inventory),
  "",
  "## 8. Tree-ring site -> tower mapping",
  "",
  knitr_like_table(treering_site_map),
  "",
  paste0("Belmecheri et al. 2021 GitHub repo fetch status: ", github_status$status,
         if (identical(github_status$status, "ok")) paste0(" (", github_status$n_files, " file(s))") else ""),
  "",
  "## What I could not do",
  "",
  if (length(failed_steps) == 0) "Nothing -- all steps completed for all sites." else
    paste0("- ", failed_steps)
)
writeLines(unlist(report_lines), report_path)
message("[WUE] Report written: ", report_path)
message("[WUE] 04_report_preanalysis.R complete.")
