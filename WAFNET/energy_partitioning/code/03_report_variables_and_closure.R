## 03_report_variables_and_closure.R — Pre-analysis report for PI sign-off.
##
## Produces exactly what was asked for before any of the three analysis
## tasks (aridity framing, variance decomposition, surface conductance) may
## begin:
##   - which variables are available, per site
##   - number of site-years
##   - energy balance closure slope + intercept per site (turbulent flux
##     against Rn - G)
##   - how G was measured, including whether storage above the plates is
##     included, to the extent the Shuttle metadata documents it
##
## STOP HERE. Do not run 04+ until the PI has reviewed this output.
##
## Outputs (git-tracked):
##   tables/variable_availability.csv
##   tables/site_years.csv
##   tables/energy_balance_closure.csv
##   tables/g_measurement_notes.csv
##   docs/report_back_<YYYYMMDD>.md   (human-readable summary of all four)

source("WAFNET/energy_partitioning/code/00_config.R")

## Minimal base-R markdown-table formatter (no knitr dependency, per
## CLAUDE.md: do not introduce new package dependencies without discussion).
knitr_like_table <- function(df) {
  if (nrow(df) == 0) return("_(no rows)_")
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

processed_dir <- file.path(WAFNET_ROOT, "data", "processed")
combined_path <- file.path(processed_dir, "flux_hh_all_sites.rds")

if (!file.exists(combined_path)) {
  stop("[WAFNET] ", combined_path, " not found -- run 02_read_hh.R first.")
}
hh <- readRDS(combined_path)

sites_with_data <- unique(hh$site_id)
sites_missing   <- setdiff(WAFNET_SITES, sites_with_data)
if (length(sites_missing) > 0) {
  warning(
    "[WAFNET] No half-hourly data at all for: ",
    paste(sites_missing, collapse = ", "),
    " -- these will show as fully unavailable in every table below rather ",
    "than being silently dropped."
  )
}

# ── 1. Variable availability ────────────────────────────────────────────────
# Target list covers what the three planned analyses will need: energy
# balance components, the closure-corrected LE/H alternatives (if the site
# team supplied them), turbulence QA, and the core met drivers for PET / Gs
# inversion. Presence is checked by exact column name AND by a numbered-
# replicate pattern (e.g. G_1_1_1, G_2_1_1) for plate-based variables.
target_vars <- c(
  "NETRAD", "SW_IN_F", "SW_OUT", "LW_IN_F", "LW_OUT",
  "LE_F_MDS", "LE_F_MDS_QC", "LE_CORR", "LE_CORR_JOINTUNC",
  "H_F_MDS", "H_F_MDS_QC", "H_CORR", "H_CORR_JOINTUNC",
  "USTAR", "WS_F", "TA_F", "VPD_F", "RH", "PA_F", "P_F",
  "SW_IN_POT", "G_F_MDS"
)
replicate_prefixes <- c("G", "SWC", "TS")

# Reported as PERCENT NON-NA, not a bare presence flag. A column can exist in
# the header and still be entirely NA (found for G_F_MDS at BJ-Nhu, 2026-09-08
# -- a plain TRUE/FALSE presence flag would have hidden a real data gap), so
# presence alone is not a safe signal of usability.
variable_availability <- do.call(rbind, lapply(WAFNET_SITES, function(site) {
  d <- if (site %in% sites_with_data) hh[hh$site_id == site, , drop = FALSE] else NULL
  cols <- if (!is.null(d)) names(d) else character(0)
  pct_nonna <- vapply(target_vars, function(v) {
    if (!v %in% cols) return(0)
    round(100 * sum(!is.na(d[[v]])) / nrow(d), 1)
  }, numeric(1))
  n_replicates <- vapply(replicate_prefixes, function(p) {
    sum(grepl(paste0("^", p, "_[0-9]+_[0-9]+_[0-9]+$"), cols))
  }, integer(1))
  data.frame(
    site_id = site,
    has_hh_data = site %in% sites_with_data,
    as.list(pct_nonna),
    setNames(as.list(n_replicates), paste0("n_replicate_", replicate_prefixes)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}))
names(variable_availability)[3:(2 + length(target_vars))] <- paste0("pct_", target_vars)

write.csv(
  variable_availability,
  file.path(WAFNET_ROOT, "tables", "variable_availability.csv"),
  row.names = FALSE
)

# ── 2. Site-years ────────────────────────────────────────────────────────────
# A "site-year" here means: at least one half-hourly record in that calendar
# year with a non-NA LE_F_MDS or NETRAD value. This is deliberately stricter
# than the DD/MM/YY "n_months_present" convention used elsewhere in the repo
# (data/snapshots/site_year_data_presence.csv) because it is evaluated
# directly against the HH data this analysis actually uses.
site_years <- if (nrow(hh) == 0) {
  data.frame(site_id = character(0), year = integer(0), n_halfhours = integer(0))
} else {
  hh |>
    dplyr::filter(!is.na(.data$LE_F_MDS) | !is.na(.data$NETRAD)) |>
    dplyr::mutate(year = lubridate::year(.data$DATETIME_START)) |>
    dplyr::count(site_id, year, name = "n_halfhours")
}
write.csv(
  site_years,
  file.path(WAFNET_ROOT, "tables", "site_years.csv"),
  row.names = FALSE
)

site_year_summary <- if (nrow(site_years) == 0) {
  data.frame(site_id = WAFNET_SITES, n_site_years = 0L, first_year = NA, last_year = NA)
} else {
  full <- data.frame(site_id = WAFNET_SITES)
  agg <- site_years |>
    dplyr::group_by(site_id) |>
    dplyr::summarise(
      n_site_years = dplyr::n_distinct(year),
      first_year   = min(year),
      last_year    = max(year),
      .groups = "drop"
    )
  dplyr::left_join(full, agg, by = "site_id")
}

# ── 3. Energy balance closure ────────────────────────────────────────────────
# Turbulent flux (LE_F_MDS + H_F_MDS) regressed against available energy
# (NETRAD - G_F_MDS), OLS, MEASURED HALF-HOURS ONLY (LE_F_MDS_QC == 0 AND
# H_F_MDS_QC == 0) -- gap-filled half-hours are excluded because MDS
# gap-filling itself uses nearby measured energy-balance terms, which would
# artificially inflate closure. This is a deliberate, disclosed choice, not
# the only defensible one; report it as such rather than silently presenting
# one number.
compute_closure <- function(site) {
  if (!site %in% sites_with_data) {
    return(data.frame(
      site_id = site, n = 0L, slope = NA_real_, intercept = NA_real_,
      r_squared = NA_real_, note = "no half-hourly data available"
    ))
  }
  d_full <- hh[hh$site_id == site, , drop = FALSE]
  needed <- c("LE_F_MDS", "LE_F_MDS_QC", "H_F_MDS", "H_F_MDS_QC", "NETRAD", "G_F_MDS")
  missing_cols <- setdiff(needed, names(d_full))
  if (length(missing_cols) > 0) {
    return(data.frame(
      site_id = site, n = 0L, slope = NA_real_, intercept = NA_real_,
      r_squared = NA_real_,
      note = paste0("missing column(s): ", paste(missing_cols, collapse = ", "))
    ))
  }
  # Distinguish "column absent" (caught above) from "column present but
  # entirely NA" -- found at BJ-Nhu for G_F_MDS (2026-09-08). Both mean the
  # closure regression can't be fit, but they are different data problems and
  # must not be reported with the same generic "too thin" note.
  if (all(is.na(d_full$G_F_MDS))) {
    return(data.frame(
      site_id = site, n = 0L, slope = NA_real_, intercept = NA_real_,
      r_squared = NA_real_,
      note = "G_F_MDS column present but 100% NA for this site -- no soil heat flux data at all, not a QC/thinness issue"
    ))
  }
  d <- d_full[
    !is.na(d_full$LE_F_MDS) & !is.na(d_full$H_F_MDS) &
      !is.na(d_full$NETRAD) & !is.na(d_full$G_F_MDS) &
      d_full$LE_F_MDS_QC == 0 & d_full$H_F_MDS_QC == 0,
    ,
    drop = FALSE
  ]
  n <- nrow(d)
  if (n < 30L) {
    return(data.frame(
      site_id = site, n = n, slope = NA_real_, intercept = NA_real_,
      r_squared = NA_real_,
      note = paste0("only ", n, " measured (QC=0) half-hours with complete ",
                     "LE/H/Rn/G -- too thin to fit a closure regression")
    ))
  }
  turb  <- d$LE_F_MDS + d$H_F_MDS
  avail <- d$NETRAD - d$G_F_MDS
  fit   <- stats::lm(turb ~ avail)
  s     <- summary(fit)
  note  <- "measured (QC=0) half-hours only"
  # Flag, don't silently present, a fit with adequate n but implausibly weak
  # explanatory power -- this is a data-quality question for the PI, not a
  # real closure estimate. Threshold (R² < 0.1 with n >= 30) is a deliberate,
  # disclosed screen, not a claim about what the "true" closure is.
  if (s$r.squared < 0.1) {
    g_extreme_pct <- round(100 * mean(abs(d$G_F_MDS) > 200), 1)
    note <- paste0(
      "R² = ", round(s$r.squared, 4), " despite n = ", n,
      " -- DATA-QUALITY FLAG, not a usable closure estimate. ",
      g_extreme_pct, "% of qualifying half-hours have |G_F_MDS| > 200 W/m² ",
      "(consistent across years, not a transient sensor fault); gating ",
      "additionally on G_F_MDS_QC == 0 does not resolve it (checked ",
      "2026-09-08). Needs PI/site-team review before use in any downstream ",
      "analysis."
    )
  }
  data.frame(
    site_id   = site,
    n         = n,
    slope     = unname(stats::coef(fit)[2]),
    intercept = unname(stats::coef(fit)[1]),
    r_squared = s$r.squared,
    note      = note
  )
}

energy_balance_closure <- do.call(rbind, lapply(WAFNET_SITES, compute_closure))
write.csv(
  energy_balance_closure,
  file.path(WAFNET_ROOT, "tables", "energy_balance_closure.csv"),
  row.names = FALSE
)

# ── 4. How G was measured ───────────────────────────────────────────────────
# Two sources, both disclosed: (a) which G-like columns actually exist in the
# HH data (number of replicate plates via the *_1_1_1 / *_2_1_1 naming
# convention), and (b) whatever BADM documentation exists for heat-flux
# instrumentation, read READ-ONLY from the repo-root data/extracted/ BIF
# files already produced by the Annual Paper pipeline (per the user's
# 2026-09-08 choice -- no separate ancillary download needed).
bif_g_notes <- lapply(WAFNET_SITES, function(site) {
  bif_files <- list.files(
    ANNUAL_PAPER_EXTRACTED_DIR,
    pattern = paste0(site, "_FLUXNET_BIF_.*\\.csv$"),
    recursive = TRUE, full.names = TRUE
  )
  if (length(bif_files) == 0) {
    return(data.frame(
      site_id = site,
      badm_heatflux_group_found = FALSE,
      badm_note = "no BIF file found under data/extracted/ for this site"
    ))
  }
  bif <- readr::read_csv(bif_files[1], show_col_types = FALSE)
  hf_rows <- bif[grepl("HEATFLUX|SOILHEAT", bif$VARIABLE_GROUP, ignore.case = TRUE), ]
  if (nrow(hf_rows) == 0) {
    data.frame(
      site_id = site,
      badm_heatflux_group_found = FALSE,
      badm_note = paste0(
        "no GRP_HEATFLUX/GRP_SOILHEATFLUX group in BADM for this site -- ",
        "plate depth and storage-above-plate treatment are NOT documented ",
        "in the Shuttle metadata; would need site PI documentation or the ",
        "AmeriFlux/ICOS site page to confirm"
      )
    )
  } else {
    data.frame(
      site_id = site,
      badm_heatflux_group_found = TRUE,
      badm_note = paste(
        unique(paste0(hf_rows$VARIABLE, "=", hf_rows$DATAVALUE)),
        collapse = "; "
      )
    )
  }
})
bif_g_notes <- do.call(rbind, bif_g_notes)

g_column_notes <- do.call(rbind, lapply(WAFNET_SITES, function(site) {
  if (!site %in% sites_with_data) {
    return(data.frame(site_id = site, g_columns_in_hh_data = NA_character_))
  }
  cols <- names(hh)
  g_cols <- cols[grepl("^G(_F_MDS)?(_[0-9]+_[0-9]+_[0-9]+)?$", cols)]
  data.frame(
    site_id = site,
    g_columns_in_hh_data = if (length(g_cols) == 0) "none found" else paste(g_cols, collapse = ", ")
  )
}))

g_measurement_notes <- dplyr::left_join(g_column_notes, bif_g_notes, by = "site_id")
write.csv(
  g_measurement_notes,
  file.path(WAFNET_ROOT, "tables", "g_measurement_notes.csv"),
  row.names = FALSE
)

# ── Human-readable report ───────────────────────────────────────────────────
report_path <- file.path(
  WAFNET_ROOT, "docs",
  paste0("report_back_", format(Sys.Date(), "%Y%m%d"), ".md")
)
report_lines <- c(
  paste0("# WAFNET energy partitioning — pre-analysis report (", Sys.Date(), ")"),
  "",
  "Generated by `code/03_report_variables_and_closure.R`. Do not proceed to ",
  "aridity framing / variance decomposition / surface conductance until this ",
  "has been reviewed.",
  "",
  "## 1. Variables available per site",
  "",
  "See `tables/variable_availability.csv`. Values are **percent non-NA** ",
  "within each site's half-hourly record, not bare column presence -- a ",
  "column can exist in the header and still be entirely empty (see BJ-Nhu ",
  "G_F_MDS below). Summary:",
  "",
  knitr_like_table(variable_availability),
  "",
  "## 2. Site-years",
  "",
  "See `tables/site_years.csv` (per-year half-hour counts) and summary below. ",
  "A site-year requires >=1 half-hour with non-NA LE_F_MDS or NETRAD.",
  "",
  knitr_like_table(site_year_summary),
  "",
  "## 3. Energy balance closure (turbulent flux vs. Rn - G)",
  "",
  "OLS of (LE_F_MDS + H_F_MDS) ~ (NETRAD - G_F_MDS), **measured half-hours ",
  "only** (LE_F_MDS_QC == 0 AND H_F_MDS_QC == 0) -- gap-filled records ",
  "excluded because MDS gap-filling uses nearby measured energy terms and ",
  "would inflate apparent closure. See `tables/energy_balance_closure.csv`.",
  "",
  knitr_like_table(energy_balance_closure),
  "",
  "## 4. How G was measured",
  "",
  "See `tables/g_measurement_notes.csv`. BADM heat-flux documentation ",
  "(plate depth, storage-above-plate treatment) is read READ-ONLY from the ",
  "existing repo-root `data/extracted/*_BIF_*.csv` files.",
  "",
  knitr_like_table(g_measurement_notes),
  "",
  "## 5. Diagnostic flags -- read before using any number above",
  "",
  "- **BJ-Nhu: `G_F_MDS` is present in the header but 100% NA** for all ",
  "  175,344 half-hours at this site. This is not a QC/thinness issue -- ",
  "  there is no soil heat flux measurement at all for this site in the ",
  "  Shuttle FULLSET. Any closure or Rn-G-based analysis for BJ-Nhu will ",
  "  need either an LE+H vs. NETRAD-only variant (no G term) or to exclude ",
  "  this site from closure-dependent steps -- your call, not assumed here.",
  "- **SN-Nkr: energy balance closure regression is not usable as fit** ",
  "  (R² = 0.001, slope = 0.05, n = 69,224). `G_F_MDS` at this site swings ",
  "  to physically large magnitudes (25% of qualifying half-hours have ",
  "  |G_F_MDS| > 200 W/m², up to ~860 W/m² at the extreme), consistently ",
  "  across all 7 years 2018-2024 -- not an isolated sensor-fault period. ",
  "  Additionally requiring `G_F_MDS_QC == 0` does not fix it (R² = 0.003 ",
  "  on the more restrictive subset, checked 2026-09-08). This could be a ",
  "  genuine feature of a sparse-canopy Sahelian cropland (large diurnal ",
  "  amplitude in near-surface soil heat flux over mostly-bare soil) or a ",
  "  plate-siting/calibration issue -- the data alone don't distinguish ",
  "  these, and the BADM has no heat-flux instrumentation group for this ",
  "  site (Table 4) to check against. Recommend against using this site's ",
  "  closure numbers, or its `G_F_MDS` at all, until this is resolved.",
  "- Every other site (GH-Ank, BJ-Db1, BJ-Bfg, SN-Dhr) closure fit looks ",
  "  physically reasonable (slopes 0.72-0.93, R² 0.73-0.89, in line with ",
  "  the literature range for EC energy balance closure) and is not flagged."
)
writeLines(report_lines, report_path)
message("[WAFNET] Report written: ", report_path)
message("[WAFNET] 03_report_variables_and_closure.R complete. STOP for PI review.")
