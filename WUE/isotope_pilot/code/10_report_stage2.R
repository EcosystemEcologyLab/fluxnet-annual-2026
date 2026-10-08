## 10_report_stage2.R — Stage 2 report. Short: what was run, the numbers
## that describe the outputs, and "What I could not do". No interpretation
## (no trend tests, no site rankings, no statements about what the series
## mean -- STANDING RULE 2, revised for stage 2: screens/metrics are now
## computed, but still not interpreted).
##
## Output (docs/, git-tracked):
##   report_back_stage2_<YYYYMMDD>.md

source("WUE/isotope_pilot/code/00_config.R")

tables_dir <- file.path(WUE_ROOT, "tables")
docs_dir   <- file.path(WUE_ROOT, "docs")

knitr_like_table <- function(df, n_max = 60) {
  if (is.null(df) || nrow(df) == 0) return("_(no rows)_")
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

read_or_null <- function(path) if (file.exists(path)) readr::read_csv(path, show_col_types = FALSE) else NULL

site_product     <- read_or_null(file.path(tables_dir, "site_product.csv"))
p_era_check       <- read_or_null(file.path(tables_dir, "p_era_check.csv"))
netrad_fits       <- read_or_null(file.path(tables_dir, "netrad_fits.csv"))
years_dropped     <- read_or_null(file.path(tables_dir, "years_dropped.csv"))
screen_attrition  <- read_or_null(file.path(tables_dir, "screen_attrition.csv"))
wue_annual        <- read_or_null(file.path(tables_dir, "wue_annual.csv"))

## ---- Expected years-dropped list, per the user's stage 2 brief, for
## comparison only -- not used to force or override the derived list. ------
expected_dropped <- tibble::tribble(
  ~site_id, ~year,
  "US-Ha1", 1991L,
  "BE-Vie", 1996L,
  "DE-Tha", 1996L,
  "FI-Hyy", 1999L,
  "FI-Hyy", 2000L,
  "US-SP1", 2000L,
  "US-SP1", 2002L,
  "US-SP1", 2014L,
  "US-Dk2", 2001L,
  "US-Fuf", 2005L,
  "US-Bar", 2018L,
  "US-Bar", 2022L,
  "US-Slt", 2022L
)

dropped_comparison_lines <- character(0)
if (!is.null(years_dropped)) {
  derived_completeness <- years_dropped[years_dropped$reason != "year 2026 (current, incomplete by instruction)", c("site_id", "year")]
  derived_key  <- paste(derived_completeness$site_id, derived_completeness$year)
  expected_key <- paste(expected_dropped$site_id, expected_dropped$year)
  only_expected <- expected_dropped[!expected_key %in% derived_key, ]
  only_derived  <- derived_completeness[!derived_key %in% expected_key, ]
  dropped_comparison_lines <- c(
    "Comparison against the expected list in the stage 2 brief (completeness-based drops only, excluding 2026):",
    "",
    if (nrow(only_expected) == 0 && nrow(only_derived) == 0) {
      "Matches exactly."
    } else {
      c(
        if (nrow(only_expected) > 0) paste0(
          "- Expected but NOT found in the derived list: ",
          paste(paste0(only_expected$site_id, " ", only_expected$year), collapse = ", ")
        ) else NULL,
        if (nrow(only_derived) > 0) paste0(
          "- Found in the derived list but NOT in the expected list: ",
          paste(paste0(only_derived$site_id, " ", only_derived$year), collapse = ", ")
        ) else NULL
      )
    }
  )
}

## ---- Units check summary (recomputed here for the report text only) ------
zhou_uwue_range <- c(3.50, 15.83); zhou_iwue_range <- c(5.32, 62.31)
units_lines <- if (!is.null(wue_annual)) {
  c(
    paste0("uWUE_y range here: ", round(min(wue_annual$uWUE_y, na.rm = TRUE), 3), " to ",
           round(max(wue_annual$uWUE_y, na.rm = TRUE), 3),
           " (mean ", round(mean(wue_annual$uWUE_y, na.rm = TRUE), 3), ") g C hPa^0.5 kg H2O-1 -- ",
           "Zhou et al. (2015): ", zhou_uwue_range[1], "-", zhou_uwue_range[2], " (mean 9.47)."),
    paste0("IWUE_y range here: ", round(min(wue_annual$IWUE_y, na.rm = TRUE), 3), " to ",
           round(max(wue_annual$IWUE_y, na.rm = TRUE), 3),
           " (mean ", round(mean(wue_annual$IWUE_y, na.rm = TRUE), 3), ") g C hPa kg H2O-1 -- ",
           "Zhou et al. (2015): ", zhou_iwue_range[1], "-", zhou_iwue_range[2], " (mean 33.62).")
  )
} else "wue_annual.csv not available."

## ---- What I could not do --------------------------------------------------
failed_steps <- character(0)
if (is.null(wue_annual) || nrow(wue_annual) == 0) {
  failed_steps <- c(failed_steps, "08_compute_metrics.R produced no site-year rows.")
}
expected_sites <- WUE_SITES_STAGE2
sites_with_annual <- if (!is.null(wue_annual)) unique(wue_annual$site) else character(0)
missing_sites <- setdiff(expected_sites, sites_with_annual)
if (length(missing_sites) > 0) {
  failed_steps <- c(failed_steps, paste0(
    "No wue_annual.csv rows for: ", paste(missing_sites, collapse = ", "),
    " -- see read_status_wue.csv / p_era_check.csv / years_dropped.csv for why."
  ))
}
failed_steps <- c(failed_steps,
  "03_fetch_treering.R / tree-ring intrinsic WUE: out of scope for stage 2, not run (README.md)."
)

report_path <- file.path(docs_dir, paste0("report_back_stage2_", format(Sys.Date(), "%Y%m%d"), ".md"))
report_lines <- c(
  paste0("# WUE isotope pilot — stage 2 report (", Sys.Date(), ")"),
  "",
  "Side analysis, not the FLUXNET Annual Paper 2026. Generated by ",
  "`code/10_report_stage2.R`. GPP is still the nighttime partition only ",
  "(Standing Rule 1, unchanged). This stage computes WUE, IWUE, uWUE, and ",
  "the VPD exponent k* (Standing Rule 2, revised) -- it does not interpret ",
  "them: no trend tests, no site rankings, no statements about what the ",
  "series mean. Tree-ring data remain out of scope.",
  "",
  "## 1. Sites",
  "",
  paste0(length(WUE_SITES_STAGE2), " sites (CH-Dav dropped by PI decision -- stage 1 closure ",
         "slope 0.46, r2 0.56, and three years with no nighttime GPP; its files are left on disk, ",
         "unread by stage 2): ", paste(WUE_SITES_STAGE2, collapse = ", ")),
  "",
  "## 2. Product choice (VUT where available, CUT where not; NEE/GPP/RECO from the same product)",
  "",
  "Per-site, same rule as `R/site_annual_fluxes.R` (`.compute_site_annual_fluxes_core()`), ",
  "applied against the sub-daily data rather than annual DuckDB rows.",
  "",
  knitr_like_table(site_product),
  "",
  "## 3. P_ERA integrity check",
  "",
  "Mean annual sub-daily P_ERA sum (this pilot's tower years) vs. ",
  "`review/diagnostics/precip_site_filter/table_1_site_level_precip_estimates.csv`'s ",
  "`p_era_mean_mm_tower_years`, required within 2% (hard stop on failure):",
  "",
  knitr_like_table(p_era_check),
  "",
  "## 4. Net radiation gap-fill fits (NETRAD ~ SW_IN_F, per site)",
  "",
  knitr_like_table(netrad_fits),
  "",
  "## 5. Years dropped (completeness < 80% nighttime GPP, or year 2026)",
  "",
  knitr_like_table(years_dropped),
  "",
  dropped_comparison_lines,
  "",
  "## 6. Screen attrition",
  "",
  "See `tables/screen_attrition.csv` (per site-year: days in year, days with P_ERA > 0, ",
  "days removed by the rain rule, days lost to the quality/daylight/day-level screens, ",
  "valid days remaining).",
  "",
  "## 7. Annual WUE metrics",
  "",
  "See `tables/wue_annual.csv` and `tables/wue_daily.csv.gz`.",
  "",
  units_lines,
  "",
  "## 8. Figures",
  "",
  "`figures/fig_wue_annual.png`, `figures/fig_iwue_annual.png`, `figures/fig_uwue_annual.png`, ",
  "`figures/fig_valid_days.png`.",
  "",
  "## What I could not do",
  "",
  if (length(failed_steps) == 0) "Nothing else -- all steps completed for all sites." else
    paste0("- ", failed_steps)
)
writeLines(unlist(report_lines), report_path)
message("[WUE] Report written: ", report_path)
message("[WUE] 10_report_stage2.R complete.")
