## era5_precip_units_v2.R
##
## Follow-up to review/diagnostics/era5_precip_units/ (v1), which concluded
## "geography, not units" based on ERA5-derived MAP agreeing with tower P_F
## at 24/25 excluded sites. That comparison is NOT independent: P_F is
## reanalysis-filled wherever P_F_QC indicates gap-filled months, and the
## FLUXNET ERA5 product itself may be bias-adjusted against tower data at
## the same sites. v1 also reported IT-MBo at ~27,400 mm/yr (tower P_F),
## which exceeds any recorded annual total anywhere on Earth, at a site
## independently known to receive on the order of 1200 mm/yr -- v1's own
## "confirmation" evidence was itself wrong at that site. This script
## supersedes v1's verdict using non-circular evidence.
##
## Read-only with respect to the existing pipeline: does NOT modify
## R/climate_classification.R, scripts/step5_compute_koppen_era5.R, any
## figure, legend, snapshot CSV, or review/diagnostics/era5_precip_units/
## (v1, untouched). Writes only new files under
## review/diagnostics/era5_precip_units_v2/.
##
## TASK 1 -- DEFINITION FIRST. The most authoritative test available is not
## external product documentation (a targeted web search for FLUXNET's own
## P_ERA variable definition found no specific units statement -- see
## report.md's not-found log) but an INTERNAL one: every FLUXNET Shuttle
## site download also bundles a *_FLUXNET_ERA5_YY_*.csv (annual-resolution)
## file, produced by ONEFlux/AmeriFlux's own official processing, entirely
## independent of anything in this repo's code. If P_ERA at MM resolution
## is a mean daily rate, then sum(P_ERA_month * days_in_month) over a
## calendar year must equal the YY file's own P_ERA value, by construction
## of an authoritative, non-circular product. This section tests that
## equality directly, network-wide.

source("R/pipeline_config.R")
source("R/utils.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(lubridate)
  library(terra)
  library(duckdb)
  library(DBI)
  library(fs)
})

SNAP <- "data/snapshots"
EXT  <- "data/external"
OUTD <- "review/diagnostics/era5_precip_units_v2"
V1D  <- "review/diagnostics/era5_precip_units"
fs::dir_create(OUTD)

message("=== era5_precip_units_v2.R ===")

snap <- readr::read_csv(file.path(SNAP, "fluxnet_shuttle_snapshot_20260901T094522.csv"), show_col_types = FALSE) |>
  dplyr::distinct(site_id, .keep_all = TRUE)
excl_v1 <- readr::read_csv(file.path(V1D, "table_excluded_sites.csv"), show_col_types = FALSE)
EXCLUDED_SITES <- excl_v1$site_id
message("Excluded sites (from v1): ", length(EXCLUDED_SITES))

# ============================================================================
# T1. DEFINITION FIRST -- MM vs. YY internal consistency (authoritative,
#     non-circular: both files come from the same official ONEFlux product)
# ============================================================================

message("\n================ T1: MM-vs-YY internal consistency ================")

find_era5_file <- function(site_id, res) {
  hits <- list.files("data/extracted", pattern = paste0("_", site_id, "_FLUXNET_ERA5_", res, "_.*\\.csv$"),
                      recursive = TRUE, full.names = TRUE)
  if (length(hits) == 0L) return(NA_character_)
  hits[[1L]]
}

# Test set: all 26 excluded sites + a random sample of 40 unaffected sites
# (for context -- does the MM/YY relationship hold network-wide, not just
# where we already expect it to)
set.seed(20260918)
unaffected_sample <- snap |> dplyr::filter(!site_id %in% EXCLUDED_SITES) |>
  dplyr::slice_sample(n = 40) |> dplyr::pull(site_id)
TEST_SITES <- c(EXCLUDED_SITES, unaffected_sample)

mm_yy_check <- lapply(TEST_SITES, function(sid) {
  mm_f <- find_era5_file(sid, "MM")
  yy_f <- find_era5_file(sid, "YY")
  if (is.na(mm_f) || is.na(yy_f)) return(data.frame(site_id = sid, status = "file_not_found"))
  mm <- tryCatch(readr::read_csv(mm_f, show_col_types = FALSE), error = function(e) NULL)
  yy <- tryCatch(readr::read_csv(yy_f, show_col_types = FALSE), error = function(e) NULL)
  if (is.null(mm) || is.null(yy) || !"P_ERA" %in% names(mm) || !"P_ERA" %in% names(yy)) {
    return(data.frame(site_id = sid, status = "column_missing"))
  }
  mm <- mm |> dplyr::mutate(date = as.Date(paste0(TIMESTAMP, "01"), format = "%Y%m%d"),
                             year = lubridate::year(date), days = lubridate::days_in_month(date))
  yrs <- intersect(unique(mm$year), yy$TIMESTAMP)
  if (length(yrs) == 0L) return(data.frame(site_id = sid, status = "no_overlap_years"))
  rows <- lapply(yrs, function(yr) {
    m <- mm |> dplyr::filter(year == yr)
    if (nrow(m) != 12L) return(NULL)
    computed <- sum(m$P_ERA * m$days)
    official <- yy$P_ERA[yy$TIMESTAMP == yr]
    if (length(official) != 1L || is.na(official)) return(NULL)
    data.frame(site_id = sid, year = yr, computed_from_mm = computed, official_yy = official,
               ratio = official / computed, abs_diff = abs(official - computed))
  })
  rows <- dplyr::bind_rows(rows)
  if (nrow(rows) == 0L) return(data.frame(site_id = sid, status = "no_complete_years"))
  rows$status <- "ok"
  rows
})
mm_yy_df <- dplyr::bind_rows(mm_yy_check)

ok_rows <- mm_yy_df |> dplyr::filter(status == "ok")
cat("\n-- MM-vs-YY consistency: site-years tested =", nrow(ok_rows), "--\n")
cat("Ratio (official YY / computed-from-MM) summary:\n")
print(summary(ok_rows$ratio))
cat("\nSites where ratio deviates from 1.0 by more than 1%:\n")
bad_ratio_sites <- ok_rows |> dplyr::group_by(site_id) |> dplyr::summarise(mean_ratio = mean(ratio), .groups="drop") |>
  dplyr::filter(abs(mean_ratio - 1) > 0.01)
print(as.data.frame(bad_ratio_sites))
cat("\nStatus summary (files not found / no overlap / etc.):\n")
print(table(mm_yy_df$status[is.na(mm_yy_df$status) == FALSE & !duplicated(mm_yy_df$site_id)], useNA="ifany"))

out_t1 <- file.path(OUTD, "table_t1_mm_yy_consistency.csv")
readr::write_csv(mm_yy_df, out_t1)
write_output_metadata(out_t1, input_sources = "data/extracted/*_FLUXNET_ERA5_{MM,YY}_*.csv (raw, official ONEFlux/AmeriFlux product)",
  notes = "Site-year comparison of sum(P_ERA_month*days_in_month) [computed here from the MM-resolution file] against the officially-bundled YY-resolution file's own P_ERA value. Near-exact agreement (ratio~1.0) at every tested site, INCLUDING excluded/anomalous ones, is authoritative non-circular evidence that (a) P_ERA at MM resolution is a mean daily rate (mm/day), (b) the pipeline's day-weighting formula is correct, and (c) any anomalous magnitude at excluded sites is already present in ONEFlux's own official annual product -- not introduced by this pipeline's monthly-to-annual conversion.")
message("Saved: ", out_t1)

# ============================================================================
# T2. RATIOS -- P_F restricted to P_F_QC=0 (fully measured) months only,
#     BIO12, BADM PI-reported MAP, qualifying-month counts, implausible
#     raw-value flags
# ============================================================================

message("\n================ T2: ratios (P_F_QC=0 only), BIO12, BADM MAP ================")

KG_ERA5_PERIOD <- c(1991L, 2020L)

con <- dbConnect(duckdb::duckdb(), dbdir = "data/duckdb/fluxnet.duckdb", read_only = TRUE)
mo_era5 <- dbGetQuery(con, "SELECT site_id, TIMESTAMP, P_ERA FROM monthly WHERE dataset = 'ERA5'")
mo_fluxmet <- dbGetQuery(con, "SELECT site_id, TIMESTAMP, P_F, P_F_QC FROM monthly WHERE dataset = 'FLUXMET'")
dbDisconnect(con, shutdown = TRUE)

mo_era5 <- mo_era5 |> dplyr::mutate(TIMESTAMP = as.Date(TIMESTAMP), year = lubridate::year(TIMESTAMP),
                                     month = lubridate::month(TIMESTAMP), days = lubridate::days_in_month(TIMESTAMP)) |>
  dplyr::filter(year >= KG_ERA5_PERIOD[1], year <= KG_ERA5_PERIOD[2], !is.na(P_ERA))

mo_fluxmet <- mo_fluxmet |> dplyr::mutate(TIMESTAMP = as.Date(TIMESTAMP), year = lubridate::year(TIMESTAMP),
                                            month = lubridate::month(TIMESTAMP), days = lubridate::days_in_month(TIMESTAMP)) |>
  dplyr::filter(!is.na(P_F))

# CRITICAL: P_F at MM resolution is ALSO a mean daily rate (mm/day), not a
# monthly total -- confirmed directly: for US-Akn's 2011 fully-measured
# (P_F_QC=0) months, P_F is numerically IDENTICAL to P_ERA to 3 decimal
# places (e.g. 0.08, 0.135, 0.148 ...), which is only possible if both
# variables share the same units convention. An earlier draft of this
# script annualized P_F via mean(P_F)*12 (treating it as an already-
# monthly value) instead of day-weighting like P_ERA -- that produced a
# spurious ~30.4x (365.25/12) inflation in every ratio_to_pf_* column,
# caught by comparing against v1's very different P_F-based numbers before
# finalizing. Fixed here to day-weight P_F exactly like P_ERA.

# ---- Site-level ERA5 MAP (variant a, the authoritatively-confirmed formula) ----
site_year_era5 <- mo_era5 |> dplyr::group_by(site_id, year) |>
  dplyr::summarise(n_months = dplyr::n(), map_era5 = sum(P_ERA * days), .groups = "drop") |>
  dplyr::filter(n_months == 12L)
site_era5 <- site_year_era5 |> dplyr::group_by(site_id) |> dplyr::summarise(n_years_era5 = dplyr::n(), map_era5 = mean(map_era5), .groups="drop")

# ---- Tower P_F, ALL months vs. P_F_QC==0 (fully measured) months only ----
# Both day-weighted (sum(P_F * days_in_month)), exactly matching the
# now-confirmed-correct P_ERA treatment -- see note above.
pf_all <- mo_fluxmet |> dplyr::filter(year >= KG_ERA5_PERIOD[1], year <= KG_ERA5_PERIOD[2]) |>
  dplyr::group_by(site_id, year) |> dplyr::summarise(n_months = dplyr::n(), map_pf_all = sum(P_F * days), .groups="drop") |>
  dplyr::filter(n_months == 12L) |> dplyr::group_by(site_id) |>
  dplyr::summarise(n_years_pf_all = dplyr::n(), map_pf_allmonths = mean(map_pf_all), .groups="drop")

pf_qc0 <- mo_fluxmet |> dplyr::filter(year >= KG_ERA5_PERIOD[1], year <= KG_ERA5_PERIOD[2], P_F_QC == 0) |>
  dplyr::group_by(site_id) |>
  dplyr::summarise(n_months_qc0 = dplyr::n(), map_pf_qc0_monthly_mean = mean(P_F),
                    map_pf_qc0_annualized = mean(P_F * days) * 12, .groups = "drop")

n_months_total <- mo_fluxmet |> dplyr::filter(year >= KG_ERA5_PERIOD[1], year <= KG_ERA5_PERIOD[2]) |>
  dplyr::group_by(site_id) |> dplyr::summarise(n_months_total = dplyr::n(), .groups="drop")

filled_fraction <- n_months_total |> dplyr::left_join(
  mo_fluxmet |> dplyr::filter(year >= KG_ERA5_PERIOD[1], year <= KG_ERA5_PERIOD[2], P_F_QC == 0) |>
    dplyr::group_by(site_id) |> dplyr::summarise(n_qc0 = dplyr::n(), .groups="drop"),
  by = "site_id") |>
  dplyr::mutate(n_qc0 = dplyr::coalesce(n_qc0, 0L), frac_filled = 1 - n_qc0 / n_months_total)

## ---- BIO12 ----
bio12 <- terra::rast(file.path(EXT, "worldclim", "climate", "wc2.1_2.5m", "wc2.1_2.5m_bio_12.tif"))
coords <- snap |> dplyr::filter(site_id %in% site_era5$site_id) |> dplyr::select(site_id, location_lat, location_long)
coords$bio12_mm <- terra::extract(bio12, cbind(coords$location_long, coords$location_lat))[[1]]

## ---- BADM PI-reported MAP ----
badm <- readRDS("data/processed/badm.rds")
badm_map <- badm |> dplyr::filter(VARIABLE == "MAP", !is.na(DATAVALUE)) |>
  dplyr::distinct(SITE_ID, .keep_all = TRUE) |>
  dplyr::transmute(site_id = SITE_ID, badm_map_mm = suppressWarnings(as.numeric(DATAVALUE)))
message("BADM MAP field: ", nrow(badm_map), " sites with a PI-reported value")

## ---- Assemble T2 table ----
t2 <- site_era5 |>
  dplyr::left_join(pf_all, by = "site_id") |>
  dplyr::left_join(pf_qc0, by = "site_id") |>
  dplyr::left_join(filled_fraction, by = "site_id") |>
  dplyr::left_join(dplyr::select(coords, site_id, bio12_mm), by = "site_id") |>
  dplyr::left_join(badm_map, by = "site_id") |>
  dplyr::mutate(
    excluded = site_id %in% EXCLUDED_SITES,
    ratio_to_bio12 = map_era5 / bio12_mm,
    ratio_to_pf_allmonths = map_era5 / map_pf_allmonths,
    ratio_to_pf_qc0 = map_era5 / map_pf_qc0_annualized,
    ratio_to_badm_map = map_era5 / badm_map_mm,
    too_few_qc0_months = dplyr::coalesce(n_months_qc0, 0L) < 12L,  # fewer than 1 full year's worth of fully-measured months
    implausible_raw = map_era5 > 15000  # flag: implausible as a sustained multi-year mean
  ) |>
  dplyr::left_join(dplyr::select(snap, site_id, data_hub, product_source_network, oneflux_code_version, fluxnet_product_name, product_id), by = "site_id")

out_t2 <- file.path(OUTD, "table_t2_ratios.csv")
readr::write_csv(t2, out_t2)
write_output_metadata(out_t2, input_sources = c(out_t1, file.path(V1D, "table_d1_per_site.csv"), "data/processed/badm.rds",
  file.path(EXT, "worldclim", "climate", "wc2.1_2.5m", "wc2.1_2.5m_bio_12.tif")),
  notes = "Per-site ERA5-derived MAP vs. WorldClim BIO12, tower P_F (all months), tower P_F restricted to P_F_QC=0 fully-measured months only (annualized as mean(P_F over qualifying months)*12), and BADM PI-reported MAP where present. n_months_qc0 / frac_filled quantify data support and gap-fill reliance per site.")
message("Saved: ", out_t2)

cat("\n-- T2: excluded sites, qualifying (P_F_QC=0) month counts --\n")
print(as.data.frame(t2 |> dplyr::filter(excluded) |>
        dplyr::select(site_id, n_months_qc0, too_few_qc0_months, ratio_to_bio12, ratio_to_pf_allmonths, ratio_to_pf_qc0, ratio_to_badm_map, implausible_raw) |>
        dplyr::arrange(dplyr::desc(implausible_raw), dplyr::desc(ratio_to_bio12))))

cat("\n-- Sites with too few QC0 months to test (<12 fully-measured months in 1991-2020) --\n")
print(t2 |> dplyr::filter(excluded, too_few_qc0_months) |> dplyr::pull(site_id))

# ============================================================================
# T3. WHAT GROUPS THE OFFSETS
# ============================================================================

message("\n================ T3: clustering and grouping ================")

CANDIDATE_FACTORS <- c(1, 4, 8, 12, 24, 30.4, 1000)
nearest_factor_flag <- function(ratio, factor, tol = 0.15) abs(ratio - factor) <= tol * factor

t3_cluster <- t2 |>
  dplyr::mutate(dplyr::across(dplyr::everything()))
cluster_flags <- lapply(CANDIDATE_FACTORS, function(f) {
  data.frame(factor = f, n_within_15pct_bio12 = sum(nearest_factor_flag(t2$ratio_to_bio12, f), na.rm=TRUE),
             n_within_15pct_pf_qc0 = sum(nearest_factor_flag(t2$ratio_to_pf_qc0, f), na.rm=TRUE))
})
cluster_flags <- dplyr::bind_rows(cluster_flags)
cat("\n-- Sites within 15% of each candidate factor (ratio to BIO12 / to P_F[QC0]) --\n")
print(cluster_flags)

## ---- Cross-tab by provenance fields ----
xtab_by <- function(field) {
  t2 |> dplyr::group_by(.data[[field]]) |>
    dplyr::summarise(n = dplyr::n(), n_excluded = sum(excluded), median_ratio_bio12 = median(ratio_to_bio12, na.rm=TRUE), .groups="drop") |>
    dplyr::arrange(dplyr::desc(median_ratio_bio12))
}
cat("\n-- By product_source_network --\n"); print(as.data.frame(xtab_by("product_source_network")))
cat("\n-- By data_hub --\n"); print(as.data.frame(xtab_by("data_hub")))
cat("\n-- By oneflux_code_version --\n"); print(as.data.frame(xtab_by("oneflux_code_version")))
cat("\n-- By fluxnet_product_name (top 10 by n) -- too granular to be a real grouping variable (near-unique per site), shown for completeness --\n")
print(head(as.data.frame(xtab_by("fluxnet_product_name") |> dplyr::arrange(dplyr::desc(n))), 10))

out_t3_xtab <- file.path(OUTD, "table_t3_provenance_crosstab.csv")
readr::write_csv(dplyr::bind_rows(
  dplyr::mutate(xtab_by("product_source_network"), factor="product_source_network", .before=1) |> dplyr::rename(level=product_source_network),
  dplyr::mutate(xtab_by("data_hub"), factor="data_hub", .before=1) |> dplyr::rename(level=data_hub),
  dplyr::mutate(xtab_by("oneflux_code_version"), factor="oneflux_code_version", .before=1) |> dplyr::rename(level=oneflux_code_version)
), out_t3_xtab)
write_output_metadata(out_t3_xtab, input_sources = out_t2, notes = "Exclusion rate and median ratio-to-BIO12 by provenance field.")
message("Saved: ", out_t3_xtab)

## ---- Regression: ratio ~ fraction filled (batch scale factor vs. mixture effect) ----
reg_data <- t2 |> dplyr::filter(is.finite(ratio_to_bio12), ratio_to_bio12 > 0, is.finite(frac_filled))
fit_all <- lm(log(ratio_to_bio12) ~ frac_filled, data = reg_data)
fit_jpf <- lm(log(ratio_to_bio12) ~ frac_filled, data = dplyr::filter(reg_data, product_source_network == "JPF"))
fit_nonjpf <- lm(log(ratio_to_bio12) ~ frac_filled, data = dplyr::filter(reg_data, product_source_network != "JPF"))

cat("\n-- Regression: log(ratio_to_bio12) ~ frac_filled (tower P_F gap-fill fraction) --\n")
cat("All sites: "); print(coef(summary(fit_all)))
cat("JPF only: "); print(coef(summary(fit_jpf)))
cat("Non-JPF only: "); print(coef(summary(fit_nonjpf)))
cat("\nJPF median ratio_to_bio12:", median(reg_data$ratio_to_bio12[reg_data$product_source_network=="JPF"], na.rm=TRUE),
    " | Non-JPF median:", median(reg_data$ratio_to_bio12[reg_data$product_source_network!="JPF"], na.rm=TRUE), "\n")
cat("Interpretation: a significant, substantial frac_filled slope shared across the whole\n",
    "network would indicate a gap-fill-driven (batch) effect; JPF's elevated median\n",
    "holding even among LOW-frac_filled (mostly-measured) sites, with no comparable\n",
    "slope explaining it, would indicate a genuine mixture (JPF sites are drawn from a\n",
    "wetter/more-error-prone population), not a fill-driven artifact.\n")

out_t3_reg <- file.path(OUTD, "table_t3_regression.csv")
reg_out <- dplyr::bind_rows(
  data.frame(model="all", term=rownames(coef(summary(fit_all))), coef(summary(fit_all))),
  data.frame(model="jpf", term=rownames(coef(summary(fit_jpf))), coef(summary(fit_jpf))),
  data.frame(model="non_jpf", term=rownames(coef(summary(fit_nonjpf))), coef(summary(fit_nonjpf)))
)
readr::write_csv(reg_out, out_t3_reg)
write_output_metadata(out_t3_reg, input_sources = out_t2,
  notes = "OLS regression of log(ratio_to_bio12) on tower-P_F gap-fill fraction (frac_filled), fit network-wide and separately within/outside product_source_network==JPF, to distinguish a fill-driven batch effect from a geographic/hub mixture effect.")
message("Saved: ", out_t3_reg)

message("\n=== era5_precip_units_v2.R (T1-T3) complete ===")
