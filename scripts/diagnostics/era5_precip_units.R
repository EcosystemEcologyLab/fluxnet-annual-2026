## era5_precip_units.R
##
## Diagnostic investigation (D1-D3) for a co-author decision: the KG
## classification's ERA5 MAP screen (R/climate_classification.R, the
## KG_ERA5_MAP_MAX_MM=5000 rule) excludes 26 current-network sites because
## every candidate year's ERA5-derived MAP exceeds 5000 mm/yr, even at
## sites (IT-MBo, NO-And, JP-Tak, US-HB4, BR-Ji3, PE-QFR, ...) that do not
## plausibly receive that much rain. This script tests whether P_ERA
## carries an inconsistent unit convention across sites, and whether the
## affected sites are geographically genuine outliers vs. a units/hub
## artifact.
##
## Read-only with respect to the existing pipeline: does NOT modify
## R/climate_classification.R, scripts/step5_compute_koppen_era5.R, any
## figure, legend, or snapshot CSV. Reads only existing files under
## data/snapshots/, data/external/, data/extracted/, and data/duckdb/
## (read_only=TRUE connection). Writes only new files under
## review/diagnostics/era5_precip_units/.
##
## NOTE ON THE "25 excluded" FIGURE IN THE TASK: the 2026-08-20 session log
## entry (at the then-current 767-site network) reported 25 sites excluded
## by this screen. This script operates on the CURRENT 781-site network,
## where the actual current count (re-derived directly from
## data/snapshots/site_koppen_era5.csv, not assumed) is 26 -- one more
## than the task's stated "25". Both figures are reported; the current
## 26-site list is treated as authoritative for D3/D4 since it reflects
## the code's present behavior, per the task's own "treat code as
## authoritative" instruction.

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
OUTD <- "review/diagnostics/era5_precip_units"
fs::dir_create(OUTD)

message("=== era5_precip_units.R ===")

KG_ERA5_PERIOD     <- c(1991L, 2020L)  # R/pipeline_config.R:56
KG_ERA5_MAP_MAX_MM <- 5000             # R/pipeline_config.R:62

snap <- readr::read_csv(file.path(SNAP, "fluxnet_shuttle_snapshot_20260901T094522.csv"), show_col_types = FALSE) |>
  dplyr::distinct(site_id, .keep_all = TRUE)
era5_kg <- readr::read_csv(file.path(SNAP, "site_koppen_era5.csv"), show_col_types = FALSE)

EXCLUDED_SITES <- era5_kg$site_id[is.na(era5_kg$koppen_class)]
message("Current excluded-by-MAP-screen sites (re-derived from site_koppen_era5.csv): ", length(EXCLUDED_SITES),
        " (task's own framing cites 25, from the 2026-08-20 767-site-network log entry -- see script header note)")
stopifnot(all(c("IT-MBo","NO-And","JP-Tak","US-HB4","BR-Ji3","PE-QFR") %in% EXCLUDED_SITES))

out_excl <- file.path(OUTD, "table_excluded_sites.csv")
readr::write_csv(data.frame(site_id = EXCLUDED_SITES), out_excl)
write_output_metadata(out_excl, input_sources = file.path(SNAP, "site_koppen_era5.csv"),
  notes = "Current (781-site network) list of sites excluded by the KG_ERA5_MAP_MAX_MM=5000 screen, re-derived directly from site_koppen_era5.csv (koppen_class is NA). 26 sites, vs. the 25 cited in the 2026-08-20 log entry for the then-current 767-site network.")

# ============================================================================
# D1. REFERENCE COMPARISON
# ============================================================================

message("\n================ D1: reference comparison ================")

## ---- ERA5 monthly P_ERA, all current-network sites, from DuckDB ----------
message("Querying DuckDB monthly table (dataset='ERA5', read-only)...")
con <- dbConnect(duckdb::duckdb(), dbdir = "data/duckdb/fluxnet.duckdb", read_only = TRUE)
mo <- dbGetQuery(con, "SELECT site_id, TIMESTAMP, P_ERA FROM monthly WHERE dataset = 'ERA5'")
## ---- Tower-observed annual P_F, from DuckDB annual/FLUXMET rows ----------
pf_annual <- dbGetQuery(con, "SELECT site_id, TIMESTAMP AS year, P_F, P_F_QC FROM annual WHERE dataset = 'FLUXMET' AND P_F IS NOT NULL")
dbDisconnect(con, shutdown = TRUE)
message("Monthly ERA5 rows: ", nrow(mo), " (", length(unique(mo$site_id)), " sites) | ",
        "Annual tower P_F rows (non-NA): ", nrow(pf_annual), " (", length(unique(pf_annual$site_id)), " sites)")

mo <- mo |>
  dplyr::mutate(TIMESTAMP = as.Date(TIMESTAMP), year = lubridate::year(TIMESTAMP),
                days = lubridate::days_in_month(TIMESTAMP)) |>
  dplyr::filter(year >= KG_ERA5_PERIOD[1], year <= KG_ERA5_PERIOD[2], !is.na(P_ERA))

## ---- Three MAP variants, per site-year (complete 12-month years only) ----
site_year <- mo |>
  dplyr::group_by(site_id, year) |>
  dplyr::summarise(
    n_months = dplyr::n(),
    map_a = sum(P_ERA * days),        # (a) pipeline method: sum(P_ERA * days_in_month)
    map_b = sum(P_ERA),               # (b) P_ERA as already a monthly total, no day weighting
    map_c = mean(P_ERA) * 365.25,     # (c) mean(P_ERA) * 365.25
    .groups = "drop"
  ) |>
  dplyr::filter(n_months == 12L)

out_siteyear <- file.path(OUTD, "table_d1_site_year_variants.csv")
readr::write_csv(site_year, out_siteyear)
write_output_metadata(out_siteyear, input_sources = "data/duckdb/fluxnet.duckdb (monthly, read-only)",
  notes = "Per-site-year MAP under 3 variants: (a) pipeline method sum(P_ERA*days_in_month), (b) sum(P_ERA) no day weighting, (c) mean(P_ERA)*365.25. Complete 12-month years only, 1991-2020 (KG_ERA5_PERIOD).")
message("Saved: ", out_siteyear, " (", nrow(site_year), " site-years, ", length(unique(site_year$site_id)), " sites)")

## ---- Site-level summary (mean across available 1991-2020 years) ----------
site_summary <- site_year |>
  dplyr::group_by(site_id) |>
  dplyr::summarise(n_years = dplyr::n(), map_a = mean(map_a), map_b = mean(map_b), map_c = mean(map_c), .groups = "drop")

## ---- WorldClim BIO12 at each site ----------------------------------------
message("Extracting WorldClim BIO12...")
bio12 <- terra::rast(file.path(EXT, "worldclim", "climate", "wc2.1_2.5m", "wc2.1_2.5m_bio_12.tif"))
coords <- snap |> dplyr::filter(site_id %in% site_summary$site_id) |> dplyr::select(site_id, location_lat, location_long)
coords$bio12_mm <- terra::extract(bio12, cbind(coords$location_long, coords$location_lat))[[1]]
site_summary <- site_summary |> dplyr::left_join(dplyr::select(coords, site_id, bio12_mm), by = "site_id")

## ---- Tower-observed P_F, site-level mean over available years within window
pf_summary <- pf_annual |>
  dplyr::filter(year >= KG_ERA5_PERIOD[1], year <= KG_ERA5_PERIOD[2]) |>
  dplyr::group_by(site_id) |>
  dplyr::summarise(n_years_pf = dplyr::n(), pf_obs_mm = mean(P_F), .groups = "drop")
site_summary <- site_summary |> dplyr::left_join(pf_summary, by = "site_id")

## ---- Ratios ----------------------------------------------------------------
site_summary <- site_summary |>
  dplyr::mutate(
    ratio_a_bio12 = map_a / bio12_mm, ratio_b_bio12 = map_b / bio12_mm, ratio_c_bio12 = map_c / bio12_mm,
    ratio_a_pf = map_a / pf_obs_mm, ratio_b_pf = map_b / pf_obs_mm, ratio_c_pf = map_c / pf_obs_mm,
    excluded = site_id %in% EXCLUDED_SITES
  ) |>
  dplyr::left_join(dplyr::select(snap, site_id, data_hub, product_source_network, oneflux_code_version, fluxnet_product_name), by = "site_id")

out_d1 <- file.path(OUTD, "table_d1_per_site.csv")
readr::write_csv(site_summary, out_d1)
write_output_metadata(out_d1, input_sources = c(out_siteyear,
  file.path(EXT, "worldclim", "climate", "wc2.1_2.5m", "wc2.1_2.5m_bio_12.tif"), "data/duckdb/fluxnet.duckdb"),
  notes = "Per-site D1 summary: mean MAP (variants a/b/c) over available complete 1991-2020 years, WorldClim BIO12, tower-observed P_F where available, and ratios of each variant to BIO12 and to P_F.")
message("Saved: ", out_d1, " (", nrow(site_summary), " sites)")

cat("\n-- D1 summary: excluded (n=", sum(site_summary$excluded), ") vs. unaffected (n=", sum(!site_summary$excluded), ") --\n")
print(site_summary |> dplyr::group_by(excluded) |>
        dplyr::summarise(median_ratio_a_bio12 = median(ratio_a_bio12, na.rm=TRUE),
                          median_ratio_b_bio12 = median(ratio_b_bio12, na.rm=TRUE),
                          median_ratio_c_bio12 = median(ratio_c_bio12, na.rm=TRUE),
                          n_with_pf = sum(!is.na(pf_obs_mm))))

# ============================================================================
# D2. UNIT HYPOTHESIS
# ============================================================================

message("\n================ D2: unit hypothesis ================")

classify_cluster <- function(ratio) {
  dplyr::case_when(
    is.na(ratio) ~ NA_character_,
    ratio >= 0.8 & ratio <= 1.25 ~ "near_1",
    ratio >= 9.6 & ratio <= 14.4 ~ "near_12",
    ratio >= 24 & ratio <= 36.5 ~ "near_30.4",
    TRUE ~ "elsewhere"
  )
}

d2_clusters <- site_summary |>
  dplyr::mutate(cluster_a = classify_cluster(ratio_a_bio12),
                cluster_b = classify_cluster(ratio_b_bio12),
                cluster_c = classify_cluster(ratio_c_bio12))

cat("\n-- Variant (a) [pipeline method] ratio-to-BIO12 clustering, ALL sites --\n")
print(table(d2_clusters$cluster_a, useNA = "ifany"))
cat("\n-- Variant (b) [no day-weighting] ratio-to-BIO12 clustering, ALL sites --\n")
print(table(d2_clusters$cluster_b, useNA = "ifany"))
cat("\n-- Variant (c) [mean*365.25] ratio-to-BIO12 clustering, ALL sites --\n")
print(table(d2_clusters$cluster_c, useNA = "ifany"))

cat("\n-- Same, restricted to the 26 EXCLUDED sites only --\n")
excl_only <- d2_clusters |> dplyr::filter(excluded)
cat("Variant (a):\n"); print(table(excl_only$cluster_a, useNA = "ifany"))
cat("Variant (b):\n"); print(table(excl_only$cluster_b, useNA = "ifany"))
cat("Variant (c):\n"); print(table(excl_only$cluster_c, useNA = "ifany"))

cat("\n-- Does (b) or (c) bring excluded sites' map_* below 5000 and near BIO12 (ratio 0.8-1.25)? --\n")
recon <- excl_only |> dplyr::summarise(
  n = dplyr::n(),
  n_a_over_5000 = sum(map_a > KG_ERA5_MAP_MAX_MM, na.rm=TRUE),
  n_b_over_5000 = sum(map_b > KG_ERA5_MAP_MAX_MM, na.rm=TRUE),
  n_c_over_5000 = sum(map_c > KG_ERA5_MAP_MAX_MM, na.rm=TRUE),
  n_b_near_bio12 = sum(cluster_b == "near_1", na.rm=TRUE),
  n_c_near_bio12 = sum(cluster_c == "near_1", na.rm=TRUE)
)
print(recon)

out_d2 <- file.path(OUTD, "table_d2_clustering.csv")
readr::write_csv(d2_clusters, out_d2)
write_output_metadata(out_d2, input_sources = out_d1,
  notes = "Per-site cluster assignment (near_1/near_12/near_30.4/elsewhere) of the ratio of each MAP variant to WorldClim BIO12, for all sites and for the 26 excluded sites specifically.")
message("Saved: ", out_d2)

# ============================================================================
# D3. PROVENANCE OF THE OFFSET
# ============================================================================

message("\n================ D3: provenance of the offset ================")

## ---- Cross-tabulation: excluded vs. provenance fields, network-wide denominators
provenance_xtab <- snap |>
  dplyr::filter(site_id %in% site_summary$site_id) |>
  dplyr::mutate(excluded = site_id %in% EXCLUDED_SITES) |>
  dplyr::group_by(product_source_network) |>
  dplyr::summarise(n_total = dplyr::n(), n_excluded = sum(excluded),
                    pct_excluded = round(100 * n_excluded / n_total, 1), .groups = "drop") |>
  dplyr::arrange(dplyr::desc(pct_excluded))

cat("\n-- Exclusion rate by product_source_network (all current-network sites with ERA5 rows) --\n")
print(as.data.frame(provenance_xtab))

data_hub_xtab <- snap |> dplyr::filter(site_id %in% site_summary$site_id) |>
  dplyr::mutate(excluded = site_id %in% EXCLUDED_SITES) |>
  dplyr::group_by(data_hub) |>
  dplyr::summarise(n_total = dplyr::n(), n_excluded = sum(excluded), pct_excluded = round(100*n_excluded/n_total,1), .groups="drop") |>
  dplyr::arrange(dplyr::desc(pct_excluded))
cat("\n-- Exclusion rate by data_hub --\n")
print(as.data.frame(data_hub_xtab))

oneflux_xtab <- snap |> dplyr::filter(site_id %in% site_summary$site_id) |>
  dplyr::mutate(excluded = site_id %in% EXCLUDED_SITES) |>
  dplyr::group_by(oneflux_code_version) |>
  dplyr::summarise(n_total = dplyr::n(), n_excluded = sum(excluded), pct_excluded = round(100*n_excluded/n_total,1), .groups="drop")
cat("\n-- Exclusion rate by oneflux_code_version --\n")
print(as.data.frame(oneflux_xtab))

out_d3_xtab <- file.path(OUTD, "table_d3_provenance_crosstab.csv")
readr::write_csv(dplyr::bind_rows(
  dplyr::mutate(provenance_xtab, factor = "product_source_network", .before=1) |> dplyr::rename(level = product_source_network),
  dplyr::mutate(data_hub_xtab, factor = "data_hub", .before=1) |> dplyr::rename(level = data_hub),
  dplyr::mutate(oneflux_xtab, factor = "oneflux_code_version", .before=1) |> dplyr::rename(level = oneflux_code_version)
), out_d3_xtab)
write_output_metadata(out_d3_xtab, input_sources = c(file.path(SNAP, "fluxnet_shuttle_snapshot_20260901T094522.csv"), out_excl),
  notes = "Cross-tabulation of MAP-screen exclusion rate by data_hub / product_source_network / oneflux_code_version, network-wide denominators (not just the 26 excluded sites) so disproportionate concentration is visible.")
message("Saved: ", out_d3_xtab)

## ---- Raw *_FLUXNET_ERA5_MM_*.csv header/metadata check, excluded sites ---
message("\nReading raw ERA5 MM CSV headers for the 26 excluded sites...")
find_era5_mm_file <- function(site_id) {
  hits <- list.files("data/extracted", pattern = paste0("_", site_id, "_FLUXNET_ERA5_MM_.*\\.csv$"),
                      recursive = TRUE, full.names = TRUE)
  if (length(hits) == 0L) return(NA_character_)
  hits[[1L]]
}
raw_file_check <- lapply(EXCLUDED_SITES, function(sid) {
  f <- find_era5_mm_file(sid)
  if (is.na(f)) return(data.frame(site_id = sid, file_found = FALSE, header = NA_character_, has_units_row = NA))
  hdr <- readLines(f, n = 2)
  data.frame(site_id = sid, file_found = TRUE, header = hdr[1],
             has_units_row = grepl("^[A-Za-z_]+,[A-Za-z_]+", hdr[2]) == FALSE && !grepl("^[0-9]", hdr[1]))
})
raw_file_check <- dplyr::bind_rows(raw_file_check)
cat("\n-- Raw MM file check: all headers identical (no per-file units metadata found)? --\n")
cat("Unique header strings across the 26 files: ", length(unique(raw_file_check$header[raw_file_check$file_found])), "\n")
print(unique(raw_file_check$header[raw_file_check$file_found]))
cat("Files found: ", sum(raw_file_check$file_found), " / ", nrow(raw_file_check), "\n")

out_d3_files <- file.path(OUTD, "table_d3_raw_file_check.csv")
readr::write_csv(raw_file_check, out_d3_files)
write_output_metadata(out_d3_files, input_sources = "data/extracted/ (raw *_FLUXNET_ERA5_MM_*.csv files)",
  notes = "Per-excluded-site raw ERA5 MM CSV header check. No units-metadata row was found in any file (plain column-name header only, confirmed by direct inspection) -- 'not found' logged explicitly per task instruction, not assumed.")
message("Saved: ", out_d3_files)

## ---- Raw monthly P_ERA distribution: affected vs. unaffected -------------
mo_dist <- mo |> dplyr::mutate(excluded = site_id %in% EXCLUDED_SITES)
out_d3_dist <- file.path(OUTD, "table_d3_raw_p_era_distribution.csv")
dist_summary <- mo_dist |> dplyr::group_by(excluded) |>
  dplyr::summarise(n = dplyr::n(), p5=quantile(P_ERA,.05), p25=quantile(P_ERA,.25), p50=quantile(P_ERA,.5),
                    p75=quantile(P_ERA,.75), p95=quantile(P_ERA,.95), max=max(P_ERA), .groups="drop")
cat("\n-- Raw monthly P_ERA (mm/day as treated by the pipeline) distribution, affected vs. unaffected --\n")
print(as.data.frame(dist_summary))
readr::write_csv(mo_dist, out_d3_dist)
write_output_metadata(out_d3_dist, input_sources = "data/duckdb/fluxnet.duckdb (monthly, read-only)",
  notes = "Raw monthly P_ERA values (dataset='ERA5'), tagged by whether the site is in the 26-site excluded set, for bimodality inspection.")
message("Saved: ", out_d3_dist)

message("\n=== era5_precip_units.R (D1-D3) complete ===")
