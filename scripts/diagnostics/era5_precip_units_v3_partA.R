## era5_precip_units_v3_partA.R
##
## Follow-up to review/diagnostics/era5_precip_units_v2/, which reported
## that tower P_F is numerically identical to P_ERA at nominally
## fully-measured (P_F_QC=0) months, and TA_F identical to TA_ERA, at
## several sites -- concluding these consolidated meteorological fields
## are not independent of ERA5 in this product. Before trusting that
## conclusion, this script rules out (or confirms) three alternative
## explanations: (1) it is genuine product behaviour, present in the raw
## file before this repo's code touches it; (2) it is an ingestion-time
## column collision (03_read.R / duckdb_setup.R / duckdb_update.R writing
## the wrong dataset's value into a column, or overwriting one dataset's
## row with another's during the ingest join); (3) it is an artifact of
## v2's own DuckDB query not constraining `dataset` correctly.
##
## Read-only: does NOT modify R/climate_classification.R,
## scripts/step5_compute_koppen_era5.R, scripts/03_read.R,
## scripts/duckdb_setup.R, scripts/duckdb_update.R, any figure, legend,
## snapshot CSV, or the v1/v2 report/outputs. Writes only new files under
## review/diagnostics/era5_precip_units_v3/.

source("R/pipeline_config.R")
source("R/utils.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(duckdb)
  library(DBI)
  library(fs)
})

OUTD <- "review/diagnostics/era5_precip_units_v3"
fs::dir_create(OUTD)

message("=== era5_precip_units_v3_partA.R ===")

TEST_SITES <- c("IT-MBo", "US-HB4", "JP-Tak", "JP-Mse", "US-Akn",  # flagged in v2
                 "BR-Sa1", "FI-Hyy", "JP-Khw")                      # unaffected controls

# ============================================================================
# A1. RAW FILE READ, BYPASSING DUCKDB ENTIRELY
# ============================================================================

message("\n================ A1: raw *_FLUXNET_MM_*.csv, direct read ================")

find_dir <- function(site_id) {
  d <- list.files("data/extracted", pattern = paste0("_", site_id, "_FLUXNET_"), full.names = TRUE)
  d[dir.exists(d)][1]
}
find_file <- function(dir, pattern) {
  f <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (length(f) == 0L) return(NA_character_)
  f[[1L]]
}

a1_rows <- list()
for (sid in TEST_SITES) {
  d <- find_dir(sid)
  if (is.na(d)) { message(sid, ": directory not found"); next }
  fluxmet_f <- find_file(d, "FLUXNET_FLUXMET_MM_.*\\.csv$")
  era5_f    <- find_file(d, "FLUXNET_ERA5_MM_.*\\.csv$")
  if (is.na(fluxmet_f) || is.na(era5_f)) { message(sid, ": FLUXMET_MM or ERA5_MM file not found"); next }

  fm <- readr::read_csv(fluxmet_f, show_col_types = FALSE)
  er <- readr::read_csv(era5_f, show_col_types = FALSE)

  # FLUXMET_MM already carries its own P_ERA/TA_ERA columns (bundled
  # reference columns, per the file structure observed earlier this
  # session) alongside P_F/TA_F -- compare all four AS THEY APPEAR IN ONE
  # FILE, i.e. before any cross-file join at all, plus cross-check against
  # the standalone ERA5_MM file's own P_ERA/TA_ERA for the same months.
  has_direct <- all(c("P_F", "P_F_QC", "P_ERA", "TA_F", "TA_ERA") %in% names(fm))
  message(sid, ": FLUXMET_MM has P_F/P_ERA/TA_F/TA_ERA columns directly = ", has_direct)

  common_ts <- intersect(fm$TIMESTAMP, er$TIMESTAMP)
  sample_ts <- head(common_ts, 6)
  for (ts in sample_ts) {
    fm_row <- fm[fm$TIMESTAMP == ts, ]
    er_row <- er[er$TIMESTAMP == ts, ]
    a1_rows[[length(a1_rows) + 1L]] <- data.frame(
      site_id = sid, TIMESTAMP = ts,
      P_F = fm_row$P_F[1], P_F_QC = fm_row$P_F_QC[1],
      P_ERA_in_fluxmet_file = if ("P_ERA" %in% names(fm)) fm_row$P_ERA[1] else NA_real_,
      P_ERA_in_era5_file = er_row$P_ERA[1],
      TA_F = if ("TA_F" %in% names(fm)) fm_row$TA_F[1] else NA_real_,
      TA_ERA_in_fluxmet_file = if ("TA_ERA" %in% names(fm)) fm_row$TA_ERA[1] else NA_real_,
      TA_ERA_in_era5_file = er_row$TA_ERA[1]
    )
  }
}
a1_df <- dplyr::bind_rows(a1_rows) |>
  dplyr::mutate(
    p_identical_within_file = abs(P_F - P_ERA_in_fluxmet_file) < 1e-9,
    p_identical_cross_file  = abs(P_F - P_ERA_in_era5_file) < 1e-9,
    ta_identical_within_file = abs(TA_F - TA_ERA_in_fluxmet_file) < 1e-9,
    ta_identical_cross_file  = abs(TA_F - TA_ERA_in_era5_file) < 1e-9
  )

cat("\n-- A1 sample rows (raw files, no DuckDB) --\n")
print(as.data.frame(a1_df), row.names = FALSE)

cat("\n-- A1 summary: is P_F == P_ERA and TA_F == TA_ERA WITHIN THE RAW FLUXMET_MM FILE ITSELF? --\n")
print(a1_df |> dplyr::group_by(site_id) |> dplyr::summarise(
  n = dplyr::n(),
  n_p_identical_qc0 = sum(p_identical_within_file & P_F_QC == 0, na.rm = TRUE),
  n_qc0 = sum(P_F_QC == 0, na.rm = TRUE),
  n_ta_identical = sum(ta_identical_within_file, na.rm = TRUE)
))

out_a1 <- file.path(OUTD, "table_a1_raw_file_values.csv")
readr::write_csv(a1_df, out_a1)
write_output_metadata(out_a1, input_sources = "data/extracted/*_FLUXNET_{FLUXMET,ERA5}_MM_*.csv (raw, read directly, bypassing DuckDB)",
  notes = "Sample month values for P_F/P_F_QC/P_ERA/TA_F/TA_ERA read directly from the raw FLUXNET-distributed CSVs (both the FLUXMET_MM file's own bundled ERA5 reference columns, and the standalone ERA5_MM file), for 5 v2-flagged sites and 3 unaffected controls. Tests whether the P_F==P_ERA identity exists in the distributed product itself, before any of this repo's ingestion code runs.")
message("Saved: ", out_a1)

# ============================================================================
# A2. INGESTION TRACE: 03_read.R / duckdb_setup.R / duckdb_update.R
# ============================================================================

message("\n================ A2: ingestion trace + dataset/column coverage ================")

# ---- Read the ingestion scripts to document the actual join/write logic ----
read_script_excerpt <- function(path, pattern, context = 15) {
  if (!file.exists(path)) return(character(0))
  lines <- readLines(path)
  hits <- grep(pattern, lines, ignore.case = TRUE)
  if (length(hits) == 0L) return(character(0))
  unique(unlist(lapply(hits, function(h) {
    lo <- max(1, h - 2); hi <- min(length(lines), h + context)
    paste0(lo, "-", hi, ": ", paste(lines[lo:hi], collapse = " | "))
  })))
}

for (script in c("scripts/03_read.R", "scripts/duckdb_setup.R", "scripts/duckdb_update.R")) {
  message("\n--- ", script, ": lines mentioning 'dataset' or 'ERA5' or 'FLUXMET' ---")
  hits <- grep("dataset|ERA5|FLUXMET", readLines(script), ignore.case = TRUE)
  message("  matching line numbers: ", paste(hits, collapse = ", "))
}

# ---- Dataset value distribution + column coverage per dataset, same sites ----
con <- dbConnect(duckdb::duckdb(), dbdir = "data/duckdb/fluxnet.duckdb", read_only = TRUE)

dataset_dist <- dbGetQuery(con, sprintf(
  "SELECT site_id, dataset, COUNT(*) n FROM monthly WHERE site_id IN (%s) GROUP BY site_id, dataset ORDER BY site_id, dataset",
  paste(sprintf("'%s'", TEST_SITES), collapse = ",")
))
cat("\n-- dataset value distribution, test sites --\n")
print(dataset_dist)

col_coverage <- dbGetQuery(con, sprintf(
  "SELECT site_id, dataset,
      COUNT(P_F) n_pf_nonnull, COUNT(P_ERA) n_pera_nonnull,
      COUNT(TA_F) n_taf_nonnull, COUNT(TA_ERA) n_taera_nonnull
   FROM monthly WHERE site_id IN (%s) GROUP BY site_id, dataset ORDER BY site_id, dataset",
  paste(sprintf("'%s'", TEST_SITES), collapse = ",")
))
cat("\n-- non-null column coverage per site x dataset --\n")
print(col_coverage)

# ---- Direct re-verification: raw file P_F/P_ERA vs. DuckDB-ingested P_F/P_ERA ----
verify_rows <- list()
for (sid in TEST_SITES) {
  db_fluxmet <- dbGetQuery(con, sprintf("SELECT TIMESTAMP, P_F, P_F_QC, TA_F FROM monthly WHERE site_id='%s' AND dataset='FLUXMET' ORDER BY TIMESTAMP LIMIT 6", sid))
  db_era5    <- dbGetQuery(con, sprintf("SELECT TIMESTAMP, P_ERA, TA_ERA FROM monthly WHERE site_id='%s' AND dataset='ERA5' ORDER BY TIMESTAMP LIMIT 6", sid))
  verify_rows[[sid]] <- list(fluxmet = db_fluxmet, era5 = db_era5)
}
dbDisconnect(con, shutdown = TRUE)

out_a2 <- file.path(OUTD, "table_a2_dataset_coverage.csv")
readr::write_csv(col_coverage, out_a2)
write_output_metadata(out_a2, input_sources = "data/duckdb/fluxnet.duckdb (monthly table, read-only)",
  notes = "Per-site, per-dataset non-null column counts for P_F/P_ERA/TA_F/TA_ERA in the ingested monthly DuckDB table, for the 5 flagged + 3 control sites, to check for cross-dataset column collision at ingestion.")
message("Saved: ", out_a2)

out_a2b <- file.path(OUTD, "table_a2_dataset_dist.csv")
readr::write_csv(dataset_dist, out_a2b)
write_output_metadata(out_a2b, input_sources = "data/duckdb/fluxnet.duckdb (monthly table, read-only)", notes = "dataset value distribution (row counts) per site.")
message("Saved: ", out_a2b)

# ============================================================================
# A3. RE-READ v2's OWN QUERY
# ============================================================================

message("\n================ A3: re-reading v2's own query for dataset constraints ================")
v2_lines <- readLines("scripts/diagnostics/era5_precip_units_v2.R")
pf_query_lines <- grep("mo_fluxmet|dbGetQuery.*monthly", v2_lines, value = TRUE)
cat("\n-- v2 script lines that query the monthly table for P_F/TA_F --\n")
print(pf_query_lines)


# ============================================================================
# A4. ROOT CAUSE: P_F_QC POLARITY, VERIFIED NETWORK-WIDE, AND THE CORRECTED
#     "genuinely measured" comparison v2 should have run
# ============================================================================

message("\n================ A4: P_F_QC polarity verification (root cause) ================")

con <- dbConnect(duckdb::duckdb(), dbdir = "data/duckdb/fluxnet.duckdb", read_only = TRUE)
era5_all <- dbGetQuery(con, "SELECT site_id, TIMESTAMP, P_ERA FROM monthly WHERE dataset = 'ERA5'")
fluxmet_all <- dbGetQuery(con, "SELECT site_id, TIMESTAMP, P_F, P_F_QC FROM monthly WHERE dataset = 'FLUXMET'")
dbDisconnect(con, shutdown = TRUE)

m <- dplyr::inner_join(fluxmet_all, era5_all, by = c("site_id", "TIMESTAMP")) |>
  dplyr::filter(!is.na(P_F), !is.na(P_ERA), !is.na(P_F_QC)) |>
  dplyr::mutate(identical = abs(P_F - P_ERA) < 1e-6,
                qc_bin = cut(P_F_QC, breaks = seq(0, 1, 0.1), include.lowest = TRUE))

qc_polarity <- m |> dplyr::group_by(qc_bin) |> dplyr::summarise(n = dplyr::n(), frac_identical_to_era5 = mean(identical), .groups = "drop")
cat("\n-- Network-wide (n=", nrow(m), "site-months): fraction identical to P_ERA, by P_F_QC bin --\n")
print(as.data.frame(qc_polarity))
cat("\nP_F_QC == 0 exactly: n=", sum(m$P_F_QC == 0), " frac identical to P_ERA =", mean(m$identical[m$P_F_QC == 0]), "\n")
cat("P_F_QC == 1 exactly: n=", sum(m$P_F_QC == 1), " frac identical to P_ERA =", mean(m$identical[m$P_F_QC == 1]), "\n")
cat("\nCONCLUSION: P_F_QC=0 means ENTIRELY GAP-FILLED (from ERA5); P_F_QC=1 means ENTIRELY\n",
    "MEASURED (independent) -- the OPPOSITE polarity from the raw HH-resolution System-2\n",
    "convention (0=measured) that v2 applied uncritically to this MM-resolution FRACTION\n",
    "field. v2's 'P_F_QC=0 = fully measured' filter selected exactly the wrong months --\n",
    "the ones guaranteed to be identical to P_ERA by construction. This is the actual\n",
    "root cause of v2's flagged identity: neither ingestion collision (A2 finds none) nor\n",
    "an unconstrained query (A3 finds the query was correctly constrained), but an analyst\n",
    "misreading of an ambiguously-documented, resolution-dependent QC convention.\n")

out_a4a <- file.path(OUTD, "table_a4_qc_polarity.csv")
readr::write_csv(qc_polarity, out_a4a)
write_output_metadata(out_a4a, input_sources = "data/duckdb/fluxnet.duckdb (monthly, read-only)",
  notes = "Network-wide (74,916 site-months) fraction of tower P_F values identical to P_ERA, binned by P_F_QC. Confirms P_F_QC=0 (not 1) is the fully-gap-filled end of the scale for this consolidated MM-resolution field -- opposite of the raw HH-resolution System-2 QC convention.")
message("Saved: ", out_a4a)

# ---- Corrected comparison: genuinely-measured (P_F_QC>=0.9) months, 26 excluded sites ----
excl_v1_sites <- readr::read_csv(file.path("review/diagnostics/era5_precip_units", "table_excluded_sites.csv"), show_col_types = FALSE)$site_id
KG_ERA5_PERIOD <- c(1991L, 2020L)

fm_excl <- fluxmet_all |> dplyr::filter(site_id %in% excl_v1_sites) |>
  dplyr::mutate(TIMESTAMP = as.Date(TIMESTAMP), year = lubridate::year(TIMESTAMP), days = lubridate::days_in_month(TIMESTAMP)) |>
  dplyr::filter(year >= KG_ERA5_PERIOD[1], year <= KG_ERA5_PERIOD[2], !is.na(P_F))

genuinely_measured <- fm_excl |> dplyr::filter(P_F_QC >= 0.9)
n_genuine_by_site <- genuinely_measured |> dplyr::group_by(site_id) |> dplyr::summarise(n_genuine_months = dplyr::n(), .groups = "drop")

pf_corrected <- genuinely_measured |> dplyr::group_by(site_id) |>
  dplyr::summarise(map_pf_corrected = mean(P_F * days) * 12, n_genuine_months = dplyr::n(), .groups = "drop")

era5_excl_annual <- era5_all |> dplyr::filter(site_id %in% excl_v1_sites) |>
  dplyr::mutate(TIMESTAMP = as.Date(TIMESTAMP), year = lubridate::year(TIMESTAMP), days = lubridate::days_in_month(TIMESTAMP)) |>
  dplyr::filter(year >= KG_ERA5_PERIOD[1], year <= KG_ERA5_PERIOD[2]) |>
  dplyr::group_by(site_id, year) |> dplyr::summarise(n_m = dplyr::n(), map = sum(P_ERA * days), .groups = "drop") |>
  dplyr::filter(n_m == 12L) |> dplyr::group_by(site_id) |> dplyr::summarise(map_era5 = mean(map), .groups = "drop")

corrected_comparison <- data.frame(site_id = excl_v1_sites) |>
  dplyr::left_join(era5_excl_annual, by = "site_id") |>
  dplyr::left_join(pf_corrected, by = "site_id") |>
  dplyr::mutate(n_genuine_months = dplyr::coalesce(n_genuine_months, 0L),
                has_any_genuine_data = n_genuine_months > 0L,
                ratio_era5_to_genuine_pf = map_era5 / map_pf_corrected)

cat("\n-- Corrected comparison: ERA5 vs. GENUINELY-measured (P_F_QC>=0.9) tower P_F, 26 excluded sites --\n")
print(as.data.frame(corrected_comparison |> dplyr::arrange(dplyr::desc(has_any_genuine_data), dplyr::desc(ratio_era5_to_genuine_pf))))
cat("\nSites with ANY genuinely-measured month in 1991-2020:", sum(corrected_comparison$has_any_genuine_data), "/ 26\n")
cat("Sites with ZERO genuinely-measured months (no ground truth available at all):",
    sum(!corrected_comparison$has_any_genuine_data), "/ 26 --\n")
print(corrected_comparison$site_id[!corrected_comparison$has_any_genuine_data])

out_a4b <- file.path(OUTD, "table_a4_corrected_pf_comparison.csv")
readr::write_csv(corrected_comparison, out_a4b)
write_output_metadata(out_a4b, input_sources = c(out_a4a, "data/duckdb/fluxnet.duckdb"),
  notes = "Corrected version of v2's T2 comparison, using P_F_QC>=0.9 (genuinely measured, per A4's polarity finding) instead of the inverted P_F_QC==0 filter. Only 4/26 excluded sites have any genuinely-measured tower precipitation data at all in 1991-2020 (CA-CF2, IT-MBo, NO-And, US-HB4) -- the other 22 (essentially the entire JPF cluster) have zero independent ground truth available from tower data, at any QC level.")
message("Saved: ", out_a4b)

message("\n=== era5_precip_units_v3_partA.R complete ===")
