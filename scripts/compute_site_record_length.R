## compute_site_record_length.R
##
## Canonical "years of data" metric per site, for network-duration claims in
## manuscript prose (e.g. "N sites hold >=10 years of data, M hold >=20").
##
## Reuses the established broad-12-variable presence convention
## (compute_site_year_presence() in R/utils.R; agreed 2026-05-07 — a site-
## month counts if any of NEE/GPP/RECO (VUT+CUT, NT+DT where applicable) or
## LE_F_MDS/H_F_MDS is non-NA), applied here to QC-passing monthly data
## (monthly_qc, QC_THRESHOLD_MM = 0.50) rather than raw monthly, with an
## added requirement of >=6 months present within a year for that year to
## count as a "record year" — a stricter bar than the base convention's
## "any month present", chosen to avoid counting single-month years as a
## full year of record. See SESSION_LOG.md 2026-07-01 for the discussion
## that led to this definition (multiple candidate definitions gave site
## counts ranging ~148-218 for the >=10yr threshold and ~36-59 for >=20yr;
## this was selected as the strictest well-justified option).

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(DBI)
  library(duckdb)
})

source("R/pipeline_config.R")
check_pipeline_config()
source("R/utils.R")

FLUX_VARS <- c(
  "NEE_VUT_REF", "NEE_CUT_REF",
  "GPP_NT_VUT_REF", "GPP_DT_VUT_REF", "GPP_NT_CUT_REF", "GPP_DT_CUT_REF",
  "RECO_NT_VUT_REF", "RECO_DT_VUT_REF", "RECO_NT_CUT_REF", "RECO_DT_CUT_REF",
  "LE_F_MDS", "H_F_MDS"
)
MIN_MONTHS_PER_YEAR <- 6L
RECORD_THRESHOLDS   <- c(10L, 20L)

# ============================================================================
# 1. Pull QC-passing monthly data from DuckDB
# ============================================================================

db_path <- file.path(FLUXNET_DATA_ROOT, "duckdb/fluxnet.duckdb")
if (!file.exists(db_path)) {
  stop("DuckDB database not found: ", db_path,
       "\nRun 03b_create_database.R, 04_qc.R, 05_units.R first.")
}
con <- DBI::dbConnect(duckdb::duckdb(), db_path, read_only = TRUE)

if (!"monthly_qc" %in% DBI::dbListTables(con)) {
  DBI::dbDisconnect(con, shutdown = TRUE)
  stop("monthly_qc table not found in DuckDB — run 04_qc.R first.")
}

cols <- paste(c("site_id", "TIMESTAMP", FLUX_VARS), collapse = ", ")
mm <- DBI::dbGetQuery(con, paste0("SELECT ", cols, " FROM monthly_qc")) |>
  dplyr::rename(DATE = "TIMESTAMP")
DBI::dbDisconnect(con, shutdown = TRUE)

message("Loaded monthly_qc: ", nrow(mm), " rows, ", length(unique(mm$site_id)), " sites")

# ============================================================================
# 2. Site-year presence via the established broad-12-var convention
#    (n_months_present per site-year), then apply the >=6-months bar
# ============================================================================

presence <- compute_site_year_presence(
  mm,
  flux_vars = FLUX_VARS,
  out_path  = "data/snapshots/site_year_data_presence_qc_monthly.csv"
)

write_output_metadata(
  "data/snapshots/site_year_data_presence_qc_monthly.csv",
  input_sources = c(db_path),
  notes = paste(
    "Intermediate output of compute_site_record_length.R, produced by",
    "calling the established compute_site_year_presence() (R/utils.R,",
    "agreed 2026-05-07) on QC-passing monthly data (monthly_qc,",
    "QC_THRESHOLD_MM = 0.50), rather than raw monthly. n_months_present is",
    "unfiltered here (the >=6-months-per-year 'record year' bar is applied",
    "downstream, in data/snapshots/site_record_length.csv). See that file's",
    "own .meta.json for the full record-length definition."
  )
)

record_years <- presence |>
  dplyr::filter(.data$n_months_present >= MIN_MONTHS_PER_YEAR) |>
  dplyr::count(.data$site_id, name = "n_record_years")

# ============================================================================
# 3. Join onto the full master site list so sites with zero record years
#    appear explicitly (not silently dropped)
# ============================================================================

snap_path <- sort(
  list.files(file.path(FLUXNET_DATA_ROOT, "snapshots"),
             pattern = "^fluxnet_shuttle_snapshot_.*\\.csv$", full.names = TRUE),
  decreasing = TRUE
)[1]
message("Using shuttle snapshot: ", snap_path)

snapshot <- readr::read_csv(snap_path, show_col_types = FALSE) |>
  dplyr::distinct(.data$site_id, .keep_all = TRUE) |>
  dplyr::select("site_id", "igbp", "network")

record_length <- snapshot |>
  dplyr::left_join(record_years, by = "site_id") |>
  dplyr::mutate(n_record_years = tidyr::replace_na(.data$n_record_years, 0L))

for (thr in RECORD_THRESHOLDS) {
  record_length[[paste0("ge_", thr, "yr")]] <- record_length$n_record_years >= thr
}

out_path <- "data/snapshots/site_record_length.csv"
readr::write_csv(record_length, out_path)
message("Written: ", out_path, " (", nrow(record_length), " sites)")

write_output_metadata(
  out_path,
  input_sources = c(db_path, snap_path),
  notes = paste(
    "Canonical 'years of data' metric for manuscript record-length claims.",
    "n_record_years = count of site-years where a 'record year' requires",
    ">=6 of 12 months with at least one of NEE_VUT_REF, NEE_CUT_REF,",
    "GPP_NT_VUT_REF, GPP_DT_VUT_REF, GPP_NT_CUT_REF, GPP_DT_CUT_REF,",
    "RECO_NT_VUT_REF, RECO_DT_VUT_REF, RECO_NT_CUT_REF, RECO_DT_CUT_REF,",
    "LE_F_MDS, or H_F_MDS non-NA, computed from monthly_qc (QC_THRESHOLD_MM",
    "= 0.50 already applied). This extends the established broad-12-var",
    "presence convention in compute_site_year_presence() (R/utils.R,",
    "agreed 2026-05-07, used for is_functionally_active()) with an added",
    ">=6-months-present-per-year threshold, which that convention does not",
    "itself apply (its own bar is 'any month present'). ge_10yr/ge_20yr are",
    "boolean flags at n_record_years >= 10 / >= 20. Sites absent from",
    "monthly_qc (not yet extracted/QC'd) get n_record_years = 0, not NA —",
    "check has_data-style presence separately if 'unknown' vs 'zero years'",
    "matters for a given use. See SESSION_LOG.md 2026-07-01 for the",
    "definition discussion and alternative counts under other definitions."
  )
)

# ============================================================================
# 4. Console summary
# ============================================================================

message("\n==== Site record length summary (>= ", MIN_MONTHS_PER_YEAR, " months/yr, QC-passing) ====")
for (thr in RECORD_THRESHOLDS) {
  n <- sum(record_length[[paste0("ge_", thr, "yr")]])
  message(sprintf("Sites with >= %d record years: %d / %d", thr, n, nrow(record_length)))
}

message("\ncompute_site_record_length.R complete.")
