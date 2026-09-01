## refresh_site_year_presence.R
##
## Regenerates data/snapshots/site_year_data_presence.csv (raw, unfiltered
## monthly presence — no QC threshold applied) from the DuckDB `monthly`
## table (FLUXMET dataset), via the established compute_site_year_presence()
## convention (R/utils.R, agreed 2026-05-07).
##
## This file had gone stale (last written 2026-05-24, predating even the
## 20260624 frozen snapshot) because nothing in the current DuckDB-based
## pipeline refreshes it — the only prior caller
## (scripts/00_candidate_figures.R) depends on the legacy RDS path
## (data/processed/flux_data_raw_mm.rds), not DuckDB. Consumed by
## scripts/generate_duration_histograms.R (Dur01/05/06/07, is_shuttle=TRUE
## panels) to count observed valid site-years rather than estimate from
## first_year/last_year. Written for the 781-site 2026-09-01 network —
## see SESSION_LOG.md 2026-09-01.
##
## Distinct from data/snapshots/site_year_data_presence_qc_monthly.csv
## (compute_site_record_length.R), which applies QC_THRESHOLD_MM = 0.50 to
## monthly_qc first; this script intentionally uses the raw, unfiltered
## `monthly` table to match generate_duration_histograms.R's existing
## "any month present" convention.

suppressPackageStartupMessages({
  library(dplyr)
  library(DBI)
  library(duckdb)
})

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
check_pipeline_config()
source("R/utils.R")

FLUX_VARS <- c(
  "NEE_VUT_REF", "NEE_CUT_REF",
  "GPP_NT_VUT_REF", "GPP_DT_VUT_REF", "GPP_NT_CUT_REF", "GPP_DT_CUT_REF",
  "RECO_NT_VUT_REF", "RECO_DT_VUT_REF", "RECO_NT_CUT_REF", "RECO_DT_CUT_REF",
  "LE_F_MDS", "H_F_MDS"
)

db_path <- file.path(FLUXNET_DATA_ROOT, "duckdb/fluxnet.duckdb")
if (!file.exists(db_path)) {
  stop("DuckDB database not found: ", db_path,
       "\nRun 03b_create_database.R first.")
}
con <- DBI::dbConnect(duckdb::duckdb(), db_path, read_only = TRUE)
if (!"monthly" %in% DBI::dbListTables(con)) {
  DBI::dbDisconnect(con, shutdown = TRUE)
  stop("monthly table not found in DuckDB — run 03b_create_database.R first.")
}

cols <- paste(c("site_id", "TIMESTAMP", FLUX_VARS), collapse = ", ")
mm <- DBI::dbGetQuery(
  con,
  paste0("SELECT ", cols, " FROM monthly WHERE dataset = 'FLUXMET'")
) |>
  dplyr::rename(DATE = "TIMESTAMP")
DBI::dbDisconnect(con, shutdown = TRUE)

message("Loaded raw monthly (FLUXMET): ", nrow(mm), " rows, ",
        length(unique(mm$site_id)), " sites")

presence <- compute_site_year_presence(
  mm,
  flux_vars = FLUX_VARS,
  out_path  = "data/snapshots/site_year_data_presence.csv"
)

write_output_metadata(
  "data/snapshots/site_year_data_presence.csv",
  input_sources = c(db_path),
  notes = paste(
    "Refreshed 2026-09-01 for the 781-site network (previously stale since",
    "2026-05-24). Raw, unfiltered monthly presence (no QC threshold) via",
    "compute_site_year_presence() (R/utils.R, agreed 2026-05-07), from the",
    "DuckDB 'monthly' table (dataset = 'FLUXMET', all sites, unfiltered).",
    "Consumed by scripts/generate_duration_histograms.R. See",
    "data/snapshots/site_year_data_presence_qc_monthly.csv",
    "(compute_site_record_length.R) for the QC_THRESHOLD_MM = 0.50 variant."
  )
)

message("\nrefresh_site_year_presence.R complete: ",
        sum(presence$has_data), " / ", nrow(presence), " site-years have data.")
