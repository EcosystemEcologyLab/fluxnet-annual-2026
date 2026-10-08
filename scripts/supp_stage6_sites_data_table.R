## supp_stage6_sites_data_table.R
##
## supplementary_data_1_sites.csv: one row per site from the 20 September
## snapshot listing (data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv,
## snapshot of record -- confirmed to match the DuckDB store exactly, see
## review/technical_validation_interim/checks.txt Check 1) -- site ID,
## product name, product version, network code, DOI or handle.
##
## Product version is parsed from the product name itself (e.g.
## "AMF_AR-Bal_FLUXNET_2012-2013_v1.3_r1.zip" -> "v1.3_r1"), not the
## snapshot's own oneflux_code_version field (which only carries the coarser
## "v1.3", dropping the per-site revision suffix "_r1"/"_r2" -- confirmed
## both r1 and r2 sites exist in this network). Network code: the product
## name's own first underscore-delimited token, same convention as
## scripts/supp_stage5_regional_networks_table.R's Table S1.

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
check_pipeline_config()
source("R/utils.R")

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(stringr)
})

msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)
msg("=== supplementary_data_1_sites.csv ===")

OUT_DIR <- "review/figures/draft_manuscript_v1/SupTables"
fs::dir_create(OUT_DIR)

SNAPSHOT_OF_RECORD <- "data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv"
snap <- read_csv(SNAPSHOT_OF_RECORD, show_col_types = FALSE)
msg("Loaded: ", nrow(snap), " sites from ", SNAPSHOT_OF_RECORD)

sites_table <- snap |>
  transmute(
    site_id         = site_id,
    product_name    = fluxnet_product_name,
    product_version = str_extract(fluxnet_product_name, "v[0-9]+\\.[0-9]+_r[0-9]+"),
    network_code    = vapply(strsplit(fluxnet_product_name, "_", fixed = TRUE), `[`, character(1), 1),
    doi_or_handle   = product_id
  ) |>
  arrange(site_id)

n_missing_version <- sum(is.na(sites_table$product_version))
n_missing_doi      <- sum(is.na(sites_table$doi_or_handle) | sites_table$doi_or_handle == "")
if (n_missing_version > 0L || n_missing_doi > 0L) {
  stop("supp_stage6_sites_data_table.R: ", n_missing_version, " site(s) with no parseable ",
       "product_version and ", n_missing_doi, " site(s) with no doi_or_handle -- stopping, not ",
       "writing a table with gaps in required fields.", call. = FALSE)
}
if (nrow(sites_table) != 781L) {
  stop("supp_stage6_sites_data_table.R: expected 781 sites, got ", nrow(sites_table),
       " -- stopping per the same check convention as Table S1.", call. = FALSE)
}
msg("Checks passed: 781 rows, no missing product_version or doi_or_handle.")

out_path <- file.path(OUT_DIR, "supplementary_data_1_sites.csv")
write_csv(sites_table, out_path)
write_output_metadata(
  out_path,
  input_sources = SNAPSHOT_OF_RECORD,
  notes = paste0(
    "One row per site from the snapshot of record, 20 September 2026. product_version parsed ",
    "from product_name itself (vX.Y_rZ), not the snapshot's coarser oneflux_code_version field. ",
    "network_code: product_name's first underscore-delimited token (same convention as Table S1). ",
    "doi_or_handle: snapshot's own product_id field. Checks run and passed: 781 rows, no missing ",
    "product_version or doi_or_handle."
  )
)
msg("Saved: ", out_path, " (", nrow(sites_table), " rows)")
msg("=== Done ===")
