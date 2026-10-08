## supp_stage6_sites_data_table.R
##
## supplementary_data_1_sites.csv: one row per site from the 20 September
## snapshot listing (data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv,
## snapshot of record -- confirmed to match the DuckDB store exactly, see
## review/technical_validation_interim/checks.txt Check 1) -- site ID,
## product name, product version, network code, identifier URL, identifier type.
##
## Product version is parsed from the product name itself (e.g.
## "AMF_AR-Bal_FLUXNET_2012-2013_v1.3_r1.zip" -> "v1.3_r1"), not the
## snapshot's own oneflux_code_version field (which only carries the coarser
## "v1.3", dropping the per-site revision suffix "_r1"/"_r2" -- confirmed
## both r1 and r2 sites exist in this network). Network code: the product
## name's own first underscore-delimited token, same convention as
## scripts/supp_stage5_regional_networks_table.R's Table S1. product_name has
## the trailing ".zip" dropped for display (2026-10-08 follow-up).
##
## identifier_url/identifier_type (2026-10-08 follow-up, replacing a single
## doi_or_handle column): AMF and TERN sites' product_id is a DOI (AMF bare,
## e.g. "10.17190/AMF/2571144"; TERN already a "https://dx.doi.org/..." URL)
## -- rewritten to a canonical "https://doi.org/<doi>" URL (dx.doi.org prefix
## stripped first, so both forms land on the same canonical host). Every
## other network code here sits on the ICOS hub (per
## scripts/supp_stage5_regional_networks_table.R's Table S1 -- hub taken from
## the snapshot's own data_hub field, not inferred from site ID or network
## code, CLAUDE.md Hard Rule 2) and its product_id is an ICOS Carbon Portal
## handle suffix, not a DOI -- rewritten to "https://hdl.handle.net/11676/
## <product_id>" (11676 is ICOS's registered handle prefix).

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

## DOI networks: AMF (bare DOI) and TERN (already a dx.doi.org URL) --
## everything else sits on the ICOS hub and carries an ICOS handle suffix
## instead (see header note).
DOI_NETWORK_CODES <- c("AMF", "TERN")
strip_doi_host <- function(x) sub("^https?://(dx\\.)?doi\\.org/", "", x)

sites_table <- snap |>
  transmute(
    site_id         = site_id,
    product_name    = str_remove(fluxnet_product_name, "\\.zip$"),
    product_version = str_extract(fluxnet_product_name, "v[0-9]+\\.[0-9]+_r[0-9]+"),
    network_code    = vapply(strsplit(fluxnet_product_name, "_", fixed = TRUE), `[`, character(1), 1),
    raw_id          = product_id
  ) |>
  mutate(
    identifier_type = ifelse(network_code %in% DOI_NETWORK_CODES, "DOI", "handle"),
    identifier_url  = ifelse(
      identifier_type == "DOI",
      paste0("https://doi.org/", strip_doi_host(raw_id)),
      paste0("https://hdl.handle.net/11676/", raw_id)
    )
  ) |>
  select(site_id, product_name, product_version, network_code, identifier_url, identifier_type) |>
  arrange(site_id)

n_missing_version <- sum(is.na(sites_table$product_version))
n_missing_id       <- sum(is.na(sites_table$identifier_url) | sites_table$identifier_url == "")
if (n_missing_version > 0L || n_missing_id > 0L) {
  stop("supp_stage6_sites_data_table.R: ", n_missing_version, " site(s) with no parseable ",
       "product_version and ", n_missing_id, " site(s) with no identifier_url -- stopping, not ",
       "writing a table with gaps in required fields.", call. = FALSE)
}
if (nrow(sites_table) != 781L) {
  stop("supp_stage6_sites_data_table.R: expected 781 sites, got ", nrow(sites_table),
       " -- stopping per the same check convention as Table S1.", call. = FALSE)
}
n_doi    <- sum(sites_table$identifier_type == "DOI")
n_handle <- sum(sites_table$identifier_type == "handle")
if (n_doi != 433L || n_handle != 348L) {
  stop("supp_stage6_sites_data_table.R: expected 433 DOIs and 348 handles, got ",
       n_doi, " DOIs and ", n_handle, " handles -- stopping.", call. = FALSE)
}
msg("Checks passed: 781 rows, no missing product_version or identifier_url, ",
    n_doi, " DOIs + ", n_handle, " handles.")

out_path <- file.path(OUT_DIR, "supplementary_data_1_sites.csv")
write_csv(sites_table, out_path)
write_output_metadata(
  out_path,
  input_sources = SNAPSHOT_OF_RECORD,
  notes = paste0(
    "One row per site from the snapshot of record, 20 September 2026. product_name has its ",
    "trailing .zip dropped. product_version parsed from product_name itself (vX.Y_rZ), not the ",
    "snapshot's coarser oneflux_code_version field. network_code: product_name's first ",
    "underscore-delimited token (same convention as Table S1). identifier_url/identifier_type: ",
    "AMF and TERN sites' product_id is a DOI, rewritten to a canonical https://doi.org/ URL; ",
    "every other (ICOS-hub) network code's product_id is an ICOS handle suffix, rewritten to ",
    "https://hdl.handle.net/11676/<product_id>. Checks run and passed: 781 rows, no missing ",
    "product_version or identifier_url, 433 DOIs + 348 handles."
  )
)
msg("Saved: ", out_path, " (", nrow(sites_table), " rows)")
msg("=== Done ===")
