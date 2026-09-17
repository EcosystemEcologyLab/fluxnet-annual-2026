## 01_download_extract_hh.R — Download + extract half-hourly (HH) FLUXNET
## Shuttle data for the six WAFNET energy-partitioning sites.
##
## Data source: FLUXNET Shuttle only (flux_listall() + flux_download()) —
## this side analysis follows the same Shuttle-as-primary-source convention
## as the Annual Paper (CLAUDE.md Hard Rule #1), by choice, for the same
## reproducibility reasons, even though that rule's text is scoped to the
## Annual Paper specifically.
##
## Writes ONLY to WAFNET/energy_partitioning/data/{raw,extracted,processed}/
## — never touches the repo-root data/ directory. See 00_config.R.

source("WAFNET/energy_partitioning/code/00_config.R")

message("[WAFNET] Fetching live Shuttle manifest via flux_listall() ...")
live_manifest <- flux_listall()

# Guard against silent hub drops (known issue — see repo memory:
# flux-listall-silently-drops-failed-hubs.md). A missing hub here would
# silently exclude a site if it happens to live on that hub.
expected_hubs <- c("AmeriFlux", "ICOS", "TERN")
missing_hubs  <- setdiff(expected_hubs, unique(live_manifest$data_hub))
if (length(missing_hubs) > 0) {
  warning(
    "[WAFNET] Hub(s) missing from live manifest -- possible upstream fetch ",
    "failure. If any WAFNET site is on a missing hub it will silently be ",
    "absent below. Missing hub(s): ", paste(missing_hubs, collapse = ", ")
  )
}

download_manifest <- dplyr::filter(live_manifest, site_id %in% WAFNET_SITES)

found_sites   <- unique(download_manifest$site_id)
missing_sites <- setdiff(WAFNET_SITES, found_sites)
if (length(missing_sites) > 0) {
  stop(
    "[WAFNET] Site(s) requested for this analysis are not in the current ",
    "Shuttle manifest: ", paste(missing_sites, collapse = ", "),
    ". Cannot proceed without them -- report back to the user rather than ",
    "silently continuing with a partial site set."
  )
}
message("[WAFNET] Manifest resolved for ", nrow(download_manifest),
        " row(s) across ", length(found_sites), " site(s): ",
        paste(found_sites, collapse = ", "))

raw_dir <- file.path(WAFNET_ROOT, "data", "raw")
message("[WAFNET] Downloading raw archives to ", raw_dir, " ...")
flux_download(file_list_df = download_manifest, download_dir = raw_dir)

zip_count <- length(list.files(raw_dir, pattern = "\\.zip$"))
message("[WAFNET] ", zip_count, " ZIP(s) present in ", raw_dir)
if (zip_count == 0L) {
  stop("[WAFNET] No ZIPs downloaded -- check flux_download() output above.")
}

extracted_dir <- file.path(WAFNET_ROOT, "data", "extracted")
message("[WAFNET] Extracting HH resolution to ", extracted_dir, " ...")
flux_extract(
  zip_dir     = raw_dir,
  output_dir  = extracted_dir,
  site_ids    = WAFNET_SITES,
  resolutions = "h"
)

file_inventory <- flux_discover_files(extracted_dir)
saveRDS(
  file_inventory,
  file.path(WAFNET_ROOT, "data", "processed", "file_inventory_hh.rds")
)

message("[WAFNET] Extraction complete. Inventory: ", nrow(file_inventory), " row(s).")
message("[WAFNET] site_id x time_resolution:")
print(table(file_inventory$site_id, file_inventory$time_resolution, useNA = "ifany"))

covered_sites <- unique(file_inventory$site_id[
  file_inventory$time_resolution %in% c("HH", "HR")
])
uncovered <- setdiff(WAFNET_SITES, covered_sites)
if (length(uncovered) > 0) {
  warning(
    "[WAFNET] No HH/HR files extracted for: ", paste(uncovered, collapse = ", "),
    " -- flag this in the report-back rather than silently proceeding with ",
    "a reduced site set."
  )
}

message("[WAFNET] 01_download_extract_hh.R complete.")
