## 01_download.R — List available sites and download raw FLUXNET data
## Uses: flux_listall(), flux_download()
## Data source: FLUXNET Shuttle ONLY. See CLAUDE.md Hard Rule #1.

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}

source("R/pipeline_config.R")
source("R/credentials.R")
check_pipeline_config()

library(fluxnet)

creds <- fluxnet_credentials()

# List all available sites from the Shuttle
live_manifest <- flux_listall()

# In development mode, optionally save a snapshot; in locked mode, use the
# snapshot CSV instead of the live manifest.
source("R/snapshot.R")
source("R/sync.R")
manifest <- resolve_snapshot(live_manifest)

snapshot_mode <- Sys.getenv("FLUXNET_SNAPSHOT_MODE", unset = "development")
snapshots_dir <- file.path(FLUXNET_DATA_ROOT, "snapshots")

# In development mode, write a snapshot for reproducibility records, then
# compare with the previous snapshot to limit downloads to changed sites.
if (snapshot_mode == "development") {
  write_snapshot(manifest, snapshot_dir = snapshots_dir)

  prev_path <- find_previous_snapshot(snapshots_dir)

  if (!is.null(prev_path)) {
    message("Comparing with previous snapshot: ", basename(prev_path))
    previous_snapshot <- load_snapshot(prev_path)
    comparison        <- compare_snapshots(manifest, previous_snapshot)

    message(
      "Update detection — ",
      "new sites: ",              nrow(comparison$new_sites),             "; ",
      "extended data coverage: ", nrow(comparison$extended_data),         "; ",
      "reprocessed AmeriFlux: ",  nrow(comparison$reprocessed_ameriflux), "; ",
      "reprocessed ICOS/TERN: pending Gilberto team response (support@fluxnet.org)"
    )

    changed_ids     <- sites_to_download(comparison)
    download_manifest <- dplyr::filter(manifest, site_id %in% changed_ids)

    if (nrow(download_manifest) == 0) {
      message("No changed sites detected. Nothing to download.")
    } else {
      message("Downloading ", nrow(download_manifest), " changed site(s).")
    }
  } else {
    message("No previous snapshot found — downloading all ", nrow(manifest), " sites.")
    download_manifest <- manifest
  }
} else {
  # Locked mode: download the full locked manifest without change detection.
  download_manifest <- manifest
}

# Apply site filter if set.
# When active, download ALL matching sites from the full manifest — not just
# the change-detected subset — so that a filtered test run always has complete
# data regardless of snapshot history.
if (length(FLUXNET_SITE_FILTER) > 0) {
  unknown_ids <- setdiff(FLUXNET_SITE_FILTER, manifest$site_id)
  if (length(unknown_ids) > 0) {
    warning(
      "FLUXNET_SITE_FILTER contains site ID(s) not in the manifest: ",
      paste(unknown_ids, collapse = ", ")
    )
  }
  download_manifest <- dplyr::filter(manifest, site_id %in% FLUXNET_SITE_FILTER)
  message("Site filter applied: ", nrow(download_manifest), " site(s) selected for download.")
}

# Extend download queue with any sites absent from data/extracted/.
# This covers fresh clones where the snapshot diff only sees changed sites,
# leaving the majority of the network undownloaded despite being in the snapshot.
extracted_dir <- file.path(FLUXNET_DATA_ROOT, "extracted")
sites_to_check <- if (length(FLUXNET_SITE_FILTER) > 0) FLUXNET_SITE_FILTER else manifest$site_id
already_extracted <- if (dir.exists(extracted_dir)) {
  # Directories are named e.g. "AMF_US-Ha1_FLUXNET_2008-2012_v1.3_r1".
  # Extract the embedded site ID (CC-XXX pattern) from each directory name.
  dirs <- list.dirs(extracted_dir, full.names = FALSE, recursive = FALSE)
  unique(regmatches(dirs, regexpr("[A-Z]{2}-[A-Za-z0-9]+", dirs)))
} else {
  character(0)
}
new_to_queue <- setdiff(
  setdiff(sites_to_check, already_extracted),
  download_manifest$site_id
)
if (length(new_to_queue) > 0) {
  message(
    length(new_to_queue),
    " site(s) in snapshot have no extracted data; adding to download queue."
  )
  download_manifest <- dplyr::bind_rows(
    download_manifest,
    dplyr::filter(manifest, site_id %in% new_to_queue)
  )
}

# Download raw data
if (nrow(download_manifest) > 0) {
  flux_download(
    file_list_df = download_manifest,
    download_dir = file.path(FLUXNET_DATA_ROOT, "raw")
  )
}
