## 01_download.R — List available sites and download raw FLUXNET data
## Uses: flux_listall(), flux_download()
## Data source: FLUXNET Shuttle ONLY. See CLAUDE.md Hard Rule #1.

source("R/pipeline_config.R")
source("R/credentials.R")
check_pipeline_config()

library(fluxnet)
if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}

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

# Download raw data
if (nrow(download_manifest) > 0) {
  flux_download(
    file_list_df = download_manifest,
    download_dir = file.path(FLUXNET_DATA_ROOT, "raw")
  )
}
