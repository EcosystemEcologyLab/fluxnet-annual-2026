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
manifest <- resolve_snapshot(live_manifest)

# In development mode, write a snapshot for reproducibility records
snapshot_mode <- Sys.getenv("FLUXNET_SNAPSHOT_MODE", unset = "development")
if (snapshot_mode == "development") {
  write_snapshot(manifest)
}

# Download raw data for all sites in the manifest
flux_download(
  file_list_df = manifest,
  download_dir = "data/raw"
)
