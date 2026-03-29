## 02_extract.R — Extract downloaded archives and discover constituent files
## Uses: flux_extract(), flux_discover_files()

source("R/pipeline_config.R")
source("R/credentials.R")
check_pipeline_config()

library(fluxnet)
if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}

flux_extract(
  zip_dir     = file.path(FLUXNET_DATA_ROOT, "raw"),
  output_dir  = file.path(FLUXNET_DATA_ROOT, "extracted"),
  site_ids    = c("DE-Tha", "US-Bi1", "AU-Wom"),
  resolutions = FLUXNET_EXTRACT_RESOLUTIONS
)

file_inventory <- flux_discover_files(file.path(FLUXNET_DATA_ROOT, "extracted"))
saveRDS(file_inventory, file.path(FLUXNET_DATA_ROOT, "processed", "file_inventory.rds"))
