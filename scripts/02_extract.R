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

extract_site_ids <- if (length(FLUXNET_SITE_FILTER) > 0) FLUXNET_SITE_FILTER else NULL

flux_extract(
  zip_dir     = file.path(FLUXNET_DATA_ROOT, "raw"),
  output_dir  = file.path(FLUXNET_DATA_ROOT, "extracted"),
  site_ids    = extract_site_ids,
  resolutions = FLUXNET_EXTRACT_RESOLUTIONS
)

file_inventory <- flux_discover_files(file.path(FLUXNET_DATA_ROOT, "extracted"))
saveRDS(file_inventory, file.path(FLUXNET_DATA_ROOT, "processed", "file_inventory.rds"))
