## 02_extract.R — Extract downloaded archives and discover constituent files
## Uses: flux_extract(), flux_discover_files()

source("R/pipeline_config.R")
check_pipeline_config()

library(fluxnet)
library(dotenv)
dotenv::load_dot_env()

flux_extract(
  raw_dir       = "data/raw",
  extracted_dir = "data/extracted"
)

file_inventory <- flux_discover_files("data/extracted")
saveRDS(file_inventory, "data/processed/file_inventory.rds")
