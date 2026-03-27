## 03_read.R — Read flux data, variable metadata, and BADM
## Uses: flux_read(), flux_varinfo(), flux_badm()

source("R/pipeline_config.R")
check_pipeline_config()

library(fluxnet)
library(dotenv)
dotenv::load_dot_env()

file_inventory <- readRDS("data/processed/file_inventory.rds")

flux_data <- flux_read(file_inventory)
var_info   <- flux_varinfo()
badm       <- flux_badm(file_inventory)

saveRDS(flux_data, "data/processed/flux_data_raw.rds")
saveRDS(var_info,  "data/processed/var_info.rds")
saveRDS(badm,      "data/processed/badm.rds")
