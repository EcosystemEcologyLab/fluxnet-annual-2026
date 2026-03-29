## 03_read.R — Read flux data, variable metadata, and BADM
## Uses: flux_read(), flux_varinfo(), flux_badm()

source("R/pipeline_config.R")
check_pipeline_config()

library(fluxnet)
if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}

processed_dir  <- file.path(FLUXNET_DATA_ROOT, "processed")

file_inventory <- readRDS(file.path(processed_dir, "file_inventory.rds"))

flux_data <- flux_read(file_inventory)
var_info   <- flux_varinfo(file_inventory)

# flux_badm() requires an explicit variable_group argument (no default).
# Read all VARIABLE_GROUP values present in the BIF files so this call is
# general rather than hard-coding specific groups.
bif_paths <- file_inventory$path[file_inventory$dataset == "BIF"]
bif_groups <- unique(unlist(lapply(bif_paths[file.exists(bif_paths)], function(p) {
  readr::read_csv(p, show_col_types = FALSE)$VARIABLE_GROUP
})))
badm <- flux_badm(file_inventory, variable_group = tolower(bif_groups))

saveRDS(flux_data, file.path(processed_dir, "flux_data_raw.rds"))
saveRDS(var_info,  file.path(processed_dir, "var_info.rds"))
saveRDS(badm,      file.path(processed_dir, "badm.rds"))
