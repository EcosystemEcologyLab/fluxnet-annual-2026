## 05_units.R — Convert variables from native FLUXNET units to analysis units
## Uses: fluxnet_convert_units()
## See CLAUDE.md Unit Conversion Reference. Never implement ad-hoc conversions.

source("R/pipeline_config.R")
check_pipeline_config()

source("R/units.R")

library(dotenv)
dotenv::load_dot_env()

flux_data_qc <- readRDS("data/processed/flux_data_qc.rds")
manifest      <- readRDS("data/processed/file_inventory.rds")  # carries temporal_resolution

flux_data_converted <- fluxnet_convert_units(flux_data_qc, manifest)

saveRDS(flux_data_converted, "data/processed/flux_data_converted.rds")
