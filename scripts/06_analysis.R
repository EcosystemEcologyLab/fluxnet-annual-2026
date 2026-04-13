## 06_analysis.R — Paper-specific analyses
## Populate this script with analyses for the FLUXNET Annual Paper 2026.

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}

source("R/pipeline_config.R")
check_pipeline_config()

library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)

processed_dir <- file.path(FLUXNET_DATA_ROOT, "processed")

flux_data <- readRDS(file.path(processed_dir, "flux_data_converted.rds"))
badm      <- readRDS(file.path(processed_dir, "badm.rds"))

# TODO: add paper-specific analyses here
