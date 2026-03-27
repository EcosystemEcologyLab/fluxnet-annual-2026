## 06_analysis.R — Paper-specific analyses
## Populate this script with analyses for the FLUXNET Annual Paper 2026.

source("R/pipeline_config.R")
check_pipeline_config()

library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(dotenv)
dotenv::load_dot_env()

flux_data <- readRDS("data/processed/flux_data_converted.rds")
badm      <- readRDS("data/processed/badm.rds")

# TODO: add paper-specific analyses here
