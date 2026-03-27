## 07_figures.R — Generate paper figures
## Output files go to figures/. Directory is gitignored.

source("R/pipeline_config.R")
check_pipeline_config()

library(ggplot2)
library(dplyr)
library(fs)
library(dotenv)
dotenv::load_dot_env()

fs::dir_create("figures")

# TODO: add figure generation code here
