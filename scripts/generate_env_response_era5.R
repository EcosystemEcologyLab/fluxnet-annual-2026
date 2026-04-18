## generate_env_response_era5.R
## Generate fig_environmental_response_era5 using site-year observations.
## Output: review/figures/climate/fig_environmental_response_era5.png

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}

source("R/pipeline_config.R")
source("R/plot_constants.R")
source("R/figures/fig_environmental_response.R")

library(patchwork)
library(dplyr)
library(readr)

data_yy <- readRDS(file.path(FLUXNET_DATA_ROOT, "processed",
                              "flux_data_converted_yy.rds"))

snap_file <- sort(
  list.files(file.path(FLUXNET_DATA_ROOT, "snapshots"),
             pattern = "fluxnet_shuttle_snapshot.*\\.csv$",
             full.names = TRUE),
  decreasing = TRUE
)[[1]]
metadata <- readr::read_csv(snap_file, show_col_types = FALSE)

message("Building fig_environmental_response_era5 ...")
p <- fig_environmental_response_era5(data_yy, metadata = metadata)

out_dir <- file.path("review", "figures", "climate")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
out_path <- file.path(out_dir, "fig_environmental_response_era5.png")

ggplot2::ggsave(out_path, plot = p, width = 14, height = 12,
                units = "in", dpi = 150, bg = "white")
message("Saved: ", out_path)
