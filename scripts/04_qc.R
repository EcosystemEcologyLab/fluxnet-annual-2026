## 04_qc.R — Apply QC filtering at HH/HR resolution
## Uses: flux_qc(), fluxnet_qc_hh()
## See CLAUDE.md QC Flag Reference for flag definitions.

source("R/pipeline_config.R")
check_pipeline_config()

source("R/qc.R")

library(fluxnet)
if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}

processed_dir <- file.path(FLUXNET_DATA_ROOT, "processed")

flux_data <- readRDS(file.path(processed_dir, "flux_data_raw.rds"))

# Package-level QC (ustar filtering already applied in distributed data)
flux_data_qc <- flux_qc(flux_data)

# HH/HR flag filtering: keep measured + good gap-fill (_QC <= 1)
flux_data_qc <- fluxnet_qc_hh(flux_data_qc, max_qc = 1L)

saveRDS(flux_data_qc, file.path(processed_dir, "flux_data_qc.rds"))
