## cluster_resolution_sample_retry.R
##
## One-off retry for 3 of the 27 sites in cluster_resolution_sample_download.R
## (RU-Ege, MY-LHP, AU-Ya1) whose ZIP downloads were truncated/corrupted on
## the first attempt (confirmed via Python zipfile: "File is not a zip file",
## "End-of-central-directory signature not found" -- not a disk-space issue,
## 121 GiB free). Not a new diagnostic step -- same sample, same seed, just
## re-fetching the 3 sites whose archives failed to complete.

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}

source("R/pipeline_config.R")
source("R/credentials.R")
check_pipeline_config()

library(fluxnet)
suppressPackageStartupMessages(library(dplyr))

RETRY_SITES <- c("RU-Ege", "MY-LHP", "AU-Ya1")

creds <- fluxnet_credentials()
live_manifest <- flux_listall()
download_manifest <- dplyr::filter(live_manifest, site_id %in% RETRY_SITES)

stopifnot(nrow(download_manifest) == length(RETRY_SITES))

message("Retrying download for: ", paste(RETRY_SITES, collapse = ", "))
flux_download(
  file_list_df = download_manifest,
  download_dir = file.path(FLUXNET_DATA_ROOT, "raw")
)

message("Extracting HH resolution for retried sites...")
flux_extract(
  zip_dir     = file.path(FLUXNET_DATA_ROOT, "raw"),
  output_dir  = file.path(FLUXNET_DATA_ROOT, "extracted"),
  site_ids    = RETRY_SITES,
  resolutions = "h"
)

fs::dir_create("logs")
writeLines(format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), "logs/cluster_resolution_sample_retry.DONE")
message("Done. Sentinel written: logs/cluster_resolution_sample_retry.DONE")
