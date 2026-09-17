## 01b_retry_gh_ank.R — one-off retry of the GH-Ank download.
##
## The first 01_download_extract_hh.R run produced a truncated/corrupt ZIP
## for GH-Ank (unzip: "End-of-central-directory signature not found") while
## the other five sites downloaded and extracted cleanly. This is a transient
## download issue, not a structural absence of HH data for GH-Ank (this
## repo's memory already documents flux_download() as not version-pinned and
## occasionally flaky — see flux-download-uses-uv-head-not-pinned-venv.md).
## Retrying just this one site before concluding GH-Ank has no HH data.

source("WAFNET/energy_partitioning/code/00_config.R")

site <- "GH-Ank"
raw_dir       <- file.path(WAFNET_ROOT, "data", "raw")
extracted_dir <- file.path(WAFNET_ROOT, "data", "extracted")

live_manifest <- flux_listall()
download_manifest <- dplyr::filter(live_manifest, site_id == site)
if (nrow(download_manifest) == 0) {
  stop("[WAFNET] ", site, " not found in live manifest on retry.")
}

message("[WAFNET] Re-downloading ", site, " ...")
flux_download(file_list_df = download_manifest, download_dir = raw_dir)

zip_path <- list.files(raw_dir, pattern = paste0(site, ".*\\.zip$"), full.names = TRUE)
if (length(zip_path) == 0) stop("[WAFNET] Retry download produced no ZIP for ", site)

ok <- tryCatch({
  utils::unzip(zip_path[1], list = TRUE)
  TRUE
}, error = function(e) FALSE)
message("[WAFNET] Retry ZIP valid: ", ok)
if (!ok) stop("[WAFNET] Retry ZIP for ", site, " is still corrupt: ", zip_path[1])

message("[WAFNET] Extracting HH resolution for ", site, " ...")
flux_extract(
  zip_dir     = raw_dir,
  output_dir  = extracted_dir,
  site_ids    = site,
  resolutions = "h"
)

file_inventory <- flux_discover_files(extracted_dir)
saveRDS(
  file_inventory,
  file.path(WAFNET_ROOT, "data", "processed", "file_inventory_hh.rds")
)
message("[WAFNET] Inventory rewritten with ", nrow(file_inventory), " row(s).")
print(table(file_inventory$site_id, file_inventory$time_resolution, useNA = "ifany"))
message("[WAFNET] 01b_retry_gh_ank.R complete.")
