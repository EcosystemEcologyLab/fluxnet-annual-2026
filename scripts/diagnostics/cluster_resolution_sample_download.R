## cluster_resolution_sample_download.R
##
## Follow-up to it_mbo_bug_hunt.R and it_mbo_parsimony.R (review/diagnostics/
## it_mbo_bug_hunt/, it_mbo_parsimony/), which found IT-MBo's reported ERA5
## anomaly is a ~21.25x DD/MM/YY-vs-HH resolution artifact specific to that
## site (a same-hub control, FI-Hyy, shows no such inconsistency), while
## US-HB4 shows a different, uniform-resolution defect. This script draws a
## stratified sample from the wider 123-site 4x/8x cluster identified in
## era5_precip_units_v4 and downloads/extracts HH resolution for the sample,
## so cluster_resolution_sample_check.R can test whether IT-MBo's DD-vs-HH
## mismatch is typical of the cluster or unusual.
##
## Read-only with respect to every existing diagnostic output and the
## pipeline itself. This script only downloads new raw data (to the normal,
## gitignored data/raw/ and data/extracted/ directories) and writes the
## sample-selection table -- it does not touch any pipeline script, figure,
## or earlier report.
##
## Intended to be launched via nohup + disown, per CLAUDE.md's long-running-
## script convention:
##   nohup Rscript scripts/diagnostics/cluster_resolution_sample_download.R \
##     > logs/cluster_resolution_sample_download_<timestamp>.log 2>&1 &
##   disown

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}

source("R/pipeline_config.R")
source("R/credentials.R")
source("R/utils.R")
check_pipeline_config()

library(fluxnet)
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(fs)
})

OUTD <- "review/diagnostics/cluster_resolution_sample"
fs::dir_create(OUTD)

message("=== cluster_resolution_sample_download.R ===")

# ============================================================================
# STEP 1: sample selection -- written to disk BEFORE any download starts
# ============================================================================
message("\n================ Step 1: sample selection ================")

SEED <- 20260920L  # today's date, stated here and in the report
set.seed(SEED)

ref <- readr::read_csv(
  "review/diagnostics/era5_reference_plots/table_site_reference_comparison.csv",
  show_col_types = FALSE
)
# cluster_membership: "4x_cluster", "8x_cluster", "not_in_4x_or_8x_cluster"
# (confirmed directly from the file's own values, not assumed).
# product_source_network: the network field -- Hard Rule 2, never inferred
# from site ID prefix.

flagged <- ref |>
  dplyr::filter(cluster_membership %in% c("4x_cluster", "8x_cluster")) |>
  dplyr::mutate(role = "flagged")
control_pool <- ref |>
  dplyr::filter(cluster_membership == "not_in_4x_or_8x_cluster") |>
  dplyr::mutate(role = "control")

networks <- sort(unique(flagged$product_source_network))
message("Networks present in the 4x/8x cluster: ", paste(networks, collapse = ", "))

sample_rows <- list()
for (net in networks) {
  net_flagged <- dplyr::filter(flagged, product_source_network == net)
  n_draw <- min(3L, nrow(net_flagged))
  drawn_flagged <- net_flagged[sample.int(nrow(net_flagged), n_draw), ]

  net_control_pool <- dplyr::filter(control_pool, product_source_network == net)
  if (nrow(net_control_pool) == 0L) {
    warning("No unflagged control site available for network ", net, " -- skipping control for this network.")
    drawn_control <- net_control_pool[0, ]
  } else {
    drawn_control <- net_control_pool[sample.int(nrow(net_control_pool), 1L), ]
  }

  sample_rows[[net]] <- dplyr::bind_rows(drawn_flagged, drawn_control) |>
    dplyr::mutate(network_flagged_pool_size = nrow(net_flagged),
                   network_draw_rule = if (nrow(net_flagged) < 3L) "all flagged (network has <3)" else "3 flagged, random")
}

sample_df <- dplyr::bind_rows(sample_rows) |>
  dplyr::select(site_id, product_source_network, data_hub, role, cluster_membership,
                network_flagged_pool_size, network_draw_rule,
                era5_map_mm, bio12_mm, badm_map_mm, ratio_to_bio12, ratio_to_badm_map)

message("\nSelected sample (", nrow(sample_df), " sites, seed=", SEED, "):")
print(as.data.frame(sample_df))

out_sample <- file.path(OUTD, "table_0_sample_selection.csv")
readr::write_csv(sample_df, out_sample)
write_output_metadata(out_sample,
  input_sources = "review/diagnostics/era5_reference_plots/table_site_reference_comparison.csv (read-only)",
  notes = sprintf(
    "Stratified sample from the 123-site 4x/8x cluster (era5_precip_units_v4): up to 3 flagged sites per product_source_network (all of them if a network has fewer than 3), plus 1 unflagged control site per network from the same file. Fixed random seed %d, drawn before any download started. %d sites total across %d networks.",
    SEED, nrow(sample_df), length(networks)))
message("Saved sample selection: ", out_sample)

# ============================================================================
# STEP 2: download + extract HH resolution for the sampled sites only
# ============================================================================
message("\n================ Step 2: download + extract HH ================")

creds <- fluxnet_credentials()
site_ids <- unique(sample_df$site_id)
message(length(site_ids), " site(s) to download: ", paste(site_ids, collapse = ", "))

live_manifest <- flux_listall()
download_manifest <- dplyr::filter(live_manifest, site_id %in% site_ids)

missing_from_manifest <- setdiff(site_ids, download_manifest$site_id)
if (length(missing_from_manifest) > 0L) {
  stop("Site(s) in the sample not found in the live manifest: ",
       paste(missing_from_manifest, collapse = ", "))
}

message("Downloading ", nrow(download_manifest), " site(s)...")
flux_download(
  file_list_df = download_manifest,
  download_dir = file.path(FLUXNET_DATA_ROOT, "raw")
)

message("Extracting HH resolution for the sampled sites...")
flux_extract(
  zip_dir     = file.path(FLUXNET_DATA_ROOT, "raw"),
  output_dir  = file.path(FLUXNET_DATA_ROOT, "extracted"),
  site_ids    = site_ids,
  resolutions = "h"
)

# Sentinel file so the (separate) polling step can detect completion without
# parsing the log.
sentinel <- "logs/cluster_resolution_sample_download.DONE"
fs::dir_create("logs")
writeLines(format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), sentinel)
message("\nDone. Sentinel written: ", sentinel)
message("=== cluster_resolution_sample_download.R complete ===")
