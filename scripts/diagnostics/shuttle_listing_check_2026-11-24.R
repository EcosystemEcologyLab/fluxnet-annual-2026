## shuttle_listing_check_2026-11-24.R — Diagnostic: how many sites does the
## FLUXNET Shuttle list today, compared with the snapshot of record?
##
## Listing only. No flux_download(), no write to data/snapshots/ (the
## committed snapshot-of-record directory) — the live listing is saved to
## data/snapshots/check_only/ instead, with the same Shuttle naming
## convention. FLUXNET_SNAPSHOT_MODE and the pipeline config are untouched.
##
## Prompt: logs/prompts/2026-11-24_shuttle_listing_check.md

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}

source("R/pipeline_config.R")
source("R/credentials.R")
check_pipeline_config()

library(fluxnet)
suppressPackageStartupMessages({
  library(dplyr)
})

message("=== shuttle_listing_check_2026-11-24.R : ", Sys.time(), " ===")

# --- 1. Installed fluxnet-shuttle version -----------------------------------
# Same lookup check_pipeline_config() uses internally (R/pipeline_config.R):
# the CLI --version string, not the Python module (which has no __version__).
installed_version <- tryCatch(
  {
    exe <- fluxnet::flux_install_shuttle()
    if (!is.null(exe) && nzchar(exe) && file.exists(exe)) {
      raw <- system2(exe, "--version", stdout = TRUE, stderr = FALSE)
      trimws(sub("^.*\\s+", "", raw[1]))
    } else {
      NA_character_
    }
  },
  error = function(e) NA_character_
)
message("Installed fluxnet-shuttle version: ", installed_version)

# --- Live listing, once -------------------------------------------------------
live_manifest <- flux_listall()
message("flux_listall() returned ", nrow(live_manifest), " row(s).")

# Hub-drop guard — same logic as scripts/01_download.R lines 21-33. Reported
# per hub here rather than just asserted, per the diagnostic's step 1.
expected_hubs <- c("AmeriFlux", "ICOS", "TERN")
hub_counts <- live_manifest |>
  dplyr::count(data_hub, name = "rows") |>
  dplyr::arrange(data_hub)
message("Rows per hub (data_hub):")
for (i in seq_len(nrow(hub_counts))) {
  message("  ", hub_counts$data_hub[i], ": ", hub_counts$rows[i])
}

missing_hubs <- setdiff(expected_hubs, unique(live_manifest$data_hub))
zero_row_hubs <- hub_counts$data_hub[hub_counts$rows == 0]
if (length(missing_hubs) > 0 || length(zero_row_hubs) > 0) {
  stop(
    "Hub-drop guard tripped — hub(s) missing or empty from the live manifest:\n  ",
    "missing entirely: ", if (length(missing_hubs) > 0) paste(missing_hubs, collapse = ", ") else "(none)", "\n  ",
    "present but zero rows: ", if (length(zero_row_hubs) > 0) paste(zero_row_hubs, collapse = ", ") else "(none)",
    "\nStopping per the diagnostic's instructions — not retrying."
  )
}
message("Hub-drop guard: all expected hubs (", paste(expected_hubs, collapse = ", "), ") present with >0 rows.")

# --- 2. Save the listing to data/snapshots/check_only/, not data/snapshots/ --
# Confirmed by grep (reported in the .md) that every glob/newest-file lookup
# in this repo (find_previous_snapshot(), find_latest_snapshot(), and every
# ad hoc list.files(...,"data/snapshots"...) in scripts/) is non-recursive
# against the top level of data/snapshots/ only, so none of them will see a
# file placed in the check_only/ subfolder.
check_only_dir <- file.path(FLUXNET_DATA_ROOT, "snapshots", "check_only")
fs::dir_create(check_only_dir)
timestamp <- format(Sys.time(), "%Y%m%dT%H%M%S")
listing_filename <- paste0("fluxnet_shuttle_snapshot_", timestamp, ".csv")
listing_path <- file.path(check_only_dir, listing_filename)
readr::write_csv(live_manifest, listing_path)
message("Live listing saved to: ", listing_path)

# --- 3. Counts from today's listing ------------------------------------------
n_rows <- nrow(live_manifest)
n_distinct_site_ids <- dplyr::n_distinct(live_manifest$site_id)
n_distinct_product_ids <- dplyr::n_distinct(live_manifest$product_id)

sites_by_hub <- live_manifest |>
  dplyr::count(product_source_network, name = "n_sites") |>
  dplyr::arrange(product_source_network)

dup_sites <- live_manifest |>
  dplyr::count(site_id, name = "n_rows") |>
  dplyr::filter(n_rows > 1)

dup_detail <- NULL
if (nrow(dup_sites) > 0) {
  dup_detail <- live_manifest |>
    dplyr::filter(site_id %in% dup_sites$site_id) |>
    dplyr::select(site_id, data_hub, product_source_network, product_id,
                  oneflux_code_version, first_year, last_year) |>
    dplyr::arrange(site_id)
}

message("Today's listing: ", n_rows, " rows; ", n_distinct_site_ids, " distinct site_id; ",
        n_distinct_product_ids, " distinct product_id; ", nrow(dup_sites), " site_id(s) listed more than once.")

# --- 4. Compare with the 20 September snapshot -------------------------------
prev_path <- "data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv"
if (!file.exists(prev_path)) {
  stop("Snapshot of record not found at expected path: ", prev_path)
}
previous_snapshot <- readr::read_csv(prev_path, show_col_types = FALSE)

prev_n_rows <- nrow(previous_snapshot)
prev_n_distinct_site_ids <- dplyr::n_distinct(previous_snapshot$site_id)
prev_n_distinct_product_ids <- dplyr::n_distinct(previous_snapshot$product_id)
prev_sites_by_hub <- previous_snapshot |>
  dplyr::count(product_source_network, name = "n_sites") |>
  dplyr::arrange(product_source_network)
prev_dup_sites <- previous_snapshot |>
  dplyr::count(site_id, name = "n_rows") |>
  dplyr::filter(n_rows > 1)

# Matched on site_id (the FLUXNET site ID), per CLAUDE.md Hard Rule #2 — never
# infer hub/region from the site ID; hub comparison here uses data_hub itself.
current_ids <- unique(live_manifest$site_id)
previous_ids <- unique(previous_snapshot$site_id)

added_ids <- setdiff(current_ids, previous_ids)
removed_ids <- setdiff(previous_ids, current_ids)

added_sites <- live_manifest |>
  dplyr::filter(site_id %in% added_ids) |>
  dplyr::distinct(site_id, data_hub, product_source_network) |>
  dplyr::arrange(site_id)

removed_sites <- previous_snapshot |>
  dplyr::filter(site_id %in% removed_ids) |>
  dplyr::distinct(site_id, data_hub, product_source_network) |>
  dplyr::arrange(site_id)

# Sites present in both, with product_id or oneflux_code_version changed.
# Both snapshots should be one row per site_id for this comparison to be
# well-defined; sites duplicated within either snapshot are excluded here
# and reported separately via dup_sites / prev_dup_sites instead.
common_ids <- intersect(current_ids, previous_ids)
common_ids_single_current <- setdiff(common_ids, dup_sites$site_id)
common_ids_single <- setdiff(common_ids_single_current, prev_dup_sites$site_id)

current_common <- live_manifest |>
  dplyr::filter(site_id %in% common_ids_single) |>
  dplyr::select(site_id, product_id, oneflux_code_version)
previous_common <- previous_snapshot |>
  dplyr::filter(site_id %in% common_ids_single) |>
  dplyr::select(site_id, product_id, oneflux_code_version)

product_changes <- dplyr::inner_join(
  previous_common, current_common,
  by = "site_id", suffix = c("_old", "_new")
) |>
  dplyr::filter(product_id_old != product_id_new | oneflux_code_version_old != oneflux_code_version_new) |>
  dplyr::arrange(site_id)

excluded_from_product_diff <- union(dup_sites$site_id, prev_dup_sites$site_id) |>
  intersect(common_ids)

message("Comparison with ", basename(prev_path), ": ",
        length(added_ids), " site(s) added, ", length(removed_ids), " site(s) removed, ",
        nrow(product_changes), " site(s) with a product_id/oneflux_code_version change.")
if (length(excluded_from_product_diff) > 0) {
  message("  (", length(excluded_from_product_diff), " site_id(s) present in both snapshots but duplicated ",
          "within one or both, excluded from the product-change diff: ",
          paste(excluded_from_product_diff, collapse = ", "), ")")
}

# --- Write artefacts for the report ------------------------------------------
out_dir <- "scripts/diagnostics"
fs::dir_create(out_dir)

readr::write_csv(sites_by_hub, file.path(out_dir, "shuttle_listing_check_2026-11-24_sites_by_hub.csv"))
readr::write_csv(hub_counts, file.path(out_dir, "shuttle_listing_check_2026-11-24_hub_counts.csv"))
if (!is.null(dup_detail)) {
  readr::write_csv(dup_detail, file.path(out_dir, "shuttle_listing_check_2026-11-24_duplicate_sites.csv"))
}
readr::write_csv(added_sites, file.path(out_dir, "shuttle_listing_check_2026-11-24_sites_added.csv"))
readr::write_csv(removed_sites, file.path(out_dir, "shuttle_listing_check_2026-11-24_sites_removed.csv"))
readr::write_csv(product_changes, file.path(out_dir, "shuttle_listing_check_2026-11-24_product_changes.csv"))

# Dump the key numbers to an RDS so the .md report step can be written from
# exact values rather than retyped by hand.
results <- list(
  installed_version = installed_version,
  listing_path = listing_path,
  n_rows = n_rows,
  n_distinct_site_ids = n_distinct_site_ids,
  n_distinct_product_ids = n_distinct_product_ids,
  sites_by_hub = sites_by_hub,
  hub_counts = hub_counts,
  dup_sites = dup_sites,
  dup_detail = dup_detail,
  prev_path = prev_path,
  prev_n_rows = prev_n_rows,
  prev_n_distinct_site_ids = prev_n_distinct_site_ids,
  prev_n_distinct_product_ids = prev_n_distinct_product_ids,
  prev_sites_by_hub = prev_sites_by_hub,
  prev_dup_sites = prev_dup_sites,
  added_sites = added_sites,
  removed_sites = removed_sites,
  product_changes = product_changes,
  excluded_from_product_diff = excluded_from_product_diff
)
saveRDS(results, file.path(out_dir, "shuttle_listing_check_2026-11-24_results.rds"))

message("=== shuttle_listing_check_2026-11-24.R complete: ", Sys.time(), " ===")
