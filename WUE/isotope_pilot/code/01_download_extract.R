## 01_download_extract.R — Download + extract sub-daily (HH/HR) and DD FLUXNET
## Shuttle data for the WUE isotope-pilot sites (WUE_SITES; 13 by default, or
## a subset via WUE_SITE_SUBSET — see 00_config.R).
##
## Data source: FLUXNET Shuttle only (flux_listall() + flux_download()) —
## same Shuttle-as-primary-source convention as the Annual Paper and WAFNET
## side analysis.
##
## Writes ONLY to WUE/isotope_pilot/data/{raw,extracted,processed}/ — never
## touches the repo-root data/ directory (the locked snapshot CSV there is
## read-only, for comparison purposes). See 00_config.R.
##
## Resilience: a per-site download/extract failure does NOT stop this script
## — it is recorded in data/processed/download_manifest_info.csv and the
## script continues with the remaining sites, so 04_report_preanalysis.R can
## state which sites failed. The only hard stop() is a site entirely absent
## from the live Shuttle manifest (cannot proceed without it at all).

source("WUE/isotope_pilot/code/00_config.R")

message("[WUE] Fetching live Shuttle manifest via flux_listall() ...")
live_manifest <- flux_listall()

# Guard against silent hub drops (known issue — see repo memory:
# flux-listall-silently-drops-failed-hubs.md). A missing hub here would
# silently exclude a site if it happens to live on that hub.
expected_hubs <- c("AmeriFlux", "ICOS", "TERN")
missing_hubs  <- setdiff(expected_hubs, unique(live_manifest$data_hub))
if (length(missing_hubs) > 0) {
  warning(
    "[WUE] Hub(s) missing from live manifest -- possible upstream fetch ",
    "failure. If any WUE site is on a missing hub it will silently be ",
    "absent below. Missing hub(s): ", paste(missing_hubs, collapse = ", ")
  )
}

download_manifest <- dplyr::filter(live_manifest, site_id %in% WUE_SITES)
found_sites   <- unique(download_manifest$site_id)
missing_sites <- setdiff(WUE_SITES, found_sites)
if (length(missing_sites) > 0) {
  stop(
    "[WUE] Site(s) requested for this analysis are not in the current ",
    "Shuttle manifest: ", paste(missing_sites, collapse = ", "),
    ". Cannot proceed without them -- report back to the user rather than ",
    "silently continuing with a partial site set."
  )
}
message(
  "[WUE] Manifest resolved for ", nrow(download_manifest), " row(s) across ",
  length(found_sites), " site(s): ", paste(found_sites, collapse = ", ")
)

raw_dir       <- file.path(WUE_ROOT, "data", "raw")
extracted_dir <- file.path(WUE_ROOT, "data", "extracted")
processed_dir <- file.path(WUE_ROOT, "data", "processed")

# --- ZIP discovery + integrity test -----------------------------------------

find_site_zip <- function(site) {
  f <- list.files(raw_dir, pattern = paste0("_", site, "_.*\\.zip$"), full.names = TRUE)
  if (length(f) == 0) return(NA_character_)
  f[1]
}

test_zip <- function(zip_path) {
  if (is.na(zip_path) || !file.exists(zip_path)) return(FALSE)
  rc <- suppressWarnings(system2(
    "unzip", c("-t", shQuote(zip_path)),
    stdout = FALSE, stderr = FALSE
  ))
  identical(rc, 0L)
}

download_one_site <- function(site) {
  manifest_row <- download_manifest[download_manifest$site_id == site, , drop = FALSE]

  existing <- find_site_zip(site)
  if (!is.na(existing) && test_zip(existing)) {
    message("[WUE] ", site, ": existing ZIP passes integrity test -- reusing (",
            existing, ")")
    return(list(download_status = "reused_existing_zip", zip_path = existing))
  }
  if (!is.na(existing)) {
    message("[WUE] ", site, ": existing ZIP failed `unzip -t` -- removing: ", existing)
    file.remove(existing)
  }

  try_download <- function() {
    tryCatch(
      {
        fluxnet::flux_download(
          file_list_df = manifest_row, download_dir = raw_dir, overwrite = TRUE
        )
        TRUE
      },
      error = function(e) {
        message("[WUE] ", site, ": flux_download() error -- ", conditionMessage(e))
        FALSE
      }
    )
  }

  ok <- try_download()
  zip_path <- find_site_zip(site)
  if (ok && test_zip(zip_path)) {
    return(list(download_status = "downloaded", zip_path = zip_path))
  }

  message("[WUE] ", site, ": download or integrity check failed on first attempt -- retrying once")
  if (!is.na(zip_path)) file.remove(zip_path)
  ok2 <- try_download()
  zip_path2 <- find_site_zip(site)
  if (ok2 && test_zip(zip_path2)) {
    return(list(download_status = "downloaded_on_retry", zip_path = zip_path2))
  }

  message("[WUE] ", site, ": download/integrity failed twice -- recording as failed, continuing")
  list(download_status = "failed", zip_path = NA_character_)
}

download_results <- lapply(WUE_SITES, function(site) {
  res <- download_one_site(site)
  data.frame(
    site_id = site,
    download_status = res$download_status,
    zip_path = res$zip_path,
    stringsAsFactors = FALSE
  )
})
download_results <- do.call(rbind, download_results)

succeeded_sites <- download_results$site_id[
  download_results$download_status %in% c("reused_existing_zip", "downloaded", "downloaded_on_retry")
]
message(
  "[WUE] Download complete. Succeeded: ", length(succeeded_sites), "/", length(WUE_SITES),
  ". Failed: ", paste(setdiff(WUE_SITES, succeeded_sites), collapse = ", ")
)

# --- Extract sub-daily (HH/HR) + DD, succeeded sites only -------------------

if (length(succeeded_sites) > 0) {
  message(
    "[WUE] Extracting resolutions [", paste(FLUXNET_EXTRACT_RESOLUTIONS, collapse = " "),
    "] to ", extracted_dir, " for ", length(succeeded_sites), " site(s) ..."
  )
  fluxnet::flux_extract(
    zip_dir     = raw_dir,
    output_dir  = extracted_dir,
    site_ids    = succeeded_sites,
    resolutions = FLUXNET_EXTRACT_RESOLUTIONS
  )
} else {
  warning("[WUE] No sites downloaded successfully -- skipping flux_extract().")
}

file_inventory <- if (dir.exists(extracted_dir)) {
  fluxnet::flux_discover_files(extracted_dir)
} else {
  data.frame()
}
saveRDS(file_inventory, file.path(processed_dir, "file_inventory.rds"))

subdaily_inventory <- if (nrow(file_inventory) > 0) {
  file_inventory[
    !is.na(file_inventory$time_resolution) &
      file_inventory$time_resolution %in% c("HH", "HR"),
    ,
    drop = FALSE
  ]
} else {
  file_inventory
}

resolution_by_site <- if (nrow(subdaily_inventory) > 0) {
  stats::aggregate(
    time_resolution ~ site_id, data = subdaily_inventory,
    FUN = function(x) paste(sort(unique(x)), collapse = "/")
  )
} else {
  data.frame(site_id = character(0), time_resolution = character(0))
}
names(resolution_by_site)[names(resolution_by_site) == "time_resolution"] <- "subdaily_resolution"

uncovered <- setdiff(succeeded_sites, resolution_by_site$site_id)
if (length(uncovered) > 0) {
  warning(
    "[WUE] No HH/HR files extracted for: ", paste(uncovered, collapse = ", "),
    " -- flag this in the report rather than silently proceeding."
  )
}

# --- Per-site manifest info + comparison to locked Annual Paper snapshot ---

manifest_info <- download_manifest[
  , c("site_id", "data_hub", "fluxnet_product_name", "oneflux_code_version",
      "first_year", "last_year")
]
manifest_info <- manifest_info[!duplicated(manifest_info$site_id), ]

if (file.exists(WUE_LOCKED_SNAPSHOT)) {
  locked <- readr::read_csv(WUE_LOCKED_SNAPSHOT, show_col_types = FALSE)
  locked <- locked[locked$site_id %in% WUE_SITES,
                    c("site_id", "fluxnet_product_name", "oneflux_code_version")]
  locked <- locked[!duplicated(locked$site_id), ]
  names(locked) <- c("site_id", "locked_product_name", "locked_oneflux_version")
} else {
  warning("[WUE] Locked snapshot not found at ", WUE_LOCKED_SNAPSHOT,
          " -- differs_from_locked_snapshot will be NA for all sites.")
  locked <- data.frame(
    site_id = character(0), locked_product_name = character(0),
    locked_oneflux_version = character(0)
  )
}

site_inventory <- merge(manifest_info, download_results, by = "site_id", all.x = TRUE)
site_inventory <- merge(site_inventory, resolution_by_site, by = "site_id", all.x = TRUE)
site_inventory <- merge(site_inventory, locked, by = "site_id", all.x = TRUE)

site_inventory$differs_from_locked_snapshot <- ifelse(
  is.na(site_inventory$locked_product_name),
  "site not in locked snapshot",
  ifelse(
    site_inventory$fluxnet_product_name != site_inventory$locked_product_name |
      site_inventory$oneflux_code_version != site_inventory$locked_oneflux_version,
    "yes", "no"
  )
)

write.csv(
  site_inventory,
  file.path(processed_dir, "download_manifest_info.csv"),
  row.names = FALSE
)

message("[WUE] 01_download_extract.R complete.")
