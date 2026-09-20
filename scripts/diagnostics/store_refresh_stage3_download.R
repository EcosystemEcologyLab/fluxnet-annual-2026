## store_refresh_stage3_download.R — Stage 3 of the 2026-09-20 store refresh.
## Compares the live Shuttle manifest against the last snapshot (as
## 01_download.R does, reusing the same R/snapshot.R + R/sync.R functions
## unmodified), then downloads + extracts every changed/new/reprocessed site
## one at a time with zip verification, retry, and a restart-safe progress
## log. This must complete -- nothing here stops the run except an
## authentication/listing failure (checked upstream in stage 0).
##
## No queue-size cap was found anywhere in scripts/01_download.R,
## R/snapshot.R, R/sync.R, or fluxnet::flux_download() -- the manifest
## produced below is downloaded in full, in a per-site loop for
## verification/retry robustness (01_download.R's single bulk
## flux_download() call has no per-site retry logic, which this task
## requires).

if (file.exists(".env")) { library(dotenv); dotenv::load_dot_env() }
source("R/pipeline_config.R")
source("R/credentials.R")
check_pipeline_config()

library(fluxnet)
suppressPackageStartupMessages({
  library(dplyr)
})

PROGRESS_LOG <- "logs/store_refresh_stage3_progress.csv"
RUN_LOG      <- "logs/store_refresh_stage3_download_20260920.log"
OUTD         <- "review/diagnostics/store_refresh_20260920/stage3"
fs::dir_create(OUTD)

message("=== store_refresh_stage3_download.R : ", Sys.time(), " ===")

creds <- fluxnet_credentials()

live_manifest <- flux_listall()
message("Live manifest: ", nrow(live_manifest), " site-products.")

source("R/snapshot.R")
source("R/sync.R")
manifest <- resolve_snapshot(live_manifest)

snapshots_dir <- file.path(FLUXNET_DATA_ROOT, "snapshots")
# Explicit permission: write a new snapshot CSV to data/snapshots/.
write_snapshot(manifest, snapshot_dir = snapshots_dir)

prev_path <- find_previous_snapshot(snapshots_dir)
if (is.null(prev_path)) {
  # find_previous_snapshot() returns the 2nd-most-recent of what's on disk
  # AFTER the write above added today's; fall back to the pre-refresh
  # baseline snapshot copied in stage 1 if that logic finds nothing.
  prev_path <- "review/diagnostics/store_refresh_20260920/baseline/fluxnet_shuttle_snapshot_20260901T094522.csv"
  message("find_previous_snapshot() found nothing new -- using stage-1 baseline snapshot: ", prev_path)
} else {
  message("Comparing with previous snapshot: ", basename(prev_path))
}
previous_snapshot <- load_snapshot(prev_path)
comparison <- compare_snapshots(manifest, previous_snapshot)

message(
  "Update detection -- new sites: ", nrow(comparison$new_sites),
  "; extended coverage: ", nrow(comparison$extended_data),
  "; reprocessed: ", nrow(comparison$reprocessed)
)

changed_ids <- sites_to_download(comparison)
download_manifest <- dplyr::filter(manifest, site_id %in% changed_ids)

# Same "extend queue with anything missing from data/extracted/" logic as
# 01_download.R (lines 97-123), reproduced here (not edited there) since
# this script replaces 01_download.R's single bulk call with a per-site loop.
extracted_dir <- file.path(FLUXNET_DATA_ROOT, "extracted")
already_extracted <- if (dir.exists(extracted_dir)) {
  dirs <- list.dirs(extracted_dir, full.names = FALSE, recursive = FALSE)
  unique(regmatches(dirs, regexpr("[A-Z]{2}-[A-Za-z0-9]+", dirs)))
} else character(0)
new_to_queue <- setdiff(setdiff(manifest$site_id, already_extracted), download_manifest$site_id)
if (length(new_to_queue) > 0) {
  message(length(new_to_queue), " site(s) with no extracted data -- adding to queue.")
  download_manifest <- dplyr::bind_rows(download_manifest, dplyr::filter(manifest, site_id %in% new_to_queue))
}

message("TOTAL QUEUE SIZE: ", nrow(download_manifest), " site(s). No cap applied.")
readr::write_csv(download_manifest, file.path(OUTD, "table_download_queue.csv"))

if (nrow(download_manifest) == 0) {
  message("Nothing to download. Stage 3 complete (no-op).")
  writeLines("STAGE3_COMPLETE_NOOP", file.path(OUTD, "STAGE3_STATUS.txt"))
  quit(save = "no", status = 0)
}

zip_dir <- file.path(FLUXNET_DATA_ROOT, "raw")
fs::dir_create(zip_dir)

check_zip_ok <- function(path) {
  if (!file.exists(path)) return(FALSE)
  ok <- tryCatch({
    lst <- zip::zip_list(path)
    nrow(lst) > 0
  }, error = function(e) FALSE)
  ok
}

# Restart-safe progress tracking.
if (file.exists(PROGRESS_LOG)) {
  progress <- readr::read_csv(PROGRESS_LOG, show_col_types = FALSE)
} else {
  progress <- tibble::tibble(
    site_id = character(0), status = character(0),
    attempts = integer(0), zip_path = character(0), timestamp = character(0)
  )
}

done_sites <- progress$site_id[progress$status %in% c("downloaded_verified", "skipped_failed_twice")]
todo <- dplyr::filter(download_manifest, !site_id %in% done_sites)
message(length(done_sites), " site(s) already resolved from a prior run; ", nrow(todo), " remaining.")

download_one <- function(row) {
  sid <- row$site_id
  res <- tryCatch({
    fluxnet::flux_download(file_list_df = row, download_dir = zip_dir, overwrite = TRUE)
    TRUE
  }, error = function(e) {
    message("  [", sid, "] download error: ", conditionMessage(e))
    FALSE
  })
  res
}

find_zip_for_site <- function(sid) {
  f <- list.files(zip_dir, pattern = paste0("_", sid, "_"), full.names = TRUE)
  f <- f[grepl("\\.zip$", f)]
  if (length(f) == 0) return(NA_character_)
  f[[1]]
}

n_total <- nrow(todo)
i_done  <- 0L
first_pass_failed <- character(0)

for (i in seq_len(n_total)) {
  row <- todo[i, ]
  sid <- row$site_id
  ok <- download_one(row)
  zpath <- find_zip_for_site(sid)
  verified <- ok && check_zip_ok(zpath)
  i_done <- i_done + 1L
  if (verified) {
    progress <- dplyr::bind_rows(progress, tibble::tibble(
      site_id = sid, status = "downloaded_verified", attempts = 1L,
      zip_path = zpath, timestamp = as.character(Sys.time())
    ))
    message(sprintf("[%d/%d] %s: OK", i_done, n_total, sid))
  } else {
    first_pass_failed <- c(first_pass_failed, sid)
    message(sprintf("[%d/%d] %s: FAILED first pass (will retry)", i_done, n_total, sid))
  }
  readr::write_csv(progress, PROGRESS_LOG)
}

message("First pass complete. ", length(first_pass_failed), " site(s) need retry.")

# Second pass: retry failures once.
retry_failed <- character(0)
if (length(first_pass_failed) > 0) {
  for (sid in first_pass_failed) {
    row <- dplyr::filter(download_manifest, site_id == sid)
    ok <- download_one(row)
    zpath <- find_zip_for_site(sid)
    verified <- ok && check_zip_ok(zpath)
    if (verified) {
      progress <- dplyr::bind_rows(progress, tibble::tibble(
        site_id = sid, status = "downloaded_verified", attempts = 2L,
        zip_path = zpath, timestamp = as.character(Sys.time())
      ))
      message("  [retry] ", sid, ": OK on retry")
    } else {
      retry_failed <- c(retry_failed, sid)
      progress <- dplyr::bind_rows(progress, tibble::tibble(
        site_id = sid, status = "skipped_failed_twice", attempts = 2L,
        zip_path = NA_character_, timestamp = as.character(Sys.time())
      ))
      message("  [retry] ", sid, ": FAILED again -- skipped")
    }
    readr::write_csv(progress, PROGRESS_LOG)
  }
}

message("Download stage complete. ", sum(progress$status == "downloaded_verified"),
        " verified, ", sum(progress$status == "skipped_failed_twice"), " skipped after 2 failures.")

# ---- Extraction: batch flux_extract() over all verified zips ----
message("Extracting verified zips at resolutions: ", FLUXNET_EXTRACT_RESOLUTIONS)
flux_extract(
  zip_dir     = zip_dir,
  output_dir  = extracted_dir,
  site_ids    = NULL,
  resolutions = FLUXNET_EXTRACT_RESOLUTIONS
)

dir.create(file.path(FLUXNET_DATA_ROOT, "processed"), recursive = TRUE, showWarnings = FALSE)
file_inventory <- flux_discover_files(extracted_dir)
saveRDS(file_inventory, file.path(FLUXNET_DATA_ROOT, "processed", "file_inventory.rds"))

# ZIP cleanup, same logic as 02_extract.R, reused not edited.
if (FLUXNET_DELETE_ZIPS) {
  zip_files <- list.files(zip_dir, pattern = "\\.zip$", full.names = TRUE)
  extracted_sites <- unique(file_inventory$site_id[
    !is.na(file_inventory$site_id) & !is.na(file_inventory$path) &
      nchar(file_inventory$path) > 0 & file.exists(file_inventory$path)
  ])
  n_deleted <- 0L
  for (zip_path in zip_files) {
    fname <- basename(zip_path)
    site_match <- regmatches(fname, regexpr("[A-Z]{2}-[A-Za-z0-9]+", fname))
    if (length(site_match) > 0 && site_match[[1]] %in% extracted_sites) {
      file.remove(zip_path)
      n_deleted <- n_deleted + 1L
    }
  }
  message("[ZIP cleanup] Deleted ", n_deleted, " zip(s).")
}

readr::write_csv(progress, file.path(OUTD, "table_download_progress_final.csv"))
writeLines("STAGE3_COMPLETE", file.path(OUTD, "STAGE3_STATUS.txt"))
message("=== Stage 3 complete: ", Sys.time(), " ===")
