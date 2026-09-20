## store_refresh_stage3b_audit_gap.R — Stage 3, part b.
##
## WHY THIS SCRIPT EXISTS (a decision recorded, per the "take the option
## that changes least" instruction): the normal compare-and-download cycle
## (store_refresh_stage3_download.R, replicating 01_download.R's logic)
## only diffs the LIVE manifest against the most recent snapshot on disk
## (2026-09-01). It found just 17 changed sites. But review/diagnostics/
## store_audit/table_stage1_changed_sites.csv independently found 61 sites
## whose on-disk extracted data is stale relative to the LIVE archive --
## because most of those 61 sites' product_id/year-range changed sometime
## between June and September without ever being re-downloaded (the
## sequential snapshot-diff chain only catches a change once, at the
## snapshot where it first appears; a site that changed before the
## 2026-09-01 snapshot was cut, but whose extracted directory was never
## regenerated, is invisible to a diff against that same 2026-09-01
## snapshot). Comparing the audit's 61-site list against stage 3's queue
## confirms this: the queue's 17 sites are a strict subset of the audit's
## 61; the other 44 would be silently left stale by the normal cycle alone.
##
## Rather than editing 01_download.R's or R/snapshot.R's comparison logic
## (forbidden for this run), this script explicitly downloads the
## remainder: audit's 61-site list minus whatever stage 3 already covered.
## Same per-site download/verify/retry logic as stage 3, not pipeline code.

if (file.exists(".env")) { library(dotenv); dotenv::load_dot_env() }
source("R/pipeline_config.R")
source("R/credentials.R")
check_pipeline_config()

library(fluxnet)
suppressPackageStartupMessages(library(dplyr))

PROGRESS_LOG <- "logs/store_refresh_stage3b_progress.csv"
OUTD <- "review/diagnostics/store_refresh_20260920/stage3"
fs::dir_create(OUTD)

message("=== store_refresh_stage3b_audit_gap.R : ", Sys.time(), " ===")

audit_stale <- readr::read_csv("review/diagnostics/store_audit/table_stage1_changed_sites.csv", show_col_types = FALSE)
stage3_queue <- readr::read_csv(file.path(OUTD, "table_download_queue.csv"), show_col_types = FALSE)

remaining_ids <- setdiff(unique(audit_stale$site_id), unique(stage3_queue$site_id))
message(nrow(audit_stale), " audit-stale sites; ", nrow(stage3_queue), " already queued by stage 3; ",
        length(remaining_ids), " remaining for stage 3b.")

live_manifest <- flux_listall()
download_manifest <- dplyr::filter(live_manifest, site_id %in% remaining_ids)
message("Stage 3b queue: ", nrow(download_manifest), " site(s) (from live manifest, matched by site_id).")
readr::write_csv(download_manifest, file.path(OUTD, "table_download_queue_3b.csv"))

missing_from_live <- setdiff(remaining_ids, download_manifest$site_id)
if (length(missing_from_live) > 0) {
  message("NOTE: ", length(missing_from_live), " audit-stale site(s) not found in the live manifest (possibly removed from the network): ",
          paste(missing_from_live, collapse = ", "))
}

if (nrow(download_manifest) == 0) {
  message("Nothing left to download in stage 3b.")
  writeLines("STAGE3B_COMPLETE_NOOP", file.path(OUTD, "STAGE3B_STATUS.txt"))
  quit(save = "no", status = 0)
}

zip_dir <- file.path(FLUXNET_DATA_ROOT, "raw")
extracted_dir <- file.path(FLUXNET_DATA_ROOT, "extracted")
fs::dir_create(zip_dir)

check_zip_ok <- function(path) {
  if (!file.exists(path)) return(FALSE)
  tryCatch({ nrow(zip::zip_list(path)) > 0 }, error = function(e) FALSE)
}
find_zip_for_site <- function(sid) {
  f <- list.files(zip_dir, pattern = paste0("_", sid, "_"), full.names = TRUE)
  f <- f[grepl("\\.zip$", f)]
  if (length(f) == 0) return(NA_character_)
  f[[1]]
}
download_one <- function(row) {
  sid <- row$site_id
  tryCatch({
    fluxnet::flux_download(file_list_df = row, download_dir = zip_dir, overwrite = TRUE)
    TRUE
  }, error = function(e) { message("  [", sid, "] download error: ", conditionMessage(e)); FALSE })
}

if (file.exists(PROGRESS_LOG)) {
  progress <- readr::read_csv(PROGRESS_LOG, show_col_types = FALSE)
} else {
  progress <- tibble::tibble(site_id = character(0), status = character(0),
                              attempts = integer(0), zip_path = character(0), timestamp = character(0))
}
done_sites <- progress$site_id[progress$status %in% c("downloaded_verified", "skipped_failed_twice")]
todo <- dplyr::filter(download_manifest, !site_id %in% done_sites)
message(length(done_sites), " already resolved; ", nrow(todo), " remaining.")

n_total <- nrow(todo)
first_pass_failed <- character(0)
for (i in seq_len(n_total)) {
  row <- todo[i, ]
  sid <- row$site_id
  ok <- download_one(row)
  zpath <- find_zip_for_site(sid)
  verified <- ok && check_zip_ok(zpath)
  if (verified) {
    progress <- dplyr::bind_rows(progress, tibble::tibble(site_id = sid, status = "downloaded_verified",
                                                             attempts = 1L, zip_path = zpath, timestamp = as.character(Sys.time())))
    message(sprintf("[%d/%d] %s: OK", i, n_total, sid))
  } else {
    first_pass_failed <- c(first_pass_failed, sid)
    message(sprintf("[%d/%d] %s: FAILED first pass (will retry)", i, n_total, sid))
  }
  readr::write_csv(progress, PROGRESS_LOG)
}

message("Stage 3b first pass complete. ", length(first_pass_failed), " need retry.")
if (length(first_pass_failed) > 0) {
  for (sid in first_pass_failed) {
    row <- dplyr::filter(download_manifest, site_id == sid)
    ok <- download_one(row)
    zpath <- find_zip_for_site(sid)
    verified <- ok && check_zip_ok(zpath)
    status <- if (verified) "downloaded_verified" else "skipped_failed_twice"
    progress <- dplyr::bind_rows(progress, tibble::tibble(site_id = sid, status = status, attempts = 2L,
                                                             zip_path = if (verified) zpath else NA_character_,
                                                             timestamp = as.character(Sys.time())))
    message("  [retry] ", sid, ": ", status)
    readr::write_csv(progress, PROGRESS_LOG)
  }
}

message("Stage 3b download complete. ", sum(progress$status == "downloaded_verified"), " verified, ",
        sum(progress$status == "skipped_failed_twice"), " skipped after 2 failures.")

message("Extracting stage 3b zips at resolutions: ", FLUXNET_EXTRACT_RESOLUTIONS)
flux_extract(zip_dir = zip_dir, output_dir = extracted_dir, site_ids = NULL, resolutions = FLUXNET_EXTRACT_RESOLUTIONS)

file_inventory <- flux_discover_files(extracted_dir)
saveRDS(file_inventory, file.path(FLUXNET_DATA_ROOT, "processed", "file_inventory.rds"))

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
    if (length(site_match) > 0 && site_match[[1]] %in% extracted_sites) { file.remove(zip_path); n_deleted <- n_deleted + 1L }
  }
  message("[ZIP cleanup] Deleted ", n_deleted, " zip(s).")
}

readr::write_csv(progress, file.path(OUTD, "table_download_progress_final_3b.csv"))
writeLines("STAGE3B_COMPLETE", file.path(OUTD, "STAGE3B_STATUS.txt"))
message("=== Stage 3b complete: ", Sys.time(), " ===")
