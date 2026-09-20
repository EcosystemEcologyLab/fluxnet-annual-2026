## store_audit_stage2_download.R -- one download pass for the store audit.
## Downloads + extracts MM/DD/YY for the site list in
## review/diagnostics/store_audit/table_stage2_site_list.csv into a
## gitignored scratch dir, verifying each zip before extracting and
## retrying a truncated site once. Restart-safe: skips sites already
## downloaded and verified. Logs progress as a running count.
##
## Usage:
##   nohup Rscript scripts/diagnostics/store_audit_stage2_download.R \
##     > logs/store_audit_stage2_download_20260920.log 2>&1 & disown

if (file.exists(".env")) { library(dotenv); dotenv::load_dot_env() }
source("R/pipeline_config.R")
check_pipeline_config()
suppressPackageStartupMessages({library(fluxnet); library(dplyr); library(readr); library(fs)})

SCRATCH   <- "data/raw/store_audit_scratch"
RAW_DIR   <- file.path(SCRATCH, "raw")
EXT_DIR   <- file.path(SCRATCH, "extracted")
dir_create(RAW_DIR); dir_create(EXT_DIR)

site_list <- read_csv("review/diagnostics/store_audit/table_stage2_site_list.csv", show_col_types = FALSE)
sites <- unique(site_list$site_id)
cat(sprintf("[%s] Stage 2 download: %d sites queued\n", Sys.time(), length(sites)))

live <- read_csv("review/diagnostics/store_audit/live_manifest_20260920.csv", show_col_types = FALSE)
manifest <- filter(live, site_id %in% sites)
missing <- setdiff(sites, manifest$site_id)
if (length(missing) > 0) cat("WARNING -- sites not found in live manifest:", paste(missing, collapse=", "), "\n")

Sys.setenv(FLUXNET_EXTRACT_RESOLUTIONS = "m d y")
Sys.setenv(FLUXNET_DELETE_ZIPS = "FALSE")  # keep zips so a restart can re-verify without re-downloading

## NOTE: the distributed zips have no top-level per-site folder, so
## utils::unzip() below extracts every site's files FLAT into EXT_DIR --
## list.dirs() would never find anything (fixed here; this bug did not
## affect the audit's single continuous run, since already_done() only
## matters on a restart, but would have broken restart-safety).
already_done <- function(site_id) {
  f <- list.files(EXT_DIR, pattern = paste0("_", site_id, "_FLUXNET_"))
  length(f) > 0
}

download_and_extract_one <- function(site_id, attempt = 1) {
  if (already_done(site_id)) {
    cat(sprintf("[%s] %s: already extracted, skipping\n", Sys.time(), site_id))
    return(TRUE)
  }
  row <- filter(manifest, site_id == !!site_id)
  if (nrow(row) == 0) return(FALSE)
  ok_download <- tryCatch({
    flux_download(file_list_df = row, download_dir = RAW_DIR)
    TRUE
  }, error = function(e) { cat(sprintf("  %s: download error -- %s\n", site_id, conditionMessage(e))); FALSE })
  if (!ok_download) return(FALSE)

  zips <- list.files(RAW_DIR, pattern = paste0("_", site_id, "_FLUXNET_.*\\.zip$"), full.names = TRUE)
  if (length(zips) == 0) { cat(sprintf("  %s: no zip found after download\n", site_id)); return(FALSE) }
  zip_path <- zips[[1]]
  verify_ok <- system2("unzip", c("-t", shQuote(zip_path)), stdout = FALSE, stderr = FALSE) == 0
  if (!verify_ok) {
    cat(sprintf("  %s: zip failed verification (attempt %d)\n", site_id, attempt))
    file_delete(zip_path)
    if (attempt == 1) {
      cat(sprintf("  %s: retrying once\n", site_id))
      return(download_and_extract_one(site_id, attempt = 2))
    } else {
      cat(sprintf("  %s: FAILED after retry\n", site_id))
      return(FALSE)
    }
  }
  utils::unzip(zip_path, exdir = EXT_DIR)
  cat(sprintf("[%s] %s: downloaded, verified, extracted%s\n", Sys.time(), site_id,
              if (attempt > 1) " (after 1 retry)" else ""))
  TRUE
}

results <- character(0)
retried <- character(0)
n_done <- 0
for (site_id in sites) {
  pre_ok <- already_done(site_id)
  ok <- download_and_extract_one(site_id)
  n_done <- n_done + 1
  status <- if (ok) "OK" else "FAILED"
  results[site_id] <- status
  cat(sprintf("[%s] Progress: %d / %d sites processed (%s: %s)\n",
              Sys.time(), n_done, length(sites), site_id, status))
}

res_df <- tibble(site_id = names(results), status = results)
write_csv(res_df, file.path("review/diagnostics/store_audit", "table_stage2_download_status.csv"))
cat(sprintf("[%s] Stage 2 download pass complete. OK: %d  FAILED: %d\n",
            Sys.time(), sum(results == "OK"), sum(results == "FAILED")))
