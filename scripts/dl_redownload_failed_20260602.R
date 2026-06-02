## dl_redownload_failed_20260602.R — Re-download the 6 sites whose ZIPs were
## corrupt (truncated downloads) from the Jun 1 2026 run.
##
## Sites: US-ARM, US-Aud, US-Bar, US-Bi1, US-Bi2, US-BZB
##
## Deletes the corrupt ZIPs from data/raw/ first, then re-downloads and
## extracts. Safe to re-run; if extraction already succeeded, the ZIP will
## be gone and nothing will happen.
##
## Usage:
##   nohup Rscript scripts/dl_redownload_failed_20260602.R \
##     > logs/dl_redownload_failed_20260602.log 2>&1 & disown

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}

source("R/pipeline_config.R")
check_pipeline_config()

library(fluxnet)
library(dplyr)
library(readr)

SNAPSHOT_PATH <- "data/snapshots/fluxnet_shuttle_snapshot_20260601T224043.csv"
raw_dir       <- file.path(FLUXNET_DATA_ROOT, "raw")
FAILED_SITES  <- c("US-ARM", "US-Aud", "US-Bar", "US-Bi1", "US-Bi2", "US-BZB")

cat("=========================================\n")
cat("  dl_redownload_failed_20260602.R\n")
cat("  Sites:", paste(FAILED_SITES, collapse = ", "), "\n")
cat("=========================================\n\n")

# ── 1. Delete the corrupt ZIPs ────────────────────────────────────────────────
cat("Removing corrupt ZIPs from", raw_dir, "...\n")
existing_zips <- list.files(raw_dir, pattern = "\\.zip$", full.names = TRUE)
for (z in existing_zips) {
  site <- regmatches(basename(z), regexpr("[A-Z]{2}-[A-Za-z0-9]+", basename(z)))
  if (site %in% FAILED_SITES) {
    cat("  Deleting corrupt ZIP:", basename(z), "\n")
    file.remove(z)
  }
}

# ── 2. Load snapshot and filter to the 6 failed sites ────────────────────────
manifest      <- read_csv(SNAPSHOT_PATH, show_col_types = FALSE)
batch_manifest <- dplyr::filter(manifest, site_id %in% FAILED_SITES)
cat("\nSites to re-download:", nrow(dplyr::distinct(batch_manifest, site_id)), "\n\n")

# ── 3. Download ───────────────────────────────────────────────────────────────
cat("--- Downloading ---\n")
tryCatch(
  flux_download(
    file_list_df = batch_manifest,
    download_dir = raw_dir
  ),
  error = function(e) {
    cat("ERROR in flux_download() —", conditionMessage(e), "\n")
    stop(e)
  }
)

# ── 4. Verify ZIPs before extracting ─────────────────────────────────────────
cat("\n--- Verifying downloaded ZIPs ---\n")
new_zips <- list.files(raw_dir, pattern = "\\.zip$", full.names = TRUE)
ok <- logical(length(new_zips))
for (j in seq_along(new_zips)) {
  result <- system2("unzip", c("-t", shQuote(new_zips[j])), stdout = FALSE, stderr = FALSE)
  ok[j] <- (result == 0)
  cat(sprintf("  %s: %s\n", basename(new_zips[j]), if (ok[j]) "OK" else "STILL CORRUPT"))
}

n_ok <- sum(ok)
cat(sprintf("\n%d / %d ZIPs verified OK.\n\n", n_ok, length(new_zips)))

if (n_ok == 0) {
  cat("No valid ZIPs to extract — stopping.\n")
  quit(status = 1)
}

# ── 5. Extract and clean up ZIPs ─────────────────────────────────────────────
cat("--- Extracting ---\n")
Sys.setenv(FLUXNET_DELETE_ZIPS = "TRUE")
tryCatch(
  source("scripts/02_extract.R"),
  error = function(e) {
    cat("ERROR in 02_extract.R —", conditionMessage(e), "\n")
    stop(e)
  }
)

cat("\n=========================================\n")
cat(" Re-download complete.\n")
cat(" Check data/extracted/ for the 6 sites.\n")
cat("=========================================\n")
