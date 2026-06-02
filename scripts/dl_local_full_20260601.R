## dl_local_full_20260601.R — Download all 759 sites from the Jun 1 2026
## snapshot to the local machine.
##
## The Codespace copy (672 sites) does not exist locally. This script builds
## a complete local copy. Skips any site already present as a ZIP in
## data/raw/ or as an extracted directory in data/extracted/, so it is safe
## to re-run after interruption.
##
## Uses flux_download() directly (no FLUXNET_SITE_FILTER hack) with the Jun 1
## snapshot as the locked site list.
##
## Does NOT run 03_read / 04_qc / 05_units — run those once all downloads
## and extractions are complete.
##
## Usage:
##   nohup Rscript scripts/dl_local_full_20260601.R \
##     > logs/dl_local_full_20260601.log 2>&1 & disown

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}

source("R/pipeline_config.R")
check_pipeline_config()

library(fluxnet)
library(dplyr)
library(readr)

BATCH_SIZE     <- 50L
SNAPSHOT_PATH  <- "data/snapshots/fluxnet_shuttle_snapshot_20260601T224043.csv"
raw_dir        <- file.path(FLUXNET_DATA_ROOT, "raw")
extracted_dir  <- file.path(FLUXNET_DATA_ROOT, "extracted")
snapshots_dir  <- file.path(FLUXNET_DATA_ROOT, "snapshots")
progress_path  <- file.path(snapshots_dir, "download_progress_local.csv")

disk_free_gb <- function() {
  tryCatch({
    kb <- system(
      paste("df -k", shQuote(FLUXNET_DATA_ROOT), "| awk 'NR==2{print $4}'"),
      intern = TRUE
    )
    round(as.numeric(trimws(kb)) / 1024^2, 2)
  }, error = function(e) NA_real_)
}

cat("=========================================\n")
cat("  dl_local_full_20260601.R\n")
cat("  Snapshot: Jun 1 2026 (759 sites)\n")
cat("  Progress:", progress_path, "\n")
cat("=========================================\n\n")

# ── 1. Load Jun 1 snapshot as the authoritative site list ─────────────────────
manifest <- read_csv(SNAPSHOT_PATH, show_col_types = FALSE)
all_sites <- sort(unique(manifest$site_id))
cat("Snapshot sites:", length(all_sites), "\n")

# ── 2. Identify sites already present locally ─────────────────────────────────
zip_sites <- character(0)
if (dir.exists(raw_dir)) {
  zips <- list.files(raw_dir, pattern = "\\.zip$", full.names = FALSE)
  zip_sites <- unique(regmatches(zips, regexpr("[A-Z]{2}-[A-Za-z0-9]+", zips)))
}

extracted_sites <- character(0)
if (dir.exists(extracted_dir)) {
  dirs <- list.dirs(extracted_dir, full.names = FALSE, recursive = FALSE)
  dirs <- dirs[nzchar(dirs)]
  extracted_sites <- unique(regmatches(dirs, regexpr("[A-Z]{2}-[A-Za-z0-9]+", dirs)))
}

have_locally <- union(zip_sites, extracted_sites)
todo_sites   <- sort(setdiff(all_sites, have_locally))

cat("Already have locally (ZIP or extracted):", length(have_locally), "\n")
cat("Still to download:", length(todo_sites), "\n\n")

if (length(todo_sites) == 0) {
  cat("Nothing to download — all sites already present.\n")
  quit(status = 0)
}

# ── 3. Split remaining sites into batches ─────────────────────────────────────
batches   <- split(todo_sites, ceiling(seq_along(todo_sites) / BATCH_SIZE))
n_batches <- length(batches)
cat("Batch size:", BATCH_SIZE, "| Batches to run:", n_batches, "\n\n")

# ── 4. Load or initialise progress file ──────────────────────────────────────
if (file.exists(progress_path)) {
  progress <- read_csv(progress_path, show_col_types = FALSE,
                       col_types = cols(
                         batch_num    = col_integer(),
                         n_sites      = col_integer(),
                         site_ids     = col_character(),
                         status       = col_character(),
                         started_at   = col_character(),
                         completed_at = col_character(),
                         disk_free_gb = col_double()
                       ))
  n_done <- sum(progress$status == "done", na.rm = TRUE)
  cat("Progress file found:", n_done, "/", n_batches, "batches done.\n\n")
} else {
  progress <- data.frame(
    batch_num    = integer(0),
    n_sites      = integer(0),
    site_ids     = character(0),
    status       = character(0),
    started_at   = character(0),
    completed_at = character(0),
    disk_free_gb = numeric(0),
    stringsAsFactors = FALSE
  )
  cat("No progress file — starting fresh.\n\n")
}

# ── 5. Batch loop ─────────────────────────────────────────────────────────────
for (i in seq_along(batches)) {
  batch_sites <- batches[[i]]
  batch_label <- sprintf("Batch %d/%d (%d sites)", i, n_batches, length(batch_sites))

  if (any(progress$batch_num == i & progress$status == "done")) {
    cat("[SKIP] ", batch_label, " — already completed\n", sep = "")
    next
  }

  cat("\n--- START ", batch_label, " ---\n", sep = "")
  cat("Sites:", paste(batch_sites, collapse = " "), "\n")

  gb_before  <- disk_free_gb()
  started_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  cat("Disk free before:", gb_before, "GB |", format(Sys.time(), "%H:%M:%S"), "\n")

  progress <- dplyr::bind_rows(progress, data.frame(
    batch_num    = i,
    n_sites      = length(batch_sites),
    site_ids     = paste(batch_sites, collapse = ";"),
    status       = "started",
    started_at   = started_at,
    completed_at = NA_character_,
    disk_free_gb = gb_before,
    stringsAsFactors = FALSE
  ))
  write_csv(progress, progress_path)

  # Download directly — pass batch sub-manifest to flux_download()
  batch_manifest <- dplyr::filter(manifest, site_id %in% batch_sites)
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

  # Extract and delete ZIPs
  Sys.setenv(FLUXNET_DELETE_ZIPS = "TRUE")
  tryCatch(
    source("scripts/02_extract.R"),
    error = function(e) {
      cat("ERROR in 02_extract.R —", conditionMessage(e), "\n")
      stop(e)
    }
  )

  gb_after     <- disk_free_gb()
  completed_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  idx <- which(progress$batch_num == i & progress$status == "started")
  progress$status[idx]       <- "done"
  progress$completed_at[idx] <- completed_at
  progress$disk_free_gb[idx] <- gb_after
  write_csv(progress, progress_path)

  cat(
    "--- DONE ", batch_label,
    " | disk free: ", gb_after, " GB",
    " (used ~", round(gb_before - gb_after, 1), " GB net) |",
    format(Sys.time(), "%H:%M:%S"), "---\n",
    sep = ""
  )
}

n_done <- sum(progress$status == "done", na.rm = TRUE)
cat(
  "\n=========================================\n",
  " Download complete: ", n_done, " batches done.\n",
  " Progress log: ", progress_path, "\n",
  " Next: run scripts 03_read, 04_qc, 05_units\n",
  "=========================================\n",
  sep = ""
)
