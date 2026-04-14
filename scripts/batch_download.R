## batch_download.R — Download all Shuttle sites in batches of 50, with ZIP
## cleanup after each batch. Resumes automatically if interrupted. Runs the
## full read → QC → units pipeline once at the end on all accumulated data.
##
## Usage (set FLUXNET_EXTRACT_RESOLUTIONS before running):
##   FLUXNET_EXTRACT_RESOLUTIONS="y m" Rscript scripts/batch_download.R
##
## Resume an interrupted run by simply running the script again — completed
## batches are skipped based on data/snapshots/download_progress.csv.
##
## FLUXNET_DELETE_ZIPS=TRUE (default) keeps peak disk usage per batch low:
## raw ZIPs (~750 MB for 50 sites) are deleted after extraction, leaving only
## the extracted CSVs (~25 MB for y+m only).

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}

# Point reticulate at the project virtualenv before any fluxnet calls.
Sys.setenv(
  RETICULATE_PYTHON = path.expand("~/.virtualenvs/fluxnet/bin/python")
)

source("R/pipeline_config.R")
check_pipeline_config()

library(fluxnet)
library(dplyr)
library(readr)

BATCH_SIZE    <- 50L
snapshots_dir <- file.path(FLUXNET_DATA_ROOT, "snapshots")
progress_path <- file.path(snapshots_dir, "download_progress.csv")

# ── Helper: free GB on /workspaces ────────────────────────────────────────────
disk_free_gb <- function() {
  tryCatch({
    kb <- system("df -k /workspaces | awk 'NR==2{print $4}'", intern = TRUE)
    round(as.numeric(trimws(kb)) / 1024^2, 2)
  }, error = function(e) NA_real_)
}

cat("=========================================\n")
cat("  batch_download.R\n")
cat("  Extract resolutions:", paste(FLUXNET_EXTRACT_RESOLUTIONS, collapse = " "), "\n")
cat("  Delete ZIPs:        ", FLUXNET_DELETE_ZIPS, "\n")
cat("  Data root:          ", FLUXNET_DATA_ROOT, "\n")
cat("=========================================\n\n")

# ── 1. Full site list from Shuttle ────────────────────────────────────────────
cat("Fetching site list via flux_listall()...\n")
manifest  <- flux_listall()
all_sites <- sort(unique(manifest$site_id))
cat("Total sites available:", length(all_sites), "\n\n")

# ── 2. Split into batches ─────────────────────────────────────────────────────
batches   <- split(all_sites, ceiling(seq_along(all_sites) / BATCH_SIZE))
n_batches <- length(batches)
cat("Batch size:", BATCH_SIZE, "| Total batches:", n_batches, "\n")

# ── 3. Load or initialise progress file ──────────────────────────────────────
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
  n_done   <- sum(progress$status == "done", na.rm = TRUE)
  cat("Progress file found:", progress_path, "\n")
  cat("  Completed batches:", n_done, "/", n_batches, "\n\n")
} else {
  progress <- data.frame(
    batch_num    = integer(0),
    n_sites      = integer(0),
    site_ids     = character(0),   # semicolon-separated
    status       = character(0),   # "started" | "done"
    started_at   = character(0),
    completed_at = character(0),
    disk_free_gb = numeric(0),
    stringsAsFactors = FALSE
  )
  cat("No progress file — starting fresh.\n\n")
}

# ── 4. Batch loop ─────────────────────────────────────────────────────────────
for (i in seq_along(batches)) {
  batch_sites <- batches[[i]]
  batch_label <- sprintf("Batch %d/%d (%d sites)", i, n_batches, length(batch_sites))

  # Skip if already completed
  if (any(progress$batch_num == i & progress$status == "done")) {
    cat("[SKIP] ", batch_label, " — already completed\n", sep = "")
    next
  }

  cat("\n--- START ", batch_label, " ---\n", sep = "")
  cat("Sites:", paste(batch_sites, collapse = " "), "\n")

  gb_before  <- disk_free_gb()
  started_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  cat("Disk free before:", gb_before, "GB\n")

  # Record batch as started so a crash mid-batch is visible in progress file
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

  # Set site filter for this batch
  Sys.setenv(
    FLUXNET_SITE_FILTER = paste(batch_sites, collapse = " "),
    FLUXNET_DELETE_ZIPS = "TRUE"
  )

  # Download this batch
  tryCatch(
    source("scripts/01_download.R"),
    error = function(e) {
      cat("ERROR in 01_download.R —", conditionMessage(e), "\n")
      stop(e)
    }
  )

  # Extract and delete ZIPs
  tryCatch(
    source("scripts/02_extract.R"),
    error = function(e) {
      cat("ERROR in 02_extract.R —", conditionMessage(e), "\n")
      stop(e)
    }
  )

  gb_after     <- disk_free_gb()
  completed_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  # Mark batch done
  idx <- which(progress$batch_num == i & progress$status == "started")
  progress$status[idx]       <- "done"
  progress$completed_at[idx] <- completed_at
  progress$disk_free_gb[idx] <- gb_after
  write_csv(progress, progress_path)

  cat(
    "--- DONE ", batch_label,
    " | disk free: ", gb_after, " GB",
    " (used ", round(gb_before - gb_after, 2), " GB net) ---\n",
    sep = ""
  )
}

# Restore — clear site filter so downstream steps process all extracted data
Sys.setenv(FLUXNET_SITE_FILTER = "")

# ── 5. Post-download pipeline ─────────────────────────────────────────────────
n_done <- sum(progress$status == "done", na.rm = TRUE)
cat(
  "\n=========================================\n",
  " All ", n_done, "/", n_batches, " batches complete.\n",
  " Running 03_read → 04_qc → 05_units...\n",
  "=========================================\n",
  sep = ""
)

source("scripts/03_read.R")
source("scripts/04_qc.R")
source("scripts/05_units.R")

cat("\n=========================================\n")
cat(" batch_download.R complete.\n")
cat(" Progress log:", progress_path, "\n")
df_out <- disk_free_gb()
cat(" Final disk free:", df_out, "GB\n")
cat("=========================================\n")
