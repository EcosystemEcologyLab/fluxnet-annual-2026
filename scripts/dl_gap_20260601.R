## dl_gap_20260601.R — Download the 87 sites absent from the April batch job.
##
## Targets sites present in the Jun 1 2026 live manifest but not yet extracted
## locally. Uses the same FLUXNET_SITE_FILTER + 01_download.R + 02_extract.R
## pattern as batch_download.R, with a separate progress file so the April
## run record is preserved.
##
## Does NOT run 03_read / 04_qc / 05_units — run those manually once all
## downloads (April batch + this gap fill) are complete.
##
## Usage:
##   nohup Rscript scripts/dl_gap_20260601.R \
##     > logs/dl_gap_20260601.log 2>&1 & disown

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}

source("R/pipeline_config.R")
check_pipeline_config()

library(fluxnet)
library(dplyr)
library(readr)

BATCH_SIZE    <- 50L
snapshots_dir <- file.path(FLUXNET_DATA_ROOT, "snapshots")
progress_path <- file.path(snapshots_dir, "download_progress_gap_20260601.csv")

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
cat("  dl_gap_20260601.R\n")
cat("  Snapshot:   Jun 1 2026 (759 sites)\n")
cat("  Target:     sites not yet extracted\n")
cat("  Progress:  ", progress_path, "\n")
cat("=========================================\n\n")

# ── 1. Live manifest ──────────────────────────────────────────────────────────
cat("Fetching live manifest via flux_listall()...\n")
manifest  <- flux_listall()
all_sites <- sort(unique(manifest$site_id))
cat("Live manifest:", length(all_sites), "sites\n\n")

# ── 2. Identify sites not yet extracted ──────────────────────────────────────
extracted_dir <- file.path(FLUXNET_DATA_ROOT, "extracted")
already_extracted <- character(0)
if (dir.exists(extracted_dir)) {
  dirs <- list.dirs(extracted_dir, full.names = FALSE, recursive = FALSE)
  already_extracted <- unique(regmatches(dirs, regexpr("[A-Z]{2}-[A-Za-z0-9]+", dirs)))
}
gap_sites <- sort(setdiff(all_sites, already_extracted))
cat("Already extracted:", length(already_extracted), "sites\n")
cat("Gap sites to download:", length(gap_sites), "\n")
cat(gap_sites, fill = TRUE)
cat("\n")

if (length(gap_sites) == 0) {
  cat("No gap sites — nothing to do.\n")
  quit(status = 0)
}

# ── 3. Split into batches ─────────────────────────────────────────────────────
batches   <- split(gap_sites, ceiling(seq_along(gap_sites) / BATCH_SIZE))
n_batches <- length(batches)
cat("Batch size:", BATCH_SIZE, "| Total batches:", n_batches, "\n\n")

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
  cat("Progress file found:", progress_path, "\n")
  cat("  Completed batches:", n_done, "/", n_batches, "\n\n")
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
  cat("Disk free before:", gb_before, "GB\n")

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

  Sys.setenv(
    FLUXNET_SITE_FILTER = paste(batch_sites, collapse = " "),
    FLUXNET_DELETE_ZIPS = "TRUE"
  )

  tryCatch(
    source("scripts/01_download.R"),
    error = function(e) {
      cat("ERROR in 01_download.R —", conditionMessage(e), "\n")
      stop(e)
    }
  )

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
    " (used ", round(gb_before - gb_after, 2), " GB net) ---\n",
    sep = ""
  )
}

Sys.setenv(FLUXNET_SITE_FILTER = "")

n_done <- sum(progress$status == "done", na.rm = TRUE)
cat(
  "\n=========================================\n",
  " Gap fill complete: ", n_done, "/", n_batches, " batches done.\n",
  " Progress log: ", progress_path, "\n",
  " Next step: run scripts 03_read, 04_qc, 05_units\n",
  " across the full extracted dataset.\n",
  "=========================================\n",
  sep = ""
)
