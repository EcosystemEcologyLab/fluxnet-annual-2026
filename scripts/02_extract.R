## 02_extract.R — Extract downloaded archives, discover constituent files, and
## optionally delete ZIP archives after successful extraction.
##
## New: FLUXNET_DELETE_ZIPS (default TRUE) — immediately after flux_extract()
## completes, each ZIP in data/raw/ is deleted if at least one file was
## successfully extracted from it. ZIPs with no matching extracted files are
## kept and a warning is issued. This reduces peak disk usage during batch
## downloads without risking data loss.

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}

source("R/pipeline_config.R")
source("R/credentials.R")
check_pipeline_config()

library(fluxnet)

zip_dir       <- file.path(FLUXNET_DATA_ROOT, "raw")
extracted_dir <- file.path(FLUXNET_DATA_ROOT, "extracted")

extract_site_ids <- if (length(FLUXNET_SITE_FILTER) > 0) FLUXNET_SITE_FILTER else NULL

flux_extract(
  zip_dir     = zip_dir,
  output_dir  = extracted_dir,
  site_ids    = extract_site_ids,
  resolutions = FLUXNET_EXTRACT_RESOLUTIONS
)

# Discover all extracted files and save inventory to processed/.
dir.create(file.path(FLUXNET_DATA_ROOT, "processed"), recursive = TRUE,
           showWarnings = FALSE)
file_inventory <- flux_discover_files(extracted_dir)
saveRDS(file_inventory,
        file.path(FLUXNET_DATA_ROOT, "processed", "file_inventory.rds"))

# ── ZIP cleanup ───────────────────────────────────────────────────────────────
# Delete each ZIP that had at least one file successfully extracted from it.
# Safety guarantee: a ZIP is only deleted when its site ID appears in the
# file inventory with at least one path that exists on disk.
if (FLUXNET_DELETE_ZIPS) {
  if (!dir.exists(zip_dir)) {
    message("[ZIP cleanup] ", zip_dir, " does not exist — nothing to delete.")
  } else {
    zip_files <- list.files(zip_dir, pattern = "\\.zip$", full.names = TRUE)

    if (length(zip_files) == 0) {
      message("[ZIP cleanup] No ZIP files found in: ", zip_dir)
    } else {
      # Site IDs with at least one extracted file on disk
      extracted_sites <- unique(
        file_inventory$site_id[
          !is.na(file_inventory$site_id) &
          !is.na(file_inventory$path)    &
          nchar(file_inventory$path) > 0 &
          file.exists(file_inventory$path)
        ]
      )

      n_deleted  <- 0L
      n_kept     <- 0L
      mb_deleted <- 0

      for (zip_path in zip_files) {
        fname <- basename(zip_path)

        # FLUXNET/AmeriFlux ZIP filenames embed the site ID as a CC-XXX token
        # (two uppercase letters, a hyphen, alphanumerics). Extract the first match.
        site_match <- regmatches(fname, regexpr("[A-Z]{2}-[A-Za-z0-9]+", fname))

        if (length(site_match) == 0 || nchar(site_match) == 0) {
          message("  [ZIP cleanup] Cannot parse site ID from '", fname,
                  "' — keeping.")
          n_kept <- n_kept + 1L
          next
        }

        site_id_zip <- site_match[[1]]

        if (site_id_zip %in% extracted_sites) {
          zip_mb      <- round(file.size(zip_path) / 1024^2, 1)
          mb_deleted  <- mb_deleted + zip_mb
          file.remove(zip_path)
          n_deleted   <- n_deleted + 1L
          message("  [ZIP cleanup] Deleted  ", fname,
                  "  (", zip_mb, " MB)  — ", site_id_zip, " extracted OK")
        } else {
          message("  [ZIP cleanup] KEPT     ", fname,
                  "  — no extracted files confirmed for ", site_id_zip)
          n_kept <- n_kept + 1L
        }
      }

      message(
        "[ZIP cleanup] Complete: ", n_deleted, " deleted (",
        round(mb_deleted, 1), " MB freed), ", n_kept, " kept."
      )
    }
  }
} else {
  message(
    "[ZIP cleanup] FLUXNET_DELETE_ZIPS=FALSE — ZIPs retained in: ", zip_dir
  )
}
