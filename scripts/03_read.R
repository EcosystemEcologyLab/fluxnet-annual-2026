## 03_read.R — Read flux data, variable metadata, and BADM
## Uses: flux_read(), flux_varinfo(), flux_badm()
## Loops over FLUXNET_EXTRACT_RESOLUTIONS; saves one flux_data_raw_<res>.rds
## per resolution (e.g. flux_data_raw_yy.rds, flux_data_raw_mm.rds, ...).

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}

source("R/pipeline_config.R")
check_pipeline_config()

library(fluxnet)

processed_dir <- file.path(FLUXNET_DATA_ROOT, "processed")

file_inventory <- readRDS(file.path(processed_dir, "file_inventory.rds"))
message("Inventory loaded: ", nrow(file_inventory), " rows"); flush(stderr())

# ── BIF file normalisation ─────────────────────────────────────────────────────
# flux_badm() requires exactly 5 columns in every BIF file. Some data providers
# include extra columns (e.g. AU-Dry ships a spurious empty 'SITDryD' column).
# Pre-scan all BIF files and strip any non-standard columns in-place, logging
# each affected file as a warning. Files missing required columns are removed
# from the inventory and logged as unknowns so flux_badm() never sees them.
BIF_STANDARD_COLS <- c("SITE_ID", "GROUP_ID", "VARIABLE_GROUP", "VARIABLE", "DATAVALUE")

bif_inventory_rows <- which(
  !is.na(file_inventory$dataset) & file_inventory$dataset == "BIF" &
  !is.na(file_inventory$path) & nchar(file_inventory$path) > 0 &
  file.exists(file_inventory$path)
)

drop_bif_rows <- integer(0)

for (row_i in bif_inventory_rows) {
  bif_path <- file_inventory$path[row_i]
  cols     <- names(readr::read_csv(bif_path, n_max = 0L, show_col_types = FALSE))
  extra    <- setdiff(cols, BIF_STANDARD_COLS)
  missing  <- setdiff(BIF_STANDARD_COLS, cols)

  if (length(missing) > 0) {
    warning("BIF file missing required column(s) [", paste(missing, collapse = ", "),
            "] — excluding from BADM: ", bif_path)
    drop_bif_rows <- c(drop_bif_rows, row_i)
    next
  }

  if (length(extra) > 0) {
    warning("BIF file has extra column(s) [", paste(extra, collapse = ", "),
            "] — stripping and rewriting: ", bif_path)
    bif_data <- readr::read_csv(bif_path, show_col_types = FALSE)
    bif_data <- bif_data[, BIF_STANDARD_COLS]
    readr::write_csv(bif_data, bif_path)
  }
}

if (length(drop_bif_rows) > 0) {
  message(length(drop_bif_rows), " BIF file(s) with missing columns excluded from inventory.")
  file_inventory <- file_inventory[-drop_bif_rows, , drop = FALSE]
}
message("BIF normalisation complete (", length(bif_inventory_rows), " files checked, ",
        length(drop_bif_rows), " dropped)"); flush(stderr())

# BADM — read directly from the normalised BIF CSV files on disk (skip if cached).
# flux_badm() calls quit() and terminates the R session when run from a script;
# see docs/known_issues.md. Reading the CSVs directly is equivalent and avoids
# the 15–20 minute API call entirely.
badm_path    <- file.path(processed_dir, "badm.rds")
varinfo_path <- file.path(processed_dir, "var_info.rds")

if (file.exists(badm_path) && file.exists(varinfo_path)) {
  message("Cached badm.rds and var_info.rds found — skipping recomputation."); flush(stderr())
  badm     <- readRDS(badm_path)
  var_info <- readRDS(varinfo_path)
  message("Loaded badm: ", nrow(badm), " rows; var_info: ", nrow(var_info), " rows"); flush(stderr())
} else {
  bif_paths_ok <- file_inventory$path[
    !is.na(file_inventory$dataset) & file_inventory$dataset == "BIF" &
    !is.na(file_inventory$path) & nchar(file_inventory$path) > 0 &
    file.exists(file_inventory$path)
  ]
  message("Reading BADM from ", length(bif_paths_ok), " BIF files..."); flush(stderr())
  badm <- dplyr::bind_rows(lapply(bif_paths_ok, function(p) {
    readr::read_csv(p, show_col_types = FALSE, col_types = readr::cols(.default = "c"))
  }))
  message("BADM complete: ", nrow(badm), " rows"); flush(stderr())

  # Variable metadata is resolution-agnostic — read once and share across outputs.
  message("Calling flux_varinfo()..."); flush(stderr())
  var_info <- flux_varinfo(file_inventory)
  message("flux_varinfo() complete: ", nrow(var_info), " rows"); flush(stderr())
  message("Saving badm.rds and var_info.rds..."); flush(stderr())
  saveRDS(badm,     badm_path)
  saveRDS(var_info, varinfo_path)
  message("Saved badm.rds and var_info.rds"); flush(stderr())
}

# Map extract resolution codes (flux_extract / FLUXNET_EXTRACT_RESOLUTIONS) to
# the time_resolution labels used by flux_discover_files() in file_inventory.
res_to_inv <- c(y = "YY", m = "MM", w = "WW", d = "DD", h = "HH")

for (res_code in FLUXNET_EXTRACT_RESOLUTIONS) {
  inv_res <- res_to_inv[[res_code]]
  suffix  <- tolower(inv_res)   # "yy", "mm", "dd", etc.

  # HH resolution may appear as "HH" or "HR" depending on inventory version.
  if (res_code == "h") {
    rows <- file_inventory$time_resolution %in% c("HH", "HR")
  } else {
    rows <- !is.na(file_inventory$time_resolution) &
            file_inventory$time_resolution == inv_res
  }

  # Also require files to exist on disk — inventory rows without extracted
  # files have empty or non-existent paths and cannot be read.
  inv_subset <- file_inventory[
    rows & !is.na(file_inventory$path) &
    nchar(file_inventory$path) > 0 &
    file.exists(file_inventory$path),
    , drop = FALSE]

  if (nrow(inv_subset) == 0) {
    message("No extracted files on disk for resolution '", res_code,
            "' (", inv_res, ") — skipping.")
    next
  }

  out_path <- file.path(processed_dir, paste0("flux_data_raw_", suffix, ".rds"))
  if (file.exists(out_path)) {
    message("Resolution '", res_code, "' (", inv_res, ") already complete — skipping: ",
            out_path)
    next
  }

  # Read in batches so purrr::list_rbind() consolidation never runs silently
  # for more than a few seconds at a stretch. Without batching, list_rbind()
  # over 1344 files takes 25+ seconds of silence, triggering the inactivity
  # timeout in the calling shell.
  batch_size <- 100L
  n_rows     <- nrow(inv_subset)
  n_batches  <- ceiling(n_rows / batch_size)
  message("Reading resolution: ", res_code, " (", inv_res, ") — ", n_rows,
          " file(s) in ", n_batches, " batch(es) of ~", batch_size); flush(stderr())

  batch_idx  <- split(seq_len(n_rows), ceiling(seq_len(n_rows) / batch_size))
  batch_list <- vector("list", n_batches)
  for (b in seq_along(batch_idx)) {
    message("  Batch ", b, "/", n_batches, " (rows ",
            min(batch_idx[[b]]), "\u2013", max(batch_idx[[b]]), ")..."); flush(stderr())
    batch_list[[b]] <- flux_read(inv_subset[batch_idx[[b]], , drop = FALSE],
                                 resolution = res_code)
  }
  message("Combining ", n_batches, " batch(es)..."); flush(stderr())
  flux_data <- dplyr::bind_rows(batch_list)

  message("Writing ", out_path, "..."); flush(stderr())
  saveRDS(flux_data, out_path)
  message("Saved: ", out_path); flush(stderr())
}
