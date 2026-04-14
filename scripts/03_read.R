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

# BADM and variable metadata are resolution-agnostic — read once outside the
# loop and share across all resolution outputs.
bif_paths  <- file_inventory$path[file_inventory$dataset == "BIF"]
bif_groups <- unique(unlist(lapply(bif_paths[file.exists(bif_paths)], function(p) {
  readr::read_csv(p, show_col_types = FALSE)$VARIABLE_GROUP
})))
badm     <- flux_badm(file_inventory, variable_group = tolower(bif_groups))
var_info <- flux_varinfo(file_inventory)
saveRDS(badm,     file.path(processed_dir, "badm.rds"))
saveRDS(var_info, file.path(processed_dir, "var_info.rds"))

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

  message("Reading resolution: ", res_code, " (", inv_res, ") — ",
          nrow(inv_subset), " file(s)")

  flux_data <- flux_read(inv_subset, resolution = res_code)

  out_path <- file.path(processed_dir, paste0("flux_data_raw_", suffix, ".rds"))
  saveRDS(flux_data, out_path)
  message("Saved: ", out_path)
}
