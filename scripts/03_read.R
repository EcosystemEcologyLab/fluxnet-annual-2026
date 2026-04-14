## 03_read.R — Read flux data, variable metadata, and BADM
## Reads ERA5 + FLUXMET CSVs directly, one site at a time, for each resolution
## in FLUXNET_EXTRACT_RESOLUTIONS. Saves a resumable partial every 50 sites;
## renames to flux_data_raw_<res>.rds on completion.

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

# ── Per-site reading helper ────────────────────────────────────────────────────
# Applies the same timestamp conversion and -9999 → NA replacement that
# flux_read() does, without loading all files into memory simultaneously.
read_site_flux <- function(inv_rows, inv_res) {
  ts_col    <- switch(inv_res,
    YY = "TIMESTAMP",
    MM = "TIMESTAMP",
    WW = c("TIMESTAMP_START", "TIMESTAMP_END"),
    DD = "TIMESTAMP",
    HH = c("TIMESTAMP_START", "TIMESTAMP_END"),
    HR = c("TIMESTAMP_START", "TIMESTAMP_END")
  )
  ts_rename <- switch(inv_res,
    YY = "YEAR", MM = "DATE", WW = "DATE", DD = "DATE",
    HH = "DATETIME", HR = "DATETIME"
  )
  ts_fun <- switch(inv_res,
    YY = as.integer,
    MM = lubridate::ym,
    WW = lubridate::ymd,
    DD = lubridate::ymd,
    HH = lubridate::ymd_hm,
    HR = lubridate::ymd_hm
  )

  rows <- lapply(seq_len(nrow(inv_rows)), function(i) {
    tryCatch(
      readr::read_csv(inv_rows$path[i], show_col_types = FALSE) |>
        dplyr::mutate(site_id = inv_rows$site_id[i],
                      dataset  = inv_rows$dataset[i], .before = 1),
      error = function(e) {
        warning("Skipping unreadable file: ", inv_rows$path[i],
                "\n  ", conditionMessage(e))
        NULL
      }
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) return(NULL)

  dplyr::bind_rows(rows) |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(ts_col), ts_fun),
      dplyr::across(dplyr::where(is.numeric), \(x) dplyr::na_if(x, -9999))
    ) |>
    dplyr::rename_with(\(col) gsub("TIMESTAMP", ts_rename, col, fixed = TRUE))
}

# ── Resolution loop ────────────────────────────────────────────────────────────
# Map extract resolution codes to time_resolution labels in file_inventory.
res_to_inv <- c(y = "YY", m = "MM", w = "WW", d = "DD", h = "HH")

for (res_code in FLUXNET_EXTRACT_RESOLUTIONS) {
  inv_res      <- res_to_inv[[res_code]]
  suffix       <- tolower(inv_res)
  out_path     <- file.path(processed_dir, paste0("flux_data_raw_", suffix, ".rds"))
  partial_path <- file.path(processed_dir, paste0("flux_data_raw_", suffix, "_partial.rds"))
  sites_path   <- file.path(processed_dir, paste0("flux_data_raw_", suffix, "_done_sites.rds"))

  if (file.exists(out_path)) {
    message("Resolution '", res_code, "' (", inv_res, ") already complete — skipping: ",
            out_path); flush(stderr())
    next
  }

  # Filter inventory: ERA5 + FLUXMET, correct resolution, file exists on disk.
  res_rows <- if (res_code == "h") {
    file_inventory$time_resolution %in% c("HH", "HR")
  } else {
    !is.na(file_inventory$time_resolution) & file_inventory$time_resolution == inv_res
  }
  inv_flux <- file_inventory[
    res_rows &
    !is.na(file_inventory$dataset) & file_inventory$dataset %in% c("ERA5", "FLUXMET") &
    !is.na(file_inventory$path) & nchar(file_inventory$path) > 0 &
    file.exists(file_inventory$path),
  , drop = FALSE]

  all_sites <- unique(inv_flux$site_id)
  if (length(all_sites) == 0L) {
    message("No ERA5/FLUXMET files for resolution '", res_code, "' — skipping."); flush(stderr())
    next
  }

  # Resumable: reload partial progress if it exists.
  done_sites <- if (file.exists(sites_path)) readRDS(sites_path) else character(0)
  accum      <- if (file.exists(partial_path) && length(done_sites) > 0L) {
    message("Resuming from partial: ", length(done_sites), " sites already done."); flush(stderr())
    list(readRDS(partial_path))
  } else {
    list()
  }

  todo_sites <- setdiff(all_sites, done_sites)
  message("Reading resolution: ", res_code, " (", inv_res, ") — ",
          length(all_sites), " site(s) total, ", length(done_sites), " done, ",
          length(todo_sites), " to process"); flush(stderr())

  for (i in seq_along(todo_sites)) {
    site     <- todo_sites[i]
    site_inv <- inv_flux[inv_flux$site_id == site, , drop = FALSE]
    site_dat <- read_site_flux(site_inv, inv_res)

    if (!is.null(site_dat) && nrow(site_dat) > 0L) {
      accum[[length(accum) + 1L]] <- site_dat
    }
    done_sites <- c(done_sites, site)

    # Save progress and collapse accumulator every 50 sites.
    if (i %% 50L == 0L || i == length(todo_sites)) {
      message("  Progress: ", length(done_sites), "/", length(all_sites),
              " sites (", site, ")"); flush(stderr())
      combined <- dplyr::bind_rows(accum)
      saveRDS(combined,    partial_path)
      saveRDS(done_sites,  sites_path)
      accum <- list(combined)   # collapse to single df to limit memory growth
      rm(combined)
    }
  }

  message("Finalising ", inv_res, " (", nrow(dplyr::bind_rows(accum)), " rows)..."); flush(stderr())
  flux_data <- dplyr::bind_rows(accum)
  attr(flux_data, "flux_resolution") <- inv_res

  message("Writing ", out_path, "..."); flush(stderr())
  saveRDS(flux_data, out_path)

  # Remove partial progress files now that the final output is saved.
  if (file.exists(partial_path)) file.remove(partial_path)
  if (file.exists(sites_path))   file.remove(sites_path)
  message("Saved: ", out_path,
          " (", nrow(flux_data), " rows \u00d7 ", ncol(flux_data), " cols)"); flush(stderr())
}
