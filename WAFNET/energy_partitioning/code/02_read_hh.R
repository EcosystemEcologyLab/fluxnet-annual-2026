## 02_read_hh.R — Read half-hourly FLUXMET data for the six WAFNET sites into
## per-site RDS files plus one combined RDS, so downstream scripts never need
## to re-parse the raw CSVs.
##
## Mirrors the read/NA-handling convention in the Annual Paper's
## scripts/03_read.R (TIMESTAMP_START/END parsing via lubridate::ymd_hm,
## -9999 -> NA for numeric columns) as a standalone copy for this analysis --
## that file is never sourced or modified.
##
## Output (all under WAFNET/energy_partitioning/data/processed/, gitignored):
##   hh/<site_id>.rds       one data frame per site
##   flux_hh_all_sites.rds  row-bound combination of all sites read so far

source("WAFNET/energy_partitioning/code/00_config.R")

processed_dir  <- file.path(WAFNET_ROOT, "data", "processed")
inventory_path <- file.path(processed_dir, "file_inventory_hh.rds")

if (!file.exists(inventory_path)) {
  stop(
    "[WAFNET] ", inventory_path, " not found -- run ",
    "01_download_extract_hh.R first."
  )
}
file_inventory <- readRDS(inventory_path)

inv_flux <- file_inventory[
  !is.na(file_inventory$time_resolution) &
    file_inventory$time_resolution %in% c("HH", "HR") &
    !is.na(file_inventory$dataset) & file_inventory$dataset == "FLUXMET" &
    !is.na(file_inventory$path) & nchar(file_inventory$path) > 0 &
    file.exists(file_inventory$path),
  ,
  drop = FALSE
]

if (nrow(inv_flux) == 0L) {
  stop(
    "[WAFNET] No HH/HR FLUXMET files found in the inventory -- check ",
    "01_download_extract_hh.R output before re-running this script."
  )
}

# Functions combining sites across resolutions must stop on HH/HR mismatch
# (CLAUDE.md, Temporal Resolution section). Report loudly if this analysis
# ends up with a mix rather than silently combining them.
mixed_res <- length(unique(inv_flux$time_resolution)) > 1L
if (mixed_res) {
  stop(
    "[WAFNET] Mixed HH/HR resolution across sites -- aggregation from HH to ",
    "HR is an explicit step, never automatic (CLAUDE.md). Resolutions found: ",
    paste(unique(inv_flux$time_resolution), collapse = ", "),
    ". Resolve per-site before combining."
  )
}

hh_dir <- file.path(processed_dir, "hh")
dir.create(hh_dir, recursive = TRUE, showWarnings = FALSE)

read_one_site <- function(site_id, rows) {
  ts_col <- c("TIMESTAMP_START", "TIMESTAMP_END")
  dat <- dplyr::bind_rows(lapply(rows$path, function(p) {
    tryCatch(
      readr::read_csv(p, show_col_types = FALSE),
      error = function(e) {
        warning("[WAFNET] Skipping unreadable file: ", p, "\n  ", conditionMessage(e))
        NULL
      }
    )
  }))
  if (nrow(dat) == 0L) return(NULL)
  dat |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(ts_col), lubridate::ymd_hm),
      dplyr::across(dplyr::where(is.numeric), \(x) dplyr::na_if(x, -9999))
    ) |>
    dplyr::rename(
      DATETIME_START = TIMESTAMP_START,
      DATETIME_END   = TIMESTAMP_END
    ) |>
    dplyr::mutate(site_id = site_id, .before = 1)
}

for (site in WAFNET_SITES) {
  out_path <- file.path(hh_dir, paste0(site, ".rds"))
  if (file.exists(out_path)) {
    message("[WAFNET] ", site, ": already read -- skipping (", out_path, ")")
    next
  }
  site_rows <- inv_flux[inv_flux$site_id == site, , drop = FALSE]
  if (nrow(site_rows) == 0L) {
    warning("[WAFNET] ", site, ": no HH FLUXMET file in inventory -- ",
            "no half-hourly data available for this site.")
    next
  }
  message("[WAFNET] Reading ", site, " (", nrow(site_rows), " file(s))...")
  site_dat <- read_one_site(site, site_rows)
  if (is.null(site_dat)) {
    warning("[WAFNET] ", site, ": all files unreadable -- see warnings above.")
    next
  }
  saveRDS(site_dat, out_path)
  message("[WAFNET]   -> ", out_path, " (", nrow(site_dat), " rows x ",
          ncol(site_dat), " cols)")
}

combined <- dplyr::bind_rows(lapply(WAFNET_SITES, function(s) {
  p <- file.path(hh_dir, paste0(s, ".rds"))
  if (file.exists(p)) readRDS(p) else NULL
}))

read_sites    <- unique(combined$site_id)
unread_sites  <- setdiff(WAFNET_SITES, read_sites)
if (length(unread_sites) > 0L) {
  warning(
    "[WAFNET] No half-hourly data read for: ", paste(unread_sites, collapse = ", "),
    " -- these sites must be flagged as data-unavailable in the report-back, ",
    "not silently dropped from later steps."
  )
}

saveRDS(combined, file.path(processed_dir, "flux_hh_all_sites.rds"))
message("[WAFNET] Combined HH dataset: ", nrow(combined), " rows across ",
        length(read_sites), " site(s): ", paste(read_sites, collapse = ", "))
message("[WAFNET] 02_read_hh.R complete.")
