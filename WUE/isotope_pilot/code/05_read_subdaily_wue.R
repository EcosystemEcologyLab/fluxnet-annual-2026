## 05_read_subdaily_wue.R — Stage 2. Re-read the extracted sub-daily (HH/HR)
## FLUXMET files for the WUE isotope-pilot sites, one site at a time, exactly
## as 02_read_subdaily.R does, but with four extra columns needed for the WUE
## screens and metrics: P_ERA, NEE_CUT_REF, NEE_CUT_REF_QC, RECO_NT_CUT_REF.
##
## No new download, no re-extraction — reads the same
## data/extracted/.../FLUXMET_HH|HR_*.csv files 02_read_subdaily.R already
## read (confirmed: P_ERA/NEE_CUT_REF/NEE_CUT_REF_QC/RECO_NT_CUT_REF are all
## columns of the FLUXMET file itself, not the separate ERA5 file, for every
## site checked). Still no combined all-sites object — output is one RDS per
## site, as in 02.
##
## STANDING RULE 1: GPP is the nighttime partition only. No _DT_ column is
## ever added to KEEP_COLS; the guard below is a defensive double-check.
##
## Stage 2 drops CH-Dav (PI decision, 2026-10-07 -- see docs/methods_memo.md):
## this script reads WUE_SITES_STAGE2 (12 sites), not WUE_SITES (13). The
## CH-Dav raw/extracted files already on disk are untouched, just not re-read
## here.
##
## Output (data/processed/, gitignored):
##   subdaily_wue/<site_id>.rds   one data frame per site, kept columns only
##   read_status_wue.csv          per-site resolution + row count + status

source("WUE/isotope_pilot/code/00_config.R")

processed_dir  <- file.path(WUE_ROOT, "data", "processed")
inventory_path <- file.path(processed_dir, "file_inventory.rds")
subdaily_dir   <- file.path(processed_dir, "subdaily_wue")
dir.create(subdaily_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(inventory_path)) {
  stop("[WUE] ", inventory_path, " not found -- run 01_download_extract.R first.")
}
file_inventory <- readRDS(inventory_path)

inv_subdaily <- if (nrow(file_inventory) == 0) {
  file_inventory
} else {
  file_inventory[
    !is.na(file_inventory$time_resolution) &
      file_inventory$time_resolution %in% c("HH", "HR") &
      !is.na(file_inventory$dataset) & file_inventory$dataset == "FLUXMET" &
      !is.na(file_inventory$path) & nchar(file_inventory$path) > 0 &
      file.exists(file_inventory$path),
    ,
    drop = FALSE
  ]
}

# Stage 2 column whitelist: the Stage 1 (02_read_subdaily.R) KEEP_COLS plus
# the four columns this stage needs: P_ERA (rain screen + precip check),
# NEE_CUT_REF/NEE_CUT_REF_QC (CUT-fallback sites), RECO_NT_CUT_REF (not used
# by WUE metrics directly, but requested explicitly to keep NEE/GPP/RECO
# together for whichever product a site runs on).
KEEP_COLS <- c(
  "TIMESTAMP_START", "TIMESTAMP_END", "NIGHT", "SW_IN_POT", "SW_IN_F", "SW_IN_F_QC",
  "NETRAD", "TA_F", "TA_F_QC", "VPD_F", "VPD_F_QC", "PA_F", "P_F", "P_F_QC", "P_ERA",
  "WS_F", "USTAR", "CO2_F_MDS", "CO2_F_MDS_QC",
  "NEE_VUT_REF", "NEE_VUT_REF_QC", "NEE_CUT_REF", "NEE_CUT_REF_QC",
  "GPP_NT_VUT_REF", "GPP_NT_CUT_REF", "RECO_NT_VUT_REF", "RECO_NT_CUT_REF",
  "LE_F_MDS", "LE_F_MDS_QC", "H_F_MDS", "H_F_MDS_QC", "G_F_MDS"
)
stopifnot(!any(grepl(WUE_FORBIDDEN_DT_PATTERN, KEEP_COLS)))

read_one_site <- function(site_id, rows) {
  dat <- dplyr::bind_rows(lapply(rows$path, function(p) {
    tryCatch(
      readr::read_csv(p, show_col_types = FALSE),
      error = function(e) {
        warning("[WUE] ", site_id, ": skipping unreadable file: ", p, "\n  ", conditionMessage(e))
        NULL
      }
    )
  }))
  if (nrow(dat) == 0L) return(NULL)

  present_cols <- intersect(KEEP_COLS, names(dat))
  absent_cols  <- setdiff(KEEP_COLS, names(dat))
  if (length(absent_cols) > 0) {
    message("[WUE] ", site_id, ": column(s) absent in this file set -- ",
            paste(absent_cols, collapse = ", "))
  }

  dat <- dat[, present_cols, drop = FALSE]
  ts_cols <- intersect(c("TIMESTAMP_START", "TIMESTAMP_END"), present_cols)

  dat <- dat |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(ts_cols), lubridate::ymd_hm),
      dplyr::across(dplyr::where(is.numeric), \(x) dplyr::na_if(x, -9999))
    )

  stopifnot(!any(grepl(WUE_FORBIDDEN_DT_PATTERN, names(dat))))
  dat
}

read_status <- lapply(WUE_SITES_STAGE2, function(site) {
  out_path <- file.path(subdaily_dir, paste0(site, ".rds"))

  if (file.exists(out_path)) {
    message("[WUE] ", site, ": already read -- skipping (", out_path, ")")
    existing <- readRDS(out_path)
    resolution <- if (nrow(existing) == 0 || !"TIMESTAMP_START" %in% names(existing)) NA_character_ else {
      dt <- as.numeric(diff(sort(unique(existing$TIMESTAMP_START))[1:2]))
      if (is.na(dt)) NA_character_ else if (dt == 30) "HH" else if (dt == 60) "HR" else "unknown"
    }
    return(data.frame(
      site_id = site, resolution = resolution, n_rows = nrow(existing),
      status = "read_ok_cached", stringsAsFactors = FALSE
    ))
  }

  site_rows <- inv_subdaily[inv_subdaily$site_id == site, , drop = FALSE]
  if (nrow(site_rows) == 0L) {
    warning("[WUE] ", site, ": no sub-daily FLUXMET file in inventory -- ",
            "no sub-daily data available for this site.")
    return(data.frame(
      site_id = site, resolution = NA_character_, n_rows = 0L,
      status = "no_subdaily_file", stringsAsFactors = FALSE
    ))
  }

  message("[WUE] Reading ", site, " (", nrow(site_rows), " file(s), resolution ",
          paste(unique(site_rows$time_resolution), collapse = "/"), ") ...")
  site_dat <- read_one_site(site, site_rows)
  if (is.null(site_dat)) {
    warning("[WUE] ", site, ": all files unreadable -- see warnings above.")
    return(data.frame(
      site_id = site, resolution = NA_character_, n_rows = 0L,
      status = "unreadable", stringsAsFactors = FALSE
    ))
  }

  saveRDS(site_dat, out_path)
  resolution <- unique(site_rows$time_resolution)[1]
  message("[WUE]   -> ", out_path, " (", nrow(site_dat), " rows x ", ncol(site_dat), " cols)")
  data.frame(
    site_id = site, resolution = resolution, n_rows = nrow(site_dat),
    status = "read_ok", stringsAsFactors = FALSE
  )
})
read_status <- do.call(rbind, read_status)

write.csv(read_status, file.path(processed_dir, "read_status_wue.csv"), row.names = FALSE)

read_sites   <- read_status$site_id[read_status$status %in% c("read_ok", "read_ok_cached")]
unread_sites <- setdiff(WUE_SITES_STAGE2, read_sites)
if (length(unread_sites) > 0L) {
  warning(
    "[WUE] No sub-daily data read for: ", paste(unread_sites, collapse = ", "),
    " -- flagged in read_status_wue.csv, not silently dropped."
  )
}

message("[WUE] 05_read_subdaily_wue.R complete. Read: ", length(read_sites), "/", length(WUE_SITES_STAGE2))
