## 05_units.R — Convert variables from native FLUXNET units to analysis units
## Uses: fluxnet_convert_units()
## See CLAUDE.md Unit Conversion Reference. Never implement ad-hoc conversions.
## Loops over FLUXNET_EXTRACT_RESOLUTIONS; reads flux_data_qc_<res>.rds and
## writes flux_data_converted_<res>.rds + companion .meta.json for each.

source("R/pipeline_config.R")
check_pipeline_config()

source("R/units.R")
source("R/utils.R")

library(dplyr)
if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}

processed_dir <- file.path(FLUXNET_DATA_ROOT, "processed")

manifest <- readRDS(file.path(processed_dir, "file_inventory.rds"))

# file_inventory.rds uses the column name 'time_resolution' (from the fluxnet
# package); fluxnet_convert_units() accepts either name. No rename needed, but
# document the discrepancy here so it is visible. See CLAUDE.md: the CLAUDE.md
# standard name is 'temporal_resolution'; the package uses 'time_resolution'.

snapshot_paths <- list.files(
  file.path(FLUXNET_DATA_ROOT, "snapshots"),
  pattern = "fluxnet_shuttle_snapshot_.*\\.csv$",
  full.names = TRUE
)
latest_snapshot <- if (length(snapshot_paths) > 0) {
  sort(snapshot_paths, decreasing = TRUE)[[1]]
} else {
  character(0)
}

# Map extract resolution codes to output file suffixes.
res_to_suffix <- c(y = "yy", m = "mm", w = "ww", d = "dd", h = "hh")

for (res_code in FLUXNET_EXTRACT_RESOLUTIONS) {
  suffix   <- res_to_suffix[[res_code]]
  qc_path  <- file.path(processed_dir, paste0("flux_data_qc_", suffix, ".rds"))

  if (!file.exists(qc_path)) {
    message("No QC file for resolution '", res_code, "' (", suffix,
            ") — skipping.")
    next
  }

  flux_data_qc <- readRDS(qc_path)

  # fluxnet_convert_units() uses the flux_resolution attribute on flux_data_qc
  # (set by flux_read) to select the correct resolution row from the manifest
  # when multiple resolutions are present. No pre-filtering needed.
  flux_data_converted <- fluxnet_convert_units(flux_data_qc, manifest)

  out_path <- file.path(processed_dir,
                        paste0("flux_data_converted_", suffix, ".rds"))
  saveRDS(flux_data_converted, out_path)

  write_output_metadata(
    out_path,
    input_sources = c(qc_path, latest_snapshot),
    notes = paste0(
      "Unit conversions applied (", toupper(suffix), "): ",
      "NEE/GPP/RECO gC m-2 per period; LE mm H2O; H/SW_IN MJ m-2; TA K; VPD kPa."
    )
  )

  cat("\n--- 05_units.R: resolution", toupper(suffix), "---\n")
  res_attr <- attr(flux_data_converted, "flux_resolution")
  cat("Resolution:         ",
      if (!is.null(res_attr)) res_attr else "(attribute not set)", "\n")
  cat("Rows converted:     ", nrow(flux_data_converted), "\n")
  cat("Converted columns:  ",
      paste(grep("_native$", names(flux_data_converted), value = TRUE) |>
              sub("_native$", "", x = _), collapse = ", "), "\n")
  meta_path <- paste0(tools::file_path_sans_ext(out_path), ".meta.json")
  cat(".meta.json written: ", meta_path,
      if (file.exists(meta_path)) "(OK)" else "(MISSING)", "\n")
  cat("Saved:", out_path, "\n")
}

# Write session info — required by CLAUDE.md Output Metadata spec.
if (!dir.exists("outputs")) dir.create("outputs", recursive = TRUE)
session_info_path <- file.path("outputs", "session_info.txt")
writeLines(capture.output(sessionInfo()), session_info_path)
cat("\nsessionInfo() written to:", session_info_path, "\n")
