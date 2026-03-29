## 05_units.R — Convert variables from native FLUXNET units to analysis units
## Uses: fluxnet_convert_units()
## See CLAUDE.md Unit Conversion Reference. Never implement ad-hoc conversions.

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

flux_data_qc <- readRDS(file.path(processed_dir, "flux_data_qc.rds"))

# file_inventory.rds uses the column name 'time_resolution' (from the fluxnet
# package); fluxnet_convert_units() accepts either name. No rename needed, but
# document the discrepancy here so it is visible. See CLAUDE.md: the CLAUDE.md
# standard name is 'temporal_resolution'; the package uses 'time_resolution'.
manifest <- readRDS(file.path(processed_dir, "file_inventory.rds"))

# fluxnet_convert_units() uses the flux_resolution attribute on flux_data_qc
# (set by flux_read) to select the correct resolution row from the manifest
# when multiple resolutions are present. No pre-filtering needed.
flux_data_converted <- fluxnet_convert_units(flux_data_qc, manifest)

output_path <- file.path(processed_dir, "flux_data_converted.rds")
saveRDS(flux_data_converted, output_path)

# Write .meta.json companion file (required by CLAUDE.md)
snapshot_paths <- list.files(
  file.path(FLUXNET_DATA_ROOT, "snapshots"),
  pattern = "fluxnet_shuttle_snapshot_.*\\.csv$",
  full.names = TRUE
)
write_output_metadata(
  output_path,
  input_sources = c(
    file.path(processed_dir, "flux_data_qc.rds"),
    if (length(snapshot_paths) > 0) sort(snapshot_paths, decreasing = TRUE)[[1]] else character(0)
  ),
  notes = "Unit conversions applied: NEE/GPP/RECO gC m-2 per period; LE mm H2O; H/SW_IN MJ m-2; TA K; VPD kPa."
)

cat("\n--- 05_units.R summary ---\n")
res_attr <- attr(flux_data_converted, "flux_resolution")
cat("Resolution:         ", if (!is.null(res_attr)) res_attr else "(attribute not set)", "\n")
cat("Rows converted:     ", nrow(flux_data_converted), "\n")
cat("Converted columns:  ",
    paste(grep("_native$", names(flux_data_converted), value = TRUE) |>
            sub("_native$", "", x=_), collapse=", "), "\n")
meta_path <- paste0(tools::file_path_sans_ext(output_path), ".meta.json")
cat(".meta.json written: ", meta_path,
    if (file.exists(meta_path)) "(OK)" else "(MISSING)", "\n")
