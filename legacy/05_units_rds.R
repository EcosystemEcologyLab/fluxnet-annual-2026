## 05_units.R — Convert variables from native FLUXNET units to analysis units
## Uses: fluxnet_convert_units() in R/units.R
##
## fluxnet_convert_units() reads native variable units from BIFVARINFO_YY
## metadata files and only applies conversions when the source unit differs from
## the target. Carbon fluxes (NEE/GPP/RECO) at DD/MM/WW/YY resolutions are
## already pre-integrated gC m-2 values from the ONEFlux pipeline and are
## passed through WITHOUT applying the µmol→gC conversion factor.
##
## See CLAUDE.md Unit Conversion Reference. Never implement ad-hoc conversions.
## Loops over FLUXNET_EXTRACT_RESOLUTIONS; reads flux_data_qc_<res>.rds and
## writes flux_data_converted_<res>.rds + companion .meta.json for each.

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}

source("R/pipeline_config.R")
check_pipeline_config()

source("R/units.R")
source("R/utils.R")

library(dplyr)

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
  suffix  <- res_to_suffix[[res_code]]
  qc_path <- file.path(processed_dir, paste0("flux_data_qc_", suffix, ".rds"))

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

  # ── Build conversion notes from the log attribute ────────────────────────
  clog <- attr(flux_data_converted, "conversion_log")

  converted_vars <- if (!is.null(clog) && nrow(clog) > 0) {
    paste(clog$variable[clog$converted], collapse = ", ")
  } else {
    "(none)"
  }

  write_output_metadata(
    out_path,
    input_sources = c(qc_path, latest_snapshot),
    notes = paste0(
      "Unit conversions applied (", toupper(suffix), "): ",
      "carbon fluxes at coarse resolution passed through (pre-integrated); ",
      "LE mm H2O; H/SW_IN MJ m-2; TA K; VPD kPa. ",
      "Converted columns: ", converted_vars
    )
  )

  # ── Console summary ───────────────────────────────────────────────────────
  res_attr <- attr(flux_data_converted, "flux_resolution")
  cat("\n--- 05_units.R: resolution", toupper(suffix), "---\n")
  cat("Resolution:     ",
      if (!is.null(res_attr)) res_attr else "(attribute not set)", "\n")
  cat("Rows processed: ", nrow(flux_data_converted), "\n")

  if (!is.null(clog) && nrow(clog) > 0) {
    n_converted  <- sum(clog$converted)
    n_passthru   <- sum(!clog$converted)
    cat("Variables converted:     ", n_converted, "\n")
    cat("Variables passed through:", n_passthru, "\n")

    cat("\n  Conversion summary table:\n")
    cat(sprintf("  %-35s %-22s %-22s %s\n",
                "Variable", "Source unit", "Target unit", "Converted"))
    cat("  ", strrep("-", 88), "\n", sep = "")
    for (i in seq_len(nrow(clog))) {
      cat(sprintf("  %-35s %-22s %-22s %s\n",
                  clog$variable[i],
                  clog$source_unit[i],
                  clog$target_unit[i],
                  if (clog$converted[i]) "YES" else "no"))
    }
  } else {
    cat("No variables processed for this resolution.\n")
  }

  # ── Sanity-check ranges for key carbon variables ──────────────────────────
  nee_col <- intersect(c("NEE_VUT_REF", "NEE_VUT_USTAR50"), names(flux_data_converted))
  if (length(nee_col) > 0 && toupper(suffix) == "YY") {
    nee_vals <- flux_data_converted[[nee_col[[1]]]]
    nee_vals <- nee_vals[!is.na(nee_vals)]
    cat("\n  YY NEE range check (", nee_col[[1]], "):\n", sep = "")
    cat("    min:", round(min(nee_vals), 1),
        "  max:", round(max(nee_vals), 1),
        "  (expected: roughly -600 to +200 gC m-2 y-1)\n")
    out_of_range <- abs(nee_vals) > 2000
    if (any(out_of_range)) {
      warning(sum(out_of_range), " NEE values exceed |2000| gC m-2 y-1 — ",
              "possible double-conversion. Check BIFVARINFO source units.")
    }
  }

  meta_path <- paste0(tools::file_path_sans_ext(out_path), ".meta.json")
  cat("Saved: ", out_path, "\n")
  cat(".meta.json:", meta_path,
      if (file.exists(meta_path)) "(OK)" else "(MISSING)", "\n")
}

# Write session info — required by CLAUDE.md Output Metadata spec.
if (!dir.exists("outputs")) dir.create("outputs", recursive = TRUE)
session_info_path <- file.path("outputs", "session_info.txt")
writeLines(capture.output(sessionInfo()), session_info_path)
cat("\nsessionInfo() written to:", session_info_path, "\n")
