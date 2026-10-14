## 00_config.R — shared setup for the WAFNET x FLUXCOM comparison side analysis
##
## Sourced by every other script in this directory. Not run directly.
##
## This analysis is NOT part of the FLUXNET Annual Paper 2026 (see
## ../README.md). Unlike WAFNET/energy_partitioning/, it does NOT need its own
## Shuttle download: the observational data it needs (YY/MM FLUXMET extracts
## for these six sites) are already present in the repo-root data/extracted/
## from the Annual Paper's own download, at native paper QC/VUT-CUT
## conventions. This script therefore never overrides FLUXNET_DATA_ROOT and
## never writes to the repo-root data/ tree — every read of it below is
## read-only.
##
## FLUXCOM itself is non-Shuttle comparison data (CLAUDE.md Hard Rule #1) and
## is never read from or written into the repo-root data/ tree either; it
## gets its own WAFNET/fluxcom_comparison/data/ (gitignored), used only once
## Stage 2 (download) is approved.
##
## Run every script in this directory from the REPO ROOT, e.g.:
##   Rscript WAFNET/fluxcom_comparison/code/01_observations_qc_overlap.R

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}

FLUXCOM_ROOT <- "WAFNET/fluxcom_comparison"

source("R/pipeline_config.R")
source("R/units.R")
check_pipeline_config()

# Six-site filter — same sites as WAFNET/energy_partitioning (see README.md).
WAFNET_SITES <- c("GH-Ank", "BJ-Db1", "BJ-Nhu", "BJ-Bfg", "SN-Nkr", "SN-Dhr")

for (sub in c("raw", "extracted", "processed")) {
  dir.create(file.path(FLUXCOM_ROOT, "data", sub), recursive = TRUE, showWarnings = FALSE)
}
dir.create(file.path(FLUXCOM_ROOT, "logs"),    recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(FLUXCOM_ROOT, "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(FLUXCOM_ROOT, "tables"),  recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(FLUXCOM_ROOT, "docs"),    recursive = TRUE, showWarnings = FALSE)

# --- Read-only access to the Annual Paper's existing Shuttle extracts -------
# One extracted site directory per WAFNET site, already on disk at repo root.
# This mapping is read-only; nothing under ANNUAL_PAPER_EXTRACTED_DIR is ever
# written to from WAFNET/fluxcom_comparison code.
ANNUAL_PAPER_EXTRACTED_DIR <- file.path("data", "extracted")

WAFNET_SITE_DIRS <- c(
  "GH-Ank" = "EUF_GH-Ank_FLUXNET_2011-2014_v1.3_r1",
  "BJ-Db1" = "FLX_BJ-Db1_FLUXNET_2022-2024_v1.3_r1",
  "BJ-Nhu" = "FLX_BJ-Nhu_FLUXNET_2008-2017_v1.3_r1",
  "BJ-Bfg" = "FLX_BJ-Bfg_FLUXNET_2008-2017_v1.3_r1",
  "SN-Nkr" = "EUF_SN-Nkr_FLUXNET_2018-2024_v1.3_r1",
  "SN-Dhr" = "EUF_SN-Dhr_FLUXNET_2010-2022_v1.3_r1"
)

wafnet_fluxmet_path <- function(site_id, resolution = c("YY", "MM")) {
  resolution <- match.arg(resolution)
  site_dir <- WAFNET_SITE_DIRS[[site_id]]
  if (is.null(site_dir)) stop("[WAFNET/fluxcom_comparison] Unknown site_id: ", site_id)
  dir_path <- file.path(ANNUAL_PAPER_EXTRACTED_DIR, site_dir)
  pattern <- paste0("FLUXMET_", resolution, "_.*\\.csv$")
  f <- list.files(dir_path, pattern = pattern, full.names = TRUE)
  if (length(f) != 1) {
    stop(
      "[WAFNET/fluxcom_comparison] Expected exactly one FLUXMET ", resolution,
      " file for ", site_id, " in ", dir_path, ", found ", length(f)
    )
  }
  f
}

message("[WAFNET/fluxcom_comparison] Sites: ", paste(WAFNET_SITES, collapse = ", "))
message("[WAFNET/fluxcom_comparison] Reading repo-root data/extracted/ (read-only)")
