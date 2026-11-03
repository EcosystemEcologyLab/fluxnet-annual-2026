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
source("R/qc.R")
check_pipeline_config()

# Six-site filter — same sites as WAFNET/energy_partitioning (see README.md).
# Retained as-is for Stage 1 scripts (01-03); Stage 2 (04+) uses the two sets
# below instead, per Dave's decision carried from Ossenatou Mamadou's 16
# October reply (see docs/report_back_2026-11-03.md):
#   - STAGE2_SITES: the five agreed comparison sites. ML-Kem was added at
#     Ossenatou's request (Mali, WSA, 2007-2009); BJ-Db1 and SN-Nkr were
#     dropped from the agreed set in Stage 1 because they did not overlap the
#     original FLUXCOM product, NOT because of any data-quality concern.
#   - STAGE2_PENDING_SITES: BJ-Db1 and SN-Nkr, re-checked against
#     FLUXCOM-X-BASE's longer period. Kept separate from STAGE2_SITES in every
#     Stage 2 table and figure, flagged "pending Ossenatou", pending her
#     sign-off before being folded into the agreed set.
WAFNET_SITES <- c("GH-Ank", "BJ-Db1", "BJ-Nhu", "BJ-Bfg", "SN-Nkr", "SN-Dhr")
STAGE2_SITES <- c("GH-Ank", "BJ-Nhu", "BJ-Bfg", "SN-Dhr", "ML-Kem")
STAGE2_PENDING_SITES <- c("BJ-Db1", "SN-Nkr")
STAGE2_ALL_SITES <- c(STAGE2_SITES, STAGE2_PENDING_SITES)

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
  "SN-Dhr" = "EUF_SN-Dhr_FLUXNET_2010-2022_v1.3_r1",
  "ML-Kem" = "FLX_ML-Kem_FLUXNET_2007-2009_v1.3_r1"
)

# Stage 2 (non-Shuttle FLUXCOM-X-BASE comparison data) locations — gitignored,
# shared at repo root since the raw grids are reusable across any analysis.
FLUXCOM_XBASE_DIR <- file.path("data", "external", "fluxcom_x_base")
FLUXCOM_ORIGINAL_ILAMB_DIR <- file.path("data", "external", "fluxcom_original_ilamb")

# One colour per site, shared across every Stage 2 figure (scatter, map,
# Taylor diagrams, climatology) so a site is the same colour everywhere --
# fixed here, not recomputed per-script, per the 2026-11-03 fixes prompt.
# Order is STAGE2_ALL_SITES (five agreed, then the two pending) so the
# palette assignment is stable regardless of which subset a given figure uses.
STAGE2_SITE_COLORS <- stats::setNames(
  grDevices::hcl.colors(length(STAGE2_ALL_SITES), palette = "Dark 3"),
  STAGE2_ALL_SITES
)
# Single fixed colour for the X-BASE series wherever it appears alongside a
# tower series (climatology figures) -- deliberately NOT a site colour, so it
# reads as "the product" rather than "a site" in every panel.
XBASE_SERIES_COLOR <- "#444444"

# Read-only site coordinates/IGBP, from each site's own BIF file. Shared
# helper so 08_extract_statistics.R, 09_figures_draft.R, and
# 10_climatology.R don't each re-implement the same BIF read.
site_bif_value <- function(site_id, variable) {
  site_dir <- file.path(ANNUAL_PAPER_EXTRACTED_DIR, WAFNET_SITE_DIRS[[site_id]])
  f <- list.files(site_dir, pattern = "_BIF_.*\\.csv$", full.names = TRUE)
  stopifnot(length(f) == 1)
  bif <- utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
  bif$DATAVALUE[bif$VARIABLE == variable][1]
}

site_coords_for <- function(site_id) {
  c(lon = as.numeric(site_bif_value(site_id, "LOCATION_LONG")),
    lat = as.numeric(site_bif_value(site_id, "LOCATION_LAT")))
}

site_igbp_for <- function(site_id) site_bif_value(site_id, "IGBP")

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
