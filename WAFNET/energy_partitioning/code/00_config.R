## 00_config.R — shared setup for the WAFNET energy-partitioning side analysis
##
## Sourced by every other script in this directory. Not run directly.
##
## This analysis is NOT part of the FLUXNET Annual Paper 2026 (see
## ../README.md) and must never read or write the repo-root data/ directory
## used by the Annual Paper pipeline (scripts/01-07). It gets its own,
## fully isolated data root by overriding FLUXNET_DATA_ROOT before sourcing
## the shared R/pipeline_config.R — every path derived from that variable
## (raw/, extracted/, processed/) then lives under
## WAFNET/energy_partitioning/data/ instead.
##
## Run every script in this directory from the REPO ROOT (same convention as
## scripts/01-07), e.g.:
##   Rscript WAFNET/energy_partitioning/code/01_download_extract_hh.R

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}

WAFNET_ROOT <- "WAFNET/energy_partitioning"

# --- Isolated data root -----------------------------------------------------
# Overrides must happen BEFORE sourcing R/pipeline_config.R, since that file
# reads these as plain Sys.getenv() calls at source time.
Sys.setenv(FLUXNET_DATA_ROOT = file.path(WAFNET_ROOT, "data"))

# Six-site filter — humid-to-Sahelian gradient (see README.md).
WAFNET_SITES <- c("GH-Ank", "BJ-Db1", "BJ-Bfg", "BJ-Nhu", "SN-Nkr", "SN-Dhr")
Sys.setenv(FLUXNET_SITE_FILTER = paste(WAFNET_SITES, collapse = " "))

# Half-hourly only. The Annual Paper's shared data/ deliberately excludes
# 'h' (R/pipeline_config.R: FLUXNET_EXTRACT_RESOLUTIONS default "y m d") —
# which is exactly why this analysis needs its own separately-downloaded
# copy rather than reusing repo-root data/extracted/.
Sys.setenv(FLUXNET_EXTRACT_RESOLUTIONS = "h")

source("R/pipeline_config.R")
source("R/credentials.R")
check_pipeline_config()

library(fluxnet)

for (sub in c("raw", "extracted", "processed")) {
  dir.create(file.path(WAFNET_ROOT, "data", sub), recursive = TRUE, showWarnings = FALSE)
}
dir.create(file.path(WAFNET_ROOT, "logs"),    recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(WAFNET_ROOT, "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(WAFNET_ROOT, "tables"),  recursive = TRUE, showWarnings = FALSE)

# --- Read-only access to Annual Paper metadata ------------------------------
# Ancillary site metadata (canopy height, land cover, heat-flux
# instrumentation) is read from the EXISTING per-site BIF files under the
# repo-root data/extracted/ — by the user's own choice (2026-09-08), to avoid
# a redundant download. This is READ-ONLY: nothing under this path is ever
# written to from WAFNET/ code.
ANNUAL_PAPER_EXTRACTED_DIR <- file.path("data", "extracted")

message("[WAFNET] Data root: ", Sys.getenv("FLUXNET_DATA_ROOT"))
message("[WAFNET] Sites: ", paste(WAFNET_SITES, collapse = ", "))
