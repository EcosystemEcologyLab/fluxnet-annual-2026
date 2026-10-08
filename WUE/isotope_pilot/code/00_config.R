## 00_config.R — shared setup for the WUE isotope pilot side analysis
##
## Sourced by every other script in this directory. Not run directly.
##
## This analysis is NOT part of the FLUXNET Annual Paper 2026 and must never
## read or write the repo-root data/ directory used by the Annual Paper
## pipeline (scripts/01-07). It gets its own, fully isolated data root by
## overriding FLUXNET_DATA_ROOT before sourcing the shared R/pipeline_config.R
## — every path derived from that variable (raw/, extracted/, processed/)
## then lives under WUE/isotope_pilot/data/ instead. Pattern copied from
## WAFNET/energy_partitioning/code/00_config.R.
##
## Run every script in this directory from the REPO ROOT, e.g.:
##   Rscript WUE/isotope_pilot/code/01_download_extract.R

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}

WUE_ROOT <- "WUE/isotope_pilot"

# --- Isolated data root -----------------------------------------------------
# Overrides must happen BEFORE sourcing R/pipeline_config.R, since that file
# reads these as plain Sys.getenv() calls at source time.
Sys.setenv(FLUXNET_DATA_ROOT = file.path(WUE_ROOT, "data"))

# Full 13-site list for the pilot (8 with published tree-ring isotopes, 5
# flux-only) — see ../README.md.
WUE_SITES_ALL <- c(
  "US-Ha1", "US-Ho2", "US-MMS", "US-SP1", "US-Bar", "US-Slt", "US-Dk2", "US-Fuf",
  "DE-Tha", "BE-Vie", "NL-Loo", "FI-Hyy", "CH-Dav"
)

# WUE_SITE_SUBSET narrows WUE_SITES to a subset (space-separated site IDs) so
# the smoke test can run these same, unedited scripts against one site.
# Unset = all 13 sites.
WUE_SITES <- {
  subset_env <- Sys.getenv("WUE_SITE_SUBSET", unset = "")
  if (nchar(trimws(subset_env)) == 0) {
    WUE_SITES_ALL
  } else {
    strsplit(trimws(subset_env), "\\s+")[[1]]
  }
}
Sys.setenv(FLUXNET_SITE_FILTER = paste(WUE_SITES, collapse = " "))

# Sub-daily (HH or HR, auto-detected per site by flux_extract()/
# flux_discover_files()) plus daily. GPP_NT_* etc. are needed at sub-daily
# resolution; DD is read for site-level screening context only in the
# pre-analysis report. Confirmed flux_extract(resolutions = ...) accepts a
# vector, e.g. c("h", "d") (checked against its signature 2026-10-07).
Sys.setenv(FLUXNET_EXTRACT_RESOLUTIONS = "h d")

source("R/pipeline_config.R")
source("R/credentials.R")
check_pipeline_config()

library(fluxnet)

for (sub in c("raw", "extracted", "processed")) {
  dir.create(file.path(WUE_ROOT, "data", sub), recursive = TRUE, showWarnings = FALSE)
}
dir.create(file.path(WUE_ROOT, "data", "external", "treering"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(WUE_ROOT, "logs"),    recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(WUE_ROOT, "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(WUE_ROOT, "tables"),  recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(WUE_ROOT, "docs"),    recursive = TRUE, showWarnings = FALSE)

# Locked Annual Paper snapshot, read-only, used only as a comparison point in
# tables/site_inventory.csv (differs-from-locked-snapshot column). Never
# treated as a data source for this analysis — see README.md Hard Rule 1 note.
WUE_LOCKED_SNAPSHOT <- file.path(
  "data", "snapshots", "fluxnet_shuttle_snapshot_20260901T094522.csv"
)

# STANDING RULE 1 (README.md / docs/methods_memo.md): GPP is the nighttime
# partition only. Any column matching this pattern must never be read,
# summarised, plotted, or compared anywhere in this analysis.
WUE_FORBIDDEN_DT_PATTERN <- "_DT_"

message("[WUE isotope pilot] Data root: ", Sys.getenv("FLUXNET_DATA_ROOT"))
message("[WUE isotope pilot] Sites (", length(WUE_SITES), "): ", paste(WUE_SITES, collapse = ", "))

# --- Stage 2 (screens + WUE metrics) site list ------------------------------
# PI decision, 2026-10-07: CH-Dav is dropped from stage 2 (3 years with no
# nighttime GPP; stage 1 closure slope 0.46, r2 0.56 -- see
# docs/methods_memo.md). The CH-Dav files already on disk are left in place,
# only excluded from the stage-2 site list. intersect() with WUE_SITES (not
# WUE_SITES_ALL) so WUE_SITE_SUBSET still narrows stage 2 the same way it
# narrows stage 1 -- dropping CH-Dav is a no-op for a subset that excludes it.
#
# PI decision, 2026-10-07 (12-site full run attempt): NL-Loo also dropped --
# fails 06_build_site_years.R's P_ERA integrity check by -3.08% (threshold
# 2%; see tables/p_era_check.csv and docs/methods_memo.md), the only one of
# the 12 stage-2 sites to do so. Its files are likewise left on disk, only
# excluded from the stage-2 site list.
WUE_SITES_STAGE2 <- intersect(WUE_SITES, setdiff(WUE_SITES_ALL, c("CH-Dav", "NL-Loo")))
message("[WUE isotope pilot] Stage 2 sites (", length(WUE_SITES_STAGE2), "): ",
        paste(WUE_SITES_STAGE2, collapse = ", "))
