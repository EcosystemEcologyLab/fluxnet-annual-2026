## 00_config.R — shared setup for the Russ Scott AGU26 side extraction
##
## Sourced by every other script in this directory. Not run directly.
##
## This is a side project, NOT part of the FLUXNET Annual Paper 2026 pipeline
## (scripts/01-07). Unlike WAFNET/isotope-pilot-style side projects, it does
## NOT download its own data and does NOT need an isolated FLUXNET_DATA_ROOT
## override: it reads the main project's already-built DuckDB store
## (data/duckdb/fluxnet.duckdb) read-only, plus the 2026-09-20 snapshot CSV.
## No writes ever go to repo-root data/, outputs/, or the DuckDB file itself.
##
## Run every script in this directory from the REPO ROOT, e.g.:
##   Rscript RSCOTT_AGU26/code/02_build_site_years.R

if (file.exists(".env")) {
  suppressPackageStartupMessages(library(dotenv))
  dotenv::load_dot_env()
}

source("R/pipeline_config.R")
source("R/qc.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(lubridate)
  library(duckdb)
  library(DBI)
  library(fs)
  library(ggplot2)
  library(jsonlite)
})

RSCOTT_ROOT <- "RSCOTT_AGU26"
RSCOTT_DUCKDB_PATH <- "data/duckdb/fluxnet.duckdb"
RSCOTT_SNAPSHOT_PATH <- "data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv"

for (sub in c("tables", "figures", "docs", "logs")) {
  fs::dir_create(file.path(RSCOTT_ROOT, sub))
}

## -- Project constants, stated once here so every script agrees -----------
RSCOTT_IGBP_CLASSES <- c("OSH", "CSH", "GRA", "SAV", "WSA")
RSCOTT_ERA_FILLED_MAX_SHARE <- 0.10   # year counts as "measured precip" if ERA-filled share <= this
RSCOTT_MAP_SCREEN_MM <- 800           # mean annual measured P must be below this (mm)
RSCOTT_MIN_MEASURED_YEARS_FOR_SCREEN <- 2  # min years with measured precip to compute mean P and screen a site
RSCOTT_MIN_PASSING_YEARS_TO_KEEP <- 5       # min passing years (either time step) to keep a site
RSCOTT_LAMBDA_J_PER_KG <- 2.45e6      # fixed latent heat used by the Annual Paper for ET from LE

## -- Local metadata/caption helpers -----------------------------------------
## Mirrors WAFNET/write_meta.R exactly (same fields minus r_session_info,
## which stays centralised at the repo-root outputs/session_info.txt for the
## Annual Paper only -- this side project never writes there).

#' Write a .meta.json companion for one RSCOTT_AGU26 output file
#' @param output_path Path to the output file (.csv, .png, ...).
#' @param input_sources Character vector of the file(s)/table(s) this output was built from.
#' @param notes Free text; use "" (not omitted) when there are none.
#' @return Invisibly, the path written.
write_meta <- function(output_path, input_sources, notes = "") {
  if (!is.character(notes) || length(notes) != 1) {
    stop("write_meta(): notes must be a single character string (use \"\" if none).")
  }
  meta <- list(
    run_datetime_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    pipeline_version = tryCatch(system("git rev-parse --short HEAD", intern = TRUE),
                                 error = function(e) NA_character_),
    input_sources    = as.list(input_sources),
    notes            = notes
  )
  meta_path <- paste0(tools::file_path_sans_ext(output_path), ".meta.json")
  writeLines(jsonlite::toJSON(meta, pretty = TRUE, auto_unbox = TRUE), meta_path)
  invisible(meta_path)
}

#' Write a .legend.txt caption file alongside a figure
#' @param figure_path Path to the figure file (e.g. .png).
#' @param caption Character scalar or vector; vector elements joined with a blank line.
#' @return Invisibly, the path written.
write_caption <- function(figure_path, caption) {
  caption_path <- paste0(tools::file_path_sans_ext(figure_path), ".legend.txt")
  writeLines(paste(caption, collapse = "\n\n"), caption_path)
  invisible(caption_path)
}

#' Open a read-only connection to the main project's DuckDB store
rscott_duckdb_connect <- function() {
  dbConnect(duckdb::duckdb(), dbdir = RSCOTT_DUCKDB_PATH, read_only = TRUE)
}

message("[RSCOTT_AGU26] DuckDB: ", RSCOTT_DUCKDB_PATH)
message("[RSCOTT_AGU26] Snapshot: ", RSCOTT_SNAPSHOT_PATH)
