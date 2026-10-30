## 00_config.R — GSL figure rebuild, side-project config.
##
## Sourced by every script in this directory. Not run directly.
##
## Not part of the FLUXNET Annual Paper 2026 -- see ../README.md. Reads the
## repo-root R/pipeline_config.R (thresholds), R/qc.R (qc_passes()) and
## R/utils.R::log_exclusion()/log_unknown() (passing this file's log_path
## args below), but never writes to repo-root R/, scripts/, outputs/,
## review/ or docs/. NOT R/utils.R::write_output_metadata() -- see the
## local write_meta() below instead.

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}

GSL_ROOT <- "GSL_definition"

source("R/pipeline_config.R")
source("R/qc.R")
source("R/utils.R")
check_pipeline_config()

## ---- Log paths (never the repo-root outputs/, which scripts/04_qc.R now
## clears at the start of every run) -- see R/utils.R's log_path argument,
## added 2026-10-29 for this side project. ----
GSL_EXCLUSION_LOG <- file.path(GSL_ROOT, "logs", "exclusion_log.csv")
GSL_UNKNOWN_LOG   <- file.path(GSL_ROOT, "logs", "unknown_log.csv")
if (!dir.exists(file.path(GSL_ROOT, "logs"))) {
  dir.create(file.path(GSL_ROOT, "logs"), recursive = TRUE)
}

## ---- Local .meta.json writer -----------------------------------------------
## NOT R/utils.R::write_output_metadata() -- that function always appends to
## the repo-root outputs/session_info.txt, which this side project must
## never write to (same problem already solved the same way in
## TowerTango/code/update_cards.R::write_meta() -- this mirrors it rather
## than modifying R/utils.R a second time, since the framework authorized
## only the one R/utils.R change in step 1, the log_path argument).
write_meta <- function(output_path, input_sources, notes = "") {
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

## ---- Snapshot files -----------------------------------------------------
## "our 1 and 20 September snapshots" (framework.md, "Data required").
GSL_SNAPSHOT_SEP01 <- "data/snapshots/fluxnet_shuttle_snapshot_20260901T094522.csv"
GSL_SNAPSHOT_SEP20 <- "data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv"
GSL_TASH_SNAPSHOT  <- file.path(GSL_ROOT, "data", "tash_snapshot.csv") # may not exist yet

## ---- Sites table ----------------------------------------------------------
## Parsed directly out of docs/framework.md (saved verbatim from the
## framework document) rather than hand-transcribed into R, to avoid a copy
## that could silently drift from the source or contain a typo. Every row's
## year-count is checked against the table's own stated `n` below.

.gsl_parse_years <- function(years_str) {
  tokens <- trimws(strsplit(years_str, ",")[[1]])
  years <- unlist(lapply(tokens, function(tok) {
    if (grepl("-", tok)) {
      bounds <- as.integer(strsplit(tok, "-")[[1]])
      seq(bounds[1], bounds[2])
    } else {
      as.integer(tok)
    }
  }))
  sort(years)
}

.gsl_read_sites_table <- function(framework_path = file.path(GSL_ROOT, "docs", "framework.md")) {
  lines <- readLines(framework_path)
  header_idx <- grep("^\\| Site \\| IGBP \\| Group \\| Network \\| Years used \\| n \\|$", lines)
  if (length(header_idx) != 1) {
    stop("Could not find exactly one Sites table header in ", framework_path)
  }
  i <- header_idx + 2 # skip the header row and the markdown separator row
  rows <- list()
  while (i <= length(lines) && startsWith(lines[i], "|")) {
    cells <- trimws(strsplit(lines[i], "\\|")[[1]])
    cells <- cells[cells != ""]
    rows[[length(rows) + 1]] <- cells
    i <- i + 1
  }
  df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(df) <- c("site_id", "igbp", "group", "network", "years_used_str", "n_stated")
  df$n_stated <- as.integer(df$n_stated)
  df
}

.sites_raw <- .gsl_read_sites_table()
.sites_raw$years <- lapply(.sites_raw$years_used_str, .gsl_parse_years)
.sites_raw$n_computed <- vapply(.sites_raw$years, length, integer(1))

if (!all(.sites_raw$n_computed == .sites_raw$n_stated)) {
  bad <- .sites_raw$site_id[.sites_raw$n_computed != .sites_raw$n_stated]
  stop(
    "Year-count mismatch between the parsed years string and the table's ",
    "own stated n for: ", paste(bad, collapse = ", ")
  )
}

GSL_SITES <- .sites_raw[, c("site_id", "igbp", "group", "network", "years")]
GSL_SITES$n_years <- .sites_raw$n_computed
GSL_SITE_IDS <- GSL_SITES$site_id

## ---- Confirm against the framework's stated totals (stop, don't just warn,
## since everything downstream depends on this table being right). ----
GSL_TOTAL_SITE_YEARS <- sum(GSL_SITES$n_years)
if (GSL_TOTAL_SITE_YEARS != 505L) {
  stop("Expected 505 total site-years from the Sites table, got ", GSL_TOTAL_SITE_YEARS)
}

.group_counts <- table(GSL_SITES$group)
.expected_groups <- c(
  "grassland" = 17L, "evergreen forest" = 20L, "deciduous forest" = 12L,
  "woody-grass mosaic" = 12L, "mixed forest" = 1L
)
if (!identical(setNames(as.integer(.group_counts[names(.expected_groups)]), names(.expected_groups)),
               .expected_groups)) {
  stop(
    "Group counts do not match the framework's stated 17/20/12/12/1. Got: ",
    paste(names(.group_counts), as.integer(.group_counts), sep = "=", collapse = ", ")
  )
}

GSL_SITES_GE10YR <- GSL_SITES$site_id[GSL_SITES$n_years >= 10L]
if (length(GSL_SITES_GE10YR) != 21L) {
  stop("Expected 21 sites with >= 10 years, got ", length(GSL_SITES_GE10YR))
}

GSL_SITES_GE5YR <- GSL_SITES$site_id[GSL_SITES$n_years >= 5L]

message(
  "GSL_definition config loaded: ", nrow(GSL_SITES), " sites, ",
  GSL_TOTAL_SITE_YEARS, " site-years, ", length(GSL_SITES_GE10YR),
  " sites with >= 10 years (confirmed against docs/framework.md)."
)

## ============================================================================
## Stage 2 decisions (2026-10-29, made after Stage 1 at a1633a8, without
## Tash's input -- Dave accepted the risk of a rerun if an assumption proves
## wrong). Each is a named switch with the stated default, not hard-coded
## inline at each use. See docs/framework.md's "Open decisions (resolved)"
## section and docs/methods_memo.md for the reasoning behind each.
## ============================================================================

#' Year screening: option (d) at 80%. GSL is counted on the full gap-filled
#' daily series (GPP_NT_VUT_REF has a value every day); a year is kept when
#' >= 80% of its days pass QC_THRESHOLD_DD via qc_passes(). Stage 1 showed
#' this keeps all 62 sites / 505 site-years -- the project-standard run and
#' Tash's own selection cover the same site-years, so only one run is
#' carried through Stage 2 (not two). "d80" is the only value implemented;
#' kept as a named switch (not a boolean) so a different option can be
#' substituted without restructuring the code that reads it.
GSL_YEAR_SCREEN <- "d80"

#' Negative daily GPP set to 0 for the main run. The as-delivered version is
#' computed alongside for every quantity (not skipped), per the Stage 2
#' instruction, so NEG_GPP_TO_ZERO is a per-run flag passed explicitly to
#' every derivation function in 03_derive.R rather than a single global
#' toggle that silently picks one and discards the other.
GSL_NEG_GPP_TO_ZERO_MAIN <- TRUE
GSL_NEG_GPP_VARIANTS <- c(main = TRUE, as_delivered = FALSE)

#' GSL counts days with GPP > T (strict), not GPP >= T.
GSL_STRICT_GT <- TRUE

#' GPP variable: GPP_NT_VUT_REF for the main run, GPP_DT_VUT_REF for the
#' supplementary version.
GSL_GPP_VARIANTS <- c(main = "GPP_NT_VUT_REF", supplementary = "GPP_DT_VUT_REF")

#' Site/year list: the fixed Sites table (GSL_SITES, already enforced
#' above), not the Tash-rule network-wide reproduction from Stage 1. IT-Tor
#' 2012 is kept (it is already in GSL_SITES's own year list for IT-Tor, and
#' is not removed despite failing Stage 1's "complete year" reproduction
#' check). The 5 extra site-years Stage 1 found passing Tash's rule at
#' already-included sites (CA-Ca1 2004/2010, CH-Dav 2011, DE-Tha 2025,
#' NL-Vkp 2025) are deliberately NOT added -- GSL_SITES is not modified here.

#' Sites flagged for a product_id change between the repo's 1 and 20
#' September snapshots (Stage 1, tables/site_products.csv) -- reported
#' alongside every match-check statistic in Stage 2, with and without them,
#' not silently dropped.
GSL_FLAGGED_PRODUCT_SITES <- c("FR-Mej", "IT-Tor", "NL-Vkp")

message(
  "Stage 2 decisions: YEAR_SCREEN=", GSL_YEAR_SCREEN, ", NEG_GPP_TO_ZERO(main)=",
  GSL_NEG_GPP_TO_ZERO_MAIN, ", STRICT_GT=", GSL_STRICT_GT, ", GPP main=",
  GSL_GPP_VARIANTS["main"], ", GPP supplementary=", GSL_GPP_VARIANTS["supplementary"]
)
