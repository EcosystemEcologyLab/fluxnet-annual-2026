## 15_build_tower_provenance.R — Stage 3b step 2a: tower_provenance.csv.
##
## One row per site, tracing the tower data used throughout this analysis back to its exact
## source file, persistent identifier, and FLUXNET Shuttle snapshot row. READ-ONLY against the
## repo-root data/extracted/ and data/snapshots/ -- nothing here downloads or modifies tower
## data. If any site's local extracted file does not match the version/product identifier
## recorded in the snapshot, this script stops rather than silently proceeding (per the Stage 3b
## prompt: "do not substitute or re-download tower data without asking").
##
## Output (git-tracked):
##   tables/tower_provenance.csv

source("WAFNET/fluxcom_comparison/code/00_config.R")

STAGE3_SITES <- c("GH-Ank", "BJ-Nhu", "BJ-Bfg", "SN-Dhr", "ML-Kem", "SN-Nkr")

SNAPSHOT_PATH <- "data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv"
snap <- utils::read.csv(SNAPSHOT_PATH, stringsAsFactors = FALSE)

FLUXNET_SHUTTLE_VERSION <- Sys.getenv("FLUXNET_SHUTTLE_VERSION", unset = NA_character_)
if (is.na(FLUXNET_SHUTTLE_VERSION)) stop("[15_build_tower_provenance] FLUXNET_SHUTTLE_VERSION is not set.")

carbon_src_df <- utils::read.csv(file.path(FLUXCOM_ROOT, "tables", "site_carbon_source.csv"), stringsAsFactors = FALSE)
carbon_src_for <- function(site_id) carbon_src_df$carbon_src[carbon_src_df$site_id == site_id]

compute_sha256_hex <- function(path) {
  out <- system(sprintf("openssl dgst -sha256 %s", shQuote(path)), intern = TRUE)
  sub("^.*= ", "", out)
}

rows <- list()
mismatches <- character(0)

for (site_id in STAGE3_SITES) {
  snap_idx <- which(snap$site_id == site_id)
  if (length(snap_idx) != 1) stop("[15_build_tower_provenance] Expected exactly one snapshot row for ", site_id, ", found ", length(snap_idx))
  snap_row <- snap[snap_idx, ]

  local_dir_name <- WAFNET_SITE_DIRS[[site_id]]
  snap_product_name <- sub("[.]zip$", "", snap_row$fluxnet_product_name[1])

  fluxmet_path <- wafnet_fluxmet_path(site_id, "MM")
  fluxmet_file_name <- basename(fluxmet_path)

  ## -- the check the Stage 3b prompt requires: local extracted directory/file must match the
  ## version and product identifier recorded in the snapshot. STOP, do not substitute, if not. --
  version_match <- identical(local_dir_name, snap_product_name)
  if (!version_match) {
    mismatches <- c(mismatches, sprintf(
      "%s: local extracted dir '%s' does not match snapshot fluxnet_product_name '%s'",
      site_id, local_dir_name, snap_product_name
    ))
    next
  }

  product_version <- sub("^.*_(v[0-9]+[.][0-9]+_r[0-9]+)$", "\\1", local_dir_name)

  rows[[length(rows) + 1]] <- data.frame(
    site_id = site_id,
    site_name = snap_row$site_name[1],
    dataset_pid = snap_row$product_id[1],
    dataset_pid_url = paste0("https://hdl.handle.net/11676/", snap_row$product_id[1]),
    dataset_citation = snap_row$product_citation[1],
    product_version = product_version,
    oneflux_code_version = snap_row$oneflux_code_version[1],
    fluxmet_mm_file_name = fluxmet_file_name,
    fluxmet_mm_file_sha256_hex = compute_sha256_hex(fluxmet_path),
    snapshot_file = basename(SNAPSHOT_PATH),
    snapshot_row_number = snap_idx,
    fluxnet_shuttle_version = FLUXNET_SHUTTLE_VERSION,
    local_file_mtime_utc = format(file.info(fluxmet_path)$mtime, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    carbon_src = carbon_src_for(site_id),
    snapshot_version_match = version_match
  )
}

if (length(mismatches) > 0) {
  stop("[15_build_tower_provenance] STOP: ", length(mismatches), " site(s) do not match the snapshot. ",
       "Not substituting or re-downloading tower data without asking. Details:\n",
       paste(mismatches, collapse = "\n"))
}

tower_provenance <- do.call(rbind, rows)
OUT <- file.path(FLUXCOM_ROOT, "tables", "tower_provenance.csv")
utils::write.csv(tower_provenance, OUT, row.names = FALSE)
write_meta(
  OUT,
  input_sources = c(SNAPSHOT_PATH,
                     file.path(ANNUAL_PAPER_EXTRACTED_DIR, WAFNET_SITE_DIRS[STAGE3_SITES], "*_FLUXMET_MM_*.csv (read-only)")),
  notes = paste0(
    "One row per site, tracing the tower monthly data used throughout Stage 2/3/3b back to its ",
    "exact source file and persistent identifier. dataset_pid/dataset_pid_url/dataset_citation are ",
    "read directly from the snapshot's product_id/product_citation fields (not reconstructed). ",
    "product_version is parsed from the local extracted directory name (e.g. v1.3_r1); ",
    "oneflux_code_version is the snapshot's own oneflux_code_version field (v1.3 for all six ",
    "sites) -- these are two different version concepts (ONEFlux processing-code version vs. the ",
    "FLUXNET Archive product release/revision) that happen to share a '1.3' here. ",
    "fluxmet_mm_file_sha256_hex is a standard sha256sum/shasum -a 256-compatible digest of the ",
    "exact local file read throughout this analysis. local_file_mtime_utc is this repository's own ",
    "copy's filesystem timestamp (all six sites: 2026-06-01), NOT necessarily the original ICOS/",
    "Shuttle download moment -- the snapshot CSV (20 September 2026) is a later metadata listing of ",
    "the same, already-downloaded, immutable-by-PID product; snapshot_version_match confirms the ",
    "local directory name and the snapshot's fluxnet_product_name agree (TRUE for all six sites; ",
    "the script stops rather than proceeding if any site does not match)."
  )
)

cat("\n[15_build_tower_provenance] tower_provenance.csv:\n")
print(tower_provenance[, c("site_id", "dataset_pid", "product_version", "oneflux_code_version", "carbon_src", "snapshot_version_match")])
cat("\nWrote ", OUT, "\n")
