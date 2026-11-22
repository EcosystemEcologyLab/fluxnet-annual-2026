## 14_download_extract_xbase_005_v2.R — Stage 3b step 1: re-download FLUXCOM-X-BASE's 0.05
## degree monthly grid (same 84 files as Stage 3), verify each against the Stage 3 manifest's
## own SHA-256 before trusting it, then extract a 3x3 block of cells (the tower's cell plus its
## eight neighbours) per site instead of Stage 3's single centre cell. One file at a time
## (download -> verify -> extract -> record -> delete), so the global grid never accumulates on
## disk. FLUXCOM-X-BASE is non-Shuttle comparison data (CLAUDE.md Hard Rule 1) -- this is a
## Stage 3b data-package deliverable for Ossenatou Mamadou, not Annual Paper data. Stage 2 and
## Stage 3 outputs are left unchanged.
##
## Source: ICOS Carbon Portal collection zfwf1Ak2I7OlziGDTX8Xl6_T, same DOI as Stage 2/3
## (10.18160/5NZG-JMJE), filename suffix "_005_monthly.nc" (0.05deg). 84 files (GPP, NEE, ET,
## ET_T x 2001-2021).
##
## Pre-flight check (new in Stage 3b): before downloading anything, the freshly-discovered
## SPARQL file list (name, size, sha256) is compared against the Stage 3 manifest
## (tables/fluxcom_x_base_005_download_manifest.csv). Any new/missing file, or any size/sha256
## mismatch, stops the script before a single byte is downloaded -- this re-download is a
## traceability/integrity re-verification, not a blind repeat.
##
## Neighbour grid: for each site, the tower's own 0.05deg cell (row_offset = col_offset = 0,
## same cell Stage 3 used for xbase_005_monthly.csv) plus its eight neighbours
## (row_offset, col_offset in {-1, 0, 1}). terra's row index increases southward and its column
## index increases eastward (standard raster convention, confirmed via rowColFromCell/
## xyFromCell below) -- so row_offset = -1 is one cell north, +1 one cell south; col_offset = -1
## one cell west, +1 one cell east. Cell values are read directly by cell index (`r[cell]`), not
## by a second point-extraction, since the neighbour cell indices are already known exactly from
## the centre cell's row/col.
##
## Download mechanism: identical anonymous CC BY 4.0 licence click-through as Stage 2/3 -- no
## account/login/credential. Never prints or stores credentials.
##
## Site coordinates: same source as Stage 3, the 20 September 2026 FLUXNET Shuttle snapshot.
##
## Resumable: a file already recorded as extracted = TRUE in this script's own v2 manifest is
## skipped, so an interrupted run can simply be re-run.
##
## Output (gitignored, deleted as it is consumed):
##   data/external/fluxcom_x_base_005/<file>.nc (one at a time)
## Output (git-tracked):
##   tables/fluxcom_x_base_005_download_manifest_v2.csv   (same schema as Stage 3's manifest)
##   tables/xbase_005_neighbours_extraction_raw.csv        (site x cell(3x3) x variable x year x month)
##   tables/xbase_provenance.csv                           (one row per source file, package-facing)

source("WAFNET/fluxcom_comparison/code/00_config.R")
suppressPackageStartupMessages(library(terra))

STAGE3_SITES <- c("GH-Ank", "BJ-Nhu", "BJ-Bfg", "SN-Dhr", "ML-Kem", "SN-Nkr")

XBASE005_DIR <- file.path("data", "external", "fluxcom_x_base_005")
COOKIE_JAR   <- file.path(XBASE005_DIR, ".icos_cookies")
dir.create(XBASE005_DIR, recursive = TRUE, showWarnings = FALSE)

STAGE3_MANIFEST <- file.path(FLUXCOM_ROOT, "tables", "fluxcom_x_base_005_download_manifest.csv")
MANIFEST_V2     <- file.path(FLUXCOM_ROOT, "tables", "fluxcom_x_base_005_download_manifest_v2.csv")
RAW_OUT         <- file.path(FLUXCOM_ROOT, "tables", "xbase_005_neighbours_extraction_raw.csv")
PROVENANCE_OUT  <- file.path(FLUXCOM_ROOT, "tables", "xbase_provenance.csv")

if (!file.exists(STAGE3_MANIFEST)) {
  stop("[14_download_extract_xbase_005_v2] STOP: Stage 3 manifest not found at ", STAGE3_MANIFEST,
       " -- cannot verify against it. Not proceeding.")
}
stage3_manifest <- utils::read.csv(STAGE3_MANIFEST, stringsAsFactors = FALSE)

DOI             <- "10.18160/5NZG-JMJE"
SPARQL_ENDPOINT <- "https://meta.icos-cp.eu/sparql"
CITATION_DATASET <- "Nelson, J.A., Walther, S., Jung, M., Gans, F., Kraft, B., Weber, U., Hamdi, Z., Duveiller, G., Zhang, W. (2023). FLUXCOM-X-BASE. ICOS ERIC - Carbon Portal. https://doi.org/10.18160/5NZG-JMJE."
CITATION_PAPER   <- "Nelson, J.A. et al. (2024). Technical note: Rescaling FLUXCOM-X-BASE as a benchmark for eddy-covariance-based carbon and energy flux estimates. Biogeosciences 21, 5079-5115. https://doi.org/10.5194/bg-21-5079-2024."
LICENCE  <- "CC BY 4.0 (ICOS Data Licence, https://creativecommons.org/licenses/by/4.0/)"

## ---- site coordinates: 20 September 2026 snapshot, same as Stage 3 ------------------------
SNAPSHOT_PATH <- "data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv"
snap <- utils::read.csv(SNAPSHOT_PATH, stringsAsFactors = FALSE)
site_snapshot_coords <- function(site_id) {
  row <- snap[snap$site_id == site_id, ]
  if (nrow(row) == 0) stop("[14_download_extract_xbase_005_v2] Site not in snapshot: ", site_id)
  c(lon = as.numeric(row$location_long[1]), lat = as.numeric(row$location_lat[1]))
}

haversine_km <- function(lon1, lat1, lon2, lat2) {
  to_rad <- function(x) x * pi / 180
  R_EARTH_KM <- 6371.0088
  dlat <- to_rad(lat2 - lat1); dlon <- to_rad(lon2 - lon1)
  a <- sin(dlat / 2)^2 + cos(to_rad(lat1)) * cos(to_rad(lat2)) * sin(dlon / 2)^2
  2 * R_EARTH_KM * asin(pmin(1, sqrt(a)))
}

## ---- discover the 84-file list via SPARQL, same pattern as Stage 3 (code/11_...) -----------
query <- sprintf('PREFIX cpmeta: <http://meta.icos-cp.eu/ontologies/cpmeta/>
SELECT ?dobj ?fileName ?size ?sha256 WHERE {
  ?dobj cpmeta:hasDoi "%s" .
  ?dobj cpmeta:hasName ?fileName .
  ?dobj cpmeta:hasSizeInBytes ?size .
  ?dobj cpmeta:hasSha256sum ?sha256 .
  FILTER(CONTAINS(?fileName, "_005_monthly.nc"))
}
ORDER BY ?fileName', DOI)

tmp_json <- tempfile(fileext = ".json")
# system2() assembles these into a single shell command line, not an argv array -- every
# argument that can contain shell-special characters must be shQuote()'d (Stage 3's own
# first run broke on exactly this before being fixed; see code/11_... header comment).
system2("curl", c("-s", "-G", shQuote(SPARQL_ENDPOINT),
                   "--data-urlencode", shQuote(paste0("query=", query)),
                   "-H", shQuote("Accept: application/sparql-results+json"),
                   "-o", shQuote(tmp_json)))
sparql_result <- jsonlite::fromJSON(tmp_json, simplifyVector = FALSE)
bindings <- sparql_result$results$bindings
if (length(bindings) < 1) stop("[14_download_extract_xbase_005_v2] SPARQL query returned no files. Stopping without downloading.")

files_df <- do.call(rbind, lapply(bindings, function(b) {
  data.frame(
    fileName   = b$fileName$value,
    hash       = sub(".*/", "", b$dobj$value),
    size       = as.numeric(b$size$value),
    sha256_b64 = b$sha256$value,
    stringsAsFactors = FALSE
  )
}))
files_df$variable <- sub("^(.+)_[0-9]{4}_005_monthly\\.nc$", "\\1", files_df$fileName)
files_df$year     <- as.integer(sub("^.+_([0-9]{4})_005_monthly\\.nc$", "\\1", files_df$fileName))
files_df$source_key <- paste0(files_df$variable, "_", files_df$year, "_005")
files_df <- files_df[order(files_df$fileName), ]

message("[14_download_extract_xbase_005_v2] SPARQL returned ", nrow(files_df), " files, ",
        round(sum(files_df$size) / 1e9, 2), " GB total.")
if (nrow(files_df) != 84) {
  message("[14_download_extract_xbase_005_v2] WARNING: expected 84 files (GPP/NEE/ET/ET_T x 2001-2021), got ", nrow(files_df))
}

## ---- pre-flight: verify every discovered file against the Stage 3 manifest BEFORE any
## download. Stop (no download attempted) if anything is missing or does not match. -----------
mismatches <- list()
for (i in seq_len(nrow(files_df))) {
  f <- files_df[i, ]
  s3 <- stage3_manifest[stage3_manifest$filename == f$fileName, ]
  if (nrow(s3) == 0) {
    mismatches[[length(mismatches) + 1]] <- paste0(f$fileName, ": not present in Stage 3 manifest")
    next
  }
  s3_sha <- sub("=+$", "", s3$sha256_base64[1])
  f_sha  <- sub("=+$", "", f$sha256_b64)
  if (s3$expected_size_bytes[1] != f$size) {
    mismatches[[length(mismatches) + 1]] <- paste0(
      f$fileName, ": size differs (Stage 3 manifest ", s3$expected_size_bytes[1], " vs. now ", f$size, ")")
  }
  if (!identical(s3_sha, f_sha)) {
    mismatches[[length(mismatches) + 1]] <- paste0(
      f$fileName, ": sha256 differs (Stage 3 manifest ", s3_sha, " vs. now ", f_sha, ")")
  }
}
if (length(mismatches) > 0) {
  stop("[14_download_extract_xbase_005_v2] STOP: ", length(mismatches),
       " file(s) differ from the Stage 3 manifest. Not downloading. Details:\n",
       paste(unlist(mismatches), collapse = "\n"))
}
message("[14_download_extract_xbase_005_v2] Pre-flight check OK: all ", nrow(files_df),
        " discovered files match the Stage 3 manifest (filename, size, sha256). Proceeding.")

# Test hook only (default off)
test_limit <- as.integer(Sys.getenv("WAFNET_STAGE3B_TEST_LIMIT", "0"))
if (test_limit > 0) {
  files_df <- head(files_df, test_limit)
  message("[14_download_extract_xbase_005_v2] TEST MODE: WAFNET_STAGE3B_TEST_LIMIT=", test_limit)
}

## ---- checksum helpers -----------------------------------------------------------------------
compute_sha256_b64 <- function(path) {
  out <- system(sprintf("openssl dgst -sha256 -binary %s | openssl base64 -A", shQuote(path)), intern = TRUE)
  sub("=+$", "", out)
}
compute_sha256_hex <- function(path) {
  out <- system(sprintf("openssl dgst -sha256 %s", shQuote(path)), intern = TRUE)
  sub("^.*= ", "", out)
}

looks_like_html <- function(path) {
  con <- file(path, "rb")
  head_bytes <- readBin(con, "raw", n = 512)
  close(con)
  head_txt <- tolower(rawToChar(head_bytes[head_bytes != 0], multiple = FALSE))
  grepl("<html|<!doctype html", head_txt, fixed = FALSE)
}

## ---- manifest/raw-table resume support -------------------------------------------------------
already_extracted <- character(0)
if (file.exists(MANIFEST_V2)) {
  m0 <- utils::read.csv(MANIFEST_V2, stringsAsFactors = FALSE)
  already_extracted <- m0$filename[m0$extracted == TRUE]
  message("[14_download_extract_xbase_005_v2] Resuming: ", length(already_extracted), " files already extracted.")
}

append_manifest_row <- function(row_df) {
  utils::write.table(row_df, MANIFEST_V2, sep = ",", row.names = FALSE,
                      col.names = !file.exists(MANIFEST_V2), append = file.exists(MANIFEST_V2))
}
append_raw_rows <- function(df) {
  utils::write.table(df, RAW_OUT, sep = ",", row.names = FALSE,
                      col.names = !file.exists(RAW_OUT), append = file.exists(RAW_OUT))
}
append_provenance_row <- function(row_df) {
  utils::write.table(row_df, PROVENANCE_OUT, sep = ",", row.names = FALSE,
                      col.names = !file.exists(PROVENANCE_OUT), append = file.exists(PROVENANCE_OUT))
}

## ---- main loop: one global file at a time ----------------------------------------------------
n_total <- nrow(files_df)
for (i in seq_len(n_total)) {
  f <- files_df[i, ]
  if (f$fileName %in% already_extracted) {
    message(sprintf("[%d/%d] SKIP (already extracted): %s", i, n_total, f$fileName))
    next
  }
  message(sprintf("[%d/%d] %s %d: %s (%.1f MB)", i, n_total, f$variable, f$year, f$fileName, f$size / 1e6))

  local_path <- file.path(XBASE005_DIR, f$fileName)
  tmp_path   <- paste0(local_path, ".tmp")
  access_url <- paste0("https://data.icos-cp.eu/objects/", f$hash)
  pid_url    <- paste0("https://meta.icos-cp.eu/objects/", f$hash)
  download_datetime_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  system2("curl", c("-s", "-c", shQuote(COOKIE_JAR), "-b", shQuote(COOKIE_JAR), "-L",
                     shQuote(paste0("https://data.icos-cp.eu/licence_accept?ids=%5B%22", f$hash, "%22%5D")),
                     "-o", "/dev/null"))
  system2("curl", c("-s", "-c", shQuote(COOKIE_JAR), "-b", shQuote(COOKIE_JAR), "-L",
                     "-o", shQuote(tmp_path), shQuote(access_url)))

  actual_size <- suppressWarnings(file.info(tmp_path)$size)
  if (is.na(actual_size)) stop("[14_download_extract_xbase_005_v2] Download produced no file for ", f$fileName)

  if (actual_size != f$size) {
    if (actual_size < 100000 && looks_like_html(tmp_path)) {
      stop("[14_download_extract_xbase_005_v2] STOP: ", f$fileName,
           " came back as an HTML page, not data -- this looks like an unexpected login/redirect wall. ",
           "No credential of any kind has been printed or stored. Reporting rather than proceeding.")
    }
    file.remove(tmp_path)
    stop("[14_download_extract_xbase_005_v2] Size mismatch for ", f$fileName,
         ": expected ", f$size, ", got ", actual_size)
  }

  actual_sha256_b64 <- compute_sha256_b64(tmp_path)
  expected_sha256_b64 <- sub("=+$", "", f$sha256_b64)
  sha256_verified <- identical(actual_sha256_b64, expected_sha256_b64)
  if (!sha256_verified) {
    file.remove(tmp_path)
    stop("[14_download_extract_xbase_005_v2] sha256 mismatch for ", f$fileName,
         " (expected ", expected_sha256_b64, ", got ", actual_sha256_b64, ")")
  }
  sha256_hex <- compute_sha256_hex(tmp_path)
  file.rename(tmp_path, local_path)
  message("  OK: downloaded and sha256-verified (", round(actual_size / 1e6, 1), " MB)")

  ## ---- extract the 3x3 neighbourhood (9 cells) per site, all 12 months ---------------------
  r <- rast(local_path)[[1:12]]

  site_rows <- lapply(STAGE3_SITES, function(site_id) {
    coords <- site_snapshot_coords(site_id)
    cell_centre <- cellFromXY(r, cbind(coords["lon"], coords["lat"]))
    rc_centre <- rowColFromCell(r, cell_centre)

    offsets <- expand.grid(row_offset = -1:1, col_offset = -1:1)
    rows_i <- lapply(seq_len(nrow(offsets)), function(k) {
      dr <- offsets$row_offset[k]; dc <- offsets$col_offset[k]
      cell_n <- cellFromRowCol(r, rc_centre[1, 1] + dr, rc_centre[1, 2] + dc)
      xy_n <- xyFromCell(r, cell_n)
      vals <- as.numeric(r[cell_n])
      dist_km <- haversine_km(coords["lon"], coords["lat"], xy_n[1, 1], xy_n[1, 2])
      data.frame(
        site_id = site_id, variable = f$variable, year = f$year, month = 1:12,
        row_offset = dr, col_offset = dc,
        value = vals,
        tower_lon = coords["lon"], tower_lat = coords["lat"],
        cell_lon = xy_n[1, 1], cell_lat = xy_n[1, 2],
        cell_dist_km = dist_km,
        source_key = f$source_key
      )
    })
    do.call(rbind, rows_i)
  })
  append_raw_rows(do.call(rbind, site_rows))

  manifest_row <- data.frame(
    variable = f$variable, year = f$year, resolution = "0.05deg", temporal_resolution = "monthly",
    filename = f$fileName, object_hash = f$hash, doi = DOI, access_url = access_url,
    expected_size_bytes = f$size, actual_size_bytes = actual_size,
    sha256_base64 = f$sha256_b64, sha256_hex = sha256_hex, sha256_verified = sha256_verified,
    download_datetime_utc = download_datetime_utc, licence = LICENCE,
    citation_dataset = CITATION_DATASET, citation_paper = CITATION_PAPER,
    extracted = TRUE, n_sites_extracted = length(STAGE3_SITES), n_cells_per_site = 9
  )
  append_manifest_row(manifest_row)

  provenance_row <- data.frame(
    source_key = f$source_key, variable = f$variable, year = f$year, file_name = f$fileName,
    icos_object_pid_url = pid_url, icos_access_url = access_url, collection_doi = DOI,
    sha256_base64 = f$sha256_b64, sha256_hex = sha256_hex,
    download_datetime_utc = download_datetime_utc, licence = LICENCE
  )
  append_provenance_row(provenance_row)

  file.remove(local_path)
  message("  Extracted ", length(STAGE3_SITES), " sites x 9 cells, deleted global file.")
}

message("\n[14_download_extract_xbase_005_v2] Done. Manifest: ", MANIFEST_V2,
        "\nRaw neighbours extraction table: ", RAW_OUT, "\nProvenance: ", PROVENANCE_OUT)

write_meta(
  MANIFEST_V2,
  input_sources = c("https://meta.icos-cp.eu/sparql (ICOS metadata SPARQL endpoint)",
                     "https://data.icos-cp.eu/objects/<hash> (per-file download, after licence_accept)",
                     STAGE3_MANIFEST),
  notes = paste0(
    "Stage 3b re-download of the same 84 FLUXCOM-X-BASE 0.05deg monthly files as Stage 3 ",
    "(fluxcom_x_base_005_download_manifest.csv), pre-flight-verified byte-identical (filename, ",
    "size, sha256) against that Stage 3 manifest before any download was attempted. This time ",
    "extracts a 3x3 block of cells (9 cells: the tower's own cell plus its eight neighbours) per ",
    "site instead of Stage 3's single centre cell. Adds sha256_hex (standard sha256sum/shasum -a ",
    "256-compatible hex digest, computed locally) and the corrected two-part citation ",
    "(citation_dataset / citation_paper) alongside Stage 3's single combined citation field."
  )
)
write_meta(
  RAW_OUT,
  input_sources = "data/external/fluxcom_x_base_005/*.nc (consumed and deleted one at a time; see fluxcom_x_base_005_download_manifest_v2.csv)",
  notes = paste0(
    "One row per site x cell(3x3=9) x variable x year x month (STAGE3_SITES: GH-Ank, BJ-Nhu, ",
    "BJ-Bfg, SN-Dhr, ML-Kem, SN-Nkr). row_offset/col_offset in {-1,0,1}; (0,0) is the tower's own ",
    "cell (the same cell Stage 3 used for xbase_005_monthly.csv). terra's row index increases ",
    "southward and column index increases eastward (standard raster convention) -- row_offset=-1 ",
    "is one cell north, +1 one cell south; col_offset=-1 one cell west, +1 one cell east. Site ",
    "coordinates from the 20 September 2026 FLUXNET Shuttle snapshot, same as Stage 3. value is in ",
    "the product's native units as distributed (gC m-2 d-1 for GPP/NEE, mm hr-1 for ET/ET_T) -- ",
    "unit matching and LE conversion happen downstream in the packaging script. source_key links ",
    "every row to its exact source file in xbase_provenance.csv."
  )
)
write_meta(
  PROVENANCE_OUT,
  input_sources = c("https://meta.icos-cp.eu/sparql (ICOS metadata SPARQL endpoint)", STAGE3_MANIFEST),
  notes = paste0(
    "One row per FLUXCOM-X-BASE source file (84: GPP/NEE/ET/ET_T x 2001-2021), keyed by ",
    "source_key (variable_year_005), for package-level traceability. icos_object_pid_url is the ",
    "object's ICOS metadata landing page (meta.icos-cp.eu/objects/<hash>, a persistent identifier); ",
    "icos_access_url is the direct data download endpoint used here (data.icos-cp.eu/objects/<hash>, ",
    "requires the same anonymous licence-acceptance cookie as icos_object_pid_url does not). ",
    "sha256_hex is standard sha256sum/shasum -a 256-compatible; sha256_base64 is ICOS's own ",
    "reported checksum encoding, kept so both can be cross-checked against a fresh download from ",
    "either angle."
  )
)

cat("\n[14_download_extract_xbase_005_v2] ", n_total, " files processed.\n")
