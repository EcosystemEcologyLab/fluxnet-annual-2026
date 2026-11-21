## 11_download_extract_xbase_005.R — Stage 3 steps 1-2: download FLUXCOM-X-BASE at its
## native 0.05 degree monthly grid and extract the six WAF-Net site cells, one global file
## at a time (download -> extract -> record in manifest -> delete), so the global grid never
## accumulates on disk.
##
## FLUXCOM-X-BASE is non-Shuttle comparison data (CLAUDE.md Hard Rule 1) -- this is a Stage 3
## data-package deliverable for Ossenatou Mamadou's WAF-Net paper, not Annual Paper data.
## Stage 2 outputs (tables/, figures/, docs/report_back_2026-11-03.md) are left unchanged.
##
## Source: ICOS Carbon Portal collection zfwf1Ak2I7OlziGDTX8Xl6_T, same DOI as Stage 2's
## 0.5deg pull (10.18160/5NZG-JMJE) -- the file-naming decimal-degree code distinguishes
## resolution within one DOI ("050" = 0.5deg, Stage 2; "005" = 0.05deg, here). 84 files
## (GPP, NEE, ET, ET_T x 2001-2021), confirmed via the same SPARQL discovery pattern as
## 04_download_xbase.sh, ~433 MB/file, ~36.9 GB total -- about 85x Stage 2's 0.5deg volume,
## consistent with 0.05deg having 100x the grid cells of 0.5deg over the same global extent.
##
## Download mechanism: identical anonymous CC BY 4.0 licence click-through as Stage 2
## (GET /licence_accept?ids=["<hash>"] sets a cookie, then GET /objects/<hash> serves the
## file) -- no account/login/credential, confirmed again here (same ICOS mechanism, same DOI).
## If a response looks like an HTML page instead of a NetCDF file (the Earthdata-style silent
## auth-failure signature documented in CLAUDE.md's NASA Earthdata section, checked here even
## though ICOS itself needs no login, as a defensive guard), the script stops rather than
## treating it as a valid download.
##
## Site coordinates for extraction come from the 20 September 2026 FLUXNET Shuttle snapshot
## (data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv) -- per the Stage 3 prompt,
## not the BIF-derived site_coords_for() Stage 2 used (00_config.R) -- compared in the grid
## check below.
##
## Resumable: a file already recorded as extracted = TRUE in the manifest is skipped, so an
## interrupted run can simply be re-run.
##
## Output (gitignored, deleted as it is consumed):
##   data/external/fluxcom_x_base_005/<file>.nc (one at a time)
## Output (git-tracked):
##   tables/fluxcom_x_base_005_download_manifest.csv  (file, size, checksum, download date, licence, citation)
##   tables/xbase_005_site_extraction_raw.csv          (site x variable x year x month, long format)
##   tables/xbase_005_grid_check.csv                   (one site/month 0.05deg-vs-0.5deg aggregation check)

source("WAFNET/fluxcom_comparison/code/00_config.R")
suppressPackageStartupMessages(library(terra))

STAGE3_SITES <- c("GH-Ank", "BJ-Nhu", "BJ-Bfg", "SN-Dhr", "ML-Kem", "SN-Nkr")

XBASE005_DIR   <- file.path("data", "external", "fluxcom_x_base_005")
XBASE050_DIR   <- FLUXCOM_XBASE_DIR   # Stage 2's 0.5deg files, still on disk, read-only here
COOKIE_JAR     <- file.path(XBASE005_DIR, ".icos_cookies")
dir.create(XBASE005_DIR, recursive = TRUE, showWarnings = FALSE)

MANIFEST       <- file.path(FLUXCOM_ROOT, "tables", "fluxcom_x_base_005_download_manifest.csv")
RAW_OUT        <- file.path(FLUXCOM_ROOT, "tables", "xbase_005_site_extraction_raw.csv")
GRID_CHECK_OUT <- file.path(FLUXCOM_ROOT, "tables", "xbase_005_grid_check.csv")

DOI              <- "10.18160/5NZG-JMJE"
SPARQL_ENDPOINT  <- "https://meta.icos-cp.eu/sparql"
CITATION <- "Nelson, J.A., Walther, S., Jung, M., Gans, F., Kraft, B., Weber, U., Hamdi, Z., Duveiller, G., Zhang, W. (2023). FLUXCOM-X-BASE. ICOS ERIC - Carbon Portal. https://doi.org/10.18160/5NZG-JMJE. Method/product paper: Nelson et al. (2024), Biogeosciences 21:5079-5115, doi:10.5194/bg-21-5079-2024."
LICENCE  <- "CC BY 4.0 (ICOS Data Licence, https://creativecommons.org/licenses/by/4.0/)"

## ---- site coordinates: 20 September 2026 snapshot, per the Stage 3 prompt -----------------
SNAPSHOT_PATH <- "data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv"
snap <- utils::read.csv(SNAPSHOT_PATH, stringsAsFactors = FALSE)
site_snapshot_coords <- function(site_id) {
  row <- snap[snap$site_id == site_id, ]
  if (nrow(row) == 0) stop("[11_download_extract_xbase_005] Site not in snapshot: ", site_id)
  c(lon = as.numeric(row$location_long[1]), lat = as.numeric(row$location_lat[1]))
}

## ---- great-circle distance, km (base R only -- geosphere is not in the renv lockfile and
## CLAUDE.md asks not to add new package dependencies without discussion) --------------------
haversine_km <- function(lon1, lat1, lon2, lat2) {
  to_rad <- function(x) x * pi / 180
  R_EARTH_KM <- 6371.0088
  dlat <- to_rad(lat2 - lat1); dlon <- to_rad(lon2 - lon1)
  a <- sin(dlat / 2)^2 + cos(to_rad(lat1)) * cos(to_rad(lat2)) * sin(dlon / 2)^2
  2 * R_EARTH_KM * asin(pmin(1, sqrt(a)))
}

## ---- discover the 84-file list via SPARQL, same pattern as 04_download_xbase.sh, filtered
## on "_005_monthly.nc" (0.05deg) instead of "_050_monthly.nc" (Stage 2's 0.5deg) ------------
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
# system2() assembles these into a single shell command line (not an exec() argv array),
# so any argument with shell-special characters (this query has newlines, <, >, ") must be
# shQuote()'d -- the first run of this script, without quoting, broke right here before any
# download started (confirmed: no manifest row, no file under data/external/fluxcom_x_base_005/).
system2("curl", c("-s", "-G", shQuote(SPARQL_ENDPOINT),
                   "--data-urlencode", shQuote(paste0("query=", query)),
                   "-H", shQuote("Accept: application/sparql-results+json"),
                   "-o", shQuote(tmp_json)))
sparql_result <- jsonlite::fromJSON(tmp_json, simplifyVector = FALSE)
bindings <- sparql_result$results$bindings
if (length(bindings) < 1) stop("[11_download_extract_xbase_005] SPARQL query returned no files. Stopping without downloading.")

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
files_df <- files_df[order(files_df$fileName), ]
message("[11_download_extract_xbase_005] SPARQL returned ", nrow(files_df), " files, ",
        round(sum(files_df$size) / 1e9, 2), " GB total.")
if (nrow(files_df) != 84) {
  message("[11_download_extract_xbase_005] WARNING: expected 84 files (GPP/NEE/ET/ET_T x 2001-2021), got ", nrow(files_df))
}

# Test hook only (default off): WAFNET_STAGE3_TEST_LIMIT=N restricts the run to the first N
# files, for a quick end-to-end smoke test before committing to the full ~36.9 GB run.
test_limit <- as.integer(Sys.getenv("WAFNET_STAGE3_TEST_LIMIT", "0"))
if (test_limit > 0) {
  files_df <- head(files_df, test_limit)
  message("[11_download_extract_xbase_005] TEST MODE: WAFNET_STAGE3_TEST_LIMIT=", test_limit)
}

## ---- checksum helper (shells out to openssl, same as 04_download_xbase.sh, so the base64
## no-padding comparison stays identical to Stage 2's) ----------------------------------------
compute_sha256_b64 <- function(path) {
  out <- system(sprintf("openssl dgst -sha256 -binary %s | openssl base64 -A", shQuote(path)), intern = TRUE)
  sub("=+$", "", out)
}

## ---- HTML-login-page guard (defensive; ICOS needs no login, confirmed again below, but a
## malformed/garbage response should never be treated as a valid NetCDF file) ----------------
looks_like_html <- function(path) {
  con <- file(path, "rb")
  head_bytes <- readBin(con, "raw", n = 512)
  close(con)
  head_txt <- tolower(rawToChar(head_bytes[head_bytes != 0], multiple = FALSE))
  grepl("<html|<!doctype html", head_txt, fixed = FALSE)
}

## ---- manifest: resume support -- skip files already marked extracted = TRUE ---------------
manifest_cols <- c("variable", "year", "resolution", "temporal_resolution", "filename",
                    "object_hash", "doi", "access_url", "expected_size_bytes", "actual_size_bytes",
                    "sha256_base64", "sha256_verified", "download_datetime_utc", "licence",
                    "citation", "extracted", "n_sites_extracted")
already_extracted <- character(0)
if (file.exists(MANIFEST)) {
  m0 <- utils::read.csv(MANIFEST, stringsAsFactors = FALSE)
  already_extracted <- m0$filename[m0$extracted == TRUE]
  message("[11_download_extract_xbase_005] Resuming: ", length(already_extracted), " files already extracted.")
}

append_manifest_row <- function(row_df) {
  utils::write.table(row_df, MANIFEST, sep = ",", row.names = FALSE,
                      col.names = !file.exists(MANIFEST), append = file.exists(MANIFEST))
}
append_raw_rows <- function(df) {
  utils::write.table(df, RAW_OUT, sep = ",", row.names = FALSE,
                      col.names = !file.exists(RAW_OUT), append = file.exists(RAW_OUT))
}

grid_check_done <- file.exists(GRID_CHECK_OUT)

## ---- main loop: one global file at a time -------------------------------------------------
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
  download_datetime_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  # anonymous CC BY 4.0 licence click-through, identical mechanism to Stage 2 -- args shQuote()'d,
  # see the SPARQL call above for why (system2() builds a shell command line, not an argv array)
  system2("curl", c("-s", "-c", shQuote(COOKIE_JAR), "-b", shQuote(COOKIE_JAR), "-L",
                     shQuote(paste0("https://data.icos-cp.eu/licence_accept?ids=%5B%22", f$hash, "%22%5D")),
                     "-o", "/dev/null"))
  system2("curl", c("-s", "-c", shQuote(COOKIE_JAR), "-b", shQuote(COOKIE_JAR), "-L",
                     "-o", shQuote(tmp_path), shQuote(access_url)))

  actual_size <- suppressWarnings(file.info(tmp_path)$size)
  if (is.na(actual_size)) stop("[11_download_extract_xbase_005] Download produced no file for ", f$fileName)

  if (actual_size != f$size) {
    if (actual_size < 100000 && looks_like_html(tmp_path)) {
      stop("[11_download_extract_xbase_005] STOP: ", f$fileName,
           " came back as an HTML page, not data -- this looks like an unexpected login/redirect wall. ",
           "No credential of any kind has been printed or stored. Reporting rather than proceeding.")
    }
    file.remove(tmp_path)
    stop("[11_download_extract_xbase_005] Size mismatch for ", f$fileName,
         ": expected ", f$size, ", got ", actual_size)
  }

  actual_sha256_b64 <- compute_sha256_b64(tmp_path)
  expected_sha256_b64 <- sub("=+$", "", f$sha256_b64)
  sha256_verified <- identical(actual_sha256_b64, expected_sha256_b64)
  if (!sha256_verified) {
    file.remove(tmp_path)
    stop("[11_download_extract_xbase_005] sha256 mismatch for ", f$fileName,
         " (expected ", expected_sha256_b64, ", got ", actual_sha256_b64, ")")
  }
  file.rename(tmp_path, local_path)
  message("  OK: downloaded and sha256-verified (", round(actual_size / 1e6, 1), " MB)")

  ## ---- extract the six site cells, all 12 months -------------------------------------------
  r <- rast(local_path)[[1:12]]

  site_rows <- lapply(STAGE3_SITES, function(site_id) {
    coords <- site_snapshot_coords(site_id)
    pt <- vect(data.frame(lon = coords["lon"], lat = coords["lat"]), geom = c("lon", "lat"), crs = "EPSG:4326")
    vals <- as.numeric(extract(r, pt)[1, -1])
    cell_n <- cellFromXY(r, cbind(coords["lon"], coords["lat"]))
    cell_xy <- xyFromCell(r, cell_n)
    dist_km <- haversine_km(coords["lon"], coords["lat"], cell_xy[1, 1], cell_xy[1, 2])
    data.frame(
      site_id = site_id, variable = f$variable, year = f$year, month = 1:12,
      value = vals,
      tower_lon = coords["lon"], tower_lat = coords["lat"],
      cell_lon = cell_xy[1, 1], cell_lat = cell_xy[1, 2],
      cell_dist_km = dist_km
    )
  })
  append_raw_rows(do.call(rbind, site_rows))

  ## ---- one-time grid check: 0.05deg cells within one tower's 0.5deg cell average to the
  ## Stage 2 0.5deg extracted value, for one site/month (GH-Ank, 2001-01, GPP) ----------------
  if (!grid_check_done && f$variable == "GPP" && f$year == 2001) {
    check_site <- "GH-Ank"
    coords_c <- site_snapshot_coords(check_site)
    f050_path <- file.path(XBASE050_DIR, "GPP_2001_050_monthly.nc")
    if (file.exists(f050_path)) {
      r050 <- rast(f050_path)[[1]]
      pt_c <- vect(data.frame(lon = coords_c["lon"], lat = coords_c["lat"]), geom = c("lon", "lat"), crs = "EPSG:4326")
      value_050 <- as.numeric(extract(r050, pt_c)[1, -1])
      cell050_n <- cellFromXY(r050, cbind(coords_c["lon"], coords_c["lat"]))
      xy050 <- xyFromCell(r050, cell050_n)
      half <- res(r050) / 2
      box <- ext(xy050[1, 1] - half[1], xy050[1, 1] + half[1], xy050[1, 2] - half[2], xy050[1, 2] + half[2])
      r005_m1 <- r[[1]]
      cropped <- crop(r005_m1, box)
      mean_005 <- mean(terra::values(cropped), na.rm = TRUE)
      grid_check_df <- data.frame(
        site_id = check_site, variable = "GPP", year = 2001, month = 1,
        value_050deg = value_050,
        mean_005deg_within_050_cell = mean_005,
        n_005deg_cells_averaged = terra::ncell(cropped),
        diff = mean_005 - value_050,
        note_050_grid_cell_centre_lon = xy050[1, 1], note_050_grid_cell_centre_lat = xy050[1, 2],
        note_005_grid_resolution_deg = res(r005_m1)[1], note_050_grid_resolution_deg = res(r050)[1]
      )
      utils::write.csv(grid_check_df, GRID_CHECK_OUT, row.names = FALSE)
      grid_check_done <- TRUE
      message("  Grid check written: 0.5deg value ", round(value_050, 4), " vs. mean of ",
              terra::ncell(cropped), " 0.05deg cells ", round(mean_005, 4))
    } else {
      message("  Grid check skipped: Stage 2 file not found at ", f050_path)
    }
  }

  manifest_row <- data.frame(
    variable = f$variable, year = f$year, resolution = "0.05deg", temporal_resolution = "monthly",
    filename = f$fileName, object_hash = f$hash, doi = DOI, access_url = access_url,
    expected_size_bytes = f$size, actual_size_bytes = actual_size,
    sha256_base64 = f$sha256_b64, sha256_verified = sha256_verified,
    download_datetime_utc = download_datetime_utc, licence = LICENCE, citation = CITATION,
    extracted = TRUE, n_sites_extracted = length(STAGE3_SITES)
  )
  append_manifest_row(manifest_row)

  file.remove(local_path)
  message("  Extracted ", length(STAGE3_SITES), " sites, deleted global file.")
}

message("\n[11_download_extract_xbase_005] Done. Manifest: ", MANIFEST,
        "\nRaw extraction table: ", RAW_OUT, "\nGrid check: ", GRID_CHECK_OUT)

write_meta(
  MANIFEST,
  input_sources = c("https://meta.icos-cp.eu/sparql (ICOS metadata SPARQL endpoint)",
                     "https://data.icos-cp.eu/objects/<hash> (per-file download, after licence_accept)"),
  notes = paste0(
    "FLUXCOM-X-BASE 0.05deg monthly grid (Stage 3), non-Shuttle comparison data (CLAUDE.md Hard Rule 1). ",
    "Same DOI as Stage 2's 0.5deg manifest (fluxcom_x_base_download_manifest.csv); filtered on the ",
    "_005_monthly.nc filename suffix. Each of the 84 global files was downloaded, sha256-verified, had its ",
    "six WAF-Net site cells extracted (xbase_005_site_extraction_raw.csv), and was then deleted -- the ",
    "global 0.05deg grid is never retained on disk. Anonymous CC BY 4.0 licence click-through, no account/",
    "credential (same mechanism as Stage 2, confirmed again here)."
  )
)
write_meta(
  RAW_OUT,
  input_sources = "data/external/fluxcom_x_base_005/*.nc (consumed and deleted one at a time; see fluxcom_x_base_005_download_manifest.csv)",
  notes = paste0(
    "One row per site x variable x year x month (STAGE3_SITES: GH-Ank, BJ-Nhu, BJ-Bfg, SN-Dhr, ML-Kem, ",
    "SN-Nkr -- excludes BJ-Db1, which has zero overlap with any FLUXCOM release, Stage 2 section 5). ",
    "Site coordinates from the 20 September 2026 FLUXNET Shuttle snapshot (location_long/location_lat), ",
    "per the Stage 3 prompt -- not the BIF-derived site_coords_for() Stage 2 used. value is in the ",
    "product's native units as distributed (gC m-2 d-1 for GPP/NEE, mm hr-1 for ET/ET_T) -- unit matching ",
    "and LE conversion happen downstream in 12_package_ossenatou.R, not here. cell_lon/cell_lat is the ",
    "0.05deg grid cell centre terra reports for the tower coordinate; cell_dist_km is the great-circle ",
    "distance between the tower and that cell centre (base R haversine, no geosphere dependency)."
  )
)
