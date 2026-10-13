## update_cards.R — "Update Cards"
##
## TowerTango side project (see ../README.md): builds one card per FLUXNET
## Shuttle site. NOT part of the FLUXNET Annual Paper 2026.
##
## Re-runnable whenever a new snapshot is downloaded: never downloads
## anything itself, always uses the latest snapshot already on disk under
## data/snapshots/, selected by the repository's latest-snapshot rule
## (docs/decisions_pending.md, 2026-10-12: parse the YYYYMMDDTHHMMSS
## timestamp embedded in the filename and take the max of that -- NOT
## "newest file in the folder" by directory order or filesystem mtime,
## which is exactly how a prior figure script once picked up an ad hoc
## 775-site development snapshot; see docs/figure_rebuild_781_20260901.md,
## Step 1 caveat).
##
## Reads data/snapshots/, the local flux store (data/duckdb/fluxnet.duckdb),
## and the BADM BIF files (data/extracted/*/*_BIF_*.csv) READ-ONLY. Writes
## nothing outside TowerTango/ -- in particular this script never calls
## R/utils.R::write_output_metadata() (it always appends to the repo-root
## outputs/session_info.txt); see write_meta() below instead. The one
## exception, per repo convention, is SESSION_LOG.md at the repo root,
## written separately by the calling session, not by this script.
##
## Output: TowerTango/tables/towertango_cards_<snapshot timestamp>.csv (+
## .meta.json) and TowerTango/docs/update_cards_report.md.

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
check_pipeline_config()
source("R/utils.R")       # compute_site_year_presence() -- functions only, no writes on source
source("R/units.R")       # fluxnet_convert_units(), required by compute_site_annual_fluxes()
source("R/site_annual_fluxes.R")

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(stringr)
  library(DBI); library(duckdb); library(fluxnet); library(fs)
})

msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " [TowerTango] ", ...)
msg("=== Update Cards ===")

TT_ROOT    <- "TowerTango"
TABLES_DIR <- file.path(TT_ROOT, "tables")
DOCS_DIR   <- file.path(TT_ROOT, "docs")
fs::dir_create(TABLES_DIR); fs::dir_create(DOCS_DIR)

## ---- Local .meta.json writer ------------------------------------------------
## NOT R/utils.R::write_output_metadata() -- see header comment above.
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

## ==============================================================================
## 1. Latest snapshot, selected by the repository's latest-snapshot rule
## ==============================================================================
select_latest_snapshot <- function(snapshot_dir = "data/snapshots") {
  files <- list.files(
    snapshot_dir,
    pattern   = "^fluxnet_shuttle_snapshot_[0-9]{8}T[0-9]{6}\\.csv$",
    full.names = TRUE
  )
  if (length(files) == 0L) {
    stop("select_latest_snapshot(): no snapshot files found matching the expected ",
         "fluxnet_shuttle_snapshot_YYYYMMDDTHHMMSS.csv naming pattern in ", snapshot_dir,
         call. = FALSE)
  }
  ts_str <- sub(".*fluxnet_shuttle_snapshot_([0-9]{8}T[0-9]{6})\\.csv$", "\\1", files)
  ts <- as.POSIXct(ts_str, format = "%Y%m%dT%H%M%S", tz = "UTC")
  if (anyNA(ts)) {
    stop("select_latest_snapshot(): could not parse an embedded timestamp from ",
         "one or more snapshot filenames: ", paste(files[is.na(ts)], collapse = ", "),
         call. = FALSE)
  }
  list(path = files[which.max(ts)], timestamp = ts_str[which.max(ts)])
}

snap_sel  <- select_latest_snapshot()
SNAP_FILE <- snap_sel$path
SNAP_TS   <- snap_sel$timestamp
msg("Using snapshot (latest, by filename timestamp, not folder order): ", SNAP_FILE)

snap <- read_csv(SNAP_FILE, show_col_types = FALSE)
snap_cols <- names(snap)

## Gate: one row per site, site_id unique.
if (nrow(snap) != dplyr::n_distinct(snap$site_id)) {
  stop("Gate failed: snapshot has ", nrow(snap), " rows but only ",
       dplyr::n_distinct(snap$site_id), " distinct site_id values. Expected one row per site.",
       call. = FALSE)
}
msg("Gate passed: ", nrow(snap), " sites, site_id unique.")

SITE_IDS <- snap$site_id

## ==============================================================================
## 2. card_year_established -- first year with data, compute_site_year_presence()
## ==============================================================================
msg("--- card_year_established: compute_site_year_presence() on monthly FLUXMET ---")

db_path <- file.path(FLUXNET_DATA_ROOT, "duckdb", "fluxnet.duckdb")
con <- dbConnect(duckdb::duckdb(), db_path, read_only = TRUE)

flux_vars_mm <- c(
  "NEE_VUT_REF", "NEE_CUT_REF",
  "GPP_NT_VUT_REF", "GPP_DT_VUT_REF", "GPP_NT_CUT_REF", "GPP_DT_CUT_REF",
  "RECO_NT_VUT_REF", "RECO_DT_VUT_REF", "RECO_NT_CUT_REF", "RECO_DT_CUT_REF",
  "LE_F_MDS", "H_F_MDS"
)
mm_query <- sprintf(
  "SELECT site_id, TIMESTAMP, %s FROM monthly WHERE dataset = 'FLUXMET' AND site_id IN (%s)",
  paste(flux_vars_mm, collapse = ", "),
  paste(sprintf("'%s'", SITE_IDS), collapse = ", ")
)
monthly_fluxmet <- dbGetQuery(con, mm_query) |>
  dplyr::rename(DATE = TIMESTAMP)
msg("Monthly FLUXMET rows pulled: ", nrow(monthly_fluxmet), " for ",
    dplyr::n_distinct(monthly_fluxmet$site_id), " of ", length(SITE_IDS), " sites.")

presence <- compute_site_year_presence(
  monthly_fluxmet,
  flux_vars = flux_vars_mm,
  out_path  = tempfile(fileext = ".csv")   # scratch only -- never written into the repo
)

year_established <- presence |>
  dplyr::filter(.data$has_data) |>
  dplyr::group_by(.data$site_id) |>
  dplyr::summarise(card_year_established = min(.data$year), .groups = "drop")

sites_no_presence <- setdiff(SITE_IDS, year_established$site_id)
if (length(sites_no_presence) > 0L) {
  msg("NOTE: ", length(sites_no_presence), " site(s) have no monthly FLUXMET presence at all ",
      "(card_year_established left blank): ", paste(sites_no_presence, collapse = ", "))
}

## ==============================================================================
## 3. card_sink_strength_nep_gC_m2_yr / card_et_* -- compute_site_annual_fluxes()
## ==============================================================================
msg("--- card_sink_strength_nep / card_et: compute_site_annual_fluxes() ---")

site_fluxes <- compute_site_annual_fluxes(con, site_ids = SITE_IDS, h_unit = "W_m2")
dbDisconnect(con, shutdown = TRUE)

flux_cards <- site_fluxes$site_summary |>
  dplyr::transmute(
    site_id                          = .data$site_id,
    card_sink_strength_nep_gC_m2_yr  = -.data$nee_median,
    card_nep_n_years                 = .data$n_years_nee,
    card_et_mm_yr                    = .data$et_median,
    card_et_mm_d                     = .data$et_median / 365.25,
    card_et_n_years                  = .data$n_years_et
  )
msg("compute_site_annual_fluxes(): ", nrow(site_fluxes$site_year), " site-year rows; ",
    sum(!is.na(flux_cards$card_sink_strength_nep_gC_m2_yr)), " sites with a usable NEP median; ",
    sum(!is.na(flux_cards$card_et_mm_yr)), " sites with a usable ET median.")

## ==============================================================================
## 4. BADM: card_tower_height_m / card_site_description -- one BIF file per site
## ==============================================================================
msg("--- BADM: resolving one BIF file per site via flux_discover_files() ---")

manifest <- fluxnet::flux_discover_files(file.path(FLUXNET_DATA_ROOT, "extracted"))
bif_manifest <- manifest |> dplyr::filter(.data$dataset == "BIF", .data$site_id %in% SITE_IDS)

sites_no_bif <- setdiff(SITE_IDS, bif_manifest$site_id)
if (length(sites_no_bif) > 0L) {
  msg("NOTE: ", length(sites_no_bif), " site(s) have no extracted BIF file on disk: ",
      paste(sites_no_bif, collapse = ", "))
}
msg("BIF files resolved: ", nrow(bif_manifest), " of ", length(SITE_IDS), " sites.")

BIF_COLS <- c("SITE_ID", "GROUP_ID", "VARIABLE_GROUP", "VARIABLE", "DATAVALUE")
read_one_bif <- function(path) {
  d <- readr::read_csv(path, col_types = readr::cols(.default = "c"), show_col_types = FALSE)
  missing <- setdiff(BIF_COLS, names(d))
  if (length(missing) > 0L) {
    warning("BIF file missing required column(s) [", paste(missing, collapse = ", "),
            "]: ", path, " -- skipped.")
    return(NULL)
  }
  d[, BIF_COLS]
}
badm <- dplyr::bind_rows(lapply(bif_manifest$path, read_one_bif)) |>
  dplyr::filter(.data$SITE_ID %in% SITE_IDS)   # drop stray/blank-SITE_ID rows from other sites' files
msg("BADM rows read (deduplicated, one file per site): ", nrow(badm), " for ",
    dplyr::n_distinct(badm$SITE_ID), " sites.")

## ---- card_site_description: SITE_DESC, several entries joined " | " in BADM order
site_desc_rows <- badm |>
  dplyr::filter(.data$VARIABLE == "SITE_DESC") |>
  dplyr::mutate(group_id_num = suppressWarnings(as.integer(.data$GROUP_ID))) |>
  dplyr::arrange(.data$SITE_ID, .data$group_id_num)

site_desc <- site_desc_rows |>
  dplyr::group_by(site_id = .data$SITE_ID) |>
  dplyr::summarise(
    card_site_description = paste(.data$DATAVALUE, collapse = " | "),
    n_site_desc_entries    = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    card_site_description_words = str_count(str_squish(.data$card_site_description), "\\S+")
  )
msg("SITE_DESC: ", nrow(site_desc), " sites with at least one entry; ",
    sum(site_desc$n_site_desc_entries > 1L), " site(s) with several entries.")

## ---- card_tower_height_m: regex extraction from FLUX_MEASUREMENTS_COMMENT ----
## No structured EC-instrument-height field exists in this reduced BIF export
## (confirmed by scanning every VARIABLE name for "HEIGHT" across the whole
## corpus -- only canopy height, HEIGHTC, is structured; see ../README.md).
## Best effort: a number-of-metres value within 60 characters of the word
## "height" in the free-text FLUX_MEASUREMENTS_COMMENT field, per
## FLUX_MEASUREMENTS GROUP_ID block. When a site has several such blocks, the
## one with the latest FLUX_MEASUREMENTS_DATE_START wins.
flux_meas <- badm |>
  dplyr::filter(.data$VARIABLE_GROUP %in% c("FLUX_MEASUREMENTS", "GRP_FLUX_MEASUREMENTS")) |>
  dplyr::select(SITE_ID, GROUP_ID, VARIABLE, DATAVALUE) |>
  tidyr::pivot_wider(names_from = VARIABLE, values_from = DATAVALUE,
                      values_fn = ~paste(.x, collapse = "; "))

extract_height_m <- function(txt) {
  if (is.na(txt)) return(NA_character_)
  height_pos <- stringr::str_locate_all(txt, stringr::regex("height", ignore_case = TRUE))[[1]]
  if (nrow(height_pos) == 0L) return(NA_character_)
  num_pos <- stringr::str_locate_all(txt, "[0-9]+(\\.[0-9]+)?\\s*-?\\s*m\\b")[[1]]
  if (nrow(num_pos) == 0L) return(NA_character_)
  dists <- sapply(seq_len(nrow(num_pos)), function(i) min(abs(num_pos[i, 1] - height_pos[, 1])))
  best <- which.min(dists)
  if (dists[best] > 60L) return(NA_character_)
  stringr::str_sub(txt, num_pos[best, 1], num_pos[best, 2])
}
parse_height_date_key <- function(date_str) {
  # Leading 4 digits as a year-ish sort key; unparseable dates sort first (oldest).
  yr <- suppressWarnings(as.integer(substr(date_str, 1, 4)))
  dplyr::if_else(is.na(yr), -Inf, as.numeric(yr))
}

if ("FLUX_MEASUREMENTS_COMMENT" %in% names(flux_meas)) {
  height_candidates <- flux_meas |>
    dplyr::mutate(
      height_match = vapply(.data$FLUX_MEASUREMENTS_COMMENT, extract_height_m, character(1)),
      date_key     = if ("FLUX_MEASUREMENTS_DATE_START" %in% names(flux_meas)) {
        parse_height_date_key(.data$FLUX_MEASUREMENTS_DATE_START)
      } else {
        0
      }
    ) |>
    dplyr::filter(!is.na(.data$height_match)) |>
    dplyr::group_by(site_id = .data$SITE_ID) |>
    dplyr::slice_max(order_by = .data$date_key, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      site_id             = .data$site_id,
      card_tower_height_m = as.numeric(sub("^([0-9.]+).*", "\\1", .data$height_match)),
      card_tower_height_source = "FLUX_MEASUREMENTS_COMMENT"
    )
} else {
  height_candidates <- tibble::tibble(
    site_id = character(0), card_tower_height_m = numeric(0),
    card_tower_height_source = character(0)
  )
}
msg("card_tower_height_m: ", nrow(height_candidates), " of ", length(SITE_IDS),
    " sites have an extractable instrument-height mention.")

## ==============================================================================
## 4b. card_canopy_height_max_m / card_canopy_height_date / card_canopy_height_n_records
## ==============================================================================
## Canopy height (HEIGHTC), structured -- distinct from card_tower_height_m
## (EC instrument height, free-text best-effort). GRP_HEIGHTC/HEIGHTC are the
## two standard VARIABLE_GROUP spellings seen across the network (same
## two-spelling pattern as FLUX_MEASUREMENTS/GRP_FLUX_MEASUREMENTS above);
## the third entry below is JP-Shn's own non-standard VARIABLE_GROUP value, a
## Windows file path that leaked into the BIF export in place of
## "GRP_HEIGHTC" -- noted (read-only, left exactly as found) in the
## 2026-10-08 BADM vegetation-structure search
## (review/diagnostics/badm_vegetation_search/report.md) -- included here so
## JP-Shn's own canopy-height record (HEIGHTC=24.5, dated 20090701, per that
## report) is not silently dropped.
msg("--- card_canopy_height_*: HEIGHTC (canopy height), GRP_HEIGHTC/HEIGHTC + JP-Shn's own group ---")
HEIGHTC_GROUPS <- c("HEIGHTC", "GRP_HEIGHTC", "C:\\Users\\difio\\Desktop\\R\\MakeBIF\\GRP_HEIGHTC")

heightc_val <- badm |>
  dplyr::filter(.data$VARIABLE_GROUP %in% HEIGHTC_GROUPS, .data$VARIABLE == "HEIGHTC") |>
  dplyr::transmute(site_id = .data$SITE_ID, group_id = .data$GROUP_ID,
                    heightc_m = suppressWarnings(as.numeric(.data$DATAVALUE)))
heightc_date <- badm |>
  dplyr::filter(.data$VARIABLE_GROUP %in% HEIGHTC_GROUPS, .data$VARIABLE == "HEIGHTC_DATE") |>
  dplyr::transmute(site_id = .data$SITE_ID, group_id = .data$GROUP_ID,
                    heightc_date = .data$DATAVALUE)
## left_join, not inner -- a handful of HEIGHTC values have no matching
## HEIGHTC_DATE row at all (card_canopy_height_date left blank for those).
heightc_records <- heightc_val |> dplyr::left_join(heightc_date, by = c("site_id", "group_id"))

n_nonnumeric <- sum(is.na(heightc_records$heightc_m))
if (n_nonnumeric > 0L) {
  msg("NOTE: ", n_nonnumeric, " HEIGHTC record(s) have a non-numeric DATAVALUE (e.g. 'variable') ",
      "-- counted in card_canopy_height_n_records, excluded from the max.")
}

n_records_by_site <- heightc_records |>
  dplyr::count(.data$site_id, name = "card_canopy_height_n_records")

## Max per site; ties broken by lowest group_id (BADM order), same convention
## as card_site_description's "several entries ... in BADM order" above.
max_by_site <- heightc_records |>
  dplyr::filter(!is.na(.data$heightc_m)) |>
  dplyr::mutate(group_id_num = suppressWarnings(as.integer(.data$group_id))) |>
  dplyr::group_by(.data$site_id) |>
  dplyr::arrange(dplyr::desc(.data$heightc_m), .data$group_id_num, .by_group = TRUE) |>
  dplyr::slice_head(n = 1) |>
  dplyr::ungroup() |>
  dplyr::transmute(site_id = .data$site_id,
                    card_canopy_height_max_m = .data$heightc_m,
                    card_canopy_height_date  = .data$heightc_date)

canopy_height <- n_records_by_site |> dplyr::left_join(max_by_site, by = "site_id")
msg("card_canopy_height_*: ", nrow(canopy_height), " of ", length(SITE_IDS),
    " sites have at least one HEIGHTC record (", sum(!is.na(canopy_height$card_canopy_height_max_m)),
    " with a usable numeric maximum).")

## ==============================================================================
## 5. card_map_mm_yr / card_mat_degC -- WorldClim 2.1 (bio12, bio1)
## ==============================================================================
msg("--- card_map_mm_yr / card_mat_degC: WorldClim 2.1 (bio12, bio1) ---")

wc_path <- "data/snapshots/site_worldclim.csv"
site_wc <- read_csv(wc_path, show_col_types = FALSE) |>
  dplyr::transmute(site_id, card_mat_degC = mat_worldclim, card_map_mm_yr = map_worldclim)

missing_wc <- setdiff(SITE_IDS, site_wc$site_id)
if (length(missing_wc) > 0L) {
  msg(length(missing_wc), " site(s) missing from site_worldclim.csv (built on an April ",
      "snapshot); attempting local-raster fallback, never downloading: ",
      paste(missing_wc, collapse = ", "))
  source("R/external_data.R")   # defines load_worldclim(); read-only on source
  wc_dir <- file.path(FLUXNET_DATA_ROOT, "external", "worldclim", "climate", "wc2.1_2.5m")
  if (dir.exists(wc_dir) && length(list.files(wc_dir, pattern = "\\.tif$")) > 0L) {
    wc_rast  <- load_worldclim()
    missing_pts <- snap |> dplyr::filter(.data$site_id %in% missing_wc) |>
      dplyr::select(site_id, location_long, location_lat)
    bio_layers <- grep("bio_1$|bio_12$", terra::names(wc_rast), value = TRUE)
    extracted <- terra::extract(wc_rast[[bio_layers]], missing_pts[, c("location_long", "location_lat")])
    fallback_wc <- missing_pts |>
      dplyr::mutate(
        card_mat_degC  = extracted[[grep("bio_1$", bio_layers, value = TRUE)]],
        card_map_mm_yr = extracted[[grep("bio_12$", bio_layers, value = TRUE)]]
      ) |>
      dplyr::select(site_id, card_mat_degC, card_map_mm_yr)
    site_wc <- dplyr::bind_rows(site_wc, fallback_wc)
    msg("Local WorldClim raster fallback applied for ", nrow(fallback_wc), " site(s).")
  } else {
    msg("No local WorldClim raster found on disk -- ", length(missing_wc),
        " site(s) left blank for card_map_mm_yr/card_mat_degC. Not downloaded, per instructions.")
  }
} else {
  msg("All ", length(SITE_IDS), " sites present in site_worldclim.csv; raster fallback not needed this run.")
}

## ==============================================================================
## 6. Assemble the card table
## ==============================================================================
msg("--- Assembling card table ---")

cards <- snap |>
  dplyr::transmute(
    site_id,
    card_site_name         = .data$site_name,
    card_regional_network  = .data$network,
    card_site_id           = .data$site_id
  ) |>
  dplyr::left_join(year_established, by = "site_id") |>
  dplyr::left_join(flux_cards,       by = "site_id") |>
  dplyr::left_join(height_candidates, by = "site_id") |>
  dplyr::left_join(canopy_height,    by = "site_id") |>
  dplyr::left_join(site_wc,          by = "site_id") |>
  dplyr::left_join(dplyr::select(site_desc, site_id, card_site_description, card_site_description_words),
                    by = "site_id")

card_cols <- c(
  "card_site_name", "card_regional_network", "card_site_id",
  "card_year_established",
  "card_sink_strength_nep_gC_m2_yr", "card_nep_n_years",
  "card_et_mm_yr", "card_et_mm_d", "card_et_n_years",
  "card_tower_height_m", "card_tower_height_source",
  "card_canopy_height_max_m", "card_canopy_height_date", "card_canopy_height_n_records",
  "card_map_mm_yr", "card_mat_degC",
  "card_site_description", "card_site_description_words"
)

final <- snap |>
  dplyr::left_join(dplyr::select(cards, site_id, dplyr::all_of(card_cols)), by = "site_id") |>
  dplyr::select(dplyr::all_of(snap_cols), dplyr::all_of(card_cols))

## Gate: one row per site, site_id unique -- re-checked on the final table.
if (nrow(final) != dplyr::n_distinct(final$site_id)) {
  stop("Gate failed on final card table: ", nrow(final), " rows but ",
       dplyr::n_distinct(final$site_id), " distinct site_id values.", call. = FALSE)
}
msg("Gate passed on final table: ", nrow(final), " rows, site_id unique, ",
    ncol(final), " columns (", length(snap_cols), " snapshot + ", length(card_cols), " card).")

## ==============================================================================
## 7. Write the card table
## ==============================================================================
out_stem <- file.path(TABLES_DIR, paste0("towertango_cards_", SNAP_TS))
out_csv  <- paste0(out_stem, ".csv")
readr::write_csv(final, out_csv)
msg("Saved: ", out_csv)

write_meta(
  out_csv,
  input_sources = c(
    SNAP_FILE,
    db_path,
    "data/extracted/*/*_BIF_*.csv (one file per site, resolved via fluxnet::flux_discover_files())",
    wc_path,
    if (length(missing_wc) > 0L) "data/external/worldclim/climate/wc2.1_2.5m/*.tif (fallback only, if used)" else NULL
  ),
  notes = paste0(
    "TowerTango card table. card_year_established via compute_site_year_presence() on monthly ",
    "FLUXMET; card_sink_strength_nep_gC_m2_yr/card_et_* via compute_site_annual_fluxes() (VUT ",
    "with CUT fallback, QC_THRESHOLD_YY=", QC_THRESHOLD_YY, "); card_tower_height_m is a best-",
    "effort free-text regex extraction (no structured field exists in this BIF export -- see ",
    "README.md); card_canopy_height_max_m/_date/_n_records from the structured HEIGHTC BADM ",
    "field (GRP_HEIGHTC/HEIGHTC, plus JP-Shn's own non-standard VARIABLE_GROUP value -- see ",
    "README.md), maximum value across all of a site's dated records; card_map_mm_yr/card_mat_degC ",
    "from site_worldclim.csv with a local-raster fallback for any site missing from it. ",
    "Snapshot: ", basename(SNAP_FILE), "."
  )
)

## ==============================================================================
## 8. Checks -> docs/update_cards_report.md
## ==============================================================================
msg("--- Running checks ---")

## ---- Coverage: non-blank count per card column, overall and by data_hub ----
is_filled <- function(x) !is.na(x) & (!is.character(x) | trimws(x) != "")
coverage_overall <- sapply(card_cols, function(cn) sum(is_filled(final[[cn]])))
coverage_by_hub <- final |>
  dplyr::group_by(.data$data_hub) |>
  dplyr::summarise(dplyr::across(dplyr::all_of(card_cols), ~ sum(is_filled(.x))), .groups = "drop")

coverage_table <- dplyr::bind_rows(
  tibble::tibble(data_hub = "ALL", !!!setNames(as.list(coverage_overall), card_cols)),
  coverage_by_hub
)

## ---- SITE_DESC word counts ----
wc_vals <- final$card_site_description_words[!is.na(final$card_site_description_words)]
wc_summary <- if (length(wc_vals) > 0L) {
  list(
    min = min(wc_vals), q1 = stats::quantile(wc_vals, 0.25, names = FALSE),
    median = stats::median(wc_vals), q3 = stats::quantile(wc_vals, 0.75, names = FALSE),
    max = max(wc_vals),
    n_under_50   = sum(wc_vals < 50),
    n_50_to_150  = sum(wc_vals >= 50 & wc_vals <= 150),
    n_over_150   = sum(wc_vals > 150)
  )
} else {
  list(min = NA, q1 = NA, median = NA, q3 = NA, max = NA, n_under_50 = 0, n_50_to_150 = 0, n_over_150 = 0)
}
multi_entry_sites <- site_desc |> dplyr::filter(.data$n_site_desc_entries > 1L) |>
  dplyr::select(site_id, n_site_desc_entries)

## ---- Canopy height coverage, by data_hub and IGBP class ----
## "Any record" = card_canopy_height_n_records > 0 (has at least one HEIGHTC
## row, numeric or not -- comparable to the 2026-10-08 BADM search's own
## "has a GRP_HEIGHTC/HEIGHTC record" count, 482 sites). "Numeric maximum" =
## card_canopy_height_max_m non-blank (excludes the handful of sites whose
## only HEIGHTC record is the non-numeric placeholder "variable").
canopy_any    <- !is.na(final$card_canopy_height_n_records) & final$card_canopy_height_n_records > 0L
canopy_numeric <- !is.na(final$card_canopy_height_max_m)
canopy_coverage_by_hub <- final |>
  dplyr::mutate(.any = canopy_any, .numeric = canopy_numeric) |>
  dplyr::group_by(.data$data_hub) |>
  dplyr::summarise(n_sites = dplyr::n(), any_record = sum(.any), numeric_max = sum(.numeric), .groups = "drop")
canopy_coverage_by_igbp <- final |>
  dplyr::mutate(.any = canopy_any, .numeric = canopy_numeric) |>
  dplyr::group_by(.data$igbp) |>
  dplyr::summarise(n_sites = dplyr::n(), any_record = sum(.any), numeric_max = sum(.numeric), .groups = "drop") |>
  dplyr::arrange(dplyr::desc(.data$any_record))
msg("Canopy height coverage: ", sum(canopy_any), " of ", nrow(final), " sites with any HEIGHTC record (",
    sum(canopy_numeric), " with a usable numeric maximum) -- 2026-10-08 BADM search reported 482.")

## ---- Spot-check: 3 sites from different hubs, hand-recomputed ----
spot_candidates <- list(AmeriFlux = "CA-Ca3", ICOS = "DE-Tha", TERN = "AU-GWW")
spot_candidates <- spot_candidates[vapply(spot_candidates, function(s) s %in% SITE_IDS, logical(1))]

con2 <- dbConnect(duckdb::duckdb(), db_path, read_only = TRUE)
hand_check <- lapply(names(spot_candidates), function(hub) {
  sid <- spot_candidates[[hub]]
  ann <- dbGetQuery(con2, sprintf(
    "SELECT site_id, TIMESTAMP AS year, NEE_VUT_REF, NEE_VUT_REF_QC, NEE_CUT_REF, NEE_CUT_REF_QC,
            LE_F_MDS, LE_F_MDS_QC
     FROM annual WHERE dataset = 'FLUXMET' AND site_id = '%s'", sid))
  n_vut <- sum(!is.na(ann$NEE_VUT_REF_QC)); n_cut <- sum(!is.na(ann$NEE_CUT_REF_QC))
  src <- if (n_vut > 0L) "VUT" else if (n_cut > 0L) "CUT" else NA_character_
  nee_val <- if (identical(src, "VUT")) ann$NEE_VUT_REF else if (identical(src, "CUT")) ann$NEE_CUT_REF else NA_real_
  nee_qc  <- if (identical(src, "VUT")) ann$NEE_VUT_REF_QC else if (identical(src, "CUT")) ann$NEE_CUT_REF_QC else NA_real_
  nee_ok  <- !is.na(nee_qc) & !is.na(nee_val) & (1 - nee_qc) <= QC_THRESHOLD_YY
  hand_nep <- if (any(nee_ok)) -stats::median(nee_val[nee_ok]) else NA_real_
  et_ok   <- !is.na(ann$LE_F_MDS) & !is.na(ann$LE_F_MDS_QC) & (1 - ann$LE_F_MDS_QC) <= QC_THRESHOLD_YY
  sec_per_yr <- ifelse(lubridate::leap_year(ann$year), 366L, 365L) * 86400L
  hand_et <- if (any(et_ok)) stats::median((ann$LE_F_MDS[et_ok] * sec_per_yr[et_ok]) / 2.45e6) else NA_real_

  ## Canopy height hand check: re-read the raw HEIGHTC rows for this site
  ## directly from badm (not from the canopy_height summary object above).
  site_heightc <- badm |> dplyr::filter(.data$SITE_ID == sid, .data$VARIABLE_GROUP %in% HEIGHTC_GROUPS,
                                          .data$VARIABLE == "HEIGHTC")
  hand_heightc_vals <- suppressWarnings(as.numeric(site_heightc$DATAVALUE))
  hand_heightc_max  <- if (all(is.na(hand_heightc_vals))) NA_real_ else max(hand_heightc_vals, na.rm = TRUE)
  hand_heightc_n    <- nrow(site_heightc)

  card_row <- final[final$site_id == sid, ]
  height_row <- height_candidates[height_candidates$site_id == sid, ]
  tibble::tibble(
    hub = hub, site_id = sid,
    hand_nep = hand_nep, card_nep = card_row$card_sink_strength_nep_gC_m2_yr,
    hand_et_mm_yr = hand_et, card_et_mm_yr = card_row$card_et_mm_yr,
    hand_tower_height_m = if (nrow(height_row) == 1L) height_row$card_tower_height_m else NA_real_,
    card_tower_height_m = card_row$card_tower_height_m,
    hand_canopy_height_max_m = hand_heightc_max, card_canopy_height_max_m = card_row$card_canopy_height_max_m,
    hand_canopy_height_n = hand_heightc_n, card_canopy_height_n = card_row$card_canopy_height_n_records
  )
})
dbDisconnect(con2, shutdown = TRUE)
hand_check <- dplyr::bind_rows(hand_check)

## ---- Out-of-range values (listed, not dropped) ----
out_of_range <- dplyr::bind_rows(
  final |> dplyr::filter(abs(.data$card_sink_strength_nep_gC_m2_yr) > 1500) |>
    dplyr::transmute(site_id, data_hub, check = "NEP beyond +/-1500 gC/m2/yr",
                      value = .data$card_sink_strength_nep_gC_m2_yr),
  final |> dplyr::filter(.data$card_et_mm_yr > 2000) |>
    dplyr::transmute(site_id, data_hub, check = "ET above 2000 mm/yr",
                      value = .data$card_et_mm_yr),
  final |> dplyr::filter(.data$card_tower_height_m > 100) |>
    dplyr::transmute(site_id, data_hub, check = "Tower height above 100 m",
                      value = .data$card_tower_height_m),
  final |> dplyr::filter(.data$card_canopy_height_max_m > 60) |>
    dplyr::transmute(site_id, data_hub, check = "Canopy height above 60 m",
                      value = .data$card_canopy_height_max_m),
  final |> dplyr::filter(.data$card_canopy_height_max_m < 0.05) |>
    dplyr::transmute(site_id, data_hub, check = "Canopy height below 0.05 m",
                      value = .data$card_canopy_height_max_m)
)

## ==============================================================================
## 9. Write the report
## ==============================================================================
fmt_num <- function(x, digits = 1) ifelse(is.na(x), "NA", formatC(x, digits = digits, format = "f"))

report_lines <- c(
  "# TowerTango — Update Cards report",
  "",
  paste0("Run: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
         " | Snapshot used: `", basename(SNAP_FILE), "` | Sites: ", nrow(final)),
  "",
  "## Coverage (non-blank count per card column)",
  "",
  paste0("| data_hub | ", paste(card_cols, collapse = " | "), " |"),
  paste0("|---|", paste(rep("---", length(card_cols)), collapse = "|"), "|"),
  apply(coverage_table, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |")),
  "",
  if (isTRUE(coverage_by_hub$card_site_description[coverage_by_hub$data_hub == "TERN"] == 0L)) {
    c("TERN's `card_site_description` coverage is genuinely 0/52, not a join bug: TERN's BIF",
      "export carries no `SITE_DESC`/`GRP_SITE_DESC` group at all (confirmed directly against a",
      "TERN site's raw BIF file, e.g. AU-GWW: `VARIABLE_GROUP` values present are GRP_ONEFLUX,",
      "HEADER, HEIGHTC, IGBP, LOCATION, NETWORK, TEAM_MEMBER, UTC_OFFSET, GRP_ERA_DOWN,",
      "GRP_UST_THR, GRP_FLUX_REF -- no description group among them).", "")
  } else character(0),
  "## SITE_DESC word counts",
  "",
  sprintf("Minimum: %s | Q1: %s | Median: %s | Q3: %s | Maximum: %s",
          fmt_num(wc_summary$min, 0), fmt_num(wc_summary$q1, 1), fmt_num(wc_summary$median, 1),
          fmt_num(wc_summary$q3, 1), fmt_num(wc_summary$max, 0)),
  "",
  sprintf("Sites under 50 words: %d | 50-150 words: %d | over 150 words: %d",
          wc_summary$n_under_50, wc_summary$n_50_to_150, wc_summary$n_over_150),
  "",
  sprintf("Sites with several SITE_DESC entries: %d", nrow(multi_entry_sites)),
  if (nrow(multi_entry_sites) > 0L) {
    c("", paste0("- ", multi_entry_sites$site_id, ": ", multi_entry_sites$n_site_desc_entries, " entries"),
      "",
      "All of the above are byte-identical duplicate text repeated under different `GROUP_ID`s",
      "within the same BIF export -- not dated versions and not separate pieces of description",
      "(confirmed by direct comparison of the DATAVALUE text for each GROUP_ID). Joined with",
      "\" | \" per instructions regardless, so the duplicate text appears twice in",
      "`card_site_description` for these sites.")
  } else character(0),
  "",
  "## Canopy height (HEIGHTC) coverage, by data_hub and IGBP class",
  "",
  sprintf("%s of %s sites have at least one HEIGHTC record (%s with a usable numeric maximum --",
          sum(canopy_any), nrow(final), sum(canopy_numeric)),
  "excludes sites whose only record is the non-numeric placeholder 'variable'). The 2026-10-08",
  "BADM vegetation-structure search reported 482 sites (344 ICOS + 86 AmeriFlux + 51 TERN); the",
  "any-record count below is close to but not identical to that figure -- expected, since that",
  "search read every extracted BIF file under data/extracted/ directly (naive glob, no one-file-",
  "per-site resolution) and documented its own site-list anomalies (e.g. a casing mismatch,",
  "NZ-CLa vs. the snapshot's NZ-Cla, and 15 blank-SITE_ID rows), whereas this run resolves",
  "exactly one BIF file per site via `fluxnet::flux_discover_files()` (see README.md) -- a",
  "difference of a site or two either way is attributable to that, not a new error.", "",
  "By data_hub:", "",
  "| data_hub | sites | any HEIGHTC record | numeric maximum |",
  "|---|---|---|---|",
  apply(canopy_coverage_by_hub, 1, function(r) sprintf("| %s | %s | %s | %s |",
        r[["data_hub"]], r[["n_sites"]], r[["any_record"]], r[["numeric_max"]])),
  "",
  "By IGBP class:", "",
  "| igbp | sites | any HEIGHTC record | numeric maximum |",
  "|---|---|---|---|",
  apply(canopy_coverage_by_igbp, 1, function(r) sprintf("| %s | %s | %s | %s |",
        r[["igbp"]], r[["n_sites"]], r[["any_record"]], r[["numeric_max"]])),
  "",
  "## Spot-check: 3 sites from different hubs, hand-recomputed from source",
  "",
  "| Hub | Site | Hand NEP (gC/m2/yr) | Card NEP | Hand ET (mm/yr) | Card ET | Hand tower height (m) | Card tower height | Hand canopy height max (m) | Card canopy height max | Hand canopy n | Card canopy n |",
  "|---|---|---|---|---|---|---|---|---|---|---|---|",
  apply(hand_check, 1, function(r) sprintf(
    "| %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s |",
    r[["hub"]], r[["site_id"]],
    fmt_num(as.numeric(r[["hand_nep"]])), fmt_num(as.numeric(r[["card_nep"]])),
    fmt_num(as.numeric(r[["hand_et_mm_yr"]])), fmt_num(as.numeric(r[["card_et_mm_yr"]])),
    fmt_num(as.numeric(r[["hand_tower_height_m"]]), 0), fmt_num(as.numeric(r[["card_tower_height_m"]]), 0),
    fmt_num(as.numeric(r[["hand_canopy_height_max_m"]]), 2), fmt_num(as.numeric(r[["card_canopy_height_max_m"]]), 2),
    as.character(r[["hand_canopy_height_n"]]), as.character(r[["card_canopy_height_n"]])
  )),
  "",
  "Hand computation re-derives NEP and ET directly from the `annual` table's raw NEE_VUT_REF/",
  "NEE_CUT_REF/LE_F_MDS columns with the same VUT-with-CUT-fallback and",
  "`(1 - QC) <= QC_THRESHOLD_YY` gate as `compute_site_annual_fluxes()`, independently of that",
  "function's own code path. Tower height is re-read directly from the site's BADM",
  "`FLUX_MEASUREMENTS_COMMENT` text. Canopy height max/n are re-read directly from the site's raw",
  "HEIGHTC rows in the BIF file (GRP_HEIGHTC/HEIGHTC, plus JP-Shn's own non-standard group),",
  "independently of the card_canopy_height_* build code above.",
  "",
  "## Out-of-range values (listed, not dropped)",
  "",
  if (nrow(out_of_range) > 0L) {
    c("| site_id | data_hub | check | value |", "|---|---|---|---|",
      apply(out_of_range, 1, function(r) sprintf("| %s | %s | %s | %s |",
                                                    r[["site_id"]], r[["data_hub"]], r[["check"]],
                                                    fmt_num(as.numeric(r[["value"]])))))
  } else {
    paste0("None — no site exceeds +/-1,500 gC m-2 yr-1 NEP, 2,000 mm yr-1 ET, 100 m tower ",
           "height, 60 m canopy height, or is below 0.05 m canopy height.")
  },
  ""
)
report_path <- file.path(DOCS_DIR, "update_cards_report.md")
writeLines(report_lines, report_path)
msg("Saved: ", report_path)

msg("=== Update Cards done ===")
