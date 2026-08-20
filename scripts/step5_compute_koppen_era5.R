## step5_compute_koppen_era5.R
## Compute Köppen-Geiger (KG) class for each current-network FLUXNET Shuttle
## site LOCALLY from the site's own bundled ERA5 monthly reanalysis data
## (Beck et al. classification rules applied to a 1991-2020 monthly
## temperature/precipitation normal), following the same method used by
## ICOS's KG_classification script.
##
## This is now the single authoritative KG source for the current 767-site
## network, replacing two previously-inconsistent sources:
##   - BADM CLIMATE_KOEPPEN metadata field (used by Anomalies_KG figures)
##   - Beck et al. (2023) 1 km raster extraction (used by representativeness
##     figures) — this raster extraction is UNCHANGED and still runs (see
##     step4_extract_koppen_beck2023.R); its output is retained here only as
##     a comparison/QA column, and is still the source for historical-network
##     comparisons (FLUXNET2015/La Thuile/MARCONI) and the global land-area
##     backdrop, which are out of scope for this change (not Shuttle sites
##     with bundled ERA5 monthly data).
##
## Output: data/snapshots/site_koppen_era5.csv
## See review/figures/representativeness/methods_koppen_era5.md for the
## full methods write-up.

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
source("R/utils.R")
source("R/climate_classification.R")
check_pipeline_config()

library(duckdb)
library(DBI)
library(dplyr)
library(readr)

# ---- Load current-network site list (same pinned snapshot as step4) --------
snap_file <- "data/snapshots/fluxnet_shuttle_snapshot_20260624T095651.csv"
if (!file.exists(snap_file)) {
  stop("Snapshot not found: ", snap_file, call. = FALSE)
}
message("Snapshot: ", basename(snap_file))
snapshot <- readr::read_csv(snap_file, show_col_types = FALSE)
site_ids <- snapshot |> dplyr::distinct(.data$site_id) |> dplyr::pull(.data$site_id)
message("Sites in current network: ", length(site_ids))

# ---- Pull raw ERA5 monthly rows from DuckDB ---------------------------------
duckdb_path <- "data/duckdb/fluxnet.duckdb"
if (!file.exists(duckdb_path)) {
  stop(
    "DuckDB database not found: ", duckdb_path, "\n",
    "Run scripts/03b_create_database.R first.", call. = FALSE
  )
}
message("Connecting to DuckDB: ", duckdb_path)
con <- dbConnect(duckdb(), dbdir = duckdb_path, read_only = TRUE)
monthly_era5 <- dbGetQuery(
  con,
  "SELECT site_id, TIMESTAMP, TA_ERA, P_ERA FROM monthly WHERE dataset = 'ERA5'"
)
dbDisconnect(con)
message("Raw ERA5 monthly rows: ", nrow(monthly_era5),
        " (", length(unique(monthly_era5$site_id)), " sites)")

monthly_era5 <- monthly_era5 |> dplyr::filter(.data$site_id %in% site_ids)
message("Rows after restricting to current-network sites: ", nrow(monthly_era5))

missing_era5 <- setdiff(site_ids, unique(monthly_era5$site_id))
if (length(missing_era5) > 0L) {
  message(length(missing_era5), " current-network site(s) have no ERA5 monthly ",
          "rows in DuckDB: ", paste(sort(missing_era5), collapse = ", "))
  for (sid in missing_era5) {
    log_unknown(
      record_id = sid,
      reason    = "No ERA5 monthly rows found in DuckDB monthly table",
      logged_by = "step5_compute_koppen_era5.R"
    )
  }
}

# ---- Load comparison sources -------------------------------------------------
badm_path <- file.path(FLUXNET_DATA_ROOT, "processed", "badm.rds")
badm <- if (file.exists(badm_path)) {
  message("Loading BADM (comparison column): ", badm_path)
  readRDS(badm_path)
} else {
  message("BADM not found at ", badm_path, " — badm_kg_class will be NA")
  NULL
}

beck_path <- file.path(FLUXNET_DATA_ROOT, "snapshots", "site_koppen_beck2023.csv")
beck2023 <- if (file.exists(beck_path)) {
  message("Loading Beck 2023 raster extraction (comparison column): ", beck_path)
  readr::read_csv(beck_path, show_col_types = FALSE)
} else {
  message("Beck 2023 extraction not found at ", beck_path,
          " — beck2023_kg_class will be NA. Run step4_extract_koppen_beck2023.R first.")
  NULL
}

# ---- Legend (class names) — reuse step4's parsing block --------------------
leg_path <- file.path(FLUXNET_DATA_ROOT, "external", "koppen_beck2023", "legend.txt")
legend_df <- NULL
if (file.exists(leg_path)) {
  leg_lines <- readLines(leg_path)
  leg_data  <- leg_lines[grepl("^\\s*[0-9]+:", leg_lines)]
  legend_df <- do.call(rbind, lapply(leg_data, function(ln) {
    m <- regmatches(ln, regexec(
      "^\\s*(\\d+):\\s+(\\S+)\\s+(.+?)\\s+\\[",
      ln, perl = TRUE
    ))[[1]]
    if (length(m) < 4L) return(NULL)
    data.frame(
      koppen_class_code = as.integer(m[2]),
      koppen_class      = trimws(m[3]),
      koppen_class_name = trimws(m[4]),
      stringsAsFactors  = FALSE
    )
  }))
  main_map <- c(A = "Tropical", B = "Arid", C = "Temperate", D = "Cold", E = "Polar")
  legend_df <- legend_df |>
    dplyr::mutate(
      koppen_main      = substr(.data$koppen_class, 1L, 1L),
      koppen_main_name = main_map[.data$koppen_main]
    )
} else {
  message("Legend not found at ", leg_path, " — class names will be NA")
}

# ---- Compute classification --------------------------------------------------
message("\nComputing ERA5-derived KG classification (", KG_ERA5_PERIOD[1], "-",
        KG_ERA5_PERIOD[2], ", min ", KG_ERA5_MIN_YEARS, " years, MAP screen ",
        KG_ERA5_MAP_MAX_MM, " mm/yr)...")

result <- compute_site_koppen_era5(
  monthly_era5, badm = badm, beck2023 = beck2023, legend = legend_df
)

# ---- Console summary ---------------------------------------------------------
cat("\n================================================================\n")
cat("  KG class distribution (ERA5-derived)\n")
cat("================================================================\n")
print(as.data.frame(dplyr::count(result, kg_main, sort = TRUE)))

cat("\nn_years_used distribution:\n")
print(summary(result$n_years_used))

n_classified   <- sum(!is.na(result$kg_class))
n_unclassified <- sum(is.na(result$kg_class))
cat(sprintf("\nClassified: %d / %d sites (%d unclassified)\n",
            n_classified, nrow(result), n_unclassified))
if (n_unclassified > 0L) {
  print(result |> dplyr::filter(is.na(.data$kg_class)) |>
          dplyr::select(site_id, n_years_used))
}

cat("\n--- Agreement vs. BADM CLIMATE_KOEPPEN ---\n")
badm_comparable <- result |> dplyr::filter(!is.na(.data$kg_class), !is.na(.data$badm_kg_class))
cat(sprintf("Comparable sites: %d\n", nrow(badm_comparable)))
if (nrow(badm_comparable) > 0L) {
  cat(sprintf("Full-code agreement:  %.1f%%\n", 100 * mean(badm_comparable$agree_badm)))
}

cat("\n--- Agreement vs. Beck 2023 raster ---\n")
beck_comparable <- result |> dplyr::filter(!is.na(.data$kg_class), !is.na(.data$beck2023_kg_class))
cat(sprintf("Comparable sites: %d\n", nrow(beck_comparable)))
if (nrow(beck_comparable) > 0L) {
  cat(sprintf("Full-code agreement:  %.1f%%\n", 100 * mean(beck_comparable$agree_beck2023)))
  main_agree <- mean(substr(beck_comparable$kg_class, 1, 1) ==
                        substr(beck_comparable$beck2023_kg_class, 1, 1))
  cat(sprintf("Main-group agreement: %.1f%%\n", 100 * main_agree))
}

# ---- Save ---------------------------------------------------------------------
out_path <- file.path(FLUXNET_DATA_ROOT, "snapshots", "site_koppen_era5.csv")
readr::write_csv(result, out_path)
message("\nSaved: ", out_path, " (", nrow(result), " rows)")

write_output_metadata(
  out_path,
  input_sources = c(
    duckdb_path,
    snap_file,
    if (!is.null(badm)) badm_path,
    if (!is.null(beck2023)) beck_path
  ),
  notes = paste0(
    "KG class computed locally from ERA5 monthly reanalysis data bundled with ",
    "each site's FLUXNET Shuttle download (dataset='ERA5' rows in the DuckDB ",
    "monthly table), following the Beck et al. classification rule cascade ",
    "applied to a ", KG_ERA5_PERIOD[1], "-", KG_ERA5_PERIOD[2],
    " monthly T/P normal (matches Beck et al. 2023's own present-day window). ",
    "Requires >= ", KG_ERA5_MIN_YEARS, " valid years of the 30; site-years with ",
    "computed annual P_ERA total > ", KG_ERA5_MAP_MAX_MM,
    " mm/yr excluded as reanalysis spatial-averaging artifacts ",
    "(docs/known_issues.md §9a). badm_kg_class/beck2023_kg_class are retained ",
    "comparison columns only, not used for classification. Scope: current ",
    length(site_ids), "-site Shuttle network only — historical-network ",
    "comparisons and the global land-area backdrop remain Beck-2023-raster-based."
  )
)

cat("\n================================================================\n")
cat("  DONE\n")
cat("================================================================\n")
