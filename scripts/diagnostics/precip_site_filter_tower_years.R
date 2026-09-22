## precip_site_filter_tower_years.R
##
## Makes the averaging window explicit in
## review/diagnostics/precip_site_filter/table_1_site_level_precip_estimates.csv,
## so a hand check against a downloaded FLUXMET_YY file cannot be ambiguous.
##
## Every site's extraction carries two annual files: ERA5_YY, named 1981-2025
## (or 1981-2024, depending on release) at every site, and FLUXMET_YY, named
## for the tower years. table_1's p_era_mean_mm was the mean of the former
## (full ERA5 record) but was not labelled as such -- a hand check against the
## FLUXMET file's own tower-year P_ERA values therefore disagreed by an amount
## that grows as the tower record shortens. This script does not change that
## value; it renames the column to say what it is, and adds a second,
## tower-years mean alongside it.
##
## Read-only with respect to R/pipeline_config.R, every pipeline script
## (01-07), and every already-committed figure. Does not re-run or alter
## precip_site_filter.R's plots (1-6) or table_4 candidate-rule analysis --
## those used, and continue to use, the 1981-2025 window (see report.md).

source("R/pipeline_config.R")
source("R/utils.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(fs)
  library(duckdb)
  library(DBI)
  library(tibble)
})

OUTD <- "review/diagnostics/precip_site_filter"
stopifnot(dir.exists(OUTD))

message("=== precip_site_filter_tower_years.R ===")

# ============================================================================
# 1. Canonical, deduplicated file manifest -- same call precip_site_filter.R
#    used, so the ERA5_YY / FLUXMET_YY file names line up with the same
#    781-site, currently-canonical file set table_1 was built from.
# ============================================================================
message("\n================ Section 1: file manifest ================")

file_manifest <- suppressWarnings(
  fluxnet::flux_discover_files(data_dir = file.path(FLUXNET_DATA_ROOT, "extracted"))
)

yy_files <- file_manifest |>
  dplyr::filter(dataset %in% c("ERA5", "FLUXMET"), time_resolution == "YY") |>
  dplyr::transmute(site_id, dataset, path, file_name = fs::path_file(path))

era5_yy_files <- yy_files |>
  dplyr::filter(dataset == "ERA5") |>
  dplyr::transmute(site_id, era5_yy_path = path, era5_yy_file = file_name)
fm_yy_files <- yy_files |>
  dplyr::filter(dataset == "FLUXMET") |>
  dplyr::transmute(site_id, fluxmet_yy_path = path, fluxmet_yy_file = file_name)

message("ERA5_YY files found: ", nrow(era5_yy_files), "; FLUXMET_YY files found: ", nrow(fm_yy_files))

# ============================================================================
# 2. Tower-years P_ERA mean: mean of annual P_ERA over the years present in
#    each site's own FLUXMET_YY file (not the full 1981-2025 ERA5 record).
# ============================================================================
message("\n================ Section 2: tower-years P_ERA mean ================")

duckdb_path <- file.path(FLUXNET_DATA_ROOT, "duckdb", "fluxnet.duckdb")
if (!file.exists(duckdb_path)) stop("DuckDB not found: ", duckdb_path)

con <- dbConnect(duckdb(), dbdir = duckdb_path, read_only = TRUE)
era5_yy <- dbGetQuery(con, "SELECT site_id, TIMESTAMP AS year, P_ERA FROM annual_converted WHERE dataset = 'ERA5'")
fm_yy   <- dbGetQuery(con, "SELECT site_id, TIMESTAMP AS year, P_F FROM annual_converted WHERE dataset = 'FLUXMET'")
dbDisconnect(con, shutdown = TRUE)

# "Years present in that site's FLUXMET_YY file" = every TIMESTAMP row for
# that site in the FLUXMET annual table, regardless of whether P_F itself is
# NA -- the file's row set defines "tower years", not the P_F QC state.
fm_tower_years <- fm_yy |> dplyr::distinct(site_id, year)
message(nrow(fm_tower_years), " site-years present in FLUXMET_YY files, network-wide.")

era5_tower <- era5_yy |>
  dplyr::filter(!is.na(P_ERA)) |>
  dplyr::inner_join(fm_tower_years, by = c("site_id", "year")) |>
  dplyr::group_by(site_id) |>
  dplyr::summarise(
    p_era_mean_mm_tower_years = mean(P_ERA),
    n_years_era_tower_years   = dplyr::n(),
    .groups = "drop"
  )
message("Sites with a tower-years P_ERA mean: ", nrow(era5_tower))

n_dropped <- fm_tower_years |> dplyr::anti_join(era5_yy |> dplyr::filter(!is.na(P_ERA)), by = c("site_id", "year")) |> nrow()
message(n_dropped, " site-years present in a FLUXMET_YY file had no corresponding non-NA ERA5_YY year ",
        "(e.g. tower years outside the ERA5 record) and were excluded from the tower-years mean.")

# ============================================================================
# 3. Update table_1: rename p_era_mean_mm -> p_era_mean_mm_1981_2025, add the
#    tower-years mean + its own n_years. Every other column/value unchanged.
# ============================================================================
message("\n================ Section 3: update table_1 ================")

table1_path <- file.path(OUTD, "table_1_site_level_precip_estimates.csv")
table1 <- readr::read_csv(table1_path, show_col_types = FALSE)
stopifnot("p_era_mean_mm" %in% names(table1))

table1_updated <- table1 |>
  dplyr::rename(p_era_mean_mm_1981_2025 = p_era_mean_mm) |>
  dplyr::left_join(era5_tower, by = "site_id") |>
  dplyr::relocate(p_era_mean_mm_tower_years, n_years_era_tower_years, .after = p_era_mean_mm_1981_2025)

# Every other column/value must be unchanged -- verify, don't assume.
unchanged_cols <- setdiff(names(table1), "p_era_mean_mm")
stopifnot(identical(
  table1 |> dplyr::select(dplyr::all_of(unchanged_cols)) |> as.data.frame(),
  table1_updated |> dplyr::select(dplyr::all_of(unchanged_cols)) |> as.data.frame()
))
message("Verified: every pre-existing column's values are byte-identical to the original table_1.")

readr::write_csv(table1_updated, table1_path)
write_output_metadata(table1_path,
  input_sources = c(duckdb_path, "data/snapshots/site_worldclim.csv",
                     "canonical BIF files via flux_discover_files() (see table_5b_provenance_bif_files.csv)",
                     "ERA5_YY/FLUXMET_YY file names via flux_discover_files() (see table_5c_provenance_yy_files.csv)"),
  notes = paste0(
    "Same table as originally built by precip_site_filter.R (2026-09-21, commit 27082a4), with the ",
    "averaging window made explicit per 2026-09-22 follow-up request. p_era_mean_mm_1981_2025: mean of ",
    "YY-resolution P_ERA over the site's full ERA5 record (all non-NA years; this IS the original, ",
    "unlabelled p_era_mean_mm column, renamed only -- values unchanged, verified byte-identical). ",
    "p_era_mean_mm_tower_years / n_years_era_tower_years: mean of P_ERA (and count of years) restricted ",
    "to the years present in the site's own FLUXMET_YY file (n_years_era_tower_years <= record_length_years; ",
    "can be < record_length_years where a tower year falls outside the ERA5 1981-2025 record). ",
    "A hand check against a downloaded FLUXMET_YY file's own P_ERA column should be compared against ",
    "p_era_mean_mm_tower_years, not p_era_mean_mm_1981_2025 -- see report.md. Every other column and ",
    "every other value in this table is unchanged from the original (verified byte-identical in-script)."
  ))
message("Saved (updated): ", table1_path)

# ============================================================================
# 4. Update table_2 (column coverage) to reflect the rename + two new columns
# ============================================================================
message("\n================ Section 4: update table_2 (column coverage) ================")

n_sites <- nrow(table1_updated)
coverage <- tibble::tibble(
  column = names(table1_updated),
  n_non_na = purrr::map_int(table1_updated, ~ sum(!is.na(.x))),
  pct_non_na = round(100 * n_non_na / n_sites, 1)
)
table2_path <- file.path(OUTD, "table_2_column_coverage.csv")
readr::write_csv(coverage, table2_path)
write_output_metadata(table2_path, input_sources = table1_path,
  notes = "Non-NA coverage of every column in table_1_site_level_precip_estimates.csv, out of 781 sites -- regenerated 2026-09-22 to reflect the p_era_mean_mm rename to p_era_mean_mm_1981_2025 and the two added tower-years columns; all other rows unchanged from the original coverage table.")
message("Saved (updated): ", table2_path)

# ============================================================================
# 5. Per-site provenance: ERA5_YY and FLUXMET_YY source file names
# ============================================================================
message("\n================ Section 5: per-site YY file provenance ================")

file_info_of <- function(paths) {
  fi <- file.info(paths)
  tibble::tibble(
    path = paths,
    exists = file.exists(paths),
    mtime = format(fi$mtime, "%Y-%m-%dT%H:%M:%S"),
    size_bytes = fi$size
  )
}

prov_era5 <- era5_yy_files |>
  dplyr::transmute(site_id, dataset = "era5_yy", path = era5_yy_path, file_name = era5_yy_file) |>
  dplyr::bind_cols(file_info_of(era5_yy_files$era5_yy_path) |> dplyr::select(-path))
prov_fm <- fm_yy_files |>
  dplyr::transmute(site_id, dataset = "fluxmet_yy", path = fluxmet_yy_path, file_name = fluxmet_yy_file) |>
  dplyr::bind_cols(file_info_of(fm_yy_files$fluxmet_yy_path) |> dplyr::select(-path))

provenance_yy <- dplyr::bind_rows(prov_era5, prov_fm) |>
  dplyr::mutate(feeds = "table_1 (p_era_mean_mm_1981_2025 dataset='era5_yy'; p_era_mean_mm_tower_years window boundary dataset='fluxmet_yy'); spot_check_nine_sites.csv") |>
  dplyr::arrange(site_id, dataset)

table5c_path <- file.path(OUTD, "table_5c_provenance_yy_files.csv")
readr::write_csv(provenance_yy, table5c_path)
write_output_metadata(table5c_path,
  input_sources = "the 781 x 2 canonical ERA5_YY/FLUXMET_YY files identified by flux_discover_files()",
  notes = paste0(
    "Path, file name, mtime, byte size for every site's canonical ERA5_YY and FLUXMET_YY file (2 rows ",
    "per site, 'dataset' column distinguishes them). ERA5_YY file names carry the 1981-2025 (or similar ",
    "45-year) range at every site; FLUXMET_YY file names carry the tower's own year range -- compare the ",
    "two file_name values for any site to see this directly (e.g. a hand check against a downloaded ",
    "FLUXMET_YY file's P_ERA column reflects only its own file_name's year range, not ERA5_YY's)."
  ))
message("Saved: ", table5c_path)

# ============================================================================
# 6. Spot-check table for the nine sites
# ============================================================================
message("\n================ Section 6: spot_check_nine_sites.csv ================")

spot_sites <- c("JP-Tak", "JP-Nkm", "JP-Mse", "JP-Ta2", "JP-Fjy",
                 "CA-CF2", "PE-QFR", "AU-Fog", "NO-And")

spot_check <- table1_updated |>
  dplyr::filter(site_id %in% spot_sites) |>
  dplyr::select(site_id,
                p_era_mean_mm_1981_2025, n_years_era_full_record,
                p_era_mean_mm_tower_years, n_years_era_tower_years,
                bio12_mm, badm_map_mm,
                p_measured_mean_mm, n_years_measured) |>
  dplyr::left_join(era5_yy_files |> dplyr::select(site_id, era5_yy_file), by = "site_id") |>
  dplyr::left_join(fm_yy_files |> dplyr::select(site_id, fluxmet_yy_file), by = "site_id") |>
  dplyr::mutate(site_id = factor(site_id, levels = spot_sites)) |>
  dplyr::arrange(site_id) |>
  dplyr::mutate(site_id = as.character(site_id))

n_found <- nrow(spot_check)
if (n_found != length(spot_sites)) {
  message("NOTE: expected ", length(spot_sites), " sites, found ", n_found,
          " in table_1 -- missing: ", paste(setdiff(spot_sites, spot_check$site_id), collapse = ", "))
}

spot_check_path <- file.path(OUTD, "spot_check_nine_sites.csv")
readr::write_csv(spot_check, spot_check_path)
write_output_metadata(spot_check_path,
  input_sources = c(table1_path, table5c_path),
  notes = paste0(
    "Both P_ERA means (1981-2025 full record and tower-years), both year counts, BIO12, BADM, QC-measured ",
    "mean and year count, and both source file names, for the nine sites named in the 2026-09-22 ",
    "averaging-window follow-up request (", paste(spot_sites, collapse = ", "), "). p_measured_mean_mm/",
    "n_years_measured are NA where a site has no year with P_F_QC > QC_THRESHOLD_YY -- unchanged from ",
    "table_1, not recomputed here."
  ))
message("Saved: ", spot_check_path)
print(as.data.frame(spot_check))

message("\n=== precip_site_filter_tower_years.R complete ===")
message("See report.md for the window-labelling note this script's output supports.")
