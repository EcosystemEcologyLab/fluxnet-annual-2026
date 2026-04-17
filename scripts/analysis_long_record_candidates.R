# scripts/analysis_long_record_candidates.R
# Analysis-only script — no code changes.
# Generates outputs/long_record_site_candidates.csv and prints summary tables.
#
# Reproduces and audits site selection logic in fig_long_record_timeseries().

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(countrycode)
  library(fs)
})

# ---- 1. Print current selection criteria -------------------------------------
cat("=======================================================\n")
cat("  CURRENT SELECTION CRITERIA IN fig_long_record_timeseries()\n")
cat("=======================================================\n\n")

cat("Function signature:\n")
cat("  fig_long_record_timeseries(\n")
cat("    data_yy, metadata,\n")
cat("    flux_vars = c('NEE_VUT_REF', 'GPP_NT_VUT_REF', 'RECO_NT_VUT_REF',\n")
cat("                  'LE_F_MDS', 'H_F_MDS'),\n")
cat("    n_sites   = 5L,\n")
cat("    geo_level = 'continent'\n")
cat("  )\n\n")

cat("Continent assignment (R/figures/fig_timeseries.R lines 229-241):\n")
cat("  iso2 <- substr(metadata$site_id, 1L, 2L)\n")
cat("  continent <- .iso2_to_continent(iso2)\n")
cat("  .iso2_to_continent() normalises 'UK' -> 'GB', then calls:\n")
cat("    countrycode::countrycode(iso2_norm, origin='iso2c',\n")
cat("                             destination='continent', warn=FALSE)\n")
cat("  Uses UN Geoscheme 6-continent system.\n")
cat("  Sites with NA continent are dropped before selection.\n\n")

cat("Record length calculation (line 239):\n")
cat("  span = last_year - first_year   [integer subtraction from snapshot metadata]\n")
cat("  NOTE: span is the calendar span, NOT n_years_valid_nee.\n")
cat("  No actual NEE data coverage check is performed during site selection.\n\n")

cat("Site selection (lines 244-248):\n")
cat("  selected_sites <- site_meta |>\n")
cat("    group_by(continent) |>\n")
cat("    slice_max(order_by = span, n = n_sites, with_ties = FALSE) |>\n")
cat("    ungroup()\n")
cat("  Selects top n_sites = 5 per continent by maximum span.\n")
cat("  Ties broken arbitrarily (with_ties = FALSE, row order determines winner).\n\n")

# ---- 2. Load data ------------------------------------------------------------
cat("=======================================================\n")
cat("  LOADING DATA\n")
cat("=======================================================\n\n")

# Latest snapshot — must match the shuttle snapshot naming pattern
snap_files <- sort(fs::dir_ls("data/snapshots",
                               glob = "*fluxnet_shuttle_snapshot*.csv"))
snap_path  <- tail(snap_files, 1)
cat("Snapshot:", snap_path, "\n")
snapshot   <- readr::read_csv(snap_path, show_col_types = FALSE)

# Processed annual data — prefer converted (units fixed), fall back to qc
yy_path <- if (fs::file_exists("data/processed/flux_data_converted_yy.rds")) {
  "data/processed/flux_data_converted_yy.rds"
} else if (fs::file_exists("data/processed/flux_data_qc_yy.rds")) {
  "data/processed/flux_data_qc_yy.rds"
} else {
  stop("No annual processed data found in data/processed/")
}
cat("Annual data:", yy_path, "\n\n")
data_yy <- readRDS(yy_path)

# ---- 3. Identify the year column --------------------------------------------
year_col <- if ("TIMESTAMP" %in% names(data_yy)) "TIMESTAMP" else "YEAR"
cat("Year column used:", year_col, "\n")

# ---- 4. Count valid NEE years per site ---------------------------------------
cat("Counting valid NEE years per site...\n")
nee_counts <- data_yy |>
  dplyr::select(site_id, year = dplyr::all_of(year_col), NEE_VUT_REF) |>
  dplyr::mutate(year = as.integer(year)) |>
  dplyr::group_by(site_id) |>
  dplyr::summarise(
    n_years_valid_nee = sum(!is.na(NEE_VUT_REF)),
    .groups = "drop"
  )

cat("  Sites with any NEE data:", sum(nee_counts$n_years_valid_nee > 0), "\n\n")

# ---- 5. Build site metadata from snapshot ------------------------------------
# Identify needed columns — handle different snapshot column naming conventions
snap_cols <- names(snapshot)
cat("Snapshot columns available:", paste(head(snap_cols, 20), collapse = ", "), "\n\n")

# Normalise column names to expected pipeline values
# network/hub column
hub_col <- intersect(c("network", "data_hub", "hub"), snap_cols)[1]
igbp_col <- intersect(c("igbp", "IGBP", "igbp_class"), snap_cols)[1]
fy_col   <- intersect(c("first_year", "year_start", "start_year"), snap_cols)[1]
ly_col   <- intersect(c("last_year", "year_end", "end_year"), snap_cols)[1]

cat(sprintf("Using snapshot columns: hub='%s', igbp='%s', first_year='%s', last_year='%s'\n\n",
            hub_col, igbp_col, fy_col, ly_col))

site_meta <- snapshot |>
  dplyr::select(
    site_id    = site_id,
    data_hub   = dplyr::all_of(hub_col),
    igbp       = dplyr::all_of(igbp_col),
    first_year = dplyr::all_of(fy_col),
    last_year  = dplyr::all_of(ly_col)
  ) |>
  dplyr::mutate(
    first_year = as.integer(first_year),
    last_year  = as.integer(last_year),
    span_years = last_year - first_year
  )

# ---- 6. Add continent and UN subregion via countrycode -----------------------
iso2_raw  <- substr(site_meta$site_id, 1L, 2L)
iso2_norm <- dplyr::case_when(iso2_raw == "UK" ~ "GB", TRUE ~ iso2_raw)

site_meta <- site_meta |>
  dplyr::mutate(
    iso2        = iso2_norm,
    continent   = countrycode::countrycode(iso2_norm, origin = "iso2c",
                                           destination = "continent", warn = FALSE),
    un_subregion = countrycode::countrycode(iso2_norm, origin = "iso2c",
                                            destination = "un.regionsub.name", warn = FALSE)
  )

# ---- 7. Join NEE counts and filter to sites with valid NEE ------------------
candidate_base <- site_meta |>
  dplyr::left_join(nee_counts, by = "site_id") |>
  dplyr::mutate(
    n_years_valid_nee = dplyr::coalesce(as.integer(n_years_valid_nee), 0L),
    pct_coverage      = dplyr::if_else(
      span_years > 0,
      round(n_years_valid_nee / span_years * 100, 1),
      NA_real_
    )
  )

# Sites with valid NEE data (at least 1 year)
candidates <- candidate_base |>
  dplyr::filter(n_years_valid_nee > 0)

cat("Sites with valid NEE (n_years_valid_nee > 0):", nrow(candidates), "\n")
cat("Sites with no valid NEE data:", nrow(candidate_base) - nrow(candidates), "\n\n")

# ---- 8. Reproduce current selection (top 5 per continent by span) -----------
# Use same logic as fig_long_record_timeseries: span from snapshot, not NEE count
# Sites must have continent (NA dropped)
eligible <- candidates |> dplyr::filter(!is.na(continent))

selected_sites <- eligible |>
  dplyr::group_by(continent) |>
  dplyr::slice_max(order_by = span_years, n = 5L, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::pull(site_id)

candidates <- candidates |>
  dplyr::mutate(currently_selected = site_id %in% selected_sites)

# ---- 9. Final column order and save -----------------------------------------
output_tbl <- candidates |>
  dplyr::select(
    site_id, data_hub, igbp, un_subregion, continent,
    first_year, last_year, span_years,
    n_years_valid_nee, pct_coverage, currently_selected
  ) |>
  dplyr::arrange(continent, dplyr::desc(n_years_valid_nee))

fs::dir_create("outputs")
out_path <- "outputs/long_record_site_candidates.csv"
readr::write_csv(output_tbl, out_path)
cat("Saved:", out_path, "(", nrow(output_tbl), "rows )\n\n")

# ---- 10. Summary tables ------------------------------------------------------
cat("=======================================================\n")
cat("  SUMMARY: SITES WITH VALID NEE BY CONTINENT\n")
cat("=======================================================\n\n")

continents_all <- sort(unique(na.omit(eligible$continent)))

for (cont in continents_all) {
  cont_data <- eligible |>
    dplyr::filter(continent == cont) |>
    dplyr::arrange(dplyr::desc(span_years), dplyr::desc(n_years_valid_nee))

  n_total <- nrow(cont_data)
  selected_cont <- cont_data |> dplyr::filter(site_id %in% selected_sites)
  next5_cont    <- cont_data |>
    dplyr::filter(!site_id %in% selected_sites) |>
    dplyr::slice_max(order_by = n_years_valid_nee, n = 5L, with_ties = FALSE)

  cat(sprintf("--- %s  (%d sites with valid NEE) ---\n", cont, n_total))

  if (n_total < 5L) {
    cat(sprintf("  *** FEWER THAN 5 SITES — only %d available ***\n", n_total))
  }

  cat("\n  Currently selected (top 5 by span_years):\n")
  cat(sprintf("  %-12s  %-6s  %-4s  %5s  %8s  %8s  %12s\n",
              "site_id", "hub", "igbp", "span", "nee_yrs", "pct_cov", "selected_by"))
  cat("  ", strrep("-", 68), "\n", sep = "")
  for (i in seq_len(nrow(selected_cont))) {
    r <- selected_cont[i, ]
    cat(sprintf("  %-12s  %-6s  %-4s  %5d  %8d  %7.1f%%  %s\n",
                r$site_id, r$data_hub, r$igbp,
                r$span_years, r$n_years_valid_nee, r$pct_coverage,
                "span_years"))
  }

  if (nrow(next5_cont) > 0L) {
    cat("\n  Next 5 by n_years_valid_nee (would be selected if criterion changed):\n")
    cat(sprintf("  %-12s  %-6s  %-4s  %5s  %8s  %8s\n",
                "site_id", "hub", "igbp", "span", "nee_yrs", "pct_cov"))
    cat("  ", strrep("-", 58), "\n", sep = "")
    for (i in seq_len(nrow(next5_cont))) {
      r <- next5_cont[i, ]
      cat(sprintf("  %-12s  %-6s  %-4s  %5d  %8d  %7.1f%%\n",
                  r$site_id, r$data_hub, r$igbp,
                  r$span_years, r$n_years_valid_nee, r$pct_coverage))
    }
  } else {
    cat("  (no additional sites with valid NEE for this continent)\n")
  }
  cat("\n")
}

# ---- 11. Sites with no continent assignment ----------------------------------
no_continent <- candidate_base |>
  dplyr::filter(is.na(continent), n_years_valid_nee > 0)
if (nrow(no_continent) > 0L) {
  cat("=======================================================\n")
  cat("  SITES WITH VALID NEE BUT NO CONTINENT ASSIGNMENT\n")
  cat("  (dropped from selection — check ISO-2 prefix)\n")
  cat("=======================================================\n")
  print(dplyr::select(no_continent, site_id, data_hub, igbp, span_years, n_years_valid_nee))
  cat("\n")
}

# ---- 12. Cross-check: currently-selected sites missing from processed data --
selected_in_snap <- snapshot$site_id[
  snapshot$site_id %in% selected_sites
]
missing_from_yy <- setdiff(selected_sites,
                           unique(data_yy$site_id[!is.na(data_yy$NEE_VUT_REF)]))
if (length(missing_from_yy) > 0L) {
  cat("=======================================================\n")
  cat("  WARNING: SELECTED SITES WITH NO VALID NEE IN PROCESSED DATA\n")
  cat("=======================================================\n")
  cat("These sites are currently selected by span but have no valid NEE:\n")
  cat(paste(" ", missing_from_yy, collapse = "\n"), "\n\n")
} else {
  cat("All currently-selected sites have valid NEE in the processed data.\n\n")
}

cat("Done.\n")
