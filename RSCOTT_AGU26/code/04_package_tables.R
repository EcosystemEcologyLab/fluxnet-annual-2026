## 04_package_tables.R — builds sites.csv, site_years.csv, excluded_sites.csv
## for the final package (step 6), from the screening/climatology outputs of
## 03_screen_and_climatology.R.

source("RSCOTT_AGU26/code/00_config.R")

PKG_DATE <- "2026-11-23"
PKG_DIR <- file.path(RSCOTT_ROOT, paste0("package_rscott_", PKG_DATE))
fs::dir_create(PKG_DIR)

candidates <- readr::read_csv(file.path(RSCOTT_ROOT, "tables", "candidate_sites.csv"), show_col_types = FALSE)
site_years <- readr::read_csv(file.path(RSCOTT_ROOT, "tables", "site_years_candidates_full.csv"), show_col_types = FALSE)
decision <- readr::read_csv(file.path(RSCOTT_ROOT, "tables", "site_screen_decision.csv"), show_col_types = FALSE)
clim <- readr::read_csv(file.path(RSCOTT_ROOT, "tables", "driest_month_climatology.csv"), show_col_types = FALSE)

kept_sites <- decision |> dplyr::filter(keep) |> dplyr::pull(site_id)
message("Kept sites: ", length(kept_sites))

## ============================================================================
## Exact annual/monthly FLUXMET file names used, per kept site
## ============================================================================
con <- rscott_duckdb_connect()
sites_sql <- paste(sprintf("'%s'", kept_sites), collapse = ", ")
file_names <- dbGetQuery(con, sprintf("
  SELECT site_id, 'annual' AS which, path FROM annual WHERE dataset = 'FLUXMET' AND site_id IN (%s)
  UNION
  SELECT site_id, 'monthly' AS which, path FROM monthly WHERE dataset = 'FLUXMET' AND site_id IN (%s)
", sites_sql, sites_sql)) |>
  dplyr::distinct() |>
  dplyr::mutate(file_name = basename(path)) |>
  dplyr::select(site_id, which, file_name) |>
  tidyr::pivot_wider(names_from = which, values_from = file_name, names_prefix = "file_name_",
                      values_fn = function(x) paste(x, collapse = "; "))
dbDisconnect(con, shutdown = TRUE)

## ============================================================================
## sites.csv
## ============================================================================
message("\n================ Building sites.csv ================")

sites_tbl <- candidates |>
  dplyr::filter(site_id %in% kept_sites) |>
  dplyr::select(site_id, site_name, country, igbp, location_lat, location_long, hemisphere,
                product_citation, product_id, oneflux_code_version) |>
  dplyr::mutate(
    water_year_convention = ifelse(hemisphere == "N", "Oct-Sep (labelled by the year it ends)", "Jul-Jun (labelled by the year it ends)")
  ) |>
  dplyr::left_join(clim, by = "site_id") |>
  dplyr::left_join(
    decision |> dplyr::select(
      site_id, mean_annual_measured_p_mm, n_measured_years_calendar,
      n_passing_calendar_with_precip, n_passing_calendar_without_precip,
      n_passing_water_with_precip, n_passing_water_without_precip
    ),
    by = "site_id"
  ) |>
  dplyr::left_join(file_names, by = "site_id") |>
  dplyr::mutate(driest_month_name = month.abb[driest_month]) |>
  dplyr::select(
    site_id, site_name, country, igbp,
    location_lat, location_long, hemisphere, water_year_convention,
    driest_month_name, driest_month_mean_measured_p_mm, rainy_season_split_risk, boundary_month,
    mean_annual_measured_p_mm, n_measured_years_calendar,
    n_passing_calendar_with_precip, n_passing_calendar_without_precip,
    n_passing_water_with_precip, n_passing_water_without_precip,
    product_id, product_citation, oneflux_code_version,
    file_name_annual, file_name_monthly
  ) |>
  dplyr::arrange(site_id)

message("sites.csv rows: ", nrow(sites_tbl))
out_sites <- file.path(PKG_DIR, "sites.csv")
readr::write_csv(sites_tbl, out_sites)
write_meta(out_sites, input_sources = c(
    file.path(RSCOTT_ROOT, "tables", "candidate_sites.csv"),
    file.path(RSCOTT_ROOT, "tables", "site_screen_decision.csv"),
    file.path(RSCOTT_ROOT, "tables", "driest_month_climatology.csv"),
    RSCOTT_DUCKDB_PATH
  ),
  notes = "One row per kept site. product_id/product_citation/oneflux_code_version from the 2026-09-20 snapshot. file_name_annual/file_name_monthly are the exact FLUXMET file names (basename of the manifest path) used for that site's YY/MM data.")
message("Saved: ", out_sites)

## ============================================================================
## site_years.csv (kept sites only, every site-year, passing or not)
## ============================================================================
message("\n================ Building site_years.csv ================")

site_years_kept <- site_years |> dplyr::filter(site_id %in% kept_sites) |> dplyr::arrange(site_id, time_step, year)
message("site_years.csv rows: ", nrow(site_years_kept))

out_sy <- file.path(PKG_DIR, "site_years.csv")
readr::write_csv(site_years_kept, out_sy)
write_meta(out_sy, input_sources = file.path(RSCOTT_ROOT, "tables", "site_years_candidates_full.csv"),
  notes = "Every site-year (passing or not) of every kept site, both time steps (calendar from YY, water from MM/DD -- see README.md for which resolution backs which column). GPP in gC m-2 per period; ET and P in mm per period.")
message("Saved: ", out_sy)

## ============================================================================
## excluded_sites.csv
## ============================================================================
message("\n================ Building excluded_sites.csv ================")

excluded_tbl <- decision |>
  dplyr::filter(!keep) |>
  dplyr::left_join(candidates |> dplyr::select(site_id, site_name, igbp, hemisphere, first_year, last_year), by = "site_id") |>
  dplyr::mutate(record_length_years = last_year - first_year + 1L) |>
  dplyr::select(
    site_id, site_name, igbp, hemisphere, first_year, last_year, record_length_years,
    n_calendar_years_total, n_measured_years_calendar, mean_annual_measured_p_mm,
    n_passing_calendar_with_precip, n_passing_calendar_without_precip,
    n_passing_water_with_precip, n_passing_water_without_precip,
    exclusion_reason
  ) |>
  dplyr::arrange(site_id)

message("excluded_sites.csv rows: ", nrow(excluded_tbl))
out_excl <- file.path(PKG_DIR, "excluded_sites.csv")
readr::write_csv(excluded_tbl, out_excl)
write_meta(out_excl, input_sources = c(
    file.path(RSCOTT_ROOT, "tables", "site_screen_decision.csv"),
    file.path(RSCOTT_ROOT, "tables", "candidate_sites.csv")
  ),
  notes = "Every IGBP-screened candidate site not kept, with its reason and the counts behind it (fewer than 2 measured calendar years = unscreenable; mean annual measured P >= 800 mm; or fewer than 5 passing years in either time step).")
message("Saved: ", out_excl)

message("\nPackage directory: ", PKG_DIR)
message("04_package_tables.R complete.")
