## 07_v2_package.R -- builds sites.csv, site_years.csv, excluded_sites.csv for
## the v2 package (all kept sites under the relaxed rule), including the new
## sourced columns (precip_source, mean TA_F, Koppen class as used in the
## paper's panel A, aridity index as used in the paper's panel C, negative-GPP
## count). Also recomputes driest-month/rainy-season-split climatology for the
## full v2 kept-site set (v1's version only covers v1's 43 sites, not v2's 54).

source("RSCOTT_AGU26/code/00_config.R")

PKG_DATE_V1 <- "2026-11-23"
PKG_DATE_V2 <- "2026-11-23_v2"
PKG_DIR_V1 <- file.path(RSCOTT_ROOT, paste0("package_rscott_", PKG_DATE_V1))
PKG_DIR_V2 <- file.path(RSCOTT_ROOT, paste0("package_rscott_", PKG_DATE_V2))
fs::dir_create(PKG_DIR_V2)

candidates <- readr::read_csv(file.path(RSCOTT_ROOT, "tables", "candidate_sites.csv"), show_col_types = FALSE)
site_years <- readr::read_csv(file.path(RSCOTT_ROOT, "tables", "site_years_candidates_full.csv"), show_col_types = FALSE)
decision   <- readr::read_csv(file.path(RSCOTT_ROOT, "tables", "site_screen_decision_v2.csv"), show_col_types = FALSE)

kept_sites <- decision |> dplyr::filter(keep) |> dplyr::pull(site_id)
message("v2 kept sites: ", length(kept_sites))

con <- rscott_duckdb_connect()
sites_sql <- paste(sprintf("'%s'", kept_sites), collapse = ", ")

## ============================================================================
## Exact annual/monthly FLUXMET file names used, per kept site (same pattern
## as v1's 04_package_tables.R)
## ============================================================================
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

## ============================================================================
## Mean annual air temperature from TA_F (calendar years passing QC),
## native degC, source: data/duckdb/fluxnet.duckdb::annual (raw, NOT _converted)
## ============================================================================
message("\n================ Mean annual TA_F (kept sites) ================")

ta_raw <- dbGetQuery(con, sprintf("
  SELECT site_id, CAST(TIMESTAMP AS INTEGER) AS year, TA_F, TA_F_QC
  FROM annual WHERE dataset = 'FLUXMET' AND site_id IN (%s)
", sites_sql))
dbDisconnect(con, shutdown = TRUE)

ta_mean <- ta_raw |>
  dplyr::mutate(ta_pass = !is.na(TA_F_QC) & qc_passes(TA_F_QC, QC_THRESHOLD_YY) & !is.na(TA_F)) |>
  dplyr::group_by(site_id) |>
  dplyr::summarise(
    mean_ta_f_degc = ifelse(sum(ta_pass) > 0, mean(TA_F[ta_pass]), NA_real_),
    n_years_ta_f   = sum(ta_pass),
    .groups = "drop"
  )
message("TA_F available for ", sum(!is.na(ta_mean$mean_ta_f_degc)), " / ", nrow(ta_mean), " kept sites.")

## ============================================================================
## Koppen-Geiger class "as used in the paper's network sampling analysis"
## (figure4_representativeness.R panel A): PI-reported (BADM CLIMATE_KOEPPEN)
## where available, else the site's own ERA5-local classification; NA (with
## reason) where panel A's precip-dependent exclusion rules screen out an
## ERA5-fallback site (a PI-sourced site is NEVER excluded from panel A).
## Source: data/snapshots/site_koppen_era5_fig4.csv
## ============================================================================
message("\n================ Koppen class (paper panel A) ================")

kg_path <- "data/snapshots/site_koppen_era5_fig4.csv"
kg <- readr::read_csv(kg_path, show_col_types = FALSE)
kg_kept <- tibble::tibble(site_id = kept_sites) |>
  dplyr::left_join(dplyr::select(kg, site_id, panel_a_source, panel_a_class_used, panel_a_eligible), by = "site_id") |>
  dplyr::mutate(
    koppen_class_paper = dplyr::if_else(panel_a_eligible == TRUE, panel_a_class_used, NA_character_),
    koppen_na_reason = dplyr::case_when(
      is.na(panel_a_source) ~ "site not present in the current-network site_koppen_era5_fig4.csv table",
      panel_a_eligible ~ NA_character_,
      TRUE ~ paste0("panel_a_source = ", panel_a_source, " (excluded from the paper's Koppen panel A Geo-vs-Data pool)")
    )
  ) |>
  dplyr::select(site_id, koppen_class_paper, koppen_na_reason)
message("Koppen class available for ", sum(!is.na(kg_kept$koppen_class_paper)), " / ", nrow(kg_kept), " kept sites.")
if (any(is.na(kg_kept$koppen_class_paper))) {
  message("  NA reasons: "); print(table(kg_kept$koppen_na_reason[is.na(kg_kept$koppen_class_paper)], useNA = "ifany"))
}

## ============================================================================
## Aridity index "as used in the paper" (figure4_representativeness.R panel C,
## fao56_et0_mm(): AI = P_ERA / FAO-56 Penman-Monteith reference ET, computed
## from the site's OWN 1991-2020 ERA5 monthly climatology -- NOT the external
## CGIAR Aridity Index v3.1 raster used elsewhere in this repo).
## Source: data/snapshots/site_aridity_era5_fig4.csv
## ============================================================================
message("\n================ Aridity index (paper panel C) ================")

ar_path <- "data/snapshots/site_aridity_era5_fig4.csv"
ar <- readr::read_csv(ar_path, show_col_types = FALSE)
ar_kept <- tibble::tibble(site_id = kept_sites) |>
  dplyr::left_join(ar, by = "site_id") |>
  dplyr::mutate(
    aridity_na_reason = dplyr::case_when(
      is.na(ai_value) & is.na(invalid_era5_input) ~ "site not present in the current-network site_aridity_era5_fig4.csv table",
      isTRUE(invalid_era5_input) ~ "invalid_era5_input (a raw ERA5 meteorological value used by the FAO-56 PET calculation is physically impossible in >=1 calendar month)",
      isTRUE(excluded_grp_era_down) ~ "excluded_grp_era_down (site's ERA5 precipitation downscaling regression was never fitted)",
      isTRUE(excluded_p_era_ratio_high) ~ sprintf("excluded_p_era_ratio_high (1991-2020 mean P_ERA exceeds %sx every available reference: BADM MAP and/or WorldClim BIO12)", P_ERA_MAX_RATIO),
      isTRUE(excluded_p_era_ratio_low)  ~ sprintf("excluded_p_era_ratio_low (1991-2020 mean P_ERA is below %.3fx every available reference)", P_ERA_MIN_RATIO),
      TRUE ~ NA_character_
    ),
    aridity_index_paper = dplyr::if_else(is.na(aridity_na_reason), ai_value, NA_real_)
  ) |>
  dplyr::select(site_id, aridity_index_paper, aridity_na_reason)
message("Aridity index available for ", sum(!is.na(ar_kept$aridity_index_paper)), " / ", nrow(ar_kept), " kept sites.")
if (any(is.na(ar_kept$aridity_index_paper))) {
  message("  NA reasons: "); print(table(ar_kept$aridity_na_reason[is.na(ar_kept$aridity_index_paper)], useNA = "ifany"))
}

## ============================================================================
## Driest-month climatology and rainy-season-split flag, recomputed for the
## FULL v2 kept-site set (54 sites; v1's driest_month_climatology.csv only
## covers v1's 43). Identical method to 03_screen_and_climatology.R.
## ============================================================================
message("\n================ Driest-month climatology (v2 kept sites) ================")

con <- rscott_duckdb_connect()
dd_clim <- dbGetQuery(con, sprintf("
  SELECT site_id, TIMESTAMP, P_F, P_F_QC FROM daily WHERE dataset = 'FLUXMET' AND site_id IN (%s)
", sites_sql))
dbDisconnect(con, shutdown = TRUE)

monthly_clim <- dd_clim |>
  dplyr::mutate(
    date = as.Date(TIMESTAMP), cal_month = lubridate::month(date), cal_year = lubridate::year(date),
    p_measured_day = P_F * P_F_QC
  ) |>
  dplyr::filter(!is.na(p_measured_day)) |>
  dplyr::group_by(site_id, cal_year, cal_month) |>
  dplyr::summarise(p_measured_month_total = sum(p_measured_day), .groups = "drop") |>
  dplyr::group_by(site_id, cal_month) |>
  dplyr::summarise(mean_measured_p_month_mm = mean(p_measured_month_total), .groups = "drop")

driest <- monthly_clim |>
  dplyr::group_by(site_id) |>
  dplyr::slice_min(mean_measured_p_month_mm, n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::transmute(site_id, driest_month = cal_month, driest_month_mean_measured_p_mm = mean_measured_p_month_mm)

wet_rank <- monthly_clim |>
  dplyr::group_by(site_id) |>
  dplyr::mutate(wet_rank = rank(-mean_measured_p_month_mm, ties.method = "first")) |>
  dplyr::ungroup()
top4_wet <- wet_rank |> dplyr::filter(wet_rank <= 4) |> dplyr::select(site_id, cal_month)

boundary_month_fn <- function(hemisphere) ifelse(hemisphere == "N", 10L, 7L)
rainy_flag <- candidates |>
  dplyr::filter(site_id %in% kept_sites) |>
  dplyr::select(site_id, hemisphere) |>
  dplyr::mutate(boundary = boundary_month_fn(hemisphere), month_before_boundary = ((boundary - 2) %% 12) + 1)

is_wet <- function(site, month) month %in% top4_wet$cal_month[top4_wet$site_id == site]
rainy_flag$rainy_season_split_risk <- vapply(seq_len(nrow(rainy_flag)), function(i)
  is_wet(rainy_flag$site_id[i], rainy_flag$month_before_boundary[i]) &&
  is_wet(rainy_flag$site_id[i], rainy_flag$boundary[i]), logical(1))
rainy_flag <- rainy_flag |> dplyr::select(site_id, boundary_month = boundary, rainy_season_split_risk)

message("v2 kept sites with rainy-season-split risk flagged: ", sum(rainy_flag$rainy_season_split_risk))

out_clim <- file.path(RSCOTT_ROOT, "tables", "driest_month_climatology_v2.csv")
readr::write_csv(driest |> dplyr::left_join(rainy_flag, by = "site_id"), out_clim)
write_meta(out_clim, input_sources = RSCOTT_DUCKDB_PATH,
  notes = "Recomputed for the full v2 kept-site set (54 sites), identical method to v1's driest_month_climatology.csv (03_screen_and_climatology.R).")

## ============================================================================
## sites.csv (v2): all v1 columns retained, plus precip_source,
## n_measured_years_calendar, mean_ta_f_degc, koppen_class_paper,
## aridity_index_paper, n_siteyears_negative_gpp.
## ============================================================================
message("\n================ Building v2 sites.csv ================")

sites_tbl <- candidates |>
  dplyr::filter(site_id %in% kept_sites) |>
  dplyr::select(site_id, site_name, country, igbp, location_lat, location_long, hemisphere,
                product_citation, product_id, oneflux_code_version) |>
  dplyr::mutate(
    water_year_convention = ifelse(hemisphere == "N", "Oct-Sep (labelled by the year it ends)", "Jul-Jun (labelled by the year it ends)")
  ) |>
  dplyr::left_join(readr::read_csv(out_clim, show_col_types = FALSE), by = "site_id") |>
  dplyr::left_join(
    decision |> dplyr::select(
      site_id, precip_source, n_measured_years_calendar, mean_annual_p_mm,
      n_passing_calendar_relaxed, n_passing_calendar_relaxed_and_measured,
      n_passing_water_relaxed, n_passing_water_relaxed_and_measured,
      n_siteyears_negative_gpp
    ),
    by = "site_id"
  ) |>
  dplyr::left_join(ta_mean, by = "site_id") |>
  dplyr::left_join(kg_kept, by = "site_id") |>
  dplyr::left_join(ar_kept, by = "site_id") |>
  dplyr::left_join(file_names, by = "site_id") |>
  dplyr::mutate(driest_month_name = month.abb[driest_month]) |>
  dplyr::select(
    site_id, site_name, country, igbp,
    location_lat, location_long, hemisphere, water_year_convention,
    driest_month_name, driest_month_mean_measured_p_mm, rainy_season_split_risk, boundary_month,
    precip_source, n_measured_years_calendar, mean_annual_p_mm,
    n_passing_calendar_relaxed, n_passing_calendar_relaxed_and_measured,
    n_passing_water_relaxed, n_passing_water_relaxed_and_measured,
    mean_ta_f_degc, n_years_ta_f,
    koppen_class_paper, koppen_na_reason,
    aridity_index_paper, aridity_na_reason,
    n_siteyears_negative_gpp,
    product_id, product_citation, oneflux_code_version,
    file_name_annual, file_name_monthly
  ) |>
  dplyr::arrange(site_id)

message("v2 sites.csv rows: ", nrow(sites_tbl))
out_sites <- file.path(PKG_DIR_V2, "sites.csv")
readr::write_csv(sites_tbl, out_sites)
write_meta(out_sites, input_sources = c(
    file.path(RSCOTT_ROOT, "tables", "candidate_sites.csv"),
    file.path(RSCOTT_ROOT, "tables", "site_screen_decision_v2.csv"),
    out_clim, RSCOTT_DUCKDB_PATH, kg_path, ar_path
  ),
  notes = paste0(
    "v2 widened package: one row per kept site under the relaxed rule (54 sites). precip_source/",
    "n_measured_years_calendar/mean_annual_p_mm from site_screen_decision_v2.csv (RSCOTT_AGU26/code/",
    "06_v2_relaxed_selection.R). mean_ta_f_degc: mean TA_F (native degC) over calendar years where ",
    "qc_passes(TA_F_QC, QC_THRESHOLD_YY), from data/duckdb/fluxnet.duckdb::annual (raw, not ",
    "annual_converted -- TA_F is reported here in native degC, not K). koppen_class_paper: PI-reported ",
    "(BADM CLIMATE_KOEPPEN) class else ERA5-local fallback, exactly as used by scripts/",
    "figure4_representativeness.R panel A (site_koppen_era5_fig4.csv's panel_a_class_used/",
    "panel_a_eligible); NA with koppen_na_reason where panel A's own exclusion rule applies to an ",
    "ERA5-fallback site (never for a PI-sourced site). aridity_index_paper: AI = 1991-2020 mean P_ERA / ",
    "FAO-56 Penman-Monteith reference ET computed from the site's own ERA5 record, exactly as computed ",
    "by figure4_representativeness.R panel C (fao56_et0_mm(), site_aridity_era5_fig4.csv's ai_value); ",
    "NA with aridity_na_reason where the paper's own panel C exclusion rules apply (invalid ERA5 input, ",
    "or P_ERA ratio screen vs BADM MAP/WorldClim BIO12). This aridity index is NOT the CGIAR Aridity ",
    "Index v3.1 raster used elsewhere in this repo (data/external/aridity/) -- it is the site's-own-",
    "ERA5-record version the paper itself uses for its representativeness figure. ",
    "n_siteyears_negative_gpp: count of this site's site-year rows (both time steps, from ",
    "site_years_candidates_full.csv) with GPP_NT < 0 OR GPP_DT < 0."
  ))
message("Saved: ", out_sites)

## ============================================================================
## site_years.csv (v2): kept sites, every site-year, with a per-year
## precip_source column (measured / ERA-filled / unknown).
## ============================================================================
message("\n================ Building v2 site_years.csv ================")

site_years_kept <- site_years |>
  dplyr::filter(site_id %in% kept_sites) |>
  dplyr::mutate(
    pass_precip_present = !is.na(p_f_mm_yr),
    precip_source = dplyr::case_when(
      is.na(era_filled_share) ~ NA_character_,
      era_filled_share <= RSCOTT_ERA_FILLED_MAX_SHARE ~ "measured",
      TRUE ~ "ERA-filled"
    )
  ) |>
  dplyr::arrange(site_id, time_step, year)

message("v2 site_years.csv rows: ", nrow(site_years_kept))
message("Per-year precip_source counts: ", paste(capture.output(print(table(site_years_kept$precip_source, useNA = "ifany"))), collapse = " | "))

out_sy <- file.path(PKG_DIR_V2, "site_years.csv")
readr::write_csv(site_years_kept, out_sy)
write_meta(out_sy, input_sources = file.path(RSCOTT_ROOT, "tables", "site_years_candidates_full.csv"),
  notes = paste0(
    "All 54 v2-kept sites, every site-year (passing or not), both time steps. Adds pass_precip_present ",
    "(P_F non-NA, the relaxed per-year precip condition -- replaces pass_precip_measured in the v2 keep ",
    "rule, though pass_precip_measured is still reported for reference) and precip_source (per year: ",
    "'measured' if era_filled_share <= 10%, 'ERA-filled' otherwise, NA if era_filled_share itself ",
    "could not be computed for that row -- distinct from sites.csv's SITE-level precip_source column, ",
    "which uses the values 'measured'/'ERA5-only' and is based only on whether >=1 calendar year ",
    "qualifies). GPP in gC m-2 per period; ET and P in mm per period."
  ))
message("Saved: ", out_sy)

## ============================================================================
## excluded_sites.csv (v2)
## ============================================================================
message("\n================ Building v2 excluded_sites.csv ================")

excluded_tbl <- decision |>
  dplyr::filter(!keep) |>
  dplyr::left_join(candidates |> dplyr::select(site_id, site_name, igbp, hemisphere, first_year, last_year), by = "site_id") |>
  dplyr::mutate(record_length_years = last_year - first_year + 1L) |>
  dplyr::select(
    site_id, site_name, igbp, hemisphere, first_year, last_year, record_length_years,
    precip_source, n_measured_years_calendar, mean_annual_p_mm,
    n_passing_calendar_relaxed, n_passing_calendar_relaxed_and_measured,
    n_passing_water_relaxed, n_passing_water_relaxed_and_measured,
    n_siteyears_negative_gpp,
    exclusion_reason
  ) |>
  dplyr::arrange(site_id)

message("v2 excluded_sites.csv rows: ", nrow(excluded_tbl))
out_excl <- file.path(PKG_DIR_V2, "excluded_sites.csv")
readr::write_csv(excluded_tbl, out_excl)
write_meta(out_excl, input_sources = file.path(RSCOTT_ROOT, "tables", "site_screen_decision_v2.csv"),
  notes = "Every IGBP-screened candidate site not kept under the v2 relaxed rule, with its reason and the counts behind it.")
message("Saved: ", out_excl)

message("\nPackage directory: ", PKG_DIR_V2)
message("07_v2_package.R complete.")
