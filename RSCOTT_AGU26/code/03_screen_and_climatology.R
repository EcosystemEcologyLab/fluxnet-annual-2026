## 03_screen_and_climatology.R — site screen (step 3), passing-year counts
## (step 5), and the driest-month / rainy-season-split check for sites.csv.
##
## Screen (step 3, CALENDAR YEAR only, per the task):
##   - measured calendar years = pass_precip_measured (era_filled_share <= 0.10)
##   - need >= RSCOTT_MIN_MEASURED_YEARS_FOR_SCREEN such years to screen at all
##   - mean_annual_measured_P (mean p_f_mm_yr over those years) must be
##     < RSCOTT_MAP_SCREEN_MM
##
## Keep (step 5): >= RSCOTT_MIN_PASSING_YEARS_TO_KEEP years where GPP_NT, ET
## (LE_F_MDS) and measured precipitation ALL pass, counted separately for
## calendar and water years; kept if EITHER count reaches the threshold.

source("RSCOTT_AGU26/code/00_config.R")

candidates <- readr::read_csv(file.path(RSCOTT_ROOT, "tables", "candidate_sites.csv"), show_col_types = FALSE)
site_years <- readr::read_csv(file.path(RSCOTT_ROOT, "tables", "site_years_candidates_full.csv"), show_col_types = FALSE)

## ============================================================================
## Step 3: screen on calendar-year measured precipitation
## ============================================================================
message("\n================ Step 3: site screen (calendar year) ================")

cal <- site_years |> dplyr::filter(time_step == "calendar")

screen <- cal |>
  dplyr::group_by(site_id) |>
  dplyr::summarise(
    n_calendar_years_total = dplyr::n(),
    n_measured_years_calendar = sum(pass_precip_measured, na.rm = TRUE),
    mean_annual_measured_p_mm = ifelse(
      n_measured_years_calendar >= RSCOTT_MIN_MEASURED_YEARS_FOR_SCREEN,
      mean(p_f_mm_yr[pass_precip_measured], na.rm = TRUE),
      NA_real_
    ),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    screenable = n_measured_years_calendar >= RSCOTT_MIN_MEASURED_YEARS_FOR_SCREEN,
    screen_pass = screenable & !is.na(mean_annual_measured_p_mm) & mean_annual_measured_p_mm < RSCOTT_MAP_SCREEN_MM
  )

message("Candidate sites: ", nrow(screen))
message("  Unscreenable (< ", RSCOTT_MIN_MEASURED_YEARS_FOR_SCREEN, " measured calendar years): ",
        sum(!screen$screenable))
message("  Screenable but mean P >= ", RSCOTT_MAP_SCREEN_MM, " mm: ",
        sum(screen$screenable & !screen$screen_pass))
message("  Screen passed (mean P < ", RSCOTT_MAP_SCREEN_MM, " mm): ",
        sum(screen$screen_pass))

## ============================================================================
## Step 5: passing-year counts at both time steps
## ============================================================================
message("\n================ Step 5: passing-year counts ================")

count_passing <- function(d) {
  d |>
    dplyr::group_by(site_id) |>
    dplyr::summarise(
      n_years_total = dplyr::n(),
      n_passing_with_precip = sum(pass_gpp & pass_et & pass_precip_measured, na.rm = TRUE),
      n_passing_without_precip = sum(pass_gpp & pass_et, na.rm = TRUE),
      .groups = "drop"
    )
}

cal_counts <- count_passing(cal) |>
  dplyr::rename(
    n_calendar_years_total = n_years_total,
    n_passing_calendar_with_precip = n_passing_with_precip,
    n_passing_calendar_without_precip = n_passing_without_precip
  )

wat <- site_years |> dplyr::filter(time_step == "water")
wat_counts <- count_passing(wat) |>
  dplyr::rename(
    n_water_years_total = n_years_total,
    n_passing_water_with_precip = n_passing_with_precip,
    n_passing_water_without_precip = n_passing_without_precip
  )

site_decision <- screen |>
  dplyr::select(-n_calendar_years_total) |>
  dplyr::left_join(cal_counts, by = "site_id") |>
  dplyr::left_join(wat_counts, by = "site_id") |>
  dplyr::mutate(
    keep = screen_pass & (
      n_passing_calendar_with_precip >= RSCOTT_MIN_PASSING_YEARS_TO_KEEP |
      n_passing_water_with_precip >= RSCOTT_MIN_PASSING_YEARS_TO_KEEP
    ),
    exclusion_reason = dplyr::case_when(
      keep ~ NA_character_,
      !screenable ~ sprintf(
        "fewer than %d years of measured precipitation (calendar years with ERA-filled share <= %.0f%%: n=%d)",
        RSCOTT_MIN_MEASURED_YEARS_FOR_SCREEN, 100 * RSCOTT_ERA_FILLED_MAX_SHARE, n_measured_years_calendar
      ),
      screenable & !screen_pass ~ sprintf(
        "mean annual measured precipitation %.1f mm, at or above the %d mm threshold",
        mean_annual_measured_p_mm, RSCOTT_MAP_SCREEN_MM
      ),
      TRUE ~ sprintf(
        "fewer than %d passing years in either time step (calendar: %d, water: %d; without the precipitation condition -- calendar: %d, water: %d)",
        RSCOTT_MIN_PASSING_YEARS_TO_KEEP, n_passing_calendar_with_precip, n_passing_water_with_precip,
        n_passing_calendar_without_precip, n_passing_water_without_precip
      )
    )
  )

message("Sites kept: ", sum(site_decision$keep))
message("Sites excluded: ", sum(!site_decision$keep))
print(site_decision |> dplyr::filter(!keep) |> dplyr::count(dplyr::if_else(!screenable, "unscreenable",
        dplyr::if_else(screenable & !screen_pass, "map_too_high", "too_few_passing_years"))))

## ============================================================================
## Driest-month climatology and rainy-season-split flag (for sites.csv),
## kept sites only
## ============================================================================
message("\n================ Driest-month climatology (kept sites) ================")

kept_sites <- site_decision |> dplyr::filter(keep) |> dplyr::pull(site_id)

if (length(kept_sites) > 0) {
  con <- rscott_duckdb_connect()
  sites_sql <- paste(sprintf("'%s'", kept_sites), collapse = ", ")
  dd_clim <- dbGetQuery(con, sprintf("
    SELECT site_id, TIMESTAMP, P_F, P_F_QC FROM daily
    WHERE dataset = 'FLUXMET' AND site_id IN (%s)
  ", sites_sql))
  dbDisconnect(con, shutdown = TRUE)

  ## Mean-monthly MEASURED-precipitation climatology: for each site and
  ## calendar month-of-year, sum each day's measured amount (P_F*P_F_QC,
  ## i.e. the ERA-filled-share-complement amount-weighting used throughout
  ## this project) across all available years, divided by the number of
  ## distinct years contributing data to that month-of-year -- an
  ## amount-weighted mean, not a hard measured/unmeasured cutoff.
  monthly_clim <- dd_clim |>
    dplyr::mutate(
      date = as.Date(TIMESTAMP),
      cal_year = lubridate::year(date),
      cal_month = lubridate::month(date),
      p_measured_day = P_F * P_F_QC
    ) |>
    dplyr::filter(!is.na(p_measured_day)) |>
    dplyr::group_by(site_id, cal_year, cal_month) |>
    dplyr::summarise(p_measured_month_total = sum(p_measured_day), .groups = "drop") |>
    dplyr::group_by(site_id, cal_month) |>
    dplyr::summarise(
      mean_measured_p_month_mm = mean(p_measured_month_total),
      n_years_contributing = dplyr::n(),
      .groups = "drop"
    )

  driest <- monthly_clim |>
    dplyr::group_by(site_id) |>
    dplyr::slice_min(mean_measured_p_month_mm, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::transmute(site_id, driest_month = cal_month, driest_month_mean_measured_p_mm = mean_measured_p_month_mm)

  ## Rainy-season-split check: flag a site only when wetness genuinely
  ## STRADDLES the water-year cut -- i.e. the month immediately before the
  ## boundary (the last month of the outgoing water year) AND the boundary
  ## month itself (the first month of the incoming water year) are BOTH
  ## among the site's top-4 (one third of the year) wettest months. This
  ## deliberately does NOT flag a site whose wet season simply ENDS right
  ## before the boundary (e.g. a NH summer-monsoon site wet Jul-Sep, dry
  ## Oct-May: Sep is wet but Oct is not, so the Oct 1 cut falls cleanly at
  ## the wet-to-dry transition and does not split anything) -- only a site
  ## where the boundary itself sits inside a still-wet stretch counts as a
  ## split (the textbook case: a SH winter-rain site wet ~May-Aug, where the
  ## Jul 1 cut falls in the middle of that wet spell).
  wet_rank <- monthly_clim |>
    dplyr::group_by(site_id) |>
    dplyr::mutate(wet_rank = rank(-mean_measured_p_month_mm, ties.method = "first")) |>
    dplyr::ungroup()

  top4_wet <- wet_rank |> dplyr::filter(wet_rank <= 4) |> dplyr::select(site_id, cal_month)

  boundary_month <- function(hemisphere) ifelse(hemisphere == "N", 10L, 7L)

  rainy_flag <- candidates |>
    dplyr::filter(site_id %in% kept_sites) |>
    dplyr::select(site_id, hemisphere) |>
    dplyr::mutate(
      boundary = boundary_month(hemisphere),
      month_before_boundary = ((boundary - 2) %% 12) + 1
    )

  is_wet <- function(site, month) {
    wet_months <- top4_wet$cal_month[top4_wet$site_id == site]
    month %in% wet_months
  }
  rainy_flag$rainy_season_split_risk <- vapply(seq_len(nrow(rainy_flag)), function(i)
    is_wet(rainy_flag$site_id[i], rainy_flag$month_before_boundary[i]) &&
    is_wet(rainy_flag$site_id[i], rainy_flag$boundary[i]),
    logical(1))

  rainy_flag <- rainy_flag |> dplyr::select(site_id, boundary_month = boundary, rainy_season_split_risk)

  month_name <- function(m) month.abb[m]
} else {
  driest <- tibble::tibble(site_id = character(0), driest_month = integer(0), driest_month_mean_measured_p_mm = double(0))
  rainy_flag <- tibble::tibble(site_id = character(0), boundary_month = integer(0), rainy_season_split_risk = logical(0))
  monthly_clim <- tibble::tibble()
}

message("Kept sites with rainy-season-split risk flagged: ", sum(rainy_flag$rainy_season_split_risk))
if (sum(rainy_flag$rainy_season_split_risk) > 0) print(rainy_flag |> dplyr::filter(rainy_season_split_risk))

## ============================================================================
## Save intermediate screening/climatology outputs
## ============================================================================
out_decision <- file.path(RSCOTT_ROOT, "tables", "site_screen_decision.csv")
readr::write_csv(site_decision, out_decision)
write_meta(out_decision, input_sources = file.path(RSCOTT_ROOT, "tables", "site_years_candidates_full.csv"),
  notes = "Step 3 screen (calendar-year measured precip, mean P < 800 mm) and step 5 keep decision (>=5 passing years, either time step, counted with and without the precipitation condition).")

out_clim <- file.path(RSCOTT_ROOT, "tables", "driest_month_climatology.csv")
readr::write_csv(driest |> dplyr::left_join(rainy_flag, by = "site_id"), out_clim)
write_meta(out_clim, input_sources = RSCOTT_DUCKDB_PATH,
  notes = "Driest calendar month (by mean measured-precipitation amount, amount-weighted P_F*P_F_QC across all available years) and the rainy-season-split risk flag (TRUE if the water-year boundary month or an adjacent month is among the site's top-3 wettest months), kept sites only.")

message("\n03_screen_and_climatology.R complete.")
