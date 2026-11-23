## 06_v2_relaxed_selection.R -- v2 widened selection for Russ Scott.
##
## Builds on 01-03's outputs (candidate_sites.csv, site_years_candidates_full.csv,
## v1's site_screen_decision.csv) rather than re-querying the DuckDB from scratch,
## per the task's "same data, conventions, units, QC rules and water-year rules
## as the first run; no new downloads."
##
## Relaxed rules (v2 spec):
##   - precip_source per site (calendar years only): "measured" if >=1 calendar
##     year has era_filled_share <= 10% (pass_precip_measured); else "ERA5-only".
##   - 800mm screen: mean annual P_F over MEASURED calendar years for "measured"
##     sites; over ALL calendar years with any P_F value for "ERA5-only" sites.
##   - Keep: mean P < 800mm AND >=5 years (calendar OR water year) where GPP_NT
##     and ET (LE_F_MDS) both pass QC AND P_F is present (whatever its source).
##     The measured-precip condition is DROPPED from the per-year pass test
##     (that's the relaxation) -- we separately report, for transparency, how
##     many of those passing years also happen to have measured precipitation.
##   - No sign filter on GPP anywhere (confirmed: v1/v2 code never filters on
##     GPP value, only on its QC flag) -- negative annual GPP is retained with
##     its ordinary pass flag, and counted explicitly (see neg_gpp below).

source("RSCOTT_AGU26/code/00_config.R")

candidates <- readr::read_csv(file.path(RSCOTT_ROOT, "tables", "candidate_sites.csv"), show_col_types = FALSE)
site_years <- readr::read_csv(file.path(RSCOTT_ROOT, "tables", "site_years_candidates_full.csv"), show_col_types = FALSE)
v1_decision <- readr::read_csv(file.path(RSCOTT_ROOT, "tables", "site_screen_decision.csv"), show_col_types = FALSE)

cal <- site_years |> dplyr::filter(time_step == "calendar") |>
  dplyr::mutate(pass_precip_present = !is.na(p_f_mm_yr))
wat <- site_years |> dplyr::filter(time_step == "water") |>
  dplyr::mutate(pass_precip_present = !is.na(p_f_mm_yr))

## ============================================================================
## Step 1: precip_source per site (calendar years only, per the task)
## ============================================================================
message("\n================ v2 Step 1: precip_source per site ================")

precip_source_site <- cal |>
  dplyr::group_by(site_id) |>
  dplyr::summarise(
    n_measured_years_calendar = sum(pass_precip_measured, na.rm = TRUE),
    n_calendar_years_with_p   = sum(pass_precip_present, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(precip_source = ifelse(n_measured_years_calendar >= 1, "measured", "ERA5-only"))

message("precip_source counts: ", paste(capture.output(print(table(precip_source_site$precip_source))), collapse = " | "))

## Mean annual P for the 800mm screen: "measured" sites use measured calendar
## years only; "ERA5-only" sites use ALL calendar years with any P_F value.
mean_p <- cal |>
  dplyr::left_join(dplyr::select(precip_source_site, site_id, precip_source), by = "site_id") |>
  dplyr::group_by(site_id) |>
  dplyr::summarise(
    mean_annual_p_mm = {
      src <- precip_source[1]
      if (identical(src, "measured")) mean(p_f_mm_yr[pass_precip_measured], na.rm = TRUE)
      else mean(p_f_mm_yr[pass_precip_present], na.rm = TRUE)
    },
    .groups = "drop"
  ) |>
  dplyr::mutate(mean_annual_p_mm = ifelse(is.nan(mean_annual_p_mm), NA_real_, mean_annual_p_mm))

## ============================================================================
## Step 3 (relaxed): 800mm screen
## ============================================================================
message("\n================ v2 site screen ================")

screen <- precip_source_site |>
  dplyr::left_join(mean_p, by = "site_id") |>
  dplyr::mutate(
    screenable  = !is.na(mean_annual_p_mm),
    screen_pass = screenable & mean_annual_p_mm < RSCOTT_MAP_SCREEN_MM
  )
message("Unscreenable (no P_F value in any calendar year): ", sum(!screen$screenable))
message("Screenable but mean P >= ", RSCOTT_MAP_SCREEN_MM, " mm: ", sum(screen$screenable & !screen$screen_pass))
message("Screen passed: ", sum(screen$screen_pass))

## ============================================================================
## Step 5 (relaxed): passing-year counts, P_F-present condition instead of
## measured-precip condition; measured-precip subset reported separately.
## ============================================================================
message("\n================ v2 passing-year counts ================")

count_passing_v2 <- function(d) {
  d |>
    dplyr::group_by(site_id) |>
    dplyr::summarise(
      n_years_total = dplyr::n(),
      n_passing_relaxed              = sum(pass_gpp & pass_et & pass_precip_present, na.rm = TRUE),
      n_passing_relaxed_and_measured = sum(pass_gpp & pass_et & pass_precip_present & pass_precip_measured, na.rm = TRUE),
      .groups = "drop"
    )
}

cal_counts <- count_passing_v2(cal) |>
  dplyr::rename(n_calendar_years_total = n_years_total,
                n_passing_calendar_relaxed = n_passing_relaxed,
                n_passing_calendar_relaxed_and_measured = n_passing_relaxed_and_measured)
wat_counts <- count_passing_v2(wat) |>
  dplyr::rename(n_water_years_total = n_years_total,
                n_passing_water_relaxed = n_passing_relaxed,
                n_passing_water_relaxed_and_measured = n_passing_relaxed_and_measured)

decision <- screen |>
  dplyr::left_join(cal_counts, by = "site_id") |>
  dplyr::left_join(wat_counts, by = "site_id") |>
  dplyr::mutate(
    keep = screen_pass & (
      n_passing_calendar_relaxed >= RSCOTT_MIN_PASSING_YEARS_TO_KEEP |
      n_passing_water_relaxed >= RSCOTT_MIN_PASSING_YEARS_TO_KEEP
    ),
    exclusion_reason = dplyr::case_when(
      keep ~ NA_character_,
      !screenable ~ "no P_F value available in any calendar year -- cannot compute a mean annual precipitation to screen",
      screenable & !screen_pass ~ sprintf(
        "mean annual precipitation %.1f mm (precip_source=%s), at or above the %d mm threshold",
        mean_annual_p_mm, precip_source, RSCOTT_MAP_SCREEN_MM
      ),
      TRUE ~ sprintf(
        "fewer than %d relaxed-passing years in either time step (calendar: %d, water: %d; of which with measured precipitation -- calendar: %d, water: %d)",
        RSCOTT_MIN_PASSING_YEARS_TO_KEEP, n_passing_calendar_relaxed, n_passing_water_relaxed,
        n_passing_calendar_relaxed_and_measured, n_passing_water_relaxed_and_measured
      )
    )
  )

message("v2 sites kept: ", sum(decision$keep))
message("v2 sites excluded: ", sum(!decision$keep))

## US-Aud check (task explicitly says "keep US-Aud") -------------------------
aud <- decision |> dplyr::filter(site_id == "US-Aud")
v1_aud_kept <- any(v1_decision$site_id == "US-Aud" & v1_decision$keep)
message("\nUS-Aud: v1 kept = ", v1_aud_kept, "; v2 keep = ", aud$keep,
        "; v2 precip_source = ", aud$precip_source,
        "; v2 mean_annual_p_mm = ", round(aud$mean_annual_p_mm, 1))
if (!aud$keep) stop("US-Aud does not pass the v2 relaxed rule -- STOP, per directive, rather than forcing it in or silently excluding it.")

## ============================================================================
## Negative-GPP site-year counts (all candidate sites; merged into kept-site
## sites.csv downstream). Counts rows (both time steps) where GPP_NT or
## GPP_DT is negative -- an "OR" count, not two separate NT/DT counts.
## ============================================================================
neg_gpp <- site_years |>
  dplyr::group_by(site_id) |>
  dplyr::summarise(
    n_siteyears_negative_gpp = sum((gpp_nt_gC_m2_yr < 0) | (gpp_dt_gC_m2_yr < 0), na.rm = TRUE),
    .groups = "drop"
  )
message("\nCandidate sites with >=1 negative-GPP site-year: ", sum(neg_gpp$n_siteyears_negative_gpp > 0))

decision <- decision |> dplyr::left_join(neg_gpp, by = "site_id")

## ============================================================================
## Which rule newly admitted each v2-only kept site, vs v1
## ============================================================================
v1_small <- v1_decision |> dplyr::select(site_id, v1_keep = keep, v1_screenable = screenable)
newly_kept <- decision |>
  dplyr::filter(keep) |>
  dplyr::left_join(v1_small, by = "site_id") |>
  dplyr::mutate(v1_keep = ifelse(is.na(v1_keep), FALSE, v1_keep),
                v1_screenable = ifelse(is.na(v1_screenable), FALSE, v1_screenable)) |>
  dplyr::filter(!v1_keep) |>
  dplyr::mutate(added_via = ifelse(
    !v1_screenable,
    "precip_source relaxation (unscreenable under v1's >=2-measured-calendar-year rule)",
    "relaxed keep-count (measured-precip condition dropped from the per-year pass test)"
  ))
message("\nSites added by relaxation (v2 keep, v1 did not keep): ", nrow(newly_kept))
print(as.data.frame(table(newly_kept$added_via)))

out_newly <- file.path(RSCOTT_ROOT, "tables", "newly_kept_v2.csv")
readr::write_csv(newly_kept, out_newly)
write_meta(out_newly, input_sources = c(
    file.path(RSCOTT_ROOT, "tables", "site_screen_decision.csv")
  ),
  notes = "Sites kept under the v2 relaxed rule that were NOT kept in v1, classified by which relaxation let them in: precip_source relaxation (v1 required >=2 measured calendar years to even screen; v2 allows ERA5-only or single-measured-year sites), or the relaxed keep-count (v1 required measured precipitation in each passing year; v2 only requires P_F to be present).")

out_decision <- file.path(RSCOTT_ROOT, "tables", "site_screen_decision_v2.csv")
readr::write_csv(decision, out_decision)
write_meta(out_decision, input_sources = file.path(RSCOTT_ROOT, "tables", "site_years_candidates_full.csv"),
  notes = "v2 relaxed screen (mean annual P < 800mm, using measured-year mean for 'measured' sites and all-year mean for 'ERA5-only' sites) and keep decision (>=5 years, either time step, with GPP+ET QC passing and P_F present -- measured-precip no longer required per-year). n_siteyears_negative_gpp counts site-years (both time steps) with GPP_NT or GPP_DT < 0, all candidate sites.")

message("\n06_v2_relaxed_selection.R complete.")
