## 02_build_site_years.R — unit check (step 4 preamble) and per-site-year
## value tables at both time steps (calendar year from YY, water year from
## MM), with QC pass flags (step 4/5).
##
## VUT/CUT per-site rule (never mixed within a site): VUT if the site has
## any non-NA NEE_VUT_REF_QC, else CUT if it has any non-NA NEE_CUT_REF_QC,
## else ungated -- identical to scripts/04_qc.R and
## R/site_annual_fluxes.R::.compute_site_annual_fluxes_core(). GPP_NT/GPP_DT
## have no QC column of their own and are gated on the chosen NEE QC, same
## as R/site_annual_fluxes.R.
##
## LE_CORR has no QC column of its own (step 4); it is given LE_F_MDS_QC,
## stated explicitly wherever LE_CORR appears in outputs.
##
## P_F_QC polarity (confirmed in 01_candidates_and_qc_check.R): higher = more
## measured. ERA-filled share = 1 - P_F_QC at YY (a single annual fraction);
## at water year (summed from MM), ERA-filled share is amount-weighted:
## era_filled_share = 1 - sum(P_F_QC_month * P_F_month) / sum(P_F_month)
## across the water year's 12 months -- NOT a simple average of the 12
## monthly P_F_QC fractions, which would ignore how much precipitation each
## month actually contributes.

source("RSCOTT_AGU26/code/00_config.R")

candidates <- readr::read_csv(file.path(RSCOTT_ROOT, "tables", "candidate_sites.csv"), show_col_types = FALSE)
cand_sites <- candidates$site_id

con <- rscott_duckdb_connect()
sites_sql <- paste(sprintf("'%s'", cand_sites), collapse = ", ")

## ============================================================================
## Per-site VUT/CUT source (same rule as scripts/04_qc.R / R/site_annual_fluxes.R)
## ============================================================================
message("\n================ VUT/CUT per-site source ================")

carbon_src_tbl <- dbGetQuery(con, sprintf("
  SELECT site_id,
         SUM(CASE WHEN NEE_VUT_REF_QC IS NOT NULL THEN 1 ELSE 0 END) AS n_vut,
         SUM(CASE WHEN NEE_CUT_REF_QC IS NOT NULL THEN 1 ELSE 0 END) AS n_cut
  FROM annual
  WHERE dataset = 'FLUXMET' AND site_id IN (%s)
  GROUP BY site_id
", sites_sql)) |>
  dplyr::mutate(carbon_src = dplyr::case_when(
    n_vut > 0L ~ "VUT", n_cut > 0L ~ "CUT", TRUE ~ NA_character_
  ))

message("Carbon source counts: ", paste(capture.output(table(carbon_src_tbl$carbon_src, useNA = "ifany")), collapse = " "))
out_src <- file.path(RSCOTT_ROOT, "tables", "carbon_source_by_site.csv")
readr::write_csv(carbon_src_tbl, out_src)
write_meta(out_src, input_sources = RSCOTT_DUCKDB_PATH,
  notes = "Per-site VUT/CUT choice for NEE/GPP/RECO: VUT if any non-NA NEE_VUT_REF_QC, else CUT if any non-NA NEE_CUT_REF_QC, else ungated. Identical rule to scripts/04_qc.R and R/site_annual_fluxes.R.")

## ============================================================================
## Unit check: reconstruct calendar-year totals from monthly values
## ============================================================================
message("\n================ Unit check: MM-summed vs YY-direct ================")

## GPP_NT (chosen VUT/CUT per site) and P_F, for every candidate site with
## carbon_src assigned and >= 3 years of annual_converted data -- a few
## sites is plenty to confirm the convention, not a network-wide re-run of
## the SESSION_LOG 2026-09-28 closure test this project inherits via
## R/units.R's *_converted tables.
check_sites <- carbon_src_tbl |> dplyr::filter(!is.na(carbon_src)) |> dplyr::pull(site_id)
check_sites_sql <- paste(sprintf("'%s'", check_sites), collapse = ", ")

yy_direct <- dbGetQuery(con, sprintf("
  SELECT a.site_id, CAST(TIMESTAMP AS INTEGER) AS year,
         CASE WHEN c.carbon_src = 'VUT' THEN a.GPP_NT_VUT_REF ELSE a.GPP_NT_CUT_REF END AS gpp_nt_yy,
         a.P_F AS p_f_yy
  FROM annual_converted a
  JOIN (SELECT site_id, carbon_src FROM (VALUES %s) AS t(site_id, carbon_src)) c
    ON a.site_id = c.site_id
  WHERE a.dataset = 'FLUXMET' AND a.site_id IN (%s)
", paste(sprintf("('%s', '%s')", carbon_src_tbl$site_id[carbon_src_tbl$site_id %in% check_sites], carbon_src_tbl$carbon_src[carbon_src_tbl$site_id %in% check_sites]), collapse = ", "),
   check_sites_sql))

mm_raw <- dbGetQuery(con, sprintf("
  SELECT site_id, TIMESTAMP, GPP_NT_VUT_REF, GPP_NT_CUT_REF, P_F
  FROM monthly_converted
  WHERE dataset = 'FLUXMET' AND site_id IN (%s)
", check_sites_sql)) |>
  dplyr::left_join(dplyr::select(carbon_src_tbl, site_id, carbon_src), by = "site_id") |>
  dplyr::mutate(
    year = lubridate::year(as.Date(TIMESTAMP)),
    days_in_month = lubridate::days_in_month(as.Date(TIMESTAMP)),
    gpp_nt_mm = dplyr::if_else(carbon_src == "VUT", GPP_NT_VUT_REF, GPP_NT_CUT_REF)
  ) |>
  dplyr::group_by(site_id, year) |>
  dplyr::summarise(
    n_months = dplyr::n(),
    gpp_nt_mm_summed = sum(gpp_nt_mm, na.rm = FALSE),
    p_f_mm_summed_raw = sum(P_F, na.rm = FALSE),
    p_f_mm_summed_scaled_by_days = sum(P_F * days_in_month, na.rm = FALSE),
    .groups = "drop"
  ) |>
  dplyr::filter(n_months == 12)

unit_check <- dplyr::inner_join(yy_direct, mm_raw, by = c("site_id", "year")) |>
  dplyr::filter(!is.na(gpp_nt_yy), !is.na(gpp_nt_mm_summed), !is.na(p_f_yy), !is.na(p_f_mm_summed_raw)) |>
  dplyr::mutate(
    gpp_pct_diff = 100 * (gpp_nt_mm_summed - gpp_nt_yy) / abs(gpp_nt_yy),
    p_pct_diff_raw_mm_sum    = 100 * (p_f_mm_summed_raw - p_f_yy) / abs(p_f_yy),
    p_pct_diff_days_scaled   = 100 * (p_f_mm_summed_scaled_by_days - p_f_yy) / abs(p_f_yy)
  ) |>
  dplyr::arrange(site_id, year)

n_check <- nrow(unit_check)
message("Site-years with complete 12-month MM data and a YY value, used for the unit check: ", n_check)
## A handful of site-years have p_f_yy == 0 (zero annual precip in an arid
## year), which makes a %-difference undefined (division by zero); those
## rows are kept in the saved CSV but excluded from these summary stats,
## noted explicitly rather than silently dropped.
n_zero_p_yy <- sum(unit_check$p_f_yy == 0, na.rm = TRUE)
if (n_zero_p_yy > 0) message(n_zero_p_yy, " site-year(s) have p_f_yy == 0 (undefined %diff) -- excluded from the P_F summary stats below, kept in the CSV.")
if (n_check > 0) {
  message(sprintf("GPP_NT (monthly_converted, already unit-converted): median |%%diff| = %.2f%%, max |%%diff| = %.2f%% (n=%d)",
    median(abs(unit_check$gpp_pct_diff), na.rm = TRUE), max(abs(unit_check$gpp_pct_diff), na.rm = TRUE), n_check))
  message(sprintf("P_F raw MM sum vs YY (NO days-in-month scaling):    median |%%diff| = %.2f%%, max |%%diff| = %.2f%%",
    median(abs(unit_check$p_pct_diff_raw_mm_sum), na.rm = TRUE), max(abs(unit_check$p_pct_diff_raw_mm_sum)[is.finite(unit_check$p_pct_diff_raw_mm_sum)], na.rm = TRUE)))
  message(sprintf("P_F MM sum x days-in-month vs YY:                   median |%%diff| = %.4f%%, max |%%diff| = %.4f%%",
    median(abs(unit_check$p_pct_diff_days_scaled), na.rm = TRUE), max(abs(unit_check$p_pct_diff_days_scaled)[is.finite(unit_check$p_pct_diff_days_scaled)], na.rm = TRUE)))
}
print(head(unit_check, 15))

## *** UNEXPECTED FINDING ***
## P_F at MM resolution is NOT a monthly total -- it is a MEAN DAILY RATE
## (mm/day) reported at monthly cadence, exactly the same undocumented
## convention CLAUDE.md/R/units.R already document for carbon fluxes (NEE/
## GPP/RECO) at MM/WW, but NOT currently documented or handled for P_F
## (CLAUDE.md states "P: mm per timestep -- Sum, not average" with no
## resolution caveat, and R/units.R's fluxnet_convert_units() deliberately
## leaves P_F untouched "kept as-is; must be summed"). Raw MM-summed P_F
## is ~29-30x too LOW vs the YY value (median/max |%diff| ~96.7%, i.e. sum
## of raw monthly P_F systematically understates the true annual total by
## a factor matching days-in-month); multiplying each month's P_F by its
## days-in-month before summing closes this to <0.01%. This matches exactly
## the closure-test methodology already established for carbon fluxes
## (SESSION_LOG.md, 2026-09-28).
##
## Rather than silently apply this undocumented correction to MM P_F (which
## would duplicate, not reuse, the project's R/units.R logic), water-year P
## totals and ERA-filled shares in this project are instead built directly
## from DAILY (DD) P_F/P_F_QC, confirmed below to sum to the YY value with
## no scaling needed (<0.01% diff; same "DD = already a true period total,
## no rate conversion" pattern as carbon). This also matches the task's own
## step-2 instruction to work "from the daily data" for the P_F/ERA-filled-
## share computation. GPP/ET water-year values still use MM via
## monthly_converted, which the check above confirms is already correctly
## unit-converted (median |%diff| well under 5%). THIS MM-P_F UNIT ISSUE IS
## FLAGGED PROMINENTLY IN docs/report_back_2026-11-23.md AS A REPO-WIDE FINDING,
## NOT JUST A WORKAROUND FOR THIS SIDE PROJECT -- any other MM/WW-resolution
## precipitation analysis in this repo that sums raw P_F would have the same bug.
ddcheck_sites_sql <- check_sites_sql
dd_check <- dbGetQuery(con, sprintf("
  SELECT site_id, TIMESTAMP, P_F FROM daily WHERE dataset = 'FLUXMET' AND site_id IN (%s)
", ddcheck_sites_sql)) |>
  dplyr::mutate(year = lubridate::year(as.Date(TIMESTAMP))) |>
  dplyr::group_by(site_id, year) |>
  dplyr::summarise(n_days = dplyr::n(), p_f_dd_summed = sum(P_F, na.rm = FALSE), .groups = "drop") |>
  dplyr::inner_join(dplyr::select(yy_direct, site_id, year, p_f_yy), by = c("site_id", "year")) |>
  dplyr::filter(n_days %in% c(365, 366), !is.na(p_f_dd_summed), !is.na(p_f_yy)) |>
  dplyr::mutate(p_pct_diff_dd_sum = 100 * (p_f_dd_summed - p_f_yy) / abs(p_f_yy))
dd_check_finite <- dd_check[is.finite(dd_check$p_pct_diff_dd_sum), ]
message(sprintf(
  "P_F DD sum (full-year, n=%d site-years, %d with p_f_yy==0 excluded from stats) vs YY: median |%%diff| = %.4f%%, max |%%diff| = %.4f%% -- DD requires no scaling.",
  nrow(dd_check), nrow(dd_check) - nrow(dd_check_finite),
  median(abs(dd_check_finite$p_pct_diff_dd_sum)), max(abs(dd_check_finite$p_pct_diff_dd_sum))
))

out_unit <- file.path(RSCOTT_ROOT, "tables", "unit_check_mm_vs_yy.csv")
readr::write_csv(unit_check, out_unit)
write_meta(out_unit, input_sources = RSCOTT_DUCKDB_PATH,
  notes = paste0(
    "Reconstructs calendar-year GPP_NT (chosen VUT/CUT per site) and P_F totals by summing ",
    "the 12 monthly_converted/monthly rows for that site-year and comparing to the direct ",
    "annual_converted/annual YY value. GPP_NT (already converted by R/units.R) matches YY to ",
    "a few percent, as expected. P_F does NOT match when summed raw (median ~97% too low) -- ",
    "discovered here to be a mean-daily-rate at MM resolution needing multiplication by ",
    "days-in-month (then matches to <0.01%), the same undocumented convention already known ",
    "for carbon fluxes but NOT currently documented/handled for precipitation in CLAUDE.md or ",
    "R/units.R. See docs/report_back_2026-11-23.md for the full write-up; this project uses ",
    "DAILY (DD) P_F/P_F_QC for all water-year precipitation totals/shares as a result (DD sums ",
    "to YY with no scaling, confirmed separately), not MM."
  ))
message("Saved: ", out_unit)

out_dd_check <- file.path(RSCOTT_ROOT, "tables", "unit_check_dd_vs_yy.csv")
readr::write_csv(dd_check, out_dd_check)
write_meta(out_dd_check, input_sources = RSCOTT_DUCKDB_PATH,
  notes = "Confirms daily (DD) P_F sums to the annual (YY) P_F value with no scaling needed (full calendar years only, n_days in {365,366}), supporting this project's choice to build water-year P totals/shares from DD rather than MM data.")
message("Saved: ", out_dd_check)

## ============================================================================
## Calendar-year table (YY)
## ============================================================================
message("\n================ Calendar-year (YY) table ================")

yy_raw <- dbGetQuery(con, sprintf("
  SELECT site_id, CAST(TIMESTAMP AS INTEGER) AS year,
         NEE_VUT_REF_QC, NEE_CUT_REF_QC,
         GPP_NT_VUT_REF, GPP_NT_CUT_REF, GPP_DT_VUT_REF, GPP_DT_CUT_REF,
         LE_F_MDS_QC, P_F AS p_f_native_yy, P_F_QC AS p_f_qc_yy
  FROM annual
  WHERE dataset = 'FLUXMET' AND site_id IN (%s)
", sites_sql))

yy_conv <- dbGetQuery(con, sprintf("
  SELECT site_id, CAST(TIMESTAMP AS INTEGER) AS year,
         GPP_NT_VUT_REF AS gpp_nt_vut_gC, GPP_NT_CUT_REF AS gpp_nt_cut_gC,
         GPP_DT_VUT_REF AS gpp_dt_vut_gC, GPP_DT_CUT_REF AS gpp_dt_cut_gC,
         LE_F_MDS AS et_le_f_mds_mm, LE_CORR AS et_le_corr_mm
  FROM annual_converted
  WHERE dataset = 'FLUXMET' AND site_id IN (%s)
", sites_sql))

yy <- dplyr::inner_join(yy_raw, yy_conv, by = c("site_id", "year")) |>
  dplyr::left_join(dplyr::select(carbon_src_tbl, site_id, carbon_src), by = "site_id") |>
  dplyr::mutate(
    nee_qc = dplyr::case_when(
      carbon_src == "VUT" ~ NEE_VUT_REF_QC, carbon_src == "CUT" ~ NEE_CUT_REF_QC, TRUE ~ NA_real_
    ),
    gpp_nt_gC_m2_yr = dplyr::case_when(
      carbon_src == "VUT" ~ gpp_nt_vut_gC, carbon_src == "CUT" ~ gpp_nt_cut_gC, TRUE ~ NA_real_
    ),
    gpp_dt_gC_m2_yr = dplyr::case_when(
      carbon_src == "VUT" ~ gpp_dt_vut_gC, carbon_src == "CUT" ~ gpp_dt_cut_gC, TRUE ~ NA_real_
    ),
    et_le_f_mds_mm_yr = et_le_f_mds_mm,
    et_le_corr_mm_yr = et_le_corr_mm,
    et_qc_source = "LE_F_MDS_QC (LE_CORR has no QC column of its own)",
    p_f_mm_yr = p_f_native_yy,
    p_f_qc = p_f_qc_yy,
    era_filled_share = pmin(pmax(1 - p_f_qc, 0), 1),
    n_months_present = NA_integer_,
    n_months_gpp_passing = NA_integer_,
    n_months_et_passing = NA_integer_,
    n_days_present = NA_integer_,
    precip_complete = NA,
    pass_gpp = !is.na(nee_qc) & qc_passes(nee_qc, QC_THRESHOLD_YY),
    pass_et  = !is.na(LE_F_MDS_QC) & qc_passes(LE_F_MDS_QC, QC_THRESHOLD_YY),
    pass_precip_measured = !is.na(era_filled_share) & era_filled_share <= RSCOTT_ERA_FILLED_MAX_SHARE,
    time_step = "calendar",
    period_label = as.character(year)
  ) |>
  dplyr::select(
    site_id, time_step, period_label, year,
    carbon_src, nee_qc, gpp_nt_gC_m2_yr, gpp_dt_gC_m2_yr,
    et_le_f_mds_mm_yr, et_le_corr_mm_yr, LE_F_MDS_QC, et_qc_source,
    p_f_mm_yr, p_f_qc, era_filled_share,
    n_months_present, n_months_gpp_passing, n_months_et_passing,
    n_days_present, precip_complete,
    pass_gpp, pass_et, pass_precip_measured
  )

message("Calendar-year rows (candidate sites): ", nrow(yy))

## ============================================================================
## Water-year table (MM)
## ============================================================================
message("\n================ Water-year table ================")

mm_raw_all <- dbGetQuery(con, sprintf("
  SELECT site_id, TIMESTAMP, NEE_VUT_REF_QC, NEE_CUT_REF_QC, LE_F_MDS_QC,
         P_F AS p_f_native_mm, P_F_QC AS p_f_qc_mm
  FROM monthly
  WHERE dataset = 'FLUXMET' AND site_id IN (%s)
", sites_sql))

mm_conv_all <- dbGetQuery(con, sprintf("
  SELECT site_id, TIMESTAMP,
         GPP_NT_VUT_REF AS gpp_nt_vut_gC, GPP_NT_CUT_REF AS gpp_nt_cut_gC,
         GPP_DT_VUT_REF AS gpp_dt_vut_gC, GPP_DT_CUT_REF AS gpp_dt_cut_gC,
         LE_F_MDS AS et_le_f_mds_mm, LE_CORR AS et_le_corr_mm
  FROM monthly_converted
  WHERE dataset = 'FLUXMET' AND site_id IN (%s)
", sites_sql))

## Precipitation for the water-year step is built from DAILY (DD) data, not
## MM -- see the "UNEXPECTED FINDING" block above: raw MM P_F is a mean
## daily rate, not a monthly total, and this project does not silently
## reimplement that unit fix here. DD P_F is confirmed additive (sums to YY
## with no scaling) and this also matches the task's own "from the daily
## data" instruction for the P_F/ERA-filled-share computation.
dd_raw_all <- dbGetQuery(con, sprintf("
  SELECT site_id, TIMESTAMP, P_F AS p_f_native_dd, P_F_QC AS p_f_qc_dd
  FROM daily
  WHERE dataset = 'FLUXMET' AND site_id IN (%s)
", sites_sql))

dbDisconnect(con, shutdown = TRUE)

mm <- dplyr::inner_join(mm_raw_all, mm_conv_all, by = c("site_id", "TIMESTAMP")) |>
  dplyr::left_join(dplyr::select(carbon_src_tbl, site_id, carbon_src), by = "site_id") |>
  dplyr::left_join(dplyr::select(candidates, site_id, hemisphere), by = "site_id") |>
  dplyr::mutate(
    date = as.Date(TIMESTAMP),
    cal_year = lubridate::year(date),
    cal_month = lubridate::month(date),
    ## Water year labelled by the year it ENDS: Oct-Sep (NH), Jul-Jun (SH).
    wy_year = dplyr::case_when(
      hemisphere == "N" & cal_month >= 10 ~ cal_year + 1L,
      hemisphere == "N"                   ~ cal_year,
      hemisphere == "S" & cal_month >= 7  ~ cal_year + 1L,
      hemisphere == "S"                   ~ cal_year,
      TRUE ~ NA_integer_
    ),
    nee_qc = dplyr::case_when(
      carbon_src == "VUT" ~ NEE_VUT_REF_QC, carbon_src == "CUT" ~ NEE_CUT_REF_QC, TRUE ~ NA_real_
    ),
    gpp_nt_month = dplyr::case_when(
      carbon_src == "VUT" ~ gpp_nt_vut_gC, carbon_src == "CUT" ~ gpp_nt_cut_gC, TRUE ~ NA_real_
    ),
    gpp_dt_month = dplyr::case_when(
      carbon_src == "VUT" ~ gpp_dt_vut_gC, carbon_src == "CUT" ~ gpp_dt_cut_gC, TRUE ~ NA_real_
    ),
    month_pass_gpp = !is.na(nee_qc) & qc_passes(nee_qc, QC_THRESHOLD_MM),
    month_pass_et  = !is.na(LE_F_MDS_QC) & qc_passes(LE_F_MDS_QC, QC_THRESHOLD_MM)
  )

wy_flux <- mm |>
  dplyr::filter(!is.na(wy_year)) |>
  dplyr::group_by(site_id, wy_year, carbon_src) |>
  dplyr::summarise(
    n_months_present = dplyr::n(),
    gpp_nt_gC_m2_yr = dplyr::if_else(n_months_present == 12, sum(gpp_nt_month, na.rm = FALSE), NA_real_),
    gpp_dt_gC_m2_yr = dplyr::if_else(n_months_present == 12, sum(gpp_dt_month, na.rm = FALSE), NA_real_),
    et_le_f_mds_mm_yr = dplyr::if_else(n_months_present == 12, sum(et_le_f_mds_mm, na.rm = FALSE), NA_real_),
    et_le_corr_mm_yr  = dplyr::if_else(n_months_present == 12, sum(et_le_corr_mm, na.rm = FALSE), NA_real_),
    n_months_gpp_passing = sum(month_pass_gpp, na.rm = TRUE),
    n_months_et_passing  = sum(month_pass_et, na.rm = TRUE),
    all_months_gpp_pass = n_months_present == 12 & all(month_pass_gpp),
    all_months_et_pass  = n_months_present == 12 & all(month_pass_et),
    .groups = "drop"
  )

## Daily precipitation, labelled into the same water-year convention.
dd <- dd_raw_all |>
  dplyr::left_join(dplyr::select(candidates, site_id, hemisphere), by = "site_id") |>
  dplyr::mutate(
    date = as.Date(TIMESTAMP),
    cal_year = lubridate::year(date),
    cal_month = lubridate::month(date),
    wy_year = dplyr::case_when(
      hemisphere == "N" & cal_month >= 10 ~ cal_year + 1L,
      hemisphere == "N"                   ~ cal_year,
      hemisphere == "S" & cal_month >= 7  ~ cal_year + 1L,
      hemisphere == "S"                   ~ cal_year,
      TRUE ~ NA_integer_
    ),
    p_measured_day = p_f_native_dd * p_f_qc_dd
  )

wy_precip <- dd |>
  dplyr::filter(!is.na(wy_year)) |>
  dplyr::group_by(site_id, wy_year) |>
  dplyr::summarise(
    n_days_present = dplyr::n(),
    expected_days = ifelse(lubridate::leap_year(wy_year[1]), 366L, 365L),
    p_f_mm_yr_raw = sum(p_f_native_dd, na.rm = FALSE),
    p_measured_mm_yr_raw = sum(p_measured_day, na.rm = FALSE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    precip_complete = n_days_present == expected_days,
    p_f_mm_yr = dplyr::if_else(precip_complete, p_f_mm_yr_raw, NA_real_),
    p_measured_mm_yr = dplyr::if_else(precip_complete, p_measured_mm_yr_raw, NA_real_),
    era_filled_share = dplyr::case_when(
      !precip_complete ~ NA_real_,
      is.na(p_f_mm_yr) ~ NA_real_,
      p_f_mm_yr == 0   ~ NA_real_,
      TRUE ~ pmin(pmax(1 - p_measured_mm_yr / p_f_mm_yr, 0), 1)
    )
  ) |>
  dplyr::select(site_id, wy_year, n_days_present, expected_days, precip_complete,
                p_f_mm_yr, era_filled_share)

if (any(wy_precip$n_days_present > wy_precip$expected_days)) {
  warning("Some site water-years have MORE daily rows than expected (possible duplicate TIMESTAMPs) -- flagged as incomplete (precip_complete=FALSE) rather than trusted.")
  wy_precip <- wy_precip |> dplyr::mutate(
    precip_complete = precip_complete & n_days_present <= expected_days,
    p_f_mm_yr = dplyr::if_else(precip_complete, p_f_mm_yr, NA_real_),
    era_filled_share = dplyr::if_else(precip_complete, era_filled_share, NA_real_)
  )
}

wy <- dplyr::full_join(wy_flux, wy_precip, by = c("site_id", "wy_year")) |>
  dplyr::mutate(
    LE_F_MDS_QC = NA_real_,  # no single annual QC value at water-year step; per-month pass already captured
    et_qc_source = "LE_F_MDS_QC per month (LE_CORR has no QC column of its own)",
    nee_qc = NA_real_,       # per-month, not a single value; pass flag below is the all-12-months rule
    pass_gpp = all_months_gpp_pass,
    pass_et  = all_months_et_pass,
    pass_precip_measured = !is.na(era_filled_share) & era_filled_share <= RSCOTT_ERA_FILLED_MAX_SHARE,
    time_step = "water",
    period_label = paste0("WY", wy_year),
    year = wy_year,
    p_f_qc = NA_real_  # per-day values aggregated into era_filled_share directly; see README
  ) |>
  dplyr::select(
    site_id, time_step, period_label, year,
    carbon_src, nee_qc, gpp_nt_gC_m2_yr, gpp_dt_gC_m2_yr,
    et_le_f_mds_mm_yr, et_le_corr_mm_yr, LE_F_MDS_QC, et_qc_source,
    p_f_mm_yr, p_f_qc, era_filled_share,
    n_months_present, n_months_gpp_passing, n_months_et_passing,
    n_days_present, precip_complete,
    pass_gpp, pass_et, pass_precip_measured
  )

message("Water-year rows (candidate sites): ", nrow(wy))

site_years <- dplyr::bind_rows(yy, wy) |> dplyr::arrange(site_id, time_step, year)

out_sy <- file.path(RSCOTT_ROOT, "tables", "site_years_candidates_full.csv")
readr::write_csv(site_years, out_sy)
write_meta(out_sy, input_sources = c(RSCOTT_DUCKDB_PATH, file.path(RSCOTT_ROOT, "tables", "candidate_sites.csv")),
  notes = paste0(
    "All candidate (IGBP-screened) sites, both time steps, every site-year (not yet ",
    "restricted to kept sites -- that restriction happens in 03_screen_and_package.R). ",
    "GPP units gC m-2 per period (yr for calendar, water-year total for water); ET and P ",
    "in mm per period. GPP/ET at water year are built from monthly_converted (confirmed ",
    "correctly unit-converted by the unit check). P at water year is built from DAILY (DD) ",
    "P_F/P_F_QC, NOT monthly -- raw MM P_F was discovered to be a mean-daily-rate, not a ",
    "monthly total (see unit_check_mm_vs_yy.csv and docs/report_back_2026-11-23.md); DD sums ",
    "to YY with no scaling and is used instead. era_filled_share: YY = 1 - P_F_QC (single ",
    "annual fraction); water year = amount-weighted across all days in the water year ",
    "(1 - sum(P_F_QC_day*P_F_day)/sum(P_F_day)), NA if the water year is incomplete ",
    "(n_days_present != expected_days) or P_F total is exactly 0 (undefined share). ",
    "pass_gpp/pass_et at calendar year use qc_passes() at QC_THRESHOLD_YY on the single ",
    "annual QC value; at water year they require all 12 months individually qc_passes() at ",
    "QC_THRESHOLD_MM. nee_qc/LE_F_MDS_QC columns are NA at the water-year step because there ",
    "is no single annual QC value there (n_months_*_passing reports the month count instead; ",
    "n_days_present/precip_complete report daily coverage for the P_F aggregate)."
  ))
message("Saved: ", out_sy)

message("\n02_build_site_years.R complete.")
