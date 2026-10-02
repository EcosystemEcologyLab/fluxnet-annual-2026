#' Per-site-year NEE, GPP, RECO, ET and H under the paper's QC gate
#'
#' Reads the pre-QC `annual` FLUXMET rows directly from the DuckDB store --
#' never `annual_qc`/`annual_converted`, which drop a whole row whenever that
#' site's NEE QC column fails (see `scripts/04_qc.R`), discarding ET/H years
#' that have a perfectly good `LE_F_MDS_QC`/`H_F_MDS_QC` but a failing NEE QC
#' for that year. Each variable is gated on its own QC column instead:
#' - NEE, GPP, RECO: the NEE QC column chosen by the per-site VUT/CUT rule in
#'   `scripts/04_qc.R` (VUT if the site has any non-NA `NEE_VUT_REF_QC`, else
#'   CUT if it has any non-NA `NEE_CUT_REF_QC`, else the site is never
#'   gated/qualified). GPP and RECO have no annual QC column of their own, so
#'   a row qualifies for them exactly when it qualifies for NEE.
#' - ET: `LE_F_MDS_QC`, independent of the NEE gate.
#' - H: `H_F_MDS_QC`, independent of the NEE gate.
#'
#' A row qualifies when its gating QC value is non-NA and
#' `(1 - QC) <= QC_THRESHOLD_YY` -- the identical `p_gapfilled`/`threshold`
#' comparison `scripts/04_qc.R` applies, not a hardcoded QC cutoff.
#' `QC_THRESHOLD_YY` must already be in scope (via
#' `source("R/pipeline_config.R")`).
#'
#' GPP and RECO additionally use a per-site, per-flux NT-preferred/DT-fallback
#' partitioning rule -- DT is used only when NT yields zero qualifying years
#' for that site, decided independently for GPP and RECO so one flux's
#' fallback never forces the other's. Ported from the same rule in
#' `scripts/assess_flux_data_by_igbp_shuttle.R`/
#' `scripts/assess_flux_data_by_igbp_fluxnet2015.R`, which now call this
#' function (directly or via [compute_site_annual_fluxes_from_df()]) instead
#' of maintaining their own copy of the rule.
#'
#' Units: NEE/GPP/RECO pass through unchanged (YY is already a pre-integrated
#' gC m-2 yr-1 total). ET (from `LE_F_MDS`) is converted via
#' [fluxnet_convert_units()] (`R/units.R`, which must already be sourced) to
#' mm H2O yr-1. H (from `H_F_MDS`) is converted to MJ m-2 yr-1 by default, or
#' left as its native annual-mean W m-2 rate (no conversion -- `H_F_MDS` is
#' already a mean rate at every resolution, not a pre-integrated total) when
#' `h_unit = "W_m2"`.
#'
#' A site's summary value for each variable is the median of its qualifying
#' annual values (minimum one qualifying year; zero qualifying years gives
#' `NA`, not an error) -- not a mean seasonal cycle.
#'
#' @param con An open read-only DBI connection to the FLUXNET DuckDB store
#'   (`data/duckdb/fluxnet.duckdb`), already containing an `annual` table.
#' @param site_ids Character vector of site IDs to restrict to, or `NULL`
#'   (default) for every FLUXMET site in the `annual` table.
#' @param h_unit `"MJ_m2_yr"` (default) for H as a pre-integrated annual total
#'   via [fluxnet_convert_units()], or `"W_m2"` for H as the native annual
#'   mean rate (no conversion applied).
#'
#' @return A list with two data frames:
#'   - `site_year`: one row per site x year, columns `site_id`, `year`,
#'     `carbon_src` ("VUT"/"CUT"/NA), `gpp_partition`/`reco_partition`
#'     ("NT"/"DT"/NA), and `NEE`/`GPP`/`RECO`/`ET`/`H` (`NA` where that
#'     variable does not qualify that year).
#'   - `site_summary`: one row per site, `nee_source`, `gpp_partition`,
#'     `reco_partition`, `n_years_<var>` and `<var>_median` for each of
#'     nee/gpp/reco/et/h.
#' @export
compute_site_annual_fluxes <- function(con, site_ids = NULL, h_unit = c("MJ_m2_yr", "W_m2")) {
  h_unit <- match.arg(h_unit)
  where_sites <- if (!is.null(site_ids)) {
    sprintf(" AND site_id IN (%s)", paste(sprintf("'%s'", site_ids), collapse = ", "))
  } else ""

  annual <- DBI::dbGetQuery(con, sprintf("
    SELECT site_id, TIMESTAMP AS year,
           NEE_VUT_REF, NEE_VUT_REF_QC, NEE_CUT_REF, NEE_CUT_REF_QC,
           GPP_NT_VUT_REF, GPP_NT_CUT_REF, GPP_DT_VUT_REF, GPP_DT_CUT_REF,
           RECO_NT_VUT_REF, RECO_NT_CUT_REF, RECO_DT_VUT_REF, RECO_DT_CUT_REF,
           LE_F_MDS, LE_F_MDS_QC, H_F_MDS, H_F_MDS_QC
    FROM annual WHERE dataset = 'FLUXMET'%s
  ", where_sites))
  annual$year <- as.integer(annual$year)

  .compute_site_annual_fluxes_core(annual, h_unit = h_unit)
}

#' Per-site-year NEE, GPP, RECO, ET and H from a pre-loaded annual data frame
#'
#' Same rules and QC gate as [compute_site_annual_fluxes()] (its full
#' documentation applies here), for callers whose annual YY data does not
#' live in the Shuttle DuckDB store -- e.g.
#' `scripts/assess_flux_data_by_igbp_fluxnet2015.R`, which reads the separate
#' FLUXNET2015 release's own per-site CSV files (comparison-only data under
#' CLAUDE.md Hard Rule 1, never primary data).
#'
#' @param annual_df A data frame with one row per site-year and exactly the
#'   columns [compute_site_annual_fluxes()] would have queried: `site_id`,
#'   `year` (integer), `NEE_VUT_REF`, `NEE_VUT_REF_QC`, `NEE_CUT_REF`,
#'   `NEE_CUT_REF_QC`, `GPP_NT_VUT_REF`, `GPP_NT_CUT_REF`, `GPP_DT_VUT_REF`,
#'   `GPP_DT_CUT_REF`, `RECO_NT_VUT_REF`, `RECO_NT_CUT_REF`,
#'   `RECO_DT_VUT_REF`, `RECO_DT_CUT_REF`, `LE_F_MDS`, `LE_F_MDS_QC`,
#'   `H_F_MDS`, `H_F_MDS_QC`. Values already native FLUXNET units (gC m-2
#'   yr-1 for carbon, W m-2 for LE/H); `-9999` sentinels must already be
#'   converted to `NA` by the caller.
#' @param h_unit As in [compute_site_annual_fluxes()].
#'
#' @return As in [compute_site_annual_fluxes()].
#' @export
compute_site_annual_fluxes_from_df <- function(annual_df, h_unit = c("MJ_m2_yr", "W_m2")) {
  h_unit <- match.arg(h_unit)
  .compute_site_annual_fluxes_core(annual_df, h_unit = h_unit)
}

#' @keywords internal
.compute_site_annual_fluxes_core <- function(annual, h_unit = "MJ_m2_yr") {
  ## ---- Per-site VUT/CUT source for NEE/GPP/RECO (same rule as 04_qc.R):
  ## VUT if the site has ANY non-NA NEE_VUT_REF_QC, else CUT if it has ANY
  ## non-NA NEE_CUT_REF_QC, else ungated -- a per-site decision, not per-row,
  ## so a site's years are never a VUT/CUT mixture.
  site_src <- annual |>
    dplyr::group_by(site_id) |>
    dplyr::summarise(
      n_vut = sum(!is.na(NEE_VUT_REF_QC)),
      n_cut = sum(!is.na(NEE_CUT_REF_QC)),
      .groups = "drop"
    ) |>
    dplyr::mutate(carbon_src = dplyr::case_when(
      n_vut > 0L ~ "VUT", n_cut > 0L ~ "CUT", TRUE ~ NA_character_
    ))

  d <- annual |>
    dplyr::left_join(dplyr::select(site_src, site_id, carbon_src), by = "site_id") |>
    dplyr::mutate(
      nee_val = dplyr::case_when(
        carbon_src == "VUT" ~ NEE_VUT_REF, carbon_src == "CUT" ~ NEE_CUT_REF, TRUE ~ NA_real_
      ),
      nee_qc = dplyr::case_when(
        carbon_src == "VUT" ~ NEE_VUT_REF_QC, carbon_src == "CUT" ~ NEE_CUT_REF_QC, TRUE ~ NA_real_
      ),
      ## Identical p_gapfilled/threshold comparison as scripts/04_qc.R, not a
      ## literal "QC >= 0.5" -- only numerically equivalent to that at the
      ## current QC_THRESHOLD_YY=0.50.
      nee_qualifies = !is.na(nee_qc) & !is.na(nee_val) & (1 - nee_qc) <= QC_THRESHOLD_YY,
      nee_gC = dplyr::if_else(nee_qualifies, nee_val, NA_real_),

      ## GPP/RECO have no annual QC of their own -- gated on the same
      ## nee_qualifies as the NEE value for that row/site.
      gpp_nt_val = dplyr::case_when(
        carbon_src == "VUT" ~ GPP_NT_VUT_REF, carbon_src == "CUT" ~ GPP_NT_CUT_REF, TRUE ~ NA_real_
      ),
      gpp_dt_val = dplyr::case_when(
        carbon_src == "VUT" ~ GPP_DT_VUT_REF, carbon_src == "CUT" ~ GPP_DT_CUT_REF, TRUE ~ NA_real_
      ),
      reco_nt_val = dplyr::case_when(
        carbon_src == "VUT" ~ RECO_NT_VUT_REF, carbon_src == "CUT" ~ RECO_NT_CUT_REF, TRUE ~ NA_real_
      ),
      reco_dt_val = dplyr::case_when(
        carbon_src == "VUT" ~ RECO_DT_VUT_REF, carbon_src == "CUT" ~ RECO_DT_CUT_REF, TRUE ~ NA_real_
      ),
      gpp_nt_gC  = dplyr::if_else(nee_qualifies, gpp_nt_val, NA_real_),
      gpp_dt_gC  = dplyr::if_else(nee_qualifies, gpp_dt_val, NA_real_),
      reco_nt_gC = dplyr::if_else(nee_qualifies, reco_nt_val, NA_real_),
      reco_dt_gC = dplyr::if_else(nee_qualifies, reco_dt_val, NA_real_),

      ## ET and H: gated on their own QC column, independent of the NEE gate.
      et_qualifies = !is.na(LE_F_MDS) & !is.na(LE_F_MDS_QC) & (1 - LE_F_MDS_QC) <= QC_THRESHOLD_YY,
      le_Wm2 = dplyr::if_else(et_qualifies, LE_F_MDS, NA_real_),
      h_qualifies = !is.na(H_F_MDS) & !is.na(H_F_MDS_QC) & (1 - H_F_MDS_QC) <= QC_THRESHOLD_YY,
      h_Wm2 = dplyr::if_else(h_qualifies, H_F_MDS, NA_real_)
    )

  ## ---- Per-site, per-flux NT/DT decision: NT if it has >=1 qualifying
  ## year for that site, else DT, else NA. GPP and RECO decided independently.
  site_partition <- d |>
    dplyr::group_by(site_id) |>
    dplyr::summarise(
      n_gpp_nt = sum(!is.na(gpp_nt_gC)), n_gpp_dt = sum(!is.na(gpp_dt_gC)),
      n_reco_nt = sum(!is.na(reco_nt_gC)), n_reco_dt = sum(!is.na(reco_dt_gC)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      gpp_partition = dplyr::case_when(
        n_gpp_nt > 0L ~ "NT", n_gpp_dt > 0L ~ "DT", TRUE ~ NA_character_
      ),
      reco_partition = dplyr::case_when(
        n_reco_nt > 0L ~ "NT", n_reco_dt > 0L ~ "DT", TRUE ~ NA_character_
      )
    )

  d <- d |>
    dplyr::left_join(dplyr::select(site_partition, site_id, gpp_partition, reco_partition), by = "site_id") |>
    dplyr::mutate(
      gpp_gC  = dplyr::case_when(
        gpp_partition == "NT" ~ gpp_nt_gC, gpp_partition == "DT" ~ gpp_dt_gC, TRUE ~ NA_real_
      ),
      reco_gC = dplyr::case_when(
        reco_partition == "NT" ~ reco_nt_gC, reco_partition == "DT" ~ reco_dt_gC, TRUE ~ NA_real_
      )
    )

  ## ---- Units: LE -> ET (mm H2O yr-1) always via fluxnet_convert_units().
  ## H -> MJ m-2 yr-1 via the same route by default; h_unit="W_m2" instead
  ## keeps H as its native annual-mean W m-2 rate (H_F_MDS is already a mean
  ## rate, never a pre-integrated total, at any resolution -- see R/units.R),
  ## so no conversion is a legitimate unit, not a skipped step.
  manifest <- data.frame(temporal_resolution = "YY")
  conv_input <- d |> dplyr::transmute(site_id, TIMESTAMP = year, LE_F_MDS = le_Wm2, H_F_MDS = h_Wm2)
  conv <- fluxnet_convert_units(conv_input, manifest)
  d$et_mm <- conv$LE_F_MDS
  d$h_out <- if (h_unit == "W_m2") d$h_Wm2 else conv$H_F_MDS

  site_year <- d |>
    dplyr::transmute(
      site_id, year, carbon_src, gpp_partition, reco_partition,
      NEE = nee_gC, GPP = gpp_gC, RECO = reco_gC, ET = et_mm, H = h_out
    )

  site_summary <- site_year |>
    dplyr::group_by(site_id) |>
    dplyr::summarise(
      nee_source     = dplyr::first(carbon_src),
      gpp_partition  = dplyr::first(gpp_partition),
      reco_partition = dplyr::first(reco_partition),
      n_years_nee  = sum(!is.na(NEE)),
      n_years_gpp  = sum(!is.na(GPP)),
      n_years_reco = sum(!is.na(RECO)),
      n_years_et   = sum(!is.na(ET)),
      n_years_h    = sum(!is.na(H)),
      nee_median  = stats::median(NEE,  na.rm = TRUE),
      gpp_median  = stats::median(GPP,  na.rm = TRUE),
      reco_median = stats::median(RECO, na.rm = TRUE),
      et_median   = stats::median(ET,   na.rm = TRUE),
      h_median    = stats::median(H,    na.rm = TRUE),
      .groups = "drop"
    )

  list(site_year = site_year, site_summary = site_summary)
}
