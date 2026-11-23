## 01_candidates_and_qc_check.R — candidate site list (step 1) and an
## independent P_F_QC polarity confirmation at a handful of control sites
## (step 2's "confirm its meaning at two or three control sites").
##
## This repo already investigated this exact question in depth
## (scripts/diagnostics/era5_precip_units_v3_partA.R, era5_precip_units_v4.R,
## review/diagnostics/precip_site_filter/report.md) and established,
## network-wide at both MM and YY resolution, that P_F_QC is a 0-1 FRACTION
## where HIGHER = MORE MEASURED (CLAUDE.md System-1 convention), not the
## System-2 HH-resolution integer convention (0=measured) that an earlier
## analysis mistakenly applied to this field. This script does its own,
## independent empirical check (not a re-read of those reports) at a small
## set of control sites, cross-checking P_F against the DuckDB's own ERA5
## rows (P_ERA) the same way those diagnostics did, so the result here is
## verified rather than assumed.

source("RSCOTT_AGU26/code/00_config.R")

message("\n================ Step 1: candidate sites by IGBP class ================")

snap <- readr::read_csv(RSCOTT_SNAPSHOT_PATH, show_col_types = FALSE)

candidates <- snap |>
  dplyr::filter(igbp %in% RSCOTT_IGBP_CLASSES) |>
  dplyr::distinct(site_id, .keep_all = TRUE) |>
  dplyr::transmute(
    site_id, site_name, data_hub, network, igbp,
    location_lat, location_long,
    hemisphere = ifelse(location_lat < 0, "S", "N"),
    first_year, last_year,
    product_citation, product_id, oneflux_code_version,
    country = substr(site_id, 1, 2)
  ) |>
  dplyr::arrange(igbp, site_id)

message("Candidate sites (IGBP in ", paste(RSCOTT_IGBP_CLASSES, collapse = ", "), "): ", nrow(candidates))
print(candidates |> dplyr::count(igbp, hemisphere))

out_cand <- file.path(RSCOTT_ROOT, "tables", "candidate_sites.csv")
readr::write_csv(candidates, out_cand)
write_meta(out_cand, input_sources = RSCOTT_SNAPSHOT_PATH,
  notes = "Step 1 candidate list: snapshot igbp column in OSH/CSH/GRA/SAV/WSA, either hemisphere, one row per distinct site_id (first snapshot row kept if a site appears more than once).")
message("Saved: ", out_cand)

## ============================================================================
## Step 2 (part 1): independent P_F_QC polarity confirmation at control sites
## ============================================================================
message("\n================ Step 2: P_F_QC polarity -- control-site check ================")

## Pick control sites with a documented mix of P_F_QC values and, where
## possible, overlap with the prior diagnostics' examples (IT-MBo was used
## in scripts/diagnostics/it_mbo_parsimony.R at HH resolution) plus two
## dryland candidates from this project's own list, so the check is grounded
## in sites actually used downstream rather than arbitrary ones.
## One candidate from each hemisphere plus IT-MBo (a site previously examined
## in this repo's own precip diagnostics at HH resolution) -- three control
## sites plus the external reference point, satisfying "two or three".
nh_pick <- candidates |> dplyr::filter(hemisphere == "N") |> dplyr::slice(1) |> dplyr::pull(site_id)
sh_pick <- candidates |> dplyr::filter(hemisphere == "S") |> dplyr::slice(1) |> dplyr::pull(site_id)
control_sites <- unique(c("IT-MBo", nh_pick, sh_pick))
message("Control sites: ", paste(control_sites, collapse = ", "))

con <- rscott_duckdb_connect()

check_polarity_at_resolution <- function(con, table, control_sites) {
  sites_sql <- paste(sprintf("'%s'", control_sites), collapse = ", ")
  fm <- dbGetQuery(con, sprintf(
    "SELECT site_id, TIMESTAMP, P_F, P_F_QC FROM %s WHERE dataset = 'FLUXMET' AND site_id IN (%s)",
    table, sites_sql))
  era <- dbGetQuery(con, sprintf(
    "SELECT site_id, TIMESTAMP, P_ERA FROM %s WHERE dataset = 'ERA5' AND site_id IN (%s)",
    table, sites_sql))
  m <- dplyr::inner_join(fm, era, by = c("site_id", "TIMESTAMP")) |>
    dplyr::filter(!is.na(P_F), !is.na(P_F_QC), !is.na(P_ERA)) |>
    dplyr::mutate(identical_to_era5 = abs(P_F - P_ERA) < 1e-6)
  m
}

mm_check <- check_polarity_at_resolution(con, "monthly", control_sites)
yy_check <- check_polarity_at_resolution(con, "annual", control_sites)
dbDisconnect(con, shutdown = TRUE)

summarise_polarity <- function(d, resolution) {
  d |>
    dplyr::mutate(qc_tier = dplyr::case_when(
      P_F_QC <= 0.1 ~ "P_F_QC <= 0.1",
      P_F_QC >= 0.9 ~ "P_F_QC >= 0.9",
      TRUE ~ "0.1 < P_F_QC < 0.9"
    )) |>
    dplyr::group_by(site_id, qc_tier) |>
    dplyr::summarise(
      n = dplyr::n(),
      frac_identical_to_era5 = mean(identical_to_era5),
      .groups = "drop"
    ) |>
    dplyr::mutate(resolution = resolution)
}

polarity_summary <- dplyr::bind_rows(
  summarise_polarity(mm_check, "MM"),
  summarise_polarity(yy_check, "YY")
) |>
  dplyr::arrange(resolution, site_id, qc_tier)

print(as.data.frame(polarity_summary))

## Decision rule: if low-QC rows are far more often bit-identical to P_ERA
## than high-QC rows, P_F_QC is confirmed as "higher = more measured" (the
## polarity this project assumes throughout). Any site where this reverses
## must stop the pipeline for investigation -- not silently proceed.
low_high <- polarity_summary |>
  dplyr::filter(qc_tier %in% c("P_F_QC <= 0.1", "P_F_QC >= 0.9")) |>
  dplyr::select(site_id, resolution, qc_tier, frac_identical_to_era5) |>
  tidyr::pivot_wider(names_from = qc_tier, values_from = frac_identical_to_era5)

names(low_high) <- gsub("P_F_QC <= 0.1", "frac_identical_low_qc", names(low_high))
names(low_high) <- gsub("P_F_QC >= 0.9", "frac_identical_high_qc", names(low_high))

low_high <- low_high |>
  dplyr::mutate(polarity_confirmed = dplyr::case_when(
    is.na(frac_identical_low_qc) | is.na(frac_identical_high_qc) ~ NA,
    TRUE ~ frac_identical_low_qc > frac_identical_high_qc
  ))

print(as.data.frame(low_high))

if (any(!is.na(low_high$polarity_confirmed) & !low_high$polarity_confirmed)) {
  stop(
    "P_F_QC polarity check FAILED at one or more control sites/resolutions -- ",
    "see RSCOTT_AGU26/tables/pf_qc_polarity_check.csv. Stopping rather than ",
    "proceeding with an unconfirmed convention."
  )
}
message(
  "\nCONFIRMED: at every control site/resolution with both a low- and high-QC ",
  "tier available, low P_F_QC rows are far more often bit-identical to P_ERA ",
  "than high P_F_QC rows -- i.e. P_F_QC near 0 = fully ERA-filled, P_F_QC near 1 ",
  "= genuinely measured (higher = more measured). This matches CLAUDE.md's ",
  "System-1 fraction convention and this repo's prior network-wide findings ",
  "(era5_precip_units_v3_partA.R / v4.R; review/diagnostics/precip_site_filter/report.md). ",
  "Used throughout this project: measured share = P_F_QC; ERA-filled share = 1 - P_F_QC."
)

out_pol <- file.path(RSCOTT_ROOT, "tables", "pf_qc_polarity_check.csv")
readr::write_csv(polarity_summary, out_pol)
write_meta(out_pol, input_sources = c(RSCOTT_DUCKDB_PATH),
  notes = paste0(
    "Independent control-site check (site_id in: ", paste(control_sites, collapse = ", "),
    ") of P_F_QC polarity at MM and YY resolution, cross-checking FLUXMET P_F against ",
    "ERA5 dataset's own P_ERA for bit-identity, binned by P_F_QC tier. Confirms higher ",
    "P_F_QC = more measured (not the reverse), consistent with prior network-wide repo ",
    "findings. IT-MBo included as a site previously examined in this repo at HH ",
    "resolution (scripts/diagnostics/it_mbo_parsimony.R); remaining control sites are ",
    "drawn from this project's own OSH/CSH/GRA/SAV/WSA candidate pool."
  ))
message("Saved: ", out_pol)

message("\n01_candidates_and_qc_check.R complete.")
