## era5_precip_units_v4.R
##
## Follow-up to review/diagnostics/era5_precip_units_v3/. Read-and-report
## only: no counterfactual, no reclassification, no pipeline edits. Answers
## a single scoping question -- how many of the 781 sites have ERA5
## precipitation that cannot be trusted, and whether that set is confined
## to the 26 sites the KG_ERA5_MAP_MAX_MM screen already excludes.
##
## Reuses v3's Part B empirical clustering (table_b1_factor_estimates.csv)
## and v2's reference table (table_t2_ratios.csv) verbatim -- does not
## refit anything. Adds: hub/network/resolution/gauge-availability
## cross-tabs, a genuinely-independent gauge check (using the corrected
## P_F_QC>=0.9 polarity established in v3 A4), a BADM-vs-BIO12
## independence check, and a final scoping count.
##
## Read-only: does NOT modify R/climate_classification.R,
## scripts/step5_compute_koppen_era5.R, any figure, legend, snapshot CSV,
## or the v1/v2/v3 reports/outputs. Writes only new files under
## review/diagnostics/era5_precip_units_v4/.

source("R/pipeline_config.R")
source("R/utils.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(lubridate)
  library(duckdb)
  library(DBI)
  library(fs)
})

SNAP <- "data/snapshots"
OUTD <- "review/diagnostics/era5_precip_units_v4"
V1D  <- "review/diagnostics/era5_precip_units"
V2D  <- "review/diagnostics/era5_precip_units_v2"
V3D  <- "review/diagnostics/era5_precip_units_v3"
fs::dir_create(OUTD)

message("=== era5_precip_units_v4.R ===")

KG_ERA5_PERIOD <- c(1991L, 2020L)

# ============================================================================
# Load v3's B1 clustering (unchanged) and add hub/network/resolution/coords
# ============================================================================

b1 <- readr::read_csv(file.path(V3D, "table_b1_factor_estimates.csv"), show_col_types = FALSE)
excl26 <- readr::read_csv(file.path(V1D, "table_excluded_sites.csv"), show_col_types = FALSE)
kg_current <- readr::read_csv(file.path(SNAP, "site_koppen_era5.csv"), show_col_types = FALSE)

b1 <- b1 |>
  dplyr::mutate(
    group = dplyr::case_when(
      nearest_cluster %in% c("4", "8") ~ "clustered_4x_8x",
      nearest_cluster == "1"           ~ "near_1x_control",
      TRUE ~ NA_character_
    ),
    was_in_original_26 = site_id %in% excl26$site_id
  ) |>
  dplyr::left_join(
    kg_current |> dplyr::select(site_id, kg_class_current = koppen_class),
    by = "site_id"
  ) |>
  dplyr::mutate(currently_classified = !is.na(kg_class_current))

n_clustered <- sum(b1$group == "clustered_4x_8x", na.rm = TRUE)
n_control   <- sum(b1$group == "near_1x_control", na.rm = TRUE)
message("Clustered (4x/8x): ", n_clustered, " | Near-1x control: ", n_control)

## ---- native temporal resolution, from BADM PRODUCT_TIME_RESOLUTION ----
## Not available in the manifest/snapshot CSV or in file_inventory.rds (that
## file's time_resolution column records the RESOLUTION OF THE EXTRACTED
## FILE -- MM/DD/WW/YY -- not the site's native HH/HR collection interval,
## since FLUXNET_EXTRACT_RESOLUTIONS="y m d" means no HH/HR files were ever
## extracted for all but one site, US-MMS). Found instead in BADM's
## PRODUCT_TIME_RESOLUTION field, which is what CLAUDE.md's "manifest"
## reference actually resolves to at the BADM layer. Reported here as
## found; not found for the ~3% of sites (22/781) lacking any BADM record.
badm <- readRDS("data/processed/badm.rds")
res_badm <- badm |>
  dplyr::filter(VARIABLE == "PRODUCT_TIME_RESOLUTION", !is.na(DATAVALUE)) |>
  dplyr::distinct(SITE_ID, .keep_all = TRUE) |>
  dplyr::transmute(site_id = SITE_ID, temporal_resolution = DATAVALUE)
message("PRODUCT_TIME_RESOLUTION found for ", nrow(res_badm), "/781 sites (source: data/processed/badm.rds)")

## ---- coordinates ----
snap <- readr::read_csv(file.path(SNAP, "fluxnet_shuttle_snapshot_20260901T094522.csv"), show_col_types = FALSE) |>
  dplyr::distinct(site_id, .keep_all = TRUE) |>
  dplyr::select(site_id, location_lat, location_long)

## ---- measured-precip availability, network-wide, using v3's corrected
##      polarity (P_F_QC near 1 = genuinely measured, NOT near 0) ----
con <- dbConnect(duckdb::duckdb(), dbdir = "data/duckdb/fluxnet.duckdb", read_only = TRUE)
fluxmet_all <- dbGetQuery(con, "SELECT site_id, TIMESTAMP, P_F, P_F_QC FROM monthly WHERE dataset = 'FLUXMET'")
dbDisconnect(con, shutdown = TRUE)

fluxmet_all <- fluxmet_all |>
  dplyr::mutate(TIMESTAMP = as.Date(TIMESTAMP), year = lubridate::year(TIMESTAMP)) |>
  dplyr::filter(year >= KG_ERA5_PERIOD[1], year <= KG_ERA5_PERIOD[2], !is.na(P_F_QC))

measured_flag <- fluxmet_all |>
  dplyr::group_by(site_id) |>
  dplyr::summarise(
    n_months_qc_ge_0.9 = sum(P_F_QC >= 0.9, na.rm = TRUE),
    has_measured_precip = n_months_qc_ge_0.9 > 0,
    .groups = "drop"
  )
message("Sites with >=1 genuinely-measured (P_F_QC>=0.9) month, 1991-2020: ",
        sum(measured_flag$has_measured_precip), "/", nrow(measured_flag))

b1 <- b1 |>
  dplyr::left_join(res_badm, by = "site_id") |>
  dplyr::left_join(snap, by = "site_id") |>
  dplyr::left_join(measured_flag, by = "site_id") |>
  dplyr::mutate(has_measured_precip = tidyr::replace_na(has_measured_precip, FALSE))

# ============================================================================
# 1. GROUPING -- cross-tabs, clustered vs. near-1x control
# ============================================================================

message("\n================ 1. GROUPING ================")

grp <- b1 |> dplyr::filter(group %in% c("clustered_4x_8x", "near_1x_control"))

xtab_pct <- function(var) {
  tt <- table(grp$group, grp[[var]], useNA = "ifany")
  prop <- prop.table(tt, margin = 1)
  list(counts = tt, prop = prop)
}

## Separation metric: for each variable, the max absolute difference in
## row-proportions between the two groups, across categories (a simple,
## interpretable measure of "which variable separates them most cleanly" --
## not a formal association test, since several cells are small).
separation_score <- function(var) {
  tt <- table(grp$group, grp[[var]], useNA = "ifany")
  prop <- prop.table(tt, margin = 1)
  if (nrow(prop) < 2) return(NA_real_)
  max(abs(prop["clustered_4x_8x", ] - prop["near_1x_control", ]))
}

vars_to_test <- c("data_hub", "product_source_network", "temporal_resolution", "has_measured_precip")
sep_scores <- sapply(vars_to_test, separation_score)
cat("\n-- Separation score (max |row-proportion difference|) by variable --\n")
print(sort(sep_scores, decreasing = TRUE))
best_var <- names(which.max(sep_scores))
cat("\nMost cleanly separating variable:", best_var, "(score =", round(max(sep_scores, na.rm = TRUE), 3), ")\n")

for (v in vars_to_test) {
  cat("\n-- Cross-tab:", v, "(counts) --\n")
  print(table(grp$group, grp[[v]], useNA = "ifany"))
  cat("-- Cross-tab:", v, "(row proportions) --\n")
  print(round(prop.table(table(grp$group, grp[[v]], useNA = "ifany"), margin = 1), 3))
}

crosstab_df <- function(rows, cols) {
  tt <- table(rows, cols, useNA = "ifany")
  colnames(tt)[is.na(colnames(tt))] <- "NA"
  df <- as.data.frame.matrix(tt)
  df <- cbind(group = rownames(df), df)
  rownames(df) <- NULL
  df
}

out_1_hub <- file.path(OUTD, "table_1a_crosstab_hub.csv")
readr::write_csv(crosstab_df(grp$group, grp$data_hub), out_1_hub)
write_output_metadata(out_1_hub, input_sources = file.path(V3D, "table_b1_factor_estimates.csv"),
  notes = "Contingency table of data_hub, clustered (4x/8x, n=123) vs. near-1x control (n=323) sites.")

out_1_net <- file.path(OUTD, "table_1b_crosstab_network.csv")
readr::write_csv(crosstab_df(grp$group, grp$product_source_network), out_1_net)
write_output_metadata(out_1_net, input_sources = file.path(V3D, "table_b1_factor_estimates.csv"),
  notes = "Contingency table of product_source_network, clustered vs. near-1x control sites.")

out_1_res <- file.path(OUTD, "table_1c_crosstab_temporal_resolution.csv")
readr::write_csv(crosstab_df(grp$group, grp$temporal_resolution), out_1_res)
write_output_metadata(out_1_res, input_sources = "data/processed/badm.rds (PRODUCT_TIME_RESOLUTION)",
  notes = "Contingency table of native temporal resolution (HH/HR, from BADM PRODUCT_TIME_RESOLUTION; NA where no BADM record exists -- 22/781 sites network-wide), clustered vs. near-1x control sites. Not derivable from file_inventory.rds or the snapshot CSV -- see script header comment.")

out_1_meas <- file.path(OUTD, "table_1d_crosstab_has_measured_precip.csv")
readr::write_csv(crosstab_df(grp$group, grp$has_measured_precip), out_1_meas)
write_output_metadata(out_1_meas, input_sources = "data/duckdb/fluxnet.duckdb (monthly, dataset='FLUXMET')",
  notes = "Contingency table of has_measured_precip (>=1 month with P_F_QC>=0.9, 1991-2020, using v3 A4's corrected QC polarity), clustered vs. near-1x control sites.")

## ---- coordinates of clustered sites ----
coord_tab <- grp |> dplyr::filter(group == "clustered_4x_8x") |>
  dplyr::select(site_id, data_hub, product_source_network, location_lat, location_long, factor_estimate, nearest_cluster) |>
  dplyr::arrange(product_source_network, location_lat)
out_1_coord <- file.path(OUTD, "table_1e_clustered_site_coordinates.csv")
readr::write_csv(coord_tab, out_1_coord)
write_output_metadata(out_1_coord, input_sources = c(file.path(V3D, "table_b1_factor_estimates.csv"), file.path(SNAP, "fluxnet_shuttle_snapshot_20260901T094522.csv")),
  notes = "Coordinates of all 123 clustered (4x/8x) sites, for checking regional concentration.")

coord_summary <- grp |> dplyr::filter(group == "clustered_4x_8x") |>
  dplyr::group_by(product_source_network) |>
  dplyr::summarise(n = dplyr::n(),
                    lat_min = min(location_lat, na.rm = TRUE), lat_max = max(location_lat, na.rm = TRUE),
                    long_min = min(location_long, na.rm = TRUE), long_max = max(location_long, na.rm = TRUE),
                    .groups = "drop") |>
  dplyr::arrange(dplyr::desc(n))
cat("\n-- Bounding box of clustered sites, by product_source_network --\n")
print(as.data.frame(coord_summary))
out_1_bbox <- file.path(OUTD, "table_1f_clustered_bbox_by_network.csv")
readr::write_csv(coord_summary, out_1_bbox)
write_output_metadata(out_1_bbox, input_sources = out_1_coord,
  notes = "Lat/long bounding box of clustered sites, grouped by product_source_network, to assess regional concentration compactly.")

# ============================================================================
# 2. GAUGE CHECK -- corrected ERA5 vs. genuinely-measured tower P_F, for
#    clustered sites only (the reference not used to fit the factor)
# ============================================================================

message("\n================ 2. GAUGE CHECK ================")

clustered_sites <- b1$site_id[b1$group == "clustered_4x_8x"]

genuine <- fluxmet_all |>
  dplyr::filter(site_id %in% clustered_sites, P_F_QC >= 0.9) |>
  dplyr::group_by(site_id) |>
  dplyr::summarise(n_genuine_months = dplyr::n(), map_gauge = mean(P_F, na.rm = TRUE) * 12, .groups = "drop")

gauge_check <- b1 |> dplyr::filter(group == "clustered_4x_8x") |>
  dplyr::mutate(factor_applied = as.numeric(nearest_cluster),
                map_era5_corrected = map_era5 / factor_applied) |>
  dplyr::left_join(genuine, by = "site_id") |>
  dplyr::mutate(has_gauge = !is.na(map_gauge),
                ratio_corrected_to_gauge = map_era5_corrected / map_gauge)

n_with_gauge <- sum(gauge_check$has_gauge)
cat("\nClustered sites with ANY genuinely-measured (P_F_QC>=0.9) tower precipitation:",
    n_with_gauge, "/", nrow(gauge_check), "\n")

if (n_with_gauge < 10) {
  cat("\n*** LOW POWER: fewer than 10 of the", nrow(gauge_check),
      "clustered sites have independent gauge data at all. This test cannot\n",
      "confirm or refute the correction for the cluster as a whole -- reporting\n",
      "the available cases below, but they are not representative of the group. ***\n")
}

print(as.data.frame(gauge_check |> dplyr::filter(has_gauge) |>
  dplyr::select(site_id, product_source_network, factor_applied, map_era5, map_era5_corrected,
                map_gauge, ratio_corrected_to_gauge, n_genuine_months)))

out_2 <- file.path(OUTD, "table_2_gauge_check.csv")
readr::write_csv(gauge_check, out_2)
write_output_metadata(out_2, input_sources = c(file.path(V3D, "table_b1_factor_estimates.csv"), "data/duckdb/fluxnet.duckdb (monthly, dataset='FLUXMET')"),
  notes = sprintf("Corrected ERA5 MAP vs. genuinely-measured tower P_F (P_F_QC>=0.9, the reference not used to fit factor_estimate), all 123 clustered sites. Only %d/%d have any gauge data at all -- see has_gauge / power warning in report.", n_with_gauge, nrow(gauge_check)))
message("Saved: ", out_2)

# ============================================================================
# 3. IS BADM INDEPENDENT OF WORLDCLIM?
# ============================================================================

message("\n================ 3. BADM INDEPENDENCE ================")

t2 <- readr::read_csv(file.path(V2D, "table_t2_ratios.csv"), show_col_types = FALSE)

badm_ind <- t2 |>
  dplyr::filter(!is.na(bio12_mm), !is.na(badm_map_mm)) |>
  dplyr::mutate(ratio_badm_bio12 = badm_map_mm / bio12_mm,
                within_2pct = abs(ratio_badm_bio12 - 1) <= 0.02,
                badm_round_100 = (badm_map_mm %% 100) == 0,
                badm_round_50  = (badm_map_mm %% 50) == 0)

cat("\nSites with both BADM MAP and WorldClim BIO12 present:", nrow(badm_ind), "/781\n")
cat("\n-- Distribution of ratio_badm_bio12 --\n")
print(summary(badm_ind$ratio_badm_bio12))
cat("\nWithin 2% of BIO12 (ratio in [0.98, 1.02]):", sum(badm_ind$within_2pct), "/", nrow(badm_ind),
    sprintf(" (%.1f%%)\n", 100 * mean(badm_ind$within_2pct)))
cat("BADM value is a round multiple of 100 mm:", sum(badm_ind$badm_round_100), "/", nrow(badm_ind), "\n")
cat("BADM value is a round multiple of 50 mm: ", sum(badm_ind$badm_round_50), "/", nrow(badm_ind), "\n")

## ---- BADM vs. BIO12, closeness to gauge, network-wide (not just clustered) ----
genuine_all <- fluxmet_all |>
  dplyr::filter(P_F_QC >= 0.9) |>
  dplyr::group_by(site_id) |>
  dplyr::summarise(n_genuine_months = dplyr::n(), map_gauge = mean(P_F, na.rm = TRUE) * 12, .groups = "drop")

closeness <- badm_ind |>
  dplyr::inner_join(genuine_all, by = "site_id") |>
  dplyr::mutate(diff_badm = abs(badm_map_mm - map_gauge), diff_bio12 = abs(bio12_mm - map_gauge),
                closer_to = dplyr::case_when(diff_badm < diff_bio12 ~ "badm", diff_bio12 < diff_badm ~ "bio12", TRUE ~ "tie"))

cat("\nSites with a gauge AND both BADM+BIO12 (independent 3-way comparison):", nrow(closeness), "\n")
if (nrow(closeness) > 0) {
  cat("Closer to gauge:\n"); print(table(closeness$closer_to))
} else {
  cat("*** No sites have gauge + BADM + BIO12 all three -- this comparison has no power. ***\n")
}

out_3a <- file.path(OUTD, "table_3a_badm_bio12_ratio.csv")
readr::write_csv(badm_ind, out_3a)
write_output_metadata(out_3a, input_sources = file.path(V2D, "table_t2_ratios.csv"),
  notes = "BADM MAP vs. WorldClim BIO12 ratio, roundness flags, all sites with both present (independence check for the v3 correction's two references).")

out_3b <- file.path(OUTD, "table_3b_closer_to_gauge.csv")
readr::write_csv(closeness, out_3b)
write_output_metadata(out_3b, input_sources = c(out_3a, "data/duckdb/fluxnet.duckdb (monthly, dataset='FLUXMET')"),
  notes = "Sites with genuinely-measured tower precipitation (the gauge) AND both BADM MAP and WorldClim BIO12: which reference sits closer to the gauge. Tests whether BADM behaves like an independent measurement or a WorldClim lookup.")

# ============================================================================
# 4. THE COUNT
# ============================================================================

message("\n================ 4. THE COUNT ================")

n_in_doubt <- n_clustered
n_in_doubt_and_orig26 <- sum(b1$group == "clustered_4x_8x" & b1$was_in_original_26, na.rm = TRUE)
n_in_doubt_classified <- sum(b1$group == "clustered_4x_8x" & b1$currently_classified, na.rm = TRUE)

final_count <- data.frame(
  metric = c(
    "Sites in doubt for ERA5 precipitation (cluster near 4x or 8x vs. BADM+BIO12)",
    "...of those, in the original 26 sites excluded by KG_ERA5_MAP_MAX_MM",
    "...of those, NOT in the original 26 (i.e. currently pass the >5000mm screen unflagged)",
    "...of those, currently carry a successful (non-NA) KG classification today"
  ),
  count = c(n_in_doubt, n_in_doubt_and_orig26, n_in_doubt - n_in_doubt_and_orig26, n_in_doubt_classified)
)
cat("\n-- THE COUNT --\n")
print(final_count)

out_4 <- file.path(OUTD, "table_4_final_count.csv")
readr::write_csv(final_count, out_4)
write_output_metadata(out_4, input_sources = c(file.path(V3D, "table_b1_factor_estimates.csv"), file.path(V1D, "table_excluded_sites.csv"), file.path(SNAP, "site_koppen_era5.csv")),
  notes = "Scoping count: sites in doubt for ERA5 precipitation, overlap with the original 26-site KG_ERA5_MAP_MAX_MM exclusion, and how many currently carry a successful KG classification that would be affected by any future correction.")
message("Saved: ", out_4)

message("\n=== era5_precip_units_v4.R complete ===")
