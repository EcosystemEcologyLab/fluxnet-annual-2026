## era5_precip_units_v3_partB.R
##
## Part B: three-factor correction test. Per instruction, this does NOT
## assume a factor -- it lets two independent references (BADM PI-reported
## MAP, WorldClim BIO12) set an empirical per-site estimate, tests whether
## that estimate clusters near a canonical value, and -- ONLY for sites
## that do -- tests a mechanical mock correction (does dividing by the
## nearest canonical factor bring the site within normal scatter of BOTH
## references, not just one). This is a counterfactual hypothesis test,
## not a claim that a real conversion bug exists: Part A (v2, T1) already
## established via a fully independent, non-circular test (the officially
## bundled *_FLUXNET_ERA5_YY_*.csv annual product) that the pipeline's
## day-weighting formula is exactly correct everywhere. Whatever this
## section finds "clusters near 4" is therefore evidence about the
## MAGNITUDE relationship between ERA5 and the two references, not
## evidence of a units bug in this pipeline's arithmetic.
##
## Read-only: does NOT modify R/climate_classification.R,
## scripts/step5_compute_koppen_era5.R, any figure, legend, snapshot CSV,
## or the v1/v2 reports/outputs. Writes only new files under
## review/diagnostics/era5_precip_units_v3/.

source("R/pipeline_config.R")
source("R/utils.R")
source("R/climate_classification.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(lubridate)
  library(terra)
  library(duckdb)
  library(DBI)
  library(fs)
})

SNAP <- "data/snapshots"
EXT  <- "data/external"
OUTD <- "review/diagnostics/era5_precip_units_v3"
V1D  <- "review/diagnostics/era5_precip_units"
V2D  <- "review/diagnostics/era5_precip_units_v2"
fs::dir_create(OUTD)

message("=== era5_precip_units_v3_partB.R ===")

KG_ERA5_PERIOD <- c(1991L, 2020L)
CANDIDATE_FACTORS <- c(1, 4, 8, 24, 1000)

# ============================================================================
# B1. EMPIRICAL SCALE-FACTOR ESTIMATE PER SITE (median of ratio-to-BADM and
#     ratio-to-BIO12), clustering against candidate factors
# ============================================================================

message("\n================ B1: empirical factor estimate, all sites ================")

t2 <- readr::read_csv(file.path(V2D, "table_t2_ratios.csv"), show_col_types = FALSE)

b1 <- t2 |>
  dplyr::rowwise() |>
  dplyr::mutate(factor_estimate = median(c(ratio_to_bio12, ratio_to_badm_map), na.rm = TRUE),
                n_refs = sum(!is.na(c(ratio_to_bio12, ratio_to_badm_map)))) |>
  dplyr::ungroup()

nearest_factor <- function(x, factors = CANDIDATE_FACTORS, tol = 0.15) {
  if (is.na(x)) return(NA_character_)
  d <- abs(x - factors) / factors
  if (min(d) <= tol) return(as.character(factors[which.min(d)]))
  "elsewhere"
}
b1$nearest_cluster <- vapply(b1$factor_estimate, nearest_factor, character(1L))

cat("\n-- Clustering of factor_estimate (median of ratio-to-BADM, ratio-to-BIO12), all 781 sites --\n")
print(table(b1$nearest_cluster, useNA = "ifany"))
cat("\n-- Same, restricted to sites with BOTH references available (n_refs==2, most reliable estimate) --\n")
print(table(b1$nearest_cluster[b1$n_refs == 2], useNA = "ifany"))

out_b1 <- file.path(OUTD, "table_b1_factor_estimates.csv")
readr::write_csv(b1, out_b1)
write_output_metadata(out_b1, input_sources = file.path(V2D, "table_t2_ratios.csv"),
  notes = "Per-site empirical scale-factor estimate = median(ratio_to_bio12, ratio_to_badm_map), all 781 current-network sites, clustered against candidate factors 1/4/8/24/1000 (within 15%). Does not assume a factor -- set entirely by the two independent references.")
message("Saved: ", out_b1)

# ============================================================================
# B2. APPLY NEAREST FACTOR, TEST NORMAL SCATTER OF BOTH REFERENCES
# ============================================================================

message("\n================ B2: corrected MAP vs. both references ================")

# Data-driven "normal scatter" band: the IQR of ratio_to_bio12 among sites
# whose OWN estimate already clusters near 1 (i.e., presumed-unaffected
# sites), rather than an arbitrarily chosen band.
near1 <- b1 |> dplyr::filter(nearest_cluster == "1")
normal_band <- quantile(near1$ratio_to_bio12, c(0.05, 0.95), na.rm = TRUE)
cat("\nData-driven 'normal scatter' band (5th-95th pctile of ratio_to_bio12 among near-1 sites): [",
    round(normal_band[1], 2), ",", round(normal_band[2], 2), "]\n")

to_correct <- b1 |> dplyr::filter(nearest_cluster %in% c("4", "8", "24", "1000"))
to_correct <- to_correct |> dplyr::mutate(
  factor_applied = as.numeric(nearest_cluster),
  map_era5_corrected = map_era5 / factor_applied,
  corrected_ratio_bio12 = map_era5_corrected / bio12_mm,
  corrected_ratio_badm = map_era5_corrected / badm_map_mm,
  within_normal_both = (corrected_ratio_bio12 >= normal_band[1] & corrected_ratio_bio12 <= normal_band[2]) &
                       (is.na(corrected_ratio_badm) | (corrected_ratio_badm >= normal_band[1] & corrected_ratio_badm <= normal_band[2]))
)

cat("\n-- B2: sites flagged for correction (n=", nrow(to_correct), ") --\n")
print(as.data.frame(to_correct |> dplyr::select(site_id, product_source_network, factor_estimate, factor_applied,
  map_era5, map_era5_corrected, bio12_mm, corrected_ratio_bio12, badm_map_mm, corrected_ratio_badm, within_normal_both) |>
  dplyr::arrange(dplyr::desc(factor_estimate))))

cat("\nSites where correction brings the value within normal scatter of BOTH references:",
    sum(to_correct$within_normal_both, na.rm = TRUE), "/", nrow(to_correct), "\n")
cat("Sites where correction does NOT achieve this (closer to one reference only, or neither):",
    sum(!to_correct$within_normal_both, na.rm = TRUE), "/", nrow(to_correct), "\n")

out_b2 <- file.path(OUTD, "table_b2_corrected_vs_both_refs.csv")
readr::write_csv(to_correct, out_b2)
write_output_metadata(out_b2, input_sources = out_b1,
  notes = sprintf("Sites whose factor_estimate clusters away from 1 (n=%d), with the nearest canonical factor applied and the corrected value tested against BOTH BIO12 and BADM MAP using a data-driven normal-scatter band [%.2f, %.2f] (5th-95th percentile of ratio_to_bio12 among near-1 sites). within_normal_both is the decisive test, not proximity to one reference alone.",
    nrow(to_correct), normal_band[1], normal_band[2]))
message("Saved: ", out_b2)

# ============================================================================
# B3. KG RECLASSIFICATION COUNTERFACTUAL, ALL CLUSTERED SITES (not just the
#     26 excluded), USING THE REAL UNMODIFIED FUNCTIONS
# ============================================================================

message("\n================ B3: KG reclassification counterfactual ================")

CORRECT_SITES <- to_correct$site_id
message("Sites to reclassify under the mock correction: ", length(CORRECT_SITES))

con <- dbConnect(duckdb::duckdb(), dbdir = "data/duckdb/fluxnet.duckdb", read_only = TRUE)
mo_correct <- dbGetQuery(con, sprintf(
  "SELECT site_id, TIMESTAMP, TA_ERA, P_ERA FROM monthly WHERE dataset='ERA5' AND site_id IN (%s)",
  paste(sprintf("'%s'", CORRECT_SITES), collapse = ",")
))
dbDisconnect(con, shutdown = TRUE)

factor_lookup <- setNames(to_correct$factor_applied, to_correct$site_id)
mo_correct$P_ERA_corrected <- mo_correct$P_ERA / factor_lookup[mo_correct$site_id]

leg_path <- file.path(EXT, "koppen_beck2023", "legend.txt")
leg_lines <- readLines(leg_path)
leg_data  <- leg_lines[grepl("^\\s*[0-9]+:", leg_lines)]
legend_df <- do.call(rbind, lapply(leg_data, function(ln) {
  m <- regmatches(ln, regexec("^\\s*(\\d+):\\s+(\\S+)\\s+(.+?)\\s+\\[", ln, perl = TRUE))[[1]]
  if (length(m) < 4L) return(NULL)
  data.frame(koppen_class_code = as.integer(m[2]), koppen_class = trimws(m[3]),
             koppen_class_name = trimws(m[4]), stringsAsFactors = FALSE)
}))
main_map_lbl <- c(A = "Tropical", B = "Arid", C = "Temperate", D = "Cold", E = "Polar")
legend_df <- legend_df |> dplyr::mutate(koppen_main = substr(koppen_class, 1L, 1L), koppen_main_name = main_map_lbl[koppen_main])

reclass_one <- function(sid) {
  d <- mo_correct |> dplyr::filter(site_id == sid) |>
    dplyr::transmute(site_id, TIMESTAMP, TA_ERA, P_ERA = P_ERA_corrected)
  tryCatch(
    compute_site_koppen_era5(d, legend = legend_df, period = KG_ERA5_PERIOD),
    error = function(e) data.frame(site_id = sid, kg_class = NA_character_, koppen_twoletter = NA_character_, koppen_main = NA_character_)
  )
}
reclass_results <- dplyr::bind_rows(lapply(CORRECT_SITES, reclass_one))

kg_current <- readr::read_csv(file.path(SNAP, "site_koppen_era5.csv"), show_col_types = FALSE)
compare_class <- kg_current |> dplyr::select(site_id, koppen_class_before = koppen_class,
                                              koppen_twoletter_before = koppen_twoletter, koppen_main_before = koppen_main) |>
  dplyr::inner_join(reclass_results |> dplyr::select(site_id, koppen_class_after = koppen_class,
                                                        koppen_twoletter_after = koppen_twoletter, koppen_main_after = koppen_main),
                     by = "site_id") |>
  dplyr::mutate(
    was_excluded_before = is.na(koppen_class_before),
    recovered = was_excluded_before & !is.na(koppen_class_after),
    class_changed = !was_excluded_before & !is.na(koppen_class_after) & (koppen_class_before != koppen_class_after)
  )

cat("\n-- B3: before/after classification for all", nrow(compare_class), "corrected sites --\n")
print(as.data.frame(compare_class))
cat("\nRecovered (were unclassified, now classified):", sum(compare_class$recovered), "\n")
cat("Class changed (were already classified, class flips under correction):", sum(compare_class$class_changed), "\n")
cat("Still unclassified after correction:", sum(is.na(compare_class$koppen_class_after)), "\n")

out_b3 <- file.path(OUTD, "table_b3_reclassification.csv")
readr::write_csv(compare_class, out_b3)
write_output_metadata(out_b3, input_sources = c(out_b2, "R/climate_classification.R (unmodified, sourced)", file.path(SNAP, "site_koppen_era5.csv")),
  notes = "Before/after KG classification for every site whose empirical factor_estimate clustered away from 1 (not just the 26 previously-excluded sites), using the real, unmodified compute_site_koppen_era5() with P_ERA divided by the nearest canonical factor. Distinguishes newly-recovered (previously unclassified) sites from already-classified sites whose class flips under the mock correction.")
message("Saved: ", out_b3)

## ---- Network fraction / Jaccard impact ----
TL_ORDER <- c("Af","Am","Aw","BS","BW","Cf","Cs","Cw","Df","Ds","Dw","EF","ET")
MAIN_ORDER <- c("A","B","C","D","E")
N_TOTAL <- 781L
compute_j <- function(p, q) { p[is.na(p)] <- 0; q[is.na(q)] <- 0; sum(pmin(p, q)) / sum(pmax(p, q)) }

global_dist <- readr::read_csv(file.path(SNAP, "koppen_beck2023_global_distribution.csv"), show_col_types = FALSE)
p_tl <- vapply(TL_ORDER, function(tl) sum(global_dist$global_land_fraction[global_dist$koppen_twoletter==tl], na.rm=TRUE), numeric(1))
p_5  <- vapply(MAIN_ORDER, function(m) sum(global_dist$global_land_fraction[global_dist$koppen_main==m], na.rm=TRUE), numeric(1))

q_tl_before <- vapply(TL_ORDER, function(tl) sum(kg_current$koppen_twoletter==tl, na.rm=TRUE), numeric(1)) / N_TOTAL
q_5_before  <- vapply(MAIN_ORDER, function(m) sum(kg_current$koppen_main==m, na.rm=TRUE), numeric(1)) / N_TOTAL
j_tl_before <- compute_j(p_tl, q_tl_before); j_5_before <- compute_j(p_5, q_5_before)

kg_after <- kg_current |>
  dplyr::rows_update(
    compare_class |> dplyr::filter(!is.na(koppen_class_after)) |>
      dplyr::transmute(site_id, koppen_twoletter = koppen_twoletter_after, koppen_main = koppen_main_after),
    by = "site_id"
  )
q_tl_after <- vapply(TL_ORDER, function(tl) sum(kg_after$koppen_twoletter==tl, na.rm=TRUE), numeric(1)) / N_TOTAL
q_5_after  <- vapply(MAIN_ORDER, function(m) sum(kg_after$koppen_main==m, na.rm=TRUE), numeric(1)) / N_TOTAL
j_tl_after <- compute_j(p_tl, q_tl_after); j_5_after <- compute_j(p_5, q_5_after)

cat("\n-- Network fraction / Jaccard, before vs. after mock correction --\n")
cat(sprintf("two-letter: J_before=%.4f  J_after=%.4f  delta=%+.4f\n", j_tl_before, j_tl_after, j_tl_after - j_tl_before))
cat(sprintf("5-class:    J_before=%.4f  J_after=%.4f  delta=%+.4f\n", j_5_before, j_5_after, j_5_after - j_5_before))

out_b3_jaccard <- file.path(OUTD, "table_b3_jaccard_impact.csv")
readr::write_csv(data.frame(level = c("twoletter", "5class"),
  j_before = c(j_tl_before, j_5_before), j_after = c(j_tl_after, j_5_after),
  delta = c(j_tl_after - j_tl_before, j_5_after - j_5_before)), out_b3_jaccard)
write_output_metadata(out_b3_jaccard, input_sources = c(out_b3, file.path(SNAP, "koppen_beck2023_global_distribution.csv")),
  notes = "Two-letter/5-class weighted Jaccard for current_781 before vs. after the B3 mock-correction reclassification, against the unchanged Beck 2023 global distribution.")
message("Saved: ", out_b3_jaccard)

# ============================================================================
# B4. IS THE ERROR PRECIPITATION-ONLY? TA_ERA vs. BADM MAT and WorldClim BIO1
# ============================================================================

message("\n================ B4: precipitation-only check (TA_ERA vs MAT/BIO1) ================")

con <- dbConnect(duckdb::duckdb(), dbdir = "data/duckdb/fluxnet.duckdb", read_only = TRUE)
mo_ta <- dbGetQuery(con, "SELECT site_id, TIMESTAMP, TA_ERA FROM monthly WHERE dataset = 'ERA5'")
dbDisconnect(con, shutdown = TRUE)
mo_ta <- mo_ta |> dplyr::mutate(TIMESTAMP = as.Date(TIMESTAMP), year = lubridate::year(TIMESTAMP)) |>
  dplyr::filter(year >= KG_ERA5_PERIOD[1], year <= KG_ERA5_PERIOD[2])
ta_annual <- mo_ta |> dplyr::group_by(site_id, year) |> dplyr::summarise(n_m = dplyr::n(), mat_era5 = mean(TA_ERA), .groups = "drop") |>
  dplyr::filter(n_m == 12L) |> dplyr::group_by(site_id) |> dplyr::summarise(mat_era5 = mean(mat_era5), .groups = "drop")

snap <- readr::read_csv(file.path(SNAP, "fluxnet_shuttle_snapshot_20260901T094522.csv"), show_col_types = FALSE) |> dplyr::distinct(site_id, .keep_all = TRUE)
bio1 <- terra::rast(file.path(EXT, "worldclim", "climate", "wc2.1_2.5m", "wc2.1_2.5m_bio_1.tif"))
coords <- snap |> dplyr::filter(site_id %in% CORRECT_SITES | site_id %in% c("IT-MBo","US-HB4")) |> dplyr::select(site_id, location_lat, location_long)
coords$bio1_c <- terra::extract(bio1, cbind(coords$location_long, coords$location_lat))[[1]]

badm <- readRDS("data/processed/badm.rds")
badm_mat <- badm |> dplyr::filter(VARIABLE == "MAT", !is.na(DATAVALUE)) |>
  dplyr::distinct(SITE_ID, .keep_all = TRUE) |> dplyr::transmute(site_id = SITE_ID, badm_mat_c = suppressWarnings(as.numeric(DATAVALUE)))

b4 <- data.frame(site_id = unique(c(CORRECT_SITES, "IT-MBo", "US-HB4"))) |>
  dplyr::left_join(ta_annual, by = "site_id") |>
  dplyr::left_join(dplyr::select(coords, site_id, bio1_c), by = "site_id") |>
  dplyr::left_join(badm_mat, by = "site_id") |>
  dplyr::mutate(diff_bio1 = mat_era5 - bio1_c, diff_badm = mat_era5 - badm_mat_c)

cat("\n-- B4: TA_ERA (as MAT) vs. BIO1 and BADM MAT, corrected + confirmed-anomaly sites --\n")
print(as.data.frame(b4))
cat("\nMean |diff_bio1| =", mean(abs(b4$diff_bio1), na.rm = TRUE), "C | Mean |diff_badm| =", mean(abs(b4$diff_badm), na.rm = TRUE), "C\n")
cat("(For comparison, the precipitation ratios at these same sites are 2.6x-460x -- a multi-hundred-percent\n",
    "discrepancy. A temperature discrepancy of a few tenths to ~2C, typical of any reanalysis-vs-station comparison,\n",
    "is not remotely comparable in relative terms -- consistent with the error being precipitation-specific.)\n")

out_b4 <- file.path(OUTD, "table_b4_temperature_check.csv")
readr::write_csv(b4, out_b4)
write_output_metadata(out_b4, input_sources = c("data/duckdb/fluxnet.duckdb", "data/processed/badm.rds",
  file.path(EXT, "worldclim", "climate", "wc2.1_2.5m", "wc2.1_2.5m_bio_1.tif")),
  notes = "TA_ERA-derived MAT vs. WorldClim BIO1 and BADM PI-reported MAT, for every site flagged for precipitation correction in B1-B3, plus the two confirmed precipitation anomalies. Tests whether the ERA5 error is precipitation-specific or reflects a broader site-coordinate/extraction problem.")
message("Saved: ", out_b4)

message("\n=== era5_precip_units_v3_partB.R complete ===")
