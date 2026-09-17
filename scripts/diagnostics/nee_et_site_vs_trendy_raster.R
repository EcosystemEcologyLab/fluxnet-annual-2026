## nee_et_site_vs_trendy_raster.R
##
## Raster-heavy half of the NEE/ET site-vs-TRENDY diagnostic: T1's
## raw-monthly-stack re-derivation, T3 (scale/cancellation), and T4
## (temporal window). See nee_et_site_vs_trendy_core.R for T1's
## percentile/ratio part, T2, T5, T6, T7 (which don't need the raw
## ~127GB TRENDY NetCDF archive).
##
## Read-only with respect to the existing pipeline: does NOT modify any
## existing script, figure, legend, snapshot CSV, or
## representativeness_metrics.csv, and does not touch data/duckdb/ (read
## only, read_only=TRUE connection). Writes only new files under
## review/diagnostics/nee_et_site_vs_trendy/.
##
## Uses the per-model intermediate annual (34-layer, 1990-2023, 0.5deg)
## GeoTIFFs already on disk at data/external/trendy/derived/intermediate/
## (produced by scripts/figure_representativeness_trendy_compute.R) for
## T3/T4 -- these do not need to be regenerated from the raw NetCDFs. Only
## T1's specific "re-derive from monthly stacks" request reads the raw
## NetCDFs directly, for one representative monthly-native model
## (CABLE-POP), both variables, full 1990-2023 window (cheap at CABLE-POP's
## native 1deg resolution) -- a stronger check than the literal "sample of
## 20 pixels" asked for, with the 20-pixel table reported as a subset.

source("R/pipeline_config.R")
source("R/utils.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(terra)
  library(duckdb)
  library(DBI)
  library(fs)
})

SNAP <- "data/snapshots"
EXT  <- "data/external"
OUTD <- "review/diagnostics/nee_et_site_vs_trendy"
INTER <- file.path(EXT, "trendy", "derived", "intermediate")
fs::dir_create(OUTD)

message("=== nee_et_site_vs_trendy_raster.R ===")

# Final-ensemble model list per logs/trendy_analysis_complete.marker (16
# models; excludes ELM -- missing 2023 data, and CLM-FATES -- no
# intermediate file on disk, per this session's investigation)
MODELS_FINAL <- c("CABLE-POP","CLASSIC","CLM","DLEM","ED","ELM-FATES","IBIS","ISAM",
                   "JULES-ES","LPJ-GUESS","LPJml","LPJwsl","LPX-Bern","ORCHIDEE","TEM","VISIT-UT")
stopifnot(length(MODELS_FINAL) == 16L)
YEARS <- 1990:2023
SECS_MONTH <- 30.4375 * 86400
SECS_YEAR  <- 365.25 * 86400
KG_TO_G    <- 1000

for (m in MODELS_FINAL) {
  for (v in c("nbp", "evapotrans")) {
    f <- file.path(INTER, paste0(m, "_", v, "_regridded.tif"))
    if (!file.exists(f)) stop("Missing intermediate: ", f)
  }
}
message("Confirmed all 16x2 = 32 intermediate files present.")

# ============================================================================
# 0. SITE PIXELS (reuse the paired dataset from the core script)
# ============================================================================

pairs_path <- file.path(OUTD, "table_paired_measured_vs_trendy.csv")
if (!file.exists(pairs_path)) stop("Run nee_et_site_vs_trendy_core.R first -- ", pairs_path, " not found.")
paired_all <- readr::read_csv(pairs_path, show_col_types = FALSE)

flux <- readr::read_csv(file.path(SNAP, "site_flux_medians_shuttle.csv"), show_col_types = FALSE)
sites_nee <- flux |> dplyr::filter(!is.na(nep_median)) |> dplyr::select(site_id, location_lat, location_long)
sites_et  <- flux |> dplyr::filter(!is.na(et_median))  |> dplyr::select(site_id, location_lat, location_long)

trendy_grid <- terra::rast(file.path(INTER, "CABLE-POP_nbp_regridded.tif"))

# ============================================================================
# T1 (raster part). Re-derive annual model values from raw monthly stacks
# ============================================================================

message("\n================ T1 (raster): raw-monthly-stack re-derivation ================")

set.seed(42)
sample20_nee <- sites_nee |> dplyr::slice_sample(n = 20)
sample20_et  <- sites_et  |> dplyr::slice_sample(n = 20)

# CRITICAL: figure_representativeness_trendy_compute.R:118-122 explicitly
# documents that terra::time() MISPARSES this file's CF time origin --
# CABLE-POP's true start year (its own MODEL_START_YR entry,
# figure_representativeness_trendy_compute.R:92) is 1700, but terra::time()
# reports layer 1 as fractional year 1970.000 (a systematic +270-year
# offset from a pre-1678 CF-origin parsing bug). The pipeline deliberately
# bypasses terra::time() and computes years from the verified start year +
# layer position instead (its own get_years() helper). A first attempt at
# this re-derivation trusted terra::time() naively and picked entirely the
# wrong 12*270=3240-layer-shifted block -- confirmed by a global correlation
# of ~0.07 against the stored intermediate (vs. ~0.98 for evapotrans on the
# identical code path). This reproduces get_years()'s logic exactly instead.
CABLE_POP_START_YR <- 1700L  # from figure_representativeness_trendy_compute.R:92
reprocess_model_annual <- function(nc_path, years, secs_month, kg_to_g_factor, start_yr) {
  r <- terra::rast(nc_path)
  n <- terra::nlyr(r)
  n_full_years <- ceiling(n / 12L)
  yr_of_layer <- rep(seq(start_yr, by = 1L, length.out = n_full_years), each = 12L)[seq_len(n)]
  out <- vector("list", length(years))
  for (i in seq_along(years)) {
    idx <- which(yr_of_layer == years[i])
    if (length(idx) != 12L) { out[[i]] <- NULL; next }
    out[[i]] <- sum(r[[idx]]) * secs_month * kg_to_g_factor
  }
  keep <- !vapply(out, is.null, logical(1))
  s <- terra::rast(out[keep])
  names(s) <- as.character(years[keep])
  s
}

message("Reprocessing CABLE-POP nbp from raw monthly NetCDF (native 1deg, 1990-2023)...")
cable_nbp_raw <- reprocess_model_annual(
  file.path(EXT, "trendy", "v14-gcb2025", "CABLE-POP", "S3", "CABLE-POP_S3_nbp.nc"),
  YEARS, SECS_MONTH, KG_TO_G, CABLE_POP_START_YR
)
message("Reprocessing CABLE-POP evapotrans from raw monthly NetCDF...")
cable_et_raw <- reprocess_model_annual(
  file.path(EXT, "trendy", "v14-gcb2025", "CABLE-POP", "S3", "CABLE-POP_S3_evapotrans.nc"),
  YEARS, SECS_MONTH, 1, CABLE_POP_START_YR  # no KG_TO_G for evapotrans -- already a depth-equivalent rate
)

# Regrid exactly as the pipeline does (bilinear, to the 0.5deg target grid)
cable_nbp_regridded_here <- terra::resample(cable_nbp_raw, trendy_grid, method = "bilinear")
cable_et_regridded_here  <- terra::resample(cable_et_raw,  trendy_grid, method = "bilinear")

cable_nbp_stored <- terra::rast(file.path(INTER, "CABLE-POP_nbp_regridded.tif"))
cable_et_stored  <- terra::rast(file.path(INTER, "CABLE-POP_evapotrans_regridded.tif"))

compare_layers <- function(mine, stored, years) {
  common_yr <- intersect(names(mine), names(stored))
  d <- lapply(common_yr, function(y) {
    a <- terra::values(mine[[y]]); b <- terra::values(stored[[y]])
    ok <- !is.na(a) & !is.na(b)
    data.frame(year = y, n_cells = sum(ok),
               max_abs_diff = max(abs(a[ok] - b[ok])),
               cor = suppressWarnings(cor(a[ok], b[ok])))
  })
  dplyr::bind_rows(d)
}

t1_global_check_nbp <- compare_layers(cable_nbp_regridded_here, cable_nbp_stored, YEARS)
t1_global_check_et  <- compare_layers(cable_et_regridded_here,  cable_et_stored,  YEARS)

cat("\n-- Global reproduction check, CABLE-POP nbp, my-reprocessing vs. stored intermediate TIF, per year --\n")
print(t1_global_check_nbp)
cat("\n-- Global reproduction check, CABLE-POP evapotrans --\n")
print(t1_global_check_et)

extract_at_sites <- function(rast_stack, sites_df, var_label) {
  vals_mine   <- terra::extract(rast_stack, cbind(sites_df$location_long, sites_df$location_lat))
  vals_mine   <- vals_mine[, as.character(YEARS[YEARS %in% as.integer(names(rast_stack))]), drop = FALSE]
  data.frame(site_id = sites_df$site_id, var = var_label,
              mean_annual_value_mine = rowMeans(vals_mine, na.rm = TRUE))
}

t1_site20_nbp_mine   <- extract_at_sites(cable_nbp_regridded_here, sample20_nee, "nbp")
t1_site20_nbp_stored <- terra::extract(cable_nbp_stored, cbind(sample20_nee$location_long, sample20_nee$location_lat))
t1_site20_nbp_stored <- data.frame(site_id = sample20_nee$site_id,
                                    mean_annual_value_stored = rowMeans(t1_site20_nbp_stored[, as.character(YEARS), drop=FALSE], na.rm = TRUE))

t1_site20_et_mine   <- extract_at_sites(cable_et_regridded_here, sample20_et, "evapotrans")
t1_site20_et_stored <- terra::extract(cable_et_stored, cbind(sample20_et$location_long, sample20_et$location_lat))
t1_site20_et_stored <- data.frame(site_id = sample20_et$site_id,
                                   mean_annual_value_stored = rowMeans(t1_site20_et_stored[, as.character(YEARS), drop=FALSE], na.rm = TRUE))

t1_site20 <- dplyr::bind_rows(
  dplyr::left_join(t1_site20_nbp_mine, t1_site20_nbp_stored, by = "site_id"),
  dplyr::left_join(t1_site20_et_mine,  t1_site20_et_stored,  by = "site_id")
) |> dplyr::mutate(abs_diff = abs(mean_annual_value_mine - mean_annual_value_stored),
                    rel_diff_pct = 100 * abs_diff / pmax(abs(mean_annual_value_stored), 1e-9))

cat("\n-- 20-site-pixel re-derivation check (CABLE-POP, single-model, native-1deg-regridded-vs-stored) --\n")
print(as.data.frame(t1_site20))
cat("\nMax relative difference across 20-pixel sample: ", round(max(t1_site20$rel_diff_pct), 3), "%\n")

out_t1c <- file.path(OUTD, "table_t1_raw_stack_check.csv")
readr::write_csv(t1_site20, out_t1c)
write_output_metadata(out_t1c, input_sources = c(
  file.path(EXT, "trendy", "v14-gcb2025", "CABLE-POP", "S3", "CABLE-POP_S3_nbp.nc"),
  file.path(EXT, "trendy", "v14-gcb2025", "CABLE-POP", "S3", "CABLE-POP_S3_evapotrans.nc"),
  file.path(INTER, "CABLE-POP_nbp_regridded.tif"), file.path(INTER, "CABLE-POP_evapotrans_regridded.tif")
), notes = "T1 raw-monthly-stack re-derivation for CABLE-POP (representative monthly-native model), full 1990-2023 global reproduction (see console log for per-year max-abs-diff/correlation) plus this 20-site-pixel sample table. Method: sum(monthly_rate * SECS_MONTH [* KG_TO_G for nbp]) per year at native 1deg, then terra::resample(method='bilinear') to the 0.5deg target grid, identically to figure_representativeness_trendy_compute.R's own pipeline, then compared against the stored intermediate GeoTIFF.")
message("Saved: ", out_t1c)

# ============================================================================
# T3. SCALE AND CANCELLATION
# ============================================================================

message("\n================ T3: scale and cancellation ================")

# ---- 3a. Load per-model 34-layer annual intermediates, final-ensemble models only ----
load_stack <- function(model, var) terra::rast(file.path(INTER, paste0(model, "_", var, "_regridded.tif")))

nbp_stacks <- lapply(MODELS_FINAL, load_stack, var = "nbp")
et_stacks  <- lapply(MODELS_FINAL, load_stack, var = "evapotrans")
names(nbp_stacks) <- names(et_stacks) <- MODELS_FINAL

# ---- 3b. Per-pixel temporal-statistic-then-ensemble-median, at a given spatial aggregation factor ----
recompute_ensemble_at_res <- function(stacks, agg_fact, stat_fun) {
  per_model <- lapply(stacks, function(s) {
    s_agg <- if (agg_fact > 1) terra::aggregate(s, fact = agg_fact, fun = "mean", na.rm = TRUE) else s
    terra::app(s_agg, stat_fun)
  })
  st <- terra::rast(per_model)
  terra::app(st, function(v) median(v, na.rm = TRUE))
}

area_weighted_mean <- function(r, exclude_nonfinite = TRUE) {
  w <- terra::cellSize(r, unit = "km", mask = TRUE)
  vals <- terra::values(r); wts <- terra::values(w)
  ok <- !is.na(vals) & !is.na(wts)
  if (exclude_nonfinite) ok <- ok & is.finite(vals)
  n_excluded <- sum(!is.na(vals) & !is.na(wts) & !is.finite(vals))
  if (n_excluded > 0L) message("  (area_weighted_mean: excluded ", n_excluded,
                                " non-finite cells, e.g. division-by-near-zero in a ratio raster)")
  sum(vals[ok] * wts[ok]) / sum(wts[ok])
}

stat_abs_mean <- function(v) mean(abs(v), na.rm = TRUE)
stat_mean     <- function(v) mean(v, na.rm = TRUE)

AGG_FACTORS <- c(`0.5deg` = 1, `1deg` = 2, `2deg` = 4, `4deg` = 8)
message("Recomputing ensemble at 0.5/1/2/4 degree (aggregate annual fields THEN recompute per-model stat THEN median-across-models)...")

t3_resolution <- dplyr::bind_rows(lapply(names(AGG_FACTORS), function(nm) {
  fact <- AGG_FACTORS[[nm]]
  r_nbp <- recompute_ensemble_at_res(nbp_stacks, fact, stat_abs_mean)
  r_et  <- recompute_ensemble_at_res(et_stacks,  fact, stat_mean)
  data.frame(
    resolution = nm,
    global_mean_abs_nbp = area_weighted_mean(r_nbp),
    global_mean_et = area_weighted_mean(r_et)
  )
}))
cat("\n-- Global area-weighted mean vs. spatial aggregation resolution --\n")
print(t3_resolution)

# ---- 3c. Per-pixel mean(|annual|) vs |mean(annual)|, native 0.5deg, ensemble-median across models ----
message("Computing mean(|annual|) vs |mean(annual)| at native 0.5deg (ensemble-median-of-per-model-value)...")

ensemble_median_stack <- function(stacks) {
  # per-year ensemble median across models (needed for |mean(annual)| variant and T3's order-of-ensemble test)
  yrs <- Reduce(intersect, lapply(stacks, names))
  layers <- lapply(yrs, function(y) {
    yr_layers <- terra::rast(lapply(stacks, function(s) s[[y]]))
    terra::app(yr_layers, function(v) median(v, na.rm = TRUE))
  })
  s <- terra::rast(layers); names(s) <- yrs
  s
}

nbp_ens_annual <- ensemble_median_stack(nbp_stacks)  # per-year ensemble-median annual field
r_mean_abs_annual   <- recompute_ensemble_at_res(nbp_stacks, 1, stat_abs_mean)   # mean(|annual|), model-then-ensemble order (= stored method)
r_abs_mean_annual    <- abs(terra::app(nbp_ens_annual, "mean"))                   # |mean(annual)|, using ensemble-median annual field

r_cancellation_ratio <- r_mean_abs_annual / r_abs_mean_annual

t3_cancellation_global <- area_weighted_mean(r_cancellation_ratio)
t3_cancellation_global_median <- median(terra::values(r_cancellation_ratio)[is.finite(terra::values(r_cancellation_ratio))], na.rm = TRUE)
message("  (area-weighted mean is sensitive to near-zero-|mean(annual)| denominators even after excluding exact Inf; ",
        "unweighted land-cell median reported alongside as a robust alternative: ", round(t3_cancellation_global_median, 3), ")")

site_cells_nee <- terra::extract(r_cancellation_ratio, cbind(sites_nee$location_long, sites_nee$location_lat))[[1]]
t3_cancellation_sites <- data.frame(site_id = sites_nee$site_id, cancellation_ratio = site_cells_nee)

cat("\n-- mean(|annual|) / |mean(annual)| ratio (temporal cancellation), nbp -- global area-weighted:",
    round(t3_cancellation_global, 3), "--\n")
cat("At NEE site pixels: median =", round(median(t3_cancellation_sites$cancellation_ratio, na.rm=TRUE), 3),
    " IQR = [", round(quantile(t3_cancellation_sites$cancellation_ratio, .25, na.rm=TRUE), 3), ",",
    round(quantile(t3_cancellation_sites$cancellation_ratio, .75, na.rm=TRUE), 3), "]\n")

# ---- 3d. Order of ensemble step: median-across-models-of-mean|annual| vs mean|annual|-of-median-across-models ----
r_modelfirst <- r_mean_abs_annual  # = stored trendy_nee_median.tif method
r_ensfirst   <- terra::app(nbp_ens_annual, stat_abs_mean)  # mean|annual| computed on the already-ensemble-medianed annual field

t3_order <- data.frame(
  method = c("model-stat-first, then median-across-models (STORED METHOD)",
             "ensemble-median-across-models first, then mean|annual|"),
  global_area_weighted_mean = c(area_weighted_mean(r_modelfirst), area_weighted_mean(r_ensfirst))
)
cat("\n-- Order-of-ensemble-step comparison (nbp) --\n")
print(t3_order)

# Repeat cancellation + order tests for ET (no abs, so cancellation ratio should be ~1 -- control)
et_ens_annual <- ensemble_median_stack(et_stacks)
r_mean_annual_et      <- recompute_ensemble_at_res(et_stacks, 1, stat_mean)  # model-stat-first (= stored method)
r_ens_then_mean_et     <- terra::app(et_ens_annual, "mean")                  # ensemble-median-first
r_cancellation_ratio_et <- r_mean_annual_et / r_ens_then_mean_et
t3_cancellation_global_et <- area_weighted_mean(r_cancellation_ratio_et)
site_cells_et <- terra::extract(r_cancellation_ratio_et, cbind(sites_et$location_long, sites_et$location_lat))[[1]]
t3_cancellation_sites_et <- data.frame(site_id = sites_et$site_id, cancellation_ratio = site_cells_et)

t3_order_et <- data.frame(
  method = c("model-stat-first, then median-across-models (STORED METHOD)",
             "ensemble-median-across-models first, then mean"),
  global_area_weighted_mean = c(area_weighted_mean(r_mean_annual_et), area_weighted_mean(r_ens_then_mean_et))
)
cat("\n-- Order-of-ensemble-step comparison (evapotrans, control) --\n")
print(t3_order_et)

cat("\n-- ET (control): mean(annual)/mean(annual)-via-ensemble-first ratio -- global:",
    round(t3_cancellation_global_et, 3), "--\n")

out_t3a <- file.path(OUTD, "table_t3_resolution_aggregation.csv")
out_t3b <- file.path(OUTD, "table_t3_cancellation_and_order.csv")
readr::write_csv(t3_resolution, out_t3a)
readr::write_csv(dplyr::bind_rows(
  data.frame(axis = "nee", test = "cancellation_global_areaweighted_mean", value = t3_cancellation_global),
  data.frame(axis = "nee", test = "cancellation_global_unweighted_median", value = t3_cancellation_global_median),
  data.frame(axis = "nee", test = "order_modelfirst", value = t3_order$global_area_weighted_mean[1]),
  data.frame(axis = "nee", test = "order_ensfirst", value = t3_order$global_area_weighted_mean[2]),
  data.frame(axis = "et", test = "cancellation_global", value = t3_cancellation_global_et),
  data.frame(axis = "et", test = "order_modelfirst", value = t3_order_et$global_area_weighted_mean[1]),
  data.frame(axis = "et", test = "order_ensfirst", value = t3_order_et$global_area_weighted_mean[2])
), out_t3b)
out_t3c <- file.path(OUTD, "table_t3_cancellation_at_nee_sites.csv")
out_t3d <- file.path(OUTD, "table_t3_cancellation_at_et_sites.csv")
readr::write_csv(t3_cancellation_sites, out_t3c)
readr::write_csv(t3_cancellation_sites_et, out_t3d)
write_output_metadata(out_t3c, input_sources = INTER, notes = "Per-NEE-site mean(|annual|)/|mean(annual)| temporal-cancellation ratio (nbp), native 0.5deg, at each site's TRENDY grid-cell pixel.")
write_output_metadata(out_t3d, input_sources = INTER, notes = "Per-ET-site mean(annual)/mean(annual)-via-ensemble-first ratio (evapotrans, control), native 0.5deg, at each site's TRENDY grid-cell pixel.")
write_output_metadata(out_t3a, input_sources = c(INTER), notes = "Global area-weighted mean |NBP| (nee) and mean ET, recomputed with annual fields spatially aggregated to 0.5/1/2/4 degree BEFORE the per-model temporal statistic and ensemble-median-across-models steps, using the 16 final-ensemble models' intermediate annual stacks.")
write_output_metadata(out_t3b, input_sources = c(INTER), notes = "Temporal cancellation ratio (mean|annual| / |mean(annual)|) and ensemble-order-of-operations comparison, nbp and evapotrans, at native 0.5deg.")
message("Saved: ", out_t3a, ", ", out_t3b, ", and per-site cancellation tables")

# ============================================================================
# T4. TEMPORAL WINDOW
# ============================================================================

message("\n================ T4: temporal window (each site's own measured years) ================")

# Approximate "measured years" per site as calendar years where the DuckDB
# annual table's NEE_VUT_REF_QC or NEE_CUT_REF_QC >= 0.80 (QC threshold
# consistent with scripts/assess_flux_data_by_igbp_shuttle.R -- this is an
# approximation of its exact per-site VUT/CUT fallback decision, not a
# byte-identical reproduction; stated explicitly, not hidden).
con <- dbConnect(duckdb::duckdb(), dbdir = "data/duckdb/fluxnet.duckdb", read_only = TRUE)
measured_years_df <- dbGetQuery(con, "
  SELECT site_id, TIMESTAMP AS yr
  FROM annual
  WHERE (NEE_VUT_REF_QC >= 0.80 OR NEE_CUT_REF_QC >= 0.80)
")
dbDisconnect(con, shutdown = TRUE)
measured_years_df <- measured_years_df |> dplyr::filter(yr %in% YEARS)
message("QC-qualifying site-years (QC>=0.80, VUT-or-CUT, within 1990-2023): ", nrow(measured_years_df))

years_by_site <- split(measured_years_df$yr, measured_years_df$site_id)

recompute_site_stat <- function(site_id, lon, lat, stacks, stat_fun, years_avail) {
  yrs <- as.character(years_avail)
  if (length(yrs) < 3L) return(NA_real_)  # too few years to be meaningful
  vals_by_model <- vapply(stacks, function(s) {
    yrs_present <- intersect(yrs, names(s))
    if (length(yrs_present) < 1L) return(NA_real_)
    v <- terra::extract(s[[yrs_present]], cbind(lon, lat))
    stat_fun(as.numeric(v[1, ]))
  }, numeric(1L))
  median(vals_by_model, na.rm = TRUE)
}

t4_nee_targets <- sites_nee |> dplyr::filter(site_id %in% names(years_by_site))
message("Sites with >=3 QC-qualifying years in 1990-2023 window, NEE: ", nrow(t4_nee_targets))

t4_nee <- t4_nee_targets |> dplyr::rowwise() |> dplyr::mutate(
  n_measured_years = length(years_by_site[[site_id]]),
  stat_measured_years = recompute_site_stat(site_id, location_long, location_lat, nbp_stacks, stat_abs_mean, years_by_site[[site_id]])
) |> dplyr::ungroup() |>
  dplyr::left_join(dplyr::select(paired_all |> dplyr::filter(axis == "nee"), site_id, trendy_abs), by = "site_id") |>
  dplyr::rename(stat_full_34yr = trendy_abs) |>
  dplyr::mutate(paired_diff = stat_measured_years - stat_full_34yr)

t4_et_targets <- sites_et |> dplyr::filter(site_id %in% names(years_by_site))
message("Sites with >=3 QC-qualifying years in 1990-2023 window, ET: ", nrow(t4_et_targets))

t4_et <- t4_et_targets |> dplyr::rowwise() |> dplyr::mutate(
  n_measured_years = length(years_by_site[[site_id]]),
  stat_measured_years = recompute_site_stat(site_id, location_long, location_lat, et_stacks, stat_mean, years_by_site[[site_id]])
) |> dplyr::ungroup() |>
  dplyr::left_join(dplyr::select(paired_all |> dplyr::filter(axis == "et"), site_id, trendy_abs), by = "site_id") |>
  dplyr::rename(stat_full_34yr = trendy_abs) |>
  dplyr::mutate(paired_diff = stat_measured_years - stat_full_34yr)

cat("\n-- T4 paired difference (measured-years-window TRENDY stat minus stored full-34-yr stat) --\n")
cat("NEE (n=", nrow(t4_nee), "): median diff =", round(median(t4_nee$paired_diff, na.rm=TRUE), 3),
    " IQR=[", round(quantile(t4_nee$paired_diff, .25, na.rm=TRUE),3), ",", round(quantile(t4_nee$paired_diff, .75, na.rm=TRUE),3), "]\n")
cat("ET  (n=", nrow(t4_et), "): median diff =", round(median(t4_et$paired_diff, na.rm=TRUE), 3),
    " IQR=[", round(quantile(t4_et$paired_diff, .25, na.rm=TRUE),3), ",", round(quantile(t4_et$paired_diff, .75, na.rm=TRUE),3), "]\n")

out_t4 <- file.path(OUTD, "table_t4_temporal_window.csv")
readr::write_csv(dplyr::bind_rows(dplyr::mutate(t4_nee, axis="nee", .before=1), dplyr::mutate(t4_et, axis="et", .before=1)), out_t4)
write_output_metadata(out_t4, input_sources = c("data/duckdb/fluxnet.duckdb (read-only)", INTER, pairs_path),
  notes = "T4: TRENDY statistic recomputed at each site's own QC-qualifying measured years (QC>=0.80 approximation of assess_flux_data_by_igbp_shuttle.R's threshold, not its exact per-site VUT/CUT decision) vs. the stored full-1990-2023 value, paired difference = measured-years stat minus full-34yr stat.")
message("Saved: ", out_t4)

message("\n=== nee_et_site_vs_trendy_raster.R complete ===")
