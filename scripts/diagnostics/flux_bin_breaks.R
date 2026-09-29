## flux_bin_breaks.R
##
## Diagnostic: 7-bin break schemes for NEE, GPP, TER and ET, where the break
## edges are the septiles of an equal-weight mixture of the area-weighted
## global ("Geo") distribution and the empirical tower ("Data") distribution.
## Does NOT touch any production script or review/figures/candidates/ output
## -- outputs land entirely in review/diagnostics/flux_bin_breaks/.
##
## Geo: S3 ensemble-median 1991-2020 rasters, all already cached -- no
##   netCDF re-processing:
##     NEE = ra+rh-gpp : data/external/trendy/derived/trendy_nee_fluxbased_median.tif
##     GPP              : data/external/trendy/derived/candidate_gpp_median.tif
##     TER = ra+rh      : data/external/trendy/derived/candidate_ter_median.tif
##     ET               : built here from the per-model
##       data/external/trendy/derived/intermediate/<model>_evapotrans_regridded.tif
##       caches (1990-2023, 34 layers), subset to 1991-2020 -- no such
##       1991-2020-window ET raster exists yet (the committed
##       trendy_et_median.tif is a 1990-2023/16-model product, a different
##       window and ensemble than this diagnostic's other three fluxes need).
##
## Towers: current network (781 sites), post-2026-09-28-unit-fix annual
##   values from monthly_converted. NEE/GPP/TER use the per-site VUT->CUT
##   fallback (NEE_VUT_REF_QC / NEE_CUT_REF_QC presence); GPP/TER also use an
##   independent per-site NT->DT fallback -- exactly the pattern in
##   scripts/candidate_nee_gpp_ter_panels.R. ET is LE_F_MDS gated on its own
##   LE_F_MDS_QC, independent of VUT/CUT, as in
##   scripts/diagnostics/flux_tower_model_distributions.R.
##
## Unvegetated mask: cells where model GPP < 50 gC m-2 yr-1 are dropped from
## the Geo side for ALL FOUR fluxes (same mask everywhere, so the four
## fluxes' vegetated-land denominators are directly comparable). Land
## fraction removed is also reported at 12 and 100 for sensitivity.
##
## Break rule: 6 edges (7 bins) at the septiles of
##   F(x) = 0.5 * F_geo(x) + 0.5 * F_tower(x)
## where F_geo is the area-weighted CDF over vegetated land and F_tower is
## the empirical CDF over towers with a qualifying annual value. Two edge
## versions per flux: exact septiles, and a rounded set (NEE to the nearest
## 25 gC m-2 yr-1, GPP/TER to the nearest 100, ET to the nearest 50 mm yr-1);
## the outer two bins are always open-ended (-Inf / +Inf) regardless of
## rounding. The SAME edge set (per flux x version) is used for both
## comparisons below.
##
## Comparisons (2 per flux x edge version = 16 plots total):
##   "data"        : Geo (global land fraction) vs tower-measured value,
##                   classified into the same edges ("Geo vs Data").
##   "geo_at_tower": Geo (global land fraction) vs the SAME Geo raster
##                   extracted at tower coordinates, classified into the
##                   same edges ("Geo vs Geo").
## Site fractions use the CLASSIFIED sites as the denominator (fractions sum
## to 1 across the 7 bins), not the full 781-site network -- different from
## the production NEE axis's convention, per this task's explicit instruction.

suppressPackageStartupMessages({
  library(terra)
  library(dplyr)
  library(readr)
  library(duckdb)
  library(lubridate)
  library(ggplot2)
  library(fs)
})

source("R/pipeline_config.R")
check_pipeline_config()

SNAP_DIR    <- "data/snapshots"
DERIVED_DIR <- "data/external/trendy/derived"
INTER_DIR   <- file.path(DERIVED_DIR, "intermediate")
KG_PATH     <- "data/external/koppen_beck2023/1991_2020/koppen_geiger_0p5.tif"
OUT_DIR     <- "review/diagnostics/flux_bin_breaks"
fs::dir_create(OUT_DIR)
SITE_CSV    <- file.path(SNAP_DIR, "site_biomass_cci_v7.csv")
QC_THRESH_MM <- 0.80
WIN_START <- 1991L; WIN_END <- 2020L
GPP_VEG_THRESHOLD <- 50   # gC m-2 yr-1, the mask actually applied
GPP_VEG_SENSITIVITY <- c(12, 50, 100)

MODELS_TARGET <- c("CABLE-POP", "CLASSIC", "CLM", "DLEM", "ED", "ELM", "ELM-FATES",
                    "IBIS", "ISAM", "JULES-ES", "LPJ-GUESS", "LPJml", "LPJwsl",
                    "LPX-Bern", "ORCHIDEE", "TEM", "VISIT-UT")  # 17 models

LOG_START <- format(Sys.time(), "%Y%m%d_%H%M%S")
LOG_FILE  <- file.path("logs", paste0("flux_bin_breaks_", LOG_START, ".log"))
con_log <- file(LOG_FILE, open = "wt")
sink(con_log, type = "output")
sink(con_log, type = "message", append = TRUE)
on.exit({ sink(type = "message"); sink(type = "output"); close(con_log) }, add = TRUE)
msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)

write_meta <- function(output_path, input_sources, notes = "") {
  meta <- list(
    run_datetime_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    pipeline_version = system("git rev-parse --short HEAD", intern = TRUE),
    input_sources    = as.list(input_sources),
    notes            = notes
  )
  meta_path <- paste0(tools::file_path_sans_ext(output_path), ".meta.json")
  writeLines(jsonlite::toJSON(meta, pretty = TRUE, auto_unbox = TRUE), meta_path)
  invisible(meta_path)
}

msg("=== flux_bin_breaks.R ===")
msg("Log: ", LOG_FILE)

current_sites <- read_csv(SITE_CSV, show_col_types = FALSE) |>
  select(site_id, location_lat, location_long) |> distinct(site_id, .keep_all = TRUE)
n_sites <- nrow(current_sites)
geo_coords <- as.matrix(current_sites[, c("location_long", "location_lat")])

msg("Loading KG land mask (0.5 deg) ...")
kg_05 <- rast(KG_PATH)
cell_areas_05 <- cellSize(kg_05, mask = TRUE, unit = "km")
total_land_km2 <- sum(values(cell_areas_05), na.rm = TRUE)

# ============================================================================
# STEP 1: Load / build the four Geo rasters (1991-2020 ensemble median)
# ============================================================================
msg("\n=== STEP 1: Geo rasters ===")

r_nee <- rast(file.path(DERIVED_DIR, "trendy_nee_fluxbased_median.tif"))
r_gpp <- rast(file.path(DERIVED_DIR, "candidate_gpp_median.tif"))
r_ter <- rast(file.path(DERIVED_DIR, "candidate_ter_median.tif"))
msg("Loaded cached NEE/GPP/TER ensemble-median rasters (no recomputation).")

et_cache <- file.path(DERIVED_DIR, "flux_bin_breaks_et_median_1991_2020.tif")
if (file.exists(et_cache)) {
  r_et <- rast(et_cache)
  msg("Loaded cached ET 1991-2020 ensemble-median raster: ", et_cache)
} else {
  msg("No 1991-2020 ET ensemble raster cached -- building from per-model evapotrans_regridded caches ...")
  complete_mean <- function(r) {
    vals <- values(r)
    complete <- rowSums(is.na(vals)) == 0L
    out <- rep(NA_real_, nrow(vals))
    if (any(complete)) out[complete] <- rowMeans(vals[complete, , drop = FALSE])
    r1 <- r[[1L]]; values(r1) <- out; names(r1) <- "mean"
    r1
  }
  et_layers <- list()
  for (mdl in MODELS_TARGET) {
    cache <- file.path(INTER_DIR, paste0(mdl, "_evapotrans_regridded.tif"))
    if (!file.exists(cache)) { msg("  ", mdl, ": no evapotrans_regridded cache -- skipping"); next }
    r_full <- rast(cache)
    yr_idx <- which(as.integer(names(r_full)) %in% WIN_START:WIN_END)
    if (length(yr_idx) != (WIN_END - WIN_START + 1L)) {
      msg("  ", mdl, ": only ", length(yr_idx), " of ", WIN_END - WIN_START + 1L,
          " 1991-2020 layers present -- skipping")
      next
    }
    et_layers[[mdl]] <- mask(complete_mean(r_full[[yr_idx]]), kg_05)
  }
  msg("Models with usable 1991-2020 ET (", length(et_layers), "/", length(MODELS_TARGET), "): ",
      paste(names(et_layers), collapse = ", "))
  et_stack <- rast(et_layers); names(et_stack) <- names(et_layers)
  r_et <- app(et_stack, fun = function(v) median(v, na.rm = TRUE))
  writeRaster(r_et, et_cache, gdal = "COMPRESS=DEFLATE", overwrite = TRUE)
  write_meta(et_cache, input_sources = file.path(INTER_DIR, paste0(names(et_layers), "_evapotrans_regridded.tif")),
             notes = paste0("Ensemble-median ET (evapotrans), 1991-2020 mean, ", length(et_layers),
                             " models. Built for flux_bin_breaks.R specifically -- distinct from the ",
                             "committed trendy_et_median.tif (1990-2023 window, 16 models)."))
  msg("Saved: ", et_cache)
}

GEO_RASTERS <- list(NEE = r_nee, GPP = r_gpp, TER = r_ter, ET = r_et)

# ============================================================================
# STEP 2: Unvegetated mask (model GPP < 50 gC m-2 yr-1)
# ============================================================================
msg("\n=== STEP 2: Unvegetated mask (from model GPP) ===")

r_gpp_land <- mask(r_gpp, kg_05)
sensitivity_rows <- lapply(GPP_VEG_SENSITIVITY, function(thr) {
  below <- ifel(r_gpp_land < thr, cell_areas_05, NA)
  area_removed <- sum(values(below), na.rm = TRUE)
  data.frame(gpp_threshold = thr, area_removed_km2 = area_removed,
             land_fraction_removed = area_removed / total_land_km2)
})
sensitivity_df <- bind_rows(sensitivity_rows)
msg("Land fraction removed at GPP thresholds 12/50/100 gC m-2 yr-1:")
print(sensitivity_df)
write_csv(sensitivity_df, file.path(OUT_DIR, "table_unvegetated_mask_sensitivity.csv"))
write_meta(file.path(OUT_DIR, "table_unvegetated_mask_sensitivity.csv"),
           input_sources = file.path(DERIVED_DIR, "candidate_gpp_median.tif"),
           notes = "Land fraction removed from the KG land mask total at each candidate GPP threshold. 50 is the threshold actually used for masking below.")

veg_mask <- ifel(r_gpp_land >= GPP_VEG_THRESHOLD, 1, NA)
veg_land_km2 <- sum(values(mask(cell_areas_05, veg_mask)), na.rm = TRUE)
msg("Vegetated land (GPP >= ", GPP_VEG_THRESHOLD, "): ", format(round(veg_land_km2), big.mark = ","),
    " km2 (", round(100 * veg_land_km2 / total_land_km2, 1), "% of total land)")

## Towers whose model GPP falls in a masked (unvegetated) cell
gpp_at_towers <- terra::extract(r_gpp, geo_coords, method = "bilinear")[, 1]
masked_towers <- current_sites |> mutate(model_gpp = gpp_at_towers) |>
  filter(!is.na(model_gpp), model_gpp < GPP_VEG_THRESHOLD)
msg(nrow(masked_towers), " current-network towers fall in a masked (model GPP < ", GPP_VEG_THRESHOLD, ") cell:")
if (nrow(masked_towers) > 0) print(as.data.frame(masked_towers))
write_csv(masked_towers, file.path(OUT_DIR, "table_towers_in_masked_cells.csv"))
write_meta(file.path(OUT_DIR, "table_towers_in_masked_cells.csv"),
           input_sources = file.path(DERIVED_DIR, "candidate_gpp_median.tif"),
           notes = paste0("Current-network sites whose model GPP (bilinear-extracted at site coordinates) ",
                           "is below the ", GPP_VEG_THRESHOLD, " gC m-2 yr-1 vegetated-land threshold. ",
                           "Listed for reporting only -- NOT excluded from any tower-side computation below."))

# ============================================================================
# STEP 3: Tower annual values (current network, post-unit-fix)
# ============================================================================
msg("\n=== STEP 3: Tower annual values ===")

site_ids_sql <- paste(sprintf("'%s'", current_sites$site_id), collapse = ", ")
con <- dbConnect(duckdb::duckdb(), "data/duckdb/fluxnet.duckdb", read_only = TRUE)
monthly_raw <- dbGetQuery(con, sprintf("
  SELECT site_id, TIMESTAMP,
         NEE_VUT_REF, NEE_VUT_REF_QC, NEE_CUT_REF, NEE_CUT_REF_QC,
         GPP_NT_VUT_REF, GPP_DT_VUT_REF, GPP_NT_CUT_REF, GPP_DT_CUT_REF,
         RECO_NT_VUT_REF, RECO_DT_VUT_REF, RECO_NT_CUT_REF, RECO_DT_CUT_REF,
         LE_F_MDS, LE_F_MDS_QC
  FROM monthly_converted
  WHERE dataset = 'FLUXMET' AND site_id IN (%s)
", site_ids_sql))
dbDisconnect(con, shutdown = TRUE)

monthly_raw <- monthly_raw |>
  mutate(TIMESTAMP = as.Date(TIMESTAMP), year = year(TIMESTAMP), month = month(TIMESTAMP))

## ---- Per-site VUT/CUT decision (NEE_*_QC presence), shared by NEE/GPP/TER --
site_carbon_src <- monthly_raw |>
  group_by(site_id) |>
  summarise(any_vut_qc = any(!is.na(NEE_VUT_REF_QC)), any_cut_qc = any(!is.na(NEE_CUT_REF_QC)), .groups = "drop") |>
  mutate(carbon_src = case_when(any_vut_qc ~ "VUT", any_cut_qc ~ "CUT", TRUE ~ NA_character_))
msg("Per-site VUT/CUT choice: VUT=", sum(site_carbon_src$carbon_src == "VUT", na.rm = TRUE),
    "  CUT (fallback)=", sum(site_carbon_src$carbon_src == "CUT", na.rm = TRUE),
    "  neither=", sum(is.na(site_carbon_src$carbon_src)))

monthly_raw <- monthly_raw |> left_join(site_carbon_src |> select(site_id, carbon_src), by = "site_id") |>
  mutate(
    nee_val = if_else(carbon_src == "VUT", NEE_VUT_REF, NEE_CUT_REF),
    nee_qc  = if_else(carbon_src == "VUT", NEE_VUT_REF_QC, NEE_CUT_REF_QC),
    nee_qualifies = !is.na(carbon_src) & !is.na(nee_qc) & nee_qc >= QC_THRESH_MM & !is.na(nee_val),
    nee_gC = if_else(nee_qualifies, nee_val, NA_real_),
    gpp_nt = if_else(carbon_src == "VUT", GPP_NT_VUT_REF, GPP_NT_CUT_REF),
    gpp_dt = if_else(carbon_src == "VUT", GPP_DT_VUT_REF, GPP_DT_CUT_REF),
    ter_nt = if_else(carbon_src == "VUT", RECO_NT_VUT_REF, RECO_NT_CUT_REF),
    ter_dt = if_else(carbon_src == "VUT", RECO_DT_VUT_REF, RECO_DT_CUT_REF),
    et_qualifies = !is.na(LE_F_MDS) & !is.na(LE_F_MDS_QC) & LE_F_MDS_QC >= QC_THRESH_MM,
    et_mm = if_else(et_qualifies, LE_F_MDS, NA_real_)
  )
## monthly_converted's carbon columns are already gC m-2 month-1 totals
## (05_units.R's daily-rate x days-in-month conversion, 2026-09-28) -- used
## directly. LE_F_MDS is already mm H2O per month (unconditional conversion,
## no is_coarse guard) -- also used directly.

## ---- Per-site independent NT->DT fallback for GPP and TER ------------------
site_partition <- monthly_raw |>
  group_by(site_id) |>
  summarise(n_gpp_nt = sum(!is.na(gpp_nt)), n_gpp_dt = sum(!is.na(gpp_dt)),
            n_ter_nt = sum(!is.na(ter_nt)), n_ter_dt = sum(!is.na(ter_dt)), .groups = "drop") |>
  mutate(gpp_partition = case_when(n_gpp_nt > 0L ~ "NT", n_gpp_dt > 0L ~ "DT", TRUE ~ NA_character_),
         ter_partition = case_when(n_ter_nt > 0L ~ "NT", n_ter_dt > 0L ~ "DT", TRUE ~ NA_character_)) |>
  select(site_id, gpp_partition, ter_partition)
msg("GPP partitioning: NT=", sum(site_partition$gpp_partition == "NT", na.rm = TRUE),
    "  DT (fallback)=", sum(site_partition$gpp_partition == "DT", na.rm = TRUE))
msg("TER partitioning: NT=", sum(site_partition$ter_partition == "NT", na.rm = TRUE),
    "  DT (fallback)=", sum(site_partition$ter_partition == "DT", na.rm = TRUE))

monthly_raw <- monthly_raw |> left_join(site_partition, by = "site_id") |>
  mutate(
    gpp_gC = case_when(gpp_partition == "NT" ~ gpp_nt, gpp_partition == "DT" ~ gpp_dt, TRUE ~ NA_real_),
    ter_gC = case_when(ter_partition == "NT" ~ ter_nt, ter_partition == "DT" ~ ter_dt, TRUE ~ NA_real_)
  )

## ---- Annual construction: mean monthly cycle (all qualifying years), summed
build_annual <- function(df, value_col) {
  cyc <- df |> filter(!is.na(.data[[value_col]])) |> group_by(site_id, month) |>
    summarise(mean_month = mean(.data[[value_col]], na.rm = TRUE), .groups = "drop")
  all12 <- cyc |> group_by(site_id) |> summarise(n_months = n(), .groups = "drop") |>
    filter(n_months == 12L) |> pull(site_id)
  cyc |> filter(site_id %in% all12) |> group_by(site_id) |>
    summarise(tower_value = sum(mean_month), .groups = "drop")
}

tower_nee <- build_annual(monthly_raw, "nee_gC") |> left_join(current_sites, by = "site_id")
tower_gpp <- build_annual(monthly_raw, "gpp_gC") |> left_join(current_sites, by = "site_id")
tower_ter <- build_annual(monthly_raw, "ter_gC") |> left_join(current_sites, by = "site_id")
tower_et  <- build_annual(monthly_raw, "et_mm")  |> left_join(current_sites, by = "site_id")
msg("Tower annual values -- NEE: ", nrow(tower_nee), "  GPP: ", nrow(tower_gpp),
    "  TER: ", nrow(tower_ter), "  ET: ", nrow(tower_et))

TOWER_VALUES <- list(NEE = tower_nee, GPP = tower_gpp, TER = tower_ter, ET = tower_et)

# ============================================================================
# STEP 4: Model at tower coordinates (all four fluxes, unmasked extraction)
# ============================================================================
msg("\n=== STEP 4: Model at tower cells ===")

geo_at_tower <- lapply(names(GEO_RASTERS), function(fl) {
  vals <- terra::extract(GEO_RASTERS[[fl]], geo_coords, method = "bilinear")[, 1]
  current_sites |> mutate(model_value = vals) |> filter(!is.na(model_value))
})
names(geo_at_tower) <- names(GEO_RASTERS)
for (fl in names(geo_at_tower)) msg("  ", fl, ": ", nrow(geo_at_tower[[fl]]), " sites with a model value")

# ============================================================================
# STEP 5: Septile break edges (mixture of Geo area-weighted CDF and tower CDF)
# ============================================================================
msg("\n=== STEP 5: Septile break edges ===")

## Fine-step area histogram over VEGETATED land only, reused pattern from
## figure_representativeness_trendy_compute.R / nee_corrected_axis.R.
build_veg_hist <- function(r_map, veg_mask_r, cell_areas, step, lo, hi) {
  r_land <- mask(mask(r_map, kg_05), veg_mask_r)
  bins_lo <- seq(lo, hi - step, by = step)
  bins_hi <- bins_lo + step
  ids <- seq_along(bins_lo)
  catch_lo_id <- 0L; catch_hi_id <- max(ids) + 1L
  rcl <- rbind(cbind(bins_lo, bins_hi, as.numeric(ids)),
               c(-1e9, lo, catch_lo_id), c(hi, 1e9, catch_hi_id))
  r_hist <- classify(r_land, rcl, right = FALSE, include.lowest = TRUE)
  areas <- zonal(cell_areas, r_hist, fun = "sum", na.rm = TRUE)
  names(areas) <- c("bin_id", "area_km2")
  areas <- areas[!is.na(areas$bin_id), ]
  bin_lo_vec <- c(catch_lo_id = lo, bins_lo, catch_hi_id = hi)
  names(bin_lo_vec) <- as.character(c(catch_lo_id, ids, catch_hi_id))
  areas$value <- bin_lo_vec[as.character(areas$bin_id)]
  areas[order(areas$value), c("value", "area_km2")]
}

## Septile edges of F = 0.5*F_geo + 0.5*F_tower, evaluated on the histogram's
## own fine grid (same discrete-grid convention as every other bin-break
## function in this repo -- see make_signed_bins()/make_bins()).
compute_septile_edges <- function(hist_df, tower_vals) {
  hist_df <- hist_df[order(hist_df$value), ]
  F_geo   <- cumsum(hist_df$area_km2) / sum(hist_df$area_km2)
  F_tower <- ecdf(tower_vals)(hist_df$value)
  F_mix   <- 0.5 * F_geo + 0.5 * F_tower
  vapply(c(1, 2, 3, 4, 5, 6) / 7, function(f) {
    idx <- which(F_mix >= f)[1L]
    hist_df$value[idx]
  }, numeric(1))
}

FLUX_HIST_PARAMS <- list(
  NEE = list(step = 1,  lo = -500,  hi = 500),
  GPP = list(step = 5,  lo = 0,     hi = 5000),
  TER = list(step = 5,  lo = 0,     hi = 5000),
  ET  = list(step = 2,  lo = 0,     hi = 2000)
)
FLUX_ROUND <- c(NEE = 25, GPP = 100, TER = 100, ET = 50)

round_edges <- function(edges, to) {
  r <- round(edges / to) * to
  if (any(diff(r) <= 0)) {
    warning("Rounded edges are not strictly increasing for round-to=", to,
            " (", paste(r, collapse = ", "), ") -- leaving as-is; downstream ",
            "classification will collapse degenerate bins.")
  }
  r
}

hist_data <- list(); edges_exact <- list(); edges_rounded <- list()
for (fl in c("NEE", "GPP", "TER", "ET")) {
  p <- FLUX_HIST_PARAMS[[fl]]
  hist_data[[fl]] <- build_veg_hist(GEO_RASTERS[[fl]], veg_mask, cell_areas_05, p$step, p$lo, p$hi)
  edges_exact[[fl]]   <- compute_septile_edges(hist_data[[fl]], TOWER_VALUES[[fl]]$tower_value)
  edges_rounded[[fl]] <- round_edges(edges_exact[[fl]], FLUX_ROUND[[fl]])
  msg(fl, " exact septile edges:   ", paste(round(edges_exact[[fl]], 2), collapse = ", "))
  msg(fl, " rounded septile edges: ", paste(edges_rounded[[fl]], collapse = ", "))
}

# ============================================================================
# STEP 6: Classification + occupancy (land fraction, site fraction, J)
# ============================================================================
msg("\n=== STEP 6: Classification + occupancy ===")

classify_7 <- function(x, edges) {
  breaks <- c(-Inf, edges, Inf)
  b <- findInterval(x, breaks[-length(breaks)], left.open = FALSE)
  b[b < 1L] <- 1L
  b[b > 7L] <- 7L
  as.integer(b)
}

make_bin_labels <- function(edges, unit) {
  e <- round(edges, 1)
  c(sprintf("< %s", e[1]),
    sprintf("%s to %s", e[1], e[2]),
    sprintf("%s to %s", e[2], e[3]),
    sprintf("%s to %s", e[3], e[4]),
    sprintf("%s to %s", e[4], e[5]),
    sprintf("%s to %s", e[5], e[6]),
    sprintf("> %s", e[6]))
}

FLUX_UNIT <- c(NEE = "gC m-2 yr-1", GPP = "gC m-2 yr-1", TER = "gC m-2 yr-1", ET = "mm yr-1")
land_frac_masked <- sensitivity_df$land_fraction_removed[sensitivity_df$gpp_threshold == GPP_VEG_THRESHOLD]

site_fracs <- function(bins, n_total) as.numeric(table(factor(bins, levels = 1:7))) / n_total
weighted_jaccard <- function(p, q) sum(pmin(p, q)) / sum(pmax(p, q))

## occupancy_all: one row per flux x edge_version x bin x comparison
occupancy_rows <- list()
for (fl in c("NEE", "GPP", "TER", "ET")) {
  for (ev in c("exact", "rounded")) {
    edges <- if (ev == "exact") edges_exact[[fl]] else edges_rounded[[fl]]
    bin_labels <- make_bin_labels(edges, FLUX_UNIT[[fl]])

    hd <- hist_data[[fl]]
    hd$bin <- classify_7(hd$value, edges)
    land_frac <- hd |> group_by(bin) |> summarise(area_km2 = sum(area_km2), .groups = "drop") |>
      mutate(land_fraction = area_km2 / sum(area_km2))
    land_vec <- land_frac$land_fraction[match(1:7, land_frac$bin)]
    land_vec[is.na(land_vec)] <- 0

    tw <- TOWER_VALUES[[fl]]
    tw_bin <- classify_7(tw$tower_value, edges)
    n_data <- nrow(tw)
    fr_data <- site_fracs(tw_bin, n_data)
    j_data <- weighted_jaccard(land_vec, fr_data)

    gt <- geo_at_tower[[fl]]
    gt_bin <- classify_7(gt$model_value, edges)
    n_geo_at_tower <- nrow(gt)
    fr_geo <- site_fracs(gt_bin, n_geo_at_tower)
    j_geo <- weighted_jaccard(land_vec, fr_geo)

    occupancy_rows[[paste(fl, ev, "data", sep = "_")]] <- data.frame(
      flux = fl, edge_version = ev, bin = 1:7, bin_label = bin_labels,
      land_fraction = land_vec, comparison = "data", site_fraction = fr_data,
      n_classified = n_data, weighted_jaccard = j_data
    )
    occupancy_rows[[paste(fl, ev, "geo_at_tower", sep = "_")]] <- data.frame(
      flux = fl, edge_version = ev, bin = 1:7, bin_label = bin_labels,
      land_fraction = land_vec, comparison = "geo_at_tower", site_fraction = fr_geo,
      n_classified = n_geo_at_tower, weighted_jaccard = j_geo
    )
    msg(fl, " (", ev, "): J(Geo vs Data)=", round(j_data, 3),
        "  J(Geo vs Geo-at-tower)=", round(j_geo, 3),
        "  n_data=", n_data, "  n_geo_at_tower=", n_geo_at_tower)
  }
}
occupancy_df <- bind_rows(occupancy_rows)

# ============================================================================
# STEP 7: Plots -- paired bars (land fraction, site fraction) per bin
# ============================================================================
msg("\n=== STEP 7: Plots ===")

base_theme <- theme_minimal(base_size = 9) +
  theme(
    plot.background   = element_rect(fill = "white", colour = NA),
    panel.background  = element_rect(fill = "white", colour = NA),
    panel.border      = element_rect(colour = "black", fill = NA, linewidth = 0.4),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    axis.ticks        = element_line(colour = "black"),
    axis.ticks.length = unit(-0.15, "cm"),
    legend.background = element_rect(fill = "white", colour = NA)
  )

COMPARISON_TITLES <- c(data = "Geo vs Data", geo_at_tower = "Geo vs Geo (model at tower cells)")

for (fl in c("NEE", "GPP", "TER", "ET")) {
  for (ev in c("exact", "rounded")) {
    for (cmp in c("data", "geo_at_tower")) {
      sub <- occupancy_df |> filter(flux == fl, edge_version == ev, comparison == cmp) |> arrange(bin)
      sub$bin_label <- factor(sub$bin_label, levels = sub$bin_label)
      j_val <- sub$weighted_jaccard[1]
      n_val <- sub$n_classified[1]

      plot_df <- bind_rows(
        sub |> transmute(bin_label, category = "Vegetated land", fraction = land_fraction),
        sub |> transmute(bin_label, category = "Sites", fraction = site_fraction)
      )
      plot_df$category <- factor(plot_df$category, levels = c("Vegetated land", "Sites"))

      title_str <- paste0(fl, " -- ", COMPARISON_TITLES[[cmp]], " -- ", ev, " septile edges")
      p <- ggplot(plot_df, aes(x = fraction, y = bin_label, fill = category)) +
        geom_col(position = position_dodge(width = 0.75), width = 0.65, colour = "black", linewidth = 0.25) +
        scale_fill_manual(name = NULL, values = c("Vegetated land" = "#bdbdbd", "Sites" = "#2166ac")) +
        scale_x_continuous(name = "Fraction", expand = expansion(mult = c(0, 0.08))) +
        scale_y_discrete(name = paste0(fl, " (", FLUX_UNIT[[fl]], ")")) +
        labs(title = title_str) +
        base_theme +
        theme(axis.text.y = element_text(size = 7.5, margin = margin(r = 5)),
              axis.text.x = element_text(size = 7, margin = margin(t = 5)),
              plot.title = element_text(size = 9, face = "bold"),
              legend.position = "top")

      p <- p + annotate("text", x = Inf, y = Inf,
                         label = sprintf("n = %d\nland masked = %.1f%%\nJ = %.3f", n_val, 100 * land_frac_masked, j_val),
                         hjust = 1.05, vjust = 1.3, size = 2.4, colour = "grey20")

      file_name <- sprintf("fig_%s_%s_%s.png", tolower(fl), ev, cmp)
      out_path <- file.path(OUT_DIR, file_name)
      ggsave(out_path, p, width = 6.5, height = 3.6, dpi = 300, bg = "white")
    }
  }
}
msg("Saved 16 plots to ", OUT_DIR)

# ============================================================================
# STEP 8: Table
# ============================================================================
msg("\n=== STEP 8: Table ===")

edges_table <- bind_rows(lapply(c("NEE", "GPP", "TER", "ET"), function(fl) {
  bind_rows(
    data.frame(flux = fl, edge_version = "exact",   edge_index = 1:6, edge_value = edges_exact[[fl]]),
    data.frame(flux = fl, edge_version = "rounded", edge_index = 1:6, edge_value = edges_rounded[[fl]])
  )
}))
write_csv(edges_table, file.path(OUT_DIR, "table_septile_edges.csv"))
write_meta(file.path(OUT_DIR, "table_septile_edges.csv"),
           input_sources = c("trendy_nee_fluxbased_median.tif", "candidate_gpp_median.tif",
                              "candidate_ter_median.tif", "flux_bin_breaks_et_median_1991_2020.tif",
                              "data/duckdb/fluxnet.duckdb (monthly_converted)"),
           notes = "6 septile edges (7 bins) per flux x edge version. Edges are where F=0.5*F_geo+0.5*F_tower crosses k/7, k=1..6.")

occ_out <- file.path(OUT_DIR, "table_flux_bin_breaks_occupancy.csv")
write_csv(occupancy_df, occ_out)
write_meta(occ_out,
           input_sources = c("table_septile_edges.csv", "data/duckdb/fluxnet.duckdb (monthly_converted)"),
           notes = paste0("One row per flux x edge_version x bin x comparison. land_fraction is the ",
                           "fraction of VEGETATED land (model GPP >= ", GPP_VEG_THRESHOLD, ") in that bin, ",
                           "shared by both comparisons within a flux x edge_version. site_fraction and ",
                           "n_classified/weighted_jaccard differ by comparison: 'data' = tower-measured ",
                           "value; 'geo_at_tower' = the same Geo raster extracted at tower coordinates. ",
                           "Site fractions use the classified sites as the denominator (sum to 1 per ",
                           "flux x edge_version x comparison)."))
msg("Saved: ", occ_out)

msg("\n=== flux_bin_breaks.R complete ===")
