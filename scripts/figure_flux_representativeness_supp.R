## figure_flux_representativeness_supp.R
##
## Figure stage 4 (logs/figstage_prompt.md): SupFigs/supp_flux_representativeness,
## renumbered Supplementary Figure S5 under figure stage 6 (2026-10-02), then
## Supplementary Figure S4 in the supplementary material restructure
## (2026-10-08, SESSION_LOG.md; see DEST_BASE below and docs/figure_inventory.md;
## this script's own name and its FIG_DIR source basename are unchanged) -- four
## rows (NEE, GPP, RECO, ET) by two columns (gridded value at the tower left,
## the site's own value right), panels lettered a-h across
## rows, in Figure 5's own panel
## style (scripts/figure4_representativeness.R's draw_panel2() print
## rendering, header/caption grobs, column-gutter alignment -- ported
## verbatim below, not sourced, same convention that script already uses
## for flux_bin_breaks.R's own panel code).
##
## NEE and ET: reuse scripts/figure4_representativeness.R's own rasters,
## tower values (compute_site_annual_fluxes(), QC_THRESHOLD_YY) and binning
## method (run_flux_panel(), ported). A hard check at the end confirms both
## fluxes' n and J reproduce data/snapshots/representativeness_metrics_fig4.csv
## exactly (rows E/F) -- the task's explicit pass/fail condition.
##
## GPP and TER: same method, new to this script.
##   Model: 17-model TRENDY v14 S3 ensemble-median, 1991-2020 mean, on the
##     Koppen 0.5 deg land mask -- data/external/trendy/derived/
##     candidate_gpp_median.tif and candidate_ter_median.tif (TER = ra+rh),
##     already built by scripts/candidate_nee_gpp_ter_panels.R and already
##     loaded by figure4_representativeness.R itself as its NEE bar-1 mask
##     (GPP) -- reused here, not recomputed.
##   Tower: the site median from compute_site_annual_fluxes() -- GPP from
##     gpp_median, TER from reco_median (RECO, ecosystem respiration, is the
##     tower-measured counterpart of model TER = ra+rh) -- NOT flux_bin_
##     breaks.R's own older QC>=0.80 mean-monthly-cycle tower values, which
##     figure4_representativeness.R's own header note already flags as wrong
##     for this paper.
##   Bins: bar 1 = own model value < 5 gC/m2/yr (GPP_LOW_CUT/TER_LOW_CUT,
##     matching flux_bin_breaks.R's named constants), then six bars at the
##     rounded (to 100 gC/m2/yr) sextiles of the 50/50 land/tower mixture
##     CDF outside bar 1 -- identical rounding/histogram steps flux_bin_
##     breaks.R uses for these two fluxes (FLUX_HIST_PARAMS, FLUX_ROUND).
##   Geo vs Data: a tower only gets a bin if it has its own qualifying tower
##     value (classify_flux_sites(require_own = TRUE), same rule NEE/ET use).
##
## Writes: review/figures/representativeness/supp_flux_representativeness.*
## (source) + SupFigs copy + data/snapshots/representativeness_metrics_flux_supp.csv.

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
source("R/utils.R")
source("R/units.R")
source("R/site_annual_fluxes.R")
source("R/nature_format.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(duckdb)
  library(DBI)
  library(dplyr)
  library(readr)
  library(terra)
  library(ggplot2)
  library(patchwork)
  library(fs)
})

SNAP_DIR    <- "data/snapshots"
EXT         <- "data/external"
DERIVED_DIR <- file.path(EXT, "trendy", "derived")
FIG_DIR     <- "review/figures/representativeness"
DRAFT_DIR   <- "review/figures/draft_manuscript_v1"
SUPFIGS_DIR <- file.path(DRAFT_DIR, "SupFigs")
FIG4_TABLES_DIR <- file.path(FIG_DIR, "tables")
fs::dir_create(FIG_DIR); fs::dir_create(SUPFIGS_DIR); fs::dir_create(FIG4_TABLES_DIR)

LOG_START <- format(Sys.time(), "%Y%m%d_%H%M%S")
LOG_FILE  <- file.path("logs", paste0("figure_flux_representativeness_supp_", LOG_START, ".log"))
con_log <- file(LOG_FILE, open = "wt")
sink(con_log, type = "output")
sink(con_log, type = "message", append = TRUE)
on.exit({ sink(type = "message"); sink(type = "output"); close(con_log) }, add = TRUE)
msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)

msg("=== figure_flux_representativeness_supp.R (figure stage 4) ===")
msg("Log: ", LOG_FILE)

weighted_jaccard <- function(p, q) sum(pmin(p, q)) / sum(pmax(p, q))

## ---- Current-network site list: SAME pin as figure4_representativeness.R
CURRENT_SNAPSHOT <- "data/snapshots/fluxnet_shuttle_snapshot_20260901T094522.csv"
current_sites <- readr::read_csv(CURRENT_SNAPSHOT, show_col_types = FALSE) |>
  dplyr::distinct(site_id, location_lat, location_long)
n_sites <- nrow(current_sites)
msg("Current network: ", n_sites, " sites (snapshot ", basename(CURRENT_SNAPSHOT), ")")
if (n_sites != 781L) {
  warning("Expected 781 current-network sites, got ", n_sites)
}

# ==============================================================================
# STEP 1: Land mask, rasters (reuse Figure 4's own cached rasters, not recomputed)
# ==============================================================================
msg("\n=== STEP 1: Rasters and land mask ===")
KG_PATH_05 <- file.path(EXT, "koppen_beck2023", "1991_2020", "koppen_geiger_0p5.tif")
kg_05 <- terra::rast(KG_PATH_05)
cell_areas_05 <- terra::cellSize(kg_05, mask = TRUE, unit = "km")

r_nee <- terra::rast(file.path(DERIVED_DIR, "trendy_nee_fluxbased_median.tif"))
r_gpp <- terra::rast(file.path(DERIVED_DIR, "candidate_gpp_median.tif"))
r_ter <- terra::rast(file.path(DERIVED_DIR, "candidate_ter_median.tif"))
r_et  <- terra::rast(file.path(DERIVED_DIR, "flux_bin_breaks_et_median_1991_2020.tif"))

GEO_LAND_NEE <- terra::mask(r_nee, kg_05)
GEO_LAND_GPP <- terra::mask(r_gpp, kg_05)
GEO_LAND_TER <- terra::mask(r_ter, kg_05)
GEO_LAND_ET  <- terra::mask(r_et,  kg_05)

FLUX_LAND_TOTAL_KM2 <- sum(terra::values(cell_areas_05)[!is.na(terra::values(GEO_LAND_GPP))], na.rm = TRUE)
msg("Flux land total (TRENDY ensemble footprint under Koppen mask): ",
    format(round(FLUX_LAND_TOTAL_KM2), big.mark = ","), " km2")

# ==============================================================================
# STEP 2: Tower annual values via the shared compute_site_annual_fluxes()
# ==============================================================================
msg("\n=== STEP 2: Tower annual values (compute_site_annual_fluxes) ===")
duckdb_path <- "data/duckdb/fluxnet.duckdb"
con <- dbConnect(duckdb(), dbdir = duckdb_path, read_only = TRUE)
site_fluxes <- compute_site_annual_fluxes(con, site_ids = current_sites$site_id)
dbDisconnect(con, shutdown = TRUE)
site_src <- site_fluxes$site_summary
msg("Per-site VUT/CUT choice (NEE/GPP/RECO): VUT=", sum(site_src$nee_source == "VUT", na.rm = TRUE),
    "  CUT (fallback)=", sum(site_src$nee_source == "CUT", na.rm = TRUE),
    "  neither=", sum(is.na(site_src$nee_source)))

tower_from <- function(value_col) {
  site_src |>
    dplyr::filter(!is.na(.data[[value_col]])) |>
    dplyr::transmute(site_id, tower_value = .data[[value_col]]) |>
    dplyr::left_join(current_sites, by = "site_id")
}
tower_nee <- tower_from("nee_median")
tower_gpp <- tower_from("gpp_median")
tower_ter <- tower_from("reco_median")   # RECO (tower) is the measured counterpart of model TER = ra+rh
tower_et  <- tower_from("et_median")
msg("Tower annual values (QC_THRESHOLD_YY=", QC_THRESHOLD_YY, ", median of qualifying years) -- NEE: ",
    nrow(tower_nee), "  GPP: ", nrow(tower_gpp), "  TER: ", nrow(tower_ter), "  ET: ", nrow(tower_et))

geo_coords <- as.matrix(current_sites[, c("location_long", "location_lat")])
model_nee_at_site <- terra::extract(r_nee, geo_coords, method = "bilinear")[, 1]
model_gpp_at_site <- terra::extract(r_gpp, geo_coords, method = "bilinear")[, 1]
model_ter_at_site <- terra::extract(r_ter, geo_coords, method = "bilinear")[, 1]
model_et_at_site  <- terra::extract(r_et,  geo_coords, method = "bilinear")[, 1]

# ==============================================================================
# STEP 3: Binning method -- ported verbatim from figure4_representativeness.R
# (NOT sourced -- that script has side effects/re-renders Figure 4 itself)
# ==============================================================================
msg("\n=== STEP 3: Binning (ported from figure4_representativeness.R) ===")

NEE_BAR1_GPP_CUT <- 5   # gC m-2 yr-1
GPP_LOW_CUT      <- 5   # gC m-2 yr-1 -- matches scripts/diagnostics/flux_bin_breaks.R
TER_LOW_CUT      <- 5   # gC m-2 yr-1
ET_LOW_CUT       <- 5   # mm yr-1
GEO_MIXTURE_WEIGHT <- 0.5
FLUX_ROUND <- c(NEE = 25, GPP = 100, TER = 100, ET = 50)
FLUX_HIST_PARAMS <- list(
  NEE = list(step = 1, lo = -500, hi = 500),
  GPP = list(step = 5, lo = 0,    hi = 5000),
  TER = list(step = 5, lo = 0,    hi = 5000),
  ET  = list(step = 2, lo = 0,    hi = 2000)
)

classify_flux_sites <- function(mask_value, own_value, cut, edges, require_own = FALSE) {
  bin <- rep(NA_integer_, length(mask_value))
  valid_mask <- !is.na(mask_value)
  is_bar1 <- valid_mask & mask_value < cut
  bin[is_bar1] <- 1L
  breaks <- c(-Inf, edges, Inf)
  b <- findInterval(own_value, breaks[-length(breaks)], left.open = FALSE) + 1L
  b[b < 2L] <- 2L; b[b > 7L] <- 7L
  need_own <- valid_mask & !is_bar1 & !is.na(own_value)
  bin[need_own] <- b[need_own]
  if (require_own) bin[is.na(own_value)] <- NA_integer_
  as.integer(bin)
}
build_hist_outside_bar1 <- function(own_val_r, mask_val_r, bar1_cut, cell_areas, step, lo, hi) {
  r_land <- terra::mask(own_val_r, terra::ifel(mask_val_r >= bar1_cut, 1, NA))
  bins_lo <- seq(lo, hi - step, by = step); bins_hi <- bins_lo + step
  ids <- seq_along(bins_lo); catch_lo_id <- 0L; catch_hi_id <- max(ids) + 1L
  rcl <- rbind(cbind(bins_lo, bins_hi, as.numeric(ids)),
               c(-1e9, lo, catch_lo_id), c(hi, 1e9, catch_hi_id))
  r_hist <- terra::classify(r_land, rcl, right = FALSE, include.lowest = TRUE)
  areas <- terra::zonal(cell_areas, r_hist, fun = "sum", na.rm = TRUE)
  names(areas) <- c("bin_id", "area_km2"); areas <- areas[!is.na(areas$bin_id), ]
  bin_lo_vec <- c(catch_lo_id = lo, bins_lo, catch_hi_id = hi)
  names(bin_lo_vec) <- as.character(c(catch_lo_id, ids, catch_hi_id))
  areas$value <- bin_lo_vec[as.character(areas$bin_id)]
  areas[order(areas$value), c("value", "area_km2")]
}
compute_sextile_edges <- function(hist_df, tower_vals, w = GEO_MIXTURE_WEIGHT) {
  hist_df <- hist_df[order(hist_df$value), ]
  F_geo <- cumsum(hist_df$area_km2) / sum(hist_df$area_km2)
  F_tower <- ecdf(tower_vals)(hist_df$value)
  F_mix <- w * F_geo + (1 - w) * F_tower
  vapply(c(1, 2, 3, 4, 5) / 6, function(f) { idx <- which(F_mix >= f)[1L]; hist_df$value[idx] }, numeric(1))
}
round_edges <- function(edges, to) round(edges / to) * to

## Ported from run_flux_panel() in figure4_representativeness.R, with one
## addition: also returns n_data/j_data/n_geo/j_geo directly (that script
## instead wrote them straight to its own metrics_rows accumulator via
## add_metric(), which this script does not share).
run_flux_panel <- function(flux_name, own_land_r, mask_land_r, mask_value_at_site,
                            own_value_at_site_model, tower_df, bar1_cut, hist_params, round_to) {
  site_mask_value <- mask_value_at_site
  site_is_bar1 <- !is.na(site_mask_value) & site_mask_value < bar1_cut

  h <- build_hist_outside_bar1(own_land_r, mask_land_r, bar1_cut, cell_areas_05,
                                hist_params$step, hist_params$lo, hist_params$hi)
  tower_remaining <- tower_df |>
    dplyr::left_join(current_sites |> dplyr::mutate(is_bar1 = site_is_bar1) |> dplyr::select(site_id, is_bar1), by = "site_id") |>
    dplyr::filter(!is.na(is_bar1), !is_bar1)
  edges_exact <- compute_sextile_edges(h, tower_remaining$tower_value)
  edges <- round_edges(edges_exact, round_to)
  msg(flux_name, " rounded sextile edges: ", paste(edges, collapse = ", "))

  breaks <- c(-1e9, edges, 1e9)
  rcl <- cbind(breaks[-length(breaks)], breaks[-1], 2:7)
  own_bin_r <- terra::classify(own_land_r, rcl, right = FALSE, include.lowest = TRUE)
  bar1_r <- terra::ifel(mask_land_r < bar1_cut, 1, NA)
  r_bin <- terra::ifel(!is.na(bar1_r), 1, own_bin_r)
  zone_areas <- terra::zonal(cell_areas_05, r_bin, fun = "sum", na.rm = TRUE)
  names(zone_areas) <- c("bin", "area_km2")
  zone_areas <- zone_areas[!is.na(zone_areas$bin) & zone_areas$bin %in% 1:7, ]
  land_vec <- zone_areas$area_km2[match(1:7, zone_areas$bin)]; land_vec[is.na(land_vec)] <- 0
  land_vec <- land_vec / FLUX_LAND_TOTAL_KM2

  data_df <- current_sites |>
    dplyr::left_join(data.frame(site_id = current_sites$site_id, mask_value = site_mask_value), by = "site_id") |>
    dplyr::left_join(tower_df |> dplyr::select(site_id, own_value = tower_value), by = "site_id")
  data_bin <- classify_flux_sites(data_df$mask_value, data_df$own_value, bar1_cut, edges, require_own = TRUE)
  n_data <- sum(!is.na(data_bin))
  fr_data <- as.numeric(table(factor(data_bin, levels = 1:7))) / n_data
  j_data <- weighted_jaccard(land_vec, fr_data)

  geo_df <- current_sites |> dplyr::mutate(mask_value = site_mask_value, own_value = own_value_at_site_model)
  geo_bin <- classify_flux_sites(geo_df$mask_value, geo_df$own_value, bar1_cut, edges)
  n_geo <- sum(!is.na(geo_bin))
  fr_geo <- as.numeric(table(factor(geo_bin, levels = 1:7))) / n_geo
  j_geo <- weighted_jaccard(land_vec, fr_geo)

  msg(flux_name, ": J(Geo vs Data)=", round(j_data, 3), " J(Geo vs Geo)=", round(j_geo, 3),
      " n_data=", n_data, " n_geo=", n_geo)

  list(edges = edges, land_vec = land_vec, data_bin = data_bin, geo_bin = geo_bin,
       data_df = data_df, geo_df = geo_df, mask_value = site_mask_value,
       n_data = n_data, j_data = j_data, n_geo = n_geo, j_geo = j_geo)
}

nee_result <- run_flux_panel("NEE", GEO_LAND_NEE, GEO_LAND_GPP, model_gpp_at_site, model_nee_at_site,
                              tower_nee, NEE_BAR1_GPP_CUT, FLUX_HIST_PARAMS$NEE, FLUX_ROUND[["NEE"]])
gpp_result <- run_flux_panel("GPP", GEO_LAND_GPP, GEO_LAND_GPP, model_gpp_at_site, model_gpp_at_site,
                              tower_gpp, GPP_LOW_CUT, FLUX_HIST_PARAMS$GPP, FLUX_ROUND[["GPP"]])
ter_result <- run_flux_panel("TER", GEO_LAND_TER, GEO_LAND_TER, model_ter_at_site, model_ter_at_site,
                              tower_ter, TER_LOW_CUT, FLUX_HIST_PARAMS$TER, FLUX_ROUND[["TER"]])
et_result  <- run_flux_panel("ET",  GEO_LAND_ET,  GEO_LAND_ET,  model_et_at_site,  model_et_at_site,
                              tower_et, ET_LOW_CUT, FLUX_HIST_PARAMS$ET, FLUX_ROUND[["ET"]])

# ==============================================================================
# STEP 4: Hard check -- NEE and ET must reproduce Figure 4 exactly
# ==============================================================================
msg("\n=== STEP 4: Confirming NEE/ET reproduce representativeness_metrics_fig4.csv ===")
fig4_metrics <- readr::read_csv(file.path(SNAP_DIR, "representativeness_metrics_fig4.csv"), show_col_types = FALSE)
get_fig4 <- function(axis, cmp) fig4_metrics |> dplyr::filter(axis == !!axis, comparison == !!cmp)

check_against_fig4 <- function(flux_label, result) {
  row_geo  <- get_fig4(tolower(flux_label), "geo_vs_geo")
  row_data <- get_fig4(tolower(flux_label), "geo_vs_data")
  ok <- TRUE
  if (nrow(row_geo) != 1 || result$n_geo != row_geo$n_classified ||
      abs(result$j_geo - row_geo$weighted_jaccard) > 1e-9) {
    ok <- FALSE
    msg("MISMATCH (", flux_label, " geo_vs_geo): this script n=", result$n_geo, " J=", result$j_geo,
        " vs fig4 n=", if (nrow(row_geo) == 1) row_geo$n_classified else NA,
        " J=", if (nrow(row_geo) == 1) row_geo$weighted_jaccard else NA)
  }
  if (nrow(row_data) != 1 || result$n_data != row_data$n_classified ||
      abs(result$j_data - row_data$weighted_jaccard) > 1e-9) {
    ok <- FALSE
    msg("MISMATCH (", flux_label, " geo_vs_data): this script n=", result$n_data, " J=", result$j_data,
        " vs fig4 n=", if (nrow(row_data) == 1) row_data$n_classified else NA,
        " J=", if (nrow(row_data) == 1) row_data$weighted_jaccard else NA)
  }
  ok
}
nee_ok <- check_against_fig4("NEE", nee_result)
et_ok  <- check_against_fig4("ET",  et_result)
if (!nee_ok || !et_ok) {
  stop("STAGE FAILED: NEE and/or ET panels do not reproduce representativeness_metrics_fig4.csv exactly.")
}
msg("CONFIRMED: NEE and ET n/J both reproduce representativeness_metrics_fig4.csv exactly.")

# ==============================================================================
# STEP 5: Save metrics table (task requirement)
# ==============================================================================
msg("\n=== STEP 5: Save representativeness_metrics_flux_supp.csv ===")
FLUX_LAND_GRID_DESC <- "TRENDY v14 ensemble-median, 0.5 deg, Koppen land mask"
metrics_rows_supp <- list(
  data.frame(panel = "a", axis = "nee", comparison = "geo_vs_geo",  land_grid = FLUX_LAND_GRID_DESC,
             land_total_km2 = FLUX_LAND_TOTAL_KM2, n_eligible = nee_result$n_geo, n_classified = nee_result$n_geo, weighted_jaccard = nee_result$j_geo,
             bin_edges = paste(nee_result$edges, collapse = ";")),
  data.frame(panel = "b", axis = "nee", comparison = "geo_vs_data", land_grid = FLUX_LAND_GRID_DESC,
             land_total_km2 = FLUX_LAND_TOTAL_KM2, n_eligible = nee_result$n_data, n_classified = nee_result$n_data, weighted_jaccard = nee_result$j_data,
             bin_edges = paste(nee_result$edges, collapse = ";")),
  data.frame(panel = "c", axis = "gpp", comparison = "geo_vs_geo",  land_grid = FLUX_LAND_GRID_DESC,
             land_total_km2 = FLUX_LAND_TOTAL_KM2, n_eligible = gpp_result$n_geo, n_classified = gpp_result$n_geo, weighted_jaccard = gpp_result$j_geo,
             bin_edges = paste(gpp_result$edges, collapse = ";")),
  data.frame(panel = "d", axis = "gpp", comparison = "geo_vs_data", land_grid = FLUX_LAND_GRID_DESC,
             land_total_km2 = FLUX_LAND_TOTAL_KM2, n_eligible = gpp_result$n_data, n_classified = gpp_result$n_data, weighted_jaccard = gpp_result$j_data,
             bin_edges = paste(gpp_result$edges, collapse = ";")),
  data.frame(panel = "e", axis = "ter", comparison = "geo_vs_geo",  land_grid = FLUX_LAND_GRID_DESC,
             land_total_km2 = FLUX_LAND_TOTAL_KM2, n_eligible = ter_result$n_geo, n_classified = ter_result$n_geo, weighted_jaccard = ter_result$j_geo,
             bin_edges = paste(ter_result$edges, collapse = ";")),
  data.frame(panel = "f", axis = "ter", comparison = "geo_vs_data", land_grid = FLUX_LAND_GRID_DESC,
             land_total_km2 = FLUX_LAND_TOTAL_KM2, n_eligible = ter_result$n_data, n_classified = ter_result$n_data, weighted_jaccard = ter_result$j_data,
             bin_edges = paste(ter_result$edges, collapse = ";")),
  data.frame(panel = "g", axis = "et", comparison = "geo_vs_geo",  land_grid = FLUX_LAND_GRID_DESC,
             land_total_km2 = FLUX_LAND_TOTAL_KM2, n_eligible = et_result$n_geo, n_classified = et_result$n_geo, weighted_jaccard = et_result$j_geo,
             bin_edges = paste(et_result$edges, collapse = ";")),
  data.frame(panel = "h", axis = "et", comparison = "geo_vs_data", land_grid = FLUX_LAND_GRID_DESC,
             land_total_km2 = FLUX_LAND_TOTAL_KM2, n_eligible = et_result$n_data, n_classified = et_result$n_data, weighted_jaccard = et_result$j_data,
             bin_edges = paste(et_result$edges, collapse = ";"))
)
metrics_supp_df <- dplyr::bind_rows(metrics_rows_supp)
metrics_supp_path <- file.path(SNAP_DIR, "representativeness_metrics_flux_supp.csv")
readr::write_csv(metrics_supp_df, metrics_supp_path)
write_output_metadata(
  metrics_supp_path,
  input_sources = c("representativeness_metrics_fig4.csv", "trendy_nee_fluxbased_median.tif",
                     "candidate_gpp_median.tif", "candidate_ter_median.tif",
                     "flux_bin_breaks_et_median_1991_2020.tif"),
  notes = paste0(
    "Figure stage 4 (logs/figstage_prompt.md): 8-panel n/J table for SupFigs/supp_flux_representativeness ",
    "(NEE, GPP, TER, ET x Geo vs Geo/Geo vs Data). NEE/ET rows (a/b/g/h) confirmed identical to ",
    "representativeness_metrics_fig4.csv rows E/F (panel letters differ; axis/comparison/n/J match ",
    "exactly, checked programmatically before this file was written). GPP/TER (c/d/e/f) are new: tower ",
    "values from R/site_annual_fluxes.R::compute_site_annual_fluxes() (gpp_median/reco_median, ",
    "QC_THRESHOLD_YY=", QC_THRESHOLD_YY, "), model from the 17-model TRENDY v14 S3 ensemble-median, ",
    "1991-2020 mean (TER = ra+rh), same rasters scripts/diagnostics/flux_bin_breaks.R already built. ",
    "bin_edges: 5 rounded sextile edges (bars 2-7 boundaries); bar 1 is always <5 (own model value)."
  )
)
msg("Saved: ", metrics_supp_path)
print(as.data.frame(metrics_supp_df[, c("panel", "axis", "comparison", "n_classified", "weighted_jaccard")]))

# ==============================================================================
# STEP 6: Panel rendering machinery -- ported verbatim from
# figure4_representativeness.R (not sourced; see header)
# ==============================================================================
msg("\n=== STEP 6: Render panels (Figure 4 print style) ===")

LOG2_MAX    <- log2(5)
LOG2_BREAKS <- c(-LOG2_MAX, -1, 0, 1, LOG2_MAX)
LOG2_LABELS <- c("1/5×","1/2×","1×","2×","5×")
LOG2_XLIM   <- c(-LOG2_MAX - 0.25, LOG2_MAX + 0.25)

BASE_PT        <- 7
LETTER_PT      <- 8
FIG_WIDTH_MM   <- 180   # SupFig (Extended-Data-style) limit, task: "at most 180 x 240mm"
NCOL_FIG       <- 2
COL_WIDTH_MM   <- FIG_WIDTH_MM / NCOL_FIG
ROW_PITCH_MM   <- 4.2
MM_PER_PT      <- 25.4 / 72
PANEL_MARGIN_MM <- 2
FIG_FONT     <- "Helvetica"
MEASURE_FONT <- "Arial"

if (!requireNamespace("systemfonts", quietly = TRUE)) {
  stop("systemfonts is required for measured text widths.")
}
font_check <- systemfonts::match_fonts(FIG_FONT)
msg("Render font resolved: ", FIG_FONT, " -> ", font_check$path[1])

text_width_mm <- function(label, size_pt, weight = "normal") {
  w_pt <- systemfonts::string_width(label, family = MEASURE_FONT, size = size_pt, weight = weight)
  w_pt * MM_PER_PT
}

measure_panel_layout_mm <- function(class_labels, show_xlab) {
  dummy_df <- data.frame(y = factor(class_labels, levels = class_labels), x = NA_real_)
  p <- ggplot2::ggplot(dummy_df, ggplot2::aes(x = x, y = y)) +
    ggplot2::scale_x_continuous(limits = LOG2_XLIM, breaks = LOG2_BREAKS, labels = LOG2_LABELS,
                                 expand = ggplot2::expansion(mult = 0)) +
    ggplot2::scale_y_discrete(name = NULL, limits = class_labels) +
    ggplot2::theme_minimal(base_size = BASE_PT) +
    ggplot2::theme(
      text = ggplot2::element_text(family = FIG_FONT, size = BASE_PT),
      axis.text.y = ggplot2::element_text(size = BASE_PT, family = FIG_FONT),
      axis.text.x = if (show_xlab) ggplot2::element_text(size = BASE_PT, family = FIG_FONT) else ggplot2::element_blank(),
      axis.title.x = if (show_xlab) ggplot2::element_text(size = BASE_PT, family = FIG_FONT) else ggplot2::element_blank(),
      plot.margin = ggplot2::margin(PANEL_MARGIN_MM, PANEL_MARGIN_MM, PANEL_MARGIN_MM, PANEL_MARGIN_MM, "mm")
    )
  tmp_pdf <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp_pdf, width = 10, height = 10, family = "Helvetica")
  on.exit({ grDevices::dev.off(); unlink(tmp_pdf) }, add = TRUE)
  g <- ggplot2::ggplotGrob(p)
  panel_col <- g$layout$l[g$layout$name == "panel"][1]
  left_cols  <- which(seq_along(g$widths) < panel_col)
  right_cols <- which(seq_along(g$widths) > panel_col)
  left_mm  <- sum(vapply(left_cols,  function(i) grid::convertWidth(g$widths[i], "mm", valueOnly = TRUE), numeric(1)))
  right_mm <- sum(vapply(right_cols, function(i) grid::convertWidth(g$widths[i], "mm", valueOnly = TRUE), numeric(1)))
  list(left_mm = left_mm, right_mm = right_mm)
}
panel_mm_per_unit_for <- function(class_labels, show_xlab, target_left_mm) {
  layout_mm <- measure_panel_layout_mm(class_labels, show_xlab)
  panel_mm <- COL_WIDTH_MM - target_left_mm - layout_mm$right_mm
  if (panel_mm <= 10) {
    warning("Panel data-area width came out implausibly small (", round(panel_mm, 1), " mm).")
  }
  panel_mm / (LOG2_XLIM[2] - LOG2_XLIM[1])
}
format_ratio_label_one <- function(ratio) {
  if (is.na(ratio)) return(NA_character_)
  if (ratio > 1000) return(">1000×")
  if (ratio >= 10) return(paste0(round(ratio), "×"))
  sprintf("%.1f×", ratio)
}
format_ratio_label <- function(ratio) vapply(ratio, format_ratio_label_one, character(1))
format_clip_annot_one <- function(sampling_ratio, log2_sr) {
  if (is.na(sampling_ratio) || is.na(log2_sr)) return(NA_character_)
  if (log2_sr > 0) return(format_ratio_label_one(sampling_ratio))
  inv <- 1 / sampling_ratio
  if (inv > 1000) return("1/>1000×")
  if (inv >= 10) return(paste0("1/", round(inv), "×"))
  sprintf("1/%.1f×", inv)
}
format_clip_annot <- function(sampling_ratio, log2_sr) mapply(format_clip_annot_one, sampling_ratio, log2_sr)
format_land_pct <- function(frac) {
  pct <- frac * 100
  dplyr::if_else(pct < 0.1 & pct > 0, "<0.1", sprintf("%.1f", pct))
}
contrast_text_color <- function(hex) {
  rgb_mat <- grDevices::col2rgb(hex) / 255
  lum <- 0.2126 * rgb_mat["red", ] + 0.7152 * rgb_mat["green", ] + 0.0722 * rgb_mat["blue", ]
  ifelse(lum < 0.5, "white", "grey10")
}
prep_ordered <- function(df) {
  df |> dplyr::arrange(class_order) |>
    dplyr::mutate(class_label = factor(class_label, levels = unique(class_label)))
}
prep_clip2 <- function(df) {
  df |>
    dplyr::mutate(
      is_none    = global_land_fraction > 0 & (is.na(n) | n == 0L) & is.na(log2_sr),
      is_neither = global_land_fraction == 0 & (is.na(n) | n == 0L),
      log2_sr_clip = dplyr::case_when(
        is_none ~ -LOG2_MAX,
        TRUE ~ pmax(pmin(dplyr::coalesce(log2_sr, 0), LOG2_MAX), -LOG2_MAX)
      ),
      truncated = !is_none & !is.na(log2_sr) & abs(log2_sr) > LOG2_MAX,
      annot_label = dplyr::if_else(truncated, format_clip_annot(sampling_ratio, log2_sr), NA_character_),
      annot_x = dplyr::case_when(
        truncated & log2_sr > 0 ~  LOG2_MAX - 0.08,
        truncated & log2_sr < 0 ~ -LOG2_MAX + 0.08,
        TRUE ~ NA_real_
      ),
      annot_hjust = dplyr::case_when(truncated & log2_sr > 0 ~ 1, truncated & log2_sr < 0 ~ 0, TRUE ~ 0.5)
    )
}
TITLE_GAP_MM <- 1.6
header_grob <- function(letter, title_text, j_val) {
  letter_grob <- grid::textGrob(tolower(letter), x = 0, hjust = 0, vjust = 0.5,
                                 gp = grid::gpar(fontsize = LETTER_PT, fontface = "bold", fontfamily = FIG_FONT, col = "grey10"))
  title_x <- grid::grobWidth(letter_grob) + grid::unit(TITLE_GAP_MM, "mm")
  title_grob <- grid::textGrob(title_text, x = title_x, hjust = 0, vjust = 0.5,
                                gp = grid::gpar(fontsize = BASE_PT, fontfamily = FIG_FONT, col = "grey10"))
  j_text <- if (!is.na(j_val)) sprintf("J = %.3f", j_val) else ""
  j_grob <- grid::textGrob(j_text, x = 1, hjust = 1, vjust = 0.5,
                            gp = grid::gpar(fontsize = BASE_PT, fontfamily = FIG_FONT, col = "grey20"))
  grid::gTree(children = grid::gList(letter_grob, title_grob, j_grob))
}
caption_grob <- function() {
  left  <- grid::textGrob("smaller proportion", x = 0.25, hjust = 0.5, vjust = 1,
                           gp = grid::gpar(fontsize = BASE_PT, fontfamily = FIG_FONT, col = "grey30"))
  right <- grid::textGrob("greater proportion", x = 0.75, hjust = 0.5, vjust = 1,
                           gp = grid::gpar(fontsize = BASE_PT, fontfamily = FIG_FONT, col = "grey30"))
  grid::gTree(children = grid::gList(left, right))
}
replace_gtable_cell <- function(g, cell_name, new_grob) {
  idx <- which(g$layout$name == cell_name)
  if (length(idx) == 0) {
    warning("gtable cell '", cell_name, "' not found.")
    return(g)
  }
  g$grobs[[idx[1]]] <- new_grob
  g
}
align_panel_left_mm <- function(g, target_left_mm) {
  panel_col <- g$layout$l[g$layout$name == "panel"][1]
  axis_l_rows <- which(g$layout$name == "axis-l")
  if (length(axis_l_rows) == 0) {
    warning("gtable has no 'axis-l' cell -- column alignment skipped.")
    return(g)
  }
  axis_l_col <- g$layout$l[axis_l_rows[1]]
  other_left_cols <- setdiff(which(seq_along(g$widths) < panel_col), axis_l_col)
  tmp_pdf <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp_pdf, width = 10, height = 10, family = "Helvetica")
  on.exit({ grDevices::dev.off(); unlink(tmp_pdf) }, add = TRUE)
  other_left_mm <- sum(vapply(other_left_cols, function(i) grid::convertWidth(g$widths[i], "mm", valueOnly = TRUE), numeric(1)))
  g$widths[axis_l_col] <- grid::unit(max(target_left_mm - other_left_mm, 0), "mm")
  g
}
LABEL_OFFSET <- 0.15
draw_panel2 <- function(df, panel_mm_per_unit, letter, title_text, j_val,
                         show_xlab = FALSE, show_header = FALSE, label_pt = BASE_PT,
                         target_left_mm = NA_real_) {
  if (show_header) {
    header_row <- df[1, ]
    header_row[] <- NA
    header_row$class_label <- ""
    header_row$class_order <- max(df$class_order, na.rm = TRUE) + 1
    header_row$color_hex <- NA_character_
    header_row$global_land_fraction <- 0
    header_row$n <- NA_integer_
    header_row$is_none <- FALSE
    header_row$is_neither <- TRUE
    header_row$log2_sr_clip <- NA_real_
    header_row$truncated <- FALSE
    df <- dplyr::bind_rows(df, header_row) |> prep_ordered()
  }

  bar_df  <- dplyr::filter(df, !is_neither, !is_none)
  none_df <- dplyr::filter(df, is_none)
  col_vals <- setNames(df$color_hex, as.character(df$class_label))

  p <- ggplot2::ggplot(df, ggplot2::aes(x = log2_sr_clip, y = class_label)) +
    ggplot2::geom_vline(xintercept = c(-LOG2_MAX, -1, 1, LOG2_MAX), colour = "grey88", linewidth = nature_lwd(0.3)) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey40", linewidth = nature_lwd(0.45))
  if (nrow(bar_df) > 0) {
    p <- p + ggplot2::geom_col(data = bar_df, ggplot2::aes(fill = class_label), width = 0.72,
                                na.rm = TRUE, show.legend = FALSE, colour = "black", linewidth = nature_lwd(NATURE_LINEWIDTH_MIN))
  }
  if (nrow(none_df) > 0) {
    p <- p + ggplot2::geom_col(data = none_df, width = 0.72, na.rm = TRUE, show.legend = FALSE,
                                fill = "white", colour = "black", linewidth = nature_lwd(NATURE_LINEWIDTH_MIN), linetype = "dashed")
  }
  y_limits <- levels(df$class_label)
  y_breaks <- setdiff(y_limits, "")
  p <- p +
    ggplot2::scale_fill_manual(values = col_vals, na.value = NA) +
    ggplot2::scale_x_continuous(limits = LOG2_XLIM, breaks = LOG2_BREAKS, labels = LOG2_LABELS,
                                 expand = ggplot2::expansion(mult = 0), name = NULL) +
    ggplot2::scale_y_discrete(name = NULL, limits = y_limits, breaks = y_breaks) +
    ggplot2::theme_minimal(base_size = BASE_PT) +
    ggplot2::theme(
      text              = ggplot2::element_text(family = FIG_FONT, size = BASE_PT, colour = "grey10"),
      plot.background   = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background  = ggplot2::element_rect(fill = "white", colour = NA),
      panel.border      = ggplot2::element_rect(colour = "black", fill = NA, linewidth = nature_lwd(NATURE_LINEWIDTH_MAX)),
      panel.grid.major  = ggplot2::element_blank(),
      panel.grid.minor  = ggplot2::element_blank(),
      axis.ticks        = ggplot2::element_line(colour = "black"),
      axis.ticks.length = ggplot2::unit(-0.15, "cm"),
      axis.text.y   = ggplot2::element_text(size = BASE_PT, family = FIG_FONT, colour = "grey10", margin = ggplot2::margin(r = 5)),
      axis.text.x   = if (show_xlab) ggplot2::element_text(size = BASE_PT, family = FIG_FONT, colour = "grey10", margin = ggplot2::margin(t = 5)) else ggplot2::element_blank(),
      axis.ticks.x  = if (show_xlab) ggplot2::element_line() else ggplot2::element_blank(),
      axis.title.x  = ggplot2::element_text(size = BASE_PT),
      plot.margin   = ggplot2::margin(PANEL_MARGIN_MM, PANEL_MARGIN_MM, PANEL_MARGIN_MM, PANEL_MARGIN_MM, "mm"),
      plot.title    = ggplot2::element_text(size = max(LETTER_PT, BASE_PT))
    ) +
    ggplot2::labs(title = " ", x = if (show_xlab) " " else NULL)

  BEYOND_GAP <- 0.06
  lbl_df <- df |>
    dplyr::filter(!is_neither | class_label == "") |>
    dplyr::mutate(
      bar_lo = dplyr::if_else(is_none, -LOG2_MAX, pmin(0, dplyr::coalesce(log2_sr_clip, 0))),
      bar_hi = pmax(0, dplyr::coalesce(log2_sr_clip, 0)),
      has_bar_left  = is_none | (!is.na(log2_sr) & bar_lo < 0),
      has_bar_right = !is_none & !is.na(log2_sr) & bar_hi > 0,
      is_header_row = class_label == "",
      left_label  = dplyr::if_else(is_header_row, "% land", format_land_pct(global_land_fraction)),
      right_label = dplyr::if_else(is_header_row, "towers", dplyr::if_else(is_none, "none", format(n, big.mark = ","))),
      left_w_units  = vapply(left_label, text_width_mm, numeric(1), size_pt = label_pt) / panel_mm_per_unit,
      right_w_units = vapply(right_label, text_width_mm, numeric(1), size_pt = label_pt) / panel_mm_per_unit,
      left_covered  = !is_header_row & has_bar_left  & bar_lo <= -(LABEL_OFFSET + left_w_units),
      right_covered = !is_header_row & has_bar_right & bar_hi >=  (LABEL_OFFSET + right_w_units),
      left_x = dplyr::case_when(
        is_header_row ~ -LABEL_OFFSET, left_covered ~ -LABEL_OFFSET,
        has_bar_left ~ bar_lo - BEYOND_GAP, TRUE ~ -LABEL_OFFSET
      ),
      right_x = dplyr::case_when(
        is_header_row ~ LABEL_OFFSET, right_covered ~ LABEL_OFFSET,
        has_bar_right ~ bar_hi + BEYOND_GAP, TRUE ~ LABEL_OFFSET
      ),
      left_colour  = dplyr::case_when(is_header_row ~ "grey30", left_covered ~ contrast_text_color(dplyr::if_else(is_none, "#FFFFFF", color_hex)), TRUE ~ "grey10"),
      right_colour = dplyr::case_when(is_header_row ~ "grey30", right_covered ~ contrast_text_color(color_hex), TRUE ~ "grey10")
    )
  p <- p +
    ggplot2::geom_text(data = lbl_df, ggplot2::aes(x = left_x, y = class_label, label = left_label, colour = I(left_colour)),
                        inherit.aes = FALSE, hjust = 1, size = label_pt, size.unit = "pt", family = FIG_FONT) +
    ggplot2::geom_text(data = lbl_df, ggplot2::aes(x = right_x, y = class_label, label = right_label, colour = I(right_colour)),
                        inherit.aes = FALSE, hjust = 0, size = label_pt, size.unit = "pt", family = FIG_FONT)

  ann_df <- dplyr::filter(df, !is.na(annot_label)) |> dplyr::mutate(clip_colour = contrast_text_color(color_hex))
  if (nrow(ann_df) > 0) {
    p <- p + ggplot2::geom_text(
      data = ann_df, ggplot2::aes(x = annot_x, y = class_label, label = annot_label, hjust = annot_hjust, colour = I(clip_colour)),
      inherit.aes = FALSE, size = label_pt, size.unit = "pt", family = FIG_FONT
    )
  }

  g <- ggplot2::ggplotGrob(p)
  if (!is.na(target_left_mm)) g <- align_panel_left_mm(g, target_left_mm)
  g <- replace_gtable_cell(g, "title", header_grob(letter, title_text, j_val))
  if (show_xlab) g <- replace_gtable_cell(g, "xlab-b", caption_grob())
  g
}
measure_panel_overhead_mm <- function(show_xlab, show_header) {
  dummy <- data.frame(
    class_label = factor(c("X", if (show_header) "" else NULL), levels = c("X", if (show_header) "" else NULL)),
    class_order = seq_len(1 + show_header), color_hex = "#888888", global_land_fraction = 0.5,
    n = 1L, network_frac = 0.5, sampling_ratio = 1, log2_sr = 0,
    is_none = FALSE, is_neither = FALSE, log2_sr_clip = 0, truncated = FALSE,
    annot_label = NA_character_, annot_x = NA_real_, annot_hjust = NA_real_,
    stringsAsFactors = FALSE
  )
  g <- draw_panel2(dummy, panel_mm_per_unit = 10, letter = "x", title_text = "Title",
                    j_val = 0.5, show_xlab = show_xlab, show_header = FALSE, label_pt = BASE_PT)
  tmp_pdf <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp_pdf, width = 10, height = 10, family = "Helvetica")
  on.exit({ grDevices::dev.off(); unlink(tmp_pdf) }, add = TRUE)
  panel_row <- g$layout$t[g$layout$name == "panel"][1]
  other_rows <- setdiff(seq_along(g$heights), panel_row)
  sum(vapply(other_rows, function(i) grid::convertHeight(g$heights[i], "mm", valueOnly = TRUE), numeric(1)))
}

## ---- Bin labels (ported from flux_bin_labels_print() in
## figure4_representativeness.R): NEE keeps its special "unvegetated"/signed
## format; GPP/TER/ET (all strictly positive at the ensemble-median scale)
## use the plain "0-cut", "cut-e1", ..., "> e5" format.
flux_bin_labels_print <- function(flux_name, cut, edges) {
  if (flux_name == "NEE") {
    labs <- c("unvegetated",
              sprintf("< %s", edges[1]),
              sprintf("%s to %s", edges[1], edges[2]), sprintf("%s to %s", edges[2], edges[3]),
              sprintf("%s to %s", edges[3], edges[4]), sprintf("%s to %s", edges[4], edges[5]),
              sprintf("> %s", edges[5]))
    labs <- gsub("-", "−", labs, fixed = TRUE)
  } else {
    labs <- c(sprintf("0–%s", cut), sprintf("%s–%s", cut, edges[1]),
              sprintf("%s–%s", edges[1], edges[2]), sprintf("%s–%s", edges[2], edges[3]),
              sprintf("%s–%s", edges[3], edges[4]), sprintf("%s–%s", edges[4], edges[5]),
              sprintf("> %s", edges[5]))
  }
  labs
}
FLUX_ORDER_MAP <- setNames(1:7, as.character(1:7))
NEE_LABEL_MAP <- setNames(flux_bin_labels_print("NEE", NEE_BAR1_GPP_CUT, nee_result$edges), as.character(1:7))
GPP_LABEL_MAP <- setNames(flux_bin_labels_print("GPP", GPP_LOW_CUT, gpp_result$edges), as.character(1:7))
TER_LABEL_MAP <- setNames(flux_bin_labels_print("TER", TER_LOW_CUT, ter_result$edges), as.character(1:7))
ET_LABEL_MAP  <- setNames(flux_bin_labels_print("ET",  ET_LOW_CUT,  et_result$edges),  as.character(1:7))

## ---- Colour ramps: NEE/ET taken verbatim from figure4_representativeness.R
## (same rasters, same panel style -- must look identical to Figure 4's own
## e/f panels). GPP/TER new to this script: same green/orange ramps
## scripts/diagnostics/flux_bin_breaks.R already chose (GPP: task-specified
## green family; TER: that script's own judgement call, Oranges family, kept
## here for consistency with the existing diagnostic rather than re-litigated).
## Bar 1 ("unvegetated"/"0-5") is the same bare/ice colour on every row.
BAR1_COLOR <- "#f7f4f9"   # Figure 4's biomass bin-1 (bare/ice) colour -- BIO7_COLORS[["1"]]
NEE_SINK_RAMP <- grDevices::colorRampPalette(c("#0b3e09", "#eaf5e4"))(5)
NEE7_COLORS <- c("1" = BAR1_COLOR, "2" = NEE_SINK_RAMP[1], "3" = NEE_SINK_RAMP[2], "4" = NEE_SINK_RAMP[3],
                  "5" = NEE_SINK_RAMP[4], "6" = NEE_SINK_RAMP[5], "7" = "#c2703a")
ET7_COLORS  <- c("1" = BAR1_COLOR, "2" = "#bcd8f4", "3" = "#82bce8",
                  "4" = "#4498d5", "5" = "#1d74b3", "6" = "#0c4f84", "7" = "#06305a")
GPP7_COLORS <- setNames(grDevices::colorRampPalette(c("#f7fcf5", "#00441b"))(7), as.character(1:7))
GPP7_COLORS[["1"]] <- BAR1_COLOR
TER7_COLORS <- setNames(grDevices::colorRampPalette(c("#fff5eb", "#7f2704"))(7), as.character(1:7))
TER7_COLORS[["1"]] <- BAR1_COLOR

## ---- Panel specs: 8 panels, letters a-h, 4 rows (flux) x 2 cols (comparison)
flux_merged_df <- function(result, comparison) {
  bin_vec <- if (comparison == "geo_vs_data") result$data_bin else result$geo_bin
  n_classified <- sum(!is.na(bin_vec))
  cnt <- as.numeric(table(factor(bin_vec, levels = 1:7)))
  data.frame(class = as.character(1:7), global_land_fraction = result$land_vec,
             n = cnt, network_frac = cnt / n_classified, stringsAsFactors = FALSE)
}
build_panel_df <- function(merged_df, order_map, label_map, color_map, total_km2) {
  merged_df |>
    dplyr::mutate(
      class_order = unname(order_map[class]),
      class_label = unname(label_map[class]),
      color_hex   = unname(color_map[class]),
      global_land_area_km2 = global_land_fraction * total_km2,
      sampling_ratio = dplyr::if_else(global_land_fraction > 0 & network_frac > 0,
                                       network_frac / global_land_fraction, NA_real_),
      log2_sr = dplyr::if_else(!is.na(sampling_ratio), log2(sampling_ratio), NA_real_)
    ) |>
    prep_ordered() |> prep_clip2()
}

flux_title_expr <- function(flux_label, unit_expr, cmp_label) {
  as.expression(bquote(.(flux_label) ~ .(unit_expr) * .(paste0(", ", cmp_label))))
}
CMP_TITLE <- c(geo_vs_geo = "gridded value at the tower", geo_vs_data = "the site's own value")

PANEL_SPECS <- list(
  A = list(letter = "a", flux = "NEE", comparison = "geo_vs_geo", result = nee_result,
           label_map = NEE_LABEL_MAP, color_map = NEE7_COLORS,
           title_expr = flux_title_expr("NEE", quote(("g C" ~ m^{-2} ~ yr^{-1})), CMP_TITLE[["geo_vs_geo"]])),
  B = list(letter = "b", flux = "NEE", comparison = "geo_vs_data", result = nee_result,
           label_map = NEE_LABEL_MAP, color_map = NEE7_COLORS,
           title_expr = flux_title_expr("NEE", quote(("g C" ~ m^{-2} ~ yr^{-1})), CMP_TITLE[["geo_vs_data"]])),
  C = list(letter = "c", flux = "GPP", comparison = "geo_vs_geo", result = gpp_result,
           label_map = GPP_LABEL_MAP, color_map = GPP7_COLORS,
           title_expr = flux_title_expr("GPP", quote(("g C" ~ m^{-2} ~ yr^{-1})), CMP_TITLE[["geo_vs_geo"]])),
  D = list(letter = "d", flux = "GPP", comparison = "geo_vs_data", result = gpp_result,
           label_map = GPP_LABEL_MAP, color_map = GPP7_COLORS,
           title_expr = flux_title_expr("GPP", quote(("g C" ~ m^{-2} ~ yr^{-1})), CMP_TITLE[["geo_vs_data"]])),
  E = list(letter = "e", flux = "RECO", comparison = "geo_vs_geo", result = ter_result,
           label_map = TER_LABEL_MAP, color_map = TER7_COLORS,
           title_expr = flux_title_expr("RECO", quote(("g C" ~ m^{-2} ~ yr^{-1})), CMP_TITLE[["geo_vs_geo"]])),
  F = list(letter = "f", flux = "RECO", comparison = "geo_vs_data", result = ter_result,
           label_map = TER_LABEL_MAP, color_map = TER7_COLORS,
           title_expr = flux_title_expr("RECO", quote(("g C" ~ m^{-2} ~ yr^{-1})), CMP_TITLE[["geo_vs_data"]])),
  G = list(letter = "g", flux = "ET", comparison = "geo_vs_geo", result = et_result,
           label_map = ET_LABEL_MAP, color_map = ET7_COLORS,
           title_expr = flux_title_expr("ET", quote((mm ~ yr^{-1})), CMP_TITLE[["geo_vs_geo"]])),
  H = list(letter = "h", flux = "ET", comparison = "geo_vs_data", result = et_result,
           label_map = ET_LABEL_MAP, color_map = ET7_COLORS,
           title_expr = flux_title_expr("ET", quote((mm ~ yr^{-1})), CMP_TITLE[["geo_vs_data"]]))
)
ALL_LETTERS <- c("A", "B", "C", "D", "E", "F", "G", "H")
PANEL_LAYOUT   <- list(row1 = c("A", "B"), row2 = c("C", "D"), row3 = c("E", "F"), row4 = c("G", "H"))
COLUMN_LAYOUT  <- list(col1 = c("A", "C", "E", "G"), col2 = c("B", "D", "F", "H"))
show_header_for <- function(letter) letter %in% PANEL_LAYOUT$row1
show_xlab_for   <- function(letter) letter %in% PANEL_LAYOUT$row4

panel_y_limits <- function(panel_letter) {
  spec <- PANEL_SPECS[[panel_letter]]
  labs <- unname(spec$label_map[order(FLUX_ORDER_MAP)])
  if (show_header_for(panel_letter)) labs <- c(labs, "")
  labs
}
get_j_panel <- function(panel_letter) {
  spec <- PANEL_SPECS[[panel_letter]]
  if (spec$comparison == "geo_vs_data") spec$result$j_data else spec$result$j_geo
}
build_flux_panel <- function(panel_letter) {
  spec <- PANEL_SPECS[[panel_letter]]
  merged <- flux_merged_df(spec$result, spec$comparison)
  df <- build_panel_df(merged, FLUX_ORDER_MAP, spec$label_map, spec$color_map, FLUX_LAND_TOTAL_KM2)
  show_xlab   <- show_xlab_for(panel_letter)
  show_header <- show_header_for(panel_letter)
  j_val <- get_j_panel(panel_letter)
  g <- draw_panel2(df, PANEL_MM_PER_UNIT[[panel_letter]], spec$letter, spec$title_expr, j_val,
                    show_xlab = show_xlab, show_header = show_header,
                    label_pt = BASE_PT, target_left_mm = TARGET_GUTTER_MM[[panel_letter]])
  list(df = df, grob = g, n_rows = nlevels(df$class_label) + if (show_header) 1L else 0L)
}

## ---- Column alignment (same mechanism as Figure 4: align the y-axis label
## gutter within each output column so the 1x gridline lines up vertically).
panel_layout_mm <- setNames(
  lapply(ALL_LETTERS, function(l) measure_panel_layout_mm(panel_y_limits(l), show_xlab_for(l))),
  ALL_LETTERS
)
gutter_mm <- vapply(panel_layout_mm, `[[`, numeric(1), "left_mm")
TARGET_GUTTER_MM <- setNames(rep(NA_real_, length(ALL_LETTERS)), ALL_LETTERS)
for (col in COLUMN_LAYOUT) TARGET_GUTTER_MM[col] <- max(gutter_mm[col])
msg("Column gutter widths (mm): col1 (a/c/e/g) -> ", round(TARGET_GUTTER_MM[["A"]], 2),
    "mm; col2 (b/d/f/h) -> ", round(TARGET_GUTTER_MM[["B"]], 2), "mm")

PANEL_MM_PER_UNIT <- setNames(
  vapply(ALL_LETTERS, function(l) panel_mm_per_unit_for(panel_y_limits(l), show_xlab_for(l), TARGET_GUTTER_MM[[l]]), numeric(1)),
  ALL_LETTERS
)

built <- lapply(ALL_LETTERS, build_flux_panel)
names(built) <- ALL_LETTERS

## ---- Per-panel tables (same convention as figure4_representativeness.R)
write_panel_table <- function(panel_letter, df) {
  spec <- PANEL_SPECS[[panel_letter]]
  tab <- df |> dplyr::filter(!is_neither) |> dplyr::transmute(
    bin_label = as.character(class_label), land_area_km2 = global_land_area_km2,
    land_fraction = global_land_fraction, towers = dplyr::coalesce(n, 0L),
    tower_fraction = dplyr::coalesce(network_frac, 0)
  )
  out <- file.path(FIG4_TABLES_DIR, sprintf("table_%s_supp_flux_representativeness.csv", spec$letter))
  readr::write_csv(tab, out)
  write_output_metadata(
    out, input_sources = c(metrics_supp_path),
    notes = sprintf("Panel %s (%s, %s), figure stage 4.", spec$letter, spec$flux, CMP_TITLE[[spec$comparison]])
  )
}
for (letter in ALL_LETTERS) write_panel_table(letter, built[[letter]]$df)

## ---- Row heights (4 rows now, same overhead/pitch technique as Figure 4)
overhead_header <- measure_panel_overhead_mm(show_xlab = FALSE, show_header = TRUE)
overhead_mid     <- measure_panel_overhead_mm(show_xlab = FALSE, show_header = FALSE)
overhead_xlab    <- measure_panel_overhead_mm(show_xlab = TRUE,  show_header = FALSE)
n_row1 <- max(built$A$n_rows, built$B$n_rows)
n_row2 <- max(built$C$n_rows, built$D$n_rows)
n_row3 <- max(built$E$n_rows, built$F$n_rows)
n_row4 <- max(built$G$n_rows, built$H$n_rows)
h_row1 <- overhead_header + n_row1 * ROW_PITCH_MM
h_row2 <- overhead_mid    + n_row2 * ROW_PITCH_MM
h_row3 <- overhead_mid    + n_row3 * ROW_PITCH_MM
h_row4 <- overhead_xlab   + n_row4 * ROW_PITCH_MM
TOP_PAD_MM <- 6      # same empirically-derived padding as figure4_representativeness.R
BOTTOM_PAD_MM <- 10
total_height_mm <- h_row1 + h_row2 + h_row3 + h_row4 + TOP_PAD_MM + BOTTOM_PAD_MM
msg("Row heights (mm): row1=", round(h_row1, 1), " row2=", round(h_row2, 1),
    " row3=", round(h_row3, 1), " row4=", round(h_row4, 1), "; +", TOP_PAD_MM, "+", BOTTOM_PAD_MM,
    " padding; TOTAL=", round(total_height_mm, 1), " mm (hard limit 240mm)")
if (total_height_mm > 240) {
  stop("Figure height ", round(total_height_mm, 1), " mm exceeds the 240mm SupFig hard limit.")
}

grobs <- list(patchwork::plot_spacer(), patchwork::plot_spacer(),
              built$A$grob, built$B$grob, built$C$grob, built$D$grob,
              built$E$grob, built$F$grob, built$G$grob, built$H$grob,
              patchwork::plot_spacer(), patchwork::plot_spacer())
composite <- patchwork::wrap_plots(grobs, ncol = 2,
                                    heights = grid::unit(c(TOP_PAD_MM, h_row1, h_row2, h_row3, h_row4, BOTTOM_PAD_MM), "mm"))

fig_stem <- file.path(FIG_DIR, "supp_flux_representativeness")
saved <- save_nature_figure(composite, fig_stem, width_mm = FIG_WIDTH_MM, height_mm = total_height_mm, extended_data = TRUE)
msg("Saved: ", saved$png, ", ", saved$pdf, ", ", saved$jpeg)

# ==============================================================================
# STEP 7: Legend, metadata, copy to SupFigs/
# ==============================================================================
msg("\n=== STEP 7: Legend, metadata, SupFigs copy ===")
panel_n_line <- function(letter) {
  spec <- PANEL_SPECS[[letter]]
  n <- if (spec$comparison == "geo_vs_data") spec$result$n_data else spec$result$n_geo
  j <- get_j_panel(letter)
  sprintf("  %s %s (%s): n = %d / 781, J = %.3f", spec$letter, spec$flux, CMP_TITLE[[spec$comparison]], n, j)
}
n_lines <- vapply(ALL_LETTERS, panel_n_line, character(1))

legend_lines <- c(
  sprintf("FIGURE LEGEND — %s", basename(paste0(fig_stem, ".png"))),
  strrep("=", 60), "",
  "TITLE: Supplementary Figure S4 — Network sampling of the current FLUXNET network (n=781), fluxes", "",
  "PUBLICATION LEGEND:",
  "Supplementary Figure S4. Network sampling of the snapshot for four fluxes -- net ecosystem",
  "exchange, gross primary productivity, ecosystem respiration and evapotranspiration -- each",
  "compared using both the gridded value at the tower and the site's own value. Extends the main",
  "text's sampling figure (which covers net ecosystem exchange and evapotranspiration) to add",
  "gross primary productivity and ecosystem respiration. Network sampling analysis is described",
  "in Section 2.7.", "",
  "DESCRIPTION:",
  "Companion to Figure 5 (fig_05_representativeness.png) and its gridded-value-at-the-tower companion,",
  "Supplementary Figure S3 (figS3_sampling_gridded_at_tower.png) -- same panel style, bar-label",
  "conventions, and \"gridded value at the tower\"/\"the site's own value\" definitions (what Figure 5's",
  "legend itself still calls \"Geo vs Geo\"/\"Geo vs Data\" -- see Figure 5's legend for the full",
  "definitions). Eight panels, four",
  "fluxes (rows) x two comparisons (columns): a/b NEE, c/d GPP, e/f RECO (ecosystem respiration, ra+rh),",
  "g/h ET; left column (a, c, e, g) is gridded value at the tower, right column (b, d, f, h) is the",
  "site's own value.",
  "Panels a, b (NEE) and g, h (ET) are reproduced unchanged from Figure 5's own panels e and f --",
  "identical rasters, tower values and bins; their n and J are confirmed programmatically (not just",
  "visually) to equal representativeness_metrics_fig4.csv rows E/F exactly before this figure is drawn.",
  "Panels c, d (GPP) and e, f (RECO) are new to this figure.", "",
  "TOWER VALUES:",
  sprintf("Each tower's value is the median of its QC_THRESHOLD_YY=%s-qualifying annual values", QC_THRESHOLD_YY),
  "(R/site_annual_fluxes.R::compute_site_annual_fluxes()): NEE/GPP/RECO gated on the per-site",
  "VUT/CUT-chosen NEE QC column (scripts/04_qc.R's rule); ET gated on LE_F_MDS_QC, independent of the",
  "NEE gate. In \"the site's own value\", a tower is only classified (including bar 1) if it has a",
  "qualifying tower value for that flux -- a tower with no NEE years, say, is never counted via its",
  "model-GPP mask value alone.", "",
  "MODEL VALUES AND LAND GRID:",
  "TRENDY v14 S3, 17-model ensemble median, 1991-2020 mean, on the Beck 2023 Koppen-Geiger 0.5 deg land",
  "mask (same raster footprint for all four fluxes; RECO = ra+rh). NEE and ET use",
  "data/external/trendy/derived/trendy_nee_fluxbased_median.tif and",
  "flux_bin_breaks_et_median_1991_2020.tif (Figure 5's own rasters); GPP and RECO use",
  "candidate_gpp_median.tif and candidate_ter_median.tif (scripts/candidate_nee_gpp_ter_panels.R).",
  sprintf("Land total: %s km2.", format(round(FLUX_LAND_TOTAL_KM2), big.mark = ",")), "",
  "BINS:",
  "Bar 1 is \"unvegetated\" for NEE (own model GPP < 5 gC/m2/yr -- NEE is signed and cannot be cut on its",
  "own magnitude) or \"0-5\" for GPP/RECO (gC/m2/yr)/ET (mm/yr) -- each flux's own model value at that",
  "cell/tower. Bars 2-7 are the sextiles (5 edges) of the 50/50 land/tower mixture CDF outside bar 1,",
  "rounded to the nearest 25 (NEE), 100 (GPP, RECO) or 50 (ET) gC m-2 yr-1 or mm yr-1 -- same rounding",
  "steps as scripts/diagnostics/flux_bin_breaks.R. Edges differ from that script's own GPP/RECO edges",
  "because the tower values here use compute_site_annual_fluxes()/QC_THRESHOLD_YY, not that script's",
  "older QC>=0.80 mean-monthly-cycle method.", "",
  "BAR LABELS: as Figure 5 -- log2 sampling ratio, clipped at +-5x with an exact-ratio annotation at the",
  "clip; faint gridlines at 1/5x, 1/2x, 2x, 5x; \"% land\"/\"towers\" column headers above panels a/b only",
  "(same two-number convention applies to every row); J (weighted Jaccard) right-aligned per panel; the",
  "bottom row's x axis reads \"smaller proportion\"/\"greater proportion\" of towers vs. land.", "",
  sprintf("Final artwork size: %g mm wide x %.1f mm tall, Helvetica throughout. Supplementary Figure", FIG_WIDTH_MM, total_height_mm),
  "(target journal Scientific Data has no Extended Data concept).", "",
  "PER-PANEL n AND J:", n_lines, "",
  "SOURCE: scripts/figure_flux_representativeness_supp.R. Per-panel tables in",
  "review/figures/representativeness/tables/. Metrics table:",
  "data/snapshots/representativeness_metrics_flux_supp.csv. Vector PDF alongside this PNG."
)
writeLines(unlist(legend_lines), paste0(fig_stem, ".legend.txt"))

write_output_metadata(
  saved$png,
  input_sources = c("representativeness_metrics_fig4.csv", "representativeness_metrics_flux_supp.csv",
                     "trendy_nee_fluxbased_median.tif", "candidate_gpp_median.tif",
                     "candidate_ter_median.tif", "flux_bin_breaks_et_median_1991_2020.tif"),
  notes = sprintf(
    paste0("Figure stage 4 (logs/figstage_prompt.md): supp_flux_representativeness, renumbered ",
           "Supplementary Figure S5 under figure stage 6 (2026-10-02), re-numbered Supplementary ",
           "Figure S4 in the supplementary material restructure (2026-10-08, SESSION_LOG.md), ",
           "%g mm wide x %.1f mm tall, Helvetica, %dpt text (row pitch %.1fmm). NEE/ET panels ",
           "(a/b/g/h) confirmed identical to ",
           "Figure 5 (representativeness_metrics_fig4.csv rows E/F) before rendering. GPP/RECO (c/d/e/f) new."),
    FIG_WIDTH_MM, total_height_mm, BASE_PT, ROW_PITCH_MM
  )
)
msg("Saved: ", saved$png, ".meta.json and .legend.txt")

DEST_BASE <- "figS4_sampling_flux_axes"
for (ext in c(".png", ".pdf", ".jpg", ".meta.json", ".legend.txt")) {
  src_file <- paste0(fig_stem, ext)
  dst_file <- file.path(SUPFIGS_DIR, paste0(DEST_BASE, ext))
  fs::file_copy(src_file, dst_file, overwrite = TRUE)
  if (ext == ".legend.txt") {
    ## Figure stage 6 (2026-10-02): rewrite only the self-referential header
    ## line to this copy's own renumbered filename; the legend BODY already
    ## says "Supplementary Figure S5"/"Figure 5" directly (written above),
    ## since fig_stem there is the FIG_DIR source path.
    txt <- readLines(dst_file)
    txt <- sub("^FIGURE LEGEND .*$", paste0("FIGURE LEGEND \u2014 ", DEST_BASE, ".png"), txt)
    writeLines(txt, dst_file)
  }
}
msg("Copied supp_flux_representativeness -> ", DEST_BASE, " (.png/.pdf/.jpg/.meta.json/.legend.txt) to ", SUPFIGS_DIR)

msg("\n=== figure_flux_representativeness_supp.R: COMPLETE ===")
