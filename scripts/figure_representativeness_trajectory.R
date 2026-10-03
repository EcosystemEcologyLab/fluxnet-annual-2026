## figure_representativeness_trajectory.R
##
## Stage 3 of the unattended figure-production run (logs/figstage_prompt.md):
## "Geo vs Geo representativeness through time", a supplementary trajectory
## figure replacing the retired Figure 5
## (draft_manuscript_v1/deprecated/fig_05_jaccard_trajectory_with_counts.png).
##
## Four network generations (Marconi, La Thuile, FLUXNET2015, current 781
## sites) x Figure 4's own six Geo-vs-Geo axes (A Koppen-Geiger, B land cover
## as IGBP, C aridity, D biomass, E NEE, F ET). Every site is given the
## gridded product's own value at its coordinate (never a tower-measured
## value -- that is the Geo-vs-Data side of Figure 4, out of scope here).
##
## Classes, bin edges, land grids/totals and the weighted-Jaccard "classified
## sites as denominator" convention are taken AS-IS from
## scripts/figure4_representativeness.R and its committed data/snapshots/
## tables -- reproduced (ported) below exactly where code is needed, never
## redefined, per the stage's explicit instruction. In particular:
##   - Koppen/aridity/biomass global land-side distributions and per-site
##     classes for the three historical networks already exist in
##     data/snapshots/ (site_koppen_beck2023_<net>.csv,
##     site_aridity_<net>.csv, site_biomass_cci_v7_<net>.csv) -- confirmed
##     by direct inspection to use the same products/schemes
##     figure4_representativeness.R uses for the current network (Beck et
##     al. 2023 KG 1991-2020 1km; CGIAR Aridity Index v3.1; ESA CCI Biomass
##     v7.0, same biomass_bin breakpoints) -- read directly, NOT re-extracted.
##   - IGBP (panel B) and model NEE/GPP/ET (panels E-F) have no existing
##     historical-network extraction and are computed fresh here, by the
##     same method figure4_representativeness.R uses for the current
##     network (IGBP: nearest-cell terra::extract() at native MODIS
##     resolution; NEE/GPP/ET: bilinear terra::extract()) -- same rasters,
##     same CRS fix, same classify_flux_sites() logic, ported verbatim.
##   - The NEE/ET bin edges are FIXED and already published (rounded
##     sextiles of the current network's own 50/50 geo/tower mixture CDF,
##     computed once in figure4_representativeness.R) -- read back out of
##     site_nee_fig4.meta.json / site_et_fig4.meta.json's `notes` field, with
##     a defensive grepl() check against the hardcoded values below, rather
##     than recomputed. The land-side 7-bin distribution for NEE/ET (also
##     network-independent) is read directly from the already-committed
##     data/snapshots/nee_et_fig4_global_distribution.csv.
##
## Validation (required by the stage): the current-network (781) values
## computed here must reproduce the six Geo-vs-Geo rows of
## data/snapshots/representativeness_metrics_fig4.csv exactly (n and J) --
## stop() if not, per logs/figstage_prompt.md rule 9.

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
source("R/utils.R")
source("R/nature_format.R")
source("R/plot_constants.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(terra)
  library(ggplot2)
  library(jsonlite)
  library(fs)
})

SNAP_DIR    <- "data/snapshots"
EXT         <- "data/external"
FIG_DIR     <- "review/figures/representativeness"
SUPFIGS_DIR <- "review/figures/draft_manuscript_v1/SupFigs"
fs::dir_create(FIG_DIR)
fs::dir_create(SUPFIGS_DIR)

LOG_START <- format(Sys.time(), "%Y%m%d_%H%M%S")
LOG_FILE  <- file.path("logs", paste0("figure_representativeness_trajectory_", LOG_START, ".log"))
con_log <- file(LOG_FILE, open = "wt")
sink(con_log, type = "output")
sink(con_log, type = "message", append = TRUE)
on.exit({ sink(type = "message"); sink(type = "output"); close(con_log) }, add = TRUE)
msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)

msg("=== figure_representativeness_trajectory.R ===")
msg("Log: ", LOG_FILE)

weighted_jaccard <- function(p, q) sum(pmin(p, q)) / sum(pmax(p, q))

# ==============================================================================
# 1. Network site lists (location only -- classes/values joined on per axis)
# ==============================================================================
NETWORKS <- c("marconi", "la_thuile", "fluxnet2015", "current")
NETWORK_LABELS <- c(marconi = "Marconi", la_thuile = "La Thuile",
                     fluxnet2015 = "FLUXNET2015", current = "Current")
## Same pin figure4_representativeness.R uses for the current network.
CURRENT_SNAPSHOT <- "data/snapshots/fluxnet_shuttle_snapshot_20260901T094522.csv"

site_list_path <- function(net) {
  if (net == "current") return(CURRENT_SNAPSHOT)
  file.path(SNAP_DIR, paste0("sites_", net, "_clean.csv"))
}
read_site_coords <- function(net) {
  readr::read_csv(site_list_path(net), show_col_types = FALSE) |>
    dplyr::distinct(site_id, location_lat, location_long)
}
site_coords <- stats::setNames(lapply(NETWORKS, read_site_coords), NETWORKS)
n_network <- vapply(site_coords, nrow, integer(1))
msg("Network sizes: ", paste(names(n_network), n_network, sep = "=", collapse = ", "))
EXPECTED_N <- c(marconi = 35L, la_thuile = 252L, fluxnet2015 = 212L, current = 781L)
if (!isTRUE(all.equal(n_network[names(EXPECTED_N)], EXPECTED_N))) {
  warning("Network sizes differ from expected 35/252/212/781 -- got: ",
          paste(n_network, collapse = ", "))
}

# ==============================================================================
# 2. Generic categorical-axis metric (panels A koppen, B igbp, C aridity,
#    D biomass) -- ports figure4_representativeness.R's own merge/coalesce/J
#    logic exactly: network_frac denominator is n_eligible (nrow of the
#    network's site table for this axis), NOT n_classified; J is computed
#    over ALL global classes, missing/zero network classes coalesced to 0.
# ==============================================================================
compute_categorical_metric <- function(sites_df, class_col, global_df) {
  classes_raw <- as.character(sites_df[[class_col]])
  n_eligible  <- nrow(sites_df)
  valid       <- !is.na(classes_raw)
  n_classified <- sum(valid)
  cnt <- data.frame(class = classes_raw[valid], stringsAsFactors = FALSE) |>
    dplyr::count(class, name = "n") |>
    dplyr::mutate(network_frac = n / n_eligible)
  merged <- global_df |>
    dplyr::mutate(class = as.character(class)) |>
    dplyr::select(class, global_land_fraction) |>
    dplyr::full_join(cnt, by = "class") |>
    dplyr::mutate(n = dplyr::coalesce(n, 0L),
                  network_frac = dplyr::coalesce(network_frac, 0),
                  global_land_fraction = dplyr::coalesce(global_land_fraction, 0))
  j <- weighted_jaccard(merged$global_land_fraction, merged$network_frac)
  list(n_eligible = n_eligible, n_classified = n_classified, j = j,
       unclassified_site_ids = sites_df$site_id[!valid])
}

# ==============================================================================
# 3. Panel A: Koppen-Geiger (13-class, two-letter)
# ==============================================================================
msg("\n=== Panel A: Koppen-Geiger ===")
kg_global <- readr::read_csv(file.path(SNAP_DIR, "koppen_beck2023_global_distribution.csv"),
                              show_col_types = FALSE) |>
  dplyr::group_by(koppen_twoletter) |>
  dplyr::summarise(global_land_area_km2 = sum(global_land_area_km2),
                    global_land_fraction = sum(global_land_fraction), .groups = "drop") |>
  dplyr::rename(class = koppen_twoletter)
KG_LAND_TOTAL_KM2 <- sum(kg_global$global_land_area_km2)

koppen_site_path <- function(net) {
  if (net == "current") return(file.path(SNAP_DIR, "site_koppen_beck2023.csv"))
  file.path(SNAP_DIR, paste0("site_koppen_beck2023_", net, ".csv"))
}
koppen_sites <- stats::setNames(
  lapply(NETWORKS, function(net) readr::read_csv(koppen_site_path(net), show_col_types = FALSE)),
  NETWORKS
)
koppen_results <- lapply(NETWORKS, function(net) {
  r <- compute_categorical_metric(koppen_sites[[net]], "koppen_twoletter", kg_global)
  msg("  ", net, ": n=", r$n_classified, "/", r$n_eligible, " J=", round(r$j, 4))
  r
})
names(koppen_results) <- NETWORKS

# ==============================================================================
# 4. Panel B: IGBP land cover (MODIS MCD12C1 at tower, nearest cell) --
#    NEW extraction for the three historical networks; current network
#    reproduced fresh (not reused from site_igbp_fig4.csv) as the stage's
#    required validation of the raster/method itself.
# ==============================================================================
msg("\n=== Panel B: IGBP (MODIS MCD12C1 at tower) ===")
IGBP_CODE_TO_CLASS <- c(
  "0" = "Other", "1" = "ENF", "2" = "EBF", "3" = "DNF", "4" = "DBF", "5" = "MF",
  "6" = "CSH", "7" = "OSH", "8" = "WSA", "9" = "SAV", "10" = "GRA", "11" = "WET",
  "12" = "CRO", "13" = "Other", "14" = "CVM", "15" = "SNO", "16" = "BSV"
)
igbp_global <- readr::read_csv(file.path(SNAP_DIR, "igbp_mcd12c1_global_distribution.csv"),
                                show_col_types = FALSE)
IGBP_LAND_TOTAL_KM2 <- sum(igbp_global$area_km2)

modis_path <- file.path(EXT, "modis_landcover", "MCD12C1.A2022001.061.2023244164746.hdf")
modis_igbp <- terra::sds(modis_path)[1]
## The HDF4 metadata mislabels the datum -- same fix figure4_representativeness.R
## applies before any extraction.
terra::crs(modis_igbp) <- "EPSG:4326"

extract_igbp <- function(coords) {
  pts <- terra::vect(data.frame(x = coords$location_long, y = coords$location_lat),
                      geom = c("x", "y"), crs = "EPSG:4326")
  code <- terra::extract(modis_igbp, pts, ID = FALSE)[[1]]
  coords |> dplyr::mutate(igbp_modis_code = code,
                           igbp_modis_class = IGBP_CODE_TO_CLASS[as.character(code)])
}
igbp_sites <- stats::setNames(lapply(NETWORKS, function(net) extract_igbp(site_coords[[net]])), NETWORKS)
igbp_results <- lapply(NETWORKS, function(net) {
  r <- compute_categorical_metric(igbp_sites[[net]], "igbp_modis_class", igbp_global)
  msg("  ", net, ": n=", r$n_classified, "/", r$n_eligible, " J=", round(r$j, 4))
  r
})
names(igbp_results) <- NETWORKS

## ---- Validation against the committed site_igbp_fig4.csv (current network) --
fig4_igbp <- readr::read_csv(file.path(SNAP_DIR, "site_igbp_fig4.csv"), show_col_types = FALSE)
igbp_check <- igbp_sites$current |>
  dplyr::select(site_id, igbp_modis_class_new = igbp_modis_class) |>
  dplyr::inner_join(dplyr::select(fig4_igbp, site_id, igbp_modis_class), by = "site_id")
n_igbp_mismatch <- sum(igbp_check$igbp_modis_class_new != igbp_check$igbp_modis_class, na.rm = TRUE) +
  sum(is.na(igbp_check$igbp_modis_class_new) != is.na(igbp_check$igbp_modis_class))
if (n_igbp_mismatch > 0) {
  stop("Panel B validation FAILED: ", n_igbp_mismatch,
       " current-network sites' freshly-extracted MODIS IGBP class differs from ",
       "the committed site_igbp_fig4.csv -- raster/method mismatch, stage cannot proceed.")
}
msg("  Panel B validation: fresh current-network MODIS extraction matches site_igbp_fig4.csv exactly (",
    nrow(igbp_check), " sites compared).")

# ==============================================================================
# 5. Panel C: Aridity (CGIAR Aridity Index v3.1, 7-class UNEP scheme)
# ==============================================================================
msg("\n=== Panel C: Aridity ===")
aridity_global <- readr::read_csv(file.path(SNAP_DIR, "aridity_unep7_global_distribution.csv"),
                                   show_col_types = FALSE) |>
  dplyr::rename(class = unep_class)
ARIDITY_LAND_TOTAL_KM2 <- sum(aridity_global$global_land_area_km2)

aridity_site_path <- function(net) {
  if (net == "current") return(file.path(SNAP_DIR, "site_aridity.csv"))
  file.path(SNAP_DIR, paste0("site_aridity_", net, ".csv"))
}
aridity_sites <- stats::setNames(
  lapply(NETWORKS, function(net) readr::read_csv(aridity_site_path(net), show_col_types = FALSE)),
  NETWORKS
)
aridity_results <- lapply(NETWORKS, function(net) {
  r <- compute_categorical_metric(aridity_sites[[net]], "unep_class_7", aridity_global)
  msg("  ", net, ": n=", r$n_classified, "/", r$n_eligible, " J=", round(r$j, 4))
  r
})
names(aridity_results) <- NETWORKS

# ==============================================================================
# 6. Panel D: Biomass (ESA CCI Biomass v7.0, 7-bin hybrid scheme)
# ==============================================================================
msg("\n=== Panel D: Biomass ===")
biomass_global <- readr::read_csv(file.path(SNAP_DIR, "biomass_cci_v7_global_distribution.csv"),
                                   show_col_types = FALSE) |>
  dplyr::mutate(class = as.character(biomass_bin))
BIOMASS_LAND_TOTAL_KM2 <- sum(biomass_global$global_land_area_km2)

biomass_site_path <- function(net) {
  if (net == "current") return(file.path(SNAP_DIR, "site_biomass_cci_v7.csv"))
  file.path(SNAP_DIR, paste0("site_biomass_cci_v7_", net, ".csv"))
}
biomass_sites <- stats::setNames(
  lapply(NETWORKS, function(net) readr::read_csv(biomass_site_path(net), show_col_types = FALSE)),
  NETWORKS
)
biomass_results <- lapply(NETWORKS, function(net) {
  r <- compute_categorical_metric(biomass_sites[[net]], "biomass_bin", biomass_global)
  msg("  ", net, ": n=", r$n_classified, "/", r$n_eligible, " J=", round(r$j, 4))
  r
})
names(biomass_results) <- NETWORKS

# ==============================================================================
# 7. Panels E-F: model NEE and ET (TRENDY v14 ensemble-median, 0.5 deg,
#    Koppen land mask) -- bilinear extraction at every network's tower
#    coordinates; FIXED, already-published bin edges (figure4_representativeness.R)
#    read back from the committed .meta.json notes, with a defensive
#    substring check -- NEVER recomputed. classify_flux_sites() ported
#    verbatim (require_own = FALSE, i.e. Geo vs Geo).
# ==============================================================================
msg("\n=== Panels E-F: NEE and ET (TRENDY, bilinear at tower) ===")

DERIVED_DIR <- file.path(EXT, "trendy", "derived")
r_nee <- terra::rast(file.path(DERIVED_DIR, "trendy_nee_fluxbased_median.tif"))
r_gpp <- terra::rast(file.path(DERIVED_DIR, "candidate_gpp_median.tif"))
r_et  <- terra::rast(file.path(DERIVED_DIR, "flux_bin_breaks_et_median_1991_2020.tif"))

NEE_BAR1_GPP_CUT <- 5   # gC m-2 yr-1 -- figure4_representativeness.R's own constant
ET_LOW_CUT       <- 5   # mm yr-1    -- figure4_representativeness.R's own constant
NEE_EDGES <- c(-250, -100, -50, -25, 0)    # gC m-2 yr-1, rounded sextiles (fixed, published)
ET_EDGES  <- c(200, 350, 450, 600, 850)    # mm yr-1,    rounded sextiles (fixed, published)

## Defensive check: these edges/cuts must appear verbatim in the committed
## site_nee_fig4.csv / site_et_fig4.csv .meta.json notes -- never redefined
## here. stop() if the committed notes have changed without this script
## being updated to match.
nee_notes <- jsonlite::fromJSON(file.path(SNAP_DIR, "site_nee_fig4.meta.json"))$notes
et_notes  <- jsonlite::fromJSON(file.path(SNAP_DIR, "site_et_fig4.meta.json"))$notes
if (!grepl(paste0("cut=", NEE_BAR1_GPP_CUT, " gC/m2/yr"), nee_notes, fixed = TRUE) ||
    !grepl(paste(NEE_EDGES, collapse = ", "), nee_notes, fixed = TRUE)) {
  stop("NEE_BAR1_GPP_CUT/NEE_EDGES do not match site_nee_fig4.meta.json notes -- re-check ",
       "figure4_representativeness.R before proceeding (do not redefine bin edges here).")
}
if (!grepl(paste0("cut=", ET_LOW_CUT, " mm/yr"), et_notes, fixed = TRUE) ||
    !grepl(paste(ET_EDGES, collapse = ", "), et_notes, fixed = TRUE)) {
  stop("ET_LOW_CUT/ET_EDGES do not match site_et_fig4.meta.json notes -- re-check ",
       "figure4_representativeness.R before proceeding (do not redefine bin edges here).")
}
msg("  NEE/ET cuts and edges confirmed against site_nee_fig4.meta.json / site_et_fig4.meta.json.")

## Land side (7-bin distribution): network-independent, already committed --
## read directly rather than rebuilding from the rasters (same numbers,
## figure4_representativeness.R's own build_hist/classify pipeline already
## produced this file).
flux_global <- readr::read_csv(file.path(SNAP_DIR, "nee_et_fig4_global_distribution.csv"),
                                show_col_types = FALSE)
land_vec_nee <- flux_global |> dplyr::filter(flux == "NEE") |> dplyr::arrange(bin) |> dplyr::pull(land_fraction)
land_vec_et  <- flux_global |> dplyr::filter(flux == "ET")  |> dplyr::arrange(bin) |> dplyr::pull(land_fraction)
stopifnot(length(land_vec_nee) == 7, length(land_vec_et) == 7)

## classify_flux_sites(): ported verbatim from figure4_representativeness.R.
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

extract_flux <- function(coords) {
  m <- as.matrix(coords[, c("location_long", "location_lat")])
  coords |>
    dplyr::mutate(
      model_gpp = terra::extract(r_gpp, m, method = "bilinear")[, 1],
      model_nee = terra::extract(r_nee, m, method = "bilinear")[, 1],
      model_et  = terra::extract(r_et,  m, method = "bilinear")[, 1]
    )
}
flux_sites <- stats::setNames(lapply(NETWORKS, function(net) extract_flux(site_coords[[net]])), NETWORKS)

compute_flux_metric <- function(flux_df, mask_col, own_col, cut, edges, land_vec) {
  bin <- classify_flux_sites(flux_df[[mask_col]], flux_df[[own_col]], cut, edges, require_own = FALSE)
  n <- sum(!is.na(bin))
  fr <- as.numeric(table(factor(bin, levels = 1:7))) / n
  j <- weighted_jaccard(land_vec, fr)
  list(n_eligible = n, n_classified = n, j = j,
       unclassified_site_ids = flux_df$site_id[is.na(bin)])
}

nee_results <- lapply(NETWORKS, function(net) {
  r <- compute_flux_metric(flux_sites[[net]], "model_gpp", "model_nee", NEE_BAR1_GPP_CUT, NEE_EDGES, land_vec_nee)
  msg("  NEE ", net, ": n=", r$n_classified, "/", nrow(flux_sites[[net]]), " J=", round(r$j, 4))
  r
})
names(nee_results) <- NETWORKS

et_results <- lapply(NETWORKS, function(net) {
  r <- compute_flux_metric(flux_sites[[net]], "model_et", "model_et", ET_LOW_CUT, ET_EDGES, land_vec_et)
  msg("  ET  ", net, ": n=", r$n_classified, "/", nrow(flux_sites[[net]]), " J=", round(r$j, 4))
  r
})
names(et_results) <- NETWORKS

## ---- Validation against the committed site_nee_fig4.csv/site_et_fig4.csv
## bin_geo columns (current network) -- confirms the fresh raster
## bilinear-extraction + classify_flux_sites() pipeline here reproduces the
## production figure's own per-site classification exactly, not just the
## aggregate J.
fig4_nee <- readr::read_csv(file.path(SNAP_DIR, "site_nee_fig4.csv"), show_col_types = FALSE)
fig4_et  <- readr::read_csv(file.path(SNAP_DIR, "site_et_fig4.csv"),  show_col_types = FALSE)
nee_bin_new <- classify_flux_sites(flux_sites$current$model_gpp, flux_sites$current$model_nee,
                                    NEE_BAR1_GPP_CUT, NEE_EDGES, require_own = FALSE)
et_bin_new  <- classify_flux_sites(flux_sites$current$model_et, flux_sites$current$model_et,
                                    ET_LOW_CUT, ET_EDGES, require_own = FALSE)
nee_bin_check <- data.frame(site_id = flux_sites$current$site_id, bin_geo_new = nee_bin_new) |>
  dplyr::inner_join(dplyr::select(fig4_nee, site_id, bin_geo), by = "site_id")
et_bin_check <- data.frame(site_id = flux_sites$current$site_id, bin_geo_new = et_bin_new) |>
  dplyr::inner_join(dplyr::select(fig4_et, site_id, bin_geo), by = "site_id")
n_nee_mismatch <- sum(nee_bin_check$bin_geo_new != nee_bin_check$bin_geo, na.rm = TRUE) +
  sum(is.na(nee_bin_check$bin_geo_new) != is.na(nee_bin_check$bin_geo))
n_et_mismatch  <- sum(et_bin_check$bin_geo_new != et_bin_check$bin_geo, na.rm = TRUE) +
  sum(is.na(et_bin_check$bin_geo_new) != is.na(et_bin_check$bin_geo))
if (n_nee_mismatch > 0 || n_et_mismatch > 0) {
  stop("Panels E/F validation FAILED: ", n_nee_mismatch, " NEE and ", n_et_mismatch,
       " ET current-network bin_geo mismatches vs committed site_nee_fig4.csv/site_et_fig4.csv.")
}
msg("  Panels E/F validation: fresh bilinear extraction + classify_flux_sites() reproduces ",
    "committed bin_geo exactly for all ", nrow(nee_bin_check), " (NEE) / ", nrow(et_bin_check), " (ET) current-network sites.")

# ==============================================================================
# 8. Assemble the metrics table (24 rows: 6 axes x 4 networks)
# ==============================================================================
PANEL_SPECS <- list(
  list(panel = "A", axis = "koppen",  results = koppen_results,
       land_grid = "Beck 2023 1 km mask", land_total_km2 = KG_LAND_TOTAL_KM2),
  list(panel = "B", axis = "igbp",    results = igbp_results,
       land_grid = "MODIS MCD12C1 on Beck 2023 1 km mask", land_total_km2 = IGBP_LAND_TOTAL_KM2),
  list(panel = "C", axis = "aridity", results = aridity_results,
       land_grid = "CGIAR Aridity Index v3.1 (own coverage)", land_total_km2 = ARIDITY_LAND_TOTAL_KM2),
  list(panel = "D", axis = "biomass", results = biomass_results,
       land_grid = "Beck 2023 1 km mask (fine, 0.00833 deg)", land_total_km2 = BIOMASS_LAND_TOTAL_KM2),
  list(panel = "E", axis = "nee",     results = nee_results,
       land_grid = "TRENDY v14 ensemble-median, 0.5 deg, Koppen land mask",
       land_total_km2 = sum(dplyr::filter(readr::read_csv("data/snapshots/representativeness_metrics_fig4.csv", show_col_types = FALSE),
                                           panel == "E", comparison == "geo_vs_geo")$land_total_km2)),
  list(panel = "F", axis = "et",      results = et_results,
       land_grid = "TRENDY v14 ensemble-median, 0.5 deg, Koppen land mask",
       land_total_km2 = sum(dplyr::filter(readr::read_csv("data/snapshots/representativeness_metrics_fig4.csv", show_col_types = FALSE),
                                           panel == "F", comparison == "geo_vs_geo")$land_total_km2))
)

metrics_rows <- do.call(rbind, lapply(PANEL_SPECS, function(spec) {
  do.call(rbind, lapply(NETWORKS, function(net) {
    r <- spec$results[[net]]
    data.frame(panel = spec$panel, axis = spec$axis, comparison = "geo_vs_geo",
               network = net, land_grid = spec$land_grid, land_total_km2 = spec$land_total_km2,
               n_eligible = r$n_eligible, n_classified = r$n_classified,
               weighted_jaccard = r$j, stringsAsFactors = FALSE)
  }))
}))
rownames(metrics_rows) <- NULL
msg("\n=== Assembled metrics table: ", nrow(metrics_rows), " rows ===")
print(metrics_rows)

# ==============================================================================
# 9. REQUIRED CHECK: current-network values must reproduce the six
#    Geo-vs-Geo rows of representativeness_metrics_fig4.csv exactly.
# ==============================================================================
fig4_metrics <- readr::read_csv(file.path(SNAP_DIR, "representativeness_metrics_fig4.csv"), show_col_types = FALSE)
fig4_geo_geo <- fig4_metrics |> dplyr::filter(comparison == "geo_vs_geo") |>
  dplyr::select(panel, axis, n_eligible_fig4 = n_eligible, n_classified_fig4 = n_classified,
                j_fig4 = weighted_jaccard)
current_rows <- metrics_rows |> dplyr::filter(network == "current") |>
  dplyr::select(panel, axis, n_eligible, n_classified, weighted_jaccard)
compare <- dplyr::inner_join(current_rows, fig4_geo_geo, by = c("panel", "axis")) |>
  dplyr::mutate(n_eligible_match = n_eligible == n_eligible_fig4,
                n_classified_match = n_classified == n_classified_fig4,
                j_match = abs(weighted_jaccard - j_fig4) < 1e-9)
msg("\n=== Validation vs representativeness_metrics_fig4.csv (current network, Geo vs Geo) ===")
print(as.data.frame(compare))
if (nrow(compare) != 6L || !all(compare$n_eligible_match) || !all(compare$n_classified_match) || !all(compare$j_match)) {
  stop("STAGE 3 VALIDATION FAILED: current-network Geo-vs-Geo values do not reproduce ",
       "representativeness_metrics_fig4.csv exactly. See printed comparison above.")
}
msg("Validation PASSED: all six current-network Geo-vs-Geo rows match representativeness_metrics_fig4.csv exactly.")

# ==============================================================================
# 10. Unclassified historical sites -- report + log_unknown()
# ==============================================================================
msg("\n=== Unclassified sites by axis/network (historical networks only) ===")
unclassified_rows <- list()
collect_unclassified <- function(panel, axis, net, ids, reason) {
  if (length(ids) == 0) return(invisible(NULL))
  for (id in ids) {
    log_unknown(record_id = id, reason = paste0(axis, " (panel ", panel, ", ", net, "): ", reason),
                logged_by = "figure_representativeness_trajectory.R")
  }
  unclassified_rows[[length(unclassified_rows) + 1]] <<- data.frame(
    panel = panel, axis = axis, network = net, site_id = ids, reason = reason, stringsAsFactors = FALSE
  )
}
for (spec in PANEL_SPECS) {
  for (net in setdiff(NETWORKS, "current")) {
    ids <- spec$results[[net]]$unclassified_site_ids
    if (length(ids) == 0) next
    reason <- switch(spec$axis,
      koppen  = "Beck 2023 1 km Koppen raster class is NA in the existing site_koppen_beck2023_<net>.csv (pre-existing extraction result, not recomputed here)",
      aridity = "CGIAR Aridity Index v3.1 unep_class_7 is NA in the existing site_aridity_<net>.csv (raster NA at tower coordinate, pre-existing extraction result, not recomputed here)",
      biomass = "ESA CCI Biomass v7.0 biomass_bin is NA in the existing site_biomass_cci_v7_<net>.csv (pre-existing extraction result, not recomputed here)",
      igbp    = "MODIS MCD12C1 nearest-cell terra::extract() returned NA at the tower coordinate (likely a coastline/water pixel mismatch with the 0.05 deg MODIS grid)",
      nee     = "TRENDY bilinear terra::extract() returned NA for model GPP (mask) and/or model NEE (own value) at the tower coordinate (coastal cell outside the 0.5 deg Koppen land mask / model grid footprint)",
      et      = "TRENDY bilinear terra::extract() returned NA for model ET at the tower coordinate (coastal cell outside the 0.5 deg Koppen land mask / model grid footprint)"
    )
    collect_unclassified(spec$panel, spec$axis, net, ids, reason)
    msg("  panel ", spec$panel, " (", spec$axis, "), ", net, ": ", length(ids),
        " unclassified -- ", paste(ids, collapse = ", "))
  }
}
unclassified_df <- if (length(unclassified_rows) > 0) do.call(rbind, unclassified_rows) else
  data.frame(panel = character(), axis = character(), network = character(),
             site_id = character(), reason = character())
if (nrow(unclassified_df) == 0) msg("  (none)")

# ==============================================================================
# 11. Save the metrics table + metadata
# ==============================================================================
metrics_path <- file.path(SNAP_DIR, "representativeness_metrics_trajectory.csv")
readr::write_csv(metrics_rows, metrics_path)
write_output_metadata(
  metrics_path,
  input_sources = c(
    "data/snapshots/representativeness_metrics_fig4.csv",
    "data/snapshots/koppen_beck2023_global_distribution.csv",
    "data/snapshots/igbp_mcd12c1_global_distribution.csv",
    "data/snapshots/aridity_unep7_global_distribution.csv",
    "data/snapshots/biomass_cci_v7_global_distribution.csv",
    "data/snapshots/nee_et_fig4_global_distribution.csv",
    "data/snapshots/sites_marconi_clean.csv", "data/snapshots/sites_la_thuile_clean.csv",
    "data/snapshots/sites_fluxnet2015_clean.csv", CURRENT_SNAPSHOT,
    "data/snapshots/site_koppen_beck2023_marconi.csv", "data/snapshots/site_koppen_beck2023_la_thuile.csv",
    "data/snapshots/site_koppen_beck2023_fluxnet2015.csv", "data/snapshots/site_koppen_beck2023.csv",
    "data/snapshots/site_aridity_marconi.csv", "data/snapshots/site_aridity_la_thuile.csv",
    "data/snapshots/site_aridity_fluxnet2015.csv", "data/snapshots/site_aridity.csv",
    "data/snapshots/site_biomass_cci_v7_marconi.csv", "data/snapshots/site_biomass_cci_v7_la_thuile.csv",
    "data/snapshots/site_biomass_cci_v7_fluxnet2015.csv", "data/snapshots/site_biomass_cci_v7.csv",
    "data/external/modis_landcover/MCD12C1.A2022001.061.2023244164746.hdf",
    "data/external/trendy/derived/trendy_nee_fluxbased_median.tif",
    "data/external/trendy/derived/candidate_gpp_median.tif",
    "data/external/trendy/derived/flux_bin_breaks_et_median_1991_2020.tif"
  ),
  notes = paste0(
    "Stage 3 of the unattended figure-production run (logs/figstage_prompt.md): Geo vs Geo ",
    "representativeness through time, Figure 4's six axes, four network generations (Marconi 35, ",
    "La Thuile 252, FLUXNET2015 212, current 781). Classes/edges/land grids/totals/J-definition taken ",
    "from scripts/figure4_representativeness.R and its committed tables, not redefined. New raster ",
    "extractions for the three historical networks: MODIS MCD12C1 IGBP (nearest cell) and TRENDY model ",
    "NEE/GPP/ET (bilinear), same method/rasters figure4_representativeness.R uses for the current ",
    "network. Koppen/aridity/biomass for the historical networks reuse existing site_*_<net>.csv tables ",
    "(confirmed same product/class scheme). Current-network values validated to reproduce ",
    "representativeness_metrics_fig4.csv's six Geo-vs-Geo rows exactly (see script log). Unclassified ",
    "historical sites logged to outputs/unknown_log.csv via log_unknown(); see also ",
    nrow(unclassified_df), " rows collected in-script (listed in the script's own log file)."
  )
)
msg("Saved: ", metrics_path)

# ==============================================================================
# 12. Figure: J against network for the six axes, with site counts
# ==============================================================================
msg("\n=== Building trajectory figure ===")

## Colour assignment (DECISION FOR DAVE -- see review/figstage_status.md):
## Figure 4 colours its six panels by CLASS, not by panel, so there is no
## existing panel-level colour to reuse directly. The retired fig_05 used a
## 6-colour Okabe-Ito (colourblind-safe) palette for its own 6 axes (KG,
## LULC, Aridity, Biomass, TRENDY NEE-IAV, TRENDY ET-median) -- the same
## colourblind-safe palette and the same per-axis ROLE (one colour per
## conceptual axis: climate class, land-cover class, aridity class, biomass,
## NEE, ET) is reused here, least-change: IGBP takes the role (and colour)
## the retired figure's LULC axis had, since both are land-cover axes.
TRAJ_COLORS <- c(
  koppen  = "#D55E00",  # vermilion
  igbp    = "#CC79A7",  # reddish-pink (LULC's role in the retired fig_05)
  aridity = "#E69F00",  # amber
  biomass = "#009E73",  # green
  nee     = "#0072B2",  # blue
  et      = "#56B4E9"   # sky blue
)
AXIS_LABELS <- c(koppen = "Koppen-Geiger", igbp = "Land cover (IGBP)", aridity = "Aridity",
                  biomass = "Biomass", nee = "NEE", et = "ET")

traj_df <- metrics_rows |>
  dplyr::mutate(
    net_x = match(network, NETWORKS),
    axis_label = factor(AXIS_LABELS[axis], levels = unname(AXIS_LABELS[names(TRAJ_COLORS)]))
  )
colors_by_label <- stats::setNames(unname(TRAJ_COLORS), unname(AXIS_LABELS[names(TRAJ_COLORS)]))

MAX_N <- max(n_network)
bars_df <- data.frame(net_x = seq_along(NETWORKS), y_scaled = unname(n_network[NETWORKS]) / MAX_N)

p <- ggplot2::ggplot() +
  ggplot2::geom_col(data = bars_df, ggplot2::aes(x = net_x, y = y_scaled),
                     fill = "#d9d9d9", colour = "black", linewidth = nature_lwd(0.3),
                     alpha = 0.6, width = 0.38, inherit.aes = FALSE) +
  ggplot2::geom_line(data = traj_df,
                      ggplot2::aes(x = net_x, y = weighted_jaccard, colour = axis_label, group = axis_label),
                      linewidth = nature_lwd(0.7), na.rm = TRUE) +
  ggplot2::geom_point(data = traj_df,
                       ggplot2::aes(x = net_x, y = weighted_jaccard, colour = axis_label, group = axis_label),
                       size = 1.4, na.rm = TRUE) +
  ggplot2::scale_colour_manual(name = NULL, values = colors_by_label) +
  ggplot2::scale_x_continuous(breaks = seq_along(NETWORKS), labels = unname(NETWORK_LABELS[NETWORKS]),
                               limits = c(0.55, length(NETWORKS) + 0.45), expand = ggplot2::expansion(mult = 0)) +
  ggplot2::scale_y_continuous(
    name = "Weighted Jaccard (J)", limits = c(0, 1), breaks = seq(0, 1, 0.25),
    expand = ggplot2::expansion(mult = c(0, 0.04)),
    sec.axis = ggplot2::sec_axis(~ . * MAX_N, name = "n sites",
                                  breaks = c(0, 200, 400, 600, 800))
  ) +
  ggplot2::labs(x = NULL) +
  nature_theme() +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.title.y.right = ggplot2::element_text(colour = "grey40"),
    axis.text.y.right  = ggplot2::element_text(colour = "grey40"),
    axis.ticks.y.right = ggplot2::element_line(colour = "grey60", linewidth = nature_lwd(NATURE_LINEWIDTH_MIN)),
    axis.ticks = ggplot2::element_line(colour = "black", linewidth = nature_lwd(NATURE_LINEWIDTH_MIN)),
    axis.line  = ggplot2::element_line(colour = "black", linewidth = nature_lwd(NATURE_LINEWIDTH_MIN)),
    legend.position = c(0.22, 0.80),
    legend.background = ggplot2::element_rect(fill = "white", colour = "black", linewidth = nature_lwd(NATURE_LINEWIDTH_MIN)),
    legend.key = ggplot2::element_rect(fill = "white"),
    legend.key.size = grid::unit(3.2, "mm"),
    legend.margin = ggplot2::margin(2, 3, 2, 3)
  ) +
  ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(linewidth = nature_lwd(0.7), size = 1.4)))

FIG_WIDTH_MM  <- 120
FIG_HEIGHT_MM <- 100
fig_stem <- file.path(FIG_DIR, "supp_representativeness_trajectory")
out_files <- save_nature_figure(p, fig_stem, width_mm = FIG_WIDTH_MM, height_mm = FIG_HEIGHT_MM, extended_data = TRUE)
msg("Saved figure: ", out_files$png, " / ", out_files$pdf, " / ", out_files$jpeg)

# ==============================================================================
# 13. Legend (.legend.txt) -- same convention as supp_representativeness_geo_vs_geo
# ==============================================================================
legend_lines <- c(
  "FIGURE LEGEND — figS6_representativeness_trajectory.png",
  strrep("=", 60),
  "",
  "TITLE: Supplementary Figure S6 — Geo vs Geo representativeness through time",
  "",
  "DESCRIPTION:",
  "Geo vs Geo weighted Jaccard similarity (J) for Figure 5's own six axes (a Koppen-Geiger,",
  "b land cover as IGBP, c aridity, d biomass, e NEE, f ET), tracked across four FLUXNET",
  "network generations: Marconi, La Thuile, FLUXNET2015, and the current (781-site) network.",
  "Every site is classified by the gridded product's own value at its coordinate (never a",
  "tower-measured value) -- the same Geo vs Geo definition, classes, bin edges, land grids",
  "and totals as scripts/figure4_representativeness.R and figS4_representativeness_geo_vs_geo.png;",
  "see that figure's legend for the full per-axis methods notes. Classes/edges are not redefined",
  "here. New raster extractions for the three historical networks: MODIS MCD12C1 IGBP (nearest",
  "cell) and TRENDY model NEE/GPP/ET (bilinear) at each historical tower coordinate -- Koppen,",
  "aridity and biomass for the historical networks reuse existing data/snapshots/site_*_<net>.csv",
  "tables (same products/class schemes as Figure 5, confirmed before reuse).",
  paste0("Final artwork size: ", FIG_WIDTH_MM, " mm wide x ", FIG_HEIGHT_MM,
         " mm tall, Helvetica throughout. Supplementary Figure (target journal Scientific Data",
         " has no Extended Data concept) -- see docs/figure_inventory.md."),
  "",
  "X AXIS: four network generations in chronological order (Marconi, La Thuile, FLUXNET2015, Current).",
  "",
  "PRIMARY Y AXIS (left): weighted Jaccard similarity J in [0, 1]. Six coloured lines, one per axis.",
  "",
  "SECONDARY Y AXIS (right): site count (grey bars, width 0.38, black outline). Axis scaled so the",
  paste0("maximum (", MAX_N, " sites, current network) aligns with J = 1.0 on the primary axis."),
  "",
  "CLASSIFICATION AXES AND LINE COLOURS (Okabe-Ito colourblind-safe palette; IGBP takes the role",
  "the retired fig_05_jaccard_trajectory_with_counts.png's LULC axis had -- see",
  "review/figstage_status.md, Stage 3 Decisions for Dave):",
  sprintf("  %-20s %s", paste0(AXIS_LABELS, ":"), TRAJ_COLORS[names(AXIS_LABELS)]),
  "",
  "JACCARD VALUES AND SITE COUNTS (n classified / n eligible), all four networks:"
)
for (spec in PANEL_SPECS) {
  legend_lines <- c(legend_lines, paste0("  ", spec$panel, " ", AXIS_LABELS[[spec$axis]], " (", spec$axis, "):"))
  for (net in NETWORKS) {
    r <- spec$results[[net]]
    legend_lines <- c(legend_lines, sprintf("      %-12s n = %d / %d, J = %.3f",
                                             NETWORK_LABELS[[net]], r$n_classified, r$n_eligible, r$j))
  }
}
legend_lines <- c(legend_lines, "",
  "SITE COUNTS BY GENERATION: Marconi 35 | La Thuile 252 | FLUXNET2015 212 | Current 781.",
  "",
  "UNCLASSIFIED HISTORICAL SITES:")
if (nrow(unclassified_df) == 0) {
  legend_lines <- c(legend_lines, "  (none -- every historical site was classified on every axis.)")
} else {
  for (i in seq_len(nrow(unclassified_df))) {
    row <- unclassified_df[i, ]
    legend_lines <- c(legend_lines, sprintf("  %s (panel %s, %s): %s", row$site_id, row$panel, row$network, row$reason))
  }
}
legend_lines <- c(legend_lines, "",
  "REPRESENTATIVENESS METRIC: Weighted Jaccard (J) -- sum(pmin(p,q)) / sum(pmax(p,q)).",
  "",
  "SOURCE: scripts/figure_representativeness_trajectory.R.",
  "Table: data/snapshots/representativeness_metrics_trajectory.csv.",
  "Vector PDF alongside this PNG. Companion figure: figS4_representativeness_geo_vs_geo.png."
)
legend_path <- paste0(fig_stem, ".legend.txt")
writeLines(legend_lines, legend_path)
msg("Saved legend: ", legend_path)

# ==============================================================================
# 14. Copy to SupFigs/
# ==============================================================================
## SupFigs/ copy renamed to the Stage 6 numbering (figS6, the next number
## after figS5_flux_representativeness -- the task's own "S1...S6" list) --
## the FIG_DIR source above keeps its own descriptive name
## (supp_representativeness_trajectory), same pattern figure4_
## representativeness.R uses for Figure 5/figS4 (DEST_BASENAME, source name
## unchanged, only the SupFigs/draft_manuscript_v1 copy renamed).
supfigs_stem <- file.path(SUPFIGS_DIR, "figS6_representativeness_trajectory")
fs::file_copy(paste0(fig_stem, ".png"), paste0(supfigs_stem, ".png"), overwrite = TRUE)
fs::file_copy(paste0(fig_stem, ".pdf"), paste0(supfigs_stem, ".pdf"), overwrite = TRUE)
fs::file_copy(paste0(fig_stem, ".jpg"), paste0(supfigs_stem, ".jpg"), overwrite = TRUE)
fs::file_copy(legend_path, paste0(supfigs_stem, ".legend.txt"), overwrite = TRUE)
msg("Copied to SupFigs/: ", supfigs_stem, ".{png,pdf,jpg,legend.txt}")

msg("\n=== DONE ===")
msg("Metrics table: ", metrics_path)
msg("Figure: ", fig_stem, ".{png,pdf,jpg}")
msg("Unclassified historical sites: ", nrow(unclassified_df))
