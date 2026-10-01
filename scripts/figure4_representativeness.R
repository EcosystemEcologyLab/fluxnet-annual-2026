## figure4_representativeness.R
##
## Production script for the NEW Figure 4 (current network, 781 sites): two
## full versions -- "Geo vs Geo" (global land distribution vs. the gridded
## product's own value at each tower's coordinates) and "Geo vs Data"
## (global land distribution vs. each site's own measured/derived value) --
## six panels each, in order: A Koppen-Geiger, B land cover (IGBP), C
## aridity, D biomass, E NEE, F ET.
##
## Built in phases, committed separately; see SESSION_LOG.md for each
## phase's decisions and report. Does NOT source, modify, or regenerate any
## output of scripts/figure_representativeness_summary.R (Figs 001-008) --
## axis constants / colour palettes / the weighted-Jaccard convention are
## reproduced here, not imported, exactly as scripts/diagnostics/
## flux_bin_breaks.R already does for its own candidate panels this session.
##
## ---- PHASE 1 (2026-10-02): Koppen-Geiger, panel A --------------------------
## Geo vs Geo: refreshed data/snapshots/site_koppen_beck2023.csv (767->781
##   sites; see scripts/step4_extract_koppen_beck2023.R's updated snapshot
##   pin) -- the Beck et al. (2023) 1991-2020, 1 km raster class at each of
##   the 781 current-network tower coordinates.
## Geo vs Data: site classes from each site's own bundled ERA5 monthly
##   reanalysis (P_ERA, TA_ERA), 1991-2020, via R/climate_classification.R's
##   compute_site_koppen_era5() -- same method step5_compute_koppen_era5.R
##   already uses -- but called with map_max_mm = Inf, i.e. the
##   KG_ERA5_MAP_MAX_MM screen is NOT applied as an exclusion for this
##   panel. Instead, the 172 sites in the precip_downscaling_provenance
##   diagnostic's `not_fitted_slope_9999` group (review/diagnostics/
##   precip_downscaling_provenance/table_2_site_groups.csv) are excluded
##   from this panel entirely -- not just marked unclassified, removed from
##   both numerator and denominator (n_eligible = 781 - 172 = 609).
## Bug found and fixed while investigating IT-Niv (see SESSION_LOG.md):
##   R/climate_classification.R's compute_era5_monthly_climatology() lost
##   the true n_years_used count for any site with 1..(min_years-1) valid
##   years, reporting 0 instead -- fixed there, not here (shared helper);
##   step5_compute_koppen_era5.R was rerun afterward so the production
##   site_koppen_era5.csv's n_years_used column is also corrected (no
##   kg_class outcome changed).
##
## ---- ADDENDUM (2026-10-02): second precip-dependent exclusion rule --------
## Added a second exclusion rule for precipitation-dependent Geo-vs-Data
## panels (Koppen here; aridity in Phase 3): exclude a site if its 1991-2020
## mean annual P_ERA exceeds P_ERA_MAX_RATIO=3 (R/pipeline_config.R) times a
## reference mean annual precipitation (BADM MAP where present/non-zero,
## else WorldClim BIO12 at the tower). Additive to the 172 GRP_ERA_DOWN
## sites, not a replacement. Catches 12 sites beyond the 172 at ratio>3 --
## above the task's "~10" threshold, so per instruction Phase 5 (figure
## assembly) is held pending review of that list; Phases 2-4 proceed. See
## SESSION_LOG.md for the full report (ratio=2/3/4 sensitivity, the 12-site
## list, and confirmation all 4 previously-named sites are caught).

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
source("R/utils.R")
source("R/climate_classification.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(duckdb)
  library(DBI)
  library(dplyr)
  library(readr)
  library(lubridate)
  library(terra)
  library(ggplot2)
  library(patchwork)
  library(fs)
})

SNAP_DIR   <- "data/snapshots"
EXT        <- "data/external"
FIG_DIR    <- "review/figures/representativeness"
DRAFT_DIR  <- "review/figures/draft_manuscript_v1"
fs::dir_create(FIG_DIR)
fs::dir_create(DRAFT_DIR)

LOG_START <- format(Sys.time(), "%Y%m%d_%H%M%S")
LOG_FILE  <- file.path("logs", paste0("figure4_representativeness_", LOG_START, ".log"))
con_log <- file(LOG_FILE, open = "wt")
sink(con_log, type = "output")
sink(con_log, type = "message", append = TRUE)
on.exit({ sink(type = "message"); sink(type = "output"); close(con_log) }, add = TRUE)
msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)

msg("=== figure4_representativeness.R ===")
msg("Log: ", LOG_FILE)

weighted_jaccard <- function(p, q) sum(pmin(p, q)) / sum(pmax(p, q))

## ---- Current-network site list (781 sites; same pin step5 already uses,
## confirmed identical site_id set to site_biomass_cci_v7.csv / the newer
## 2026-09-20 snapshot -- see SESSION_LOG.md Phase 1 entry for the check).
CURRENT_SNAPSHOT <- "data/snapshots/fluxnet_shuttle_snapshot_20260901T094522.csv"
current_sites <- readr::read_csv(CURRENT_SNAPSHOT, show_col_types = FALSE) |>
  dplyr::distinct(site_id, location_lat, location_long)
n_sites <- nrow(current_sites)
msg("Current network: ", n_sites, " sites (snapshot ", basename(CURRENT_SNAPSHOT), ")")
if (n_sites != 781L) {
  warning("Expected 781 current-network sites, got ", n_sites,
          " -- downstream site counts/labels in this script assume 781.")
}

## ---- Metrics accumulator: one row per axis x comparison, written at the
## end of the whole script (data/snapshots/representativeness_metrics_fig4.csv),
## NOT appended to the shared representativeness_metrics.csv (that file
## backs Figs 001-008, which this script must not regenerate or disturb).
metrics_rows <- list()
add_metric <- function(panel, axis, comparison, land_grid, land_total_km2,
                        n_eligible, n_classified, j) {
  metrics_rows[[paste0(panel, "_", comparison)]] <<- data.frame(
    panel = panel, axis = axis, comparison = comparison, land_grid = land_grid,
    land_total_km2 = land_total_km2, n_eligible = n_eligible,
    n_classified = n_classified, weighted_jaccard = j
  )
}

# ==============================================================================
# SHARED: precipitation-dependent Geo-vs-Data exclusion (Koppen + aridity)
# ==============================================================================
## Added 2026-10-02, after Phase 1 was first committed (see SESSION_LOG.md):
## a second exclusion rule for the precipitation-dependent Geo-vs-Data panels
## (Koppen here; aridity in Phase 3), IN ADDITION TO the 172-site
## not_fitted_slope_9999 (GRP_ERA_DOWN) exclusion already applied. A site is
## excluded if its 1991-2020 mean annual P_ERA exceeds P_ERA_MAX_RATIO (see
## R/pipeline_config.R) times a reference mean annual precipitation: PI-
## reported BADM MAP where present and non-zero, else WorldClim BIO12 at the
## tower coordinate. Built once here as a reusable site-level table + helper,
## not duplicated per panel.
msg("\n=== SHARED: precipitation reference + P_ERA_MAX_RATIO exclusion ===")

## ---- GRP_ERA_DOWN (172-site) group, shared by every precip-dependent panel
precip_groups <- readr::read_csv(
  "review/diagnostics/precip_downscaling_provenance/table_2_site_groups.csv",
  show_col_types = FALSE)
slope9999_172 <- precip_groups$site_id[precip_groups$p_group == "not_fitted_slope_9999"]
msg("Sites in not_fitted_slope_9999 (GRP_ERA_DOWN) group: ", length(slope9999_172))
if (length(slope9999_172) != 172L) {
  warning("Expected 172 sites in not_fitted_slope_9999, found ", length(slope9999_172))
}

## ---- ERA5-derived 1991-2020 mean annual P_ERA per site, no MAP screen ----
## (compute_site_koppen_era5() conveniently returns both the Koppen class AND
## map_mm -- the 1991-2020 mean annual P_ERA used by P_ERA_MAX_RATIO below --
## from one call; Phase 1 reuses this same object for its classification.)
duckdb_path <- "data/duckdb/fluxnet.duckdb"
con <- dbConnect(duckdb(), dbdir = duckdb_path, read_only = TRUE)
monthly_era5 <- dbGetQuery(con, "SELECT site_id, TIMESTAMP, TA_ERA, P_ERA FROM monthly WHERE dataset = 'ERA5'")
dbDisconnect(con, shutdown = TRUE)
monthly_era5 <- monthly_era5 |> dplyr::filter(site_id %in% current_sites$site_id)
msg("ERA5 monthly rows for current network: ", nrow(monthly_era5),
    " (", length(unique(monthly_era5$site_id)), " sites)")
## legend = NULL: compute_site_koppen_era5() only uses `legend` to attach
## koppen_class_code/koppen_class_name/koppen_main_name (requires those exact
## column names); not needed here (Phase 1 gets class codes/colours from its
## own TL_ORDER/KG13_COLORS).
kg_era5_nomap <- compute_site_koppen_era5(monthly_era5, map_max_mm = Inf, legend = NULL)

badm_path <- file.path(FLUXNET_DATA_ROOT, "processed", "badm.rds")
badm <- readRDS(badm_path)
badm_map <- badm |>
  dplyr::filter(VARIABLE == "MAP") |>
  dplyr::distinct(SITE_ID, .keep_all = TRUE) |>
  dplyr::transmute(site_id = SITE_ID, badm_map_mm = suppressWarnings(as.numeric(DATAVALUE)))

bio12_path <- file.path(EXT, "worldclim", "climate", "wc2.1_2.5m", "wc2.1_2.5m_bio_12.tif")
bio12_rast <- terra::rast(bio12_path)
pts_all <- terra::vect(data.frame(x = current_sites$location_long, y = current_sites$location_lat),
                        geom = c("x", "y"), crs = "EPSG:4326")
bio12_vals <- terra::extract(bio12_rast, pts_all, ID = FALSE)[[1]]
if (anyNA(bio12_vals)) {
  warning(sum(is.na(bio12_vals)), " site(s) got NA WorldClim BIO12 at exact coordinates ",
          "(coastal/ocean-edge pixels?) -- not handled with a buffer fallback here; ",
          "these sites can only use BADM MAP as their reference.")
}

precip_reference <- current_sites |>
  dplyr::mutate(bio12_mm = bio12_vals) |>
  dplyr::left_join(badm_map, by = "site_id") |>
  dplyr::mutate(
    ref_source = dplyr::if_else(!is.na(badm_map_mm) & badm_map_mm > 0, "badm_map", "worldclim_bio12"),
    ref_map_mm = dplyr::if_else(!is.na(badm_map_mm) & badm_map_mm > 0, badm_map_mm, bio12_mm)
  )
msg("Precip reference source: badm_map=", sum(precip_reference$ref_source == "badm_map"),
    "  worldclim_bio12=", sum(precip_reference$ref_source == "worldclim_bio12"),
    "  (of ", nrow(precip_reference), ")")

precip_ref_path <- file.path(SNAP_DIR, "site_precip_reference.csv")
readr::write_csv(precip_reference, precip_ref_path)
write_output_metadata(
  precip_ref_path,
  input_sources = c(CURRENT_SNAPSHOT, badm_path, bio12_path),
  notes = paste0(
    "Reference mean annual precipitation per current-network site, for the P_ERA_MAX_RATIO ",
    "exclusion rule (figure4_representativeness.R, applied to the Koppen and aridity Geo-vs-Data ",
    "panels). ref_map_mm = BADM MAP (PI-reported) where present and non-zero, else WorldClim v2.1 ",
    "BIO12 (1970-2000 baseline, 2.5 arc-min) extracted at the exact tower coordinate (no buffer ",
    "fallback for NA returns). ref_source records which was used per site."
  )
)
msg("Saved: ", precip_ref_path)

## Returns a data frame (site_id, p_era_map_mm, ref_map_mm, ref_source, ratio,
## excluded_grp_era_down, excluded_p_era_ratio, excluded_any) for every site
## in `p_era_df` (must have site_id + a P_ERA 1991-2020 mean annual column).
## `excluded_by` is passed to log_exclusion() for the ratio rule only (the
## GRP_ERA_DOWN rule was already logged once, by Phase 1, with its own
## wording -- not re-logged here to avoid duplicate log rows on reruns of
## later phases within the same script execution... but see note at call
## sites: each phase that calls this still logs its OWN GRP_ERA_DOWN rows,
## since log_exclusion() appends and this script does not deduplicate its
## own log output across phases/panels by design (every exclusion row names
## which panel excluded it via `variable`).
compute_precip_exclusions <- function(p_era_df, p_era_col, slope9999_sites, excluded_by, panel_name) {
  d <- p_era_df |>
    dplyr::rename(p_era_map_mm = dplyr::all_of(p_era_col)) |>
    dplyr::left_join(dplyr::select(precip_reference, site_id, ref_map_mm, ref_source), by = "site_id") |>
    dplyr::mutate(
      ratio = p_era_map_mm / ref_map_mm,
      excluded_grp_era_down = site_id %in% slope9999_sites,
      excluded_p_era_ratio  = !is.na(ratio) & ratio > P_ERA_MAX_RATIO & !excluded_grp_era_down,
      excluded_any = excluded_grp_era_down | excluded_p_era_ratio
    )

  for (sid in slope9999_sites) {
    log_exclusion(
      site_id = sid, variable = paste0(panel_name, " (Geo vs Data panel)"), timestamp = "ALL",
      reason = "Site is in the precip_downscaling_provenance not_fitted_slope_9999 (GRP_ERA_DOWN) group",
      threshold = "p_group == 'not_fitted_slope_9999'", excluded_by = excluded_by
    )
  }
  ratio_caught <- d |> dplyr::filter(excluded_p_era_ratio)
  for (i in seq_len(nrow(ratio_caught))) {
    log_exclusion(
      site_id = ratio_caught$site_id[i], variable = paste0(panel_name, " (Geo vs Data panel)"),
      timestamp = "ALL",
      reason = sprintf("P_ERA/reference ratio %.2f exceeds P_ERA_MAX_RATIO (P_ERA=%.1f mm/yr, ref=%.1f mm/yr, ref_source=%s)",
                        ratio_caught$ratio[i], ratio_caught$p_era_map_mm[i],
                        ratio_caught$ref_map_mm[i], ratio_caught$ref_source[i]),
      threshold = paste0("P_ERA_MAX_RATIO=", P_ERA_MAX_RATIO), excluded_by = excluded_by
    )
  }
  d
}

## ---- Required reporting (ratio sensitivity, 4 named sites, >10 check) ----
## Uses Koppen's kg_era5_nomap$map_mm as "1991-2020 mean annual P_ERA" -- this
## is a general ERA5 climatology byproduct, not Koppen-specific (same number
## aridity's Geo-vs-Data panel will use in Phase 3).
ratio_check <- kg_era5_nomap |> dplyr::select(site_id, map_mm) |>
  dplyr::left_join(dplyr::select(precip_reference, site_id, ref_map_mm, ref_source), by = "site_id") |>
  dplyr::mutate(ratio = map_mm / ref_map_mm, in_172 = site_id %in% slope9999_172)

for (r in c(2, 3, 4)) {
  n_tot <- sum(ratio_check$ratio > r, na.rm = TRUE)
  n_beyond <- sum(ratio_check$ratio > r & !ratio_check$in_172, na.rm = TRUE)
  msg("P_ERA/reference ratio > ", r, ": ", n_tot, " sites caught in total, ", n_beyond,
      " beyond the 172 GRP_ERA_DOWN sites")
}

caught_beyond_172 <- ratio_check |> dplyr::filter(ratio > P_ERA_MAX_RATIO, !in_172) |>
  dplyr::arrange(dplyr::desc(ratio))
msg("\nSites caught by P_ERA_MAX_RATIO=", P_ERA_MAX_RATIO, " beyond the 172 (n=",
    nrow(caught_beyond_172), "):")
print(as.data.frame(caught_beyond_172[, c("site_id", "map_mm", "ref_map_mm", "ref_source", "ratio")]))
if (nrow(caught_beyond_172) > 10L) {
  msg("*** HOLD: ", nrow(caught_beyond_172), " sites caught beyond the 172 (> ~10) -- ",
      "per task instruction, Phase 5 (figure assembly) is NOT run in this script execution. ",
      "Phases 2-4 proceed. Awaiting review of the list above before Phase 5.")
}

four_named <- ratio_check |> dplyr::filter(site_id %in% c("CA-CF2", "IT-Niv", "NO-And", "US-HB4"))
msg("\nCA-CF2 / IT-Niv / NO-And / US-HB4 all caught by ratio>", P_ERA_MAX_RATIO, "? ",
    all(four_named$ratio > P_ERA_MAX_RATIO, na.rm = TRUE))
print(as.data.frame(four_named[, c("site_id", "map_mm", "ref_map_mm", "ref_source", "ratio")]))

# ==============================================================================
# PHASE 1: Koppen-Geiger (panel A)
# ==============================================================================
msg("\n=== PHASE 1: Koppen-Geiger (panel A) ===")

## ---- Legend + colours (reproduced from figure_representativeness_summary.R
## / flux_bin_breaks.R -- not sourced, see header) ----------------------------
kg_leg_raw <- readLines(file.path(EXT, "koppen_beck2023", "legend.txt"))
kg_leg_raw <- kg_leg_raw[grepl("^\\s+\\d+:", kg_leg_raw)]
kg_leg_df <- data.frame(
  koppen_class = sub("^\\s*\\d+:\\s+(\\S+)\\s+.*", "\\1", kg_leg_raw),
  r = as.integer(sub(".*\\[(\\d+)\\s+\\d+\\s+\\d+\\].*", "\\1", kg_leg_raw)),
  g = as.integer(sub(".*\\[\\d+\\s+(\\d+)\\s+\\d+\\].*", "\\1", kg_leg_raw)),
  b = as.integer(sub(".*\\[\\d+\\s+\\d+\\s+(\\d+)\\].*", "\\1", kg_leg_raw)),
  stringsAsFactors = FALSE
) |> dplyr::mutate(koppen_twoletter = substr(koppen_class, 1, 2))
TL_ORDER <- c("Af", "Am", "Aw", "BS", "BW", "Cf", "Cs", "Cw", "Df", "Ds", "Dw", "EF", "ET")
KG13_COLORS <- setNames(
  vapply(TL_ORDER, function(tl) {
    m <- kg_leg_df[kg_leg_df$koppen_twoletter == tl, ]
    grDevices::rgb(mean(m$r), mean(m$g), mean(m$b), maxColorValue = 255)
  }, character(1L)),
  TL_ORDER
)

## ---- Global land side: Beck 2023 1 km mask, aggregated to 2-letter classes
kg_global <- readr::read_csv(file.path(SNAP_DIR, "koppen_beck2023_global_distribution.csv"),
                              show_col_types = FALSE) |>
  dplyr::group_by(koppen_twoletter) |>
  dplyr::summarise(global_land_area_km2 = sum(global_land_area_km2),
                    global_land_fraction = sum(global_land_fraction), .groups = "drop") |>
  dplyr::rename(class = koppen_twoletter)
KG_LAND_TOTAL_KM2 <- sum(kg_global$global_land_area_km2)
msg("Koppen global land total (Beck 2023 1km mask): ",
    format(round(KG_LAND_TOTAL_KM2), big.mark = ","), " km2")

## ---- Geo vs Geo: refreshed site_koppen_beck2023.csv (781 sites) -----------
## Refreshed by scripts/step4_extract_koppen_beck2023.R (snapshot pin bumped
## 2026-06-24/767 -> 2026-09-01/781; rerun before this script, see
## SESSION_LOG.md). Read directly here, not re-extracted, to avoid a second
## raster-extraction pass.
beck_path <- file.path(SNAP_DIR, "site_koppen_beck2023.csv")
kg_geo_geo <- readr::read_csv(beck_path, show_col_types = FALSE)
if (nrow(kg_geo_geo) != 781L) {
  stop("site_koppen_beck2023.csv has ", nrow(kg_geo_geo), " rows, expected 781 -- ",
       "rerun scripts/step4_extract_koppen_beck2023.R first.")
}
n_geo_geo_classified <- sum(!is.na(kg_geo_geo$koppen_twoletter))
msg("Geo vs Geo (Beck raster at tower): ", n_geo_geo_classified, " / ", nrow(kg_geo_geo),
    " sites classified")

cnt_geo_geo <- kg_geo_geo |>
  dplyr::filter(!is.na(koppen_twoletter)) |>
  dplyr::count(koppen_twoletter, name = "n") |>
  dplyr::rename(class = koppen_twoletter) |>
  dplyr::mutate(network_frac = n / nrow(kg_geo_geo))
merged_geo_geo <- kg_global |>
  dplyr::select(class, global_land_fraction) |>
  dplyr::full_join(cnt_geo_geo, by = "class") |>
  dplyr::mutate(n = dplyr::coalesce(n, 0L), network_frac = dplyr::coalesce(network_frac, 0),
                global_land_fraction = dplyr::coalesce(global_land_fraction, 0))
j_kg_geo_geo <- weighted_jaccard(merged_geo_geo$global_land_fraction, merged_geo_geo$network_frac)
msg("Koppen J (Geo vs Geo) = ", round(j_kg_geo_geo, 3))
add_metric("A", "koppen", "geo_vs_geo", "Beck 2023 1 km mask", KG_LAND_TOTAL_KM2,
           nrow(kg_geo_geo), n_geo_geo_classified, j_kg_geo_geo)

## ---- Geo vs Data: ERA5-local classification, no MAP screen, dual exclusion
## (172 GRP_ERA_DOWN + P_ERA_MAX_RATIO -- added 2026-10-02, see SHARED section
## above and SESSION_LOG.md). kg_era5_nomap/slope9999_172 computed there.
kg_precip_excl <- compute_precip_exclusions(
  kg_era5_nomap, p_era_col = "map_mm", slope9999_sites = slope9999_172,
  excluded_by = "figure4_representativeness.R", panel_name = "koppen_era5"
)
kg_geo_data_pool <- kg_era5_nomap |>
  dplyr::filter(site_id %in% kg_precip_excl$site_id[!kg_precip_excl$excluded_any])
n_excl_ratio_only <- sum(kg_precip_excl$excluded_p_era_ratio)
msg("Geo vs Data exclusions: ", length(slope9999_172), " GRP_ERA_DOWN + ",
    n_excl_ratio_only, " P_ERA_MAX_RATIO-only = ", sum(kg_precip_excl$excluded_any),
    " total excluded; n_eligible = ", nrow(kg_geo_data_pool))
n_geo_data_classified <- sum(!is.na(kg_geo_data_pool$koppen_twoletter))
msg("Geo vs Data (ERA5 local, no MAP screen, dual exclusion): ",
    n_geo_data_classified, " / ", nrow(kg_geo_data_pool), " eligible sites classified")

cnt_geo_data <- kg_geo_data_pool |>
  dplyr::filter(!is.na(koppen_twoletter)) |>
  dplyr::count(koppen_twoletter, name = "n") |>
  dplyr::rename(class = koppen_twoletter) |>
  dplyr::mutate(network_frac = n / nrow(kg_geo_data_pool))
merged_geo_data <- kg_global |>
  dplyr::select(class, global_land_fraction) |>
  dplyr::full_join(cnt_geo_data, by = "class") |>
  dplyr::mutate(n = dplyr::coalesce(n, 0L), network_frac = dplyr::coalesce(network_frac, 0),
                global_land_fraction = dplyr::coalesce(global_land_fraction, 0))
j_kg_geo_data <- weighted_jaccard(merged_geo_data$global_land_fraction, merged_geo_data$network_frac)
msg("Koppen J (Geo vs Data) = ", round(j_kg_geo_data, 3))
add_metric("A", "koppen", "geo_vs_data", "Beck 2023 1 km mask", KG_LAND_TOTAL_KM2,
           nrow(kg_geo_data_pool), n_geo_data_classified, j_kg_geo_data)

## ---- Required Phase 1 report: 26 (old MAP-screen unclassified) vs 172 -----
old_era5 <- readr::read_csv(file.path(SNAP_DIR, "site_koppen_era5.csv"), show_col_types = FALSE)
old_unclassified_26 <- old_era5$site_id[is.na(old_era5$kg_class)]
msg("\n--- Overlap: 26 old-rule-unclassified sites vs 172 not_fitted_slope_9999 ---")
msg("n old-unclassified = ", length(old_unclassified_26), " (expect 26)")
overlap_22 <- intersect(old_unclassified_26, slope9999_172)
outside_4  <- setdiff(old_unclassified_26, slope9999_172)
msg("In both (26 ∩ 172) = ", length(overlap_22), ": ", paste(sort(overlap_22), collapse = ", "))
msg("In 26 but NOT in 172 (now reclassified under the new rule) = ", length(outside_4), ":")
outside_report <- kg_era5_nomap |>
  dplyr::filter(site_id %in% outside_4) |>
  dplyr::select(site_id, n_years_used, mat_degc, map_mm, kg_class, koppen_twoletter) |>
  dplyr::arrange(site_id)
print(as.data.frame(outside_report))
msg("Flagged as implausible precipitation (ERA5 spatial-averaging artifact candidates): ",
    "US-HB4 (map_mm=", round(outside_report$map_mm[outside_report$site_id == "US-HB4"], 0),
    " mm/yr -- known case per task instruction, docs/known_issues.md ",
    "§9a-style artifact) and NO-And (map_mm=",
    round(outside_report$map_mm[outside_report$site_id == "NO-And"], 0),
    " mm/yr -- implausibly high for any terrestrial site, flagged though not previously ",
    "documented as a known case). CA-CF2 (",
    round(outside_report$map_mm[outside_report$site_id == "CA-CF2"], 0),
    " mm/yr) and IT-Niv (", round(outside_report$map_mm[outside_report$site_id == "IT-Niv"], 0),
    " mm/yr) are high but within plausible range for their classified climates ",
    "(Df/ET respectively) -- not flagged.")

msg("\n--- IT-Niv: why it lost 3 further years beyond the old MAP screen ---")
msg("IT-Niv has complete ERA5 data for all 30/1991-2020 years (12/12 months, no NAs). ",
    "27 of those 30 years exceed the old KG_ERA5_MAP_MAX_MM=5000mm/yr screen, leaving 3 ",
    "years (1997, 1998, 2005) that should have survived it. The OLD site_koppen_era5.csv ",
    "nonetheless reported n_years_used=0 for IT-Niv, not 3 -- this was a genuine bug in ",
    "compute_era5_monthly_climatology() (R/climate_classification.R), fixed in this phase ",
    "(see header and R/climate_classification.R's inline comment): the function's final ",
    "output join started from the valid-sites-only climatology table, silently dropping ",
    "the true (sub-threshold) year count for any site below min_years to 0 instead of its ",
    "real value. step5_compute_koppen_era5.R was rerun after the fix; site_koppen_era5.csv ",
    "now correctly reports IT-Niv's n_years_used=3 (still unclassifiable under the OLD ",
    "MAP-screened rule, since 3 < KG_ERA5_MIN_YEARS=20 -- but for the true, honest reason, ",
    "not a misleading zero). Several of the other 25 old-unclassified sites' n_years_used ",
    "values changed too (previously misreported as 0); none of the 755 classified sites' ",
    "kg_class changed -- this bug only affected the n_years_used diagnostic column for ",
    "already-unclassified sites, never a classification outcome.")

# ============================================================================
# Save panel A (Koppen) outputs
# ============================================================================
msg("\n=== Saving panel A (Koppen) outputs ===")

fig4_kg_era5_path <- file.path(SNAP_DIR, "site_koppen_era5_fig4.csv")
kg_era5_nomap |>
  dplyr::left_join(
    dplyr::select(kg_precip_excl, site_id, ratio, ref_source,
                   excluded_grp_era_down, excluded_p_era_ratio, excluded_any),
    by = "site_id"
  ) |>
  dplyr::rename(excluded_fig4_geo_vs_data = excluded_any) |>
  readr::write_csv(fig4_kg_era5_path)
write_output_metadata(
  fig4_kg_era5_path,
  input_sources = c(duckdb_path, CURRENT_SNAPSHOT, precip_ref_path,
                     "review/diagnostics/precip_downscaling_provenance/table_2_site_groups.csv"),
  notes = paste0(
    "Koppen classification for figure4_representativeness.R's Geo-vs-Data panel A. Same method as ",
    "site_koppen_era5.csv (R/climate_classification.R::compute_site_koppen_era5(), 1991-2020 ERA5 ",
    "monthly T/P normal, >= 20 of 30 years required) EXCEPT the KG_ERA5_MAP_MAX_MM=5000mm/yr ",
    "per-site-year screen is NOT applied here (map_max_mm=Inf). Two exclusion rules instead, both ",
    "flagged in `excluded_fig4_geo_vs_data` (= excluded_grp_era_down | excluded_p_era_ratio): (1) the ",
    "172 sites in the precip_downscaling_provenance not_fitted_slope_9999 (GRP_ERA_DOWN) group; (2) ",
    "sites whose 1991-2020 mean annual P_ERA exceeds P_ERA_MAX_RATIO=", P_ERA_MAX_RATIO,
    " times their reference MAP (site_precip_reference.csv: BADM MAP where present/non-zero, else ",
    "WorldClim BIO12 at the tower) -- 12 sites beyond the 172 at this threshold (added 2026-10-02, ",
    "see SESSION_LOG.md for the full list and the ratio=2/3/4 sensitivity check). All sites are still ",
    "classified in this file for reference; the panel's numerator AND denominator must exclude both ",
    "groups (n_eligible = 781 - 172 - 12 = 597)."
  )
)
msg("Saved: ", fig4_kg_era5_path)

# ==============================================================================
# PHASE 2: Land cover as IGBP (panel B)
# ==============================================================================
## Global side: MODIS MCD12C1.061 "Majority_Land_Cover_Type_1" (IGBP scheme,
## 0.05 deg native, 2022001 = day 1 of 2022), the file already in
## data/external/modis_landcover/. Area on the 1 km Beck 2023 land mask (same
## mask/total as panel A): the MODIS raster is resampled onto the Beck grid
## (nearest-neighbour, categorical) and masked to it, which reproduces the
## exact 147,322,862 km2 total (checked below) -- not computed on MODIS's own
## native 0.05 deg footprint, per the task's explicit "area on the 1 km Beck
## land mask" instruction (flux_bin_breaks.R's TRENDY axes use their own
## footprint instead; this panel deliberately does not, since it was asked
## for by name here).
##
## MODIS's HDF4 CRS metadata mislabels the datum ("Clarke 1866 ellipsoid");
## both rasters share the identical -180/180/-90/90 lon/lat extent (a known
## MCD12C1 CMG quirk, not a real projection mismatch), so MODIS's CRS is set
## to EPSG:4326 (matching Beck's) before resampling rather than left to warn
## or silently assumed.
##
## Allowable classes = the IGBP classes PIs actually report in BADM/BIF
## metadata (the `igbp` column already in the pinned snapshot CSV, sourced
## from each site's BIF, not a free-text BADM field -- no direct "IGBP"
## BADM VARIABLE exists, confirmed by grep). All 781 current-network sites
## report one of 15 classes: ENF, EBF, DNF, DBF, MF, CSH, OSH, WSA, SAV,
## GRA, WET, CRO, CVM, BSV, SNO -- i.e. every standard IGBP class EXCEPT
## Water (code 0) and Urban-and-built-up (code 13; no flux tower is sited on
## open water or in a city). MODIS classes 0 and 13 are therefore outside
## the PI vocabulary; JUDGEMENT CALL (flagged, not resolved): folded into a
## 16th "Other" bin on the global/Geo side (land area counted, like the
## existing LULC high-level axis's own "Other" category), which can never
## receive a site count since no PI reports Water or Urban -- not excluded
## from the land total, since the task's land total (147.3M km2) must match
## the Koppen/biomass panels' own total exactly.
msg("\n=== PHASE 2: Land cover as IGBP (panel B) ===")

IGBP_ORDER <- c("ENF", "EBF", "DNF", "DBF", "MF", "CSH", "OSH", "WSA", "SAV",
                 "GRA", "WET", "CRO", "CVM", "BSV", "SNO", "Other")
IGBP_CODE_TO_CLASS <- c(
  "0" = "Other", "1" = "ENF", "2" = "EBF", "3" = "DNF", "4" = "DBF", "5" = "MF",
  "6" = "CSH", "7" = "OSH", "8" = "WSA", "9" = "SAV", "10" = "GRA", "11" = "WET",
  "12" = "CRO", "13" = "Other", "14" = "CVM", "15" = "SNO", "16" = "BSV"
)
## Colours: a new axis, not reused from any existing palette in this repo --
## judgement call, flagged. Forest types in greens (darker = denser canopy),
## shrub/savanna/grassland in tans/golds, wetland teal, cropland bright
## yellow-green, cropland mosaic mustard, snow/ice near-white, barren beige,
## Other grey.
IGBP_COLORS <- c(
  ENF = "#1b4332", EBF = "#2d6a4f", DNF = "#52b788", DBF = "#74c69d", MF = "#95d5b2",
  CSH = "#9c6644", OSH = "#bc9a6b", WSA = "#ddb892", SAV = "#e9c46a",
  GRA = "#d4e157", WET = "#4a9bb8", CRO = "#ffd60a", CVM = "#e0a106",
  BSV = "#d8c3a5", SNO = "#f1faee", Other = "#adb5bd"
)

## ---- Global side: MODIS at Beck 1 km resolution ---------------------------
beck_rast_path <- file.path(EXT, "koppen_beck2023", "1991_2020", "koppen_geiger_0p00833333.tif")
beck_rast <- terra::rast(beck_rast_path)
modis_path <- file.path(EXT, "modis_landcover", "MCD12C1.A2022001.061.2023244164746.hdf")
modis_igbp <- terra::sds(modis_path)[1]
terra::crs(modis_igbp) <- "EPSG:4326"

modis_1km <- terra::resample(modis_igbp, beck_rast, method = "near")
modis_1km_land <- terra::mask(modis_1km, beck_rast)
igbp_class_r <- terra::classify(
  modis_1km_land,
  cbind(as.integer(names(IGBP_CODE_TO_CLASS)), seq_along(IGBP_ORDER)[match(IGBP_CODE_TO_CLASS, IGBP_ORDER)])
)
cell_area_1km <- terra::cellSize(igbp_class_r, mask = TRUE, unit = "km")
igbp_zonal <- terra::zonal(cell_area_1km, igbp_class_r, fun = "sum", na.rm = TRUE)
names(igbp_zonal) <- c("bin", "area_km2")
igbp_global <- data.frame(class = IGBP_ORDER, bin = seq_along(IGBP_ORDER)) |>
  dplyr::left_join(igbp_zonal, by = "bin") |>
  dplyr::mutate(area_km2 = dplyr::coalesce(area_km2, 0))
IGBP_LAND_TOTAL_KM2 <- sum(igbp_global$area_km2)
igbp_global$global_land_fraction <- igbp_global$area_km2 / IGBP_LAND_TOTAL_KM2
msg("IGBP global land total (MODIS resampled onto Beck 1km mask): ",
    format(round(IGBP_LAND_TOTAL_KM2), big.mark = ","), " km2",
    if (abs(IGBP_LAND_TOTAL_KM2 - KG_LAND_TOTAL_KM2) < 1) " (matches Koppen panel A exactly)" else
      paste0(" *** MISMATCH vs Koppen panel A's ", format(round(KG_LAND_TOTAL_KM2), big.mark = ","), " km2 ***"))
print(igbp_global)

## ---- Geo vs Geo: MODIS class at each tower (native 0.05 deg resolution,
## not degraded through the 1km resample used for the area accounting above)
pts_igbp <- terra::vect(data.frame(x = current_sites$location_long, y = current_sites$location_lat),
                         geom = c("x", "y"), crs = "EPSG:4326")
igbp_at_tower_code <- terra::extract(modis_igbp, pts_igbp, ID = FALSE)[[1]]
igbp_geo_geo <- current_sites |>
  dplyr::mutate(igbp_modis_code = igbp_at_tower_code,
                igbp_modis_class = IGBP_CODE_TO_CLASS[as.character(igbp_modis_code)])
msg("Geo vs Geo (MODIS at tower): ", sum(!is.na(igbp_geo_geo$igbp_modis_class)), " / ",
    nrow(igbp_geo_geo), " sites classified; class distribution:")
print(table(igbp_geo_geo$igbp_modis_class, useNA = "ifany"))

cnt_igbp_geo_geo <- igbp_geo_geo |>
  dplyr::filter(!is.na(igbp_modis_class)) |>
  dplyr::count(igbp_modis_class, name = "n") |>
  dplyr::rename(class = igbp_modis_class) |>
  dplyr::mutate(network_frac = n / nrow(igbp_geo_geo))
merged_igbp_geo_geo <- igbp_global |>
  dplyr::select(class, global_land_fraction) |>
  dplyr::full_join(cnt_igbp_geo_geo, by = "class") |>
  dplyr::mutate(n = dplyr::coalesce(n, 0L), network_frac = dplyr::coalesce(network_frac, 0),
                global_land_fraction = dplyr::coalesce(global_land_fraction, 0))
j_igbp_geo_geo <- weighted_jaccard(merged_igbp_geo_geo$global_land_fraction, merged_igbp_geo_geo$network_frac)
msg("IGBP J (Geo vs Geo) = ", round(j_igbp_geo_geo, 3))
add_metric("B", "igbp", "geo_vs_geo", "MODIS MCD12C1 on Beck 2023 1 km mask", IGBP_LAND_TOTAL_KM2,
           nrow(igbp_geo_geo), sum(!is.na(igbp_geo_geo$igbp_modis_class)), j_igbp_geo_geo)

## ---- Geo vs Data: each site's PI-reported IGBP class (snapshot `igbp`) ---
igbp_geo_data <- current_sites |>
  dplyr::left_join(readr::read_csv(CURRENT_SNAPSHOT, show_col_types = FALSE) |>
                      dplyr::distinct(site_id, igbp), by = "site_id")
n_pi_classified <- sum(!is.na(igbp_geo_data$igbp))
msg("Geo vs Data (PI-reported IGBP): ", n_pi_classified, " / ", nrow(igbp_geo_data), " sites classified")
if (n_pi_classified != 781L) {
  warning("Expected all 781 sites to have a PI-reported IGBP class, found ", n_pi_classified)
}

cnt_igbp_geo_data <- igbp_geo_data |>
  dplyr::filter(!is.na(igbp)) |>
  dplyr::count(igbp, name = "n") |>
  dplyr::rename(class = igbp) |>
  dplyr::mutate(network_frac = n / nrow(igbp_geo_data))
merged_igbp_geo_data <- igbp_global |>
  dplyr::select(class, global_land_fraction) |>
  dplyr::full_join(cnt_igbp_geo_data, by = "class") |>
  dplyr::mutate(n = dplyr::coalesce(n, 0L), network_frac = dplyr::coalesce(network_frac, 0),
                global_land_fraction = dplyr::coalesce(global_land_fraction, 0))
j_igbp_geo_data <- weighted_jaccard(merged_igbp_geo_data$global_land_fraction, merged_igbp_geo_data$network_frac)
msg("IGBP J (Geo vs Data) = ", round(j_igbp_geo_data, 3))
add_metric("B", "igbp", "geo_vs_data", "MODIS MCD12C1 on Beck 2023 1 km mask", IGBP_LAND_TOTAL_KM2,
           nrow(igbp_geo_data), n_pi_classified, j_igbp_geo_data)

## ---- Required report: PI vs MODIS disagreement, by class -----------------
pi_vs_modis <- igbp_geo_data |>
  dplyr::rename(igbp_pi = igbp) |>
  dplyr::left_join(dplyr::select(igbp_geo_geo, site_id, igbp_modis_code, igbp_modis_class), by = "site_id") |>
  dplyr::mutate(agree = igbp_pi == igbp_modis_class)
n_comparable <- sum(!is.na(pi_vs_modis$igbp_pi) & !is.na(pi_vs_modis$igbp_modis_class))
n_disagree <- sum(!pi_vs_modis$agree, na.rm = TRUE)
msg("\n--- PI-reported vs MODIS-at-tower IGBP disagreement ---")
msg("Comparable sites (both classified): ", n_comparable, "; disagree: ", n_disagree,
    " (", round(100 * n_disagree / n_comparable, 1), "%)")
disagree_by_class <- pi_vs_modis |>
  dplyr::filter(!agree, !is.na(igbp_pi), !is.na(igbp_modis_class)) |>
  dplyr::count(igbp_pi, igbp_modis_class, name = "n", sort = TRUE)
msg("Disagreement by PI class (top pairs, PI class -> MODIS class, count):")
print(as.data.frame(disagree_by_class), row.names = FALSE)
disagree_summary_by_pi <- pi_vs_modis |>
  dplyr::filter(!is.na(igbp_pi), !is.na(igbp_modis_class)) |>
  dplyr::group_by(igbp_pi) |>
  dplyr::summarise(n_sites = dplyr::n(), n_disagree = sum(!agree), .groups = "drop") |>
  dplyr::mutate(pct_disagree = round(100 * n_disagree / n_sites, 1)) |>
  dplyr::arrange(dplyr::desc(n_disagree))
msg("Disagreement rate by PI-reported class:")
print(as.data.frame(disagree_summary_by_pi), row.names = FALSE)

## ---- Save panel B outputs --------------------------------------------------
fig4_igbp_path <- file.path(SNAP_DIR, "site_igbp_fig4.csv")
pi_vs_modis |>
  dplyr::select(site_id, location_lat, location_long, igbp_pi, igbp_modis_class, igbp_modis_code, agree) |>
  readr::write_csv(fig4_igbp_path)
write_output_metadata(
  fig4_igbp_path,
  input_sources = c(CURRENT_SNAPSHOT, modis_path, beck_rast_path),
  notes = paste0(
    "Panel B (land cover as IGBP) site-level data for figure4_representativeness.R. igbp_pi = PI-",
    "reported IGBP class from the pinned snapshot's `igbp` column (used for the Geo-vs-Data panel). ",
    "igbp_modis_class/igbp_modis_code = MODIS MCD12C1.061 Majority_Land_Cover_Type_1 class at the ",
    "exact tower coordinate, native 0.05 deg resolution (used for the Geo-vs-Geo panel). Codes 0 ",
    "(Water) and 13 (Urban/built-up) are recoded to 'Other' -- no PI reports either class among the ",
    "781 current-network sites. agree = igbp_pi == igbp_modis_class."
  )
)
msg("Saved: ", fig4_igbp_path)

igbp_global_path <- file.path(SNAP_DIR, "igbp_mcd12c1_global_distribution.csv")
readr::write_csv(igbp_global, igbp_global_path)
write_output_metadata(
  igbp_global_path,
  input_sources = c(modis_path, beck_rast_path),
  notes = paste0(
    "Global IGBP land-cover class distribution: MODIS MCD12C1.061 Majority_Land_Cover_Type_1 ",
    "(2022, 0.05 deg native), resampled (nearest-neighbour) onto the Beck et al. (2023) 1 km Koppen ",
    "land mask grid and masked to it -- total land area reproduces the Koppen panel A total exactly ",
    "(", format(round(IGBP_LAND_TOTAL_KM2), big.mark = ","), " km2). Classes 0 (Water) and 13 (Urban/",
    "built-up) recoded to 'Other' -- outside the 15-class vocabulary PIs actually report in BADM/BIF ",
    "metadata for the current 781-site network (judgement call, flagged in SESSION_LOG.md: these are ",
    "real land-cover types, kept in the land total rather than excluded, but can never receive a site ",
    "count since no PI reports them)."
  )
)
msg("Saved: ", igbp_global_path)

# ==============================================================================
# PHASE 3: Aridity (panel C)
# ==============================================================================
## Global side + Geo vs Geo: unchanged -- CGIAR Aridity Index v3.1 (7-class
## UNEP scheme), data/snapshots/site_aridity.csv (already 781 sites, already
## current) and aridity_unep7_global_distribution.csv (already current,
## 134,761,545 km2 -- this axis's OWN native coverage, smaller than the
## 147.3M km2 Koppen/IGBP/biomass share; see the 2026-10-01 draft-Fig-4-audit
## entry above for why).
##
## Geo vs Data: AI = P/PET from each site's OWN 1991-2020 ERA5 meteorology.
## P = the same climatological mean annual P_ERA already computed for panel
## A (kg_era5_nomap$map_mm). PET = FAO-56 Penman-Monteith reference
## evapotranspiration (grass reference surface), computed monthly from the
## 1991-2020 ERA5 climatological monthly means and summed to an annual
## total, using exactly the ERA5 variables available in this DuckDB's
## `monthly` table for dataset='ERA5': TA_ERA (deg C), SW_IN_ERA and
## LW_IN_ERA (W/m2, incoming short/longwave), VPD_ERA (hPa), PA_ERA (kPa),
## WS_ERA (m/s). Units confirmed empirically against a known site (US-Ha1)
## before use, not assumed from variable names alone -- see SESSION_LOG.md.
##
## Approximations (reported per task instruction):
## 1. Wind: WS_ERA assumed to be ERA5's native 10 m wind (not explicitly
##    documented in this repo), converted to the FAO-56 reference height of
##    2 m via the standard log-wind-profile formula (u2 = u10 * 4.87 /
##    ln(67.8*10-5.42)).
## 2. Net radiation: FAO-56's own Rn procedure is built for the case where
##    only Rs (shortwave) is measured, estimating net longwave from a
##    cloudiness/clear-sky-radiation parametrization. This ERA5 bundle has
##    BOTH incoming shortwave AND incoming longwave directly, so net
##    radiation is computed more directly instead: Rns = (1-albedo)*Rs with
##    albedo=0.23 (FAO-56's grass reference value); outgoing longwave is
##    estimated via Stefan-Boltzmann (sigma=4.903e-9 MJ K^-4 m^-2 day^-1)
##    applied to TA_ERA as a proxy for surface skin temperature (not
##    available in this bundle) with an assumed surface emissivity of 0.96;
##    Rnl = LW_in - LW_out; Rn = Rns + Rnl.
## 3. Saturation vapour pressure (es) and its slope (Delta) are computed
##    from the monthly MEAN temperature (TA_ERA), not averaged from daily
##    Tmax/Tmin as FAO-56 recommends -- true daily/monthly Tmax/Tmin are not
##    in this ERA5 bundle (only TA_ERA_DAY/TA_ERA_NIGHT, an approximate day/
##    night split, not a true diurnal max/min; not used, to avoid compounding
##    approximations). This is a recognised FAO-56 simplification when only
##    mean T is available and slightly underestimates ET0 (Delta is convex).
## 4. Soil heat flux G is set to 0 -- FAO-56's standard simplification at
##    monthly-to-annual timescales, where G approximately cancels over a
##    full annual cycle.
## 5. Caption note (required): CGIAR's own baseline period is 1970-2000;
##    this Geo-vs-Data PET/AI calculation uses 1991-2020 ERA5 (matching the
##    Koppen/IGBP panels' period) -- a ~20-30 year period mismatch between
##    the Geo and Data sides of this one panel, not present in the other
##    panels. Must be stated in the figure caption (Phase 5).
msg("\n=== PHASE 3: Aridity (panel C) ===")

ARIDITY_ORDER <- c("Hyper-Arid", "Arid", "Semi-Arid", "Dry Sub-Humid",
                    "Humid (low)", "Humid (moderate)", "Hyper-Humid")
ARIDITY_COLORS <- c(
  "Hyper-Arid" = "#d73027", "Arid" = "#fc8d59", "Semi-Arid" = "#ffff33",
  "Dry Sub-Humid" = "#66bd63", "Humid (low)" = "#74add1",
  "Humid (moderate)" = "#4575b4", "Hyper-Humid" = "#313695"
)

aridity_global <- readr::read_csv(file.path(SNAP_DIR, "aridity_unep7_global_distribution.csv"),
                                   show_col_types = FALSE) |>
  dplyr::rename(class = unep_class)
ARIDITY_LAND_TOTAL_KM2 <- sum(aridity_global$global_land_area_km2)
msg("Aridity global land total (CGIAR Aridity Index v3.1, own native coverage): ",
    format(round(ARIDITY_LAND_TOTAL_KM2), big.mark = ","), " km2")

classify_unep7 <- function(ai) {
  edges <- aridity_global[order(aridity_global$ai_min), ]
  out <- rep(NA_character_, length(ai))
  for (i in seq_len(nrow(edges))) {
    hi <- if (is.na(edges$ai_max[i])) Inf else edges$ai_max[i]
    sel <- !is.na(ai) & ai >= edges$ai_min[i] & ai < hi
    out[sel] <- edges$class[i]
  }
  out
}

## ---- Geo vs Geo: unchanged, existing site_aridity.csv ---------------------
aridity_geo_geo <- readr::read_csv(file.path(SNAP_DIR, "site_aridity.csv"), show_col_types = FALSE)
n_arid_geo_geo_classified <- sum(!is.na(aridity_geo_geo$unep_class_7))
msg("Geo vs Geo (CGIAR raster at tower, existing snapshot): ", n_arid_geo_geo_classified, " / ",
    nrow(aridity_geo_geo), " sites classified")

cnt_arid_geo_geo <- aridity_geo_geo |>
  dplyr::filter(!is.na(unep_class_7)) |>
  dplyr::count(unep_class_7, name = "n") |>
  dplyr::rename(class = unep_class_7) |>
  dplyr::mutate(network_frac = n / nrow(aridity_geo_geo))
merged_arid_geo_geo <- aridity_global |>
  dplyr::select(class, global_land_fraction) |>
  dplyr::full_join(cnt_arid_geo_geo, by = "class") |>
  dplyr::mutate(n = dplyr::coalesce(n, 0L), network_frac = dplyr::coalesce(network_frac, 0),
                global_land_fraction = dplyr::coalesce(global_land_fraction, 0))
j_arid_geo_geo <- weighted_jaccard(merged_arid_geo_geo$global_land_fraction, merged_arid_geo_geo$network_frac)
msg("Aridity J (Geo vs Geo) = ", round(j_arid_geo_geo, 3))
add_metric("C", "aridity", "geo_vs_geo", "CGIAR Aridity Index v3.1 (own coverage)", ARIDITY_LAND_TOTAL_KM2,
           nrow(aridity_geo_geo), n_arid_geo_geo_classified, j_arid_geo_geo)

## ---- Geo vs Data: AI = P_ERA / FAO-56 PET, 1991-2020 ----------------------
monthly_era5_aridity <- dbConnect(duckdb(), dbdir = duckdb_path, read_only = TRUE)
monthly_met <- dbGetQuery(
  monthly_era5_aridity,
  "SELECT site_id, TIMESTAMP, TA_ERA, P_ERA, SW_IN_ERA, LW_IN_ERA, VPD_ERA, PA_ERA, WS_ERA
   FROM monthly WHERE dataset = 'ERA5'"
)
dbDisconnect(monthly_era5_aridity, shutdown = TRUE)
monthly_met <- monthly_met |>
  dplyr::filter(site_id %in% current_sites$site_id) |>
  dplyr::mutate(TIMESTAMP = as.Date(TIMESTAMP), year = lubridate::year(TIMESTAMP),
                month = lubridate::month(TIMESTAMP), ndays_row = lubridate::days_in_month(TIMESTAMP),
                p_tot = P_ERA * ndays_row) |>
  dplyr::filter(year >= KG_ERA5_PERIOD[1], year <= KG_ERA5_PERIOD[2])

met_clim <- monthly_met |>
  dplyr::group_by(site_id, month) |>
  dplyr::summarise(
    TA = mean(TA_ERA, na.rm = TRUE), SW = mean(SW_IN_ERA, na.rm = TRUE),
    LW = mean(LW_IN_ERA, na.rm = TRUE), VPD = mean(VPD_ERA, na.rm = TRUE),
    PA = mean(PA_ERA, na.rm = TRUE), WS = mean(WS_ERA, na.rm = TRUE),
    P = mean(p_tot, na.rm = TRUE), ndays = mean(ndays_row), n_years = dplyr::n(),
    .groups = "drop"
  )

fao56_et0_mm <- function(TA, SW, LW, VPD_hpa, PA_kpa, WS, ndays) {
  es <- 0.6108 * exp(17.27 * TA / (TA + 237.3))
  Delta <- 4098 * es / (TA + 237.3)^2
  ea <- es - VPD_hpa / 10
  gamma <- 0.000665 * PA_kpa
  u2 <- WS * 4.87 / log(67.8 * 10 - 5.42)
  Rns <- (1 - 0.23) * (SW * 0.0864)
  LWout_mj <- 0.96 * 4.903e-9 * (TA + 273.16)^4
  Rnl <- (LW * 0.0864) - LWout_mj
  Rn <- Rns + Rnl
  et0_day <- (0.408 * Delta * Rn + gamma * (900 / (TA + 273)) * u2 * (es - ea)) /
    (Delta + gamma * (1 + 0.34 * u2))
  ## Floored at 0: at high latitude in winter, Rn can be strongly negative
  ## (near-zero insolation, net longwave loss dominates), which can drive the
  ## raw FAO-56 formula negative for that month -- not physically meaningful
  ## (evapotranspiration cannot be negative) and standard FAO-56 practice is
  ## to floor ET0 at 0 per period before summing, rather than let a
  ## spuriously negative winter month distort the annual total (found via a
  ## sanity check: unclipped, annual PET could itself be negative or
  ## near-zero at cold sites, producing impossible/absurd AI values).
  pmax(et0_day, 0) * ndays
}
met_clim <- met_clim |> dplyr::mutate(et0_month = fao56_et0_mm(TA, SW, LW, VPD, PA, WS, ndays))

## Input-validity screen, found necessary by a post-hoc plausibility check
## (below): a handful of sites have individual ERA5 variables corrupted far
## beyond any physically possible value for that variable (e.g. US-Sne's
## LW_IN_ERA reaches ~32,000 W/m^2 in a winter month -- true downwelling
## longwave never exceeds roughly 700 W/m^2 even at the hottest real surface
## temperatures; CD-Ygb's VPD_ERA reaches ~1,660 hPa -- true VPD never
## exceeds roughly 12 hPa even in the driest deserts). Feeding these into
## fao56_et0_mm() produces a nonsensical annual PET (110,884 mm/yr and
## 44,002 mm/yr respectively, against a physically plausible terrestrial
## range of roughly 200-3,000 mm/yr) and hence a nonsensical AI. Screened
## PER SITE (any one invalid calendar month invalidates that site's PET for
## this panel) against generous physical ceilings, not tuned to these two
## cases specifically: LW_IN/SW_IN <0 or >1000 W/m^2, VPD <0 or >100 hPa,
## WS <=0 or >50 m/s, PA outside [50,110] kPa, TA outside [-90,60] deg C.
met_clim <- met_clim |>
  dplyr::mutate(invalid_month = LW < 0 | LW > 1000 | SW < 0 | SW > 1000 |
                  VPD < 0 | VPD > 100 | WS <= 0 | WS > 50 |
                  PA < 50 | PA > 110 | TA < -90 | TA > 60)
invalid_sites <- met_clim |> dplyr::filter(invalid_month) |> dplyr::distinct(site_id) |> dplyr::pull(site_id)
if (length(invalid_sites) > 0L) {
  msg("Sites with a physically invalid ERA5 input variable for PET (excluded, data-quality issue): ",
      paste(sort(invalid_sites), collapse = ", "))
  for (sid in invalid_sites) {
    log_exclusion(
      site_id = sid, variable = "aridity_era5 (Geo vs Data panel)", timestamp = "ALL",
      reason = "At least one ERA5 meteorological variable used by the FAO-56 PET calculation has a physically impossible value in at least one calendar month (e.g. LW_IN_ERA or VPD_ERA far outside any real-world range) -- ERA5 data-quality issue, not an aridity exclusion rule",
      threshold = "LW/SW<0 or >1000 W/m2; VPD<0 or >100 hPa; WS<=0 or >50 m/s; PA outside [50,110] kPa; TA outside [-90,60] C",
      excluded_by = "figure4_representativeness.R"
    )
  }
}

aridity_annual <- met_clim |>
  dplyr::filter(!site_id %in% invalid_sites) |>
  dplyr::group_by(site_id) |>
  dplyr::summarise(PET_mm = sum(et0_month), P_mm = sum(P), n_months = dplyr::n(),
                    n_years_min = min(n_years), .groups = "drop") |>
  dplyr::mutate(ai_value = P_mm / PET_mm, unep_class_7 = classify_unep7(ai_value))
msg("FAO-56 PET computed for ", nrow(aridity_annual), " / ", length(unique(met_clim$site_id)),
    " sites with valid inputs (", length(invalid_sites), " excluded for invalid ERA5 inputs; expect 781 total; ",
    sum(aridity_annual$n_months < 12L), " with <12 calendar months of ERA5 data)")
msg("AI summary: ", paste(capture.output(print(summary(aridity_annual$ai_value))), collapse = " | "))
implausible_pet <- aridity_annual |> dplyr::filter(PET_mm < 200 | PET_mm > 3000)
if (nrow(implausible_pet) > 0L) {
  msg("Sites with PET outside a generously plausible 200-3000 mm/yr range (kept, flagged not excluded -- ",
      "a known limitation of the net-radiation approximation at some sites, see PHASE 3 header comment, ",
      "not a data-validity violation like the sites excluded above):")
  print(as.data.frame(implausible_pet[, c("site_id", "PET_mm", "P_mm", "ai_value")]))
}

aridity_precip_excl <- compute_precip_exclusions(
  aridity_annual, p_era_col = "P_mm", slope9999_sites = slope9999_172,
  excluded_by = "figure4_representativeness.R", panel_name = "aridity_era5"
)
aridity_geo_data_pool <- aridity_annual |>
  dplyr::filter(site_id %in% aridity_precip_excl$site_id[!aridity_precip_excl$excluded_any])
n_arid_excl_ratio_only <- sum(aridity_precip_excl$excluded_p_era_ratio)
msg("Aridity Geo vs Data exclusions: ", length(slope9999_172), " GRP_ERA_DOWN + ",
    n_arid_excl_ratio_only, " P_ERA_MAX_RATIO-only = ", sum(aridity_precip_excl$excluded_any),
    " total excluded; n_eligible = ", nrow(aridity_geo_data_pool))
n_arid_geo_data_classified <- sum(!is.na(aridity_geo_data_pool$unep_class_7))
msg("Geo vs Data (AI=P_ERA/PET, dual exclusion): ", n_arid_geo_data_classified, " / ",
    nrow(aridity_geo_data_pool), " eligible sites classified")

cnt_arid_geo_data <- aridity_geo_data_pool |>
  dplyr::filter(!is.na(unep_class_7)) |>
  dplyr::count(unep_class_7, name = "n") |>
  dplyr::rename(class = unep_class_7) |>
  dplyr::mutate(network_frac = n / nrow(aridity_geo_data_pool))
merged_arid_geo_data <- aridity_global |>
  dplyr::select(class, global_land_fraction) |>
  dplyr::full_join(cnt_arid_geo_data, by = "class") |>
  dplyr::mutate(n = dplyr::coalesce(n, 0L), network_frac = dplyr::coalesce(network_frac, 0),
                global_land_fraction = dplyr::coalesce(global_land_fraction, 0))
j_arid_geo_data <- weighted_jaccard(merged_arid_geo_data$global_land_fraction, merged_arid_geo_data$network_frac)
msg("Aridity J (Geo vs Data) = ", round(j_arid_geo_data, 3))
add_metric("C", "aridity", "geo_vs_data", "CGIAR Aridity Index v3.1 (own coverage)", ARIDITY_LAND_TOTAL_KM2,
           nrow(aridity_geo_data_pool), n_arid_geo_data_classified, j_arid_geo_data)

## ---- Save panel C outputs --------------------------------------------------
fig4_aridity_path <- file.path(SNAP_DIR, "site_aridity_era5_fig4.csv")
current_sites |>
  dplyr::select(site_id) |>
  dplyr::left_join(aridity_annual, by = "site_id") |>
  dplyr::left_join(
    dplyr::select(aridity_precip_excl, site_id, ratio, ref_source,
                   excluded_grp_era_down, excluded_p_era_ratio, excluded_any),
    by = "site_id"
  ) |>
  dplyr::rename(excluded_fig4_geo_vs_data = excluded_any) |>
  dplyr::mutate(invalid_era5_input = site_id %in% invalid_sites) |>
  readr::write_csv(fig4_aridity_path)
write_output_metadata(
  fig4_aridity_path,
  input_sources = c(duckdb_path, CURRENT_SNAPSHOT, precip_ref_path, "site_koppen_era5_fig4.csv",
                     "review/diagnostics/precip_downscaling_provenance/table_2_site_groups.csv"),
  notes = paste0(
    "Panel C (aridity) Geo-vs-Data site-level data for figure4_representativeness.R, all 781 current-",
    "network sites (PET_mm/ai_value/unep_class_7 are NA for the 4 with invalid_era5_input=TRUE). ",
    "ai_value = P_mm (1991-2020 mean annual P_ERA) / PET_mm (FAO-56 Penman-Monteith reference ET, ",
    "computed monthly from 1991-2020 ERA5 climatological means and summed to an annual total -- see ",
    "script comments above PHASE 3 for the exact variables used and every approximation: wind height, ",
    "net-radiation estimation from SW_IN_ERA+LW_IN_ERA, es/Delta from mean T not Tmax/Tmin, G=0, ET0 ",
    "floored at 0 per month (standard FAO-56 practice for strongly-negative-Rn winter months)). ",
    "invalid_era5_input=TRUE (4 sites: CD-Ygb, DE-Zrk, FR-LBr, US-Sne) flags a physically impossible ",
    "raw ERA5 value (e.g. LW_IN_ERA ~30,000 W/m2 or VPD_ERA ~1,660 hPa) in >=1 calendar month -- an ",
    "ERA5 data-quality issue, excluded from this panel entirely (both as its own screen and inherently ",
    "via excluded_fig4_geo_vs_data, since these sites have no PET_mm to exclude by ratio). Two further ",
    "sites (DE-SbM, KE-Aq2) have implausible PET (0 and 122 mm/yr) from valid-looking raw inputs -- a ",
    "known limitation of the net-radiation approximation, not a data-validity violation -- kept, not ",
    "screened, but both happen to already be excluded by the other two rules (GRP_ERA_DOWN/ratio) so ",
    "neither affects the panel's final 594-site pool either way. unep_class_7 via the same CGIAR ",
    "UNEP-7 breakpoints as aridity_unep7_global_distribution.csv. Dual exclusion as panel A ",
    "(excluded_fig4_geo_vs_data = excluded_grp_era_down | excluded_p_era_ratio): the 172 ",
    "GRP_ERA_DOWN sites plus any site whose P_ERA exceeds P_ERA_MAX_RATIO times its reference MAP. ",
    "CAPTION NOTE (required): CGIAR's own baseline period is 1970-2000; this Geo-vs-Data calculation ",
    "uses 1991-2020 ERA5 -- a period mismatch between this panel's two sides not present elsewhere."
  )
)
msg("Saved: ", fig4_aridity_path)

# ==============================================================================
# Save accumulated metrics (all phases implemented so far)
# ==============================================================================
metrics_fig4_path <- file.path(SNAP_DIR, "representativeness_metrics_fig4.csv")
metrics_df <- dplyr::bind_rows(metrics_rows)
readr::write_csv(metrics_df, metrics_fig4_path)
write_output_metadata(
  metrics_fig4_path,
  input_sources = c("site_koppen_beck2023.csv", "site_koppen_era5_fig4.csv", "site_igbp_fig4.csv",
                     "site_aridity.csv", "site_aridity_era5_fig4.csv",
                     "koppen_beck2023_global_distribution.csv", "igbp_mcd12c1_global_distribution.csv",
                     "aridity_unep7_global_distribution.csv"),
  notes = paste0(
    "Weighted-Jaccard metrics for the new (2026-10) Figure 4, built incrementally across phases -- ",
    "see SESSION_LOG.md for each phase. Separate from the shared data/snapshots/representativeness_metrics.csv ",
    "(which backs Figs 001-008 and is not touched by this script). One row per panel x comparison. ",
    "This run includes phases implemented so far: panel A (Koppen), panel B (IGBP land cover), ",
    "panel C (aridity)."
  )
)
msg("Saved: ", metrics_fig4_path)
print(as.data.frame(metrics_df))

msg("\n=== figure4_representativeness.R complete (phases implemented so far) ===")
