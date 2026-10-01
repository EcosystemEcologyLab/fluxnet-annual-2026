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

## ---- Geo vs Data: ERA5-local classification, no MAP screen, 172 excluded --
duckdb_path <- "data/duckdb/fluxnet.duckdb"
con <- dbConnect(duckdb(), dbdir = duckdb_path, read_only = TRUE)
monthly_era5 <- dbGetQuery(con, "SELECT site_id, TIMESTAMP, TA_ERA, P_ERA FROM monthly WHERE dataset = 'ERA5'")
dbDisconnect(con, shutdown = TRUE)
monthly_era5 <- monthly_era5 |> dplyr::filter(site_id %in% current_sites$site_id)
msg("ERA5 monthly rows for current network: ", nrow(monthly_era5),
    " (", length(unique(monthly_era5$site_id)), " sites)")

## legend = NULL: compute_site_koppen_era5() only uses `legend` to attach
## koppen_class_code/koppen_class_name/koppen_main_name (it requires those
## exact column names); kg_leg_df above only has koppen_class/r/g/b/
## koppen_twoletter (built for KG13_COLORS, a different shape), and this
## panel doesn't need the name columns anyway (class codes/colours come
## from TL_ORDER/KG13_COLORS).
kg_era5_nomap <- compute_site_koppen_era5(monthly_era5, map_max_mm = Inf, legend = NULL)

precip_groups <- readr::read_csv(
  "review/diagnostics/precip_downscaling_provenance/table_2_site_groups.csv",
  show_col_types = FALSE)
slope9999_172 <- precip_groups$site_id[precip_groups$p_group == "not_fitted_slope_9999"]
msg("Sites in not_fitted_slope_9999 group (excluded from this panel): ", length(slope9999_172))
if (length(slope9999_172) != 172L) {
  warning("Expected 172 sites in not_fitted_slope_9999, found ", length(slope9999_172))
}

for (sid in slope9999_172) {
  log_exclusion(
    site_id = sid, variable = "koppen_era5 (Geo vs Data panel)", timestamp = "ALL",
    reason = "Site is in the precip_downscaling_provenance not_fitted_slope_9999 group (P_ERA regression against measured P has no usable slope) -- excluded from the Koppen Geo-vs-Data panel entirely, not just left unclassified",
    threshold = "p_group == 'not_fitted_slope_9999'", excluded_by = "figure4_representativeness.R"
  )
}

kg_geo_data_pool <- kg_era5_nomap |> dplyr::filter(!site_id %in% slope9999_172)
n_geo_data_classified <- sum(!is.na(kg_geo_data_pool$koppen_twoletter))
msg("Geo vs Data (ERA5 local, no MAP screen, 172 excluded): ",
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
# Save Phase 1 outputs
# ============================================================================
msg("\n=== Saving Phase 1 outputs ===")

fig4_kg_era5_path <- file.path(SNAP_DIR, "site_koppen_era5_fig4.csv")
kg_era5_nomap |>
  dplyr::mutate(excluded_fig4_geo_vs_data = site_id %in% slope9999_172) |>
  readr::write_csv(fig4_kg_era5_path)
write_output_metadata(
  fig4_kg_era5_path,
  input_sources = c(duckdb_path, CURRENT_SNAPSHOT,
                     "review/diagnostics/precip_downscaling_provenance/table_2_site_groups.csv"),
  notes = paste0(
    "Koppen classification for figure4_representativeness.R's Geo-vs-Data panel A. Same method as ",
    "site_koppen_era5.csv (R/climate_classification.R::compute_site_koppen_era5(), 1991-2020 ERA5 ",
    "monthly T/P normal, >= 20 of 30 years required) EXCEPT the KG_ERA5_MAP_MAX_MM=5000mm/yr ",
    "per-site-year screen is NOT applied here (map_max_mm=Inf). `excluded_fig4_geo_vs_data` flags ",
    "the 172 sites in the precip_downscaling_provenance not_fitted_slope_9999 group; these are ",
    "classified in this file (for reference) but must be excluded from the Geo-vs-Data panel's ",
    "numerator AND denominator (n_eligible = 781-172 = 609), not merely treated as unclassified. ",
    "All 609 eligible sites classify under this rule (0 unclassified within the pool); flagged ",
    "implausible precipitation values among the 4 sites newly reclassifiable vs. the old MAP-",
    "screened rule: US-HB4 (known ERA5 spatial-averaging artifact) and NO-And -- see SESSION_LOG.md."
  )
)
msg("Saved: ", fig4_kg_era5_path)

metrics_fig4_path <- file.path(SNAP_DIR, "representativeness_metrics_fig4.csv")
metrics_df <- dplyr::bind_rows(metrics_rows)
readr::write_csv(metrics_df, metrics_fig4_path)
write_output_metadata(
  metrics_fig4_path,
  input_sources = c("site_koppen_beck2023.csv", fig4_kg_era5_path, "koppen_beck2023_global_distribution.csv"),
  notes = paste0(
    "Weighted-Jaccard metrics for the new (2026-10) Figure 4, built incrementally across phases -- ",
    "see SESSION_LOG.md for each phase. Separate from the shared data/snapshots/representativeness_metrics.csv ",
    "(which backs Figs 001-008 and is not touched by this script). One row per panel x comparison. ",
    "This run includes phases implemented so far: panel A (Koppen) only."
  )
)
msg("Saved: ", metrics_fig4_path)
print(as.data.frame(metrics_df))

msg("\n=== figure4_representativeness.R: Phase 1 complete ===")
