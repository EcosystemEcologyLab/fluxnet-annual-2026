## figure4_representativeness.R
##
## Production script for the NEW Figure 4 (current network, 781 sites): two
## full versions -- "Geo vs Geo" (global land distribution vs. the gridded
## product's own value at each tower's coordinates) and "Geo vs Data"
## (global land distribution vs. each site's own measured/derived value) --
## six panels each, in order: A Koppen-Geiger, B land cover (IGBP), C
## aridity, D biomass, E NEE, F ET.
##
## ---- Naming (2026-10-02): Geo vs Data is main-text Figure 4; Geo vs Geo is
## a supplemental figure. Output basenames are fig_04_representativeness.*
## (Geo vs Data) and supp_representativeness_geo_vs_geo.* (Geo vs Geo), via
## fig4_output_name() below -- the internal `comparison` value
## ("geo_vs_data"/"geo_vs_geo") still drives all data-selection logic
## throughout this script; only the output basename changed. Per-panel
## tables use the same basenames. These FIG_DIR basenames are this script's
## own canonical, unnumbered names and are unchanged by figure stage 6
## (2026-10-02): only the DRAFT_DIR/SUPFIGS_DIR *copy* filenames were
## renumbered, to fig_05_representativeness.* (was referred to as Figure 4
## throughout this script/its legends before the stage-6 renumbering; now
## Figure 5) and figS4_representativeness_geo_vs_geo.* (now Supplementary
## Figure S4) respectively -- see DEST_BASENAME below and
## docs/figure_inventory.md. The previous Figure 4
## (fig_04_current_network_sampling_ratios.png, from fig_rep001_current.png
## via scripts/figure_representativeness_summary.R) is superseded and its
## draft_manuscript_v1/ copy moved to draft_manuscript_v1/deprecated/.
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
## panels (Koppen, aridity): exclude a site if its 1991-2020 mean annual
## P_ERA exceeds P_ERA_MAX_RATIO=3 (R/pipeline_config.R) times a reference
## mean annual precipitation. Additive to the 172 GRP_ERA_DOWN sites, not a
## replacement. First version (single preferred reference) caught 12 sites
## beyond the 172, above the task's "~10" threshold -- Phase 5 was held.
##
## ---- REVISION (2026-10-02, same day): dual-reference AND logic -----------
## Revised per instruction: a site is now excluded by the ratio rule only if
## P_ERA exceeds P_ERA_MAX_RATIO times EVERY reference available for it --
## both BADM MAP (where present/non-zero) AND WorldClim BIO12 at the tower
## (extracted for all 781 sites), not just whichever one was preferred.
## Where only one reference exists, that one decides alone. This stops a
## single wrong/stale BADM value from excluding a site BIO12 would confirm
## is sound. Phase 5 now proceeds regardless of the revised count (per this
## revision's explicit instruction). See SESSION_LOG.md for the full report:
## the revised list, which of the original 12 are no longer excluded, and
## panels A/C recomputed.

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
source("R/utils.R")
source("R/climate_classification.R")
source("R/units.R")
source("R/site_annual_fluxes.R")
source("R/nature_format.R")
source("R/plot_constants.R")
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
## SupFigs/ (revised 2026-10-02): the geo_vs_geo figure is a Supplementary
## Figure (target journal Scientific Data has no Extended Data concept) --
## draft_manuscript_v1/ itself keeps only main-text figures. Only the
## DRAFT_DIR *copy destination* for geo_vs_geo moves here; its primary
## output (FIG_DIR, review/figures/representativeness/) is unchanged.
SUPFIGS_DIR <- file.path(DRAFT_DIR, "SupFigs")
fs::dir_create(FIG_DIR)
fs::dir_create(DRAFT_DIR)
fs::dir_create(SUPFIGS_DIR)

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

## Returns a data frame (site_id, p_era_map_mm, badm_map_mm, ratio_badm,
## bio12_mm, ratio_bio12, excluded_grp_era_down, excluded_p_era_ratio,
## excluded_any) for every site in `p_era_df` (must have site_id + a P_ERA
## 1991-2020 mean annual column).
## `excluded_by` is passed to log_exclusion() for the ratio rule only (the
## GRP_ERA_DOWN rule was already logged once, by Phase 1, with its own
## wording -- not re-logged here to avoid duplicate log rows on reruns of
## later phases within the same script execution... but see note at call
## sites: each phase that calls this still logs its OWN GRP_ERA_DOWN rows,
## since log_exclusion() appends and this script does not deduplicate its
## own log output across phases/panels by design (every exclusion row names
## which panel excluded it via `variable`).
## Revised 2026-10-02: a site is now excluded by the ratio rule only if its
## P_ERA exceeds P_ERA_MAX_RATIO times EVERY reference available for it --
## both BADM MAP (where present/non-zero) and WorldClim BIO12 at the tower,
## not just whichever one `precip_reference` happened to prefer. Where only
## one reference exists (BADM MAP absent or zero), that one reference alone
## decides, same as before. This stops a single wrong/stale BADM metadata
## value from excluding a site whose precipitation is otherwise sound --
## the original single-reference rule would exclude on BADM MAP alone even
## if BIO12 agreed with P_ERA.
flag_precip_exclusions <- function(p_era_df, p_era_col, slope9999_sites) {
  p_era_df |>
    dplyr::rename(p_era_map_mm = dplyr::all_of(p_era_col)) |>
    dplyr::left_join(dplyr::select(precip_reference, site_id, badm_map_mm, bio12_mm), by = "site_id") |>
    dplyr::mutate(
      ratio_badm  = dplyr::if_else(!is.na(badm_map_mm) & badm_map_mm > 0, p_era_map_mm / badm_map_mm, NA_real_),
      ratio_bio12 = p_era_map_mm / bio12_mm,
      exceeds_badm  = is.na(ratio_badm) | ratio_badm > P_ERA_MAX_RATIO,   # NA (no BADM) treated as "not a blocker"
      exceeds_bio12 = !is.na(ratio_bio12) & ratio_bio12 > P_ERA_MAX_RATIO,
      ## Low-side mirror of the above (P_ERA_MIN_RATIO, added 2026-10-02): a
      ## site fails the low side only if it is below P_ERA_MIN_RATIO times
      ## EVERY reference available -- same dual-reference AND logic, same
      ## "NA badm = not a blocker" treatment.
      below_badm  = is.na(ratio_badm) | ratio_badm < P_ERA_MIN_RATIO,
      below_bio12 = !is.na(ratio_bio12) & ratio_bio12 < P_ERA_MIN_RATIO,
      excluded_grp_era_down     = site_id %in% slope9999_sites,
      excluded_p_era_ratio_high = exceeds_badm & exceeds_bio12 & !excluded_grp_era_down,
      excluded_p_era_ratio_low  = below_badm & below_bio12 & !excluded_grp_era_down,
      excluded_p_era_ratio      = excluded_p_era_ratio_high | excluded_p_era_ratio_low,
      excluded_any = excluded_grp_era_down | excluded_p_era_ratio
    )
}

compute_precip_exclusions <- function(p_era_df, p_era_col, slope9999_sites, excluded_by, panel_name) {
  d <- flag_precip_exclusions(p_era_df, p_era_col, slope9999_sites)

  for (sid in slope9999_sites) {
    log_exclusion(
      site_id = sid, variable = paste0(panel_name, " (Geo vs Data panel)"), timestamp = "ALL",
      reason = "Site is in the precip_downscaling_provenance not_fitted_slope_9999 (GRP_ERA_DOWN) group",
      threshold = "p_group == 'not_fitted_slope_9999'", excluded_by = excluded_by
    )
  }
  ratio_caught <- d |> dplyr::filter(excluded_p_era_ratio_high)
  for (i in seq_len(nrow(ratio_caught))) {
    refs_txt <- if (!is.na(ratio_caught$ratio_badm[i])) {
      sprintf("BADM MAP=%.1f mm/yr (ratio %.2f), WorldClim BIO12=%.1f mm/yr (ratio %.2f)",
              ratio_caught$badm_map_mm[i], ratio_caught$ratio_badm[i],
              ratio_caught$bio12_mm[i], ratio_caught$ratio_bio12[i])
    } else {
      sprintf("WorldClim BIO12=%.1f mm/yr (ratio %.2f) -- only reference available (no BADM MAP)",
              ratio_caught$bio12_mm[i], ratio_caught$ratio_bio12[i])
    }
    log_exclusion(
      site_id = ratio_caught$site_id[i], variable = paste0(panel_name, " (Geo vs Data panel)"),
      timestamp = "ALL",
      reason = sprintf("P_ERA (%.1f mm/yr) exceeds P_ERA_MAX_RATIO times EVERY available reference: %s",
                        ratio_caught$p_era_map_mm[i], refs_txt),
      threshold = paste0("P_ERA_MAX_RATIO=", P_ERA_MAX_RATIO, " (all available references)"),
      excluded_by = excluded_by
    )
  }
  ratio_low_caught <- d |> dplyr::filter(excluded_p_era_ratio_low)
  for (i in seq_len(nrow(ratio_low_caught))) {
    refs_txt <- if (!is.na(ratio_low_caught$ratio_badm[i])) {
      sprintf("BADM MAP=%.1f mm/yr (ratio %.2f), WorldClim BIO12=%.1f mm/yr (ratio %.2f)",
              ratio_low_caught$badm_map_mm[i], ratio_low_caught$ratio_badm[i],
              ratio_low_caught$bio12_mm[i], ratio_low_caught$ratio_bio12[i])
    } else {
      sprintf("WorldClim BIO12=%.1f mm/yr (ratio %.2f) -- only reference available (no BADM MAP)",
              ratio_low_caught$bio12_mm[i], ratio_low_caught$ratio_bio12[i])
    }
    log_exclusion(
      site_id = ratio_low_caught$site_id[i], variable = paste0(panel_name, " (Geo vs Data panel)"),
      timestamp = "ALL",
      reason = sprintf("P_ERA (%.1f mm/yr) is below P_ERA_MIN_RATIO times EVERY available reference: %s",
                        ratio_low_caught$p_era_map_mm[i], refs_txt),
      threshold = paste0("P_ERA_MIN_RATIO=", round(P_ERA_MIN_RATIO, 4), " (all available references)"),
      excluded_by = excluded_by
    )
  }
  d
}

## ---- Required reporting: revised dual-reference ratio rule ---------------
## Uses Koppen's kg_era5_nomap$map_mm as "1991-2020 mean annual P_ERA" -- this
## is a general ERA5 climatology byproduct, not Koppen-specific (same number
## aridity's Geo-vs-Data panel uses in Phase 3). The OLD (single-reference)
## 12-site list is recomputed here too, purely for the "which of the
## previous 12 are no longer excluded" comparison -- not logged, informational.
ratio_check <- flag_precip_exclusions(
  kg_era5_nomap |> dplyr::select(site_id, map_mm), p_era_col = "map_mm", slope9999_sites = slope9999_172
)

old_12 <- c("US-HB4", "CA-CF2", "US-RGF", "NO-And", "CA-CF1", "EE-Rng", "IT-Niv",
            "CA-HPC", "US-DS1", "US-DS2", "US-BRG", "GL-ZaF")
new_caught_beyond_172 <- ratio_check |> dplyr::filter(excluded_p_era_ratio) |>
  dplyr::arrange(dplyr::desc(dplyr::coalesce(ratio_badm, ratio_bio12)))
msg("\n--- Revised (dual-reference, AND logic) P_ERA_MAX_RATIO=", P_ERA_MAX_RATIO, " rule ---")
msg("Sites caught beyond the 172 (n=", nrow(new_caught_beyond_172), "), with both references and both ratios:")
print(as.data.frame(new_caught_beyond_172[, c("site_id", "p_era_map_mm", "badm_map_mm", "ratio_badm",
                                               "bio12_mm", "ratio_bio12")]))
no_longer_excluded <- setdiff(old_12, new_caught_beyond_172$site_id)
still_excluded <- intersect(old_12, new_caught_beyond_172$site_id)
msg("Of the previous 12: still excluded (", length(still_excluded), ") = ",
    paste(still_excluded, collapse = ", "))
msg("Of the previous 12: NO LONGER excluded (", length(no_longer_excluded), ") = ",
    paste(no_longer_excluded, collapse = ", "), " -- BIO12 did not confirm P_ERA as implausible for these.")
msg("Not holding Phase 5 for this (per task instruction) regardless of count.")

for (r in c(2, 3, 4)) {
  n_beyond_r <- sum((dplyr::coalesce(ratio_check$ratio_badm, Inf) > r &
                       dplyr::coalesce(ratio_check$ratio_bio12, Inf) > r) & !ratio_check$excluded_grp_era_down,
                    na.rm = TRUE)
  msg("Dual-reference ratio > ", r, " (both refs, beyond the 172): ", n_beyond_r, " sites")
}

four_named <- ratio_check |> dplyr::filter(site_id %in% c("CA-CF2", "IT-Niv", "NO-And", "US-HB4"))
msg("\nCA-CF2 / IT-Niv / NO-And / US-HB4 all caught by the revised rule? ",
    all(four_named$excluded_p_era_ratio))
print(as.data.frame(four_named[, c("site_id", "p_era_map_mm", "badm_map_mm", "ratio_badm", "bio12_mm", "ratio_bio12")]))

## ---- Required reporting: new low-side rule (P_ERA_MIN_RATIO, added 2026-
## 10-02), same dual-reference AND logic and same reporting shape as the
## high-side block above -- the general (not panel-specific) catch list and
## 1/2, 1/3, 1/4 sensitivity counts the task asks for.
low_caught_beyond_172 <- ratio_check |> dplyr::filter(excluded_p_era_ratio_low) |>
  dplyr::arrange(dplyr::coalesce(ratio_badm, ratio_bio12))
msg("\n--- New P_ERA_MIN_RATIO=", round(P_ERA_MIN_RATIO, 3), " (1/", round(1 / P_ERA_MIN_RATIO), ") rule, dual-reference AND logic ---")
msg("Sites caught beyond the 172 (n=", nrow(low_caught_beyond_172), "), with both references and both ratios:")
print(as.data.frame(low_caught_beyond_172[, c("site_id", "p_era_map_mm", "badm_map_mm", "ratio_badm",
                                               "bio12_mm", "ratio_bio12")]))
for (r in c(2, 3, 4)) {
  n_below_r <- sum((dplyr::coalesce(ratio_check$ratio_badm, -Inf) < 1 / r &
                       dplyr::coalesce(ratio_check$ratio_bio12, -Inf) < 1 / r) & !ratio_check$excluded_grp_era_down,
                    na.rm = TRUE)
  msg("Dual-reference ratio < 1/", r, " (both refs, beyond the 172): ", n_below_r, " sites")
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

## ---- Geo vs Data: PI-reported class first, ERA5 fallback second ----------
## Revised 2026-10-02 (main-figure panel a only; the Geo vs Geo supplemental
## panel above is NOT affected by this revision). review/diagnostics/
## koppen_pi_vs_era5/ (2026-10-02 (3) SESSION_LOG entry) found the ERA5-
## derived class disagrees with the PI-reported class (BADM CLIMATE_KOEPPEN)
## more often than it agrees (59.5% full-class agreement, n=603 comparable
## sites), while the PI class agrees much better with the independent Beck
## 2023 raster (69.2%) -- i.e. ERA5 is the less reliable of the two
## available Geo-vs-Data sources for this panel, even though that same
## diagnostic's naive network-wide Jaccard swap (J=0.359, not restricted to
## the panel's own eligible pool) looked worse than the ERA5-only figure
## (J=0.411, n=599) -- not an apples-to-apples comparison, since it compared
## different populations. This panel now uses the PI-reported class (case-
## normalised against the 30 canonical Koppen codes, same lookup as
## scripts/diagnostics/koppen_pi_vs_era5.R) for every site that has one, and
## falls back to the ERA5-local class (kg_era5_nomap, computed above in the
## SHARED section) only for sites without a valid PI value. The
## precipitation-dependent exclusion rules (172 GRP_ERA_DOWN,
## P_ERA_MAX_RATIO, P_ERA_MIN_RATIO) screen the ERA5 climatology itself,
## which a PI-reported class never uses -- so they apply ONLY to fallback
## sites; a site with a PI-reported class is NEVER excluded from this panel.
VALID_KG30 <- c(
  "Af", "Am", "Aw",
  "BWh", "BWk", "BSh", "BSk",
  "Csa", "Csb", "Csc", "Cwa", "Cwb", "Cwc", "Cfa", "Cfb", "Cfc",
  "Dsa", "Dsb", "Dsc", "Dsd", "Dwa", "Dwb", "Dwc", "Dwd", "Dfa", "Dfb", "Dfc", "Dfd",
  "ET", "EF"
)
KG30_CANON_LOOKUP <- setNames(VALID_KG30, toupper(VALID_KG30))

badm_kg_pi_raw <- badm |>
  dplyr::filter(VARIABLE == "CLIMATE_KOEPPEN", !is.na(DATAVALUE)) |>
  dplyr::distinct(SITE_ID, .keep_all = TRUE) |>
  dplyr::transmute(site_id = SITE_ID, pi_raw = DATAVALUE)

pi_class_df <- current_sites |>
  dplyr::select(site_id) |>
  dplyr::left_join(badm_kg_pi_raw, by = "site_id") |>
  dplyr::mutate(
    pi_canonical = unname(KG30_CANON_LOOKUP[toupper(pi_raw)]),
    pi_twoletter = substr(pi_canonical, 1, 2)
  )
n_pi <- sum(!is.na(pi_class_df$pi_twoletter))
msg("Panel A PI-reported class (BADM CLIMATE_KOEPPEN, case-normalised): ", n_pi, " / ",
    nrow(pi_class_df), " sites have a valid class")

## kg_era5_nomap/slope9999_172 computed in the SHARED section above.
kg_precip_excl <- compute_precip_exclusions(
  kg_era5_nomap, p_era_col = "map_mm", slope9999_sites = slope9999_172,
  excluded_by = "figure4_representativeness.R", panel_name = "koppen_era5"
)

panel_a_source_df <- pi_class_df |>
  dplyr::left_join(dplyr::select(kg_era5_nomap, site_id, era5_twoletter = koppen_twoletter), by = "site_id") |>
  dplyr::left_join(
    dplyr::select(kg_precip_excl, site_id, excluded_grp_era_down, excluded_p_era_ratio_high,
                   excluded_p_era_ratio_low, excluded_any),
    by = "site_id"
  ) |>
  dplyr::mutate(
    has_pi = !is.na(pi_twoletter),
    panel_a_source = dplyr::case_when(
      has_pi                                ~ "PI",
      !has_pi & excluded_grp_era_down       ~ "excluded_grp_era_down",
      !has_pi & excluded_p_era_ratio_high   ~ "excluded_p_era_ratio_high",
      !has_pi & excluded_p_era_ratio_low    ~ "excluded_p_era_ratio_low",
      TRUE                                  ~ "era5_fallback"
    ),
    class_used = dplyr::if_else(has_pi, pi_twoletter, era5_twoletter)
  )
msg("Panel A source counts: ", paste(capture.output(print(table(panel_a_source_df$panel_a_source))), collapse = " | "))

kg_geo_data_pool <- panel_a_source_df |> dplyr::filter(panel_a_source %in% c("PI", "era5_fallback"))
n_excl_grp        <- sum(panel_a_source_df$panel_a_source == "excluded_grp_era_down")
n_excl_ratio_high <- sum(panel_a_source_df$panel_a_source == "excluded_p_era_ratio_high")
n_excl_ratio_low  <- sum(panel_a_source_df$panel_a_source == "excluded_p_era_ratio_low")
msg("Geo vs Data exclusions (fallback sites only -- PI-sourced sites are never excluded): ",
    n_excl_grp, " GRP_ERA_DOWN + ", n_excl_ratio_high, " P_ERA_MAX_RATIO + ",
    n_excl_ratio_low, " P_ERA_MIN_RATIO = ", n_excl_grp + n_excl_ratio_high + n_excl_ratio_low,
    " total excluded; n_eligible = ", nrow(kg_geo_data_pool),
    " (", n_pi, " PI + ", nrow(kg_geo_data_pool) - n_pi, " ERA5 fallback)")
n_geo_data_classified <- sum(!is.na(kg_geo_data_pool$class_used))
msg("Geo vs Data (PI-first, ERA5 fallback): ", n_geo_data_classified, " / ", nrow(kg_geo_data_pool),
    " eligible sites classified")

cnt_geo_data <- kg_geo_data_pool |>
  dplyr::filter(!is.na(class_used)) |>
  dplyr::count(class_used, name = "n") |>
  dplyr::rename(class = class_used) |>
  dplyr::mutate(network_frac = n / nrow(kg_geo_data_pool))
merged_geo_data <- kg_global |>
  dplyr::select(class, global_land_fraction) |>
  dplyr::full_join(cnt_geo_data, by = "class") |>
  dplyr::mutate(n = dplyr::coalesce(n, 0L), network_frac = dplyr::coalesce(network_frac, 0),
                global_land_fraction = dplyr::coalesce(global_land_fraction, 0))
j_kg_geo_data <- weighted_jaccard(merged_geo_data$global_land_fraction, merged_geo_data$network_frac)
msg("Koppen J (Geo vs Data, PI-first) = ", round(j_kg_geo_data, 3),
    " (previous ERA5-only figure: J=0.411, n=599)")
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
    dplyr::select(panel_a_source_df, site_id, pi_raw, pi_canonical, pi_twoletter,
                   panel_a_source, panel_a_class_used = class_used),
    by = "site_id"
  ) |>
  dplyr::left_join(
    dplyr::select(kg_precip_excl, site_id, badm_map_mm, ratio_badm, bio12_mm, ratio_bio12,
                   excluded_grp_era_down, excluded_p_era_ratio_high, excluded_p_era_ratio_low,
                   excluded_p_era_ratio, excluded_any),
    by = "site_id"
  ) |>
  dplyr::rename(excluded_fig4_geo_vs_data = excluded_any) |>
  dplyr::mutate(panel_a_eligible = panel_a_source %in% c("PI", "era5_fallback")) |>
  readr::write_csv(fig4_kg_era5_path)
write_output_metadata(
  fig4_kg_era5_path,
  input_sources = c(duckdb_path, CURRENT_SNAPSHOT, precip_ref_path, badm_path,
                     "review/diagnostics/precip_downscaling_provenance/table_2_site_groups.csv",
                     "review/diagnostics/koppen_pi_vs_era5/"),
  notes = paste0(
    "Koppen classification for figure4_representativeness.R's panel A. koppen_twoletter/map_mm/etc. ",
    "(from compute_site_koppen_era5(), 1991-2020 ERA5 monthly T/P normal, >= 20 of 30 years required, ",
    "KG_ERA5_MAP_MAX_MM NOT applied here i.e. map_max_mm=Inf) are the ERA5-local classification used ",
    "ONLY as a fallback. `excluded_fig4_geo_vs_data` (= excluded_grp_era_down | excluded_p_era_ratio, ",
    "excluded_p_era_ratio = excluded_p_era_ratio_high | excluded_p_era_ratio_low) is a pure precip-rule ",
    "flag on the ERA5 climatology -- (1) the 172 not_fitted_slope_9999 (GRP_ERA_DOWN) sites, (2) P_ERA ",
    "exceeding P_ERA_MAX_RATIO=", P_ERA_MAX_RATIO, " times EVERY available reference (ratio_badm AND ",
    "ratio_bio12), (3) P_ERA below P_ERA_MIN_RATIO=", round(P_ERA_MIN_RATIO, 3), " times EVERY available ",
    "reference -- same dual-reference AND logic both sides, see methods_precip_exclusions.md. REVISED ",
    "2026-10-02: panel A's actual Geo-vs-Data side (main figure only) no longer uses this ERA5 class and ",
    "its exclusion flag directly for every site -- it uses the PI-reported class (BADM CLIMATE_KOEPPEN, ",
    "case-normalised against the 30 canonical Koppen codes: pi_raw/pi_canonical/pi_twoletter) for every ",
    "site that has one, falling back to the ERA5 class/exclusion flag above only for sites without a PI ",
    "class. `panel_a_source` records which: 'PI', 'era5_fallback', or the specific rule that excluded a ",
    "fallback site ('excluded_grp_era_down'/'excluded_p_era_ratio_high'/'excluded_p_era_ratio_low'); a ",
    "PI-sourced site is NEVER excluded even if its own ERA5 climatology would have failed one of these ",
    "rules. panel_a_class_used/panel_a_eligible are the class and inclusion flag actually used by the ",
    "panel. See review/diagnostics/koppen_pi_vs_era5/ for the agreement/coverage analysis that prompted ",
    "this change, and SESSION_LOG.md for the before/after n and J."
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
## the PI vocabulary; folded into a 16th "Other" bin on the global/Geo side
## (land area counted, like the existing LULC high-level axis's own "Other"
## category), which can never receive a site count since no PI reports
## Water or Urban -- CONFIRMED 2026-10-02 (task instruction: "keep MODIS
## water and urban cells in the land total"), not excluded from the land
## total, since the task's land total (147.3M km2) must match
## the Koppen/biomass panels' own total exactly.
msg("\n=== PHASE 2: Land cover as IGBP (panel B) ===")

## IGBP_ORDER/IGBP_COLORS (task 2, 2026-10-02): the shared 15-class palette
## from R/plot_constants.R::PAPER_IGBP_ORDER/PAPER_IGBP_COLOURS -- originally
## defined HERE and promoted there as the paper-wide source of truth -- plus
## this script's own "Other" bin (panel B's merged Water+Urban land-cover
## class), which is specific to this script's raster classification and not
## part of the shared point/bar-colouring palette.
IGBP_ORDER <- c(PAPER_IGBP_ORDER, "Other")
IGBP_CODE_TO_CLASS <- c(
  "0" = "Other", "1" = "ENF", "2" = "EBF", "3" = "DNF", "4" = "DBF", "5" = "MF",
  "6" = "CSH", "7" = "OSH", "8" = "WSA", "9" = "SAV", "10" = "GRA", "11" = "WET",
  "12" = "CRO", "13" = "Other", "14" = "CVM", "15" = "SNO", "16" = "BSV"
)
## Colours: the standard MCD12 IGBP legend palette (2026-10-02, revised from
## this script's first, invented palette per task instruction). Source:
## Google Earth Engine's documented default visualization palette for
## MODIS/061/MCD12Q1 band LC_Type1 (IGBP classification) -- the same IGBP
## class codes 1-16 this script already uses, confirmed via
## https://developers.google.com/earth-engine/datasets/catalog/MODIS_061_MCD12Q1
## (fetched 2026-10-02; GEE's table lists Water as value 17, but the hex
## #1c0dff is unambiguous and matches this script's code 0 = Water in the
## actual MCD12C1 raster, confirmed against this file's own terra::freq()
## output). "Other" (this script's merged Water+Urban bin) has no official
## single colour in the source legend -- ADAPTATION, flagged: uses Urban's
## own official grey (#a5a5a5), the larger and more land-relevant of the two
## merged classes, rather than Water's blue (#1c0dff), which would read as
## open water and mislead.
IGBP_COLORS <- c(PAPER_IGBP_COLOURS, Other = "#a5a5a5")

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
n_arid_excl_ratio_high <- sum(aridity_precip_excl$excluded_p_era_ratio_high)
n_arid_excl_ratio_low  <- sum(aridity_precip_excl$excluded_p_era_ratio_low)
msg("Aridity Geo vs Data exclusions: ", length(slope9999_172), " GRP_ERA_DOWN + ",
    n_arid_excl_ratio_high, " P_ERA_MAX_RATIO + ", n_arid_excl_ratio_low, " P_ERA_MIN_RATIO = ",
    sum(aridity_precip_excl$excluded_any), " total excluded; n_eligible = ", nrow(aridity_geo_data_pool))
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
    dplyr::select(aridity_precip_excl, site_id, badm_map_mm, ratio_badm, bio12_mm, ratio_bio12,
                   excluded_grp_era_down, excluded_p_era_ratio_high, excluded_p_era_ratio_low,
                   excluded_p_era_ratio, excluded_any),
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
    "neither affects the panel's final eligible pool either way. unep_class_7 via the same CGIAR ",
    "UNEP-7 breakpoints as aridity_unep7_global_distribution.csv. Three-rule exclusion, same rules as ",
    "panel A's ERA5 fallback side (excluded_fig4_geo_vs_data = excluded_grp_era_down | ",
    "excluded_p_era_ratio_high | excluded_p_era_ratio_low): the 172 GRP_ERA_DOWN sites; P_ERA exceeding ",
    "P_ERA_MAX_RATIO=", P_ERA_MAX_RATIO, " times EVERY reference available (dual-reference AND logic, ",
    "revised 2026-10-02); and P_ERA below P_ERA_MIN_RATIO=", round(P_ERA_MIN_RATIO, 3),
    " times EVERY reference available (added 2026-10-02, same AND logic). Aridity has no PI-reported ",
    "analogue, so (unlike panel A) this panel's exclusion rules are unchanged by the panel A PI-first ",
    "revision. CAPTION NOTE (required): CGIAR's own baseline period is 1970-2000; this Geo-vs-Data ",
    "calculation uses 1991-2020 ERA5 -- a period mismatch between this panel's two sides not present ",
    "elsewhere."
  )
)
msg("Saved: ", fig4_aridity_path)

## ---- Required reporting: exclusion trace for panels A and C --------------
## Every site excluded from each precip-dependent Geo-vs-Data panel, listed
## by WHICH rule caught it, so every panel's n is traceable. Also names the
## one overlap between the 172 GRP_ERA_DOWN group and the aridity-only
## invalid-ERA5-input screen that the Phase 3 SESSION_LOG entry left
## unnamed: DE-Zrk is in both (it's one of the 172, and separately its raw
## ERA5 LW_IN_ERA/VPD_ERA also fail the physical-plausibility screen) -- it
## is excluded from aridity's Geo-vs-Data panel either way, but counted only
## once, which is why that panel's "172 GRP_ERA_DOWN" tally among its 777
## valid-input sites is effectively 171 distinct sites, not a double-count.
msg("\n=== Exclusion trace: panels A (Koppen) and C (aridity) ===")

## Panel A's own trace (NOT the generic trace_panel() below): since the
## PI-first revision, excluded_grp_era_down/excluded_p_era_ratio_* on the
## ERA5 climatology no longer determine panel eligibility by themselves --
## a PI-sourced site is never excluded even if its own ERA5 climatology
## would fail one of these rules -- so panel A's trace is read directly off
## panel_a_source_df (built above), not off the precip-rule flags alone.
msg("Panel A (Koppen): ", n_pi, " PI-sourced (never excluded) + ",
    nrow(kg_geo_data_pool) - n_pi, " ERA5 fallback = ", nrow(kg_geo_data_pool), " eligible")
msg("  Among sites without a PI class: ", n_excl_grp, " GRP_ERA_DOWN, ", n_excl_ratio_high,
    " P_ERA_MAX_RATIO, ", n_excl_ratio_low, " P_ERA_MIN_RATIO excluded (",
    n_excl_grp + n_excl_ratio_high + n_excl_ratio_low, " total of ",
    sum(!panel_a_source_df$has_pi), " fallback-eligible sites)")
msg("  n_eligible = ", nrow(kg_geo_data_pool), " (781 total - ", n_excl_grp, " GRP-only - ",
    n_excl_ratio_high, " max-ratio-only - ", n_excl_ratio_low, " min-ratio-only, all three only ",
    "evaluated among the ", sum(!panel_a_source_df$has_pi), " sites without a PI class)")

## Panel C's trace: unaffected by the PI-first revision (aridity has no PI-
## reported analogue), same precip-rule flags as before, now 3-way (GRP vs
## P_ERA_MAX_RATIO vs the new P_ERA_MIN_RATIO) instead of 2-way.
trace_panel <- function(df, panel_label, has_invalid_col) {
  ## NA in excluded_grp_era_down/excluded_p_era_ratio_* means "rule not
  ## evaluated for this site" (aridity: the 4 invalid-input sites never
  ## reached compute_precip_exclusions()) -- treated as FALSE for this
  ## trace, not a third state, so logical indexing below doesn't pick up
  ## spurious NA entries (R's x[NA] inserts an NA element, silently
  ## inflating counts and corrupting the printed site lists).
  grp    <- dplyr::coalesce(df$excluded_grp_era_down, FALSE)
  rat_hi <- dplyr::coalesce(df$excluded_p_era_ratio_high, FALSE)
  rat_lo <- dplyr::coalesce(df$excluded_p_era_ratio_low, FALSE)
  rat    <- rat_hi | rat_lo
  inv    <- if (has_invalid_col) dplyr::coalesce(df$invalid_era5_input, FALSE) else rep(FALSE, nrow(df))
  grp_only     <- df$site_id[grp & !rat]
  ratio_hi_only <- df$site_id[rat_hi & !grp]
  ratio_lo_only <- df$site_id[rat_lo & !grp]
  both         <- df$site_id[grp & rat]
  msg(panel_label, ": GRP_ERA_DOWN only = ", length(grp_only),
      "; P_ERA_MAX_RATIO only = ", length(ratio_hi_only),
      "; P_ERA_MIN_RATIO only = ", length(ratio_lo_only),
      "; both (GRP + a ratio rule) = ", length(both),
      if (has_invalid_col) paste0("; invalid ERA5 input (separate screen) = ", sum(inv)) else "")
  if (length(both) > 0) msg("  Sites caught by BOTH GRP_ERA_DOWN and a ratio rule: ", paste(sort(both), collapse = ", "))
  n_elig <- sum(!grp & !rat & !inv)
  msg("  n_eligible = ", n_elig, " (", nrow(df), " total - ", length(grp_only), " GRP-only - ",
      length(ratio_hi_only), " max-ratio-only - ", length(ratio_lo_only), " min-ratio-only - ",
      length(both), " both",
      if (has_invalid_col) paste0(" - ", sum(inv), " invalid-input") else "", ")")
}
arid_trace <- readr::read_csv(fig4_aridity_path, show_col_types = FALSE)
trace_panel(arid_trace, "Panel C (aridity)", has_invalid_col = TRUE)
msg("DE-Zrk is in BOTH the 172 GRP_ERA_DOWN group AND panel C's invalid-ERA5-input screen -- ",
    "named here per task instruction.")

# ==============================================================================
# PHASE 4: Biomass (panel D), NEE and ET (panels E-F)
# ==============================================================================
## Biomass: unchanged -- same site-level lookup (ESA CCI Biomass v7, 7-bin
## hybrid) used for BOTH Geo vs Geo and Geo vs Data, since biomass has no
## independent "data" observation distinct from the raster-at-tower value
## (same convention the 4 reproduced axes in flux_bin_breaks.R already use).
##
## NEE and ET: the flux_bin_breaks.R scheme and edges, PORTED here (not
## sourced -- sourcing would re-run that diagnostic script's own rendering
## as a side effect) into production: Koppen land mask, bar 1 = model GPP <
## 5 gC/m2/yr (NEE is signed, cannot be cut on its own magnitude), rounded
## sextiles of the 50/50 geo/tower mixture CDF outside bar 1. ET uses the
## dedicated 1991-2020, 17-model ensemble-median raster
## (flux_bin_breaks_et_median_1991_2020.tif), not the older committed
## trendy_et_median.tif (mismatched 1990-2023/16-model window -- see the
## 2026-10-01 draft-Fig-4-audit entry on why that distinction matters).
## "Geo vs Data" = tower-measured annual value (VUT->CUT per-site fallback,
## Step-3 annual method); "Geo vs Geo" = the model's own value at the tower
## cell. GPP/TER are NOT part of this 6-panel figure (flux_bin_breaks.R
## computed them too, for its own broader diagnostic, but panels E-F here
## are NEE and ET only, per this figure's explicit 6-axis list).
msg("\n=== PHASE 4: Biomass (panel D), NEE and ET (panels E-F) ===")

## ---- Panel D: biomass (unchanged, same lookup both versions) -------------
bio7_global <- readr::read_csv(file.path(SNAP_DIR, "biomass_cci_v7_global_distribution.csv"),
                                show_col_types = FALSE) |>
  dplyr::mutate(class = as.character(biomass_bin))
BIOMASS_LAND_TOTAL_KM2 <- sum(bio7_global$global_land_area_km2)
biomass_sites <- readr::read_csv(file.path(SNAP_DIR, "site_biomass_cci_v7.csv"), show_col_types = FALSE)
n_bio_classified <- sum(!is.na(biomass_sites$biomass_bin))
cnt_bio <- biomass_sites |>
  dplyr::filter(!is.na(biomass_bin)) |>
  dplyr::count(biomass_bin, name = "n") |>
  dplyr::rename(class = biomass_bin) |>
  dplyr::mutate(class = as.character(class), network_frac = n / nrow(biomass_sites))
merged_bio <- bio7_global |>
  dplyr::select(class, global_land_fraction) |>
  dplyr::full_join(cnt_bio, by = "class") |>
  dplyr::mutate(n = dplyr::coalesce(n, 0L), network_frac = dplyr::coalesce(network_frac, 0),
                global_land_fraction = dplyr::coalesce(global_land_fraction, 0))
j_bio <- weighted_jaccard(merged_bio$global_land_fraction, merged_bio$network_frac)
msg("Biomass J (same both versions) = ", round(j_bio, 3), "; n=", n_bio_classified, "/", nrow(biomass_sites))
add_metric("D", "biomass", "geo_vs_geo", "Beck 2023 1 km mask (fine, 0.00833 deg)", BIOMASS_LAND_TOTAL_KM2,
           nrow(biomass_sites), n_bio_classified, j_bio)
add_metric("D", "biomass", "geo_vs_data", "Beck 2023 1 km mask (fine, 0.00833 deg)", BIOMASS_LAND_TOTAL_KM2,
           nrow(biomass_sites), n_bio_classified, j_bio)

## ---- Panels E-F: NEE and ET (ported from flux_bin_breaks.R) --------------
DERIVED_DIR <- file.path(EXT, "trendy", "derived")
KG_PATH_05  <- file.path(EXT, "koppen_beck2023", "1991_2020", "koppen_geiger_0p5.tif")
kg_05 <- terra::rast(KG_PATH_05)
cell_areas_05 <- terra::cellSize(kg_05, mask = TRUE, unit = "km")

r_nee <- terra::rast(file.path(DERIVED_DIR, "trendy_nee_fluxbased_median.tif"))
r_gpp <- terra::rast(file.path(DERIVED_DIR, "candidate_gpp_median.tif"))
r_et  <- terra::rast(file.path(DERIVED_DIR, "flux_bin_breaks_et_median_1991_2020.tif"))
GEO_LAND_NEE <- terra::mask(r_nee, kg_05)
GEO_LAND_GPP <- terra::mask(r_gpp, kg_05)
GEO_LAND_ET  <- terra::mask(r_et, kg_05)
FLUX_LAND_TOTAL_KM2 <- sum(terra::values(cell_areas_05)[!is.na(terra::values(GEO_LAND_GPP))], na.rm = TRUE)
msg("Flux (NEE/ET) land total (TRENDY ensemble footprint under Koppen mask): ",
    format(round(FLUX_LAND_TOTAL_KM2), big.mark = ","), " km2")

NEE_BAR1_GPP_CUT <- 5   # gC m-2 yr-1 -- same named constant as flux_bin_breaks.R
ET_LOW_CUT       <- 5   # mm yr-1
GEO_MIXTURE_WEIGHT <- 0.5
FLUX_ROUND <- c(NEE = 25, ET = 50)

## Tower annual values (revised 2026-10-02): the paper's actual QC gate
## (QC_THRESHOLD_YY, R/pipeline_config.R -- a config constant, never a
## literal; the 0.80 this block used until now was copied from
## assess_flux_data_by_igbp_shuttle.R and was wrong for this paper), via the
## shared compute_site_annual_fluxes() (R/site_annual_fluxes.R). That
## function reads the pre-QC `annual` table directly (not annual_qc/
## annual_converted, which drop a whole row on NEE QC and would wrongly
## discard ET years with good LE_F_MDS_QC) and gates each variable on its own
## QC column: NEE on the per-site VUT/CUT-chosen NEE QC column (same rule as
## scripts/04_qc.R); ET on LE_F_MDS_QC, independent of the NEE gate. A tower's
## value is the median of its QC-qualifying annual values (at least one
## year) -- the mean-monthly-cycle method this block used until now is
## retired. Figures 2/3 will take their own tower NEE/GPP/RECO/ET/H from this
## same function in a separate revision.
con <- dbConnect(duckdb(), duckdb_path, read_only = TRUE)
site_fluxes <- compute_site_annual_fluxes(con, site_ids = current_sites$site_id)
dbDisconnect(con, shutdown = TRUE)

site_src <- site_fluxes$site_summary
msg("Per-site VUT/CUT choice (NEE/GPP/RECO): VUT=", sum(site_src$nee_source == "VUT", na.rm = TRUE),
    "  CUT (fallback)=", sum(site_src$nee_source == "CUT", na.rm = TRUE),
    "  neither=", sum(is.na(site_src$nee_source)))

tower_nee <- site_src |>
  dplyr::filter(!is.na(nee_median)) |>
  dplyr::transmute(site_id, tower_value = nee_median) |>
  dplyr::left_join(current_sites, by = "site_id")
tower_et <- site_src |>
  dplyr::filter(!is.na(et_median)) |>
  dplyr::transmute(site_id, tower_value = et_median) |>
  dplyr::left_join(current_sites, by = "site_id")
msg("Tower annual values (QC_THRESHOLD_YY=", QC_THRESHOLD_YY, ", median of qualifying years) -- NEE: ",
    nrow(tower_nee), "  ET: ", nrow(tower_et))

geo_coords <- as.matrix(current_sites[, c("location_long", "location_lat")])
model_nee_at_site <- terra::extract(r_nee, geo_coords, method = "bilinear")[, 1]
model_gpp_at_site <- terra::extract(r_gpp, geo_coords, method = "bilinear")[, 1]
model_et_at_site  <- terra::extract(r_et,  geo_coords, method = "bilinear")[, 1]

## require_own = TRUE (Geo vs Data only): a site only gets a bin -- including
## bar 1 -- when it has its own (tower) value for this flux. Without this, a
## site whose MASK value (e.g. model GPP, for NEE) falls below the bar-1 cut
## was counted in bar 1 regardless of whether the site has any tower value at
## all for the flux being binned -- four towers with no qualifying NEE
## (CA-Mtk, GL-ZaH, GL-ZaF, SJ-Adv) were being counted as "unvegetated" in
## panel E's Geo vs Data side this way. Geo vs Geo (require_own = FALSE,
## default) is unaffected: its own_value is the model's own value at the same
## tower coordinate as mask_value, so it is never selectively missing.
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

run_flux_panel <- function(panel_letter, flux_name, own_land_r, mask_land_r, mask_value_at_site,
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
  add_metric(panel_letter, tolower(flux_name), "geo_vs_data",
             "TRENDY v14 ensemble-median, 0.5 deg, Koppen land mask", FLUX_LAND_TOTAL_KM2, n_data, n_data, j_data)
  add_metric(panel_letter, tolower(flux_name), "geo_vs_geo",
             "TRENDY v14 ensemble-median, 0.5 deg, Koppen land mask", FLUX_LAND_TOTAL_KM2, n_geo, n_geo, j_geo)

  list(edges = edges, land_vec = land_vec, data_bin = data_bin, geo_bin = geo_bin,
       data_df = data_df, geo_df = geo_df, mask_value = site_mask_value)
}

nee_result <- run_flux_panel("E", "NEE", GEO_LAND_NEE, GEO_LAND_GPP, model_gpp_at_site, model_nee_at_site,
                              tower_nee, NEE_BAR1_GPP_CUT,
                              list(step = 1, lo = -500, hi = 500), FLUX_ROUND[["NEE"]])
et_result  <- run_flux_panel("F", "ET", GEO_LAND_ET, GEO_LAND_ET, model_et_at_site, model_et_at_site,
                              tower_et, ET_LOW_CUT,
                              list(step = 2, lo = 0, hi = 2000), FLUX_ROUND[["ET"]])

## ---- Save panel E-F outputs -------------------------------------------------
save_flux_site_csv <- function(result, path, flux_name) {
  current_sites |>
    dplyr::mutate(
      mask_value = result$mask_value,
      tower_value = result$data_df$own_value,
      model_value_at_tower = result$geo_df$own_value,
      bin_data = result$data_bin,
      bin_geo = result$geo_bin
    ) |>
    readr::write_csv(path)
}
fig4_nee_path <- file.path(SNAP_DIR, "site_nee_fig4.csv")
save_flux_site_csv(nee_result, fig4_nee_path, "NEE")
write_output_metadata(
  fig4_nee_path,
  input_sources = c(duckdb_path, CURRENT_SNAPSHOT,
                     "data/external/trendy/derived/trendy_nee_fluxbased_median.tif",
                     "data/external/trendy/derived/candidate_gpp_median.tif"),
  notes = paste0(
    "Panel E (NEE) site-level data for figure4_representativeness.R, binning scheme ported from scripts/",
    "diagnostics/flux_bin_breaks.R (not sourced). mask_value = model GPP at tower (bar-1 vegetation mask, ",
    "cut=", NEE_BAR1_GPP_CUT, " gC/m2/yr). tower_value = the median of the site's QC_THRESHOLD_YY=",
    QC_THRESHOLD_YY, "-qualifying annual NEE values (R/site_annual_fluxes.R::compute_site_annual_fluxes(); ",
    "NEE QC column chosen by the per-site VUT/CUT rule in scripts/04_qc.R; at least one qualifying year ",
    "required) -- revised 2026-10-02 from a QC>=0.80, mean-monthly-cycle method. In this (Geo vs Data) ",
    "version, bin_data is NA for a site with no qualifying tower value, even if its mask_value alone would ",
    "put it in bar 1 (classify_flux_sites(require_own=TRUE); fixes four such towers, CA-Mtk/GL-ZaH/GL-ZaF/",
    "SJ-Adv, previously counted as unvegetated with no actual NEE value). model_value_at_tower = model NEE ",
    "at tower (bilinear). bin_data/bin_geo = 1-7 classification (1 = bar-1 mask; 2-7 = rounded sextile edges ",
    "of the 50/50 geo/tower mixture CDF, edges: ", paste(nee_result$edges, collapse = ", "), " gC/m2/yr)."
  )
)
msg("Saved: ", fig4_nee_path)

fig4_et_path <- file.path(SNAP_DIR, "site_et_fig4.csv")
save_flux_site_csv(et_result, fig4_et_path, "ET")
write_output_metadata(
  fig4_et_path,
  input_sources = c(duckdb_path, CURRENT_SNAPSHOT,
                     "data/external/trendy/derived/flux_bin_breaks_et_median_1991_2020.tif"),
  notes = paste0(
    "Panel F (ET) site-level data for figure4_representativeness.R, binning scheme ported from scripts/",
    "diagnostics/flux_bin_breaks.R (not sourced). Uses the dedicated 1991-2020, 17-model TRENDY ensemble-",
    "median raster (flux_bin_breaks_et_median_1991_2020.tif), not the older committed trendy_et_median.tif ",
    "(1990-2023, 16-model -- see the 2026-10-01 draft-Fig-4-audit entry). tower_value = the median of the ",
    "site's QC_THRESHOLD_YY=", QC_THRESHOLD_YY, "-qualifying annual ET values (R/site_annual_fluxes.R::",
    "compute_site_annual_fluxes(); gated on LE_F_MDS_QC, independent of the NEE QC gate; LE_F_MDS converted ",
    "to mm H2O yr-1 via fluxnet_convert_units(); at least one qualifying year required) -- revised 2026-10-02 ",
    "from a QC>=0.80, mean-monthly-cycle method. Geo vs Data's bin_data requires a qualifying tower value, ",
    "same as panel E (classify_flux_sites(require_own=TRUE)). mask_value/model_value_at_tower/bin_data/",
    "bin_geo otherwise as for panel E, but ET's own value is both the mask and the own-value (bar-1 cut=",
    ET_LOW_CUT, " mm/yr). Edges (mm/yr): ", paste(et_result$edges, collapse = ", "), "."
  )
)
msg("Saved: ", fig4_et_path)

flux_global_dist <- function(result, flux_label) {
  data.frame(flux = flux_label, bin = 1:7, land_fraction = result$land_vec)
}
flux_global_path <- file.path(SNAP_DIR, "nee_et_fig4_global_distribution.csv")
dplyr::bind_rows(flux_global_dist(nee_result, "NEE"), flux_global_dist(et_result, "ET")) |>
  readr::write_csv(flux_global_path)
write_output_metadata(
  flux_global_path,
  input_sources = c("data/external/trendy/derived/trendy_nee_fluxbased_median.tif",
                     "data/external/trendy/derived/flux_bin_breaks_et_median_1991_2020.tif"),
  notes = paste0(
    "Global land-fraction distribution (bins 1-7, TRENDY ensemble footprint under the Koppen 0.5 deg ",
    "land mask, ", format(round(FLUX_LAND_TOTAL_KM2), big.mark = ","), " km2 total) for Fig 4 panels E ",
    "(NEE) and F (ET). Bin edges recorded in site_nee_fig4.csv/site_et_fig4.csv's own .meta.json."
  )
)
msg("Saved: ", flux_global_path)

# ==============================================================================
# Save accumulated metrics (all phases implemented so far)
# ==============================================================================
metrics_fig4_path <- file.path(SNAP_DIR, "representativeness_metrics_fig4.csv")
metrics_df <- dplyr::bind_rows(metrics_rows)
readr::write_csv(metrics_df, metrics_fig4_path)
write_output_metadata(
  metrics_fig4_path,
  input_sources = c("site_koppen_beck2023.csv", "site_koppen_era5_fig4.csv", "site_igbp_fig4.csv",
                     "site_aridity.csv", "site_aridity_era5_fig4.csv", "site_biomass_cci_v7.csv",
                     "site_nee_fig4.csv", "site_et_fig4.csv",
                     "koppen_beck2023_global_distribution.csv", "igbp_mcd12c1_global_distribution.csv",
                     "aridity_unep7_global_distribution.csv", "biomass_cci_v7_global_distribution.csv",
                     "nee_et_fig4_global_distribution.csv"),
  notes = paste0(
    "Weighted-Jaccard metrics for the new (2026-10) Figure 4, built incrementally across phases -- ",
    "see SESSION_LOG.md for each phase. Separate from the shared data/snapshots/representativeness_metrics.csv ",
    "(which backs Figs 001-008 and is not touched by this script). One row per panel x comparison. ",
    "This run includes all 6 panels: A Koppen, B IGBP land cover, C aridity, D biomass, E NEE, F ET."
  )
)
msg("Saved: ", metrics_fig4_path)
print(as.data.frame(metrics_df))


# ==============================================================================
# PHASE 5 (re-rendered 2026-10-02 for print): Nature final-artwork specs
# ==============================================================================
## Changes rendering ONLY -- no data/bins/exclusions computed above this point
## are touched. n and J are read from metrics_df (already written to
## representativeness_metrics_fig4.csv earlier in this run) and compared
## against that file after saving, to confirm nothing moved.
msg("\n=== PHASE 5 (print re-render): Nature final-artwork specs ===")

LOG2_MAX    <- log2(5)
LOG2_BREAKS <- c(-LOG2_MAX, -1, 0, 1, LOG2_MAX)
LOG2_LABELS <- c("1/5×", "1/2×", "1×", "2×", "5×")
LOG2_XLIM   <- c(-LOG2_MAX - 0.25, LOG2_MAX + 0.25)

## Font: task allows "Helvetica or Arial". Two different, independently-
## confirmed problems ruled out using ONE name for everything:
## - RENDERING (actual glyphs drawn, both ragg/PNG and base pdf()/PDF):
##   Helvetica resolves to /System/Library/Fonts/Helvetica.ttc and renders
##   correctly in both devices (confirmed directly); "Arial" is not a valid
##   `family` for base grDevices::pdf() (only the 14 PostScript standard
##   names are), so the PDF export needs "Helvetica".
## - MEASUREMENT (systemfonts::string_width(), used for the "measure the
##   rendered text width, don't estimate" placement rule): Helvetica.ttc is
##   a TrueType COLLECTION file that systemfonts::string_width() cannot read
##   (freetype error 133, confirmed directly); Arial resolves to a plain
##   .ttf and measures correctly.
## Resolution: FIG_FONT ("Helvetica") is used for all actual text rendering
## (PNG via ragg, PDF via base pdf(), and ggplot/grid text elements);
## MEASURE_FONT ("Arial") is used ONLY inside text_width_mm(), as the
## metrics source for the placement decisions. Helvetica and Arial are
## metrically near-identical by design (Arial was drawn to be substitutable
## for Helvetica), so this is not a meaningful approximation in practice --
## but it is one, and is flagged as such rather than assumed exact.
FIG_FONT       <- "Helvetica"
MEASURE_FONT   <- "Arial"
BASE_PT        <- 7
LETTER_PT      <- 8
FIG_WIDTH_MM   <- 183
NCOL_FIG       <- 2
COL_WIDTH_MM   <- FIG_WIDTH_MM / NCOL_FIG
ROW_PITCH_MM   <- 4.2
MM_PER_PT      <- 25.4 / 72     # systemfonts::string_width() uses 72 pt/inch (res=72 default)
PANEL_MARGIN_MM <- 2            # each panel's own plot.margin, all sides

if (!requireNamespace("systemfonts", quietly = TRUE)) {
  stop("systemfonts is required for measured text widths (already an installed dependency of the ",
       "ggplot2/ragg stack on this machine -- not a new dependency added for this task).")
}
font_check <- systemfonts::match_fonts(FIG_FONT)
msg("Render font resolved: ", FIG_FONT, " -> ", font_check$path[1])
measure_font_check <- systemfonts::match_fonts(MEASURE_FONT)
msg("Measurement font resolved: ", MEASURE_FONT, " -> ", measure_font_check$path[1])

## ---- Measured text width (mm) at a given point size/weight, NOT a per-
## character constant -- systemfonts::string_width() queries the actual font
## file's glyph metrics directly (no open device required). Uses MEASURE_FONT
## (Arial), not FIG_FONT -- see the header comment above.
text_width_mm <- function(label, size_pt, weight = "normal") {
  w_pt <- systemfonts::string_width(label, family = MEASURE_FONT, size = size_pt, weight = weight)
  w_pt * MM_PER_PT
}

## ---- Per-panel data-width calibration: how many mm correspond to 1 log2-
## sampling-ratio unit, for THIS panel's own y-axis label gutter (which
## varies by panel -- "unvegetated" vs "Af" vs "Humid (moderate)"). Found by
## building the panel's axis/theme (no bars) as a gtable, summing every
## column EXCEPT the "panel" (data) column (all fixed/text-metric widths,
## resolvable without a live interactive device -- a throwaway pdf() device
## is opened only so grid can resolve font metrics), and subtracting that
## from the panel's known total outer width (COL_WIDTH_MM, 1 of 2 equal
## figure columns). This replaces guessing a fixed label-gutter width.
##
## Revised 2026-10-02 (task item 4, column alignment): returns the LEFT
## (everything left of "panel", i.e. the y-axis label gutter) and RIGHT
## (plot margin only -- no content there) overhead separately, in mm,
## instead of a single combined "other_mm". The two are needed separately
## because column alignment (below) must equalise only the LEFT gutter
## across a/c/e and across b/d/f -- the right-hand overhead is already
## identical (a fixed plot margin) and must stay untouched.
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
  grDevices::pdf(tmp_pdf, width = 10, height = 10, family = "Helvetica")  # base14 name just to open a working device; grobs still request FIG_FONT via gpar
  on.exit({ grDevices::dev.off(); unlink(tmp_pdf) }, add = TRUE)
  g <- ggplot2::ggplotGrob(p)
  panel_col <- g$layout$l[g$layout$name == "panel"][1]
  left_cols  <- which(seq_along(g$widths) < panel_col)
  right_cols <- which(seq_along(g$widths) > panel_col)
  left_mm  <- sum(vapply(left_cols,  function(i) grid::convertWidth(g$widths[i], "mm", valueOnly = TRUE), numeric(1)))
  right_mm <- sum(vapply(right_cols, function(i) grid::convertWidth(g$widths[i], "mm", valueOnly = TRUE), numeric(1)))
  list(left_mm = left_mm, right_mm = right_mm)
}

## panel_mm_per_unit for a panel rendered against a (possibly shared/
## column-aligned) LEFT gutter width target_left_mm, rather than its own
## natural gutter -- see column-alignment block below, where target_left_mm
## is the max natural gutter among the panel's column-mates (a/c/e or
## b/d/f), so panels with shorter labels give up bar-area width to match.
panel_mm_per_unit_for <- function(class_labels, show_xlab, target_left_mm) {
  layout_mm <- measure_panel_layout_mm(class_labels, show_xlab)
  panel_mm <- COL_WIDTH_MM - target_left_mm - layout_mm$right_mm
  if (panel_mm <= 10) {
    warning("Panel data-area width came out implausibly small (", round(panel_mm, 1),
            " mm) for labels: ", paste(utils::head(class_labels, 3), collapse = ", "), "...")
  }
  panel_mm / (LOG2_XLIM[2] - LOG2_XLIM[1])
}

## ---- Clip-label formatting: no decimals at ratio>=10, cap display at
## ">1000x". Vectorized (called with whole dplyr columns, not scalars).
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
format_clip_annot <- function(sampling_ratio, log2_sr) {
  mapply(format_clip_annot_one, sampling_ratio, log2_sr)
}

## ---- Land-share value formatting: 1 decimal, "<0.1" below that -----------
format_land_pct <- function(frac) {
  pct <- frac * 100
  dplyr::if_else(pct < 0.1 & pct > 0, "<0.1", sprintf("%.1f", pct))
}

## ---- Contrast colour for text drawn on a bar fill (white on dark, near-
## black on light; relative-luminance threshold 0.5) -- same rule used in
## flux_bin_breaks.R and the first (non-print) render of this figure.
contrast_text_color <- function(hex) {
  rgb_mat <- grDevices::col2rgb(hex) / 255
  lum <- 0.2126 * rgb_mat["red", ] + 0.7152 * rgb_mat["green", ] + 0.0722 * rgb_mat["blue", ]
  ifelse(lum < 0.5, "white", "grey10")
}

prep_ordered <- function(df) {
  df |> dplyr::arrange(class_order) |>
    dplyr::mutate(class_label = factor(class_label, levels = unique(class_label)))
}

## ---- prep_clip2(): like prep_clip() but with the new clip-label format and
## explicit handling for "none" bins (land present, zero towers) and
## "neither" bins (no land, no towers -- no bar at all).
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

## ---- Header-row replacement helper: bold lowercase letter + 7pt title (left)
## + J (7pt, right), all one line, via grid grob replacement -- ggplot2 title/
## subtitle elements can't mix font sizes/weights within one string, so the
## "title" gtable cell's content is swapped out after ggplotGrob(). Reused
## for the bottom-row smaller-proportion/greater-proportion caption via the "xlab-b"
## cell (same technique, different cell).
replace_gtable_cell <- function(g, cell_name, new_grob) {
  idx <- which(g$layout$name == cell_name)
  if (length(idx) == 0) {
    warning("gtable cell '", cell_name, "' not found -- ggplot2 internals may have changed; ",
            "header/caption not drawn for this panel.")
    return(g)
  }
  g$grobs[[idx[1]]] <- new_grob
  g
}
## `title_text` may be a plain character string (panels a/b/c) OR a plotmath
## expression (`as.expression(bquote(...))`, panels d/e/f) -- see
## PANEL_SPECS' `title_expr` field. Confirmed by direct PDF inspection
## (task item 4, superscript legibility): the base grDevices::pdf()
## PostScript "Helvetica" font has no usable glyph for the Unicode
## superscript-minus character (U+207B) used in d/e/f's unit titles (e.g.
## "Mg ha⁻¹") -- it rendered as a barely-visible baseline dot, not
## a minus sign, even though the same string renders correctly in the PNG
## (ragg/Helvetica.ttc has the glyph). A plotmath expression sidesteps this
## entirely: grid typesets the superscript by scaling/raising an ordinary
## ASCII hyphen and digit via its own font metrics, which both devices
## handle correctly, rather than depending on one special Unicode glyph
## being present in the active font. A fixed `TITLE_GAP_MM` (not a literal
## "  " prefix baked into the label, as the previous character-only version
## used) provides the letter-to-title gap so this works identically for
## both label types.
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
## Wording revised 2026-10-02 (task item 5): "under-sampled"/"over-sampled"
## implied a value judgement about sampling adequacy this figure doesn't
## make -- it only reports the ratio of each class's tower share to its
## land share. Replaced with the neutral "smaller proportion"/"greater
## proportion"; the legend (write_fig4_legend()) states once, in full, what
## that means (smaller/greater proportion of towers than of land).
caption_grob <- function() {
  left  <- grid::textGrob("smaller proportion", x = 0.25, hjust = 0.5, vjust = 1,
                           gp = grid::gpar(fontsize = BASE_PT, fontfamily = FIG_FONT, col = "grey30"))
  right <- grid::textGrob("greater proportion", x = 0.75, hjust = 0.5, vjust = 1,
                           gp = grid::gpar(fontsize = BASE_PT, fontfamily = FIG_FONT, col = "grey30"))
  grid::gTree(children = grid::gList(left, right))
}

## ---- draw_panel2(): the Nature-print panel renderer -----------------------
## `show_header` adds a blank pseudo-row at the top of the DATA (a real row,
## included in the row-pitch height math, not an overflow annotation) and
## overlays "% land"/"towers" column headers on it. `show_xlab` replaces the
## x-axis title row with the smaller-proportion/greater-proportion caption.
LABEL_OFFSET <- 0.15

## ---- Column alignment (task item 4): force this panel's LEFT (y-axis
## label) gutter to a shared target width across its output column (a/c/e
## or b/d/f), so the "panel" (bar/data) column -- a flexible "null" gtable
## unit that otherwise absorbs whatever's left after the fixed-width
## columns around it -- starts at the SAME horizontal offset for every
## panel in that column, and the 1x gridline lines up. Needed because
## ggplot auto-sizes the axis-l column to each panel's OWN y-axis label
## text at render time, independent of panel_mm_per_unit_for()'s upstream
## mm calibration (which only affects in-bar label placement, not the
## rendered gtable itself) -- confirmed empirically: without this, panel a
## (short 2-letter Koppen labels) and panel c (long labels like "Humid
## (moderate)") had their shared 1x line about 7.7mm apart in the first
## print render. Only the axis-l column is touched (set to an explicit mm
## width); the panel column is left as "null" and absorbs the
## corresponding change automatically at final (patchwork) layout time --
## exactly the task's "take it from the bar area" instruction, not a font
## change.
align_panel_left_mm <- function(g, target_left_mm) {
  panel_col <- g$layout$l[g$layout$name == "panel"][1]
  axis_l_rows <- which(g$layout$name == "axis-l")
  if (length(axis_l_rows) == 0) {
    warning("gtable has no 'axis-l' cell -- column alignment skipped for this panel.")
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
  ## ---- Row order (task item 4): explicit `limits` on the y scale, NOT
  ## relied-on factor level order. Confirmed by direct reproduction that
  ## without this, ggplot2's discrete-scale training over TWO geom_col
  ## layers with different `data=` subsets (bar_df vs none_df, used below
  ## for the dashed "none" bars) can silently reorder the row whose only
  ## occurrence is in the second (none_df) layer to the END of the trained
  ## range instead of its correct factor position -- e.g. Koppen's EF (a
  ## permanent "none" class, no PI/ERA5 class ever assigns a tower to pure
  ## ice) rendered ABOVE ET in the first print render, and ET panel F's
  ## bar-1 "0-5" class (always "none", since no vegetated tower can fall
  ## below the GPP-based bar-1 cut) rendered at the very top instead of the
  ## bottom. `limits = levels(df$class_label)` forces the correct, already-
  ## computed ascending class_order sequence regardless of this layer-
  ## training quirk, and is identical between the Geo vs Geo/Geo vs Data
  ## comparisons for a given panel (same global class universe either way),
  ## satisfying "identical in both figures". `breaks` drops the header
  ## pseudo-row's blank class from the set of positions that get a tick
  ## mark (task item 4, "remove the unlabelled tick on the header row").
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

  ## ---- Bar numbers: measured-width-based placement, incl. header row -----
  ## Three-way placement (task spec, revised from the first render's two-way
  ## rule): (1) bar fully contains the label -> fixed at LABEL_OFFSET next
  ## to the 1x line, contrast colour; (2) a bar exists on that side but is
  ## too short to contain the label -> position MOVES to just beyond that
  ## bar's own outer end (not the fixed offset), near-black; (3) no bar at
  ## all on that side -> fixed at LABEL_OFFSET, near-black. Case (2) is new:
  ## the first render always used the fixed offset for both (2) and (3),
  ## which let a near-black number straddle a short bar's edge (e.g. the
  ## Köppen "BS" row) since the fixed anchor sat ON the bar's edge instead
  ## of beside it -- caught by checking exactly this row.
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

  ## Clip-annotation text (e.g. "5.4x" beside a truncated bar): previously
  ## `label_pt - 1`, i.e. 6pt at the figure's 7pt base size -- below the
  ## task's "no text smaller than 7pt" floor (task item 4). Fixed at
  ## `label_pt` like every other on-panel label; never shrunk to fit.
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

## ---- Overhead measurement: every gtable row EXCEPT "panel", in mm, for a
## given panel's own configuration (show_xlab/show_header change which rows
## exist). Combined with ROW_PITCH_MM * effective_n_rows (effective_n_rows
## includes the +1 header pseudo-row where applicable), gives the exact
## total panel height -- same technique as the width calibration above.
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
  grDevices::pdf(tmp_pdf, width = 10, height = 10, family = "Helvetica")  # base14 name just to open a working device; grobs still request FIG_FONT via gpar
  on.exit({ grDevices::dev.off(); unlink(tmp_pdf) }, add = TRUE)
  panel_row <- g$layout$t[g$layout$name == "panel"][1]
  other_rows <- setdiff(seq_along(g$heights), panel_row)
  sum(vapply(other_rows, function(i) grid::convertHeight(g$heights[i], "mm", valueOnly = TRUE), numeric(1)))
}

## ---- Shortened class labels (print re-render) -----------------------------
## NEE: drop the "(GPP < 5)" qualifier from bar 1, true Unicode minus signs
## (U+2212) in place of ASCII hyphens throughout.
flux_bin_labels_print <- function(flux_name, cut, edges) {
  if (flux_name == "NEE") {
    labs <- c("unvegetated",
              sprintf("< %s", edges[1]),
              sprintf("%s to %s", edges[1], edges[2]), sprintf("%s to %s", edges[2], edges[3]),
              sprintf("%s to %s", edges[3], edges[4]), sprintf("%s to %s", edges[4], edges[5]),
              sprintf("> %s", edges[5]))
    labs <- gsub("-", "\u2212", labs, fixed = TRUE)
  } else {
    labs <- c(sprintf("0\u2013%s", cut), sprintf("%s\u2013%s", cut, edges[1]),
              sprintf("%s\u2013%s", edges[1], edges[2]), sprintf("%s\u2013%s", edges[2], edges[3]),
              sprintf("%s\u2013%s", edges[3], edges[4]), sprintf("%s\u2013%s", edges[4], edges[5]),
              sprintf("> %s", edges[5]))
  }
  labs
}
NEE_BIN_LABELS <- flux_bin_labels_print("NEE", NEE_BAR1_GPP_CUT, nee_result$edges)
ET_BIN_LABELS  <- flux_bin_labels_print("ET", ET_LOW_CUT, et_result$edges)
NEE_LABEL_MAP <- setNames(NEE_BIN_LABELS, as.character(1:7))
ET_LABEL_MAP  <- setNames(ET_BIN_LABELS, as.character(1:7))
FLUX_ORDER_MAP <- setNames(1:7, as.character(1:7))

## Biomass: strip " Mg/ha" (unit now in the panel title) and trailing ".0".
clean_biomass_label <- function(x) {
  x <- sub(" Mg/ha$", "", x)
  gsub("\\.0(?=[\u20130-9>]|$)", "", x, perl = TRUE)
}
BIOMASS_LABEL_MAP <- setNames(clean_biomass_label(bio7_global$biomass_bin_label), bio7_global$class)
BIOMASS_ORDER_MAP <- setNames(as.integer(bio7_global$class), bio7_global$class)
BIO7_COLORS <- c("1" = "#f7f4f9", "2" = "#f0e1c4", "3" = "#d4d491",
                  "4" = "#a3c585", "5" = "#6cb375", "6" = "#2e8b57", "7" = "#14532d")

KG_LABEL_MAP <- setNames(TL_ORDER, TL_ORDER)
KG_ORDER_MAP <- setNames(seq_along(TL_ORDER), TL_ORDER)
IGBP_LABEL_MAP <- setNames(IGBP_ORDER, IGBP_ORDER)
IGBP_ORDER_MAP <- setNames(seq_along(IGBP_ORDER), IGBP_ORDER)
ARIDITY_LABEL_MAP <- setNames(ARIDITY_ORDER, ARIDITY_ORDER)
ARIDITY_ORDER_MAP <- setNames(seq_along(ARIDITY_ORDER), ARIDITY_ORDER)

NEE_SINK_RAMP <- grDevices::colorRampPalette(c("#0b3e09", "#eaf5e4"))(5)
NEE7_COLORS <- c("1" = unname(BIO7_COLORS[["1"]]),
                  "2" = NEE_SINK_RAMP[1], "3" = NEE_SINK_RAMP[2], "4" = NEE_SINK_RAMP[3],
                  "5" = NEE_SINK_RAMP[4], "6" = NEE_SINK_RAMP[5], "7" = "#c2703a")
ET7_COLORS  <- c("1" = unname(BIO7_COLORS[["1"]]), "2" = "#bcd8f4", "3" = "#82bce8",
                  "4" = "#4498d5", "5" = "#1d74b3", "6" = "#0c4f84", "7" = "#06305a")

## Panel specs: letter + title kept separate (unit suffix on d/e/f titles, no
## dash -- the dash was absorbed into the two-grob header mechanism).
PANEL_SPECS <- list(
  A = list(letter = "a", title = "K\u00f6ppen-Geiger", axis = "koppen",
           order_map = KG_ORDER_MAP, label_map = KG_LABEL_MAP, color_map = KG13_COLORS,
           total_km2 = KG_LAND_TOTAL_KM2, land_grid = "Beck 2023 1 km mask"),
  B = list(letter = "b", title = "Land cover (IGBP)", axis = "igbp",
           order_map = IGBP_ORDER_MAP, label_map = IGBP_LABEL_MAP, color_map = IGBP_COLORS,
           total_km2 = IGBP_LAND_TOTAL_KM2, land_grid = "MODIS MCD12C1 on Beck 2023 1 km mask"),
  C = list(letter = "c", title = "Aridity", axis = "aridity",
           order_map = ARIDITY_ORDER_MAP, label_map = ARIDITY_LABEL_MAP, color_map = ARIDITY_COLORS,
           total_km2 = ARIDITY_LAND_TOTAL_KM2, land_grid = "CGIAR Aridity Index v3.1"),
  D = list(letter = "d", title = "Biomass (Mg ha\u207b\u00b9)", axis = "biomass",
           title_expr = as.expression(bquote(Biomass ~ (Mg ~ ha^{-1}))),
           order_map = BIOMASS_ORDER_MAP, label_map = BIOMASS_LABEL_MAP, color_map = BIO7_COLORS,
           total_km2 = BIOMASS_LAND_TOTAL_KM2, land_grid = "Beck 2023 1 km mask (fine)"),
  E = list(letter = "e", title = "NEE (g C m\u207b\u00b2 yr\u207b\u00b9)", axis = "nee",
           title_expr = as.expression(bquote(NEE ~ ("g C" ~ m^{-2} ~ yr^{-1}))),
           order_map = FLUX_ORDER_MAP, label_map = NEE_LABEL_MAP, color_map = NEE7_COLORS,
           total_km2 = FLUX_LAND_TOTAL_KM2, land_grid = "TRENDY v14 ensemble-median, 0.5 deg"),
  F = list(letter = "f", title = "ET (mm yr\u207b\u00b9)", axis = "et",
           title_expr = as.expression(bquote(ET ~ (mm ~ yr^{-1}))),
           order_map = FLUX_ORDER_MAP, label_map = ET_LABEL_MAP, color_map = ET7_COLORS,
           total_km2 = FLUX_LAND_TOTAL_KM2, land_grid = "TRENDY v14 ensemble-median, 0.5 deg")
)
## `title_expr` is used for the RENDERED panel title (header_grob(), both
## PNG and PDF) wherever present; `title` (plain string) is still used for
## all plain-text output (legend DESCRIPTION list, panel_n_line()) -- kept
## as two separate fields rather than one, since sprintf("%s", <expression>)
## does not produce the intended text.
panel_title_for_render <- function(letter) {
  spec <- PANEL_SPECS[[letter]]
  if (!is.null(spec$title_expr)) spec$title_expr else spec$title
}

get_j_fig4 <- function(panel, cmp) {
  v <- metrics_df$weighted_jaccard[metrics_df$panel == panel & metrics_df$comparison == cmp]
  if (length(v) == 0) NA_real_ else v[[1]]
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
flux_merged_df <- function(result, comparison) {
  bin_vec <- if (comparison == "geo_vs_data") result$data_bin else result$geo_bin
  n_classified <- sum(!is.na(bin_vec))
  cnt <- as.numeric(table(factor(bin_vec, levels = 1:7)))
  data.frame(class = as.character(1:7), global_land_fraction = result$land_vec,
             n = cnt, network_frac = cnt / n_classified, stringsAsFactors = FALSE)
}
build_merged_for_panel <- function(panel_letter, comparison) {
  switch(panel_letter,
    A = if (comparison == "geo_vs_geo") merged_geo_geo else merged_geo_data,
    B = if (comparison == "geo_vs_geo") merged_igbp_geo_geo else merged_igbp_geo_data,
    C = if (comparison == "geo_vs_geo") merged_arid_geo_geo else merged_arid_geo_data,
    D = merged_bio,
    E = flux_merged_df(nee_result, comparison),
    F = flux_merged_df(et_result, comparison)
  )
}

## ---- Per-panel font-size override (7pt default; filled in below only if a
## panel is found, after visual inspection, to still crowd at 7pt -- see
## SESSION_LOG.md for the outcome of that check).
PANEL_LABEL_PT <- setNames(rep(BASE_PT, 6), c("A", "B", "C", "D", "E", "F"))

PANEL_LAYOUT <- list(
  row1 = c("A", "B"), row2 = c("C", "D"), row3 = c("E", "F")
)
COLUMN_LAYOUT <- list(col1 = c("A", "C", "E"), col2 = c("B", "D", "F"))
show_header_for <- function(letter) letter %in% PANEL_LAYOUT$row1
show_xlab_for   <- function(letter) letter %in% PANEL_LAYOUT$row3

## ---- Fixed per-axis row order (task item 4): the full, ascending (bottom-
## to-top) class_label sequence for a panel letter, independent of
## comparison (the global class universe -- and hence this sequence -- is
## the same for Geo vs Geo and Geo vs Data; build_merged_for_panel() always
## full_joins every possible class, never a comparison-specific subset).
## Includes the blank header pseudo-row at the end (top) when that panel is
## in row1, so this is also the exact `y_limits` draw_panel2() will use.
panel_y_limits <- function(panel_letter) {
  spec <- PANEL_SPECS[[panel_letter]]
  labs <- unname(spec$label_map[order(spec$order_map)])
  if (show_header_for(panel_letter)) labs <- c(labs, "")
  labs
}

ALL_LETTERS <- c("A", "B", "C", "D", "E", "F")

build_fig4_panel2 <- function(panel_letter, comparison) {
  spec <- PANEL_SPECS[[panel_letter]]
  merged <- build_merged_for_panel(panel_letter, comparison)
  df <- build_panel_df(merged, spec$order_map, spec$label_map, spec$color_map, spec$total_km2)
  show_xlab   <- show_xlab_for(panel_letter)
  show_header <- show_header_for(panel_letter)
  j_val <- get_j_fig4(panel_letter, comparison)
  g <- draw_panel2(df, PANEL_MM_PER_UNIT[[panel_letter]], spec$letter,
                    panel_title_for_render(panel_letter), j_val,
                    show_xlab = show_xlab, show_header = show_header,
                    label_pt = PANEL_LABEL_PT[[panel_letter]],
                    target_left_mm = TARGET_GUTTER_MM[[panel_letter]])
  list(df = df, grob = g, n_rows = nlevels(df$class_label) + if (show_header) 1L else 0L,
       show_xlab = show_xlab, show_header = show_header)
}

## ---- Row heights: overhead (everything but the "panel" row) + n_rows*pitch
overhead_row1 <- measure_panel_overhead_mm(show_xlab = FALSE, show_header = TRUE)
overhead_row2 <- measure_panel_overhead_mm(show_xlab = FALSE, show_header = FALSE)
overhead_row3 <- measure_panel_overhead_mm(show_xlab = TRUE,  show_header = FALSE)
msg("Row overhead (mm, excl. bar area): row1(header)=", round(overhead_row1, 2),
    " row2=", round(overhead_row2, 2), " row3(xlab+caption)=", round(overhead_row3, 2))

## ---- Output naming: Geo vs Data is the main-text Figure 4; Geo vs Geo is a
## supplemental figure (see header note). Figures, PDFs, legends, .meta.json
## and per-panel tables all use this same basename per comparison.
fig4_output_name <- function(comparison) {
  if (comparison == "geo_vs_data") "fig_04_representativeness" else "supp_representativeness_geo_vs_geo"
}

## ---- Build, save, confirm (both comparisons) ------------------------------
FIG4_TABLES_DIR <- file.path(FIG_DIR, "tables")
fs::dir_create(FIG4_TABLES_DIR)
write_panel_table <- function(panel_letter, comparison, df) {
  tab <- df |> dplyr::filter(!is_neither) |> dplyr::transmute(
    bin_label = as.character(class_label), land_area_km2 = global_land_area_km2,
    land_fraction = global_land_fraction, towers = dplyr::coalesce(n, 0L),
    tower_fraction = dplyr::coalesce(network_frac, 0)
  )
  out <- file.path(FIG4_TABLES_DIR, sprintf("table_%s_%s.csv", tolower(panel_letter), fig4_output_name(comparison)))
  readr::write_csv(tab, out)
  write_output_metadata(
    out, input_sources = c(metrics_fig4_path),
    notes = sprintf("Panel %s (%s), %s version. Land grid: %s (%s km2 total).",
                     panel_letter, PANEL_SPECS[[panel_letter]]$axis, comparison,
                     PANEL_SPECS[[panel_letter]]$land_grid,
                     format(round(PANEL_SPECS[[panel_letter]]$total_km2), big.mark = ","))
  )
}

## ---- Per-comparison figure width (revised 2026-10-02): geo_vs_data stays
## the main-text 183mm double-column width; geo_vs_geo is the supplemental
## figure, moved into SupFigs/ as an Extended Data figure (<=180mm wide),
## re-rendered at 180mm without any text-size change (all pt sizes are
## absolute, never derived from FIG_WIDTH_MM) -- see SESSION_LOG.md. Reassigns
## the globals FIG_WIDTH_MM/COL_WIDTH_MM read by panel_mm_per_unit_for() and
## the ggsave() calls below; everything that depends on them (gutter/
## PANEL_MM_PER_UNIT calibration) is therefore recomputed inside this loop,
## not hoisted above it, even though it only actually changes for geo_vs_geo.
fig_heights_mm <- list()
fig_widths_mm  <- list()
for (comparison in c("geo_vs_geo", "geo_vs_data")) {
  msg("\n--- Building ", fig4_output_name(comparison), " (print) ---")
  FIG_WIDTH_MM <- if (comparison == "geo_vs_geo") 180 else 183
  COL_WIDTH_MM <- FIG_WIDTH_MM / NCOL_FIG
  msg("Figure width for this comparison: ", FIG_WIDTH_MM, "mm (column width ", COL_WIDTH_MM, "mm)")

  ## ---- Column alignment (task item 4): measure each panel's own natural
  ## left (y-axis label) gutter -- identical between the two output figures
  ## for a given letter, since panel_y_limits() doesn't depend on comparison
  ## or COL_WIDTH_MM -- then align every panel in a column to the WIDER of
  ## its column-mates' gutters. Recomputed every iteration (cheap; only the
  ## resulting PANEL_MM_PER_UNIT bar-area calibration actually changes
  ## between the two comparisons' now-different COL_WIDTH_MM).
  panel_layout_mm <- setNames(
    lapply(ALL_LETTERS, function(l) measure_panel_layout_mm(panel_y_limits(l), show_xlab_for(l))),
    ALL_LETTERS
  )
  gutter_mm <- vapply(panel_layout_mm, `[[`, numeric(1), "left_mm")
  TARGET_GUTTER_MM <- setNames(rep(NA_real_, 6), ALL_LETTERS)
  for (col in COLUMN_LAYOUT) {
    TARGET_GUTTER_MM[col] <- max(gutter_mm[col])
  }
  msg("Column gutter widths (mm), own -> aligned: col1 (a/c/e) ",
      paste(sprintf("%s=%.2f", names(gutter_mm[COLUMN_LAYOUT$col1]), gutter_mm[COLUMN_LAYOUT$col1]), collapse = ", "),
      " -> ", round(TARGET_GUTTER_MM[["A"]], 2),
      "mm; col2 (b/d/f) ",
      paste(sprintf("%s=%.2f", names(gutter_mm[COLUMN_LAYOUT$col2]), gutter_mm[COLUMN_LAYOUT$col2]), collapse = ", "),
      " -> ", round(TARGET_GUTTER_MM[["B"]], 2), "mm")

  PANEL_MM_PER_UNIT <- setNames(
    vapply(ALL_LETTERS, function(l) {
      panel_mm_per_unit_for(panel_y_limits(l), show_xlab_for(l), TARGET_GUTTER_MM[[l]])
    }, numeric(1)),
    ALL_LETTERS
  )

  fig_widths_mm[[comparison]] <- FIG_WIDTH_MM

  built <- lapply(c("A", "B", "C", "D", "E", "F"), build_fig4_panel2, comparison = comparison)
  names(built) <- c("A", "B", "C", "D", "E", "F")
  for (letter in names(built)) write_panel_table(letter, comparison, built[[letter]]$df)

  n_row1 <- max(built$A$n_rows, built$B$n_rows)
  n_row2 <- max(built$C$n_rows, built$D$n_rows)
  n_row3 <- max(built$E$n_rows, built$F$n_rows)
  h_row1 <- overhead_row1 + n_row1 * ROW_PITCH_MM
  h_row2 <- overhead_row2 + n_row2 * ROW_PITCH_MM
  h_row3 <- overhead_row3 + n_row3 * ROW_PITCH_MM
  ## TOP_PAD_MM: when patchwork's fixed-height rows (via heights=unit(...))
  ## sum to EXACTLY the device height, patchwork vertically centres that
  ## content block in the device -- with zero slack, the top row's title
  ## (whose "1grobheight" row estimate is a hair short of its true rendered
  ## extent) sits flush against the device's top edge and clips. Confirmed
  ## empirically (not guessed): clipped at +0/+0.5/.../+5mm of extra device
  ## height with the SAME row heights, resolved by +7mm (just visible) and
  ## reliably clear by +10mm -- a rendering-engine margin of error in
  ## patchwork/grid's row-height resolution, not a sizing mistake in this
  ## script's own row-pitch math (h_row1/h_row2/h_row3, i.e. the content
  ## heights actually reported, are unaffected by this padding).
  ## Explicit top/bottom spacer rows, NOT reliance on patchwork auto-centring
  ## a shorter-than-device content block (found unreliable: even generous
  ## uniform extra device height left the bottom caption clipped while
  ## fixing the top title -- the two edges needed independently-sized
  ## slack). TOP_PAD_MM=6mm: empirically sufficient for the title row's
  ## "1grobheight" rendering margin of error. BOTTOM_PAD_MM=10mm (revised
  ## 2026-10-02, up from a shared 6mm): confirmed by direct pixel inspection
  ## that 6mm still left the caption's descenders ("p" in "proportion")
  ## touching the very last PNG row (ink present at row height-1, the bottom
  ## edge, with zero blank rows beneath). 10mm leaves the bottom rows blank
  ## -- checked below, after render, for both PNGs.
  TOP_PAD_MM <- 6
  BOTTOM_PAD_MM <- 10
  total_height_mm <- h_row1 + h_row2 + h_row3 + TOP_PAD_MM + BOTTOM_PAD_MM
  fig_heights_mm[[comparison]] <- total_height_mm
  ## geo_vs_geo is now an Extended Data figure (<=180mm wide, <=240mm tall);
  ## geo_vs_data is the main-text figure (<=247mm tall, no narrower limit
  ## beyond the fixed 183mm width already enforced above).
  height_limit_mm <- if (comparison == "geo_vs_geo") 240 else 247
  msg("Row heights (mm): row1=", round(h_row1, 1), " row2=", round(h_row2, 1),
      " row3=", round(h_row3, 1), "; +", TOP_PAD_MM, "mm explicit top + ", BOTTOM_PAD_MM,
      "mm explicit bottom spacer; TOTAL=", round(total_height_mm, 1),
      " mm (hard limit ", height_limit_mm, "mm)")
  if (total_height_mm > height_limit_mm) {
    stop("Figure height ", round(total_height_mm, 1), " mm exceeds the ", height_limit_mm, "mm hard limit.")
  }

  grobs <- list(patchwork::plot_spacer(), patchwork::plot_spacer(),
                built$A$grob, built$B$grob, built$C$grob, built$D$grob, built$E$grob, built$F$grob,
                patchwork::plot_spacer(), patchwork::plot_spacer())
  composite <- patchwork::wrap_plots(grobs, ncol = 2,
                                      heights = grid::unit(c(TOP_PAD_MM, h_row1, h_row2, h_row3, BOTTOM_PAD_MM), "mm"))

  png_path <- file.path(FIG_DIR, paste0(fig4_output_name(comparison), ".png"))
  ggplot2::ggsave(png_path, composite, width = FIG_WIDTH_MM, height = total_height_mm, units = "mm",
                   dpi = 600, device = ragg::agg_png)
  pdf_path <- file.path(FIG_DIR, paste0(fig4_output_name(comparison), ".pdf"))
  ## base grDevices::pdf() only accepts PostScript base14 family NAMES
  ## ("Helvetica", not "Arial") -- the glyphs/metrics are the same base14
  ## Helvetica either way; the PNG (ragg) render is the one actually using
  ## the Arial font file via systemfonts.
  ggplot2::ggsave(pdf_path, composite, width = FIG_WIDTH_MM, height = total_height_mm, units = "mm",
                   device = grDevices::pdf, family = "Helvetica")
  if (comparison == "geo_vs_geo") {
    ## Extended Data figure: also write the 300 p.p.i. JPEG required alongside
    ## the PNG/PDF (task 6, 2026-10-02) -- this script predates
    ## save_nature_figure() and keeps its own bespoke composite/ggsave
    ## pipeline (patchwork row-height calibration above), so the JPEG is
    ## added directly here rather than routing the whole script through
    ## save_nature_figure().
    jpeg_path <- file.path(FIG_DIR, paste0(fig4_output_name(comparison), ".jpg"))
    ggplot2::ggsave(jpeg_path, composite, width = FIG_WIDTH_MM, height = total_height_mm, units = "mm",
                     dpi = NATURE_ED_JPEG_DPI, device = ragg::agg_jpeg, bg = "white", quality = 95)
    msg("Saved: ", png_path, ", ", pdf_path, " and ", jpeg_path)
  } else {
    msg("Saved: ", png_path, " and ", pdf_path)
  }
}

## ---- Confirm no data/n/J moved: reload the saved metrics CSV and diff
## against the in-memory metrics_df (computed before any Phase 5 rendering
## code ran) -- required by the task before finishing.
msg("\n=== Confirming n/J unchanged vs representativeness_metrics_fig4.csv ===")
reloaded_metrics <- readr::read_csv(metrics_fig4_path, show_col_types = FALSE)
compare_metrics <- dplyr::inner_join(
  metrics_df |> dplyr::select(panel, comparison, n_classified, weighted_jaccard),
  reloaded_metrics |> dplyr::select(panel, comparison, n_classified, weighted_jaccard),
  by = c("panel", "comparison"), suffix = c("_inmem", "_onfile")
) |> dplyr::mutate(
  n_match = n_classified_inmem == n_classified_onfile,
  j_match = abs(weighted_jaccard_inmem - weighted_jaccard_onfile) < 1e-9
)
if (all(compare_metrics$n_match) && all(compare_metrics$j_match)) {
  msg("CONFIRMED: all 12 rows' n and J match representativeness_metrics_fig4.csv exactly -- ",
      "no data, bins, exclusions, n, or J changed by this re-render.")
} else {
  bad <- compare_metrics |> dplyr::filter(!n_match | !j_match)
  stop("MISMATCH vs representativeness_metrics_fig4.csv for: ",
       paste(bad$panel, bad$comparison, sep = "/", collapse = ", "),
       " -- Phase 5 re-render must not change data. Investigate before proceeding.")
}
print(as.data.frame(compare_metrics[, c("panel", "comparison", "n_classified_onfile", "weighted_jaccard_onfile")]))

# ==============================================================================
# PHASE 5 (print): legends, figure .meta.json, copy to draft_manuscript_v1/
# ==============================================================================
msg("\n=== PHASE 5 (print): Legends, metadata, draft copy ===")

panel_n_line <- function(letter, cmp) {
  n <- metrics_df$n_classified[metrics_df$panel == letter & metrics_df$comparison == cmp]
  j <- metrics_df$weighted_jaccard[metrics_df$panel == letter & metrics_df$comparison == cmp]
  sprintf("  %s %s (%s): n = %d / 781, J = %.3f", PANEL_SPECS[[letter]]$letter, PANEL_SPECS[[letter]]$title,
          PANEL_SPECS[[letter]]$axis, n[1], j[1])
}

## ---- Caption land-area equivalents (task item 6): what 1/5/10/20/30% of
## each land grid's total actually is, in million km2, computed here from
## the same *_LAND_TOTAL_KM2 constants the panels themselves use -- so a
## reader can translate a panel's "% land" bar number into an area without
## doing the arithmetic themselves.
land_pct_line <- function(total_km2) {
  pcts <- c(1, 5, 10, 20, 30)
  vals <- total_km2 * pcts / 100 / 1e6
  paste(sprintf("%d%%=%.2f", pcts, vals), collapse = ", ")
}

write_fig4_legend <- function(comparison, fig_path, height_mm, width_mm) {
  cmp_label <- if (comparison == "geo_vs_geo") "Geo vs Geo" else "Geo vs Data"
  n_lines <- vapply(c("A", "B", "C", "D", "E", "F"), panel_n_line, character(1), cmp = comparison)

  if (comparison == "geo_vs_data") {
    ## Main-text Figure 4: full description, including the one-time plain-
    ## language definition of "Geo vs Data"/"Geo vs Geo" that the
    ## supplemental legend (below) refers back to rather than repeating.
    lines <- c(
      sprintf("FIGURE LEGEND — %s", basename(fig_path)),
      strrep("=", 60), "",
      "TITLE: Figure 5 — Representativeness of the current FLUXNET network (n=781), Geo vs Data", "",
      "(manuscript copy: draft_manuscript_v1/fig_05_representativeness.png; this source file",
      "keeps its own canonical, unnumbered name per docs/figure_inventory.md)", "",
      "DEFINITIONS (shared with the companion Supplementary Figure S4, Geo vs Geo,",
      "supp_representativeness_geo_vs_geo.png):",
      "\"Geo vs Data\" (this figure) compares the global land distribution of each axis against",
      "each site's own measured or site-derived value. \"Geo vs Geo\" (Supplementary Figure S4)",
      "compares the same global land distribution against the gridded product's own value",
      "sampled at each tower's coordinate, instead of the site's own measurement.", "",
      "DESCRIPTION:",
      "Six-panel sampling-ratio figure comparing the global land distribution of six",
      "environmental/biogeochemical axes against the current 781-site FLUXNET network, each",
      "panel showing global land vs. each site's own measured or site-derived value (see",
      "DEFINITIONS above).",
      "Panels: a Koppen-Geiger (13-class), b land cover as IGBP (15 PI-reported classes + an",
      "Other bin), c aridity (CGIAR UNEP 7-class), d biomass (ESA CCI v7, 7-bin), e NEE",
      "(signed sink/source, 7-bin), f ET (7-bin).",
      sprintf("Final artwork size: %g mm wide x %.1f mm tall, Helvetica throughout.", width_mm, height_mm), "",
      "Panels e and f's tower value (this Geo vs Data figure only) is the median of each site's annual",
      sprintf("values passing QC_THRESHOLD_YY=%s, each flux gated on its own QC column, per-site VUT/CUT", QC_THRESHOLD_YY),
      "(R/site_annual_fluxes.R::compute_site_annual_fluxes()).", "",
      "BAR LABELS:",
      "Each bar's length is the log2 sampling ratio (that class's share of current-network",
      "towers, divided by its share of global land area), clipped at +-5x; a bar truncated at",
      "the clip is annotated with its exact (unclipped) ratio at the bar's outer end (no",
      "decimals at 10x or above; capped at \">1000x\"). Faint vertical gridlines mark 1/5x,",
      "1/2x, 2x and 5x. Column headers \"% land\" and \"towers\", shown once above panels a and",
      "b, label the two number columns either side of the 1x line: the left number is that",
      "class's share of global land area (%, one decimal, \"<0.1\" below that); the right",
      "number is the current-network tower count (of 781) in that class. A number is set",
      "inside its bar, in a colour contrasting with the fill, only when the bar is long enough",
      "to fully contain it (measured at the figure's final rendered size, not estimated); if",
      "the bar is too short the number sits in near-black just beyond its outer end; with no",
      "bar on that side, the number sits beside the 1x line. A class with land but no current-",
      "network towers is drawn as a bar to the left clip limit with a white fill and dashed",
      "outline, labelled \"none\" instead of a tower count. J (weighted Jaccard overlap between",
      "the land and tower distributions) is right-aligned above each panel. Per-panel tower n",
      "is NOT shown on the panel -- see the per-panel n/J list below. The bottom row's x axis is",
      "labelled \"smaller proportion\" (left of 1x) / \"greater proportion\" (right of 1x): left of",
      "1x, that class holds a smaller proportion of current-network towers than of global land;",
      "right of 1x, a greater proportion.", "",
      "LAND GRIDS AND TOTALS:",
      "  Koppen, land cover and biomass: Beck et al. (2023) 1 km (0.00833 deg) Koppen-Geiger",
      "    land mask, 147,322,862 km2 -- land cover and biomass both reuse this same mask",
      "    directly, not a separately-resolved or finer version of it.",
      sprintf("    1/5/10/20/30%% of this total = %s million km2.", land_pct_line(KG_LAND_TOTAL_KM2)),
      "  Aridity: CGIAR Aridity Index v3.1's own native raster coverage, 134,761,545 km2 --",
      "    smaller than the shared 147.3M km2 total because the CGIAR raster ends at 60 deg S",
      "    (no Antarctic grid cells), unlike the Beck Koppen mask.",
      sprintf("    1/5/10/20/30%% of this total = %s million km2.", land_pct_line(ARIDITY_LAND_TOTAL_KM2)),
      "  NEE and ET: TRENDY v14 ensemble-median 0.5 deg grid under the Koppen land mask,",
      "    163,331,649 km2 -- at this coarse resolution a coastal cell straddling land and",
      "    ocean counts as whole land (no fractional-coverage weighting).",
      sprintf("    1/5/10/20/30%% of this total = %s million km2.", land_pct_line(FLUX_LAND_TOTAL_KM2)), "",
      sprintf("PER-PANEL n AND J (%s):", cmp_label), n_lines, "",
      "EXCLUSIONS AND SOURCES (Geo vs Data, panels a and c only):",
      sprintf("  Panel a (Koppen): the PI-reported class (BADM CLIMATE_KOEPPEN, case-normalised) is used"),
      sprintf("    for %d of 781 sites; the other %d fall back to the ERA5-local class, of which %d are",
              n_pi, 781L - n_pi, n_excl_grp + n_excl_ratio_high + n_excl_ratio_low),
      "    excluded by the rules below -- a PI-reported site is NEVER excluded even if its own ERA5",
      sprintf("    climatology would fail one of these rules. n = %d PI + %d ERA5 fallback = %d / 781.",
              n_pi, nrow(kg_geo_data_pool) - n_pi, nrow(kg_geo_data_pool)),
      "  1. GRP_ERA_DOWN: 172 sites whose BIF-recorded ERA_SLOPE for precipitation is the",
      "     sentinel -9999, distinguishing them from the other 609 sites' ERA_SLOPE=1.0 (a",
      "     different, more common sentinel) -- neither pattern is a genuinely fitted",
      "     regression: zero of the 781 current-network sites have one. See",
      "     methods_precip_exclusions.md.",
      sprintf("  2. P_ERA_MAX_RATIO=%d / P_ERA_MIN_RATIO=1/%d: P_ERA exceeds/falls below that many times",
              P_ERA_MAX_RATIO, round(1 / P_ERA_MIN_RATIO)),
      "     EVERY reference available (BADM MAP AND WorldClim BIO12) -- same dual-reference AND logic",
      "     both sides.",
      sprintf("  Panel a fallback-only exclusions: %d GRP_ERA_DOWN, %d P_ERA_MAX_RATIO, %d P_ERA_MIN_RATIO.",
              n_excl_grp, n_excl_ratio_high, n_excl_ratio_low),
      "  Panel c (aridity) applies all three rules to every site (no PI-reported analogue exists for",
      "  aridity) plus 4 sites with physically impossible raw ERA5 inputs to the FAO-56 PET calculation",
      "  (CD-Ygb, DE-Zrk, FR-LBr, US-Sne); DE-Zrk is also one of the 172 GRP_ERA_DOWN sites, counted once.",
      sprintf("  Panel c: %d GRP_ERA_DOWN, %d P_ERA_MAX_RATIO, %d P_ERA_MIN_RATIO, 4 invalid-input (less 1",
              length(slope9999_172), n_arid_excl_ratio_high, n_arid_excl_ratio_low),
      sprintf("  for DE-Zrk counted under both GRP_ERA_DOWN and invalid-input) excluded; n = %d / 781.",
              nrow(aridity_geo_data_pool)), "",
      "PERIOD MISMATCH (panel c only): CGIAR's Aridity Index v3.1 baseline is 1970-2000;",
      "panel c's Geo vs Data side (AI = P_ERA / FAO-56 PET) uses 1991-2020 ERA5 instead, to",
      "match the other ERA5-derived panels.", "",
      "SOURCE: scripts/figure4_representativeness.R. Per-panel tables (bin, land area km2,",
      "land fraction, towers, tower fraction) in review/figures/representativeness/tables/.",
      "Vector PDF alongside this PNG. Methods notes: methods_igbp.md, methods_aridity_era5.md,",
      "methods_flux_bin_scheme.md, methods_precip_exclusions.md (same directory)."
    )
  } else {
    ## Supplemental Geo vs Geo figure: refers to Figure 4 for everything the
    ## two share (definitions, panel layout, bar-label conventions, land
    ## grids/totals, methods notes) and states only what differs -- its own
    ## per-panel n/J (no precipitation exclusions apply to this side: every
    ## panel is n=781/781 since the gridded product has a value at every
    ## tower coordinate by construction).
    lines <- c(
      sprintf("FIGURE LEGEND — %s", basename(fig_path)),
      strrep("=", 60), "",
      "TITLE: Supplementary Figure S4 — Representativeness of the current FLUXNET network (n=781), Geo vs Geo", "",
      "(manuscript copy: SupFigs/figS4_representativeness_geo_vs_geo.png; this source file",
      "keeps its own canonical, unnumbered name per docs/figure_inventory.md)", "",
      "DESCRIPTION:",
      "Companion to Figure 5 (fig_05_representativeness.png, source fig_04_representativeness.png),",
      "same six panels (a Koppen-Geiger, b land cover as IGBP, c aridity, d biomass, e NEE, f ET) and",
      "the same panel layout, bar-label conventions, land grids/totals, and column headers -- see",
      "Figure 5's legend for all of that, including the \"Geo vs Data\"/\"Geo vs Geo\" definitions, which",
      "this figure shares in full. The only difference is the site-side value: Geo vs Geo classifies",
      "every tower by the gridded product's own value at that tower's coordinate, rather than the",
      "site's own measured or site-derived value. Because every tower has a value in the gridded",
      "product by construction, no panel here has the precipitation-dependent exclusions that apply to",
      "Figure 5's panels a and c -- n = 781/781 for all six panels. Panels e and f here use the",
      "model's own value at the tower, not a tower measurement -- Figure 5's own panels e and f",
      sprintf("(Geo vs Data) instead use each site's median annual value passing QC_THRESHOLD_YY=%s,", QC_THRESHOLD_YY),
      "each flux gated on its own QC column, per-site VUT/CUT (see Figure 5's legend).",
      sprintf("Final artwork size: %g mm wide x %.1f mm tall, Helvetica throughout. Supplementary Figure", width_mm, height_mm),
      "(target journal Scientific Data has no Extended Data concept) -- see",
      "docs/figure_inventory.md.", "",
      sprintf("PER-PANEL n AND J (%s):", cmp_label), n_lines, "",
      "SOURCE: scripts/figure4_representativeness.R. Per-panel tables (bin, land area km2,",
      "land fraction, towers, tower fraction) in review/figures/representativeness/tables/.",
      "Vector PDF alongside this PNG. Methods notes: methods_igbp.md, methods_aridity_era5.md,",
      "methods_flux_bin_scheme.md, methods_precip_exclusions.md (same directory)."
    )
  }
  writeLines(unlist(lines), paste0(tools::file_path_sans_ext(fig_path), ".legend.txt"))
}

for (comparison in c("geo_vs_geo", "geo_vs_data")) {
  fig_path <- file.path(FIG_DIR, paste0(fig4_output_name(comparison), ".png"))
  write_fig4_legend(comparison, fig_path, fig_heights_mm[[comparison]], fig_widths_mm[[comparison]])
  write_output_metadata(
    fig_path,
    input_sources = c(metrics_fig4_path, "site_koppen_beck2023.csv", "site_koppen_era5_fig4.csv",
                       "site_igbp_fig4.csv", "site_aridity.csv", "site_aridity_era5_fig4.csv",
                       "site_biomass_cci_v7.csv", "site_nee_fig4.csv", "site_et_fig4.csv"),
    notes = sprintf(
      paste0("%s (%s version), Nature final-artwork re-render: %g mm wide x %.1f mm tall, ",
             "Helvetica, %dpt text (row pitch %.1fmm). Rendering only -- confirmed against ",
             "representativeness_metrics_fig4.csv that no n or J changed from the prior (non-print) ",
             "render. Vector PDF saved alongside. See SESSION_LOG.md."),
      if (comparison == "geo_vs_geo") "Supplementary Figure S4 (companion to Figure 5)" else "Figure 5",
      if (comparison == "geo_vs_geo") "Geo vs Geo" else "Geo vs Data",
      fig_widths_mm[[comparison]], fig_heights_mm[[comparison]],
      BASE_PT, ROW_PITCH_MM
    )
  )
  msg("Saved: ", fig_path, ".meta.json and .legend.txt")
}

## geo_vs_data (main-text Figure 5, FIG_DIR basename fig_04_representativeness)
## copies into DRAFT_DIR under its renumbered name. geo_vs_geo (Supplementary
## Figure S4, FIG_DIR basename supp_representativeness_geo_vs_geo) copies into
## SUPFIGS_DIR under ITS renumbered name -- draft_manuscript_v1/ itself keeps
## only main-text figures. Target journal is Scientific Data (no Extended
## Data concept); figure stage 6 (2026-10-02) renumbered both copy
## destinations -- see docs/figure_inventory.md. DEST_BASENAME differs from
## the FIG_DIR source basename (fig4_output_name()); FIG_DIR itself is
## unchanged (that script/data file keeps its own name).
DEST_BASENAME <- list(geo_vs_geo = "figS4_representativeness_geo_vs_geo",
                       geo_vs_data = "fig_05_representativeness")
for (comparison in c("geo_vs_geo", "geo_vs_data")) {
  base <- fig4_output_name(comparison)
  dest_base <- DEST_BASENAME[[comparison]]
  dest_dir <- if (comparison == "geo_vs_geo") SUPFIGS_DIR else DRAFT_DIR
  exts <- if (comparison == "geo_vs_geo") c(".png", ".pdf", ".jpg", ".meta.json", ".legend.txt")
          else c(".png", ".pdf", ".meta.json", ".legend.txt")
  for (ext in exts) {
    src_file <- file.path(FIG_DIR, paste0(base, ext))
    dst_file <- file.path(dest_dir, paste0(dest_base, ext))
    fs::file_copy(src_file, dst_file, overwrite = TRUE)
    if (ext == ".legend.txt" && dest_base != base) {
      ## Rewrite only the self-referential header line to this copy's own
      ## renumbered filename; the legend BODY text already says "Figure 5"/
      ## "Supplementary Figure S4" directly (written above by
      ## write_fig4_legend()), since fig_path there is the FIG_DIR source
      ## path -- only the header's basename(fig_path) needs correcting here.
      txt <- readLines(dst_file)
      txt <- sub("^FIGURE LEGEND .*$", paste0("FIGURE LEGEND — ", dest_base, ".png"), txt)
      writeLines(txt, dst_file)
    }
  }
  msg("Copied ", base, " -> ", dest_base, " (", paste(exts, collapse = "/"), ") to ", dest_dir)
}

msg("\n=== figure4_representativeness.R: PRINT RE-RENDER COMPLETE ===")
