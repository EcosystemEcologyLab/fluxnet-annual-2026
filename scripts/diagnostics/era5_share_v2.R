## era5_share_v2.R
##
## Replacement for review/diagnostics/era5_share_for_coordination/ (18 September),
## for the FLUXNET Coordination Project and hub data managers. Diagnostic and
## correspondence material only -- no change to any paper figure, snapshot,
## pipeline code, or either of the two analysis folders this reads from.
##
## The previous package built its own ad hoc 4x/8x empirical clustering off
## review/diagnostics/era5_reference_plots/table_site_reference_comparison.csv.
## This version instead reads the four exclusion flags that
## scripts/figure4_representativeness.R ("Figure 5" after the 2026-10-02
## renumbering -- see that script's own header) actually applies to its
## precipitation-dependent Geo-vs-Data panels, straight off the already-
## committed data/snapshots/site_aridity_era5_fig4.csv -- no new flag logic is
## invented here.
##
## Inputs are committed tables, plus (section 6 only) the four invalid-input
## sites' own already-extracted raw ERA5 monthly files -- read-only, no
## re-extraction, no raster:
##   - data/snapshots/site_aridity_era5_fig4.csv
##   - review/diagnostics/precip_site_filter/table_1_site_level_precip_estimates.csv
##   - review/diagnostics/precip_downscaling_provenance/table_2_site_groups.csv
##   - data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv
##   - data/snapshots/site_koppen_era5_fig4.csv (section 9: Koppen-panel-dropped list)
##   - data/extracted/*/*_ERA5_MM_*.csv (section 6 only: the 4 invalid-input
##     sites' own raw monthly ERA5 file, to name the specific offending
##     variable/months/values -- the same raw-file read era5_share_for_coordination.R
##     and era5_cumulative_test.R already use elsewhere in this repo)
##
## scripts/diagnostics/era5_cumulative_test.R is separately re-run, UNCHANGED,
## against the current store, with its output copied (not generated in place)
## to era5_cumulative_test_rerun/ here -- see that subfolder's own note and
## README.md section "Cumulative-total re-test" for how that was done without
## touching the September outputs or editing the script.

source("R/pipeline_config.R")
source("R/utils.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(ggplot2)
  library(fs)
  library(lubridate)
  library(sf)
  library(purrr)
  library(tibble)
})

OUTD <- "review/diagnostics/era5_share_for_coordination_v2"
fs::dir_create(OUTD)

message("=== era5_share_v2.R ===")

# ============================================================================
# 0. INPUTS (read-only; committed tables only)
# ============================================================================

fig4 <- readr::read_csv("data/snapshots/site_aridity_era5_fig4.csv", show_col_types = FALSE)
t1 <- readr::read_csv("review/diagnostics/precip_site_filter/table_1_site_level_precip_estimates.csv", show_col_types = FALSE)
t2 <- readr::read_csv("review/diagnostics/precip_downscaling_provenance/table_2_site_groups.csv", show_col_types = FALSE)
snap <- readr::read_csv("data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv", show_col_types = FALSE)

stopifnot(nrow(fig4) == 781, nrow(t1) == 781, nrow(t2) == 781, nrow(snap) == 781)

snap_site <- snap |>
  dplyr::distinct(site_id, data_hub, site_name, product_source_network,
                   fluxnet_product_name, product_id, location_lat, location_long)
stopifnot(nrow(snap_site) == 781)

## Resolvable identifier link per hub. AmeriFlux's product_id is a bare DOI
## suffix (e.g. "10.17190/AMF/2571144"); ICOS's is the handle suffix under
## hdl.handle.net/11676/ (confirmed against every ICOS product_citation in
## this snapshot); TERN's product_id is already a full resolvable URL.
make_identifier_link <- function(data_hub, product_id) {
  dplyr::case_when(
    data_hub == "AmeriFlux" ~ paste0("https://doi.org/", product_id),
    data_hub == "ICOS" ~ paste0("https://hdl.handle.net/11676/", product_id),
    data_hub == "TERN" ~ product_id,
    TRUE ~ NA_character_
  )
}

site_core <- fig4 |>
  dplyr::select(site_id,
                era5_map_mm = P_mm, bio12_mm, ratio_era5_to_bio12 = ratio_bio12,
                badm_map_mm, ratio_era5_to_badm = ratio_badm,
                excluded_grp_era_down, excluded_p_era_ratio_high,
                excluded_p_era_ratio_low, invalid_era5_input) |>
  dplyr::left_join(dplyr::select(t1, site_id, n_years_measured, mean_measured_precip_mm = p_measured_mean_mm), by = "site_id") |>
  dplyr::left_join(dplyr::select(t2, site_id, era_slope, p_group), by = "site_id") |>
  dplyr::left_join(snap_site, by = "site_id") |>
  dplyr::mutate(identifier_link = make_identifier_link(data_hub, product_id))

stopifnot(!anyNA(site_core$data_hub))

## Each site carries at most one of the four flags in the source table
## (verified below); flag_label() reads that off directly rather than
## re-deriving any threshold logic.
n_flags_set <- with(site_core,
  dplyr::coalesce(excluded_grp_era_down, FALSE) +
  dplyr::coalesce(excluded_p_era_ratio_high, FALSE) +
  dplyr::coalesce(excluded_p_era_ratio_low, FALSE) +
  dplyr::coalesce(invalid_era5_input, FALSE))
stopifnot(all(n_flags_set <= 1))

flag_label <- function(df) {
  dplyr::case_when(
    df$excluded_grp_era_down ~ "no_slope",
    df$excluded_p_era_ratio_high ~ "above_3x_every_reference",
    df$excluded_p_era_ratio_low ~ "below_one_third",
    df$invalid_era5_input ~ "invalid_input",
    TRUE ~ NA_character_
  )
}
site_core <- site_core |> dplyr::mutate(flag = flag_label(site_core))

flagged <- site_core |> dplyr::filter(!is.na(flag))
message(sprintf("Flagged sites (exactly one of the four flags): %d / %d", nrow(flagged), nrow(site_core)))

FLAG_ORDER <- c("no_slope", "above_3x_every_reference", "below_one_third", "invalid_input")

# ============================================================================
# TIER CLASSIFICATION (used by site lists, summary_tiers_by_hub.csv, and the map)
# ============================================================================

## Tier 1: invalid_input. Tier 2: above_3x_every_reference or below_one_third
## (both ends of the direct ratio rule -- no_slope sites never reach tier 2
## regardless of their own ratio, since the ratio rule is skipped for them;
## see README.md). Tiers 3-5 subdivide the no_slope group only, by how many
## of its (up to two) available references its own ratio actually exceeds
## 3x -- a ratio-based severity ordering within a group whose own exclusion
## reason (a metadata sentinel) carries no severity information by itself.
assign_tier <- function(df) {
  has_badm <- !is.na(df$ratio_era5_to_badm)
  above3_badm <- has_badm & df$ratio_era5_to_badm > 3
  above3_bio12 <- !is.na(df$ratio_era5_to_bio12) & df$ratio_era5_to_bio12 > 3
  dplyr::case_when(
    df$flag == "invalid_input" ~ 1L,
    df$flag %in% c("above_3x_every_reference", "below_one_third") ~ 2L,
    df$flag == "no_slope" & has_badm & above3_badm & above3_bio12 ~ 3L,
    df$flag == "no_slope" & ((!has_badm & above3_bio12) | (has_badm & xor(above3_badm, above3_bio12))) ~ 4L,
    df$flag == "no_slope" ~ 5L,
    TRUE ~ NA_integer_
  )
}
flagged <- flagged |> dplyr::mutate(tier = assign_tier(flagged))
stopifnot(!anyNA(flagged$tier))

## "Largest departure first" within a tier: tier 2 mixes sites that are
## above 3x (ratio > 3) and sites that are below 1/3 (ratio < 1/3), so a
## plain descending sort on the ratio itself would not rank both directions
## by departure. abs(log10(ratio)) ranks departure from parity (ratio = 1)
## symmetrically in both directions and reduces to a plain descending ratio
## sort within tiers 1 and 3-5, where every ratio already exceeds 1.
flagged <- flagged |> dplyr::mutate(departure = abs(log10(ratio_era5_to_bio12)))

## Shared-ERA5-value group label, computed within each hub's flagged list
## (verified to give an identical result whether computed within-hub,
## network-wide, or restricted to the no_slope subset alone -- see
## README.md). Only sites sharing their era5_map_mm with >=1 other flagged
## site in the same hub get a label; all others are blank.
hub_abbrev <- c(ICOS = "ICOS", AmeriFlux = "AMF", TERN = "TERN")
assign_shared_groups <- function(df) {
  df |>
    dplyr::group_by(data_hub, era5_map_mm) |>
    dplyr::mutate(.grp_n = dplyr::n(), .grp_key = era5_map_mm) |>
    dplyr::ungroup() |>
    dplyr::group_by(data_hub) |>
    dplyr::mutate(
      .grp_rank = dplyr::if_else(.grp_n > 1, match(.grp_key, sort(unique(.grp_key[.grp_n > 1]), decreasing = TRUE)), NA_integer_)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      shared_value_group = dplyr::if_else(!is.na(.grp_rank), paste0(hub_abbrev[data_hub], "-G", sprintf("%02d", .grp_rank)), NA_character_)
    ) |>
    dplyr::select(-.grp_n, -.grp_key, -.grp_rank)
}
flagged <- flagged |> assign_shared_groups()

# ============================================================================
# 1. SITE LISTS, ONE PER HUB
# ============================================================================

message("\n================ Site lists per hub ================")

site_list_cols <- function(df) {
  df |>
    dplyr::arrange(tier, dplyr::desc(departure)) |>
    dplyr::select(site_id, site_name, product_source_network, fluxnet_product_name,
                   identifier_link, flag, tier,
                   era5_map_mm, bio12_mm, badm_map_mm,
                   ratio_era5_to_bio12, ratio_era5_to_badm,
                   n_years_measured, mean_measured_precip_mm, era_slope,
                   shared_value_group)
}

hub_counts <- list()
for (hub in c("ICOS", "AmeriFlux", "TERN")) {
  hub_flagged <- flagged |> dplyr::filter(data_hub == hub) |> site_list_cols()
  out_path <- file.path(OUTD, sprintf("site_list_%s.csv", hub))
  readr::write_csv(hub_flagged, out_path)
  write_output_metadata(
    out_path,
    input_sources = c("data/snapshots/site_aridity_era5_fig4.csv",
                       "review/diagnostics/precip_site_filter/table_1_site_level_precip_estimates.csv",
                       "review/diagnostics/precip_downscaling_provenance/table_2_site_groups.csv",
                       "data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv"),
    notes = sprintf("Every %s site carrying at least one of the four figure4_representativeness.R ('Figure 5') exclusion flags (no_slope, above_3x_every_reference, below_one_third, invalid_input), with its severity tier (1-5, see README.md) and, where it shares its ERA5 annual value with another flagged %s site, that group's label. Sorted by tier, then by departure from parity (abs(log10(ratio to BIO12)), largest first). %d rows.", hub, hub, nrow(hub_flagged))
  )
  message(sprintf("Saved: %s (%d rows)", out_path, nrow(hub_flagged)))
  hub_counts[[hub]] <- nrow(hub_flagged)
}

# ============================================================================
# 2. SUMMARY TABLES
# ============================================================================

message("\n================ Summary tables ================")

summary_by_hub <- site_core |>
  dplyr::filter(!is.na(flag)) |>
  dplyr::mutate(flag = factor(flag, levels = FLAG_ORDER)) |>
  dplyr::count(data_hub, flag, .drop = FALSE) |>
  tidyr::pivot_wider(names_from = flag, values_from = n, values_fill = 0) |>
  dplyr::mutate(total_flagged = no_slope + above_3x_every_reference + below_one_third + invalid_input) |>
  dplyr::arrange(data_hub)

summary_by_hub_path <- file.path(OUTD, "summary_by_hub.csv")
readr::write_csv(summary_by_hub, summary_by_hub_path)
write_output_metadata(
  summary_by_hub_path,
  input_sources = "data/snapshots/site_aridity_era5_fig4.csv",
  notes = "Count of each of the four Figure 5 exclusion flags, by hub, plus each hub's total flagged-site count."
)
message("Saved: ", summary_by_hub_path)
cat("\nsummary_by_hub.csv:\n")
print(as.data.frame(summary_by_hub))

summary_icos_by_network <- site_core |>
  dplyr::filter(data_hub == "ICOS") |>
  dplyr::group_by(product_source_network) |>
  dplyr::summarise(n_flagged = sum(!is.na(flag)), n_total = dplyr::n(), .groups = "drop") |>
  dplyr::arrange(product_source_network)

summary_icos_path <- file.path(OUTD, "summary_icos_by_source_network.csv")
readr::write_csv(summary_icos_by_network, summary_icos_path)
write_output_metadata(
  summary_icos_path,
  input_sources = c("data/snapshots/site_aridity_era5_fig4.csv",
                     "data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv"),
  notes = "ICOS-hub sites only, broken out by product_source_network (the contributing regional network, e.g. CNF/EUF/FLX/ICOS/JPF/KOF/SAEON): flagged count and total site count per network."
)
message("Saved: ", summary_icos_path)
cat("\nsummary_icos_by_source_network.csv:\n")
print(as.data.frame(summary_icos_by_network))

# ============================================================================
# 3. NO-SLOPE GROUP: MEDIAN RATIO AND 3-6X SHARE, PER HUB
# ============================================================================

message("\n================ No-slope group: median ratio and 3-6x share ================")

no_slope_by_hub <- site_core |>
  dplyr::filter(excluded_grp_era_down) |>
  dplyr::group_by(data_hub) |>
  dplyr::summarise(
    n = dplyr::n(),
    median_ratio_to_bio12 = median(ratio_era5_to_bio12, na.rm = TRUE),
    n_between_3x_and_6x = sum(ratio_era5_to_bio12 >= 3 & ratio_era5_to_bio12 <= 6, na.rm = TRUE),
    share_between_3x_and_6x = n_between_3x_and_6x / n,
    .groups = "drop"
  ) |>
  dplyr::arrange(data_hub)

no_slope_path <- file.path(OUTD, "summary_no_slope_group_by_hub.csv")
readr::write_csv(no_slope_by_hub, no_slope_path)
write_output_metadata(
  no_slope_path,
  input_sources = "data/snapshots/site_aridity_era5_fig4.csv",
  notes = "For the no_slope (GRP_ERA_DOWN / not_fitted_slope_9999) flagged group only: per hub, median ratio of ERA5 annual precipitation to WorldClim BIO12, and the share of that hub's no_slope sites with a ratio between 3x and 6x inclusive."
)
message("Saved: ", no_slope_path)
cat("\nsummary_no_slope_group_by_hub.csv:\n")
print(as.data.frame(no_slope_by_hub))

# ============================================================================
# 4. FIGURE: ERA5/BIO12 RATIO DISTRIBUTION, LOG AXIS, BY SLOPE GROUP x HUB
# ============================================================================

message("\n================ Figure: ratio distribution by slope group ================")

fig_df <- site_core |>
  dplyr::filter(!is.na(p_group), !is.na(ratio_era5_to_bio12), ratio_era5_to_bio12 > 0) |>
  dplyr::mutate(
    slope_group = dplyr::case_when(
      p_group == "not_fitted_slope1" ~ "ERA_SLOPE = 1 (standard 'not fitted' sentinel)",
      p_group == "not_fitted_slope_9999" ~ "ERA_SLOPE = -9999 (GRP_ERA_DOWN sentinel; no_slope flag)",
      TRUE ~ p_group
    )
  )

fig_ratio <- ggplot(fig_df, aes(x = ratio_era5_to_bio12, fill = data_hub)) +
  geom_histogram(bins = 40, colour = "white", linewidth = 0.1) +
  scale_x_log10(name = "ERA5 annual precipitation / WorldClim BIO12 (log scale)") +
  scale_fill_manual(values = c("AmeriFlux" = "#4C72B0", "ICOS" = "#55A868", "TERN" = "#C44E52"), name = "hub") +
  facet_wrap(~slope_group, ncol = 1) +
  labs(y = "number of sites",
       subtitle = sprintf("n = %d sites with a positive, finite ratio (of %d total)", nrow(fig_df), nrow(site_core))) +
  theme_minimal(base_size = 13) +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = margin(t = 8, r = 12, b = 6, l = 6),
        plot.subtitle = element_text(size = 10, colour = "grey30"),
        legend.position = "bottom")

out_fig <- file.path(OUTD, "fig1_ratio_by_slope_group_and_hub.png")
ggsave(out_fig, fig_ratio, width = 9, height = 7.5, dpi = 300, bg = "white")
write_output_metadata(
  out_fig,
  input_sources = c("data/snapshots/site_aridity_era5_fig4.csv",
                     "review/diagnostics/precip_downscaling_provenance/table_2_site_groups.csv",
                     "data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv"),
  notes = "Distribution of the ERA5-to-BIO12 annual precipitation ratio, log10 x-axis, for the two ERA_SLOPE metadata sentinel groups (slope=1 vs slope=-9999/GRP_ERA_DOWN), coloured by hub, all 781 current-network sites (minus the 4 invalid_era5_input sites and 1 zero-ratio site, which have no finite positive ratio)."
)
message("Saved: ", out_fig)

# ============================================================================
# 6. INVALID-INPUT SITES: WHICH RAW ERA5 VARIABLE, HOW MANY MONTHS, WHAT RANGE
# ============================================================================

message("\n================ Invalid inputs: raw monthly ERA5 file, each of the 4 sites ================")

INVALID_SITES <- c("US-Sne", "CD-Ygb", "DE-Zrk", "FR-LBr")
ERA5_PERIOD <- KG_ERA5_PERIOD  # 1991-2020, same window figure4_representativeness.R screens

extracted_dirs <- list.dirs("data/extracted", recursive = FALSE, full.names = TRUE)
dir_site_ids <- sub("^[A-Za-z0-9]+_([A-Za-z]{2}-[A-Za-z0-9]+)_FLUXNET_.*$", "\\1", basename(extracted_dirs))
site_dir_lookup <- setNames(extracted_dirs, dir_site_ids)

read_era5_mm_raw <- function(site_id) {
  d <- site_dir_lookup[[site_id]]
  if (is.null(d) || is.na(d)) return(NULL)
  f <- list.files(d, pattern = "_ERA5_MM_.*\\.csv$", full.names = TRUE)
  if (length(f) != 1) return(NULL)
  readr::read_csv(f, show_col_types = FALSE, progress = FALSE) |>
    dplyr::transmute(site_id = site_id, year = TIMESTAMP %/% 100L, month = TIMESTAMP %% 100L,
                      TA_ERA, SW_IN_ERA, LW_IN_ERA, VPD_ERA, PA_ERA, WS_ERA) |>
    dplyr::filter(year >= ERA5_PERIOD[1], year <= ERA5_PERIOD[2])
}

## Same per-month plausibility screen figure4_representativeness.R applies
## (LW_IN/SW_IN <0 or >1000 W/m^2; VPD <0 or >100 hPa; WS <=0 or >50 m/s; PA
## outside [50,110] kPa; TA outside [-90,60] degC), applied here directly to
## the raw monthly file (every individual calendar month in 1991-2020), not
## to the 12-point climatological-mean series the production script screens
## -- so "in how many months" below counts raw site-months, up to 360 (30
## years x 12).
SCREEN <- list(
  TA_ERA = list(bad = function(x) x < -90 | x > 60, label = "TA outside [-90, 60] degC"),
  SW_IN_ERA = list(bad = function(x) x < 0 | x > 1000, label = "SW_IN outside [0, 1000] W/m^2"),
  LW_IN_ERA = list(bad = function(x) x < 0 | x > 1000, label = "LW_IN outside [0, 1000] W/m^2"),
  VPD_ERA = list(bad = function(x) x < 0 | x > 100, label = "VPD outside [0, 100] hPa"),
  PA_ERA = list(bad = function(x) x < 50 | x > 110, label = "PA outside [50, 110] kPa"),
  WS_ERA = list(bad = function(x) x <= 0 | x > 50, label = "WS outside (0, 50] m/s")
)

invalid_inputs_rows <- purrr::map_dfr(INVALID_SITES, function(sid) {
  raw <- read_era5_mm_raw(sid)
  stopifnot(!is.null(raw))
  n_total <- nrow(raw)
  per_var <- purrr::map_dfr(names(SCREEN), function(v) {
    x <- raw[[v]]
    bad <- SCREEN[[v]]$bad(x)
    bad[is.na(bad)] <- FALSE
    if (sum(bad) == 0L) return(NULL)
    tibble::tibble(
      site_id = sid, variable = v, threshold = SCREEN[[v]]$label,
      n_invalid_months = sum(bad), n_total_months = n_total,
      min_offending_value = min(x[bad], na.rm = TRUE), max_offending_value = max(x[bad], na.rm = TRUE)
    )
  })
  stopifnot(nrow(per_var) >= 1L)  # every one of these 4 sites fails for exactly one reason
  per_var
})

invalid_inputs_rows <- invalid_inputs_rows |>
  dplyr::left_join(dplyr::select(snap_site, site_id, data_hub, product_source_network), by = "site_id") |>
  dplyr::select(site_id, data_hub, product_source_network, variable, threshold,
                n_invalid_months, n_total_months, min_offending_value, max_offending_value)

cat("\ntable_invalid_inputs.csv:\n")
print(as.data.frame(invalid_inputs_rows))

out_invalid <- file.path(OUTD, "table_invalid_inputs.csv")
readr::write_csv(invalid_inputs_rows, out_invalid)
write_output_metadata(
  out_invalid,
  input_sources = "data/extracted/*/*_ERA5_MM_*.csv (the 4 invalid_era5_input sites' own raw monthly ERA5 files)",
  notes = "For each of the 4 invalid_era5_input sites, the single raw ERA5 variable that fails figure4_representativeness.R's physical-plausibility screen, applied here per raw calendar month (1991-2020, up to 360 site-months) rather than to the script's own 12-point climatological-mean series: how many months fail, and the range of the offending raw monthly values. Each site fails for exactly one variable (no site has more than one variable flagged)."
)
message("Saved: ", out_invalid)

# ============================================================================
# 7. TIER SUMMARY, BY HUB
# ============================================================================

message("\n================ Tier summary, by hub ================")

TIER_LABELS <- c(
  "1" = "tier1_invalid_input",
  "2" = "tier2_above3x_or_below_third",
  "3" = "tier3_no_slope_both_refs_above3x",
  "4" = "tier4_no_slope_one_ref_above3x",
  "5" = "tier5_no_slope_below3x"
)

summary_tiers_by_hub <- flagged |>
  dplyr::mutate(tier_label = factor(TIER_LABELS[as.character(tier)], levels = unname(TIER_LABELS))) |>
  dplyr::count(data_hub, tier_label, .drop = FALSE) |>
  tidyr::pivot_wider(names_from = tier_label, values_from = n, values_fill = 0) |>
  dplyr::mutate(total_flagged = tier1_invalid_input + tier2_above3x_or_below_third +
                  tier3_no_slope_both_refs_above3x + tier4_no_slope_one_ref_above3x + tier5_no_slope_below3x) |>
  dplyr::arrange(data_hub)

tiers_path <- file.path(OUTD, "summary_tiers_by_hub.csv")
readr::write_csv(summary_tiers_by_hub, tiers_path)
write_output_metadata(
  tiers_path,
  input_sources = "data/snapshots/site_aridity_era5_fig4.csv",
  notes = "Count of each of the 5 severity tiers (see README.md for exact definitions), by hub, plus each hub's total flagged-site count."
)
message("Saved: ", tiers_path)
cat("\nsummary_tiers_by_hub.csv:\n")
print(as.data.frame(summary_tiers_by_hub))

# ============================================================================
# 8. SHARED-VALUE SUMMARY, BY HUB
# ============================================================================

message("\n================ Shared ERA5 value summary, by hub ================")

summary_shared_values_by_hub <- flagged |>
  dplyr::group_by(data_hub) |>
  dplyr::summarise(
    n_sites_sharing_a_value = sum(!is.na(shared_value_group)),
    n_groups = dplyr::n_distinct(shared_value_group[!is.na(shared_value_group)]),
    .groups = "drop"
  ) |>
  dplyr::arrange(data_hub)

shared_path <- file.path(OUTD, "summary_shared_values_by_hub.csv")
readr::write_csv(summary_shared_values_by_hub, shared_path)
write_output_metadata(
  shared_path,
  input_sources = "data/snapshots/site_aridity_era5_fig4.csv",
  notes = "Per hub: number of flagged sites whose ERA5 1991-2020 mean annual precipitation is numerically identical to >=1 other flagged site in the same hub's site list, and the number of distinct such groups. Computed within each hub's own flagged list; verified to give an identical result whether computed within-hub, network-wide, or restricted to the no_slope subset alone (all three scopes agree exactly)."
)
message("Saved: ", shared_path)
cat("\nsummary_shared_values_by_hub.csv:\n")
print(as.data.frame(summary_shared_values_by_hub))

# ============================================================================
# 9. SOURCE NETWORKS, ALL HUBS
# ============================================================================

message("\n================ Source networks, all hubs ================")

summary_by_source_network <- site_core |>
  dplyr::group_by(data_hub, product_source_network) |>
  dplyr::summarise(n_flagged = sum(!is.na(flag)), n_total = dplyr::n(), .groups = "drop") |>
  dplyr::arrange(data_hub, product_source_network)

source_net_path <- file.path(OUTD, "summary_by_source_network.csv")
readr::write_csv(summary_by_source_network, source_net_path)
write_output_metadata(
  source_net_path,
  input_sources = c("data/snapshots/site_aridity_era5_fig4.csv",
                     "data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv"),
  notes = "Flagged and total site counts per product_source_network (the contributing regional network), with its hub, across the whole 781-site current network."
)
message("Saved: ", source_net_path)
cat("\nsummary_by_source_network.csv:\n")
print(as.data.frame(summary_by_source_network))

# ============================================================================
# 10. SITES DROPPED FROM THE KOPPEN PANEL (FIGURE 5, GEO VS DATA)
# ============================================================================

message("\n================ Koppen panel (Figure 5, Geo vs Data): dropped sites ================")

koppen_fig4 <- readr::read_csv("data/snapshots/site_koppen_era5_fig4.csv", show_col_types = FALSE)
stopifnot(nrow(koppen_fig4) == 781, "panel_a_eligible" %in% names(koppen_fig4))

koppen_dropped <- koppen_fig4 |>
  dplyr::filter(!panel_a_eligible) |>
  dplyr::left_join(dplyr::select(snap_site, site_id, data_hub, product_source_network), by = "site_id") |>
  dplyr::select(site_id, data_hub, product_source_network, panel_a_source,
                badm_map_mm, ratio_badm, bio12_mm, ratio_bio12) |>
  dplyr::arrange(data_hub, panel_a_source, site_id)

stopifnot(!anyNA(koppen_dropped$data_hub))
cat("\ntable_koppen_panel_dropped.csv: ", nrow(koppen_dropped), " sites dropped\n")
print(table(koppen_dropped$panel_a_source, koppen_dropped$data_hub))

koppen_dropped_path <- file.path(OUTD, "table_koppen_panel_dropped.csv")
readr::write_csv(koppen_dropped, koppen_dropped_path)
write_output_metadata(
  koppen_dropped_path,
  input_sources = c("data/snapshots/site_koppen_era5_fig4.csv",
                     "data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv"),
  notes = sprintf("Every site dropped from Figure 5's Koppen (panel A) Geo-vs-Data panel (panel_a_eligible == FALSE), with hub, source network, and panel_a_source (which rule caught it: excluded_grp_era_down or excluded_p_era_ratio_low here -- none dropped by excluded_p_era_ratio_high, since panel A is PI-class-first and every site that would be caught by the high-ratio rule has a PI-reported class instead). %d sites, distinct from this package's own 207-site aridity-panel flagged list: panel A never excludes a PI-sourced site even if its own ERA5 climatology would otherwise fail one of these rules.", nrow(koppen_dropped))
)
message("Saved: ", koppen_dropped_path)

# ============================================================================
# 11. MAP: ALL FLAGGED SITES, COLOURED BY TIER
# ============================================================================

message("\n================ Figure: map of flagged sites by tier ================")

.sf_use_s2_old <- sf::sf_use_s2(FALSE)
land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |> sf::st_make_valid()
suppressWarnings(land <- sf::st_crop(land, xmin = -180, xmax = 180, ymin = -60, ymax = 85))

map_df <- flagged |>
  dplyr::filter(!is.na(location_lat), !is.na(location_long)) |>
  dplyr::mutate(tier_label = factor(TIER_LABELS[as.character(tier)], levels = unname(TIER_LABELS)))
stopifnot(nrow(map_df) == nrow(flagged))

TIER_COLORS <- c(
  tier1_invalid_input = "#999999",
  tier2_above3x_or_below_third = "#C44E52",
  tier3_no_slope_both_refs_above3x = "#4C72B0",
  tier4_no_slope_one_ref_above3x = "#8172B2",
  tier5_no_slope_below3x = "#64B5CD"
)

fig_map <- ggplot() +
  geom_sf(data = land, fill = "gray95", colour = "gray80", linewidth = 0.2) +
  geom_point(data = map_df, aes(x = location_long, y = location_lat, colour = tier_label, shape = data_hub),
             size = 1.9, alpha = 0.85) +
  scale_colour_manual(values = TIER_COLORS, name = "tier",
                       labels = c("1: invalid input", "2: above 3x / below 1/3", "3: no slope, both refs > 3x",
                                  "4: no slope, one ref > 3x", "5: no slope, below 3x")) +
  scale_shape_manual(values = c(AmeriFlux = 16, ICOS = 17, TERN = 15), name = "hub") +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 85), expand = FALSE) +
  labs(subtitle = sprintf("n = %d flagged sites", nrow(map_df))) +
  theme_minimal(base_size = 12) +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        panel.grid = element_line(colour = "gray92", linewidth = 0.2),
        axis.title = element_blank(),
        plot.subtitle = element_text(size = 10, colour = "grey30"),
        legend.position = "bottom", legend.box = "vertical")

out_map <- file.path(OUTD, "fig2_map_flagged_sites_by_tier.png")
ggsave(out_map, fig_map, width = 11, height = 6.5, dpi = 300, bg = "white")
write_output_metadata(
  out_map,
  input_sources = c("data/snapshots/site_aridity_era5_fig4.csv", "data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv"),
  notes = "World map of all 207 flagged sites, coloured by severity tier (1-5, see README.md), shaped by hub. Land outline from rnaturalearthdata (medium scale), equirectangular (unprojected lat/long)."
)
message("Saved: ", out_map)
sf::sf_use_s2(.sf_use_s2_old)

# ============================================================================
# 5. DATA DICTIONARY
# ============================================================================

dict_text <- c(
  "DATA DICTIONARY -- site_list_<hub>.csv",
  "",
  "site_id                    FLUXNET site ID.",
  "site_name                  Site name as recorded in the FLUXNET Shuttle manifest.",
  "product_source_network     The contributing regional network that produced this",
  "                           site's data product (e.g. AMF, ICOS, JPF, CNF, EUF,",
  "                           FLX, KOF, SAEON, TERN). Not derivable from the site ID.",
  "fluxnet_product_name       The distributed product's file/version name.",
  "identifier_link            A resolvable link to the product: a doi.org link for",
  "                           AmeriFlux, an hdl.handle.net link for ICOS, and the",
  "                           product's own URL (already a DOI link) for TERN.",
  "flag                       Which of the four figure4_representativeness.R ('Figure",
  "                           5') exclusion flags this site carries. Exactly one per",
  "                           flagged site: 'no_slope', 'above_3x_every_reference',",
  "                           'below_one_third', or 'invalid_input'. See README.md for",
  "                           the exact rule and threshold behind each.",
  "tier                       Severity tier, 1 (most severe) to 5: 1=invalid_input;",
  "                           2=above_3x_every_reference or below_one_third; 3/4/5",
  "                           subdivide no_slope by how many available references its",
  "                           own ratio exceeds 3x (both / one / neither). See README.md.",
  "                           Site lists are sorted by tier, then by departure from",
  "                           parity (largest first).",
  "era5_map_mm                ERA5 mean annual precipitation, 1991-2020, mm/year.",
  "                           Blank for invalid_input sites (see README.md).",
  "bio12_mm                   WorldClim v2.1 BIO12 (annual precipitation), mm/year, at",
  "                           the tower coordinate.",
  "badm_map_mm                Mean annual precipitation as reported by the site PI in",
  "                           the site's own BADM metadata, mm/year. Blank where not",
  "                           reported.",
  "ratio_era5_to_bio12        era5_map_mm / bio12_mm.",
  "ratio_era5_to_badm         era5_map_mm / badm_map_mm. Blank where badm_map_mm is",
  "                           blank.",
  "n_years_measured           Number of years of tower-measured precipitation",
  "                           available for this site (precip_site_filter.R).",
  "mean_measured_precip_mm    Mean annual precipitation from the tower's own gauge",
  "                           record, mm/year, where any measured year exists",
  "                           (precip_site_filter.R, p_measured_mean_mm). Blank where",
  "                           n_years_measured is 0.",
  "era_slope                  The ERA_SLOPE value recorded in the site's own BIF",
  "                           downscaling metadata (GRP_ERA_DOWN, ERA_VARIABLE=P): 1",
  "                           for the standard 'not fitted' sentinel, or -9999 for the",
  "                           GRP_ERA_DOWN sentinel that drives the no_slope flag.",
  "shared_value_group          Group label (e.g. 'ICOS-G01') where this site's",
  "                           era5_map_mm is numerically identical to >=1 other flagged",
  "                           site in the same hub's list. Blank where its value is",
  "                           unique within that hub's flagged list."
)
out_dict <- file.path(OUTD, "data_dictionary.txt")
writeLines(dict_text, out_dict)
message("Saved: ", out_dict)

message("\n=== era5_share_v2.R complete (site lists + tiers + summaries + 2 figures + dictionary; README.md written separately) ===")
