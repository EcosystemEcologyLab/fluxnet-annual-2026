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
## Inputs are committed tables only -- no rasters, no re-extraction, nothing
## read from data/extracted/ or data/raw/:
##   - data/snapshots/site_aridity_era5_fig4.csv
##   - review/diagnostics/precip_site_filter/table_1_site_level_precip_estimates.csv
##   - review/diagnostics/precip_downscaling_provenance/table_2_site_groups.csv
##   - data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv

source("R/pipeline_config.R")
source("R/utils.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(ggplot2)
  library(fs)
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
                   fluxnet_product_name, product_id)
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
  dplyr::left_join(dplyr::select(t1, site_id, n_years_measured), by = "site_id") |>
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

# ============================================================================
# 1. SITE LISTS, ONE PER HUB
# ============================================================================

message("\n================ Site lists per hub ================")

FLAG_ORDER <- c("no_slope", "above_3x_every_reference", "below_one_third", "invalid_input")

site_list_cols <- function(df) {
  df |>
    dplyr::mutate(flag = factor(flag, levels = FLAG_ORDER)) |>
    dplyr::arrange(flag, ratio_era5_to_bio12) |>
    dplyr::select(site_id, site_name, product_source_network, fluxnet_product_name,
                   identifier_link, flag,
                   era5_map_mm, bio12_mm, badm_map_mm,
                   ratio_era5_to_bio12, ratio_era5_to_badm,
                   n_years_measured, era_slope)
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
    notes = sprintf("Every %s site carrying at least one of the four figure4_representativeness.R ('Figure 5') exclusion flags (no_slope, above_3x_every_reference, below_one_third, invalid_input). Sorted by flag, then by ratio to BIO12. %d rows.", hub, nrow(hub_flagged))
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
  "era_slope                  The ERA_SLOPE value recorded in the site's own BIF",
  "                           downscaling metadata (GRP_ERA_DOWN, ERA_VARIABLE=P): 1",
  "                           for the standard 'not fitted' sentinel, or -9999 for the",
  "                           GRP_ERA_DOWN sentinel that drives the no_slope flag."
)
out_dict <- file.path(OUTD, "data_dictionary.txt")
writeLines(dict_text, out_dict)
message("Saved: ", out_dict)

message("\n=== era5_share_v2.R complete (site lists + summaries + figure + dictionary; README.md written separately) ===")
