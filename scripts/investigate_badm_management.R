## investigate_badm_management.R
##
## BADM land-management coverage across the FLUXNET Shuttle network.
##
## Supports discussion-section prose on how network expansion into managed
## lands (crops, forests, grasslands, wetlands) aligns with GHG-monitoring
## priorities and Paris Agreement carbon accounting.
##
## Data source: BIF ("Biological, Ancillary, Disturbance and Metadata")
## files shipped inside each site's FLUXNET Shuttle download package
## (data/extracted/*/*_BIF_*.csv). This is a reduced ONEFlux-processing-time
## BADM export, NOT the full AmeriFlux BADM template — see
## review/figures/methods_badm_management.md for the coverage finding and
## known limitations (no HARV_M/TILL_M/FERT_M/GRZ_M/IRR/BURN/THIN groups
## anywhere in the network; management signal comes from the coarse
## DOM_DIST_MGMT category plus free-text SITE_DESC keyword mining).
##
## Outputs:
##   data/snapshots/badm_management_coverage.csv (+ .meta.json)
##   data/snapshots/badm_management_summary.csv  (+ .meta.json)
##   review/figures/candidates/fig_supp_badm_management_by_igbp.png (+ .txt caption)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(fs)
  library(ggplot2)
  library(countrycode)
})

source("R/pipeline_config.R")
check_pipeline_config()
source("R/utils.R")
source("R/plot_constants.R")

SNAP_DIR      <- file.path(FLUXNET_DATA_ROOT, "snapshots")
EXTRACTED_DIR <- file.path(FLUXNET_DATA_ROOT, "extracted")
FIG_DIR       <- file.path("review", "figures", "candidates")
fs::dir_create(FIG_DIR)

SCRIPT_NAME <- "investigate_badm_management.R"

# ============================================================================
# 1. Master site list (site_id, IGBP, network, region) from latest snapshot
# ============================================================================

snap_path <- sort(
  list.files(SNAP_DIR, pattern = "^fluxnet_shuttle_snapshot_.*\\.csv$", full.names = TRUE),
  decreasing = TRUE
)[1]
message("Using shuttle snapshot: ", snap_path)

snapshot <- readr::read_csv(snap_path, show_col_types = FALSE)

sites <- snapshot |>
  dplyr::distinct(.data$site_id, .keep_all = TRUE) |>
  dplyr::transmute(
    site_id       = .data$site_id,
    igbp          = .data$igbp,
    network       = .data$network,
    location_lat  = .data$location_lat,
    location_long = .data$location_long
  )

message("Master site list: ", nrow(sites), " sites")

# ---- Region: ISO2 country-code prefix -> continent -> 6-way regional bucket
# Same convention as .iso2_to_continent() in R/figures/fig_timeseries.R (site
# ID prefix IS the ISO 3166-1 country code — this is NOT the site-ID-prefix ->
# hub inference forbidden by CLAUDE.md, which concerns hub/network membership,
# not country/continent geography).
iso2      <- substr(sites$site_id, 1, 2)
iso2_norm <- dplyr::if_else(iso2 == "UK", "GB", iso2)
subregion <- countrycode::countrycode(
  iso2_norm, origin = "iso2c", destination = "un.regionsub.name", warn = FALSE
)

REGION_MAP <- c(
  "Northern America"           = "N. America",
  "Central America"            = "S. America",
  "Caribbean"                  = "S. America",
  "South America"              = "S. America",
  # The installed countrycode version's un.regionsub.name lumps all of Latin
  # America and the Caribbean (incl. Mexico) into one label rather than
  # splitting Central/South America/Caribbean per the finer UN M49 geoscheme.
  # Folded entirely into "S. America" here as a documented simplification —
  # see review/figures/methods_badm_management.md.
  "Latin America and the Caribbean" = "S. America",
  "Northern Europe"            = "Europe",
  "Western Europe"             = "Europe",
  "Southern Europe"            = "Europe",
  "Eastern Europe"             = "Europe",
  "Northern Africa"            = "Africa",
  "Sub-Saharan Africa"         = "Africa",
  "Eastern Africa"             = "Africa",
  "Western Africa"             = "Africa",
  "Middle Africa"              = "Africa",
  "Southern Africa"            = "Africa",
  "Western Asia"               = "Asia",
  "Central Asia"               = "Asia",
  "Eastern Asia"               = "Asia",
  "South-eastern Asia"         = "Asia",
  "Southern Asia"              = "Asia",
  "Australia and New Zealand"  = "Australia",
  "Melanesia"                  = "Australia",
  "Micronesia"                 = "Australia",
  "Polynesia"                  = "Australia"
)
sites$region <- unname(REGION_MAP[subregion])

unmapped <- sites$site_id[is.na(sites$region)]
for (sid in unmapped) {
  log_unknown(sid, paste("Could not map ISO2 prefix to a region bucket (subregion:",
                          subregion[sites$site_id == sid], ")"), SCRIPT_NAME)
}
if (length(unmapped) > 0) {
  message(length(unmapped), " site(s) with unmapped region logged to outputs/unknown_log.csv")
}

# ============================================================================
# 2. Read all BIF files directly from data/extracted (not the cached
#    data/processed/badm.rds, so this script is reproducible from committed +
#    extracted data alone and reflects the CURRENT extracted-site set)
# ============================================================================

bif_files <- fs::dir_ls(EXTRACTED_DIR, recurse = 1, glob = "*_BIF_*.csv")
bif_files <- bif_files[!grepl("BIFVARINFO", bif_files)]
message("Found ", length(bif_files), " site BIF files under ", EXTRACTED_DIR)

BIF_STANDARD_COLS <- c("SITE_ID", "GROUP_ID", "VARIABLE_GROUP", "VARIABLE", "DATAVALUE")

read_one_bif <- function(path) {
  cols    <- names(readr::read_csv(path, n_max = 0L, show_col_types = FALSE))
  missing <- setdiff(BIF_STANDARD_COLS, cols)
  if (length(missing) > 0) {
    log_unknown(basename(path),
                paste("BIF file missing required column(s):", paste(missing, collapse = ", ")),
                SCRIPT_NAME)
    return(NULL)
  }
  readr::read_csv(path, col_types = readr::cols(.default = "c"))[, BIF_STANDARD_COLS]
}

badm <- dplyr::bind_rows(lapply(bif_files, read_one_bif))
message("Combined BADM/BIF table: ", nrow(badm), " rows, ", length(unique(badm$SITE_ID)), " sites")

# ============================================================================
# 3. Variable-group inventory (answers analytical question 2)
# ============================================================================

variable_group_inventory <- badm |>
  dplyr::group_by(.data$VARIABLE_GROUP) |>
  dplyr::summarise(n_sites = dplyr::n_distinct(.data$SITE_ID), n_records = dplyr::n(), .groups = "drop") |>
  dplyr::arrange(dplyr::desc(.data$n_sites))

MGMT_GROUPS_SOUGHT <- c(
  "DM", "HARV", "HARV_M", "TILL", "TILL_M", "FERT", "FERT_M", "GRZ", "GRZ_M",
  "IRR", "IRR_M", "BURN", "THIN", "LU", "DRA"
)
groups_found_sought <- intersect(MGMT_GROUPS_SOUGHT, unique(badm$VARIABLE_GROUP))
message("Standard BADM management/disturbance groups sought: ", paste(MGMT_GROUPS_SOUGHT, collapse = ", "))
message("... found in this network's BIF files: ",
        if (length(groups_found_sought) == 0) "NONE" else paste(groups_found_sought, collapse = ", "))
message("Only management-relevant group actually present: DOM_DIST_MGMT (categorical, event-count not tracked)")

# ============================================================================
# 4. Derive management signal per site
#    (a) DOM_DIST_MGMT — structured but coarse categorical tag(s) per site
#    (b) SITE_DESC — free-text keyword mining (heuristic, finer-grained)
#    (c) GRP_WTD — water-table-depth *measurement* group (ancillary only;
#        presence of WTD monitoring is not itself evidence of active
#        drainage/water-table *management*, so it is reported separately and
#        NOT folded into any management flag)
# ============================================================================

dom_dist <- badm |>
  dplyr::filter(.data$VARIABLE_GROUP == "DOM_DIST_MGMT", .data$VARIABLE == "DOM_DIST_MGMT")

dom_dist_wide <- dom_dist |>
  dplyr::group_by(.data$SITE_ID) |>
  dplyr::summarise(
    dom_dist_mgmt_values       = paste(sort(unique(.data$DATAVALUE)), collapse = "; "),
    dom_dist_agriculture       = any(.data$DATAVALUE == "Agriculture"),
    dom_dist_forestry          = any(.data$DATAVALUE == "Forestry"),
    dom_dist_grazing           = any(.data$DATAVALUE == "Grazing"),
    dom_dist_fire              = any(.data$DATAVALUE == "Fire"),
    dom_dist_land_cover_change = any(.data$DATAVALUE == "Land cover change"),
    dom_dist_hydrologic_event  = any(.data$DATAVALUE == "Hydrologic event"),
    dom_dist_natural_other     = any(.data$DATAVALUE %in% c("Drought", "Storm or wind",
                                                              "Temperature extreme", "Pests and disease")),
    dom_dist_undisturbed       = any(.data$DATAVALUE == "Undisturbed"),
    .groups = "drop"
  ) |>
  dplyr::rename(site_id = "SITE_ID")

site_desc <- badm |> dplyr::filter(.data$VARIABLE == "SITE_DESC")

KW <- list(
  txt_harvest       = "harvest",
  txt_tillage       = "till|plow|plough|disk(ed|ing)?",
  txt_fertilization = "fertili[sz]",
  txt_grazing       = "graz|livestock|cattle|sheep|pastur",
  txt_irrigation    = "irrigat",
  txt_thinning      = "thinn",
  txt_burning       = "burn|fire|prescribed fire",
  txt_drainage      = "drain|water[- ]table|rewet|ditch"
)

site_desc_wide <- site_desc |>
  dplyr::group_by(.data$SITE_ID) |>
  dplyr::summarise(site_desc_text = paste(unique(.data$DATAVALUE), collapse = " || "), .groups = "drop") |>
  dplyr::rename(site_id = "SITE_ID")

for (nm in names(KW)) {
  site_desc_wide[[nm]] <- grepl(KW[[nm]], site_desc_wide$site_desc_text, ignore.case = TRUE)
}

wtd_sites <- badm |>
  dplyr::filter(.data$VARIABLE_GROUP == "GRP_WTD") |>
  dplyr::distinct(.data$SITE_ID) |>
  dplyr::transmute(site_id = .data$SITE_ID, wtd_group_present = TRUE)

badm_site_ids <- unique(badm$SITE_ID)

for (sid in setdiff(sites$site_id, badm_site_ids)) {
  log_unknown(sid, "No BIF/BADM file found in data/extracted for this site", SCRIPT_NAME)
}

# ============================================================================
# 5. Assemble per-site coverage table
# ============================================================================

coverage <- sites |>
  dplyr::mutate(has_badm_record = .data$site_id %in% badm_site_ids) |>
  dplyr::left_join(dom_dist_wide,  by = "site_id") |>
  dplyr::left_join(site_desc_wide, by = "site_id") |>
  dplyr::left_join(wtd_sites,      by = "site_id") |>
  dplyr::mutate(
    dplyr::across(dplyr::starts_with("dom_dist_") & !dplyr::any_of("dom_dist_mgmt_values"),
                  ~ tidyr::replace_na(.x, FALSE)),
    dplyr::across(dplyr::starts_with("txt_"), ~ tidyr::replace_na(.x, FALSE)),
    wtd_group_present  = tidyr::replace_na(.data$wtd_group_present, FALSE),
    has_dom_dist_mgmt  = !is.na(.data$dom_dist_mgmt_values),
    has_site_desc      = !is.na(.data$site_desc_text)
  )

coverage <- coverage |>
  dplyr::mutate(
    mgmt_harvest               = .data$txt_harvest,
    mgmt_tillage                = .data$txt_tillage,
    mgmt_fertilization          = .data$txt_fertilization,
    mgmt_grazing                 = .data$txt_grazing | .data$dom_dist_grazing,
    mgmt_irrigation              = .data$txt_irrigation,
    mgmt_thinning                = .data$txt_thinning,
    mgmt_burning                 = .data$txt_burning | .data$dom_dist_fire,
    mgmt_drainage_wtd            = .data$txt_drainage,
    mgmt_forestry_unspecified    = .data$dom_dist_forestry & !(.data$txt_harvest | .data$txt_thinning),
    mgmt_other                   = (.data$dom_dist_agriculture | .data$dom_dist_land_cover_change) &
      !(.data$mgmt_harvest | .data$mgmt_tillage | .data$mgmt_fertilization | .data$mgmt_irrigation |
          .data$mgmt_grazing | .data$mgmt_burning | .data$mgmt_drainage_wtd)
  ) |>
  dplyr::mutate(
    any_management = .data$mgmt_harvest | .data$mgmt_tillage | .data$mgmt_fertilization |
      .data$mgmt_grazing | .data$mgmt_irrigation | .data$mgmt_thinning | .data$mgmt_burning |
      .data$mgmt_drainage_wtd | .data$mgmt_forestry_unspecified | .data$mgmt_other
  )

MGMT_COLS <- c("mgmt_harvest", "mgmt_tillage", "mgmt_fertilization", "mgmt_grazing",
               "mgmt_irrigation", "mgmt_thinning", "mgmt_burning", "mgmt_drainage_wtd",
               "mgmt_forestry_unspecified", "mgmt_other", "any_management")

# Sites with no BADM record at all: management flags are unknown (NA), not FALSE
coverage <- coverage |>
  dplyr::mutate(
    dplyr::across(dplyr::all_of(c(MGMT_COLS, "has_dom_dist_mgmt", "has_site_desc")),
                  ~ dplyr::if_else(.data$has_badm_record, .x, NA))
  )

coverage_out <- coverage |>
  dplyr::select(
    "site_id", "igbp", "region", "network", "location_lat", "location_long",
    "has_badm_record", "has_dom_dist_mgmt", "dom_dist_mgmt_values", "has_site_desc",
    dplyr::all_of(MGMT_COLS), "wtd_group_present"
  ) |>
  dplyr::arrange(.data$site_id)

out_coverage_path <- "data/snapshots/badm_management_coverage.csv"
readr::write_csv(coverage_out, out_coverage_path)
message("Written: ", out_coverage_path, " (", nrow(coverage_out), " sites)")

# ============================================================================
# 6. Cross-tabulated summary (IGBP x management-type, region x management-type)
# ============================================================================

summarise_by <- function(df, group_col) {
  df |>
    dplyr::filter(.data$has_badm_record) |>
    tidyr::pivot_longer(cols = dplyr::all_of(MGMT_COLS), names_to = "management_type", values_to = "present") |>
    dplyr::group_by(.data[[group_col]], .data$management_type) |>
    dplyr::summarise(
      n_sites_with_badm = dplyr::n(),
      n_sites_positive  = sum(.data$present, na.rm = TRUE),
      pct               = round(100 * .data$n_sites_positive / .data$n_sites_with_badm, 1),
      .groups = "drop"
    ) |>
    dplyr::rename(group = 1) |>
    dplyr::mutate(dimension = group_col) |>
    dplyr::relocate("dimension", "group")
}

summary_all <- dplyr::bind_rows(
  summarise_by(coverage, "igbp"),
  summarise_by(coverage, "region")
)

out_summary_path <- "data/snapshots/badm_management_summary.csv"
readr::write_csv(summary_all, out_summary_path)
message("Written: ", out_summary_path, " (", nrow(summary_all), " rows)")

# ============================================================================
# 7. Metadata companions
# ============================================================================

coverage_notes <- paste(
  "BADM source: BIF files shipped in FLUXNET Shuttle download packages,",
  "data/extracted/*/*_BIF_*.csv (read directly, not the cached data/processed/badm.rds).",
  "Variable groups searched:", paste(MGMT_GROUPS_SOUGHT, collapse = ", "), "-",
  "NONE of these standard AmeriFlux BADM management templates were found anywhere in the",
  "network's BIF files. Coverage definition: 'any record present in this category' means",
  "EITHER (a) the site's single coarse DOM_DIST_MGMT categorical tag matches (Grazing ->",
  "mgmt_grazing, Fire -> mgmt_burning, Forestry -> mgmt_forestry_unspecified when no harvest/",
  "thinning keyword confirms which), OR (b) a case-insensitive keyword match in the free-text",
  "SITE_DESC field (harvest, till/plow/disk, fertili[sz]e, graz/livestock/cattle/sheep/pasture,",
  "irrigat, thinn, burn/fire, drain/water table/rewet/ditch). mgmt_other = DOM_DIST_MGMT tag of",
  "Agriculture or Land cover change with no specific subtype resolved by text mining. Sites with",
  "no extracted BIF file (has_badm_record = FALSE) have NA (unknown), not FALSE, for all",
  "management flags. wtd_group_present flags GRP_WTD (water-table-depth measurement) presence;",
  "this is an ancillary environmental-monitoring signal, NOT itself evidence of active drainage/",
  "water-table management, and is deliberately excluded from any_management.",
  "Snapshot date/source:", basename(snap_path)
)

write_output_metadata(
  out_coverage_path,
  input_sources = c(snap_path, "data/extracted/*/*_BIF_*.csv"),
  notes = coverage_notes
)

write_output_metadata(
  out_summary_path,
  input_sources = c(out_coverage_path),
  notes = paste(
    "Cross-tabulation of badm_management_coverage.csv by IGBP class and by region",
    "(6-way regional bucket derived from ISO2 site-ID country-code prefix via",
    "countrycode::countrycode(..., 'un.regionsub.name'), NOT from network/hub membership).",
    "n_sites_with_badm is the denominator (sites with an extracted BIF file in this",
    "IGBP/region group); n_sites_positive / pct are computed over that denominator only."
  )
)

# ============================================================================
# 8. Console summary (printed for SESSION_LOG / methods doc drafting)
# ============================================================================

n_total   <- nrow(coverage_out)
n_badm    <- sum(coverage_out$has_badm_record)
n_any_mgmt <- sum(coverage_out$any_management, na.rm = TRUE)

message("\n==== BADM management coverage summary ====")
message(sprintf("Sites in master list: %d", n_total))
message(sprintf("Sites with an extracted BIF file: %d (%.1f%%)", n_badm, 100 * n_badm / n_total))
message(sprintf("Sites with any management record: %d / %d BADM sites (%.1f%%)",
                 n_any_mgmt, n_badm, 100 * n_any_mgmt / n_badm))

for (col in MGMT_COLS) {
  n_pos <- sum(coverage_out[[col]], na.rm = TRUE)
  message(sprintf("  %-28s %4d / %4d (%.1f%%)", col, n_pos, n_badm, 100 * n_pos / n_badm))
}

message("\n==== Variable group inventory (top 20 by site count) ====")
print(head(variable_group_inventory, 20))

message("\n==== Unknown/exclusion logging ====")
message("outputs/unknown_log.csv and outputs/exclusion_log.csv updated (see file for detail)")

# ============================================================================
# 9. Illustrative site examples (for methods doc / SESSION_LOG)
# ============================================================================

illustrative_candidates <- coverage_out |>
  dplyr::filter(.data$has_badm_record) |>
  dplyr::mutate(n_mgmt_flags = rowSums(dplyr::across(dplyr::all_of(MGMT_COLS[MGMT_COLS != "any_management"])),
                                        na.rm = TRUE))

pick_example <- function(df, igbp_filter, mgmt_col) {
  cand <- df |> dplyr::filter(.data$igbp %in% igbp_filter, .data[[mgmt_col]])
  if (nrow(cand) == 0) return(NULL)
  cand |> dplyr::arrange(dplyr::desc(.data$n_mgmt_flags)) |> dplyr::slice(1) |> dplyr::pull(.data$site_id)
}

example_ids <- c(
  cropland = pick_example(illustrative_candidates, c("CRO"), "mgmt_fertilization"),
  forest   = pick_example(illustrative_candidates, c("ENF","EBF","DNF","DBF","MF"), "mgmt_forestry_unspecified"),
  grazed   = pick_example(illustrative_candidates, c("GRA","SAV","WSA"), "mgmt_grazing"),
  wetland  = pick_example(illustrative_candidates, c("WET"), "mgmt_drainage_wtd")
)
example_ids <- example_ids[!vapply(example_ids, is.null, logical(1))]

message("\n==== Illustrative site candidates ====")
for (nm in names(example_ids)) {
  sid <- example_ids[[nm]]
  message(sprintf("-- %s: %s --", nm, sid))
  desc <- site_desc_wide$site_desc_text[site_desc_wide$site_id == sid]
  dd   <- dom_dist_wide$dom_dist_mgmt_values[dom_dist_wide$site_id == sid]
  if (length(desc) > 0) message("   SITE_DESC: ", substr(desc, 1, 300))
  if (length(dd) > 0)   message("   DOM_DIST_MGMT: ", dd)
}

# ============================================================================
# 10. Supplementary figure: management-record counts by IGBP class x
#     management type (heat map)
# ============================================================================

MGMT_LABELS <- c(
  mgmt_harvest               = "Harvest",
  mgmt_tillage               = "Tillage",
  mgmt_fertilization         = "Fertilization",
  mgmt_grazing               = "Grazing",
  mgmt_irrigation            = "Irrigation",
  mgmt_thinning              = "Thinning",
  mgmt_burning               = "Burning",
  mgmt_drainage_wtd          = "Drainage / water table",
  mgmt_forestry_unspecified  = "Forestry (unspecified)",
  mgmt_other                 = "Other / unclassified"
)

heat_data <- coverage |>
  dplyr::filter(.data$has_badm_record, !is.na(.data$igbp)) |>
  tidyr::pivot_longer(cols = dplyr::all_of(names(MGMT_LABELS)),
                       names_to = "management_type", values_to = "present") |>
  dplyr::group_by(.data$igbp, .data$management_type) |>
  dplyr::summarise(n = sum(.data$present, na.rm = TRUE), .groups = "drop") |>
  dplyr::mutate(management_type = factor(MGMT_LABELS[.data$management_type], levels = unname(MGMT_LABELS)))

# IGBP_order (R/plot_constants.R) is the standard 15-class MODIS IGBP scheme
# and omits a few non-standard codes present in this network's BADM (BSV, CVM,
# SNO — flagged in the methods doc as a data-quality note). Canonical classes
# keep their house ordering; any extra classes are appended at the end so no
# site is silently dropped from the figure. IGBP_colours already defines BSV;
# CVM/SNO fall back to a neutral grey since they have no assigned palette entry.
igbp_present <- unique(heat_data$igbp)
igbp_levels  <- c(intersect(IGBP_order, igbp_present), setdiff(igbp_present, IGBP_order))
heat_data$igbp <- factor(heat_data$igbp, levels = igbp_levels)

igbp_fallback_colour <- "#9E9E9E"
igbp_tick_colours <- ifelse(igbp_levels %in% names(IGBP_colours), IGBP_colours[igbp_levels], igbp_fallback_colour)
igbp_label_html <- setNames(
  paste0("<span style='color:", igbp_tick_colours, "'>", igbp_levels, "</span>"),
  igbp_levels
)

p_heat <- ggplot2::ggplot(heat_data, ggplot2::aes(x = .data$igbp, y = .data$management_type, fill = .data$n)) +
  ggplot2::geom_tile(color = "white", linewidth = 0.5) +
  ggplot2::geom_text(ggplot2::aes(label = ifelse(.data$n == 0, "", .data$n)), size = 3.2, color = "black") +
  ggplot2::scale_x_discrete(labels = igbp_label_html) +
  ggplot2::scale_fill_gradient(low = "#F5F5F5", high = "#B71C1C", name = "Sites") +
  ggplot2::labs(x = NULL, y = NULL) +
  fluxnet_theme(base_size = 13) +
  ggplot2::theme(
    axis.text.x  = ggtext::element_markdown(angle = 45, hjust = 1),
    panel.border = ggplot2::element_blank(),
    axis.ticks   = ggplot2::element_blank(),
    legend.position = "right"
  )

fig_path <- file.path(FIG_DIR, "fig_supp_badm_management_by_igbp.png")
ggplot2::ggsave(fig_path, plot = p_heat, width = 9, height = 5.5, units = "in", dpi = 300, bg = "white")
message("Figure saved: ", fig_path)

fig_caption <- paste0(
  "Supplementary Figure — BADM Management-Record Counts by IGBP Class\n\n",
  "Heat map of the number of FLUXNET Shuttle sites (of ", n_badm, " sites with an extracted ",
  "BIF/BADM file) carrying a management-relevant record in each of 10 categories, by IGBP land-",
  "cover class. Cell values are site counts; IGBP x-axis labels are coloured with the network's ",
  "standard IGBP palette (R/plot_constants.R).\n\n",
  "Categories combine two BADM sources: the coarse structured DOM_DIST_MGMT (Dominant ",
  "Disturbance/Management) categorical tag (used for Grazing, Burning, and Forestry-unspecified) ",
  "and case-insensitive keyword mining of the free-text SITE_DESC field (used for Harvest, ",
  "Tillage, Fertilization, Irrigation, Thinning, Drainage/water table, and to refine Grazing/",
  "Burning). 'Other/unclassified' = a DOM_DIST_MGMT tag of Agriculture or Land cover change with ",
  "no specific subtype resolved by text mining. See review/figures/methods_badm_management.md for ",
  "full method and limitations — the network's BIF files contain NO standard AmeriFlux ",
  "event-based management BADM groups (HARV_M, TILL_M, FERT_M, GRZ_M, IRR, BURN, THIN); this ",
  "figure is a best-effort proxy, not a count of discrete management events.\n\n",
  "Data source: FLUXNET Shuttle download packages, data/extracted/*/*_BIF_*.csv. ",
  "Snapshot: ", basename(snap_path), "."
)
writeLines(fig_caption, file.path(FIG_DIR, "fig_supp_badm_management_by_igbp.txt"))
message("Caption saved: ", file.path(FIG_DIR, "fig_supp_badm_management_by_igbp.txt"))

message("\ninvestigate_badm_management.R complete.")
