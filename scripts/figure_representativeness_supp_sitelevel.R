## figure_representativeness_supp_sitelevel.R
##
## Supplementary representativeness (Jaccard) figures addressing a co-author
## comment on Figs 4/5: NEE and ET should compare against what was actually
## MEASURED at the towers, not TRENDY model output sampled at site
## coordinates; and land cover should use the IGBP scheme flux-tower
## metadata natively carries, not ESA CCI's own scheme. KG is left exactly
## as-is (unchanged from Figs 4/5 / the 2026-09-17 KG source-consistency
## investigation).
##
## This script does NOT modify figure_representativeness_summary.R,
## figure_representativeness_kg.R, any data/snapshots/*.csv, or
## representativeness_metrics.csv. It only READS existing snapshot files
## and WRITES new files under review/figures/candidates/. Helper functions
## (count_sites/merge_sr/compute_repr_metrics-equivalents) are duplicated
## locally rather than sourced from the two scripts above, consistent with
## how those two scripts already duplicate similar logic independently.
##
## Axis substitutions vs. AXES6 in figure_representativeness_summary.R:
##   KG       unchanged  (site: site_koppen_era5.csv / site_koppen_beck2023_*;
##                         global: koppen_beck2023_global_distribution.csv)
##   Aridity  unchanged  (site: site_aridity*.csv; global: aridity_unep7_*)
##   Biomass  unchanged  (site: site_biomass_cci_v7*.csv; global: biomass_cci_v7_*)
##   LULC  -> IGBP        site: `igbp` field from each network's own site list
##                         (tower metadata, full 17-class IGBP scheme, not the
##                         12-class STANDARD_IGBP subset used by
##                         scripts/assess_flux_data_by_igbp_shuttle.R);
##                         global: constructed here from a documented ESA-CCI
##                         -native -> IGBP crosswalk applied to the existing
##                         landcover_cci_native_global_distribution.csv (see
##                         Supp_methods_siteKG_IGBP_NEE_ET.txt for the full
##                         crosswalk table and rationale, including the
##                         MODIS/IGBP published-reference comparison that was
##                         attempted and could not be verified -- documented,
##                         not silently dropped).
##   NEE-IAV/ET-median (TRENDY, site-extracted) -> NEE/ET (measured):
##                         site: nep_median / et_median from
##                         site_flux_medians_shuttle.csv (current_781) and
##                         site_flux_medians_fluxnet2015.csv (fluxnet2015) --
##                         already-vetted QC=0.80 VUT/CUT+NT/DT medians built
##                         for Fig 3. La Thuile/Marconi have no downloaded
##                         flux time series in this repo -> NA for those two
##                         networks in the trajectory figure (explicit gap,
##                         not interpolated). nep_median is abs()-transformed
##                         to match the existing TRENDY nee_median axis's own
##                         "mean |flux|" convention
##                         (figure_representativeness_trendy_compute.R:381,543);
##                         global: UNCHANGED -- reuse
##                         trendy_nee_median_global_distribution{,_18bin}.csv /
##                         trendy_et_median_global_distribution{,_18bin}.csv
##                         verbatim, including their stored bin edges, so the
##                         global side of this comparison is genuinely
##                         untouched (same pattern the KG axis already uses:
##                         site source changes, global reference stays put).
##
## Outputs (review/figures/candidates/):
##   Supp_sampling_ratio_siteKG_IGBP_NEE_ET.png / .legend.txt
##   Supp_jaccard_trajectory_siteKG_IGBP_NEE_ET.png / .legend.txt
##   Supp_methods_siteKG_IGBP_NEE_ET.txt   (consolidated detailed methods)
##   table_igbp_esacci_crosswalk.csv        (+ .meta.json)
##   table_igbp_global_distribution_crosswalk.csv (+ .meta.json)
##   table_supp_sampling_ratio_siteKG_IGBP_NEE_ET.csv (+ .meta.json)
##   table_supp_jaccard_trajectory_siteKG_IGBP_NEE_ET.csv (+ .meta.json)

source("R/pipeline_config.R")
source("R/utils.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(fs)
})

SNAP <- "data/snapshots"
OUTD <- "review/figures/candidates"
fs::dir_create(OUTD)

message("=== figure_representativeness_supp_sitelevel.R ===")

# ============================================================================
# 1. IGBP 17-CLASS SCHEME AND ESA-CCI-NATIVE -> IGBP CROSSWALK
# ============================================================================

IGBP_ORDER <- c("ENF","EBF","DNF","DBF","MF","CSH","OSH","WSA","SAV","GRA",
                "WET","CRO","URB","CVM","SNO","BSV","WAT")
IGBP_NAMES <- c(
  ENF = "Evergreen Needleleaf Forest", EBF = "Evergreen Broadleaf Forest",
  DNF = "Deciduous Needleleaf Forest", DBF = "Deciduous Broadleaf Forest",
  MF  = "Mixed Forest",                CSH = "Closed Shrublands",
  OSH = "Open Shrublands",             WSA = "Woody Savannas",
  SAV = "Savannas",                    GRA = "Grasslands",
  WET = "Permanent Wetlands",          CRO = "Croplands",
  URB = "Urban and Built-up",          CVM = "Cropland/Natural Vegetation Mosaic",
  SNO = "Permanent Snow and Ice",      BSV = "Barren or Sparsely Vegetated",
  WAT = "Water Bodies"
)

# Documented, constructed crosswalk (NOT an official ESA/NASA product -- no
# verifiable published ESA-CCI<->IGBP correspondence table was found; see
# Supp_methods_siteKG_IGBP_NEE_ET.txt for the search record). Built from the
# two schemes' own published class definitions. `rationale` records the
# judgment call for every native class that isn't a direct 1:1 match.
igbp_crosswalk <- readr::read_csv(
  file.path(SNAP, "cci_landcover_aggregation_lookup.csv"), show_col_types = FALSE
) |>
  dplyr::select(lulc_native, lulc_native_name) |>
  dplyr::mutate(
    igbp_class = dplyr::case_match(lulc_native,
      c(10L, 11L, 12L, 20L) ~ "CRO",
      c(30L, 40L)           ~ "CVM",
      50L                   ~ "EBF",
      c(60L, 61L, 62L)      ~ "DBF",
      c(70L, 71L, 72L)      ~ "ENF",
      c(80L, 81L, 82L)      ~ "DNF",
      90L                   ~ "MF",
      100L                  ~ "WSA",
      110L                  ~ "SAV",
      c(120L, 121L, 122L)   ~ "OSH",
      130L                  ~ "GRA",
      140L                  ~ "GRA",
      c(150L, 151L, 152L, 153L) ~ "BSV",
      c(160L, 170L, 180L)   ~ "WET",
      190L                  ~ "URB",
      c(200L, 201L, 202L)   ~ "BSV",
      210L                  ~ "WAT",
      220L                  ~ "SNO",
      .default = NA_character_
    ),
    rationale = dplyr::case_match(lulc_native,
      c(10L, 11L, 12L, 20L) ~ "Direct: all CCI cropland subtypes -> CRO",
      c(30L, 40L) ~ "Mosaic cropland/natural-vegetation classes (neither component >60%) -> CVM per IGBP's own mosaic definition",
      c(50L, 60L, 61L, 62L, 70L, 71L, 72L, 80L, 81L, 82L, 90L) ~ "Direct: closed/open tree-cover classes map 1:1 on leaf type + phenology",
      100L ~ "Mosaic tree-and-shrub(>50%)/herbaceous(<50%): woody-dominated with herbaceous understory -> closest IGBP match is Woody Savanna (30-60% tree cover, herbaceous understory)",
      110L ~ "Mosaic herbaceous(>50%)/tree-and-shrub(<50%): herbaceous-dominated with woody component -> closest IGBP match is Savanna (10-30% tree cover)",
      c(120L, 121L, 122L) ~ "CCI shrubland classes carry no canopy-density split (IGBP splits Closed >60% vs Open 10-60%); defaulted to Open Shrublands as the globally more common case -- CSH is never assigned by this crosswalk",
      130L ~ "Direct: Grassland -> GRA",
      140L ~ "Lichens and mosses: non-vascular but vegetated ground cover, not bare -- closest IGBP match is Grasslands (no explicit tundra/moss class in IGBP)",
      c(150L, 151L, 152L, 153L) ~ "CCI 'sparse vegetation, <15% cover' aligns with IGBP Barren's own threshold ('never >10% vegetated cover') -> BSV",
      c(160L, 170L, 180L) ~ "Direct: all CCI flooded/inundated classes -> WET",
      190L ~ "Direct: Urban areas -> URB",
      c(200L, 201L, 202L) ~ "Direct: Bare areas -> BSV",
      210L ~ "Direct: Water bodies -> WAT",
      220L ~ "Direct: Permanent snow/ice -> SNO",
      .default = NA_character_
    )
  )
stopifnot(!anyNA(igbp_crosswalk$igbp_class))

out_crosswalk <- file.path(OUTD, "table_igbp_esacci_crosswalk.csv")
readr::write_csv(igbp_crosswalk, out_crosswalk)
write_output_metadata(
  out_crosswalk,
  input_sources = file.path(SNAP, "cci_landcover_aggregation_lookup.csv"),
  notes = paste0(
    "Constructed ESA-CCI-native(37-class)->IGBP(17-class) crosswalk for the ",
    "supplementary co-author-response representativeness figures. Not an ",
    "official ESA/NASA crosswalk (none found verifiable -- see ",
    "Supp_methods_siteKG_IGBP_NEE_ET.txt for the search record); built from ",
    "each scheme's own published class definitions. See the `rationale` ",
    "column for every non-trivial mapping choice."
  )
)
message("Saved: ", out_crosswalk)

# ---- IGBP global distribution: re-aggregate the existing CCI global dist ---
igbp_global <- readr::read_csv(
  file.path(SNAP, "landcover_cci_native_global_distribution.csv"), show_col_types = FALSE
) |>
  dplyr::left_join(dplyr::select(igbp_crosswalk, lulc_native, igbp_class), by = "lulc_native") |>
  dplyr::group_by(igbp_class) |>
  dplyr::summarise(global_land_fraction = sum(global_land_fraction), .groups = "drop") |>
  dplyr::right_join(data.frame(igbp_class = IGBP_ORDER, stringsAsFactors = FALSE), by = "igbp_class") |>
  dplyr::mutate(global_land_fraction = dplyr::coalesce(global_land_fraction, 0)) |>
  dplyr::arrange(match(igbp_class, IGBP_ORDER))
stopifnot(abs(sum(igbp_global$global_land_fraction) - 1) < 1e-6)

out_igbp_global <- file.path(OUTD, "table_igbp_global_distribution_crosswalk.csv")
readr::write_csv(igbp_global, out_igbp_global)
write_output_metadata(
  out_igbp_global,
  input_sources = c(file.path(SNAP, "landcover_cci_native_global_distribution.csv"), out_crosswalk),
  notes = "Global IGBP-class land fraction, built by applying table_igbp_esacci_crosswalk.csv to the existing (unchanged) ESA CCI native-class global distribution. Sums to 1.0 (land-only, no ocean)."
)
message("Saved: ", out_igbp_global, " (sums to ", round(sum(igbp_global$global_land_fraction), 6), ")")

# ============================================================================
# 2. SITE LOADERS
# ============================================================================

NET_ORDER  <- c("marconi", "la_thuile", "fluxnet2015", "current_781")
NET_NSITES <- c(marconi = 35L, la_thuile = 252L, fluxnet2015 = 212L, current_781 = 781L)
NET_TITLES <- c(current_781 = "Current FLUXNET network (n=781)",
                marconi = "Marconi Conference (n=35)", la_thuile = "La Thuile (n=252)",
                fluxnet2015 = "FLUXNET2015 (n=212)")

# -- KG / Aridity / Biomass: identical file-lookup logic to AXES6 in
#    figure_representativeness_summary.R (duplicated, not sourced) --
site_csv <- function(base, network) {
  suffix <- if (network == "current_781") "" else paste0("_", network)
  file.path(SNAP, paste0("site_", base, suffix, ".csv"))
}

count_sites <- function(df, class_col, levels_vec = NULL, n_total = NULL) {
  n_total <- if (is.null(n_total)) nrow(df) else n_total
  out <- df |>
    dplyr::filter(!is.na(.data[[class_col]])) |>
    dplyr::count(.data[[class_col]], name = "n") |>
    dplyr::rename(class = 1) |>
    dplyr::mutate(class = as.character(class))
  if (!is.null(levels_vec)) {
    out <- data.frame(class = levels_vec, stringsAsFactors = FALSE) |>
      dplyr::left_join(out, by = "class") |>
      dplyr::mutate(n = dplyr::coalesce(n, 0L))
  }
  out |> dplyr::mutate(network_frac = n / n_total)
}

merge_sr <- function(site_counts, global_df) {
  global_df |>
    dplyr::select(class, global_land_fraction) |>
    dplyr::left_join(dplyr::select(site_counts, class, n, network_frac), by = "class") |>
    dplyr::mutate(
      n = dplyr::coalesce(n, 0L), network_frac = dplyr::coalesce(network_frac, 0.0),
      sampling_ratio = dplyr::if_else(global_land_fraction > 0 & network_frac > 0,
                                       network_frac / global_land_fraction, NA_real_),
      log2_sr = dplyr::if_else(!is.na(sampling_ratio), log2(sampling_ratio), NA_real_)
    )
}

compute_repr_metrics <- function(p, q) {
  p[is.na(p)] <- 0; q[is.na(q)] <- 0
  list(weighted_jaccard = sum(pmin(p, q)) / sum(pmax(p, q)),
       hellinger_distance = (1 / sqrt(2)) * sqrt(sum((sqrt(p) - sqrt(q))^2)))
}

kg13_global <- readr::read_csv(file.path(SNAP, "koppen_beck2023_global_distribution.csv"), show_col_types = FALSE) |>
  dplyr::group_by(koppen_twoletter) |> dplyr::summarise(global_land_fraction = sum(global_land_fraction), .groups = "drop") |>
  dplyr::rename(class = koppen_twoletter)
aridity_global <- readr::read_csv(file.path(SNAP, "aridity_unep7_global_distribution.csv"), show_col_types = FALSE) |>
  dplyr::rename(class = unep_class)
bio7_global <- readr::read_csv(file.path(SNAP, "biomass_cci_v7_global_distribution.csv"), show_col_types = FALSE) |>
  dplyr::mutate(class = as.character(biomass_bin))
igbp_global_df <- igbp_global |> dplyr::rename(class = igbp_class)

load_kg <- function(net) {
  kg_base <- if (net == "current_781") "koppen_era5" else "koppen_beck2023"
  readr::read_csv(site_csv(kg_base, net), show_col_types = FALSE) |>
    count_sites("koppen_twoletter")
}
load_aridity <- function(net) {
  readr::read_csv(site_csv("aridity", net), show_col_types = FALSE) |> count_sites("unep_class_7")
}
load_biomass <- function(net) {
  readr::read_csv(site_csv("biomass_cci_v7", net), show_col_types = FALSE) |> count_sites("biomass_bin")
}

# -- IGBP: site metadata `igbp` field, full 17-class scheme, all 4 networks --
IGBP_SITE_FILE <- c(
  current_781 = "fluxnet_shuttle_snapshot_20260901T094522.csv",  # pinned, see SESSION_LOG.md 2026-09-01
  fluxnet2015 = "sites_fluxnet2015_clean.csv",
  la_thuile   = "sites_la_thuile_clean.csv",
  marconi     = "sites_marconi_clean.csv"
)
load_igbp <- function(net) {
  n_total <- NET_NSITES[[net]]
  df <- readr::read_csv(file.path(SNAP, IGBP_SITE_FILE[[net]]), show_col_types = FALSE) |>
    dplyr::distinct(site_id, .keep_all = TRUE) |>
    dplyr::mutate(igbp = dplyr::if_else(igbp %in% IGBP_ORDER, igbp, NA_character_))
  stopifnot(nrow(df) == n_total)
  count_sites(df, "igbp", levels_vec = IGBP_ORDER, n_total = n_total)
}

# -- NEE/ET measured: site_flux_medians_{shuttle,fluxnet2015}.csv only --
FLUX_MEDIANS_FILE <- c(current_781 = "site_flux_medians_shuttle.csv",
                        fluxnet2015 = "site_flux_medians_fluxnet2015.csv")

load_measured_flux <- function(net) {
  if (!net %in% names(FLUX_MEDIANS_FILE)) return(NULL)  # la_thuile/marconi: no data, explicit gap
  readr::read_csv(file.path(SNAP, FLUX_MEDIANS_FILE[[net]]), show_col_types = FALSE) |>
    dplyr::mutate(nee_abs = abs(nep_median))
}

classify_into_bins <- function(x, bin_df) {
  idx <- findInterval(x, bin_df$min_value, all.inside = FALSE)
  idx[idx < 1L] <- NA_integer_
  bin_df$bin[idx]
}

load_measured_axis <- function(net, value_col, bin_df, n_total) {
  flux <- load_measured_flux(net)
  if (is.null(flux)) {
    return(data.frame(class = as.character(bin_df$bin), n = 0L, network_frac = 0,
                       stringsAsFactors = FALSE))  # no data for this network: all-zero, flagged upstream
  }
  bin_assigned <- classify_into_bins(flux[[value_col]], bin_df)
  count_sites(data.frame(bin = as.character(bin_assigned), stringsAsFactors = FALSE),
              "bin", levels_vec = as.character(bin_df$bin), n_total = n_total)
}

# ============================================================================
# 3. FIGURE 1: SAMPLING-RATIO GRID (current_781 only, 7-bin hybrid for NEE/ET)
# ============================================================================

nee7_bins <- readr::read_csv(file.path(SNAP, "trendy_nee_median_global_distribution.csv"), show_col_types = FALSE)
et7_bins  <- readr::read_csv(file.path(SNAP, "trendy_et_median_global_distribution.csv"), show_col_types = FALSE)
nee7_global <- nee7_bins |> dplyr::transmute(class = as.character(bin), global_land_fraction)
et7_global  <- et7_bins  |> dplyr::transmute(class = as.character(bin), global_land_fraction)

n_current <- NET_NSITES[["current_781"]]

TL_ORDER <- c("Af","Am","Aw","BS","BW","Cf","Cs","Cw","Df","Ds","Dw","EF","ET")
ARIDITY_ORDER <- c("Hyper-Arid","Arid","Semi-Arid","Dry Sub-Humid",
                    "Humid (low)","Humid (moderate)","Hyper-Humid")

panel_data_1 <- list(
  kg      = list(title = "Köppen-Geiger (13-class, unchanged)",
                 counts = load_kg("current_781"), global_df = kg13_global,
                 order = TL_ORDER),
  igbp    = list(title = "IGBP (17-class, site metadata vs. ESA-CCI crosswalk)",
                 counts = load_igbp("current_781"), global_df = igbp_global_df,
                 order = IGBP_ORDER),
  aridity = list(title = "CGIAR Aridity Index v3.1 (7-class, unchanged)",
                 counts = load_aridity("current_781"), global_df = aridity_global,
                 order = ARIDITY_ORDER),
  biomass = list(title = "ESA CCI Biomass v7 (7-bin, unchanged)",
                 counts = load_biomass("current_781"), global_df = bio7_global,
                 order = as.character(1:7)),
  nee     = list(title = "NEE, site-measured (NEP magnitude, 7-bin)",
                 counts = load_measured_axis("current_781", "nee_abs", nee7_bins, n_current),
                 global_df = nee7_global, order = as.character(1:7)),
  et      = list(title = "ET, site-measured (7-bin)",
                 counts = load_measured_axis("current_781", "et_median", et7_bins, n_current),
                 global_df = et7_global, order = as.character(1:7))
)

AXES_KEYS_1 <- c("kg", "igbp", "aridity", "biomass", "nee", "et")

LOG2_MAX <- log2(5); LOG2_BREAKS <- c(-LOG2_MAX, -1, 0, 1, LOG2_MAX)
LOG2_LABELS <- c("1/5×","1/2×","1×","2×","5×")
LOG2_XLIM <- c(-LOG2_MAX - 0.25, LOG2_MAX + 0.25)

base_theme <- theme_minimal(base_size = 9) +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.4),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_line(colour = "black"))

make_panel <- function(pd, panel_label, show_xlab) {
  df <- merge_sr(pd$counts, pd$global_df) |>
    dplyr::mutate(
      log2_sr_clip = pmax(pmin(dplyr::coalesce(log2_sr, 0), LOG2_MAX), -LOG2_MAX),
      class = factor(class, levels = pd$order)
    )
  j <- compute_repr_metrics(pd$global_df$global_land_fraction[match(levels(df$class), pd$global_df$class)],
                             df$network_frac[match(levels(df$class), df$class)])$weighted_jaccard
  ggplot(df, aes(x = log2_sr_clip, y = class, fill = class)) +
    geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.5) +
    geom_col(width = 0.72, show.legend = FALSE, colour = "black", linewidth = 0.2) +
    scale_x_continuous(limits = LOG2_XLIM, breaks = LOG2_BREAKS, labels = LOG2_LABELS,
                        name = if (show_xlab) "Sampling ratio" else NULL) +
    scale_y_discrete(name = NULL) +
    annotate("text", x = -Inf, y = Inf, label = panel_label, hjust = -0.3, vjust = 1.5,
             size = 3.5, fontface = "bold") +
    annotate("text", x = Inf, y = Inf, label = sprintf("J = %.3f", j), hjust = 1.2, vjust = 1.5, size = 2.5) +
    labs(title = pd$title) +
    base_theme +
    theme(axis.text.y = element_text(size = 6.5), plot.title = element_text(size = 7.5),
          axis.text.x = if (show_xlab) element_text(size = 7) else element_blank())
}

panels_1 <- lapply(seq_along(AXES_KEYS_1), function(i) {
  make_panel(panel_data_1[[AXES_KEYS_1[i]]], LETTERS[i], AXES_KEYS_1[i] %in% c("biomass","nee","et"))
})
fig1 <- gridExtra::arrangeGrob(grobs = panels_1, nrow = 2, ncol = 3,
  top = grid::textGrob("Supplementary: site-level KG/IGBP/measured-NEE-ET sampling ratios (current_781)",
                        gp = grid::gpar(fontface = "bold", fontsize = 10), x = 0.01, hjust = 0))
out_fig1 <- file.path(OUTD, "Supp_sampling_ratio_siteKG_IGBP_NEE_ET.png")
ggsave(out_fig1, fig1, width = 10, height = 7, dpi = 300, bg = "white")
message("Saved: ", out_fig1)

# CSV of the panel data behind Fig 1
fig1_table <- dplyr::bind_rows(lapply(AXES_KEYS_1, function(k) {
  pd <- panel_data_1[[k]]
  merge_sr(pd$counts, pd$global_df) |> dplyr::mutate(axis = k, .before = 1)
}))
out_fig1_csv <- file.path(OUTD, "table_supp_sampling_ratio_siteKG_IGBP_NEE_ET.csv")
readr::write_csv(fig1_table, out_fig1_csv)
write_output_metadata(out_fig1_csv, input_sources = out_fig1,
                       notes = "Per-class p/q/sampling-ratio data behind Supp_sampling_ratio_siteKG_IGBP_NEE_ET.png.")
message("Saved: ", out_fig1_csv)

# ============================================================================
# 4. FIGURE 2: JACCARD TRAJECTORY (4 networks, 18-bin hybrid for NEE/ET,
#    NEE/ET lines gapped at la_thuile/marconi -- no measured flux data there)
# ============================================================================

nee18_bins <- readr::read_csv(file.path(SNAP, "trendy_nee_median_global_distribution_18bin.csv"), show_col_types = FALSE)
et18_bins  <- readr::read_csv(file.path(SNAP, "trendy_et_median_global_distribution_18bin.csv"), show_col_types = FALSE)
nee18_global <- nee18_bins |> dplyr::transmute(class = as.character(bin), global_land_fraction)
et18_global  <- et18_bins  |> dplyr::transmute(class = as.character(bin), global_land_fraction)

j_for <- function(counts, global_df) {
  df <- merge_sr(counts, global_df)
  compute_repr_metrics(global_df$global_land_fraction[match(df$class, global_df$class)], df$network_frac)$weighted_jaccard
}

traj_rows <- list()
for (net in NET_ORDER) {
  n_tot <- NET_NSITES[[net]]
  traj_rows[[paste(net, "kg")]]      <- data.frame(axis = "KG (13-class, unchanged)", network = net,
                                                     jaccard = j_for(load_kg(net), kg13_global))
  traj_rows[[paste(net, "igbp")]]    <- data.frame(axis = "IGBP (17-class, site metadata)", network = net,
                                                     jaccard = j_for(load_igbp(net), igbp_global_df))
  traj_rows[[paste(net, "aridity")]] <- data.frame(axis = "Aridity (7-class, unchanged)", network = net,
                                                     jaccard = j_for(load_aridity(net), aridity_global))
  traj_rows[[paste(net, "biomass")]] <- data.frame(axis = "Biomass (7-bin, unchanged)", network = net,
                                                     jaccard = j_for(load_biomass(net), bio7_global))
  has_flux <- net %in% names(FLUX_MEDIANS_FILE)
  traj_rows[[paste(net, "nee")]] <- data.frame(axis = "NEE (site-measured NEP magnitude, 18-bin)", network = net,
    jaccard = if (has_flux) j_for(load_measured_axis(net, "nee_abs", nee18_bins, n_tot), nee18_global) else NA_real_)
  traj_rows[[paste(net, "et")]] <- data.frame(axis = "ET (site-measured, 18-bin)", network = net,
    jaccard = if (has_flux) j_for(load_measured_axis(net, "et_median", et18_bins, n_tot), et18_global) else NA_real_)
}
traj_df <- dplyr::bind_rows(traj_rows) |>
  dplyr::mutate(net_x = match(network, NET_ORDER), n_sites = NET_NSITES[network])

TRAJ_COLORS <- c(
  "KG (13-class, unchanged)"           = "#D55E00",
  "IGBP (17-class, site metadata)"     = "#CC79A7",
  "Aridity (7-class, unchanged)"       = "#E69F00",
  "Biomass (7-bin, unchanged)"         = "#009E73",
  "NEE (site-measured NEP magnitude, 18-bin)" = "#0072B2",
  "ET (site-measured, 18-bin)"         = "#56B4E9"
)
traj_df$axis <- factor(traj_df$axis, levels = names(TRAJ_COLORS))

bars_df <- data.frame(net_x = 1:4, y_scaled = as.numeric(NET_NSITES[NET_ORDER]) / max(NET_NSITES))

fig2 <- ggplot() +
  geom_col(data = bars_df, aes(x = net_x, y = y_scaled), fill = "#d9d9d9", colour = "black",
           linewidth = 0.3, alpha = 0.4, width = 0.38, inherit.aes = FALSE) +
  geom_line(data = traj_df, aes(x = net_x, y = jaccard, colour = axis, group = axis), linewidth = 0.7, na.rm = TRUE) +
  geom_point(data = traj_df, aes(x = net_x, y = jaccard, colour = axis, group = axis), size = 2.5, na.rm = TRUE) +
  scale_colour_manual(name = NULL, values = TRAJ_COLORS) +
  scale_x_continuous(breaks = 1:4, labels = c("Marconi\n(n=35)","La Thuile\n(n=252)","FLUXNET2015\n(n=212)","Current\n(n=781)"),
                      limits = c(0.6, 4.4), expand = expansion(mult = 0)) +
  scale_y_continuous(name = "Weighted Jaccard", limits = c(0, 1), breaks = seq(0, 1, 0.25),
                      sec.axis = sec_axis(~ . * max(NET_NSITES), name = "n sites",
                                           breaks = c(0, 200, 400, 600, 800))) +
  labs(title = "Supplementary Jaccard trajectory: site-level KG/IGBP/measured NEE-ET",
       x = NULL,
       caption = "NEE/ET lines connect only current_781 and FLUXNET2015 -- no downloaded flux time series exist for La Thuile/Marconi in this repo (explicit gap, not interpolated).") +
  base_theme +
  theme(legend.position = c(0.02, 0.98), legend.justification = c(0, 1),
        legend.background = element_rect(fill = grDevices::adjustcolor("white", 0.85), colour = "black", linewidth = 0.2),
        legend.text = element_text(size = 7), plot.caption = element_text(size = 6.5, hjust = 0, colour = "grey30"))

out_fig2 <- file.path(OUTD, "Supp_jaccard_trajectory_siteKG_IGBP_NEE_ET.png")
ggsave(out_fig2, fig2, width = 7, height = 5, dpi = 300, bg = "white")
message("Saved: ", out_fig2)

out_fig2_csv <- file.path(OUTD, "table_supp_jaccard_trajectory_siteKG_IGBP_NEE_ET.csv")
readr::write_csv(traj_df, out_fig2_csv)
write_output_metadata(out_fig2_csv, input_sources = out_fig2,
                       notes = "Per-axis/per-network weighted Jaccard behind Supp_jaccard_trajectory_siteKG_IGBP_NEE_ET.png. jaccard is NA for NEE/ET at la_thuile/marconi (no measured flux data).")
message("Saved: ", out_fig2_csv)

# ============================================================================
# 5. CONSOLE SUMMARY (site counts / QC dilution, mirrors figure_representativeness_kg.R)
# ============================================================================

cat("\n================ Site-level classification coverage (current_781) ================\n")
for (k in AXES_KEYS_1) {
  cnt <- panel_data_1[[k]]$counts
  n_classified <- sum(cnt$n)
  cat(sprintf("  %-8s classified: %d / %d\n", k, n_classified, n_current))
}

message("\n=== figure_representativeness_supp_sitelevel.R complete ===")
