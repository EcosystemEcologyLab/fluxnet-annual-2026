## figure_representativeness_supp_compare_grid.R
##
## Candidate figure: side-by-side comparison of the geospatial-axis
## sampling ratios (fig_rep001_current.png / draft Fig 4) against the
## site-level-axis sampling ratios (Supp_sampling_ratio_siteKG_IGBP_NEE_ET.png)
## for the current FLUXNET network only.
##
## LEFT column: the 6 fig_rep001_current axes (KG, ESA CCI land cover,
## Aridity, Biomass, TRENDY NEE-IAV, TRENDY ET-median), computed here from
## the SAME stored site-level + global-distribution CSVs
## figure_representativeness_summary.R's AXES6 reads for current_781 (7-bin
## hybrid for the two continuous TRENDY axes, matching fig_rep001's own
## resolution) -- read as-is, not re-extracted or re-derived.
## RIGHT column: the 6 axes from Supp_sampling_ratio_siteKG_IGBP_NEE_ET.png
## (KG unchanged, IGBP, Aridity unchanged, Biomass unchanged, site-measured
## NEE, site-measured ET), read directly from
## table_supp_sampling_ratio_siteKG_IGBP_NEE_ET.csv (this script's own
## sibling output from a prior session) -- not recomputed from raw site
## data a second time.
##
## Rows 1 (KG), 3 (Aridity), 4 (Biomass) use identical underlying data in
## both columns (both scripts read the same site+global CSVs for these
## three axes) -- computed once here and rendered in both columns,
## labelled "(unchanged, identical both columns)".
##
## This script does NOT modify figure_representativeness_summary.R,
## figure_representativeness_kg.R, figure_representativeness_supp_sitelevel.R,
## any data/snapshots/*.csv, or representativeness_metrics.csv. It only
## READS existing files and WRITES new files under review/figures/candidates/.
##
## VERIFICATION (reported, not silently corrected):
##   Left column J values are compared against the stored
##   data/snapshots/representativeness_metrics.csv rows for
##   (axis, aggregation_level, current_781) -- the same lookup
##   figure_representativeness_summary.R's get_j() performs.
##   Right column J values are compared against the values already
##   published in this repo's own
##   review/figures/candidates/Supp_sampling_ratio_siteKG_IGBP_NEE_ET.legend.txt.
##   Any mismatch beyond floating-point tolerance is printed to the console
##   and written to table_compare_geospatial_vs_sitelevel_verification.csv;
##   nothing is adjusted to force agreement.
##
## Outputs (review/figures/candidates/):
##   Supp_compare_geospatial_vs_sitelevel_grid.png
##   Supp_compare_geospatial_vs_sitelevel_grid.legend.txt
##   table_compare_geospatial_vs_sitelevel_grid.csv (+ .meta.json)
##   table_compare_geospatial_vs_sitelevel_verification.csv (+ .meta.json)

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

message("=== figure_representativeness_supp_compare_grid.R ===")

N_TOTAL <- 781L

# ============================================================================
# 1. SHARED HELPERS (duplicated from figure_representativeness_summary.R /
#    figure_representativeness_supp_sitelevel.R, not sourced, per this
#    repo's established convention of independent per-script duplication)
# ============================================================================

count_sites <- function(df, class_col, levels_vec = NULL, n_total = N_TOTAL) {
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

compute_j <- function(df) {
  p <- df$global_land_fraction; q <- df$network_frac
  p[is.na(p)] <- 0; q[is.na(q)] <- 0
  sum(pmin(p, q)) / sum(pmax(p, q))
}

TL_ORDER      <- c("Af","Am","Aw","BS","BW","Cf","Cs","Cw","Df","Ds","Dw","EF","ET")
ARIDITY_ORDER <- c("Hyper-Arid","Arid","Semi-Arid","Dry Sub-Humid",
                    "Humid (low)","Humid (moderate)","Hyper-Humid")
IGBP_ORDER    <- c("ENF","EBF","DNF","DBF","MF","CSH","OSH","WSA","SAV","GRA",
                   "WET","CRO","URB","CVM","SNO","BSV","WAT")

# ============================================================================
# 2. LEFT COLUMN: the 6 fig_rep001_current axes, from the same stored CSVs
#    AXES6 in figure_representativeness_summary.R reads for current_781
# ============================================================================

left_kg <- {
  s <- readr::read_csv(file.path(SNAP, "site_koppen_era5.csv"), show_col_types = FALSE) |>
    count_sites("koppen_twoletter", TL_ORDER)
  g <- readr::read_csv(file.path(SNAP, "koppen_beck2023_global_distribution.csv"), show_col_types = FALSE) |>
    dplyr::group_by(koppen_twoletter) |> dplyr::summarise(global_land_fraction = sum(global_land_fraction), .groups = "drop") |>
    dplyr::rename(class = koppen_twoletter)
  merge_sr(s, g) |> dplyr::mutate(order = match(class, TL_ORDER))
}

left_lulc <- {
  s <- readr::read_csv(file.path(SNAP, "site_landcover_cci.csv"), show_col_types = FALSE) |>
    dplyr::mutate(class = as.character(cci_high_level_class)) |>
    count_sites("class", as.character(1:10))
  g_raw <- readr::read_csv(file.path(SNAP, "landcover_cci_highlevel_global_distribution.csv"), show_col_types = FALSE)
  g <- g_raw |> dplyr::transmute(class = as.character(cci_high_level_class), global_land_fraction)
  labels <- setNames(g_raw$cci_high_level_class_name, as.character(g_raw$cci_high_level_class))
  merge_sr(s, g) |> dplyr::mutate(order = as.integer(class), label = labels[class])
}

left_aridity <- {
  s <- readr::read_csv(file.path(SNAP, "site_aridity.csv"), show_col_types = FALSE) |>
    count_sites("unep_class_7", ARIDITY_ORDER)
  g <- readr::read_csv(file.path(SNAP, "aridity_unep7_global_distribution.csv"), show_col_types = FALSE) |>
    dplyr::rename(class = unep_class)
  merge_sr(s, g) |> dplyr::mutate(order = match(class, ARIDITY_ORDER))
}

left_biomass <- {
  s <- readr::read_csv(file.path(SNAP, "site_biomass_cci_v7.csv"), show_col_types = FALSE) |>
    dplyr::mutate(class = as.character(biomass_bin)) |>
    count_sites("class", as.character(1:7))
  g <- readr::read_csv(file.path(SNAP, "biomass_cci_v7_global_distribution.csv"), show_col_types = FALSE) |>
    dplyr::mutate(class = as.character(biomass_bin))
  merge_sr(s, g) |> dplyr::mutate(order = as.integer(class))
}

left_nee <- {
  s <- readr::read_csv(file.path(SNAP, "site_trendy_nee_iav.csv"), show_col_types = FALSE) |>
    dplyr::mutate(class = as.character(trendy_nee_iav_bin)) |>
    count_sites("class", as.character(1:7))
  g <- readr::read_csv(file.path(SNAP, "trendy_nee_iav_global_distribution.csv"), show_col_types = FALSE) |>
    dplyr::mutate(class = as.character(bin))
  merge_sr(s, g) |> dplyr::mutate(order = as.integer(class))
}

left_et <- {
  s <- readr::read_csv(file.path(SNAP, "site_trendy_et_median.csv"), show_col_types = FALSE) |>
    dplyr::mutate(class = as.character(trendy_et_median_bin)) |>
    count_sites("class", as.character(1:7))
  g <- readr::read_csv(file.path(SNAP, "trendy_et_median_global_distribution.csv"), show_col_types = FALSE) |>
    dplyr::mutate(class = as.character(bin))
  merge_sr(s, g) |> dplyr::mutate(order = as.integer(class))
}

# ============================================================================
# 3. RIGHT COLUMN: read directly from the existing supplementary table
# ============================================================================

supp_table <- readr::read_csv(
  file.path(OUTD, "table_supp_sampling_ratio_siteKG_IGBP_NEE_ET.csv"), show_col_types = FALSE
)

right_from_supp <- function(ax, order_vec) {
  supp_table |> dplyr::filter(axis == ax) |>
    dplyr::select(class, global_land_fraction, n, network_frac, sampling_ratio, log2_sr) |>
    dplyr::mutate(order = match(class, order_vec))
}

right_kg      <- right_from_supp("kg", TL_ORDER)
right_igbp    <- right_from_supp("igbp", IGBP_ORDER)
right_aridity <- right_from_supp("aridity", ARIDITY_ORDER)
right_biomass <- right_from_supp("biomass", as.character(1:7))
right_nee     <- right_from_supp("nee", as.character(1:7))
right_et      <- right_from_supp("et", as.character(1:7))

# ============================================================================
# 4. VERIFICATION (report only -- do not adjust anything to force agreement)
# ============================================================================

metrics_df <- readr::read_csv(file.path(SNAP, "representativeness_metrics.csv"), show_col_types = FALSE)
get_stored_j <- function(axis, agg, net = "current_781") {
  v <- metrics_df |> dplyr::filter(.data$axis == !!axis, aggregation_level == agg, network == net) |>
    dplyr::pull(weighted_jaccard)
  if (length(v) == 0L) NA_real_ else v[[1L]]
}

# Published right-column J values, from this repo's own
# Supp_sampling_ratio_siteKG_IGBP_NEE_ET.legend.txt (written in a prior session)
published_right_j <- c(kg = 0.420, igbp = 0.394, aridity = 0.666, biomass = 0.636, nee = 0.233, et = 0.489)

verification <- dplyr::bind_rows(
  data.frame(column = "left", axis = "kg",      computed_j = compute_j(left_kg),
             reference_j = get_stored_j("koppen_beck2023", "13class_twoletter"),
             reference_source = "representativeness_metrics.csv"),
  data.frame(column = "left", axis = "lulc",    computed_j = compute_j(left_lulc),
             reference_j = get_stored_j("landcover_cci", "high_level"),
             reference_source = "representativeness_metrics.csv"),
  data.frame(column = "left", axis = "aridity", computed_j = compute_j(left_aridity),
             reference_j = get_stored_j("aridity_unep7", "unep7"),
             reference_source = "representativeness_metrics.csv"),
  data.frame(column = "left", axis = "biomass", computed_j = compute_j(left_biomass),
             reference_j = get_stored_j("biomass_cci_v7", "7bin_hybrid"),
             reference_source = "representativeness_metrics.csv"),
  data.frame(column = "left", axis = "nee",     computed_j = compute_j(left_nee),
             reference_j = get_stored_j("trendy_nee_iav", "7bin_hybrid"),
             reference_source = "representativeness_metrics.csv"),
  data.frame(column = "left", axis = "et",      computed_j = compute_j(left_et),
             reference_j = get_stored_j("trendy_et_median", "7bin_hybrid"),
             reference_source = "representativeness_metrics.csv"),
  data.frame(column = "right", axis = "kg",      computed_j = compute_j(right_kg),      reference_j = published_right_j[["kg"]],      reference_source = "Supp_sampling_ratio_siteKG_IGBP_NEE_ET.legend.txt"),
  data.frame(column = "right", axis = "igbp",    computed_j = compute_j(right_igbp),    reference_j = published_right_j[["igbp"]],    reference_source = "Supp_sampling_ratio_siteKG_IGBP_NEE_ET.legend.txt"),
  data.frame(column = "right", axis = "aridity", computed_j = compute_j(right_aridity), reference_j = published_right_j[["aridity"]], reference_source = "Supp_sampling_ratio_siteKG_IGBP_NEE_ET.legend.txt"),
  data.frame(column = "right", axis = "biomass", computed_j = compute_j(right_biomass), reference_j = published_right_j[["biomass"]], reference_source = "Supp_sampling_ratio_siteKG_IGBP_NEE_ET.legend.txt"),
  data.frame(column = "right", axis = "nee",     computed_j = compute_j(right_nee),     reference_j = published_right_j[["nee"]],     reference_source = "Supp_sampling_ratio_siteKG_IGBP_NEE_ET.legend.txt"),
  data.frame(column = "right", axis = "et",      computed_j = compute_j(right_et),      reference_j = published_right_j[["et"]],      reference_source = "Supp_sampling_ratio_siteKG_IGBP_NEE_ET.legend.txt")
) |>
  dplyr::mutate(
    delta = computed_j - reference_j,
    # published_right_j is rounded to 3dp in the legend text; left-column
    # reference is full-precision from the stored metrics CSV
    tolerance = dplyr::if_else(column == "right", 5e-4, 1e-6),
    mismatch = abs(delta) > tolerance
  )

cat("\n================ VERIFICATION: computed vs. stored/published J ================\n")
print(as.data.frame(verification[, c("column","axis","computed_j","reference_j","delta","mismatch")]))
n_mismatch <- sum(verification$mismatch)
if (n_mismatch > 0L) {
  cat("\n*** ", n_mismatch, " MISMATCH(ES) FOUND -- reported as-is, not corrected: ***\n")
  print(as.data.frame(verification[verification$mismatch, ]))
} else {
  cat("\nNo mismatches beyond tolerance.\n")
}

out_verify <- file.path(OUTD, "table_compare_geospatial_vs_sitelevel_verification.csv")
readr::write_csv(verification, out_verify)
write_output_metadata(
  out_verify,
  input_sources = c(file.path(SNAP, "representativeness_metrics.csv"),
                     file.path(OUTD, "Supp_sampling_ratio_siteKG_IGBP_NEE_ET.legend.txt"),
                     file.path(OUTD, "table_supp_sampling_ratio_siteKG_IGBP_NEE_ET.csv")),
  notes = "Left column J recomputed here from the same stored site+global CSVs figure_representativeness_summary.R's AXES6 reads, checked against representativeness_metrics.csv. Right column J recomputed from table_supp_sampling_ratio_siteKG_IGBP_NEE_ET.csv, checked against the published Supp_sampling_ratio_siteKG_IGBP_NEE_ET.legend.txt values. Mismatches reported, not corrected."
)
message("Saved: ", out_verify)

# ============================================================================
# 5. PANELS
# ============================================================================

LOG2_MAX <- log2(5); LOG2_BREAKS <- c(-LOG2_MAX, -1, 0, 1, LOG2_MAX)
LOG2_LABELS <- c("1/5×","1/2×","1×","2×","5×")
LOG2_XLIM <- c(-LOG2_MAX - 0.25, LOG2_MAX + 0.25)

base_theme <- theme_minimal(base_size = 9) +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.4),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_line(colour = "black"))

make_panel <- function(df, title, panel_label, show_xlab, n_classified = NULL,
                        class_labels = NULL) {
  df <- df |> dplyr::mutate(
    log2_sr_clip = pmax(pmin(dplyr::coalesce(log2_sr, 0), LOG2_MAX), -LOG2_MAX),
    class = factor(class, levels = class[order(order)])
  )
  if (!is.null(class_labels)) {
    levels(df$class) <- class_labels[levels(df$class)]
  }
  j <- compute_j(df)
  n_tot <- sum(df$n)
  p <- ggplot(df, aes(x = log2_sr_clip, y = class, fill = class)) +
    geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.5) +
    geom_col(width = 0.72, show.legend = FALSE, colour = "black", linewidth = 0.2) +
    scale_x_continuous(limits = LOG2_XLIM, breaks = LOG2_BREAKS, labels = LOG2_LABELS,
                        name = if (show_xlab) "Sampling ratio" else NULL) +
    scale_y_discrete(name = NULL) +
    annotate("text", x = -Inf, y = Inf, label = panel_label, hjust = -0.3, vjust = 1.4,
             size = 3.2, fontface = "bold") +
    annotate("text", x = Inf, y = Inf,
             label = if (n_tot < N_TOTAL) sprintf("J = %.3f\nn = %d", j, n_tot) else sprintf("J = %.3f", j),
             hjust = 1.05, vjust = 1.3, size = 2.3, lineheight = 0.95) +
    labs(title = title) +
    base_theme +
    theme(axis.text.y = element_text(size = 6.2), plot.title = element_text(size = 7.5),
          axis.text.x = if (show_xlab) element_text(size = 7) else element_blank())
  p
}

lulc_labels <- setNames(
  readr::read_csv(file.path(SNAP, "landcover_cci_highlevel_global_distribution.csv"), show_col_types = FALSE)$cci_high_level_class_name,
  as.character(readr::read_csv(file.path(SNAP, "landcover_cci_highlevel_global_distribution.csv"), show_col_types = FALSE)$cci_high_level_class)
)

ROW_TITLES_LEFT  <- c("Köppen-Geiger (13-class) -- unchanged, identical both columns",
                      "ESA CCI Land Cover (10-class, high-level)",
                      "CGIAR Aridity Index v3.1 -- unchanged, identical both columns",
                      "ESA CCI Biomass v7 (7-bin) -- unchanged, identical both columns",
                      "TRENDY v14 NEE-IAV (7-bin, at site coordinates)",
                      "TRENDY v14 ET-median (7-bin, at site coordinates)")
ROW_TITLES_RIGHT <- c("Köppen-Geiger (13-class) -- unchanged, identical both columns",
                      "IGBP (17-class, site metadata)",
                      "CGIAR Aridity Index v3.1 -- unchanged, identical both columns",
                      "ESA CCI Biomass v7 (7-bin) -- unchanged, identical both columns",
                      "NEE, site-measured (NEP magnitude, 7-bin)",
                      "ET, site-measured (7-bin)")

panels <- list(
  left_kg      = make_panel(left_kg,      ROW_TITLES_LEFT[1],  "A", FALSE),
  right_kg     = make_panel(right_kg,     ROW_TITLES_RIGHT[1], "A'", FALSE),
  left_lulc    = make_panel(left_lulc,    ROW_TITLES_LEFT[2],  "B", FALSE, class_labels = lulc_labels),
  right_igbp   = make_panel(right_igbp,   ROW_TITLES_RIGHT[2], "B'", FALSE),
  left_aridity = make_panel(left_aridity, ROW_TITLES_LEFT[3],  "C", FALSE),
  right_aridity= make_panel(right_aridity,ROW_TITLES_RIGHT[3], "C'", FALSE),
  left_biomass = make_panel(left_biomass, ROW_TITLES_LEFT[4],  "D", FALSE),
  right_biomass= make_panel(right_biomass,ROW_TITLES_RIGHT[4], "D'", FALSE),
  left_nee     = make_panel(left_nee,     ROW_TITLES_LEFT[5],  "E", FALSE),
  right_nee    = make_panel(right_nee,    ROW_TITLES_RIGHT[5], "E'", FALSE),
  left_et      = make_panel(left_et,      ROW_TITLES_LEFT[6],  "F", TRUE),
  right_et     = make_panel(right_et,     ROW_TITLES_RIGHT[6], "F'", TRUE)
)

# Arrange in reading order left-to-right, top-to-bottom (6 rows x 2 cols)
grob_order <- c("left_kg","right_kg","left_lulc","right_igbp","left_aridity","right_aridity",
                 "left_biomass","right_biomass","left_nee","right_nee","left_et","right_et")
grobs <- lapply(panels[grob_order], ggplotGrob)

title_grob <- grid::textGrob(
  "Supplementary: geospatial vs. site-level representativeness axes (current_781)",
  gp = grid::gpar(fontface = "bold", fontsize = 12), x = 0.01, hjust = 0
)

col_headers <- gridExtra::arrangeGrob(
  grid::textGrob("GEOSPATIAL\n(fig_rep001_current axes, from AXES6's stored site+global CSVs)",
                 gp = grid::gpar(fontface = "bold", fontsize = 9, lineheight = 1.2)),
  grid::textGrob("SITE-LEVEL\n(Supp_sampling_ratio_siteKG_IGBP_NEE_ET axes, from its stored table)",
                 gp = grid::gpar(fontface = "bold", fontsize = 9, lineheight = 1.2)),
  ncol = 2
)

grid_body <- gridExtra::arrangeGrob(grobs = grobs, nrow = 6, ncol = 2)
fig <- gridExtra::arrangeGrob(
  title_grob, col_headers, grid_body, nrow = 3,
  heights = grid::unit(c(0.7, 1.1, 1), c("cm", "cm", "null"))
)

out_png <- file.path(OUTD, "Supp_compare_geospatial_vs_sitelevel_grid.png")
ggsave(out_png, fig, width = 9, height = 16.3, dpi = 300, bg = "white")
message("Saved: ", out_png)

# ============================================================================
# 6. COMPANION CSV
# ============================================================================

tag_rows <- function(df, column, axis) df |> dplyr::mutate(column = column, axis = axis, .before = 1)
grid_table <- dplyr::bind_rows(
  tag_rows(left_kg, "left", "kg"),           tag_rows(right_kg, "right", "kg"),
  tag_rows(left_lulc, "left", "lulc"),       tag_rows(right_igbp, "right", "igbp"),
  tag_rows(left_aridity, "left", "aridity"), tag_rows(right_aridity, "right", "aridity"),
  tag_rows(left_biomass, "left", "biomass"), tag_rows(right_biomass, "right", "biomass"),
  tag_rows(left_nee, "left", "nee"),         tag_rows(right_nee, "right", "nee"),
  tag_rows(left_et, "left", "et"),           tag_rows(right_et, "right", "et")
) |>
  dplyr::select(column, axis, class, order, global_land_fraction, n, network_frac, sampling_ratio, log2_sr)

out_csv <- file.path(OUTD, "table_compare_geospatial_vs_sitelevel_grid.csv")
readr::write_csv(grid_table, out_csv)
write_output_metadata(
  out_csv, input_sources = out_png,
  notes = "Per-class p/q/sampling-ratio data behind Supp_compare_geospatial_vs_sitelevel_grid.png, left (geospatial) and right (site-level) columns, all 6 rows."
)
message("Saved: ", out_csv)

message("\n=== figure_representativeness_supp_compare_grid.R complete ===")
