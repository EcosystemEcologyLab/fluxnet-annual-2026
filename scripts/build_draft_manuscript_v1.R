## scripts/build_draft_manuscript_v1.R
## Assembles the six draft-manuscript candidate figures (rebuilt at
## consistent journal-ready specifications — see SESSION_LOG.md 2026-06-30)
## into review/figures/draft_manuscript_v1/ under descriptive filenames,
## alongside a matching .legend.txt for each.
##
## This is a pure file-copy utility (no flux data read, no credentials) —
## it intentionally does not call check_pipeline_config(). Source figures
## must already exist; run the relevant generate_*.R / figure_*.R script
## first if a source is missing or stale.
##
## Fig 1B note: fig_dur11_CumulativeSiteYears_IGBP.png has no script that
## writes it directly under a fig_01b_* name — scripts/generate_duration_
## histograms.R writes it under its canonical Dur11 name in
## review/figures/network/, and this script performs the copy/rename step
## explicitly (as a committed, reusable script) rather than via an
## undocumented one-off shell command, addressing the "missing direct
## source" gap noted in the 2026-06-30 figure audit.
##
## Fig 2 note: as of 2026-07-02, Figure 2's source is
## review/figures/whittaker/fig_whit_fig2_with_both_contours.png — the
## network hexbin (fig_whittaker_worldclim(), unmodified) with the 95%
## solid / 99% dashed global ice-free-land HDR contours overlaid (no
## in-panel contour label; see scripts/generate_whittaker_overlays.R). This
## replaces the earlier no-contour fig_05_whittaker_current.png source. Its
## legend is NOT copied from the source figure's own .legend.txt (that file
## carries the retired Mahalanobis coverage statistic, which must not appear
## in the manuscript legend) — it is instead hardcoded below so the swap
## reproduces deterministically without that statistic.

library(fs)

out_dir <- file.path("review", "figures", "draft_manuscript_v1")
fs::dir_create(out_dir)

# ---- Figure copy map ---------------------------------------------------------
# source PNG -> descriptive draft-manuscript filename
figs <- list(
  list(src = "review/figures/candidates/fig_03_map_current.png",
       dst = "fig_01a_map_current_network.png"),
  list(src = "review/figures/network/fig_dur11_CumulativeSiteYears_IGBP.png",
       dst = "fig_01b_cumulative_siteyears_igbp.png"),
  list(src = "review/figures/whittaker/fig_whit_fig2_with_both_contours.png",
       dst = "fig_02_whittaker_current.png"),
  list(src = "review/figures/flux_medians/fig_flux_comparison_combo_nep_et_h.png",
       dst = "fig_03_flux_comparison_combo_nep_et_h.png"),
  list(src = "review/figures/representativeness/fig_rep001_current.png",
       dst = "fig_04_current_network_sampling_ratios.png"),
  list(src = "review/figures/representativeness/fig_rep008_jaccard_trajectory_with_counts.png",
       dst = "fig_05_jaccard_trajectory_with_counts.png")
)

for (f in figs) {
  if (!file.exists(f$src)) {
    stop("Source figure not found: ", f$src,
         " — run its generate_*.R / figure_*.R script first.")
  }
  dst_path <- file.path(out_dir, f$dst)
  file.copy(f$src, dst_path, overwrite = TRUE)
  message("Copied: ", f$src, " -> ", dst_path)
}

# ---- Legend copy map ---------------------------------------------------------
# source .legend.txt -> descriptive draft-manuscript legend filename
# Fig 4/5 legends already existed (Rep008/Rep001) and are copied verbatim
# except for a corrected DIMENSIONS line reflecting this session's resize.
# Fig 4 and Fig 5 were swapped 2026-07-01 to match the manuscript section
# order (sampling ratios now precedes the Jaccard trajectory) — see
# SESSION_LOG.md 2026-07-01.
# Fig 2's legend is NOT in this copy map — see the "Fig 2 note" above and the
# hardcoded block below.
legends <- list(
  list(src = "review/figures/candidates/fig_03_map_current.legend.txt",
       dst = "fig_01a_map_current_network.legend.txt",
       fix_dims = NULL),
  list(src = "review/figures/network/fig_dur11_CumulativeSiteYears_IGBP.legend.txt",
       dst = "fig_01b_cumulative_siteyears_igbp.legend.txt",
       fix_dims = NULL),
  list(src = "review/figures/flux_medians/fig_flux_comparison_combo_nep_et_h.legend.txt",
       dst = "fig_03_flux_comparison_combo_nep_et_h.legend.txt",
       fix_dims = NULL),
  list(src = "review/figures/representativeness/fig_rep001_current.legend.txt",
       dst = "fig_04_current_network_sampling_ratios.legend.txt",
       fix_dims = "DIMENSIONS:    7 x 5 inches, 300 dpi"),
  list(src = "review/figures/representativeness/fig_rep008_jaccard_trajectory_with_counts.legend.txt",
       dst = "fig_05_jaccard_trajectory_with_counts.legend.txt",
       fix_dims = "DIMENSIONS:    3.5 x 4 inches, 300 dpi")
)

for (l in legends) {
  if (!file.exists(l$src)) {
    stop("Source legend not found: ", l$src)
  }
  dst_path <- file.path(out_dir, l$dst)
  txt <- readLines(l$src)
  if (!is.null(l$fix_dims)) {
    txt <- sub("^DIMENSIONS:.*$", l$fix_dims, txt)
  }
  writeLines(txt, dst_path)
  message("Copied: ", l$src, " -> ", dst_path)
}

# ---- Fig 2 legend: hardcoded (base layer + contour overlay, no coverage stat) ----
# fig_whit_fig2_with_both_contours.legend.txt (the source figure's own legend,
# written by scripts/generate_whittaker_overlays.R) carries the retired
# Mahalanobis coverage statistic and is NOT copied here. This block writes the
# manuscript legend directly so it reproduces deterministically without that
# statistic; keep this text in sync by hand if the base layer or overlay
# method changes materially.
fig02_legend <- c(
  "FIGURE LEGEND — fig_02_whittaker_current.png",
  "==============================================",
  "",
  "TITLE: Whittaker climate-space distribution of the current FLUXNET network,",
  "coloured by median annual net ecosystem exchange, with global ice-free-land",
  "climate-space contours overlaid",
  "",
  "DESCRIPTION:",
  "Two layers on one panel.",
  "",
  "BASE LAYER: hexagonal-binned scatter plot placing every current FLUXNET",
  "Shuttle site (N = 767 sites) in Whittaker climate space — mean annual",
  "temperature (MAT, WorldClim v2.1 BIO1) by mean annual precipitation (MAP,",
  "BIO12). Each hexagonal bin is coloured by the median annual net ecosystem",
  "exchange (NEE_VUT_REF, with a coalesce fallback to NEE_CUT_REF for",
  "CUT-only sites) observed across all site-years falling within that",
  "climate-space bin, on a diverging blue-white-red scale centred on",
  "NEE = 0 (white); bins with no observed flux data are left uncoloured (NA,",
  "not drawn). Individual site locations are additionally overplotted as",
  "semi-transparent grey points so that sampling density within each hexagon",
  "is visible alongside the flux colour. An inset reports the site count and",
  "site-year count (N sites | site-years). This base layer is built by the",
  "unmodified fig_whittaker_worldclim() and is otherwise identical to the",
  "no-contour version of this figure used in earlier drafts.",
  "",
  "OVERLAY: two contour lines mark the 95% (solid) and 99% (dashed)",
  "highest-density regions (HDR) of the area-weighted distribution of global",
  "ice-free land in the same MAT/MAP space — i.e. where global land area is",
  "concentrated in climate space, independent of the FLUXNET network. The",
  "density surface is computed from WorldClim v2.1 BIO1/BIO12 over an ESA",
  "CCI land-cover 2015 ice-free-land mask (water bodies and permanent",
  "snow/ice excluded, plus a latitude backstop for Antarctica), area-weighted",
  "by the cosine of latitude (WorldClim is an equal-angle, not equal-area,",
  "grid), via a weighted 2D kernel density estimate over the full, unclipped",
  "global distribution. The 95% contour marks the climates where most global",
  "land area sits; the 99% contour extends further out to include rarer",
  "climatic extremes. The two lines are distinguished by line type only",
  "(both black) and are identified in this legend rather than by an",
  "in-panel label or key — no contour text or legend box is drawn on the",
  "figure itself.",
  "",
  "A small fraction of global land area (1.18%) falls in climates outside",
  "the displayed axis ranges (MAT below -15 or above 35 degC; MAP above",
  "4000 mm/yr) and is not shown, on either layer.",
  "",
  "AXES:",
  "  X — Mean Annual Temperature (°C), fixed range -15 to 35",
  "  Y — Mean Annual Precipitation (mm yr⁻¹), fixed range 0 to 4000",
  "",
  "Four-sided black tick marks (inward) are drawn on all axes via a",
  "duplicated secondary axis with no labels. Panel has a solid black border",
  "and no gridlines.",
  "",
  "COLOUR SCALE (base layer only — the overlay has no fill/colour):",
  "Diverging blue–white–red scale (colorspace \"Blue-Red 3\"), centred on",
  "NEE = 0 (white), blue = net carbon uptake (negative NEE), red = net carbon",
  "release (positive NEE). Colour limits are computed from the 5th–95th",
  "percentile of the full, unfiltered annual Shuttle NEE distribution.",
  "",
  "BINNING (base layer):",
  "Hexagonal binning via ggplot2::stat_summary_hex(), 15 bins along each",
  "axis; summary statistic per bin is the median of per-site median annual",
  "NEE values (median-of-medians).",
  "",
  "NETWORK AND SITE COUNT:",
  "FLUXNET Shuttle network, current development-mode snapshot: N = 767",
  "sites; inset text on the figure reports N = 767 sites | 4,236 site-years.",
  "",
  "DATA SOURCES:",
  "  - Site list: data/snapshots/fluxnet_shuttle_snapshot_20260624T095651.csv",
  "    (767 sites)",
  "  - Climate (MAT/MAP), both layers: WorldClim v2.1, 2.5 arc-minute BIO1",
  "    and BIO12; base layer uses the pre-extracted per-site table",
  "    data/snapshots/site_worldclim.csv (fallback: on-the-fly",
  "    terra::extract()); overlay uses the full global BIO1/BIO12 GeoTIFFs",
  "  - Land mask (overlay only): ESA CCI land cover 2015",
  "    (ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif)",
  "  - Flux data (base layer only): annual_converted table in the project's",
  "    DuckDB database (data/duckdb/fluxnet.duckdb), FLUXMET dataset, after",
  "    QC filtering (04_qc.R) and unit conversion (05_units.R)",
  "",
  "EXCLUSIONS (base layer):",
  "Sites lacking a WorldClim climate match, or with no finite annual NEE",
  "value after the VUT/CUT coalesce, are dropped from both the hexbin",
  "summary and the overlaid points. No IGBP or other classification filter",
  "is applied.",
  "",
  "REPRODUCIBILITY:",
  "Base layer function: fig_whittaker_worldclim() in R/figures/fig_climate.R",
  "Overlay functions:   fig_whittaker_global_contour(),",
  "                     build_global_landclimate(), .weighted_density_grid(),",
  "                     .hdr_levels() in R/figures/fig_climate.R",
  "Source script:       scripts/generate_whittaker_overlays.R",
  "Output:              review/figures/whittaker/fig_whit_fig2_with_both_contours.png,",
  "                     copied to",
  "                     review/figures/draft_manuscript_v1/fig_02_whittaker_current.png",
  "                     by scripts/build_draft_manuscript_v1.R",
  "DIMENSIONS: 3.5 × 3.5 inches, 300 dpi, white background"
)
fig02_legend_path <- file.path(out_dir, "fig_02_whittaker_current.legend.txt")
writeLines(fig02_legend, fig02_legend_path)
message("Wrote: ", fig02_legend_path, " (hardcoded, not copied)")

message("\nDone. draft_manuscript_v1/ contains ", length(figs), " figures and ",
        length(legends) + 1, " legends.")
