## generate_fig_cumulative_siteyears.R
## Figure 2: cumulative site-years by IGBP class, with a framed record-length
## inset (rebuilt 2026-10-12 -- logs/prompts/2026-10-12_renumber_figures.md).
##
## Rebuild history:
##  - Figure stage 2 (2026-10-02): promoted to its own main-text figure,
##    single panel, no inset, 89mm, pinned snapshot 20260901T094522.
##  - 2026-10-12 renumber: rebuilt to match Dave's edited mockup
##    (review/figures/candidates/fig_02alt_mockup_dave_02.png) -- a framed
##    inset (the S6-style record-length histogram, via
##    fig_record_length_histogram(), R/figures/fig_network_growth.R) in the
##    upper left of the main panel, with a single-column IGBP legend inside
##    the inset's own frame. The three historical-release dashed lines
##    (2000/2007/2015) are removed from the main panel; the Marconi line and
##    label are recoloured dark grey (#404040) so La Thuile (red) and
##    FLUXNET2015 (blue) read unambiguously without them. 120x120mm (not the
##    89mm save_nature_figure() ceiling -- see save_candidate() below).
##  - Snapshot: moved from the old pinned fluxnet_shuttle_snapshot_
##    20260901T094522.csv to the latest snapshot
##    (fluxnet_shuttle_snapshot_20260920T102211.csv), per the "latest
##    snapshot is the default" decision (docs/decisions_pending.md
##    2026-10-12) -- this script had no documented pin reason of its own
##    (unlike scripts/supp_stage2_record_length_collections_figure.R, which
##    stays pinned for S6/S1 stability). Both snapshots carry the same 781
##    sites; gates below confirm the switch changes nothing material.
##
## Output: review/figures/draft_manuscript_v1/fig_02_cumulative_siteyears_igbp.png/.pdf/.legend.txt/.meta.json
## Superseded files (the pre-rebuild, 5 October version): moved to
## review/figures/draft_manuscript_v1/deprecated/fig_02_cumulative_siteyears_igbp_20261005.*
##
## Gates (stop, do not save, on any mismatch): main panel 6,200 site-years;
## inset 781 sites; threshold counts 483 / 225 / 63 (same gates as the
## candidate this rebuild is based on,
## scripts/generate_fig02alt_cumulative_siteyears_record_length_inset.R).

if (file.exists(".env")) {
  library(dotenv)
  dotenv::load_dot_env()
}
source("R/pipeline_config.R")
source("R/utils.R")
source("R/plot_constants.R")
source("R/figures/fig_network_growth.R")
source("R/nature_format.R")
source("R/presentation_format.R")
check_pipeline_config()

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(ggplot2); library(readxl); library(tidyr)
  library(patchwork); library(fs)
})

msg <- function(...) message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), " ", ...)
msg("=== Figure 2: cumulative site-years by IGBP, with framed record-length inset ===")

OUT_DIR  <- file.path("review", "figures", "draft_manuscript_v1")
OUT_STEM <- file.path(OUT_DIR, "fig_02_cumulative_siteyears_igbp")
fs::dir_create(OUT_DIR)

SNAP_FILE <- "data/snapshots/fluxnet_shuttle_snapshot_20260920T102211.csv"
if (!file.exists(SNAP_FILE)) stop("Latest snapshot not found: ", SNAP_FILE, call. = FALSE)
msg("Using snapshot (latest): ", SNAP_FILE)
shuttle_meta <- read_csv(SNAP_FILE, show_col_types = FALSE)
if (dplyr::n_distinct(shuttle_meta$site_id) != 781L) {
  stop("Expected 781 sites in ", SNAP_FILE, ", got ", dplyr::n_distinct(shuttle_meta$site_id), call. = FALSE)
}

sites_marconi     <- read_csv("data/snapshots/sites_marconi_clean.csv", show_col_types = FALSE) |>
  left_join(read_csv("data/snapshots/years_marconi.csv", show_col_types = FALSE), by = "site_id")
sites_la_thuile   <- read_csv("data/snapshots/sites_la_thuile_clean.csv", show_col_types = FALSE) |>
  left_join(read_csv("data/snapshots/years_la_thuile.csv", show_col_types = FALSE), by = "site_id")
sites_fluxnet2015 <- read_csv("data/snapshots/sites_fluxnet2015_clean.csv", show_col_types = FALSE) |>
  left_join(read_csv("data/snapshots/years_fluxnet2015.csv", show_col_types = FALSE), by = "site_id")

presence_df <- read_csv(file.path(FLUXNET_DATA_ROOT, "snapshots", "site_year_data_presence.csv"),
                         show_col_types = FALSE) |>
  mutate(year = as.integer(.data$year), has_data = as.logical(.data$has_data))
msg("Loaded presence_df: ", nrow(presence_df), " rows")

## ---- La Thuile actual year-indicator presence (not first-to-last span) -----
## Correct total 965 site-years -- see R/figures/fig_network_growth.R docs.
la_thuile_xlsx        <- read_excel("data/lists/LaThuileList.xlsx")
la_thuile_yr_cols     <- names(la_thuile_xlsx)[grepl("^[0-9]{4}$", names(la_thuile_xlsx))]
la_thuile_year_matrix <- la_thuile_xlsx |>
  dplyr::select(site_id = "SITE", dplyr::all_of(la_thuile_yr_cols)) |>
  tidyr::pivot_longer(cols = dplyr::all_of(la_thuile_yr_cols), names_to = "year", values_to = "flag") |>
  dplyr::mutate(year = as.integer(.data$year), present = !is.na(.data$flag) & .data$flag == 1)
msg("Loaded La Thuile year-indicator matrix: ",
    sum(la_thuile_year_matrix$present), " site-years (expect 965)")

## =============================================================================
## Gate checks -- stop, do not save, on any mismatch
## =============================================================================
window <- data_year_window(presence_df)
siteyears_plotted <- presence_df |>
  dplyr::filter(has_data, year >= window$first_year, year <= window$last_year) |>
  dplyr::left_join(
    shuttle_meta |> dplyr::distinct(site_id, .keep_all = TRUE) |> dplyr::select(site_id, igbp),
    by = "site_id"
  ) |>
  dplyr::filter(!is.na(igbp), igbp %in% PAPER_IGBP_ORDER)
main_total_siteyears <- nrow(siteyears_plotted)
msg("Main panel total site-years (", window$first_year, "-", window$last_year, "): ", main_total_siteyears)

record_length <- fig_record_length_histogram(
  site_meta           = shuttle_meta,
  presence_df         = presence_df,
  thresholds          = c(5L, 10L, 20L),
  base_size           = NATURE_SMALL_PT,
  show_legend         = FALSE,
  show_thresh_labels  = FALSE,
  x_label             = NULL,
  y_label             = "Sites"
)
msg("Inset sites: ", record_length$n_sites,
    "; threshold counts (>=5,>=10,>=20): ", paste(record_length$thresh_counts, collapse = ", "))

gate_issues <- character(0)
if (main_total_siteyears != 6200L) {
  gate_issues <- c(gate_issues, paste0("main panel total site-years = ", main_total_siteyears, ", expected 6200"))
}
if (record_length$n_sites != 781L) {
  gate_issues <- c(gate_issues, paste0("inset n_sites = ", record_length$n_sites, ", expected 781"))
}
if (!identical(as.integer(record_length$thresh_counts), c(483L, 225L, 63L))) {
  gate_issues <- c(gate_issues, paste0("inset threshold counts = ",
                                        paste(record_length$thresh_counts, collapse = ", "),
                                        ", expected 483, 225, 63"))
}
if (length(gate_issues) > 0L) {
  stop("Gate check FAILED -- not saving any output:\n  - ", paste(gate_issues, collapse = "\n  - "), call. = FALSE)
}
msg("Gate checks PASSED: 6200 main-panel site-years; 781 inset sites; thresholds 483/225/63.")

## =============================================================================
## Build panels
## =============================================================================

## ---- Main panel: no release-year dashed lines; Marconi dark grey -----------
main_panel <- fig_cumulative_siteyears_igbp(
  presence_df              = presence_df,
  shuttle_meta              = shuttle_meta,
  sites_marconi             = sites_marconi,
  sites_la_thuile           = sites_la_thuile,
  sites_fluxnet2015         = sites_fluxnet2015,
  base_size                 = 9L,
  la_thuile_year_matrix     = la_thuile_year_matrix,
  show_release_year_lines   = FALSE,
  marconi_colour            = "#404040"
) +
  nature_theme(base_size = NATURE_BASE_PT) +
  ggplot2::theme(
    legend.position  = "none",   # the shared legend is drawn inside the inset's frame instead
    plot.margin      = ggplot2::margin(t = 5.5, r = 14, b = 5.5, l = 5.5, unit = "pt")
  )

## ---- Inset footprint, in main-panel data coordinates -----------------------
## Matches review/figures/candidates/fig_02alt_mockup_dave_02.png (measured
## directly from the mockup PNG's own pixel geometry: panel border, inset
## border, and axis tick-label positions -- not eyeballed) -- a large, framed
## box filling most of the upper-left quadrant, flush with the panel's top
## and left edges. Converted to npc via the main panel's own scale_x/y_
## continuous() expansion (1% each side on x; 0/18% on y), exactly as the
## candidate inset script does, so the footprint stays correct if the
## data-driven year window or y-axis max ever changes.
##
## The mockup's own bottom edge measured at ~y=1,894 cumulative site-years --
## checked directly against this script's actual data (not assumed): the
## main-panel stacked-area cumulative total reaches 1,968 site-years by year
## 2012 (the inset's right edge), which would sit INSIDE a box bottomed at
## 1,894. Kept at y=2,300 instead -- the same bottom edge already proven
## clear (332 site-years of margin at the right edge) in the Figure 2 inset
## candidate's revision 2 (logs/prompts/2026-10-12_fig02_inset_rev2.md) --
## rather than reproduce the mockup's unsafe literal value. The top edge
## follows the mockup up to near the panel's own top (y=7,200 of a ~7,316
## max), taller than the candidate's y=7,000, since the candidate's bottom
## edge (2,300) is unchanged and Dave's mockup asks for the inset to fill
## more of the available vertical space.
x_expand_half <- 0.01 * (window$last_year - window$first_year)
x_lo <- window$first_year - x_expand_half
x_hi <- window$last_year  + x_expand_half
npc_x <- function(year) (year - x_lo) / (x_hi - x_lo)

y_hi <- main_total_siteyears * 1.18   # scale_y_continuous(expand = c(0, 0.18)) in fig_cumulative_siteyears_igbp()
npc_y <- function(siteyears) siteyears / y_hi

INSET_LEFT   <- npc_x(1992)
INSET_RIGHT  <- npc_x(2012)
INSET_BOTTOM <- npc_y(2300)
INSET_TOP    <- npc_y(7200)
msg("Inset footprint (npc): left=", round(INSET_LEFT, 3), " right=", round(INSET_RIGHT, 3),
    " bottom=", round(INSET_BOTTOM, 3), " top=", round(INSET_TOP, 3),
    " (", round(100 * (INSET_RIGHT - INSET_LEFT), 1), "% width x ",
    round(100 * (INSET_TOP - INSET_BOTTOM), 1), "% height of the panel)")

## ---- Legend: single-column IGBP key, INSIDE the inset's own frame ----------
## Unlike the candidate (whose legend could extend past the inset's own right
## edge, capped only by the 2015 reference line), the rebuilt figure's legend
## box is a strict sub-rectangle of [INSET_LEFT, INSET_RIGHT] x [INSET_BOTTOM,
## INSET_TOP] -- "single-column IGBP key inside the inset", per Dave's
## mockup -- so it can never newly overlap main-panel data beyond what the
## inset itself already, legitimately covers; the only remaining check is
## that it stays clear of the histogram's own bars (verified after
## rendering -- see run report / SESSION_LOG.md).
## Fractions measured directly from the mockup's own legend swatch/title
## pixel positions within the inset's cropped pixel box (title "IGBP" flush
## at the inset's own top edge; last legend row ends about 13% up from the
## inset's own bottom edge; swatches begin about 74% across the inset's own
## width).
LEGEND_LEFT   <- INSET_LEFT   + 0.74 * (INSET_RIGHT - INSET_LEFT)
LEGEND_RIGHT  <- INSET_RIGHT  - 0.015 * (INSET_RIGHT - INSET_LEFT)
LEGEND_BOTTOM <- INSET_BOTTOM + 0.13  * (INSET_TOP - INSET_BOTTOM)
LEGEND_TOP    <- INSET_TOP    - 0.02  * (INSET_TOP - INSET_BOTTOM)

## Frame line weight: same as nature_theme()'s default panel.border weight
## (the main panel's own frame), per "inset frame at the main panel's frame
## line weight".
FRAME_LWD <- nature_lwd((NATURE_LINEWIDTH_MIN + NATURE_LINEWIDTH_MAX) / 2)

## A near-empty theme_void() plot carrying only the legend -- same mechanism
## as the Figure 2 inset candidate's legend_panel() (keeps absolute pt/mm
## sizes regardless of its assigned inset box; see that script's comments for
## why a raw extracted grob was rejected). Rev2 fixes carried over unchanged:
## alpha = 0.8 keys matching the plotted fills' own alpha, and geom_tile()
## (not geom_point()) so legend.key.size alone governs key size, set here to
## about one text-line tall.
legend_panel <- function(legend_size) {
  key_mm <- legend_size * 0.37   # ~1 text line height in mm (1pt = 0.3528mm; a little slack for ascenders)
  ggplot2::ggplot(record_length$hist_df, ggplot2::aes(x = 1, y = 1, fill = .data$igbp)) +
    ggplot2::geom_tile(alpha = 0) +
    scale_fill_paper_igbp(name = "IGBP", na.value = "grey70") +
    ggplot2::guides(fill = ggplot2::guide_legend(
      override.aes = list(alpha = 0.8),   # matches geom_area()/geom_histogram()'s own alpha = 0.8
      ncol = 1
    )) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position         = "inside",
      legend.position.inside  = c(0, 1),
      legend.justification    = c(0, 1),
      legend.background       = ggplot2::element_blank(),
      legend.key              = ggplot2::element_rect(fill = "white", colour = NA),
      legend.key.size         = grid::unit(key_mm, "mm"),
      legend.key.spacing.y    = grid::unit(key_mm * 0.15, "mm"),
      legend.spacing.y        = grid::unit(0, "pt"),
      legend.text             = ggplot2::element_text(size = legend_size, family = NATURE_FONT, colour = "grey10"),
      legend.title            = ggplot2::element_text(size = legend_size, family = NATURE_FONT, colour = "grey10"),
      plot.background         = ggplot2::element_rect(fill = NA, colour = NA)
    )
}

## ---- Inset: record-length histogram, framed, opaque white background ------
## Axis titles and "n=" threshold labels as in S6 (both restored from the
## candidate, which already had them). The frame (plot.background, not
## panel.border -- the mockup's border encloses the axis titles and tick
## labels too, not just the inner data panel) is drawn here at FRAME_LWD.
build_inset_panel <- function(base_size) {
  rl <- fig_record_length_histogram(
    site_meta           = shuttle_meta,
    presence_df         = presence_df,
    thresholds          = c(5L, 10L, 20L),
    base_size           = base_size,
    show_legend         = FALSE,
    show_thresh_labels  = TRUE,
    x_label             = "Record length (years)",
    y_label             = "Number of sites"
  )
  rl$plot +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = "white", colour = "black", linewidth = FRAME_LWD),
      panel.border     = ggplot2::element_blank(),   # single outer frame only, not a second inner box
      plot.margin      = ggplot2::margin(t = 3, r = 3, b = 3, l = 3, unit = "pt"),
      axis.title       = ggplot2::element_text(size = base_size),
      axis.text        = ggplot2::element_text(size = base_size)
    )
}

build_combined <- function(inset_base_size, legend_size) {
  inset_panel  <- build_inset_panel(inset_base_size)
  legend_inset <- legend_panel(legend_size)
  main_panel +
    patchwork::inset_element(
      inset_panel,
      left = INSET_LEFT, bottom = INSET_BOTTOM, right = INSET_RIGHT, top = INSET_TOP,
      align_to = "panel"
    ) +
    patchwork::inset_element(   # added after (drawn on top of) inset_panel, inside its own frame
      legend_inset,
      left = LEGEND_LEFT, bottom = LEGEND_BOTTOM, right = LEGEND_RIGHT, top = LEGEND_TOP,
      align_to = "panel"
    )
}

## ---- Local save helper (not save_nature_figure(): that function asserts
## main-text widths of exactly 89 or 183mm, and this rebuild is 120x120mm per
## Dave's mockup -- same PNG/PDF mechanics otherwise: ragg::agg_png at
## 600dpi, base grDevices::pdf with Helvetica, white bg.) -----------------
save_candidate <- function(plot, path_stem, width_mm, height_mm) {
  png_path <- paste0(path_stem, ".png")
  pdf_path <- paste0(path_stem, ".pdf")
  ggplot2::ggsave(png_path, plot, width = width_mm, height = height_mm, units = "mm",
                   dpi = 600, device = ragg::agg_png, bg = "white")
  ggplot2::ggsave(pdf_path, plot, width = width_mm, height = height_mm, units = "mm",
                   device = grDevices::pdf, family = "Helvetica", bg = "white")
  invisible(list(png = png_path, pdf = pdf_path))
}

## Text no larger than 7pt (NATURE_BASE_PT) anywhere -- main panel, inset
## axis/tick text, and legend text all at 7pt (checked by rendering and
## inspection to fit cleanly at 120mm -- see run report).
combined <- build_combined(inset_base_size = NATURE_BASE_PT, legend_size = NATURE_BASE_PT)
saved <- save_candidate(combined, OUT_STEM, width_mm = 120, height_mm = 120)
msg("Saved: ", saved$png, ", ", saved$pdf)

## ---- Historical line end points (reported in the legend) -------------------
marconi_end     <- sites_marconi |>
  dplyr::distinct(site_id, .keep_all = TRUE) |>
  dplyr::filter(!is.na(first_year), !is.na(last_year)) |>
  dplyr::summarise(n = sum(last_year - first_year + 1L)) |> dplyr::pull(n)
la_thuile_end   <- sum(la_thuile_year_matrix$present)
fluxnet2015_end <- sites_fluxnet2015 |>
  dplyr::distinct(site_id, .keep_all = TRUE) |>
  dplyr::filter(!is.na(first_year), !is.na(last_year)) |>
  dplyr::summarise(n = sum(last_year - first_year + 1L)) |> dplyr::pull(n)
msg("Historical line end points -- Marconi: ", marconi_end,
    ", La Thuile: ", la_thuile_end, ", FLUXNET2015: ", fluxnet2015_end)

## ---- Legend text -------------------------------------------------------------
legend_lines <- c(
  "FIGURE LEGEND -- fig_02_cumulative_siteyears_igbp.png",
  strrep("=", 60), "",
  "TITLE: Figure 2. Cumulative site-years of the FLUXNET Shuttle network through time,",
  "by IGBP land-cover class, with a record-length inset", "",
  "DESCRIPTION:",
  "Cumulative site-years of the current network through time, stacked by IGBP class",
  "(paper-wide palette, R/plot_constants.R::PAPER_IGBP_COLOURS), with Marconi/La Thuile/",
  "FLUXNET2015 cumulative site-year lines overlaid for historical context (comparison data",
  "only, not primary data -- see CLAUDE.md Hard Rule 1). Marconi is drawn dark grey",
  "(#404040); La Thuile stays red, FLUXNET2015 stays blue. No panel letter.",
  "",
  paste0("INSET (framed, upper left): distribution of years with data per site in the snapshot"),
  "(781 sites), stacked by IGBP class, same measure and palette as the main panel.",
  "\"Record length\" = years with any of the flux variables checked by",
  "compute_site_year_presence() (R/utils.R) present in at least one month of that year --",
  "not a published site-table listing (the current network has no such table). Dashed",
  "vertical lines mark 5, 10 and 20 years with data, each labelled with the number of sites",
  paste0("at or above that threshold: n=", record_length$thresh_counts[1], " (>=5yr), n=",
         record_length$thresh_counts[2], " (>=10yr), n=", record_length$thresh_counts[3],
         " (>=20yr), positioned clear of every bar (confirmed by measurement of the rendered"),
  "PDF, not visual inspection -- see SESSION_LOG.md 2026-10-12). Opaque white background,",
  "solid black frame at the main panel's own border line weight. Same panel-building",
  "function as Supplementary Figure S1 panel a and the retired record-length inset",
  "candidate (fig_record_length_histogram(), R/figures/fig_network_growth.R).",
  "",
  "LEGEND: one shared IGBP legend for both panels (both use PAPER_IGBP_ORDER/",
  "PAPER_IGBP_COLOURS), drawn as a single column inside the inset's own frame, in its",
  "empty upper-right region, clear of every bar. Keys drawn at the same alpha (0.8, over",
  "white) as the plotted fills, confirmed by pixel sampling to match exactly; key height",
  "set to approximately one text line. No separate border around the legend -- it shares",
  "the inset's single outer frame.",
  "",
  "SITE-YEAR RULE (each series counts site-years by its own collection's convention):",
  paste0("  Current network: compute_site_year_presence() -- a site-year counts if any of its"),
  paste0("    flux variables (NEE/GPP/RECO/LE/H) has a non-NA monthly value; has_data = TRUE, ",
         window$first_year, "-", window$last_year, "."),
  paste0("  Marconi: sum(last_year - first_year + 1) over its own 'Years in Marconi' ranges",
         " -- ends at ", marconi_end, "."),
  paste0("  La Thuile: count of 1s in its own year-indicator matrix, NOT the first-to-last",
         " span (which over-counts at 1008) -- ends at ", la_thuile_end, "."),
  paste0("  FLUXNET2015: count of non-NA cells in its own year-presence matrix -- ends at ",
         fluxnet2015_end, "."),
  "",
  paste0("YEAR WINDOW (data-driven, data_year_window() in R/utils.R -- not hard-coded): ",
         window$first_year, "-", window$last_year, ". ", window$n_sites_last_year,
         " of ", dplyr::n_distinct(shuttle_meta$site_id), " network sites report data in ",
         window$last_year, ", the most recent year with any site data (ignoring empty",
         " ONEFlux padding years)."),
  paste0("TOTAL SITE-YEARS PLOTTED (", window$first_year, "-", window$last_year, "): ",
         main_total_siteyears, "."),
  "",
  paste0("DATA: ", SNAP_FILE, " (781 sites, the snapshot used for this figure -- see",
         " docs/decisions_pending.md 2026-10-12)."),
  "",
  "SOURCE: scripts/generate_fig_cumulative_siteyears.R, calling",
  "R/figures/fig_network_growth.R::fig_cumulative_siteyears_igbp() and",
  "::fig_record_length_histogram(). Gate checks (stop, no save, on mismatch): main panel",
  "6,200 site-years; inset 781 sites; threshold counts 483/225/63 -- all passed.",
  "",
  "DIMENSIONS: 120 x 120 mm, 600 dpi PNG + vector PDF, Helvetica, white background.",
  "All text 7pt (the ceiling for this rebuild)."
)
writeLines(legend_lines, paste0(OUT_STEM, ".legend.txt"))
msg("Saved: ", paste0(OUT_STEM, ".legend.txt"))

write_output_metadata(
  saved$png,
  input_sources = c(SNAP_FILE,
                     "data/snapshots/site_year_data_presence.csv",
                     "data/snapshots/sites_marconi_clean.csv", "data/snapshots/years_marconi.csv",
                     "data/snapshots/sites_la_thuile_clean.csv", "data/snapshots/years_la_thuile.csv",
                     "data/lists/LaThuileList.xlsx",
                     "data/snapshots/sites_fluxnet2015_clean.csv", "data/snapshots/years_fluxnet2015.csv"),
  notes = paste0("Rebuilt 2026-10-12 to match Dave's edited mockup (fig_02alt_mockup_dave_02.png): ",
                  "framed record-length inset, no release-year dashed lines, Marconi dark grey. ",
                  "Snapshot: ", SNAP_FILE, ". Gate checks passed: 6200 main-panel site-years, ",
                  "781 inset sites, thresholds 483/225/63. See .legend.txt.")
)

## =============================================================================
## Presentation mode (slide deck) -- additive, gated by an env var; does not
## touch anything above (same data/gates/colours as the manuscript figure).
## Default (unset) behaviour is unaffected -- see R/presentation_format.R.
## =============================================================================
presentation_mode <- identical(Sys.getenv("FLUXNET_PRESENTATION_MODE"), "1")
if (presentation_mode) {
  msg("--- Presentation mode: building Figure 2 slides ---")
  SLIDE_DIR <- file.path("review", "figures", "presentation_figures", "slides")
  fs::dir_create(SLIDE_DIR)

  ## Step 1: historical lines only, on the final axes. The function's own
  ## geom_label() direct end-of-line labels already identify the 3 series --
  ## no separate legend needed.
  pres_step1 <- fig_cumulative_siteyears_igbp(
    presence_df             = presence_df,
    shuttle_meta             = shuttle_meta,
    sites_marconi            = sites_marconi,
    sites_la_thuile          = sites_la_thuile,
    sites_fluxnet2015        = sites_fluxnet2015,
    base_size                = 32L,
    la_thuile_year_matrix    = la_thuile_year_matrix,
    show_current_network     = FALSE,
    show_release_year_lines  = FALSE,
    marconi_colour           = "#404040",
    line_pt                  = 2.5
  ) + presentation_theme()
  save_presentation_figure(pres_step1, file.path(SLIDE_DIR, "fig02_step1_historical_lines"))

  ## Step 2: + IGBP-stacked areas, no inset, one large legend, same axes as
  ## step 1 (fig_cumulative_siteyears_igbp() keeps the y-range identical
  ## either way via its internal geom_blank() mechanism).
  pres_step2 <- fig_cumulative_siteyears_igbp(
    presence_df             = presence_df,
    shuttle_meta             = shuttle_meta,
    sites_marconi            = sites_marconi,
    sites_la_thuile          = sites_la_thuile,
    sites_fluxnet2015        = sites_fluxnet2015,
    base_size                = 32L,
    la_thuile_year_matrix    = la_thuile_year_matrix,
    show_current_network     = TRUE,
    show_release_year_lines  = FALSE,
    marconi_colour           = "#404040",
    line_pt                  = 2.5
  ) + presentation_theme()
  save_presentation_figure(pres_step2, file.path(SLIDE_DIR, "fig02_step2_igbp_areas"))

  ## Step 3: record-length histogram, standalone, full slide (not the small
  ## framed inset version) -- same 5/10/20yr thresholds and counts.
  pres_rl <- fig_record_length_histogram(
    site_meta           = shuttle_meta,
    presence_df         = presence_df,
    thresholds          = c(5L, 10L, 20L),
    base_size           = NATURE_BASE_PT,
    annot_pt            = 20,
    show_legend         = TRUE,
    legend_key_mm       = 8,
    show_thresh_labels  = TRUE,
    x_label             = "Record length (years)",
    y_label             = "Number of sites"
  )
  pres_step3 <- pres_rl$plot + presentation_theme()
  save_presentation_figure(pres_step3, file.path(SLIDE_DIR, "fig02_step3_record_length_histogram"))

  msg("Presentation slides saved: fig02_step1/2/3 -> ", SLIDE_DIR)
}

msg("=== Done ===")
